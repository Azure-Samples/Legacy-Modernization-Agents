import copy
import json
import os
import subprocess
import unittest
from unittest.mock import patch

from test_rekt_dependency_evidence import DependencyFixture
import platform_copybook_contracts as contracts
import rekt_dependency_evidence as evidence


class PlatformCopybookContractTests(DependencyFixture):
    def profile(self):
        return {
            "schemaVersion": 1, "profileKind": "platform-replacement", "profileName": "test-terminal-events",
            "copybooks": {"DFHAID": {
                "recipe": "symbolic-terminal-events-v1", "selector": "EIBAID",
                "symbols": dict(contracts.SYMBOLS), "documentation": [contracts.DOCUMENTATION],
                "targetSemantics": "named-events-preserve-source-branches",
            }},
        }

    def test_explicit_opt_in_and_genuine_supplied_definition_precedence(self):
        output = self.source / ".preprocessed"
        output.mkdir()
        configured = self.work / "platform.json"
        configured.write_text(json.dumps(self.profile()))
        contracts.generate(self.source, output, None)
        self.assertFalse((output / "DFHAID.cpy").exists())
        contracts.generate(self.source, output, str(configured))
        projected = output / "DFHAID.cpy"
        self.assertEqual(configured.resolve(), evidence.current_transform(projected))
        self.assertFalse(any(" VALUE " in line for line in projected.read_text().splitlines() if line[6] != "*"))
        projected.unlink()
        self.write("sdk/DFHAID.cpy", "       01 ENTITLED-TEST-DEFINITION PIC X.\n")
        contracts.generate(self.source, output, str(configured))
        self.assertFalse(projected.exists())

    def test_rejects_source_bytes_unknown_symbols_selectors_and_business_actions(self):
        for field, value in [("selector", "ACCOUNT"), ("symbols", {"DFHENTER": 125}),
                             ("targetSemantics", "ENTER-CREATES-ACCOUNT"),
                             ("documentation", ["https://example.test/guessed"])]:
            profile = self.profile()
            profile["copybooks"]["DFHAID"][field] = value
            with self.subTest(field=field), self.assertRaises(ValueError):
                contracts.validate(profile)

    def test_bms_projection_requires_explicit_reviewed_recipe_and_never_uses_generated_constants(self):
        profile = self.profile()
        profile["copybooks"]["DFHBMSCA"] = copy.deepcopy(contracts.BMS_CONTRACT)
        configured = self.work / "platform.json"
        configured.write_text(json.dumps(profile))
        output = self.source / ".preprocessed"
        output.mkdir()
        self.write("copy-generated/DFHBMSCA.cpy", "       01 DFHBMSCA.\n           02 DFHBMASK PIC X VALUE X'00'.\n")
        contracts.generate(self.source, output, str(configured))
        projected = (output / "DFHBMSCA.cpy").read_text()
        self.assertIn("02 DFHBMASK PIC X.", projected)
        self.assertFalse(any(" VALUE " in line for line in projected.splitlines() if line[6] != "*"))
        profile["copybooks"]["DFHBMSCA"]["symbols"]["DFHBMASK"] = "A:UNPROTECTED"
        with self.assertRaises(ValueError):
            contracts.validate(profile)

    def fixture(self):
        self.program.write_text(
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROG.\n"
            "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       COPY DFHAID.\n"
            "       PROCEDURE DIVISION.\n           IF EIBAID = DFHENTER\n"
            "               CONTINUE\n           END-IF.\n           GOBACK.\n")
        # Preprocessing snapshots the profile next to the estate's projections.
        output = self.source / ".preprocessed"
        output.mkdir()
        configured = output / "platform-contracts.json"
        profile = self.profile()
        configured.write_text(json.dumps(profile))
        contracts.generate(self.source, output, str(configured))
        staging = self.source / ".rekt-staging"
        evidence.stage(self.program, staging / self.relative)
        evidence.stage(self.program, staging / ".parser-input" / self.relative / self.program.name)
        evidence.stage(output / "DFHAID.cpy", staging / "DFHAID.cpy", derived=True)
        syntax = {"rule": "ConditionContext", "children": [
            {"rule": "SimpleConditionContext", "children": [
                {"token": "EIBAID"}, {"token": "="}, {"token": "DFHENTER"}]}]}
        binding = {
            "name": "DFHAID", "profileName": profile["profileName"], "profileKind": "platform-replacement",
            "sourcePath": str(configured), "sourceSha256": evidence.digest(configured),
            "outputPath": "/source/.rekt-staging/DFHAID.cpy", "outputSha256": evidence.digest(staging / "DFHAID.cpy"),
            "definition": profile["copybooks"]["DFHAID"], "resolutionKind": "symbolic-platform-replacement",
            "runtimeMapping": "implementation-required", "sourceValues": "not-supplied", "bindingStatus": "complete",
            "usages": [{"grammarRule": "ConditionContext", "syntax": syntax, "sourceText": "EIBAID = DFHENTER",
                        "line": 8, "column": 14, "coordinateSpace": "expanded-parser-input"}],
        }
        node = {
            "dependencyKind": "PLATFORM_COPY", "callTarget": {"name": "DFHAID", "programReferenceType": "STATIC"},
            "runtimeContractBindingComplete": True, "dependencyAnalysisComplete": False, "runtimeContract": binding,
            "dependencies": [],
        }
        document = {
            "dependencyExportSchemaVersion": 1, "callTarget": {"name": "PROG.cbl", "programReferenceType": "STATIC"},
            "sourcePath": "/source/.rekt-staging/.parser-input/" + self.relative + "/" + self.program.name,
            "sourceSha256": evidence.digest(self.program),
            "dependencyAnalysisComplete": True, "dependencyDiagnostics": [],
            "dependencyCoverage": dict.fromkeys(evidence.KINDS, "complete"),
            "dependencyAbsenceEvidence": dict(evidence.ABSENCE_PROOFS), "dependencies": [node],
        }
        self.export.write_text(json.dumps(document))
        return configured, profile, staging, node

    def test_records_distinct_replacement_not_vendor_copy_and_rejects_tampering(self):
        configured, profile, staging, node = self.fixture()
        with patch.dict(os.environ, {"REKT_PLATFORM_CONTRACTS": str(configured)}):
            result = self.build(staging=staging)
            self.assertEqual([], result["unresolvedDiagnostics"])
            self.assertEqual({"PLATFORM_COPY", "PLATFORM_INPUT"}, {item["kind"] for item in result["definitions"]})
            evidence.write_json(self.output / (self.relative + ".report/dependency-evidence.json"), result)
            self.assertTrue(evidence.evidence_current(self.source, self.relative, self.output))
            for name in ("source", "projection", "profile"):
                damaged = copy.deepcopy(node)
                if name == "source":
                    damaged["sourcePath"] = str(self.program)
                elif name == "projection":
                    damaged["runtimeContract"]["outputSha256"] = "0" * 64
                else:
                    damaged["runtimeContract"]["definition"]["symbols"]["DFHENTER"] = "CREATE-CUSTOMER"
                with self.subTest(damage=name):
                    self.assertFalse(evidence.native_platform_contract_bound(damaged, profile))
            (staging / "DFHAID.cpy").write_text("       01 DFHAID PIC X VALUE SPACE.\n")
            self.assertTrue(any("native-platform-contract-unverified" in error
                                for error in self.build(staging=staging)["unresolvedDiagnostics"]))

    def test_application_and_conjunct_usage_is_bounded(self):
        _, profile, _, node = self.fixture()
        usage = node["runtimeContract"]["usages"][0]
        conjunct = {**usage, "grammarRule": "SimpleConditionContext", "conditionRole": "application-and-conjunct",
                    "syntax": usage["syntax"]["children"][0]}
        node["runtimeContract"]["usages"] = [conjunct]
        self.assertTrue(evidence.native_platform_contract_bound(node, profile))
        two_events = {"rule": "SimpleConditionContext", "children": [
            {"token": "EIBAID"}, {"token": "="}, {"token": "DFHENTER"}, {"token": "DFHPF3"}]}
        for name, damaged in (
                ("whole condition", {**conjunct, "syntax": usage["syntax"]}),
                ("unknown role", {**conjunct, "conditionRole": "application-or-conjunct"}),
                ("two events", {**conjunct, "syntax": two_events}),
                ("application tokens", {**conjunct, "syntax": {"rule": "SimpleConditionContext", "children": [
                    {"token": "WS-FLAG"}, {"token": "="}, {"token": "DFHENTER"}]}})):
            with self.subTest(damage=name):
                node["runtimeContract"]["usages"] = [damaged]
                self.assertFalse(evidence.native_platform_contract_bound(node, profile))

    def test_evaluate_eibaid_usages_are_ordered_and_bounded(self):
        _, profile, _, node = self.fixture()
        def usage(role, rule, tokens, text):
            return {"grammarRule": rule, "conditionRole": role, "sourceText": text, "line": 5,
                    "column": 12, "coordinateSpace": "expanded-parser-input",
                    "syntax": {"rule": rule, "children": [{"token": token} for token in tokens]}}
        subject = usage("evaluate-subject", "EvaluateSelectContext", ["EIBAID"], "EIBAID")
        enter = usage("evaluate-when", "EvaluateWhenContext", ["WHEN", "DFHENTER"], "WHEN DFHENTER")
        pf3 = usage("evaluate-when", "EvaluateWhenContext", ["WHEN", "DFHPF3"], "WHEN DFHPF3")
        node["runtimeContract"]["usages"] = [subject, enter, pf3]
        self.assertTrue(evidence.native_platform_contract_bound(node, profile))
        for name, usages in (
                ("subject without when", [subject]),
                ("when without subject", [enter, pf3]),
                ("application subject", [{**subject, "syntax": {"rule": "EvaluateSelectContext",
                                                                 "children": [{"token": "WS-KEY"}]}}, enter]),
                ("unknown event", [subject, usage("evaluate-when", "EvaluateWhenContext",
                                                  ["WHEN", "DFHPF99"], "WHEN DFHPF99")]),
                ("two events", [subject, usage("evaluate-when", "EvaluateWhenContext",
                                               ["WHEN", "DFHPF3", "DFHPF4"], "WHEN DFHPF3 DFHPF4")]),
                ("wrong rule", [subject, {**enter, "grammarRule": "EvaluateSelectContext"}])):
            with self.subTest(damage=name):
                node["runtimeContract"]["usages"] = usages
                self.assertFalse(evidence.native_platform_contract_bound(node, profile))

    def test_qualified_multi_target_bms_move_binds_every_target(self):
        def field(name, suffix):
            return {"name": name + suffix, "field": name, "suffix": suffix, "map": "CACTVWA",
                    "mapset": "COACTVW", "layoutPath": "x.bms-layout.json", "layoutSha256": "a",
                    "outputPath": "x", "outputSha256": "b"}
        declaration = {"rule": "DataDescriptionEntryFormat1Context", "children": []}
        tokens = ["MOVE", "DFHBMDAR", "TO", "ACCTSIDA", "OF", "CACTVWAI", "CUSTIDA", "OF", "CACTVWAI"]
        usage = {"grammarRule": "MoveStatementContext", "coordinateSpace": "expanded-parser-input", "line": 9,
                 "sourceText": " ".join(tokens), "valueKind": "named-attribute",
                 "syntax": {"rule": "MoveStatementContext", "children": [{"token": t} for t in tokens]},
                 "fieldBindings": [field("ACCTSID", "A"), field("CUSTID", "A")],
                 "definitionSyntaxes": [declaration, declaration]}
        self.assertTrue(contracts.bms_usage_bound(usage))
        single = {key: value for key, value in usage.items() if key not in ("fieldBindings", "definitionSyntaxes")}
        for name, damaged in (
                ("missing binding", {**usage, "fieldBindings": usage["fieldBindings"][:1],
                                     "definitionSyntaxes": [declaration]}),
                ("foreign group", {**usage, "sourceText": usage["sourceText"].replace("CACTVWAI", "OTHERI", 1),
                                   "syntax": {"rule": "MoveStatementContext", "children": [
                                       {"token": "OTHERI" if i == 5 else t} for i, t in enumerate(tokens)]}}),
                ("class mismatch", {**usage, "sourceText": usage["sourceText"].replace("DFHBMDAR", "DFHRED"),
                                    "syntax": {"rule": "MoveStatementContext", "children": [
                                        {"token": "DFHRED" if i == 1 else t} for i, t in enumerate(tokens)]}}),
                ("single shape for qualified", {**single, "fieldBinding": field("ACCTSID", "A"),
                                                "definitionSyntax": declaration}),
                ("both shapes", {**usage, "fieldBinding": field("ACCTSID", "A"), "definitionSyntax": declaration})):
            with self.subTest(damage=name):
                self.assertFalse(contracts.bms_usage_bound(damaged))

    def test_bms_overlay_table_move_binds_each_occurrence_to_the_proved_offset(self):
        def entry(*tokens):
            return {"rule": "DataDescriptionEntryFormat1Context", "children": [{"token": t} for t in tokens]}
        record = entry("01", "FIL-R005", "REDEFINES", "MAPAI", ".")
        members = [entry("05", "FILLER", "PIC", "X(12)", "."), entry("05", "ROWS", "OCCURS", "3", "TIMES", "."),
                   entry("10", "SELL", "PIC", "S9(4)", "COMP", "."), entry("10", "SELF", "PIC", "X", "."),
                   entry("10", "FIL-R006", "REDEFINES", "SELF", "."), entry("15", "SELA", "PIC", "X", "."),
                   entry("10", "SELI", "PIC", "X(2)", ".")]
        words = [[c["token"] for c in item["children"]] for item in members]
        self.assertEqual(("MAPAI", 14, 5, 3),
                         contracts.overlay_layout([c["token"] for c in record["children"]], words, 5))
        tokens = ["MOVE", "DFHBMPRO", "TO", "SELA", "(", "I", ")"]
        binding = {"name": "SELA", "subscript": "I", "map": "MAPA", "mapset": "MAPS", "suffix": "A",
                   "layoutPath": "x.bms-layout.json", "layoutSha256": "a", "outputPath": "x", "outputSha256": "b",
                   "recordSyntax": record, "memberSyntaxes": members, "definitionSyntax": members[5],
                   "occurrences": [{"field": f"SEL{row}", "offset": 14 + 5 * row} for row in range(3)]}
        usage = {"grammarRule": "MoveStatementContext", "coordinateSpace": "expanded-parser-input", "line": 9,
                 "sourceText": " ".join(tokens), "valueKind": "named-attribute",
                 "syntax": {"rule": "MoveStatementContext", "children": [{"token": t} for t in tokens]},
                 "overlayBindings": [binding]}
        self.assertTrue(contracts.bms_usage_bound(usage))
        shifted = copy.deepcopy(usage)
        shifted["overlayBindings"][0]["occurrences"][1]["offset"] += 1
        wider = copy.deepcopy(usage)
        wider["overlayBindings"][0]["memberSyntaxes"][0] = entry("05", "FILLER", "PIC", "X(13)", ".")
        packed = copy.deepcopy(usage)
        packed["overlayBindings"][0]["memberSyntaxes"][6] = entry("10", "SELI", "PIC", "9(3)", "COMP-3", ".")
        colour = copy.deepcopy(usage)
        colour["syntax"]["children"][1]["token"] = "DFHRED"
        colour["sourceText"] = colour["sourceText"].replace("DFHBMPRO", "DFHRED")
        for name, damaged in (("shifted occurrence", shifted), ("layout changed", wider),
                              ("unsupported member", packed), ("attribute class", colour)):
            with self.subTest(damage=name):
                self.assertFalse(contracts.bms_usage_bound(damaged))

    def test_unselected_contract_and_missing_business_copy_still_block(self):
        configured, _, staging, _ = self.fixture()
        with patch.dict(os.environ, {"REKT_PLATFORM_CONTRACTS": ""}):
            self.assertTrue(self.build(staging=staging)["unresolvedDiagnostics"])
        self.program.write_text(self.program.read_text() + "       COPY BUSINESS.\n")
        evidence.stage(self.program, staging / self.relative)
        evidence.stage(self.program, staging / ".parser-input" / self.relative / self.program.name)
        document = json.loads(self.export.read_text())
        document["sourceSha256"] = evidence.digest(self.program)
        self.export.write_text(json.dumps(document))
        with patch.dict(os.environ, {"REKT_PLATFORM_CONTRACTS": str(configured)}):
            self.assertIn("copy-not-exported:BUSINESS", self.build(staging=staging)["unresolvedDiagnostics"])

    @unittest.skipUnless(os.environ.get("REKT_NATIVE_PLATFORM_IMAGE"), "native candidate image not selected")
    def test_native_bms_attributes_preserve_field_bindings_and_literal_assignments(self):
        import bms_symbolic_map as bms
        self.program.write_text(
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROG.\n"
            "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n"
            "       COPY SCREEN.\n       COPY DFHBMSCA.\n"
            "       PROCEDURE DIVISION.\n"
            "           MOVE DFHBMASK TO FIELD1A.\n"
            "           MOVE DFHGREEN TO FIELD1C.\n"
            "           MOVE DFHUNDLN TO FIELD1H.\n"
            "           MOVE 'A' TO FIELD1A.\n"
            "           MOVE HIGH-VALUES TO FIELD1H.\n"
            "           MOVE DFHBMPRF TO FIELD1A.\n"
            "           MOVE DFHBMFSE TO FIELD1A.\n"
            "           MOVE DFHNEUTR TO FIELD1C.\n"
            "           GOBACK.\n")
        self.write("SCREEN.bms",
                   "SCREEN   DFHMSD TYPE=DSECT,LANG=COBOL,MODE=INOUT,".ljust(71) + "X\n"
                   "               DSATTS=(COLOR,HILIGHT),MAPATTS=(COLOR,HILIGHT)\n"
                   "PANEL    DFHMDI SIZE=(24,80)\n"
                   "FIELD1   DFHMDF POS=(1,1),LENGTH=4\n"
                   "         DFHMSD TYPE=FINAL\n         END\n")
        generated = self.source / ".preprocessed"
        generated.mkdir(parents=True, exist_ok=True)
        # Same layout as doctor.sh: the profile sits beside its transformed copybooks.
        configured = generated / "platform-contracts.json"
        profile = self.profile()
        profile["copybooks"]["DFHBMSCA"] = copy.deepcopy(contracts.BMS_CONTRACT)
        configured.write_text(json.dumps(profile))
        _, warnings = bms.generate(self.source, generated)
        self.assertEqual([], warnings)
        contracts.generate(self.source, generated, str(configured))
        staging = self.source / ".rekt-staging"
        evidence.stage(self.program, staging / self.relative)
        evidence.stage(self.program, staging / ".parser-input" / self.relative / self.program.name)
        for name in ("SCREEN", "DFHBMSCA"):
            evidence.stage(generated / f"{name}.cpy", staging / f"{name}.cpy", derived=True)
        result = subprocess.run([
            "docker", "run", "--rm", "--memory=1g", "--network", "none", "--entrypoint", "java",
            "-e", "JAVA_TOOL_OPTIONS=-Xmx512m -XX:-UsePerfData",
            "-v", f"{self.source}:/source:ro", "-v", f"{self.output}:/output",
            "-v", f"{configured}:{configured}:ro", "-e", f"REKT_PLATFORM_CONTRACTS={configured}",
            os.environ["REKT_NATIVE_PLATFORM_IMAGE"], "-jar", "/app/smojol-cli.jar",
            "dependency", "PROG.cbl", "--srcDir=/source/.rekt-staging/.parser-input/estate/PROG.cbl",
            "--copyBooksDir=/source/.rekt-staging", "--dialectJarPath=/app/dialect-idms.jar",
            f"--export=/output/{self.export.relative_to(self.output)}",
        ], capture_output=True, text=True, timeout=120)
        (self.output / "native-bms.log").write_text(result.stdout + result.stderr)
        document = json.loads(self.export.read_text())
        self.assertEqual(0, result.returncode, document.get("dependencyDiagnostics") or (result.stdout + result.stderr)[-5000:])
        leaf = next(node for node in document["dependencies"] if node.get("dependencyKind") == "PLATFORM_COPY")
        self.assertTrue(evidence.native_platform_contract_bound(leaf, profile))
        self.assertEqual(8, len(leaf["runtimeContract"]["usages"]))
        self.assertEqual({"named-attribute", "source-character-literal", "figurative-constant"},
                         {usage["valueKind"] for usage in leaf["runtimeContract"]["usages"]})
        with patch.dict(os.environ, {"REKT_PLATFORM_CONTRACTS": str(configured)}):
            bound = self.build(staging=staging)
        self.assertEqual([], bound["unresolvedDiagnostics"])
        report = self.output / f"{self.relative}.report/dependency-evidence.json"
        evidence.write_json(report, bound)
        self.assertTrue(evidence.evidence_current(self.source, self.relative, self.output))

        def gate(expected, name):
            assembly = os.environ.get("REKT_PLATFORM_GATE_ASSEMBLY")
            if not assembly:
                return
            target = self.work / f"bms-gate-{name}"
            checked = subprocess.run(["dotnet", assembly, "dependency-plan", "--source", str(self.source),
                                      "--facts", str(self.output), "--output", str(target)],
                                     capture_output=True, text=True, timeout=60)
            self.assertEqual(0, checked.returncode, checked.stdout + checked.stderr)
            programs = json.loads((target / "draft-conversion-plan.json").read_text())["programs"]
            self.assertEqual([expected], [program["status"] for program in programs], programs)

        gate("eligible", "valid")
        for index, damage in enumerate(("mapset", "field", "literal", "omitted-use")):
            with self.subTest(damage=damage):
                forged = copy.deepcopy(document)
                contract = next(node["runtimeContract"] for node in forged["dependencies"]
                                if node.get("dependencyKind") == "PLATFORM_COPY")
                if damage == "mapset":
                    contract["usages"][0]["fieldBinding"]["mapset"] = "OTHER"
                elif damage == "field":
                    contract["usages"][0]["fieldBinding"]["field"] = "OTHER"
                elif damage == "literal":
                    contract["usages"][3]["sourceText"] = "MOVE 'B' TO FIELD1A"
                else:
                    contract["usages"].pop()
                self.export.write_text(json.dumps(forged))
                evidence.write_json(report, {**bound, "exportSha256": evidence.digest(self.export)})
                self.assertFalse(evidence.evidence_current(self.source, self.relative, self.output))
                gate("blocked", index)
        original = self.program.read_text()
        for value in ("DFHGREEN", "'B'"):
            self.program.write_text(original.replace("MOVE DFHBMASK TO FIELD1A", f"MOVE {value} TO FIELD1A"))
            evidence.stage(self.program, staging / self.relative)
            evidence.stage(self.program, staging / ".parser-input" / self.relative / self.program.name)
            rejected = subprocess.run(result.args, capture_output=True, text=True, timeout=120)
            self.assertNotEqual(0, rejected.returncode)
            diagnostic = json.loads(self.export.read_text())["dependencyDiagnostics"]
            self.assertTrue(any("platform-contract-unsupported-bms-value:" in issue for issue in diagnostic), diagnostic)

    def test_bms_source_uses_include_hash_bound_procedure_copybooks_with_tag_replacing(self):
        import tempfile
        from pathlib import Path
        root = Path(tempfile.mkdtemp())
        layout = root / "MAP.json"
        layout.write_text(json.dumps({"maps": [{"fields": [{"name": "STATUS"}]}]}))
        book = root / "SETATTR.cpy"
        book.write_text("           IF (VAR1) = SPACES\n               MOVE DFHRED TO (SCRN)C OF MAPO\n           END-IF.\n")
        data = root / "DATA.cpy"
        data.write_text("       01 DATA-ONLY.\n           05 OTHERC PIC X.\n")
        source = ("000100 DATA DIVISION.                                                   00010000\n"
                  "000200     COPY DATA.                                                   00020000\n"
                  "000300 PROCEDURE DIVISION.                                              00030000\n"
                  "000400     COPY SETATTR REPLACING                                       00040000\n"
                  "000500       ==(VAR1)== BY ==WS-STATUS==                                00050000\n"
                  "000600       ==(SCRN)== BY ==STATUS== .                                 00060000\n")
        def definitions(**overrides):
            items = [{"kind": "BMS_LAYOUT", "name": "MAP", "path": str(layout), "sha256": evidence.digest(layout)},
                     {"kind": "COPY", "name": "SETATTR.cpy", "path": str(book), "sha256": evidence.digest(book)},
                     {"kind": "COPY", "name": "DATA.cpy", "path": str(data), "sha256": evidence.digest(data)}]
            return [{**item, **overrides.get(item["name"], {})} for item in items]
        binding = {"usages": [{"sourceText": "MOVE DFHRED TO STATUSC OF MAPO"}]}
        self.assertTrue(contracts.bms_source_uses_bound(binding, source, definitions()))
        self.assertFalse(contracts.bms_source_uses_bound({"usages": []}, source, definitions()))
        self.assertFalse(contracts.bms_source_uses_bound(binding, source, definitions(**{"SETATTR.cpy": {"sha256": "0" * 64}})))
        self.assertFalse(contracts.bms_source_uses_bound(
            binding, source, [item for item in definitions() if item["name"] != "SETATTR.cpy"]))
        for unsupported in ("COPY SETATTR OF LIB REPLACING", "COPY SETATTR REPLACING ==(VAR1) X== BY ==Y== ==(Z)== BY"):
            self.assertFalse(contracts.bms_source_uses_bound(
                binding, source.replace("COPY SETATTR REPLACING", unsupported), definitions()))

    @unittest.skipUnless(os.environ.get("REKT_NATIVE_PLATFORM_IMAGE"), "native candidate image not selected")
    def test_native_symbolic_predicates_and_unsupported_aggregate_writes_and_comparisons(self):
        configured, profile, staging, _ = self.fixture()
        prefix, _ = self.program.read_text().split("       PROCEDURE DIVISION.", 1)
        prefix += "       01 WS-FLAG PIC X.\n"
        cases = [
            ("           IF EIBAID = DFHENTER CONTINUE END-IF.\n", True),
            ("           IF EIBAID = DFHPF12 CONTINUE END-IF.\n", True),
            ("           IF EIBAID = DFHENTER AND WS-FLAG NOT = 'Y'\n"
             "               CONTINUE END-IF.\n", True),
            ("           IF EIBAID = DFHENTER OR WS-FLAG = 'Y'\n"
             "               CONTINUE END-IF.\n", False),
            ("           IF WS-FLAG = 'Y' AND DFHPF3 CONTINUE END-IF.\n", False),
            ("           IF EIBAID = DFHENTER AND WS-FLAG CONTINUE END-IF.\n", False),
            ("           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3 CONTINUE END-IF.\n", True),
            ("           IF (EIBAID = DFHENTER)\n"
             "               OR (EIBAID = DFHPF3)\n"
             "               CONTINUE END-IF.\n", True),
            ("           IF EIBAID = DFHAID CONTINUE END-IF.\n", False),
            ("           IF EIBAID = DFHAID OR DFHPF12\n"
             "               CONTINUE END-IF.\n", False),
            ("           MOVE DFHENTER TO EIBAID.\n", False),
            ("           IF DFHPF1 = DFHPF2 OR EIBAID = DFHPF3 CONTINUE END-IF.\n", False),
        ]
        for index, (body, supported) in enumerate(cases):
            with self.subTest(body=body):
                self.program.write_text(prefix + "       PROCEDURE DIVISION.\n" + body + "           GOBACK.\n")
                evidence.stage(self.program, staging / self.relative)
                evidence.stage(self.program, staging / ".parser-input" / self.relative / self.program.name)
                native = self.output / f"native-{index}.json"
                result = subprocess.run([
                    "docker", "run", "--rm", "--network", "none", "--entrypoint", "java",
                    "-v", f"{self.source}:/source:ro", "-v", f"{self.output}:/output",
                    "-v", f"{configured}:{configured}:ro", "-e", f"REKT_PLATFORM_CONTRACTS={configured}",
                    os.environ["REKT_NATIVE_PLATFORM_IMAGE"], "-jar", "/app/smojol-cli.jar",
                    "dependency", "PROG.cbl", "--srcDir=/source/.rekt-staging/.parser-input/estate/PROG.cbl",
                    "--copyBooksDir=/source/.rekt-staging", "--dialectJarPath=/app/dialect-idms.jar",
                    f"--export=/output/{native.name}",
                ], capture_output=True, text=True, timeout=120)
                (self.output / f"native-{index}.log").write_text(result.stdout + result.stderr)
                self.assertEqual(supported, result.returncode == 0, (result.stdout + result.stderr)[-4000:])
                document = json.loads(native.read_text())
                leaf = next(node for node in document["dependencies"] if node.get("dependencyKind") == "PLATFORM_COPY")
                self.assertEqual(supported, evidence.native_platform_contract_bound(leaf, profile))
                if supported:
                    self.assertTrue(leaf["runtimeContract"]["usages"])
                    self.export.write_bytes(native.read_bytes())
                    with patch.dict(os.environ, {"REKT_PLATFORM_CONTRACTS": str(configured)}):
                        produced = self.build(staging=staging)
                    self.assertEqual([], produced["unresolvedDiagnostics"])
                    evidence.write_json(self.output / (self.relative + ".report/dependency-evidence.json"), produced)
                    gate = os.environ.get("REKT_PLATFORM_GATE_ASSEMBLY")
                    if gate:
                        target = self.work / f"strict-plan-{index}"
                        checked = subprocess.run([
                            "dotnet", gate, "dependency-plan", "--source", str(self.source),
                            "--facts", str(self.output), "--output", str(target),
                        ], capture_output=True, text=True, timeout=60)
                        self.assertEqual(0, checked.returncode, checked.stdout + checked.stderr)
                        plans = json.loads((target / "draft-conversion-plan.json").read_text())["programs"]
                        self.assertEqual(["eligible"], [program["status"] for program in plans], plans)
                else:
                    self.assertTrue(any("platform-contract-" in issue or "UNRESOLVED_CICS_AID_SEMANTIC:" in issue
                                        for issue in document["dependencyDiagnostics"]))
                    if "EIBAID = DFHAID" in body:
                        self.assertEqual(1, sum(issue.startswith("UNRESOLVED_CICS_AID_SEMANTIC:")
                                                for issue in document["dependencyDiagnostics"]))
                    self.assertNotIn("incomplete-callee:DFHAID", document["dependencyDiagnostics"])
