#!/usr/bin/env python3
import copy
import json
import os
import shutil
import subprocess
import sys
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "tools"))
import rekt_dependency_evidence as evidence


class DependencyFixture(unittest.TestCase):
    def setUp(self):
        self.work = REPO / "tools/tests/_work" / self._testMethodName
        self.work.mkdir(parents=True)
        self.source = self.work / "source"
        self.output = self.work / "output"
        self.source.mkdir()
        self.output.mkdir()
        self.relative = "estate/PROG.cbl"
        self.program = self.write("estate/PROG.cbl", "       COPY REAL.\n           CALL 'SUBPROG'.\n")
        self.definition = self.write("estate/copy/REAL.cpy", "       01 REAL-FIELD PIC X.\n")
        self.export = self.output / (self.relative + "-deps.json")
        self.export.parent.mkdir(parents=True)
        self.export.write_text(json.dumps({
            "callTarget": {"name": "PROG.cbl", "programReferenceType": "STATIC"},
            "dependencies": [{"name": "REAL.cpy"}, {"callTarget": {"name": "SUBPROG"}, "dependencies": []}],
        }))

    def tearDown(self):
        # Keep tools/tests/_work: Docker Desktop can miss bind mounts under a recreated parent.
        shutil.rmtree(self.work)

    def write(self, relative, text):
        path = self.source / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="latin-1")
        return path

    def build(self, exit_code=0, staging=None):
        return evidence.build_evidence(self.source, self.relative, self.output, exit_code, staging)

    def native_export(self):
        document = json.loads(self.export.read_text())
        document.update({
            "dependencyExportSchemaVersion": 1, "sourcePath": str(self.program),
            "sourceSha256": evidence.digest(self.program),
            "dependencyCoverage": {**dict.fromkeys(evidence.KINDS, "unavailable"), "COPY": "complete", "CALL": "complete"},
            "dependencyDiagnostics": [],
        })
        callee = self.write("estate/SUBPROG.cbl", "           GOBACK.\n")
        document["dependencies"] = [
            {"callTarget": {"name": "REAL.cpy", "programReferenceType": "STATIC"}, "dependencyKind": "COPY",
             "sourcePath": str(self.definition), "sourceSha256": evidence.digest(self.definition), "dependencies": []},
            {"callTarget": {"name": "SUBPROG", "programReferenceType": "STATIC"}, "dependencyKind": "CALL",
             "sourcePath": str(callee), "sourceSha256": evidence.digest(callee), "dependencies": [],
             "dependencyAnalysisComplete": True, "dependencyCoverage": dict.fromkeys(evidence.KINDS, "complete"),
             "dependencyAbsenceEvidence": dict(evidence.ABSENCE_PROOFS), "dependencyDiagnostics": []},
        ]
        self.export.write_text(json.dumps(document))
        return document


class DependencyEvidenceTests(DependencyFixture):
    def test_cics_program_attributes_accept_only_explicit_or_default_no(self):
        self.assertTrue(evidence.cics_program_attributes_valid({"language": "COBOL", "dynamic": "NO"}))
        self.assertTrue(evidence.cics_program_attributes_valid(
            {"language": "COBOL", "dynamic": "NO", "dynamicProvenance": "cics-default"}))
        for attributes in ({"language": "COBOL", "dynamic": "YES"},
                           {"language": "COBOL", "dynamic": "MAYBE"},
                           {"language": "COBOL"},
                           {"language": "COBOL", "dynamic": "YES", "dynamicProvenance": "cics-default"},
                           {"language": "COBOL", "dynamic": "NO", "dynamicProvenance": "guessed"},
                           {"language": "COBOL", "dynamic": "NO", "dynamicProvenance": "cics-default", "extra": 1}):
            self.assertFalse(evidence.cics_program_attributes_valid(attributes), attributes)

    def test_handle_abend_cancel_binds_without_a_label_but_not_reset_or_fabricated_resources(self):
        statement = {
            "kind": "EXEC_CICS", "operation": "HANDLE", "grammarRule": "Cics_handleContext",
            "sourceText": "HANDLE ABEND CANCEL", "bindingStatus": "complete",
            "bindingBasis": "parsed-cics-control-binding", "resourceReferences": [], "resourceBindings": [],
            "syntax": {"rule": "Cics_handleContext", "children": [
                {"token": "HANDLE"}, {"rule": "Cics_handle_abendContext", "children": [
                    {"token": "ABEND"}, {"token": "CANCEL"}]}]},
        }
        self.assertTrue(evidence.native_cics_statement_bound(statement))
        damaged = copy.deepcopy(statement)
        damaged["syntax"]["children"][1]["children"][1]["token"] = "RESET"
        damaged["sourceText"] = "HANDLE ABEND RESET"
        self.assertFalse(evidence.native_cics_statement_bound(damaged))
        statement["resourceBindings"] = [{"kind": "CICS_LABEL", "name": "INVENTED"}]
        self.assertFalse(evidence.native_cics_statement_bound(statement))

    def cics_return_export(self):
        self.program.write_text("       COPY REAL.\n           CALL 'SUBPROG'.\n"
                                "           EXEC CICS RETURN END-EXEC.\n")
        document = self.native_export()
        document.update(
            dependencyAnalysisComplete=True,
            dependencyCoverage=dict.fromkeys(evidence.KINDS, "complete"),
            dependencyAbsenceEvidence={"EXEC_SQL": "antlr-tree:no-sql-contexts",
                                       "FILE_IO": evidence.ABSENCE_PROOFS["FILE_IO"]},
            dependencyPresenceEvidence={"EXEC_CICS": "antlr-tree:resource-free-cics-return"},
            dependencyDialectStatements=[{
                "kind": "EXEC_CICS", "operation": "RETURN", "grammarRule": "Cics_returnContext",
                "sourceText": "RETURN", "bindingStatus": "complete", "runtimeMapping": "unverified",
                "syntax": {"rule": "Cics_returnContext", "children": [{"token": "RETURN"}]},
            }],
        )
        return document

    def test_positive_native_return_evidence_is_not_an_absence_or_runtime_mapping_claim(self):
        document = self.cics_return_export()
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertEqual([], result["unresolvedDiagnostics"])
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), result["coverage"])
        self.assertEqual("unverified", document["dependencyDialectStatements"][0]["runtimeMapping"])
        evidence.write_json(self.output / (self.relative + ".report/dependency-evidence.json"), result)
        self.assertTrue(evidence.evidence_current(self.source, self.relative, self.output))

    def test_original_operations_removed_by_preprocessing_block_evidence_but_literals_and_debug_lines_do_not(self):
        for extra, blocked in [
            ("           EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n", True),
            ("           DISPLAY 'EXEC CICS DELAY'.\n", False),
            ("           DISPLAY 'Example\n      -    ' EXEC CICS DELAY'.\n", False),
            ("      D    EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n", False),
            ("       SOURCE-COMPUTER. IBM WITH DEBUGGING MODE.\n"
             "      D    EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n", True),
        ]:
            with self.subTest(extra=extra):
                document = self.cics_return_export()
                self.program.write_text(self.program.read_text() + extra)
                document["sourceSha256"] = evidence.digest(self.program)
                self.export.write_text(json.dumps(document))
                result = self.build()
                if blocked:
                    self.assertIn("original-operation-not-exported:EXEC_CICS:DELAY:1", result["unresolvedDiagnostics"])
                    self.assertTrue(all(value == "partial" for value in result["coverage"].values()))
                    result["coverage"] = document["dependencyCoverage"]
                    result["unresolvedDiagnostics"] = []
                    evidence.write_json(self.output / (self.relative + ".report") / "dependency-evidence.json", result)
                    self.assertFalse(evidence.evidence_current(self.source, self.relative, self.output))
                else:
                    self.assertEqual([], result["unresolvedDiagnostics"])

    def test_missing_symbolic_cics_expressions_reject_production_and_old_cache(self):
        document = self.cics_return_export()
        self.program.write_text(self.program.read_text()
                                + "           IF STATUS = DFHVALUE(ABEND) CONTINUE END-IF.\n"
                                + "           IF STATUS = DFHRESP(NORMAL) CONTINUE END-IF.\n")
        document["sourceSha256"] = evidence.digest(self.program)
        self.export.write_text(json.dumps(document))
        result = self.build()
        for operation in ("DFHVALUE", "DFHRESP"):
            self.assertIn(f"original-operation-not-exported:EXEC_CICS:{operation}:1",
                          result["unresolvedDiagnostics"])
        result["coverage"] = document["dependencyCoverage"]
        result["unresolvedDiagnostics"] = []
        evidence.write_json(self.output / (self.relative + ".report/dependency-evidence.json"), result)
        self.assertFalse(evidence.evidence_current(self.source, self.relative, self.output))

    def test_native_return_presence_cannot_approve_an_optioned_return_or_malformed_syntax(self):
        for change in ({"sourceText": "RETURN TRANSID('NEXT')"}, {"syntax": None},
                       {"syntax": {"rule": "Cics_returnContext", "children": [
                           {"token": "RETURN"}, {"token": "TRANSID"}]}}):
            with self.subTest(change=change):
                document = self.cics_return_export()
                document["dependencyDialectStatements"][0].update(change)
                self.export.write_text(json.dumps(document))
                result = self.build()
                self.assertNotEqual("complete", result["coverage"]["EXEC_CICS"])
                self.assertIn("native-absence-proof-missing:EXEC_CICS", result["unresolvedDiagnostics"])

    def test_native_dialect_presence_contradicts_a_legacy_no_dialect_attestation(self):
        document = self.cics_return_export()
        document["dependencyAbsenceEvidence"] = dict(evidence.ABSENCE_PROOFS)
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertNotEqual("complete", result["coverage"]["EXEC_CICS"])
        self.assertIn("native-absence-proof-missing:EXEC_CICS", result["unresolvedDiagnostics"])

    def bound_cics_file_export(self):
        document = self.cics_return_export()
        catalog = self.write("cics/resources.yaml", "resourceDefinitions:\n"
                             "  - file:\n      name: TESTFILE\n      dsname: TEST.DATA\n"
                             "      recordsize: 80\n      keylength: 8\n")
        document["dependencyDialectStatements"].insert(0, {
            "kind": "EXEC_CICS", "operation": "WRITE", "grammarRule": "Cics_writeContext",
            "sourceText": "WRITE FILE('TESTFILE') FROM(REAL-FIELD)",
            "bindingStatus": "complete", "runtimeMapping": "unverified",
            "resourceBindings": [{
                "kind": "CICS_FILE", "name": "TESTFILE", "sourcePath": str(catalog),
                "sourceSha256": evidence.digest(catalog),
                "attributes": {"dsname": "TEST.DATA", "recordsize": 80, "keylength": 8},
            }],
            "syntax": {"rule": "Cics_writeContext", "children": [
                {"token": "WRITE"},
                {"rule": "Cics_write_fileContext", "children": [
                    {"rule": "Cics_file_nameContext", "children": [
                        {"token": "FILE"}, {"rule": "Cics_nameContext", "children": [
                            {"token": "("}, {"rule": "NameContext", "children": [{"token": "'TESTFILE'"}]},
                            {"token": ")"}]}]}]}]},
        })
        document["dependencyPresenceEvidence"] = {
            "EXEC_CICS": "antlr-tree:bound-cics-resources",
            "FILE_IO": "antlr-tree:bound-cics-file-resources",
        }
        del document["dependencyAbsenceEvidence"]["FILE_IO"]
        return document, catalog

    def test_bound_cics_file_is_positive_file_io_evidence_and_rechecks_its_real_definition(self):
        document, catalog = self.bound_cics_file_export()
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertEqual([], result["unresolvedDiagnostics"])
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), result["coverage"])
        self.assertTrue(any(definition["kind"] == "CICS_FILE" and definition["path"] == str(catalog)
                            for definition in result["definitions"]))
        path = self.output / (self.relative + ".report/dependency-evidence.json")
        evidence.write_json(path, result)
        self.assertTrue(evidence.evidence_current(self.source, self.relative, self.output))
        result["definitions"] = [item for item in result["definitions"] if item["kind"] != "CICS_FILE"]
        evidence.write_json(path, result)
        self.assertFalse(evidence.evidence_current(self.source, self.relative, self.output))
        catalog.write_text("changed")
        self.assertTrue(any("native CICS resource definition identity/hash mismatch" in issue
                            for issue in self.build()["unresolvedDiagnostics"]))

    def test_bound_cics_file_cannot_claim_absence_or_use_the_wrong_resource_or_remote_sysid(self):
        for change in ("absence", "wrong-name", "missing-binding", "sysid", "malformed-syntax"):
            with self.subTest(change=change):
                document, _ = self.bound_cics_file_export()
                statement = document["dependencyDialectStatements"][0]
                if change == "absence":
                    document["dependencyAbsenceEvidence"]["FILE_IO"] = evidence.ABSENCE_PROOFS["FILE_IO"]
                elif change == "wrong-name":
                    statement["resourceBindings"][0]["name"] = "OTHER"
                elif change == "missing-binding":
                    statement["resourceBindings"] = []
                elif change == "sysid":
                    statement["syntax"]["children"].append({"token": "SYSID"})
                else:
                    statement["syntax"]["children"] = None
                self.assertFalse(evidence.native_domain_evidence(document, "FILE_IO"))

    def native_file_control_export(self):
        document = self.native_export()
        token = lambda text: {"token": text}
        file_name = lambda name: {"rule": "FileNameContext", "children": [
            {"rule": "CobolWordContext", "children": [token(name)]}]}
        binding = {
            "kind": "COBOL_FILE", "name": "IN-FILE", "assignment": "INFILE", "organization": "SEQUENTIAL",
            "accessMode": "SEQUENTIAL", "records": ["IN-REC"], "definitionScope": "expanded-parser-input",
            "runtimeMapping": "unverified",
            "definitionSyntax": {"rule": "FileControlEntryContext", "children": [
                {"rule": "SelectClauseContext", "children": [token("SELECT"), file_name("IN-FILE")]}]},
            "descriptionSyntax": {"rule": "FileDescriptionEntryClausesContext", "children": [
                token("FD"), {"rule": "CobolWordContext", "children": [token("IN-FILE")]}]},
        }
        document["dependencyDialectStatements"] = [{
            "kind": "FILE_IO", "operation": "READ", "grammarRule": "ReadStatementContext",
            "sourceText": "READ IN-FILE", "bindingStatus": "complete", "runtimeMapping": "unverified",
            "fileOperands": ["IN-FILE"], "resourceBindings": [binding],
            "syntax": {"rule": "ReadStatementContext", "children": [token("READ"), file_name("IN-FILE")]},
        }]
        document["dependencyCoverage"] = dict.fromkeys(evidence.KINDS, "complete")
        document["dependencyAnalysisComplete"] = True
        document["dependencyAbsenceEvidence"] = {"EXEC_SQL": "antlr-tree:no-sql-contexts",
                                                 "EXEC_CICS": "antlr-tree:no-cics-contexts"}
        document["dependencyPresenceEvidence"] = {"FILE_IO": "antlr-tree:bound-file-control-resources"}
        return document

    def test_native_select_and_fd_binding_is_positive_file_io_evidence(self):
        self.export.write_text(json.dumps(self.native_file_control_export()))
        result = self.build()
        self.assertEqual([], result["unresolvedDiagnostics"])
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), result["coverage"])

    def test_native_file_binding_rejects_unbound_or_mismatched_declarations(self):
        for change in ("absence", "wrong-select", "unknown-operand", "no-binding", "unresolved", "cics-file"):
            with self.subTest(change=change):
                document = self.native_file_control_export()
                statement = document["dependencyDialectStatements"][0]
                binding = statement["resourceBindings"][0]
                if change == "absence":
                    document["dependencyAbsenceEvidence"]["FILE_IO"] = evidence.ABSENCE_PROOFS["FILE_IO"]
                elif change == "wrong-select":
                    binding["definitionSyntax"]["children"][0]["children"][1]["children"][0]["children"] = [
                        {"token": "OTHER-FILE"}]
                elif change == "unknown-operand":
                    statement["fileOperands"] = ["OTHER-FILE"]
                elif change == "no-binding":
                    statement["resourceBindings"] = []
                elif change == "unresolved":
                    statement["bindingStatus"] = "unresolved"
                else:
                    document["dependencyPresenceEvidence"]["FILE_IO"] = "antlr-tree:bound-cics-file-resources"
                self.assertFalse(evidence.native_domain_evidence(document, "FILE_IO"))

    def test_incomplete_cics_does_not_misreport_a_valid_sql_absence_proof_as_missing(self):
        document = self.cics_return_export()
        document["dependencyAnalysisComplete"] = False
        document["dependencyCoverage"]["EXEC_CICS"] = "partial"
        document["dependencyDiagnostics"] = ["cics-file-definition-not-found:TESTFILE"]
        self.export.write_text(json.dumps(document))
        result = self.build(1)
        self.assertNotIn("native-absence-proof-missing:EXEC_SQL", result["unresolvedDiagnostics"])
        self.assertIn("native:cics-file-definition-not-found:TESTFILE", result["unresolvedDiagnostics"])
        self.assertNotEqual("complete", result["coverage"]["EXEC_SQL"])

    def test_successful_export_binds_original_and_actual_export_without_claiming_full_coverage(self):
        result = self.build()
        self.assertEqual(evidence.digest(self.program), result["originalSourceSha256"])
        self.assertEqual(str(self.program.resolve()), result["originalSourcePath"])
        self.assertEqual(self.relative, result["relativePath"])
        self.assertEqual(evidence.digest(self.export), result["exportSha256"])
        self.assertEqual([], result["definitions"])
        self.assertIn("definition-resolution-not-exported:REAL", result["unresolvedDiagnostics"])
        self.assertEqual("partial", result["coverage"]["COPY"])
        self.assertEqual("partial", result["coverage"]["CALL"])
        self.assertEqual("unavailable", result["coverage"]["EXEC_SQL"])
        self.assertIn("exporter-coverage-not-declared:COPY,CALL", result["unresolvedDiagnostics"])

    def test_empty_copy_export_cannot_contradict_original_or_be_filled_by_source_scanner(self):
        exported = json.loads(self.export.read_text())
        exported["dependencies"] = []
        self.export.write_text(json.dumps(exported))
        result = self.build()
        self.assertIn("empty-COPY-export-contradiction", result["unresolvedDiagnostics"])
        self.assertIn("copy-not-exported:REAL", result["unresolvedDiagnostics"])
        self.assertEqual([], result["definitions"])
        self.assertEqual([], json.loads(self.export.read_text())["dependencies"])

    def test_failed_and_malformed_exports_retain_truthful_evidence(self):
        self.export.unlink()
        result = self.build(17)
        self.assertEqual(17, result["exportExitCode"])
        self.assertIsNone(result["exportSha256"])
        self.assertIn("dependency-export-failed:17", result["unresolvedDiagnostics"])
        self.assertEqual("unavailable", result["coverage"]["COPY"])
        self.export.write_text('{"dependencies":[]}')
        result = self.build()
        self.assertTrue(any("identity-mismatch" in item for item in result["unresolvedDiagnostics"]))

    def test_unresolved_nested_copybook_and_parser_diagnostics_are_preserved(self):
        self.definition.write_text("       COPY ABSENT.\n")
        report = self.output / (self.relative + ".report")
        report.mkdir(parents=True)
        (report / "parse-diagnostics.json").write_text(json.dumps([
            {"code": "missing copybook", "message": "ABSENT definition required"},
        ]))
        result = self.build()
        self.assertIn("nested-copy-not-exported:ABSENT", result["unresolvedDiagnostics"])
        self.assertIn("unresolved-copybook:ABSENT", result["unresolvedDiagnostics"])
        self.assertTrue(any("ABSENT definition required" in item for item in result["unresolvedDiagnostics"]))
        self.assertFalse(any(path.stem == "ABSENT" for path in self.source.rglob("*.cpy")))

    def test_copy_name_length_info_is_dropped_only_for_a_bound_copybook(self):
        note = "Copybook declaration has more than 8 characters for: {}"
        document = self.native_export()
        document["dependencyDiagnostics"] = ["parser-diagnostic::" + note.format("REAL"),
                                             "parser-diagnostic::" + note.format("ABSENT")]
        self.export.write_text(json.dumps(document))
        report = self.output / (self.relative + ".report")
        report.mkdir(parents=True)
        (report / "parse-diagnostics.json").write_text(json.dumps([
            {"severity": "INFO", "message": note.format("REAL")},
            {"severity": "INFO", "message": note.format("ABSENT")},
            {"severity": "WARNING", "message": note.format("REAL").replace("REAL", "REAL ")},
        ]))
        bound = self.build()
        self.assertIn("REAL", {item["name"] for item in bound["definitions"] if item["kind"] == "COPY"})
        issues = bound["unresolvedDiagnostics"]
        self.assertNotIn("parser-diagnostic:None:" + note.format("REAL"), issues)
        self.assertNotIn("native:parser-diagnostic::" + note.format("REAL"), issues)
        self.assertIn("parser-diagnostic:None:" + note.format("ABSENT"), issues)
        self.assertIn("native:parser-diagnostic::" + note.format("ABSENT"), issues)
        self.assertIn("parser-diagnostic:None:" + note.format("REAL "), issues)

        self.definition.unlink()
        unbound = self.build()["unresolvedDiagnostics"]
        self.assertIn("parser-diagnostic:None:" + note.format("REAL"), unbound)
        self.assertIn("native:parser-diagnostic::" + note.format("REAL"), unbound)

    def test_placeholder_and_unproven_generated_definitions_are_not_resolved(self):
        self.definition.write_text("      * AUTO-GENERATED STUB COPYBOOK\n       01 REAL-STUB PIC X.\n")
        result = self.build()
        self.assertTrue(result["placeholderContaminated"])
        self.assertEqual([], result["definitions"])
        self.definition.unlink()
        self.write("copy-generated/REAL.cpy", "       01 REAL-FIELD PIC X.\n")
        result = self.build()
        self.assertFalse(result["placeholderContaminated"])  # Unknown provenance, not a proven stub.
        self.assertIn("unverified-copybook:REAL", result["unresolvedDiagnostics"])
        self.assertEqual([], result["definitions"])

    def test_explicit_stub_in_parser_artifact_contaminates_otherwise_clean_inputs(self):
        report = self.output / (self.relative + ".report")
        report.mkdir(parents=True)
        (report / "data.json").write_text('{"name":"DFHAID-STUB"}')
        self.assertTrue(self.build()["placeholderContaminated"])

    def test_stale_preprocessed_shadow_cannot_override_current_original(self):
        transformed = self.source / ".preprocessed/REAL.cpy"
        transformed.parent.mkdir()
        transformed.write_text("       01 OLD-FIELD PIC 9.\n")
        evidence.provenance(self.definition, transformed)
        self.definition.write_text("       01 CURRENT-FIELD PIC X(2).\n")
        staged = self.work / "staging/REAL.cpy"
        evidence.stage(self.definition, staged, transformed)
        self.assertEqual(self.definition.read_bytes(), staged.read_bytes())

    def test_wrong_original_source_identity_cannot_reuse_same_named_transformation(self):
        other = self.write("other/PROG.cbl", "           DISPLAY 'OTHER'.\n")
        transformed = self.source / ".preprocessed/PROG.cbl"
        transformed.parent.mkdir()
        transformed.write_text("           DISPLAY 'TRANSFORMED OTHER'.\n")
        evidence.provenance(other, transformed)
        staged = self.work / "staging/PROG.cbl"
        evidence.stage(self.program, staged, transformed)
        self.assertEqual(self.program.read_bytes(), staged.read_bytes())

    def test_staged_copy_hash_must_match_original_and_actual_parser_input(self):
        staging = self.work / "staging"
        evidence.stage(self.program, staging / self.relative)
        evidence.stage(self.definition, staging / self.definition.name)
        (staging / self.definition.name).write_text("       01 STALE-FIELD PIC X.\n")
        result = self.build(staging=staging)
        self.assertIn("unverified-staged-copybook:REAL", result["unresolvedDiagnostics"])
        self.assertEqual([], result["definitions"])

    def test_old_complete_claim_cannot_hide_changed_definition_or_empty_copy_export(self):
        self.native_export()
        result = self.build()
        # Simulate legacy persisted claims, not a new producer assertion of completeness.
        result["coverage"] = dict.fromkeys(evidence.KINDS, "complete")
        result["unresolvedDiagnostics"] = []
        path = self.output / (self.relative + ".report/dependency-evidence.json")
        evidence.write_json(path, result)
        self.definition.write_text("       01 CHANGED PIC X.\n")
        self.assertFalse(evidence.evidence_current(self.source, self.relative, self.output))
        result["definitions"][0]["sha256"] = evidence.digest(self.definition)
        export = json.loads(self.export.read_text())
        export["dependencies"] = []
        self.export.write_text(json.dumps(export))
        result["exportSha256"] = evidence.digest(self.export)
        evidence.write_json(path, result)
        self.assertFalse(evidence.evidence_current(self.source, self.relative, self.output))

    def test_native_parser_coverage_requires_actual_resolved_input_hashes(self):
        self.native_export()
        result = self.build()
        self.assertEqual([], result["unresolvedDiagnostics"])
        self.assertEqual("complete", result["coverage"]["COPY"])
        self.assertEqual("complete", result["coverage"]["CALL"])
        self.assertEqual("unavailable", result["coverage"]["EXEC_CICS"])
        self.assertEqual({"COPY", "CALL"}, {item["kind"] for item in result["definitions"]})
        self.definition.write_text("       01 CHANGED-SINCE-PARSE PIC X.\n")
        result = self.build()
        self.assertEqual("partial", result["coverage"]["COPY"])
        self.assertTrue(any("native resolved input hash mismatch" in item for item in result["unresolvedDiagnostics"]))
        self.assertFalse(any(item["kind"] == "COPY" for item in result["definitions"]))

    def test_failed_native_export_retains_hash_bound_positive_evidence_without_complete_status(self):
        self.native_export()
        result = self.build(1)
        self.assertIn("dependency-export-failed:1", result["unresolvedDiagnostics"])
        self.assertEqual("partial", result["coverage"]["COPY"])
        self.assertEqual({"COPY", "CALL"}, {item["kind"] for item in result["definitions"]})

    def test_native_claim_for_same_named_other_original_is_rejected(self):
        document = self.native_export()
        other = self.write("other/PROG.cbl", self.program.read_text())
        document["sourcePath"] = str(other)
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertEqual("partial", result["coverage"]["COPY"])
        self.assertTrue(any("source identity/hash mismatch" in item for item in result["unresolvedDiagnostics"]))

    def test_native_dynamic_call_stays_unresolved_not_concrete(self):
        document = self.native_export()
        self.program.write_text("       COPY REAL.\n           CALL WS-TARGET.\n")
        document["sourceSha256"] = evidence.digest(self.program)
        document["dependencies"][1] = {
            "callTarget": {"name": "WS-TARGET", "programReferenceType": "DYNAMIC"}, "dependencyKind": "CALL",
            "dependencies": [],
        }
        document["dependencyCoverage"]["CALL"] = "partial"
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertIn("dynamic-call-target-unresolved:WS-TARGET", result["unresolvedDiagnostics"])
        self.assertFalse(any(item["kind"] == "CALL" for item in result["definitions"]))
        self.assertNotEqual("complete", result["coverage"]["CALL"])

    def test_literals_and_literal_comment_markers_are_not_dependency_contradictions(self):
        self.program.write_text(
            "       COPY 'REAL'.\n"
            "           DISPLAY 'COPY ABSENT. CALL WS-FAKE *> not a comment'.\n"
            "           DISPLAY 'It''s COPY WRONG'.\n"
            "           CALL 'SUBPROG'. *> COPY COMMENTED.\n")
        self.native_export()
        result = self.build()
        self.assertEqual([], result["unresolvedDiagnostics"])
        self.assertEqual("complete", result["coverage"]["COPY"])
        self.assertEqual("complete", result["coverage"]["CALL"])

    def test_hyphenated_paragraph_ending_in_call_is_not_a_dynamic_call(self):
        self.program.write_text(
            "       COPY REAL.\n"
            "           PERFORM 3200-INSERT-IMS-CALL THRU 3200-EXIT.\n"
            "           CALL 'SUBPROG'.\n")
        self.native_export()
        result = self.build()
        self.assertNotIn("dynamic-CALL-target-unresolved", result["unresolvedDiagnostics"])
        self.assertNotIn("call-not-exported:INSERT-IMS-CALL", result["unresolvedDiagnostics"])
        self.program.write_text("       COPY REAL.\n           CALL WS-TARGET.\n           CALL 'SUBPROG'.\n")
        self.native_export()
        self.assertIn("dynamic-CALL-target-unresolved", self.build()["unresolvedDiagnostics"])

    def test_grounded_bms_hashes_do_not_attest_complete_symbolic_layout(self):
        bms = self.write("bms/REAL.bms", "MAP DFHMSD LANG=COBOL\n")
        evidence.provenance(bms, self.definition, producer="bms-symbolic-map")
        self.native_export()
        result = self.build()
        self.assertIn("partial-derived-bms-layout:REAL.cpy", result["unresolvedDiagnostics"])
        self.assertEqual("partial", result["coverage"]["COPY"])
        self.assertTrue(any(item["kind"] == "BMS" and item["path"] == str(bms)
                            for item in result["definitions"]))

    def test_native_supported_absence_can_complete_all_kinds_and_reuse_current_evidence(self):
        document = self.native_export()
        document["dependencyCoverage"] = dict.fromkeys(evidence.KINDS, "complete")
        document["dependencyAnalysisComplete"] = True
        document["dependencyAbsenceEvidence"] = dict(evidence.ABSENCE_PROOFS)
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertEqual([], result["unresolvedDiagnostics"])
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), result["coverage"])
        evidence.write_json(self.output / (self.relative + ".report/dependency-evidence.json"), result)
        self.assertTrue(evidence.evidence_current(self.source, self.relative, self.output))

    def test_native_unsupported_kind_cannot_be_complete_without_full_tree_absence_attestation(self):
        document = self.native_export()
        document["dependencyCoverage"] = dict.fromkeys(evidence.KINDS, "complete")
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertIn("native-absence-proof-missing:EXEC_CICS", result["unresolvedDiagnostics"])
        self.assertIn("native-absence-proof-missing:EXEC_SQL", result["unresolvedDiagnostics"])
        self.assertIn("native-absence-proof-missing:FILE_IO", result["unresolvedDiagnostics"])
        self.assertFalse(any(status == "complete" for status in result["coverage"].values()))

    def test_old_cached_complete_claim_without_native_absence_proof_is_not_reusable(self):
        document = self.native_export()
        document["dependencyCoverage"] = dict.fromkeys(evidence.KINDS, "complete")
        document["dependencyAnalysisComplete"] = True
        self.export.write_text(json.dumps(document))
        result = self.build()
        result["coverage"] = dict.fromkeys(evidence.KINDS, "complete")
        result["unresolvedDiagnostics"] = []
        evidence.write_json(self.output / (self.relative + ".report/dependency-evidence.json"), result)
        self.assertFalse(evidence.evidence_current(self.source, self.relative, self.output))

    def test_native_present_or_unknown_contexts_remain_partial_with_explicit_diagnostics(self):
        document = self.native_export()
        document["dependencyAnalysisComplete"] = True
        document["dependencyCoverage"].update(dict.fromkeys(evidence.ABSENCE_PROOFS, "partial"))
        document["dependencyDiagnostics"] = ["unsupported-dialect-dependency-contexts:DialectStatementContext",
                                              "unsupported-file-io-dependency-contexts:opaque-dialect"]
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertEqual("partial", result["coverage"]["EXEC_SQL"])
        self.assertEqual("partial", result["coverage"]["EXEC_CICS"])
        self.assertEqual("partial", result["coverage"]["FILE_IO"])
        self.assertTrue(any("DialectStatementContext" in item for item in result["unresolvedDiagnostics"]))

    def test_child_copy_cannot_hide_missing_root_copy_export_or_create_root_call_edges(self):
        document = self.native_export()
        child_copy = document["dependencies"].pop(0)
        child = document["dependencies"][0]
        child["dependencies"] = [child_copy, {
            "callTarget": {"name": "THIRD", "programReferenceType": "STATIC"},
            "dependencyKind": "CALL", "dependencies": [],
        }]
        self.export.write_text(json.dumps(document))
        copies, calls = evidence.exported_names(document, self.program.name)
        self.assertEqual(set(), copies)
        self.assertEqual({"SUBPROG"}, calls)
        result = self.build()
        self.assertIn("copy-not-exported:REAL", result["unresolvedDiagnostics"])
        self.assertIn("empty-COPY-export-contradiction", result["unresolvedDiagnostics"])
        self.assertEqual("partial", result["coverage"]["COPY"])

    def test_native_root_cannot_hide_incomplete_callee_behind_complete_parent_metadata(self):
        document = self.native_export()
        document.update({"dependencyAnalysisComplete": True,
                         "dependencyCoverage": dict.fromkeys(evidence.KINDS, "complete"),
                         "dependencyAbsenceEvidence": dict(evidence.ABSENCE_PROOFS)})
        document["dependencies"][1]["dependencyCoverage"]["FILE_IO"] = "partial"
        document["dependencies"][1]["dependencyAnalysisComplete"] = False
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertIn("native-incomplete-callee:SUBPROG", result["unresolvedDiagnostics"])
        self.assertFalse(any(status == "complete" for status in result["coverage"].values()))

    def test_placeholder_in_supporting_callee_copy_contaminates_root_evidence(self):
        document = self.native_export()
        placeholder = self.write("copy/CHILD.cpy", "      * AUTO-GENERATED STUB COPYBOOK\n       01 CHILD-STUB PIC X.\n")
        document["dependencies"][1]["dependencies"] = [{
            "callTarget": {"name": "CHILD.cpy", "programReferenceType": "STATIC"}, "dependencyKind": "COPY",
            "sourcePath": str(placeholder), "sourceSha256": evidence.digest(placeholder), "dependencies": [],
        }]
        self.export.write_text(json.dumps(document))
        result = self.build()
        self.assertTrue(result["placeholderContaminated"])
        self.assertFalse(any(item["name"] == "CHILD" for item in result["definitions"]))
        self.assertEqual("partial", result["coverage"]["COPY"])
    def test_quarantine_preserves_only_target_identity(self):
        untouched = self.output / "other/PROG.cbl-deps.json"
        untouched.parent.mkdir()
        untouched.write_text("unrelated")
        subprocess.run([sys.executable, str(REPO / "tools/rekt_dependency_evidence.py"), "quarantine",
                        "--output-root", str(self.output), "--relative-path", self.relative], check=True)
        self.assertFalse(self.export.exists())
        self.assertEqual(1, len(list((self.output / ".previous").rglob("PROG.cbl-deps.json"))))
        self.assertEqual("unrelated", untouched.read_text())


class CommareaClosureTests(DependencyFixture):
    LAYOUT = ["01CARDDEMO-COMMAREA.", "05CDEMO-INFO.", "10CDEMO-TO-PROGRAMPICX(8)."]

    def participant(self, name, records=(), sends=(), carriers=(), source=None):
        relative = f"est/{name}.cbl"
        path = self.write(relative, source or "       PROCEDURE DIVISION.\n           GOBACK.\n")
        if records is None:
            return
        inventory = {"schema": 1, "records": list(records), "carriers": list(carriers), "sends": list(sends)}
        (self.output / "est").mkdir(exist_ok=True)
        (self.output / (relative + "-deps.json")).write_text(json.dumps(
            {"sourceSha256": name.ljust(64, "0"), "commareaInventory": inventory}))
        report = self.output / (relative + ".report")
        report.mkdir(parents=True, exist_ok=True)
        (report / "dependency-evidence.json").write_text(json.dumps({"originalSourceSha256": evidence.digest(path)}))

    def record(self, values, entry, loaded=None, layout=None):
        return {"name": "CARDDEMO-COMMAREA", "layout": layout or self.LAYOUT, "loaded": entry if loaded is None else loaded,
                "issues": [], "slots": {"CDEMO-TO-PROGRAM": {"length": 8, "values": values, "entry": entry,
                                                             "copies": [], "unsupported": []}}}

    def send(self, kind="record", **extra):
        return {"command": "XCTL", "operand": "CARDDEMO-COMMAREA", "line": 1, "column": 0, "kind": kind,
                "record": "CARDDEMO-COMMAREA", **extra}

    def test_closure_unions_every_sender_into_each_loader(self):
        self.participant("SIGNON", [self.record([None, "MENU"], False)], [self.send()])
        self.participant("MENU", [self.record([None, "LIST"], True)], [self.send()])
        self.participant("LIST", [self.record([None], True, layout=self.LAYOUT + ["05EXTRAPICX(4)."])],
                         [self.send("carrier", carrier="WS-COMMAREA")],
                         [{"name": "WS-COMMAREA", "record": "CARDDEMO-COMMAREA", "issues": []}])
        self.participant("MQREAD", None, source="       PROCEDURE DIVISION.\n           CALL 'MQGET'.\n")
        closure = evidence.commarea_closure(self.source, self.output, "est")
        self.assertEqual([], closure["errors"])
        self.assertEqual({None, "MENU", "LIST"},
                         set(closure["programs"]["LIST".ljust(64, "0")]["slots"]["CDEMO-TO-PROGRAM"]))
        self.assertEqual(["lexical"], [item["mode"] for item in closure["participants"] if "MQREAD" in item["relativePath"]])

    def test_closure_ignores_layouts_linked_only_to_programs_loading_no_record(self):
        fraud = ["01WS-FRAUD-DATA.", "05FRAUD-KEYPICX(8)."]
        link = dict(self.send(command="LINK", targets=["HELPER"], bindingStatus="complete"),
                    operand="WS-FRAUD-DATA", record="WS-FRAUD-DATA")
        self.participant("MENU", [self.record([None, "LIST"], True)], [self.send()])
        self.participant("SENDER", [self.record([None], False), dict(self.record([None], False, layout=fraud),
                                                                    name="WS-FRAUD-DATA")], [self.send(), link])
        self.participant("HELPER", [])
        self.assertEqual([], evidence.commarea_closure(self.source, self.output, "est")["errors"])
        self.participant("HELPER", [self.record([None], True)])
        self.assertTrue(evidence.commarea_closure(self.source, self.output, "est")["errors"])

    def test_closure_refuses_an_open_world(self):
        cases = {
            "unsupported-write": lambda: self.participant(
                "BAD", [dict(self.record([None], False), slots={"CDEMO-TO-PROGRAM": {
                    "length": 8, "values": [None], "entry": False, "copies": [], "unsupported": ["sending-expression"]}})],
                [self.send()]),
            "foreign-to-loader": lambda: self.participant(
                "LINKER", [], [self.send("foreign", operand="WS-OTHER", targets=["MENU"])]),
            "carrier-issue": lambda: self.participant(
                "CARRIER", [self.record([None], False)], [self.send("carrier", carrier="WS-COMMAREA")],
                [{"name": "WS-COMMAREA", "record": "CARDDEMO-COMMAREA", "issues": ["carrier-write:Other"]}]),
            "unparsed-sender": lambda: self.participant(
                "LOST", None, source="       PROCEDURE DIVISION.\n           EXEC CICS XCTL COMMAREA(X) END-EXEC.\n"),
        }
        for name, add in cases.items():
            with self.subTest(name=name):
                shutil.rmtree(self.source / "est", ignore_errors=True)
                shutil.rmtree(self.output / "est", ignore_errors=True)
                self.participant("MENU", [self.record([None, "LIST"], True)], [self.send()])
                add()
                closure = evidence.commarea_closure(self.source, self.output, "est")
                if name == "unsupported-write":
                    self.assertNotIn("CDEMO-TO-PROGRAM", closure["programs"]["MENU".ljust(64, "0")]["slots"])
                else:
                    self.assertTrue(closure["errors"])
                    self.assertEqual({}, closure["programs"])


class DependencySearchViewTests(DependencyFixture):
    def prepare(self, transformed=False):
        document = self.native_export()
        self.callee = self.source / "estate/SUBPROG.cbl"
        self.staging = self.source / ".rekt-staging"
        actual = self.callee
        if transformed:
            actual = self.source / ".preprocessed/SUBPROG.cbl"
            actual.parent.mkdir()
            actual.write_text("           DISPLAY 'NORMALIZED'.\n           GOBACK.\n")
            evidence.provenance(self.callee, actual)
        for source, relative, selected in (
                (self.program, self.relative, None),
                (self.definition, "REAL.cpy", None),
                (self.callee, "estate/SUBPROG.cbl", actual if transformed else None)):
            evidence.stage(source, self.staging / relative, selected)
        isolated = self.staging / ".parser-input" / self.relative / self.program.name
        isolated.parent.mkdir(parents=True)
        shutil.copyfile(self.staging / self.relative, isolated)
        self.view = evidence.dependency_search_view(self.staging, self.relative)
        document.update(sourcePath=str(self.view / self.program.name),
                        dependencyAnalysisComplete=True,
                        dependencyCoverage=dict.fromkeys(evidence.KINDS, "complete"),
                        dependencyAbsenceEvidence=dict(evidence.ABSENCE_PROOFS))
        for node, path in zip(document["dependencies"],
                              (self.staging / "REAL.cpy", self.view / self.callee.name)):
            node.update(sourcePath=str(path), sourceSha256=evidence.digest(path))
        self.export.write_text(json.dumps(document))
        return document, actual

    def build_view(self):
        return evidence.build_evidence(self.source, self.relative, self.output, 0,
                                       self.staging, self.source)

    def test_transformed_callee_binds_current_original_and_persistent_actual_input(self):
        _, transformed = self.prepare(transformed=True)
        result = self.build_view()
        self.assertEqual([], result["unresolvedDiagnostics"])
        bound = {item["path"]: item["sha256"] for item in result["definitions"] if item["kind"] == "CALL"}
        self.assertEqual({str(self.callee): evidence.digest(self.callee),
                          str(transformed): evidence.digest(transformed)}, bound)
        evidence.write_json(self.output / (self.relative + ".report/dependency-evidence.json"), result)
        self.assertTrue(evidence.evidence_current(self.source, self.relative, self.output))
        transformed.write_text("changed actual parser representation")
        self.assertFalse(evidence.evidence_current(self.source, self.relative, self.output))

    def test_view_copy_cannot_change_even_if_native_hash_is_updated(self):
        document, _ = self.prepare()
        callee = self.view / self.callee.name
        callee.write_text("           DISPLAY 'DIFFERENT'.\n")
        document["dependencies"][1]["sourceSha256"] = evidence.digest(callee)
        self.export.write_text(json.dumps(document))
        result = self.build_view()
        self.assertTrue(any("callee search view provenance invalid" in item
                            for item in result["unresolvedDiagnostics"]))
        self.assertFalse(any(item["kind"] == "CALL" for item in result["definitions"]))

    def test_changed_original_callee_is_not_hidden_by_unchanged_view_copy(self):
        self.prepare()
        self.callee.write_text("           DISPLAY 'CHANGED ORIGINAL'.\n")
        result = self.build_view()
        self.assertTrue(any("staged definition provenance invalid" in item
                            for item in result["unresolvedDiagnostics"]))

    def test_same_named_root_with_identical_bytes_cannot_replace_selected_entry(self):
        document, _ = self.prepare()
        other = self.write("other/PROG.cbl", self.program.read_text())
        document["sourcePath"] = str(other)
        self.export.write_text(json.dumps(document))
        result = self.build_view()
        self.assertTrue(any(item.startswith("native-export-source-unverified:")
                            for item in result["unresolvedDiagnostics"]))


class DoctorDependencyExportTests(DependencyFixture):
    def test_dependency_inputs_persist_from_local_config_without_overriding_explicit_shell_values(self):
        script = (REPO / "doctor.sh").read_text()
        settings = script.split("# Optional dependency inputs preserve an explicitly empty shell override.\n", 1)[1]
        settings = settings.split("# A moved port", 1)[0]
        for configured, override, expected in [
            ("", None, "unset"), ("/configured/input", None, "/configured/input"),
            ("/configured/input", "/shell/input", "/shell/input"), ("/configured/input", "", ""),
        ]:
            with self.subTest(configured=configured, override=override):
                environment = dict(os.environ, CONFIGURED_INPUT=configured)
                for key in ("REKT_CICS_RESOURCE_DEFINITIONS", "REKT_DEPENDENCY_PROFILE", "REKT_SQL_DECLARATIONS"):
                    environment.pop(key, None)
                    if override is not None:
                        environment[key] = override
                result = subprocess.run(
                    ["bash", "-c", 'read_local_config_value() { printf "%s" "$CONFIGURED_INPUT"; }\n'
                     + settings + """\nbash -c 'printf "%s\\n" "${REKT_CICS_RESOURCE_DEFINITIONS-unset}" "${REKT_DEPENDENCY_PROFILE-unset}" "${REKT_SQL_DECLARATIONS-unset}"'"""],
                    env=environment, capture_output=True, text=True, check=True)
                self.assertEqual([expected, expected, expected], result.stdout.splitlines())

    def test_estate_profile_replaces_configured_profile_only_for_that_estate(self):
        script = (REPO / "doctor.sh").read_text()
        function = script[script.index("rekt_dependency_profile_for() {"):script.index("\nrekt_source_programs() {")]
        (self.work / "tools/cobol-rekt/contracts/estates").mkdir(parents=True)
        (self.work / "tools/cobol-rekt/contracts/estates/carddemo.json").write_text("{}")
        result = subprocess.run(
            ["bash", "-c", function + "\nfor estate in carddemo bank-of-z .all; do rekt_dependency_profile_for \"$estate\"; done"],
            env=dict(os.environ, REPO_ROOT=str(self.work), REKT_DEPENDENCY_PROFILE="/configured.json"),
            capture_output=True, text=True, check=True)
        self.assertEqual([str(self.work / "tools/cobol-rekt/contracts/estates/carddemo.json"),
                          "/configured.json", "/configured.json"], result.stdout.splitlines())

    def test_estate_cics_resources_use_its_own_csd_files_unless_configured_inside_the_estate(self):
        script = (REPO / "doctor.sh").read_text()
        function = script[script.index("rekt_cics_resources_for() {"):script.index("\nrekt_source_programs() {")]
        for path in ("source/carddemo/csd/CARDDEMO.CSD", "source/carddemo/app-mq/csd/MQ.csd",
                     "source/carddemo/.preprocessed/OLD.csd", "source/bank-of-z/cics/defs.yaml"):
            (self.work / path).parent.mkdir(parents=True, exist_ok=True)
            (self.work / path).write_text("")
        (self.work / "source/other-estate").mkdir(parents=True)
        result = subprocess.run(
            ["bash", "-c", function + "\nfor estate in carddemo bank-of-z other-estate .root; do rekt_cics_resources_for \"$estate\"; done"],
            env=dict(os.environ, REPO_ROOT=str(self.work),
                     REKT_CICS_RESOURCE_DEFINITIONS="/source/bank-of-z/cics/defs.yaml"),
            capture_output=True, text=True, check=True)
        self.assertEqual(["/source/carddemo/app-mq/csd/MQ.csd:/source/carddemo/csd/CARDDEMO.CSD",
                          "/source/bank-of-z/cics/defs.yaml", "/source/bank-of-z/cics/defs.yaml",
                          "/source/bank-of-z/cics/defs.yaml"], result.stdout.splitlines())

    def test_carddemo_profile_declares_only_documented_language_environment_contracts(self):
        path = REPO / "tools/cobol-rekt/contracts/estates/carddemo.json"
        profile = json.loads(path.read_text())
        node = {"dependencyKind": "EXTERNAL_RUNTIME", "runtimeContractBindingComplete": True,
                "dependencyAnalysisComplete": False, "sourcePath": None, "sourceSha256": None,
                "callTarget": {"name": "CEE3ABD", "programReferenceType": "STATIC"},
                "runtimeContract": {"profileName": "carddemo-v1", "profileKind": "target-port",
                                    "sourcePath": str(path), "sourceSha256": "0" * 64, "name": "CEE3ABD",
                                    "resolutionKind": "documented-platform-contract", "runtimeMapping": "unverified",
                                    "definition": profile["runtimeContracts"]["CEE3ABD"]}}
        self.assertEqual({"CEE3ABD", "CEEDAYS", "CBLTDLI", "DSNTIAC", "MVSWAIT", "COBDATFT"},
                         set(profile["runtimeContracts"]))
        bank = json.loads((REPO / "tools/cobol-rekt/contracts/bank-of-z-spring-h2-v2.json").read_text())
        self.assertEqual(bank["runtimeContracts"]["CEEDAYS"], profile["runtimeContracts"]["CEEDAYS"])
        self.assertNotIn("sqlDefaultSchema", profile)
        self.assertTrue(evidence.native_runtime_contract_bound(node, profile))
        extra = copy.deepcopy(node)
        extra["runtimeContract"]["definition"]["parameters"].append(
            dict(extra["runtimeContract"]["definition"]["parameters"][0], name="fc"))
        self.assertFalse(evidence.native_runtime_contract_bound(extra))

    def test_dynamic_call_resolution_binds_documented_runtime_to_read_only_constant(self):
        path = REPO / "tools/cobol-rekt/contracts/estates/carddemo.json"
        profile = json.loads(path.read_text())

        def entry(*tokens):
            return {"rule": "DataDescriptionEntryFormat1Context", "children": [{"token": t} for t in tokens]}
        call = ["CALL", "LIT-DSNTIAC", "USING", "DFHEIBLK", "DFHCOMMAREA", "SQLCA", "WS-MSG", "WS-LRECL"]
        node = {"dependencyKind": "EXTERNAL_RUNTIME", "runtimeContractBindingComplete": True,
                "dependencyAnalysisComplete": False, "sourcePath": None, "sourceSha256": None,
                "callTarget": {"name": "DSNTIAC", "programReferenceType": "STATIC"},
                "runtimeContract": {
                    "profileName": "carddemo-v1", "profileKind": "target-port", "sourcePath": str(path),
                    "sourceSha256": "0" * 64, "name": "DSNTIAC", "resolutionKind": "documented-platform-contract",
                    "runtimeMapping": "unverified", "definition": profile["runtimeContracts"]["DSNTIAC"],
                    "targetResolution": {
                        "operand": "LIT-DSNTIAC",
                        "targetResolution": {
                            "kind": "immutable-working-storage-literal", "operand": "LIT-DSNTIAC",
                            "definitionSyntax": entry("05", "LIT-DSNTIAC", "PIC", "X(7)", "VALUE", "'DSNTIAC'", "."),
                            "ancestorSyntax": [entry("01", "WS-LITERALS", ".")],
                            "usageProof": evidence.CONSTANT_OPERAND_PROOF},
                        "callSyntax": {"rule": "CallStatementContext", "children": [{"token": t} for t in call]}}}}
        self.assertTrue(evidence.native_runtime_contract_bound(node, profile))
        resolution = node["runtimeContract"]["targetResolution"]
        other_value = copy.deepcopy(node)
        other_value["runtimeContract"]["targetResolution"]["targetResolution"]["definitionSyntax"] = entry(
            "05", "LIT-DSNTIAC", "PIC", "X(7)", "VALUE", "'DSNTIAR'", ".")
        other_call = copy.deepcopy(node)
        other_call["runtimeContract"]["targetResolution"]["callSyntax"]["children"][1]["token"] = "WS-PGM"
        unproved = copy.deepcopy(node)
        unproved["runtimeContract"]["targetResolution"]["targetResolution"]["usageProof"] = "assumed"
        subscripted = copy.deepcopy(node)
        subscripted["runtimeContract"]["targetResolution"]["callSyntax"]["children"][2:2] = [{"token": "("}]
        extra = copy.deepcopy(node)
        extra["runtimeContract"]["targetResolution"] = dict(resolution, note="x")
        for name, damaged in (("other constant", other_value), ("other operand", other_call),
                              ("usage not proved", unproved), ("subscripted operand", subscripted),
                              ("extra key", extra)):
            with self.subTest(damage=name):
                self.assertFalse(evidence.native_runtime_contract_bound(damaged, profile))

    def run_doctor(self, structural_success, export_success=True, native=False, duplicate_callee=False,
                   cics_definitions=None, facts_cli=None, other_estate=True, expected_estate="estate"):
        (self.work / "tools").mkdir()
        for helper in ("rekt_dependency_evidence.py", "rekt_source_identity.py",
                       "rekt_parse_diagnostics.py", "preprocess-for-rekt.sh", "bms_symbolic_map.py",
                       "native_run_activity.py"):
            shutil.copy2(REPO / "tools" / helper, self.work / "tools" / helper)
        # Doctor uses output/rekt; keep all writes inside this test's fixture.
        output = self.work / "output/rekt"
        output.mkdir()
        old_report = output / (self.relative + ".report")
        old_report.mkdir(parents=True)
        (old_report / "data.json").write_text('{"name":"DFHAID-STUB"}')
        other = output / "other/PROG.cbl-deps.json"
        other.parent.mkdir()
        other.write_text("unrelated")
        if other_estate:
            self.write("other/PROG.cbl", "           DISPLAY 'OTHER'.\n")
        if native:
            self.write("estate/SUBPROG.cbl", "           GOBACK.\n")
            if duplicate_callee:
                self.write(duplicate_callee, "           DISPLAY 'NOT THE CALLEE'.\n")
        script = (REPO / "doctor.sh").read_text()
        functions = script[script.index("run_rekt_logged() {"):script.index("\nvalidate_rekt_jcl_source() {")]
        mock = self.work / "mock_parser.py"
        mock.write_text(r'''
import json, os, sys
from pathlib import Path
root = Path(os.environ["REPO_ROOT"])
args = sys.argv[1:]
with (root / "calls.jsonl").open("a") as log:
    log.write(json.dumps(args) + "\n")
if "sh" in args:
    print("10")
    sys.exit(0)
if "dependency" in args:
    print("export diagnostic retained", file=sys.stderr)
    if os.environ["EXPORT_SUCCESS"] != "true":
        sys.exit(23)
    export = next(arg.split("=", 1)[1] for arg in args if arg.startswith("--export="))
    path = root / "output/rekt" / export.removeprefix("/output/")
    path.parent.mkdir(parents=True, exist_ok=True)
    if os.environ["NATIVE_EXPORT"] == "true":
        import hashlib
        source_dir = next(arg.split("=", 1)[1] for arg in args if arg.startswith("--srcDir="))
        view = root / "source" / source_dir.removeprefix("/source/")
        assert (view / "PROG.cbl").read_bytes() == (root / "source/estate/PROG.cbl").read_bytes()
        coverage = dict.fromkeys(("COPY", "CALL", "EXEC_CICS", "EXEC_SQL", "FILE_IO"), "complete")
        absence = {"EXEC_CICS":"antlr-tree:no-dialect-contexts",
                   "EXEC_SQL":"antlr-tree:no-dialect-contexts",
                   "FILE_IO":"antlr-tree:no-file-io-contexts"}
        def node(name, kind, host, native_path):
            return {"callTarget":{"name":name,"programReferenceType":"STATIC"}, "dependencyKind":kind,
                    "sourcePath":native_path,"sourceSha256":hashlib.sha256(host.read_bytes()).hexdigest(),
                    "dependencyAnalysisComplete":True,"dependencyCoverage":dict(coverage),
                    "dependencyAbsenceEvidence":dict(absence),"dependencyDiagnostics":[],"dependencies":[]}
        document = node("PROG.cbl", "PROGRAM", view / "PROG.cbl", source_dir + "/PROG.cbl")
        document["dependencyExportSchemaVersion"] = 1
        # Each estate is staged separately; copybooks resolve only from the program's estate.
        estate_staging = source_dir.split("/.dependency-input/", 1)[0]
        assert estate_staging == "/source/.rekt-staging/" + os.environ["EXPECT_ESTATE"], estate_staging
        copybook = root / "source" / estate_staging.removeprefix("/source/") / "REAL.cpy"
        document["dependencies"].append(node("REAL.cpy", "COPY", copybook, estate_staging + "/REAL.cpy"))
        callees = [candidate for candidate in view.glob("SUBPROG.*")
                   if candidate.suffix.lower() in (".cbl", ".cob")]
        assert len(callees) <= 1, "ambiguous program was exposed to native find-first lookup"
        if callees:
            callee = callees[0]
            document["dependencies"].append(node("SUBPROG", "CALL", callee, source_dir + "/" + callee.name))
        else:
            document["dependencies"].append({"callTarget":{"name":"SUBPROG","programReferenceType":"STATIC"},
                "dependencyKind":"CALL","dependencies":[],"dependencyAnalysisComplete":False,
                "dependencyDiagnostics":["source-not-found:SUBPROG"]})
            document["dependencyAnalysisComplete"] = False
            document["dependencyCoverage"]["CALL"] = "partial"
            document["dependencyDiagnostics"] = ["incomplete-callee:SUBPROG"]
        path.write_text(json.dumps(document))
        sys.exit(0 if callees else 1)
    path.write_text(json.dumps({"callTarget":{"name":"PROG.cbl"},"dependencies":[{"name":"REAL.cpy"}]}))
    sys.exit(0)
if "run" in args and os.environ["STRUCTURAL_SUCCESS"] == "true":
    source_dir = next(arg.split("=",1)[1] for arg in args if arg.startswith("--srcDir="))
    isolated = root / "source" / source_dir.removeprefix("/source/")
    assert [file.name for file in isolated.iterdir()] == ["PROG.cbl"], "structural root isolation changed"
    report = next(arg.split("=",1)[1] for arg in args if arg.startswith("--reportDir="))
    path = root / "output/rekt" / report.removeprefix("/output/") / "PROG.cbl.report"
    for kind in ("flow_ast", "cfg"):
        (path / kind).mkdir(parents=True, exist_ok=True)
        (path / kind / "program.json").write_text('{"id":"PROGRAM"}')
    sys.exit(0)
print("structural parse unavailable", file=sys.stderr)
sys.exit(1)
''')
        harness = r'''
resolve_runtime_setting() { echo source; }
validate_rekt_jcl_source() { return 0; }
detect_docker_api_version() { return 0; }
ensure_rekt_containers() { return 0; }
scan_rekt_jcl() { return 0; }
run_compose() { echo "Unexpected container restart" >&2; return 1; }
container_exec() { "$PYTHON_CMD" "$REPO_ROOT/mock_parser.py" "$@"; }
''' + functions + "\nrun_rekt_parse\n"
        env = dict(os.environ, REPO_ROOT=str(self.work), PYTHON_CMD=sys.executable,
                   REKT_CONTAINER="mock-parser", REKT_SERVICE="mock-service",
                   REKT_STUB_MARKER="AUTO-GENERATED STUB COPYBOOK",
                   REKT_GENERATED_COPYBOOK_DIRS="copy-generated",
                   REKT_PREPROCESS_RULES=str(self.work / "no-rules.json"),
                   _REKT_PROGRAM_FILTER=self.relative, _PROGRAM_FACTS="false",
                   STRUCTURAL_SUCCESS=str(structural_success).lower(),
                   EXPORT_SUCCESS=str(export_success).lower(),
                   NATIVE_EXPORT=str(native).lower(), EXPECT_ESTATE=expected_estate,
                   BLUE="", CYAN="", GREEN="", RED="", YELLOW="", NC="")
        env.pop("REKT_CICS_RESOURCE_DEFINITIONS", None)
        if cics_definitions is not None:
            env["REKT_CICS_RESOURCE_DEFINITIONS"] = cics_definitions
        if facts_cli is not None:
            (self.work / "CobolToQuarkusMigration.csproj").touch()
            commands = self.work / "commands"
            commands.mkdir()
            dotnet = commands / "dotnet"
            dotnet.write_text(f"#!{sys.executable}\n"
                              "import json, os, sys\n"
                              "from pathlib import Path\n"
                              "with (Path(os.environ['REPO_ROOT']) / 'dotnet-calls.jsonl').open('a') as log:\n"
                              "    log.write(json.dumps(sys.argv[1:]) + '\\n')\n")
            dotnet.chmod(0o755)
            env.update(PATH=str(commands) + os.pathsep + env["PATH"],
                       MIGRATION_CLI_ASSEMBLY=facts_cli, _PROGRAM_FACTS="true",
                       _PROGRAM_FACTS_PRUNE_ORPHANS="true")
        result = subprocess.run(["bash", "-c", harness], cwd=self.work, env=env,
                                text=True, capture_output=True)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        calls = [json.loads(line) for line in (self.work / "calls.jsonl").read_text().splitlines()]
        self.assertEqual(1, sum("dependency" in call for call in calls), calls)
        self.assertEqual("unrelated", other.read_text())
        archived = list((output / ".previous").rglob("data.json"))
        self.assertEqual(1, len(archived))
        current = json.loads((output / (self.relative + ".report/dependency-evidence.json")).read_text())
        self.assertFalse(current["placeholderContaminated"])
        self.assertEqual(self.relative, current["relativePath"])
        self.assertEqual(str(self.program.resolve()), current["originalSourcePath"])
        self.assertIn("export diagnostic retained", (output / (self.relative + ".parse.log")).read_text())
        self.assertIn("export diagnostic retained", result.stdout)
        native_logs = list((self.work / "output/rekt-scans").glob("doctor-*/native.log"))
        self.assertEqual(1, len(native_logs))
        self.assertIn("export diagnostic retained", native_logs[0].read_text())
        for call in calls:
            if "run" in call or "dependency" in call:
                source_dir = next(arg for arg in call if arg.startswith("--srcDir="))
                scope = ".dependency-input" if "dependency" in call else ".parser-input"
                self.assertIn(f"/{scope}/estate/PROG.cbl", source_dir)
        return current, calls

    def test_doctor_exports_after_successful_structural_parse(self):
        current, calls = self.run_doctor(True)
        self.assertEqual(0, current["exportExitCode"])
        self.assertEqual(1, sum("run" in call for call in calls))
        self.assertEqual("partial", current["coverage"]["COPY"])

    def test_doctor_uses_explicit_prebuilt_cli_for_fact_extraction_and_pruning(self):
        assembly = str(self.work / "build output/CobolToQuarkusMigration.dll")
        self.run_doctor(True, facts_cli=assembly)
        calls = [json.loads(line) for line in (self.work / "dotnet-calls.jsonl").read_text().splitlines()]
        facts = [call for call in calls if "program-facts" in call]
        self.assertEqual(2, len(facts), calls)
        self.assertEqual({"extract", "prune-orphans"}, {call[2] for call in facts})
        self.assertTrue(all(call[0] == assembly and call[1] == "program-facts" for call in facts))

    def test_doctor_forwards_explicit_cics_resource_paths_only_to_dependency_export(self):
        paths = "/source/region one/resources.yaml:/source/region-two/resources.yaml"
        _, calls = self.run_doctor(True, cics_definitions=paths)
        export, = [call for call in calls if "dependency" in call]
        self.assertIn("-e", export)
        self.assertEqual("REKT_CICS_RESOURCE_DEFINITIONS=" + paths, export[export.index("-e") + 1])
        self.assertFalse(any("-e" in call for call in calls if "run" in call))

    def test_filtered_native_progress_counts_selected_programs_not_dependency_search_context(self):
        self.run_doctor(True, native=True)
        manifests = list((self.work / "output/rekt-scans").glob("*/scan.json"))
        self.assertEqual(1, len(manifests))
        activity = json.loads(manifests[0].read_text())
        self.assertEqual(1, activity["expectedPrograms"])
        self.assertEqual([self.relative], [row["relativePath"] for row in activity["programs"]])
        self.assertTrue((self.source / "estate/SUBPROG.cbl").exists())
        self.assertTrue((self.source / "other/PROG.cbl").exists())

    def test_doctor_single_program_folder_resolves_copybooks_from_sibling_folders(self):
        # A source/cbl + source/cpy style layout: programs sit in one folder only, so all of
        # source/ is one estate and the sibling copybook folder is visible.
        shutil.rmtree(self.source / "estate/copy")
        self.definition = self.write("cpy/REAL.cpy", "       01 REAL-FIELD PIC X.\n")
        current, _ = self.run_doctor(True, native=True, other_estate=False, expected_estate=".all")
        self.assertEqual([], current["unresolvedDiagnostics"])
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), current["coverage"])

    def test_doctor_retains_dependency_evidence_when_every_structural_attempt_fails(self):
        current, calls = self.run_doctor(False)
        self.assertEqual(0, current["exportExitCode"])
        self.assertEqual(3, sum("run" in call for call in calls))

    def test_doctor_failed_export_still_writes_failure_sidecar_and_retains_errors(self):
        current, _ = self.run_doctor(True, False)
        self.assertEqual(23, current["exportExitCode"])
        self.assertIn("dependency-export-failed:23", current["unresolvedDiagnostics"])
        self.assertEqual("unavailable", current["coverage"]["COPY"])

    def test_doctor_dependency_view_resolves_genuine_sibling_without_changing_root_identity(self):
        current, _ = self.run_doctor(True, native=True)
        self.assertEqual([], current["unresolvedDiagnostics"],
                         (self.work / "output/rekt" / (self.relative + ".parse.log")).read_text())
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), current["coverage"])
        calls = [item for item in current["definitions"] if item["kind"] == "CALL"]
        self.assertEqual([{"kind": "CALL", "name": "SUBPROG",
                           "path": str(self.source / "estate/SUBPROG.cbl"),
                           "sha256": evidence.digest(self.source / "estate/SUBPROG.cbl")}], calls)

    def test_doctor_dependency_view_withholds_duplicate_callee_stems_across_extensions(self):
        current, _ = self.run_doctor(True, native=True, duplicate_callee="estate/dup/SUBPROG.cob")
        self.assertEqual(1, current["exportExitCode"])
        self.assertEqual("partial", current["coverage"]["CALL"])
        self.assertIn("ambiguous-call-target:SUBPROG", current["unresolvedDiagnostics"])
        self.assertFalse(any(item["kind"] == "CALL" for item in current["definitions"]))

    def test_doctor_other_estate_cannot_shadow_or_stage_into_this_estate(self):
        current, _ = self.run_doctor(True, native=True, duplicate_callee="other/SUBPROG.cob")
        self.assertEqual([], current["unresolvedDiagnostics"])
        self.assertEqual([str(self.source / "estate/SUBPROG.cbl")],
                         [item["path"] for item in current["definitions"] if item["kind"] == "CALL"])
        self.assertFalse((self.source / "other/.preprocessed").exists())


@unittest.skipUnless(os.environ.get("REKT_TEST_IMAGE"), "Set REKT_TEST_IMAGE for native dependency integration")
class NativeDependencyExportTests(unittest.TestCase):
    def setUp(self):
        self.work = REPO / "tools/tests/_work" / self._testMethodName
        self.source = self.work / "source"
        self.output = self.work / "output"
        self.source.mkdir(parents=True)
        (self.output / "java-work").mkdir(parents=True)

    def tearDown(self):
        shutil.rmtree(self.work)

    def run_parser(self, body, missing=False, file_io=False, callee=True, callee_dialect=False,
                   empty_file_section=False, cics_resources=None, declarations="", profile=None,
                   profile_outside_source=False, sql_declarations=None, cics_resources_file="resources.yaml",
                   vsam_definitions=None, environment=None):
        (self.source / "PILOT.cbl").write_text(
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PILOT.\n"
            + ("       ENVIRONMENT DIVISION.\n       INPUT-OUTPUT SECTION.\n       FILE-CONTROL.\n"
               "           SELECT INPUT-FILE ASSIGN TO 'fixture.dat'.\n" if file_io else "")
            + "       DATA DIVISION.\n"
            + ("       FILE SECTION.\n       FD INPUT-FILE.\n       01 INPUT-RECORD PIC X(10).\n"
               if file_io else "       FILE SECTION.\n" if empty_file_section else "")
            + "       WORKING-STORAGE SECTION.\n"
            + ("       COPY PAYREC.\n" if missing or "PAY-AMOUNT" in body else "")
            + "       01 WS-TARGET PIC X(8) VALUE 'PAYHELP'.\n"
            + declarations + "       PROCEDURE DIVISION.\n" + body + "           GOBACK.\n")
        if not missing:
            (self.source / "PAYREC.cpy").write_text("       01 PAY-AMOUNT PIC 9(4).\n")
        if callee:
            (self.source / "PAYHELP.cbl").write_text(
                "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYHELP.\n"
                "       DATA DIVISION.\n       LINKAGE SECTION.\n       01 AMOUNT PIC 9(4).\n"
                "       PROCEDURE DIVISION USING AMOUNT.\n"
                + ("           EXEC CICS RETURN TRANSID('NEXT') END-EXEC.\n" if callee_dialect == "options"
                   else "           EXEC CICS RETURN END-EXEC.\n" if callee_dialect else "")
                + "           GOBACK.\n")
        command = [
            "docker", "run", "--rm", "--memory=1g", "--entrypoint=java", "--workdir=/output",
            "--cidfile", str(self.output / "container-id"),
            "-e", "JAVA_TOOL_OPTIONS=-Xmx512m -XX:-UsePerfData -Djava.io.tmpdir=/output/java-work",
            "-v", f"{self.source}:/source:ro", "-v", f"{self.output}:/output",
            os.environ["REKT_TEST_IMAGE"], "-jar", "/app/smojol-cli.jar", "dependency", "PILOT.cbl",
            "--srcDir=/source", "--copyBooksDir=/source", "--dialectJarPath=/app/dialect-idms.jar",
            "--export=/output/PILOT.cbl-deps.json",
        ]
        if cics_resources is not None:
            (self.source / cics_resources_file).write_text(cics_resources)
            command[2:2] = ["-e", f"REKT_CICS_RESOURCE_DEFINITIONS=/source/{cics_resources_file}"]
        if sql_declarations is not None:
            command[2:2] = ["-e", f"REKT_SQL_DECLARATIONS={sql_declarations}"]
        if vsam_definitions is not None:
            names = []
            for index, text in enumerate(vsam_definitions):
                (self.source / f"VSAM{index}.jcl").write_text(text)
                names.append(f"/source/VSAM{index}.jcl")
            command[2:2] = ["-e", "REKT_VSAM_DEFINITIONS=" + ":".join(names)]
        for key, value in (environment or {}).items():
            command[2:2] = ["-e", f"{key}={value}"]
        if profile is not None:
            profile_path = (self.work if profile_outside_source else self.source) / "profile.json"
            profile_path.write_text(json.dumps(profile) if isinstance(profile, dict) else profile)
            parser_profile = str(profile_path.resolve()) if profile_outside_source else "/source/profile.json"
            command[2:2] = ["-e", f"REKT_DEPENDENCY_PROFILE={parser_profile}"]
            if profile_outside_source:
                command[2:2] = ["-v", f"{profile_path.resolve()}:{parser_profile}:ro"]
        try:
            result = subprocess.run(command, capture_output=True, text=True, timeout=120)
        finally:
            cid_file = self.output / "container-id"
            if cid_file.is_file():
                subprocess.run(["docker", "rm", "-f", cid_file.read_text().strip()],
                               capture_output=True, timeout=20, check=False)
                cid_file.unlink()
        exported = json.loads((self.output / "PILOT.cbl-deps.json").read_text())
        return result, exported

    def target_profile(self):
        return json.loads((REPO / "tools/cobol-rekt/contracts/bank-of-z-spring-h2-v2.json").read_text())

    def terminal_map(self):
        import bms_symbolic_map as bms
        screen = self.source / "SCREEN.bms"
        screen.write_text("SCREEN   DFHMSD TYPE=DSECT,LANG=COBOL,MODE=INOUT\n"
                          "PANEL    DFHMDI SIZE=(24,80)\n"
                          "FIELD1   DFHMDF POS=(1,1),LENGTH=4\n"
                          "         DFHMSD TYPE=FINAL\n         END\n")
        generated = self.source / ".preprocessed"
        _, warnings = bms.generate(self.source, generated)
        self.assertEqual([], warnings)
        evidence.stage(generated / "SCREEN.cpy", self.source / "SCREEN.cpy", derived=True)

    def check_terminal_gate(self, allowed, index):
        gate = os.environ.get("REKT_PLATFORM_GATE_ASSEMBLY")
        if not gate:
            return
        target = self.work / f"terminal-gate-{index}"
        checked = subprocess.run([
            "dotnet", gate, "dependency-plan", "--source", str(self.source),
            "--facts", str(self.output), "--output", str(target),
        ], capture_output=True, text=True, timeout=60)
        self.assertEqual(0, checked.returncode, checked.stdout + checked.stderr)
        programs = json.loads((target / "draft-conversion-plan.json").read_text())["programs"]
        pilot = next(program for program in programs if program["program"] == "PILOT.cbl")
        self.assertEqual("eligible" if allowed else "blocked", pilot["status"], pilot)

    def test_native_terminal_bindings_and_continuations_pass_consumers_and_reject_tampering(self):
        self.terminal_map()
        body = (
            "           EXEC CICS SEND CONTROL ERASE FREEKB END-EXEC.\n"
            "           EXEC CICS SEND TEXT FROM(MESSAGE-TEXT) ERASE\n"
            "               RESP(RESPONSE-CODE) RESP2(RESPONSE-TWO) END-EXEC.\n"
            "           EXEC CICS SEND MAP('PANEL') MAPSET('SCREEN')\n"
            "               FROM(PANELO) DATAONLY FREEKB END-EXEC.\n"
            "           EXEC CICS SEND MAP('PANEL') MAPSET('SCREEN')\n"
            "               MAPONLY ERASE END-EXEC.\n"
            "           EXEC CICS RECEIVE MAP('PANEL') MAPSET('SCREEN')\n"
            "               INTO(PANELI) TERMINAL ASIS\n"
            "               RESP(RESPONSE-CODE) END-EXEC.\n"
            "           EXEC CICS INQUIRE TERMINAL(EIBTRMID)\n"
            "               UCTRANST(TRANSLATION) RESP(RESPONSE-CODE) END-EXEC.\n"
            "           EXEC CICS INQUIRE ASSOCIATION(EIBTASKN)\n"
            "               ODAPPLID(MESSAGE-TEXT) ODUSERID(MESSAGE-TEXT)\n"
            "               ODFACILNAME(MESSAGE-TEXT)\n"
            "               ODNETWORKID(MESSAGE-TEXT)\n"
            "               ODFACILTYPE(TRANSLATION) END-EXEC.\n"
            "           EXEC CICS SET TERMINAL(EIBTRMID)\n"
            "               UCTRANST(TRANSLATION) RESP2(RESPONSE-TWO) END-EXEC.\n"
            "           IF TRANSLATION = DFHVALUE(UCTRAN)\n"
            "               CONTINUE END-IF.\n"
            "           IF TRANSLATION = DFHVALUE(TRANIDONLY)\n"
            "               CONTINUE END-IF.\n"
            "           IF TRANSLATION = DFHVALUE(NOUCTRAN)\n"
            "               CONTINUE END-IF.\n"
            "           EXEC CICS HANDLE ABEND CANCEL END-EXEC.\n"
            "           EXEC CICS BIF DEEDIT FIELD(FIELD1I)\n"
            "               LENGTH(4) RESP(RESPONSE-CODE) END-EXEC.\n"
            "           EXEC CICS RETURN TRANSID('NEXT')\n"
            "               COMMAREA(MESSAGE-TEXT) LENGTH(8) IMMEDIATE\n"
            "               RESP(RESPONSE-CODE) RESP2(RESPONSE-TWO) END-EXEC.\n")
        resources = ("resourceDefinitions:\n"
                     "  - program:\n      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n"
                     "  - transaction:\n      name: NEXT\n      program: PAYHELP\n"
                     "      dynamic: 'NO'\n      routable: 'NO'\n      status: ENABLED\n")
        result, exported = self.run_parser(body, declarations=(
            "       COPY SCREEN.\n       01 MESSAGE-TEXT PIC X(8).\n"
            "       01 RESPONSE-CODE PIC S9(8) COMP.\n       01 RESPONSE-TWO PIC S9(8) COMP.\n"
            "       01 TRANSLATION PIC S9(8) COMP.\n"), cics_resources=resources)
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        self.assertTrue(all(evidence.native_cics_statement_bound(statement)
                            for statement in exported["dependencyDialectStatements"]))
        self.assertEqual(["COPY"], [child["dependencyKind"] for child in exported["dependencies"]])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        self.assertTrue(any(item["kind"] == "CICS_CONTINUATION" for item in bound["definitions"]))
        report = self.output / "PILOT.cbl.report/dependency-evidence.json"
        evidence.write_json(report, bound)
        self.assertTrue(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        self.check_terminal_gate(True, 0)
        for index, damage in enumerate(("source-text", "map", "context", "continuation", "data", "missing-map"), 1):
            with self.subTest(damage=damage):
                document = copy.deepcopy(exported)
                statements = document["dependencyDialectStatements"]
                if damage == "source-text":
                    statements[0]["sourceText"] += " ALARM"
                elif damage == "map":
                    binding = next(item for statement in statements for item in statement.get("resourceBindings", [])
                                   if item["kind"] == "CICS_MAP")
                    binding["mapset"] = "OTHER"
                elif damage == "context":
                    binding = next(item for statement in statements for item in statement.get("resourceBindings", [])
                                   if item["kind"] == "CICS_CONTEXT")
                    binding["role"] = "invented-storage"
                elif damage == "continuation":
                    binding = next(item for statement in statements for item in statement.get("resourceBindings", [])
                                   if item["kind"] == "CICS_TRANSACTION")
                    binding["controlEffect"] = "synchronous-call"
                elif damage == "data":
                    binding = next(item for statement in statements for item in statement.get("resourceBindings", [])
                                   if item["kind"] == "CICS_DATA")
                    binding["definitionSyntax"]["rule"] = "UnknownContext"
                else:
                    statement = next(statement for statement in statements if statement["operation"] == "RECEIVE")
                    statement["resourceBindings"] = [item for item in statement["resourceBindings"]
                                                     if item["kind"] != "CICS_MAP"]
                (self.output / "PILOT.cbl-deps.json").write_text(json.dumps(document))
                self.assertFalse(all(evidence.native_cics_statement_bound(statement) for statement in statements))
                forged = {**bound, "exportSha256": evidence.digest(self.output / "PILOT.cbl-deps.json")}
                evidence.write_json(report, forged)
                self.assertFalse(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
                self.check_terminal_gate(False, index)
        (self.output / "PILOT.cbl-deps.json").write_text(json.dumps(exported))
        bound["exportSha256"] = evidence.digest(self.output / "PILOT.cbl-deps.json")
        evidence.write_json(report, bound)
        (self.source / "SCREEN.bms").write_text((self.source / "SCREEN.bms").read_text().replace("LENGTH=4", "LENGTH=5"))
        self.assertFalse(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        self.check_terminal_gate(False, 7)

    CONSTANT_DECLARATIONS = (
        "       COPY SCREEN.\n       01 MESSAGE-TEXT PIC X(8).\n"
        "       01 WS-LITERALS.\n"
        "          05 LIT-TRAN PIC X(4) VALUE 'NEXT'.\n"
        "          05 LIT-MAP PIC X(7) VALUE 'PANEL'.\n"
        "          05 LIT-MAPSET PIC X(8) VALUE 'SCREEN'.\n")
    CONSTANT_BODY = (
        "           MOVE LIT-TRAN TO MESSAGE-TEXT.\n"
        "           IF MESSAGE-TEXT = LIT-TRAN CONTINUE END-IF.\n"
        "           EXEC CICS RECEIVE MAP(LIT-MAP) MAPSET(LIT-MAPSET)\n"
        "               INTO(PANELI) END-EXEC.\n"
        "           EXEC CICS SEND MAP(LIT-MAP) MAPSET(LIT-MAPSET)\n"
        "               FROM(PANELO) DATAONLY END-EXEC.\n"
        "           EXEC CICS RETURN TRANSID(LIT-TRAN) END-EXEC.\n")
    # DFHCSDUP form as shipped with CardDemo; PAYHELP omits LANGUAGE like several CardDemo programs.
    CONSTANT_CSD = (
        " DEFINE PROGRAM(PAYHELP) GROUP(PILOT)\n DESCRIPTION(PILOT HELPER)\n"
        "        RELOAD(NO) STATUS(ENABLED) DYNAMIC(NO) TRANSID(NEXT)\n"
        " DEFINE TRANSACTION(NEXT) GROUP(PILOT)\n"
        "        PROGRAM(PAYHELP) STATUS(ENABLED) DYNAMIC(NO) ROUTABLE(NO)\n")

    def test_native_read_only_constants_resolve_cics_names_from_a_csd_and_reject_tampering(self):
        self.terminal_map()
        result, exported = self.run_parser(self.CONSTANT_BODY, declarations=self.CONSTANT_DECLARATIONS,
                                           cics_resources=self.CONSTANT_CSD, cics_resources_file="PILOT.CSD")
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statements = exported["dependencyDialectStatements"]
        self.assertTrue(all(evidence.native_cics_statement_bound(statement) for statement in statements))
        bindings = [item for statement in statements for item in statement.get("resourceBindings", [])]
        transaction = next(item for item in bindings if item["kind"] == "CICS_TRANSACTION")
        self.assertEqual(("NEXT", "immutable-working-storage-literal", "cics-blank-language-environment"),
                         (transaction["name"], transaction["targetResolution"]["kind"],
                          transaction["programBinding"]["attributes"]["languageProvenance"]))
        self.assertEqual(2, sum(item["kind"] == "CICS_MAP" and item["targetResolution"]["mapset"]["operand"] == "LIT-MAPSET"
                                for item in bindings))
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        evidence.write_json(self.output / "PILOT.cbl.report/dependency-evidence.json", bound)
        self.check_terminal_gate(True, 0)
        for index, damage in enumerate(("transaction-value", "map-ancestor", "literal-claim"), 1):
            with self.subTest(damage=damage):
                document = copy.deepcopy(exported)
                found = [item for statement in document["dependencyDialectStatements"]
                         for item in statement.get("resourceBindings", [])]
                if damage == "transaction-value":
                    proof = next(item for item in found if item["kind"] == "CICS_TRANSACTION")["targetResolution"]
                    for node in evidence.native_syntax_nodes(proof["definitionSyntax"]):
                        if node.get("token") == "'NEXT'":
                            node["token"] = "'NEXX'"
                elif damage == "map-ancestor":
                    proof = next(item for item in found if item["kind"] == "CICS_MAP")["targetResolution"]["map"]
                    proof["ancestorSyntax"] = []
                else:
                    next(item for item in found if item["kind"] == "CICS_TRANSACTION")["targetResolution"] = {"kind": "literal"}
                self.assertFalse(all(evidence.native_cics_statement_bound(statement)
                                     for statement in document["dependencyDialectStatements"]))
                (self.output / "PILOT.cbl-deps.json").write_text(json.dumps(document))
                evidence.write_json(self.output / "PILOT.cbl.report/dependency-evidence.json",
                                    {**bound, "exportSha256": evidence.digest(self.output / "PILOT.cbl-deps.json")})
                self.check_terminal_gate(False, index)

    CARDDEMO_FORMS = (
        "           INITIALIZE WS-WORK-AREA.\n"
        "           MOVE LIT-MAP TO WS-MAP.\n"
        "           EXEC CICS HANDLE CONDITION MAPFAIL(FAILURE)\n"
        "               ERROR(FAILURE) END-EXEC.\n"
        "           EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n"
        "               MAPONLY ERASE END-EXEC.\n"
        "           EXEC CICS SEND FROM(MESSAGE-TEXT) LENGTH(8)\n"
        "               WAIT RESP(RESPONSE-CODE) END-EXEC.\n"
        "           EXEC CICS SEND FROM(MESSAGE-TEXT)\n"
        "               LENGTH(LENGTH OF MESSAGE-TEXT) NOHANDLE ERASE END-EXEC.\n"
        "           EXEC CICS WRITEQ TD QUEUE('JOBS') FROM(MESSAGE-TEXT)\n"
        "               LENGTH(8) RESP(RESPONSE-CODE) END-EXEC.\n"
        "           GOBACK.\n       FAILURE.\n")
    CARDDEMO_DECLARATIONS = ("       COPY SCREEN.\n       01 MESSAGE-TEXT PIC X(8).\n"
                             "       01 WS-WORK-AREAS.\n         05 WS-WORK-AREA.\n"
                             "           10 WS-MAP PIC X(7).\n"
                             "       01 LIT-MAP PIC X(7) VALUE 'PANEL'.\n"
                             "       01 RESPONSE-CODE PIC S9(8) COMP.\n")
    # DFHCSDUP form of CardDemo's internal-reader queue.
    CARDDEMO_TDQ = (" DEFINE TDQUEUE(JOBS) GROUP(PILOT)\n"
                    "        TYPE(EXTRA) DDNAME(INREADER) TYPEFILE(OUTPUT)\n"
                    "        RECORDSIZE(80) RECORDFORMAT(FIXED)\n")

    def test_native_carddemo_screen_queue_and_condition_forms_bind_exactly(self):
        self.terminal_map()
        result, exported = self.run_parser(self.CARDDEMO_FORMS, declarations=self.CARDDEMO_DECLARATIONS,
                                           cics_resources=self.CARDDEMO_TDQ, cics_resources_file="PILOT.CSD")
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statements = exported["dependencyDialectStatements"]
        self.assertTrue(all(evidence.native_cics_statement_bound(statement) for statement in statements))
        bindings = [item for statement in statements for item in statement.get("resourceBindings", [])]
        self.assertEqual(["structured-reaching-assignments"],
                         [item["targetResolution"]["map"]["kind"] for item in bindings if item["kind"] == "CICS_MAP"])
        queue = next(item for item in bindings if item["kind"] == "CICS_TDQUEUE")
        self.assertEqual(("JOBS", "INREADER"), (queue["name"], queue["attributes"]["ddname"]))
        self.assertEqual(2, sum(item["kind"] == "CICS_LABEL" for item in bindings))
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        evidence.write_json(self.output / "PILOT.cbl.report/dependency-evidence.json", bound)
        self.check_terminal_gate(True, 0)
        damaged = copy.deepcopy(next(statement for statement in statements if statement["operation"] == "WRITEQ"))
        next(item for item in damaged["resourceBindings"] if item["kind"] == "CICS_TDQUEUE")["attributes"]["typefile"] = "INPUT"
        self.assertFalse(evidence.native_cics_statement_bound(damaged))
        for name, body, csd, diagnostic in [
            ("two-maps", self.CARDDEMO_FORMS.replace("           EXEC CICS SEND MAP", "           IF RESPONSE-CODE = 0\n"
             "               MOVE 'OTHER' TO WS-MAP END-IF.\n           EXEC CICS SEND MAP"), self.CARDDEMO_TDQ,
             "dynamic-cics-map-binding-unresolved"),
            ("handler-reaches-map", self.CARDDEMO_FORMS + "           EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n"
             "               MAPONLY END-EXEC.\n", self.CARDDEMO_TDQ, "dynamic-cics-map-binding-unresolved"),
            ("missing-queue", self.CARDDEMO_FORMS, self.CARDDEMO_TDQ.replace("JOBS", "LOGS"),
             "cics-tdqueue-definition-not-found:JOBS"),
            ("intra-queue", self.CARDDEMO_FORMS, self.CARDDEMO_TDQ.replace("EXTRA", "INTRA"),
             "cics-tdqueue-definition-invalid:"),
        ]:
            with self.subTest(name=name):
                result, exported = self.run_parser(body, declarations=self.CARDDEMO_DECLARATIONS,
                                                   cics_resources=csd, cics_resources_file="PILOT.CSD")
                self.assertNotEqual(0, result.returncode)
                self.assertTrue(any(diagnostic in issue for issue in exported["dependencyDiagnostics"]),
                                exported["dependencyDiagnostics"])

    STRAIGHT_LINE_FORMS = (
        "           MOVE LIT-MAP TO WS-MAP.\n"
        "           GO TO SHOW-SCREEN.\n"
        "       SHOW-SCREEN.\n"
        "           MOVE LIT-MAP TO WS-MAP\n"
        "           SET IS-READY TO TRUE\n"
        "           EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n"
        "               MAPONLY ERASE END-EXEC.\n"
        "           GOBACK.\n")
    STRAIGHT_LINE_DECLARATIONS = CARDDEMO_DECLARATIONS + "       01 READY-FLAG PIC X.\n         88 IS-READY VALUE 'Y'.\n"

    def test_native_straight_line_assignment_binds_map_despite_go_to(self):
        self.terminal_map()
        result, exported = self.run_parser(self.STRAIGHT_LINE_FORMS, declarations=self.STRAIGHT_LINE_DECLARATIONS)
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statements = exported["dependencyDialectStatements"]
        self.assertTrue(all(evidence.native_cics_statement_bound(statement) for statement in statements))
        send = next(statement for statement in statements if statement["operation"] == "SEND")
        proof = next(item for item in send["resourceBindings"] if item["kind"] == "CICS_MAP")["targetResolution"]["map"]
        self.assertEqual(("single-native-program:paragraph-straight-line-assignment", ["PANEL"], 1),
                         (proof["usageProof"], proof["reachingValues"], len(proof["assignments"])))
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        evidence.write_json(self.output / "PILOT.cbl.report/dependency-evidence.json", bound)
        self.check_terminal_gate(True, 0)
        doubled = copy.deepcopy(send)
        map_proof = next(item for item in doubled["resourceBindings"] if item["kind"] == "CICS_MAP")["targetResolution"]["map"]
        map_proof["assignments"].append(map_proof["assignments"][0])
        self.assertFalse(evidence.native_cics_statement_bound(doubled))
        for name, body, declarations in [
            ("nested-send", self.STRAIGHT_LINE_FORMS.replace(
                "           EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n               MAPONLY ERASE END-EXEC.\n",
                "           IF RESPONSE-CODE = 0\n               MOVE 'OTHER' TO WS-MAP\n"
                "               EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n"
                "                   MAPONLY ERASE END-EXEC\n           END-IF.\n"),
             self.STRAIGHT_LINE_DECLARATIONS),
            ("condition-on-item", self.STRAIGHT_LINE_FORMS.replace("IS-READY", "IS-PANEL"),
             self.STRAIGHT_LINE_DECLARATIONS.replace("PIC X(7).\n", "PIC X(7).\n             88 IS-PANEL VALUE 'X'.\n", 1)),
            ("truncated", self.STRAIGHT_LINE_FORMS,
             self.STRAIGHT_LINE_DECLARATIONS.replace("10 WS-MAP PIC X(7).", "10 WS-MAP PIC X(4).")),
        ]:
            with self.subTest(name=name):
                result, exported = self.run_parser(body, declarations=declarations)
                self.assertNotEqual(0, result.returncode)
                self.assertIn("dynamic-cics-map-binding-unresolved", exported["dependencyDiagnostics"])

    def test_native_constants_that_may_change_stay_unresolved(self):
        self.terminal_map()
        for name, declarations, body in [
            ("moved-into", self.CONSTANT_DECLARATIONS, "           MOVE 'XXXX' TO LIT-TRAN.\n"),
            ("group-initialized", self.CONSTANT_DECLARATIONS, "           INITIALIZE WS-LITERALS.\n"),
            ("redefined", self.CONSTANT_DECLARATIONS + "       01 OTHER-VIEW REDEFINES WS-LITERALS PIC X(19).\n", ""),
            ("condition-name", self.CONSTANT_DECLARATIONS.replace("VALUE 'NEXT'.", "VALUE 'NEXT'.\n             88 IS-NEXT VALUE 'NEXT'."), ""),
            ("occurs-group", self.CONSTANT_DECLARATIONS.replace("01 WS-LITERALS.\n", "01 WS-LITERALS.\n          03 WS-ROW OCCURS 2.\n"), ""),
            ("passed-by-reference", self.CONSTANT_DECLARATIONS, "           CALL 'PAYHELP' USING LIT-TRAN.\n"),
        ]:
            with self.subTest(name=name):
                result, exported = self.run_parser(body + self.CONSTANT_BODY, declarations=declarations,
                                                   cics_resources=self.CONSTANT_CSD, cics_resources_file="PILOT.CSD")
                diagnostics = [diagnostic for statement in exported["dependencyDialectStatements"]
                               for diagnostic in statement.get("bindingDiagnostics", [])]
                self.assertIn("dynamic-cics-return-transaction-unresolved:(LIT-TRAN)", diagnostics)

    def test_native_terminal_missing_resources_declarations_and_unsupported_forms_stay_blocked(self):
        self.terminal_map()
        for body, diagnostic in [
            ("EXEC CICS BIF DEEDIT FIELD(MISSING-FIELD) END-EXEC.", "cics-declaration-not-found"),
            ("EXEC CICS RETURN TRANSID('NEXT') END-EXEC.", "cics-transaction-definition-not-found"),
            ("EXEC CICS SEND MAP('MISSING') MAPSET('SCREEN') MAPONLY END-EXEC.", "cics-map-definition-not-found"),
            ("EXEC CICS SEND MAP('PANEL') MAPONLY END-EXEC.", "cics-map-definition-not-found"),
            ("EXEC CICS HANDLE ABEND RESET END-EXEC.", "cics-handle-operation-unsupported"),
            ("EXEC CICS BIF DIGEST RECORD('X') RESULT(WS-TARGET) END-EXEC.", "cics-terminal-variant-unsupported"),
            ("EXEC CICS INQUIRE ASSOCIATION(EIBTASKN)\n"
             "               ODAPPLID(MISSING-FIELD) END-EXEC.", "cics-declaration-not-found"),
            ("EXEC CICS INQUIRE ASSOCIATION(EIBTASKN)\n"
             "               ODAPPLID(WS-TARGET)\n"
             "               ODAPPLID(WS-TARGET) END-EXEC.", "cics-option-ambiguous"),
        ]:
            with self.subTest(body=body):
                result, exported = self.run_parser("           " + body.replace(
                    " END-EXEC.", "\n               END-EXEC.") + "\n")
                self.assertNotEqual(0, result.returncode)
                self.assertTrue(any(diagnostic in issue for issue in exported["dependencyDiagnostics"]),
                                exported["dependencyDiagnostics"])
        for suffix in ("", ".bms-source", ".bms-layout.json"):
            shutil.copyfile(str(self.source / "SCREEN.cpy") + suffix, str(self.source / "OTHER.cpy") + suffix)
        result, exported = self.run_parser(
            "           EXEC CICS SEND MAP('PANEL') MAPSET('SCREEN')\n"
            "               MAPONLY END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("cics-map-definition-ambiguous:SCREEN:PANEL", exported["dependencyDiagnostics"])

    def test_native_channel_fetch_delay_and_symbolic_completion_values_keep_all_operands(self):
        body = (
            "           EXEC CICS PUT CONTAINER(CONT-NAME) FROM(PAYLOAD)\n"
            "               FLENGTH(DATA-LENGTH) CHANNEL(CHAN-NAME) END-EXEC.\n"
            "           EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n"
            "           EXEC CICS FETCH ANY(CHILD-TOKEN) CHANNEL(CHAN-NAME)\n"
            "               NOSUSPEND COMPSTATUS(CHILD-STATUS) ABCODE(ABEND-CODE)\n"
            "               RESP(RESPONSE-CODE) END-EXEC.\n"
            "           EXEC CICS GET CONTAINER(CONT-NAME) CHANNEL(CHAN-NAME)\n"
            "               INTO(PAYLOAD) FLENGTH(DATA-LENGTH) END-EXEC.\n"
            "           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)\n"
            "               YYMMDD(FORMAT-DATE) DATESEP('.') END-EXEC.\n"
            "           IF CHILD-STATUS = DFHVALUE(NORMAL) CONTINUE END-IF.\n"
            "           IF CHILD-STATUS = DFHVALUE(ABEND) CONTINUE END-IF.\n"
            "           IF CHILD-STATUS = DFHVALUE(SECERROR) CONTINUE END-IF.\n")
        declarations = ("       01 CONT-NAME PIC X(16).\n       01 CHAN-NAME PIC X(16).\n"
                        "       01 PAYLOAD PIC X(8).\n       01 DATA-LENGTH PIC S9(9) COMP.\n"
                        "       01 CHILD-TOKEN PIC X(16).\n       01 CHILD-STATUS PIC S9(9) COMP.\n"
                        "       01 ABS-TIME PIC S9(15) COMP-3.\n       01 FORMAT-DATE PIC X(8).\n"
                        "       01 ABEND-CODE PIC X(4).\n       01 RESPONSE-CODE PIC S9(9) COMP.\n")
        result, exported = self.run_parser(body, declarations=declarations)
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        self.assertEqual(["PUT", "DELAY", "FETCH", "GET", "FORMATTIME", "DFHVALUE", "DFHVALUE", "DFHVALUE"],
                         [item["operation"] for item in exported["dependencyDialectStatements"]])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])

    def test_native_async_transaction_targets_require_a_complete_finite_loop_and_real_catalog_sources(self):
        declarations = ("       01 LOOP-NUM PIC 9 VALUE 0.\n       01 TRAN-ID PIC X(4) VALUE SPACES.\n"
                        "       01 CHAN-NAME PIC X(16).\n       01 CHILD-TOKEN PIC X(16).\n")
        body = ("           PERFORM VARYING LOOP-NUM FROM 1 BY 1 UNTIL LOOP-NUM > 2\n"
                "               STRING 'TST' DELIMITED BY SIZE\n"
                "                   LOOP-NUM DELIMITED BY SIZE INTO TRAN-ID END-STRING\n"
                "               EXEC CICS RUN TRANSID(TRAN-ID) CHANNEL(CHAN-NAME)\n"
                "                   CHILD(CHILD-TOKEN) END-EXEC\n"
                "           END-PERFORM.\n")
        catalog = ("resourceDefinitions:\n"
                   "  - program:\n      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n"
                   "  - transaction:\n      name: TST1\n      program: PAYHELP\n"
                   "      dynamic: 'NO'\n      routable: 'NO'\n      status: ENABLED\n"
                   "  - transaction:\n      name: TST2\n      program: PAYHELP\n"
                   "      dynamic: 'NO'\n      routable: 'NO'\n      status: ENABLED\n")
        for damage in ("", "target-write", "counter-write", "missing-route", "remote", "test-after"):
            with self.subTest(damage=damage):
                prepared = body
                resources = catalog
                if damage == "target-write":
                    prepared = body.replace("               EXEC CICS", "               MOVE 'BADX' TO TRAN-ID\n               EXEC CICS")
                elif damage == "counter-write":
                    prepared = body.replace("               EXEC CICS", "               ADD 1 TO LOOP-NUM\n               EXEC CICS")
                elif damage == "missing-route":
                    resources = catalog[:catalog.index("  - transaction:\n      name: TST2")]
                elif damage == "remote":
                    resources = catalog.replace("routable: 'NO'", "routable: 'YES'")
                elif damage == "test-after":
                    prepared = body.replace("PERFORM VARYING", "PERFORM WITH TEST AFTER VARYING")
                result, exported = self.run_parser(prepared, declarations=declarations, cics_resources=resources)
                self.assertEqual(not damage, result.returncode == 0,
                                 exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
                if not damage:
                    run, = exported["dependencyDialectStatements"]
                    transactions = [item for item in run["resourceBindings"] if item["kind"] == "CICS_TRANSACTION"]
                    self.assertEqual(["TST1", "TST2"], [item["name"] for item in transactions])
                    self.assertEqual(["CICS_RUN"], [item["dependencyKind"] for item in exported["dependencies"]])
                    bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
                    self.assertEqual([], bound["unresolvedDiagnostics"])
                    report = self.output / "PILOT.cbl.report/dependency-evidence.json"
                    evidence.write_json(report, bound)
                    self.assertTrue(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
                    transactions[0]["targetResolution"]["transactionNames"] = ["TST1"]
                    export = self.output / "PILOT.cbl-deps.json"
                    export.write_text(json.dumps(exported))
                    bound["exportSha256"] = evidence.digest(export)
                    evidence.write_json(report, bound)
                    self.assertFalse(evidence.evidence_current(self.source, "PILOT.cbl", self.output))

    def test_native_bms_layout_requires_the_exact_supported_source_copy_and_manifest(self):
        import bms_symbolic_map as bms
        screen = self.source / "SCREEN.bms"
        screen.write_text("SCREEN   DFHMSD TYPE=DSECT,LANG=COBOL,MODE=INOUT\n"
                          "PANEL    DFHMDI SIZE=(24,80)\n"
                          "FIELD1   DFHMDF POS=(1,1),LENGTH=4\n"
                          "         DFHMSD TYPE=FINAL\n         END\n")
        generated = self.work / "derived"
        _, warnings = bms.generate(self.source, generated)
        self.assertEqual([], warnings)
        copybook = self.source / "SCREEN.cpy"
        evidence.stage(generated / "SCREEN.cpy", copybook, derived=True)
        body = "           MOVE 'TEST' TO FIELD1O.\n"
        result, exported = self.run_parser(body, declarations="       COPY SCREEN.\n")
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        binding, = exported["dependencyDerivedInputs"]
        self.assertEqual("BMS_LAYOUT", binding["kind"])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        self.assertEqual({"BMS", "BMS_LAYOUT", "COPY"}, {item["kind"] for item in bound["definitions"]})
        report = self.output / "PILOT.cbl.report/dependency-evidence.json"
        evidence.write_json(report, bound)
        self.assertTrue(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        original_export = (self.output / "PILOT.cbl-deps.json").read_text()
        exported["dependencyDerivedInputs"] = []
        (self.output / "PILOT.cbl-deps.json").write_text(json.dumps(exported))
        bound["exportSha256"] = evidence.digest(self.output / "PILOT.cbl-deps.json")
        evidence.write_json(report, bound)
        self.assertFalse(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        (self.output / "PILOT.cbl-deps.json").write_text(original_export)
        Path(str(copybook) + ".bms-source").write_text(screen.read_text().replace("LENGTH=4", "LENGTH=5"))
        result, exported = self.run_parser(body, declarations="       COPY SCREEN.\n")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("partial-derived-bms-layout:SCREEN.cpy", exported["dependencyDiagnostics"])

    def test_native_shared_sql_catalog_is_explicit_hash_bound_and_does_not_replace_local_declarations(self):
        catalog = self.source / "catalog.cpy"
        catalog.write_text("           EXEC SQL DECLARE STTESTER.CONTROL TABLE\n"
                           "               (CONTROL_VALUE_NUM INTEGER)\n           END-EXEC.\n")
        body = ("           EXEC SQL SELECT CONTROL_VALUE_NUM INTO :RESULT-VALUE\n"
                "               FROM CONTROL END-EXEC.\n")
        host = "       01 RESULT-VALUE PIC S9(9) COMP.\n"
        result, exported = self.run_parser(body, declarations=host, profile=self.target_profile(),
                                          sql_declarations="/source/catalog.cpy")
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        bindings = exported["dependencyDialectStatements"][0]["resourceBindings"]
        binding, = [item for item in bindings if item["kind"] == "SQL_RELATION"]
        self.assertEqual("explicit-sql-catalog", binding["definitionScope"])
        self.assertEqual("/source/catalog.cpy", binding["sourcePath"])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0,
                                       parser_source_root="/source", sql_declarations="/source/catalog.cpy")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        declaration, = [item for item in bound["definitions"] if item["kind"] == "SQL_DECLARATION"]
        self.assertEqual(str(catalog.resolve()), declaration["path"])
        report = self.output / "PILOT.cbl.report/dependency-evidence.json"
        evidence.write_json(report, bound)
        self.assertTrue(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        bound["definitions"].remove(declaration)
        evidence.write_json(report, bound)
        self.assertFalse(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        rejected = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertTrue(any("native-sql-catalog-unverified" in item for item in rejected["unresolvedDiagnostics"]))
        catalog.write_text(catalog.read_text().replace("INTEGER", "CHAR(12)"))
        changed = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0,
                                         parser_source_root="/source", sql_declarations="/source/catalog.cpy")
        self.assertTrue(any("native-sql-catalog-unverified" in item for item in changed["unresolvedDiagnostics"]))

        result, exported = self.run_parser(body, declarations=host, profile=self.target_profile())
        self.assertNotEqual(0, result.returncode)
        self.assertIn("sql-declaration-not-found:SQL_RELATION:CONTROL", exported["dependencyDiagnostics"])
        local = ("           EXEC SQL DECLARE STTESTER.CONTROL TABLE\n"
                 "               (CONTROL_VALUE_NUM INTEGER)\n           END-EXEC.\n")
        result, exported = self.run_parser(body, declarations=host + local, profile=self.target_profile(),
                                          sql_declarations="/source/catalog.cpy")
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics"))
        binding, = [item for statement in exported["dependencyDialectStatements"]
                    for item in statement["resourceBindings"] if item["kind"] == "SQL_RELATION"]
        self.assertEqual("expanded-parser-input", binding["definitionScope"])

    def test_native_shared_sql_catalog_rejects_ambiguous_declarations_and_non_declaration_sql(self):
        declaration = ("           EXEC SQL DECLARE STTESTER.CONTROL TABLE\n"
                       "               (CONTROL_VALUE_NUM INTEGER)\n           END-EXEC.\n")
        for name in ("first.cpy", "second.cpy"):
            (self.source / name).write_text(declaration)
        body = ("           EXEC SQL SELECT CONTROL_VALUE_NUM INTO :RESULT-VALUE\n"
                "               FROM CONTROL END-EXEC.\n")
        result, exported = self.run_parser(
            body, declarations="       01 RESULT-VALUE PIC S9(9) COMP.\n", profile=self.target_profile(),
            sql_declarations="/source/first.cpy:/source/second.cpy")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("sql-declaration-ambiguous:SQL_RELATION:CONTROL", exported["dependencyDiagnostics"])
        (self.source / "first.cpy").write_text("           EXEC SQL COMMIT END-EXEC.\n")
        result, _ = self.run_parser(
            body, declarations="       01 RESULT-VALUE PIC S9(9) COMP.\n", profile=self.target_profile(),
            sql_declarations="/source/first.cpy")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("Only complete DECLARE TABLE", result.stdout + result.stderr)

    def test_native_lilian_and_local_time_services_bind_documented_contracts_not_cobol_stubs(self):
        result, exported = self.run_parser(
            "           CALL 'CEEDAYS' USING INPUT-DATE DATE-PICTURE\n"
            "               LILIAN-DAYS FC.\n"
            "           CALL 'CEELOCT' USING LILIAN-DAYS LOCAL-SECONDS\n"
            "               LOCAL-TEXT FC.\n",
            declarations="       01 INPUT-DATE.\n"
                         "          05 DATE-LENGTH PIC S9(4) COMP VALUE 10.\n"
                         "          05 DATE-TEXT PIC X(10) VALUE '2020-01-02'.\n"
                         "       01 DATE-PICTURE.\n"
                         "          05 PICTURE-LENGTH PIC S9(4) COMP VALUE 10.\n"
                         "          05 PICTURE-TEXT PIC X(10) VALUE 'YYYY-MM-DD'.\n"
                         "       01 LILIAN-DAYS PIC S9(8) COMP.\n"
                         "       01 LOCAL-SECONDS COMP-2.\n"
                         "       01 LOCAL-TEXT PIC X(17).\n"
                         "       01 FC PIC X(12).\n", profile=self.target_profile())
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        leaves = [node for node in exported["dependencies"] if node["dependencyKind"] == "EXTERNAL_RUNTIME"]
        self.assertEqual({"CEEDAYS", "CEELOCT"}, {node["callTarget"]["name"] for node in leaves})
        self.assertTrue(all(node["dependencyAnalysisComplete"] is False
                            and node["runtimeContract"]["runtimeMapping"] == "unverified"
                            and "sourcePath" not in node for node in leaves))
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        self.assertEqual({"CEEDAYS", "CEELOCT"},
                         {item["name"] for item in bound["definitions"] if item["kind"] == "RUNTIME_CONTRACT"})

    def test_native_qualified_cics_fields_locks_and_response_symbols_bind_without_runtime_claims(self):
        body = (
            "           EXEC CICS ASKTIME ABSTIME(ABS-TIME OF TIME-AREA)\n"
            "               END-EXEC.\n"
            "           EXEC CICS FORMATTIME ABSTIME(ABS-TIME OF TIME-AREA)\n"
            "               TIME(PRINT-TIME OF TIME-AREA) END-EXEC.\n"
            "           EXEC CICS ENQ RESOURCE(LOCK-KEY) LENGTH(8)\n"
            "               RESP(RESPONSE-CODE) END-EXEC.\n"
            "           EXEC CICS DEQ RESOURCE(LOCK-KEY) LENGTH(8)\n"
            "               RESP(RESPONSE-CODE) END-EXEC.\n")
        for symbol in ("NORMAL", "INVREQ", "MAPFAIL", "NOTFINISHED", "NOTFND", "ENDFILE", "DUPREC", "DUPKEY"):
            body += f"           IF RESPONSE-CODE = DFHRESP({symbol})\n               CONTINUE END-IF.\n"
        result, exported = self.run_parser(
            body, declarations="       01 TIME-AREA.\n"
                               "          05 ABS-TIME PIC S9(15) COMP-3.\n"
                               "          05 PRINT-TIME PIC X(6).\n"
                               "       01 OTHER-AREA.\n"
                               "          05 PRINT-TIME PIC X(6).\n"
                               "       01 LOCK-KEY PIC X(8).\n"
                               "       01 RESPONSE-CODE PIC S9(8) COMP.\n")
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        statements = exported["dependencyDialectStatements"]
        locks = [statement for statement in statements if statement["operation"] in ("ENQ", "DEQ")]
        self.assertEqual(2, len(locks))
        self.assertTrue(all(statement["bindingBasis"] == "parsed-cics-lock-operation" for statement in locks))
        self.assertTrue(all(statement["runtimeMapping"] == "unverified" for statement in statements))
        qualified = next(binding for statement in statements for binding in statement.get("resourceBindings", [])
                         if binding["name"] == "PRINT-TIMEOFTIME-AREA")
        self.assertEqual(2, len(qualified["qualificationPath"]))
        qualified["qualificationPath"] = [qualified["definitionSyntax"]]
        (self.output / "PILOT.cbl-deps.json").write_text(json.dumps(exported))
        rejected = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertNotEqual("complete", rejected["coverage"]["EXEC_CICS"])

    def test_native_qualified_cics_fields_reject_missing_qualifiers_and_ambiguous_bare_names(self):
        for operand in ("PRINT-TIME OF MISSING-AREA", "PRINT-TIME"):
            with self.subTest(operand=operand):
                result, exported = self.run_parser(
                    f"           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)\n"
                    f"               TIME({operand}) END-EXEC.\n",
                    declarations="       01 ABS-TIME PIC S9(15) COMP-3.\n"
                                 "       01 TIME-AREA.\n          05 PRINT-TIME PIC X(6).\n"
                                 "       01 OTHER-AREA.\n          05 PRINT-TIME PIC X(6).\n")
                self.assertNotEqual(0, result.returncode)
                self.assertTrue(any("cics-declaration-" in issue for issue in exported["dependencyDiagnostics"]))

    def test_native_cics_program_literals_allow_only_trailing_padding_within_eight_bytes(self):
        for operand, declarations, allowed in [
            ("'PAYHELP '", "", True),
            ("WS-PGM", "       01 WS-PGM PIC X(8) VALUE 'PAYHELP '.\n", True),
            ("'PAY HELP'", "", False),
            ("'PAYHELP  '", "", False),
        ]:
            with self.subTest(operand=operand):
                result, exported = self.run_parser(
                    f"           EXEC CICS LINK PROGRAM({operand}) END-EXEC.\n",
                    declarations=declarations,
                    cics_resources="resourceDefinitions:\n  - program:\n"
                                   "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
                self.assertEqual(allowed, result.returncode == 0, result.stdout + result.stderr)
                bound = evidence.build_evidence(
                    self.source, "PILOT.cbl", self.output, result.returncode, parser_source_root="/source")
                self.assertEqual(allowed, not bound["unresolvedDiagnostics"], bound["unresolvedDiagnostics"])

    def test_native_cics_lock_requires_a_resource_and_response_symbols_must_be_supported(self):
        for body in ("           EXEC CICS ENQ NOSUSPEND END-EXEC.\n",
                     "           IF 0 = DFHRESP(NOTREAL) CONTINUE END-IF.\n"):
            with self.subTest(body=body):
                result, exported = self.run_parser(body)
                self.assertNotEqual(0, result.returncode)
                self.assertFalse(exported["dependencyAnalysisComplete"])

    def test_native_external_profile_requires_exact_configuration_and_current_hash(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET\n"
            "               FROM CONTROL END-EXEC.\n"
            "           CALL 'CEEGMT' USING GMT-LILIAN GMT-SECONDS FC.\n"
            "           EXEC CICS WRITE FILE('TESTFILE') FROM(WS-TARGET)\n"
            "               END-EXEC.\n",
            declarations="           EXEC SQL DECLARE STTESTER.CONTROL TABLE\n"
                         "               (COL1 CHAR(8)) END-EXEC.\n"
                         "       01 GMT-LILIAN PIC S9(8) BINARY.\n"
                         "       01 GMT-SECONDS COMP-2.\n"
                         "       01 FC PIC X(12).\n",
            profile=self.target_profile(), profile_outside_source=True,
            cics_resources="resourceDefinitions:\n  - file:\n      name: TESTFILE\n"
                           "      dsname: TEST.DATA\n      recordsize: 80\n      keylength: 8\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(exported["dependencyAnalysisComplete"])
        profile_path = (self.work / "profile.json").resolve()
        profile_text = profile_path.read_text()
        other_path = self.work / "other-profile.json"
        other_path.write_text(profile_text)
        for configured in (None, other_path):
            with self.subTest(configured=configured):
                bound = evidence.build_evidence(
                    self.source, "PILOT.cbl", self.output, 0,
                    parser_source_root="/source", dependency_profile=configured)
                self.assertTrue(any(issue.startswith("native-sql-profile-unverified:")
                                    for issue in bound["unresolvedDiagnostics"]))
                self.assertTrue(any(issue.startswith("native-runtime-contract-unverified:CEEGMT:")
                                    for issue in bound["unresolvedDiagnostics"]))
                self.assertNotEqual(dict.fromkeys(evidence.KINDS, "complete"), bound["coverage"])
        bound = evidence.build_evidence(
            self.source, "PILOT.cbl", self.output, 0,
            parser_source_root="/source", dependency_profile=profile_path)
        self.assertEqual([], bound["unresolvedDiagnostics"])
        self.assertEqual({"ANALYSIS_PROFILE", "RUNTIME_CONTRACT", "CICS_FILE"},
                         {item["kind"] for item in bound["definitions"]})
        self.assertTrue(all(item["path"] == str(profile_path) for item in bound["definitions"]
                            if item["kind"] in ("ANALYSIS_PROFILE", "RUNTIME_CONTRACT")))
        evidence.write_json(self.output / "PILOT.cbl.report/dependency-evidence.json", bound)
        self.assertTrue(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        profile_path.write_text(json.dumps({**self.target_profile(), "sqlDefaultSchema": "OTHER"}))
        self.assertFalse(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        stale = evidence.build_evidence(
            self.source, "PILOT.cbl", self.output, 0,
            parser_source_root="/source", dependency_profile=profile_path)
        self.assertNotEqual([], stale["unresolvedDiagnostics"])
        profile_path.write_text(profile_text)
        self.assertTrue(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        cics_binding = next(binding for statement in exported["dependencyDialectStatements"]
                            for binding in statement["resourceBindings"] if binding["kind"] == "CICS_FILE")
        cics_binding.update(sourcePath=str(profile_path), sourceSha256=evidence.digest(profile_path))
        (self.output / "PILOT.cbl-deps.json").write_text(json.dumps(exported))
        rejected = evidence.build_evidence(
            self.source, "PILOT.cbl", self.output, 0,
            parser_source_root="/source", dependency_profile=profile_path)
        self.assertTrue(any(issue.startswith("native-cics-resource-definition-unverified:")
                            and "outside declared parser source root" in issue
                            for issue in rejected["unresolvedDiagnostics"]))

    def test_native_explicit_target_schema_binds_only_its_actual_qualified_declaration(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET\n"
            "               FROM CONTROL END-EXEC.\n",
            declarations="           EXEC SQL DECLARE STTESTER.CONTROL TABLE\n"
                         "               (COL1 CHAR(8)) END-EXEC.\n",
            profile=self.target_profile())
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_SQL"))
        binding = next(binding for binding in exported["dependencyDialectStatements"][-1]["resourceBindings"]
                       if binding["kind"] == "SQL_RELATION")
        self.assertEqual("STTESTER", binding["qualification"]["schema"])
        self.assertEqual("target-port", binding["qualification"]["profileKind"])
        self.assertEqual(evidence.digest(self.source / "profile.json"), binding["qualification"]["sourceSha256"])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        self.assertTrue(any(definition["kind"] == "ANALYSIS_PROFILE" for definition in bound["definitions"]))

    def test_native_wrong_explicit_schema_does_not_guess_another_declared_schema(self):
        profile = self.target_profile()
        profile["sqlDefaultSchema"] = "OTHER"
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET\n"
            "               FROM CONTROL END-EXEC.\n",
            declarations="           EXEC SQL DECLARE STTESTER.CONTROL TABLE\n"
                         "               (COL1 CHAR(8)) END-EXEC.\n", profile=profile)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("sql-declaration-not-found:SQL_RELATION:CONTROL",
                      "\n".join(exported["dependencyDiagnostics"]))

    def test_native_documented_platform_calls_bind_contracts_not_invented_cobol_sources(self):
        result, exported = self.run_parser(
            "           CALL 'CEEGMT' USING GMT-LILIAN GMT-SECONDS FC.\n"
            "           CALL 'CEEDATM' USING GMT-SECONDS DATM-PICTURE\n"
            "               DATM-CONV FC.\n",
            declarations="       01 GMT-LILIAN PIC S9(8) BINARY.\n"
                         "       01 GMT-SECONDS COMP-2.\n"
                         "       01 FC PIC X(12).\n"
                         "       01 DATM-PICTURE.\n"
                         "          05 DATM-LENGTH PIC 9999 COMP.\n"
                         "          05 DATM-FORMAT PIC X(14).\n"
                         "       01 DATM-CONV PIC X(80).\n", profile=self.target_profile())
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        leaves = [node for node in exported["dependencies"] if node["dependencyKind"] == "EXTERNAL_RUNTIME"]
        self.assertEqual(2, len(leaves))
        for leaf in leaves:
            self.assertFalse(leaf["dependencyAnalysisComplete"])
            self.assertTrue(leaf["runtimeContractBindingComplete"])
            self.assertNotIn("sourcePath", leaf)
            self.assertFalse(leaf["dependencyAbsenceEvidence"])
            self.assertTrue(evidence.native_runtime_contract_bound(leaf, self.target_profile()))
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        self.assertEqual({"CEEGMT", "CEEDATM"},
                         {definition["name"] for definition in bound["definitions"] if definition["kind"] == "RUNTIME_CONTRACT"})
        report = self.output / "PILOT.cbl.report" / "dependency-evidence.json"
        report.parent.mkdir(exist_ok=True)
        report.write_text(json.dumps(bound))
        self.assertTrue(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        (self.source / "profile.json").write_text(json.dumps({**self.target_profile(), "sqlDefaultSchema": "OTHER"}))
        self.assertFalse(evidence.evidence_current(self.source, "PILOT.cbl", self.output))

    def test_native_unknown_platform_call_still_requires_a_real_definition(self):
        result, exported = self.run_parser("           CALL 'NOTREAL'.\n", profile=self.target_profile())
        self.assertNotEqual(0, result.returncode)
        self.assertIn("source-not-found:NOTREAL", "\n".join(exported["dependencyDiagnostics"]))

    def test_native_sql_read_only_clause_is_not_a_host_indicator_and_another_fetch(self):
        result, exported = self.run_parser(
            "           EXEC SQL DECLARE TEST-CURSOR CURSOR FOR\n"
            "               SELECT COL1 FROM TESTTAB\n"
            "               WHERE COL1 = :WS-TARGET FOR FETCH ONLY\n"
            "               END-EXEC.\n"
            "           EXEC SQL OPEN TEST-CURSOR END-EXEC.\n",
            declarations="           EXEC SQL DECLARE TESTTAB TABLE\n"
                         "               (COL1 CHAR(8)) END-EXEC.\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_SQL"))
        cursor = next(statement for statement in exported["dependencyDialectStatements"]
                      if any(reference["name"] == "TEST-CURSOR" and reference["role"] == "declaration"
                             for reference in statement["resourceReferences"]))
        self.assertEqual({"TEST-CURSOR", "TESTTAB", "WS-TARGET"},
                         {reference["name"] for reference in cursor["resourceReferences"]})

    def test_native_sql_null_indicator_is_retained_and_bound_as_a_real_host_variable(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET\n"
            "               INDICATOR :WS-INDICATOR FROM TESTTAB END-EXEC.\n",
            declarations="       01 WS-INDICATOR PIC S9(4) COMP.\n"
                         "           EXEC SQL DECLARE TESTTAB TABLE\n"
                         "               (COL1 CHAR(8)) END-EXEC.\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_SQL"))
        select = next(statement for statement in exported["dependencyDialectStatements"]
                      if statement["operation"] == "SELECT")
        self.assertEqual({"WS-TARGET", "WS-INDICATOR"},
                         {binding["name"] for binding in select["resourceBindings"]
                          if binding["kind"] == "SQL_HOST_VARIABLE"})

    def test_native_current_task_cics_commands_bind_actual_data_and_local_abend_label(self):
        result, exported = self.run_parser(
            "           EXEC CICS HANDLE ABEND LABEL(FAILURE) END-EXEC.\n"
            "           EXEC CICS ASSIGN APPLID(WS-TARGET) END-EXEC.\n"
            "           EXEC CICS ASKTIME ABSTIME(WS-CLOCK) END-EXEC.\n"
            "           EXEC CICS FORMATTIME ABSTIME(WS-CLOCK)\n"
            "               YYYYMMDD(WS-TARGET) END-EXEC.\n"
            "           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.\n"
            "           GOBACK.\n       FAILURE SECTION.\n"
            "           EXEC CICS ABEND ABCODE('TEST') NODUMP END-EXEC.\n",
            declarations="       01 WS-CLOCK PIC S9(15) COMP-3.\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_CICS"))
        self.assertTrue(evidence.native_domain_evidence(exported, "FILE_IO"))
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        self.assertFalse(any(definition["kind"] == "CICS_FILE" for definition in bound["definitions"]))

    def test_native_cics_missing_local_data_or_label_stays_unresolved(self):
        result, exported = self.run_parser(
            "           EXEC CICS HANDLE ABEND LABEL(NOT-THERE) END-EXEC.\n"
            "           EXEC CICS ASKTIME ABSTIME(NOT-DECLARED) END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        diagnostics = "\n".join(exported["dependencyDiagnostics"])
        self.assertIn("cics-declaration-not-found:CICS_LABEL:NOT-THERE", diagnostics)
        self.assertIn("cics-declaration-not-found:CICS_DATA:NOT-DECLARED", diagnostics)

    def test_native_local_cics_link_binds_catalog_and_parsed_callee_source(self):
        result, exported = self.run_parser(
            "           EXEC CICS LINK PROGRAM('PAYHELP')\n"
            "               COMMAREA(WS-TARGET) END-EXEC.\n",
            cics_resources="resourceDefinitions:\n  - program:\n"
                           "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_CICS"))
        callee = next(node for node in exported["dependencies"] if node["dependencyKind"] == "CICS_LINK")
        self.assertEqual("PAYHELP", callee["callTarget"]["name"])
        self.assertTrue(callee["dependencyAnalysisComplete"])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        self.assertTrue(any(definition["kind"] == "CALL" and definition["name"] == "PAYHELP"
                            for definition in bound["definitions"]))
        self.assertTrue(any(definition["kind"] == "CICS_PROGRAM" for definition in bound["definitions"]))
        report = self.output / "PILOT.cbl.report" / "dependency-evidence.json"
        report.parent.mkdir(exist_ok=True)
        report.write_text(json.dumps(bound))
        self.assertTrue(evidence.evidence_current(self.source, "PILOT.cbl", self.output))
        (self.source / "resources.yaml").write_text("resourceDefinitions: []\n")
        self.assertFalse(evidence.evidence_current(self.source, "PILOT.cbl", self.output))

    def test_native_cics_length_of_operand_binds_the_named_item(self):
        declarations = "       01 WS-PGMS.\n         05 WS-HELP-PGM PIC X(08) VALUE 'PAYHELP'.\n"
        body = ("           EXEC CICS LINK PROGRAM(WS-HELP-PGM)\n"
                "               COMMAREA(WS-TARGET)\n"
                "               LENGTH(LENGTH OF WS-TARGET) END-EXEC.\n")
        catalog = "resourceDefinitions:\n  - program:\n      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n"
        result, exported = self.run_parser(body, declarations=declarations, cics_resources=catalog)
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statement = exported["dependencyDialectStatements"][-1]
        self.assertTrue(evidence.native_cics_statement_bound(statement))
        result, exported = self.run_parser(body.replace("OF WS-TARGET", "OF NOT-THERE"),
                                           declarations=declarations, cics_resources=catalog)
        self.assertNotEqual(0, result.returncode)
        self.assertTrue(any("cics-declaration-not-found:CICS_DATA:" in issue
                            for issue in exported["dependencyDiagnostics"]), exported["dependencyDiagnostics"])

    def test_native_cics_link_binds_a_read_only_group_member_constant(self):
        declarations = "       01 WS-PGMS.\n         05 WS-HELP-PGM PIC X(08) VALUE 'PAYHELP'.\n"
        body = "           EXEC CICS LINK PROGRAM(WS-HELP-PGM)\n               COMMAREA(WS-TARGET) END-EXEC.\n"
        catalog = "resourceDefinitions:\n  - program:\n      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n"
        result, exported = self.run_parser(body, declarations=declarations, cics_resources=catalog)
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statement = exported["dependencyDialectStatements"][-1]
        programs = [item for item in statement["resourceBindings"] if item["kind"] == "CICS_PROGRAM"]
        self.assertEqual([("PAYHELP", "call")], [(item["name"], item["controlEffect"]) for item in programs])
        self.assertTrue(evidence.native_cics_statement_bound(statement))
        self.assertEqual("PAYHELP", next(node for node in exported["dependencies"]
                                         if node["dependencyKind"] == "CICS_LINK")["callTarget"]["name"])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        damaged = copy.deepcopy(statement)
        next(item for item in damaged["resourceBindings"] if item["kind"] == "CICS_PROGRAM")["name"] = "PAYMORE"
        self.assertFalse(evidence.native_cics_statement_bound(damaged))
        for path in self.output.iterdir():
            if path.is_file():
                path.unlink()
        # A literal that reaches the LINK is the target; one without a CSD definition still blocks.
        result, exported = self.run_parser("           MOVE 'PAYMORE' TO WS-HELP-PGM.\n" + body,
                                           declarations=declarations, cics_resources=catalog)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("cics-program-definition-not-found:PAYMORE",
                      exported["dependencyDialectStatements"][-1]["bindingDiagnostics"])
        for path in self.output.iterdir():
            if path.is_file():
                path.unlink()
        result, exported = self.run_parser("           MOVE WS-TARGET TO WS-HELP-PGM.\n" + body,
                                           declarations=declarations, cics_resources=catalog)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("dynamic-cics-program-binding-unresolved:WS-HELP-PGM",
                      exported["dependencyDialectStatements"][-1]["bindingDiagnostics"])

    def test_native_cics_link_does_not_hide_an_incomplete_callee(self):
        result, exported = self.run_parser(
            "           EXEC CICS LINK PROGRAM('PAYHELP') END-EXEC.\n",
            callee_dialect="options",
            cics_resources="resourceDefinitions:\n  - program:\n"
                           "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
        self.assertNotEqual(0, result.returncode)
        self.assertFalse(exported["dependencyAnalysisComplete"])
        self.assertFalse(next(node for node in exported["dependencies"]
                              if node["dependencyKind"] == "CICS_LINK")["dependencyAnalysisComplete"])

    def test_native_cics_link_requires_a_real_program_catalog_even_when_source_exists(self):
        result, exported = self.run_parser("           EXEC CICS LINK PROGRAM('PAYHELP') END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("cics-program-definition-not-found:PAYHELP", "\n".join(exported["dependencyDiagnostics"]))

    def test_native_cics_program_dynamic_defaults_to_no_only_when_omitted(self):
        program = "resourceDefinitions:\n  - program:\n      name: PAYHELP\n      language: COBOL\n"
        cases = (
            ("", {"language": "COBOL", "dynamic": "NO", "dynamicProvenance": "cics-default"}, None),
            ("      dynamic: 'NO'\n", {"language": "COBOL", "dynamic": "NO"}, None),
            ("      dynamic: 'YES'\n", None, "Dynamic CICS program resources are not supported: PAYHELP"),
            ("      dynamic: 'MAYBE'\n", None, "Invalid CICS program DYNAMIC value"),
        )
        for dynamic, attributes, error in cases:
            with self.subTest(dynamic=dynamic.strip() or "omitted"):
                result, exported = self.run_parser(
                    "           EXEC CICS LINK PROGRAM('PAYHELP') END-EXEC.\n",
                    cics_resources=program + dynamic)
                if error:
                    self.assertNotEqual(0, result.returncode)
                    self.assertIn(error, result.stdout + result.stderr + "\n".join(exported.get("dependencyDiagnostics", [])))
                    continue
                self.assertEqual(0, result.returncode, result.stdout + result.stderr)
                binding = next(binding for statement in exported["dependencyDialectStatements"]
                               for binding in statement.get("resourceBindings", [])
                               if binding["kind"] == "CICS_PROGRAM")
                self.assertEqual(attributes, binding["attributes"])
                bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
                self.assertEqual([], bound["unresolvedDiagnostics"])

    def test_native_remote_cics_link_does_not_bind_the_local_program(self):
        result, exported = self.run_parser(
            "           EXEC CICS LINK PROGRAM('PAYHELP') SYSID('RMT1') END-EXEC.\n",
            cics_resources="resourceDefinitions:\n  - program:\n"
                           "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("cics-link-variant-unsupported", "\n".join(exported["dependencyDiagnostics"]))

    def test_native_cics_program_initial_value_requires_a_whole_program_no_write_proof(self):
        result, exported = self.run_parser(
            "           EXEC CICS LINK PROGRAM(WS-PGM)\n"
            "               COMMAREA(WS-TARGET) END-EXEC.\n",
            declarations="       01 WS-PGM PIC X(8) VALUE 'PAYHELP'.\n"
                         "       01 UNRELATED-PTR USAGE POINTER.\n",
            cics_resources="resourceDefinitions:\n  - program:\n"
                           "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_CICS"))
        link = next(statement for statement in exported["dependencyDialectStatements"]
                    if statement["operation"] == "LINK")
        program = next(binding for binding in link["resourceBindings"] if binding["kind"] == "CICS_PROGRAM")
        self.assertEqual("immutable-working-storage-literal", program["targetResolution"]["kind"])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])

    def test_native_mutated_cics_program_initial_value_is_not_a_static_target(self):
        result, exported = self.run_parser(
            "           MOVE 'OTHER' TO WS-PGM.\n"
            "           EXEC CICS LINK PROGRAM(WS-PGM)\n"
            "               COMMAREA(WS-TARGET) END-EXEC.\n",
            declarations="       01 WS-PGM PIC X(8) VALUE 'PAYHELP'.\n",
            cics_resources="resourceDefinitions:\n  - program:\n"
                           "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("dynamic-cics-program-binding-unresolved:WS-PGM", "\n".join(exported["dependencyDiagnostics"]))
        self.assertFalse(any(node["dependencyKind"] == "CICS_LINK" for node in exported["dependencies"]))

    def test_native_local_storage_program_literal_has_the_same_whole_program_no_write_proof(self):
        result, exported = self.run_parser(
            "           EXEC CICS LINK PROGRAM(WS-PGM)\n"
            "               COMMAREA(WS-TARGET) END-EXEC.\n",
            declarations="       LOCAL-STORAGE SECTION.\n"
                         "       01 WS-PGM PIC X(8) VALUE 'PAYHELP'.\n",
            cics_resources="resourceDefinitions:\n  - program:\n"
                           "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_CICS"))
        self.assertTrue(evidence.native_domain_evidence(exported, "FILE_IO"))
        link = next(statement for statement in exported["dependencyDialectStatements"]
                    if statement["operation"] == "LINK")
        program = next(binding for binding in link["resourceBindings"] if binding["kind"] == "CICS_PROGRAM")
        self.assertEqual("immutable-local-storage-literal", program["targetResolution"]["kind"])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])

    def test_native_local_storage_program_mutation_is_not_hidden_by_its_initial_value(self):
        result, exported = self.run_parser(
            "           MOVE 'OTHER' TO WS-PGM.\n"
            "           EXEC CICS LINK PROGRAM(WS-PGM) END-EXEC.\n",
            declarations="       LOCAL-STORAGE SECTION.\n"
                         "       01 WS-PGM PIC X(8) VALUE 'PAYHELP'.\n",
            cics_resources="resourceDefinitions:\n  - program:\n"
                           "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("dynamic-cics-program-binding-unresolved:WS-PGM", "\n".join(exported["dependencyDiagnostics"]))
        self.assertFalse(any(node["dependencyKind"] == "CICS_LINK" for node in exported["dependencies"]))

    def test_native_cics_program_written_through_sql_is_not_an_immutable_target(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :WS-PGM FROM CONTROL END-EXEC.\n"
            "           EXEC CICS LINK PROGRAM(WS-PGM) END-EXEC.\n",
            declarations="       01 WS-PGM PIC X(8) VALUE 'PAYHELP'.\n"
                         "           EXEC SQL DECLARE CONTROL TABLE (COL1 CHAR(8)) END-EXEC.\n",
            cics_resources="resourceDefinitions:\n  - program:\n"
                           "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("dynamic-cics-program-binding-unresolved:WS-PGM", "\n".join(exported["dependencyDiagnostics"]))

    def test_native_cics_program_address_escape_is_not_an_immutable_target(self):
        result, exported = self.run_parser(
            "           SET WS-PTR TO ADDRESS OF WS-PGM.\n"
            "           EXEC CICS LINK PROGRAM(WS-PGM) END-EXEC.\n",
            declarations="       01 WS-PGM PIC X(8) VALUE 'PAYHELP'.\n"
                         "       01 WS-PTR USAGE POINTER.\n",
            cics_resources="resourceDefinitions:\n  - program:\n"
                           "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("dynamic-cics-program-binding-unresolved:WS-PGM", "\n".join(exported["dependencyDiagnostics"]))

    def test_native_copy_and_static_call_export_are_hash_bound_to_actual_inputs(self):
        result, exported = self.run_parser("           CALL 'PAYHELP' USING PAY-AMOUNT.\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual(1, exported["dependencyExportSchemaVersion"])
        self.assertEqual(evidence.digest(self.source / "PILOT.cbl"), exported["sourceSha256"])
        nodes = evidence.exported_nodes(exported, "PILOT.cbl")
        copy = next(node for node in nodes if node["dependencyKind"] == "COPY")
        call = next(node for node in nodes if node["dependencyKind"] == "CALL")
        self.assertEqual("PAYREC.cpy", copy["callTarget"]["name"])
        self.assertEqual(evidence.digest(self.source / "PAYREC.cpy"), copy["sourceSha256"])
        self.assertEqual("/source/PAYREC.cpy", copy["sourcePath"])
        self.assertEqual("PAYHELP", call["callTarget"]["name"])
        self.assertEqual(evidence.digest(self.source / "PAYHELP.cbl"), call["sourceSha256"])
        self.assertEqual("complete", exported["dependencyCoverage"]["COPY"])
        self.assertEqual("complete", exported["dependencyCoverage"]["CALL"])
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), exported["dependencyCoverage"])
        self.assertEqual(evidence.ABSENCE_PROOFS, exported["dependencyAbsenceEvidence"])

    def test_native_dynamic_call_does_not_claim_resolved_static_coverage(self):
        result, exported = self.run_parser("           CALL WS-TARGET.\n")
        self.assertNotEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual("partial", exported["dependencyCoverage"]["CALL"])
        calls = [node for node in evidence.exported_nodes(exported, "PILOT.cbl")
                 if node["dependencyKind"] == "CALL"]
        self.assertEqual("DYNAMIC", calls[0]["callTarget"]["programReferenceType"])
        self.assertNotIn("sourceSha256", calls[0])

    def test_native_missing_copybook_is_failure_not_successful_empty_export(self):
        result, exported = self.run_parser("           DISPLAY 'KNOWN'.\n", missing=True)
        self.assertNotEqual(0, result.returncode)
        self.assertNotEqual("complete", exported["dependencyCoverage"]["COPY"])
        self.assertTrue(exported["dependencyDiagnostics"])
        self.assertFalse((self.source / "PAYREC.cpy").exists())

    def test_native_zero_reference_coverage_comes_from_actual_successful_parser(self):
        result, exported = self.run_parser("           DISPLAY 'NO DEPENDENCIES'.\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual([], exported["dependencies"])
        self.assertEqual("complete", exported["dependencyCoverage"]["COPY"])
        self.assertEqual("complete", exported["dependencyCoverage"]["CALL"])
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), exported["dependencyCoverage"])
        self.assertEqual(evidence.ABSENCE_PROOFS, exported["dependencyAbsenceEvidence"])

    def test_native_file_io_is_bound_presence_never_zero_file_io(self):
        result, exported = self.run_parser("           OPEN INPUT INPUT-FILE.\n           CLOSE INPUT-FILE.\n", file_io=True)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertNotIn("FILE_IO", exported["dependencyAbsenceEvidence"])
        self.assertEqual("antlr-tree:bound-file-control-resources", exported["dependencyPresenceEvidence"]["FILE_IO"])
        self.assertTrue(evidence.native_domain_evidence(exported, "FILE_IO"))

    def test_native_empty_file_section_is_not_a_file_dependency(self):
        result, exported = self.run_parser("           DISPLAY 'NO FILE OPERATIONS'.\n", empty_file_section=True)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), exported["dependencyCoverage"])
        self.assertEqual(evidence.ABSENCE_PROOFS, exported["dependencyAbsenceEvidence"])

    def test_native_file_declaration_without_io_statements_still_blocks_absence(self):
        result, exported = self.run_parser("           DISPLAY 'DECLARED FILE'.\n", file_io=True)
        self.assertNotEqual(0, result.returncode)
        self.assertNotEqual("complete", exported["dependencyCoverage"]["FILE_IO"])
        self.assertNotIn("FILE_IO", exported["dependencyAbsenceEvidence"])
        self.assertTrue(any("FileDescriptionEntryContext" in item for item in exported["dependencyDiagnostics"]))

    def test_native_bare_cics_return_has_positive_coverage_without_claiming_runtime_mapping(self):
        result, exported = self.run_parser("           EXEC CICS RETURN END-EXEC.\n", empty_file_section=True)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), exported["dependencyCoverage"])
        self.assertEqual("antlr-tree:no-sql-contexts", exported["dependencyAbsenceEvidence"]["EXEC_SQL"])
        self.assertNotIn("EXEC_CICS", exported["dependencyAbsenceEvidence"])
        self.assertEqual("antlr-tree:resource-free-cics-return",
                         exported["dependencyPresenceEvidence"]["EXEC_CICS"])
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_CICS"))
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_SQL"))
        statement, = exported["dependencyDialectStatements"]
        self.assertEqual("RETURN", statement["operation"])
        self.assertEqual("unverified", statement["runtimeMapping"])
        transfer, = exported["dependencyControlTransfers"]
        self.assertEqual("bare", transfer["variant"])
        self.assertEqual("expanded-parser-input", transfer["coordinateSpace"])
        self.assertIn("RETURN", transfer["sourceText"].upper())
        source_line = (self.source / "PILOT.cbl").read_text().splitlines()[int(transfer["line"]) - 1]
        self.assertEqual("RETURN", source_line[int(transfer["column"]):].split()[0])
        self.assertEqual("unverified", transfer["runtimeMapping"])
        self.assertEqual([], exported["dependencyDiagnostics"])
        self.assertFalse(any("FileSectionContext" in item for item in exported["dependencyDiagnostics"]))

    def test_native_optioned_cics_return_is_not_collapsed_to_bare_return(self):
        result, exported = self.run_parser("           EXEC CICS RETURN TRANSID('NEXT') END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        transfer, = exported["dependencyControlTransfers"]
        self.assertEqual("options", transfer["variant"])
        self.assertIn("TRANSID", transfer["sourceText"].upper())
        self.assertIn("NEXT", transfer["sourceText"])
        self.assertNotIn("EXEC_CICS", exported["dependencyAbsenceEvidence"])
        self.assertNotEqual("complete", exported["dependencyCoverage"]["EXEC_CICS"])
        self.assertEqual("antlr-tree:no-sql-contexts", exported["dependencyAbsenceEvidence"]["EXEC_SQL"])
        self.assertFalse(evidence.native_domain_evidence(exported, "EXEC_CICS"))

    def test_native_lowercase_return_retains_valid_positive_evidence(self):
        result, exported = self.run_parser("           exec cics return end-exec.\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_CICS"))

    def test_native_empty_cics_block_is_not_mistaken_for_cics_absence(self):
        result, exported = self.run_parser("           EXEC CICS END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        self.assertNotIn("EXEC_CICS", exported["dependencyAbsenceEvidence"])
        self.assertNotEqual("complete", exported["dependencyCoverage"]["EXEC_CICS"])
        self.assertTrue(exported["dependencyDialectStatements"])

    def test_native_cics_link_retains_its_grammar_operands_without_inventing_resource_bindings(self):
        result, exported = self.run_parser(
            "           EXEC CICS LINK PROGRAM('PAYHELP')\n"
            "               COMMAREA(WS-TARGET) LENGTH(8) END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        statement, = exported["dependencyDialectStatements"]
        self.assertEqual("EXEC_CICS", statement["kind"])
        self.assertEqual("LINK", statement["operation"])
        self.assertIn("PAYHELP", statement["sourceText"])
        self.assertIn("WS-TARGET", json.dumps(statement["syntax"]))
        self.assertEqual("unresolved", statement["bindingStatus"])
        self.assertEqual("complete", exported["dependencyCoverage"]["EXEC_SQL"])
        self.assertNotEqual("complete", exported["dependencyCoverage"]["EXEC_CICS"])

    def test_native_cics_write_binds_the_supplied_file_and_retains_runtime_requirement(self):
        result, exported = self.run_parser(
            "           EXEC CICS WRITE FILE('TESTFILE') FROM(WS-TARGET)\n"
            "               RESP(PAY-AMOUNT) END-EXEC.\n"
            "           IF PAY-AMOUNT NOT = DFHRESP(NORMAL)\n"
            "               EXEC CICS RETURN END-EXEC\n           END-IF.\n",
            cics_resources="resourceDefinitions:\n  - file:\n      name: TESTFILE\n"
                           "      dsname: TEST.DATA\n      recordsize: 80\n      keylength: 8\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), exported["dependencyCoverage"])
        self.assertNotIn("FILE_IO", exported["dependencyAbsenceEvidence"])
        self.assertTrue(evidence.native_domain_evidence(exported, "FILE_IO"))
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_CICS"))
        write = next(item for item in exported["dependencyDialectStatements"] if item["operation"] == "WRITE")
        binding, = [item for item in write["resourceBindings"] if item["kind"] == "CICS_FILE"]
        self.assertEqual("TESTFILE", binding["name"])
        self.assertEqual(evidence.digest(self.source / "resources.yaml"), binding["sourceSha256"])
        self.assertEqual("unverified", write["runtimeMapping"])

    def test_native_cics_write_rejects_missing_duplicate_dynamic_and_remote_file_bindings(self):
        resources = ("resourceDefinitions:\n  - file:\n      name: TESTFILE\n"
                     "      dsname: TEST.DATA\n      recordsize: 80\n      keylength: 8\n")
        for operand, suffix, catalog, expected in [
            ("'MISSING'", "", resources, "cics-file-definition-not-found:MISSING"),
            ("WS-TARGET", "", resources, "dynamic-cics-file-binding-unresolved:WS-TARGET"),
            ("'TESTFILE'", " SYSID(WS-TARGET)", resources, "remote-cics-file-binding-unresolved:"),
            ("'TESTFILE'", "", resources + resources.split("resourceDefinitions:\n")[1],
             "Ambiguous CICS file definition"),
        ]:
            with self.subTest(operand=operand, suffix=suffix):
                result, exported = self.run_parser(
                    f"           EXEC CICS WRITE FILE({operand}) FROM(WS-TARGET)\n"
                    f"               RESP(PAY-AMOUNT){suffix} END-EXEC.\n", cics_resources=catalog)
                self.assertNotEqual(0, result.returncode)
                self.assertTrue(any(expected in issue for issue in exported["dependencyDiagnostics"]),
                                exported["dependencyDiagnostics"])
                self.assertNotEqual("complete", exported["dependencyCoverage"]["FILE_IO"])
                for path in self.output.iterdir():
                    if path.is_file():
                        path.unlink()

    # IDCAMS as shipped with CardDemo: base KSDS clusters plus an alternate-index path.
    IDCAMS = (
        "//PILOTV   JOB 'DEFINE PILOT FILES'\n"
        "//STEP10   EXEC PGM=IDCAMS\n"
        "//SYSPRINT DD   SYSOUT=*\n"
        "//SYSIN    DD   *\n"
        "   DEFINE CLUSTER (NAME(PILOT.ACCT.KSDS) -\n"
        "          KEYS(11 0) RECORDSIZE(300 300) INDEXED) -\n"
        "          DATA (NAME(PILOT.ACCT.KSDS.DATA))\n"
        "   DEFINE CLUSTER (NAME(PILOT.CARD.KSDS) KEYS(16 0) -\n"
        "          RECORDSIZE(150 150) INDEXED)\n"
        "   DEFINE ALTERNATEINDEX (NAME(PILOT.CARD.AIX) -\n"
        "          RELATE(PILOT.CARD.KSDS) KEYS(11 16) NONUNIQUEKEY)\n"
        "   DEFINE PATH (NAME(PILOT.CARD.AIX.PATH) PATHENTRY(PILOT.CARD.AIX))\n"
        "/*\n")
    FILE_CSD = (
        " DEFINE FILE(ACCTDAT) GROUP(PILOT)\n"
        "        DSNAME(PILOT.ACCT.KSDS) RECORDFORMAT(V) ADD(YES)\n"
        "        BROWSE(YES) DELETE(YES) READ(YES) UPDATE(YES)\n"
        " DEFINE FILE(CARDAIX) GROUP(PILOT)\n"
        "        DSNAME(PILOT.CARD.AIX.PATH) BROWSE(YES) READ(YES)\n")
    FILE_DECLARATIONS = ("       01 WS-REC PIC X(300).\n       01 WS-KEY PIC X(16).\n"
                         "       01 LIT-PGM PIC X(8) VALUE 'PAYHELP'.\n")
    FILE_BODY = (
        "           EXEC CICS READ DATASET('ACCTDAT') INTO(WS-REC)\n"
        "               RIDFLD(WS-KEY) KEYLENGTH(11) UPDATE END-EXEC.\n"
        "           EXEC CICS REWRITE DATASET('ACCTDAT') FROM(WS-REC) END-EXEC.\n"
        "           EXEC CICS WRITE DATASET('ACCTDAT') FROM(WS-REC)\n"
        "               RIDFLD(WS-KEY) END-EXEC.\n"
        "           EXEC CICS DELETE DATASET('ACCTDAT') END-EXEC.\n"
        "           EXEC CICS STARTBR FILE('CARDAIX') RIDFLD(WS-KEY)\n"
        "               GTEQ END-EXEC.\n"
        "           EXEC CICS READNEXT FILE('CARDAIX') INTO(WS-REC)\n"
        "               RIDFLD(WS-KEY) END-EXEC.\n"
        "           EXEC CICS READPREV FILE('CARDAIX') INTO(WS-REC)\n"
        "               RIDFLD(WS-KEY) END-EXEC.\n"
        "           EXEC CICS ENDBR FILE('CARDAIX') END-EXEC.\n"
        "           EXEC CICS XCTL PROGRAM(LIT-PGM) END-EXEC.\n")

    def test_native_csd_files_bind_supplied_idcams_definitions_and_xctl_constants(self):
        result, exported = self.run_parser(
            self.FILE_BODY, declarations=self.FILE_DECLARATIONS, cics_resources=self.CONSTANT_CSD + self.FILE_CSD,
            cics_resources_file="PILOT.CSD", vsam_definitions=[self.IDCAMS])
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statements = exported["dependencyDialectStatements"]
        self.assertEqual(["READ", "REWRITE", "WRITE", "DELETE", "STARTBR", "READNEXT", "READPREV", "ENDBR", "XCTL"],
                         [statement["operation"] for statement in statements])
        self.assertTrue(all(evidence.native_cics_statement_bound(statement) for statement in statements))
        files = {binding["name"]: binding for statement in statements
                 for binding in statement["resourceBindings"] if binding["kind"] == "CICS_FILE"}
        self.assertEqual((300, 11, 0, "base-cluster"),
                         tuple(files["ACCTDAT"]["attributes"][key]
                               for key in ("recordsize", "keylength", "keyOffset", "accessPath")))
        self.assertEqual((150, 11, 16, False, "alternate-index-path"),
                         tuple(files["CARDAIX"]["attributes"][key]
                               for key in ("recordsize", "keylength", "keyOffset", "uniqueKeys", "accessPath")))
        program = next(item for item in statements[-1]["resourceBindings"] if item["kind"] == "CICS_PROGRAM")
        self.assertEqual(("PAYHELP", "transfer-control"), (program["name"], program["controlEffect"]))
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        self.assertEqual({"PILOT.ACCT.KSDS", "PILOT.CARD.KSDS", "PILOT.CARD.AIX", "PILOT.CARD.AIX.PATH"},
                         {item["name"] for item in bound["definitions"] if item["kind"] == "CICS_DATASET"})
        for damage in ("keylength", "service", "dataset-source", "xctl-effect"):
            with self.subTest(damage=damage):
                document = copy.deepcopy(exported)
                statements = document["dependencyDialectStatements"]
                if damage == "keylength":
                    next(binding for binding in statements[0]["resourceBindings"]
                         if binding["kind"] == "CICS_FILE")["attributes"]["keylength"] = 301
                elif damage == "service":
                    next(binding for binding in statements[3]["resourceBindings"]
                         if binding["kind"] == "CICS_FILE")["attributes"]["services"]["delete"] = "NO"
                elif damage == "dataset-source":
                    next(binding for binding in statements[4]["resourceBindings"]
                         if binding["kind"] == "CICS_FILE")["datasetDefinitions"].pop()
                else:
                    next(binding for binding in statements[-1]["resourceBindings"]
                         if binding["kind"] == "CICS_PROGRAM")["controlEffect"] = "call"
                self.assertFalse(all(evidence.native_cics_statement_bound(statement) for statement in statements))

    def test_native_csd_files_without_matching_idcams_or_services_stay_unresolved(self):
        conflicting = self.IDCAMS.replace("/*\n", "   DEFINE CLUSTER (NAME(PILOT.ACCT.KSDS) -\n"
                                                 "          KEYS(11 0) RECORDSIZE(200 300))\n/*\n")
        no_delete = self.FILE_CSD.replace("DELETE(YES)", "DELETE(NO)")
        for name, body, csd, idcams, expected in [
            ("no-idcams", "           EXEC CICS ENDBR FILE('CARDAIX') END-EXEC.\n", self.FILE_CSD, [],
             "CICS file dataset has no supplied IDCAMS definition"),
            ("conflicting", "           EXEC CICS DELETE DATASET('ACCTDAT') END-EXEC.\n", self.FILE_CSD,
             [conflicting], "Conflicting IDCAMS CLUSTER definitions"),
            ("service", "           EXEC CICS DELETE DATASET('ACCTDAT') END-EXEC.\n", no_delete,
             [self.IDCAMS], "cics-file-operation-not-permitted:ACCTDAT:delete"),
            ("alternate-write", "           EXEC CICS WRITE FILE('CARDAIX') FROM(WS-REC)\n"
                                "               RIDFLD(WS-KEY) END-EXEC.\n", self.FILE_CSD,
             [self.IDCAMS], "cics-file-operation-not-permitted:CARDAIX:add"),
            ("dynamic-xctl", "           MOVE 'OTHER' TO LIT-PGM.\n"
                             "           EXEC CICS XCTL PROGRAM(LIT-PGM) END-EXEC.\n", self.FILE_CSD,
             [self.IDCAMS], "cics-program-definition-not-found:OTHER"),
        ]:
            with self.subTest(name=name):
                result, exported = self.run_parser(
                    body, declarations=self.FILE_DECLARATIONS, cics_resources=self.CONSTANT_CSD + csd,
                    cics_resources_file="PILOT.CSD", vsam_definitions=idcams)
                self.assertNotEqual(0, result.returncode)
                diagnostics = [diagnostic for statement in exported["dependencyDialectStatements"]
                               for diagnostic in statement.get("bindingDiagnostics", [])]
                self.assertTrue(any(expected in diagnostic for diagnostic in diagnostics), diagnostics)
                self.assertFalse(evidence.native_domain_evidence(exported, "EXEC_CICS"))

    MENU_DECLARATIONS = (
        "       01 MENU-OPTIONS.\n"
        "         05 MENU-DATA.\n"
        "           10 FILLER PIC 9(02) VALUE 1.\n"
        "           10 FILLER PIC X(08) VALUE 'PAYHELP'.\n"
        "           10 FILLER PIC 9(02) VALUE 2.\n"
        "           10 FILLER PIC X(08) VALUE 'PAYMORE'.\n"
        "         05 MENU-TABLE REDEFINES MENU-DATA.\n"
        "           10 MENU-OPT OCCURS 3 TIMES.\n"
        "             15 MENU-NUM PIC 9(02).\n"
        "             15 MENU-PGM PIC X(08).\n"
        "       01 WS-OPTION PIC 9(02) VALUE 1.\n")
    MENU_BODY = (
        "           IF MENU-PGM(WS-OPTION) = 'PAYHELP'\n"
        "               EXEC CICS INQUIRE PROGRAM(MENU-PGM(WS-OPTION))\n"
        "                   NOHANDLE END-EXEC\n"
        "           END-IF.\n"
        "           EXEC CICS XCTL PROGRAM(MENU-PGM(WS-OPTION)) END-EXEC.\n")
    MENU_CSD = " DEFINE PROGRAM(PAYMORE) GROUP(PILOT)\n        RELOAD(NO) STATUS(ENABLED) DYNAMIC(NO)\n"

    def test_native_read_only_redefined_menu_tables_bind_every_defined_program(self):
        (self.source / "PAYMORE.cbl").write_text(
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYMORE.\n"
            "       PROCEDURE DIVISION.\n           GOBACK.\n")
        result, exported = self.run_parser(self.MENU_BODY, declarations=self.MENU_DECLARATIONS,
                                           cics_resources=self.CONSTANT_CSD + self.MENU_CSD,
                                           cics_resources_file="PILOT.CSD")
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statements = exported["dependencyDialectStatements"]
        self.assertEqual(["INQUIRE", "XCTL"], [statement["operation"] for statement in statements])
        self.assertTrue(all(evidence.native_cics_statement_bound(statement) for statement in statements))
        for statement, effect in zip(statements, ("program-definition-inquiry", "transfer-control")):
            programs = [item for item in statement["resourceBindings"] if item["kind"] == "CICS_PROGRAM"]
            self.assertEqual(["PAYHELP", "PAYMORE"], [item["name"] for item in programs])
            self.assertEqual({effect}, {item["controlEffect"] for item in programs})
            self.assertEqual(["PAYHELP", "PAYMORE", None], programs[0]["targetResolution"]["occurrenceValues"])
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        evidence.write_json(self.output / "PILOT.cbl.report/dependency-evidence.json", bound)
        self.check_terminal_gate(True, "menu")
        for damage in ("occurrence", "dropped-target", "value-syntax", "subscript"):
            with self.subTest(damage=damage):
                document = copy.deepcopy(exported)
                statement = document["dependencyDialectStatements"][-1]
                programs = [item for item in statement["resourceBindings"] if item["kind"] == "CICS_PROGRAM"]
                if damage == "occurrence":
                    for item in programs:
                        item["targetResolution"]["occurrenceValues"][2] = "PAYHELP"
                elif damage == "dropped-target":
                    statement["resourceBindings"].remove(programs[-1])
                elif damage == "value-syntax":
                    for item in programs:
                        item["targetResolution"]["valueSyntax"].pop()
                else:
                    next(item for item in statement["resourceBindings"]
                         if item["kind"] == "CICS_DATA").pop("subscriptSyntax")
                self.assertFalse(evidence.native_cics_statement_bound(statement))
        for name, change in [("table-write", "           MOVE SPACES TO MENU-DATA.\n"),
                             ("member-write", "           MOVE 'OTHER' TO MENU-PGM(2).\n")]:
            with self.subTest(name=name):
                for path in self.output.iterdir():
                    if path.is_file():
                        path.unlink()
                result, exported = self.run_parser(change + self.MENU_BODY, declarations=self.MENU_DECLARATIONS,
                                                   cics_resources=self.CONSTANT_CSD + self.MENU_CSD,
                                                   cics_resources_file="PILOT.CSD")
                self.assertNotEqual(0, result.returncode)
                diagnostics = [diagnostic for statement in exported["dependencyDialectStatements"]
                               for diagnostic in statement.get("bindingDiagnostics", [])]
                self.assertIn("dynamic-cics-program-binding-unresolved:MENU-PGM(WS-OPTION)", diagnostics)

    REACHING_DECLARATIONS = (
        "       01 WS-COMMAREA.\n"
        "         05 WS-INFO.\n"
        "           10 WS-TO-PGM PIC X(08).\n"
        "           10 WS-FLAG PIC X.\n"
        "       01 WS-OTHER PIC X(08).\n"
        "       01 WS-SIGNON PIC X(08) VALUE 'PAYMORE'.\n")
    REACHING_BODY = (
        "           IF WS-FLAG = 'Y'\n"
        "               MOVE 'PAYHELP' TO WS-TO-PGM\n"
        "               PERFORM GO-PREV\n"
        "           END-IF\n"
        "           MOVE WS-SIGNON TO WS-TO-PGM\n"
        "           PERFORM GO-PREV\n"
        "           EXEC CICS RETURN END-EXEC.\n"
        "       GO-PREV.\n"
        "           IF WS-TO-PGM = SPACES\n"
        "               MOVE 'PAYHELP' TO WS-TO-PGM\n"
        "           END-IF\n"
        "           EXEC CICS XCTL PROGRAM(WS-TO-PGM)\n"
        "               COMMAREA(WS-COMMAREA) END-EXEC.\n")

    def test_native_xctl_binds_every_structured_reaching_assignment(self):
        (self.source / "PAYMORE.cbl").write_text(
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYMORE.\n"
            "       PROCEDURE DIVISION.\n           GOBACK.\n")
        result, exported = self.run_parser(self.REACHING_BODY, declarations=self.REACHING_DECLARATIONS,
                                           cics_resources=self.CONSTANT_CSD + self.MENU_CSD,
                                           cics_resources_file="PILOT.CSD")
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statement = exported["dependencyDialectStatements"][-1]
        self.assertEqual("XCTL", statement["operation"])
        self.assertTrue(evidence.native_cics_statement_bound(statement))
        programs = [item for item in statement["resourceBindings"] if item["kind"] == "CICS_PROGRAM"]
        self.assertEqual(["PAYHELP", "PAYMORE"], [item["name"] for item in programs])
        proof = programs[0]["targetResolution"]
        self.assertEqual("structured-reaching-assignments", proof["kind"])
        self.assertFalse(proof["initialReaches"])
        self.assertEqual(3, len(proof["assignments"]))
        bound = evidence.build_evidence(self.source, "PILOT.cbl", self.output, 0, parser_source_root="/source")
        self.assertEqual([], bound["unresolvedDiagnostics"])
        for damage in ("values", "assignment", "constant"):
            with self.subTest(damage=damage):
                document = copy.deepcopy(exported)
                statement = document["dependencyDialectStatements"][-1]
                for item in statement["resourceBindings"]:
                    if item["kind"] != "CICS_PROGRAM":
                        continue
                    if damage == "values":
                        item["targetResolution"]["reachingValues"].reverse()
                    elif damage == "assignment":
                        item["targetResolution"]["assignments"].pop(1)
                    else:
                        item["targetResolution"]["assignments"][1].pop("sourceResolution")
                self.assertFalse(evidence.native_cics_statement_bound(statement))
        for name, change in [("dynamic-source", "           MOVE WS-OTHER TO WS-TO-PGM\n"),
                             ("group-write", "           MOVE WS-OTHER TO WS-INFO\n")]:
            with self.subTest(name=name):
                for path in self.output.iterdir():
                    if path.is_file():
                        path.unlink()
                body = self.REACHING_BODY.replace("           PERFORM GO-PREV\n           EXEC CICS RETURN",
                                                  change + "           PERFORM GO-PREV\n           EXEC CICS RETURN")
                result, exported = self.run_parser(body, declarations=self.REACHING_DECLARATIONS,
                                                   cics_resources=self.CONSTANT_CSD + self.MENU_CSD,
                                                   cics_resources_file="PILOT.CSD")
                self.assertNotEqual(0, result.returncode)
                diagnostics = [diagnostic for statement in exported["dependencyDialectStatements"]
                               for diagnostic in statement.get("bindingDiagnostics", [])]
                self.assertIn("dynamic-cics-program-binding-unresolved:WS-TO-PGM", diagnostics)

    def test_native_xctl_falls_back_to_every_slot_write_when_control_flow_is_unstructured(self):
        (self.source / "PAYMORE.cbl").write_text(
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYMORE.\n"
            "       PROCEDURE DIVISION.\n           GOBACK.\n")
        body = self.REACHING_BODY.replace("           PERFORM GO-PREV\n           EXEC CICS RETURN",
                                          "           GO TO GO-PREV\n           PERFORM GO-PREV\n           EXEC CICS RETURN")
        result, exported = self.run_parser(body, declarations=self.REACHING_DECLARATIONS,
                                           cics_resources=self.CONSTANT_CSD + self.MENU_CSD,
                                           cics_resources_file="PILOT.CSD")
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statement = exported["dependencyDialectStatements"][-1]
        programs = [item for item in statement["resourceBindings"] if item["kind"] == "CICS_PROGRAM"]
        self.assertEqual({"PAYHELP", "PAYMORE"}, {item["name"] for item in programs})
        self.assertEqual("flow-insensitive-slot-writes", programs[0]["targetResolution"]["kind"])
        self.assertTrue(evidence.native_cics_statement_bound(statement))
        for damage in ("extra-value", "dropped-write"):
            with self.subTest(damage=damage):
                document = copy.deepcopy(exported)
                statement = document["dependencyDialectStatements"][-1]
                for item in statement["resourceBindings"]:
                    if item["kind"] != "CICS_PROGRAM":
                        continue
                    if damage == "extra-value":
                        item["targetResolution"]["universe"].append("OTHER")
                    else:
                        writes = item["targetResolution"]["slots"]["WS-TO-PGM"]["writes"]
                        writes.remove(next(write for write in writes if write.get("value") == "PAYMORE"))
                self.assertFalse(evidence.native_cics_statement_bound(statement))

    def test_native_xctl_of_an_incoming_commarea_slot_binds_only_through_the_estate_closure(self):
        declarations = self.REACHING_DECLARATIONS + "       LINKAGE SECTION.\n       01 DFHCOMMAREA PIC X(100).\n"
        body = ("           MOVE DFHCOMMAREA(1:EIBCALEN) TO WS-COMMAREA\n"
                "           EXEC CICS XCTL PROGRAM(WS-TO-PGM)\n"
                "               COMMAREA(WS-COMMAREA) END-EXEC.\n")
        result, exported = self.run_parser(body, declarations=declarations,
                                           cics_resources=self.CONSTANT_CSD, cics_resources_file="PILOT.CSD")
        self.assertNotEqual(0, result.returncode)
        diagnostics = [diagnostic for statement in exported["dependencyDialectStatements"]
                       for diagnostic in statement.get("bindingDiagnostics", [])]
        self.assertIn("cics-commarea-closure-pending:WS-TO-PGM", diagnostics)
        record = exported["commareaInventory"]["records"][0]
        self.assertEqual(("WS-COMMAREA", True), (record["name"], record["loaded"]))
        self.assertTrue(record["slots"]["WS-TO-PGM"]["entry"])
        closure = {"schema": 1, "programs": {exported["sourceSha256"]: {
            "relativePath": "PILOT.cbl", "slots": {"WS-TO-PGM": [None, "PAYHELP"]}}}}
        (self.output / "closure.json").write_text(json.dumps(closure))
        result, exported = self.run_parser(body, declarations=declarations,
                                           cics_resources=self.CONSTANT_CSD, cics_resources_file="PILOT.CSD",
                                           environment={"REKT_COMMAREA_CLOSURE": "/output/closure.json"})
        self.assertEqual(0, result.returncode, exported.get("dependencyDiagnostics") or result.stdout + result.stderr)
        statement = exported["dependencyDialectStatements"][-1]
        programs = [item for item in statement["resourceBindings"] if item["kind"] == "CICS_PROGRAM"]
        self.assertEqual(["PAYHELP"], [item["name"] for item in programs])
        self.assertEqual("closed-world-commarea-slot", programs[0]["targetResolution"]["kind"])
        self.assertTrue(evidence.native_cics_statement_bound(statement))
        tampered = copy.deepcopy(statement)
        for item in tampered["resourceBindings"]:
            if item["kind"] == "CICS_PROGRAM":
                item["targetResolution"]["slots"]["WS-TO-PGM"]["entry"] = False
        self.assertFalse(evidence.native_cics_statement_bound(tampered))

    def test_native_sql_cursor_declaration_is_retained_from_data_division(self):
        result, exported = self.run_parser(
            "           EXEC SQL OPEN TEST-CURSOR END-EXEC.\n",
            declarations="           EXEC SQL DECLARE TEST-CURSOR CURSOR FOR\n"
                         "               SELECT 1 FROM TESTSCHEMA.UNDECLARED END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        statements = exported["dependencyDialectStatements"]
        self.assertEqual(["DECLARE", "OPEN"], [statement["operation"] for statement in statements])
        self.assertIn("TESTSCHEMA.UNDECLARED", statements[0]["sourceText"])
        self.assertFalse(any("parser-diagnostic:" in issue for issue in exported["dependencyDiagnostics"]),
                         exported["dependencyDiagnostics"])

    def test_native_sql_binds_declared_table_cursor_and_host_variable_without_runtime_claim(self):
        result, exported = self.run_parser(
            "           EXEC SQL OPEN TEST-CURSOR END-EXEC.\n"
            "           EXEC SQL FETCH TEST-CURSOR INTO :WS-TARGET END-EXEC.\n"
            "           EXEC SQL CLOSE TEST-CURSOR END-EXEC.\n"
            "           EXEC SQL COMMIT WORK END-EXEC.\n",
            declarations="           EXEC SQL DECLARE TESTTAB TABLE\n"
                         "               (COL1 CHAR(8)) END-EXEC.\n"
                         "           EXEC SQL DECLARE TEST-CURSOR CURSOR FOR\n"
                         "               SELECT COL1 FROM TESTTAB\n"
                         "               WHERE COL1 = :WS-TARGET END-EXEC.\n")
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertEqual(dict.fromkeys(evidence.KINDS, "complete"), exported["dependencyCoverage"])
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_SQL"))
        self.assertTrue(evidence.native_domain_evidence(exported, "FILE_IO"))
        self.assertNotIn("EXEC_SQL", exported["dependencyAbsenceEvidence"])
        self.assertNotIn("FILE_IO", exported["dependencyAbsenceEvidence"])
        self.assertEqual("antlr-tree:bound-sql-resources", exported["dependencyPresenceEvidence"]["FILE_IO"])
        statements = exported["dependencyDialectStatements"]
        self.assertEqual(6, len(statements))
        self.assertTrue(all(statement["runtimeMapping"] == "unverified" for statement in statements))
        for change in ("omit-table-declaration", "wrong-binding", "omit-host-reference"):
            tampered = json.loads(json.dumps(exported))
            if change == "omit-table-declaration":
                tampered["dependencyDialectStatements"].pop(0)
            elif change == "wrong-binding":
                tampered["dependencyDialectStatements"][1]["resourceBindings"][0]["name"] = "OTHER"
            else:
                tampered["dependencyDialectStatements"][3]["resourceReferences"] = []
            self.assertFalse(evidence.native_domain_evidence(tampered, "EXEC_SQL"), change)
            self.assertFalse(evidence.native_domain_evidence(tampered, "FILE_IO"), change)

    def test_native_sql_duplicate_table_declarations_remain_ambiguous(self):
        declaration = ("           EXEC SQL DECLARE TESTTAB TABLE\n"
                       "               (COL1 CHAR(8)) END-EXEC.\n")
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET\n"
            "               FROM TESTTAB END-EXEC.\n", declarations=declaration * 2)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("sql-declaration-ambiguous:SQL_RELATION:TESTTAB",
                      "\n".join(exported["dependencyDiagnostics"]))
        self.assertFalse(evidence.native_domain_evidence(exported, "EXEC_SQL"))

    def test_native_sql_unknown_host_and_implicit_schema_are_not_silently_bound(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :MISSING-HOST\n"
            "               FROM TESTTAB END-EXEC.\n",
            declarations="           EXEC SQL DECLARE OWNER.TESTTAB TABLE\n"
                         "               (COL1 CHAR(8)) END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        diagnostics = "\n".join(exported["dependencyDiagnostics"])
        self.assertIn("sql-declaration-not-found:SQL_RELATION:TESTTAB", diagnostics)
        self.assertIn("sql-declaration-not-found:SQL_HOST_VARIABLE:MISSING-HOST", diagnostics)
        self.assertFalse(evidence.native_domain_evidence(exported, "EXEC_SQL"))

    def test_native_sql_external_function_cannot_hide_behind_known_local_table_and_host(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT MYFUNC(COL1) INTO :WS-TARGET\n"
            "               FROM TESTTAB END-EXEC.\n",
            declarations="           EXEC SQL DECLARE TESTTAB TABLE\n"
                         "               (COL1 CHAR(8)) END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        self.assertTrue(any("SQL_FUNCTION:MYFUNC" in issue for issue in exported["dependencyDiagnostics"]),
                        exported["dependencyDiagnostics"])
        self.assertFalse(evidence.native_domain_evidence(exported, "EXEC_SQL"))

    def test_native_sql_builtin_types_and_functions_are_not_external_dependencies(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT ABS(ABS) INTO :PAY-AMOUNT\n"
            "               FROM TESTTAB END-EXEC.\n",
            declarations="           EXEC SQL DECLARE TESTTAB TABLE\n"
                         "               (ABS INTEGER, COL2 SMALLINT,\n"
                         "                COL3 DATE) END-EXEC.\n")
        self.assertEqual(0, result.returncode, exported["dependencyDiagnostics"])
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_SQL"))
        references = [reference for statement in exported["dependencyDialectStatements"]
                      for reference in statement["resourceReferences"]]
        self.assertFalse(any(reference["kind"] in ("SQL_TYPE", "SQL_FUNCTION") for reference in references))

    def test_native_sql_documented_db2_zos_builtin_outside_grammar_is_not_external(self):
        declarations = ("           EXEC SQL DECLARE TESTTAB TABLE\n"
                        "               (COL1 TIMESTAMP) END-EXEC.\n")
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET FROM TESTTAB\n"
            "               WHERE COL1 = TIMESTAMP_FORMAT(:WS-TARGET,\n"
            "               'YYYY-MM-DD') END-EXEC.\n", declarations=declarations)
        self.assertEqual(0, result.returncode, exported["dependencyDiagnostics"])
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_SQL"))
        for path in self.output.iterdir():
            if path.is_file():
                path.unlink()
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET FROM TESTTAB\n"
            "               WHERE COL1 = TIMESTAMP_FORMATX(:WS-TARGET,\n"
            "               'YYYY-MM-DD') END-EXEC.\n", declarations=declarations)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("sql-declaration-not-found:SQL_FUNCTION:TIMESTAMP_FORMATX",
                      "\n".join(exported["dependencyDiagnostics"]))

    def test_native_sql_qualified_db2_catalog_dummy_table_is_not_an_estate_relation(self):
        declarations = ("           EXEC SQL DECLARE TESTTAB TABLE\n"
                        "               (COL1 CHAR(8)) END-EXEC.\n")
        result, exported = self.run_parser(
            "           EXEC SQL SELECT CURRENT DATE INTO :WS-TARGET\n"
            "               FROM SYSIBM.SYSDUMMY1 END-EXEC.\n", declarations=declarations)
        self.assertEqual(0, result.returncode, exported["dependencyDiagnostics"])
        self.assertTrue(evidence.native_domain_evidence(exported, "EXEC_SQL"))
        for path in self.output.iterdir():
            if path.is_file():
                path.unlink()
        result, exported = self.run_parser(
            "           EXEC SQL SELECT CURRENT DATE INTO :WS-TARGET\n"
            "               FROM SYSDUMMY1 END-EXEC.\n", declarations=declarations)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("sql-declaration-not-found:SQL_RELATION:SYSDUMMY1",
                      "\n".join(exported["dependencyDiagnostics"]))

    def test_native_sql_custom_type_is_not_mistaken_for_a_builtin(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET\n"
            "               FROM TESTTAB END-EXEC.\n",
            declarations="           EXEC SQL DECLARE TESTTAB TABLE\n"
                         "               (COL1 MONEY) END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("sql-declaration-not-found:SQL_TYPE:MONEY",
                      "\n".join(exported["dependencyDiagnostics"]))
        self.assertFalse(evidence.native_domain_evidence(exported, "EXEC_SQL"))

    def test_native_sql_invalid_query_keeps_original_parser_failure(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT 1 INTO :WS-TARGET FROM END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        self.assertFalse(exported["dependencyAnalysisComplete"])
        self.assertTrue(any(issue.startswith("parser-diagnostic:")
                            for issue in exported["dependencyDiagnostics"]), exported["dependencyDiagnostics"])
        self.assertFalse(evidence.native_domain_evidence(exported, "EXEC_SQL"))

    def test_native_sql_retains_query_and_host_variable_syntax_without_claiming_cics_presence(self):
        result, exported = self.run_parser(
            "           EXEC SQL SELECT 1 INTO :WS-TARGET\n"
            "               FROM TESTSCHEMA.UNDECLARED END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        statement, = exported["dependencyDialectStatements"]
        self.assertEqual("EXEC_SQL", statement["kind"])
        syntax = json.dumps(statement["syntax"])
        self.assertIn("WS-TARGET", syntax)
        self.assertIn("UNDECLARED", syntax)
        self.assertEqual("unresolved", statement["bindingStatus"])
        self.assertEqual("complete", exported["dependencyCoverage"]["EXEC_CICS"])
        self.assertEqual("antlr-tree:no-cics-contexts", exported["dependencyAbsenceEvidence"]["EXEC_CICS"])
        self.assertNotEqual("complete", exported["dependencyCoverage"]["EXEC_SQL"])

    def test_native_mixed_dialects_and_repeated_returns_are_not_lost_or_deduplicated_by_text(self):
        result, exported = self.run_parser(
            "           EXEC CICS RETURN END-EXEC.\n"
            "           EXEC SQL SELECT 1 INTO :WS-TARGET\n"
            "               FROM TESTSCHEMA.UNDECLARED END-EXEC.\n"
            "           EXEC CICS RETURN END-EXEC.\n")
        self.assertNotEqual(0, result.returncode)
        statements = exported["dependencyDialectStatements"]
        self.assertEqual(["EXEC_CICS", "EXEC_SQL", "EXEC_CICS"], [s["kind"] for s in statements])
        self.assertNotEqual(statements[0]["line"], statements[2]["line"])
        self.assertNotIn("EXEC_SQL", exported["dependencyAbsenceEvidence"])
        self.assertNotIn("EXEC_CICS", exported["dependencyAbsenceEvidence"])

    def test_native_resource_free_return_in_callee_keeps_dependency_coverage_complete(self):
        result, exported = self.run_parser("           CALL 'PAYHELP' USING PAY-AMOUNT.\n", callee_dialect=True)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        child = next(node for node in exported["dependencies"] if node["dependencyKind"] == "CALL")
        self.assertTrue(evidence.native_domain_evidence(child, "EXEC_CICS"))
        self.assertTrue(exported["dependencyAnalysisComplete"])

    def test_native_cics_return_does_not_hide_real_file_operations(self):
        result, exported = self.run_parser(
            "           OPEN INPUT INPUT-FILE.\n           CLOSE INPUT-FILE.\n"
            "           EXEC CICS RETURN END-EXEC.\n", file_io=True)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertNotIn("FILE_IO", exported["dependencyAbsenceEvidence"])
        self.assertEqual(1, len(exported["dependencyControlTransfers"]))
        self.assertEqual(["INPUT-FILE", "INPUT-FILE"], [operand for statement in exported["dependencyDialectStatements"]
                                                        for operand in statement.get("fileOperands", [])])
        self.assertTrue(evidence.native_domain_evidence(exported, "FILE_IO"))

    def test_native_missing_callee_propagates_incomplete_status_to_root(self):
        result, exported = self.run_parser("           CALL 'PAYHELP' USING PAY-AMOUNT.\n", callee=False)
        self.assertNotEqual(0, result.returncode)
        self.assertFalse(exported["dependencyAnalysisComplete"])
        self.assertEqual("partial", exported["dependencyCoverage"]["CALL"])
        self.assertTrue(any("incomplete-callee:PAYHELP" in item for item in exported["dependencyDiagnostics"]))

    def test_native_unsupported_callee_domain_cannot_be_hidden_by_clean_parent(self):
        result, exported = self.run_parser("           CALL 'PAYHELP' USING PAY-AMOUNT.\n", callee_dialect="options")
        self.assertNotEqual(0, result.returncode)
        self.assertFalse(exported["dependencyAnalysisComplete"])
        self.assertNotEqual("complete", exported["dependencyCoverage"]["EXEC_CICS"])
        self.assertTrue(any("callee:PAYHELP:" in item for item in exported["dependencyDiagnostics"]))


if __name__ == "__main__":
    unittest.main()
