#!/usr/bin/env python3
import json
import os
import shutil
import subprocess
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "tools" / "preprocess-for-rekt.sh"
WORK_ROOT = REPO_ROOT / "tools" / "tests" / "_work"
# Mirrors StubCopybookCatalog.Marker and REKT_STUB_MARKER in doctor.sh.
MARKER = "AUTO-GENERATED STUB COPYBOOK"


class PreprocessForRektTests(unittest.TestCase):
    maxDiff = None

    def setUp(self):
        WORK_ROOT.mkdir(parents=True, exist_ok=True)
        self.work_dir = WORK_ROOT / self._testMethodName
        if self.work_dir.exists():
            shutil.rmtree(self.work_dir)
        self.work_dir.mkdir()
        self.source_dir = self.work_dir / "source with 'quotes and spaces"
        self.source_dir.mkdir()

    def tearDown(self):
        shutil.rmtree(self.work_dir, ignore_errors=True)
        if WORK_ROOT.exists() and not any(WORK_ROOT.iterdir()):
            WORK_ROOT.rmdir()

    def run_preprocessor(self, stubs=False, rules=None, shadowed=None):
        env = os.environ.copy()
        if shadowed is not None:
            env["REKT_SHADOWED_COPYBOOKS_FILE"] = str(shadowed)
        env["REKT_NO_STUB_COPYBOOKS"] = "false" if stubs else "true"
        # Never pick up a developer's local estate rules from Config/.
        env["REKT_PREPROCESS_RULES"] = str(rules) if rules else str(self.work_dir / "no-rules.json")
        return subprocess.run(
            [str(SCRIPT_PATH), str(self.source_dir)],
            cwd=REPO_ROOT,
            env=env,
            capture_output=True,
            check=True,
            text=True,
        )

    def staged_text(self, filename):
        # Unchanged programs get no .preprocessed copy; the parser reads the source itself.
        preprocessed = self.source_dir / ".preprocessed" / filename
        path = preprocessed if preprocessed.exists() else self.source_dir / filename
        return path.read_text(encoding="latin-1")

    def preprocessed_text(self, filename):
        return (self.source_dir / ".preprocessed" / filename).read_text(
            encoding="latin-1"
        )

    @staticmethod
    def as_cobol_comment(line):
        return f"{line[:6]}*{line[7:]}" if len(line) >= 7 else f"*{line}"

    def test_handles_quotes_and_spaces_in_program_and_copybook_paths(self):
        copybook_dir = self.source_dir / "copybooks 'quoted'"
        program_dir = self.source_dir / "programs 'quoted'"
        copybook_dir.mkdir()
        program_dir.mkdir()

        copybook_name = "quoted ' copybook.cpy"
        program_name = "quoted ' program.cbl"

        (copybook_dir / copybook_name).write_text(
            "\n".join(
                [
                    "       01  SAMPLE-FIELD COMP-2.",
                    "",
                ]
            ),
            encoding="latin-1",
        )
        (program_dir / program_name).write_text(
            "\n".join(
                [
                    "       IDENTIFICATION DIVISION.",
                    "       PROGRAM-ID. QUOTEDP.",
                    "       DATA DIVISION.",
                    "       WORKING-STORAGE SECTION.",
                    "       01  WS-FLAG PIC 9.",
                    "       PROCEDURE DIVISION.",
                    "           MOVE 0(1) TO WS-FLAG.",
                    "           GOBACK.",
                    "",
                ]
            ),
            encoding="latin-1",
        )

        result = self.run_preprocessor()

        self.assertIn("Preprocessed 1 program(s) and 0 copybook(s)", result.stdout)
        self.assertFalse((self.source_dir / ".preprocessed" / copybook_name).exists())
        self.assertIn("SAMPLE-FIELD COMP-2.", (copybook_dir / copybook_name).read_text())
        self.assertIn("MOVE 0 TO WS-FLAG.", self.preprocessed_text("programs 'quoted'/" + program_name))

    def test_preserves_floating_point_usage_in_programs_and_copybooks(self):
        declarations = (
            "       01 SHORT-FLOAT COMP-1.\n"
            "       01 LONG-FLOAT COMP-2.\n"
            "       01 SHORT-ALIAS COMPUTATIONAL-1.\n"
            "       01 LONG-ALIAS COMPUTATIONAL-2.\n"
        )
        (self.source_dir / "FLOATS.cbl").write_text(
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. FLOATS.\n"
            "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n"
            + declarations + "       01 WS-FLAG PIC 9.\n"
            "       PROCEDURE DIVISION.\n           MOVE 0(1) TO WS-FLAG.\n           GOBACK.\n"
        )
        (self.source_dir / "FLOATS.cpy").write_text(declarations)
        self.run_preprocessor()
        for filename in ("FLOATS.cbl", "FLOATS.cpy"):
            prepared = self.source_dir / ".preprocessed" / filename
            output = (prepared if prepared.exists() else self.source_dir / filename).read_text()
            for declaration in declarations.splitlines():
                self.assertIn(declaration, output)
            self.assertNotIn("PIC X(4)", output)
            self.assertNotIn("PIC X(8)", output)

    def test_duplicate_program_rewrites_preserve_source_relative_identity(self):
        for area in ("one", "two"):
            (self.source_dir / area).mkdir()
            (self.source_dir / area / "SAMPLE.cbl").write_text(
                "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. SAMPLE.\n"
                "       PROCEDURE DIVISION.\n           MOVE 0(1) TO WS-FLAG.\n"
                f"           DISPLAY '{area}'.\n           GOBACK.\n")
        self.run_preprocessor()
        for area in ("one", "two"):
            output = self.source_dir / ".preprocessed" / area / "SAMPLE.cbl"
            self.assertIn("MOVE 0 TO WS-FLAG.", output.read_text())
            self.assertIn(f"DISPLAY '{area}'.", output.read_text())
            provenance = json.loads(Path(str(output) + ".preprocess.json").read_text())
            self.assertEqual(str((self.source_dir / area / "SAMPLE.cbl").resolve()),
                             provenance["originalSourcePath"])
        self.assertFalse((self.source_dir / ".preprocessed/SAMPLE.cbl").exists())
        checked = subprocess.run(
            [os.sys.executable, str(REPO_ROOT / "tools/rekt_source_identity.py"),
             str(self.source_dir), str(self.source_dir / ".preprocessed")],
            capture_output=True, text=True)
        self.assertEqual(0, checked.returncode, checked.stdout + checked.stderr)

    def test_sql_cursor_declaration_and_its_query_are_not_commented_out(self):
        declaration = (
            "           EXEC SQL DECLARE TEST-CURSOR CURSOR FOR\n"
            "               SELECT CUSTOMER_NUMBER FROM CUSTOMER\n"
            "               WHERE CUSTOMER_NUMBER = :WS-CUSTOMER\n"
            "           END-EXEC.\n")
        source = self.source_dir / "cursor.cbl"
        original = ("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. CURSORP.\n"
                    "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n"
                    "       01 WS-CUSTOMER PIC X(10).\n" + declaration
                    + "       PROCEDURE DIVISION.\n"
                    "           EXEC SQL OPEN TEST-CURSOR END-EXEC.\n           GOBACK.\n")
        source.write_text(original)
        self.run_preprocessor()
        output = self.source_dir / ".preprocessed/cursor.cbl"
        transformed = output.read_text() if output.exists() else source.read_text()
        self.assertIn(declaration, transformed)
        self.assertNotIn("*SQL>", transformed)
        self.assertEqual(original, source.read_text())

    def test_async_cics_commands_and_container_lengths_are_preserved(self):
        commands = (
            "           EXEC CICS PUT CONTAINER(CONT-NAME) FROM(PAYLOAD)\n"
            "               FLENGTH(DATA-LENGTH) CHANNEL(CHAN-NAME) END-EXEC.\n"
            "           EXEC CICS RUN TRANSID(TRAN-NAME) CHANNEL(CHAN-NAME)\n"
            "               CHILD(CHILD-TOKEN) END-EXEC.\n"
            "           EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n"
            "           EXEC CICS FETCH ANY(CHILD-TOKEN) CHANNEL(CHAN-NAME)\n"
            "               NOSUSPEND COMPSTATUS(CHILD-STATUS) END-EXEC.\n"
            "           EXEC CICS GET CONTAINER(CONT-NAME) INTO(PAYLOAD)\n"
            "               FLENGTH(DATA-LENGTH) CHANNEL(CHAN-NAME) END-EXEC.\n"
            "           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)\n"
            "               YYMMDD(FORMAT-DATE) DATESEP('.') END-EXEC.\n"
            "           IF CHILD-STATUS = DFHVALUE(NORMAL) CONTINUE END-IF.\n"
            "           IF CHILD-STATUS = DFHVALUE(ABEND) CONTINUE END-IF.\n"
            "           IF CHILD-STATUS = DFHVALUE(SECERROR) CONTINUE END-IF.\n")
        source = self.source_dir / "async.cbl"
        original = ("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. ASYNCP.\n"
                    "       PROCEDURE DIVISION.\n" + commands
                    + "           MOVE 0(1) TO WS-FLAG.\n           GOBACK.\n")
        source.write_text(original)
        self.run_preprocessor()
        self.assertIn(commands, self.preprocessed_text("async.cbl"))
        self.assertEqual(original, source.read_text())

    def test_cics_inquire_association_and_origin_fields_are_preserved(self):
        command = (
            "           EXEC CICS INQUIRE ASSOCIATION(EIBTASKN)\n"
            "               ODAPPLID(SUBPGM-APPLID)\n"
            "               ODUSERID(SUBPGM-USERID)\n"
            "               ODFACILNAME(SUBPGM-FACILITY-NAME)\n"
            "               ODNETWORKID(SUBPGM-NETWRK-ID)\n"
            "               ODFACILTYPE(SUBPGM-FACILTYPE)\n"
            "           END-EXEC.\n")
        source = self.source_dir / "association.cbl"
        original = ("       IDENTIFICATION DIVISION.\n"
                    "       PROGRAM-ID. ASSOCIATIONP.\n"
                    "       PROCEDURE DIVISION.\n" + command
                    + "           MOVE 0(1) TO WS-FLAG.\n"
                    "           GOBACK.\n")
        source.write_text(original)
        self.run_preprocessor()
        self.assertIn(command, self.preprocessed_text("association.cbl"))
        self.assertEqual(original, source.read_text())

    def test_preserves_unsupported_in_conditions_as_opaque(self):
        arithmetic_if = "           IF WS-VALUE IN WS-GROUP + 1"
        arithmetic_and = "              AND WS-OTHER = 1"
        comparison_if = "           IF WS-VALUE IN WS-GROUP > 1"
        opaque_if = "           IF REKT-OPAQUE-IN-CONDITION"

        (self.source_dir / "unsupported-in.cbl").write_text(
            "\n".join(
                [
                    "       IDENTIFICATION DIVISION.",
                    "       PROGRAM-ID. INQUAL.",
                    "       PROCEDURE DIVISION.",
                    arithmetic_if,
                    arithmetic_and,
                    "               DISPLAY 'ARITH'.",
                    "           END-IF.",
                    comparison_if,
                    "               DISPLAY 'COMPARE'.",
                    "           END-IF.",
                    "           GOBACK.",
                    "",
                ]
            ),
            encoding="latin-1",
        )

        self.run_preprocessor()
        output = self.preprocessed_text("unsupported-in.cbl")

        self.assertNotIn("IF TRUE", output)
        self.assertEqual(2, output.count(opaque_if))
        self.assertIn(
            "\n".join(
                [
                    self.as_cobol_comment(arithmetic_if),
                    self.as_cobol_comment(arithmetic_and),
                    opaque_if,
                ]
            ),
            output,
        )
        self.assertIn(
            "\n".join(
                [
                    self.as_cobol_comment(comparison_if),
                    opaque_if,
                ]
            ),
            output,
        )

    def test_missing_copybooks_are_not_synthesised_even_with_legacy_opt_in(self):
        (self.source_dir / "sqlprog.cbl").write_text(
            "\n".join(
                [
                    "       IDENTIFICATION DIVISION.",
                    "       PROGRAM-ID. SQLPROG.",
                    "       DATA DIVISION.",
                    "       WORKING-STORAGE SECTION.",
                    "           EXEC SQL INCLUDE SQLCA END-EXEC.",
                    "       COPY ABSENTCB.",
                    "       PROCEDURE DIVISION.",
                    "           GOBACK.",
                    "",
                ]
            ),
            encoding="latin-1",
        )

        self.run_preprocessor(stubs=True)

        self.assertNotIn(MARKER, self.preprocessed_text("SQLCA.cpy"))
        self.assertIn("SQLCODE", self.preprocessed_text("SQLCA.cpy"))
        self.assertFalse((self.source_dir / ".preprocessed" / "ABSENTCB.cpy").exists())

    def test_unverified_generated_candidate_does_not_suppress_bundled_definition(self):
        generated = self.source_dir / "copy-generated"
        generated.mkdir()
        candidate = generated / "SQLCA.cpy"
        candidate.write_text("       01 SQLCA PIC X.\n")

        self.run_preprocessor()

        bundled = REPO_ROOT / "tools" / "system-copybooks" / "SQLCA.cpy"
        output = self.source_dir / ".preprocessed" / "SQLCA.cpy"
        self.assertEqual(bundled.read_bytes(), output.read_bytes())
        provenance = json.loads(Path(str(output) + ".preprocess.json").read_text())
        self.assertEqual(str(bundled.resolve()), provenance["originalSourcePath"])
        self.assertEqual("       01 SQLCA PIC X.\n", candidate.read_text())

    def test_supplied_definition_takes_precedence_over_bundled_definition(self):
        supplied = self.source_dir / "copy"
        supplied.mkdir()
        definition = supplied / "SQLCA.cpy"
        definition.write_text("       01 SQLCA.\n           05 CUSTOM-SQLCODE PIC S9(9).\n")

        self.run_preprocessor()

        self.assertFalse((self.source_dir / ".preprocessed" / "SQLCA.cpy").exists())
        self.assertIn("CUSTOM-SQLCODE", definition.read_text())

    def test_quarantines_legacy_shadows_without_touching_supplied_source(self):
        generated = self.source_dir / ".preprocessed"
        generated.mkdir()
        placeholder = generated / "ABSENTCB.cpy"
        placeholder.write_text(f"      * {MARKER}\n       01 INVENTED PIC X.\n")
        sidecar = generated / "ABSENTCB.cpy.preprocess.json"
        sidecar.write_text("{}")
        genuine = generated / "REALCB.cpy"
        genuine.write_text("       01 REAL-FIELD PIC X.\n")
        supplied = self.source_dir / "SUPPLIED-STUB.cpy"
        supplied.write_text(f"      * {MARKER}\n       01 INVENTED PIC X.\n")
        self.run_preprocessor(stubs=True)
        self.assertFalse(placeholder.exists())
        self.assertFalse(sidecar.exists())
        self.assertFalse(genuine.exists())
        archived = list((generated / ".previous").rglob("REALCB.cpy"))
        self.assertEqual(1, len(archived))
        self.assertEqual("       01 REAL-FIELD PIC X.\n", archived[0].read_text())
        self.assertEqual(1, len(list((generated / ".previous").rglob("ABSENTCB.cpy"))))
        self.assertEqual(f"      * {MARKER}\n       01 INVENTED PIC X.\n", supplied.read_text())
        self.assertFalse((generated / supplied.name).exists())
        self.assertFalse((generated / "aliases" / "SUPPLIED.cpy").exists())

    def write_program(self, name, procedure_lines):
        (self.source_dir / name).write_text(
            "\n".join(
                [
                    "       IDENTIFICATION DIVISION.",
                    "       PROGRAM-ID. TESTPGM.",
                    "       PROCEDURE DIVISION.",
                    *procedure_lines,
                    "           GOBACK.",
                    "",
                ]
            ),
            encoding="latin-1",
        )

    def test_inserts_continue_when_then_branch_holds_only_comments(self):
        self.write_program(
            "empty-then.cbl",
            [
                "           IF WS-FILE-OK",
                "      *        nothing to do yet",
                "           ELSE",
                "               DISPLAY 'FAILED'",
                "           END-IF",
                "           IF WS-OTHER-OK",
                "               DISPLAY 'OK'",
                "           ELSE",
                "               DISPLAY 'NOT OK'",
                "           END-IF",
            ],
        )

        self.run_preprocessor()
        output = self.preprocessed_text("empty-then.cbl")

        self.assertIn(
            "\n".join(
                [
                    "           IF WS-FILE-OK",
                    "           CONTINUE",
                    "      *        nothing to do yet",
                    "           ELSE",
                ]
            ),
            output,
        )
        self.assertEqual(1, output.count("CONTINUE"))

    def test_length_of_is_preserved_in_procedure_statements(self):
        self.write_program(
            "table-length.cbl",
            [
                "           COMPUTE TOTAL = 48 + (COUNT * (LENGTH OF ENTRY(1))).",
                "           COMPUTE TOTAL = LENGTH OF ENTRY(INDEX) OF GROUP.",
                "           MOVE A TO B(LENGTH OF C + 1:LENGTH OF A)",
            ],
        )
        self.run_preprocessor()
        output = self.staged_text("table-length.cbl")
        self.assertIn("COMPUTE TOTAL = 48 + (COUNT * (LENGTH OF ENTRY(1))).", output)
        self.assertIn("COMPUTE TOTAL = LENGTH OF ENTRY(INDEX) OF GROUP.", output)
        self.assertIn("MOVE A TO B(LENGTH OF C + 1:LENGTH OF A)", output)

    def test_rewritten_padded_lines_stay_within_column_80_and_keep_conditions(self):
        padded = [
            "           IF  FLG-FILTER-BLANK".ljust(80),
            "           AND FLG-REENTER".ljust(80),
            "               MOVE LENGTH OF WS-REC TO WS-LEN".ljust(80),
            "           END-IF".ljust(80),
            "           EXEC DLI SCHD PSB((PSB-NAME)) END-EXEC".ljust(80),
        ]
        self.write_program("padded.cbl", padded)
        self.run_preprocessor()
        output = self.preprocessed_text("padded.cbl")
        self.assertEqual([], [line for line in output.split("\n") if len(line) > 80])
        stripped = "\n".join(line.rstrip() for line in output.split("\n"))
        self.assertIn("           IF  FLG-FILTER-BLANK\n           AND FLG-REENTER\n", stripped)
        self.assertIn("               MOVE LENGTH OF WS-REC TO WS-LEN", stripped)
        self.assertIn("      *IMS>     EXEC DLI SCHD PSB((PSB-NAME)) END-EXEC", stripped)

    def test_preserves_inspect_for_all_syntax(self):
        self.write_program(
            "inspect-count.cbl",
            [
                "           INSPECT INPUT-VALUE TALLYING",
                "              DIGIT-COUNT FOR ALL '0'",
                "              POINT-COUNT for ALL '.'.",
                "           INSPECT INPUT-VALUE REPLACING ALL '_' BY SPACE.",
                "           INSPECT INPUT-VALUE REPLACING",
                "              ALL '_' BY SPACE",
                "              ALL '%' BY SPACE.",
                "           MOVE ALL '%' TO OUTPUT-VALUE.",
            ],
        )
        self.run_preprocessor()
        output = self.preprocessed_text("inspect-count.cbl")
        self.assertIn("DIGIT-COUNT FOR ALL '0'", output)
        self.assertIn("POINT-COUNT for ALL '.'", output)
        self.assertIn("REPLACING ALL '_' BY SPACE.", output)
        self.assertIn("              ALL '_' BY SPACE", output)
        self.assertIn("              ALL '%' BY SPACE.", output)
        self.assertIn("MOVE '%' TO OUTPUT-VALUE.", output)

    def test_drops_identification_area_text_instead_of_pulling_it_into_code(self):
        # Columns 73-80 are ignored by the compiler. Shifting their text left turns a note
        # or a short sequence number into a token, and the parser then writes no AST.
        def fixed(code, ident):
            return code.ljust(73) + ident

        program = "\n".join(
            [
                fixed("       IDENTIFICATION DIVISION.", "0002000"),
                fixed("       PROGRAM-ID. TESTPGM.", "0003000"),
                "       DATA DIVISION.",
                "       WORKING-STORAGE SECTION.",
                "       01  WS-TS          PIC X(10).",
                fixed("       01  FILLER REDEFINES WS-TS.", ""),
                fixed("           06 WS-YYYY      PIC X(004).", "E"),
                fixed("           06 WS-SEP       PIC X.", "-"),
                "       PROCEDURE DIVISION.",
                fixed("           GOBACK.", "AB12CD34"),
                "",
            ]
        )
        (self.source_dir / "idarea.cbl").write_text(program, encoding="latin-1")
        (self.source_dir / "idarea.cpy").write_text(
            fixed("       01  CPY-FLD        PIC X(004).", "E") + "\n", encoding="latin-1"
        )

        self.run_preprocessor()

        for name in ("idarea.cbl", "idarea.cpy"):
            for line in self.preprocessed_text(name).split("\n"):
                self.assertLessEqual(len(line.rstrip()), 72, f"{name}: {line!r}")
                for ident in ("0002000", "0003000", "AB12CD34"):
                    self.assertNotIn(ident, line, f"{name}: {line!r}")
                self.assertFalse(line.rstrip().endswith((" E", " -")), f"{name}: {line!r}")
        self.assertIn("PIC X(004).", self.preprocessed_text("idarea.cbl"))

    def test_preserves_qualified_length_of(self):
        self.write_program(
            "length-of.cbl",
            [
                "           PERFORM VARYING WS-IDX",
                "                   FROM LENGTH OF OPTIONI OF MAPAI BY -1 UNTIL",
                "                   WS-IDX = 1",
                "           END-PERFORM",
            ],
        )

        self.run_preprocessor()
        output = self.staged_text("length-of.cbl")

        self.assertIn("FROM LENGTH OF OPTIONI OF MAPAI BY -1 UNTIL", output)

    def test_skips_generated_copybook_that_a_real_copybook_replaces(self):
        real_dir = self.source_dir / "real"
        generated_dir = self.source_dir / "app" / "copy-generated"
        real_dir.mkdir()
        generated_dir.mkdir(parents=True)
        # Both need rewriting (column 73+ text), so each would be written to .preprocessed/.
        (real_dir / "SHARED.cpy").write_text(
            "       01  REAL-FIELD     PIC X(004).".ljust(73) + "E\n", encoding="latin-1"
        )
        generated = generated_dir / "SHARED.cpy"
        generated.write_text(
            "       01  STUB-FIELD     PIC X(004).".ljust(73) + "E\n", encoding="latin-1"
        )
        shadowed = self.work_dir / "shadowed.txt"
        shadowed.write_text(str(generated) + "\n")

        self.run_preprocessor(shadowed=shadowed)

        output = self.preprocessed_text("SHARED.cpy")
        self.assertIn("REAL-FIELD", output)
        self.assertNotIn("STUB-FIELD", output)

    def test_applies_local_rules_file_by_stage(self):
        self.write_program(
            "local-rules.cbl",
            ["           PERFORM 100-STEP UNTIL WS-A-DONE AND WS-B-DONE"],
        )
        rules = self.work_dir / "rules.json"
        rules.write_text(
            json.dumps(
                {
                    "programRewrites": [
                        {
                            "stage": "early",
                            "literal": True,
                            "find": "UNTIL WS-A-DONE AND WS-B-DONE",
                            "replace": "UNTIL WS-A = 'Y' AND WS-B = 'Y'",
                        },
                        {
                            "stage": "late",
                            "find": r"PERFORM (\d+)-STEP",
                            "replace": r"PERFORM \1-NEXT",
                        },
                    ]
                }
            ),
            encoding="utf-8",
        )

        result = self.run_preprocessor(rules=rules)

        self.assertIn("Using local preprocess rules", result.stdout)
        self.assertIn(
            "PERFORM 100-NEXT UNTIL WS-A = 'Y' AND WS-B = 'Y'",
            self.preprocessed_text("local-rules.cbl"),
        )

    def test_reports_and_ignores_a_malformed_rules_file(self):
        self.write_program("plain.cbl", ["           MOVE 0(1) TO WS-FLAG"])
        rules = self.work_dir / "broken.json"
        rules.write_text("{not json", encoding="utf-8")

        result = self.run_preprocessor(rules=rules)

        self.assertIn("Ignoring", result.stdout)
        self.assertIn("Preprocessed 1 program(s)", result.stdout)

    def write_program(self, name, procedure_lines):
        (self.source_dir / name).write_text(
            "\n".join(
                [
                    "       IDENTIFICATION DIVISION.",
                    "       PROGRAM-ID. TESTPGM.",
                    "       PROCEDURE DIVISION.",
                    *procedure_lines,
                    "           GOBACK.",
                    "",
                ]
            ),
            encoding="latin-1",
        )

    def test_inserts_continue_when_then_branch_holds_only_comments(self):
        self.write_program(
            "empty-then.cbl",
            [
                "           IF WS-FILE-OK",
                "      *        nothing to do yet",
                "           ELSE",
                "               DISPLAY 'FAILED'",
                "           END-IF",
                "           IF WS-OTHER-OK",
                "               DISPLAY 'OK'",
                "           ELSE",
                "               DISPLAY 'NOT OK'",
                "           END-IF",
            ],
        )

        self.run_preprocessor()
        output = self.preprocessed_text("empty-then.cbl")

        self.assertIn(
            "\n".join(
                [
                    "           IF WS-FILE-OK",
                    "           CONTINUE",
                    "      *        nothing to do yet",
                    "           ELSE",
                ]
            ),
            output,
        )
        self.assertEqual(1, output.count("CONTINUE"))

    def test_drops_identification_area_text_instead_of_pulling_it_into_code(self):
        # Columns 73-80 are ignored by the compiler. Shifting their text left turns a note
        # or a short sequence number into a token, and the parser then writes no AST.
        def fixed(code, ident):
            return code.ljust(73) + ident

        program = "\n".join(
            [
                fixed("       IDENTIFICATION DIVISION.", "0002000"),
                fixed("       PROGRAM-ID. TESTPGM.", "0003000"),
                "       DATA DIVISION.",
                "       WORKING-STORAGE SECTION.",
                "       01  WS-TS          PIC X(10).",
                fixed("       01  FILLER REDEFINES WS-TS.", ""),
                fixed("           06 WS-YYYY      PIC X(004).", "E"),
                fixed("           06 WS-SEP       PIC X.", "-"),
                "       PROCEDURE DIVISION.",
                fixed("           GOBACK.", "AB12CD34"),
                "",
            ]
        )
        (self.source_dir / "idarea.cbl").write_text(program, encoding="latin-1")
        (self.source_dir / "idarea.cpy").write_text(
            fixed("       01  CPY-FLD        PIC X(004).", "E") + "\n", encoding="latin-1"
        )

        self.run_preprocessor()

        for name in ("idarea.cbl", "idarea.cpy"):
            for line in self.preprocessed_text(name).split("\n"):
                self.assertLessEqual(len(line.rstrip()), 72, f"{name}: {line!r}")
                for ident in ("0002000", "0003000", "AB12CD34"):
                    self.assertNotIn(ident, line, f"{name}: {line!r}")
                self.assertFalse(line.rstrip().endswith((" E", " -")), f"{name}: {line!r}")
        self.assertIn("PIC X(004).", self.preprocessed_text("idarea.cbl"))

    def test_preserves_qualified_length_of(self):
        self.write_program(
            "length-of.cbl",
            [
                "           PERFORM VARYING WS-IDX",
                "                   FROM LENGTH OF OPTIONI OF MAPAI BY -1 UNTIL",
                "                   WS-IDX = 1",
                "           END-PERFORM",
            ],
        )

        self.run_preprocessor()
        output = self.staged_text("length-of.cbl")

        self.assertIn("FROM LENGTH OF OPTIONI OF MAPAI BY -1 UNTIL", output)

    def test_skips_generated_copybook_that_a_real_copybook_replaces(self):
        real_dir = self.source_dir / "real"
        generated_dir = self.source_dir / "app" / "copy-generated"
        real_dir.mkdir()
        generated_dir.mkdir(parents=True)
        # Both need rewriting (column 73+ text), so each would be written to .preprocessed/.
        (real_dir / "SHARED.cpy").write_text(
            "       01  REAL-FIELD     PIC X(004).".ljust(73) + "E\n", encoding="latin-1"
        )
        generated = generated_dir / "SHARED.cpy"
        generated.write_text(
            "       01  STUB-FIELD     PIC X(004).".ljust(73) + "E\n", encoding="latin-1"
        )
        shadowed = self.work_dir / "shadowed.txt"
        shadowed.write_text(str(generated) + "\n")

        self.run_preprocessor(shadowed=shadowed)

        output = self.preprocessed_text("SHARED.cpy")
        self.assertIn("REAL-FIELD", output)
        self.assertNotIn("STUB-FIELD", output)

    def test_applies_local_rules_file_by_stage(self):
        self.write_program(
            "local-rules.cbl",
            ["           PERFORM 100-STEP UNTIL WS-A-DONE AND WS-B-DONE"],
        )
        rules = self.work_dir / "rules.json"
        rules.write_text(
            json.dumps(
                {
                    "programRewrites": [
                        {
                            "stage": "early",
                            "literal": True,
                            "find": "UNTIL WS-A-DONE AND WS-B-DONE",
                            "replace": "UNTIL WS-A = 'Y' AND WS-B = 'Y'",
                        },
                        {
                            "stage": "late",
                            "find": r"PERFORM (\d+)-STEP",
                            "replace": r"PERFORM \1-NEXT",
                        },
                    ]
                }
            ),
            encoding="utf-8",
        )

        result = self.run_preprocessor(rules=rules)

        self.assertIn("Using local preprocess rules", result.stdout)
        self.assertIn(
            "PERFORM 100-NEXT UNTIL WS-A = 'Y' AND WS-B = 'Y'",
            self.preprocessed_text("local-rules.cbl"),
        )

    def test_reports_and_ignores_a_malformed_rules_file(self):
        self.write_program("plain.cbl", ["           MOVE 0(1) TO WS-FLAG"])
        rules = self.work_dir / "broken.json"
        rules.write_text("{not json", encoding="utf-8")

        result = self.run_preprocessor(rules=rules)

        self.assertIn("Ignoring", result.stdout)
        self.assertIn("Preprocessed 1 program(s)", result.stdout)


if __name__ == "__main__":
    unittest.main()
