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

    def run_preprocessor(self, stubs=False, rules=None):
        env = os.environ.copy()
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

        self.assertIn("Preprocessed 1 program(s) and 1 copybook(s)", result.stdout)
        self.assertIn("PIC X(8)", self.preprocessed_text(copybook_name))
        self.assertIn("MOVE 0 TO WS-FLAG.", self.preprocessed_text(program_name))

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

    def test_marks_synthesised_stubs_but_not_bundled_system_copybooks(self):
        # doctor.sh decides a program's fidelity by looking for this marker, because
        # .preprocessed/ mixes invented layouts with real content. If a bundled copybook
        # ever gained the marker, every program reaching it would be reported as
        # stub-backed even though its layout is real.
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
        self.assertIn(MARKER, self.preprocessed_text("ABSENTCB.cpy"))

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

    def test_replaces_qualified_length_of_including_its_qualifiers(self):
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
        output = self.preprocessed_text("length-of.cbl")

        self.assertIn("FROM 0 BY -1 UNTIL", output)
        self.assertNotIn("OF MAPAI", output)

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
