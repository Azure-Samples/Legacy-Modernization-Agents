#!/usr/bin/env python3
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT_PATH = REPO_ROOT / "tools" / "rekt_source_identity.py"


class RektSourceIdentityTests(unittest.TestCase):
    def setUp(self):
        self.work_dir = Path(tempfile.mkdtemp(prefix="rekt-identity-"))
        self.source = self.work_dir / "source"
        self.source.mkdir()

    def tearDown(self):
        shutil.rmtree(self.work_dir, ignore_errors=True)

    def write(self, rel, text):
        path = self.source / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="latin-1")

    def program(self, rel, *copy_lines):
        body = "".join(f"{line}\n" for line in copy_lines)
        self.write(
            rel,
            "       IDENTIFICATION DIVISION.\n"
            f"       PROGRAM-ID. {Path(rel).stem.upper()}.\n"
            "       DATA DIVISION.\n"
            "       WORKING-STORAGE SECTION.\n"
            f"{body}"
            "       PROCEDURE DIVISION.\n"
            "           GOBACK.\n",
        )

    def run_check(self, *extra):
        result = subprocess.run(
            [sys.executable, str(SCRIPT_PATH), str(self.source), str(self.source / ".preprocessed"), *extra],
            capture_output=True,
            text=True,
        )
        rows = [line.split("\t") for line in result.stdout.splitlines() if line]
        return result.returncode, rows

    def kinds(self, rows):
        return [(row[0], row[1]) for row in rows]

    def test_identical_copies_of_a_used_copybook_do_not_block(self):
        self.write("a/copy/SHARED.cpy", "       01 WS-A PIC X.\n")
        self.write("b/copy/SHARED.cpy", "       01  WS-A   PIC X.   \n")
        self.program("a/cobol/PROGA.cbl", "           COPY SHARED.")

        code, rows = self.run_check()

        self.assertEqual(0, code)
        self.assertEqual([("copybook-duplicate-identical", "shared.cpy")], self.kinds(rows))

    def test_differing_copies_that_nothing_uses_do_not_block(self):
        self.write("api/one/COMMAREA.cpy", "       01 WS-A PIC X.\n")
        self.write("api/two/COMMAREA.cpy", "       01 WS-B PIC 9.\n")
        self.program("base/PROGA.cbl", "           COPY OTHER.")
        self.write("base/OTHER.cpy", "       01 WS-C PIC X.\n")

        code, rows = self.run_check()

        self.assertEqual(0, code)
        self.assertEqual([("copybook-duplicate-unused", "commarea.cpy")], self.kinds(rows))
        self.assertEqual(["api/one/COMMAREA.cpy", "api/two/COMMAREA.cpy"], rows[0][2:])

    def test_differing_copies_that_a_program_uses_block_and_name_the_user(self):
        self.write("one/COMMAREA.cpy", "       01 WS-A PIC X.\n")
        self.write("two/COMMAREA.cpy", "       01 WS-B PIC 9.\n")
        self.program("one/PROGA.cbl", "           COPY 'COMMAREA'.")

        code, rows = self.run_check()

        self.assertEqual(1, code)
        self.assertEqual(
            [("copybook-collision", "commarea.cpy"), ("copybook-collision-users", "commarea.cpy")],
            self.kinds(rows),
        )
        self.assertEqual(["one/PROGA.cbl"], rows[1][2:])

    def test_use_through_the_eight_character_alias_counts(self):
        self.write("one/INPUT-AREA_request_0.cpy", "       01 WS-A PIC X.\n")
        self.write("two/INPUT-AREA_request_0.cpy", "       01 WS-B PIC 9.\n")
        self.program("one/PROGA.cbl", "           COPY INPUT-AR.")

        code, rows = self.run_check()

        self.assertEqual(1, code)
        self.assertEqual("copybook-collision", rows[0][0])

    def test_use_from_a_nested_copybook_or_sql_include_counts(self):
        self.write("one/RECORD.cpy", "       01 WS-A PIC X.\n")
        self.write("two/RECORD.cpy", "       01 WS-B PIC 9.\n")
        self.write("one/WRAPPER.cpy", "           EXEC SQL INCLUDE RECORD END-EXEC.\n")
        self.program("one/PROGA.cbl", "           COPY WRAPPER.")

        code, rows = self.run_check()

        self.assertEqual(1, code)
        self.assertEqual(["one/WRAPPER.cpy"], rows[1][2:])

    def test_commented_out_copy_does_not_count_as_use(self):
        self.write("one/COMMAREA.cpy", "       01 WS-A PIC X.\n")
        self.write("two/COMMAREA.cpy", "       01 WS-B PIC 9.\n")
        self.program(
            "one/PROGA.cbl",
            "      *    COPY COMMAREA.",
            "           MOVE 1 TO WS-X. *> COPY COMMAREA.",
        )

        code, rows = self.run_check()

        self.assertEqual(0, code)
        self.assertEqual("copybook-duplicate-unused", rows[0][0])

    def test_hyphenated_identifier_ending_in_copy_is_not_a_reference(self):
        self.write("one/COMMAREA.cpy", "       01 WS-A PIC X.\n")
        self.write("two/COMMAREA.cpy", "       01 WS-B PIC 9.\n")
        self.program("one/PROGA.cbl", "           MOVE WS-COPY COMMAREA-LEN.")

        code, _ = self.run_check()

        self.assertEqual(0, code)

    def test_preprocessed_duplicate_program_blocks(self):
        self.program("one/PROGA.cbl")
        self.program("two/PROGA.cbl")
        self.write(".preprocessed/PROGA.cbl", "       IDENTIFICATION DIVISION.\n")

        code, rows = self.run_check()

        self.assertEqual(1, code)
        self.assertEqual(
            [("program-duplicate", "proga.cbl"), ("preprocessed-program-collision", "proga.cbl")],
            self.kinds(rows),
        )

    def test_staging_folders_are_ignored(self):
        self.write("one/SHARED.cpy", "       01 WS-A PIC X.\n")
        self.write(".rekt-staging/SHARED.cpy", "       01 WS-B PIC 9.\n")
        self.write(".convert-123/SHARED.cpy", "       01 WS-C PIC 9.\n")
        self.program("one/PROGA.cbl", "           COPY SHARED.")

        code, rows = self.run_check()

        self.assertEqual(0, code)
        self.assertEqual([], rows)

    def test_shadowed_generated_copybook_is_left_out(self):
        self.write("copy-generated/CEEIGZCT.cpy", "       01 STUB PIC X.\n")
        self.write("lib/CEEIGZCT.cpy", "       01 REAL-FIELD PIC 9(4).\n")
        self.program("lib/PROGA.cbl", "           COPY CEEIGZCT.")
        shadowed = self.work_dir / "shadowed.txt"
        shadowed.write_text(f"{self.source / 'copy-generated' / 'CEEIGZCT.cpy'}\n", encoding="utf-8")

        blocked, _ = self.run_check()
        code, rows = self.run_check(str(shadowed))

        self.assertEqual(1, blocked)
        self.assertEqual(0, code)
        self.assertEqual([], rows)


if __name__ == "__main__":
    unittest.main()
