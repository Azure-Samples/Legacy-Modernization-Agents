import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path


@unittest.skipUnless(os.environ.get("REKT_TEST_IMAGE"), "Set REKT_TEST_IMAGE for parser integration")
class MissingCopybookParserTests(unittest.TestCase):
    def run_fixture(self, root, recovery=True, invalid=False, unknown_layout=False):
        source = root / "source"
        output = root / "output"
        source.mkdir()
        output.mkdir()
        lines = [
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. RECOVERY.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01 KNOWN-VALUE PIC 9.",
            "       COPY ABSENTCB.",
            "       PROCEDURE DIVISION.",
            "       MAIN-LOGIC.",
            "           IF KNOWN-VALUE = 1",
            "               MOVE 2 TO KNOWN-VALUE",
            "           END-IF.",
            "           GOBACK.",
        ]
        if invalid:
            lines.insert(-1, "           MOVE TO.")
        if unknown_layout:
            lines.insert(5, "       01 UNKNOWN-LAYOUT.")
        (source / "RECOVERY.cbl").write_text("\n".join(lines) + "\n")
        command = ["docker", "run", "--rm", "--entrypoint", "java",
                   "-v", f"{source}:/source:ro", "-v", f"{output}:/output"]
        if recovery:
            command += ["-e", "REKT_ALLOW_MISSING_COPYBOOKS=true"]
        command += [
            os.environ["REKT_TEST_IMAGE"], "-jar", "/app/smojol-cli.jar",
            "run", "RECOVERY.cbl", "--commands=BUILD_BASE_ANALYSIS WRITE_FLOW_AST WRITE_CFG WRITE_DATA_STRUCTURES",
            "--srcDir=/source", "--copyBooksDir=/source", "--reportDir=/output", "--generation=PROGRAM",
        ]
        result = subprocess.run(command, capture_output=True, text=True, timeout=120)
        report = output / "RECOVERY.cbl.report"
        return result, report, source

    def test_recovers_known_control_flow_and_preserves_missing_diagnostics(self):
        with tempfile.TemporaryDirectory(prefix="rekt-recovery-") as directory:
            result, report, source = self.run_fixture(Path(directory))
            self.assertEqual(0, result.returncode, result.stdout + result.stderr)
            diagnostics = json.loads((report / "parse-diagnostics.json").read_text())
            self.assertTrue(any(d.get("code") == "missing copybook" and "ABSENTCB" in d["message"]
                                for d in diagnostics))
            artifacts = list(report.rglob("flow-ast-*.json"))
            self.assertEqual(1, len(artifacts))
            ast = artifacts[0].read_text()
            self.assertIn("KNOWN-VALUE", ast)
            self.assertIn("MOVE 2", ast)
            self.assertIn('"IF', ast)
            data = list((report / "data_structures").glob("*.json"))
            self.assertEqual(1, len(data))
            self.assertNotIn("ABSENTCB-STUB", data[0].read_text())
            self.assertNotIn("ABSENTCB-VAL", data[0].read_text())
            self.assertEqual(["RECOVERY.cbl"], [p.name for p in source.iterdir()])

    def test_strict_mode_still_rejects_missing_definitions(self):
        with tempfile.TemporaryDirectory(prefix="rekt-strict-") as directory:
            result, report, _ = self.run_fixture(Path(directory), recovery=False)
            self.assertNotEqual(0, result.returncode)
            self.assertFalse(list(report.rglob("flow-ast-*.json")))
            self.assertTrue((report / "parse-diagnostics.json").exists())

    def test_recovery_does_not_accept_syntax_errors(self):
        with tempfile.TemporaryDirectory(prefix="rekt-invalid-") as directory:
            result, report, _ = self.run_fixture(Path(directory), invalid=True)
            self.assertNotEqual(0, result.returncode)
            self.assertFalse(list(report.rglob("flow-ast-*.json")))

    def test_unknown_layout_does_not_become_one_byte(self):
        with tempfile.TemporaryDirectory(prefix="rekt-layout-") as directory:
            result, report, _ = self.run_fixture(Path(directory), unknown_layout=True)
            self.assertNotEqual(0, result.returncode)
            self.assertIn("Unknown data layout", result.stdout + result.stderr)
            self.assertFalse(list((report / "data_structures").glob("*.json")))


if __name__ == "__main__":
    unittest.main()
