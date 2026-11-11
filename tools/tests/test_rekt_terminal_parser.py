import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path


@unittest.skipUnless(os.environ.get("REKT_TEST_IMAGE"), "Set REKT_TEST_IMAGE to run parser integration tests")
class TerminalParserTests(unittest.TestCase):
    def test_terminal_commands_preserve_surrounding_control_flow(self):
        with tempfile.TemporaryDirectory(prefix="rekt-terminal-") as directory:
            root = Path(directory)
            source = root / "source"
            output = root / "output"
            source.mkdir()
            output.mkdir()
            for verb in ("INQUIRE", "SET"):
                name = f"TERMINAL-{verb}"
                program = "\n".join([
                    "       IDENTIFICATION DIVISION.",
                    f"       PROGRAM-ID. {name}.",
                    "       DATA DIVISION.",
                    "       WORKING-STORAGE SECTION.",
                    "       01 TERMINAL-ID PIC X(4).",
                    "       01 TRANSLATION-MODE PIC S9(8) COMP.",
                    "       01 RESPONSE-CODE PIC S9(8) COMP.",
                    "       01 DETAIL-CODE PIC S9(8) COMP.",
                    "       PROCEDURE DIVISION.",
                    "       MAIN-LOGIC.",
                    "           IF TRANSLATION-MODE = 1",
                    f"              EXEC CICS {verb} TERMINAL(TERMINAL-ID)",
                    "                   UCTRANST(TRANSLATION-MODE)",
                    "                   RESP(RESPONSE-CODE)",
                    "                   RESP2(DETAIL-CODE)",
                    "              END-EXEC",
                    "              MOVE 2 TO TRANSLATION-MODE",
                    "           END-IF.",
                    "           GOBACK.",
                    "",
                ])
                (source / f"{name}.cbl").write_text(program)
                result = subprocess.run([
                    "docker", "run", "--rm", "--entrypoint", "java",
                    "-v", f"{source}:/source:ro", "-v", f"{output}:/output",
                    os.environ["REKT_TEST_IMAGE"], "-jar", "/app/smojol-cli.jar",
                    "run", f"{name}.cbl",
                    "--commands=BUILD_BASE_ANALYSIS WRITE_FLOW_AST",
                    "--srcDir=/source", "--copyBooksDir=/source", "--reportDir=/output",
                    "--generation=PROGRAM",
                ], capture_output=True, text=True, timeout=120)
                self.assertEqual(0, result.returncode, result.stdout + result.stderr)
                artifacts = list((output / f"{name}.cbl.report").rglob("flow-ast-*.json"))
                self.assertEqual(1, len(artifacts), result.stdout + result.stderr)
                nodes = []

                def walk(value):
                    if isinstance(value, dict):
                        nodes.append(value)
                        for child in value.values():
                            walk(child)
                    elif isinstance(value, list):
                        for child in value:
                            walk(child)

                walk(json.loads(artifacts[0].read_text()))
                self.assertTrue(any(node.get("type") == "PARAGRAPH" for node in nodes))
                texts = [node.get("originalText", "").upper() for node in nodes]
                self.assertTrue(any(f"EXEC CICS {verb}" in text and "UCTRANST" in text for text in texts))
                self.assertTrue(any("MOVE 2 TO TRANSLATION-MODE" in text for text in texts))
                self.assertTrue(any("IF TRANSLATION-MODE" in text for text in texts))


if __name__ == "__main__":
    unittest.main()
