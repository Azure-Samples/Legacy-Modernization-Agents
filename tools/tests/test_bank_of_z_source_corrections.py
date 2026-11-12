import re
import unittest
from pathlib import Path

COBOL = Path(__file__).resolve().parents[2] / "source" / "bank-of-z" / "cobol"
PF12_PROGRAMS = ("BNK1CAC", "BNK1CCA", "BNK1CRA", "BNK1DCS", "BNK1TFN", "BNK1UAC")


def code_lines(path):
    return [line[6:72] for line in path.read_text().splitlines() if len(line) > 6 and line[6] not in "*/"]


@unittest.skipUnless(COBOL.is_dir(), "Bank-of-Z sample not present under source/")
class BankOfZSourceCorrectionTests(unittest.TestCase):
    def test_no_aggregate_dfhaid_comparison_remains(self):
        for path in sorted(COBOL.glob("*.cbl")):
            for line in code_lines(path):
                with self.subTest(program=path.stem):
                    self.assertIsNone(re.search(r"=\s*DFHAID\b", line), line)

    def test_pf12_alone_reaches_each_termination_branch(self):
        for program in PF12_PROGRAMS:
            with self.subTest(program=program):
                lines = code_lines(COBOL / f"{program}.cbl")
                index = next(i for i, line in enumerate(lines) if line.strip() == "WHEN EIBAID = DFHPF12")
                branch = next(i for i in range(index + 1, len(lines)) if lines[i].strip().startswith("WHEN "))
                body = " ".join(lines[index + 1:branch])
                self.assertIn("PERFORM SEND-TERMINATION-MSG", body)
                self.assertRegex(body, r"EXEC CICS\s+RETURN\s+END-EXEC")

    def test_group_comparison_with_ibm_dfhaid_prefix_is_never_true(self):
        # IBM DFHAID starts DFHNULL (space), DFHENTER (quote), DFHCLEAR (underscore).
        group = b" '_"
        for value in range(256):
            self.assertNotEqual(bytes([value]).ljust(len(group), b" "), group)


if __name__ == "__main__":
    unittest.main()
