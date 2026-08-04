import unittest

from source_paths import artifact_source_path, scoped_graph_id, source_relative_path


class SourceRelativePathTests(unittest.TestCase):
    def test_preserves_nested_directory(self):
        result = source_relative_path(
            "/source/finance/batch/ACCOUNTS.cbl",
            "/source",
        )

        self.assertEqual("finance/batch/ACCOUNTS.cbl", result)

    def test_top_level_file_remains_basename(self):
        result = source_relative_path("/source/ACCOUNTS.cbl", "/source")

        self.assertEqual("ACCOUNTS.cbl", result)

    def test_artifact_report_directory_preserves_nested_source_path(self):
        result = artifact_source_path(
            "/output/finance/batch/ACCOUNTS.cbl.report/flow-ast-ACCOUNTS.cbl.json",
            "/output",
            ["finance/batch/ACCOUNTS.cbl", "archive/ACCOUNTS.cbl"],
        )

        self.assertEqual("finance/batch/ACCOUNTS.cbl", result)

    def test_artifact_filename_resolves_unique_source_basename(self):
        result = artifact_source_path(
            "/output/ACCOUNTS.cbl.report/cfg-ACCOUNTS.cbl.json",
            "/output",
            ["finance/batch/ACCOUNTS.cbl"],
        )

        self.assertEqual("finance/batch/ACCOUNTS.cbl", result)

    def test_artifact_filename_rejects_ambiguous_source_basename(self):
        result = artifact_source_path(
            "/output/ACCOUNTS.cbl.report/flow-ast-ACCOUNTS.cbl.json",
            "/output",
            ["finance/ACCOUNTS.cbl", "archive/ACCOUNTS.cbl"],
        )

        self.assertIsNone(result)

    def test_graph_identity_is_scoped_by_run_and_program(self):
        first = scoped_graph_id(1, "finance/ACCOUNTS.cbl", "ROOT")
        same = scoped_graph_id(1, "finance/ACCOUNTS.cbl", "ROOT")
        other_program = scoped_graph_id(1, "archive/ACCOUNTS.cbl", "ROOT")
        other_run = scoped_graph_id(2, "finance/ACCOUNTS.cbl", "ROOT")

        self.assertEqual(first, same)
        self.assertNotEqual(first, other_program)
        self.assertNotEqual(first, other_run)


if __name__ == "__main__":
    unittest.main()
