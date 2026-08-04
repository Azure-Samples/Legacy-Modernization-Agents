import unittest

from source_paths import source_relative_path


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


if __name__ == "__main__":
    unittest.main()
