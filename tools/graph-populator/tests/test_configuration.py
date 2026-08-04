import unittest

from configuration import required_environment_variable


class RequiredEnvironmentVariableTests(unittest.TestCase):
    def test_returns_configured_value(self):
        result = required_environment_variable(
            "NEO4J_PASSWORD",
            {"NEO4J_PASSWORD": "local-secret"},
        )

        self.assertEqual("local-secret", result)

    def test_rejects_missing_value(self):
        with self.assertRaisesRegex(RuntimeError, "NEO4J_PASSWORD is required"):
            required_environment_variable("NEO4J_PASSWORD", {})

    def test_rejects_empty_value(self):
        with self.assertRaisesRegex(RuntimeError, "NEO4J_PASSWORD is required"):
            required_environment_variable("NEO4J_PASSWORD", {"NEO4J_PASSWORD": ""})
