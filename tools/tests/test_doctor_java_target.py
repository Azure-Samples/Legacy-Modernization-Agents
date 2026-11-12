import json
import os
import re
import subprocess
import unittest

from test_doctor_jcl_scan import JclCommandFixture, REPO


class DoctorJavaFrameworkTests(JclCommandFixture):
    def test_spring_flag_selects_java_and_dry_run_reports_framework(self):
        self.env.pop("JAVA_FRAMEWORK", None)
        result = self.run_command("convert-only", "--java-framework", "spring", "--dry-run")
        self.assert_success(result)
        self.assertIn("Spring", result.stdout)
        self.assertFalse(any("build" in call for call in self.calls("dotnet")))

    def test_spring_and_csharp_are_rejected_in_either_flag_order(self):
        for flags in (("--java-framework", "Spring", "--language", "CSharp"),
                      ("--language", "CSharp", "--java-framework", "Spring")):
            with self.subTest(flags=flags):
                result = self.run_command("convert-only", *flags, "--dry-run")
                self.assertNotEqual(0, result.returncode)
                self.assertIn("non-Java target", result.stderr)

    def test_invalid_framework_is_not_silently_changed_to_quarkus(self):
        result = self.run_command("convert-only", "--java-framework", "Sping", "--dry-run")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("Unknown Java framework", result.stderr)


class DoctorInteractiveTargetTests(unittest.TestCase):
    def select(self, answer="", **variables):
        source = (REPO / "doctor.sh").read_text()
        function = re.search(r"^select_conversion_target\(\) \{.*?^\}", source, re.M | re.S).group()
        env = {key: value for key, value in os.environ.items()
               if key not in ("TARGET_LANGUAGE", "JAVA_FRAMEWORK")}
        env.update(variables)
        script = function + '\nselect_conversion_target && printf "RESULT:%s:%s\\n" "$TARGET_LANGUAGE" "${JAVA_FRAMEWORK:-}"'
        return subprocess.run(["bash", "-c", script], input=answer, text=True,
                              capture_output=True, env=env, timeout=10)

    def test_interactive_spring_and_existing_quarkus_default(self):
        self.assertIn("RESULT:Java:Spring", self.select("3\n").stdout)
        self.assertIn("RESULT:Java:Quarkus", self.select("\n").stdout)

    def test_preselected_spring_does_not_consume_another_language_prompt(self):
        result = self.select(TARGET_LANGUAGE="Java", JAVA_FRAMEWORK="Spring")
        self.assertEqual(0, result.returncode, result.stderr)
        self.assertIn("RESULT:Java:Spring", result.stdout)
        self.assertNotIn("Select Target Language", result.stdout)


class DoctorConversionCommandTests(JclCommandFixture):
    def setUp(self):
        super().setUp()
        (self.root / "Config/load-config.sh").write_text("""
load_ai_config() { return 0; }
check_ai_connectivity() { echo "CHECKED-CONNECTIVITY"; [[ "${TEST_CONNECTIVITY_FAIL:-}" != true ]]; }
select_speed_profile() { return 0; }
print_jcl_job_summary() { return 0; }
get_migration_db_path() { printf '%s' "$REPO_ROOT/Data/migration.db"; }
""")
        self.env["MCP_AUTO_LAUNCH"] = "0"
        (self.bin / "pgrep").write_text("#!/bin/bash\nexit 1\n")
        (self.bin / "pgrep").chmod(0o755)
        self.selected_source = self.root / "selected source's"
        self.program = self.selected_source / "batch jobs/PAY'ONE.cbl"
        self.program.parent.mkdir(parents=True)
        self.program.write_text("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYONE.\n")

    def test_copied_selection_preserves_custom_roots_quotes_policy_and_language_through_doctor(self):
        for root in (str(self.selected_source), self.selected_source.name):
            with self.subTest(root=root):
                self.env.update(REKT_FACTS_FOLDER=str(self.root / "snapshot evidence"),
                                CONVERSION_DEPENDENCY_MODE="strict")
                result = self.run_command("convert-only", "--source", root, "--language", "Java",
                                          "--programs", "batch jobs/PAY'ONE.cbl")
                self.assert_success(result)
                self.assertIn("CHECKED-CONNECTIVITY", result.stdout)
                args = [call for call in self.calls("dotnet") if "--source" in call][-1]
                self.assertEqual(str(self.selected_source), args[args.index("--source") + 1])
                self.assertEqual("batch jobs/PAY'ONE.cbl", args[args.index("--programs") + 1])
                self.assertIn("--skip-reverse-engineering", args)
                self.assertEqual({
                    "REKT_FACTS_FOLDER": self.env["REKT_FACTS_FOLDER"],
                    "CONVERSION_DEPENDENCY_MODE": "strict", "TARGET_LANGUAGE": "Java"
                }, json.loads((self.root / "conversion-env.json").read_text()))

    def test_missing_selection_or_failed_connectivity_does_not_run_conversion(self):
        result = self.run_command("convert-only", "--source", str(self.selected_source),
                                  "--programs", "MISSING.cbl")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("No source file matches", result.stdout)
        self.env["TEST_CONNECTIVITY_FAIL"] = "true"
        result = self.run_command("convert-only", "--source", str(self.selected_source),
                                  "--programs", "batch jobs/PAY'ONE.cbl")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("fix connection issues", result.stdout)
        self.assertFalse(any("--source" in call for call in self.calls("dotnet")))

    def test_custom_source_is_reported_in_dry_run_and_rejected_for_native_scan(self):
        result = self.run_command("convert-only", "--source", str(self.selected_source), "--dry-run",
                                  "--programs", "batch jobs/PAY'ONE.cbl")
        self.assert_success(result)
        self.assertIn(str(self.selected_source), result.stdout)
        self.assertFalse(any("--source" in call for call in self.calls("dotnet")))
        rejected = self.run_command("rekt-full", "--source", str(self.selected_source))
        self.assertNotEqual(0, rejected.returncode)
        self.assertIn("--source is supported only", rejected.stderr)


if __name__ == "__main__":
    unittest.main()
