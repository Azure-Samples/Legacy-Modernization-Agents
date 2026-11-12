import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


REPO = Path(__file__).resolve().parents[2]


def _bash():
    # On Windows a bare "bash" resolves to the System32 WSL stub before Git Bash.
    if os.name == "nt" and (git := shutil.which("git")):
        for candidate in (Path(git).parent.parent / "bin" / "bash.exe", Path(git).parent / "bash.exe"):
            if candidate.exists():
                return str(candidate)
    return shutil.which("bash") or "bash"


BASH = _bash()

STUB = r'''
import json, os, subprocess, sys
from pathlib import Path

args = sys.argv[2:]
kind = sys.argv[1]
root = Path(os.environ["JCL_TEST_ROOT"])
with open(root / "calls.jsonl", "a", encoding="utf-8") as log:
    log.write(json.dumps([kind, args]) + "\n")
if kind == "docker":
    if args[:1] == ["version"]:
        print("1.45")
    elif args[:1] == ["exec"]:
        if os.environ.get("JCL_TEST_AUTH_FAIL") and "RETURN 1" in args:
            sys.exit(3)
        if "run" in args and "java" in args:
            name = args[args.index("run") + 1]
            report = root / "output/rekt" / (name + ".report")
            for folder in ("flow_ast", "cfg"):
                (report / folder).mkdir(parents=True, exist_ok=True)
                (report / folder / (folder + ".json")).write_text("{}")
        else:
            print("5")
elif kind == "dotnet":
    if "run" in args and "--source" in args:
        (root / "conversion-env.json").write_text(json.dumps({
            key: os.environ.get(key) for key in ("REKT_FACTS_FOLDER", "CONVERSION_DEPENDENCY_MODE", "TARGET_LANGUAGE")
        }))
    if args == ["--list-runtimes"]:
        print("Microsoft.NETCore.App 10.0.0")
    elif "jcl-facts" in args:
        if os.environ.get("JCL_TEST_FAIL"):
            sys.exit(7)
        command = args[args.index("jcl-facts"):]
        if os.environ.get("JCL_REAL_DLL"):
            sys.exit(subprocess.call([os.environ["JCL_REAL_DOTNET"], os.environ["JCL_REAL_DLL"], *command]))
        output = Path(args[args.index("--output-dir") + 1])
        output.mkdir(parents=True, exist_ok=True)
        (output / "SAMPLE.JCL.job.json").write_text('{"job":{"name":"SAMPLE"}}')
        print("jcl-facts: wrote 1 job fact file(s)")
    elif "jcl-jobs" in args:
        sys.exit(subprocess.call([os.environ["JCL_REAL_DOTNET"], os.environ["JCL_REAL_DLL"], *args[args.index("jcl-jobs"):]]))
elif kind == "populator":
    if args[:1] == ["-c"]:
        sys.exit(0)
    if args[:2] != ["populator.py", "ingest"]:
        sys.exit(91)
    if (root / "output/rekt/.jcl-scan-failed").exists():
        sys.exit(92)
    files = sorted(str(p.relative_to(root / "output/rekt")) for p in (root / "output/rekt").rglob("*.job.json"))
    (root / "ingested.json").write_text(json.dumps(files))
else:
    sys.exit(93)
'''


class JclCommandFixture(unittest.TestCase):
    def setUp(self):
        self.scratch = tempfile.TemporaryDirectory(prefix="jcl-test-")
        self.addCleanup(self.scratch.cleanup)
        self.root = Path(self.scratch.name) / "checkout with spaces"
        self.root.mkdir()
        self.bin = self.root / "controlled-bin"
        self.bin.mkdir()
        for relative in ("doctor.sh", "tools/lib/ports.sh", "tools/rekt_source_identity.py",
                         "tools/rekt_dependency_evidence.py", "tools/native_run_activity.py"):
            target = self.root / relative
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(REPO / relative, target)
        (self.root / "source").mkdir()
        (self.root / "Config").mkdir()
        (self.root / "CobolToQuarkusMigration.csproj").write_text("<Project/>")
        (self.root / "boundary.py").write_text(STUB)
        self.env = {k: v for k, v in os.environ.items() if not (
            k.startswith(("JCL_", "COBOL_", "_REKT_", "NEO4J_", "REKT_", "ApplicationSettings__", "MIGRATION_",
                          "AZURE_OPENAI_", "AISETTINGS__", "COPILOT_", "OPENAI_"))
            or k in ("PROGRAM_SELECTION", "TARGET_LANGUAGE", "JAVA_OUTPUT_FOLDER", "CSHARP_OUTPUT_FOLDER",
                     "GITHUB_TOKEN", "GH_TOKEN"))}
        self.env.update(PATH=str(self.bin) + os.pathsep + os.environ["PATH"],
                        JCL_TEST_ROOT=str(self.root), CopilotSkipCliDownload="true",
                        NEO4J_PASSWORD="isolated-test-only", TMPDIR=self.root.as_posix(),
                        TEMP=self.root.as_posix(), TMP=self.root.as_posix())
        for name in ("docker", "dotnet"):
            self.executable(self.bin / name, name)
        populator = self.root / "tools/graph-populator"
        populator.mkdir(parents=True)
        (populator / "populator.py").write_text("")
        (populator / "requirements.txt").write_text("")
        self.executable(populator / ".venv/bin/python", "populator")

    def executable(self, path, kind):
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(f'#!/bin/bash\nexec "{Path(sys.executable).as_posix()}" '
                        f'"{(self.root / "boundary.py").as_posix()}" {kind} "$@"\n', newline="\n")
        path.chmod(0o755)

    def job(self, relative="source/nested jobs/SAMPLE.JCL", text="//SAMPLE JOB\n//S1 EXEC PGM=IEFBR14\n"):
        path = self.root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)
        return path

    def run_command(self, *args):
        return subprocess.run([BASH, (self.root / "doctor.sh").as_posix(), *args],
                              cwd=self.root, env=self.env, capture_output=True, text=True,
                              encoding="utf-8", errors="replace", timeout=90)

    def calls(self, kind):
        log = self.root / "calls.jsonl"
        return [args for name, args in map(json.loads, log.read_text().splitlines()) if name == kind] if log.exists() else []

    def assert_success(self, result):
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)


class DoctorJclScanTests(JclCommandFixture):
    def test_native_jcl_scan_uses_the_explicit_isolated_cli_without_building(self):
        self.job()
        assembly = self.root / "isolated output/Migration.dll"
        self.env["MIGRATION_CLI_ASSEMBLY"] = str(assembly)
        self.assert_success(self.run_command("rekt-parse"))
        facts = next(args for args in self.calls("dotnet") if "jcl-facts" in args)
        self.assertEqual(str(assembly), facts[0])
        self.assertNotIn("run", facts)
        self.assertNotIn("--project", facts)

    def test_jcl_only_full_dispatches_nested_uppercase_facts_then_ingest_without_scaffolding(self):
        self.job()
        evidence = self.job("output/rekt/old.report/retained.log", "retained")
        self.assert_success(self.run_command("rekt-full"))
        calls = self.calls("dotnet")
        facts = next(args for args in calls if "jcl-facts" in args)
        self.assertEqual(self.root / "source", Path(facts[facts.index("jcl-facts") + 1]))
        self.assertEqual(self.root / "output/rekt", Path(facts[facts.index("--output-dir") + 1]))
        self.assertFalse(any("jcl-jobs" in args or "build" in args for args in calls))
        self.assertEqual(["SAMPLE.JCL.job.json"], json.loads((self.root / "ingested.json").read_text()))
        self.assertEqual("retained", evidence.read_text())
        self.assertFalse(any("java" in args for args in self.calls("docker")))

    def test_relative_absolute_and_backslash_roots_preserve_space_arguments(self):
        for configured in ("custom jobs", str(self.root / "custom jobs"),
                           str(self.root / "custom jobs").replace("/", "\\")):
            with self.subTest(configured=configured):
                self.job("custom jobs/nested/SAMPLE.JCL")
                self.env["JCL_SOURCE_FOLDER"] = configured
                self.assert_success(self.run_command("rekt-parse"))
                facts = [args for args in self.calls("dotnet") if "jcl-facts" in args][-1]
                self.assertEqual(self.root / "custom jobs", Path(facts[facts.index("jcl-facts") + 1]))
                self.assertFalse(self.calls("populator"))
                self.assertFalse(self.calls("docker"))

    def test_windows_drive_and_unc_roots_are_not_prefixed_with_checkout(self):
        for configured in ("Q:\\external jobs", "Q:/external jobs", "//host/share/jobs"):
            with self.subTest(configured=configured):
                self.env["JCL_SOURCE_FOLDER"] = configured
                result = self.run_command("rekt-parse")
                self.assertNotEqual(0, result.returncode)
                self.assertNotIn(str(self.root) + "/" + configured.replace("\\", "/"), result.stdout)
                self.assertFalse(self.calls("populator"))

    def test_local_config_root_is_used_without_ai_configuration(self):
        self.job("configured jobs/SAMPLE.jcl")
        (self.root / "Config/ai-config.local.env").write_text('JCL_SOURCE_FOLDER="configured jobs"\n')
        self.assert_success(self.run_command("rekt-parse"))
        facts = next(args for args in self.calls("dotnet") if "jcl-facts" in args)
        self.assertEqual(self.root / "configured jobs", Path(facts[facts.index("jcl-facts") + 1]))

    def test_empty_root_setting_defaults_to_source(self):
        self.job()
        self.env["JCL_SOURCE_FOLDER"] = ""
        self.assert_success(self.run_command("rekt-parse"))

    def test_nonempty_shell_root_wins_over_local_config(self):
        self.job("environment jobs/SAMPLE.JCL")
        (self.root / "Config/ai-config.local.env").write_text('JCL_SOURCE_FOLDER="missing folder"\n')
        self.env["JCL_SOURCE_FOLDER"] = "environment jobs"
        self.assert_success(self.run_command("rekt-parse"))
        facts = next(args for args in self.calls("dotnet") if "jcl-facts" in args)
        self.assertEqual(self.root / "environment jobs", Path(facts[facts.index("jcl-facts") + 1]))

    def test_absent_empty_or_rejected_graph_config_never_runs_ingest(self):
        self.job()
        for setting in ("absent", "empty", "bad password"):
            with self.subTest(setting=setting):
                config = self.root / "Config/ai-config.local.env"
                self.env.pop("NEO4J_PASSWORD", None)
                if setting == "empty":
                    config.write_text('NEO4J_PASSWORD=""\n')
                elif setting == "bad password":
                    self.env["NEO4J_PASSWORD"] = "rejected-test-only"
                    self.env["JCL_TEST_AUTH_FAIL"] = "1"
                result = self.run_command("rekt-full")
                self.assertNotEqual(0, result.returncode)
                self.assertFalse(self.calls("populator"))
                if setting == "absent":
                    self.assertIn("configuration is missing", result.stdout)
                elif setting == "empty":
                    self.assertIn("NEO4J_PASSWORD is empty", result.stdout)
                else:
                    self.assertIn("does not authenticate", result.stdout)

    def test_missing_or_empty_configured_root_refuses_before_destructive_work(self):
        for configured in ("missing folder", "empty folder"):
            with self.subTest(configured=configured):
                (self.root / "empty folder").mkdir(exist_ok=True)
                evidence = self.job("output/rekt/retained.job.json", "retained")
                self.env["JCL_SOURCE_FOLDER"] = configured
                self.assertNotEqual(0, self.run_command("rekt-full").returncode)
                self.assertEqual("retained", evidence.read_text())
                self.assertFalse(self.calls("populator"))
                self.assertFalse(self.calls("docker"))

    def test_filter_refusal_preserves_evidence_and_never_extracts_or_ingests(self):
        self.job()
        evidence = self.job("output/rekt/retained.job.json", "retained")
        self.env["_REKT_PROGRAM_FILTER"] = "ABSENT.cbl"
        result = self.run_command("rekt-full")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("program filter", result.stdout)
        self.assertEqual("retained", evidence.read_text())
        self.assertFalse(any("jcl-facts" in args for args in self.calls("dotnet")))
        self.assertFalse(self.calls("populator"))

    def test_extraction_failure_stops_full_pipeline_and_preserves_evidence(self):
        self.job()
        evidence = self.job("output/rekt/retained.job.json", "retained")
        self.env["JCL_TEST_FAIL"] = "1"
        result = self.run_command("rekt-full")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("JCL scan failed", result.stdout)
        self.assertEqual("retained", evidence.read_text())
        self.assertTrue((self.root / "output/rekt/.jcl-scan-failed").exists())
        self.assertFalse(self.calls("populator"))

    def test_no_sources_is_not_success(self):
        self.assertNotEqual(0, self.run_command("rekt-full").returncode)
        self.assertFalse(self.calls("populator"))

    def test_documented_cobol_root_limit_is_preserved(self):
        self.job()
        self.env["COBOL_SOURCE_FOLDER"] = "other"
        result = self.run_command("rekt-full")
        self.assertNotEqual(0, result.returncode)
        self.assertIn("parser reads source/", result.stdout)
        self.assertFalse(self.calls("docker"))

    def test_cobol_only_still_parses_and_explicitly_refreshes_empty_jcl(self):
        self.job("source/PROGRAM.cbl", "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROGRAM.\n")
        self.assert_success(self.run_command("rekt-full"))
        self.assertTrue(any("java" in args and "run" in args for args in self.calls("docker")))
        facts = next(args for args in self.calls("dotnet") if "jcl-facts" in args)
        self.assertIn("--allow-empty", facts)
        self.assertTrue((self.root / "ingested.json").exists())


if __name__ == "__main__":
    unittest.main()
