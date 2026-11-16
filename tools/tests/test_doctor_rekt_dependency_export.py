#!/usr/bin/env python3
# Drives doctor.sh's REKT parse path with a mocked parser container and the real .NET CLI.
import hashlib
import json
import os
import shutil
import subprocess
import sys
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
KINDS = ("COPY", "CALL", "EXEC_CICS", "EXEC_SQL", "FILE_IO")


class DoctorDependencyExportTests(unittest.TestCase):
    def setUp(self):
        self.work = REPO / "tools/tests/_work" / self._testMethodName
        self.work.mkdir(parents=True)
        self.source = self.work / "source"
        self.output = self.work / "output"
        self.source.mkdir()
        self.output.mkdir()
        self.relative = "estate/PROG.cbl"
        self.program = self.write("estate/PROG.cbl", "       COPY REAL.\n           CALL 'SUBPROG'.\n")
        self.definition = self.write("estate/copy/REAL.cpy", "       01 REAL-FIELD PIC X.\n")
        self.export = self.output / (self.relative + "-deps.json")
        self.export.parent.mkdir(parents=True)
        self.export.write_text(json.dumps({
            "callTarget": {"name": "PROG.cbl", "programReferenceType": "STATIC"},
            "dependencies": [{"name": "REAL.cpy"}, {"callTarget": {"name": "SUBPROG"}, "dependencies": []}],
        }))

    def tearDown(self):
        # Keep tools/tests/_work: Docker Desktop can miss bind mounts under a recreated parent.
        shutil.rmtree(self.work)

    def write(self, relative, text):
        path = self.source / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="latin-1")
        return path

    def test_dependency_inputs_persist_from_local_config_without_overriding_explicit_shell_values(self):
        script = (REPO / "doctor.sh").read_text()
        settings = script.split("# Optional dependency inputs preserve an explicitly empty shell override.\n", 1)[1]
        settings = settings.split("# A moved port", 1)[0]
        for configured, override, expected in [
            ("", None, "unset"), ("/configured/input", None, "/configured/input"),
            ("/configured/input", "/shell/input", "/shell/input"), ("/configured/input", "", ""),
        ]:
            with self.subTest(configured=configured, override=override):
                environment = dict(os.environ, CONFIGURED_INPUT=configured)
                for key in ("REKT_CICS_RESOURCE_DEFINITIONS", "REKT_DEPENDENCY_PROFILE", "REKT_SQL_DECLARATIONS"):
                    environment.pop(key, None)
                    if override is not None:
                        environment[key] = override
                result = subprocess.run(
                    ["bash", "-c", 'read_local_config_value() { printf "%s" "$CONFIGURED_INPUT"; }\n'
                     + settings + """\nbash -c 'printf "%s\\n" "${REKT_CICS_RESOURCE_DEFINITIONS-unset}" "${REKT_DEPENDENCY_PROFILE-unset}" "${REKT_SQL_DECLARATIONS-unset}"'"""],
                    env=environment, capture_output=True, text=True, check=True)
                self.assertEqual([expected, expected, expected], result.stdout.splitlines())

    def test_estate_profile_replaces_configured_profile_only_for_that_estate(self):
        script = (REPO / "doctor.sh").read_text()
        function = script[script.index("rekt_dependency_profile_for() {"):script.index("\nrekt_source_programs() {")]
        (self.work / "tools/cobol-rekt/contracts/estates").mkdir(parents=True)
        (self.work / "tools/cobol-rekt/contracts/estates/carddemo.json").write_text("{}")
        result = subprocess.run(
            ["bash", "-c", function + "\nfor estate in carddemo bank-of-z .all; do rekt_dependency_profile_for \"$estate\"; done"],
            env=dict(os.environ, REPO_ROOT=str(self.work), REKT_DEPENDENCY_PROFILE="/configured.json"),
            capture_output=True, text=True, check=True)
        self.assertEqual([str(self.work / "tools/cobol-rekt/contracts/estates/carddemo.json"),
                          "/configured.json", "/configured.json"], result.stdout.splitlines())

    def test_estate_cics_resources_use_its_own_csd_files_unless_configured_inside_the_estate(self):
        script = (REPO / "doctor.sh").read_text()
        function = script[script.index("rekt_cics_resources_for() {"):script.index("\nrekt_source_programs() {")]
        for path in ("source/carddemo/csd/CARDDEMO.CSD", "source/carddemo/app-mq/csd/MQ.csd",
                     "source/carddemo/.preprocessed/OLD.csd", "source/bank-of-z/cics/defs.yaml"):
            (self.work / path).parent.mkdir(parents=True, exist_ok=True)
            (self.work / path).write_text("")
        (self.work / "source/other-estate").mkdir(parents=True)
        result = subprocess.run(
            ["bash", "-c", function + "\nfor estate in carddemo bank-of-z other-estate .root; do rekt_cics_resources_for \"$estate\"; done"],
            env=dict(os.environ, REPO_ROOT=str(self.work),
                     REKT_CICS_RESOURCE_DEFINITIONS="/source/bank-of-z/cics/defs.yaml"),
            capture_output=True, text=True, check=True)
        self.assertEqual(["/source/carddemo/app-mq/csd/MQ.csd:/source/carddemo/csd/CARDDEMO.CSD",
                          "/source/bank-of-z/cics/defs.yaml", "/source/bank-of-z/cics/defs.yaml",
                          "/source/bank-of-z/cics/defs.yaml"], result.stdout.splitlines())

    def run_doctor(self, structural_success, export_success=True, native=False, duplicate_callee=False,
                   cics_definitions=None, facts_cli=None, other_estate=True, expected_estate="estate"):
        (self.work / "tools").mkdir()
        shutil.copy2(REPO / "tools/preprocess-for-rekt.sh", self.work / "tools/preprocess-for-rekt.sh")
        # Doctor uses output/rekt; keep all writes inside this test's fixture.
        output = self.work / "output/rekt"
        output.mkdir()
        old_report = output / (self.relative + ".report")
        old_report.mkdir(parents=True)
        (old_report / "data.json").write_text('{"name":"DFHAID-STUB"}')
        other = output / "other/PROG.cbl-deps.json"
        other.parent.mkdir()
        other.write_text("unrelated")
        if other_estate:
            self.write("other/PROG.cbl", "           DISPLAY 'OTHER'.\n")
        if native:
            self.write("estate/SUBPROG.cbl", "           GOBACK.\n")
            if duplicate_callee:
                self.write(duplicate_callee, "           DISPLAY 'NOT THE CALLEE'.\n")
        script = (REPO / "doctor.sh").read_text()
        functions = (script[script.index("ensure_lma_cli() {"):script.index("ensure_rekt_containers() {")]
                     + script[script.index("run_rekt_logged() {"):script.index("\nvalidate_rekt_jcl_source() {")])
        mock = self.work / "mock_parser.py"
        mock.write_text(r'''
import json, os, sys
from pathlib import Path
root = Path(os.environ["REPO_ROOT"])
args = sys.argv[1:]
with (root / "calls.jsonl").open("a") as log:
    log.write(json.dumps(args) + "\n")
if "sh" in args:
    print("10")
    sys.exit(0)
if "dependency" in args:
    print("export diagnostic retained", file=sys.stderr)
    if os.environ["EXPORT_SUCCESS"] != "true":
        sys.exit(23)
    export = next(arg.split("=", 1)[1] for arg in args if arg.startswith("--export="))
    path = root / "output/rekt" / export.removeprefix("/output/")
    path.parent.mkdir(parents=True, exist_ok=True)
    if os.environ["NATIVE_EXPORT"] == "true":
        import hashlib
        source_dir = next(arg.split("=", 1)[1] for arg in args if arg.startswith("--srcDir="))
        view = root / "source" / source_dir.removeprefix("/source/")
        assert (view / "PROG.cbl").read_bytes() == (root / "source/estate/PROG.cbl").read_bytes()
        coverage = dict.fromkeys(("COPY", "CALL", "EXEC_CICS", "EXEC_SQL", "FILE_IO"), "complete")
        absence = {"EXEC_CICS":"antlr-tree:no-dialect-contexts",
                   "EXEC_SQL":"antlr-tree:no-dialect-contexts",
                   "FILE_IO":"antlr-tree:no-file-io-contexts"}
        def node(name, kind, host, native_path):
            return {"callTarget":{"name":name,"programReferenceType":"STATIC"}, "dependencyKind":kind,
                    "sourcePath":native_path,"sourceSha256":hashlib.sha256(host.read_bytes()).hexdigest(),
                    "dependencyAnalysisComplete":True,"dependencyCoverage":dict(coverage),
                    "dependencyAbsenceEvidence":dict(absence),"dependencyDiagnostics":[],"dependencies":[]}
        document = node("PROG.cbl", "PROGRAM", view / "PROG.cbl", source_dir + "/PROG.cbl")
        document["dependencyExportSchemaVersion"] = 1
        # Each estate is staged separately; copybooks resolve only from the program's estate.
        estate_staging = source_dir.split("/.dependency-input/", 1)[0]
        assert estate_staging == "/source/.rekt-staging/" + os.environ["EXPECT_ESTATE"], estate_staging
        copybook = root / "source" / estate_staging.removeprefix("/source/") / "REAL.cpy"
        document["dependencies"].append(node("REAL.cpy", "COPY", copybook, estate_staging + "/REAL.cpy"))
        callees = [candidate for candidate in view.glob("SUBPROG.*")
                   if candidate.suffix.lower() in (".cbl", ".cob")]
        assert len(callees) <= 1, "ambiguous program was exposed to native find-first lookup"
        if callees:
            callee = callees[0]
            document["dependencies"].append(node("SUBPROG", "CALL", callee, source_dir + "/" + callee.name))
        else:
            document["dependencies"].append({"callTarget":{"name":"SUBPROG","programReferenceType":"STATIC"},
                "dependencyKind":"CALL","dependencies":[],"dependencyAnalysisComplete":False,
                "dependencyDiagnostics":["source-not-found:SUBPROG"]})
            document["dependencyAnalysisComplete"] = False
            document["dependencyCoverage"]["CALL"] = "partial"
            document["dependencyDiagnostics"] = ["incomplete-callee:SUBPROG"]
        path.write_text(json.dumps(document))
        sys.exit(0 if callees else 1)
    path.write_text(json.dumps({"callTarget":{"name":"PROG.cbl"},"dependencies":[{"name":"REAL.cpy"}]}))
    sys.exit(0)
if "run" in args and os.environ["STRUCTURAL_SUCCESS"] == "true":
    source_dir = next(arg.split("=",1)[1] for arg in args if arg.startswith("--srcDir="))
    isolated = root / "source" / source_dir.removeprefix("/source/")
    assert [file.name for file in isolated.iterdir()] == ["PROG.cbl"], "structural root isolation changed"
    report = next(arg.split("=",1)[1] for arg in args if arg.startswith("--reportDir="))
    path = root / "output/rekt" / report.removeprefix("/output/") / "PROG.cbl.report"
    for kind in ("flow_ast", "cfg"):
        (path / kind).mkdir(parents=True, exist_ok=True)
        (path / kind / "program.json").write_text('{"id":"PROGRAM"}')
    sys.exit(0)
print("structural parse unavailable", file=sys.stderr)
sys.exit(1)
''')
        harness = r'''
resolve_runtime_setting() { echo source; }
validate_rekt_jcl_source() { return 0; }
detect_docker_api_version() { return 0; }
ensure_rekt_containers() { return 0; }
scan_rekt_jcl() { return 0; }
run_compose() { echo "Unexpected container restart" >&2; return 1; }
container_exec() { "$PYTHON_CMD" "$REPO_ROOT/mock_parser.py" "$@"; }
''' + functions + "\nrun_rekt_parse\n"
        cli = Path(os.environ.get("JCL_TEST_DLL", REPO / "bin/Debug/net10.0/CobolToQuarkusMigration.dll"))
        self.assertTrue(cli.is_file(), "Build CobolToQuarkusMigration.csproj before running the doctor tests")
        # The genuine CLI judges parser artifacts; only the parser and fact extraction are mocked.
        env = dict(os.environ, REPO_ROOT=str(self.work), PYTHON_CMD=sys.executable,
                   _LMA_CLI_ASSEMBLY=str(cli), DOTNET_CMD=shutil.which("dotnet"),
                   REKT_CONTAINER="mock-parser", REKT_SERVICE="mock-service",
                   REKT_STUB_MARKER="AUTO-GENERATED STUB COPYBOOK",
                   REKT_GENERATED_COPYBOOK_DIRS="copy-generated",
                   REKT_PREPROCESS_RULES=str(self.work / "no-rules.json"),
                   _REKT_PROGRAM_FILTER=self.relative, _PROGRAM_FACTS="false",
                   STRUCTURAL_SUCCESS=str(structural_success).lower(),
                   EXPORT_SUCCESS=str(export_success).lower(),
                   NATIVE_EXPORT=str(native).lower(), EXPECT_ESTATE=expected_estate,
                   BLUE="", CYAN="", GREEN="", RED="", YELLOW="", NC="")
        env.pop("REKT_CICS_RESOURCE_DEFINITIONS", None)
        if cics_definitions is not None:
            env["REKT_CICS_RESOURCE_DEFINITIONS"] = cics_definitions
        if facts_cli is not None:
            (self.work / "CobolToQuarkusMigration.csproj").touch()
            commands = self.work / "commands"
            commands.mkdir()
            dotnet = commands / "dotnet"
            dotnet.write_text(f"#!{sys.executable}\n"
                              "import json, os, sys\n"
                              "from pathlib import Path\n"
                              "with (Path(os.environ['REPO_ROOT']) / 'dotnet-calls.jsonl').open('a') as log:\n"
                              "    log.write(json.dumps(sys.argv[1:]) + '\\n')\n")
            dotnet.chmod(0o755)
            env.update(PATH=str(commands) + os.pathsep + env["PATH"],
                       MIGRATION_CLI_ASSEMBLY=facts_cli, _PROGRAM_FACTS="true",
                       _PROGRAM_FACTS_PRUNE_ORPHANS="true")
        result = subprocess.run(["bash", "-c", harness], cwd=self.work, env=env,
                                text=True, capture_output=True)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        calls = [json.loads(line) for line in (self.work / "calls.jsonl").read_text().splitlines()]
        self.assertEqual(1, sum("dependency" in call for call in calls), calls)
        self.assertEqual("unrelated", other.read_text())
        archived = list((output / ".previous").rglob("data.json"))
        self.assertEqual(1, len(archived))
        current = json.loads((output / (self.relative + ".report/dependency-evidence.json")).read_text())
        self.assertFalse(current["placeholderContaminated"])
        self.assertEqual(self.relative, current["relativePath"])
        self.assertEqual(str(self.program.resolve()), current["originalSourcePath"])
        self.assertIn("export diagnostic retained", (output / (self.relative + ".parse.log")).read_text())
        self.assertIn("export diagnostic retained", result.stdout)
        native_logs = list((self.work / "output/rekt-scans").glob("doctor-*/native.log"))
        self.assertEqual(1, len(native_logs))
        self.assertIn("export diagnostic retained", native_logs[0].read_text())
        for call in calls:
            if "run" in call or "dependency" in call:
                source_dir = next(arg for arg in call if arg.startswith("--srcDir="))
                scope = ".dependency-input" if "dependency" in call else ".parser-input"
                self.assertIn(f"/{scope}/estate/PROG.cbl", source_dir)
        return current, calls

    def test_doctor_exports_after_successful_structural_parse(self):
        current, calls = self.run_doctor(True)
        self.assertEqual(0, current["exportExitCode"])
        self.assertEqual(1, sum("run" in call for call in calls))
        self.assertEqual("partial", current["coverage"]["COPY"])

    def test_doctor_uses_explicit_prebuilt_cli_for_fact_extraction_and_pruning(self):
        assembly = str(self.work / "build output/CobolToQuarkusMigration.dll")
        self.run_doctor(True, facts_cli=assembly)
        calls = [json.loads(line) for line in (self.work / "dotnet-calls.jsonl").read_text().splitlines()]
        facts = [call for call in calls if "program-facts" in call]
        self.assertEqual(2, len(facts), calls)
        self.assertEqual({"extract", "prune-orphans"}, {call[2] for call in facts})
        self.assertTrue(all(call[0] == assembly and call[1] == "program-facts" for call in facts))

    def test_doctor_forwards_explicit_cics_resource_paths_only_to_dependency_export(self):
        paths = "/source/region one/resources.yaml:/source/region-two/resources.yaml"
        _, calls = self.run_doctor(True, cics_definitions=paths)
        export, = [call for call in calls if "dependency" in call]
        self.assertIn("-e", export)
        self.assertEqual("REKT_CICS_RESOURCE_DEFINITIONS=" + paths, export[export.index("-e") + 1])
        self.assertFalse(any("-e" in call for call in calls if "run" in call))

    def test_filtered_native_progress_counts_selected_programs_not_dependency_search_context(self):
        self.run_doctor(True, native=True)
        manifests = list((self.work / "output/rekt-scans").glob("*/scan.json"))
        self.assertEqual(1, len(manifests))
        activity = json.loads(manifests[0].read_text())
        self.assertEqual(1, activity["expectedPrograms"])
        self.assertEqual([self.relative], [row["relativePath"] for row in activity["programs"]])
        self.assertTrue((self.source / "estate/SUBPROG.cbl").exists())
        self.assertTrue((self.source / "other/PROG.cbl").exists())

    def test_doctor_single_program_folder_resolves_copybooks_from_sibling_folders(self):
        # A source/cbl + source/cpy style layout: programs sit in one folder only, so all of
        # source/ is one estate and the sibling copybook folder is visible.
        shutil.rmtree(self.source / "estate/copy")
        self.definition = self.write("cpy/REAL.cpy", "       01 REAL-FIELD PIC X.\n")
        current, _ = self.run_doctor(True, native=True, other_estate=False, expected_estate=".all")
        self.assertEqual([], current["unresolvedDiagnostics"])
        self.assertEqual(dict.fromkeys(KINDS, "complete"), current["coverage"])

    def test_doctor_retains_dependency_evidence_when_every_structural_attempt_fails(self):
        current, calls = self.run_doctor(False)
        self.assertEqual(0, current["exportExitCode"])
        self.assertEqual(3, sum("run" in call for call in calls))

    def test_doctor_failed_export_still_writes_failure_sidecar_and_retains_errors(self):
        current, _ = self.run_doctor(True, False)
        self.assertEqual(23, current["exportExitCode"])
        self.assertIn("dependency-export-failed:23", current["unresolvedDiagnostics"])
        self.assertEqual("unavailable", current["coverage"]["COPY"])

    def test_doctor_dependency_view_resolves_genuine_sibling_without_changing_root_identity(self):
        current, _ = self.run_doctor(True, native=True)
        self.assertEqual([], current["unresolvedDiagnostics"],
                         (self.work / "output/rekt" / (self.relative + ".parse.log")).read_text())
        self.assertEqual(dict.fromkeys(KINDS, "complete"), current["coverage"])
        calls = [item for item in current["definitions"] if item["kind"] == "CALL"]
        self.assertEqual([{"kind": "CALL", "name": "SUBPROG",
                           "path": str(self.source / "estate/SUBPROG.cbl"),
                           "sha256": hashlib.sha256((self.source / "estate/SUBPROG.cbl").read_bytes()).hexdigest()}], calls)

    def test_doctor_dependency_view_withholds_duplicate_callee_stems_across_extensions(self):
        current, _ = self.run_doctor(True, native=True, duplicate_callee="estate/dup/SUBPROG.cob")
        self.assertEqual(1, current["exportExitCode"])
        self.assertEqual("partial", current["coverage"]["CALL"])
        self.assertIn("ambiguous-call-target:SUBPROG", current["unresolvedDiagnostics"])
        self.assertFalse(any(item["kind"] == "CALL" for item in current["definitions"]))

    def test_doctor_other_estate_cannot_shadow_or_stage_into_this_estate(self):
        current, _ = self.run_doctor(True, native=True, duplicate_callee="other/SUBPROG.cob")
        self.assertEqual([], current["unresolvedDiagnostics"])
        self.assertEqual([str(self.source / "estate/SUBPROG.cbl")],
                         [item["path"] for item in current["definitions"] if item["kind"] == "CALL"])
        self.assertFalse((self.source / "other/.preprocessed").exists())


if __name__ == "__main__":
    unittest.main()
