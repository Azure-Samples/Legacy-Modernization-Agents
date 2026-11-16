using System.Diagnostics;
using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Rekt;

// The portal shows scan.json as the native scan's lifecycle; a flag it shows must be measured, never assumed.
public sealed class NativeRunActivityTests : IDisposable
{
    private static readonly string Dll = Path.Join(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll");
    private readonly string _folder;

    // Resolved because evidence records resolved paths and macOS temp folders sit behind a symlink.
    public NativeRunActivityTests() =>
        _folder = PyCompat.Resolve(Directory.CreateDirectory(Path.Join(Path.GetTempPath(), $"native-activity-{Guid.NewGuid():N}")).FullName);

    public void Dispose() => Directory.Delete(_folder, recursive: true);

    [Fact]
    public void Completion_MeasuresCheckedArtifactsAndTheActualFailedExport()
    {
        var (source, output) = (Path.Join(_folder, "source"), Path.Join(_folder, "output", "rekt"));
        Directory.CreateDirectory(source);
        File.WriteAllText(Path.Join(source, "A.cbl"), "           GOBACK.\n");
        var report = Path.Join(output, "A.cbl.report");
        foreach (var directory in new[] { "flow_ast", "cfg", "data_structures" })
        {
            Directory.CreateDirectory(Path.Join(report, directory));
            File.WriteAllText(Path.Join(report, directory, "A.json"), """{"nodes":[1]}""");
        }
        File.WriteAllText(Path.Join(report, "dependency-evidence.json"),
            PyJson.Dumps(RektEvidence.BuildEvidence(source, "A.cbl", output, 19)));
        var path = Invoke("start", Path.Join(_folder, "catalogue"));
        Invoke("program", path, "--program", "A.cbl", "--status", "Partial", "--source-root", source, "--output-root", output);

        var row = Read(path)["programs"]![0]!;
        foreach (var key in new[] { "hasAst", "hasCfg", "hasData" }) PyJson.IsTrue(row[key]).Should().BeTrue(key);
        row["strictDependencyComplete"]!.GetValue<bool>().Should().BeFalse();
        row["dependencyExportExitCode"]!.GetValue<int>().Should().Be(19);
        row.AsObject().ContainsKey("structureExitCode").Should().BeFalse("a parser process exit was not captured");
        PyJson.Str(row["reportPath"]).Should().Be(report);
        var details = PyJson.Str(row["measurementReasons"]!["dependencyDetails"])!;
        details.Should().Contain("dependency-export-failed:19");
        details.Length.Should().BeLessThanOrEqualTo(1000);
        row["dependencyDiagnostics"]!.AsArray().Select(PyJson.Str).Should().Contain("dependency-export-failed:19");
        row["resolvedDefinitions"]!.AsArray().Should().BeEmpty();

        Invoke("program", path, "--program", "A.cbl", "--status", "running");
        var running = Read(path)["programs"]![0]!.AsObject();
        foreach (var key in new[] { "dependencyDiagnostics", "resolvedDefinitions", "inputSourceRoot", "strictDependencyComplete" })
            running.ContainsKey(key).Should().BeFalse(key);
    }

    [Fact]
    public void RecordedBindings_AreBoundedMetadataWithExplicitDiagnosticTruncation()
    {
        var (source, output) = (Path.Join(_folder, "source"), Path.Join(_folder, "output"));
        Directory.CreateDirectory(source);
        File.WriteAllText(Path.Join(source, "A.cbl"), "           GOBACK.\n");
        var copy = Path.Join(source, "SHARED.cpy");
        File.WriteAllText(copy, "       01 SHARED PIC X.\n");
        var evidence = RektEvidence.BuildEvidence(source, "A.cbl", output, 1);
        evidence["definitions"] = new JsonArray(new JsonObject
        {
            ["kind"] = "COPY", ["name"] = "SHARED", ["path"] = copy, ["sha256"] = PyCompat.Digest(copy),
        });
        evidence["unresolvedDiagnostics"] = new JsonArray(new[] { "native:unresolved-copybook:HEADER" }
            .Concat(Enumerable.Range(0, 40).Select(index => $"unknown-condition:{index}"))
            .Select(item => (JsonNode?)item).ToArray());
        var report = Path.Join(output, "A.cbl.report");
        Directory.CreateDirectory(report);
        File.WriteAllText(Path.Join(report, "dependency-evidence.json"), PyJson.Dumps(evidence));

        var row = NativeRunActivity.Measurements(source, output, "A.cbl");

        PyJson.Dumps(row["resolvedDefinitions"]).Should().Be(PyJson.Dumps(new JsonArray(new JsonObject
        {
            ["kind"] = "COPY", ["name"] = "SHARED", ["path"] = copy,
        })));
        row["dependencyDiagnostics"]!.AsArray().Should().HaveCount(32);
        PyJson.IsTrue(row["dependencyDiagnosticsTruncated"]).Should().BeTrue();
        PyJson.Str(row["inputSourceRoot"]).Should().Be(PyCompat.Resolve(source));
        row["strictDependencyComplete"]!.GetValue<bool>().Should().BeFalse();
    }

    [Fact]
    public void MissingInvalidAndIntegrityFailures_RemainUnknown()
    {
        var (source, output) = (Path.Join(_folder, "source"), Path.Join(_folder, "output"));
        Directory.CreateDirectory(source);
        var program = Path.Join(source, "A.cbl");
        File.WriteAllText(program, "           GOBACK.\n");

        var missing = NativeRunActivity.Measurements(source, output, "A.cbl");
        foreach (var key in new[] { "hasAst", "hasCfg", "hasData", "strictDependencyComplete" })
            missing[key].Should().BeNull(key);
        PyJson.Str(missing["measurementReasons"]!["hasAst"]).Should().Be("artifact-not-recorded");

        var report = Path.Join(output, "A.cbl.report");
        Directory.CreateDirectory(Path.Join(report, "flow_ast"));
        File.WriteAllText(Path.Join(report, "flow_ast", "A.json"), "{invalid");
        File.WriteAllText(Path.Join(report, "dependency-evidence.json"),
            PyJson.Dumps(RektEvidence.BuildEvidence(source, "A.cbl", output, 1)));
        File.WriteAllText(program, "           STOP RUN.\n");

        var invalid = NativeRunActivity.Measurements(source, output, "A.cbl");
        invalid["hasAst"].Should().BeNull();
        invalid["strictDependencyComplete"].Should().BeNull();
        invalid["dependencyExportExitCode"].Should().BeNull();
        PyJson.Str(invalid["measurementReasons"]!["strictDependencyComplete"])
            .Should().Be("dependency-evidence-invalid-or-integrity-mismatch");
    }

    [Fact]
    public void CachedCompletion_ChecksArtifactsInsteadOfAssumingFlags()
    {
        var path = Invoke("start", Path.Join(_folder, "catalogue"));
        Invoke("program", path, "--program", "A.cbl", "--status", "cached",
            "--source-root", Path.Join(_folder, "source"), "--output-root", Path.Join(_folder, "output"));

        var row = Read(path)["programs"]![0]!;
        row["hasAst"].Should().BeNull();
        row["strictDependencyComplete"].Should().BeNull();
    }

    [Fact]
    public void StrictComplete_RequiresCurrentNativeProofsAndExportIntegrity()
    {
        var (source, output) = (Path.Join(_folder, "source"), Path.Join(_folder, "output"));
        Directory.CreateDirectory(source);
        Directory.CreateDirectory(output);
        var program = Path.Join(source, "A.cbl");
        File.WriteAllText(program, "           GOBACK.\n");
        var export = Path.Join(output, "A.cbl-deps.json");
        var coverage = new JsonObject();
        foreach (var kind in RektEvidence.Kinds) coverage[kind] = "complete";
        var absence = new JsonObject();
        foreach (var (kind, proof) in RektEvidence.AbsenceProofs) absence[kind] = proof;
        File.WriteAllText(export, PyJson.Dumps(new JsonObject
        {
            ["dependencyExportSchemaVersion"] = 1, ["sourcePath"] = program, ["sourceSha256"] = PyCompat.Digest(program),
            ["callTarget"] = new JsonObject { ["name"] = "A.cbl", ["programReferenceType"] = "STATIC" },
            ["dependencies"] = new JsonArray(), ["dependencyAnalysisComplete"] = true,
            ["dependencyCoverage"] = coverage, ["dependencyAbsenceEvidence"] = absence, ["dependencyDiagnostics"] = new JsonArray(),
        }));
        var report = Path.Join(output, "A.cbl.report");
        Directory.CreateDirectory(report);
        var evidencePath = Path.Join(report, "dependency-evidence.json");
        File.WriteAllText(evidencePath, PyJson.Dumps(RektEvidence.BuildEvidence(source, "A.cbl", output, 0)));
        PyJson.IsTrue(Strict()).Should().BeTrue();

        var evidence = PyJson.Parse(File.ReadAllText(evidencePath))!.AsObject();
        evidence.Remove("coverage");
        File.WriteAllText(evidencePath, PyJson.Dumps(evidence));
        Strict().Should().BeNull();

        File.WriteAllText(evidencePath, PyJson.Dumps(RektEvidence.BuildEvidence(source, "A.cbl", output, 0)));
        File.WriteAllText(export, "{}");
        Strict().Should().BeNull();

        File.WriteAllText(export, "[]");
        evidence["coverage"] = coverage.DeepClone();
        evidence["exportSha256"] = PyCompat.Digest(export);
        File.WriteAllText(evidencePath, PyJson.Dumps(evidence));
        Strict().Should().BeNull();

        JsonNode? Strict() => NativeRunActivity.Measurements(source, output, "A.cbl")["strictDependencyComplete"];
    }

    [Fact]
    public void ConcurrentProgramUpdates_KeepEveryProgram()
    {
        var path = Invoke("start", Path.Join(_folder, "catalogue"));
        var programs = Enumerable.Range(0, 12).Select(index => $"P{index:00}.cbl").ToArray();
        var workers = programs.Select(name => Start("program", path, "--program", name, "--status", "Full")).ToArray();
        foreach (var worker in workers)
        {
            worker.WaitForExit(120_000).Should().BeTrue();
            worker.ExitCode.Should().Be(0);
        }

        Read(path)["programs"]!.AsArray().Select(row => PyJson.Str(row!["relativePath"])).Order()
            .Should().Equal(programs);
        Directory.GetFiles(Path.GetDirectoryName(path)!, "*.pending").Should().BeEmpty();
    }

    [Fact]
    public void RunningPartialAndCancelled_AreRecordedWithoutModelEvents()
    {
        var path = Invoke("start", _folder);
        var initial = Read(path);
        PyJson.Str(initial["status"]).Should().Be("running");
        initial.ContainsKey("finishedAt").Should().BeFalse();

        Invoke("program", path, "--program", "estate/A.cbl", "--status", "running");
        Invoke("finish", path, "--exit-code", "143");

        var final = Read(path);
        PyJson.Str(final["status"]).Should().Be("cancelled");
        PyJson.Str(final["programs"]![0]!["status"]).Should().Be("running");
        PyJson.Str(final["startedAt"]).Should().Be(PyJson.Str(initial["startedAt"]));
        string.CompareOrdinal(PyJson.Str(final["finishedAt"]), PyJson.Str(initial["startedAt"])).Should().BeGreaterThanOrEqualTo(0);
        final.ContainsKey("runId").Should().BeFalse();
        final.ContainsKey("model").Should().BeFalse();
    }

    [Fact]
    public void ProgramUpdates_PreserveExactPathsAndFailedOutcomes()
    {
        var path = Invoke("start", _folder);
        foreach (var (program, status) in new[] { ("one/A.cbl", "running"), ("two/A.cbl", "Failed"), ("one/A.cbl", "Partial") })
            Invoke("program", path, "--program", program, "--status", status);
        Invoke("finish", path, "--exit-code", "0");

        var final = Read(path);
        PyJson.Str(final["status"]).Should().Be("completed-with-gaps");
        var rows = final["programs"]!.AsArray();
        rows.Select(row => PyJson.Str(row!["relativePath"])).Should().BeEquivalentTo("two/A.cbl", "one/A.cbl");
        File.Exists(Path.ChangeExtension(path, ".pending")).Should().BeFalse();
        var finished = rows.Single(row => PyJson.Str(row!["relativePath"]) == "one/A.cbl")!;
        string.CompareOrdinal(PyJson.Str(finished["startedAt"]), PyJson.Str(finished["finishedAt"])).Should().BeLessThanOrEqualTo(0);
    }

    [Theory]
    [InlineData("", 7, "failed")]
    [InlineData("Full", 0, "completed")]
    [InlineData("Partial", 0, "completed-with-gaps")]
    [InlineData("Failed", 0, "completed-with-gaps")]
    [InlineData("running", 130, "cancelled")]
    [InlineData("running", 143, "cancelled")]
    public void DoctorWrapper_RecordsTerminalResultsWithoutRunningAScan(string programStatus, int exitCode, string expected)
    {
        var command = "PROGRAM_STATUS=\"$2\"; EXIT_CODE=\"$3\"\n" + DoctorSection("native_activity_program() {", "\nrun_rekt_parse_impl()")
            + """

            run_rekt_parse_impl() {
            echo "synthetic lifecycle test"
            if [[ -n "$PROGRAM_STATUS" ]]; then
            native_activity_program estate/A.cbl running
            native_activity_program estate/A.cbl "$PROGRAM_STATUS"
            fi
            return "$EXIT_CODE"
            }
            run_rekt_parse
            """;

        var result = Bash(_folder, command, programStatus, exitCode.ToString());

        result.ExitCode.Should().Be(exitCode, result.StdErr);
        var manifests = Directory.GetFiles(Path.Join(_folder, "output", "rekt-scans"), "scan.json", SearchOption.AllDirectories);
        manifests.Should().ContainSingle();
        var final = Read(manifests[0]);
        PyJson.Str(final["status"]).Should().Be(expected);
        final["exitCode"]!.GetValue<int>().Should().Be(exitCode);
        PyJson.Str(final["finishedAt"]).Should().NotBeNullOrEmpty();
        File.ReadAllText(Path.Join(Path.GetDirectoryName(manifests[0]), PyJson.Str(final["logPath"])))
            .Should().Contain("synthetic lifecycle test");
        if (programStatus.Length == 0)
        {
            final["programs"]!.AsArray().Should().BeEmpty();
            return;
        }
        var program = final["programs"]![0]!.AsObject();
        PyJson.Str(program["relativePath"]).Should().Be("estate/A.cbl");
        PyJson.Str(program["status"]).Should().Be(programStatus);
        PyJson.Str(program["startedAt"]).Should().NotBeNullOrEmpty();
        program.ContainsKey("finishedAt").Should().Be(programStatus != "running");
    }

    [Theory]
    [InlineData("rekt-full", 0, 0, "completed")]
    [InlineData("rekt-full", 0, 7, "failed")]
    [InlineData("rekt-full", 8, 0, "failed")]
    [InlineData("rekt-ingest", 0, 0, "completed")]
    [InlineData("rekt-ingest", 0, 7, "failed")]
    public void FullAndIngestOperations_OwnOneJournalUntilTheEntireOperationReturns(
        string operation, int parseExit, int ingestExit, string expected)
    {
        var command = "OPERATION=\"$2\"; PARSE_EXIT=\"$3\"; INGEST_EXIT=\"$4\"\n"
            + DoctorSection("native_activity_program() {", "\nrun_rekt_parse_impl()") + "\n"
            + DoctorSection("run_rekt_ingest() {", "\nrun_rekt_ingest_impl()") + "\n"
            + DoctorSection("run_rekt_full() {", "\nrun_rekt_status()") + "\n"
            + """
            run_rekt_parse_impl() {
                native_activity_program estate/A.cbl running
                native_activity_program estate/A.cbl Full
                [[ "$PARSE_EXIT" == 0 ]] || return "$PARSE_EXIT"
                native_activity_stage "Native JCL scan"
            }
            run_rekt_ingest_impl() {
                cp "$_REKT_ACTIVITY_PATH" "$REPO_ROOT/ingest-observation.json"
                return "$INGEST_EXIT"
            }
            case "$OPERATION" in
                rekt-full) run_rekt_full ;;
                rekt-ingest) run_rekt_ingest ;;
            esac
            """;

        var result = Bash(_folder, command, operation, parseExit.ToString(), ingestExit.ToString());

        result.ExitCode.Should().Be(expected == "completed" ? 0 : operation == "rekt-full" ? 1 : ingestExit, result.StdErr);
        var manifests = Directory.GetFiles(Path.Join(_folder, "output", "rekt-scans"), "scan.json", SearchOption.AllDirectories);
        manifests.Should().ContainSingle("a full operation must not create a second parse/ingest journal");
        var final = Read(manifests[0]);
        PyJson.Str(final["command"]).Should().Be(operation);
        PyJson.Str(final["status"]).Should().Be(expected);
        PyJson.Str(final["finishedAt"]).Should().NotBeNullOrEmpty();
        final.ContainsKey("graphSnapshotId").Should().BeFalse();
        var stages = final["stages"]!.AsArray();
        var observationPath = Path.Join(_folder, "ingest-observation.json");
        if (parseExit != 0)
        {
            File.Exists(observationPath).Should().BeFalse("ingestion must not run after a parse failure");
            PyJson.Str(stages[^1]!["name"]).Should().Be("Native parsing");
        }
        else
        {
            var observation = Read(observationPath);
            PyJson.Str(observation["name"]).Should().Be(PyJson.Str(final["name"]));
            PyJson.Str(observation["status"]).Should().Be("running");
            observation.ContainsKey("finishedAt").Should().BeFalse();
            PyJson.Str(observation["currentStage"]).Should().Be("Native graph ingestion");
            observation["stages"]!.AsArray()[^1]!.AsObject().ContainsKey("finishedAt").Should().BeFalse();
            PyJson.Str(stages[^1]!["status"]).Should().Be(expected);
            if (operation == "rekt-full")
            {
                stages.Select(stage => PyJson.Str(stage!["name"]))
                    .Should().Equal("Native parsing", "Native JCL scan", "Native graph ingestion");
                PyJson.Str(final["programs"]![0]!["status"]).Should().Be("Full");
            }
        }
        stages.Should().OnlyContain(stage => !string.IsNullOrEmpty(PyJson.Str(stage!["finishedAt"])));
    }

    private string Invoke(params string[] args)
    {
        var result = LmaCliProcess.Run(Path.Join(_folder, "cwd"), null, ["rekt-activity", .. args]);
        result.ExitCode.Should().Be(0, result.StdErr);
        return result.StdOut.Trim();
    }

    private Process Start(params string[] args)
    {
        var psi = new ProcessStartInfo("dotnet") { WorkingDirectory = Directory.CreateDirectory(Path.Join(_folder, "cwd")).FullName };
        foreach (var arg in new[] { Dll, "rekt-activity" }.Concat(args)) psi.ArgumentList.Add(arg);
        return Process.Start(psi)!;
    }

    private static JsonObject Read(string path) => PyJson.Parse(File.ReadAllText(path))!.AsObject();

    private static string DoctorSection(string start, string end)
    {
        var doctor = File.ReadAllText(Path.Join(RepoRoot(), "doctor.sh"));
        var from = doctor.IndexOf(start, StringComparison.Ordinal);
        from.Should().BeGreaterThanOrEqualTo(0, start);
        var to = doctor.IndexOf(end, from, StringComparison.Ordinal);
        to.Should().BeGreaterThan(from, end);
        return Section(doctor, "\nensure_lma_cli() {") + Section(doctor, "\nlma_cli() {") + doctor[from..to];
    }

    private static string Section(string doctor, string start)
    {
        var from = doctor.IndexOf(start, StringComparison.Ordinal);
        from.Should().BeGreaterThanOrEqualTo(0, start);
        return doctor[from..(doctor.IndexOf("\n}\n", from, StringComparison.Ordinal) + 3)];
    }

    // Only the lifecycle helpers run: no source, graph, containers or native CLI, and the journal comes from this build.
    private static (int ExitCode, string StdOut, string StdErr) Bash(string repoRoot, string script, params string[] args)
    {
        var psi = new ProcessStartInfo("bash") { RedirectStandardOutput = true, RedirectStandardError = true };
        foreach (var arg in new[] { "-c", "REPO_ROOT=\"$1\"\n" + script, "fixture", repoRoot }.Concat(args)) psi.ArgumentList.Add(arg);
        psi.Environment["_LMA_CLI_ASSEMBLY"] = Dll;
        using var process = Process.Start(psi)!;
        var stdErr = process.StandardError.ReadToEndAsync();
        var stdOut = process.StandardOutput.ReadToEnd();
        process.WaitForExit(120_000).Should().BeTrue();
        return (process.ExitCode, stdOut, stdErr.Result);
    }

    private static string RepoRoot()
    {
        for (var directory = new DirectoryInfo(AppContext.BaseDirectory); directory is not null; directory = directory.Parent)
            if (File.Exists(Path.Join(directory.FullName, "doctor.sh"))) return directory.FullName;
        throw new InvalidOperationException("doctor.sh not found above the test output");
    }
}
