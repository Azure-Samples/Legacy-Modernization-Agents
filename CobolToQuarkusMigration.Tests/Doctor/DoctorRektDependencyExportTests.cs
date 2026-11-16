using System.Security.Cryptography;
using System.Text.Json.Nodes;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Doctor;

// Drives doctor.sh rekt-parse with a mocked parser container; the genuine CLI judges the parser artifacts.
public sealed class DoctorRektDependencyExportTests : IDisposable
{
    private static readonly string[] Kinds = ["COPY", "CALL", "EXEC_CICS", "EXEC_SQL", "FILE_IO"];
    private const string Relative = "estate/PROG.cbl";

    private readonly DoctorFixture _doctor = new();
    private readonly string _program;

    public DoctorRektDependencyExportTests()
    {
        _program = Write(Relative, "       COPY REAL.\n           CALL 'SUBPROG'.\n");
        Write("estate/copy/REAL.cpy", "       01 REAL-FIELD PIC X.\n");
        var staleExport = Path.Join(Root, "output", Relative + "-deps.json");
        Directory.CreateDirectory(Path.GetDirectoryName(staleExport)!);
        File.WriteAllText(staleExport, """
            {"callTarget": {"name": "PROG.cbl", "programReferenceType": "STATIC"},
             "dependencies": [{"name": "REAL.cpy"}, {"callTarget": {"name": "SUBPROG"}, "dependencies": []}]}
            """);
    }

    public void Dispose() => _doctor.Dispose();

    private string Root => _doctor.Root;
    private string Source => Path.Join(Root, "source");
    private string RektOutput => Path.Join(Root, "output", "rekt");

    [Theory]
    [InlineData("", null, "unset")]
    [InlineData("/configured/input", null, "/configured/input")]
    [InlineData("/configured/input", "/shell/input", "/shell/input")]
    [InlineData("/configured/input", "", "")]
    public void DependencyInputs_PersistFromLocalConfig_WithoutOverridingExplicitShellValues(
        string configured, string? shellOverride, string expected)
    {
        var settings = DoctorScript.Split("# Optional dependency inputs preserve an explicitly empty shell override.\n", 2)[1]
            .Split("# A moved port", 2)[0];
        var env = DoctorFixture.ScrubbedEnvironment();
        env["CONFIGURED_INPUT"] = configured;
        if (shellOverride is not null)
            foreach (var key in new[] { "REKT_CICS_RESOURCE_DEFINITIONS", "REKT_DEPENDENCY_PROFILE", "REKT_SQL_DECLARATIONS" })
                env[key] = shellOverride;

        var output = Bash(env, "read_local_config_value() { printf \"%s\" \"$CONFIGURED_INPUT\"; }\n" + settings
            + "\nbash -c 'printf \"%s\\n\" \"${REKT_CICS_RESOURCE_DEFINITIONS-unset}\" \"${REKT_DEPENDENCY_PROFILE-unset}\" \"${REKT_SQL_DECLARATIONS-unset}\"'");

        output.Should().Equal(expected, expected, expected);
    }

    [Fact]
    public void EstateProfile_ReplacesConfiguredProfileOnlyForThatEstate()
    {
        var function = Between("rekt_dependency_profile_for() {", "\nrekt_source_programs() {");
        var profile = Path.Join(Root, "tools/cobol-rekt/contracts/estates/carddemo.json");
        Directory.CreateDirectory(Path.GetDirectoryName(profile)!);
        File.WriteAllText(profile, "{}");
        var env = DoctorFixture.ScrubbedEnvironment();
        env["REPO_ROOT"] = DoctorFixture.Posix(Root);
        env["REKT_DEPENDENCY_PROFILE"] = "/configured.json";

        var output = Bash(env, function + "\nfor estate in carddemo bank-of-z .all; do rekt_dependency_profile_for \"$estate\"; done");

        output.Should().Equal(DoctorFixture.Posix(profile), "/configured.json", "/configured.json");
    }

    [Fact]
    public void EstateCicsResources_UseItsOwnCsdFiles_UnlessConfiguredInsideTheEstate()
    {
        var function = Between("rekt_cics_resources_for() {", "\nrekt_source_programs() {");
        foreach (var path in new[] { "source/carddemo/csd/CARDDEMO.CSD", "source/carddemo/app-mq/csd/MQ.csd",
                     "source/carddemo/.preprocessed/OLD.csd", "source/bank-of-z/cics/defs.yaml" })
        {
            Directory.CreateDirectory(Path.GetDirectoryName(Path.Join(Root, path))!);
            File.WriteAllText(Path.Join(Root, path), "");
        }
        Directory.CreateDirectory(Path.Join(Root, "source/other-estate"));
        var env = DoctorFixture.ScrubbedEnvironment();
        env["REPO_ROOT"] = DoctorFixture.Posix(Root);
        env["REKT_CICS_RESOURCE_DEFINITIONS"] = "/source/bank-of-z/cics/defs.yaml";

        var output = Bash(env, function + "\nfor estate in carddemo bank-of-z other-estate .root; do rekt_cics_resources_for \"$estate\"; done");

        output.Should().Equal("/source/carddemo/app-mq/csd/MQ.csd:/source/carddemo/csd/CARDDEMO.CSD",
            "/source/bank-of-z/cics/defs.yaml", "/source/bank-of-z/cics/defs.yaml", "/source/bank-of-z/cics/defs.yaml");
    }

    [Fact]
    public void Doctor_ExportsAfterSuccessfulStructuralParse()
    {
        var (current, calls) = RunDoctor(structuralSuccess: true);

        current["exportExitCode"]!.GetValue<int>().Should().Be(0);
        calls.Count(call => call.Contains("run")).Should().Be(1);
        current["coverage"]!["COPY"]!.GetValue<string>().Should().Be("partial");
    }

    [Fact]
    public void Doctor_UsesExplicitPrebuiltCliForFactExtractionAndPruning()
    {
        var assembly = Path.Join(Root, "build output/CobolToQuarkusMigration.dll");

        RunDoctor(structuralSuccess: true, factsCli: assembly);

        var dotnetCalls = _doctor.Calls("dotnet");
        var facts = dotnetCalls.Where(call => call.Contains("program-facts")).ToList();
        facts.Should().HaveCount(2, "{0}", string.Join(" | ", dotnetCalls.Select(call => string.Join(' ', call))));
        facts.Select(call => call[2]).Should().BeEquivalentTo("extract", "prune-orphans");
        facts.Should().OnlyContain(call => call[0] == assembly && call[1] == "program-facts");
    }

    [Fact]
    public void Doctor_ForwardsExplicitCicsResourcePathsOnlyToDependencyExport()
    {
        const string paths = "/source/region one/resources.yaml:/source/region-two/resources.yaml";

        var (_, calls) = RunDoctor(structuralSuccess: true, cicsDefinitions: paths);

        var export = calls.Should().ContainSingle(call => call.Contains("dependency")).Which;
        export.Should().Contain("-e");
        DoctorFixture.After(export, "-e").Should().Be("REKT_CICS_RESOURCE_DEFINITIONS=" + paths);
        calls.Where(call => call.Contains("run")).Should().NotContain(call => call.Contains("-e"));
    }

    [Fact]
    public void FilteredNativeProgress_CountsSelectedProgramsNotDependencySearchContext()
    {
        RunDoctor(structuralSuccess: true, native: true);

        var manifest = Directory.GetDirectories(Path.Join(Root, "output/rekt-scans"))
            .Select(dir => Path.Join(dir, "scan.json")).Where(File.Exists).Should().ContainSingle().Which;
        var activity = JsonNode.Parse(File.ReadAllText(manifest))!;
        activity["expectedPrograms"]!.GetValue<int>().Should().Be(1);
        activity["programs"]!.AsArray().Select(row => row!["relativePath"]!.GetValue<string>()).Should().Equal(Relative);
        File.Exists(Path.Join(Source, "estate/SUBPROG.cbl")).Should().BeTrue();
        File.Exists(Path.Join(Source, "other/PROG.cbl")).Should().BeTrue();
    }

    [Fact]
    public void Doctor_SingleProgramFolder_ResolvesCopybooksFromSiblingFolders()
    {
        // A source/cbl + source/cpy layout: programs sit in one folder, so all of source/ is one estate.
        Directory.Delete(Path.Join(Source, "estate/copy"), recursive: true);
        Write("cpy/REAL.cpy", "       01 REAL-FIELD PIC X.\n");

        var (current, _) = RunDoctor(structuralSuccess: true, native: true, otherEstate: false, expectedEstate: ".all");

        current["unresolvedDiagnostics"]!.AsArray().Should().BeEmpty();
        Coverage(current).Should().Equal(Kinds.ToDictionary(kind => kind, _ => "complete"));
    }

    [Fact]
    public void Doctor_RetainsDependencyEvidenceWhenEveryStructuralAttemptFails()
    {
        var (current, calls) = RunDoctor(structuralSuccess: false);

        current["exportExitCode"]!.GetValue<int>().Should().Be(0);
        calls.Count(call => call.Contains("run")).Should().Be(3);
    }

    [Fact]
    public void Doctor_FailedExport_StillWritesFailureSidecarAndRetainsErrors()
    {
        var (current, _) = RunDoctor(structuralSuccess: true, exportSuccess: false);

        current["exportExitCode"]!.GetValue<int>().Should().Be(23);
        Strings(current["unresolvedDiagnostics"]).Should().Contain("dependency-export-failed:23");
        current["coverage"]!["COPY"]!.GetValue<string>().Should().Be("unavailable");
    }

    [Fact]
    public void Doctor_DependencyView_ResolvesGenuineSiblingWithoutChangingRootIdentity()
    {
        var (current, _) = RunDoctor(structuralSuccess: true, native: true);

        current["unresolvedDiagnostics"]!.AsArray().Should().BeEmpty(
            File.ReadAllText(Path.Join(RektOutput, Relative + ".parse.log")));
        Coverage(current).Should().Equal(Kinds.ToDictionary(kind => kind, _ => "complete"));
        var callee = Path.Join(Source, "estate/SUBPROG.cbl");
        var call = CallDefinitions(current).Should().ContainSingle().Which;
        call.Select(property => property.Key).Should().BeEquivalentTo("kind", "name", "path", "sha256");
        call["kind"]!.GetValue<string>().Should().Be("CALL");
        call["name"]!.GetValue<string>().Should().Be("SUBPROG");
        DoctorFixture.ShouldBeSamePath(call["path"]!.GetValue<string>(), DoctorFixture.RealPath(callee));
        call["sha256"]!.GetValue<string>().Should().Be(Convert.ToHexStringLower(SHA256.HashData(File.ReadAllBytes(callee))));
    }

    [Fact]
    public void Doctor_DependencyView_WithholdsDuplicateCalleeStemsAcrossExtensions()
    {
        var (current, _) = RunDoctor(structuralSuccess: true, native: true, duplicateCallee: "estate/dup/SUBPROG.cob");

        current["exportExitCode"]!.GetValue<int>().Should().Be(1);
        current["coverage"]!["CALL"]!.GetValue<string>().Should().Be("partial");
        Strings(current["unresolvedDiagnostics"]).Should().Contain("ambiguous-call-target:SUBPROG");
        CallDefinitions(current).Should().BeEmpty();
    }

    [Fact]
    public void Doctor_OtherEstateCannotShadowOrStageIntoThisEstate()
    {
        var (current, _) = RunDoctor(structuralSuccess: true, native: true, duplicateCallee: "other/SUBPROG.cob");

        current["unresolvedDiagnostics"]!.AsArray().Should().BeEmpty();
        var call = CallDefinitions(current).Should().ContainSingle().Which;
        DoctorFixture.ShouldBeSamePath(call["path"]!.GetValue<string>(), DoctorFixture.RealPath(Path.Join(Source, "estate/SUBPROG.cbl")));
        Directory.Exists(Path.Join(Source, "other/.preprocessed")).Should().BeFalse();
    }

    private (JsonNode Current, List<IReadOnlyList<string>> Calls) RunDoctor(bool structuralSuccess,
        bool exportSuccess = true, bool native = false, string? duplicateCallee = null, string? cicsDefinitions = null,
        string? factsCli = null, bool otherEstate = true, string expectedEstate = "estate")
    {
        _doctor.AddCheckoutFile("tools/preprocess-for-rekt.sh");
        var oldReport = Path.Join(RektOutput, Relative + ".report");
        Directory.CreateDirectory(oldReport);
        File.WriteAllText(Path.Join(oldReport, "data.json"), "{\"name\":\"DFHAID-STUB\"}");
        var other = Path.Join(RektOutput, "other/PROG.cbl-deps.json");
        Directory.CreateDirectory(Path.GetDirectoryName(other)!);
        File.WriteAllText(other, "unrelated");
        if (otherEstate) Write("other/PROG.cbl", "           DISPLAY 'OTHER'.\n");
        if (native)
        {
            Write("estate/SUBPROG.cbl", "           GOBACK.\n");
            if (duplicateCallee is not null) Write(duplicateCallee, "           DISPLAY 'NOT THE CALLEE'.\n");
        }

        _doctor.UseParserContainer(ParserContainer);
        var env = _doctor.Env;
        env["_REKT_PROGRAM_FILTER"] = Relative;
        env["_PROGRAM_FACTS"] = "false";
        env["STRUCTURAL_SUCCESS"] = structuralSuccess ? "true" : "false";
        env["EXPORT_SUCCESS"] = exportSuccess ? "true" : "false";
        env["NATIVE_EXPORT"] = native ? "true" : "false";
        env["EXPECT_ESTATE"] = expectedEstate;
        if (cicsDefinitions is not null) env["REKT_CICS_RESOURCE_DEFINITIONS"] = cicsDefinitions;
        if (factsCli is not null)
        {
            Directory.CreateDirectory(Path.GetDirectoryName(factsCli)!);
            File.WriteAllText(factsCli, "");
            env["MIGRATION_CLI_ASSEMBLY"] = factsCli;
            env["_PROGRAM_FACTS"] = "true";
            env["_PROGRAM_FACTS_PRUNE_ORPHANS"] = "true";
        }

        var result = _doctor.Run("rekt-parse");

        result.ShouldSucceed();
        var calls = _doctor.Calls("docker").Where(call => call.Count > 0 && call[0] == "exec")
            .Select(call => (IReadOnlyList<string>)call.Skip(1).ToList()).ToList();
        calls.Count(call => call.Contains("dependency")).Should().Be(1);
        File.ReadAllText(other).Should().Be("unrelated");
        Directory.GetFiles(Path.Join(RektOutput, ".previous"), "data.json", SearchOption.AllDirectories).Should().ContainSingle();
        var current = JsonNode.Parse(File.ReadAllText(Path.Join(RektOutput, Relative + ".report/dependency-evidence.json")))!;
        current["placeholderContaminated"]!.GetValue<bool>().Should().BeFalse();
        current["relativePath"]!.GetValue<string>().Should().Be(Relative);
        DoctorFixture.ShouldBeSamePath(current["originalSourcePath"]!.GetValue<string>(), DoctorFixture.RealPath(_program));
        File.ReadAllText(Path.Join(RektOutput, Relative + ".parse.log")).Should().Contain("export diagnostic retained");
        result.StdOut.Should().Contain("export diagnostic retained");
        var nativeLog = Directory.GetDirectories(Path.Join(Root, "output/rekt-scans"), "doctor-*")
            .Select(dir => Path.Join(dir, "native.log")).Where(File.Exists).Should().ContainSingle().Which;
        File.ReadAllText(nativeLog).Should().Contain("export diagnostic retained");
        foreach (var call in calls.Where(call => call.Contains("run") || call.Contains("dependency")))
        {
            var scope = call.Contains("dependency") ? ".dependency-input" : ".parser-input";
            call.Should().Contain(arg => arg.StartsWith("--srcDir=") && arg.Contains($"/{scope}/{Relative}"));
        }
        return (current, calls);
    }

    private static string DoctorScript { get; } =
        File.ReadAllText(Path.Join(DoctorFixture.RepoRoot, "doctor.sh")).ReplaceLineEndings("\n");

    private static string Between(string start, string end)
    {
        var from = DoctorScript.IndexOf(start, StringComparison.Ordinal);
        return DoctorScript[from..DoctorScript.IndexOf(end, from, StringComparison.Ordinal)];
    }

    private string[] Bash(Dictionary<string, string> env, string script)
    {
        var run = DoctorFixture.RunProcess(DoctorFixture.Bash, ["-c", script], Root, env, TimeSpan.FromSeconds(30));
        run.ShouldSucceed();
        var output = run.StdOut.ReplaceLineEndings("\n");
        if (output.EndsWith('\n')) output = output[..^1];
        return output.Length == 0 ? [] : output.Split('\n');
    }

    private string Write(string relative, string text)
    {
        var path = Path.Join(Source, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, text);
        return path;
    }

    private static Dictionary<string, string> Coverage(JsonNode current) =>
        current["coverage"]!.AsObject().ToDictionary(pair => pair.Key, pair => pair.Value!.GetValue<string>());

    private static List<string> Strings(JsonNode? array) =>
        array!.AsArray().Select(item => item!.GetValue<string>()).ToList();

    private static List<JsonObject> CallDefinitions(JsonNode current) =>
        current["definitions"]!.AsArray().Select(item => item!.AsObject())
            .Where(item => item["kind"]!.GetValue<string>() == "CALL").ToList();

    // Stands in for the parser container: smojol dependency/run calls write what the real parser
    // would, after checking that doctor staged exactly the inputs the parser may see.
    private const string ParserContainer = """
        #!/bin/bash
        args=("$@")
        root="$JCL_TEST_ROOT"
        shopt -s nullglob
        has() { local a; for a in "${args[@]}"; do [[ "$a" == "$1" ]] && return 0; done; return 1; }
        value_of() { local a; for a in "${args[@]}"; do [[ "$a" == "$1="* ]] && { printf '%s' "${a#*=}"; return 0; }; done; return 1; }
        fail() { echo "parser container: $*" >&2; exit 97; }
        sha256() { { sha256sum "$1" 2>/dev/null || shasum -a 256 "$1"; } | cut -d' ' -f1; }
        # name kind host-file container-path complete call-coverage diagnostics dependencies
        node() {
            printf '{"callTarget":{"name":"%s","programReferenceType":"STATIC"},"dependencyKind":"%s","sourcePath":"%s","sourceSha256":"%s",' \
                "$1" "$2" "$4" "$(sha256 "$3")"
            printf '"dependencyAnalysisComplete":%s,"dependencyCoverage":{"COPY":"complete","CALL":"%s","EXEC_CICS":"complete","EXEC_SQL":"complete","FILE_IO":"complete"},' \
                "${5:-true}" "${6:-complete}"
            printf '"dependencyAbsenceEvidence":{"EXEC_CICS":"antlr-tree:no-dialect-contexts","EXEC_SQL":"antlr-tree:no-dialect-contexts","FILE_IO":"antlr-tree:no-file-io-contexts"},'
            printf '"dependencyDiagnostics":%s,"dependencies":[%s]' "${7:-[]}" "${8:-}"
            [[ "$2" == PROGRAM ]] && printf ',"dependencyExportSchemaVersion":1'
            printf '}'
        }

        if has sh; then
            echo 10
            exit 0
        fi
        if ! has /app/smojol-cli.jar; then
            echo 5
            exit 0
        fi
        if has dependency; then
            echo "export diagnostic retained" >&2
            [[ "$EXPORT_SUCCESS" == true ]] || exit 23
            export_path="$(value_of --export)"
            path="$root/output/rekt/${export_path#/output/}"
            mkdir -p "$(dirname "$path")"
            if [[ "$NATIVE_EXPORT" != true ]]; then
                printf '{"callTarget":{"name":"PROG.cbl"},"dependencies":[{"name":"REAL.cpy"}]}' > "$path"
                exit 0
            fi
            source_dir="$(value_of --srcDir)"
            view="$root/source/${source_dir#/source/}"
            cmp -s "$view/PROG.cbl" "$root/source/estate/PROG.cbl" || fail "dependency view does not hold the program"
            # Each estate is staged separately; copybooks resolve only from the program's estate.
            estate_staging="${source_dir%%/.dependency-input/*}"
            [[ "$estate_staging" == "/source/.rekt-staging/$EXPECT_ESTATE" ]] || fail "staged in $estate_staging"
            copybook="$root/source/${estate_staging#/source/}/REAL.cpy"
            callees=()
            for candidate in "$view"/SUBPROG.*; do
                case "${candidate##*.}" in [cC][bB][lL]|[cC][oO][bB]) callees+=("$candidate") ;; esac
            done
            (( ${#callees[@]} <= 1 )) || fail "ambiguous program was exposed to native find-first lookup"
            real="$(node REAL.cpy COPY "$copybook" "$estate_staging/REAL.cpy")"
            if (( ${#callees[@]} == 1 )); then
                callee="$(node SUBPROG CALL "${callees[0]}" "$source_dir/$(basename "${callees[0]}")")"
                node PROG.cbl PROGRAM "$view/PROG.cbl" "$source_dir/PROG.cbl" true complete '[]' "$real,$callee" > "$path"
                exit 0
            fi
            callee='{"callTarget":{"name":"SUBPROG","programReferenceType":"STATIC"},"dependencyKind":"CALL","dependencies":[],"dependencyAnalysisComplete":false,"dependencyDiagnostics":["source-not-found:SUBPROG"]}'
            node PROG.cbl PROGRAM "$view/PROG.cbl" "$source_dir/PROG.cbl" false partial '["incomplete-callee:SUBPROG"]' "$real,$callee" > "$path"
            exit 1
        fi
        if has run && [[ "$STRUCTURAL_SUCCESS" == true ]]; then
            source_dir="$(value_of --srcDir)"
            [[ "$(ls -A "$root/source/${source_dir#/source/}")" == PROG.cbl ]] || fail "structural root isolation changed"
            report="$(value_of --reportDir)"
            path="$root/output/rekt/${report#/output/}/PROG.cbl.report"
            for kind in flow_ast cfg; do
                mkdir -p "$path/$kind"
                printf '{"id":"PROGRAM"}' > "$path/$kind/program.json"
            done
            exit 0
        fi
        echo "structural parse unavailable" >&2
        exit 1
        """;
}
