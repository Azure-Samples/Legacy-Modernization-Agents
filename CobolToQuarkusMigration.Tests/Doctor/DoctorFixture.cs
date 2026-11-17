using System.Collections;
using System.Diagnostics;
using System.Text;
using System.Text.RegularExpressions;
using FluentAssertions;

namespace CobolToQuarkusMigration.Tests.Doctor;

public sealed record DoctorRun(int ExitCode, string StdOut, string StdErr)
{
    public string Output => StdOut + StdErr;

    public void ShouldSucceed() => ExitCode.Should().Be(0, "{0}", Output);
}

// Runs the real doctor.sh from a throwaway checkout with fake docker/dotnet on PATH. The fakes are
// bash so the harness needs no Python; parser artifacts are still judged by the genuine CLI.
public sealed class DoctorFixture : IDisposable
{
    // The doctor paths under test shell out to these helpers, so they must exist in the checkout.
    public static readonly string[] CheckoutFiles =
    [
        "doctor.sh",
        "tools/lib/ports.sh",
        "tools/lib/lma-cli.sh",
    ];

    private static readonly string[] ScrubbedPrefixes =
    [
        "JCL_", "COBOL_", "_REKT_", "NEO4J_", "REKT_", "ApplicationSettings__", "MIGRATION_",
        "AZURE_OPENAI_", "AISETTINGS__", "COPILOT_", "OPENAI_",
    ];

    private static readonly string[] ScrubbedNames =
    [
        "PROGRAM_SELECTION", "TARGET_LANGUAGE", "JAVA_OUTPUT_FOLDER", "CSHARP_OUTPUT_FOLDER", "GITHUB_TOKEN", "GH_TOKEN",
    ];

    private static readonly TimeSpan RunTimeout = TimeSpan.FromSeconds(90);
    private static readonly StringComparison KeyComparison =
        OperatingSystem.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;

    public static string RepoRoot { get; } = FindRepoRoot();
    public static string Bash { get; } = ResolveBash();
    public static string RealDotnet { get; } = ResolveDotnet();
    public static string CliAssembly { get; } =
        Environment.GetEnvironmentVariable("JCL_TEST_DLL") is { Length: > 0 } dll
            ? dll
            : Path.Join(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll");

    private readonly DirectoryInfo _scratch = Directory.CreateTempSubdirectory("jcl-test-");

    public string Root { get; }
    public string Bin { get; }
    public Dictionary<string, string> Env { get; }

    public DoctorFixture()
    {
        Root = Path.Join(_scratch.FullName, "checkout with spaces");
        Bin = Path.Join(Root, "controlled-bin");
        Directory.CreateDirectory(Bin);
        foreach (var relative in CheckoutFiles)
            AddCheckoutFile(relative);
        Directory.CreateDirectory(Path.Join(Root, "source"));
        Directory.CreateDirectory(Path.Join(Root, "Config"));
        File.WriteAllText(Path.Join(Root, "CobolToQuarkusMigration.csproj"), "<Project/>");

        Env = ScrubbedEnvironment();
        var posixRoot = Posix(Root);
        Env["PATH"] = Bin + Path.PathSeparator + (Environment.GetEnvironmentVariable("PATH") ?? "");
        Env["JCL_TEST_ROOT"] = Root;
        Env["CopilotSkipCliDownload"] = "true";
        Env["NEO4J_PASSWORD"] = "isolated-test-only";
        Env["TMPDIR"] = posixRoot;
        Env["TEMP"] = posixRoot;
        Env["TMP"] = posixRoot;

        File.Exists(CliAssembly).Should().BeTrue("the doctor tests forward parser checks to the built CLI at {0}", CliAssembly);
        Env["LMA_TEST_DLL"] = CliAssembly;
        Env["LMA_REAL_DOTNET"] = Posix(RealDotnet);
        WriteExecutable("docker", $"#!/bin/bash\nkind=docker\n{BoundaryScript}");
        WriteExecutable("dotnet", $"#!/bin/bash\nkind=dotnet\n{BoundaryScript}");
        // The concurrent-run guard would otherwise see CLI processes started by tests running in parallel.
        WriteExecutable("pgrep", "#!/bin/bash\nexit 1\n");
    }

    public void Dispose()
    {
        try { _scratch.Delete(recursive: true); }
        catch (IOException) { }
        catch (UnauthorizedAccessException) { }
    }

    // jcl-facts and jcl-jobs are then answered by the genuine CLI instead of a canned fact file.
    public void UseRealJclCli()
    {
        Env["JCL_REAL_DLL"] = CliAssembly;
        Env["JCL_REAL_DOTNET"] = Posix(RealDotnet);
    }

    public void AddCheckoutFile(string relative)
    {
        var target = Path.Join(Root, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(target)!);
        File.Copy(Path.Join(RepoRoot, relative), target);
    }

    // docker exec calls are handed to this script (arguments after "exec") instead of the canned answers.
    public void UseParserContainer(string script)
    {
        WriteExecutable("parser-container", script);
        Env["LMA_TEST_PARSER"] = Posix(Path.Join(Bin, "parser-container"));
    }

    public void WriteExecutable(string name, string script)
    {
        var path = Path.Join(Bin, name);
        File.WriteAllText(path, script.ReplaceLineEndings("\n"));
        if (!OperatingSystem.IsWindows())
            File.SetUnixFileMode(path, UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute
                | UnixFileMode.GroupRead | UnixFileMode.GroupExecute | UnixFileMode.OtherRead | UnixFileMode.OtherExecute);
    }

    public string Job(string relative = "source/nested jobs/SAMPLE.JCL", string text = "//SAMPLE JOB\n//S1 EXEC PGM=IEFBR14\n")
    {
        var path = Path.Join(Root, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, text);
        return path;
    }

    public DoctorRun Run(params string[] args) =>
        RunProcess(Bash, [Posix(Path.Join(Root, "doctor.sh")), .. args], Root, Env, RunTimeout);

    public IReadOnlyList<IReadOnlyList<string>> Calls(string kind)
    {
        var log = Path.Join(Root, "calls.log");
        if (!File.Exists(log)) return [];
        return File.ReadAllText(log)
            .Split('\x1e', StringSplitOptions.RemoveEmptyEntries)
            .Select(record => record.Split('\x1f'))
            .Where(fields => fields[0] == kind)
            // Every field, the last included, is \x1f-terminated, which leaves one empty trailing element.
            .Select(fields => (IReadOnlyList<string>)fields[1..^1])
            .ToList();
    }

    public IReadOnlyList<IReadOnlyList<string>> Ingests() =>
        Calls("dotnet").Where(args => args.Contains("graph-ingest")).ToList();

    public static string After(IReadOnlyList<string> args, string flag)
    {
        var index = args.ToList().IndexOf(flag);
        index.Should().BeGreaterThanOrEqualTo(0, "{0} should be among [{1}]", flag, string.Join(", ", args));
        return args[index + 1];
    }

    public static void ShouldBeSamePath(string actual, string expected) =>
        NormalizePath(actual).Should().Be(NormalizePath(expected), "{0} should name {1}", actual, expected);

    public static DoctorRun RunProcess(string fileName, IEnumerable<string> args, string workingDirectory,
        IReadOnlyDictionary<string, string> env, TimeSpan timeout, string? stdin = null)
    {
        var start = new ProcessStartInfo(fileName)
        {
            WorkingDirectory = workingDirectory,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            RedirectStandardInput = true,
            StandardOutputEncoding = Encoding.UTF8,
            StandardErrorEncoding = Encoding.UTF8,
            UseShellExecute = false,
        };
        foreach (var arg in args) start.ArgumentList.Add(arg);
        start.Environment.Clear();
        foreach (var (key, value) in env) start.Environment[key] = value;

        using var process = Process.Start(start)!;
        var stdout = process.StandardOutput.ReadToEndAsync();
        var stderr = process.StandardError.ReadToEndAsync();
        if (stdin is not null) process.StandardInput.Write(stdin);
        process.StandardInput.Close();
        if (!process.WaitForExit(timeout))
        {
            process.Kill(entireProcessTree: true);
            throw new TimeoutException($"{fileName} {string.Join(' ', start.ArgumentList)} did not finish within {timeout}");
        }
        process.WaitForExit();
        return new DoctorRun(process.ExitCode, stdout.Result, stderr.Result);
    }

    public static Dictionary<string, string> ScrubbedEnvironment()
    {
        var env = new Dictionary<string, string>(
            OperatingSystem.IsWindows() ? StringComparer.OrdinalIgnoreCase : StringComparer.Ordinal);
        foreach (DictionaryEntry entry in Environment.GetEnvironmentVariables())
        {
            var key = (string)entry.Key;
            if (ScrubbedPrefixes.Any(prefix => key.StartsWith(prefix, KeyComparison))
                || ScrubbedNames.Any(name => string.Equals(key, name, KeyComparison)))
                continue;
            env[key] = (string?)entry.Value ?? "";
        }
        return env;
    }

    public static Dictionary<string, string> EnvironmentWithout(params string[] names)
    {
        var env = new Dictionary<string, string>(
            OperatingSystem.IsWindows() ? StringComparer.OrdinalIgnoreCase : StringComparer.Ordinal);
        foreach (DictionaryEntry entry in Environment.GetEnvironmentVariables())
            if (!names.Contains((string)entry.Key, StringComparer.FromComparison(KeyComparison)))
                env[(string)entry.Key] = (string?)entry.Value ?? "";
        return env;
    }

    public static string Posix(string path) => path.Replace('\\', '/');

    // The CLI records resolved source paths; macOS temp folders sit behind the /var -> /private/var link.
    public static string RealPath(string path)
    {
        var full = Path.GetFullPath(path);
        var current = Path.GetPathRoot(full)!;
        foreach (var part in full[current.Length..].Split(Path.DirectorySeparatorChar, StringSplitOptions.RemoveEmptyEntries))
        {
            current = Path.Join(current, part);
            if (new FileInfo(current).LinkTarget is not null)
                current = new FileInfo(current).ResolveLinkTarget(returnFinalTarget: true)!.FullName;
        }
        return current;
    }

    private static string NormalizePath(string path)
    {
        if (OperatingSystem.IsWindows())
        {
            // Bash-to-bash calls keep Git Bash's /c/... spelling; only native programs receive C:/...
            var msys = Regex.Match(path, "^/([A-Za-z])(/|$)");
            if (msys.Success) path = $"{msys.Groups[1].Value}:/{path[msys.Length..]}";
        }
        var full = Path.GetFullPath(path).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
        return OperatingSystem.IsWindows() ? full.ToUpperInvariant() : full;
    }

    private static string FindRepoRoot()
    {
        for (var dir = new DirectoryInfo(AppContext.BaseDirectory); dir is not null; dir = dir.Parent)
            if (File.Exists(Path.Join(dir.FullName, "doctor.sh")))
                return dir.FullName;
        throw new InvalidOperationException("doctor.sh not found above " + AppContext.BaseDirectory);
    }

    private static string ResolveBash()
    {
        // On Windows a bare "bash" resolves to the System32 WSL stub before Git Bash.
        if (OperatingSystem.IsWindows() && FindOnPath("git") is { } git)
        {
            var gitDir = Path.GetDirectoryName(git)!;
            foreach (var candidate in new[] { Path.Join(gitDir, "..", "bin", "bash.exe"), Path.Join(gitDir, "bash.exe") })
                if (File.Exists(candidate)) return Path.GetFullPath(candidate);
        }
        return FindOnPath("bash") ?? "bash";
    }

    // Resolved before PATH is replaced so forwarding reaches the dotnet running the tests, not the fake.
    private static string ResolveDotnet() =>
        Environment.GetEnvironmentVariable("DOTNET_HOST_PATH") is { Length: > 0 } host && File.Exists(host)
            ? host
            : FindOnPath("dotnet") ?? throw new InvalidOperationException("dotnet not found on PATH");

    private static string? FindOnPath(string name)
    {
        var extensions = OperatingSystem.IsWindows()
            ? (Environment.GetEnvironmentVariable("PATHEXT") ?? ".EXE").Split(';').Prepend("").ToArray()
            : [""];
        foreach (var dir in (Environment.GetEnvironmentVariable("PATH") ?? "").Split(Path.PathSeparator))
        {
            if (dir.Length == 0) continue;
            foreach (var extension in extensions)
            {
                var candidate = Path.Join(dir, name + extension);
                if (File.Exists(candidate)) return candidate;
            }
        }
        return null;
    }

    // Shared body of the fake docker and dotnet. Each call is logged as \x1f-terminated fields in a
    // \x1e-terminated record, so arguments may hold spaces, quotes, backslashes and newlines.
    private const string BoundaryScript = """
        args=("$@")
        root="$JCL_TEST_ROOT"
        printf -v record '%s\037' "$kind" "$@"
        printf '%s\036' "$record" >> "$root/calls.log"

        has() { local a; for a in "${args[@]}"; do [[ "$a" == "$1" ]] && return 0; done; return 1; }
        first_of() { local name; for name in "$@"; do has "$name" && { printf '%s' "$name"; return 0; }; done; return 1; }
        index_of() { local i; for i in "${!args[@]}"; do [[ "${args[$i]}" == "$1" ]] && { printf '%s' "$i"; return 0; }; done; return 1; }
        after() { local i; i="$(index_of "$1")" || return 1; printf '%s' "${args[$((i + 1))]}"; }
        forward_from() {
            local i
            i="$(index_of "$1")"
            [[ -n "$2" && -n "$3" ]] || exit 1
            exec "$2" "$3" "${args[@]:$i}"
        }
        json_string() { local s="$1"; s="${s//\\/\\\\}"; s="${s//\"/\\\"}"; printf '"%s"' "$s"; }
        json_value() { if [[ "$1" == set ]]; then json_string "$2"; else printf null; fi; }

        if [[ "$kind" == docker ]]; then
            if [[ "${1:-}" == version ]]; then
                echo "1.45"
            elif [[ "${1:-}" == exec ]]; then
                [[ -n "${LMA_TEST_PARSER:-}" ]] && exec "$LMA_TEST_PARSER" "${args[@]:1}"
                if [[ -n "${JCL_TEST_AUTH_FAIL:-}" ]] && has "RETURN 1"; then exit 3; fi
                if has run && has java; then
                    report="$root/output/rekt/$(after run).report"
                    for folder in flow_ast cfg; do
                        mkdir -p "$report/$folder"
                        printf '{}' > "$report/$folder/$folder.json"
                    done
                else
                    echo "5"
                fi
            fi
        elif [[ "$kind" == dotnet ]]; then
            if has run && has --source; then
                printf '{"REKT_FACTS_FOLDER": %s, "CONVERSION_DEPENDENCY_MODE": %s, "TARGET_LANGUAGE": %s}' \
                    "$(json_value "${REKT_FACTS_FOLDER+set}" "${REKT_FACTS_FOLDER-}")" \
                    "$(json_value "${CONVERSION_DEPENDENCY_MODE+set}" "${CONVERSION_DEPENDENCY_MODE-}")" \
                    "$(json_value "${TARGET_LANGUAGE+set}" "${TARGET_LANGUAGE-}")" > "$root/conversion-env.json"
            fi
            if [[ $# -eq 1 && "$1" == --list-runtimes ]]; then
                echo "Microsoft.NETCore.App 10.0.0"
            elif has -getProperty:TargetPath; then
                printf '%s\n' "$LMA_TEST_DLL"
            elif offline="$(first_of rekt-diagnostics doctor-report rekt-evidence rekt-activity rekt-source-identity rekt-bms-map rekt-platform-contracts rekt-preprocess)"; then
                forward_from "$offline" "$LMA_REAL_DOTNET" "$LMA_TEST_DLL"
            elif has jcl-facts; then
                [[ -n "${JCL_TEST_FAIL:-}" ]] && exit 7
                [[ -n "${JCL_REAL_DLL:-}" ]] && forward_from jcl-facts "${JCL_REAL_DOTNET:-}" "$JCL_REAL_DLL"
                output="$(after --output-dir)"
                mkdir -p "$output"
                printf '%s' '{"job":{"name":"SAMPLE"}}' > "$output/SAMPLE.JCL.job.json"
                echo "jcl-facts: wrote 1 job fact file(s)"
            elif has jcl-jobs; then
                forward_from jcl-jobs "${JCL_REAL_DOTNET:-}" "${JCL_REAL_DLL:-}"
            elif has graph-ingest; then
                output="$(after --rekt-output)"
                [[ -e "$output/.jcl-scan-failed" ]] && exit 92
                listing="["
                separator=""
                while IFS= read -r file; do
                    listing+="$separator$(json_string "${file#./}")"
                    separator=", "
                done < <(cd "$output" 2>/dev/null && find . -name '*.job.json' | LC_ALL=C sort)
                printf '%s]' "$listing" > "$root/ingested.json"
            fi
        else
            exit 93
        fi
        exit 0
        """;
}
