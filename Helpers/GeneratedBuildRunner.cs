// Builds a generated C# run folder with the real compiler and reads back what it reported. The
// compiler is the only authority on whether generated code compiles; everything upstream of it is
// an estimate.

using System.Diagnostics;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Helpers;

public sealed record CompilerDiagnostic(string File, int Line, int Column, string Code, string Message)
{
    public override string ToString() => $"{File}({Line},{Column}): error {Code}: {Message}";
}

public sealed record BuildOutcome(
    bool Ran,
    IReadOnlyList<CompilerDiagnostic> Errors,
    IReadOnlyList<string> NonCompilerErrors,
    string? FailureReason)
{
    /// <summary>The build ran, the compiler reported no errors and nothing else failed.</summary>
    public bool Succeeded => Ran && Errors.Count == 0 && NonCompilerErrors.Count == 0;
}

public static class GeneratedBuildRunner
{
    // `path(line,col): error CS0246: message [project.csproj]`
    private static readonly Regex CompilerError = new(
        @"^(?<file>[^\r\n(]+?)\((?<line>\d+),(?<col>\d+)\):\s+error\s+(?<code>CS\d{4}):\s+(?<msg>.*?)(?:\s+\[[^\]\r\n]+\])?\s*$",
        RegexOptions.Multiline | RegexOptions.Compiled);

    // Restore and SDK failures (NU1101, MSB4019, ...) mean the compiler never ran on the code.
    private static readonly Regex OtherError = new(
        @"^.*?\berror\s+(?<code>(?:NU|MSB|NETSDK)\d+):\s+(?<msg>.*?)(?:\s+\[[^\]\r\n]+\])?\s*$",
        RegexOptions.Multiline | RegexOptions.Compiled);

    public static async Task<BuildOutcome> BuildAsync(
        string runFolder, TimeSpan timeout, CancellationToken cancellationToken = default)
    {
        var project = Directory.EnumerateFiles(runFolder, "*.csproj", SearchOption.TopDirectoryOnly)
            .OrderBy(p => p, StringComparer.Ordinal)
            .FirstOrDefault();
        if (project is null)
            return new BuildOutcome(false, [], [], "no project file in the run folder");

        var start = new ProcessStartInfo("dotnet")
        {
            WorkingDirectory = runFolder,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
        };
        foreach (var arg in new[] { "build", project, "-nologo", "-v", "q", "-clp:NoSummary" })
            start.ArgumentList.Add(arg);
        // Keep the toolchain's own MSBuild state out of the generated project's build.
        start.Environment.Remove("MSBuildExtensionsPath");
        start.Environment.Remove("MSBuildSDKsPath");
        start.Environment["DOTNET_CLI_TELEMETRY_OPTOUT"] = "1";

        Process process;
        try
        {
            process = Process.Start(start) ?? throw new InvalidOperationException("dotnet did not start");
        }
        catch (Exception ex) when (ex is System.ComponentModel.Win32Exception or InvalidOperationException)
        {
            return new BuildOutcome(false, [], [], $"could not start dotnet: {ex.Message}");
        }

        using (process)
        {
            var stdout = process.StandardOutput.ReadToEndAsync(cancellationToken);
            var stderr = process.StandardError.ReadToEndAsync(cancellationToken);

            using var timer = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
            timer.CancelAfter(timeout);
            try
            {
                await process.WaitForExitAsync(timer.Token);
            }
            catch (OperationCanceledException)
            {
                try { process.Kill(entireProcessTree: true); } catch (InvalidOperationException) { }
                return new BuildOutcome(false, [], [], $"build did not finish within {timeout.TotalSeconds:0}s");
            }

            var output = await stdout + Environment.NewLine + await stderr;
            var (errors, other) = Parse(output, runFolder);
            if (process.ExitCode != 0 && errors.Count == 0 && other.Count == 0)
                other = [$"build exited with {process.ExitCode} without a recognisable error"];

            return new BuildOutcome(true, errors, other, null);
        }
    }

    /// <summary>
    /// Unique compiler errors, with paths relative to <paramref name="runFolder"/>, and any restore
    /// or SDK errors that stopped the compiler from running.
    /// </summary>
    public static (IReadOnlyList<CompilerDiagnostic> Errors, IReadOnlyList<string> Other) Parse(
        string buildOutput, string runFolder)
    {
        var known = Directory.Exists(runFolder)
            ? Directory.EnumerateFiles(runFolder, "*.cs", SearchOption.AllDirectories)
                .Select(p => Path.GetRelativePath(runFolder, p).Replace('\\', '/'))
                .OrderByDescending(p => p.Length)
                .ToList()
            : [];

        var errors = CompilerError.Matches(buildOutput)
            .Select(m => new CompilerDiagnostic(
                Relative(m.Groups["file"].Value.Trim(), known),
                int.Parse(m.Groups["line"].Value),
                int.Parse(m.Groups["col"].Value),
                m.Groups["code"].Value,
                m.Groups["msg"].Value.Trim()))
            .Distinct()
            .OrderBy(d => d.File, StringComparer.Ordinal)
            .ThenBy(d => d.Line)
            .ThenBy(d => d.Column)
            .ToList();

        var other = OtherError.Matches(buildOutput)
            .Select(m => $"{m.Groups["code"].Value}: {m.Groups["msg"].Value.Trim()}")
            .Distinct()
            .ToList();

        return (errors, other);
    }

    // The compiler reports paths as it resolved them (on macOS /tmp becomes /private/tmp), so they
    // are matched to the run's own files by suffix rather than by prefix.
    private static string Relative(string path, IReadOnlyList<string> known)
    {
        var normalized = path.Replace('\\', '/');
        return known.FirstOrDefault(k => normalized == k || normalized.EndsWith("/" + k, StringComparison.Ordinal))
               ?? normalized;
    }
}
