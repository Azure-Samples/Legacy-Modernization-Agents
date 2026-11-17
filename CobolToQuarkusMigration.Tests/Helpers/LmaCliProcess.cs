using System.Diagnostics;
using FluentAssertions;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Runs the built CLI the way doctor.sh and preprocess-for-rekt.sh do, so exit codes and stdout are pinned end to end.
internal static class LmaCliProcess
{
    public static (int ExitCode, string StdOut, string StdErr) Run(
        string workingDirectory, IReadOnlyDictionary<string, string?>? environment, params string[] args)
    {
        var psi = new ProcessStartInfo("dotnet")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            // An empty working directory keeps a developer's Config/ and Logs/ out of the result.
            WorkingDirectory = Directory.CreateDirectory(workingDirectory).FullName,
        };
        psi.ArgumentList.Add(Path.Join(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll"));
        foreach (var arg in args) psi.ArgumentList.Add(arg);
        foreach (var (key, value) in environment ?? new Dictionary<string, string?>())
            if (value is null) psi.Environment.Remove(key); else psi.Environment[key] = value;

        using var process = Process.Start(psi)!;
        var stdErr = process.StandardError.ReadToEndAsync();
        var stdOut = process.StandardOutput.ReadToEnd();
        process.WaitForExit(120_000).Should().BeTrue("the CLI should not hang");
        return (process.ExitCode, stdOut, stdErr.Result);
    }
}
