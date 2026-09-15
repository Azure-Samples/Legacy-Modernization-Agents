using System.Diagnostics;
using System.Reflection;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Resolves which Copilot CLI binary the SDK will actually launch, and its
/// version.
///
/// The SDK deserializes the CLI's JSON-RPC responses into fixed DTOs, so a CLI
/// whose payload shape has drifted from the SDK's expectations fails during the
/// startup handshake with an opaque error. Reporting the resolved path and
/// version alongside every diagnostic turns that class of failure into
/// something a user can act on immediately instead of guessing.
///
/// Resolution mirrors the SDK: a binary bundled next to the application wins,
/// otherwise the first match on PATH. This is done in-process rather than in
/// the shell so bash, Git Bash and PowerShell all report the same result.
/// </summary>
public static class CopilotCliInfo
{
    private static readonly string[] ExecutableNames =
        OperatingSystem.IsWindows()
            ? new[] { "copilot.exe", "copilot.cmd", "copilot.bat", "copilot" }
            : new[] { "copilot" };

    /// <summary>
    /// The CLI version the referenced GitHub.Copilot.SDK package was built
    /// against, captured from the SDK's own MSBuild property at compile time.
    /// Empty when the property was unavailable.
    /// </summary>
    public static string ExpectedVersion { get; } =
        Assembly.GetExecutingAssembly()
            .GetCustomAttributes<AssemblyMetadataAttribute>()
            .FirstOrDefault(a => a.Key == "CopilotCliVersion")?.Value ?? "";

    public static string? ResolvePath()
    {
        foreach (var name in ExecutableNames)
        {
            var bundled = Path.Combine(AppContext.BaseDirectory, name);
            if (File.Exists(bundled))
                return bundled;
        }

        var pathVariable = Environment.GetEnvironmentVariable("PATH");
        if (string.IsNullOrEmpty(pathVariable))
            return null;

        foreach (var directory in pathVariable.Split(Path.PathSeparator))
        {
            if (string.IsNullOrWhiteSpace(directory))
                continue;

            foreach (var name in ExecutableNames)
            {
                string candidate;
                try { candidate = Path.Combine(directory.Trim(), name); }
                catch (ArgumentException) { continue; }

                if (File.Exists(candidate))
                    return candidate;
            }
        }

        return null;
    }

    /// <summary>
    /// Runs `copilot --version` and extracts the semantic version. Returns null
    /// when the CLI is missing or does not answer within <paramref name="timeout"/>.
    /// </summary>
    public static string? TryGetVersion(string? cliPath, TimeSpan timeout)
    {
        if (string.IsNullOrWhiteSpace(cliPath))
            return null;

        try
        {
            using var process = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = cliPath,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    CreateNoWindow = true
                }
            };
            process.StartInfo.ArgumentList.Add("--version");

            if (!process.Start())
                return null;

            var stdout = process.StandardOutput.ReadToEnd();
            var stderr = process.StandardError.ReadToEnd();

            if (!process.WaitForExit((int)timeout.TotalMilliseconds))
            {
                try { process.Kill(entireProcessTree: true); } catch { }
                return null;
            }

            // Output is free-form (e.g. "GitHub Copilot CLI 1.0.84-1."), so take
            // the first version-looking token rather than the whole line, and
            // do not let a sentence-ending period become part of the version.
            var match = Regex.Match($"{stdout} {stderr}", @"\d+\.\d+\.\d+(?:-[0-9A-Za-z.]+)?");
            return match.Success ? match.Value.TrimEnd('.') : null;
        }
        catch
        {
            return null;
        }
    }
}
