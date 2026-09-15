using System.ComponentModel;
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
        // Path.Join rather than Path.Combine: the executable names are never
        // rooted, and Join never discards the directory it is given.
        var bundled = ExecutableNames
            .Select(name => Path.Join(AppContext.BaseDirectory, name))
            .FirstOrDefault(File.Exists);
        if (bundled is not null)
            return bundled;

        var pathVariable = Environment.GetEnvironmentVariable("PATH");
        if (string.IsNullOrEmpty(pathVariable))
            return null;

        return pathVariable
            .Split(Path.PathSeparator)
            .Where(directory => !string.IsNullOrWhiteSpace(directory))
            .SelectMany(directory => ExecutableNames
                .Select(name => Path.Join(directory.Trim(), name)))
            .FirstOrDefault(File.Exists);
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
                // The CLI hung. Killing is best-effort: it may have exited on
                // its own between the timeout and the kill, and we return null
                // either way.
                try
                {
                    process.Kill(entireProcessTree: true);
                }
                catch (InvalidOperationException) { /* already exited */ }
                catch (NotSupportedException) { /* not a local process */ }
                catch (AggregateException) { /* one or more children survived */ }
                catch (Win32Exception) { /* OS refused the kill */ }

                return null;
            }

            // Output is free-form (e.g. "GitHub Copilot CLI 1.0.84-1."), so take
            // the first version-looking token rather than the whole line, and
            // do not let a sentence-ending period become part of the version.
            var match = Regex.Match($"{stdout} {stderr}", @"\d+\.\d+\.\d+(?:-[0-9A-Za-z.]+)?");
            return match.Success ? match.Value.TrimEnd('.') : null;
        }
        catch (Win32Exception)
        {
            // CLI is not executable on this platform, or launching it failed.
            return null;
        }
        catch (InvalidOperationException)
        {
            return null;
        }
        catch (IOException)
        {
            // Pipe broke while reading the CLI's output.
            return null;
        }
        catch (UnauthorizedAccessException)
        {
            return null;
        }
        catch (PlatformNotSupportedException)
        {
            return null;
        }
    }
}
