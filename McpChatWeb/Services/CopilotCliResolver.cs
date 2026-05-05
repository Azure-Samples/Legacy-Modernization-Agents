using GitHub.Copilot.SDK;

namespace McpChatWeb.Services;

/// <summary>
/// Resolves a usable Copilot CLI binary path. The SDK's NuGet build target
/// downloads the CLI into <c>bin/.../runtimes/&lt;rid&gt;/native/copilot</c>;
/// when that download fails (offline build, restricted npm, or the user
/// already has the CLI installed), we fall back to the system PATH and to
/// well-known install locations so the AI provider setup keeps working.
/// </summary>
public static class CopilotCliResolver
{
    private static string? _cachedPath;
    private static bool _cacheChecked;

    /// <summary>Build a CopilotClientOptions with CliPath set to the first usable binary.</summary>
    public static CopilotClientOptions BuildOptions(bool useStdio = true, string? githubToken = null)
    {
        var opts = new CopilotClientOptions { UseStdio = useStdio };
        if (!string.IsNullOrWhiteSpace(githubToken)) opts.GitHubToken = githubToken;
        var cli = ResolveCliPath();
        if (!string.IsNullOrWhiteSpace(cli)) opts.CliPath = cli;
        return opts;
    }

    /// <summary>Return the path to a usable Copilot CLI binary, or null if none found.</summary>
    public static string? ResolveCliPath()
    {
        if (_cacheChecked) return _cachedPath;
        _cacheChecked = true;

        // Allow explicit override via env var.
        var envOverride = Environment.GetEnvironmentVariable("COPILOT_CLI_PATH");
        if (!string.IsNullOrWhiteSpace(envOverride) && File.Exists(envOverride))
        {
            _cachedPath = envOverride; return _cachedPath;
        }

        var binName = OperatingSystem.IsWindows() ? "copilot.exe" : "copilot";

        // 1) The SDK-managed location alongside the .NET binaries.
        var baseDir = AppContext.BaseDirectory;
        var rid = OperatingSystem.IsWindows()
            ? (System.Runtime.InteropServices.RuntimeInformation.OSArchitecture == System.Runtime.InteropServices.Architecture.Arm64 ? "win-arm64" : "win-x64")
            : OperatingSystem.IsMacOS()
                ? (System.Runtime.InteropServices.RuntimeInformation.OSArchitecture == System.Runtime.InteropServices.Architecture.Arm64 ? "osx-arm64" : "osx-x64")
                : (System.Runtime.InteropServices.RuntimeInformation.OSArchitecture == System.Runtime.InteropServices.Architecture.Arm64 ? "linux-arm64" : "linux-x64");
        var sdkPath = Path.Combine(baseDir, "runtimes", rid, "native", binName);
        if (File.Exists(sdkPath)) { _cachedPath = sdkPath; return _cachedPath; }

        // 2) Walk PATH looking for a `copilot` binary.
        var pathEnv = Environment.GetEnvironmentVariable("PATH") ?? string.Empty;
        var separator = OperatingSystem.IsWindows() ? ';' : ':';
        foreach (var dir in pathEnv.Split(separator, StringSplitOptions.RemoveEmptyEntries))
        {
            try
            {
                var candidate = Path.Combine(dir.Trim(), binName);
                if (File.Exists(candidate)) { _cachedPath = candidate; return _cachedPath; }
            }
            catch { /* skip malformed PATH entries */ }
        }

        // 3) Well-known macOS / Linux install locations (homebrew, /usr/local, ~/.local).
        var home = Environment.GetEnvironmentVariable("HOME") ?? string.Empty;
        var fallbacks = new[]
        {
            "/opt/homebrew/bin/copilot",
            "/usr/local/bin/copilot",
            "/usr/bin/copilot",
            Path.Combine(home, ".local/bin/copilot"),
            Path.Combine(home, ".npm-global/bin/copilot"),
        };
        foreach (var candidate in fallbacks)
        {
            if (File.Exists(candidate)) { _cachedPath = candidate; return _cachedPath; }
        }

        _cachedPath = null;
        return null;
    }
}
