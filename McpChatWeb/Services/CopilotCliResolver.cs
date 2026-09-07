using GitHub.Copilot;
using CobolToQuarkusMigration.Helpers;

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
    /// <summary>Build a CopilotClientOptions with the connection pointed at the first usable CLI binary.</summary>
    public static CopilotClientOptions BuildOptions(
        bool useStdio = true,
        string? githubToken = null,
        string? githubHost = null)
    {
        // SDK 1.0: Mode defaults to CopilotCli and a null Connection means
        // "ForStdio() with the bundled runtime". We only override Connection
        // when we resolve a CLI binary at a non-default location, so the SDK's
        // own bundled-runtime auto-discovery still works out of the box.
        var opts = new CopilotClientOptions { Mode = CopilotClientMode.CopilotCli };
        githubToken ??= CopilotRouting.ResolveToken();
        if (!string.IsNullOrWhiteSpace(githubToken)) opts.GitHubToken = githubToken;
        var cli = ResolveCliPath();
        if (!string.IsNullOrWhiteSpace(cli)) opts.Connection = RuntimeConnection.ForStdio(cli, null);
        CopilotRouting.ApplyTo(opts, githubHost);
        return opts;
    }

    /// <summary>Return the path to a usable Copilot CLI binary, or null if none found.</summary>
    public static string? ResolveCliPath()
        => CopilotRouting.ResolveCliPath(AppContext.BaseDirectory);
}
