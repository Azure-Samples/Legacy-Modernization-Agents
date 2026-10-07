namespace McpChatWeb.Services;

/// <summary>
/// The MCP server process exited before answering. Carries what it printed to stderr so the
/// portal can show the reason (for example "no migration runs") instead of a stack trace.
/// </summary>
public sealed class McpServerUnavailableException : InvalidOperationException
{
    private const string NoRunsMarker = "No migration runs available";

    public McpServerUnavailableException(int? exitCode, IReadOnlyList<string> stderrLines)
        : base(BuildMessage(exitCode, stderrLines))
    {
        ExitCode = exitCode;
        StderrLines = stderrLines;
        Reason = stderrLines.Count > 0 ? stderrLines[^1] : null;
        NoMigrationRuns = stderrLines.Any(l => l.Contains(NoRunsMarker, StringComparison.OrdinalIgnoreCase));
    }

    public int? ExitCode { get; }

    public IReadOnlyList<string> StderrLines { get; }

    /// <summary>Last line the server wrote to stderr, which is normally why it stopped.</summary>
    public string? Reason { get; }

    public bool NoMigrationRuns { get; }

    private static string BuildMessage(int? exitCode, IReadOnlyList<string> stderrLines)
    {
        var message = "MCP process exited while waiting for a response";
        if (exitCode.HasValue)
        {
            message += $" (exit code {exitCode.Value})";
        }

        return stderrLines.Count > 0 ? $"{message}: {stderrLines[^1]}" : message + ".";
    }
}
