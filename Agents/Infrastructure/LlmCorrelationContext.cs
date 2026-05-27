namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// AsyncLocal-scoped correlation context for LLM calls.
/// Set once at the top of a logical unit of work (a conversion run, a chat
/// turn, a chunk attempt) so every structured log line emitted by the limiter,
/// retry helper, and provider clients carries the same <see cref="RunId"/>
/// and (optionally) <see cref="CorrelationId"/>.
/// </summary>
/// <remarks>
/// <para>
/// Use a <c>using</c> block:
/// </para>
/// <code>
/// using var _ = LlmCorrelationContext.Begin(runId: "rekt-202605271200", correlationId: "convert:BDSDA2F");
/// await client.GetResponseAsync(...);
/// </code>
/// <para>
/// Scope nests: inner <see cref="Begin"/> calls override the outer values for
/// the duration of the inner scope, and dispose restores the previous values.
/// </para>
/// </remarks>
public static class LlmCorrelationContext
{
    private static readonly AsyncLocal<string?> _runId = new();
    private static readonly AsyncLocal<string?> _correlationId = new();

    /// <summary>Current run id (workflow / pipeline run), or <c>"-"</c> if unset.</summary>
    public static string RunId => _runId.Value ?? "-";

    /// <summary>Current per-call correlation id, or <c>"-"</c> if unset.</summary>
    public static string CorrelationId => _correlationId.Value ?? "-";

    /// <summary>
    /// Begin a new scope. The previous values are restored when the returned
    /// scope is disposed. Null values inherit the outer scope.
    /// </summary>
    public static Scope Begin(string? runId = null, string? correlationId = null)
    {
        var scope = new Scope(_runId.Value, _correlationId.Value);
        if (runId is not null) _runId.Value = runId;
        if (correlationId is not null) _correlationId.Value = correlationId;
        return scope;
    }

    public readonly struct Scope : IDisposable
    {
        private readonly string? _prevRun;
        private readonly string? _prevCorr;
        internal Scope(string? prevRun, string? prevCorr)
        {
            _prevRun = prevRun;
            _prevCorr = prevCorr;
        }
        public void Dispose()
        {
            _runId.Value = _prevRun;
            _correlationId.Value = _prevCorr;
        }
    }
}
