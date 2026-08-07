namespace CobolToQuarkusMigration.Agents.Infrastructure;

public static class LlmCorrelationContext
{
    private static readonly AsyncLocal<string?> _runId = new();
    private static readonly AsyncLocal<string?> _correlationId = new();

    public static string RunId => _runId.Value ?? "-";

    public static string CorrelationId => _correlationId.Value ?? "-";

    // Nested scopes restore the previous values when disposed.
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
