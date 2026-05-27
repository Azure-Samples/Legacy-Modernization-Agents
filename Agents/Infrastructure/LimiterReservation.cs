namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Concrete reservation handle backed by <see cref="RateLimitTracker"/>. Exposed
/// type so call sites can keep a typed reference for tests / diagnostics.
/// </summary>
public sealed class LimiterReservation : IRateLimitReservation
{
    private readonly RateLimitTracker _tracker;
    private readonly long _id;
    private bool _settled;

    internal LimiterReservation(RateLimitTracker tracker, long id)
    {
        _tracker = tracker;
        _id = id;
    }

    /// <inheritdoc />
    public void Commit(int actualTokens)
    {
        if (_settled) return;
        _settled = true;
        _tracker.RecordUsage(_id, actualTokens);
    }

    /// <inheritdoc />
    public void Cancel()
    {
        if (_settled) return;
        _settled = true;
        _tracker.ReleaseReservation(_id);
    }

    public void Dispose()
    {
        if (!_settled) Cancel();
    }
}
