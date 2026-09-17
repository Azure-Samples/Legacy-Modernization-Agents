// The service contracts this portal exposes.
//
// Each one corresponds to a function module in the COBOL estate under source/bd. The shapes are
// the functional contracts — a banking date, a diagnosed database error, a throughput reading —
// not the COBOL record layouts, which stay with the customer's source and its generated output.

namespace Modernized.Banking.Portal.Domain;

/// <summary>Where a capability came from, so a reviewer can trace the portal back to the estate.</summary>
public sealed record CapabilityOrigin(
    string Capability,
    string Program,
    string SourcePath,
    string Summary,
    string Endpoint);

// ── Banking calendar ────────────────────────────────────────────────────────────────────────

public enum DayClassification
{
    BankingDay,
    Weekend,
    Holiday,
}

public sealed record BankingDateResult(
    DateOnly Requested,
    DateOnly BankingDate,
    DayClassification Classification,
    int DaysShifted,
    string Explanation);

public sealed record BankingDateRequest(DateOnly? AsOf, int OffsetDays = 0);

public sealed record DateValidationRequest(string? Value);

public sealed record DateValidationResult(
    bool IsValid,
    DateOnly? Parsed,
    string Code,
    string Message);

public interface IBankingCalendar
{
    BankingDateResult Resolve(DateOnly asOf, int offsetDays);
    DateValidationResult Validate(string? value);
    IReadOnlyList<DateOnly> HolidaysIn(int year);
}

// ── Batch throughput ────────────────────────────────────────────────────────────────────────

public sealed record ThroughputRequest(
    string? Label,
    long UnitsProcessed,
    double ElapsedSeconds);

public sealed record ThroughputResult(
    string Label,
    long UnitsProcessed,
    double ElapsedSeconds,
    double UnitsPerMinute,
    string DisplayLine);

public interface IThroughputReporter
{
    ThroughputResult Measure(ThroughputRequest request);
}

// ── Database diagnostics ────────────────────────────────────────────────────────────────────

public sealed record SqlDiagnosticRequest(
    int SqlCode,
    string? SqlState,
    string? Statement,
    int RowsAffected = 0);

public sealed record SqlDiagnosticResult(
    int SqlCode,
    string SqlState,
    string Severity,
    string Condition,
    string Explanation,
    string RecommendedAction,
    bool IsRetryable);

public interface ISqlDiagnostics
{
    SqlDiagnosticResult Diagnose(SqlDiagnosticRequest request);
}

// ── Interest rate reconciliation ────────────────────────────────────────────────────────────

public sealed record RateEntry(
    string Key,
    decimal Rate,
    string Category,
    bool Fictitious = false);

public sealed record ReconcileRequest(
    IReadOnlyList<RateEntry> Primary,
    IReadOnlyList<RateEntry> Secondary,
    string RequiredCategory = "KRD");

public sealed record RateDifference(
    string Key,
    decimal PrimaryRate,
    decimal SecondaryRate,
    decimal Delta);

public sealed record ReconcileResult(
    IReadOnlyList<RateEntry> Matched,
    IReadOnlyList<RateEntry> MissingCounterpart,
    IReadOnlyList<RateDifference> Differing,
    IReadOnlyList<string> Rejected,
    int PrimaryCount,
    int SecondaryCount);

public interface IRateReconciler
{
    ReconcileResult Reconcile(ReconcileRequest request);
}
