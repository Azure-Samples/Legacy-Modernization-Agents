using FluentAssertions;
using Modernized.Banking.Portal.Domain;
using Modernized.Banking.Portal.Services;
using Xunit;

namespace Modernized.Banking.Portal.Tests;

// The behaviour these cover is the reason the COBOL existed: which day work is booked to, whether
// a failure should be retried, and whether two rate files agree. Getting any of them subtly wrong
// is the failure mode a modernization is judged on.
public class DomainBehaviourTests
{
    private readonly BankingCalendar _calendar = new();
    private readonly ThroughputReporter _throughput = new();
    private readonly SqlDiagnostics _diagnostics = new();
    private readonly RateReconciler _reconciler = new();

    // ── Banking calendar ────────────────────────────────────────────────

    [Fact]
    public void AWeekdayThatIsNotAHolidayIsTheBankingDayItself()
    {
        // 2026-09-16 is a Wednesday.
        var result = _calendar.Resolve(new DateOnly(2026, 9, 16), 0);

        result.Classification.Should().Be(DayClassification.BankingDay);
        result.BankingDate.Should().Be(new DateOnly(2026, 9, 16));
        result.DaysShifted.Should().Be(0);
    }

    [Fact]
    public void ASaturdayRollsForwardToMonday()
    {
        // 2026-09-19 is a Saturday.
        var result = _calendar.Resolve(new DateOnly(2026, 9, 19), 0);

        result.Classification.Should().Be(DayClassification.Weekend);
        result.BankingDate.Should().Be(new DateOnly(2026, 9, 21));
        result.DaysShifted.Should().Be(2);
    }

    [Fact]
    public void ChristmasDayIsNotABankingDay()
    {
        var result = _calendar.Resolve(new DateOnly(2026, 12, 25), 0);

        result.Classification.Should().Be(DayClassification.Holiday);
        result.BankingDate.Should().BeAfter(new DateOnly(2026, 12, 25));
    }

    // A run of holidays and a weekend back to back is exactly where an off-by-one hides.
    [Fact]
    public void AHolidayRunFollowedByAWeekendStillLandsOnABankingDay()
    {
        var result = _calendar.Resolve(new DateOnly(2026, 12, 24), 0);

        result.BankingDate.DayOfWeek.Should().NotBe(DayOfWeek.Saturday);
        result.BankingDate.DayOfWeek.Should().NotBe(DayOfWeek.Sunday);
        _calendar.HolidaysIn(2026).Should().NotContain(result.BankingDate);
    }

    [Fact]
    public void TheOffsetIsAppliedBeforeTheDayIsClassified()
    {
        // Wednesday + 3 days = Saturday, which must then roll forward.
        var result = _calendar.Resolve(new DateOnly(2026, 9, 16), 3);

        result.Requested.Should().Be(new DateOnly(2026, 9, 19));
        result.BankingDate.Should().Be(new DateOnly(2026, 9, 21));
    }

    [Fact]
    public void EasterMovesWithTheYear()
    {
        // Good Friday 2026 is 3 April; in 2027 it is 26 March.
        _calendar.HolidaysIn(2026).Should().Contain(new DateOnly(2026, 4, 3));
        _calendar.HolidaysIn(2027).Should().Contain(new DateOnly(2027, 3, 26));
    }

    [Theory]
    [InlineData("2026-09-16")]
    [InlineData("20260916")]
    [InlineData("16-09-2026")]
    [InlineData("16.09.2026")]
    public void TheDateFormatsUsedInTheEstateAreAllAccepted(string value)
    {
        var result = _calendar.Validate(value);

        result.IsValid.Should().BeTrue();
        result.Parsed.Should().Be(new DateOnly(2026, 9, 16));
    }

    [Theory]
    [InlineData(null, "DATE_MISSING")]
    [InlineData("", "DATE_MISSING")]
    [InlineData("not-a-date", "DATE_FORMAT")]
    [InlineData("18000101", "DATE_RANGE_LOW")]
    public void AnUnusableDateIsRejectedWithACodeRatherThanAnException(string? value, string code)
    {
        var result = _calendar.Validate(value);

        result.IsValid.Should().BeFalse();
        result.Code.Should().Be(code);
        result.Message.Should().NotBeNullOrWhiteSpace();
    }

    // ── Throughput ──────────────────────────────────────────────────────

    [Fact]
    public void ThroughputIsUnitsPerMinuteNotPerSecond()
    {
        var result = _throughput.Measure(new ThroughputRequest("POSTING", 6000, 60));

        result.UnitsPerMinute.Should().Be(6000);
    }

    // Dividing by a zero-length run would give infinity, which then renders as "∞ units/min".
    [Fact]
    public void ARunShorterThanTheClockDoesNotDivideByZero()
    {
        var result = _throughput.Measure(new ThroughputRequest("INSTANT", 500, 0));

        result.UnitsPerMinute.Should().Be(0);
        double.IsFinite(result.UnitsPerMinute).Should().BeTrue();
    }

    [Fact]
    public void AnUnlabelledRunStillProducesAUsableLogLine()
    {
        var result = _throughput.Measure(new ThroughputRequest(null, 10, 5));

        result.Label.Should().Be("BATCH");
        result.DisplayLine.Should().Contain("BATCH").And.Contain("units/min");
    }

    // ── SQL diagnostics ─────────────────────────────────────────────────

    [Fact]
    public void NotFoundIsAnExpectedConditionRatherThanAnError()
    {
        var result = _diagnostics.Diagnose(new SqlDiagnosticRequest(100, null, null));

        result.Severity.Should().Be("Info");
        result.IsRetryable.Should().BeFalse();
    }

    [Theory]
    [InlineData(-911)]
    [InlineData(-913)]
    [InlineData(-904)]
    public void ContentionAndUnavailableResourcesAreTheOnlyRetryableConditions(int sqlCode)
    {
        _diagnostics.Diagnose(new SqlDiagnosticRequest(sqlCode, null, null))
            .IsRetryable.Should().BeTrue();
    }

    [Theory]
    [InlineData(-803)]
    [InlineData(-407)]
    [InlineData(-502)]
    public void AProgrammingFaultIsNeverPresentedAsRetryable(int sqlCode)
    {
        var result = _diagnostics.Diagnose(new SqlDiagnosticRequest(sqlCode, null, null));

        result.IsRetryable.Should().BeFalse();
        result.Severity.Should().Be("Error");
    }

    [Fact]
    public void AnUnclassifiedCodeIsReportedHonestlyRatherThanGuessed()
    {
        var result = _diagnostics.Diagnose(new SqlDiagnosticRequest(-9999, null, null));

        result.Condition.Should().Contain("Unhandled");
        result.Explanation.Should().Contain("-9999");
    }

    [Fact]
    public void ASuppliedSqlStateIsPreferredOverTheDerivedOne()
    {
        var result = _diagnostics.Diagnose(new SqlDiagnosticRequest(-911, "40502", null));

        result.SqlState.Should().Be("40502");
    }

    [Fact]
    public void SuccessWithNoRowsIsDistinguishedFromSuccessWithRows()
    {
        _diagnostics.Diagnose(new SqlDiagnosticRequest(0, null, null, 0))
            .Condition.Should().Be("Success, no rows");
        _diagnostics.Diagnose(new SqlDiagnosticRequest(0, null, null, 4))
            .Condition.Should().Be("Success");
    }

    // ── Rate reconciliation ─────────────────────────────────────────────

    private static ReconcileRequest Sets(RateEntry[] primary, RateEntry[] secondary) =>
        new(primary, secondary);

    [Fact]
    public void EntriesPresentOnBothSidesWithTheSameRateMatch()
    {
        var result = _reconciler.Reconcile(Sets(
            [new("K1", 2.25m, "KRD")],
            [new("K1", 2.25m, "KRD")]));

        result.Matched.Should().ContainSingle().Which.Key.Should().Be("K1");
        result.Differing.Should().BeEmpty();
        result.MissingCounterpart.Should().BeEmpty();
    }

    [Fact]
    public void ADifferentRateIsReportedWithItsDeltaRatherThanAsAMatch()
    {
        var result = _reconciler.Reconcile(Sets(
            [new("K1", 3.10m, "KRD")],
            [new("K1", 3.15m, "KRD")]));

        result.Matched.Should().BeEmpty();
        var diff = result.Differing.Should().ContainSingle().Subject;
        diff.Delta.Should().Be(0.05m);
    }

    [Fact]
    public void AnEntryWithNoCounterpartIsReportedSeparately()
    {
        var result = _reconciler.Reconcile(Sets(
            [new("K1", 1m, "KRD"), new("K2", 2m, "KRD")],
            [new("K1", 1m, "KRD")]));

        result.MissingCounterpart.Should().ContainSingle().Which.Key.Should().Be("K2");
    }

    [Fact]
    public void FictitiousRatesAreExcludedAndTheReasonIsGiven()
    {
        var result = _reconciler.Reconcile(Sets(
            [new("K1", 9.99m, "KRD", Fictitious: true)],
            [new("K1", 9.99m, "KRD")]));

        result.Matched.Should().BeEmpty();
        result.Rejected.Should().ContainSingle().Which.Should().Contain("fictitious");
    }

    [Fact]
    public void RatesOutsideTheRequiredCategoryAreExcluded()
    {
        var result = _reconciler.Reconcile(new ReconcileRequest(
            [new("K1", 2.5m, "DEP")], [new("K1", 2.5m, "DEP")], "KRD"));

        result.Matched.Should().BeEmpty();
        result.Rejected.Should().NotBeEmpty();
    }

    // The COBOL required both files sorted and one-to-one. Matching on the key instead means an
    // unsorted input reconciles correctly rather than silently producing wrong output.
    [Fact]
    public void AnUnsortedInputReconcilesTheSameAsASortedOne()
    {
        RateEntry[] primary = [new("K3", 3m, "KRD"), new("K1", 1m, "KRD"), new("K2", 2m, "KRD")];
        RateEntry[] secondary = [new("K2", 2m, "KRD"), new("K3", 3m, "KRD"), new("K1", 1m, "KRD")];

        var result = _reconciler.Reconcile(Sets(primary, secondary));

        result.Matched.Should().HaveCount(3);
        result.MissingCounterpart.Should().BeEmpty();
        result.Differing.Should().BeEmpty();
    }

    [Fact]
    public void AnEntryWithoutAKeyCannotBeReconciledAndIsRejected()
    {
        var result = _reconciler.Reconcile(Sets([new("", 1m, "KRD")], []));

        result.Rejected.Should().ContainSingle().Which.Should().Contain("no key");
    }

    [Fact]
    public void TheOriginalInputSizesAreReportedSoExclusionsAreVisible()
    {
        var result = _reconciler.Reconcile(Sets(
            [new("K1", 1m, "KRD"), new("K2", 1m, "DEP")],
            [new("K1", 1m, "KRD")]));

        result.PrimaryCount.Should().Be(2);
        result.SecondaryCount.Should().Be(1);
        result.Matched.Should().ContainSingle();
    }
}
