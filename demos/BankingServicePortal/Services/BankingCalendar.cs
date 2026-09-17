// Resolves banking dates and validates date input.
//
// Modelled on the KDATO function module and its companion error-text module in the estate: one
// finds the applicable banking date, the other turns a rejected date into something a user can
// read. The holiday set here is the common Western European bank holiday pattern rather than any
// customer's calendar, which is data rather than logic and belongs in configuration.

namespace Modernized.Banking.Portal.Services;

using Modernized.Banking.Portal.Domain;

public sealed class BankingCalendar : IBankingCalendar
{
    private const int MaxShiftDays = 14;

    public BankingDateResult Resolve(DateOnly asOf, int offsetDays)
    {
        var target = asOf.AddDays(offsetDays);
        var classification = Classify(target);

        var shifted = target;
        var moved = 0;
        while (Classify(shifted) != DayClassification.BankingDay && moved < MaxShiftDays)
        {
            shifted = shifted.AddDays(1);
            moved++;
        }

        var explanation = classification == DayClassification.BankingDay
            ? $"{target:yyyy-MM-dd} is a banking day."
            : $"{target:yyyy-MM-dd} is a {classification.ToString().ToLowerInvariant()}; "
              + $"the next banking day is {shifted:yyyy-MM-dd}.";

        return new BankingDateResult(target, shifted, classification, moved, explanation);
    }

    public DateValidationResult Validate(string? value)
    {
        if (string.IsNullOrWhiteSpace(value))
            return new DateValidationResult(false, null, "DATE_MISSING", "No date was supplied.");

        var trimmed = value.Trim();

        // The estate's date fields are unseparated numerics, so both forms are accepted and the
        // caller is told which one was understood rather than being left to guess.
        string[] formats = ["yyyy-MM-dd", "yyyyMMdd", "dd-MM-yyyy", "ddMMyyyy", "dd.MM.yyyy"];
        if (!DateOnly.TryParseExact(trimmed, formats, null,
                System.Globalization.DateTimeStyles.None, out var parsed))
        {
            return new DateValidationResult(false, null, "DATE_FORMAT",
                $"'{trimmed}' is not a recognised date. Accepted forms: {string.Join(", ", formats)}.");
        }

        if (parsed.Year < 1900)
            return new DateValidationResult(false, parsed, "DATE_RANGE_LOW",
                $"{parsed:yyyy-MM-dd} is before 1900 and is treated as unset.");

        if (parsed.Year > DateTime.UtcNow.Year + 50)
            return new DateValidationResult(false, parsed, "DATE_RANGE_HIGH",
                $"{parsed:yyyy-MM-dd} is implausibly far in the future.");

        return new DateValidationResult(true, parsed, "OK", $"{parsed:yyyy-MM-dd} is a valid date.");
    }

    public IReadOnlyList<DateOnly> HolidaysIn(int year) =>
        FixedHolidays(year).Concat(EasterRelated(year)).OrderBy(d => d).ToList();

    private DayClassification Classify(DateOnly date)
    {
        if (date.DayOfWeek is DayOfWeek.Saturday or DayOfWeek.Sunday)
            return DayClassification.Weekend;

        return HolidaysIn(date.Year).Contains(date)
            ? DayClassification.Holiday
            : DayClassification.BankingDay;
    }

    private static IEnumerable<DateOnly> FixedHolidays(int year) =>
    [
        new(year, 1, 1),    // New Year's Day
        new(year, 5, 1),    // Labour Day
        new(year, 12, 24),  // Christmas Eve
        new(year, 12, 25),  // Christmas Day
        new(year, 12, 26),  // Boxing Day
        new(year, 12, 31),  // New Year's Eve
    ];

    private static IEnumerable<DateOnly> EasterRelated(int year)
    {
        var easter = Easter(year);
        yield return easter.AddDays(-3);  // Maundy Thursday
        yield return easter.AddDays(-2);  // Good Friday
        yield return easter.AddDays(1);   // Easter Monday
        yield return easter.AddDays(39);  // Ascension Day
        yield return easter.AddDays(50);  // Whit Monday
    }

    /// <summary>Anonymous Gregorian computus — the standard Easter algorithm.</summary>
    private static DateOnly Easter(int year)
    {
        var a = year % 19;
        var b = year / 100;
        var c = year % 100;
        var d = b / 4;
        var e = b % 4;
        var f = (b + 8) / 25;
        var g = (b - f + 1) / 3;
        var h = (19 * a + b - d - g + 15) % 30;
        var i = c / 4;
        var k = c % 4;
        var l = (32 + 2 * e + 2 * i - h - k) % 7;
        var m = (a + 11 * h + 22 * l) / 451;
        var month = (h + l - 7 * m + 114) / 31;
        var day = ((h + l - 7 * m + 114) % 31) + 1;
        return new DateOnly(year, month, day);
    }
}
