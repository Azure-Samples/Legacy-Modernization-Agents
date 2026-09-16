// Reconciles two sets of interest rates.
//
// Modelled on the rate-comparison batch in the estate, which read two sequential files expected
// to be sorted on the same key and one-to-one, wrote the entries that agreed to one output and
// the entries without a counterpart to another, and rejected rates that were not of the required
// category or were flagged as fictitious.
//
// The COBOL depended on both files arriving sorted, which is a property of the job that produced
// them rather than of the data. Matching on the key here removes that assumption, so an unsorted
// input is reconciled correctly instead of silently producing wrong output.

namespace Modernized.Banking.Portal.Services;

using Modernized.Banking.Portal.Domain;

public sealed class RateReconciler : IRateReconciler
{
    public ReconcileResult Reconcile(ReconcileRequest request)
    {
        var rejected = new List<string>();

        var primary = Eligible(request.Primary, request.RequiredCategory, "primary", rejected);
        var secondary = Eligible(request.Secondary, request.RequiredCategory, "secondary", rejected)
            .ToDictionary(e => e.Key, StringComparer.OrdinalIgnoreCase);

        var matched = new List<RateEntry>();
        var missing = new List<RateEntry>();
        var differing = new List<RateDifference>();

        foreach (var entry in primary)
        {
            if (!secondary.TryGetValue(entry.Key, out var counterpart))
            {
                missing.Add(entry);
                continue;
            }

            if (counterpart.Rate == entry.Rate)
            {
                matched.Add(entry);
            }
            else
            {
                differing.Add(new RateDifference(
                    entry.Key, entry.Rate, counterpart.Rate, counterpart.Rate - entry.Rate));
            }
        }

        return new ReconcileResult(
            matched, missing, differing, rejected,
            request.Primary.Count, request.Secondary.Count);
    }

    private static List<RateEntry> Eligible(
        IReadOnlyList<RateEntry> entries, string requiredCategory, string side, List<string> rejected)
    {
        var eligible = new List<RateEntry>();

        foreach (var entry in entries)
        {
            if (string.IsNullOrWhiteSpace(entry.Key))
            {
                rejected.Add($"{side}: an entry has no key and cannot be reconciled.");
                continue;
            }

            if (entry.Fictitious)
            {
                rejected.Add($"{side}: {entry.Key} is flagged fictitious and is excluded.");
                continue;
            }

            if (!string.IsNullOrWhiteSpace(requiredCategory)
                && !string.Equals(entry.Category, requiredCategory, StringComparison.OrdinalIgnoreCase))
            {
                rejected.Add($"{side}: {entry.Key} is category '{entry.Category}', not '{requiredCategory}'.");
                continue;
            }

            eligible.Add(entry);
        }

        return eligible;
    }
}
