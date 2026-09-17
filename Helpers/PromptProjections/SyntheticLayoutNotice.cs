// Tells the converter which record layouts in its context were invented by the toolchain
// rather than parsed from the customer's source.
//
// When a COPY target is absent from the source drop, tools/preprocess-for-rekt.sh writes a
// placeholder copybook so smojol can still produce a full AST. The placeholder reaches the
// converter through data_structures JSON looking exactly like a parsed layout, while the
// surrounding policy instructs the model to emit every field of every group and never
// simplify. The model resolves that by inventing a plausible structure and documenting it
// as though it were known, which is the one failure mode that cannot be detected downstream:
// the output is confident, complete, and unfounded.
//
// Naming the synthetic layouts explicitly lets the completeness rules stay strict everywhere
// they are justified, and stop exactly where the evidence stops.

namespace CobolToQuarkusMigration.Helpers.PromptProjections;

using System.Text;
using CobolToQuarkusMigration.Helpers;

internal static class SyntheticLayoutNotice
{
    private const string WarningPrefix = "generated-copybook-stub:";

    /// <summary>
    /// Renders the synthetic-layout policy for the stub copybooks named in <paramref name="warnings"/>,
    /// or an empty string when the program's layouts all came from a real parse.
    /// </summary>
    public static string Build(IReadOnlyList<string> warnings)
    {
        var copybooks = StubCopybooksIn(warnings);
        if (copybooks.Count == 0) return string.Empty;

        var names = new StringBuilder();
        foreach (var name in copybooks) names.AppendLine($"  • {name}");

        return PromptLoader.LoadSectionValidated(
            "RektContext", "SyntheticLayoutPolicy", new Dictionary<string, string>
            {
                ["StubCopybooks"] = names.ToString().TrimEnd('\r', '\n')
            });
    }

    private static List<string> StubCopybooksIn(IReadOnlyList<string> warnings) => warnings
        .Where(w => w.StartsWith(WarningPrefix, StringComparison.OrdinalIgnoreCase))
        .Select(w => w[WarningPrefix.Length..].Trim())
        .Where(name => name.Length > 0)
        .Distinct(StringComparer.OrdinalIgnoreCase)
        .OrderBy(name => name, StringComparer.OrdinalIgnoreCase)
        .ToList();
}
