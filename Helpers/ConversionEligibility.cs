using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

public static class ConversionEligibility
{
    public const string EnvironmentVariable = "CONVERSION_DEPENDENCY_MODE";

    public static string FactsDirectory(string repositoryRoot)
    {
        var configured = Environment.GetEnvironmentVariable("REKT_FACTS_FOLDER");
        return Path.GetFullPath(string.IsNullOrWhiteSpace(configured)
            ? Path.Combine(repositoryRoot, "output", "rekt")
            : Path.IsPathRooted(configured) ? configured : Path.Combine(repositoryRoot, configured));
    }

    public static string ValidateMode(string mode) => mode is "strict" or "draft" or "partial" ? mode
        : throw new ArgumentException("Dependency mode must be 'strict', 'draft' or explicit source-isolated 'partial'.");

    public static bool IsPartial(ConversionSettings settings) => ValidateMode(settings.DependencyMode) == "partial";

    public static bool IsDraft(ConversionSettings settings) =>
        ValidateMode(settings.DependencyMode) == "draft";

    public static List<CobolFile> Select(
        List<CobolFile> files, string sourceRoot, string factsDirectory, string outputFolder,
        ConversionSettings settings, DependencyMap? reusedMap)
    {
        if (IsPartial(settings))
            throw new InvalidDataException("Source-isolated partial conversion must use the partial-tree process, not whole-program eligibility.");
        var gate = new DependencyEvidenceGate(factsDirectory);
        if (!IsDraft(settings))
        {
            files = gate.ExcludeSupersededCopybooks(files, sourceRoot);
            gate.RequireVerified(files, sourceRoot, reusedMap);
            ConversionSourceScope.Set(files);
            return files;
        }
        if (Directory.Exists(outputFolder) && Directory.EnumerateFileSystemEntries(outputFolder).Any())
            throw new InvalidDataException("Draft conversion requires a fresh empty output directory; previous code must not masquerade as an eligible draft.");
        var plan = DraftConversionPlanner.Assess(gate, factsDirectory, files, sourceRoot);
        plan.Save(outputFolder);
        var eligible = plan.Programs.Where(p => p.Status == "eligible").Select(p => p.Program).ToHashSet(StringComparer.Ordinal);
        var selected = files.Where(f => eligible.Contains(DependencyEvidenceGate.Identity(f, sourceRoot))).ToList();
        if (selected.Count == 0)
            throw new InvalidDataException("No independently justified whole programs. See draft-conversion-plan.json for exact input/evidence blockers and corrective actions; no model conversion is permitted.");
        try { gate.RequireVerified(selected, sourceRoot); }
        catch (InvalidDataException ex)
        {
            plan = new(plan.Programs.Select(p => p.Status != "eligible" ? p : p with
            {
                Status = "blocked",
                Blockers = [new("artifact-blocker", "Consistent current selected-program evidence",
                    ex.Message, "Regenerate matching selected-program facts and exports for the exact sources; reassess before any model conversion.")]
            }).ToArray());
            plan.Save(outputFolder);
            throw;
        }
        ConversionSourceScope.Set(selected);
        return selected;
    }
}
