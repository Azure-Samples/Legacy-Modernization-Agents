using System.Text.Json;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed record ConversionBlocker(
    string Category, string MissingEvidence, string Reason, string Action,
    string? Routine = null, string? Location = null, string? Raw = null, string? DiagnosticCategory = null);

public sealed record ProgramConversionPlan(string Program, string Status, IReadOnlyList<ConversionBlocker> Blockers)
{
    public DependencyInputInventoryResult? DependencyInputs { get; init; }
}

public sealed record DraftConversionPlan(IReadOnlyList<ProgramConversionPlan> Programs)
{
    public string Mode => "draft";
    public bool Verified => false;
    public string Status => Programs.Any(p => p.Status == "eligible") ? "partial-not-verified" : "blocked";
    public string Isolation => "Whole programs only; no routine safety or affected-routine counts established.";

    public void Save(string outputDirectory)
    {
        Directory.CreateDirectory(outputDirectory);
        File.WriteAllText(Path.Combine(outputDirectory, "draft-conversion-plan.json"),
            JsonSerializer.Serialize(this, new JsonSerializerOptions { WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase }));
        File.WriteAllText(Path.Combine(outputDirectory, "DRAFT-NOT-VERIFIED.txt"),
            "PARTIAL DRAFT — NOT VERIFIED. Only explicitly eligible whole programs may be converted. " +
            "Blocked programs have no generated implementation. See draft-conversion-plan.json. " +
            "Compile/parity results for a subset do not establish completeness or estate equivalence.");
    }
}

public static class DraftConversionPlanner
{
    public static DraftConversionPlan Assess(
        DependencyEvidenceGate gate, string factsDirectory, IReadOnlyList<CobolFile> files, string sourceRoot,
        bool propagateSelectedCallees = true)
    {
        var plans = new List<ProgramConversionPlan>();
        var catalog = ProgramSourceCatalog.FromStagingDirectory(sourceRoot);
        foreach (var file in files)
        {
            var identity = SourcePathHelper.NormalizeRelativePath(Path.GetRelativePath(sourceRoot, file.FilePath));
            if (!SourceTypeRegistry.IsCobolProgram(file.FileName))
            {
                plans.Add(new(identity, "blocked", [new("definition-context", "Verified program-context projection for standalone conversion",
                    "A copybook is not an independently justified program; shared fields and control flow cannot be reconstructed.",
                    "Use the real hash-bound definition in a verified program context; do not generate a standalone layout or stub.")]));
                continue;
            }
            try
            {
                gate.RequireVerified([file], sourceRoot, null, catalog);
                plans.Add(new(identity, "eligible", []));
            }
            catch (InvalidDataException ex)
            {
                var issues = ex.Message.Split("\n- ", StringSplitOptions.RemoveEmptyEntries).Skip(1);
                plans.Add(new(identity, "blocked", issues.Select(issue => DependencyBlockerDescriptions.Describe(identity, issue)).ToArray()));
            }
        }

        var inventory = new DependencyInputInventory(sourceRoot);
        plans = plans.Select(plan => plan with { DependencyInputs = inventory.Read(factsDirectory, plan.Program) }).ToList();
        if (!propagateSelectedCallees) return new(plans);

        // Do not admit a caller whose selected callee is blocked, even if the caller's export is otherwise valid.
        bool changed;
        do
        {
            changed = false;
            for (var i = 0; i < plans.Count; i++)
            {
                var plan = plans[i];
                if (plan.Status != "eligible") continue;
                var evidence = DependencyEvidenceGate.LoadEvidence(factsDirectory, plan.Program)!;
                var blocked = evidence.Definitions.Where(d => d.Kind == "CALL")
                    .Where(d => plans.Any(p => p.Status == "blocked" &&
                        Path.GetFullPath(Path.Combine(sourceRoot, p.Program)) == Path.GetFullPath(d.Path)))
                    .Select(d => d.Name).ToArray();
                if (blocked.Length == 0) continue;
                plans[i] = plan with
                {
                    Status = "blocked",
                    Blockers = [new("coverage-gap",
                    $"Selected CALL targets: {string.Join(", ", blocked)}",
                    "A selected callee is blocked; independent whole-program conversion is not established.",
                    "Resolve the callee's recorded blockers and regenerate caller/callee evidence.")]
                };
                changed = true;
            }
        } while (changed);
        return new(plans);
    }

}
