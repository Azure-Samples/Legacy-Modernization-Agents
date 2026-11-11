using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;
using Microsoft.Extensions.Logging.Abstractions;

namespace McpChatWeb.Services;

public sealed record DependencyReadiness(
    IReadOnlyList<ProgramConversionPlan> Programs, string SourceFolder, string FactsFolder,
    DateTime CheckedAt, string? SelectionError = null)
{
    public bool StrictAllowed => Programs.Count > 0
        && Programs.All(p => p.Status == "eligible") && SelectionError is null;
    public bool DraftAllowed => Programs.Any(p => p.Status == "eligible") && SelectionError is null;

    public bool Allows(string mode) => mode switch
    {
        "strict" => StrictAllowed,
        "draft" => DraftAllowed,
        "partial" => true,
        _ => false
    };

    public string BlockedMessage => SelectionError
        ?? "Conversion will not start: the selected programs lack valid program dependency evidence. "
        + "Review the per-program blockers, supply the real inputs and regenerate matching parser evidence and facts. "
        + "Parsed structure alone does not establish dependency readiness.";

    public static async Task<DependencyReadiness> AssessAsync(
        string sourceRoot, string factsRoot, IReadOnlyList<string>? selectors = null,
        CancellationToken ct = default)
    {
        var files = (await new FileHelper(NullLogger<FileHelper>.Instance)
                .ScanDirectoryForCobolFilesAsync(sourceRoot))
            .Where(file => !file.IsCopybook).ToList();
        ct.ThrowIfCancellationRequested();
        if (selectors is { Count: > 0 })
        {
            files = ProgramSelection.Apply(files, selectors, out var unmatched);
            if (unmatched.Count > 0)
                throw new ArgumentException($"Selected programs are not present: {string.Join(", ", unmatched)}.");
        }
        var gate = new DependencyEvidenceGate(factsRoot);
        var plans = DraftConversionPlanner.Assess(gate, factsRoot, files, sourceRoot,
            propagateSelectedCallees: selectors is not null).Programs;
        string? error = null;
        if (selectors is not null)
        {
            var eligible = plans.Where(p => p.Status == "eligible").Select(p => p.Program)
                .ToHashSet(StringComparer.Ordinal);
            var selected = files.Where(file => eligible.Contains(DependencyEvidenceGate.Identity(file, sourceRoot))).ToList();
            if (selected.Count > 0)
            {
                try { gate.RequireVerified(selected, sourceRoot); }
                catch (InvalidDataException ex) { error = ex.Message; }
            }
        }
        return new(plans, sourceRoot, factsRoot, DateTime.UtcNow, error);
    }
}
