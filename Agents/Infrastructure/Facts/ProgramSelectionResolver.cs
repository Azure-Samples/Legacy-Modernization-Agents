namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed record ProgramSelection
{
    public IReadOnlyList<string> Programs { get; init; } = Array.Empty<string>();

    public bool IncludeCallers { get; init; }

    public bool IncludeCallees { get; init; }

    public bool WantsClosure => IncludeCallers || IncludeCallees;
}

public sealed record SelectionMatch(string Program, string Reason);

public sealed record ProgramSelectionResult
{
    public IReadOnlyList<string> Programs { get; init; } = Array.Empty<string>();

    public IReadOnlyList<SelectionMatch> Matches { get; init; } = Array.Empty<SelectionMatch>();

    public IReadOnlyList<string> UnresolvedCallTargets { get; init; } = Array.Empty<string>();
}

public sealed record ProgramCallEdges(
    IReadOnlyList<string> Callers,
    IReadOnlyList<string> Callees);

public interface IProgramClosureSource
{
    // Null means "no evidence recorded", which is distinct from "recorded as having no edges".
    ProgramCallEdges? TryGetEdges(string programRelativePath);

    string EvidenceLocation { get; }
}

public sealed class FactsClosureSource(string factsDir) : IProgramClosureSource
{
    public string EvidenceLocation { get; } = factsDir;

    public ProgramCallEdges? TryGetEdges(string programRelativePath)
    {
        var facts = ProgramFactsArtifactLocator.TryLoad(factsDir, programRelativePath);
        return facts is null ? null : new ProgramCallEdges(facts.Callers, facts.Callees);
    }
}

public sealed class ProgramSelectionResolver(
    ProgramSourceCatalog catalog,
    IProgramClosureSource closureSource)
{
    public ProgramSelectionResult Resolve(ProgramSelection selection)
    {
        if (selection.Programs.Count == 0)
        {
            throw new InvalidOperationException(
                "Conversion scope requires at least one program selector, but no program selector was supplied.");
        }

        var reasons = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        var pending = new Queue<string>();

        foreach (var selector in selection.Programs)
        {
            var resolved = catalog.ResolveSelector(selector);
            if (reasons.TryAdd(resolved, $"program selector '{selector}'"))
                pending.Enqueue(resolved);
        }

        var unresolved = new List<string>();

        if (selection.WantsClosure)
        {
            while (pending.Count > 0)
            {
                var current = pending.Dequeue();
                var edges = closureSource.TryGetEdges(current)
                    ?? throw MissingClosureEvidence(current);

                if (selection.IncludeCallees)
                    Expand(edges.Callees, origin => $"called by {origin}");
                if (selection.IncludeCallers)
                    Expand(edges.Callers, origin => $"calls {origin}");

                void Expand(IReadOnlyList<string> targets, Func<string, string> describe)
                {
                    foreach (var target in targets)
                    {
                        if (!catalog.TryResolveSourceIdentity(target, out var resolvedTarget))
                        {
                            var raw = target.Trim();
                            if (raw.Length > 0 && !unresolved.Contains(raw, StringComparer.OrdinalIgnoreCase))
                                unresolved.Add(raw);
                            continue;
                        }

                        if (reasons.TryAdd(resolvedTarget, describe(current)))
                            pending.Enqueue(resolvedTarget);
                    }
                }
            }
        }

        var ordered = reasons.Keys
            .OrderBy(program => program, StringComparer.OrdinalIgnoreCase)
            .ToList();

        return new ProgramSelectionResult
        {
            Programs = ordered,
            Matches = ordered.Select(program => new SelectionMatch(program, reasons[program])).ToList(),
            UnresolvedCallTargets = unresolved
                .OrderBy(target => target, StringComparer.OrdinalIgnoreCase)
                .ToList(),
        };
    }

    private Exception MissingClosureEvidence(string programRelativePath) =>
        new InvalidOperationException(
            $"Dependency closure needs REKT facts for '{programRelativePath}', but none were found under " +
            $"'{closureSource.EvidenceLocation}'. Run a rekt-scan over the whole estate first, or drop " +
            "--include-callers/--include-callees. Converting a partial closure would silently omit programs.");
}
