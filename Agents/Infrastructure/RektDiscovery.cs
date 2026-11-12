using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

public enum DiscoverySource
{
    Parse,
    TextScan,
}

public sealed record DiscoveryResult<T>(T Value, DiscoverySource Source)
{
    public bool FromParse => Source == DiscoverySource.Parse;
}

/// <summary>Reads verified local REKT disk exports. It never queries Neo4j or runs a parser.</summary>
public sealed class RektDiscovery(string factsDirectory)
{
    public bool Available => Directory.Exists(factsDirectory);
    public int AnsweredByParse { get; private set; }
    public int AnsweredByTextScan => 0;

    public DependencyMap RequireVerified(IReadOnlyList<CobolFile> files, string sourceRoot)
    {
        // Do not cache validation: either a source or a resolved definition may change between requests.
        var map = new DependencyEvidenceGate(factsDirectory).RequireVerified(files, sourceRoot);
        AnsweredByParse += map.CopybookUsage.Count;
        return map;
    }

    // Compatibility overloads cannot validate original source bytes. Never call their scanner.
    public DiscoveryResult<List<string>> Copybooks(string programFileName, Func<List<string>> fallback) =>
        throw MissingSource(programFileName);

    public DiscoveryResult<List<string>> Callees(string programFileName, Func<List<string>> fallback) =>
        throw MissingSource(programFileName);

    public DiscoveryResult<List<string>> DbTables(string programFileName, Func<List<string>> fallback) =>
        throw MissingSource(programFileName);

    private static InvalidDataException MissingSource(string identity) =>
        new($"{identity}: dependency evidence requires original source identity/hash and per-kind coverage; " +
            "use RequireVerified with the selected source root. High confidence and text scanning are not authoritative.");

    public string Summarise() => AnsweredByParse == 0
        ? "No dependency discovery was performed."
        : $"Dependency discovery: {AnsweredByParse} program(s) verified from local REKT disk exports. " +
            "No text scanner fallback, Neo4j query, or new parser execution.";
}
