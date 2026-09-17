// Answers "what does this program depend on" from the parse rather than from a guess.
//
// Discovery happens three times in this pipeline and only one of them reads the parser's output.
// The dependency mapper matches COPY, EXEC SQL and READ/WRITE with hand-written regular
// expressions; the analyzer spends an LLM call per file describing structure; and REKT, which has
// already parsed the same source into an AST, a control-flow graph and a set of data structures,
// is consulted by the converters alone.
//
// Three mechanisms that never compare answers is the failure this estate keeps producing: two
// sources disagree, nothing notices, and the reassuring one is believed. A regex also cannot see
// what a parser sees — a COPY inside a copybook, a CALL reached through a chain, an EXEC SQL verb
// rather than the word EXEC.
//
// So the parse answers where it can, and the caller is told which source answered. It is not
// authoritative everywhere: where a copybook was missing the parser was handed a synthesised stub,
// so on one estate only 6 programs of 41 parsed at full fidelity. Below the required confidence
// this yields and the existing extraction runs unchanged.

namespace CobolToQuarkusMigration.Agents.Infrastructure;

using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

/// <summary>Which mechanism produced a dependency answer.</summary>
public enum DiscoverySource
{
    /// <summary>Read from the REKT parse.</summary>
    Parse,

    /// <summary>Derived by pattern matching over the source text.</summary>
    TextScan,
}

public sealed record DiscoveryResult<T>(T Value, DiscoverySource Source)
{
    public bool FromParse => Source == DiscoverySource.Parse;
}

public sealed class RektDiscovery
{
    /// <summary>
    /// Facts below this are not trusted over a text scan. Partial means a copybook was absent and
    /// the parser worked from a stub, so its structural claims are inference with better
    /// presentation — exactly the thing that must not silently outrank a plain scan.
    /// </summary>
    public const FactConfidence MinimumConfidence = FactConfidence.High;

    private readonly string _factsDirectory;
    private readonly Dictionary<string, ProgramFacts?> _cache = new(StringComparer.OrdinalIgnoreCase);

    public RektDiscovery(string factsDirectory) => _factsDirectory = factsDirectory;

    /// <summary>Whether any parse output exists at all.</summary>
    public bool Available => Directory.Exists(_factsDirectory);

    /// <summary>Counts of how each answer was reached, for reporting after a run.</summary>
    public int AnsweredByParse { get; private set; }

    public int AnsweredByTextScan { get; private set; }

    /// <summary>
    /// The copybooks a program uses, from the parse when it is trustworthy.
    /// </summary>
    /// <param name="fallback">
    /// Invoked only when the parse cannot answer. Passed rather than called first so that the
    /// text scan is not paid for on every program that the parse already covers.
    /// </param>
    public DiscoveryResult<List<string>> Copybooks(string programFileName, Func<List<string>> fallback)
        => Resolve(programFileName, facts => facts.Data.CopybooksUsed, fallback);

    /// <summary>The programs a program calls, from the parse when it is trustworthy.</summary>
    public DiscoveryResult<List<string>> Callees(string programFileName, Func<List<string>> fallback)
        => Resolve(programFileName, facts => facts.Callees, fallback);

    /// <summary>The database tables a program touches, from the parse when it is trustworthy.</summary>
    public DiscoveryResult<List<string>> DbTables(string programFileName, Func<List<string>> fallback)
        => Resolve(
            programFileName,
            facts => facts.Io.DbTables.Select(t => t.Name).ToList(),
            fallback);

    private DiscoveryResult<List<string>> Resolve(
        string programFileName,
        Func<ProgramFacts, IEnumerable<string>> read,
        Func<List<string>> fallback)
    {
        var facts = Load(programFileName);

        if (facts is not null && facts.Confidence >= MinimumConfidence)
        {
            var values = read(facts)
                .Where(v => !string.IsNullOrWhiteSpace(v))
                .Select(v => v.Trim())
                .Distinct(StringComparer.OrdinalIgnoreCase)
                .ToList();

            // An empty list from a full parse is an answer — the program really does COPY
            // nothing — but only when the parse is complete enough to say so.
            AnsweredByParse++;
            return new DiscoveryResult<List<string>>(values, DiscoverySource.Parse);
        }

        AnsweredByTextScan++;
        return new DiscoveryResult<List<string>>(fallback(), DiscoverySource.TextScan);
    }

    private ProgramFacts? Load(string programFileName)
    {
        var basename = Path.GetFileName(programFileName);
        if (_cache.TryGetValue(basename, out var cached)) return cached;

        ProgramFacts? facts = null;
        if (Available)
        {
            try { facts = ProgramFactsArtifactLocator.TryLoad(_factsDirectory, basename); }
            catch (IOException) { }
        }

        return _cache[basename] = facts;
    }

    /// <summary>A line for the run log saying how much of the answer came from the parse.</summary>
    public string Summarise()
    {
        var total = AnsweredByParse + AnsweredByTextScan;
        if (total == 0) return "No dependency discovery was performed.";

        var percent = AnsweredByParse * 100 / total;
        return $"Dependency discovery: {AnsweredByParse} of {total} answers came from the REKT parse "
             + $"({percent}%); {AnsweredByTextScan} fell back to a text scan because no parse output "
             + "was trustworthy for that program.";
    }
}
