// Decides who declares the interface for a called program, and what it looks like.
//
// The converter prompt asked each program to "generate a service interface (e.g. IDateService)"
// for every CALL it makes, and specified neither the name nor the shape. Seven programs calling
// the same module produced seven declarations of IBdsmfjlService — and the three examined declared
// three different methods on it: ExecuteAsync, ReportAsync and ErrorCodemeldAsync. While every
// program had its own package that merely duplicated; once a service shares one namespace it does
// not compile.
//
// A COBOL program has one entry point, so its interface has one method. Naming both, and naming a
// single program responsible for declaring it, removes the thing the callers were disagreeing
// about rather than asking them to agree.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using System.Text.RegularExpressions;

public sealed record CallTargetContract(
    string Target,
    string InterfaceName,
    string MethodName,
    string DeclaredBy,
    IReadOnlyList<string> Callers)
{
    /// <summary>True when <paramref name="program"/> is the one that must declare the interface.</summary>
    public bool IsDeclaredBy(string program) =>
        string.Equals(DeclaredBy, program, StringComparison.OrdinalIgnoreCase);
}

public sealed class CallTargetRegistry
{
    public const string EntryPointMethod = "ExecuteAsync";

    // CALL 'NAME' and CALL NAME both occur; a CALL through a data name cannot be resolved
    // statically and is left alone.
    private static readonly Regex CallDirective = new(
        @"\bCALL\s+['""]([A-Za-z][A-Za-z0-9_-]*)['""]",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private readonly Dictionary<string, CallTargetContract> _byTarget =
        new(StringComparer.OrdinalIgnoreCase);

    public IReadOnlyCollection<CallTargetContract> Contracts => _byTarget.Values;

    public static CallTargetRegistry Build(string sourceFolder)
    {
        var registry = new CallTargetRegistry();
        if (!Directory.Exists(sourceFolder)) return registry;

        var programs = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (var path in SourceTypeRegistry.EnumerateProgramFiles(sourceFolder))
        {
            try { programs[Stem(path)] = File.ReadAllText(path); }
            catch (IOException) { }
        }

        // Every (callee, caller) pair the source states, grouped by callee. A program calling
        // itself is dropped: recursion needs no interface, and handing a program one for itself
        // would have it declare and inject a service that is already the class being written.
        var callers = programs
            .SelectMany(program => CallDirective
                .Matches(StripComments(program.Value))
                .Select(match => (Target: match.Groups[1].Value, Caller: program.Key)))
            .Where(edge => !string.Equals(edge.Target, edge.Caller, StringComparison.OrdinalIgnoreCase))
            .GroupBy(edge => edge.Target, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(
                group => group.Key,
                group => new SortedSet<string>(group.Select(e => e.Caller), StringComparer.OrdinalIgnoreCase),
                StringComparer.OrdinalIgnoreCase);

        // The called program knows its own contract, so it declares it. When the target is not in
        // this source drop there is no such program, and the first caller in a stable order is
        // made responsible — an arbitrary choice, but the same arbitrary choice on every run and
        // for every caller, which is what stops the duplicate.
        var contracts = callers.Select(entry => new CallTargetContract(
            Target: entry.Key,
            InterfaceName: "I" + ToPascalCase(entry.Key) + "Service",
            MethodName: EntryPointMethod,
            DeclaredBy: programs.ContainsKey(entry.Key) ? entry.Key : entry.Value.First(),
            Callers: entry.Value.ToList()));

        foreach (var contract in contracts)
            registry._byTarget[contract.Target] = contract;

        return registry;
    }

    /// <summary>
    /// The contract block for one program: what it must declare, and what it must only reference.
    /// </summary>
    public string ToPromptBlock(string programStem, string targetLanguage)
    {
        var relevant = _byTarget.Values
            .Where(c => c.IsDeclaredBy(programStem)
                     || c.Callers.Contains(programStem, StringComparer.OrdinalIgnoreCase))
            .OrderBy(c => c.Target, StringComparer.OrdinalIgnoreCase)
            .ToList();

        if (relevant.Count == 0) return string.Empty;

        var sharedNamespace = ConversionNamespacePolicy.ForSharedTypes(targetLanguage);

        var declares = new StringBuilder();
        var references = new StringBuilder();

        foreach (var contract in relevant)
        {
            if (contract.IsDeclaredBy(programStem))
            {
                declares.AppendLine(
                    $"  • {contract.InterfaceName} — one method, {contract.MethodName}, "
                    + $"for the single entry point of {contract.Target}. "
                    + $"Called by {contract.Callers.Count} program(s).");
            }
            else
            {
                references.AppendLine(
                    $"  • {contract.InterfaceName}.{contract.MethodName} — declared by {contract.DeclaredBy}.");
            }
        }

        return Environment.NewLine + PromptLoader.LoadSectionValidated(
            "RektContext", "CallTargetContracts", new Dictionary<string, string>
            {
                ["SharedNamespace"] = sharedNamespace,
                ["Declares"] = declares.Length == 0 ? "  (none)" : declares.ToString().TrimEnd(),
                ["References"] = references.Length == 0 ? "  (none)" : references.ToString().TrimEnd(),
            });
    }

    private static string Stem(string path) =>
        Path.GetFileNameWithoutExtension(Path.GetFileName(path));

    private static string StripComments(string text) => string.Join('\n',
        text.Split('\n').Where(line =>
            !(line.Length > 6 && (line[6] == '*' || line[6] == '/'))
            && !line.TrimStart().StartsWith("*", StringComparison.Ordinal)));

    private static string ToPascalCase(string name)
    {
        var parts = name.Split(new[] { '-', '_' }, StringSplitOptions.RemoveEmptyEntries);
        var sb = new StringBuilder();
        foreach (var part in parts)
        {
            sb.Append(char.ToUpperInvariant(part[0]));
            if (part.Length > 1) sb.Append(part[1..].ToLowerInvariant());
        }
        return sb.Length == 0 ? name : sb.ToString();
    }
}

public static class CallTargetRegistryHolder
{
    private static readonly object Lock = new();
    private static readonly Dictionary<string, CallTargetRegistry> Cache =
        new(StringComparer.OrdinalIgnoreCase);

    public static CallTargetRegistry GetOrBuild(string repoRoot, string sourceFolder)
    {
        // An absolute source folder replaces the repository root rather than being appended to
        // it. Written as a test so the cache key cannot quietly become a path that exists
        // nowhere — a miss here is silent, which is the worst place for an implicit rule.
        var key = Path.IsPathRooted(sourceFolder) ? sourceFolder : Path.Join(repoRoot, sourceFolder);
        lock (Lock)
        {
            if (Cache.TryGetValue(key, out var existing)) return existing;
            return Cache[key] = CallTargetRegistry.Build(key);
        }
    }
}
