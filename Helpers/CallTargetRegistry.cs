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

        var callers = new Dictionary<string, SortedSet<string>>(StringComparer.OrdinalIgnoreCase);
        foreach (var (name, content) in programs)
        {
            foreach (Match match in CallDirective.Matches(StripComments(content)))
            {
                var target = match.Groups[1].Value;
                if (string.Equals(target, name, StringComparison.OrdinalIgnoreCase)) continue;

                if (!callers.TryGetValue(target, out var set))
                    callers[target] = set = new SortedSet<string>(StringComparer.OrdinalIgnoreCase);
                set.Add(name);
            }
        }

        foreach (var (target, callingPrograms) in callers)
        {
            // The called program knows its own contract, so it declares it. When the target is not
            // in this source drop there is no such program, and the first caller in a stable order
            // is made responsible — an arbitrary choice, but the same arbitrary choice on every
            // run and for every caller, which is what stops the duplicate.
            var declaredBy = programs.ContainsKey(target) ? target : callingPrograms.First();

            _ = registry._byTarget.TryAdd(target, new CallTargetContract(
                Target: target,
                InterfaceName: "I" + ToPascalCase(target) + "Service",
                MethodName: EntryPointMethod,
                DeclaredBy: declaredBy,
                Callers: callingPrograms.ToList()));
        }

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
        // Combine, not Join: sourceFolder may be absolute, in which case it is meant to win.
        var key = Path.Combine(repoRoot, sourceFolder);
        lock (Lock)
        {
            if (Cache.TryGetValue(key, out var existing)) return existing;
            return Cache[key] = CallTargetRegistry.Build(key);
        }
    }
}
