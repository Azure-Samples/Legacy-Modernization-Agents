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

/// <summary>
/// One area the called program receives, in USING order. <see cref="TypeName"/> is the copybook type
/// its LINKAGE record copies, or <c>null</c> when the record is written out in the program itself.
/// </summary>
public sealed record CallParameter(string CobolName, string Name, string? TypeName);

public sealed record CallTargetContract(
    string Target,
    string InterfaceName,
    string MethodName,
    string DeclaredBy,
    IReadOnlyList<string> Callers)
{
    /// <summary>Known only when the called program is in the source; empty otherwise.</summary>
    public IReadOnlyList<CallParameter> Parameters { get; init; } = [];

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

    /// <summary>Source files that could not be read, so their CALL statements were not seen.</summary>
    public int UnreadableSources { get; private set; }

    public static CallTargetRegistry Build(string sourceFolder)
    {
        var registry = new CallTargetRegistry();
        if (!Directory.Exists(sourceFolder)) return registry;

        // Programs are what a CALL can name; copybooks are read as callers only. A copybook
        // containing a CALL becomes a converted file that would otherwise invent its own
        // interface for the callee, which is where most of the duplicate service interfaces in
        // the measured output came from.
        var programs = Read(SourceTypeRegistry.EnumerateProgramFiles(sourceFolder), registry);
        var callingFiles = programs
            .Concat(Read(SourceTypeRegistry.EnumerateCopybookFiles(sourceFolder), registry))
            .ToDictionary(e => e.Key, e => e.Value, StringComparer.OrdinalIgnoreCase);

        // Every (callee, caller) pair the source states, grouped by callee. A program calling
        // itself is dropped: recursion needs no interface, and handing a program one for itself
        // would have it declare and inject a service that is already the class being written.
        var callers = callingFiles
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
            Callers: entry.Value.ToList())
        {
            Parameters = programs.TryGetValue(entry.Key, out var target) ? UsingParameters(target) : [],
        });

        foreach (var contract in contracts)
            registry._byTarget[contract.Target] = contract;

        return registry;
    }

    private static readonly Regex ProcedureUsing = new(
        @"\bPROCEDURE\s+DIVISION\s+USING\s+(?<args>[^.]*)\.", RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex LinkageRecord = new(
        @"(?:^|\s)01\s+(?<name>[A-Za-z0-9][A-Za-z0-9-]*)\s*\.(?<body>.*?)(?=\s01\s+[A-Za-z0-9]|\z)",
        RegexOptions.IgnoreCase | RegexOptions.Singleline | RegexOptions.Compiled);

    private static readonly Regex OnlyACopy = new(
        @"^\s*COPY\s+[""']?(?<copybook>[A-Za-z0-9][A-Za-z0-9_-]*)[""']?[^.]*\.\s*$",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>
    /// The entry point's parameters: the USING list, each typed by the copybook its LINKAGE record
    /// consists of. Callers and callee both read them from here, so neither invents a signature.
    /// </summary>
    public static IReadOnlyList<CallParameter> UsingParameters(string programText)
    {
        var code = string.Join(' ', StripComments(programText).Split('\n')
            .Select(l => l.Length > 6 && l.Take(6).All(c => char.IsDigit(c) || c == ' ') ? l[7..Math.Min(l.Length, 72)] : l));
        var procedure = ProcedureUsing.Match(code);
        if (!procedure.Success) return [];

        var linkageStart = code.IndexOf("LINKAGE SECTION", StringComparison.OrdinalIgnoreCase);
        var linkage = linkageStart < 0 ? "" : code[linkageStart..procedure.Index];
        var records = LinkageRecord.Matches(linkage)
            .GroupBy(m => m.Groups["name"].Value, StringComparer.OrdinalIgnoreCase)
            .ToDictionary(g => g.Key, g => g.First().Groups["body"].Value, StringComparer.OrdinalIgnoreCase);

        return procedure.Groups["args"].Value
            .Split((char[]?)null, StringSplitOptions.RemoveEmptyEntries)
            .Where(a => !a.Equals("BY", StringComparison.OrdinalIgnoreCase)
                        && !a.Equals("REFERENCE", StringComparison.OrdinalIgnoreCase)
                        && !a.Equals("CONTENT", StringComparison.OrdinalIgnoreCase)
                        && !a.Equals("VALUE", StringComparison.OrdinalIgnoreCase))
            .TakeWhile(a => !a.Equals("RETURNING", StringComparison.OrdinalIgnoreCase))
            .Select(a =>
            {
                var copy = records.TryGetValue(a, out var body) ? OnlyACopy.Match(body) : Match.Empty;
                var pascal = ToPascalCase(a);
                return new CallParameter(a, char.ToLowerInvariant(pascal[0]) + pascal[1..],
                    copy.Success ? ToPascalCase(copy.Groups["copybook"].Value) : null);
            })
            .ToList();
    }

    private static string Signature(CallTargetContract contract, string targetLanguage)
    {
        var cs = ConversionNamespacePolicy.IsCSharp(targetLanguage);
        var parameters = contract.Parameters
            .Select(p => (p.TypeName ?? (cs ? "string" : "String")) + " " + p.Name)
            .ToList();
        if (cs) parameters.Add("CancellationToken cancellationToken = default");
        return (cs ? "Task " : "void ") + contract.MethodName + "(" + string.Join(", ", parameters) + ")";
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
                if (contract.Parameters.Count > 0)
                    declares.AppendLine($"      {Signature(contract, targetLanguage)}");
            }
            else
            {
                references.AppendLine(
                    $"  • {contract.InterfaceName}.{contract.MethodName} — declared by {contract.DeclaredBy}.");
                if (contract.Parameters.Count > 0)
                    references.AppendLine($"      {Signature(contract, targetLanguage)}");
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

    private static Dictionary<string, string> Read(
        IEnumerable<string> paths, CallTargetRegistry registry)
    {
        var map = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (var path in paths)
        {
            try
            {
                map[Stem(path)] = File.ReadAllText(path);
            }
            catch (IOException)
            {
                // A file that cannot be read contributes no CALL edges, so a callee it alone
                // calls may be assigned a different declarer than it would otherwise have been.
                // Counted rather than swallowed, because the effect is not local to this file.
                registry.UnreadableSources++;
            }
        }
        return map;
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
