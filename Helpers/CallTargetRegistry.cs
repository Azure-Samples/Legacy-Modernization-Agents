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
        var copybooks = Read(SourceTypeRegistry.EnumerateCopybookFiles(sourceFolder), registry);
        var callingFiles = programs
            .Concat(copybooks)
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
        var recordTypes = RecordTypes(copybooks);
        var contracts = callers.Select(entry => new CallTargetContract(
            Target: entry.Key,
            InterfaceName: "I" + ToPascalCase(entry.Key) + "Service",
            MethodName: EntryPointMethod,
            DeclaredBy: programs.ContainsKey(entry.Key) ? entry.Key : entry.Value.First(),
            Callers: entry.Value.ToList())
        {
            Parameters = programs.TryGetValue(entry.Key, out var target)
                ? UsingParameters(target)
                : AgreedCallSiteParameters(entry.Key, entry.Value.Select(c => callingFiles[c]), recordTypes),
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
        var code = CodeArea(programText);
        var procedure = ProcedureUsing.Match(code);
        if (!procedure.Success) return [];

        var linkageStart = code.IndexOf("LINKAGE SECTION", StringComparison.OrdinalIgnoreCase);
        var linkage = linkageStart < 0 ? "" : code[linkageStart..procedure.Index];
        var records = RecordsThatAreOnlyACopy(linkage);

        return Arguments(procedure.Groups["args"].Value)
            .Select(a => Parameter(a, records.GetValueOrDefault(a)))
            .ToList();
    }

    private static readonly Regex FirstRecord = new(
        @"^\s*01\s+(?<name>[A-Za-z0-9][A-Za-z0-9-]*)", RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly HashSet<string> EndOfCallArguments = new(StringComparer.OrdinalIgnoreCase)
    {
        "END-CALL", "ON", "NOT", "RETURNING", "MOVE", "IF", "PERFORM", "EVALUATE", "CALL", "DISPLAY",
        "COMPUTE", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "SET", "INITIALIZE", "GO", "GOBACK", "EXIT",
        "CONTINUE", "STRING", "UNSTRING", "READ", "WRITE", "OPEN", "CLOSE", "ELSE", "END-IF", "WHEN",
        "END-EVALUATE", "EXEC", "INSPECT", "ACCEPT", "STOP",
    };

    // A copybook whose first entry is an 01 record gives that record its type wherever it is copied.
    private static Dictionary<string, string> RecordTypes(IReadOnlyDictionary<string, string> copybooks)
    {
        var types = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (var (stem, text) in copybooks)
        {
            var first = CodeArea(text).TrimStart();
            var record = FirstRecord.Match(first);
            if (record.Success)
                types.TryAdd(record.Groups["name"].Value, ToPascalCase(stem));
        }
        return types;
    }

    // A callee outside the source drop states no signature, but its callers do. Asserted only when every
    // argument is a copybook record and all calls agree; a caller's own field could be of any type.
    private static IReadOnlyList<CallParameter> AgreedCallSiteParameters(
        string target, IEnumerable<string> callerTexts, IReadOnlyDictionary<string, string> recordTypes)
    {
        var callTo = new Regex(@"\bCALL\s+['""]" + Regex.Escape(target) + @"['""]\s+USING\s+(?<args>[^.]*)",
            RegexOptions.IgnoreCase);
        List<CallParameter>? agreed = null;
        foreach (var text in callerTexts)
        {
            var code = CodeArea(text);
            var local = RecordsThatAreOnlyACopy(code);
            foreach (Match call in callTo.Matches(code))
            {
                var site = Arguments(call.Groups["args"].Value)
                    .Select(a => Parameter(a, local.GetValueOrDefault(a) ?? recordTypes.GetValueOrDefault(a)))
                    .ToList();
                if (site.Count == 0 || site.Any(p => p.TypeName is null)) return [];
                if (agreed is null) agreed = site;
                else if (!agreed.Select(p => p.TypeName).SequenceEqual(site.Select(p => p.TypeName)))
                    return [];
            }
        }
        return agreed ?? [];
    }

    private static Dictionary<string, string> RecordsThatAreOnlyACopy(string code)
    {
        var records = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (Match record in LinkageRecord.Matches(code))
        {
            var copy = OnlyACopy.Match(record.Groups["body"].Value);
            if (copy.Success) records.TryAdd(record.Groups["name"].Value, ToPascalCase(copy.Groups["copybook"].Value));
        }
        return records;
    }

    private static IEnumerable<string> Arguments(string clause) => clause
        .Split((char[]?)null, StringSplitOptions.RemoveEmptyEntries)
        .TakeWhile(a => !EndOfCallArguments.Contains(a))
        .Where(a => !a.Equals("BY", StringComparison.OrdinalIgnoreCase)
                    && !a.Equals("REFERENCE", StringComparison.OrdinalIgnoreCase)
                    && !a.Equals("CONTENT", StringComparison.OrdinalIgnoreCase)
                    && !a.Equals("VALUE", StringComparison.OrdinalIgnoreCase));

    private static CallParameter Parameter(string cobolName, string? typeName)
    {
        var pascal = ToPascalCase(cobolName);
        return new CallParameter(cobolName, char.ToLowerInvariant(pascal[0]) + pascal[1..], typeName);
    }

    private static string CodeArea(string text) => string.Join(' ', StripComments(text).Split('\n')
        .Select(l => l.TrimEnd('\r'))
        .Select(l => !l.Take(6).All(c => char.IsDigit(c) || c == ' ') ? l
            : l.Length > 7 ? l[7..Math.Min(l.Length, 72)]
            : ""));

    // With the signature known, the C# interface is written with the build scaffolding rather than by a
    // model: models told only to reference an interface were observed declaring it with another shape.
    public static bool IsGenerated(CallTargetContract contract, string targetLanguage) =>
        contract.Parameters.Count > 0 && ConversionNamespacePolicy.IsCSharp(targetLanguage);

    public IReadOnlyList<CallTargetContract> GeneratedInterfaces(string targetLanguage) => _byTarget.Values
        .Where(c => IsGenerated(c, targetLanguage))
        .OrderBy(c => c.InterfaceName, StringComparer.Ordinal)
        .ToList();

    public string RenderCSharpInterfaces(string sharedNamespace)
    {
        var sb = new StringBuilder();
        sb.AppendLine("// Generated from the COBOL: one interface per called program whose parameters the source states.");
        sb.AppendLine("// Regenerated with the build scaffolding; declarations of these names elsewhere are removed.");
        sb.AppendLine("using System.Threading;");
        sb.AppendLine("using System.Threading.Tasks;");
        sb.AppendLine();
        sb.AppendLine($"namespace {sharedNamespace}");
        sb.AppendLine("{");
        foreach (var contract in GeneratedInterfaces("C#"))
        {
            sb.AppendLine($"    public interface {contract.InterfaceName}");
            sb.AppendLine("    {");
            sb.AppendLine($"        {Signature(contract, "C#")};");
            sb.AppendLine("    }");
        }
        sb.AppendLine("}");
        return sb.ToString();
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
            if (IsGenerated(contract, targetLanguage))
            {
                var role = contract.IsDeclaredBy(programStem) ? "implement it" : "inject it";
                references.AppendLine(
                    $"  • {contract.InterfaceName}.{contract.MethodName} — already generated in the shared namespace; {role}.");
                references.AppendLine($"      {Signature(contract, targetLanguage)}");
            }
            else if (contract.IsDeclaredBy(programStem))
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

    // The same registry the converter prompts were built from, located the way RektPromptInjector does.
    public static CallTargetRegistry? ForRunningRepository()
    {
        var root = new DirectoryInfo(AppContext.BaseDirectory);
        while (root != null && !File.Exists(Path.Combine(root.FullName, "doctor.sh"))) root = root.Parent;
        return root is null
            ? null
            : GetOrBuild(root.FullName, Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER") ?? "source");
    }

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
