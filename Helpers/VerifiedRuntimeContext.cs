using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

public static class VerifiedRuntimeContext
{
    public static IReadOnlyList<RuntimeImplementationRequirement> Load(string factsDirectory, string sourceRoot, string program)
    {
        if (DependencyEvidenceGate.LoadEvidence(factsDirectory, program) is null) return [];
        var sourcePath = Path.Combine(sourceRoot, SourcePathHelper.ToOsRelativePath(program));
        var source = new CobolFile { FileName = Path.GetFileName(sourcePath), FilePath = sourcePath };
        return new DependencyEvidenceGate(factsDirectory).RequireVerified([source], sourceRoot).RuntimeRequirements;
    }

    public static string Build(string factsDirectory, string sourceRoot, string program)
    {
        var requirements = Load(factsDirectory, sourceRoot, program);
        if (requirements.Count == 0) return "";
        return "\n## Source-backed runtime implementation requirements\n" +
            "These operations, bindings and supplied documented platform contracts come from the current " +
            "hash-verified native export, including supporting callees. Explicit target-port profiles " +
            "are deployment choices, not proof of the original mainframe settings. These are requirements for code you must IMPLEMENT, not " +
            "a demand for pre-existing Java/C# adapters. Preserve command options, resource layouts, " +
            "SQL, host variables, status/error paths and control effects. Use the selected target's real " +
            "libraries and configurable integration boundaries; never delete operations or replace them " +
            "with successful no-ops. Do not invent missing source contracts or deployment settings. " +
            "A runtimeMapping label is not verification. Build the generated project and repair actual " +
            "compiler diagnostics; behavioral equivalence requires separate evidence.\n" +
            (requirements.Any(requirement => requirement.Kind == "EXEC_CICS" && requirement.Operation == "RETURN")
                ? "RETURN TRANSID is a transaction continuation, not a synchronous CALL or recursive method invocation. " +
                  "Preserve the transaction route, COMMAREA, length, IMMEDIATE and response handling. " +
                  "Continuation source hashes establish the supplied destination, not a parsed callee body or a working target route.\n"
                : "") +
            (requirements.Any(requirement => requirement.Kind == "PLATFORM_COPY")
                ? "PLATFORM_COPY is an explicit symbolic replacement, not a resolved vendor copybook. " +
                  "For this validated contract only, the absence of the vendor copybook is intentional, " +
                  "not a missing business layout. " +
                  "For DFHAID, implement EIBAID/key comparisons as named input events using the recorded symbols, " +
                  "preserving every original branch and condition. The analysis projection has NO runtime " +
                  "values: never allocate or blank-initialize its symbols as ordinary storage, invent IBM " +
                  "bytes, or map a key directly to a banking action. Bind actual target input events and " +
                  "test the recorded predicates; the contract does not prove behavioral equivalence. " +
                  "For DFHBMSCA, implement the named BMS attributes on the exact bound map/mapset/field, " +
                  "including protection, autoskip, modified-data state, colour and highlighting. " +
                  "Preserve source-character literals and HIGH-VALUES as distinct recorded assignments: " +
                  "do not infer their bytes or treat a literal 'A' as a protection flag. Implement their " +
                  "original CICS/character-encoding semantics explicitly in the target adapter and test edit/protect transitions.\n"
                : "") +
            JsonSerializer.Serialize(requirements.Select(Compact)) + "\n";
    }

    // The export carries every statement and host-variable definition as a full parse tree, which
    // made this block alone exceed the model's prompt limit for larger programs. The tree's tokens
    // are its facts; the rule wrappers only restate the grammar, so the prompt gets the token text.
    // The gate and validators still read the original trees.
    private static object Compact(RuntimeImplementationRequirement requirement) => new
    {
        requirement.Program,
        requirement.Kind,
        requirement.Operation,
        Evidence = Compact(requirement.Evidence),
    };

    private static object? Compact(JsonElement element)
    {
        switch (element.ValueKind)
        {
            case JsonValueKind.Object when IsSyntaxNode(element):
                return string.Join(' ', Tokens(element));
            case JsonValueKind.Object:
                return element.EnumerateObject().ToDictionary(p => p.Name, p => Compact(p.Value));
            case JsonValueKind.Array:
                return element.EnumerateArray().Select(Compact).ToList();
            default:
                return element;
        }
    }

    private static bool IsSyntaxNode(JsonElement element) =>
        element.TryGetProperty("token", out _)
        || (element.TryGetProperty("rule", out _) && element.TryGetProperty("children", out _));

    private static IEnumerable<string> Tokens(JsonElement node)
    {
        if (node.TryGetProperty("token", out var token))
        {
            yield return token.ToString();
            yield break;
        }
        if (!node.TryGetProperty("children", out var children) || children.ValueKind != JsonValueKind.Array) yield break;
        foreach (var child in children.EnumerateArray())
            foreach (var text in Tokens(child))
                yield return text;
    }
}
