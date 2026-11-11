using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Text.Json;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed partial class DependencyEvidenceGate
{
    internal static IReadOnlyList<string> ReadPlatformProjectionNames(string path, string identity) =>
        ReadExportTree(path, identity, requireIdentity: false).RuntimeRequirements
            .Where(requirement => requirement.Program == identity && requirement.Kind == "PLATFORM_COPY")
            .Select(requirement => requirement.Operation).ToArray();

    private static readonly IReadOnlyDictionary<string, string> TerminalEvents =
        new[] { ("DFHENTER", "ENTER"), ("DFHCLEAR", "CLEAR") }
            .Concat(Enumerable.Range(1, 3).Select(index => ($"DFHPA{index}", $"PA{index}")))
            .Concat(Enumerable.Range(1, 24).Select(index => ($"DFHPF{index}", $"PF{index}")))
            .ToDictionary(pair => pair.Item1, pair => pair.Item2, StringComparer.Ordinal);

    private static bool Keys(JsonElement value, params string[] keys) =>
        value.ValueKind == JsonValueKind.Object
        && value.EnumerateObject().Select(property => property.Name).Order(StringComparer.Ordinal)
            .SequenceEqual(keys.Order(StringComparer.Ordinal));

    private static bool ValidTerminalDefinition(JsonElement definition) =>
        Keys(definition, "recipe", "selector", "symbols", "documentation", "targetSemantics")
        && Text(definition, "recipe") == "symbolic-terminal-events-v1"
        && Text(definition, "selector") == "EIBAID"
        && Text(definition, "targetSemantics") == "named-events-preserve-source-branches"
        && definition.TryGetProperty("symbols", out var symbols)
        && Keys(symbols, TerminalEvents.Keys.ToArray())
        && TerminalEvents.All(pair => Text(symbols, pair.Key) == pair.Value)
        && definition.TryGetProperty("documentation", out var documentation)
        && documentation.ValueKind == JsonValueKind.Array && documentation.GetArrayLength() == 1
        && documentation[0].ValueKind == JsonValueKind.String
        && documentation[0].GetString() == "https://www.ibm.com/docs/en/cics-ts/6.x";

    private static byte[] TerminalProjection(string name = "DFHAID")
    {
        var text = new StringBuilder(
            "      * ANALYSIS-ONLY PLATFORM EVENT CONTRACT; NOT A VENDOR COPYBOOK\n" +
            "      * No VALUE clauses: these names are symbolic, not blank data.\n" +
            $"       01 {name}.\n");
        foreach (var symbol in (name == "DFHBMSCA" ? BmsAttributes : TerminalEvents).Keys.Order(StringComparer.Ordinal))
            text.Append("          02 ").Append(symbol).Append(" PIC X.\n");
        return Encoding.ASCII.GetBytes(text.ToString());
    }

    private static RuntimeImplementationRequirement ReadPlatformCopy(JsonElement node, string owner, string name)
    {
        if (name is not ("DFHAID" or "DFHBMSCA") || Text(node.GetProperty("callTarget"), "programReferenceType") != "STATIC"
            || !node.TryGetProperty("runtimeContractBindingComplete", out var bound) || bound.ValueKind != JsonValueKind.True
            || !node.TryGetProperty("dependencyAnalysisComplete", out var analyzed) || analyzed.ValueKind != JsonValueKind.False
            || node.TryGetProperty("sourcePath", out var body) && body.ValueKind != JsonValueKind.Null
            || node.TryGetProperty("sourceSha256", out var hash) && hash.ValueKind != JsonValueKind.Null
            || !node.TryGetProperty("dependencies", out var children)
            || children.ValueKind != JsonValueKind.Array || children.GetArrayLength() != 0
            || !node.TryGetProperty("runtimeContract", out var contract)
            || Text(contract, "name") != name || Text(contract, "profileKind") != "platform-replacement"
            || string.IsNullOrWhiteSpace(Text(contract, "profileName"))
            || Text(contract, "resolutionKind") != "symbolic-platform-replacement"
            || Text(contract, "runtimeMapping") != "implementation-required"
            || Text(contract, "sourceValues") != "not-supplied" || Text(contract, "bindingStatus") != "complete"
            || !contract.TryGetProperty("definition", out var definition)
            || !(name == "DFHBMSCA" ? ValidBmsDefinition(definition) : ValidTerminalDefinition(definition))
            || !contract.TryGetProperty("usages", out var usages) || usages.ValueKind != JsonValueKind.Array
            || (name == "DFHBMSCA" ? usages.EnumerateArray().Any(usage => !ValidBmsUsage(usage))
                : !ValidTerminalUsages(usages.EnumerateArray().ToArray())))
            throw new InvalidDataException($"invalid symbolic platform replacement for {name}; original vendor values were not supplied.");
        return new(owner, "PLATFORM_COPY", name, contract.Clone());
    }

    private static IEnumerable<string> PlatformSyntaxTokens(JsonElement syntax) =>
        syntax.TryGetProperty("token", out var token) ? [token.GetString()!]
            : syntax.GetProperty("children").EnumerateArray().SelectMany(PlatformSyntaxTokens);

    private static bool ValidTerminalUsages(JsonElement[] usages)
    {
        string? previous = null;
        for (var index = 0; index < usages.Length; index++)
        {
            var role = usages[index].TryGetProperty("conditionRole", out var value) ? value.GetString() : null;
            if (role is "evaluate-subject" or "evaluate-when")
            {
                var following = index + 1 < usages.Length && usages[index + 1].TryGetProperty("conditionRole", out var next)
                    ? next.GetString() : null;
                if (!ValidEvaluateUsage(usages[index], role)
                    || role == "evaluate-subject" && following != "evaluate-when"
                    || role == "evaluate-when" && previous is not ("evaluate-subject" or "evaluate-when"))
                    return false;
            }
            else if (!ValidTerminalUsage(usages[index]))
                return false;
            previous = role;
        }
        return true;
    }

    // EVALUATE EIBAID: one subject usage followed by one usage per WHEN <named event>.
    private static bool ValidEvaluateUsage(JsonElement usage, string role)
    {
        var rule = role == "evaluate-subject" ? "EvaluateSelectContext" : "EvaluateWhenContext";
        if (Text(usage, "grammarRule") != rule
            || Text(usage, "coordinateSpace") != "expanded-parser-input"
            || string.IsNullOrWhiteSpace(Text(usage, "sourceText"))
            || !usage.TryGetProperty("line", out var line) || !line.TryGetInt32(out var number) || number < 1
            || !usage.TryGetProperty("syntax", out var syntax) || !ValidSyntax(syntax)
            || Text(syntax, "rule") != rule)
            return false;
        var tokens = PlatformSyntaxTokens(syntax).Select(value => value.ToUpperInvariant()).ToArray();
        return role == "evaluate-subject"
            ? tokens.SequenceEqual(["EIBAID"])
            : tokens.Length == 2 && tokens[0] == "WHEN" && TerminalEvents.ContainsKey(tokens[1]);
    }

    private static bool ValidTerminalUsage(JsonElement usage)
    {
        // An EIBAID equality ANDed with application relations is recorded as its own conjunct.
        var conjunct = usage.TryGetProperty("conditionRole", out var role);
        if (conjunct && role.GetString() != "application-and-conjunct") return false;
        var rule = conjunct ? "SimpleConditionContext" : "ConditionContext";
        if (Text(usage, "grammarRule") != rule
            || Text(usage, "coordinateSpace") != "expanded-parser-input"
            || string.IsNullOrWhiteSpace(Text(usage, "sourceText"))
            || !usage.TryGetProperty("line", out var line) || !line.TryGetInt32(out var number) || number < 1
            || !usage.TryGetProperty("syntax", out var syntax) || !ValidSyntax(syntax)
            || Text(syntax, "rule") != rule || !ValidTerminalComparisons(syntax))
            return false;
        var tokens = PlatformSyntaxTokens(syntax).Select(value => value.ToUpperInvariant()).ToArray();
        if (conjunct && (tokens[0] != "EIBAID" || tokens.Count(TerminalEvents.ContainsKey) != 1)) return false;
        return tokens.Contains("EIBAID") && tokens.Any(TerminalEvents.ContainsKey)
            && tokens.Any(value => value is "=" or "EQUAL")
            && tokens.All(value => TerminalEvents.ContainsKey(value)
                || value is "EIBAID" or "=" or "NOT" or "AND" or "OR" or "IS" or "EQUAL" or "TO" or "(" or ")");
    }

    private static bool ValidTerminalComparisons(JsonElement syntax, string? parent = null)
    {
        if (syntax.TryGetProperty("token", out _)) return true;
        var rule = Text(syntax, "rule");
        if (rule == "SimpleConditionContext")
        {
            var tokens = PlatformSyntaxTokens(syntax).Select(value => value.ToUpperInvariant()).ToArray();
            if (tokens.Length == 1)
            {
                if (parent != "AdditionalConditionContext" || !TerminalEvents.ContainsKey(tokens[0])) return false;
            }
            else if (tokens.Length < 3 || tokens[0] != "EIBAID" || !TerminalEvents.ContainsKey(tokens[^1]))
                return false;
        }
        return syntax.GetProperty("children").EnumerateArray().All(child => ValidTerminalComparisons(child, rule));
    }

    // The parser records the per-estate contract snapshot by its container path (/source/...).
    // That mount is the program's own source root, which its original path minus its relative path gives.
    // Parser usages are reported in expanded-input order, so copybook bodies are spliced at their COPY statements
    // from hash-bound COPY definitions (PROCEDURE DIVISION only). REPLACING accepts only whole-word or delimited-tag pseudo-text, e.g.
    // ==(TAG)== / ==:TAG:==; library qualifiers, other operands, unbound bodies and cycles fail closed.
    private static List<SourceToken>? ExpandedContradictionTokens(string path, DependencyEvidence evidence) =>
        ExpandedContradictionTokens(File.ReadLines(path).ToList(), evidence, new HashSet<string>(StringComparer.OrdinalIgnoreCase));

    private static List<SourceToken>? ExpandedContradictionTokens(
        IReadOnlyList<string> lines, DependencyEvidence evidence, HashSet<string> active)
    {
        if (active.Count > 16) return null;
        var tokens = ContradictionTokens(lines);
        var result = new List<SourceToken>();
        // Symbol uses are procedural; DATA DIVISION copybooks contribute declarations, not uses.
        var procedure = active.Count > 0;
        for (var index = 0; index < tokens.Count; index++)
        {
            if (!tokens[index].IsLiteral && index + 1 < tokens.Count
                && tokens[index].Text.Equals("PROCEDURE", StringComparison.OrdinalIgnoreCase)
                && tokens[index + 1].Text.Equals("DIVISION", StringComparison.OrdinalIgnoreCase))
                procedure = true;
            if (!procedure || tokens[index].IsLiteral || !tokens[index].Text.Equals("COPY", StringComparison.OrdinalIgnoreCase)
                || index + 1 >= tokens.Count)
            {
                result.Add(tokens[index]);
                continue;
            }
            var name = tokens[index + 1].Text;
            var definitions = evidence.Definitions.Where(definition => definition.Kind == "COPY"
                && SameTarget(definition.Name, name)).ToList();
            if (definitions.Count == 0)
            {
                result.Add(tokens[index]);
                continue;
            }
            if (definitions.Count != 1 || !File.Exists(definitions[0].Path)
                || !string.Equals(Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(definitions[0].Path))),
                    definitions[0].Sha256, StringComparison.OrdinalIgnoreCase))
                return null;
            var end = index + 1;
            var replacements = new List<(string From, string To, bool Tag)>();
            var next = index + 2 < tokens.Count && !tokens[index + 2].IsLiteral ? tokens[index + 2].Text.ToUpperInvariant() : ".";
            if (next is "OF" or "IN" or "SUPPRESS") return null;
            if (next == "REPLACING")
            {
                end = index + 3;
                while (end < tokens.Count && tokens[end].Text != ".")
                {
                    var from = CopyOperand(tokens, ref end);
                    if (from is null || end >= tokens.Count || !tokens[end].Text.Equals("BY", StringComparison.OrdinalIgnoreCase))
                        return null;
                    end++;
                    var to = CopyOperand(tokens, ref end);
                    var tag = Regex.IsMatch(from, @"^(?:\([A-Za-z0-9-]+\)|:[A-Za-z0-9-]+:)$");
                    if (to is null || !tag && !Regex.IsMatch(from, "^[A-Za-z0-9-]+$") || !Regex.IsMatch(to, "^[A-Za-z0-9-]*$"))
                        return null;
                    replacements.Add((from, to, tag));
                }
                if (end >= tokens.Count || replacements.Count == 0
                    || replacements.Any(item => replacements.Any(other =>
                        item.To.Contains(other.From, StringComparison.OrdinalIgnoreCase))))
                    return null;
            }
            var body = File.ReadLines(definitions[0].Path).ToList();
            if (replacements.Count > 0 && (body = ReplaceCopyText(body, replacements)!) is null) return null;
            if (!active.Add(name)) return null;
            var expanded = ExpandedContradictionTokens(body, evidence, active);
            active.Remove(name);
            if (expanded is null) return null;
            result.AddRange(expanded);
            index = end;
        }
        return result;
    }

    private static string? CopyOperand(IReadOnlyList<SourceToken> tokens, ref int index)
    {
        if (index + 1 < tokens.Count && tokens[index].Text == "=" && tokens[index + 1].Text == "=" && !tokens[index].IsLiteral)
        {
            var text = new StringBuilder();
            for (index += 2; index + 1 < tokens.Count; index++)
            {
                if (tokens[index].IsLiteral) return null;
                if (tokens[index].Text == "=" && tokens[index + 1].Text == "=")
                {
                    index += 2;
                    return text.ToString();
                }
                text.Append(tokens[index].Text);
            }
            return null;
        }
        if (index >= tokens.Count || tokens[index].IsLiteral || !Regex.IsMatch(tokens[index].Text, "^[A-Za-z0-9-]+$"))
            return null;
        return tokens[index++].Text;
    }

    private static List<string>? ReplaceCopyText(IReadOnlyList<string> lines, IReadOnlyList<(string From, string To, bool Tag)> replacements)
    {
        var result = new List<string>();
        foreach (var raw in lines)
        {
            var fixedFormat = raw.Length > 6 && raw.Take(6).All(character => character is ' ' or >= '0' and <= '9');
            if (fixedFormat && raw[6] is '*' or '/')
            {
                result.Add(raw);
                continue;
            }
            var code = fixedFormat ? raw[7..Math.Min(raw.Length, 72)] : raw;
            var replaced = code;
            foreach (var (from, to, tag) in replacements)
                replaced = tag ? Regex.Replace(replaced, Regex.Escape(from), to.Replace("$", "$$"), RegexOptions.IgnoreCase)
                    : Regex.Replace(replaced, $@"(?<![A-Za-z0-9_-]){Regex.Escape(from)}(?![A-Za-z0-9_-])", to.Replace("$", "$$"),
                        RegexOptions.IgnoreCase);
            if (replaced == code)
            {
                result.Add(raw);
                continue;
            }
            // A changed line is re-emitted free-form so substitution growth is never cut at column 72.
            var free = replaced.TrimStart();
            if (fixedFormat && raw[6] != ' ' || replaced.IndexOfAny(['\'', '"']) >= 0
                || free.Length > 6 && free.Take(6).All(character => character is ' ' or >= '0' and <= '9'))
                return null;
            result.Add(free);
        }
        return result;
    }

    private static string? HostSourcePath(string? native, DependencyEvidence evidence)
    {
        const string mount = "/source/";
        if (native is null || !native.StartsWith(mount, StringComparison.Ordinal)) return native;
        var relative = evidence.RelativePath.Replace('\\', '/');
        var original = evidence.OriginalSourcePath.Replace('\\', '/');
        if (relative.Length == 0 || !original.EndsWith("/" + relative, StringComparison.Ordinal)) return native;
        var root = Path.GetFullPath(evidence.OriginalSourcePath[..^(relative.Length + 1)]);
        var host = Path.GetFullPath(Path.Join(root, native[mount.Length..].Replace('/', Path.DirectorySeparatorChar)));
        return host.StartsWith(root + Path.DirectorySeparatorChar, StringComparison.Ordinal) ? host : native;
    }

    private static void ValidatePlatformCopies(string identity, DependencyEvidence evidence,
        IReadOnlyList<RuntimeImplementationRequirement> requirements, List<string> issues)
    {
        var platform = requirements.Where(requirement => requirement.Kind == "PLATFORM_COPY").ToArray();
        foreach (var definition in evidence.Definitions.Where(definition => definition.Kind is "PLATFORM_COPY" or "PLATFORM_INPUT"))
            if (!platform.Any(requirement => requirement.Operation == definition.Name))
                issues.Add($"{identity}: unused platform replacement definition {definition.Name}.");
        foreach (var requirement in platform)
        {
            var binding = requirement.Evidence;
            var profiles = evidence.Definitions.Where(definition => definition.Kind == "PLATFORM_COPY"
                && definition.Name == requirement.Operation && definition.Sha256 == Text(binding, "sourceSha256")).ToArray();
            var inputs = evidence.Definitions.Where(definition => definition.Kind == "PLATFORM_INPUT"
                && definition.Name == requirement.Operation && definition.Sha256 == Text(binding, "outputSha256")).ToArray();
            if (profiles.Length != 1 || inputs.Length != 1
                || profiles[0].Path != HostSourcePath(Text(binding, "sourcePath"), evidence)
                || !string.Equals(inputs[0].Sha256, Convert.ToHexString(SHA256.HashData(TerminalProjection(requirement.Operation))),
                    StringComparison.OrdinalIgnoreCase))
            {
                issues.Add($"{identity}: platform replacement {requirement.Operation} lacks hash-bound profile/projection inputs.");
                continue;
            }
            using var document = JsonDocument.Parse(File.ReadAllText(profiles[0].Path));
            var profile = document.RootElement;
            if (!Keys(profile, "schemaVersion", "profileKind", "profileName", "copybooks")
                || !profile.TryGetProperty("schemaVersion", out var version) || !version.TryGetInt32(out var schema) || schema != 1
                || Text(profile, "profileKind") != "platform-replacement"
                || Text(profile, "profileName") != Text(binding, "profileName")
                || !profile.TryGetProperty("copybooks", out var contracts) || contracts.ValueKind != JsonValueKind.Object
                || !contracts.EnumerateObject().Any()
                || contracts.EnumerateObject().Any(property => property.Name switch
                {
                    "DFHAID" => !ValidTerminalDefinition(property.Value),
                    "DFHBMSCA" => !ValidBmsDefinition(property.Value),
                    _ => true
                })
                || !contracts.TryGetProperty(requirement.Operation, out var recipe)
                || !JsonElement.DeepEquals(recipe, binding.GetProperty("definition"))
                || !File.ReadAllBytes(inputs[0].Path).SequenceEqual(TerminalProjection(requirement.Operation)))
                issues.Add($"{identity}: platform replacement {requirement.Operation} differs from its explicit symbolic recipe.");

            var source = requirement.Program == identity ? evidence.OriginalSourcePath
                : evidence.Definitions.FirstOrDefault(definition => definition.Kind == "CALL"
                    && SameTarget(definition.Name, requirement.Program)
                    && !definition.Path.Contains($"{Path.DirectorySeparatorChar}.preprocessed{Path.DirectorySeparatorChar}",
                        StringComparison.Ordinal))?.Path;
            if (source is null || !File.Exists(source))
            {
                issues.Add($"{identity}: platform replacement {requirement.Operation} has no original source for {requirement.Program}.");
                continue;
            }
            if (requirement.Operation == "DFHBMSCA")
            {
                ValidateBmsAttributes(identity, evidence, binding, source, issues);
                continue;
            }
            var tokens = ExpandedContradictionTokens(source, evidence);
            if (tokens is null)
            {
                issues.Add($"{identity}: platform replacement {requirement.Operation} uses in {requirement.Program} cannot be compared against hash-bound COPY-expanded source.");
                continue;
            }
            var relevant = tokens.Select((token, index) => (token, index))
                .Where(item => !item.token.IsLiteral && (TerminalEvents.ContainsKey(item.token.Text.ToUpperInvariant())
                    || item.token.Text.Equals("EIBAID", StringComparison.OrdinalIgnoreCase)
                    || item.token.Text.Equals("DFHAID", StringComparison.OrdinalIgnoreCase)
                       && !(item.index > 0 && tokens[item.index - 1].Text.Equals("COPY", StringComparison.OrdinalIgnoreCase))))
                .Select(item => item.token.Text.ToUpperInvariant()).ToArray();
            var reported = binding.GetProperty("usages").EnumerateArray().SelectMany(usage =>
                PlatformSyntaxTokens(usage.GetProperty("syntax")).Where(token => TerminalEvents.ContainsKey(token.ToUpperInvariant())
                    || token.Equals("EIBAID", StringComparison.OrdinalIgnoreCase))).Select(token => token.ToUpperInvariant()).ToArray();
            if (!relevant.SequenceEqual(reported, StringComparer.Ordinal)
                || !PlatformPredicatesPresent(tokens, binding.GetProperty("usages")))
                issues.Add($"{identity}: platform replacement symbol uses do not cover the original {requirement.Program}; aggregate/raw-byte or omitted uses remain unresolved.");
        }
    }

    private static bool PlatformPredicatesPresent(IReadOnlyList<SourceToken> source, JsonElement usages)
    {
        var cursor = 0;
        foreach (var usage in usages.EnumerateArray())
        {
            var expected = PlatformSyntaxTokens(usage.GetProperty("syntax")).ToArray();
            var text = ContradictionTokens(Text(usage, "sourceText")!.Split('\n'));
            if (text.Any(token => token.IsLiteral)
                || !text.Select(token => token.Text).SequenceEqual(expected, StringComparer.OrdinalIgnoreCase))
                return false;
            var found = false;
            for (; cursor + expected.Length <= source.Count; cursor++)
            {
                var candidate = source.Skip(cursor).Take(expected.Length).ToArray();
                if (candidate.Any(token => token.IsLiteral)
                    || !candidate.Select(token => token.Text).SequenceEqual(expected, StringComparer.OrdinalIgnoreCase))
                    continue;
                cursor += expected.Length;
                found = true;
                break;
            }
            if (!found) return false;
        }
        return true;
    }
}
