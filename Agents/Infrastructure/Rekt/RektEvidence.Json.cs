using System.Collections.Concurrent;
using System.Globalization;
using System.Text;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Accessors that read the native parser's JSON the way the evidence rules were written: a missing key or a
// value of the wrong shape simply fails the check instead of throwing.
public static partial class RektEvidence
{
    private static readonly ConcurrentDictionary<(string, bool), Regex> Patterns = new();

    internal static Regex Rx(string pattern, bool ignoreCase = false) =>
        Patterns.GetOrAdd((pattern, ignoreCase), key => new Regex(key.Item1,
            RegexOptions.CultureInvariant | (key.Item2 ? RegexOptions.IgnoreCase : RegexOptions.None)));

    // re.fullmatch: the whole input must match.
    internal static Match? Full(string pattern, string? input, bool ignoreCase = false)
    {
        if (input is null) return null;
        var match = Rx(@"\A(?:" + pattern + @")\z", ignoreCase).Match(input);
        return match.Success ? match : null;
    }

    // re.match: anchored at the start only.
    internal static Match? Start(string pattern, string? input, bool ignoreCase = false)
    {
        if (input is null) return null;
        var match = Rx(@"\A(?:" + pattern + ")", ignoreCase).Match(input);
        return match.Success ? match : null;
    }

    internal static bool Search(string pattern, string input, bool ignoreCase = false) => Rx(pattern, ignoreCase).IsMatch(input);

    internal static int Int(Match match, int group) => int.Parse(match.Groups[group].Value, CultureInfo.InvariantCulture);

    internal static string? Group(Match match, int group) => match.Groups[group].Success ? match.Groups[group].Value : null;

    internal static JsonNode? Get(JsonNode? node, string key) =>
        node is JsonObject obj && obj.TryGetPropertyValue(key, out var value) ? value : null;

    internal static bool Has(JsonNode? node, string key) => node is JsonObject obj && obj.ContainsKey(key);

    internal static string? Str(JsonNode? node, string key) => PyJson.Str(Get(node, key));

    internal static JsonArray? Arr(JsonNode? node, string key) => Get(node, key) as JsonArray;

    internal static JsonObject? Obj(JsonNode? node, string key) => Get(node, key) as JsonObject;

    // dict.get(key, []) iterated: absent and null both give nothing.
    internal static IEnumerable<JsonNode?> Items(JsonNode? node, string key) => Arr(node, key) ?? [];

    internal static bool KeysAre(JsonNode? node, params string[] keys) =>
        node is JsonObject obj && obj.Count == keys.Distinct().Count() && keys.All(obj.ContainsKey);

    internal static bool KeysWithin(JsonNode? node, params string[] keys) =>
        node is JsonObject obj && obj.All(pair => keys.Contains(pair.Key));

    internal static bool IsInt(JsonNode? node, out long value) => PyJson.IsInt(node, out value);

    internal static bool StrEquals(JsonNode? node, string? text) =>
        text is null ? node is null || PyJson.Scalar(node) is null : PyJson.Str(node) == text;

    internal static bool IsHex64(JsonNode? node) => Full("[0-9a-f]{64}", PyJson.PyStr(node)) is not null;

    // Exports come from the Linux parser container, so /source/... is absolute on a Windows host too.
    internal static bool IsAbsolute(string? path) => path is not null && (path.StartsWith('/') || Path.IsPathFullyQualified(path));

    internal static string PyKey(JsonNode? node) => node is JsonValue && PyJson.Str(node) is { } text ? "s:" + text
        : node is null || PyJson.Scalar(node) is null ? "n" : "o:" + PyJson.Dumps(node, sortKeys: true);

    internal static bool AnyTruthy(IEnumerable<JsonNode?> items) => items.Any(PyJson.Truthy);

    internal static JsonArray ToArray(IEnumerable<string?> values) => new(values.Select(value => (JsonNode?)value).ToArray());

    public static List<JsonObject> SyntaxNodes(JsonNode? syntax)
    {
        if (syntax is not JsonObject obj) return [];
        if (obj.Count == 1 && obj.ContainsKey("token"))
            return PyJson.IsStr(obj["token"]) ? [obj] : [];
        if (!KeysAre(obj, "rule", "children") || !PyJson.IsStr(obj["rule"]) || obj["children"] is not JsonArray children)
            return [];
        var nodes = new List<JsonObject> { obj };
        foreach (var child in children)
        {
            var descendants = SyntaxNodes(child);
            if (descendants.Count == 0) return [];
            nodes.AddRange(descendants);
        }
        return nodes;
    }

    internal static List<string> Tokens(IEnumerable<JsonObject> nodes) =>
        nodes.Select(node => PyJson.Str(Get(node, "token"))).OfType<string>().ToList();

    internal static string SyntaxText(JsonNode? syntax)
    {
        var builder = new StringBuilder();
        foreach (var node in SyntaxNodes(syntax))
            if (node.ContainsKey("token")) builder.Append(PyJson.Str(node["token"]));
        return builder.ToString();
    }

    private sealed class Rejected : Exception;

    private static string RStrip(string text, char character = ' ') => text.TrimEnd(character);
}
