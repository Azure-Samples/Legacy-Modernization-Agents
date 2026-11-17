using System.Globalization;
using System.Text;
using System.Text.Json;

namespace CobolToQuarkusMigration.Persistence.GraphIngest;

// Graph identities and properties were defined by a Python populator; these helpers keep its
// str(), truthiness and dict.get(key, default) semantics so the same uids and values come out.
internal static class GraphJson
{
    // Parser ASTs nest far deeper than the reader's default of 64.
    private static readonly JsonDocumentOptions ReadOptions = new() { MaxDepth = 4096 };

    private static readonly UTF8Encoding Utf8 = new(encoderShouldEmitUTF8Identifier: false, throwOnInvalidBytes: false);

    public static JsonDocument Parse(string path) => JsonDocument.Parse(File.ReadAllBytes(path), ReadOptions);

    // Python's read_text decodes with universal newlines, so stored content never holds \r.
    public static string ReadSourceText(string path) =>
        Utf8.GetString(File.ReadAllBytes(path)).Replace("\r\n", "\n").Replace('\r', '\n');

    public static bool TryGet(JsonElement element, string key, out JsonElement value)
    {
        if (element.ValueKind == JsonValueKind.Object && element.TryGetProperty(key, out value))
        {
            return true;
        }
        value = default;
        return false;
    }

    public static object? Get(JsonElement element, string key, object? fallback) =>
        TryGet(element, key, out var value) ? ToGraphValue(value) : fallback;

    // dict.get(key) for a value that is then tested for truthiness or used as text.
    public static JsonElement? GetOrNull(JsonElement element, string key) =>
        TryGet(element, key, out var value) && value.ValueKind != JsonValueKind.Null ? value : null;

    public static string? GetString(JsonElement element, string key) =>
        GetOrNull(element, key) is { } value ? Str(value) : null;

    public static IEnumerable<JsonElement> Items(JsonElement element, string key) =>
        TryGet(element, key, out var value) && value.ValueKind == JsonValueKind.Array
            ? value.EnumerateArray()
            : [];

    public static object? ToGraphValue(JsonElement value) => value.ValueKind switch
    {
        JsonValueKind.String => value.GetString(),
        JsonValueKind.Number => value.TryGetInt64(out var integer) ? (object)integer : value.GetDouble(),
        JsonValueKind.True => true,
        JsonValueKind.False => false,
        JsonValueKind.Array => value.EnumerateArray().Select(ToGraphValue).ToList(),
        JsonValueKind.Object => value.EnumerateObject().ToDictionary(p => p.Name, p => ToGraphValue(p.Value)),
        _ => null,
    };

    public static string Str(JsonElement value) => value.ValueKind switch
    {
        JsonValueKind.String => value.GetString()!,
        JsonValueKind.True => "True",
        JsonValueKind.False => "False",
        JsonValueKind.Null or JsonValueKind.Undefined => "None",
        _ => value.GetRawText(),
    };

    public static string Str(object? value) => value switch
    {
        null => "None",
        bool b => b ? "True" : "False",
        double d => PythonFloat(d),
        JsonElement element => Str(element),
        _ => Convert.ToString(value, CultureInfo.InvariantCulture)!,
    };

    // repr(float): integral values keep a trailing .0.
    private static string PythonFloat(double value) =>
        double.IsInteger(value) && Math.Abs(value) < 1e16
            ? value.ToString("F1", CultureInfo.InvariantCulture)
            : value.ToString("R", CultureInfo.InvariantCulture);

    public static bool Truthy(JsonElement? value) => value is { } v && v.ValueKind switch
    {
        JsonValueKind.String => v.GetString()!.Length > 0,
        JsonValueKind.Number => v.GetDouble() != 0,
        JsonValueKind.True => true,
        JsonValueKind.Array => v.GetArrayLength() > 0,
        JsonValueKind.Object => v.EnumerateObject().Any(),
        _ => false,
    };

    // Python's len() counts code points, not UTF-16 units.
    public static int CodePointLength(string text) => text.Length - text.Count(char.IsLowSurrogate);
}
