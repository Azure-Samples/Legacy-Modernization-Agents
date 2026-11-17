using System.Globalization;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// The REKT records are hashed and compared byte-for-byte by the portal and the evidence gate,
// so they keep the exact json.dumps layout (ensure_ascii, ", "/": " separators) of the original tooling.
public static class PyJson
{
    private static readonly JsonNodeOptions NodeOptions = new() { PropertyNameCaseInsensitive = false };
    private static readonly JsonDocumentOptions DocumentOptions = new() { MaxDepth = 4096 };

    public static JsonNode? Parse(string text) => JsonNode.Parse(text, NodeOptions, DocumentOptions);

    public static JsonNode? ReadFile(string path) => Parse(File.ReadAllText(path, Encoding.UTF8));

    public static string Dumps(JsonNode? value, int? indent = null, bool sortKeys = false)
    {
        var builder = new StringBuilder();
        Write(builder, value, indent, sortKeys, 0);
        return builder.ToString();
    }

    private static void Write(StringBuilder builder, JsonNode? value, int? indent, bool sortKeys, int depth)
    {
        switch (value)
        {
            case null:
                builder.Append("null");
                return;
            case JsonObject obj:
            {
                if (obj.Count == 0) { builder.Append("{}"); return; }
                builder.Append('{');
                IEnumerable<KeyValuePair<string, JsonNode?>> items = obj;
                if (sortKeys) items = items.OrderBy(pair => pair.Key, StringComparer.Ordinal);
                var first = true;
                foreach (var (key, item) in items)
                {
                    builder.Append(first ? "" : ",");
                    first = false;
                    NewLine(builder, indent, depth + 1);
                    WriteString(builder, key);
                    builder.Append(": ");
                    Write(builder, item, indent, sortKeys, depth + 1);
                }
                NewLine(builder, indent, depth);
                builder.Append('}');
                return;
            }
            case JsonArray array:
            {
                if (array.Count == 0) { builder.Append("[]"); return; }
                builder.Append('[');
                for (var index = 0; index < array.Count; index++)
                {
                    if (index > 0) builder.Append(',');
                    NewLine(builder, indent, depth + 1);
                    Write(builder, array[index], indent, sortKeys, depth + 1);
                }
                NewLine(builder, indent, depth);
                builder.Append(']');
                return;
            }
            default:
                WriteScalar(builder, value.AsValue());
                return;
        }
    }

    private static void NewLine(StringBuilder builder, int? indent, int depth)
    {
        if (indent is { } width)
            builder.Append('\n').Append(' ', width * depth);
        else if (builder[^1] == ',')
            builder.Append(' ');
    }

    private static void WriteScalar(StringBuilder builder, JsonValue value)
    {
        if (value.TryGetValue<JsonElement>(out var element))
        {
            switch (element.ValueKind)
            {
                case JsonValueKind.String: WriteString(builder, element.GetString()!); return;
                case JsonValueKind.True: builder.Append("true"); return;
                case JsonValueKind.False: builder.Append("false"); return;
                case JsonValueKind.Null: builder.Append("null"); return;
                case JsonValueKind.Number:
                    var raw = element.GetRawText();
                    builder.Append(raw.IndexOfAny(['.', 'e', 'E']) < 0
                        ? System.Numerics.BigInteger.Parse(raw, CultureInfo.InvariantCulture).ToString(CultureInfo.InvariantCulture)
                        : Float(element.GetDouble()));
                    return;
            }
        }
        if (value.TryGetValue<string>(out var text)) { WriteString(builder, text); return; }
        if (value.TryGetValue<bool>(out var flag)) { builder.Append(flag ? "true" : "false"); return; }
        if (value.TryGetValue<double>(out var real) && !value.TryGetValue<long>(out _)) { builder.Append(Float(real)); return; }
        if (value.TryGetValue<float>(out var single) && !value.TryGetValue<long>(out _)) { builder.Append(Float(single)); return; }
        builder.Append(Convert.ToString(value.GetValue<object>(), CultureInfo.InvariantCulture));
    }

    // repr(float): shortest round-trip digits, ".0" for integral values, exponent form outside 1e-4..1e16.
    public static string Float(double value)
    {
        if (double.IsNaN(value)) return "NaN";
        if (double.IsInfinity(value)) return value > 0 ? "Infinity" : "-Infinity";
        var magnitude = Math.Abs(value);
        if (magnitude != 0 && (magnitude >= 1e16 || magnitude < 1e-4))
        {
            var text = value.ToString("E16", CultureInfo.InvariantCulture);
            var shortest = value.ToString("R", CultureInfo.InvariantCulture);
            var mantissa = shortest.Contains('E') ? shortest[..shortest.IndexOf('E')] : shortest;
            var exponent = int.Parse(text[(text.IndexOf('E') + 1)..], CultureInfo.InvariantCulture);
            if (!shortest.Contains('E'))
            {
                var digits = shortest.TrimStart('-').Replace(".", "").TrimStart('0').TrimEnd('0');
                mantissa = (value < 0 ? "-" : "") + digits[0] + (digits.Length > 1 ? "." + digits[1..] : "");
            }
            return mantissa + "e" + (exponent < 0 ? "-" : "+") + Math.Abs(exponent).ToString("00", CultureInfo.InvariantCulture);
        }
        var plain = value.ToString("R", CultureInfo.InvariantCulture);
        return plain.Contains('.') ? plain : plain + ".0";
    }

    public static void WriteString(StringBuilder builder, string text)
    {
        builder.Append('"');
        foreach (var character in text)
        {
            switch (character)
            {
                case '"': builder.Append("\\\""); break;
                case '\\': builder.Append("\\\\"); break;
                case '\n': builder.Append("\\n"); break;
                case '\r': builder.Append("\\r"); break;
                case '\t': builder.Append("\\t"); break;
                case '\b': builder.Append("\\b"); break;
                case '\f': builder.Append("\\f"); break;
                default:
                    if (character is < ' ' or > '~')
                        builder.Append("\\u").Append(((int)character).ToString("x4", CultureInfo.InvariantCulture));
                    else
                        builder.Append(character);
                    break;
            }
        }
        builder.Append('"');
    }

    public static string Quote(string text)
    {
        var builder = new StringBuilder();
        WriteString(builder, text);
        return builder.ToString();
    }

    // Python ==: dicts ignore key order, and 1 == 1.0 == True.
    public static bool PyEquals(JsonNode? left, JsonNode? right)
    {
        if (left is null || right is null)
            return left is null && right is null
                || IsNullValue(left) && IsNullValue(right);
        if (left is JsonObject a)
        {
            if (right is not JsonObject b || a.Count != b.Count) return false;
            foreach (var (key, item) in a)
                if (!b.TryGetPropertyValue(key, out var other) || !PyEquals(item, other)) return false;
            return true;
        }
        if (left is JsonArray x)
        {
            if (right is not JsonArray y || x.Count != y.Count) return false;
            for (var index = 0; index < x.Count; index++)
                if (!PyEquals(x[index], y[index])) return false;
            return true;
        }
        if (right is JsonObject or JsonArray) return false;
        var l = Scalar(left);
        var r = Scalar(right);
        if (l is string ls) return r is string rs && ls == rs;
        if (l is null) return r is null;
        if (l is double ld) return r is double rd && ld == rd;
        return false;
    }

    private static bool IsNullValue(JsonNode? node) => node is null || node is JsonValue value && Scalar(value) is null;

    // Strings stay strings; numbers and booleans compare numerically as in Python.
    public static object? Scalar(JsonNode? node)
    {
        if (node is not JsonValue value) return node;
        if (value.TryGetValue<JsonElement>(out var element))
            return element.ValueKind switch
            {
                JsonValueKind.String => element.GetString(),
                JsonValueKind.True => 1d,
                JsonValueKind.False => 0d,
                JsonValueKind.Number => element.GetDouble(),
                _ => null,
            };
        if (value.TryGetValue<string>(out var text)) return text;
        if (value.TryGetValue<bool>(out var flag)) return flag ? 1d : 0d;
        if (value.TryGetValue<double>(out var real)) return real;
        return Convert.ToDouble(value.GetValue<object>(), CultureInfo.InvariantCulture);
    }

    public static string? Str(JsonNode? node) =>
        node is JsonValue value && value.GetValueKind() == JsonValueKind.String ? value.GetValue<string>() : null;

    public static bool IsStr(JsonNode? node) => Str(node) is not null;

    public static bool IsBool(JsonNode? node) =>
        node is JsonValue value && value.GetValueKind() is JsonValueKind.True or JsonValueKind.False;

    public static bool IsTrue(JsonNode? node) => node is JsonValue value && value.GetValueKind() == JsonValueKind.True;

    // type(x) is int: an integral JSON number written without a fraction or exponent; bool excluded.
    public static bool IsInt(JsonNode? node, out long result)
    {
        result = 0;
        if (node is not JsonValue value || value.GetValueKind() != JsonValueKind.Number) return false;
        if (value.TryGetValue<JsonElement>(out var element))
            return element.GetRawText().IndexOfAny(['.', 'e', 'E']) < 0 && element.TryGetInt64(out result);
        if (value.TryGetValue<long>(out result)) return true;
        if (value.TryGetValue<int>(out var small)) { result = small; return true; }
        return false;
    }

    public static bool IsInt(JsonNode? node) => IsInt(node, out _);

    // Python truthiness of a decoded JSON value.
    public static bool Truthy(JsonNode? node) => node switch
    {
        null => false,
        JsonObject obj => obj.Count > 0,
        JsonArray array => array.Count > 0,
        _ => Scalar(node) switch { string text => text.Length > 0, double number => number != 0, _ => false },
    };

    // str() of a decoded JSON value as Python would print it inside an f-string.
    public static string PyStr(JsonNode? node)
    {
        if (node is null) return "None";
        if (node is JsonValue value)
        {
            switch (value.GetValueKind())
            {
                case JsonValueKind.String: return value.GetValue<string>();
                case JsonValueKind.True: return "True";
                case JsonValueKind.False: return "False";
                case JsonValueKind.Null: return "None";
                case JsonValueKind.Number:
                    return IsInt(node, out var whole) ? whole.ToString(CultureInfo.InvariantCulture)
                        : Float(Convert.ToDouble(Scalar(node), CultureInfo.InvariantCulture));
            }
        }
        return Repr(node);
    }

    // repr() of decoded JSON: single-quoted strings, True/False/None.
    public static string Repr(JsonNode? node)
    {
        switch (node)
        {
            case null: return "None";
            case JsonObject obj:
                return "{" + string.Join(", ", obj.Select(pair => ReprString(pair.Key) + ": " + Repr(pair.Value))) + "}";
            case JsonArray array:
                return "[" + string.Join(", ", array.Select(Repr)) + "]";
            default:
                return Str(node) is { } text ? ReprString(text) : PyStr(node);
        }
    }

    public static string ReprString(string text)
    {
        var quote = text.Contains('\'') && !text.Contains('"') ? '"' : '\'';
        var builder = new StringBuilder().Append(quote);
        foreach (var character in text)
        {
            if (character == quote || character == '\\') builder.Append('\\').Append(character);
            else if (character == '\n') builder.Append("\\n");
            else if (character == '\r') builder.Append("\\r");
            else if (character == '\t') builder.Append("\\t");
            else if (character < ' ' || character == '\x7f')
                builder.Append("\\x").Append(((int)character).ToString("x2", CultureInfo.InvariantCulture));
            else builder.Append(character);
        }
        return builder.Append(quote).ToString();
    }

    public static JsonNode? Clone(JsonNode? node) => node?.DeepClone();
}
