// RegressionFixtureAgent.cs — Synthesises input/output golden fixtures purely
// from REKT data structures (no LLM). Produces JSON fixtures suitable for
// differential testing between the original COBOL and the converted code.
//
// For each top-level data group:
//   - emit a fixture with default values per field type (X→string, 9→numeric, etc.)
//   - one "happy path" fixture + one "edge case" fixture (max-length strings,
//     boundary numeric values)
//
// Output: list of fixture files keyed by relative path under output/fixtures/<program>/.

using System.Text.Json;

namespace CobolToQuarkusMigration.Helpers;

public sealed class RegressionFixture
{
    public string Path { get; set; } = "";
    public string Content { get; set; } = "";
}

public static class RegressionFixtureAgent
{
    /// <summary>
    /// Generate happy-path + edge-case fixtures for every top-level group in the
    /// data structure. No LLM call — fully deterministic.
    /// </summary>
    public static List<RegressionFixture> Generate(StructuralContext sc)
    {
        var result = new List<RegressionFixture>();
        var program = System.IO.Path.GetFileNameWithoutExtension(sc.Program);
        foreach (var group in sc.Context.DataStructure)
        {
            if (string.IsNullOrEmpty(group.Name)) continue;
            var happy = RenderJson(group, edgeCase: false);
            var edge  = RenderJson(group, edgeCase: true);
            result.Add(new RegressionFixture
            {
                Path = $"output/fixtures/{program}/{group.Name.ToLowerInvariant()}-happy.json",
                Content = happy,
            });
            result.Add(new RegressionFixture
            {
                Path = $"output/fixtures/{program}/{group.Name.ToLowerInvariant()}-edge.json",
                Content = edge,
            });
        }
        return result;
    }

    private static string RenderJson(RektDataItem group, bool edgeCase)
    {
        var jsonObj = BuildObject(group, edgeCase);
        return JsonSerializer.Serialize(jsonObj, new JsonSerializerOptions { WriteIndented = true });
    }

    private static object BuildObject(RektDataItem item, bool edgeCase)
    {
        if (item.Children.Count > 0)
        {
            var dict = new Dictionary<string, object?>();
            foreach (var child in item.Children)
                dict[CamelCase(child.Name)] = BuildObject(child, edgeCase);
            return dict;
        }
        return DefaultValue(item.PicClause, edgeCase);
    }

    private static object? DefaultValue(string? pic, bool edgeCase)
    {
        if (string.IsNullOrEmpty(pic)) return edgeCase ? "" : "VALUE";
        var p = pic.ToUpperInvariant();

        // PIC X(n) → string
        if (p.Contains("X"))
        {
            var len = ExtractLength(p);
            if (edgeCase) return new string('Z', Math.Min(len, 64));
            return new string('A', Math.Min(Math.Max(1, len / 2), 32));
        }

        // PIC 9(n)V9(s) → decimal
        if (p.Contains("V") && p.Contains("9"))
        {
            var (whole, frac) = ExtractDecimalDims(p);
            return edgeCase
                ? Math.Pow(10, whole) - Math.Pow(10, -frac)
                : 123.45;
        }

        // PIC 9(n) → int / long
        if (p.Contains("9"))
        {
            var len = ExtractLength(p);
            return edgeCase
                ? (long)Math.Pow(10, Math.Min(len, 18)) - 1
                : len switch { <= 4 => 42, <= 9 => 12345, _ => 9876543210L };
        }

        return null;
    }

    private static int ExtractLength(string pic)
    {
        var open = pic.IndexOf('(');
        var close = pic.IndexOf(')');
        if (open >= 0 && close > open && int.TryParse(pic.Substring(open + 1, close - open - 1), out var n))
            return n;
        // Fallback: count Xs / 9s.
        return Math.Max(1, pic.Count(c => c is 'X' or '9'));
    }

    private static (int whole, int frac) ExtractDecimalDims(string pic)
    {
        // Crude: split on V, count 9s on each side.
        var idx = pic.IndexOf('V');
        if (idx < 0) return (ExtractLength(pic), 0);
        var leftPart = pic.Substring(0, idx);
        var rightPart = pic.Substring(idx + 1);
        var leftLen = leftPart.Count(c => c == '9');
        var rightLen = rightPart.Count(c => c == '9');
        if (leftLen == 0) leftLen = ExtractLength(leftPart);
        if (rightLen == 0) rightLen = ExtractLength(rightPart);
        return (leftLen, rightLen);
    }

    private static string CamelCase(string s)
    {
        if (string.IsNullOrEmpty(s)) return s;
        var parts = s.Split(new[] { '-', '_', ' ' }, StringSplitOptions.RemoveEmptyEntries);
        if (parts.Length == 0) return s.ToLowerInvariant();
        var sb = new System.Text.StringBuilder(parts[0].ToLowerInvariant());
        for (var i = 1; i < parts.Length; i++)
        {
            var w = parts[i].ToLowerInvariant();
            sb.Append(char.ToUpperInvariant(w[0])).Append(w.AsSpan(1));
        }
        return sb.ToString();
    }
}
