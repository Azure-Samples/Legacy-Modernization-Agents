using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Jcl.Generation;

public enum JclCompare { Gt, Lt, Eq, Ne, Ge, Le }

public abstract record JclExpr;

public sealed record JclNot(JclExpr Operand) : JclExpr;

public sealed record JclLogical(JclExpr Left, bool IsAnd, JclExpr Right) : JclExpr;

// Step is null for the unqualified keyword, which covers every step run so far.
public sealed record JclReturnCodeTest(string? Step, JclCompare Op, int Value) : JclExpr;

public sealed record JclAbendCodeTest(string? Step, JclCompare Op, string Code) : JclExpr;

public sealed record JclAbendTest(string? Step) : JclExpr;

public sealed record JclRunTest(string Step) : JclExpr;

// One COND test: the step is bypassed when Code Op (the step's return code) is true.
public sealed record JclCondTest(int Code, JclCompare Op, string? Step);

public sealed record JclCondParameter(IReadOnlyList<JclCondTest> Tests, bool Even, bool Only);

// Reads IF relational expressions and COND parameters into a form each target renders.
public static class JclConditions
{
    private static readonly Regex Token = new(
        @"\s*(?:(?<op>¬=|\^=|!=|¬>|\^>|!>|¬<|\^<|!<|>=|<=|[><=¬^!&|()])|(?<word>[A-Z0-9$#@.]+))",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Dictionary<string, JclCompare> Comparisons = new(StringComparer.OrdinalIgnoreCase)
    {
        [">"] = JclCompare.Gt, ["GT"] = JclCompare.Gt,
        ["<"] = JclCompare.Lt, ["LT"] = JclCompare.Lt,
        ["="] = JclCompare.Eq, ["EQ"] = JclCompare.Eq,
        ["¬="] = JclCompare.Ne, ["^="] = JclCompare.Ne, ["!="] = JclCompare.Ne, ["NE"] = JclCompare.Ne,
        [">="] = JclCompare.Ge, ["GE"] = JclCompare.Ge, ["¬<"] = JclCompare.Ge, ["^<"] = JclCompare.Ge, ["!<"] = JclCompare.Ge, ["NL"] = JclCompare.Ge,
        ["<="] = JclCompare.Le, ["LE"] = JclCompare.Le, ["¬>"] = JclCompare.Le, ["^>"] = JclCompare.Le, ["!>"] = JclCompare.Le, ["NG"] = JclCompare.Le,
    };

    // AND and OR share one precedence and are read left to right, as z/OS evaluates them.
    public static JclExpr? ParseExpression(string text, out string? error)
    {
        var tokens = new List<string>();
        var position = 0;
        while (position < text.Length)
        {
            if (char.IsWhiteSpace(text[position])) { position++; continue; }
            var match = Token.Match(text, position);
            if (!match.Success || match.Index != position)
            {
                error = $"Unexpected text at '{text[position..]}'.";
                return null;
            }
            tokens.Add(match.Groups["op"].Success ? match.Groups["op"].Value : match.Groups["word"].Value.ToUpperInvariant());
            position += match.Length;
        }

        var reader = new Reader(tokens);
        try
        {
            var expression = reader.Expression();
            if (!reader.AtEnd) throw new FormatException($"Unexpected '{reader.Peek}'.");
            error = null;
            return expression;
        }
        catch (FormatException ex)
        {
            error = ex.Message;
            return null;
        }
    }

    public static JclCondParameter? ParseCond(string? text, out string? error)
    {
        error = null;
        if (string.IsNullOrWhiteSpace(text)) return null;

        var items = JclOperands.Sublist(text);
        // (4,LT) is one test; ((4,LT),(8,GT,S1)) and ((4,LT),EVEN) are lists.
        if (items.Count > 0 && !items[0].StartsWith('(') && items[0] is not ("EVEN" or "ONLY"))
            items = [text.Trim()];

        var tests = new List<JclCondTest>();
        bool even = false, only = false;
        foreach (var item in items)
        {
            var upper = item.Trim().ToUpperInvariant();
            if (upper == "EVEN") { even = true; continue; }
            if (upper == "ONLY") { only = true; continue; }

            var parts = JclOperands.Sublist(upper);
            if (parts.Count is < 2 or > 3
                || !int.TryParse(parts[0], out var code)
                || !Comparisons.TryGetValue(parts[1], out var op)
                || parts[1].Length != 2)
            {
                error = $"COND test '{item}' is not (code,operator[,step]).";
                return null;
            }
            tests.Add(new JclCondTest(code, op, parts.Count == 3 ? parts[2] : null));
        }

        return new JclCondParameter(tests, even, only);
    }

    // After an abend, a step inside IF runs only when its expression asks about an abend.
    public static bool TestsAbend(JclExpr expression) => expression switch
    {
        JclAbendTest or JclAbendCodeTest => true,
        JclNot not => TestsAbend(not.Operand),
        JclLogical logical => TestsAbend(logical.Left) || TestsAbend(logical.Right),
        _ => false,
    };

    public static IEnumerable<string> StepReferences(JclExpr expression) => expression switch
    {
        JclReturnCodeTest { Step: { } s } => [s],
        JclAbendCodeTest { Step: { } s } => [s],
        JclAbendTest { Step: { } s } => [s],
        JclRunTest r => [r.Step],
        JclNot not => StepReferences(not.Operand),
        JclLogical logical => StepReferences(logical.Left).Concat(StepReferences(logical.Right)),
        _ => [],
    };

    private sealed class Reader(List<string> tokens)
    {
        private int _index;

        public bool AtEnd => _index >= tokens.Count;
        public string? Peek => AtEnd ? null : tokens[_index];

        private string Next() => AtEnd ? throw new FormatException("The expression ends too early.") : tokens[_index++];

        public JclExpr Expression()
        {
            var left = Primary();
            while (Peek is "&" or "|" or "AND" or "OR")
            {
                var isAnd = Next() is "&" or "AND";
                left = new JclLogical(left, isAnd, Primary());
            }
            return left;
        }

        private JclExpr Primary()
        {
            var token = Next();
            if (token is "¬" or "^" or "!" or "NOT") return new JclNot(Primary());
            if (token == "(")
            {
                var inner = Expression();
                if (Next() != ")") throw new FormatException("Missing ')'.");
                return inner;
            }

            var parts = token.Split('.');
            var keyword = parts[^1];
            var step = parts.Length > 1 ? string.Join('.', parts[..^1]) : null;
            switch (keyword)
            {
                case "RC":
                    return new JclReturnCodeTest(step, Comparison(), Number());
                case "ABENDCC":
                    return new JclAbendCodeTest(step, Comparison(), Next());
                case "ABEND":
                    return Truth(new JclAbendTest(step));
                case "RUN":
                    return Truth(new JclRunTest(step ?? throw new FormatException("RUN needs a step name.")));
                default:
                    throw new FormatException($"'{token}' is not RC, ABEND, ABENDCC or RUN.");
            }
        }

        private JclCompare Comparison()
        {
            var token = Next();
            return Comparisons.TryGetValue(token, out var op) ? op : throw new FormatException($"'{token}' is not a comparison.");
        }

        private int Number()
        {
            var token = Next();
            return int.TryParse(token, out var value) ? value : throw new FormatException($"'{token}' is not a number.");
        }

        private JclExpr Truth(JclExpr test)
        {
            if (Peek is not ("=" or "EQ" or "¬=" or "^=" or "!=" or "NE")) return test;
            var negated = Next() is not ("=" or "EQ");
            var value = Next();
            if (value is not ("TRUE" or "FALSE")) throw new FormatException($"'{value}' is not TRUE or FALSE.");
            return negated ^ (value == "FALSE") ? new JclNot(test) : test;
        }
    }
}
