namespace CobolToQuarkusMigration.Jcl;

// One JCL statement with its continuations joined, and the in-stream data it opened, if any.
public sealed record JclStatement(
    int Line,
    string? Name,
    string Operation,
    string Operands,
    IReadOnlyList<string>? InStream = null,
    bool Implicit = false);

// Splits JCL text into statements. Columns 73-80 are sequence numbers and are dropped. Comments,
// JES control statements and the null statement produce nothing.
public static class JclLexer
{
    private const int StatementColumns = 71;
    private const int DataColumns = 72;

    // A continued quoted string resumes in column 16.
    private const int QuotedContinuationColumn = 15;

    public static IReadOnlyList<JclStatement> Lex(string text)
    {
        var lines = text.Replace("\r\n", "\n").Replace('\r', '\n').Split('\n');
        var statements = new List<JclStatement>();
        var i = 0;

        while (i < lines.Length)
        {
            var line = lines[i];
            if (IsComment(line) || line.StartsWith("/*", StringComparison.Ordinal) || IsNullStatement(line))
            {
                i++;
                continue;
            }

            if (!line.StartsWith("//", StringComparison.Ordinal))
            {
                // Data with no DD * before it is read as SYSIN, as JES does.
                if (line.Trim().Length == 0) { i++; continue; }
                var start = i;
                var data = ReadInStream(lines, ref i, delimiter: null, isData: false);
                statements.Add(new JclStatement(start + 1, "SYSIN", "DD", "*", data, Implicit: true));
                continue;
            }

            var lineNumber = i + 1;
            var text0 = StatementText(line);
            var (name, operation, operands, open) = Fields(text0);
            i++;

            if (operation.Equals("IF", StringComparison.OrdinalIgnoreCase))
            {
                operands = IfCondition(text0);
            }
            else
            {
                while (open != Continuation.None && i < lines.Length && IsContinuation(lines[i]))
                {
                    var next = StatementText(lines[i]);
                    string more;
                    if (open == Continuation.Quoted)
                    {
                        more = next.Length > QuotedContinuationColumn ? next[QuotedContinuationColumn..] : "";
                        (more, open) = OperandField(more, quoteOpen: true);
                    }
                    else
                    {
                        (more, open) = OperandField(next[2..].TrimStart(), quoteOpen: false);
                    }

                    operands += more;
                    i++;
                }
            }

            IReadOnlyList<string>? inStream = null;
            if (operation.Equals("DD", StringComparison.OrdinalIgnoreCase))
            {
                var parsed = JclOperands.Parse(operands);
                var first = parsed.Positional.FirstOrDefault()?.ToUpperInvariant();
                if (first is "*" or "DATA")
                {
                    var delimiter = parsed.Keyword("DLM") is { } dlm ? JclOperands.Unquote(dlm) : null;
                    inStream = ReadInStream(lines, ref i, delimiter, isData: first == "DATA");
                }
            }

            statements.Add(new JclStatement(lineNumber, name, operation.ToUpperInvariant(), operands, inStream));
        }

        return statements;
    }

    private enum Continuation { None, Operand, Quoted }

    private static bool IsComment(string line) => line.StartsWith("//*", StringComparison.Ordinal);

    private static bool IsNullStatement(string line) => line.TrimEnd() == "//";

    private static bool IsContinuation(string line) =>
        line.StartsWith("//", StringComparison.Ordinal) && !IsComment(line)
        && line.Length > 2 && line[2] == ' ' && line.Trim().Length > 2;

    private static string StatementText(string line) =>
        (line.Length > StatementColumns ? line[..StatementColumns] : line).TrimEnd();

    private static (string? Name, string Operation, string Operands, Continuation Open) Fields(string text)
    {
        var nameEnd = 2;
        while (nameEnd < text.Length && text[nameEnd] != ' ') nameEnd++;
        var name = nameEnd > 2 ? text[2..nameEnd].ToUpperInvariant() : null;

        var pos = SkipBlanks(text, nameEnd);
        var opEnd = pos;
        while (opEnd < text.Length && text[opEnd] != ' ') opEnd++;
        var operation = text[pos..opEnd];

        pos = SkipBlanks(text, opEnd);
        var (operands, open) = OperandField(pos < text.Length ? text[pos..] : "", quoteOpen: false);
        return (name, operation, operands, open);
    }

    // The operand field ends at the first blank outside apostrophes; what follows is a comment.
    private static (string Field, Continuation Open) OperandField(string text, bool quoteOpen)
    {
        var inQuote = quoteOpen;
        var end = 0;
        while (end < text.Length)
        {
            var c = text[end];
            if (c == '\'') inQuote = !inQuote;
            else if (c == ' ' && !inQuote) break;
            end++;
        }

        var field = text[..end];
        if (inQuote) return (field, Continuation.Quoted);
        return (field, field.EndsWith(',') ? Continuation.Operand : Continuation.None);
    }

    // IF operands contain blanks, so they run to THEN rather than to the first blank.
    private static string IfCondition(string text)
    {
        var start = text.IndexOf(" IF ", StringComparison.OrdinalIgnoreCase);
        var body = start < 0 ? "" : text[(start + 4)..].Trim();
        var then = body.LastIndexOf("THEN", StringComparison.OrdinalIgnoreCase);
        return (then < 0 ? body : body[..then]).Trim();
    }

    private static int SkipBlanks(string text, int pos)
    {
        while (pos < text.Length && text[pos] == ' ') pos++;
        return pos;
    }

    private static List<string> ReadInStream(string[] lines, ref int i, string? delimiter, bool isData)
    {
        var data = new List<string>();
        while (i < lines.Length)
        {
            var line = lines[i];
            if (delimiter is not null)
            {
                if (line.StartsWith(delimiter, StringComparison.Ordinal)) { i++; break; }
            }
            else if (line.StartsWith("/*", StringComparison.Ordinal))
            {
                i++;
                break;
            }
            else if (!isData && line.StartsWith("//", StringComparison.Ordinal))
            {
                break;
            }

            data.Add((line.Length > DataColumns ? line[..DataColumns] : line).TrimEnd());
            i++;
        }

        // Trailing blank lines are file padding, not data.
        while (data.Count > 0 && data[^1].Length == 0) data.RemoveAt(data.Count - 1);
        return data;
    }
}
