// ChunkDeduplicator.cs — post-processor that removes duplicate method
// definitions and dangling partial lines produced when chunk-aware converters
// process overlapping sections of the same COBOL program.
//
// Root cause: chunks overlap by 300 lines (OverlapLines setting) so the LLM
// converts the same COBOL section in two adjacent chunks. The chunk assembler
// concatenates them, producing duplicate methods and — at chunk boundaries —
// truncated statements that dangle outside any method body.
//
// Strategy:
//   1. Parse method signatures (name + param types) from the merged output.
//   2. Keep the FIRST occurrence of each signature; remove subsequent duplicates.
//   3. Remove lines that appear before the first method or after the last closing
//      brace and don't look like valid class-level declarations (field, property,
//      using, namespace, class, comment, attribute).

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using System.Text.RegularExpressions;

public static class ChunkDeduplicator
{
    // Matches C# method declarations: access modifier + return type + name + (
    private static readonly Regex MethodDeclRx = new(
        @"^\s+(private|public|protected|internal)\s+" +
        @"(?:static\s+|async\s+|override\s+|virtual\s+|new\s+)*" +
        @"[\w<>\[\]?,\s]+\s+" +       // return type
        @"([\w]+)\s*\(",               // method name
        RegexOptions.Compiled);

    // Lines that are valid at class level (fields, properties, constants, comments, attributes).
    private static readonly Regex ClassLevelRx = new(
        @"^\s*(//|///|/\*|\*|#|" +
        @"\[|" +                       // attributes
        @"(private|public|protected|internal)\s+" +
        @"(static\s+|readonly\s+|const\s+|override\s+|virtual\s+|new\s+)*" +
        @"[\w<>\[\]?,\s]+\s+[\w]+\s*(=|;|\{))", // field/property/const
        RegexOptions.Compiled);

    /// <summary>
    /// Remove duplicate methods and dangling partial lines from a merged
    /// chunk output. Safe to call on non-chunked output (no-op when no
    /// duplicates are found).
    /// </summary>
    public static string Deduplicate(string code, string? language = null)
    {
        if (string.IsNullOrWhiteSpace(code)) return code;

        var lines = code.Split('\n');
        var result = new List<string>(lines.Length);
        var seenMethods = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var inDuplicateMethod = false;
        var braceDepth = 0;
        var removedCount = 0;
        var danglingRemoved = 0;
        bool insideAnyMethod = false;

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            var trimmed = line.TrimStart();

            // Detect method declaration
            var methodMatch = MethodDeclRx.Match(line);
            if (methodMatch.Success)
            {
                insideAnyMethod = true;
                var methodName = methodMatch.Groups[2].Value;
                // Build a signature key: name + rough param shape
                var sigKey = methodName;
                var parenStart = line.IndexOf('(', methodMatch.Index);
                if (parenStart >= 0)
                {
                    var parenEnd = line.IndexOf(')', parenStart);
                    if (parenEnd > parenStart)
                    {
                        var paramBlock = line.Substring(parenStart + 1, parenEnd - parenStart - 1).Trim();
                        // Normalize: strip names, keep types
                        var paramTypes = Regex.Replace(paramBlock, @"\b\w+\s*(?=[,)]|$)", "").Trim();
                        sigKey = $"{methodName}({paramTypes})";
                    }
                }

                if (seenMethods.Contains(sigKey))
                {
                    // Duplicate — skip until the method's closing brace
                    inDuplicateMethod = true;
                    braceDepth = 0;
                    removedCount++;
                    // Count opening braces on this line
                    braceDepth += line.Count(c => c == '{') - line.Count(c => c == '}');
                    continue;
                }
                seenMethods.Add(sigKey);
            }

            // If we're inside a duplicate method, skip lines until braces balance
            if (inDuplicateMethod)
            {
                braceDepth += line.Count(c => c == '{') - line.Count(c => c == '}');
                if (braceDepth <= 0)
                    inDuplicateMethod = false;
                continue;
            }

            // Dangling line detection: code that sits outside any method and
            // doesn't look like a valid class-level declaration. These are
            // chunk-boundary artefacts (truncated statements from the previous
            // chunk's last line).
            if (!insideAnyMethod && i > 0 && trimmed.Length > 0)
            {
                // Allow: using, namespace, class, comment, attribute, empty, brace
                if (!trimmed.StartsWith("using ") &&
                    !trimmed.StartsWith("namespace ") &&
                    !trimmed.StartsWith("public ") &&
                    !trimmed.StartsWith("private ") &&
                    !trimmed.StartsWith("protected ") &&
                    !trimmed.StartsWith("internal ") &&
                    !trimmed.StartsWith("//") &&
                    !trimmed.StartsWith("///") &&
                    !trimmed.StartsWith("/*") &&
                    !trimmed.StartsWith("*") &&
                    !trimmed.StartsWith("[") &&
                    !trimmed.StartsWith("{") &&
                    !trimmed.StartsWith("}") &&
                    !trimmed.StartsWith("#") &&
                    !trimmed.StartsWith("static ") &&
                    !trimmed.StartsWith("const ") &&
                    trimmed != "")
                {
                    // Likely a dangling partial statement — remove it
                    danglingRemoved++;
                    continue;
                }
            }

            result.Add(line);
        }

        // Second pass: remove orphan brace pairs (empty `{ }` blocks) left
        // behind when a chunk boundary cut a method signature off and we
        // removed the dangling first half but left the braces.
        var cleaned = new List<string>(result.Count);
        for (int j = 0; j < result.Count; j++)
        {
            var t = result[j].TrimStart();
            // Remove a lone `{` followed immediately by a lone `}`
            if (t == "{" && j + 1 < result.Count && result[j + 1].TrimStart() == "}")
            {
                j++; // skip both
                danglingRemoved += 2;
                continue;
            }
            // Remove lone `{` that sits directly under another `{` or at class level
            // with nothing between them except whitespace
            cleaned.Add(result[j]);
        }

        if (removedCount > 0 || danglingRemoved > 0)
        {
            Console.WriteLine($"  🧹 ChunkDeduplicator: removed {removedCount} duplicate method(s) and {danglingRemoved} dangling line(s)");
        }

        return string.Join('\n', cleaned);
    }
}
