using System.Collections.Concurrent;
using System.Text;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Python `re` patterns, flags and replacement templates in .NET. Preprocess rules written for the former
// Python preprocessor keep their meaning; anything that cannot be translated exactly is rejected.
public static class PyRegex
{
    private static readonly ConcurrentDictionary<(string, RegexOptions), Regex> Cache = new();

    // Python's \s also matches the information separators \x1c-\x1f, which .NET's does not.
    public static Regex Compile(string pattern, RegexOptions options = RegexOptions.None) =>
        Cache.GetOrAdd((pattern, options), key =>
            new Regex(TranslatePattern(key.Item1, key.Item2), key.Item2 | RegexOptions.CultureInvariant));

    public static string Sub(string text, string pattern, string replacement, RegexOptions options = RegexOptions.None) =>
        Compile(pattern, options).Replace(text, replacement);

    public static string Sub(string text, string pattern, MatchEvaluator evaluator, RegexOptions options = RegexOptions.None, int count = -1) =>
        Compile(pattern, options).Replace(text, evaluator, count);

    // re.match: only a match starting at index 0 counts; the leftmost match is found first, so this is exact.
    public static Match? MatchStart(string text, string pattern, RegexOptions options = RegexOptions.None) =>
        Compile(pattern, options).Match(text) is { Success: true, Index: 0 } match ? match : null;

    public static bool Search(string text, string pattern, RegexOptions options = RegexOptions.None) =>
        Compile(pattern, options).IsMatch(text);

    public static RegexOptions Flags(IEnumerable<string> names)
    {
        var options = RegexOptions.None;
        foreach (var name in names)
            options |= name switch
            {
                "IGNORECASE" or "I" => RegexOptions.IgnoreCase,
                "MULTILINE" or "M" => RegexOptions.Multiline,
                "DOTALL" or "S" => RegexOptions.Singleline,
                "VERBOSE" or "X" => RegexOptions.IgnorePatternWhitespace,
                "UNICODE" or "U" or "NOFLAG" => RegexOptions.None,
                _ => throw new NotSupportedException($"flag {name} has no .NET equivalent"),
            };
        return options;
    }

    public static string TranslatePattern(string pattern, RegexOptions options = RegexOptions.None)
    {
        var groups = CaptureGroups(pattern, options);
        var output = new StringBuilder();
        var inClass = false;
        var verbose = Verbose(pattern, options);
        for (var index = 0; index < pattern.Length; index++)
        {
            var character = pattern[index];
            if (character == '\\')
            {
                if (index + 1 < pattern.Length && char.IsAsciiLetter(pattern[index + 1])
                    && !"abBdDfnrsStvwWxuAZ".Contains(pattern[index + 1]))
                    throw new NotSupportedException($@"bad escape \{pattern[index + 1]}");
                if (index + 1 >= pattern.Length) throw new NotSupportedException("pattern ends with a backslash");
                var next = pattern[++index];
                if (char.IsAsciiDigit(next) && next != '0' && !inClass)
                {
                    var end = index + 1;
                    if (end < pattern.Length && char.IsAsciiDigit(pattern[end])) end++;
                    var digits = pattern[index..end];
                    if (digits.Length == 2 && end < pattern.Length && char.IsAsciiDigit(pattern[end])
                        && digits.All(digit => digit < '8') && pattern[end] < '8')
                        throw new NotSupportedException("octal escapes are not supported");
                    output.Append(@"\k<").Append(Reference(groups, int.Parse(digits))).Append('>');
                    index = end - 1;
                    continue;
                }
                output.Append(next switch
                {
                    's' => inClass ? @"\s\x1c-\x1f" : @"[\s\x1c-\x1f]",
                    'S' when !inClass => @"[^\s\x1c-\x1f]",
                    'Z' when !inClass => @"\z",
                    'S' or 'Z' or 'N' or 'U' => throw new NotSupportedException($@"escape \{next} is not supported"),
                    _ when char.IsAsciiDigit(next) => throw new NotSupportedException("octal escapes are not supported"),
                    _ => "\\" + next,
                });
                continue;
            }
            if (inClass)
            {
                if (character == ']') inClass = false;
                output.Append(character == '[' ? @"\[" : character.ToString());
                continue;
            }
            if (verbose && character == '#')
            {
                while (index < pattern.Length && pattern[index] != '\n') index++;
                output.Append('\n');
                continue;
            }
            if (character == '[')
            {
                inClass = true;
                output.Append('[');
                if (index + 1 < pattern.Length && pattern[index + 1] == '^') output.Append(pattern[++index]);
                if (index + 1 < pattern.Length && pattern[index + 1] == ']') { output.Append(@"\]"); index++; }
                continue;
            }
            if (character == '(' && index + 1 < pattern.Length && pattern[index + 1] == '?')
            {
                var rest = pattern[(index + 2)..];
                if (rest.StartsWith("P<", StringComparison.Ordinal)) { output.Append("(?<"); index += 3; continue; }
                if (rest.StartsWith("P=", StringComparison.Ordinal))
                {
                    var close = rest.IndexOf(')');
                    if (close < 0) throw new NotSupportedException("unterminated group reference");
                    output.Append(@"\k<").Append(rest[2..close]).Append('>');
                    index += 2 + close;
                    continue;
                }
                if (rest.StartsWith('P') || rest.StartsWith('<') && !rest.StartsWith("<=") && !rest.StartsWith("<!"))
                    throw new NotSupportedException("group syntax is not supported");
                var flagLength = rest.TakeWhile(flag => "aiLmsux-".Contains(flag)).Count();
                if (flagLength > 0 && flagLength < rest.Length && rest[flagLength] is ')' or ':')
                {
                    var flags = rest[..flagLength];
                    if (flags.Contains('a') || flags.Contains('L')) throw new NotSupportedException("ASCII and LOCALE flags are not supported");
                    flags = flags.Replace("u", "");
                    if (flags.Length == 0 && rest[flagLength] == ')') { index += 2 + flagLength; continue; }
                    output.Append("(?").Append(flags.Length == 0 ? "" : flags);
                    index += 1 + flagLength;
                    continue;
                }
            }
            if (character == '{' && Regex.Match(pattern[index..], @"^\{,(\d+)\}") is { Success: true } upper)
            {
                output.Append("{0,").Append(upper.Groups[1].Value).Append('}');
                index += upper.Length - 1;
                continue;
            }
            output.Append(character);
        }
        return output.ToString();
    }

    // Python numbers named and unnamed groups left to right; .NET numbers the unnamed ones first.
    // Returns the .NET reference for each Python group number (1-based).
    private static List<string> CaptureGroups(string pattern, RegexOptions options)
    {
        var groups = new List<string>();
        var unnamed = 0;
        var inClass = false;
        var verbose = Verbose(pattern, options);
        for (var index = 0; index < pattern.Length; index++)
        {
            var character = pattern[index];
            if (character == '\\') { index++; continue; }
            if (inClass) { if (character == ']') inClass = false; continue; }
            if (verbose && character == '#') { while (index < pattern.Length && pattern[index] != '\n') index++; continue; }
            if (character == '[')
            {
                inClass = true;
                if (index + 1 < pattern.Length && pattern[index + 1] == '^') index++;
                if (index + 1 < pattern.Length && pattern[index + 1] == ']') index++;
                continue;
            }
            if (character != '(') continue;
            if (index + 1 < pattern.Length && pattern[index + 1] == '?')
            {
                if (pattern.AsSpan(index + 2).StartsWith("P<"))
                {
                    var close = pattern.IndexOf('>', index);
                    if (close > 0) groups.Add(pattern[(index + 4)..close]);
                }
                continue;
            }
            groups.Add((++unnamed).ToString());
        }
        return groups;
    }

    // Python only accepts global inline flags at the start of the pattern.
    private static bool Verbose(string pattern, RegexOptions options) =>
        options.HasFlag(RegexOptions.IgnorePatternWhitespace) || Regex.IsMatch(pattern, @"^\(\?[aiLmsux]*x[aiLmsux]*\)");

    private static string Reference(List<string> groups, int number) =>
        number <= groups.Count ? groups[number - 1] : throw new NotSupportedException($"invalid group reference {number}");

    // Python replacement template (\1, \g<name>, \n, \\) as a .NET substitution with literal $ escaped.
    public static string TranslateReplacement(string template, string pattern, RegexOptions options = RegexOptions.None)
    {
        var groups = CaptureGroups(pattern, options);
        var output = new StringBuilder();
        for (var index = 0; index < template.Length; index++)
        {
            var character = template[index];
            if (character == '$') { output.Append("$$"); continue; }
            if (character != '\\') { output.Append(character); continue; }
            if (index + 1 >= template.Length) throw new NotSupportedException("replacement ends with a backslash");
            var next = template[++index];
            if (next == 'g')
            {
                var close = template.IndexOf('>', index);
                if (index + 1 >= template.Length || template[index + 1] != '<' || close < 0)
                    throw new NotSupportedException(@"malformed \g<...> reference");
                var name = template[(index + 2)..close];
                if (name.Length > 0 && name.All(char.IsAsciiDigit))
                {
                    var number = int.Parse(name);
                    output.Append("${").Append(number == 0 ? "0" : Reference(groups, number)).Append('}');
                }
                else if (groups.Contains(name)) output.Append("${").Append(name).Append('}');
                else throw new NotSupportedException($"unknown group name {name}");
                index = close;
                continue;
            }
            if (char.IsAsciiDigit(next))
            {
                if (next == '0') throw new NotSupportedException("octal escapes are not supported");
                var end = index + 1;
                if (end < template.Length && char.IsAsciiDigit(template[end]))
                {
                    if (end + 1 < template.Length && char.IsAsciiDigit(template[end + 1])
                        && next < '8' && template[end] < '8' && template[end + 1] < '8')
                        throw new NotSupportedException("octal escapes are not supported");
                    end++;
                }
                output.Append("${").Append(Reference(groups, int.Parse(template[index..end]))).Append('}');
                index = end - 1;
                continue;
            }
            output.Append(next switch
            {
                '\\' => "\\",
                'n' => "\n",
                'r' => "\r",
                't' => "\t",
                'a' => "\a",
                'b' => "\b",
                'f' => "\f",
                'v' => "\v",
                _ when char.IsAsciiLetter(next) => throw new NotSupportedException($@"bad escape \{next}"),
                '$' => "\\$$",
                _ => "\\" + next,
            });
        }
        return output.ToString();
    }
}
