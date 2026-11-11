using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Processes;

internal static class ChunkedOutputAssembler
{
    // This is a fragment protocol guard, not a compiler: unknown forms fail closed without rewriting source.
    internal static IReadOnlyList<CodeFile> Assemble(ChunkedFileResult input, bool isJava)
    {
        input.Success = false;
        input.AssemblyErrors.Clear();
        var chunks = input.Result.ChunkResults.OrderBy(c => c.ChunkIndex).ToList();
        var expected = input.Plan.ChunkCount;
        if (string.IsNullOrWhiteSpace(input.SourceFile))
            input.AssemblyErrors.Add("An authoritative source path is required for assembly output.");
        if (expected <= 0 || input.Result.TotalChunks != expected)
            input.AssemblyErrors.Add("The plan and received total must agree on a positive chunk count.");
        if (!chunks.Select(c => c.ChunkIndex).SequenceEqual(Enumerable.Range(0, Math.Max(0, expected))))
            input.AssemblyErrors.Add("Received indices must contain each planned chunk exactly once (no holes, duplicates or extra indices).");
        if (input.Plan.Chunks.Count > 0 && !input.Plan.Chunks.Select(c => c.ChunkIndex).Order()
            .SequenceEqual(Enumerable.Range(0, Math.Max(0, expected))))
            input.AssemblyErrors.Add("The planned chunk indices do not match the declared chunk count.");
        if (!input.Result.IsConsistent || input.Result.FailedChunks > 0)
            input.AssemblyErrors.Add("Chunk conversion or reconciliation is incomplete.");
        foreach (var chunk in chunks)
        {
            if (!chunk.Success || chunk.ConversionResult is not { Success: true })
                input.AssemblyErrors.Add($"Chunk {chunk.ChunkIndex}: {chunk.ConversionResult?.ErrorMessage ?? "conversion failed"}");
            if (chunk.ConversionResult?.ChunkIndex != chunk.ChunkIndex)
                input.AssemblyErrors.Add($"Chunk {chunk.ChunkIndex}: response index does not match the requested index.");
            if (string.IsNullOrWhiteSpace(chunk.ConversionResult?.ConvertedCode))
                input.AssemblyErrors.Add($"Chunk {chunk.ChunkIndex}: empty output.");
        }
        if (input.AssemblyErrors.Count > 0) return [];

        var code = string.Join("\n", chunks.Select(c => c.ConversionResult.ConvertedCode));
        try
        {
            var boundaries = new List<int>();
            var offset = 0;
            foreach (var chunk in chunks.SkipLast(1))
            {
                offset += chunk.ConversionResult.ConvertedCode!.Length + 1;
                boundaries.Add(offset);
            }
            var className = ValidateFragments(code, boundaries, isJava);
            input.Success = true;
            return [new CodeFile
            {
                FileName = className + (isJava ? ".java" : ".cs"),
                ClassName = className,
                Content = code,
                OriginalCobolFileName = Path.GetFileName(input.SourceFile),
                OriginalSourcePath = input.SourceFile,
                TargetLanguage = isJava ? "Java" : "CSharp"
            }];
        }
        catch (InvalidDataException ex)
        {
            input.AssemblyErrors.Add(ex.Message);
            return [];
        }
    }

    private sealed class Scope(string kind, string name, int start)
    {
        public string Kind { get; } = kind;
        public string Name { get; } = name;
        public int Start { get; set; } = start;
        public HashSet<string> Members { get; } = new(StringComparer.Ordinal);
        public Dictionary<string, bool> MemberKinds { get; } = new(StringComparer.Ordinal);
        public bool IsEnum { get; init; }
    }

    private static string ValidateFragments(string code, List<int> boundaries, bool isJava)
    {
        var masked = MaskNonCode(code, isJava, boundaries);
        var scopes = new Stack<Scope>();
        scopes.Push(new Scope("root", "", 0));
        var delimiters = new Stack<char>();
        var typeNames = new HashSet<string>(StringComparer.Ordinal);
        string? primary = null;
        string? publicJavaType = null;
        Scope? primaryScope = null;
        var primaryStart = -1;
        var primaryEnd = -1;
        var namespaceSeen = false;
        var tokens = Regex.Matches(masked, @"[A-Za-z_@][A-Za-z_0-9]*|=>|[^\s]");
        foreach (Match token in tokens)
        {
            var text = token.Value;
            if (text == "#") throw new InvalidDataException("Preprocessor directives are not supported by ordered fragment assembly.");
            if (text is "(" or "[") delimiters.Push(text[0]);
            if (text is ")" or "]")
            {
                if (delimiters.Count == 0 || delimiters.Pop() != (text == ")" ? '(' : '['))
                    throw new InvalidDataException("Unbalanced parentheses or brackets.");
            }
            if (text == "{")
            {
                if (delimiters.Count > 0)
                    throw new InvalidDataException("Braced expressions inside parameter or attribute lists are unsupported.");
                var parent = scopes.Peek();
                var header = masked[parent.Start..token.Index].Trim();
                var type = Regex.Match(header, @"\b(class|interface|struct|record|enum)\s+(?:(?:class|struct)\s+)?(?<name>@?[A-Za-z_$][A-Za-z_0-9$]*)(?=\s|<|\(|:|$)");
                if (parent.Kind is "root" or "namespace" or "type" && type.Success)
                {
                    var prefix = RemoveAttributes(header[..type.Index], isJava);
                    if (!Regex.IsMatch(prefix, @"^(?:(?:public|private|protected|internal|static|abstract|sealed|partial|final|strictfp|readonly|ref)\s*)*$"))
                        throw new InvalidDataException("Unsupported type header; source has not been rewritten.");
                    var name = type.Groups["name"].Value.TrimStart('@');
                    var afterName = header[(type.Groups["name"].Index + type.Groups["name"].Length)..].TrimStart();
                    var arity = 0;
                    if (!isJava && afterName.StartsWith('<'))
                    {
                        var end = afterName.IndexOf('>');
                        if (end < 0) throw new InvalidDataException("Unsupported generic type header.");
                        arity = SplitParameters(afterName[1..end]).Count;
                    }
                    var identity = name + "`" + arity;
                    var key = string.Join(".", scopes.Reverse().Where(s => s.Kind is "type" or "namespace").Select(s => s.Name)) + "." + identity;
                    if (!typeNames.Add(key))
                        throw new InvalidDataException($"Conflicting or repeated type declaration: {key}.");
                    if (primary == null)
                    {
                        primary = name;
                        primaryStart = token.Index;
                    }
                    if (isJava && parent.Kind == "root" && Regex.IsMatch(prefix, @"\bpublic\b"))
                    {
                        if (publicJavaType != null)
                            throw new InvalidDataException("Multiple public Java top-level types cannot share one fragment output file.");
                        publicJavaType = name;
                    }
                    var typeScope = new Scope("type", identity, token.Index + 1) { IsEnum = type.Groups[1].Value == "enum" };
                    primaryScope ??= typeScope;
                    scopes.Push(typeScope);
                }
                else if (parent.Kind == "root" && Regex.IsMatch(header, @"^namespace\s+[\w.]+$"))
                {
                    CheckHeader(token.Index);
                    if (namespaceSeen) throw new InvalidDataException("Repeated or incompatible namespace declarations.");
                    namespaceSeen = true;
                    scopes.Push(new Scope("namespace", header[10..].Trim(), token.Index + 1));
                }
                else
                {
                    if (parent.Kind is "root" or "namespace")
                        throw new InvalidDataException("Expected a supported type declaration, not a loose body or complete-unit continuation.");
                    if (parent.Kind == "type") RegisterMember(parent, header, isJava);
                    scopes.Push(new Scope("body", "", token.Index + 1));
                }
            }
            else if (text == "}")
            {
                if (scopes.Count == 1 || delimiters.Count > 0)
                    throw new InvalidDataException("Unbalanced braces.");
                var closed = scopes.Pop();
                var pending = masked[closed.Start..token.Index].Trim();
                if (closed.Kind == "type" && pending.Length > 0 && !closed.IsEnum)
                    throw new InvalidDataException("Unsupported or unterminated member before the type's closing brace.");
                if (ReferenceEquals(closed, primaryScope))
                    primaryEnd = token.Index;
                scopes.Peek().Start = token.Index + 1;
            }
            else if (text == ";" && delimiters.Count == 0)
            {
                var scope = scopes.Peek();
                var header = masked[scope.Start..token.Index].Trim();
                if (scope.Kind is "root" or "namespace")
                {
                    if (!Regex.IsMatch(header, isJava
                        ? @"^(?:package|import)\s+[\w.*\s]+$"
                        : @"^(?:global\s+)?using\s+[\w.:=\s]+$|^namespace\s+[\w.]+$"))
                        throw new InvalidDataException("Unsupported top-level statement or missing type declaration.");
                    CheckHeader(token.Index);
                    if (header.StartsWith("package ") || header.StartsWith("namespace "))
                    {
                        if (namespaceSeen) throw new InvalidDataException("Repeated or incompatible package/namespace declarations.");
                        namespaceSeen = true;
                    }
                }
                else if (scope.Kind == "type" && header.Length > 0)
                    RegisterMember(scope, header, isJava);
                scope.Start = token.Index + 1;
            }
        }
        if (scopes.Count != 1 || delimiters.Count > 0)
            throw new InvalidDataException("Incomplete fragment stream: unclosed type, body, parentheses or brackets.");
        if (primary == null || primaryEnd < 0)
            throw new InvalidDataException("No complete supported type was produced.");
        if (!string.IsNullOrWhiteSpace(masked[scopes.Peek().Start..]))
            throw new InvalidDataException("Unconsumed trailing output.");
        if (boundaries.Any(b => b <= primaryStart || b > primaryEnd))
            throw new InvalidDataException("Expected first/middle/last continuation fragments; the primary type must remain open across every boundary. Complete-unit chunks are not merged.");
        foreach (var (start, end) in new[] { 0 }.Concat(boundaries).Zip(boundaries.Concat(new[] { code.Length })))
            if (string.IsNullOrWhiteSpace(masked[start..end]))
                throw new InvalidDataException("A chunk contains no code (comments alone do not establish source coverage).");
        return publicJavaType ?? primary;

        void CheckHeader(int position)
        {
            if (primary != null || boundaries.Any(b => position >= b))
                throw new InvalidDataException("Package, namespace and import/using directives belong only to the first fragment before its types.");
        }
    }

    private static void RegisterMember(Scope scope, string header, bool isJava)
    {
        header = RemoveAttributes(header, isJava).Trim();
        if (Regex.IsMatch(header, @"^(?:package|namespace|import|using)\b"))
            throw new InvalidDataException("Continuation fragments must not repeat headers.");
        var paren = header.IndexOf('(');
        var assignment = header.IndexOf('=');
        string key;
        string memberName;
        var isMethod = paren >= 0 && (assignment < 0 || paren < assignment);
        if (isMethod)
        {
            var name = Regex.Match(header[..paren], @"(?<name>@?\w+)(?<generic><[^<>]+>)?\s*$");
            var end = paren + 1;
            var depth = 1;
            for (; end < header.Length; end++)
            {
                if (header[end] == '(') depth++;
                if (header[end] == ')' && --depth == 0) break;
            }
            if (!name.Success || end >= header.Length)
                throw new InvalidDataException("Unsupported member signature.");
            var generic = isJava
                ? Regex.Match(header, @"^(?:(?:public|private|protected|static|final|abstract|synchronized|native|strictfp|default)\s+)*<(?<generic>[^<>]+)>").Groups["generic"].Value
                : name.Groups["generic"].Value.Trim('<', '>');
            var genericNames = generic.Split(',', StringSplitOptions.TrimEntries | StringSplitOptions.RemoveEmptyEntries)
                .Select(g => g.Split(' ', StringSplitOptions.RemoveEmptyEntries)[0]).ToArray();
            var parameters = SplitParameters(header[(paren + 1)..end]);
            var types = parameters.Select(p =>
            {
                var declaration = RemoveAttributes(p.Split('=')[0].Trim(), isJava);
                declaration = Regex.Replace(declaration, @"\b(?:params|final)\s+", "");
                declaration = Regex.Replace(declaration, @"\b(?:out|in)\s+", "ref ");
                var ids = Regex.Matches(declaration, @"@?\b[A-Za-z_]\w*");
                if (ids.Count < 2) throw new InvalidDataException("Unsupported parameter declaration.");
                var parameterName = ids[^1];
                declaration = declaration.Remove(parameterName.Index, parameterName.Length);
                for (var i = 0; i < genericNames.Length; i++)
                    declaration = Regex.Replace(declaration, @"\b" + Regex.Escape(genericNames[i]) + @"\b", "!" + i);
                if (isJava)
                {
                    var erased = new System.Text.StringBuilder();
                    var genericDepth = 0;
                    foreach (var c in declaration)
                    {
                        if (c == '<') genericDepth++;
                        else if (c == '>') genericDepth--;
                        else if (genericDepth == 0) erased.Append(c);
                    }
                    declaration = erased.ToString();
                }
                return Regex.Replace(declaration, @"\s+", "");
            });
            key = $"method:{name.Groups["name"].Value}`{genericNames.Length}({string.Join(",", types)})";
            memberName = name.Groups["name"].Value;
        }
        else
        {
            var declaration = header.Split('=')[0].Trim();
            if (SplitParameters(declaration).Count > 1)
                throw new InvalidDataException("Multiple field declarators are unsupported; emit separate declarations.");
            var name = Regex.Match(declaration, @"(?<name>@?\w+)\s*(?:\[\s*\])?$");
            if (!name.Success) throw new InvalidDataException("Unsupported field, property or initializer.");
            key = "member:" + name.Groups["name"].Value;
            memberName = name.Groups["name"].Value;
        }
        if (scope.MemberKinds.TryGetValue(memberName, out var previousIsMethod) && (!previousIsMethod || !isMethod))
            throw new InvalidDataException($"Repeated or conflicting member name in {scope.Name}: {memberName}.");
        scope.MemberKinds[memberName] = isMethod;
        if (!scope.Members.Add(key))
            throw new InvalidDataException($"Repeated or conflicting member in {scope.Name}: {key}. Even identical overlapping definitions must not be emitted twice.");
    }

    private static List<string> SplitParameters(string text)
    {
        var result = new List<string>();
        var depth = 0;
        var start = 0;
        for (var i = 0; i < text.Length; i++)
        {
            if (text[i] is '<' or '(' or '[') depth++;
            if (text[i] is '>' or ')' or ']') depth--;
            if (text[i] == ',' && depth == 0)
            {
                result.Add(text[start..i].Trim());
                start = i + 1;
            }
        }
        if (text[start..].Trim() is { Length: > 0 } last) result.Add(last);
        return result;
    }

    private static string RemoveAttributes(string header, bool isJava) => isJava
        ? Regex.Replace(header, @"@\w+(?:\.\w+)*(?:\([^()]*\))?\s*", "")
        : Regex.Replace(header, @"^\s*(?:\[[^\[\]]*\]\s*)+", "");

    private static string MaskNonCode(string code, bool isJava, List<int> boundaries)
    {
        var masked = code.ToCharArray();
        for (var i = 0; i < code.Length; i++)
        {
            var start = i;
            if (code[i] == '/' && i + 1 < code.Length && code[i + 1] is '/' or '*')
            {
                if (code[i + 1] == '/')
                {
                    while (i < code.Length && code[i] != '\n') i++;
                }
                else
                {
                    var end = code.IndexOf("*/", i + 2, StringComparison.Ordinal);
                    if (end < 0) throw new InvalidDataException("Unterminated block comment.");
                    i = end + 2;
                }
            }
            else if (code[i] is '"' or '\'')
            {
                var quote = code[i];
                var verbatim = !isJava && i > 0 && code[i - 1] == '@';
                if (!isJava && i > 0 && (code[i - 1] == '$' || verbatim && i > 1 && code[i - 2] == '$'))
                    throw new InvalidDataException("Interpolated strings require compiler validation and are unsupported by this fragment guard.");
                if (code.AsSpan(i).StartsWith("\"\"\""))
                {
                    var end = code.IndexOf("\"\"\"", i + 3, StringComparison.Ordinal);
                    if (end < 0) throw new InvalidDataException("Unterminated multiline string.");
                    i = end + 3;
                    if (i < code.Length && code[i] == '"')
                        throw new InvalidDataException("Raw string delimiters longer than three quotes are unsupported.");
                }
                else
                {
                    i++;
                    var closed = false;
                    while (i < code.Length)
                    {
                        if (!verbatim && code[i] == '\\') { i += 2; continue; }
                        if (!verbatim && code[i] is '\r' or '\n')
                            throw new InvalidDataException("A normal string or character literal cannot span lines or chunks.");
                        if (code[i++] != quote) continue;
                        if (verbatim && i < code.Length && code[i] == quote) { i++; continue; }
                        closed = true;
                        break;
                    }
                    if (!closed) throw new InvalidDataException("Unterminated string or character literal.");
                }
                // A literal remains a token, so a field initializer cannot disappear into a comment.
                masked[start] = '0';
                start++;
            }
            else continue;
            if (boundaries.Any(b => b > start && b < i))
                throw new InvalidDataException("Comments and literals must not span chunk boundaries.");
            for (var j = start; j < Math.Min(i, masked.Length); j++)
                if (masked[j] is not '\n' and not '\r') masked[j] = ' ';
            i--;
        }
        return new string(masked);
    }
}
