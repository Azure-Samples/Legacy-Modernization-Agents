// Finds types that more than one generated file declares in the same namespace.
//
// Grouping a service's programs into one namespace is what makes them a service, but it also
// removes the accident that was hiding a real problem: when every program had its own package, two
// programs could each invent a type of the same name and neither the compiler nor a reviewer would
// notice. Sharing a namespace turns that into a build error.
//
// The duplicates are rarely identical. Each caller of a program invents its own interface for it,
// with its own method name and shape, so the collision is not redundancy to be deleted but
// disagreement to be resolved — three callers describing the same callee three different ways.
// Reporting it names the disagreement while the evidence is still to hand.

namespace CobolToQuarkusMigration.Helpers;

using System.Text.RegularExpressions;

public sealed record TypeCollision(
    string Namespace,
    string TypeName,
    IReadOnlyList<string> Files);

public static class GeneratedTypeCollisions
{
    private static readonly Regex NamespaceLine = new(
        @"^\s*namespace\s+([\w.]+)",
        RegexOptions.Compiled);

    private static readonly Regex TypeLine = new(
        @"^\s*(?:public\s+|internal\s+|protected\s+|private\s+)?(?:static\s+|sealed\s+|abstract\s+|partial\s+)*"
        + @"(?:class|record|struct|interface|enum)\s+([A-Za-z_]\w*)",
        RegexOptions.Compiled);

    // A nested type is scoped by the type containing it, so it cannot collide with a sibling
    // file's. Depth is tracked by braces rather than indentation, because a file-scoped namespace
    // puts its top-level types at the same indentation a block-scoped one gives nested ones.
    public static IReadOnlyList<TypeCollision> Find(string generatedDirectory)
    {
        if (!Directory.Exists(generatedDirectory)) return Array.Empty<TypeCollision>();

        var declarations = new Dictionary<(string Namespace, string Type), SortedSet<string>>();

        foreach (var file in Directory.EnumerateFiles(generatedDirectory, "*.cs", SearchOption.AllDirectories))
        {
            string[] lines;
            try { lines = File.ReadAllLines(file); }
            catch (IOException) { continue; }

            var currentNamespace = "";
            var depth = 0;
            var namespaceDepth = -1;

            foreach (var line in lines)
            {
                var ns = NamespaceLine.Match(line);
                if (ns.Success)
                {
                    currentNamespace = ns.Groups[1].Value;
                    // "namespace X;" is file-scoped and opens no level. Anything else is
                    // block-scoped, and its members sit one level in — whether the brace is on
                    // this line or the next.
                    var fileScoped = line.TrimEnd().EndsWith(';');
                    namespaceDepth = fileScoped ? depth : depth + 1;
                    depth += NetBraces(line);
                    continue;
                }

                if (currentNamespace.Length > 0 && depth == namespaceDepth)
                {
                    var type = TypeLine.Match(line);
                    if (type.Success)
                    {
                        var key = (currentNamespace, type.Groups[1].Value);
                        if (!declarations.TryGetValue(key, out var files))
                            declarations[key] = files = new SortedSet<string>(StringComparer.Ordinal);
                        files.Add(file);
                    }
                }

                depth += NetBraces(line);
            }
        }

        return declarations
            .Where(entry => entry.Value.Count > 1)
            .Select(entry => new TypeCollision(entry.Key.Namespace, entry.Key.Type, entry.Value.ToList()))
            .OrderByDescending(c => c.Files.Count)
            .ThenBy(c => c.Namespace, StringComparer.Ordinal)
            .ThenBy(c => c.TypeName, StringComparer.Ordinal)
            .ToList();
    }

    private static int NetBraces(string line)
    {
        var net = 0;
        foreach (var ch in line)
        {
            if (ch == '{') net++;
            else if (ch == '}') net--;
        }
        return net;
    }

    /// <summary>A short report naming the disagreements, or an empty string when there are none.</summary>
    public static string Describe(IReadOnlyList<TypeCollision> collisions, string rootDirectory)
    {
        if (collisions.Count == 0) return string.Empty;

        var lines = new List<string>
        {
            $"{collisions.Count} type name(s) are declared by more than one file in the same namespace.",
            "These will not compile together. Each is usually several programs describing the same",
            "thing differently rather than the same definition twice, so they need reconciling.",
            "",
        };

        foreach (var collision in collisions.Take(20))
        {
            lines.Add($"  {collision.Namespace}.{collision.TypeName}  ({collision.Files.Count} files)");
            foreach (var file in collision.Files.Take(5))
                lines.Add($"      {Path.GetRelativePath(rootDirectory, file)}");
        }

        if (collisions.Count > 20)
            lines.Add($"  … and {collisions.Count - 20} more.");

        return string.Join(Environment.NewLine, lines);
    }
}
