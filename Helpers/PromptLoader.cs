using System.Collections.Concurrent;
using System.Text;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

// Loads Markdown prompt sections and replaces {{Name}} placeholders.
public static class PromptLoader
{
    private static readonly Regex PlaceholderPattern =
        new(@"\{\{(?<name>[A-Za-z0-9_]+)\}\}", RegexOptions.Compiled);

    private static readonly ConcurrentDictionary<string, string> FileCache = new();
    private static readonly ConcurrentDictionary<string, Dictionary<string, string>> SectionCache = new();

    // Populates the global {{CodebaseProfile}} placeholder.
    public static string? CodebaseProfile { get; set; }

    private static string PromptsDirectory =>
        Path.Combine(AppContext.BaseDirectory, "Agents", "Prompts");

    public static string Load(string promptName)
    {
        return FileCache.GetOrAdd(promptName, static name =>
        {
            var path = Path.Combine(PromptsDirectory, $"{name}.md");
            if (!File.Exists(path))
                throw new FileNotFoundException($"Prompt template not found: {path}");
            return File.ReadAllText(path);
        });
    }

    public static string Load(string promptName, Dictionary<string, string> replacements)
    {
        return ApplyReplacements(Load(promptName), replacements);
    }

    public static string LoadValidated(string promptName, IReadOnlyDictionary<string, string> replacements)
    {
        return RenderValidated(Load(promptName), replacements, promptName, sectionName: null);
    }

    public static string LoadSection(string promptName, string sectionName)
    {
        return ApplyGlobalReplacements(GetSection(promptName, sectionName));
    }

    public static string LoadSection(string promptName, string sectionName, Dictionary<string, string> replacements)
    {
        return ApplyReplacements(LoadSection(promptName, sectionName), replacements);
    }

    // Use LoadSection for templates intentionally rendered in multiple stages.
    public static string LoadSectionValidated(
        string promptName,
        string sectionName,
        IReadOnlyDictionary<string, string> replacements)
    {
        return RenderValidated(
            GetSection(promptName, sectionName), replacements, promptName, sectionName);
    }

    private static string GetSection(string promptName, string sectionName)
    {
        var sections = SectionCache.GetOrAdd(promptName, static name => ParseSections(Load(name)));

        if (!sections.TryGetValue(sectionName, out var content))
            throw new KeyNotFoundException($"Section '{sectionName}' not found in prompt '{promptName}'. Available: {string.Join(", ", sections.Keys)}");

        return content;
    }

    private static Dictionary<string, string> ParseSections(string content)
    {
        var sections = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        var lines = content.Split('\n');
        string? currentSection = null;
        var buffer = new List<string>();

        foreach (var line in lines)
        {
            if (line.StartsWith("## SECTION: "))
            {
                if (currentSection != null)
                    sections[currentSection] = string.Join("\n", buffer).Trim();

                currentSection = line["## SECTION: ".Length..].Trim();
                buffer.Clear();
            }
            else if (currentSection != null)
            {
                buffer.Add(line);
            }
        }

        if (currentSection != null)
            sections[currentSection] = string.Join("\n", buffer).Trim();

        return sections;
    }

    private static string ApplyGlobalReplacements(string template)
    {
        return template.Replace("{{CodebaseProfile}}", CodebaseProfile ?? string.Empty);
    }

    private static string ApplyReplacements(
        string template,
        IReadOnlyDictionary<string, string> replacements)
    {
        foreach (var (key, value) in replacements)
            template = template.Replace($"{{{{{key}}}}}", value);
        return template;
    }

    private static string RenderValidated(
        string template,
        IReadOnlyDictionary<string, string> replacements,
        string promptName,
        string? sectionName)
    {
        var unresolved = new SortedSet<string>(StringComparer.Ordinal);
        var rendered = PlaceholderPattern.Replace(template, match =>
        {
            var name = match.Groups["name"].Value;
            if (replacements.TryGetValue(name, out var value))
                return value;
            if (name == "CodebaseProfile")
                return CodebaseProfile ?? string.Empty;

            unresolved.Add(name);
            return match.Value;
        });

        if (unresolved.Count > 0)
        {
            var location = sectionName is null
                ? $"prompt '{promptName}'"
                : $"prompt '{promptName}', section '{sectionName}'";
            throw new InvalidOperationException(
                $"Unresolved placeholder(s) in {location}: {string.Join(", ", unresolved)}.");
        }

        return rendered;
    }

    // Summarizes the scanned corpus for the global prompt profile.
    public static string GenerateCodebaseProfile(IReadOnlyList<CobolFile> files)
    {
        var programs = files.Where(f => !f.IsCopybook).ToList();
        var copybooks = files.Where(f => f.IsCopybook).ToList();
        var totalLines = files.Sum(f => f.Content.Split('\n').Length);

        var features = DetectFeatures(files);

        var sb = new StringBuilder();
        sb.AppendLine("## Source Codebase Profile");
        sb.AppendLine($"- **Programs**: {programs.Count} | **Copybooks**: {copybooks.Count} | **Total lines**: {totalLines:N0}");

        if (programs.Count > 0)
            sb.AppendLine($"- **Program files**: {string.Join(", ", programs.Select(p => p.FileName))}");
        if (copybooks.Count > 0)
            sb.AppendLine($"- **Copybook files**: {string.Join(", ", copybooks.Select(c => c.FileName))}");

        if (features.Count > 0)
            sb.AppendLine($"- **Detected features**: {string.Join(", ", features)}");

        return sb.ToString().TrimEnd();
    }

    private static readonly (string Feature, string[] Patterns)[] FeatureDetectors =
    [
        ("ARITHMETIC",      ["COMPUTE ", "ADD ", "MULTIPLY "]),
        ("CALL_PROGRAM",    ["CALL "]),
        ("CICS_SCREEN",     ["SEND MAP", "RECEIVE MAP"]),
        ("COPYBOOK_REF",    ["COPY "]),
        ("EXEC_CICS",       ["EXEC CICS"]),
        ("EXEC_SQL",        ["EXEC SQL"]),
        ("FILE_IO",         ["OPEN ", "READ "]),      // both must match
        ("SORT_MERGE",      ["SORT ", "MERGE "]),
        ("STRING_HANDLING", ["STRING ", "UNSTRING "]),
        ("TABLE_HANDLING",  ["OCCURS "]),
    ];

    private static List<string> DetectFeatures(IReadOnlyList<CobolFile> files)
    {
        var allContent = string.Join("\n", files.Select(f => f.Content));
        var features = new List<string>();

        foreach (var (feature, patterns) in FeatureDetectors)
        {
            // FILE_IO requires all patterns present; others require any
            bool detected = feature == "FILE_IO"
                ? patterns.All(p => allContent.Contains(p, StringComparison.OrdinalIgnoreCase))
                : patterns.Any(p => allContent.Contains(p, StringComparison.OrdinalIgnoreCase));

            if (detected)
                features.Add(feature);
        }

        return features;
    }
}
