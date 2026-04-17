using System.Collections.Concurrent;
using System.Text;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Loads prompt templates from the Agents/Prompts directory and supports placeholder replacement.
/// Placeholders use the {{Name}} syntax.
/// Files may contain multiple named sections delimited by "## SECTION: Name" headers.
/// </summary>
public static class PromptLoader
{
    private static readonly ConcurrentDictionary<string, string> FileCache = new();
    private static readonly ConcurrentDictionary<string, Dictionary<string, string>> SectionCache = new();

    /// <summary>
    /// Gets or sets the current codebase profile text. Set this after scanning COBOL files
    /// so that prompt templates with {{CodebaseProfile}} are populated automatically.
    /// </summary>
    public static string? CodebaseProfile { get; set; }

    private static string PromptsDirectory =>
        Path.Combine(AppContext.BaseDirectory, "Agents", "Prompts");

    /// <summary>
    /// Loads a prompt template by name (without extension). Files are expected at Agents/Prompts/{name}.md.
    /// </summary>
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

    /// <summary>
    /// Loads a prompt template and replaces {{placeholder}} tokens with the supplied values.
    /// </summary>
    public static string Load(string promptName, Dictionary<string, string> replacements)
    {
        var template = Load(promptName);
        foreach (var (key, value) in replacements)
        {
            template = template.Replace($"{{{{{key}}}}}", value);
        }
        return template;
    }

    /// <summary>
    /// Loads a named section from a prompt file that contains "## SECTION: Name" delimiters.
    /// Automatically applies the {{CodebaseProfile}} replacement if set.
    /// </summary>
    public static string LoadSection(string promptName, string sectionName)
    {
        var sections = SectionCache.GetOrAdd(promptName, static name => ParseSections(Load(name)));

        if (!sections.TryGetValue(sectionName, out var content))
            throw new KeyNotFoundException($"Section '{sectionName}' not found in prompt '{promptName}'. Available: {string.Join(", ", sections.Keys)}");

        return ApplyGlobalReplacements(content);
    }

    /// <summary>
    /// Loads a named section and replaces {{placeholder}} tokens with the supplied values.
    /// Automatically applies the {{CodebaseProfile}} replacement if set.
    /// </summary>
    public static string LoadSection(string promptName, string sectionName, Dictionary<string, string> replacements)
    {
        var template = LoadSection(promptName, sectionName);
        foreach (var (key, value) in replacements)
        {
            template = template.Replace($"{{{{{key}}}}}", value);
        }
        return template;
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
        if (CodebaseProfile is not null)
            template = template.Replace("{{CodebaseProfile}}", CodebaseProfile);
        return template;
    }

    /// <summary>
    /// Generates a codebase profile string from the scanned COBOL files.
    /// Set the result on <see cref="CodebaseProfile"/> before agents start processing.
    /// </summary>
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
