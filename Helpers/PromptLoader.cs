using System.Collections.Concurrent;

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
    /// </summary>
    public static string LoadSection(string promptName, string sectionName)
    {
        var sections = SectionCache.GetOrAdd(promptName, static name => ParseSections(Load(name)));

        if (!sections.TryGetValue(sectionName, out var content))
            throw new KeyNotFoundException($"Section '{sectionName}' not found in prompt '{promptName}'. Available: {string.Join(", ", sections.Keys)}");

        return content;
    }

    /// <summary>
    /// Loads a named section and replaces {{placeholder}} tokens with the supplied values.
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
}
