using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;
using McpChatWeb.Models;

namespace McpChatWeb.Services;

// "Chat with report" answers the question asked from the report the user picked.
//
// The earlier handler loaded the report into a prompt variable it then never used, and the
// question fell through to the general path. That path prepends the newest run's statistics and
// reverse-engineering rows, so a question about one program came back as a summary of the run.
// Report mode now builds its own prompt and returns before any run context is gathered.

public sealed class PortalChatOptions
{
    public const string SectionName = "PortalChat";

    // Reports up to this size go to the model whole; larger ones are cut to the relevant sections.
    public int WholeReportMaxChars { get; set; } = 60_000;
    // Room for the sections picked from a large report.
    public int SectionBudgetChars { get; set; } = 40_000;
    // The heading list sent with a cut report, so the model can say where else to look.
    public int OutlineMaxChars { get; set; } = 4_000;
    // Earlier turns sent back so a follow-up ("and its copybooks?") keeps its subject.
    public int MaxHistoryMessages { get; set; } = 8;
    public int MaxHistoryMessageChars { get; set; } = 1_500;
    // Conversations kept in the browser's saved-chat list; the oldest are dropped first.
    public int MaxSavedConversations { get; set; } = 100;
    public int MaxAnswerTokens { get; set; } = 4000;

    public static PortalChatOptions Load(string appSettingsPath, out string? warning)
    {
        warning = null;
        if (!File.Exists(appSettingsPath)) return new();
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(appSettingsPath),
                new JsonDocumentOptions { CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true });
            return doc.RootElement.TryGetProperty(SectionName, out var section)
                ? section.Deserialize<PortalChatOptions>(new JsonSerializerOptions { PropertyNameCaseInsensitive = true }) ?? new()
                : new();
        }
        catch (Exception ex) when (ex is JsonException or IOException)
        {
            warning = $"{SectionName} in {appSettingsPath} could not be read ({ex.Message}); using defaults.";
            return new();
        }
    }
}

public sealed record ReportSection(int Order, string Heading, string Breadcrumb, string Text);

public sealed record ReportFileInfo(string Name, string Path, long SizeBytes, DateTime LastModified);

public static class ReportCatalog
{
    public const string LatestSentinel = "latest";

    public static IReadOnlyList<ReportFileInfo> List(string repoRoot)
    {
        var outputDir = Path.Combine(repoRoot, "output");
        if (!Directory.Exists(outputDir)) return Array.Empty<ReportFileInfo>();

        // Scan checkouts symlink back into output/; following them multiplies the walk per checkout.
        var walk = new EnumerationOptions
        {
            RecurseSubdirectories = true,
            IgnoreInaccessible = true,
            AttributesToSkip = FileAttributes.Hidden | FileAttributes.System | FileAttributes.ReparsePoint,
        };
        var reports = Directory.EnumerateFiles(outputDir, "*.md", walk)
            .Where(f => f.Contains("reverse-engineering", StringComparison.OrdinalIgnoreCase)
                     || f.Contains("re-report", StringComparison.OrdinalIgnoreCase)
                     || f.Contains("migration-report", StringComparison.OrdinalIgnoreCase)
                     || f.Contains("analysis", StringComparison.OrdinalIgnoreCase))
            .Select(f => new FileInfo(f))
            .OrderByDescending(fi => fi.LastWriteTimeUtc)
            .Select(fi => new ReportFileInfo(fi.Name, Path.GetRelativePath(repoRoot, fi.FullName), fi.Length, fi.LastWriteTimeUtc))
            .ToList();

        var standard = new FileInfo(Path.Combine(outputDir, "reverse-engineering-details.md"));
        if (standard.Exists && !reports.Any(r => r.Name == standard.Name))
            reports.Insert(0, new ReportFileInfo(standard.Name, Path.GetRelativePath(repoRoot, standard.FullName), standard.Length, standard.LastWriteTimeUtc));

        return reports;
    }

    // "Latest" means the newest reverse-engineering report; migration reports are listed too, but a
    // conversion finishing after the RE run must not take its place. Falls back to the newest report.
    public static ReportFileInfo? Latest(IReadOnlyList<ReportFileInfo> reports) =>
        reports.FirstOrDefault(r => r.Name.Contains("reverse-engineering", StringComparison.OrdinalIgnoreCase)
                                 || r.Name.Contains("re-report", StringComparison.OrdinalIgnoreCase))
        ?? reports.FirstOrDefault();

    // The requested report, or the latest one when the caller asks for "latest" (issue #29's default).
    // Only a listed report qualifies, so a browser-supplied path cannot send configuration or other
    // repository files to the model, and no component may be a link that leaves the repository.
    public static string? Resolve(string repoRoot, string? requested)
    {
        if (string.IsNullOrWhiteSpace(requested)) return null;
        var reports = List(repoRoot);
        ReportFileInfo? report;
        if (string.Equals(requested.Trim(), LatestSentinel, StringComparison.OrdinalIgnoreCase))
            report = Latest(reports);
        else
        {
            var path = RepositoryPath.ResolveInside(repoRoot, requested);
            if (path is null) return null;
            var comparison = OperatingSystem.IsWindows() ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
            report = reports.FirstOrDefault(r => string.Equals(
                Path.GetFullPath(Path.Combine(repoRoot, r.Path)), path, comparison));
        }
        if (report is null) return null;
        var full = Path.GetFullPath(Path.Combine(repoRoot, report.Path));
        return HasLinkedComponent(repoRoot, full) ? null : full;
    }

    // The identity the portal lists reports under, for a path Resolve returned.
    public static string RelativePath(string repoRoot, string resolvedPath) =>
        Path.GetRelativePath(repoRoot, resolvedPath).Replace('\\', '/');

    private static bool HasLinkedComponent(string repoRoot, string fullPath)
    {
        var root = Path.GetFullPath(repoRoot);
        for (var current = fullPath; current.Length > root.Length; current = Path.GetDirectoryName(current)!)
        {
            FileSystemInfo info = File.Exists(current) ? new FileInfo(current) : new DirectoryInfo(current);
            if (info.LinkTarget is not null) return true;
        }
        return false;
    }
}

public static class ReportChat
{
    // Tells the MCP server the prompt is self-contained and must not be mixed with run context.
    public const string McpContext = "report";

    public const string AnswerOnlyRule =
        "Answer only the question asked. Do not summarise the report, the run or the estate unless the question asks for a summary.";

    private static readonly Regex Heading = new(@"^(#{1,4})\s+(.+?)\s*#*\s*$", RegexOptions.Compiled);
    private static readonly Regex Word = new(@"[A-Za-z0-9][A-Za-z0-9_\-]*", RegexOptions.Compiled);

    private static readonly HashSet<string> StopWords = new(StringComparer.OrdinalIgnoreCase)
    {
        "the","and","for","are","was","what","which","who","whom","how","why","when","where","does","did","doing",
        "this","that","these","those","with","from","into","about","there","their","they","them","then","than",
        "have","has","had","can","could","would","should","will","shall","may","might","must","not","any","all",
        "our","your","you","its","it's","his","her","she","him","one","also","just","only","some","more","most",
        "tell","show","give","list","explain","describe","please","report","there's","is","be","of","in","on","to",
    };

    public static IReadOnlyList<ReportSection> Split(string markdown)
    {
        var sections = new List<ReportSection>();
        var trail = new string?[5];
        var current = new StringBuilder();
        string heading = "(start of report)";
        string breadcrumb = heading;
        var inFence = false;

        void Flush()
        {
            if (current.ToString().Trim().Length > 0)
                sections.Add(new ReportSection(sections.Count, heading, breadcrumb, current.ToString().TrimEnd()));
            current.Clear();
        }

        foreach (var line in markdown.Replace("\r\n", "\n").Split('\n'))
        {
            if (line.TrimStart().StartsWith("```", StringComparison.Ordinal)) inFence = !inFence;
            var m = inFence ? Match.Empty : Heading.Match(line);
            if (m.Success)
            {
                Flush();
                var level = m.Groups[1].Value.Length;
                trail[level] = m.Groups[2].Value.Trim();
                for (var i = level + 1; i < trail.Length; i++) trail[i] = null;
                heading = trail[level]!;
                breadcrumb = string.Join(" > ", trail.Skip(1).Where(t => t is not null));
            }
            current.AppendLine(line);
        }
        Flush();
        return sections;
    }

    public static IReadOnlyList<string> Terms(string text) =>
        Word.Matches(text)
            .Select(m => m.Value.ToLowerInvariant())
            .Where(t => t.Length >= 3 && !StopWords.Contains(t))
            .Distinct()
            .ToList();

    // Rare terms count for more than common ones, and a hit in a section's own heading counts for
    // more than one in body text: asking about PAYRB005 should land on the PAYRB005 section, not on every
    // section that mentions "program".
    public static IReadOnlyList<ReportSection> Select(
        IReadOnlyList<ReportSection> sections, string question, IEnumerable<ChatHistoryMessage>? history, int budgetChars)
    {
        if (sections.Count == 0 || budgetChars <= 0) return Array.Empty<ReportSection>();

        var lastUserTurn = history?.LastOrDefault(h => string.Equals(h.Role, "user", StringComparison.OrdinalIgnoreCase))?.Content;
        var questionTerms = Terms(question);
        // The previous question carries the subject of a follow-up, at a lower weight.
        var followUpTerms = lastUserTurn is null ? new List<string>() : Terms(lastUserTurn).Except(questionTerms).ToList();

        var lowered = sections.Select(s => (
            Heading: s.Heading.ToLowerInvariant(),
            Ancestors: s.Breadcrumb.ToLowerInvariant(),
            Body: s.Text.ToLowerInvariant())).ToArray();
        double Idf(string term)
        {
            var df = lowered.Count(l => l.Body.Contains(term, StringComparison.Ordinal));
            return df == 0 ? 0 : Math.Log(1 + (double)sections.Count / df);
        }

        var weights = questionTerms.Select(t => (t, w: Idf(t)))
            .Concat(followUpTerms.Select(t => (t, w: Idf(t) * 0.5)))
            .Where(x => x.w > 0)
            .ToList();

        var scored = sections.Select((s, i) =>
        {
            double score = 0;
            foreach (var (term, w) in weights)
            {
                // A subsection only inherits its parent's name, so that counts for less than its own heading.
                if (lowered[i].Heading.Contains(term, StringComparison.Ordinal)) score += 5 * w;
                else if (lowered[i].Ancestors.Contains(term, StringComparison.Ordinal)) score += w;
                score += Math.Min(CountOccurrences(lowered[i].Body, term), 5) * w;
            }
            return (Section: s, Score: score);
        }).ToList();

        IEnumerable<ReportSection> ranked = scored.Any(x => x.Score > 0)
            ? scored.Where(x => x.Score > 0).OrderByDescending(x => x.Score).ThenBy(x => x.Section.Order).Select(x => x.Section)
            : sections; // nothing matched: send the opening sections so the model can say so with context

        var picked = new List<ReportSection>();
        var used = 0;
        foreach (var s in ranked)
        {
            var remaining = budgetChars - used;
            if (remaining <= 0) break;
            if (s.Text.Length <= remaining)
            {
                picked.Add(s);
                used += s.Text.Length;
            }
            else if (picked.Count == 0)
            {
                // A single section bigger than the budget is still the best match; send its head.
                picked.Add(s with { Text = s.Text[..remaining] + "\n[... section truncated ...]" });
                used = budgetChars;
            }
        }
        return picked.OrderBy(s => s.Order).ToList();
    }

    public static string FormatHistory(IEnumerable<ChatHistoryMessage>? history, PortalChatOptions options)
    {
        var turns = (history ?? Enumerable.Empty<ChatHistoryMessage>())
            .Where(h => !string.IsNullOrWhiteSpace(h.Content)
                     && (string.Equals(h.Role, "user", StringComparison.OrdinalIgnoreCase)
                      || string.Equals(h.Role, "assistant", StringComparison.OrdinalIgnoreCase)))
            .TakeLast(Math.Max(0, options.MaxHistoryMessages))
            .ToList();
        if (turns.Count == 0) return "";

        var sb = new StringBuilder("PRIOR CONVERSATION (context for follow-up questions only; do not answer it again):\n");
        foreach (var t in turns)
        {
            var role = string.Equals(t.Role, "assistant", StringComparison.OrdinalIgnoreCase) ? "Assistant" : "User";
            var text = t.Content.Length > options.MaxHistoryMessageChars
                ? t.Content[..options.MaxHistoryMessageChars] + "…"
                : t.Content;
            sb.Append(role).Append(": ").AppendLine(text);
        }
        return sb.AppendLine("---").ToString();
    }

    public static string BuildPrompt(
        string reportName, string reportContent, string question,
        IReadOnlyList<ChatHistoryMessage>? history, PortalChatOptions options)
    {
        var sb = new StringBuilder();
        sb.AppendLine("You answer one question about a reverse-engineering report.");
        sb.AppendLine("Rules:");
        sb.AppendLine("- " + AnswerOnlyRule);
        sb.AppendLine("- Use only the report text below. If it does not contain the answer, say so plainly and name the sections that came closest. Do not use outside knowledge or invent details.");
        sb.AppendLine("- Name the report sections you used.");
        sb.AppendLine("- Keep the answer as short as the question allows.");
        sb.AppendLine();

        if (reportContent.Length <= options.WholeReportMaxChars)
        {
            sb.AppendLine($"=== REPORT: {reportName} ({reportContent.Length:N0} chars) ===");
            sb.AppendLine(reportContent);
        }
        else
        {
            var sections = Split(reportContent);
            var picked = Select(sections, question, history, options.SectionBudgetChars);
            sb.AppendLine($"=== REPORT: {reportName} ({reportContent.Length:N0} chars, {sections.Count} sections) ===");
            sb.AppendLine($"Only the {picked.Count} sections most relevant to the question are included. The outline lists every section.");
            sb.AppendLine();
            sb.AppendLine("OUTLINE:");
            sb.AppendLine(Outline(sections, options.OutlineMaxChars));
            sb.AppendLine();
            foreach (var s in picked)
            {
                sb.AppendLine($"--- SECTION: {s.Breadcrumb} ---");
                sb.AppendLine(s.Text);
                sb.AppendLine();
            }
        }
        sb.AppendLine("=== END OF REPORT ===");
        sb.AppendLine();
        sb.Append(FormatHistory(history, options));
        sb.Append("QUESTION: ").AppendLine(question);
        return sb.ToString();
    }

    private static string Outline(IReadOnlyList<ReportSection> sections, int maxChars)
    {
        var sb = new StringBuilder();
        foreach (var s in sections)
        {
            var line = "- " + s.Breadcrumb;
            if (sb.Length + line.Length + 1 > maxChars)
            {
                sb.AppendLine($"- ... {sections.Count - s.Order} more sections");
                break;
            }
            sb.AppendLine(line);
        }
        return sb.ToString().TrimEnd();
    }

    private static int CountOccurrences(string text, string term)
    {
        var count = 0;
        for (var i = text.IndexOf(term, StringComparison.Ordinal); i >= 0; i = text.IndexOf(term, i + term.Length, StringComparison.Ordinal))
            count++;
        return count;
    }
}
