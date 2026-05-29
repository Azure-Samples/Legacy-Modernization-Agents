using System.Globalization;
using System.Text.Json;
using System.Text.RegularExpressions;
using Microsoft.Data.Sqlite;

namespace McpChatWeb.Services;

/// <summary>
/// REKT-driven business-capability classifier + service locator.
///
/// <para>
/// Two responsibilities (kept in one service because they share the same
/// keyword/lexical scan over source + facts.json):
/// </para>
///
/// <para>
/// 1. <b>Classify</b> each COBOL program into one or more business
///    capabilities (fraud, gambling, KYC, …) using a deterministic keyword
///    dictionary (<c>Data/capabilities.json</c>). No LLM cost. Multi-label.
///    Weights: paragraph names ×3 · CALL targets ×2 · SQL tables ×2 ·
///    data groups ×2 · copybook names ×1 · raw paragraph headers ×3.
///    Confidence = min(1, totalScore / 8).
/// </para>
///
/// <para>
/// 2. <b>Locate</b> generated Java/C# services (e.g. "CALC_INTEREST" or
///    "CalcInterestService") back to the originating COBOL program and the
///    matching paragraph inside its facts.json — so a user can jump from
///    the generated code back into REKT/AST. Supports both target
///    languages: scans output/runs/**, output/java/**, output/csharp/**.
/// </para>
///
/// <para>
/// Both endpoints are read-only and fail-soft. Dictionary is reloaded on
/// every request — edit and refresh.
/// </para>
/// </summary>
public sealed class CapabilityClassifier
{
    private readonly string _repoRoot;
    private readonly ILogger<CapabilityClassifier> _logger;

    public CapabilityClassifier(IConfiguration config, ILogger<CapabilityClassifier> logger)
    {
        _repoRoot = ResolveRepoRoot(config);
        _logger = logger;
    }

    private static string ResolveRepoRoot(IConfiguration config)
    {
        var envRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        if (!string.IsNullOrEmpty(envRoot) && Directory.Exists(envRoot)) return envRoot;
        var dir = new DirectoryInfo(Directory.GetCurrentDirectory());
        while (dir != null && !File.Exists(Path.Combine(dir.FullName, "doctor.sh"))) dir = dir.Parent;
        return dir?.FullName ?? Directory.GetCurrentDirectory();
    }

    // ─────────────────────────────────────────────────────────────────────
    // Capability classification
    // ─────────────────────────────────────────────────────────────────────

    public CapabilityCatalog GetCapabilities()
    {
        var dict = LoadDictionary();
        var catalog = new CapabilityCatalog();
        foreach (var c in dict) catalog.Capabilities.Add(new CapabilityBucket(c));

        var sourceDir = Path.Combine(_repoRoot, "source");
        var factsDir = Path.Combine(_repoRoot, "output", "rekt");
        if (!Directory.Exists(sourceDir)) return catalog;

        var cblFiles = Directory.EnumerateFiles(sourceDir, "*.cbl", SearchOption.AllDirectories)
            .Where(f => !f.Contains("/.convert-", StringComparison.Ordinal)
                     && !f.Contains("/.rekt-staging", StringComparison.Ordinal)
                     && !f.Contains("/.preprocessed", StringComparison.Ordinal));

        foreach (var cbl in cblFiles)
        {
            var basename = Path.GetFileName(cbl);
            var stem = Path.GetFileNameWithoutExtension(cbl);
            var factsPath = Path.Combine(factsDir, $"{stem}.facts.json");

            var signals = ExtractSignals(cbl, factsPath);
            var scores = ScoreAgainstDictionary(signals, dict);

            // Record top scores (>0) on the relevant buckets
            var anyScored = false;
            foreach (var (capId, score, hits) in scores)
            {
                if (score <= 0) continue;
                anyScored = true;
                var bucket = catalog.Capabilities.First(b => b.Id == capId);
                bucket.Programs.Add(new CapabilityProgram(
                    Basename: basename,
                    Score: score,
                    Confidence: Math.Min(1.0, score / 8.0),
                    Hits: hits.Take(8).ToList()));
            }
            if (!anyScored)
            {
                catalog.Unclassified.Add(basename);
            }
        }

        // Sort programs inside each bucket by score desc
        foreach (var b in catalog.Capabilities)
        {
            b.Programs.Sort((a, x) => x.Score.CompareTo(a.Score));
        }
        catalog.Capabilities.Sort((a, b) => b.Programs.Count.CompareTo(a.Programs.Count));
        catalog.Unclassified.Sort(StringComparer.OrdinalIgnoreCase);
        return catalog;
    }

    /// <summary>Lexical signals extracted from one COBOL program.</summary>
    private record ProgramSignals(
        List<string> ParagraphsFromFacts,
        List<string> ParagraphsFromSource,
        List<string> CallTargets,
        List<string> DataGroups,
        List<string> Copybooks,
        List<string> SqlTables);

    private ProgramSignals ExtractSignals(string cblPath, string factsPath)
    {
        var paragraphsFacts = new List<string>();
        var callees = new List<string>();
        var groups = new List<string>();
        var copybooks = new List<string>();
        var sqlTables = new List<string>();

        if (File.Exists(factsPath))
        {
            try
            {
                using var doc = JsonDocument.Parse(File.ReadAllText(factsPath));
                var root = doc.RootElement;
                if (root.TryGetProperty("controlFlow", out var cf))
                {
                    if (cf.TryGetProperty("performChains", out var pc) && pc.ValueKind == JsonValueKind.Array)
                        foreach (var chain in pc.EnumerateArray())
                            if (chain.ValueKind == JsonValueKind.Array)
                                foreach (var p in chain.EnumerateArray())
                                    paragraphsFacts.Add(p.ToString());
                    if (cf.TryGetProperty("entryPoints", out var ep) && ep.ValueKind == JsonValueKind.Array)
                        foreach (var p in ep.EnumerateArray())
                            paragraphsFacts.Add(p.ToString());
                }
                if (root.TryGetProperty("callees", out var c) && c.ValueKind == JsonValueKind.Array)
                    foreach (var x in c.EnumerateArray()) callees.Add(x.ToString());
                if (root.TryGetProperty("data", out var d))
                {
                    if (d.TryGetProperty("groups", out var gs) && gs.ValueKind == JsonValueKind.Array)
                        foreach (var g in gs.EnumerateArray())
                            if (g.TryGetProperty("name", out var n)) groups.Add(n.ToString());
                    if (d.TryGetProperty("copybooksUsed", out var cu) && cu.ValueKind == JsonValueKind.Array)
                        foreach (var x in cu.EnumerateArray()) copybooks.Add(x.ToString());
                }
                if (root.TryGetProperty("io", out var io))
                {
                    if (io.TryGetProperty("dbTables", out var tb) && tb.ValueKind == JsonValueKind.Array)
                        foreach (var x in tb.EnumerateArray()) sqlTables.Add(x.ToString());
                }
            }
            catch { /* fail-soft */ }
        }

        // Also scan the .cbl source directly for paragraph headers + COPY +
        // CALL — captures programs where facts.json is incomplete or absent.
        var paragraphsSource = new List<string>();
        try
        {
            var text = File.ReadAllText(cblPath);
            foreach (Match m in Regex.Matches(text,
                @"^\s*([A-Z0-9][A-Z0-9\-_]{2,40})\s*\.\s*$",
                RegexOptions.Multiline | RegexOptions.IgnoreCase))
            {
                var label = m.Groups[1].Value.Trim();
                // Skip obviously-non-paragraph tokens
                if (label.Length < 4) continue;
                if (label == "EXIT" || label == "END-EXEC" || label == "PROGRAM-ID") continue;
                paragraphsSource.Add(label);
            }
            foreach (Match m in Regex.Matches(text,
                @"\bCOPY\s+([A-Z0-9$@#\-_]+)",
                RegexOptions.IgnoreCase))
            {
                copybooks.Add(m.Groups[1].Value);
            }
            foreach (Match m in Regex.Matches(text,
                @"\bCALL\s+['""]([A-Z0-9$@#\-_]+)['""]",
                RegexOptions.IgnoreCase))
            {
                callees.Add(m.Groups[1].Value);
            }
            foreach (Match m in Regex.Matches(text,
                @"\b(?:FROM|INTO|UPDATE|JOIN)\s+([A-Z][A-Z0-9_]{2,30})\b",
                RegexOptions.IgnoreCase))
            {
                sqlTables.Add(m.Groups[1].Value);
            }
        }
        catch { /* fail-soft */ }

        return new ProgramSignals(
            paragraphsFacts.Distinct(StringComparer.OrdinalIgnoreCase).ToList(),
            paragraphsSource.Distinct(StringComparer.OrdinalIgnoreCase).ToList(),
            callees.Distinct(StringComparer.OrdinalIgnoreCase).ToList(),
            groups.Distinct(StringComparer.OrdinalIgnoreCase).ToList(),
            copybooks.Distinct(StringComparer.OrdinalIgnoreCase).ToList(),
            sqlTables.Distinct(StringComparer.OrdinalIgnoreCase).ToList());
    }

    private static bool TokenMatch(string haystack, string needle)
    {
        // Treat hyphens/underscores/digits as token separators so short
        // keywords like "str" don't match inside "TRATAR" or "REPOSICIONAR".
        // For keywords >= 5 chars, allow substring (catches Danish/Spanish
        // variants where the stem appears inside compound names).
        if (string.IsNullOrEmpty(needle)) return false;
        if (needle.Length >= 5)
            return haystack.Contains(needle, StringComparison.OrdinalIgnoreCase);
        // Short keywords: must appear as a whole token.
        var tokens = Regex.Split(haystack, @"[^A-Za-z]+");
        foreach (var t in tokens)
        {
            if (string.Equals(t, needle, StringComparison.OrdinalIgnoreCase)) return true;
        }
        return false;
    }

    private List<(string CapId, double Score, List<CapabilityHit> Hits)>
        ScoreAgainstDictionary(ProgramSignals s, List<CapabilityDef> dict)
    {
        var results = new List<(string, double, List<CapabilityHit>)>();
        foreach (var cap in dict)
        {
            double score = 0;
            var hits = new List<CapabilityHit>();

            foreach (var kw in cap.Keywords)
            {
                foreach (var p in s.ParagraphsFromFacts.Concat(s.ParagraphsFromSource))
                    if (TokenMatch(p, kw))
                    { score += 3; hits.Add(new CapabilityHit("paragraph", p, kw)); }
                foreach (var c in s.CallTargets)
                    if (TokenMatch(c, kw))
                    { score += 2; hits.Add(new CapabilityHit("call", c, kw)); }
                foreach (var t in s.SqlTables)
                    if (TokenMatch(t, kw))
                    { score += 2; hits.Add(new CapabilityHit("sql-table", t, kw)); }
                foreach (var g in s.DataGroups)
                    if (TokenMatch(g, kw))
                    { score += 2; hits.Add(new CapabilityHit("data-group", g, kw)); }
                foreach (var cb in s.Copybooks)
                    if (TokenMatch(cb, kw))
                    { score += 1; hits.Add(new CapabilityHit("copybook", cb, kw)); }
            }
            results.Add((cap.Id, score, hits));
        }
        return results;
    }

    private List<CapabilityDef> LoadDictionary()
    {
        var path = Path.Combine(_repoRoot, "Data", "capabilities.json");
        if (!File.Exists(path)) return new();
        try
        {
            using var doc = JsonDocument.Parse(File.ReadAllText(path));
            var arr = doc.RootElement.GetProperty("capabilities");
            var list = new List<CapabilityDef>();
            foreach (var el in arr.EnumerateArray())
            {
                var keywords = new List<string>();
                if (el.TryGetProperty("keywords", out var k) && k.ValueKind == JsonValueKind.Array)
                    foreach (var x in k.EnumerateArray()) keywords.Add(x.ToString());
                var bian = new List<string>();
                if (el.TryGetProperty("bian", out var b) && b.ValueKind == JsonValueKind.Array)
                    foreach (var x in b.EnumerateArray()) bian.Add(x.ToString());
                list.Add(new CapabilityDef(
                    Id: el.GetProperty("id").GetString() ?? "",
                    Emoji: el.TryGetProperty("emoji", out var e) ? e.GetString() ?? "" : "",
                    Display: el.TryGetProperty("display", out var disp) ? disp.GetString() ?? "" : el.GetProperty("id").GetString() ?? "",
                    Keywords: keywords,
                    Bian: bian));
            }
            return list;
        }
        catch (Exception ex)
        {
            _logger.LogWarning("capabilities.json parse failed: {Msg}", ex.Message);
            return new();
        }
    }

    // ─────────────────────────────────────────────────────────────────────
    // Service Locator — name → Java file + COBOL program + paragraph
    // ─────────────────────────────────────────────────────────────────────

    public LocatorResult Locate(string query)
    {
        var result = new LocatorResult { Query = query };
        if (string.IsNullOrWhiteSpace(query)) return result;

        // Normalize: build several candidate forms
        var raw = query.Trim();
        var noJavaSuffix = Regex.Replace(raw, @"(Service|Handler|Repository|Controller|Manager)$", "", RegexOptions.IgnoreCase);
        var noExt = Regex.Replace(noJavaSuffix, @"\.java$", "", RegexOptions.IgnoreCase);
        var screaming = ToScreamingSnake(noExt);                  // CalcInterest → CALC_INTEREST
        var hyphenated = screaming.Replace('_', '-');             // CALC_INTEREST → CALC-INTEREST
        var lowered = noExt.ToLowerInvariant();
        var forms = new HashSet<string>(StringComparer.OrdinalIgnoreCase) {
            raw, noJavaSuffix, noExt, screaming, hyphenated, lowered
        };

        // 1. Find matching generated-code files under output/runs/** and legacy
        //    output/java/** + output/csharp/**. Restricted to source-code
        //    extensions so we don't surface migration-report.md, logs, etc.
        var javaMatches = new List<JavaFileMatch>();
        var seenJava = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var codeExts = new HashSet<string>(StringComparer.OrdinalIgnoreCase) {
            ".java", ".cs", ".kt", ".ts", ".scala"
        };
        foreach (var rootDir in new[] { Path.Combine(_repoRoot, "output", "runs"),
                                        Path.Combine(_repoRoot, "output", "java"),
                                        Path.Combine(_repoRoot, "output", "csharp") })
        {
            if (!Directory.Exists(rootDir)) continue;
            foreach (var f in Directory.EnumerateFiles(rootDir, "*", SearchOption.AllDirectories))
            {
                if (!codeExts.Contains(Path.GetExtension(f))) continue;
                var name = Path.GetFileNameWithoutExtension(f);
                if (!forms.Any(form => name.Contains(form, StringComparison.OrdinalIgnoreCase))) continue;
                if (!seenJava.Add(f)) continue;
                javaMatches.Add(new JavaFileMatch(
                    Path: Path.GetRelativePath(_repoRoot, f),
                    FileName: Path.GetFileName(f),
                    RunFolder: ExtractRunFolder(f),
                    Language: Path.GetExtension(f).TrimStart('.').ToLowerInvariant()));
            }
        }
        result.JavaMatches = javaMatches.Take(20).ToList();

        // 2. Search COBOL source files for matching paragraph headers
        var cobolMatches = new List<CobolProgramMatch>();
        var sourceDir = Path.Combine(_repoRoot, "source");
        if (Directory.Exists(sourceDir))
        {
            foreach (var cbl in Directory.EnumerateFiles(sourceDir, "*.cbl", SearchOption.AllDirectories)
                .Where(f => !f.Contains("/.convert-", StringComparison.Ordinal)
                         && !f.Contains("/.rekt-staging", StringComparison.Ordinal)
                         && !f.Contains("/.preprocessed", StringComparison.Ordinal)))
            {
                string text;
                try { text = File.ReadAllText(cbl); } catch { continue; }
                var basename = Path.GetFileName(cbl);
                var stem = Path.GetFileNameWithoutExtension(cbl);
                var matchedParagraphs = new List<string>();

                // Look for the screaming/hyphenated forms as paragraph headers
                foreach (var form in new[] { screaming, hyphenated })
                {
                    if (string.IsNullOrEmpty(form)) continue;
                    foreach (Match m in Regex.Matches(text,
                        $@"^\s*({Regex.Escape(form)}[A-Z0-9\-_]*)\s*\.",
                        RegexOptions.Multiline | RegexOptions.IgnoreCase))
                    {
                        matchedParagraphs.Add(m.Groups[1].Value);
                    }
                }

                // Also program-id match
                bool programIdMatch = Regex.IsMatch(text,
                    $@"PROGRAM-ID\.?\s+{Regex.Escape(noExt)}\b",
                    RegexOptions.IgnoreCase);

                // Also match if the COBOL program basename itself contains the query
                bool basenameMatch = forms.Any(f => stem.Contains(f, StringComparison.OrdinalIgnoreCase));

                if (matchedParagraphs.Count == 0 && !programIdMatch && !basenameMatch) continue;

                cobolMatches.Add(new CobolProgramMatch(
                    Basename: basename,
                    RelativePath: Path.GetRelativePath(_repoRoot, cbl),
                    MatchedParagraphs: matchedParagraphs.Distinct(StringComparer.OrdinalIgnoreCase).Take(10).ToList(),
                    ProgramIdMatch: programIdMatch,
                    BasenameMatch: basenameMatch,
                    FactsPath: File.Exists(Path.Combine(_repoRoot, "output", "rekt", $"{stem}.facts.json"))
                        ? Path.Combine("output", "rekt", $"{stem}.facts.json").Replace('\\','/')
                        : null));
            }
        }
        result.CobolMatches = cobolMatches.Take(20).ToList();

        result.Forms = forms.ToList();
        return result;
    }

    private string? ExtractRunFolder(string javaPath)
    {
        // Walks up to find the per-run directory under output/runs/
        var dir = new FileInfo(javaPath).Directory;
        while (dir != null && dir.Parent != null)
        {
            if (string.Equals(dir.Parent.Name, "runs", StringComparison.OrdinalIgnoreCase)
                && string.Equals(dir.Parent.Parent?.Name, "output", StringComparison.OrdinalIgnoreCase))
                return dir.Name;
            dir = dir.Parent;
        }
        return null;
    }

    private static string ToScreamingSnake(string camelOrPascal)
    {
        // CalcInterestService → CALC_INTEREST_SERVICE  ·  cS → handled by uppering
        if (string.IsNullOrEmpty(camelOrPascal)) return "";
        var sb = new System.Text.StringBuilder();
        for (int i = 0; i < camelOrPascal.Length; i++)
        {
            var c = camelOrPascal[i];
            if (i > 0 && char.IsUpper(c) && !char.IsUpper(camelOrPascal[i - 1])) sb.Append('_');
            sb.Append(char.ToUpperInvariant(c));
        }
        return sb.ToString();
    }
}

// ─────────────────────────────────────────────────────────────────────────
// DTOs
// ─────────────────────────────────────────────────────────────────────────

public record CapabilityDef(string Id, string Emoji, string Display, List<string> Keywords, List<string> Bian);

public record CapabilityHit(string Source, string Match, string Keyword);

public record CapabilityProgram(string Basename, double Score, double Confidence, List<CapabilityHit> Hits);

public class CapabilityBucket
{
    public string Id { get; set; }
    public string Emoji { get; set; }
    public string Display { get; set; }
    public List<string> Bian { get; set; }
    public List<CapabilityProgram> Programs { get; } = new();
    public CapabilityBucket(CapabilityDef d)
    {
        Id = d.Id; Emoji = d.Emoji; Display = d.Display; Bian = d.Bian;
    }
}

public class CapabilityCatalog
{
    public List<CapabilityBucket> Capabilities { get; } = new();
    public List<string> Unclassified { get; } = new();
}

public class LocatorResult
{
    public string Query { get; set; } = "";
    public List<string> Forms { get; set; } = new();
    public List<JavaFileMatch> JavaMatches { get; set; } = new();
    public List<CobolProgramMatch> CobolMatches { get; set; } = new();
}

public record JavaFileMatch(string Path, string FileName, string? RunFolder, string Language);

public record CobolProgramMatch(
    string Basename,
    string RelativePath,
    List<string> MatchedParagraphs,
    bool ProgramIdMatch,
    bool BasenameMatch,
    string? FactsPath);
