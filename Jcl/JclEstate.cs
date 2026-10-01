using System.Text.Json;
using System.Text.Json.Serialization;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Jcl;

public sealed record JclJobFacts
{
    public const int CurrentSchemaVersion = 1;

    public int SchemaVersion { get; init; } = CurrentSchemaVersion;
    public required JclJob Job { get; init; }

    // Every program the job runs, utilities excluded, in step order.
    public required IReadOnlyList<string> Programs { get; init; }
    public required IReadOnlyList<string> Upstream { get; init; }
    public required IReadOnlyList<string> Downstream { get; init; }
}

// Parses every job under a source root and writes one facts file per job, plus the estate's dataset lineage.
public static class JclEstate
{
    public const string LineageFileName = "jcl-lineage.json";
    public const string FactsSuffix = ".job.json";

    public static readonly JsonSerializerOptions JsonOptions = new()
    {
        WriteIndented = true,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
        Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
    };

    public static IReadOnlyList<JclJob> Parse(string sourceRoot)
    {
        var parser = new JclParser(JclMemberLibrary.FromDirectory(sourceRoot));
        var jobs = new List<JclJob>();
        foreach (var file in SourceTypeRegistry.EnumerateJclFiles(sourceRoot)
                     .Distinct(StringComparer.OrdinalIgnoreCase)
                     .OrderBy(f => f, StringComparer.OrdinalIgnoreCase))
        {
            string text;
            try { text = File.ReadAllText(file); }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException) { continue; }
            if (JclParser.IsMember(text)) continue;
            jobs.Add(parser.Parse(text, Path.GetRelativePath(sourceRoot, file).Replace('\\', '/')));
        }

        return jobs;
    }

    public static IReadOnlyList<string> Programs(JclJob job) =>
        job.Steps.SelectMany(s => s.ProgramsRun).Distinct(StringComparer.OrdinalIgnoreCase).ToList();

    public static IReadOnlyList<JclJobFacts> Facts(IReadOnlyList<JclJob> jobs, JclEstateLineage lineage) =>
        jobs.Select(job => new JclJobFacts
        {
            Job = job,
            Programs = Programs(job),
            Upstream = lineage.Dependencies.Where(d => d.Downstream == job.Name).Select(d => d.Upstream).ToList(),
            Downstream = lineage.Dependencies.Where(d => d.Upstream == job.Name).Select(d => d.Downstream).ToList(),
        }).ToList();

    public static string FactsPath(string outputDir, string relativePath) =>
        Path.Join(outputDir, relativePath + FactsSuffix);

    // Writes the facts and returns how many jobs were written.
    public static int Write(string sourceRoot, string outputDir)
    {
        var jobs = Parse(sourceRoot);
        var lineage = JclEstateLineage.Build(jobs);
        foreach (var facts in Facts(jobs, lineage))
        {
            var path = FactsPath(outputDir, facts.Job.File);
            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, JsonSerializer.Serialize(facts, JsonOptions));
        }

        Directory.CreateDirectory(outputDir);
        File.WriteAllText(Path.Join(outputDir, LineageFileName), JsonSerializer.Serialize(lineage, JsonOptions));
        return jobs.Count;
    }
}
