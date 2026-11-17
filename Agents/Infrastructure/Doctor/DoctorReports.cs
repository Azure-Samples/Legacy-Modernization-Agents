using System.Globalization;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;
using System.Text.RegularExpressions;
using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Doctor;

// Text doctor.sh prints or parses. Output formats are the ones the shell already reads, so the
// pipe-separated and column layouts must not change without updating doctor.sh.
public static class DoctorReports
{
    public static string MigrationDbPath(string repoRoot, string? configuredPath)
    {
        var path = configuredPath;
        if (string.IsNullOrEmpty(path))
        {
            var config = Path.Join(repoRoot, "Config", "appsettings.json");
            path = File.Exists(config)
                ? JsonNode.Parse(File.ReadAllText(config))?["ApplicationSettings"]?["MigrationDatabasePath"] is JsonValue v
                  && v.TryGetValue<string>(out var configured) && configured.Length > 0 ? configured : "Data/migration.db"
                : "Data/migration.db";
        }
        return Path.GetFullPath(path, Path.GetFullPath(repoRoot));
    }

    // "<id>|<status>|<completed-or-started>" for the newest run, or null when there is none.
    public static string? LatestRun(string dbPath)
    {
        if (!File.Exists(dbPath)) return null;
        using var connection = new SqliteConnection(new SqliteConnectionStringBuilder
        {
            DataSource = dbPath, Mode = SqliteOpenMode.ReadOnly, Pooling = false,
        }.ToString());
        connection.Open();
        using var command = connection.CreateCommand();
        command.CommandText = "SELECT id, status, coalesce(completed_at, started_at) FROM runs ORDER BY started_at DESC LIMIT 1";
        using var reader = command.ExecuteReader();
        if (!reader.Read()) return null;
        string Text(int i) => reader.IsDBNull(i) ? "" : Convert.ToString(reader.GetValue(i), CultureInfo.InvariantCulture)!;
        return $"{Text(0)}|{Text(1)}|{Text(2)}";
    }

    // Depth first with each folder's files before its subfolders, both in ordinal order, so the
    // parse order is stable across platforms. Symlinked folders are listed but not entered.
    public static IEnumerable<string> SourcePrograms(string root)
    {
        var rootInfo = new DirectoryInfo(root);
        if (!rootInfo.Exists) yield break;
        var pending = new Stack<DirectoryInfo>();
        pending.Push(rootInfo);
        while (pending.Count > 0)
        {
            var dir = pending.Pop();
            foreach (var file in dir.EnumerateFiles().OrderBy(f => f.Name, StringComparer.Ordinal))
                if (file.Name.EndsWith(".cbl", StringComparison.OrdinalIgnoreCase)
                    || file.Name.EndsWith(".cob", StringComparison.OrdinalIgnoreCase))
                    yield return Path.GetRelativePath(rootInfo.FullName, file.FullName).Replace('\\', '/');
            var children = dir.EnumerateDirectories()
                .Where(d => d.Name is not (".rekt-staging" or ".preprocessed") && !d.Name.StartsWith(".convert-", StringComparison.Ordinal))
                .Where(d => d.LinkTarget is null)
                .OrderBy(d => d.Name, StringComparer.Ordinal)
                .ToList();
            for (var i = children.Count - 1; i >= 0; i--) pending.Push(children[i]);
        }
    }

    // A portal response that is not JSON yields nothing: the selection then stays as typed.
    public static IEnumerable<string> CallClosure(string json)
    {
        JsonNode? node;
        try { node = JsonNode.Parse(json); }
        catch (JsonException) { return []; }
        return node is JsonObject obj && obj["callClosure"] is JsonArray closure
            ? closure.Select(p => p is JsonValue v && v.TryGetValue<string>(out var s) ? s : p?.ToJsonString() ?? "")
            : [];
    }

    public static string SliceSelectors(string json)
    {
        var slice = JsonNode.Parse(json) ?? throw new InvalidDataException("the slice is empty");
        return string.Join(',', Strings(slice, "programSelectors").Concat(Strings(slice, "needSelectors")));
    }

    public static string ProgramList(string json)
    {
        var data = JsonNode.Parse(json) as JsonObject ?? throw new InvalidDataException("the program list is not an object");
        var text = new StringBuilder();
        text.Append($"{"PROGRAM",-24}{"LINES",8}  {"PARSE",-10}{"CALLS",6}{"CALLED BY",11}  PATH\n");
        text.Append(new string('-', 96)).Append('\n');
        foreach (var program in data["programs"] as JsonArray ?? [])
        {
            var missing = program?["missingCopybookCount"] is JsonValue m && m.GetValue<long>() != 0
                ? $"  ⚠ {m.GetValue<long>()} missing cpy" : "";
            text.Append($"{Required(program, "basename"),-24}{Required(program, "linesOfCode"),8}  {Required(program, "parseFidelity"),-10}")
                .Append($"{Required(program, "callClosureCount"),6}{Required(program, "calledByCount"),11}  {Required(program, "relativePath")}{missing}\n");
        }
        text.Append('\n');
        text.Append($"{Count(data, "totalPrograms")} programs, {Count(data, "totalCopybooks")} copybooks.\n");
        text.Append("Copybooks are always included in a conversion and are not selected directly.\n");
        return text.ToString();
    }

    // Per job, what it still needs before it runs end to end. selectedJobs narrows it to --job names.
    public static string JclJobSummary(string manifestPath, IEnumerable<string> selectedJobs)
    {
        var entries = (JsonNode.Parse(File.ReadAllText(manifestPath)) as JsonArray
                       ?? throw new InvalidDataException("the jobs manifest is not a list")).Select(e => e!).ToList();
        var typed = new Dictionary<string, string>(StringComparer.Ordinal);
        foreach (var name in selectedJobs.Select(n => n.Trim()).Where(n => n.Length > 0))
            typed[name.ToLowerInvariant()] = name;
        var text = new StringBuilder();
        if (typed.Count > 0)
        {
            var known = entries.SelectMany(JobNames).ToHashSet(StringComparer.Ordinal);
            foreach (var name in typed.Keys.Where(n => !known.Contains(n)).Order(StringComparer.Ordinal))
                text.Append($"⚠️  No JCL job is named {typed[name]}.\n");
            entries = entries.Where(e => JobNames(e).Any(typed.ContainsKey)).ToList();
        }

        var ready = 0;
        text.Append('\n');
        text.Append($"🧾 JCL jobs ({entries.Count}): see {manifestPath}\n");
        foreach (var entry in entries)
        {
            var job = (string)Required(entry, "job");
            var missing = Strings(entry, "programsMissing").ToList();
            var blocked = (entry["stepsNotRunnable"] as JsonArray)?.Count ?? 0;
            var programs = (entry["programs"] as JsonArray)?.Count ?? 0;
            if (missing.Count == 0 && blocked == 0)
            {
                ready++;
                text.Append($"  ✅ {job,-10} ")
                    .Append(programs > 0 ? $"all {programs} program(s) converted" : "runs utilities only, every step runnable").Append('\n');
                continue;
            }
            var parts = new List<string>
            {
                programs > 0
                    ? $"{(entry["programsImplemented"] as JsonArray)?.Count ?? 0}/{programs} program(s) converted"
                      + (missing.Count > 0 ? $", not yet: {string.Join(", ", missing)}" : "")
                    : "no resolved estate programs",
            };
            if (blocked > 0)
                parts.Add($"{blocked} step(s) cannot run (missing procedure, unsupported utility or unreadable condition)");
            text.Append($"  ⚠️  {job,-10} {string.Join("; ", parts)}\n");
        }
        text.Append($"  {ready} of {entries.Count} job(s) have every program and step they need.\n");
        if (ready < entries.Count)
            text.Append("  A missing procedure or INCLUDE member is named in the manifest's diagnostics: add it to the source.\n");
        text.Append("  Utilities other than IEFBR14 and IDCAMS (a sort, a Db2 unload) stop the job until implemented: docs/jcl-jobs.md\n");
        return text.ToString();
    }

    // A repeated job name runs as NAME_2; the member name is what the source folder shows.
    private static IEnumerable<string> JobNames(JsonNode entry)
    {
        var job = ((string)Required(entry, "job")).ToLowerInvariant();
        var file = (string)Required(entry, "file");
        var member = file[(file.LastIndexOf('/') + 1)..];
        var dot = member.LastIndexOf('.');
        if (dot > 0) member = member[..dot];
        return [job, Regex.Replace(job, @"_\d+$", ""), member.ToLowerInvariant()];
    }

    private static IEnumerable<string> Strings(JsonNode node, string key) =>
        (node[key] as JsonArray ?? []).Select(v => v!.GetValue<string>());

    private static object Required(JsonNode? node, string key) =>
        node?[key] is JsonValue value
            ? value.TryGetValue<string>(out var s) ? s : value.TryGetValue<long>(out var l) ? l : value.ToJsonString()
            : throw new InvalidDataException($"missing '{key}'");

    private static long Count(JsonObject data, string key) =>
        data[key] is JsonValue v && v.TryGetValue<long>(out var n) ? n : 0;
}
