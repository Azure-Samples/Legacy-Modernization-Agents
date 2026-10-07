using System.CommandLine;
using System.Globalization;
using System.Text.Json;
using CobolToQuarkusMigration.Estate;

namespace CobolToQuarkusMigration.Cli;

public static class EstateGraphCommand
{
    public static Command Build()
    {
        var cmd = new Command("estate-graph",
            "Build the deterministic estate graph (programs, copybooks, jobs, data, clusters, waves) and write it as JSON.");

        var sourceArg = new Argument<string>("source-dir", "Directory containing the COBOL sources.");
        cmd.AddArgument(sourceArg);

        var jclOption = new Option<string?>("--jcl-source", "Directory containing the JCL. Defaults to the source directory.");
        cmd.AddOption(jclOption);

        var outputOption = new Option<string?>("--output", "Output file. Defaults to <cwd>/output/estate/estate-graph.json.");
        cmd.AddOption(outputOption);

        var configOption = new Option<string?>("--config", "appsettings.json holding the EstateGraph section. Defaults to Config/appsettings.json.");
        cmd.AddOption(configOption);

        var sliceOption = new Option<string?>("--slice", "Print the conversion slice of one cluster (e.g. C01) instead of a summary.");
        cmd.AddOption(sliceOption);

        cmd.SetHandler((string sourceDir, string? jclDir, string? output, string? config, string? slice) =>
        {
            Environment.ExitCode = Run(sourceDir, jclDir, output, config, slice, Console.Out, Console.Error);
        }, sourceArg, jclOption, outputOption, configOption, sliceOption);

        return cmd;
    }

    public static int Run(string sourceDir, string? jclDir, string? output, string? config, string? slice, TextWriter stdout, TextWriter stderr)
    {
        if (!Directory.Exists(sourceDir))
        {
            stderr.WriteLine($"Source dir not found: {sourceDir}");
            return 2;
        }
        if (jclDir is not null && !Directory.Exists(jclDir))
        {
            stderr.WriteLine($"JCL dir not found: {jclDir}");
            return 2;
        }

        var options = EstateGraphOptions.Load(config ?? Path.Join(Directory.GetCurrentDirectory(), "Config", "appsettings.json"), out var warning);
        if (warning is not null) stderr.WriteLine($"estate-graph: {warning}");

        var graph = EstateGraphBuilder.Build(sourceDir, jclDir, options);
        var path = output ?? Path.Join(Directory.GetCurrentDirectory(), "output", "estate", "estate-graph.json");
        Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath(path))!);
        File.WriteAllText(path, JsonSerializer.Serialize(graph, EstateGraph.JsonOptions));

        if (slice is not null)
        {
            var s = EstateAnalysis.Slice(graph, slice);
            if (s is null)
            {
                stderr.WriteLine($"No cluster '{slice}'. Clusters: {string.Join(", ", graph.Clusters.Select(c => c.Id))}");
                return 1;
            }
            stdout.WriteLine(JsonSerializer.Serialize(s, EstateGraph.JsonOptions));
            return 0;
        }

        stderr.WriteLine($"estate-graph: wrote {path}");
        stdout.WriteLine(string.Join("  ", graph.Counts.Select(kv => $"{kv.Key}={kv.Value}")));
        foreach (var wave in graph.Waves)
        {
            stdout.WriteLine($"Wave {wave.Number}:");
            foreach (var id in wave.Clusters)
            {
                var c = graph.Clusters.First(x => x.Id == id);
                stdout.WriteLine(string.Create(CultureInfo.InvariantCulture, $"  {c.Id,-10} score {c.CarveScore,5:0.0}  {c.Programs.Count,3} program(s)  {c.Label}") +
                                 (c.DependsOn.Count > 0 ? $"  needs {string.Join(",", c.DependsOn)}" : "") +
                                 (c.Missing.Count > 0 ? $"  missing {c.Missing.Count}" : ""));
            }
            foreach (var cycle in wave.Cycles) stdout.WriteLine($"  cycle: {string.Join(" <-> ", cycle)}");
        }
        // The same missing procedure or INCLUDE shows up at every use; one line per distinct message.
        var notes = graph.Diagnostics
            .GroupBy(d => System.Text.RegularExpressions.Regex.Replace(d, @"^\S+:\d+\s+", ""))
            .OrderByDescending(g => g.Count()).ThenBy(g => g.Key, StringComparer.Ordinal).ToList();
        foreach (var g in notes)
            stderr.WriteLine(g.Count() > 1 ? $"  note: {g.Key} ({g.Count()} places)" : $"  note: {g.First()}");
        return 0;
    }
}
