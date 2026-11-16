using System.CommandLine;
using System.Text.Json;
using CobolToQuarkusMigration.Persistence;
using CobolToQuarkusMigration.Persistence.GraphIngest;
using Neo4j.Driver;

namespace CobolToQuarkusMigration.Cli;

// doctor.sh runs this after every REKT parse; it needs Neo4j but no AI settings.
public static class GraphIngestCommand
{
    public static Command Build()
    {
        var cmd = new Command("graph-ingest",
            "Load parsed REKT output, JCL job facts and optionally a migration SQLite database into Neo4j.");

        var sourceOption = new Option<string>("--source-dir", () => "source", "COBOL source directory.");
        var outputOption = new Option<string>("--rekt-output", () => Path.Join("output", "rekt"), "REKT JSON output directory.");
        var runOption = new Option<long>("--run-id", "Run ID the graph nodes are scoped to.") { IsRequired = true };
        var sqliteOption = new Option<string?>("--sqlite-db", "Optional SQLite migration database to copy into the graph.");
        var scanNameOption = new Option<string?>("--scan-name", "Name a fresh REKT scan without replacing an existing run.");
        var manifestOption = new Option<string?>("--scan-manifest",
            "Named scan's scan.json (default: scan.json beside the --rekt-output directory).");
        var dryRunOption = new Option<bool>("--dry-run",
            "Print the nodes and relationships as JSON instead of writing them; needs no Neo4j.");
        foreach (var option in new Option[] { sourceOption, outputOption, runOption, sqliteOption, scanNameOption, manifestOption, dryRunOption })
        {
            cmd.AddOption(option);
        }

        cmd.SetHandler(async context =>
        {
            var parse = context.ParseResult;
            var options = new GraphIngestOptions(
                parse.GetValueForOption(sourceOption)!, parse.GetValueForOption(outputOption)!, parse.GetValueForOption(runOption),
                parse.GetValueForOption(sqliteOption), parse.GetValueForOption(scanNameOption), parse.GetValueForOption(manifestOption));
            context.ExitCode = await RunAsync(options, parse.GetValueForOption(dryRunOption), Console.Out, Console.Error);
        });
        return cmd;
    }

    public static async Task<int> RunAsync(GraphIngestOptions options, bool dryRun, TextWriter stdout, TextWriter stderr)
    {
        // Checked before connecting so a failed JCL refresh never reaches the graph.
        if (GraphIngestion.JclScanFailed(options.RektOutput))
        {
            stderr.WriteLine($"graph-ingest: {GraphIngestion.JclScanFailedMessage}");
            return 1;
        }

        if (dryRun)
        {
            var recording = new RecordingGraphTarget();
            try
            {
                await new GraphIngestion(recording, stderr).RunAsync(options);
            }
            catch (Exception ex) when (ex is GraphIngestException or IOException or JsonException or InvalidDataException)
            {
                stderr.WriteLine($"graph-ingest: {ex.Message}");
                return 1;
            }
            stdout.WriteLine(JsonSerializer.Serialize(Plan(recording)));
            return 0;
        }

        var password = Environment.GetEnvironmentVariable("NEO4J_PASSWORD");
        if (string.IsNullOrEmpty(password))
        {
            stderr.WriteLine("graph-ingest: NEO4J_PASSWORD is required. Run ./doctor.sh setup and configure Config/ai-config.local.env.");
            return 1;
        }
        var uri = Environment.GetEnvironmentVariable("NEO4J_URI") is { Length: > 0 } configured
            ? configured
            : $"bolt://localhost:{(Environment.GetEnvironmentVariable("NEO4J_BOLT_PORT") is { Length: > 0 } port ? port : "7687")}";
        var user = Environment.GetEnvironmentVariable("NEO4J_USER") is { Length: > 0 } u ? u : "neo4j";

        await using var driver = GraphDatabase.Driver(uri, AuthTokens.Basic(user, password));
        try
        {
            await new GraphIngestion(new Neo4jGraphTarget(driver), stdout).RunAsync(options);
            return 0;
        }
        catch (Exception ex) when (ex is AuthenticationException or ServiceUnavailableException)
        {
            stderr.WriteLine($"graph-ingest: {Neo4jConnection.Describe(ex, uri)}");
            return 1;
        }
        catch (Exception ex) when (ex is GraphIngestException or Neo4jException or IOException or JsonException or InvalidDataException
                                       or Microsoft.Data.Sqlite.SqliteException)
        {
            stderr.WriteLine($"graph-ingest: {ex.Message}");
            return 1;
        }
    }

    private static object Plan(RecordingGraphTarget recording)
    {
        static object? KeyOf(RecordedNode node) =>
            node.Properties.TryGetValue("uid", out var uid) ? uid : node.Properties.GetValueOrDefault("id");
        return new
        {
            nodes = recording.Nodes.Select(n => new { label = n.Label, properties = n.Properties }),
            relationships = recording.Relationships.Select(r => new
            {
                type = r.Type,
                fromLabel = r.From.Label,
                from = KeyOf(r.From),
                toLabel = r.To.Label,
                to = KeyOf(r.To),
                properties = r.Properties,
            }),
        };
    }
}
