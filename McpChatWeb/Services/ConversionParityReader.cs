using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Helpers;

namespace McpChatWeb.Services;

public sealed class ConversionParityReader
{
    // The label is the target language; the folder it lives in is configurable, so a custom
    // JAVA_OUTPUT_FOLDER would otherwise leave the panel permanently reporting no data.
    private static readonly (string Target, string EnvVar)[] OutputTargets =
        [("java", "JAVA_OUTPUT_FOLDER"), ("csharp", "CSHARP_OUTPUT_FOLDER")];

    private readonly ILogger<ConversionParityReader> _logger;

    // repoRoot is supplied by tests so a synthetic estate can be read without
    // mutating process-global environment variables.
    public ConversionParityReader(ILogger<ConversionParityReader> logger, string? repoRoot = null)
    {
        _logger = logger;
        RepoRoot = repoRoot ?? ResolveRepoRoot();
    }

    public string RepoRoot { get; }

    private static string ResolveRepoRoot() => RepositoryRoot.Resolve();

    private static string ResolveOutputFolder(string envVar, string target)
    {
        var configured = Environment.GetEnvironmentVariable(envVar)?.Trim().Trim('"');

        // An absolute path cannot be combined with the repo root, and reading outside the repo
        // is not something the portal should do on the strength of an env var.
        return string.IsNullOrEmpty(configured) || Path.IsPathRooted(configured)
            ? Path.Combine("output", target)
            : configured;
    }

    public async Task<ConversionParityEstate> ReadAsync(CancellationToken cancellationToken = default)
    {
        var estate = new ConversionParityEstate();

        foreach (var (folder, envVar) in OutputTargets)
        {
            var relative = ResolveOutputFolder(envVar, folder);

            // A conversion writes to a dated run folder under the language root, so the language
            // root is where to look but not what to read. The portal starts independently of any
            // conversion and cannot be told the timestamp, so the newest run is resolved here.
            relative = ConversionOutputFolder.ResolveLatest(
                RepoRoot, relative, ConversionParityPostPass.ArtifactName);

            var path = Path.Combine(RepoRoot, relative, ConversionParityPostPass.ArtifactName);
            if (!File.Exists(path))
            {
                estate.MissingTargets.Add(folder);
                continue;
            }

            try
            {
                var json = await File.ReadAllTextAsync(path, cancellationToken);
                var report = JsonSerializer.Deserialize<ConversionParityReport>(json);
                if (report is null)
                {
                    estate.UnreadableTargets.Add(folder);
                    continue;
                }

                report.SourcePath = Path.Combine(relative, ConversionParityPostPass.ArtifactName);
                estate.Reports.Add(report);
            }
            catch (Exception ex) when (ex is not OperationCanceledException)
            {
                _logger.LogWarning(ex, "Could not read conversion parity report at {Path}", path);
                estate.UnreadableTargets.Add(folder);
            }
        }

        return estate;
    }
}

public sealed class ConversionParityEstate
{
    public List<ConversionParityReport> Reports { get; init; } = [];

    // Named separately so the portal can distinguish "never converted" from "converted but
    // the report could not be parsed" instead of showing both as an empty panel.
    public List<string> MissingTargets { get; init; } = [];

    public List<string> UnreadableTargets { get; init; } = [];
}
