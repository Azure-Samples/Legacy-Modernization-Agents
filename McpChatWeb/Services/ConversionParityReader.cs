using System.Text.Json;
using CobolToQuarkusMigration.Agents;

namespace McpChatWeb.Services;

public sealed class ConversionParityReader
{
    private static readonly string[] OutputFolders = ["java", "csharp"];

    private readonly ILogger<ConversionParityReader> _logger;

    // repoRoot is supplied by tests so a synthetic estate can be read without
    // mutating process-global environment variables.
    public ConversionParityReader(ILogger<ConversionParityReader> logger, string? repoRoot = null)
    {
        _logger = logger;
        RepoRoot = repoRoot ?? ResolveRepoRoot();
    }

    public string RepoRoot { get; }

    private static string ResolveRepoRoot()
    {
        var envRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        if (!string.IsNullOrEmpty(envRoot) && Directory.Exists(envRoot)) return envRoot;

        var dir = new DirectoryInfo(Directory.GetCurrentDirectory());
        while (dir != null && !File.Exists(Path.Combine(dir.FullName, "doctor.sh")))
            dir = dir.Parent;
        return dir?.FullName ?? Directory.GetCurrentDirectory();
    }

    public async Task<ConversionParityEstate> ReadAsync(CancellationToken cancellationToken = default)
    {
        var estate = new ConversionParityEstate();

        foreach (var folder in OutputFolders)
        {
            var path = Path.Combine(RepoRoot, "output", folder, ConversionParityPostPass.ArtifactName);
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

                report.SourcePath = Path.Combine("output", folder, ConversionParityPostPass.ArtifactName);
                estate.Reports.Add(report);
            }
            catch (Exception ex)
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
