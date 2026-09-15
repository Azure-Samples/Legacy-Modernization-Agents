using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

namespace McpChatWeb.Services;

// Focused conversion (preview). The portal builds its converter command directly rather than
// going through doctor.sh, so it stages the selected scope here and starts an ordinary run
// against the copy. Staging itself is the shared implementation, so both agree on the scope.
public sealed class ConversionScopeService
{
    private const string StagingRoot = "source/.conversion-staging";

    private readonly string _repoRoot;

    public ConversionScopeService(string repoRoot) => _repoRoot = repoRoot;

    public ConversionScope Stage(
        IReadOnlyList<string> programs,
        bool includeCallers,
        bool includeCallees,
        IReadOnlyCollection<string>? activeSourceFolders = null,
        ILogger? logger = null)
    {
        var selectors = programs
            .Where(program => !string.IsNullOrWhiteSpace(program))
            .Select(program => program.Trim())
            .ToList();

        // An empty selection would stage nothing and silently widen the run to the whole estate.
        if (selectors.Count == 0)
            throw new InvalidOperationException("Select at least one program to convert.");

        ReclaimAbandonedScopes(activeSourceFolders ?? Array.Empty<string>());

        // Runs are not serialised, so each focused conversion gets its own scope: staging over a
        // live run would delete the tree its converter is still reading through --source.
        var scopeId = Guid.NewGuid().ToString("N")[..8];
        var sourceFolder = $"{StagingRoot}/{scopeId}";

        var sourceDir = Path.Combine(_repoRoot, "source");
        var stagingDir = Path.Combine(_repoRoot, StagingRoot.Replace('/', Path.DirectorySeparatorChar), scopeId);
        var factsDir = Path.Combine(_repoRoot, "output", "rekt");
        var manifestPath = Path.Combine(_repoRoot, "output", "conversion-selection", $"{scopeId}.json");

        var selection = new ProgramSelection
        {
            Programs = selectors,
            IncludeCallers = includeCallers,
            IncludeCallees = includeCallees,
        };

        var staged = new ConversionScopeStager(sourceDir, stagingDir).Stage(selection, factsDir, logger);

        SelectionManifestWriter.Write(manifestPath, sourceDir, factsDir, selection, staged.Selection);

        return new ConversionScope(
            sourceFolder,
            manifestPath,
            staged.Programs,
            staged.Copybooks,
            staged.Selection.Matches.Select(match => new ConversionScopeMatch(match.Program, match.Reason)).ToList(),
            staged.Selection.UnresolvedCallTargets);
    }

    // Manifests are the record of what each run converted, so only the staged copies are reclaimed.
    private void ReclaimAbandonedScopes(IReadOnlyCollection<string> activeSourceFolders)
    {
        var stagingRoot = Path.Combine(_repoRoot, StagingRoot.Replace('/', Path.DirectorySeparatorChar));
        if (!Directory.Exists(stagingRoot))
            return;

        var inUse = activeSourceFolders
            .Select(folder => folder.TrimEnd('/').Split('/').Last())
            .ToHashSet(StringComparer.Ordinal);

        foreach (var scopeDir in Directory.EnumerateDirectories(stagingRoot))
        {
            if (inUse.Contains(Path.GetFileName(scopeDir)))
                continue;

            try
            {
                Directory.Delete(scopeDir, recursive: true);
            }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException)
            {
                // A scope still open elsewhere is left for the next reclaim rather than failing this run.
            }
        }
    }
}

public sealed record ConversionScope(
    string SourceFolder,
    string ManifestPath,
    IReadOnlyList<string> Programs,
    int Copybooks,
    IReadOnlyList<ConversionScopeMatch> Matches,
    IReadOnlyList<string> UnresolvedCallTargets);

public sealed record ConversionScopeMatch(string Program, string Reason);
