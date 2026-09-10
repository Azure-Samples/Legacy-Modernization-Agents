using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

namespace McpChatWeb.Services;

// Focused conversion (preview). The portal builds its converter command directly rather than
// going through doctor.sh, so it stages the selected scope here and starts an ordinary run
// against the copy. Staging itself is the shared implementation, so both agree on the scope.
public sealed class ConversionScopeService
{
    public const string StagedSourceFolder = "source/.conversion-staging";

    private readonly string _repoRoot;

    public ConversionScopeService(string repoRoot) => _repoRoot = repoRoot;

    public ConversionScope Stage(
        IReadOnlyList<string> programs,
        bool includeCallers,
        bool includeCallees,
        ILogger? logger = null)
    {
        var selectors = programs
            .Where(program => !string.IsNullOrWhiteSpace(program))
            .Select(program => program.Trim())
            .ToList();

        // An empty selection would stage nothing and silently widen the run to the whole estate.
        if (selectors.Count == 0)
            throw new InvalidOperationException("Select at least one program to convert.");

        var sourceDir = Path.Combine(_repoRoot, "source");
        var stagingDir = Path.Combine(_repoRoot, "source", ".conversion-staging");
        var factsDir = Path.Combine(_repoRoot, "output", "rekt");

        var selection = new ProgramSelection
        {
            Programs = selectors,
            IncludeCallers = includeCallers,
            IncludeCallees = includeCallees,
        };

        var staged = new ConversionScopeStager(sourceDir, stagingDir).Stage(selection, factsDir, logger);

        SelectionManifestWriter.Write(
            Path.Combine(_repoRoot, "output", "conversion-selection.json"),
            stagingDir,
            factsDir,
            selection,
            staged.Selection);

        return new ConversionScope(
            StagedSourceFolder,
            staged.Programs,
            staged.Copybooks,
            staged.Selection.Matches.Select(match => new ConversionScopeMatch(match.Program, match.Reason)).ToList(),
            staged.Selection.UnresolvedCallTargets);
    }
}

public sealed record ConversionScope(
    string SourceFolder,
    IReadOnlyList<string> Programs,
    int Copybooks,
    IReadOnlyList<ConversionScopeMatch> Matches,
    IReadOnlyList<string> UnresolvedCallTargets);

public sealed record ConversionScopeMatch(string Program, string Reason);
