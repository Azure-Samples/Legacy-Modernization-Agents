using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

// Focused conversion (preview): the converter has no selector awareness, so a narrowed run is
// expressed as a copy of the selected scope that --source is repointed at. doctor.sh and the
// portal share this so both stage the same files for the same selector.
public sealed class ConversionScopeStager
{
    private readonly string _sourceDir;
    private readonly string _stagingDir;

    public ConversionScopeStager(string sourceDir, string stagingDir)
    {
        _sourceDir = sourceDir;
        _stagingDir = stagingDir;
    }

    public ConversionScopeStagingResult Stage(
        ProgramSelection selection,
        string factsDir,
        ILogger? logger = null)
    {
        // Clearing before resolving keeps the previous scope out of the catalog, where its copies
        // would duplicate every basename, and guarantees a refused selector leaves nothing behind.
        if (Directory.Exists(_stagingDir))
            Directory.Delete(_stagingDir, recursive: true);

        var resolver = new ProgramSelectionResolver(
            ProgramSourceCatalog.FromStagingDirectory(_sourceDir),
            new FactsClosureSource(factsDir));

        var result = resolver.Resolve(selection);

        if (result.Programs.Count == 0)
            throw new InvalidOperationException("Conversion selector resolved to no programs.");

        Directory.CreateDirectory(_stagingDir);

        foreach (var relativePath in result.Programs)
        {
            var target = Path.Combine(_stagingDir, relativePath.Replace('/', Path.DirectorySeparatorChar));
            Directory.CreateDirectory(Path.GetDirectoryName(target)!);
            File.Copy(Path.Combine(_sourceDir, relativePath.Replace('/', Path.DirectorySeparatorChar)), target, overwrite: true);
        }

        var copybooks = StageCopybooks();

        foreach (var target in result.UnresolvedCallTargets)
        {
            logger?.LogWarning(
                "[ConversionScope] CALL target {Target} has no program in {SourceDir}; it cannot be converted.",
                target, _sourceDir);
        }

        return new ConversionScopeStagingResult(_stagingDir, result, copybooks);
    }

    // COPY resolves by basename, so copybooks stage flat whatever their source layout.
    private int StageCopybooks()
    {
        // Materialised before copying: staging sits inside the source tree, so a lazy walk would
        // rediscover the copies it just wrote.
        var copybooks = EnumerateVisibleFiles(_sourceDir)
            .Where(path => path.EndsWith(".cpy", StringComparison.OrdinalIgnoreCase))
            .ToList();

        foreach (var copybook in copybooks)
            File.Copy(copybook, Path.Combine(_stagingDir, Path.GetFileName(copybook)), overwrite: true);

        return copybooks.Count;
    }

    // Hidden directories are skipped so a previous staging run cannot contribute duplicates.
    private static IEnumerable<string> EnumerateVisibleFiles(string directory)
    {
        foreach (var file in Directory.EnumerateFiles(directory))
            yield return file;

        foreach (var child in Directory.EnumerateDirectories(directory))
        {
            if (Path.GetFileName(child).StartsWith('.'))
                continue;

            foreach (var file in EnumerateVisibleFiles(child))
                yield return file;
        }
    }
}

public sealed record ConversionScopeStagingResult(
    string StagingDir,
    ProgramSelectionResult Selection,
    int Copybooks)
{
    public IReadOnlyList<string> Programs => Selection.Programs;
}
