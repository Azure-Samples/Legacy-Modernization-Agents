using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

public sealed class CompileRepairSourceIndex
{
    private sealed record Source(string Identity, string Content);
    private readonly Dictionary<string, Source> _byGeneratedFile = new(StringComparer.Ordinal);
    internal IReadOnlyDictionary<string, string> UniqueLayouts { get; }
    internal IReadOnlySet<string> AmbiguousNames { get; }

    public CompileRepairSourceIndex(
        string runFolder,
        IReadOnlyList<CodeFile> generatedFiles,
        IReadOnlyList<CobolFile> sourceFiles,
        string? sourceDirectory = null)
    {
        var sources = new Dictionary<string, Source>(StringComparer.Ordinal);
        foreach (var file in sourceFiles)
        {
            var identity = SourceIdentity(
                string.IsNullOrWhiteSpace(file.FilePath) ? file.FileName : file.FilePath, sourceDirectory);
            if (!sources.TryAdd(identity, new Source(identity, file.Content)))
                throw new InvalidOperationException($"Duplicate COBOL source identity '{identity}' in compile repair.");
        }

        var byName = sources.Values.GroupBy(source => NameKey(Path.GetFileNameWithoutExtension(source.Identity))).ToArray();
        UniqueLayouts = byName.Where(group => group.Count() == 1)
            .ToDictionary(group => group.Key, group => group.Single().Content, StringComparer.Ordinal);
        AmbiguousNames = byName.Where(group => group.Count() > 1)
            .Select(group => group.Key).ToHashSet(StringComparer.Ordinal);

        foreach (var file in generatedFiles)
        {
            if (string.IsNullOrWhiteSpace(file.FilePath) || string.IsNullOrWhiteSpace(file.OriginalSourcePath))
                throw new InvalidOperationException("Compile repair requires generated FilePath and OriginalSourcePath.");
            var relative = Path.GetRelativePath(Path.GetFullPath(runFolder), Path.GetFullPath(file.FilePath)).Replace('\\', '/');
            if (relative == ".." || relative.StartsWith("../", StringComparison.Ordinal) || Path.IsPathRooted(relative))
                throw new InvalidOperationException("Compile repair generated FilePath lies outside the run folder.");
            var identity = SourceIdentity(file.OriginalSourcePath, sourceDirectory);
            if (!sources.TryGetValue(identity, out var source))
                throw new InvalidOperationException($"No authoritative COBOL source for generated file '{relative}'.");
            if (!_byGeneratedFile.TryAdd(relative, source))
                throw new InvalidOperationException($"Multiple generated owners for compile repair file '{relative}'.");
        }
    }

    internal CompileRepairTask WithOriginalSource(CompileRepairTask task)
    {
        if (!_byGeneratedFile.TryGetValue(task.File.Replace('\\', '/'), out var source))
            throw new InvalidOperationException($"No authoritative source mapping for repair file '{task.File}'.");
        return task with
        {
            Instructions = task.Instructions.Append(
                $"Original COBOL source: {source.Identity}{Environment.NewLine}```cobol{Environment.NewLine}" +
                source.Content.TrimEnd() + $"{Environment.NewLine}```").ToArray()
        };
    }

    private static string SourceIdentity(string path, string? sourceDirectory)
    {
        if (string.IsNullOrWhiteSpace(path))
            throw new InvalidOperationException("Compile repair source identity is missing.");
        path = path.Replace('\\', '/');
        return Path.IsPathRooted(path)
            ? Path.GetFullPath(path).Replace('\\', '/')
            : sourceDirectory is not null
                ? Path.GetFullPath(Path.Combine(sourceDirectory, path)).Replace('\\', '/')
                : SourcePathHelper.NormalizeRelativePath(path);
    }

    internal static string NameKey(string name) =>
        new(name.Where(char.IsLetterOrDigit).Select(char.ToLowerInvariant).ToArray());
}
