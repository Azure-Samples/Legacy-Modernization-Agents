using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using CobolToQuarkusMigration.Helpers;
using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Services;

public sealed class PromptStudioWriterTests : IDisposable
{
    private readonly string _promptsDirectory = Path.Combine(Path.GetTempPath(), "prompt-studio-" + Guid.NewGuid().ToString("N"));

    public PromptStudioWriterTests()
    {
        Directory.CreateDirectory(_promptsDirectory);
        foreach (var file in Directory.GetFiles(RepoPromptsDirectory(), "*.md"))
            File.Copy(file, Path.Combine(_promptsDirectory, Path.GetFileName(file)));
    }

    [Fact]
    public void SaveNotes_RefreshesEveryAgentPromptAndKeepsItsPlaceholders()
    {
        var features = new HashSet<string> { "EXEC_SQL", "EXEC_CICS", "CALL_PROGRAM" };
        var files = Directory.GetFiles(_promptsDirectory, "*.md").OrderBy(f => f, StringComparer.Ordinal).ToList();
        Assert.True(files.Count >= 12, $"expected the shipped agent prompts, found {files.Count}");
        var originals = files.ToDictionary(f => f, File.ReadAllText);

        foreach (var file in files)
        {
            var id = Path.GetFileNameWithoutExtension(file);
            var notes = PromptBuilders.BuildCodebaseNotes(id, 32, 140, features, "online-transaction", 48_000);

            var first = PromptStudioWriter.SaveNotes(file, notes);
            var second = PromptStudioWriter.SaveNotes(file, notes);

            Assert.True(first.Saved, $"{id}: {first.Reason}");
            Assert.True(second.Saved, $"{id}: {second.Reason}");
            var written = File.ReadAllText(file);
            Assert.Equal(PromptPlaceholders.In(originals[file]).OrderBy(p => p), PromptPlaceholders.In(written).OrderBy(p => p));
            Assert.Single(written.Split('\n'), l => l.Trim() == PromptStudioNotes.BeginMarker);
            Assert.Contains("embedded SQL (host variables, cursors, SQLCODE checks)", written);
            var original = originals[file];
            var nextSection = original.IndexOf("\n## SECTION: ", original.IndexOf("## SECTION: System", StringComparison.Ordinal), StringComparison.Ordinal);
            var handWrittenSystem = (nextSection < 0 ? original : original[..nextSection]).TrimEnd();
            Assert.StartsWith(handWrittenSystem, written);
            Assert.EndsWith(nextSection < 0 ? "" : original[nextSection..], written);
        }
    }

    [Fact]
    public void SaveNotes_LeavesAPromptWithoutSystemSectionUntouched()
    {
        var file = Path.Combine(_promptsDirectory, "NoSystem.md");
        const string content = "## SECTION: User\n\n{{CobolContent}}\n";
        File.WriteAllText(file, content);

        var result = PromptStudioWriter.SaveNotes(file, "notes");

        Assert.False(result.Saved);
        Assert.Contains("System", result.Reason);
        Assert.Equal(content, File.ReadAllText(file));
    }

    [Theory]
    [InlineData("CSharpConverter", "convert each one with its behaviour intact")]
    [InlineData("CompileRepair", "repair them without changing behaviour")]
    [InlineData("RektContext", "The parser context will mention")]
    [InlineData("DependencyMapper", "Map the dependencies")]
    public void BuildCodebaseNotes_PhrasesTheFeatureListForTheAgentsJob(string promptId, string expected)
    {
        var notes = PromptBuilders.BuildCodebaseNotes(promptId, 1, 1, new HashSet<string> { "FILE_IO" }, "file-processing", 100);

        Assert.Contains(expected, notes);
        Assert.Contains("file I/O (SELECT/ASSIGN, FD records, FILE STATUS)", notes);
    }

    [Fact]
    public void EstateSourceFiles_SkipsScratchCopiesAndNonCobolFiles()
    {
        var sourceDir = Path.Combine(_promptsDirectory, "source");
        void Write(string relative)
        {
            var path = Path.Combine(sourceDir, relative);
            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, "       IDENTIFICATION DIVISION.");
        }
        Write(Path.Combine("demo", "cobol", "PROG1.cbl"));
        Write(Path.Combine("demo", "copy", "REC1.cpy"));
        Write(Path.Combine("demo", "README.md"));
        Write(Path.Combine(".preprocessed", ".previous", "other", "OLDPROG.cbl"));
        Write(Path.Combine("demo", ".preprocessed", "PROG1.cbl"));
        Write(Path.Combine(".rekt-staging", "STAGED.cbl"));
        Write(Path.Combine("demo", ".convert-1", "COPY.cpy"));

        var found = PromptBuilders.EstateSourceFiles(sourceDir)
            .Select(f => Path.GetRelativePath(sourceDir, f).Replace('\\', '/'))
            .OrderBy(f => f, StringComparer.Ordinal);

        Assert.Equal(new[] { "demo/cobol/PROG1.cbl", "demo/copy/REC1.cpy" }, found);
    }

    public void Dispose()
    {
        if (Directory.Exists(_promptsDirectory))
            Directory.Delete(_promptsDirectory, recursive: true);
    }

    private static string RepoPromptsDirectory()
    {
        for (var dir = new DirectoryInfo(AppContext.BaseDirectory); dir != null; dir = dir.Parent)
        {
            var candidate = Path.Combine(dir.FullName, "Agents", "Prompts");
            if (File.Exists(Path.Combine(candidate, "CSharpConverter.md")))
                return candidate;
        }
        throw new DirectoryNotFoundException("Agents/Prompts not found above the test output directory.");
    }
}
