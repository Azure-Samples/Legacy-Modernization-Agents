using System;
using System.IO;
using System.Linq;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

// The portal narrows a conversion the same way doctor.sh does: stage the selected scope, then
// point the run at the copy. These pin the portal's half of that contract.
public sealed class ConversionScopeServiceTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(),
        $"portal-scope-{Guid.NewGuid():N}");

    [Fact]
    public void StagesTheSelectionAndReturnsARepoRelativeSourceFolder()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/LEDGER.cbl");

        var scope = Service().Stage(new[] { "finance/ACCOUNTS.cbl" }, false, false);

        Assert.Equal("source/.conversion-staging", scope.SourceFolder);
        Assert.Equal(new[] { "finance/ACCOUNTS.cbl" }, scope.Programs.ToArray());
        Assert.True(File.Exists(Path.Combine(_root, "source", ".conversion-staging", "finance", "ACCOUNTS.cbl")));
    }

    // ProcessManager rejects traversal and odd characters, then prefixes "./" when building the
    // command, so an absolute path would silently become ".//abs/path" and point nowhere.
    [Fact]
    public void StagedSourceFolderUsesForwardSlashesAndNoTraversal()
    {
        WriteProgram("finance/ACCOUNTS.cbl");

        var scope = Service().Stage(new[] { "finance/ACCOUNTS.cbl" }, false, false);

        Assert.False(Path.IsPathRooted(scope.SourceFolder));
        Assert.DoesNotContain("..", scope.SourceFolder);
        Assert.DoesNotContain('\\', scope.SourceFolder);
        Assert.Matches(@"^[a-zA-Z0-9_\-./]+$", scope.SourceFolder);
    }

    [Fact]
    public void ClosureStagesTheCalleeAlongsideTheSelectedProgram()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("shared/UTILITY.cbl");
        WriteFacts("finance/ACCOUNTS.cbl", callees: new[] { "UTILITY" });
        WriteFacts("shared/UTILITY.cbl");

        var scope = Service().Stage(new[] { "finance/ACCOUNTS.cbl" }, includeCallers: false, includeCallees: true);

        Assert.Contains("shared/UTILITY.cbl", scope.Programs);
    }

    [Fact]
    public void AmbiguousSelectorIsRefusedRatherThanResolvedToOneOfTheCandidates()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/ACCOUNTS.cbl");

        var refuse = () => Service().Stage(new[] { "ACCOUNTS.cbl" }, false, false);

        var error = Assert.Throws<InvalidOperationException>(refuse);
        Assert.Contains("ACCOUNTS.cbl", error.Message);
    }

    [Fact]
    public void EmptySelectionIsRefusedBecauseItWouldConvertTheWholeEstate()
    {
        WriteProgram("finance/ACCOUNTS.cbl");

        Assert.Throws<InvalidOperationException>(
            () => Service().Stage(Array.Empty<string>(), false, false));
    }

    private ConversionScopeService Service() => new(_root);

    private void WriteProgram(string relativePath)
    {
        var path = Path.Combine(_root, "source", relativePath.Replace('/', Path.DirectorySeparatorChar));
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, "       IDENTIFICATION DIVISION.\n");
    }

    private void WriteFacts(string relativePath, string[]? callees = null)
    {
        var stem = Path.GetFileNameWithoutExtension(relativePath);
        var facts = new ProgramFacts
        {
            Basename = Path.GetFileName(relativePath),
            Stem = stem,
            RelativePath = relativePath,
            SourceHash = "test",
            Confidence = FactConfidence.High,
            Summary = new ProgramSummary { ProgramId = stem },
            Callees = callees ?? Array.Empty<string>(),
        };

        // Serialized from the real type through the real locator so the fixture cannot
        // drift from the artefact format the resolver actually reads.
        var path = ProgramFactsArtifactLocator.GetFactsFilePath(
            Path.Combine(_root, "output", "rekt"), relativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, JsonSerializer.Serialize(facts));
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
