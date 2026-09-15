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

        Assert.StartsWith("source/.conversion-staging/", scope.SourceFolder);
        Assert.Equal(new[] { "finance/ACCOUNTS.cbl" }, scope.Programs.ToArray());
        Assert.True(File.Exists(Path.Combine(_root, scope.SourceFolder, "finance", "ACCOUNTS.cbl")));
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

    // Without a manifest a portal-launched focused conversion leaves no record of what was
    // selected, so a partial output cannot be told apart from a deliberately narrow scope.
    [Fact]
    public void RecordsTheSelectionManifestSoAPortalRunIsReproducible()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/LEDGER.cbl");

        var scope = Service().Stage(new[] { "finance/ACCOUNTS.cbl" }, false, false);

        Assert.True(File.Exists(scope.ManifestPath), $"expected a selection manifest at {scope.ManifestPath}");

        using var manifest = JsonDocument.Parse(File.ReadAllText(scope.ManifestPath));
        var root = manifest.RootElement;
        Assert.Equal(
            new[] { "finance/ACCOUNTS.cbl" },
            root.GetProperty("programs").EnumerateArray().Select(p => p.GetString()).ToArray());
        Assert.Equal(
            new[] { "finance/ACCOUNTS.cbl" },
            root.GetProperty("selectors").GetProperty("programs").EnumerateArray().Select(p => p.GetString()).ToArray());
        Assert.False(root.GetProperty("selectors").GetProperty("includeCallees").GetBoolean());

        // resolve-programs records the directory it resolved against. The portal has to record the
        // same thing or the field means one of two directories depending on who wrote the file.
        Assert.Equal(Path.Combine(_root, "source"), root.GetProperty("stagingDir").GetString());
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

    // ProcessManager places no cap on live runs, so a second focused conversion must not stage over
    // the first: the converter is still reading that tree through --source.
    [Fact]
    public void ConcurrentScopesDoNotShareAStagingDirectory()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/LEDGER.cbl");

        var service = Service();
        var first = service.Stage(new[] { "finance/ACCOUNTS.cbl" }, false, false);
        var second = service.Stage(new[] { "archive/LEDGER.cbl" }, false, false, new[] { first.SourceFolder });

        Assert.NotEqual(first.SourceFolder, second.SourceFolder);
        Assert.True(
            File.Exists(Path.Combine(_root, first.SourceFolder, "finance", "ACCOUNTS.cbl")),
            "the first run's staged source must survive a second run being staged");
    }

    [Fact]
    public void ConcurrentScopesKeepSeparateManifests()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/LEDGER.cbl");

        var service = Service();
        var first = service.Stage(new[] { "finance/ACCOUNTS.cbl" }, false, false);
        var second = service.Stage(new[] { "archive/LEDGER.cbl" }, false, false, new[] { first.SourceFolder });

        Assert.NotEqual(first.ManifestPath, second.ManifestPath);
        Assert.Equal(new[] { "finance/ACCOUNTS.cbl" }, ManifestPrograms(first.ManifestPath));
        Assert.Equal(new[] { "archive/LEDGER.cbl" }, ManifestPrograms(second.ManifestPath));
    }

    // Isolated scopes would otherwise accumulate one staged copy of the estate per focused run.
    [Fact]
    public void ReclaimsStagedScopesThatNoLiveRunIsUsing()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/LEDGER.cbl");

        var service = Service();
        var abandoned = service.Stage(new[] { "finance/ACCOUNTS.cbl" }, false, false);
        service.Stage(new[] { "archive/LEDGER.cbl" }, false, false, Array.Empty<string>());

        Assert.False(
            Directory.Exists(Path.Combine(_root, abandoned.SourceFolder)),
            "a scope no live run is using should be reclaimed");
    }

    // The manifest is the record of what a run converted, so reclaiming a scope must not take it.
    [Fact]
    public void ReclaimingAScopeKeepsItsManifest()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/LEDGER.cbl");

        var service = Service();
        var abandoned = service.Stage(new[] { "finance/ACCOUNTS.cbl" }, false, false);
        service.Stage(new[] { "archive/LEDGER.cbl" }, false, false, Array.Empty<string>());

        Assert.True(File.Exists(abandoned.ManifestPath));
    }

    private string?[] ManifestPrograms(string manifestPath)
    {
        using var manifest = JsonDocument.Parse(File.ReadAllText(manifestPath));
        return manifest.RootElement.GetProperty("programs")
            .EnumerateArray().Select(p => p.GetString()).ToArray();
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
