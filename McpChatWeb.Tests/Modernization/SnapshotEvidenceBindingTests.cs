using System;
using System.IO;
using System.Text.Json;
using McpChatWeb.Services;
using McpChatWeb.Tests.Services;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

public sealed class SnapshotEvidenceBindingTests : IDisposable
{
    private readonly string _root = Directory.CreateTempSubdirectory("snapshot-evidence-").FullName;

    private (string Folder, string Source, string Parsed, string Facts, string Manifest) Prepare()
    {
        var folder = Path.Combine(_root, "output", "rekt-scans", "native-scan");
        var source = Path.Combine(folder, "inputs", "source");
        var parsed = Path.Combine(folder, "inputs", "parsed-source");
        var facts = Path.Combine(folder, "artifacts");
        foreach (var path in new[] { source, parsed, facts }) Directory.CreateDirectory(path);
        File.WriteAllText(Path.Combine(source, "SELECTED.cbl"), "IDENTIFICATION DIVISION.");
        File.WriteAllText(Path.Combine(source, "COPY-SEARCH-ONLY.cbl"), "IDENTIFICATION DIVISION.");
        var manifest = Path.Combine(folder, "scan.json");
        File.WriteAllText(manifest, JsonSerializer.Serialize(new
        {
            name = "native-scan", sourceRoot = source, parsedSourceRoot = parsed, artifactsRoot = facts,
            expectedPrograms = 1, programs = new[] { new { relativePath = "SELECTED.cbl" } }
        }));
        return (folder, source, parsed, facts, manifest);
    }

    [Fact]
    public void CompletedIngestionBindsFrozenOriginalsAndExactArtifactsInsteadOfDefaultsOrPreparedBytes()
    {
        var paths = Prepare();
        var binding = SnapshotEvidenceBinding.Resolve(_root, paths.Manifest, paths.Parsed, paths.Facts);
        Assert.Equal(paths.Source, binding.SourceRoot);
        Assert.Equal(paths.Facts, binding.FactsRoot);
        Assert.Equal(paths.Manifest, binding.ManifestPath);
        Assert.NotEqual(paths.Parsed, binding.SourceRoot);
        Assert.Equal(new[] { "SELECTED.cbl" }, binding.Programs);
    }

    [Fact]
    public void DifferentGraphIngestionPathsCannotAuthorizeAnotherManifest()
    {
        var paths = Prepare();
        var error = Assert.Throws<InvalidDataException>(() =>
            SnapshotEvidenceBinding.Resolve(_root, paths.Manifest, paths.Source, paths.Facts));
        Assert.Contains("differ from this manifest", error.Message);
    }

    [Fact]
    public void ManifestCannotRedirectAssessmentOutsideItsNamedEvidenceFolder()
    {
        var paths = Prepare();
        File.WriteAllText(paths.Manifest, JsonSerializer.Serialize(new
        {
            name = "native-scan", sourceRoot = _root,
            parsedSourceRoot = paths.Parsed, artifactsRoot = paths.Facts
        }));
        var error = Assert.Throws<InvalidDataException>(() =>
            SnapshotEvidenceBinding.Resolve(_root, paths.Manifest, paths.Parsed, paths.Facts));
        Assert.Contains("escapes", error.Message);
    }

    [Fact]
    public void MissingOriginalSourcePathDoesNotFallBackToCurrentSources()
    {
        var paths = Prepare();
        File.WriteAllText(paths.Manifest, JsonSerializer.Serialize(new
        {
            name = "native-scan", parsedSourceRoot = paths.Parsed, artifactsRoot = paths.Facts
        }));
        var error = Assert.Throws<InvalidDataException>(() =>
            SnapshotEvidenceBinding.Resolve(_root, paths.Manifest, paths.Parsed, paths.Facts));
        Assert.Contains("does not record sourceRoot", error.Message);
    }

    [UnixFact]
    public void LinkedSourceDirectoriesCannotEscapeTheBinding()
    {
        var paths = Prepare();
        var link = Path.Combine(paths.Folder, "linked-source");
        Directory.CreateSymbolicLink(link, _root);
        File.WriteAllText(paths.Manifest, JsonSerializer.Serialize(new
        {
            name = "native-scan", sourceRoot = link,
            parsedSourceRoot = paths.Parsed, artifactsRoot = paths.Facts
        }));
        var error = Assert.Throws<InvalidDataException>(() =>
            SnapshotEvidenceBinding.Resolve(_root, paths.Manifest, paths.Parsed, paths.Facts));
        Assert.Contains("Linked evidence", error.Message);
    }

    public void Dispose() => Directory.Delete(_root, recursive: true);
}
