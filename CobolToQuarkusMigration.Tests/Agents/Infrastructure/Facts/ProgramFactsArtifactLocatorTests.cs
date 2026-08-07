using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Facts;

public sealed class ProgramFactsArtifactLocatorTests : IDisposable
{
    private readonly string _root = Path.Combine(
        AppContext.BaseDirectory,
        "test-artifacts",
        $"program-facts-artifacts-{Guid.NewGuid():N}");

    [Fact]
    public void TryLoad_FindsUniqueNestedArtifactByBasename()
    {
        var factsDir = Path.Combine(_root, "facts");
        var factsPath = Path.Combine(factsDir, "finance", "ACCOUNTS.cbl.facts.json");
        Directory.CreateDirectory(Path.GetDirectoryName(factsPath)!);
        File.WriteAllText(factsPath, CreateFactsJson("finance/ACCOUNTS.cbl", "ACCOUNTS.cbl"));

        var facts = ProgramFactsArtifactLocator.TryLoad(factsDir, "ACCOUNTS.cbl");

        facts.Should().NotBeNull();
        facts!.RelativePath.Should().Be("finance/ACCOUNTS.cbl");
    }

    [Fact]
    public void TryLoad_DoesNotGuessAcrossAmbiguousBasenames()
    {
        var factsDir = Path.Combine(_root, "facts");
        var firstPath = Path.Combine(factsDir, "finance", "ACCOUNTS.cbl.facts.json");
        var secondPath = Path.Combine(factsDir, "archive", "ACCOUNTS.cbl.facts.json");
        Directory.CreateDirectory(Path.GetDirectoryName(firstPath)!);
        Directory.CreateDirectory(Path.GetDirectoryName(secondPath)!);
        File.WriteAllText(firstPath, CreateFactsJson("finance/ACCOUNTS.cbl", "ACCOUNTS.cbl"));
        File.WriteAllText(secondPath, CreateFactsJson("archive/ACCOUNTS.cbl", "ACCOUNTS.cbl"));

        var facts = ProgramFactsArtifactLocator.TryLoad(factsDir, "ACCOUNTS.cbl");

        facts.Should().BeNull();
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }

    private static string CreateFactsJson(string relativePath, string basename) =>
        $$"""
        {
          "schemaVersion": 1,
          "identitySchemeVersion": "{{ProgramFacts.CurrentIdentitySchemeVersion}}",
          "basename": "{{basename}}",
          "stem": "{{Path.GetFileNameWithoutExtension(basename)}}",
          "relativePath": "{{relativePath}}",
          "sourceHash": "hash",
          "confidence": 3,
          "warnings": [],
          "preprocessNotes": [],
          "summary": {
            "loc": 1,
            "paragraphs": 0,
            "sections": 0,
            "isCopybook": false,
            "programId": "TEST"
          },
          "io": {
            "files": [],
            "screens": [],
            "dbTables": [],
            "queues": []
          },
          "data": {
            "groups": [],
            "copybooksUsed": []
          },
          "callers": [],
          "callees": [],
          "controlFlow": {
            "entryPoints": [],
            "performChains": [],
            "exits": []
          },
          "externalEffects": []
        }
        """;
}
