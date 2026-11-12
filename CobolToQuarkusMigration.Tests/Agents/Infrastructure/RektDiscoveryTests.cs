using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure;

// Discovery happens three times in this pipeline and only the converters read the parser's output.
// The dependency mapper matches COPY and EXEC SQL with regular expressions; a regex cannot see a
// COPY nested inside a copybook, and records the word EXEC where the parse records the real verb.
// Three mechanisms that never compare answers is how two sources come to disagree with nobody
// noticing.
public sealed class RektDiscoveryTests : IDisposable
{
    private readonly string _factsDir = Path.Join(
        Path.GetTempPath(), "rekt-discovery-" + Guid.NewGuid().ToString("N"));

    public RektDiscoveryTests() => Directory.CreateDirectory(_factsDir);

    public void Dispose()
    {
        try { Directory.Delete(_factsDir, recursive: true); }
        catch (IOException ex) { Console.Error.WriteLine($"Could not remove {_factsDir}: {ex.Message}"); }
    }

    private void WriteFacts(
        string basename,
        int confidence,
        string[]? copybooks = null,
        string[]? callees = null,
        string[]? tables = null)
    {
        string List(string[]? items) =>
            items is null ? "" : string.Join(", ", items.Select(i => $"\"{i}\""));

        string Tables(string[]? items) =>
            items is null ? "" : string.Join(", ",
                items.Select(i => $$"""{"name": "{{i}}", "operations": ["SELECT"]}"""));

        var json = $$"""
        {
          "schemaVersion": {{ProgramFacts.CurrentSchemaVersion}},
          "identitySchemeVersion": "{{ProgramFacts.CurrentIdentitySchemeVersion}}",
          "basename": "{{basename}}",
          "stem": "{{Path.GetFileNameWithoutExtension(basename)}}",
          "relativePath": "src/{{basename}}",
          "sourceHash": "hash",
          "confidence": {{confidence}},
          "warnings": [],
          "preprocessNotes": [],
          "summary": { "loc": 1, "paragraphs": 0, "sections": 0, "isCopybook": false, "programId": "T" },
          "io": { "files": [], "screens": [], "dbTables": [{{Tables(tables)}}], "queues": [] },
          "data": { "groups": [], "copybooksUsed": [{{List(copybooks)}}] },
          "callers": [],
          "callees": [{{List(callees)}}],
          "controlFlow": { "entryPoints": [], "performChains": [], "exits": [] },
          "externalEffects": []
        }
        """;

        File.WriteAllText(Path.Join(_factsDir, basename + ".facts.json"), json);
    }

    private RektDiscovery Discovery() => new(_factsDir);

    private static List<string> NeverCalled() =>
        throw new InvalidOperationException("the text scan should not have been consulted");

    // The whole point: a full parse answers, and the expensive scan is never paid for.
    [Fact]
    public void AFullParseAnswersWithoutConsultingTheTextScan()
    {
        WriteFacts("ORDER.cbl", (int)FactConfidence.High, copybooks: ["CUSTREC", "ORDREC"]);

        var result = Discovery().Copybooks("ORDER.cbl", NeverCalled);

        result.FromParse.Should().BeTrue();
        result.Value.Should().BeEquivalentTo("CUSTREC", "ORDREC");
    }

    // Where a copybook was missing the parser worked from a stub, so its structural claims are
    // inference with better presentation. That must not silently outrank a plain scan.
    [Theory]
    [InlineData(FactConfidence.Partial)]
    [InlineData(FactConfidence.Low)]
    [InlineData(FactConfidence.None)]
    public void ADegradedParseYieldsToTheTextScan(FactConfidence confidence)
    {
        WriteFacts("ORDER.cbl", (int)confidence, copybooks: ["FROM-PARSE"]);

        var result = Discovery().Copybooks("ORDER.cbl", () => ["FROM-SCAN"]);

        result.FromParse.Should().BeFalse();
        result.Value.Should().BeEquivalentTo("FROM-SCAN");
    }

    [Fact]
    public void AProgramWithNoParseOutputFallsBack()
    {
        var result = Discovery().Copybooks("NEVER-PARSED.cbl", () => ["FROM-SCAN"]);

        result.FromParse.Should().BeFalse();
        result.Value.Should().BeEquivalentTo("FROM-SCAN");
    }

    // "This program COPYs nothing" is a real answer when the parse was complete enough to say so,
    // and must not be mistaken for "the parse knows nothing".
    [Fact]
    public void AnEmptyResultFromAFullParseIsAnAnswerNotAGap()
    {
        WriteFacts("LEAF.cbl", (int)FactConfidence.High, copybooks: []);

        var result = Discovery().Copybooks("LEAF.cbl", NeverCalled);

        result.FromParse.Should().BeTrue();
        result.Value.Should().BeEmpty();
    }

    [Fact]
    public void CalleesComeFromTheParseToo()
    {
        WriteFacts("ORDER.cbl", (int)FactConfidence.High, callees: ["PRICING", "TAX"]);

        Discovery().Callees("ORDER.cbl", NeverCalled).Value
            .Should().BeEquivalentTo("PRICING", "TAX");
    }

    // The regex records the word EXEC; the parse records the table.
    [Fact]
    public void TablesComeFromTheParseToo()
    {
        WriteFacts("ORDER.cbl", (int)FactConfidence.High, tables: ["CUSTOMER_TBL", "ORDER_TBL"]);

        Discovery().DbTables("ORDER.cbl", NeverCalled).Value
            .Should().BeEquivalentTo("CUSTOMER_TBL", "ORDER_TBL");
    }

    [Fact]
    public void DuplicatesAndBlanksAreRemoved()
    {
        WriteFacts("ORDER.cbl", (int)FactConfidence.High, copybooks: ["CUSTREC", "custrec", "", "  "]);

        Discovery().Copybooks("ORDER.cbl", NeverCalled).Value.Should().ContainSingle();
    }

    [Fact]
    public void AProgramIsReadFromDiskOnlyOnce()
    {
        WriteFacts("ORDER.cbl", (int)FactConfidence.High, copybooks: ["CUSTREC"]);
        var discovery = Discovery();

        discovery.Copybooks("ORDER.cbl", NeverCalled);
        File.Delete(Path.Join(_factsDir, "ORDER.cbl.facts.json"));

        discovery.Copybooks("ORDER.cbl", NeverCalled).FromParse.Should().BeTrue();
    }

    [Fact]
    public void APathIsAcceptedWhereABasenameIsExpected()
    {
        WriteFacts("ORDER.cbl", (int)FactConfidence.High, copybooks: ["CUSTREC"]);

        Discovery().Copybooks("src/billing/ORDER.cbl", NeverCalled).FromParse.Should().BeTrue();
    }

    [Fact]
    public void WithNoParseOutputAtAllEverythingFallsBack()
    {
        var discovery = new RektDiscovery(Path.Join(_factsDir, "absent"));

        discovery.Available.Should().BeFalse();
        discovery.Copybooks("ORDER.cbl", () => ["FROM-SCAN"]).FromParse.Should().BeFalse();
    }

    // Which source answered has to be visible, or this silently becomes a third mechanism that
    // nobody can tell apart from the other two.
    [Fact]
    public void TheRunReportsHowMuchCameFromTheParse()
    {
        WriteFacts("PARSED.cbl", (int)FactConfidence.High, copybooks: ["A"]);
        var discovery = Discovery();

        discovery.Copybooks("PARSED.cbl", NeverCalled);
        discovery.Copybooks("UNPARSED.cbl", () => ["B"]);

        discovery.AnsweredByParse.Should().Be(1);
        discovery.AnsweredByTextScan.Should().Be(1);
        discovery.Summarise().Should().Contain("1 of 2").And.Contain("50%");
    }

    [Fact]
    public void WithNothingDiscoveredTheSummarySaysSo()
    {
        Discovery().Summarise().Should().Contain("No dependency discovery");
    }
}
