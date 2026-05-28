using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Facts;

/// <summary>
/// Tests <see cref="ProgramFactsExtractor"/> against a synthetic on-disk REKT
/// output dir. Avoids any docker / smojol dependency by writing the exact
/// flow-ast / flow-data / *-deps JSONs the loader expects.
/// </summary>
public class ProgramFactsExtractorTests : IDisposable
{
    private readonly string _root;
    private readonly string _stagingDir;
    private readonly string _rektDir;
    private readonly string _outputDir;

    public ProgramFactsExtractorTests()
    {
        _root = Path.Combine(Path.GetTempPath(), $"pr3-{Guid.NewGuid():N}");
        _stagingDir = Path.Combine(_root, "stage");
        _rektDir = Path.Combine(_root, "output", "rekt");
        _outputDir = Path.Combine(_root, "facts");
        Directory.CreateDirectory(_stagingDir);
        Directory.CreateDirectory(_rektDir);
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }

    [Fact]
    public async Task Extract_PopulatesRequiredFields()
    {
        // Source bytes that drive heuristics (file IO + PROGRAM-ID extraction).
        File.WriteAllText(Path.Combine(_stagingDir, "PROG.cbl"),
            "       IDENTIFICATION DIVISION.\n" +
            "       PROGRAM-ID. PROG.\n" +
            "       PROCEDURE DIVISION.\n" +
            "           OPEN INPUT CUSTFILE.\n" +
            "           READ CUSTFILE.\n" +
            "           CLOSE CUSTFILE.\n" +
            "           CALL 'CHILD'.\n" +
            "           GOBACK.\n");

        var extractor = new ProgramFactsExtractor(_root, _stagingDir, _rektDir, scanCache: null, logger: NullLogger.Instance);
        var written = await extractor.ExtractAllAsync(new[] { "PROG.cbl" }, _outputDir);

        written.Should().Be(1);
        var path = Path.Combine(_outputDir, "PROG.facts.json");
        File.Exists(path).Should().BeTrue();

        var facts = JsonSerializer.Deserialize<ProgramFacts>(File.ReadAllText(path));
        facts.Should().NotBeNull();
        facts!.Basename.Should().Be("PROG.cbl");
        facts.Stem.Should().Be("PROG");
        facts.SchemaVersion.Should().Be(1);
        facts.IdentitySchemeVersion.Should().Be("v1-basename");
        facts.SourceHash.Should().NotBeNullOrEmpty();
        facts.Summary.ProgramId.Should().Be("PROG");
        facts.Io.Files.Should().Contain(f => f.Name == "CUSTFILE");
        facts.Io.Files[0].Operations.Should().Contain("OPEN");
        facts.ExternalEffects.Should().Contain("FILE_IO");
        facts.ControlFlow.Exits.Should().Contain("GOBACK");
        facts.Warnings.Should().Contain(w => w.StartsWith("rekt-output-empty"),
            "no flow-ast/flow-data files were planted → expect explicit warning");
    }

    [Fact]
    public async Task Extract_RespectsCorpusInverseForCallers()
    {
        File.WriteAllText(Path.Combine(_stagingDir, "PARENT.cbl"),
            "       PROGRAM-ID. PARENT.\n           CALL 'CHILD'.\n");
        File.WriteAllText(Path.Combine(_stagingDir, "CHILD.cbl"),
            "       PROGRAM-ID. CHILD.\n");

        // Plant minimal *-deps.json for PARENT so RektContextLoader sees the call.
        File.WriteAllText(Path.Combine(_rektDir, "PARENT-deps.json"),
            "{\"dependencies\":[{\"name\":\"CHILD\",\"type\":\"CALL\"}]}");

        var extractor = new ProgramFactsExtractor(_root, _stagingDir, _rektDir);
        await extractor.ExtractAllAsync(new[] { "PARENT.cbl", "CHILD.cbl" }, _outputDir);

        var childFacts = JsonSerializer.Deserialize<ProgramFacts>(
            File.ReadAllText(Path.Combine(_outputDir, "CHILD.facts.json")));
        childFacts!.Callers.Should().Contain("PARENT.cbl",
            "the inverse callers map should be built from the corpus-wide callee scan");
    }

    [Fact]
    public async Task Extract_PicksUpPreprocessNotes_WhenSidecarPresent()
    {
        File.WriteAllText(Path.Combine(_stagingDir, "X.cbl"),
            "       PROGRAM-ID. X.\n");

        // Simulate a PR5-style preprocess sidecar that PR3 reads opportunistically.
        File.WriteAllText(Path.Combine(_stagingDir, "X.cbl.preprocess.json"),
            "{\"schemaVersion\":1,\"transforms\":[" +
            "{\"rule\":\"move-zero\",\"line\":42,\"before\":\"MOVE 0(1)\",\"after\":\"MOVE ZERO\"}" +
            "]}");

        var extractor = new ProgramFactsExtractor(_root, _stagingDir, _rektDir);
        await extractor.ExtractAllAsync(new[] { "X.cbl" }, _outputDir);

        var facts = JsonSerializer.Deserialize<ProgramFacts>(
            File.ReadAllText(Path.Combine(_outputDir, "X.facts.json")));
        facts!.PreprocessNotes.Should().HaveCount(1);
        facts.PreprocessNotes[0].Rule.Should().Be("move-zero");
        facts.PreprocessNotes[0].Line.Should().Be(42);
    }

    [Fact]
    public async Task Extract_FlagsCicsAsNotYetExtracted()
    {
        File.WriteAllText(Path.Combine(_stagingDir, "C.cbl"),
            "       PROGRAM-ID. C.\n" +
            "           EXEC CICS SEND MAP('SCREEN1') END-EXEC.\n");

        var extractor = new ProgramFactsExtractor(_root, _stagingDir, _rektDir);
        await extractor.ExtractAllAsync(new[] { "C.cbl" }, _outputDir);

        var facts = JsonSerializer.Deserialize<ProgramFacts>(
            File.ReadAllText(Path.Combine(_outputDir, "C.facts.json")));
        facts!.Warnings.Should().Contain(w => w.StartsWith("cics-detected-screens-not-extracted"));
        facts.ExternalEffects.Should().Contain("CICS");
        facts.Io.Screens.Should().BeEmpty("PR3 does not extract screens; warning makes this explicit");
    }

    [Fact]
    public async Task Extract_WhenSourceMissing_EmitsExplicitWarning()
    {
        // Don't write the source file — only the extraction target.
        var extractor = new ProgramFactsExtractor(_root, _stagingDir, _rektDir);
        await extractor.ExtractAllAsync(new[] { "MISSING.cbl" }, _outputDir);

        var facts = JsonSerializer.Deserialize<ProgramFacts>(
            File.ReadAllText(Path.Combine(_outputDir, "MISSING.facts.json")));
        facts!.SourceHash.Should().BeEmpty();
        facts.Warnings.Should().Contain(w => w.StartsWith("source-not-found"));
    }

    [Fact]
    public async Task Extract_SameInput_DeterministicSourceHash()
    {
        File.WriteAllText(Path.Combine(_stagingDir, "D.cbl"),
            "       PROGRAM-ID. D.\n");

        var extractor = new ProgramFactsExtractor(_root, _stagingDir, _rektDir);
        await extractor.ExtractAllAsync(new[] { "D.cbl" }, _outputDir);
        var first = JsonSerializer.Deserialize<ProgramFacts>(File.ReadAllText(Path.Combine(_outputDir, "D.facts.json")));

        await extractor.ExtractAllAsync(new[] { "D.cbl" }, _outputDir);
        var second = JsonSerializer.Deserialize<ProgramFacts>(File.ReadAllText(Path.Combine(_outputDir, "D.facts.json")));

        second!.SourceHash.Should().Be(first!.SourceHash, "the source-bytes hash must be deterministic");
    }
}
