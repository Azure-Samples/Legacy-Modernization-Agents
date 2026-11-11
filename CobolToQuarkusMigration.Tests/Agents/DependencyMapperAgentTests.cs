using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

public sealed class DependencyMapperAgentTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Directory.GetCurrentDirectory(), ".test-artifacts", $"dependency-mapper-{Guid.NewGuid():N}");

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public async Task DiagramRendersOnlySuppliedEdgesWithoutCallingModel(bool enableAiGeneration)
    {
        using var client = new RecordingClient();
        var agent = CreateAgent(client);
        var map = new DependencyMap
        {
            CopybookUsage = new() { ["A-B.cbl"] = ["BOOK.cpy"], ["A_B.cbl"] = [] },
            Dependencies =
            [
                new() { SourceFile = "A-B.cbl", TargetFile = "BOOK.cpy", DependencyType = "COPY" },
                new() { SourceFile = "A_B.cbl", TargetFile = "B\"<end>.cbl", DependencyType = "CALL" },
                new() { SourceFile = "A_B.cbl", TargetFile = "TABLE", DependencyType = "EXEC SQL" }
            ]
        };

        var diagram = await agent.GenerateMermaidDiagramAsync(map, enableAiGeneration);
        map.Dependencies.Reverse();
        var reordered = await agent.GenerateMermaidDiagramAsync(map, !enableAiGeneration);

        client.Calls.Should().Be(0);
        diagram.Should().Be(reordered);
        diagram.Should().Contain("[\"A-B.cbl\"]").And.Contain("[\"A_B.cbl\"]")
            .And.Contain("B&quot;&lt;end&gt;.cbl")
            .And.Contain("-->|\"CALL\"|").And.Contain("-->|\"EXEC SQL\"|");
        diagram.Split('\n').Count(line => line.Contains("-->")).Should().Be(3);
        map.Dependencies.Should().HaveCount(3);
    }

    [Fact]
    public async Task VerifiedExportSuppliesEdgesAndAiCannotAddDependencyFacts()
    {
        var source = Source("billing/MAIN.cbl", "       COPY PARSED.\n           DISPLAY 'RAW-SENTINEL'.");
        var book = Source("books/PARSED.cpy", "       01 PARSED-RECORD PIC X.");
        DependencyEvidenceFixture.Write(_root, Path.Combine(_root, "source"), "billing/MAIN.cbl",
            new DependencyDefinition
            {
                Kind = "COPY", Name = "PARSED.cpy", Path = book.FilePath,
                Sha256 = DependencyEvidenceFixture.Hash(book.FilePath)
            });
        using var client = new RecordingClient();
        var map = await CreateAgent(client).AnalyzeDependenciesAsync([source], []);

        map.CopybookUsage["billing/MAIN.cbl"].Should().Equal("PARSED.cpy");
        map.Dependencies.Should().ContainSingle(d => d.DependencyType == "COPY")
            .Which.TargetFile.Should().Be("PARSED.cpy");
        map.Dependencies.Single().LineNumber.Should().Be(0);
        map.Dependencies.Should().NotContain(d => d.TargetFile.Contains("SCANNED") || d.TargetFile.Contains("INVENTED"));
        map.MermaidDiagram.Should().NotContain("INVENTED");
        map.EvidenceDiagnostics.Should().Contain(d => d.Contains("current source/export/definition hashes verified"));
        map.AnalysisInsights.Should().Contain("AI advisory commentary (not dependency facts)");
        client.Calls.Should().Be(1, "only advisory commentary, never Mermaid, calls the model");
        client.SystemPrompt.Should().Contain("Do not discover, add, remove or infer dependency edges");
        client.UserPrompt.Should().Contain("Evidence diagnostics:").And.NotContain("RAW-SENTINEL");
    }

    [Fact]
    public async Task MissingEvidenceBlocksBeforeCommentaryOrScannerFallback()
    {
        using var client = new RecordingClient();
        var analyze = () => CreateAgent(client).AnalyzeDependenciesAsync(
        [
            Source("MAIN.cbl", "       COPY BOOK.\n           CALL 'STATIC'.\n           CALL WS-TARGET USING X.")
        ], []);

        await analyze.Should().ThrowAsync<InvalidDataException>().WithMessage("*missing dependency-evidence.json*");
        client.Calls.Should().Be(0);
    }

    [Fact]
    public async Task EmptyHighConfidenceCopyFactsAreRejectedBeforeModelCall()
    {
        WriteFacts("MAIN.cbl", [], []);
        using var client = new RecordingClient();
        var analyze = () => CreateAgent(client).AnalyzeDependenciesAsync(
            [Source("MAIN.cbl", "       COPY BOOK.")], []);

        await analyze.Should().ThrowAsync<InvalidDataException>().WithMessage("*legacy High confidence is not dependency coverage*");
        client.Calls.Should().Be(0);
    }

    [Fact]
    public async Task CalledProgramsSubtreeIsSupportingEvidenceNotInventedRootRelationships()
    {
        var a = Source("A.cbl", "           CALL 'B'.");
        var b = Source("B.cbl", "           CALL 'C'.\n       COPY Q.");
        var c = Source("C.cbl", "           STOP RUN.");
        var q = Source("Q.cpy", "       01 Q-RECORD PIC X.");
        DependencyDefinition Definition(string kind, CobolFile file) => new()
        {
            Kind = kind, Name = file.FileName, Path = file.FilePath, Sha256 = DependencyEvidenceFixture.Hash(file.FilePath)
        };
        var sourceRoot = Path.Combine(_root, "source");
        var evidence = DependencyEvidenceFixture.Write(_root, sourceRoot, "A.cbl",
            Definition("CALL", b), Definition("CALL", c), Definition("COPY", q));
        DependencyEvidenceFixture.BindExport(_root, evidence, """
            {"callTarget":{"name":"A.cbl"},"dependencies":[
              {"dependencyKind":"CALL","callTarget":{"name":"B.cbl","programReferenceType":"STATIC"},"dependencies":[
                {"dependencyKind":"CALL","callTarget":{"name":"C.cbl","programReferenceType":"STATIC"},"dependencies":[]},
                {"dependencyKind":"COPY","callTarget":{"name":"Q.cpy"},"dependencies":[]}]}]}
            """);
        DependencyEvidenceFixture.Write(_root, sourceRoot, "B.cbl", Definition("CALL", c), Definition("COPY", q));
        DependencyEvidenceFixture.Write(_root, sourceRoot, "C.cbl");
        using var client = new RecordingClient();
        var map = await CreateAgent(client).AnalyzeDependenciesAsync([a, b, c, q], []);

        map.Dependencies.Where(edge => edge.SourceFile == "A.cbl").Should().ContainSingle()
            .Which.TargetFile.Should().Be("B.cbl");
        map.CopybookUsage["A.cbl"].Should().BeEmpty();
        map.CopybookUsage["B.cbl"].Should().Equal("Q.cpy");
        map.Dependencies.Where(edge => edge.SourceFile == "B.cbl").Select(edge => edge.TargetFile)
            .Should().BeEquivalentTo("C.cbl", "Q.cpy");
        map.CopybookUsage.Should().NotContainKey("Q.cpy");
        map.Dependencies.Should().NotContain(edge => edge.SourceFile == "Q.cpy");
    }

    private DependencyMapperAgent CreateAgent(IChatClient client) => new(
        client, NullLogger<DependencyMapperAgent>.Instance, "isolated-dependency-test-model")
    {
        RepositoryRoot = _root,
        SourceDirectory = Path.Combine(_root, "source")
    };

    private CobolFile Source(string relativePath, string content)
    {
        var path = Path.Combine(_root, "source", relativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, content);
        return new() { FileName = Path.GetFileName(relativePath), FilePath = path, Content = content };
    }

    private void WriteFacts(string relativePath, string[] copybooks, string[] callees)
    {
        var path = Path.Combine(_root, "output", "rekt", relativePath + ".facts.json");
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, JsonSerializer.Serialize(new ProgramFacts
        {
            Basename = Path.GetFileName(relativePath),
            Stem = Path.GetFileNameWithoutExtension(relativePath),
            RelativePath = relativePath,
            SourceHash = "unverified-test-hash",
            Confidence = FactConfidence.High,
            Summary = new ProgramSummary(),
            Data = new DataFacts { CopybooksUsed = copybooks },
            Callees = callees
        }));
    }

    private sealed class RecordingClient : IChatClient
    {
        public int Calls { get; private set; }
        public string? SystemPrompt { get; private set; }
        public string? UserPrompt { get; private set; }

        public Task<ChatResponse> GetResponseAsync(IEnumerable<ChatMessage> messages,
            ChatOptions? options = null, CancellationToken cancellationToken = default)
        {
            Calls++;
            SystemPrompt = messages.First().Text;
            UserPrompt = messages.Last().Text;
            return Task.FromResult(new ChatResponse(new ChatMessage(ChatRole.Assistant,
                "INVENTED.cbl --> INVENTED.cpy")) { FinishReason = ChatFinishReason.Stop });
        }

        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(IEnumerable<ChatMessage> messages,
            ChatOptions? options = null, CancellationToken cancellationToken = default) => throw new NotSupportedException();
        public object? GetService(Type serviceType, object? serviceKey = null) => null;
        public void Dispose() { }
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }
}
