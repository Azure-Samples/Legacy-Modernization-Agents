using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Processes;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Moq;
using Xunit;
using ChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Tests.Agents;

[Collection("EnvironmentSensitive")]
public sealed class ChunkConverterProtocolTests : IDisposable
{
    private readonly string _root = Path.GetFullPath(Path.Combine(".test-artifacts", "chunk-protocol-" + Guid.NewGuid()));
    private readonly string? _oldRekt = Environment.GetEnvironmentVariable("ENABLE_REKT_CONTEXT");
    private readonly int? _oldRunId = MetricsSink.CurrentRunId;

    public ChunkConverterProtocolTests()
    {
        Directory.CreateDirectory(_root);
        Environment.SetEnvironmentVariable("ENABLE_REKT_CONTEXT", "false");
        MetricsSink.CurrentRunId = null;
    }

    public void Dispose()
    {
        Environment.SetEnvironmentVariable("ENABLE_REKT_CONTEXT", _oldRekt);
        MetricsSink.CurrentRunId = _oldRunId;
        Directory.Delete(_root, recursive: true);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task ConvertChunkAsync_ActualFirstMiddleLastPromptsProduceLosslessAssembledSource(bool java)
    {
        string[] responses =
        [
            java ? "package bank;\npublic final class Account {\nint total;\nvoid run() {"
                : "namespace Bank;\npublic sealed class Account {\nint total;\nvoid Run() {",
            "total++;\n// braces are not structure: } {",
            "total += 2;\n}\n}"
        ];
        var prompts = new List<string>();
        var converter = CreateConverter(java, responses, prompts);
        var result = new ChunkedFileResult
        {
            SourceFile = Path.Combine(_root, "ACCOUNTS.cbl"),
            Plan = new ChunkingPlan { ChunkCount = 3 },
            Result = new ChunkingResult { TotalChunks = 3, IsConsistent = true }
        };
        for (var i = 0; i < 3; i++)
        {
            var converted = await converter.ConvertChunkAsync(Chunk(i), new ChunkContext { TotalChunks = 3 });
            converted.Success.Should().BeTrue(converted.ErrorMessage);
            result.Result.ChunkResults.Insert(0, new ChunkProcessingResult
            {
                ChunkIndex = i, Success = converted.Success, ConversionResult = converted
            });
        }

        var file = ChunkedOutputAssembler.Assemble(result, java).Should().ContainSingle().Which;

        file.Content.Should().Be(string.Join("\n", responses));
        file.OriginalSourcePath.Should().Be(Path.Combine(_root, "ACCOUNTS.cbl"));
        prompts.Should().HaveCount(3);
        prompts[0].Should().Contain("This is the FIRST chunk").And.Contain("Do NOT close the class");
        prompts[1].Should().Contain("This is a MIDDLE chunk").And.Contain("Never repeat a previous member");
        prompts[2].Should().Contain("This is the LAST chunk").And.Contain("concatenation of all chunks");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task ConvertChunkAsync_SingleChunkRequestsCompleteUnitAndAcceptsOnlyWholeResponseFence(bool java)
    {
        var code = java ? "package bank;\npublic class Account { int total; }"
            : "namespace Bank;\npublic class Account { int total; }";
        var prompts = new List<string>();
        var converter = CreateConverter(java, [$"```{(java ? "java" : "cs")}\n{code}\n```"], prompts);

        var converted = await converter.ConvertChunkAsync(Chunk(0), new ChunkContext { TotalChunks = 1 });

        converted.Success.Should().BeTrue(converted.ErrorMessage);
        converted.ConvertedCode.Should().Be(code);
        prompts.Single().Should().Contain("This is the ONLY chunk").And.NotContain("Do NOT close the class");
        var input = new ChunkedFileResult
        {
            SourceFile = Path.Combine(_root, "ACCOUNTS.cbl"),
            Plan = new ChunkingPlan { ChunkCount = 1 },
            Result = new ChunkingResult
            {
                TotalChunks = 1, IsConsistent = true,
                ChunkResults = [new ChunkProcessingResult { Success = true, ConversionResult = converted }]
            }
        };
        ChunkedOutputAssembler.Assemble(input, java).Should().ContainSingle().Which.Content.Should().Be(code);
    }

    [Fact]
    public async Task SpringChunkUsesSpringPolicyAndRetainsTheChunkProtocol()
    {
        var prompts = new List<string>();
        var converter = CreateConverter(true, ["package bank; public class Account {}"], prompts, "Spring");
        var result = await converter.ConvertChunkAsync(Chunk(0), new ChunkContext { TotalChunks = 1 });
        result.Success.Should().BeTrue(result.ErrorMessage);
        prompts.Single().Should().Contain("COBOL-to-Java Spring Boot conversion specialist")
            .And.Contain("This is the ONLY chunk")
            .And.Contain("A variable CALL is not a verified static target")
            .And.NotContain("Replace all EXEC SQL with Panache repository pattern");
    }

    [Theory]
    [InlineData("strict")]
    [InlineData("partial")]
    public async Task SpringWholeFileAgentReceivesTheSelectedScopePrompt(string dependencyMode)
    {
        var messages = new List<ChatMessage>();
        var client = new Mock<IChatClient>();
        client.Setup(c => c.GetResponseAsync(It.IsAny<IEnumerable<ChatMessage>>(), It.IsAny<ChatOptions>(), It.IsAny<CancellationToken>()))
            .Returns((IEnumerable<ChatMessage> input, ChatOptions? _, CancellationToken _) =>
            {
                messages.AddRange(input);
                return Task.FromResult(new ChatResponse(new ChatMessage(ChatRole.Assistant,
                    "package bank;\npublic class AccountService { public void process() {} }"))
                {
                    FinishReason = ChatFinishReason.Stop,
                    Usage = new UsageDetails { InputTokenCount = 20, OutputTokenCount = 30 }
                });
            });
        var limiter = new Mock<IRateLimiter>();
        limiter.Setup(l => l.AcquireAsync(It.IsAny<int>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(Mock.Of<IRateLimitReservation>());
        var agent = new JavaConverterAgent(client.Object, NullLogger<JavaConverterAgent>.Instance,
            "offline-test", rateLimiter: limiter.Object,
            settings: new AppSettings { ConversionSettings = new() { JavaFramework = "Spring", DependencyMode = dependencyMode } });
        agent.SourceDirectory = _root;
        var file = await agent.ConvertToJavaAsync(new CobolFile
        {
            FileName = "ACCOUNTS.cbl", FilePath = Path.Join(_root, "ACCOUNTS.cbl"),
            Content = "       PROGRAM-ID. ACCOUNTS.\n       PROCEDURE DIVISION.\n           MOVE 0 TO FEJL-RESET.\n           GOBACK."
        }, new CobolAnalysis());
        file.Content.Should().Contain("class AccountService");
        if (dependencyMode == "partial")
            messages.Where(m => m.Role == ChatRole.User).Single().Text.Should().Contain("FEJL-RESET");
        messages.Where(m => m.Role == ChatRole.System).Single().Text.Should()
            .Contain(dependencyMode == "partial" ? "source-isolated COBOL routines" : "COBOL-to-Java Spring Boot conversion specialist")
            .And.NotContain("Java/Quarkus conversion specialist");
        messages.Where(m => m.Role == ChatRole.User).Single().Text.Should()
            .Contain(dependencyMode == "partial" ? "Source-isolated conversion input" : "Convert the following COBOL program to Java with Spring Boot");
    }

    // A refusal carries no package. Both placeholders used to land in one catch-all package,
    // where the program and the copybook of the same stem collided and the run was lost.
    [Fact]
    public async Task RefusedProgramAndSameStemCopybookLandInTheirAssignedPackages()
    {
        var client = new Mock<IChatClient>();
        client.Setup(c => c.GetResponseAsync(It.IsAny<IEnumerable<ChatMessage>>(), It.IsAny<ChatOptions>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new ChatResponse(new ChatMessage(ChatRole.Assistant,
                "Conversion blocked: the runtime contract for RETURN is not supplied."))
            {
                FinishReason = ChatFinishReason.Stop,
                Usage = new UsageDetails { InputTokenCount = 20, OutputTokenCount = 30 }
            });
        var limiter = new Mock<IRateLimiter>();
        limiter.Setup(l => l.AcquireAsync(It.IsAny<int>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(Mock.Of<IRateLimitReservation>());
        var agent = new JavaConverterAgent(client.Object, NullLogger<JavaConverterAgent>.Instance,
            "offline-test", rateLimiter: limiter.Object,
            settings: new AppSettings { ConversionSettings = new() { JavaFramework = "Spring", DependencyMode = "strict" } });
        agent.SourceDirectory = _root;

        var program = await agent.ConvertToJavaAsync(new CobolFile
        {
            FileName = "INQ.cbl", FilePath = Path.Join(_root, "bank", "INQ.cbl"),
            Content = "       PROGRAM-ID. INQ.\n       PROCEDURE DIVISION.\n           GOBACK."
        }, new CobolAnalysis());
        var copybook = await agent.ConvertToJavaAsync(new CobolFile
        {
            FileName = "INQ.cpy", FilePath = Path.Join(_root, "bank", "INQ.cpy"), IsCopybook = true,
            Content = "       01 INQ-AREA PIC X(8)."
        }, new CobolAnalysis());

        program.Content.Should().Contain("CONVERSION DID NOT PRODUCE USABLE");
        program.PackageName.Should().Be(ConversionNamespacePolicy.ForProgram("Java", "bank/INQ.cbl"));
        copybook.PackageName.Should().Be(ConversionNamespacePolicy.ForSharedTypes("Java"));
        program.PackageName.Should().NotBe(copybook.PackageName);
    }

    [Theory]
    [InlineData(false, "blocks")]
    [InlineData(true, "blocks")]
    [InlineData(false, "prose")]
    [InlineData(true, "prose")]
    [InlineData(false, "unusable")]
    [InlineData(true, "unusable")]
    public async Task ConvertChunkAsync_RejectsAmbiguousResponsesAndRetainsRawOutputWithoutSentinels(bool java, string kind)
    {
        var tag = java ? "java" : "csharp";
        var response = kind switch
        {
            "blocks" => $"```{tag}\npublic class Account {{ int first; }}\n```\n```{tag}\npublic class Account {{ int second; }}\n```",
            "prose" => $"Here is the output:\n```{tag}\npublic class Account {{ int first; }}\n```\nImportant missing behavior",
            _ => "The provider returned explanatory text, not code."
        };
        var converter = CreateConverter(java, [response], []);

        var converted = await converter.ConvertChunkAsync(Chunk(0), new ChunkContext { TotalChunks = 1 });

        converted.Success.Should().BeFalse();
        converted.ErrorMessage.Should().NotBeNullOrWhiteSpace();
        converted.ConvertedCode.Should().Be(response);
    }

    private ChunkResult Chunk(int index) => new()
    {
        ChunkIndex = index,
        SourceFile = Path.Combine(_root, "ACCOUNTS.cbl"),
        Content = "       ADD 1 TO TOTAL.",
        StartLine = index + 1,
        EndLine = index + 1
    };

    private IChunkAwareConverter CreateConverter(bool java, string[] responses, List<string> prompts,
        string javaFramework = "Quarkus")
    {
        var next = 0;
        var client = new Mock<IChatClient>(MockBehavior.Strict);
        client.Setup(c => c.GetResponseAsync(It.IsAny<IEnumerable<ChatMessage>>(), It.IsAny<ChatOptions>(), It.IsAny<CancellationToken>()))
            .Returns((IEnumerable<ChatMessage> messages, ChatOptions? _, CancellationToken _) =>
            {
                prompts.Add(string.Join("\n", messages.Where(m => m.Role == ChatRole.System).Select(m => m.Text)));
                return Task.FromResult(new ChatResponse(new ChatMessage(ChatRole.Assistant, responses[next++]))
                {
                    FinishReason = ChatFinishReason.Stop,
                    Usage = new UsageDetails { InputTokenCount = 20, OutputTokenCount = 30 }
                });
            });
        var limiter = new Mock<IRateLimiter>();
        limiter.Setup(l => l.AcquireAsync(It.IsAny<int>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(Mock.Of<IRateLimitReservation>());
        AgentBase agent = java
            ? new ChunkAwareJavaConverter(client.Object, NullLogger<ChunkAwareJavaConverter>.Instance,
                "offline-test", new ConversionSettings { JavaFramework = javaFramework }, rateLimiter: limiter.Object)
            : new ChunkAwareCSharpConverter(client.Object, NullLogger<ChunkAwareCSharpConverter>.Instance,
                "offline-test", new ConversionSettings(), rateLimiter: limiter.Object);
        agent.SourceDirectory = _root;
        return (IChunkAwareConverter)agent;
    }
}
