using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

// Observed on ORDDATAI: one duplicated fragment left a complete file a brace short, three
// continuations appended invented services, and the brace check then stubbed the whole type.
public sealed class ConverterContinuationTests
{
    private const string OneBraceShort = """
        ```csharp
        namespace Modernized.Shared;

        public sealed class Orddatai
        {
            public int A { get => 1 { get => 1; }
            public int B { get; set; }
        }
        ```
        """;

    private sealed class RecordingChatClient(ChatFinishReason? finishReason) : IChatClient
    {
        public List<string> Prompts { get; } = [];

        public Task<ChatResponse> GetResponseAsync(
            IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default)
        {
            Prompts.Add(messages.Last().Text);
            var text = Prompts.Count == 1 ? OneBraceShort : "```csharp\n}\n```";
            return Task.FromResult(new ChatResponse(new ChatMessage(ChatRole.Assistant, text))
            {
                FinishReason = finishReason,
            });
        }

        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(
            IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default) => throw new NotSupportedException();

        public object? GetService(Type serviceType, object? serviceKey = null) => null;

        public void Dispose() { }
    }

    private static Task<CodeFile> Convert(IChatClient client) =>
        new CSharpConverterAgent(client, NullLogger<CSharpConverterAgent>.Instance, "model")
            .ConvertAsync(
                new CobolFile { FileName = "ORDDATAI.cpy", Content = "       01  ORDDATAI-REC.\n           05 A PIC 9.\n" },
                new CobolAnalysis { FileName = "ORDDATAI.cpy", RawAnalysisData = "record" });

    [Fact]
    public async Task AResponseTheModelEndedIsKeptWithoutContinuation()
    {
        var client = new RecordingChatClient(ChatFinishReason.Stop);

        var file = await Convert(client);

        client.Prompts.Should().ContainSingle();
        file.Content.Should().Contain("public sealed class Orddatai").And.NotContain("CONVERSION DID NOT PRODUCE");
    }

    [Fact]
    public async Task AResponseWithoutAnEndSignalIsContinued()
    {
        var client = new RecordingChatClient(finishReason: null);

        var file = await Convert(client);

        client.Prompts.Should().HaveCount(2);
        client.Prompts[1].Should().Contain("truncated mid-output");
        file.Content.Should().NotContain("CONVERSION DID NOT PRODUCE");
    }
}
