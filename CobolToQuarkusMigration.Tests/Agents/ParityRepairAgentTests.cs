using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

[Collection("EnvironmentSensitive")]
public sealed class ParityRepairAgentTests : IDisposable
{
    private readonly string _root = Path.Combine(Path.GetTempPath(), $"parity-agent-{Guid.NewGuid():N}");
    private readonly string? _originalRoot = Environment.GetEnvironmentVariable("REPO_ROOT");

    [Theory]
    [InlineData(true, true)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    public async Task Repair_UsesAuthoritativeRequestAndRequiresCompleteJsonResponse(bool validJson, bool normalEnd)
    {
        Environment.SetEnvironmentVariable("REPO_ROOT", _root);
        var path = Path.Combine(_root, "Record.java");
        const string source = "       01 RECORD.\n          05 VALUE PIC 9 VALUE 1.";
        const string candidate = "public class Record { int value = 1; }";
        using var client = new ControlledClient(validJson
            ? JsonSerializer.Serialize(new Dictionary<string, string> { [path] = candidate })
            : "not a JSON object", normalEnd);
        var agent = ParityRepairAgent.Create(null, client, NullLogger.Instance, "isolated-parity-model", new AppSettings());
        var result = await agent.RepairAsync(new("RECORD.cpy", "Java", source,
            new Dictionary<string, string> { [path] = "public class Record {}" },
            new ProgramParityResult { Program = "RECORD.cpy", Outcome = ParityOutcome.NotEvaluated }));
        if (validJson && normalEnd)
            result.Should().ContainSingle().Which.Should().Be(new KeyValuePair<string, string>(path, candidate));
        else
            result.Should().BeNull();
        using var request = JsonDocument.Parse(client.UserPrompt!);
        request.RootElement.GetProperty("Source").GetString().Should().Be(source);
        request.RootElement.GetProperty("Files").GetProperty(path).GetString().Should().Be("public class Record {}");
        client.SystemPrompt.Should().Contain("Never invent missing copybook fields");
        File.Exists(path).Should().BeFalse("the transport proposes code but cannot write files");
    }

    private sealed class ControlledClient(string response, bool normalEnd) : IChatClient
    {
        public string? SystemPrompt { get; private set; }
        public string? UserPrompt { get; private set; }

        public Task<ChatResponse> GetResponseAsync(IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default)
        {
            SystemPrompt = messages.First().Text;
            UserPrompt = messages.Last().Text;
            return Task.FromResult(new ChatResponse(new ChatMessage(ChatRole.Assistant, response))
                { FinishReason = normalEnd ? ChatFinishReason.Stop : ChatFinishReason.Length });
        }

        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(IEnumerable<ChatMessage> messages,
            ChatOptions? options = null, CancellationToken cancellationToken = default) => throw new NotSupportedException();
        public object? GetService(Type serviceType, object? serviceKey = null) => null;
        public void Dispose() { }
    }

    public void Dispose()
    {
        Environment.SetEnvironmentVariable("REPO_ROOT", _originalRoot);
        if (Directory.Exists(_root)) Directory.Delete(_root, true);
    }
}
