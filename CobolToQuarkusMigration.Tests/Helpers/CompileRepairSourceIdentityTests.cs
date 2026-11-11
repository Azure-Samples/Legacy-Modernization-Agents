using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;
using ChatMessage = Microsoft.Extensions.AI.ChatMessage;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class CompileRepairSourceIdentityTests : IDisposable
{
    private readonly string _root = Path.Combine(AppContext.BaseDirectory, "test-artifacts", "repair-identity-" + Guid.NewGuid().ToString("N"));

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task ActualRepairPromptsUseEachSavedFilesOriginalSourceDespiteIdenticalBasenames(bool reverse)
    {
        var (generated, originals, sources) = await CreateSources();
        if (reverse) originals.Reverse();
        var sourceIndex = new CompileRepairSourceIndex(_root, generated, originals);
        var tasks = CompileRepairPlanner.PlanWithSourceIdentity(
            sources.Keys.Select(file => new CompilerDiagnostic(file, 2, 1, "CS1002", "; expected")).ToArray(),
            GeneratedTypeIndex.FromSources(sources), sources, "Shared", new CompileGateSettings(), sourceIndex);
        tasks.Should().HaveCount(2);
        using var client = new RecordingClient();
        var agent = CompileRepairAgent.Create(null, client, NullLogger.Instance, "identity-test-" + Guid.NewGuid().ToString("N"));
        foreach (var task in tasks)
        {
            client.Response = sources[task.File];
            (await agent.RepairAsync(task, sources[task.File])).Should().Be(sources[task.File]);
            var prompt = client.Prompts[^1];
            var isLeft = task.File.StartsWith("Left/", StringComparison.Ordinal);
            prompt.Should().Contain(isLeft ? "LEFT-ONLY PIC X" : "RIGHT-ONLY PIC 9");
            prompt.Should().NotContain(isLeft ? "RIGHT-ONLY" : "LEFT-ONLY");
            prompt.Should().Contain(isLeft ? originals.Single(file => file.Content.Contains("LEFT-ONLY")).FilePath
                : originals.Single(file => file.Content.Contains("RIGHT-ONLY")).FilePath);
        }
        client.Prompts.Should().HaveCount(2);
    }

    [Fact]
    public async Task MissingTypeWithAmbiguousOriginalStemFailsBeforeAnyLayoutCanBecomeRepairInput()
    {
        var (generated, originals, sources) = await CreateSources();
        var errors = sources.Keys.Select(file => new CompilerDiagnostic(file, 2, 1, "CS0246",
            "The type or namespace name 'Same' could not be found")).ToArray();
        Action plan = () => CompileRepairPlanner.PlanWithSourceIdentity(
            errors, GeneratedTypeIndex.FromSources(sources), sources, "Shared", new CompileGateSettings(),
            new CompileRepairSourceIndex(_root, generated, originals));
        plan.Should().Throw<InvalidOperationException>().WithMessage("*Ambiguous COBOL source*");
    }

    [Fact]
    public async Task UnknownOriginalIdentityCannotFallBackToTheOnlyMatchingBasename()
    {
        var (generated, originals, _) = await CreateSources();
        generated[0].OriginalSourcePath = Path.Combine(_root, "source", "unknown", "SAME.cbl");
        Action index = () => new CompileRepairSourceIndex(_root, generated, originals);
        index.Should().Throw<InvalidOperationException>().WithMessage("*No authoritative COBOL source*");
    }

    [Fact]
    public async Task SourceRelativeIdentityResolvesAgainstExplicitSourceDirectoryNotCurrentCheckout()
    {
        var (generated, originals, sources) = await CreateSources();
        var sourceRoot = Path.Combine(_root, "source");
        foreach (var file in generated)
            file.OriginalSourcePath = Path.GetRelativePath(sourceRoot, file.OriginalSourcePath);
        var tasks = CompileRepairPlanner.PlanWithSourceIdentity(
            [new CompilerDiagnostic(sources.Keys.First(), 2, 1, "CS1002", "; expected")],
            GeneratedTypeIndex.FromSources(sources), sources, "Shared", new CompileGateSettings(),
            new CompileRepairSourceIndex(_root, generated, originals, sourceRoot));
        tasks.Should().ContainSingle().Which.Instructions.Should().ContainSingle().Which
            .Should().Contain("LEFT-ONLY").And.NotContain("RIGHT-ONLY");
    }

    [Fact]
    public void LegacyDictionaryCannotSilentlyCollapseNormalizedMemberNames()
    {
        var sources = new Dictionary<string, string> { ["A.cs"] = "class A {}" };
        Action plan = () => CompileRepairPlanner.Plan([], GeneratedTypeIndex.FromSources(sources), sources, "Shared",
            new CompileGateSettings(), new Dictionary<string, string> { ["SAME-NAME"] = "left", ["SAMENAME"] = "right" });
        plan.Should().Throw<InvalidOperationException>().WithMessage("*Ambiguous COBOL member names*");
    }

    private async Task<(List<CodeFile> Generated, List<CobolFile> Originals, Dictionary<string, string> Sources)> CreateSources()
    {
        var originals = new List<CobolFile>
        {
            new() { FileName = "SAME.cbl", FilePath = Path.Combine(_root, "source", "left", "SAME.cbl"), Content = "       01 LEFT-ONLY PIC X." },
            new() { FileName = "SAME.cbl", FilePath = Path.Combine(_root, "source", "right", "SAME.cbl"), Content = "       01 RIGHT-ONLY PIC 9." }
        };
        var generated = new List<CodeFile>();
        var sources = new Dictionary<string, string>(StringComparer.Ordinal);
        foreach (var (original, ns) in originals.Zip(new[] { "Left", "Right" }))
        {
            var file = new CodeFile
            {
                FileName = "Same.cs",
                NamespaceName = ns + ".Output",
                OriginalSourcePath = original.FilePath,
                Content = $"namespace {ns}.Output;\npublic class Service {{ }}"
            };
            await new FileHelper(NullLogger<FileHelper>.Instance).SaveCodeFileAsync(file, _root);
            generated.Add(file);
            sources.Add(Path.GetRelativePath(_root, file.FilePath).Replace('\\', '/'), await File.ReadAllTextAsync(file.FilePath));
        }
        return (generated, originals, sources);
    }

    private sealed class RecordingClient : IChatClient
    {
        public string Response { get; set; } = "";
        public List<string> Prompts { get; } = [];
        public Task<ChatResponse> GetResponseAsync(IEnumerable<ChatMessage> messages, ChatOptions? options = null,
            CancellationToken cancellationToken = default)
        {
            Prompts.Add(messages.Last().Text);
            return Task.FromResult(new ChatResponse(new ChatMessage(ChatRole.Assistant, Response)) { FinishReason = ChatFinishReason.Stop });
        }

        public IAsyncEnumerable<ChatResponseUpdate> GetStreamingResponseAsync(IEnumerable<ChatMessage> messages,
            ChatOptions? options = null, CancellationToken cancellationToken = default) => throw new NotSupportedException();
        public object? GetService(Type serviceType, object? serviceKey = null) => null;
        public void Dispose() { }
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, true);
    }
}
