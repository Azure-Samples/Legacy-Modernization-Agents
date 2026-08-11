using System.Reflection;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

public class ChunkAwarePromptIntegrityTests
{
    [Fact]
    public async Task JavaPrompts_PreserveAllCrossChunkContext()
    {
        var converter = new ChunkAwareJavaConverter(
            Mock.Of<IChatClient>(),
            Mock.Of<ILogger<ChunkAwareJavaConverter>>(),
            "test-model",
            new ConversionSettings());

        await AssertCompleteContextAsync(converter);
    }

    [Fact]
    public async Task CSharpPrompts_PreserveAllCrossChunkContext()
    {
        var converter = new ChunkAwareCSharpConverter(
            Mock.Of<IChatClient>(),
            Mock.Of<ILogger<ChunkAwareCSharpConverter>>(),
            "test-model",
            new ConversionSettings());

        await AssertCompleteContextAsync(converter);
    }

    private static async Task AssertCompleteContextAsync(object converter)
    {
        var chunk = new ChunkResult
        {
            ChunkIndex = 1,
            TotalChunks = 3,
            SourceFile = "LARGE.cbl",
            StartLine = 101,
            EndLine = 200,
            Content = "PROCEDURE DIVISION.",
        };
        var context = new ChunkContext
        {
            TotalChunks = 3,
            PreviousSignatures = Enumerable.Range(1, 22)
                .Select(i => new SignatureSummary { TargetSignature = $"signature-{i}" })
                .ToList(),
            PreviousVariables = Enumerable.Range(1, 32)
                .Select(i => new VariableSummary { TargetType = "string", TargetName = $"variable-{i}" })
                .ToList(),
            PendingForwardReferences = Enumerable.Range(1, 12)
                .Select(i => new ForwardReferenceSummary { TargetMethod = $"forward-reference-{i}" })
                .ToList(),
        };

        var flags = BindingFlags.Instance | BindingFlags.NonPublic;
        var systemMethod = converter.GetType().GetMethod("BuildChunkAwareSystemPrompt", flags);
        var userMethod = converter.GetType().GetMethod("BuildChunkAwareUserPromptAsync", flags);

        systemMethod.Should().NotBeNull();
        userMethod.Should().NotBeNull();

        var systemPrompt = (string)systemMethod!.Invoke(converter, new object[] { chunk, context })!;
        var userPromptTask = (Task<string>)userMethod!.Invoke(converter, new object[] { chunk, context })!;
        var userPrompt = await userPromptTask;

        systemPrompt.Should().Contain("signature-22");
        systemPrompt.Should().Contain("variable-32");
        userPrompt.Should().Contain("forward-reference-12");
    }
}