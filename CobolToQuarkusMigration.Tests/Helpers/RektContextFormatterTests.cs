using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class RektContextFormatterTests
{
    [Fact]
    public void LegacyTruncationHelpers_PreserveCompleteContent()
    {
        var content = new string('x', 40_000) + "TAIL-SENTINEL";

#pragma warning disable CS0618
        var tokenResult = TokenHelper.TruncateToTokenLimit(content, maxTokens: 1);
        var cobolResult = TokenHelper.TruncateCobolIntelligently(content, maxTokens: 1);
#pragma warning restore CS0618

        tokenResult.Content.Should().Be(content);
        tokenResult.WasTruncated.Should().BeFalse();
        cobolResult.Content.Should().Be(content);
        cobolResult.WasTruncated.Should().BeFalse();
    }

    [Fact]
    public void CodeReviewerPrompt_PreservesCompleteConvertedCode()
    {
        var convertedCode = new string('x', 40_000) + "TAIL-SENTINEL";

        var prompt = PromptLoader.Load("CodeReviewer", new Dictionary<string, string>
        {
            ["TargetLanguage"] = "C#",
            ["StructuralContext"] = "FULL-STRUCTURAL-CONTEXT",
            ["Code"] = convertedCode,
        });

        prompt.Should().Contain("TAIL-SENTINEL");
        prompt.Should().Contain(convertedCode);
        prompt.Should().Contain("FULL-STRUCTURAL-CONTEXT");
        prompt.Should().NotContain("{{");
        prompt.Should().NotContain("TRUNCATED");
    }

    [Fact]
    public void ToPromptBlock_PreservesEntriesBeyondFormerLimits()
    {
        var context = new RektContext
        {
            Sections =
            {
                new RektSection
                {
                    Name = "MAIN",
                    Paragraphs = Enumerable.Range(1, 8)
                        .Select(i => new RektParagraph { Name = $"PARAGRAPH-{i}" })
                        .ToList(),
                },
            },
            PerformGraph = Enumerable.Range(1, 32)
                .Select(i => new RektPerformEdge { From = $"FROM-{i}", To = $"TO-{i}" })
                .ToList(),
            SqlStatements = Enumerable.Range(1, 22)
                .Select(i => new RektSqlStatement
                {
                    Operation = "SELECT",
                    Tables = new List<string> { $"TABLE-{i}" },
                    LineNumber = i,
                })
                .ToList(),
            DataStructure =
            {
                new RektDataItem
                {
                    Level = 1,
                    Name = "ROOT",
                    Children = Enumerable.Range(1, 32)
                        .Select(i => new RektDataItem { Level = 5, Name = $"FIELD-{i}" })
                        .ToList(),
                },
            },
        };

        var prompt = RektContextFormatter.ToPromptBlock(new StructuralContext
        {
            Program = "LARGE-PROGRAM",
            Context = context,
        });

        prompt.Should().Contain("PARAGRAPH-8");
        prompt.Should().Contain("FROM-32 → TO-32");
        prompt.Should().Contain("TABLE-22");
        prompt.Should().Contain("FIELD-32");
        prompt.Should().NotContain("… and");
    }
}