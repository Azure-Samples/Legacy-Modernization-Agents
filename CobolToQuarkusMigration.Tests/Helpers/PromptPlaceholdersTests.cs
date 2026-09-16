using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// A prompt template is data to the editor and contract to the renderer. Losing a placeholder does
// not fail: the template still renders and still reads sensibly, the model is simply given less.
public class PromptPlaceholdersTests
{
    [Fact]
    public void DroppedNamesWhatARewriteRemoved()
    {
        // The exact loss observed after an AI-assisted rewrite of the chunked Java converter.
        const string before = """
            Convert this COBOL chunk (lines {{StartLine}}-{{EndLine}}) to Java:
            {{SemanticUnitsContext}}
            {{ForwardReferencesContext}}
            {{CobolContent}}
            """;
        const string after = """
            Convert this COBOL chunk to Java:
            {{CobolContent}}
            """;

        PromptPlaceholders.Dropped(before, after)
            .Should().BeEquivalentTo(new[] { "EndLine", "ForwardReferencesContext", "SemanticUnitsContext", "StartLine" });
    }

    [Fact]
    public void SubstitutingAPlaceholderForItsRenderedValueCountsAsLoss()
    {
        // How {{CodebaseProfile}} was lost: replaced by what it had rendered to, which reads
        // correctly and is wrong for every estate but the one it was captured from.
        const string before = "You are a converter.\n\n{{CodebaseProfile}}\n\nRules:";
        const string after = "You are a converter.\n\n## Source Codebase Profile\n- Programs: 69\n\nRules:";

        PromptPlaceholders.Dropped(before, after).Should().ContainSingle()
            .Which.Should().Be("CodebaseProfile");
    }

    [Fact]
    public void RewordingAroundThePlaceholdersIsAllowed()
    {
        const string before = "Convert {{CobolContent}} carefully.";
        const string after = "You are an expert. Convert the following faithfully:\n{{CobolContent}}";

        PromptPlaceholders.Dropped(before, after).Should().BeEmpty();
    }

    [Fact]
    public void AddingPlaceholdersIsAllowed()
    {
        PromptPlaceholders.Dropped("{{A}}", "{{A}} {{B}}").Should().BeEmpty();
    }

    [Fact]
    public void ANewPromptHasNothingToLose()
    {
        PromptPlaceholders.Dropped(null, "anything at all").Should().BeEmpty();
        PromptPlaceholders.Dropped("", "{{A}}").Should().BeEmpty();
    }

    [Fact]
    public void TheMessageNamesThePlaceholdersAndWhyTheyMatter()
    {
        var message = PromptPlaceholders.DescribeLoss("ChunkAwareJavaConverter", new[] { "StartLine", "EndLine" });

        message.Should().Contain("ChunkAwareJavaConverter");
        message.Should().Contain("{{StartLine}}");
        message.Should().Contain("{{EndLine}}");
        message.Should().Contain("still renders");
    }
}
