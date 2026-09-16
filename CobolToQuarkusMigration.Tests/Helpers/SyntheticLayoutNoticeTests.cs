using CobolToQuarkusMigration.Helpers.PromptProjections;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// A stub copybook reaches the converter looking exactly like a parsed one, while the surrounding
// policy demands every field of every group. Without an explicit exemption the model resolves
// that conflict by inventing a structure and documenting it as fact, which no downstream check
// can distinguish from a real conversion.
public class SyntheticLayoutNoticeTests
{
    [Fact]
    public void SaysNothingWhenEveryLayoutWasParsed()
    {
        SyntheticLayoutNotice.Build(new[] { "cics-detected-screens-not-extracted" })
            .Should().BeEmpty();
    }

    [Fact]
    public void NamesTheCopybooksWhoseLayoutsWereInvented()
    {
        var block = SyntheticLayoutNotice.Build(new[]
        {
            "generated-copybook-stub:QPIPCCAB",
            "rekt-output-empty: no AST/CFG/DataStructure JSONs found",
            "generated-copybook-stub:ERRCODES",
        });

        block.Should().Contain("QPIPCCAB").And.Contain("ERRCODES");
        block.Should().NotContain("rekt-output-empty");
    }

    [Fact]
    public void ForbidsInventingFieldsAndRequiresAnExplicitUnknownMarker()
    {
        var block = SyntheticLayoutNotice.Build(new[] { "generated-copybook-stub:QPIPCCAB" });

        block.Should().Contain("Do NOT invent fields");
        block.Should().Contain("TODO: layout unknown");
        // The filler the generator emits must not be mistaken for a real field.
        block.Should().Contain("-STUB");
    }

    [Fact]
    public void KeepsTheCompletenessRuleForEveryOtherGroup()
    {
        var block = SyntheticLayoutNotice.Build(new[] { "generated-copybook-stub:QPIPCCAB" });

        block.Should().Contain("Every other data group");
    }

    [Fact]
    public void ListsEachCopybookOnceRegardlessOfCasing()
    {
        var block = SyntheticLayoutNotice.Build(new[]
        {
            "generated-copybook-stub:QPIPCCAB",
            "GENERATED-COPYBOOK-STUB:qpipccab",
        });

        block.Split("QPIPCCAB", StringSplitOptions.None).Length.Should().Be(2);
    }
}
