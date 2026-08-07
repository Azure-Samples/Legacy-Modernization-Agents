using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Helpers.PromptProjections;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class PromptLoaderTests
{
    [Fact]
    public void LoadSection_AllowsStagedRendering()
    {
        var section = PromptLoader.LoadSection("JavaConverter", "User");

        section.Should().Contain("{{CobolContent}}");
        section.Should().Contain("{{StructuralContext}}");
    }

    [Fact]
    public void LoadSectionValidated_ReportsUnresolvedPlaceholders()
    {
        var act = () => PromptLoader.LoadSectionValidated(
            "JavaConverter", "User", new Dictionary<string, string>
            {
                ["CobolContent"] = "IDENTIFICATION DIVISION.",
                ["Analysis"] = "analysis",
                ["BusinessLogicContext"] = string.Empty
            });

        act.Should().Throw<InvalidOperationException>()
            .WithMessage("*JavaConverter*User*StructuralContext*");
    }

    [Theory]
    [InlineData("JavaConverter", "Java with Quarkus", "package com.example.something;")]
    [InlineData("CSharpConverter", "C# with .NET", "namespace CobolMigration.Something;")]
    public void WholeFileUserPrompt_PreservesDynamicContextOrder(
        string promptName,
        string conversionText,
        string finalRequirement)
    {
        var prompt = PromptLoader.LoadSectionValidated(promptName, "User", new Dictionary<string, string>
        {
            ["CobolContent"] = "       DISPLAY 'ÆØÅ {{DomainValue}}'.",
            ["Analysis"] = "RAW ANALYSIS",
            ["BusinessLogicContext"] = "BUSINESS LOGIC",
            ["StructuralContext"] = "REKT CONTEXT"
        });

        prompt.Should().Contain(conversionText);
        prompt.Should().Contain("```cobol");
        prompt.Should().Contain("{{DomainValue}}");
        prompt.Should().Contain(finalRequirement);
        prompt.IndexOf("DISPLAY 'ÆØÅ'", StringComparison.Ordinal)
            .Should().BeLessThan(prompt.IndexOf("RAW ANALYSIS", StringComparison.Ordinal));
        prompt.IndexOf("RAW ANALYSIS", StringComparison.Ordinal)
            .Should().BeLessThan(prompt.IndexOf("BUSINESS LOGIC", StringComparison.Ordinal));
        prompt.IndexOf("BUSINESS LOGIC", StringComparison.Ordinal)
            .Should().BeLessThan(prompt.IndexOf("REKT CONTEXT", StringComparison.Ordinal));
        prompt.IndexOf("REKT CONTEXT", StringComparison.Ordinal)
            .Should().BeLessThan(prompt.IndexOf("IMPORTANT REQUIREMENTS:", StringComparison.Ordinal));
    }

    [Fact]
    public void RektProjection_UsesCommonPolicyAndTargetSpecificMappings()
    {
        var facts = CreateFacts();

        var java = JavaConverterProjection.BuildPromptBlock(facts);
        var csharp = CSharpConverterProjection.BuildPromptBlock(facts);

        java.Should().Contain("Treat the structural context below as GROUND TRUTH.");
        java.Should().Contain("PIC X→String");
        java.Should().Contain("interface + @Inject field + method call");
        csharp.Should().Contain("PIC X→string");
        csharp.Should().Contain("constructor-injected field + method call");
        csharp.Should().NotContain("PIC X→String,");

        foreach (var prompt in new[] { java, csharp })
        {
            prompt.IndexOf("(source: program-facts.json", StringComparison.Ordinal)
                .Should().BeLessThan(prompt.IndexOf("FACT-LOCKING RULES", StringComparison.Ordinal));
            prompt.IndexOf("FACT-LOCKING RULES", StringComparison.Ordinal)
                .Should().BeLessThan(prompt.IndexOf("WARNINGS", StringComparison.Ordinal));
            prompt.IndexOf("WARNINGS", StringComparison.Ordinal)
                .Should().BeLessThan(prompt.IndexOf("PROGRAM SUMMARY", StringComparison.Ordinal));
            prompt.Should().Contain("  • preserve this warning");
        }
    }

    private static ProgramFacts CreateFacts() => new()
    {
        Basename = "SAMPLE.cbl",
        Stem = "SAMPLE",
        SourceHash = "abc123",
        Confidence = FactConfidence.High,
        Warnings = ["preserve this warning"],
        Summary = new ProgramSummary
        {
            ProgramId = "SAMPLE",
            Loc = 42,
            Sections = 1,
            Paragraphs = 2
        },
        Callees = ["TARGET"]
    };
}
