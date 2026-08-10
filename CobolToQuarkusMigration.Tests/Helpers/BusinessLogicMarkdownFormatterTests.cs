using System.Text;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class BusinessLogicMarkdownFormatterTests
{
    [Fact]
    public void AppendTotals_NoProcessFeatures_OmitsMisleadingZeroFeatureCount()
    {
        var builder = new StringBuilder();

        BusinessLogicMarkdownFormatter.AppendTotals(
            builder,
            totalUseCases: 4,
            totalProcessFeatures: 0,
            totalBusinessRules: 28);

        var markdown = builder.ToString();
        markdown.Should().Contain("**Total Use Cases**: 4");
        markdown.Should().Contain("**Total Business Rules**: 28");
        markdown.Should().NotContain("Features");
    }

    [Fact]
    public void AppendUserStories_RendersNumberedKeyStepsWithoutBusinessRuleLabel()
    {
        var builder = new StringBuilder();
        var businessLogic = new BusinessLogic
        {
            UserStories =
            [
                new UserStory
                {
                    Id = "US-1",
                    Title = "Find customer",
                    AcceptanceCriteria =
                    [
                        "Accept the customer identifier.",
                        "Retrieve the matching record."
                    ]
                }
            ]
        };

        BusinessLogicMarkdownFormatter.AppendUserStories(builder, businessLogic);

        var markdown = builder.ToString();
        markdown.Should().Contain("### Use Cases");
        markdown.Should().NotContain("### Feature Descriptions");
        markdown.Should().Contain("**Key Steps:**");
        markdown.Should().Contain("1. Accept the customer identifier.");
        markdown.Should().Contain("2. Retrieve the matching record.");
        markdown.Should().NotContain("**Business Rules:**");
    }

    [Fact]
    public void AppendBusinessRules_RendersStructuredRuleDetails()
    {
        var builder = new StringBuilder();
        var businessLogic = new BusinessLogic
        {
            BusinessRules =
            [
                new BusinessRule
                {
                    Id = "BR-1",
                    Description = "Customer must exist",
                    Condition = "No customer matches the identifier.",
                    Action = "Inform the operator.",
                    SourceLocation = "CUSTOMER-INQUIRY.cbl:38-43 — keyed customer read with found/not-found branches"
                }
            ]
        };
        businessLogic.FileName = "CUSTOMER-INQUIRY.cbl";

        BusinessLogicMarkdownFormatter.AppendBusinessRules(builder, businessLogic);

        var markdown = builder.ToString();
        markdown.Should().Contain("### Business Rules");
        markdown.Should().Contain("#### BR-1: Customer must exist");
        markdown.Should().Contain("**Condition:** No customer matches the identifier.");
        markdown.Should().Contain("**Action:** Inform the operator.");
        markdown.Should().Contain(
            "*Source: CUSTOMER-INQUIRY.cbl:38-43 — keyed customer read with found/not-found branches*");
    }

    [Fact]
    public void AppendBusinessRules_LegacySource_PrefixesFileName()
    {
        var builder = new StringBuilder();
        var businessLogic = new BusinessLogic
        {
            FileName = "CUSTOMER-INQUIRY.cbl",
            BusinessRules =
            [
                new BusinessRule
                {
                    Id = "BR-1",
                    Description = "Customer must exist",
                    SourceLocation = "SEARCH-CUSTOMER paragraph"
                }
            ]
        };

        BusinessLogicMarkdownFormatter.AppendBusinessRules(builder, businessLogic);

        builder.ToString().Should().Contain(
            "*Source: CUSTOMER-INQUIRY.cbl — SEARCH-CUSTOMER paragraph*");
    }

    [Fact]
    public void AppendBusinessRules_NoRules_WritesNothing()
    {
        var builder = new StringBuilder();

        BusinessLogicMarkdownFormatter.AppendBusinessRules(builder, new BusinessLogic());

        builder.ToString().Should().BeEmpty();
    }
}
