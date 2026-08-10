using System.Text;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class BusinessLogicMarkdownFormatterTests
{
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
                    SourceLocation = "SEARCH-CUSTOMER / INVALID KEY"
                }
            ]
        };

        BusinessLogicMarkdownFormatter.AppendBusinessRules(builder, businessLogic);

        var markdown = builder.ToString();
        markdown.Should().Contain("### Business Rules");
        markdown.Should().Contain("#### BR-1: Customer must exist");
        markdown.Should().Contain("**Condition:** No customer matches the identifier.");
        markdown.Should().Contain("**Action:** Inform the operator.");
        markdown.Should().Contain("*Source: SEARCH-CUSTOMER / INVALID KEY*");
    }

    [Fact]
    public void AppendBusinessRules_NoRules_WritesNothing()
    {
        var builder = new StringBuilder();

        BusinessLogicMarkdownFormatter.AppendBusinessRules(builder, new BusinessLogic());

        builder.ToString().Should().BeEmpty();
    }
}
