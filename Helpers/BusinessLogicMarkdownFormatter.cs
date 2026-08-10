using System.Text;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

internal static class BusinessLogicMarkdownFormatter
{
    internal static void AppendTotals(
        StringBuilder builder,
        int totalUseCases,
        int totalProcessFeatures,
        int totalBusinessRules)
    {
        builder.AppendLine($"**Total Use Cases**: {totalUseCases}");
        if (totalProcessFeatures > 0)
        {
            builder.AppendLine($"**Total Process Features**: {totalProcessFeatures}");
        }
        builder.AppendLine($"**Total Business Rules**: {totalBusinessRules}");
    }

    internal static void AppendUserStories(StringBuilder builder, BusinessLogic businessLogic)
    {
        if (!businessLogic.UserStories.Any())
        {
            return;
        }

        builder.AppendLine("### Use Cases");
        builder.AppendLine();

        foreach (var story in businessLogic.UserStories)
        {
            builder.AppendLine($"#### {story.Id}: {story.Title}");
            builder.AppendLine();

            if (!string.IsNullOrWhiteSpace(story.Role))
            {
                builder.AppendLine($"**Trigger:** {story.Role}");
            }

            if (!string.IsNullOrWhiteSpace(story.Action))
            {
                builder.AppendLine($"**Description:** {story.Action}");
            }

            if (!string.IsNullOrWhiteSpace(story.Benefit))
            {
                builder.AppendLine($"**Benefit:** {story.Benefit}");
            }

            if (story.AcceptanceCriteria.Any())
            {
                builder.AppendLine();
                builder.AppendLine("**Key Steps:**");
                for (int i = 0; i < story.AcceptanceCriteria.Count; i++)
                {
                    builder.AppendLine($"{i + 1}. {story.AcceptanceCriteria[i]}");
                }
            }

            if (!string.IsNullOrWhiteSpace(story.SourceLocation))
            {
                builder.AppendLine();
                builder.AppendLine($"*Source: {story.SourceLocation}*");
            }

            builder.AppendLine();
        }
    }

    internal static void AppendBusinessRules(StringBuilder builder, BusinessLogic businessLogic)
    {
        if (!businessLogic.BusinessRules.Any())
        {
            return;
        }

        builder.AppendLine("### Business Rules");
        builder.AppendLine();

        foreach (var rule in businessLogic.BusinessRules)
        {
            builder.AppendLine($"#### {rule.Id}: {rule.Description}");
            builder.AppendLine();

            if (!string.IsNullOrWhiteSpace(rule.Condition))
            {
                builder.AppendLine($"**Condition:** {rule.Condition}");
            }

            if (!string.IsNullOrWhiteSpace(rule.Action))
            {
                builder.AppendLine($"**Action:** {rule.Action}");
            }

            if (!string.IsNullOrWhiteSpace(rule.SourceLocation))
            {
                var source = rule.SourceLocation.StartsWith(
                    businessLogic.FileName,
                    StringComparison.OrdinalIgnoreCase)
                    ? rule.SourceLocation
                    : $"{businessLogic.FileName} — {rule.SourceLocation}";
                builder.AppendLine($"*Source: {source}*");
            }

            builder.AppendLine();
        }
    }
}
