using GitHub.Copilot;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Checks the configured models against the provider's catalogue before any work starts.
///
/// An unavailable model is only discovered when a request is rejected, and the migration
/// treats that as a per-file failure, so a single bad model id is reported once per program
/// and the run still walks the whole estate before finishing with nothing converted. Asking
/// the catalogue once turns that into one message, in seconds, naming the models that exist.
///
/// This deliberately does not substitute a working model: a conversion silently performed by
/// a model the operator did not choose is worse than one that stops and says so.
/// </summary>
public static class ModelPreflight
{
    /// <summary>
    /// Returns an error describing the unavailable models, or null when everything checks out
    /// or when the catalogue could not be consulted (which must not block a run on its own).
    /// </summary>
    public static async Task<string?> ValidateCopilotModelsAsync(
        IEnumerable<string> modelIds,
        ILogger? logger = null,
        CancellationToken cancellationToken = default)
    {
        var requested = modelIds
            .Where(id => !string.IsNullOrWhiteSpace(id))
            .Select(id => id.Trim())
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToList();

        if (requested.Count == 0)
        {
            return null;
        }

        IReadOnlyList<string> available;
        try
        {
            using var client = new CopilotClient(new CopilotClientOptions { Mode = CopilotClientMode.CopilotCli });
            var models = await client.ListModelsAsync(cancellationToken);
            available = models
                .Select(m => m.Id ?? m.Name)
                .Where(id => !string.IsNullOrWhiteSpace(id))
                .Select(id => id!)
                .ToList();
        }
        catch (Exception ex)
        {
            // The catalogue is a convenience, not a gate. If the CLI cannot be reached the run
            // should still proceed and fail with the provider's own error.
            logger?.LogDebug(ex, "Could not list Copilot models; skipping preflight validation.");
            return null;
        }

        if (available.Count == 0)
        {
            return null;
        }

        var unknown = requested
            .Where(id => !available.Contains(id, StringComparer.OrdinalIgnoreCase))
            .ToList();

        if (unknown.Count == 0)
        {
            return null;
        }

        var lines = new List<string>
        {
            unknown.Count == 1
                ? $"Model '{unknown[0]}' is not available via GitHub Copilot."
                : $"These models are not available via GitHub Copilot: {string.Join(", ", unknown)}",
            "",
            $"Available models ({available.Count}):",
        };

        lines.AddRange(available.OrderBy(id => id, StringComparer.OrdinalIgnoreCase).Select(id => $"  • {id}"));
        lines.Add("");
        lines.Add("Run './doctor.sh setup' to choose one, or set AZURE_OPENAI_MODEL_ID.");
        lines.Add("A per-agent override in Config/appsettings.json takes precedence over both.");

        return string.Join(Environment.NewLine, lines);
    }
}
