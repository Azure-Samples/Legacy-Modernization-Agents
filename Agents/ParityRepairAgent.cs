using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents;

public sealed record ParityRepairRequest(
    string Program, string Language, string Source,
    IReadOnlyDictionary<string, string> Files, ProgramParityResult Evidence,
    IReadOnlyList<Helpers.CompilerDiagnostic>? CompilerDiagnostics = null,
    IReadOnlyDictionary<string, string>? RelatedFiles = null,
    IReadOnlyList<RuntimeImplementationRequirement>? RuntimeRequirements = null);

public sealed class ParityRepairAgent : AgentBase
{
    protected override string AgentName => "ParityRepairAgent";

    private ParityRepairAgent(IChatClient client, ILogger logger, string modelId, AppSettings settings)
        : base(client, logger, modelId, null, null, null, settings) { }

    private ParityRepairAgent(ResponsesApiClient client, ILogger logger, string modelId, AppSettings settings)
        : base(client, logger, modelId, null, null, null, settings) { }

    public static ParityRepairAgent Create(ResponsesApiClient? responses, IChatClient client,
        ILogger logger, string modelId, AppSettings settings) =>
        responses is null ? new(client, logger, modelId, settings) : new(responses, logger, modelId, settings);

    public async Task<IReadOnlyDictionary<string, string>?> RepairAsync(ParityRepairRequest request)
    {
        var system = PromptLoader.LoadSectionValidated("ParityRepair", "System", new Dictionary<string, string>());
        if (request.Language == "Java")
            system += "\n" + PromptLoader.LoadSectionValidated("ParityRepair", "Java", new Dictionary<string, string>
            {
                ["Framework"] = JavaGeneration.Framework(Settings?.ConversionSettings)
            });
        var user = PromptLoader.LoadSectionValidated("ParityRepair", "User", new Dictionary<string, string>
        {
            ["Request"] = JsonSerializer.Serialize(request)
        });
        var (response, fallback, reason) = await ExecuteWithFallbackAsync(system, user, request.Program);
        if (fallback || string.IsNullOrWhiteSpace(response)
            || (!UseResponsesApi && !ProviderReportedNormalEnd(request.Program)))
        {
            Logger.LogWarning("Parity repair produced no complete candidate for {Program}: {Reason}", request.Program, reason);
            return null;
        }
        try
        {
            return JsonSerializer.Deserialize<Dictionary<string, string>>(
                ConversionOutputGuard.ExtractFencedCode(response, "```json"));
        }
        catch (JsonException ex)
        {
            Logger.LogWarning(ex, "Parity repair returned invalid JSON for {Program}", request.Program);
            return null;
        }
    }
}
