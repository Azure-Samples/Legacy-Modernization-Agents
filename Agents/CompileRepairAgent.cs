using System.Text;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Repairs one generated C# file from the compiler's errors and the project facts the planner
/// derived. A repair that drops a type it was not told to remove is rejected, because silencing an
/// error by deleting code is exactly the failure a compile gate must not reward.
/// </summary>
public sealed class CompileRepairAgent : AgentBase
{
    protected override string AgentName => "CompileRepairAgent";

    private CompileRepairAgent(ResponsesApiClient client, ILogger logger, string modelId,
        EnhancedLogger? enhancedLogger, ChatLogger? chatLogger, AppSettings? settings)
        : base(client, logger, modelId, enhancedLogger, chatLogger, null, settings) { }

    private CompileRepairAgent(IChatClient client, ILogger logger, string modelId,
        EnhancedLogger? enhancedLogger, ChatLogger? chatLogger, AppSettings? settings)
        : base(client, logger, modelId, enhancedLogger, chatLogger, null, settings) { }

    public static CompileRepairAgent Create(
        ResponsesApiClient? responsesClient, IChatClient? chatClient, ILogger logger, string modelId,
        EnhancedLogger? enhancedLogger = null, ChatLogger? chatLogger = null, AppSettings? settings = null) =>
        responsesClient != null
            ? new CompileRepairAgent(responsesClient, logger, modelId, enhancedLogger, chatLogger, settings)
            : new CompileRepairAgent(chatClient!, logger, modelId, enhancedLogger, chatLogger, settings);

    /// <summary>The repaired source, or <c>null</c> when no acceptable repair came back.</summary>
    public async Task<string?> RepairAsync(CompileRepairTask task, string source)
    {
        var system = PromptLoader.LoadSectionValidated("CompileRepair", "System", new Dictionary<string, string>());
        var user = PromptLoader.LoadSectionValidated("CompileRepair", "User", new Dictionary<string, string>
        {
            ["File"] = task.File,
            ["Source"] = source,
            ["Errors"] = task.Errors.Count == 0
                ? "  (none in this file; see PROJECT FACTS)"
                : string.Join(Environment.NewLine, task.Errors.Select(e => $"  line {e.Line}, col {e.Column}: {e.Code}: {e.Message}")),
            ["Instructions"] = task.Instructions.Count == 0
                ? "  (none)"
                : string.Join(Environment.NewLine, task.Instructions.Select(i => "  • " + i)),
            ["Declarations"] = task.Declarations.Count == 0
                ? "  (none)"
                : string.Join(Environment.NewLine + Environment.NewLine, task.Declarations),
        });

        var (response, usedFallback, reason) = await ExecuteWithFallbackAsync(system, user, $"{task.File} [compile-repair]");
        if (usedFallback || string.IsNullOrWhiteSpace(response))
        {
            Logger.LogWarning("[{Agent}] No repair for {File}: {Reason}", AgentName, task.File, reason ?? "empty response");
            return null;
        }

        var repaired = GeneratedCSharpSyntax.FixAccessorTerminators(GeneratedCodeEntities.DecodeInCode(
            ConversionOutputGuard.ExtractFencedCode(response, "```csharp", "```c#", "```cs")));

        var rejection = Reject(source, repaired, task.MayRemove);
        if (rejection is not null)
        {
            Logger.LogWarning("[{Agent}] Rejected repair of {File}: {Reason}", AgentName, task.File, rejection);
            return null;
        }

        return repaired;
    }

    /// <summary>Why a repair is unacceptable, or <c>null</c> when it may be written.</summary>
    public static string? Reject(string before, string after, IReadOnlySet<string> mayRemove)
    {
        if (string.IsNullOrWhiteSpace(after)) return "empty output";

        var kept = TypeNames(after);
        var lost = TypeNames(before).Where(t => !mayRemove.Contains(t) && !kept.Contains(t)).ToList();
        if (lost.Count > 0) return "removed type(s) it was not told to remove: " + string.Join(", ", lost);

        // Other files find these types by namespace; moving one breaks every reference to it.
        var namespacesAfter = Declarations(after).ToLookup(d => d.Name, d => d.Namespace);
        var moved = Declarations(before)
            .Where(d => kept.Contains(d.Name) && !namespacesAfter[d.Name].Contains(d.Namespace))
            .Select(d => $"{d.Name} ({d.Namespace})")
            .Distinct()
            .ToList();
        if (moved.Count > 0) return "moved type(s) out of their namespace: " + string.Join(", ", moved);

        var opens = after.Count(c => c == '{');
        var closes = after.Count(c => c == '}');
        if (opens != closes) return $"unbalanced braces ({opens}/{closes})";

        if (kept.Count == 0 && TypeNames(before).Count > 0 && mayRemove.Count == 0) return "no type declarations left";

        return null;
    }

    private static IReadOnlyList<TypeDeclaration> Declarations(string source) =>
        GeneratedTypeIndex.FromSources(new Dictionary<string, string> { ["f"] = source }).Declarations;

    private static HashSet<string> TypeNames(string source) =>
        Declarations(source).Select(d => d.Name).ToHashSet(StringComparer.Ordinal);
}
