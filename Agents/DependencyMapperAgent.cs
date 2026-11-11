using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;
using System.Text;

namespace CobolToQuarkusMigration.Agents;

/// <summary>Maps verified REKT disk exports, with optional separate advisory AI commentary.</summary>
public class DependencyMapperAgent : AgentBase, IDependencyMapperAgent
{
    protected override string AgentName => "DependencyMapperAgent";

    private RektDiscovery Discovery => new(ConversionEligibility.FactsDirectory(
        RepositoryRoot ?? Environment.GetEnvironmentVariable("REPO_ROOT") ?? FindRepositoryRoot()));

    private static string FindRepositoryRoot()
    {
        var directory = new DirectoryInfo(AppContext.BaseDirectory);
        while (directory is not null && !File.Exists(Path.Combine(directory.FullName, "doctor.sh")))
            directory = directory.Parent;
        return directory?.FullName
            ?? throw new InvalidDataException("Repository root is required to verify dependency evidence.");
    }

    private string RequiredSourceDirectory => SourceDirectory
        ?? throw new InvalidDataException("A selected original source directory is required to verify dependency evidence.");

    public static DependencyMapperAgent Create(
        ResponsesApiClient? responsesClient,
        IChatClient? chatClient,
        ILogger<DependencyMapperAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        IRateLimiter? rateLimiter = null,
        AppSettings? settings = null)
    {
        return responsesClient != null
            ? new DependencyMapperAgent(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
            : new DependencyMapperAgent(chatClient!, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings);
    }

    public DependencyMapperAgent(
        ResponsesApiClient responsesClient,
        ILogger<DependencyMapperAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        IRateLimiter? rateLimiter = null,
        AppSettings? settings = null)
        : base(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
    }

    public DependencyMapperAgent(
        IChatClient chatClient,
        ILogger<DependencyMapperAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        IRateLimiter? rateLimiter = null,
        AppSettings? settings = null)
        : base(chatClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
    }

    public async Task<DependencyMap> AnalyzeDependenciesAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> analyses)
    {
        var stopwatch = Stopwatch.StartNew();
        Logger.LogInformation("Verifying REKT dependency evidence for {Count} COBOL files", cobolFiles.Count);
        var dependencyMap = Discovery.RequireVerified(cobolFiles, RequiredSourceDirectory);

        BuildReverseDependencies(dependencyMap);
        await AnalyzeInsightsAsync(cobolFiles, dependencyMap);
        CalculateMetrics(dependencyMap, cobolFiles);
        dependencyMap.MermaidDiagram = await GenerateMermaidDiagramAsync(dependencyMap);

        EnhancedLogger?.LogBehindTheScenes("DEPENDENCY_EVIDENCE", "DEPENDENCY_ANALYSIS_COMPLETE",
            $"Verified local REKT exports in {stopwatch.ElapsedMilliseconds}ms. Found {dependencyMap.Dependencies.Count} dependencies");
        return dependencyMap;
    }

    public Task<Dictionary<string, List<string>>> AnalyzeCopybookUsageAsync(List<CobolFile> cobolFiles)
        => Task.FromResult(Discovery.RequireVerified(cobolFiles, RequiredSourceDirectory).CopybookUsage);

    public Task<string> GenerateMermaidDiagramAsync(DependencyMap dependencyMap, bool enableAiGeneration = true)
    {
        Logger.LogInformation("Rendering Mermaid from supplied relationships; no AI discovery or Neo4j query");
        return Task.FromResult(RenderMermaidDiagram(dependencyMap));
    }

    private static void BuildReverseDependencies(DependencyMap dependencyMap)
    {
        foreach (var (program, copybooks) in dependencyMap.CopybookUsage)
        {
            foreach (var copybook in copybooks)
            {
                if (!dependencyMap.ReverseDependencies.TryGetValue(copybook, out var programs))
                    dependencyMap.ReverseDependencies[copybook] = programs = new List<string>();
                if (!programs.Contains(program)) programs.Add(program);
            }
        }
    }

    private async Task AnalyzeInsightsAsync(List<CobolFile> cobolFiles, DependencyMap dependencyMap)
    {
        Logger.LogInformation("Requesting advisory AI commentary; dependency relationships are already verified");
        try
        {
            if (cobolFiles.Any())
            {
                var systemPrompt = PromptLoader.LoadSection("DependencyMapper", "AnalysisSystem");
                var fileStructure = string.Join("\n", cobolFiles.Take(5).Select(f =>
                    $"File: {f.FileName}\nType: {(SourceTypeRegistry.IsCobolProgram(f.FileName) ? "Program" : "Copybook")}\nSize: {f.Content.Length} chars"));
                var userPrompt = PromptLoader.LoadSection("DependencyMapper", "AnalysisUser", new Dictionary<string, string>
                {
                    ["FileStructure"] = fileStructure,
                    ["CopybookUsagePatterns"] = string.Join("\n", dependencyMap.CopybookUsage.Take(10).Select(kvp =>
                        $"{kvp.Key} uses: {string.Join(", ", kvp.Value)}")),
                    ["EvidenceDiagnostics"] = string.Join("\n", dependencyMap.EvidenceDiagnostics)
                });
                var insights = await ExecuteChatCompletionAsync(systemPrompt, userPrompt, "dependency-analysis");
                dependencyMap.AnalysisInsights = "Evidence diagnostics:\n" +
                    string.Join("\n", dependencyMap.EvidenceDiagnostics) +
                    "\n\nAI advisory commentary (not dependency facts):\n" + insights;
            }
        }
        catch (OperationCanceledException)
        {
            throw;
        }
        catch (Exception ex)
        {
            Logger.LogWarning(ex, "Dependency evidence is verified; advisory commentary is unavailable");
            dependencyMap.AnalysisInsights = "Evidence diagnostics:\n" +
                string.Join("\n", dependencyMap.EvidenceDiagnostics) +
                $"\n\nDependency insights unavailable: {ex.Message}";
        }
    }

    private void CalculateMetrics(DependencyMap dependencyMap, List<CobolFile> cobolFiles)
    {
        var programs = cobolFiles.Count(f => SourceTypeRegistry.IsCobolProgram(f.FileName));
        dependencyMap.Metrics.TotalPrograms = programs;
        dependencyMap.Metrics.TotalCopybooks = cobolFiles.Count(f => SourceTypeRegistry.IsCopybook(f.FileName));
        dependencyMap.Metrics.TotalDependencies = dependencyMap.Dependencies.Count;
        dependencyMap.Metrics.AverageDependenciesPerProgram = programs > 0
            ? (double)dependencyMap.Dependencies.Count / programs : 0;
        if (dependencyMap.ReverseDependencies.Any())
        {
            var mostUsed = dependencyMap.ReverseDependencies.OrderByDescending(kvp => kvp.Value.Count).First();
            dependencyMap.Metrics.MostUsedCopybook = mostUsed.Key;
            dependencyMap.Metrics.MostUsedCopybookCount = mostUsed.Value.Count;
        }
    }

    private static string RenderMermaidDiagram(DependencyMap dependencyMap)
    {
        var edges = dependencyMap.Dependencies
            .Select(d => (Source: d.SourceFile, Target: d.TargetFile, Type: d.DependencyType))
            .Concat(dependencyMap.CopybookUsage.SelectMany(kv =>
                kv.Value.Select(target => (Source: kv.Key, Target: target, Type: "COPY"))))
            .Distinct()
            .OrderBy(e => e.Source, StringComparer.Ordinal)
            .ThenBy(e => e.Target, StringComparer.Ordinal)
            .ThenBy(e => e.Type, StringComparer.Ordinal)
            .ToList();
        var nodes = dependencyMap.CopybookUsage.Keys
            .Concat(edges.SelectMany(e => new[] { e.Source, e.Target }))
            .Distinct(StringComparer.Ordinal)
            .OrderBy(name => name, StringComparer.Ordinal)
            .Select((name, index) => (name, id: $"N{index}"))
            .ToDictionary(n => n.name, n => n.id, StringComparer.Ordinal);
        var sb = new StringBuilder();
        sb.AppendLine("graph TB");
        sb.AppendLine("    %% Deterministic view of supplied relationships, not proof of complete REKT evidence.");
        foreach (var (name, id) in nodes)
            sb.AppendLine($"    {id}[\"{EscapeMermaid(name)}\"]");
        foreach (var edge in edges)
            sb.AppendLine($"    {nodes[edge.Source]} -->|\"{EscapeMermaid(edge.Type)}\"| {nodes[edge.Target]}");
        return sb.ToString();
    }

    private static string EscapeMermaid(string value) =>
        value.Replace("&", "&amp;").Replace("#", "&#35;")
            .Replace("\"", "&quot;").Replace("<", "&lt;").Replace(">", "&gt;")
            .Replace("|", "&#124;").Replace("\r", " ").Replace("\n", " ");
}
