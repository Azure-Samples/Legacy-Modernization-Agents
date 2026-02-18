using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;
using System.Text;
using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Implementation of the COBOL dependency mapper agent supporting both Responses API (codex) and Chat Completions API.
/// </summary>
public class DependencyMapperAgent : AgentBase, IDependencyMapperAgent
{
    /// <inheritdoc/>
    protected override string AgentName => "DependencyMapperAgent";

    /// <summary>
    /// Initializes a new instance using Responses API (for codex models like gpt-5.1-codex-mini).
    /// </summary>
    public DependencyMapperAgent(
        ResponsesApiClient responsesClient,
        ILogger<DependencyMapperAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
        : base(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
    }

    /// <summary>
    /// Initializes a new instance using Chat Completions API (for chat models).
    /// </summary>
    public DependencyMapperAgent(
        IChatClient chatClient,
        ILogger<DependencyMapperAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
        : base(chatClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
    }

    /// <inheritdoc/>
    public async Task<DependencyMap> AnalyzeDependenciesAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> analyses)
    {
        var stopwatch = Stopwatch.StartNew();

        Logger.LogInformation("Analyzing dependencies for {Count} COBOL files", cobolFiles.Count);
        EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "DEPENDENCY_ANALYSIS_START",
            $"Starting dependency analysis for {cobolFiles.Count} COBOL files");

        var dependencyMap = new DependencyMap();

        try
        {
            // Analyze copybook usage patterns
            dependencyMap.CopybookUsage = await AnalyzeCopybookUsageAsync(cobolFiles);

            // Extract program call dependencies
            ExtractProgramCallDependencies(cobolFiles, dependencyMap);

            // Extract detailed dependencies (SQL, I/O, CICS)
            ExtractDetailedDependencies(cobolFiles, dependencyMap);

            // Build reverse dependencies
            BuildReverseDependencies(dependencyMap);

            // Analyze detailed dependencies using AI
            var aiAnalysisSucceeded = await AnalyzeDetailedDependenciesAsync(cobolFiles, analyses, dependencyMap);

            // Calculate metrics
            CalculateMetrics(dependencyMap, cobolFiles);

            // Generate Mermaid diagram
            dependencyMap.MermaidDiagram = await GenerateMermaidDiagramAsync(dependencyMap, aiAnalysisSucceeded);

            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "DEPENDENCY_ANALYSIS_COMPLETE",
                $"Completed dependency analysis in {stopwatch.ElapsedMilliseconds}ms. Found {dependencyMap.Dependencies.Count} dependencies");

            Logger.LogInformation("Dependency analysis completed. Found {Count} dependencies", dependencyMap.Dependencies.Count);
            return dependencyMap;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("ERROR", "DEPENDENCY_ANALYSIS_ERROR",
                $"Failed dependency analysis: {ex.Message}", ex);
            Logger.LogError(ex, "Error analyzing COBOL dependencies");
            throw;
        }
    }

    /// <inheritdoc/>
    public Task<Dictionary<string, List<string>>> AnalyzeCopybookUsageAsync(List<CobolFile> cobolFiles)
    {
        Logger.LogInformation("Analyzing copybook usage and program calls");

        var copybookUsage = new Dictionary<string, List<string>>();

        foreach (var cobolFile in cobolFiles.Where(f => f.FileName.EndsWith(".cbl")))
        {
            var copybooks = ExtractCopybookReferences(cobolFile.Content);
            copybookUsage[cobolFile.FileName] = copybooks;
        }

        return Task.FromResult(copybookUsage);
    }

    /// <inheritdoc/>
    public async Task<string> GenerateMermaidDiagramAsync(DependencyMap dependencyMap, bool enableAiGeneration = true)
    {
        Logger.LogInformation("Generating Mermaid diagram for dependency map");

        if (!enableAiGeneration)
        {
            Logger.LogWarning("Skipping Azure OpenAI Mermaid generation because previous analysis failed.");
            return GenerateFallbackMermaidDiagram(dependencyMap);
        }

        try
        {
            var systemPrompt = PromptLoader.LoadSection("DependencyMapper", "MermaidSystem");

            var userPrompt = PromptLoader.LoadSection("DependencyMapper", "MermaidUser", new Dictionary<string, string>
            {
                ["CopybookUsage"] = string.Join("\n", dependencyMap.CopybookUsage.Select(kv => $"- {kv.Key}: {string.Join(", ", kv.Value)}")),
                ["Dependencies"] = string.Join("\n", dependencyMap.Dependencies.Select(d => $"- {d.SourceFile} → {d.TargetFile} ({d.DependencyType})")),
                ["TotalPrograms"] = dependencyMap.Metrics.TotalPrograms.ToString(),
                ["TotalCopybooks"] = dependencyMap.Metrics.TotalCopybooks.ToString()
            });

            var mermaidDiagram = await ExecuteChatCompletionAsync(systemPrompt, userPrompt, "dependency-diagram");
            mermaidDiagram = mermaidDiagram.Replace("```mermaid", "").Replace("```", "").Trim();

            Logger.LogInformation("Mermaid diagram generated successfully");
            return mermaidDiagram;
        }
        catch (Exception ex)
        {
            Logger.LogError(ex, "Error generating Mermaid diagram");
            dependencyMap.AnalysisInsights = $"Dependency insights unavailable: {ex.Message}";
            return GenerateFallbackMermaidDiagram(dependencyMap);
        }
    }

    private void ExtractDetailedDependencies(List<CobolFile> cobolFiles, DependencyMap dependencyMap)
    {
        Logger.LogInformation("Extracting detailed dependencies (SQL, CICS, I/O)");

        foreach (var cobolFile in cobolFiles.Where(f => f.FileName.EndsWith(".cbl")))
        {
            var lines = cobolFile.Content.Split('\n');
            bool inExecSql = false;
            var sqlBuffer = new StringBuilder();
            int sqlStartLine = 0;

            for (int i = 0; i < lines.Length; i++)
            {
                var line = lines[i].Trim();
                // Skip comments (basic check, not handling inline comments perfectly but sufficient for 80-col COBOL)
                if (line.Length > 6 && line[6] == '*') continue;
                if (string.IsNullOrWhiteSpace(line)) continue;

                // 1. SQL Analysis
                if (Regex.IsMatch(line, @"EXEC\s+SQL", RegexOptions.IgnoreCase))
                {
                    inExecSql = true;
                    sqlStartLine = i + 1;
                    sqlBuffer.Clear();
                    sqlBuffer.Append(line);
                }
                
                if (inExecSql)
                {
                    if (lines[i].Trim() != sqlBuffer.ToString()) sqlBuffer.Append(" " + line);
                    
                    if (Regex.IsMatch(line, @"END-EXEC", RegexOptions.IgnoreCase))
                    {
                        inExecSql = false;
                        AnalyzeSqlBlock(sqlBuffer.ToString(), sqlStartLine, cobolFile.FileName, dependencyMap);
                    }
                }
                else
                {
                     // 2. CICS Analysis
                    if (Regex.IsMatch(line, @"EXEC\s+CICS", RegexOptions.IgnoreCase))
                    {
                        var match = Regex.Match(line, @"LINK\s+PROGRAM\s*\('([^']+)'\)", RegexOptions.IgnoreCase);
                        if (match.Success)
                        {
                            AddDependency(dependencyMap, cobolFile.FileName, match.Groups[1].Value, "EXEC CICS", i + 1, "CICS LINK");
                        }
                    }

                    // 3. File I/O Analysis
                    var openMatch = Regex.Match(line, @"^OPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+([A-Z0-9_-]+)", RegexOptions.IgnoreCase);
                    if (openMatch.Success) AddDependency(dependencyMap, cobolFile.FileName, openMatch.Groups[2].Value, "OPEN", i + 1, $"{openMatch.Groups[1].Value} Mode");

                    var readMatch = Regex.Match(line, @"^READ\s+([A-Z0-9_-]+)", RegexOptions.IgnoreCase);
                    if (readMatch.Success) AddDependency(dependencyMap, cobolFile.FileName, readMatch.Groups[1].Value, "READ", i + 1, "File Read");

                    var writeMatch = Regex.Match(line, @"^WRITE\s+([A-Z0-9_-]+)", RegexOptions.IgnoreCase);
                    if (writeMatch.Success) AddDependency(dependencyMap, cobolFile.FileName, writeMatch.Groups[1].Value, "WRITE", i + 1, "File Write");

                    var closeMatch = Regex.Match(line, @"^CLOSE\s+([A-Z0-9_-]+)", RegexOptions.IgnoreCase);
                    if (closeMatch.Success) AddDependency(dependencyMap, cobolFile.FileName, closeMatch.Groups[1].Value, "CLOSE", i + 1, "File Close");
                }
            }
        }
    }

    private void AnalyzeSqlBlock(string sqlText, int lineNumber, string sourceFile, DependencyMap dependencyMap)
    {
        var buffer = sqlText.ToUpper(); // Simplified upper casing
        
        // Find table names after FROM, JOIN, INTO, UPDATE, TABLE, INCLUDE
        var keywords = new[] { "FROM", "JOIN", "INTO", "UPDATE", "TABLE", "INCLUDE" };
        foreach (var keyword in keywords)
        {
            var matches = Regex.Matches(buffer, $@"{keyword}\s+([A-Z0-9_-]+)", RegexOptions.IgnoreCase);
            foreach (Match match in matches)
            {
                var target = match.Groups[1].Value;
                if (!IsReservedWord(target))
                {
                    AddDependency(dependencyMap, sourceFile, target, "EXEC SQL", lineNumber, $"{keyword} {target}");
                }
            }
        }
    }

    private bool IsReservedWord(string word)
    {
        var reserved = new HashSet<string> { "DCLGEN", "OF", "IN", "A", "B", "SELECT", "INSERT", "UPDATE", "DELETE", "WHERE", "GROUP", "ORDER", "BY", "HAVING" };
        return reserved.Contains(word, StringComparer.OrdinalIgnoreCase);
    }

    private void AddDependency(DependencyMap map, string source, string target, string type, int line, string context)
    {
        var dep = new DependencyRelationship
        {
            SourceFile = source,
            TargetFile = target,
            DependencyType = type,
            LineNumber = line,
            Context = context
        };
        
        if (!map.Dependencies.Any(d => d.SourceFile == dep.SourceFile && d.TargetFile == dep.TargetFile && d.DependencyType == dep.DependencyType && d.LineNumber == dep.LineNumber))
        {
            map.Dependencies.Add(dep);
        }
    }

    private List<string> ExtractCopybookReferences(string cobolContent)
    {
        var copybooks = new List<string>();
        var patterns = new[]
        {
            @"COPY\s+([A-Za-z0-9_-]+)",
            @"COPY\s+([A-Za-z0-9_-]+)\.cpy",
            @"INCLUDE\s+([A-Za-z0-9_-]+)",
            @"COPY\s+'([A-Za-z0-9_-]+)'",
        };

        foreach (var pattern in patterns)
        {
            var matches = Regex.Matches(cobolContent, pattern, RegexOptions.IgnoreCase);
            foreach (Match match in matches)
            {
                var copybookName = match.Groups[1].Value;
                if (!copybookName.EndsWith(".cpy")) copybookName += ".cpy";
                if (!copybooks.Contains(copybookName)) copybooks.Add(copybookName);
            }
        }

        return copybooks;
    }

    private List<(string programName, int lineNumber)> ExtractProgramCallsWithLines(string cobolContent)
    {
        var calledPrograms = new List<(string programName, int lineNumber)>();
        var lines = cobolContent.Split('\n');
        var patterns = new[]
        {
            @"CALL\s+'([A-Za-z0-9_-]+)'",
            @"CALL\s+""([A-Za-z0-9_-]+)""",
            @"CALL\s+([A-Z][A-Z0-9_-]+)\s+USING",
        };

        for (int i = 0; i < lines.Length; i++)
        {
            foreach (var pattern in patterns)
            {
                var match = Regex.Match(lines[i], pattern, RegexOptions.IgnoreCase);
                if (match.Success)
                {
                    var programName = match.Groups[1].Value.Trim();
                    if (!programName.EndsWith(".cbl", StringComparison.OrdinalIgnoreCase))
                        programName += ".cbl";
                    if (!calledPrograms.Any(cp => cp.programName == programName))
                        calledPrograms.Add((programName, i + 1));
                    break;
                }
            }
        }

        return calledPrograms;
    }

    private void ExtractProgramCallDependencies(List<CobolFile> cobolFiles, DependencyMap dependencyMap)
    {
        Logger.LogInformation("Extracting all dependency types");

        foreach (var cobolFile in cobolFiles.Where(f => f.FileName.EndsWith(".cbl")))
        {
            // Extract CALL statements
            var calledPrograms = ExtractProgramCallsWithLines(cobolFile.Content);
            foreach (var (programName, lineNumber) in calledPrograms)
            {
                var dependency = new DependencyRelationship
                {
                    SourceFile = cobolFile.FileName,
                    TargetFile = programName,
                    DependencyType = "CALL",
                    LineNumber = lineNumber,
                    Context = $"Line {lineNumber}: CALL '{programName.Replace(".cbl", "")}'"
                };

                if (!dependencyMap.Dependencies.Contains(dependency))
                    dependencyMap.Dependencies.Add(dependency);
            }
        }

        Logger.LogInformation($"Found {dependencyMap.Dependencies.Count} dependencies");
    }

    private void BuildReverseDependencies(DependencyMap dependencyMap)
    {
        foreach (var kvp in dependencyMap.CopybookUsage)
        {
            var program = kvp.Key;
            var copybooks = kvp.Value;

            foreach (var copybook in copybooks)
            {
                if (!dependencyMap.ReverseDependencies.ContainsKey(copybook))
                    dependencyMap.ReverseDependencies[copybook] = new List<string>();

                if (!dependencyMap.ReverseDependencies[copybook].Contains(program))
                    dependencyMap.ReverseDependencies[copybook].Add(program);
            }
        }
    }

    private async Task<bool> AnalyzeDetailedDependenciesAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> analyses, DependencyMap dependencyMap)
    {
        Logger.LogInformation("Performing detailed dependency analysis using AI");

        try
        {
            // Create dependency relationships for each copybook usage
            foreach (var kvp in dependencyMap.CopybookUsage)
            {
                var program = kvp.Key;
                var copybooks = kvp.Value;

                foreach (var copybook in copybooks)
                {
                    var dependency = new DependencyRelationship
                    {
                        SourceFile = program,
                        TargetFile = copybook,
                        DependencyType = "COPY",
                        Context = "Copybook inclusion"
                    };
                    dependencyMap.Dependencies.Add(dependency);
                }
            }

            // Perform AI-powered analysis for additional insights
            if (cobolFiles.Any())
            {
                var systemPrompt = PromptLoader.LoadSection("DependencyMapper", "AnalysisSystem");

                var fileStructure = string.Join("\n", cobolFiles.Take(5).Select(f =>
                    $"File: {f.FileName}\nType: {(f.FileName.EndsWith(".cbl") ? "Program" : "Copybook")}\nSize: {f.Content.Length} chars"));

                var userPrompt = PromptLoader.LoadSection("DependencyMapper", "AnalysisUser", new Dictionary<string, string>
                {
                    ["FileStructure"] = fileStructure,
                    ["CopybookUsagePatterns"] = string.Join("\n", dependencyMap.CopybookUsage.Take(10).Select(kvp =>
                        $"{kvp.Key} uses: {string.Join(", ", kvp.Value)}"))
                });

                var insights = await ExecuteChatCompletionAsync(systemPrompt, userPrompt, "dependency-analysis");
                dependencyMap.AnalysisInsights = insights;
            }

            return true;
        }
        catch (Exception ex)
        {
            Logger.LogWarning(ex, "Error during detailed dependency analysis, continuing with basic analysis");
            dependencyMap.AnalysisInsights = $"Dependency insights unavailable: {ex.Message}";
            return false;
        }
    }

    private void CalculateMetrics(DependencyMap dependencyMap, List<CobolFile> cobolFiles)
    {
        var programs = cobolFiles.Where(f => f.FileName.EndsWith(".cbl")).ToList();
        var copybooks = cobolFiles.Where(f => f.FileName.EndsWith(".cpy")).ToList();

        dependencyMap.Metrics.TotalPrograms = programs.Count;
        dependencyMap.Metrics.TotalCopybooks = copybooks.Count;
        dependencyMap.Metrics.TotalDependencies = dependencyMap.Dependencies.Count;

        if (programs.Count > 0)
        {
            dependencyMap.Metrics.AverageDependenciesPerProgram =
                (double)dependencyMap.Dependencies.Count / programs.Count;
        }

        if (dependencyMap.ReverseDependencies.Any())
        {
            var mostUsed = dependencyMap.ReverseDependencies
                .OrderByDescending(kvp => kvp.Value.Count)
                .First();

            dependencyMap.Metrics.MostUsedCopybook = mostUsed.Key;
            dependencyMap.Metrics.MostUsedCopybookCount = mostUsed.Value.Count;
        }

        Logger.LogInformation("Calculated metrics: {Programs} programs, {Copybooks} copybooks, {Dependencies} dependencies",
            dependencyMap.Metrics.TotalPrograms, dependencyMap.Metrics.TotalCopybooks, dependencyMap.Metrics.TotalDependencies);
    }

    private string GenerateFallbackMermaidDiagram(DependencyMap dependencyMap)
    {
        var sb = new StringBuilder();
        sb.AppendLine("graph TB");
        sb.AppendLine("    subgraph \"COBOL Programs\"");

        var programs = dependencyMap.CopybookUsage.Keys.ToList();
        for (int i = 0; i < programs.Count; i++)
            sb.AppendLine($"        P{i}[\"{programs[i]}\"]");

        sb.AppendLine("    end");
        sb.AppendLine("    subgraph \"Copybooks\"");

        var copybooks = dependencyMap.ReverseDependencies.Keys.ToList();
        for (int i = 0; i < copybooks.Count; i++)
            sb.AppendLine($"        C{i}[\"{copybooks[i]}\"]");

        sb.AppendLine("    end");

        foreach (var kvp in dependencyMap.CopybookUsage)
        {
            var programIndex = programs.IndexOf(kvp.Key);
            foreach (var copybook in kvp.Value)
            {
                var copybookIndex = copybooks.IndexOf(copybook);
                if (copybookIndex >= 0)
                    sb.AppendLine($"    P{programIndex} --> C{copybookIndex}");
            }
        }

        sb.AppendLine("    classDef programClass fill:#81c784");
        sb.AppendLine("    classDef copybookClass fill:#ffb74d");

        for (int i = 0; i < programs.Count; i++)
            sb.AppendLine($"    class P{i} programClass");
        for (int i = 0; i < copybooks.Count; i++)
            sb.AppendLine($"    class C{i} copybookClass");

        return sb.ToString();
    }
}
