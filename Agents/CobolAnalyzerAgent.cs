using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.ChatCompletion;
using Microsoft.SemanticKernel.Connectors.OpenAI;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Implementation of the COBOL analyzer agent with enhanced API call tracking.
/// </summary>
public class CobolAnalyzerAgent : ICobolAnalyzerAgent
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<CobolAnalyzerAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;

    /// <summary>
    /// Initializes a new instance of the <see cref="CobolAnalyzerAgent"/> class.
    /// </summary>
    /// <param name="kernelBuilder">The kernel builder.</param>
    /// <param name="logger">The logger.</param>
    /// <param name="modelId">The model ID to use for analysis.</param>
    /// <param name="enhancedLogger">Enhanced logger for API call tracking.</param>
    /// <param name="chatLogger">Chat logger for Azure OpenAI conversation tracking.</param>
    public CobolAnalyzerAgent(IKernelBuilder kernelBuilder, ILogger<CobolAnalyzerAgent> logger, string modelId, EnhancedLogger? enhancedLogger = null, ChatLogger? chatLogger = null)
    {
        _kernelBuilder = kernelBuilder;
        _logger = logger;
        _modelId = modelId;
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
    }

    /// <inheritdoc/>
    public async Task<CobolAnalysis> AnalyzeCobolFileAsync(CobolFile cobolFile)
    {
        var stopwatch = Stopwatch.StartNew();
        
        _logger.LogInformation("Analyzing COBOL file: {FileName}", cobolFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "COBOL_ANALYSIS_START", 
            $"Starting analysis of {cobolFile.FileName}", cobolFile.FileName);
        
        var kernel = _kernelBuilder.Build();
        
        // Declare apiCallId outside try block for proper scope
        var apiCallId = 0;
        
        try
        {
            // Create system prompt for COBOL analysis
            var systemPrompt = @"
You are an expert COBOL analyzer. Your task is to analyze COBOL source code and extract key information about the program structure, variables, paragraphs, logic flow and embedded SQL or DB2.
Analyze the provided COBOL program and provide a detailed, structured analysis that includes:

1. Overall program description
2. Data divisions and their purpose
3. Procedure divisions and their purpose
4. Variables (name, level, type, size, group structure)
5. Paragraphs/sections (name, description, logic, variables used, paragraphs called)
6. Copybooks referenced
7. File access (file name, mode, verbs used, status variable, FD linkage)
8. Any embedded SQL or DB2 statements (type, purpose, variables used)


Your analysis should be structured in a way that can be easily parsed by a Java conversion system.
";

            // Create prompt for COBOL analysis
            var prompt = $@"
Analyze the following COBOL program:

```cobol
{cobolFile.Content}
```

Provide a detailed, structured analysis as described in your instructions.
";

            // Log API call start
            apiCallId = _enhancedLogger?.LogApiCallStart(
                "CobolAnalyzerAgent", 
                "POST", 
                "Azure OpenAI Chat Completion", 
                _modelId, 
                $"Analyzing {cobolFile.FileName} ({cobolFile.Content.Length} chars)"
            ) ?? 0;

            // Log user message to chat logger
            _chatLogger?.LogUserMessage("CobolAnalyzerAgent", cobolFile.FileName, prompt, systemPrompt);

            // Create execution settings
            var executionSettings = new OpenAIPromptExecutionSettings
            {
                MaxTokens = 32768, // Setting max limit within model
                Temperature = 0.1,
                TopP = 0.5
                // Model ID/deployment name is handled at the kernel level
            };
            
            // Create the full prompt including system and user message
            var fullPrompt = $"{systemPrompt}\n\n{prompt}";
            
            _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "PROMPT_GENERATION", 
                "Generated analysis prompt", $"System prompt: {systemPrompt.Length} chars, User prompt: {prompt.Length} chars");
            
            // Convert OpenAI settings to kernel arguments
            var kernelArguments = new KernelArguments(executionSettings);
            
            var functionResult = await kernel.InvokePromptAsync(
                fullPrompt,
                kernelArguments);
            
            var analysisText = functionResult.GetValue<string>() ?? string.Empty;
            
            // Log AI response to chat logger
            _chatLogger?.LogAIResponse("CobolAnalyzerAgent", cobolFile.FileName, analysisText);
            
            stopwatch.Stop();
            
            // Log API call completion
            _enhancedLogger?.LogApiCallEnd(apiCallId, analysisText, analysisText.Length / 4, 0.001m); // Rough token estimate
            _enhancedLogger?.LogPerformanceMetrics($"COBOL Analysis - {cobolFile.FileName}", stopwatch.Elapsed, 1);
            
            // Parse the analysis into a structured object
            var analysis = new CobolAnalysis
            {
                FileName = cobolFile.FileName,
                FilePath = cobolFile.FilePath,
                RawAnalysisData = analysisText
            };
            
            // In a real implementation, we would parse the analysis text to extract structured data
            // For this example, we'll just set some basic information
            analysis.ProgramDescription = "Extracted from AI analysis";
            
            _logger.LogInformation("Completed analysis of COBOL file: {FileName}", cobolFile.FileName);
            
            return analysis;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            
            // Log API call error if we have a call ID
            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, ex.Message);
            }
            
            _enhancedLogger?.LogBehindTheScenes("ERROR", "COBOL_ANALYSIS_FAILED", 
                $"Failed to analyze {cobolFile.FileName}: {ex.Message}", ex.GetType().Name);
            
            _logger.LogError(ex, "Error analyzing COBOL file: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <inheritdoc/>
    public async Task<List<CobolAnalysis>> AnalyzeCobolFilesAsync(List<CobolFile> cobolFiles, Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Analyzing {Count} COBOL files", cobolFiles.Count);
        
        var analyses = new List<CobolAnalysis>();
        int processedCount = 0;
        
        foreach (var cobolFile in cobolFiles)
        {
            var analysis = await AnalyzeCobolFileAsync(cobolFile);
            analyses.Add(analysis);
            
            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }
        
        _logger.LogInformation("Completed analysis of {Count} COBOL files", cobolFiles.Count);
        
        return analyses;
    }
}
