using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Implementation of the C# converter agent supporting both Responses API (codex) and Chat Completions API.
/// </summary>
public class CSharpConverterAgent : AgentBase, ICodeConverterAgent
{
    /// <inheritdoc/>
    protected override string AgentName => "CSharpConverterAgent";

    public string TargetLanguage => "CSharp";
    public string FileExtension => ".cs";

    /// <summary>
    /// Initializes a new instance using Responses API (for codex models like gpt-5.1-codex-mini).
    /// </summary>
    public CSharpConverterAgent(
        ResponsesApiClient responsesClient,
        ILogger<CSharpConverterAgent> logger,
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
    public CSharpConverterAgent(
        IChatClient chatClient,
        ILogger<CSharpConverterAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
        : base(chatClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
    }

    /// <inheritdoc/>
    public async Task<CodeFile> ConvertAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        var stopwatch = Stopwatch.StartNew();

        Logger.LogInformation("Converting COBOL file to C#: {FileName}", cobolFile.FileName);
        EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "CSHARP_CONVERSION_START",
            $"Starting C# conversion of {cobolFile.FileName}", cobolFile.FileName);

        try
        {
            var systemPrompt = @"
You are an expert in converting COBOL programs to C# with .NET framework. Your task is to convert COBOL source code to modern, maintainable C# code.

LANGUAGE REQUIREMENT (CRITICAL):
- ALL generated code, comments, variable names, and documentation MUST be in ENGLISH
- Translate any non-English comments or identifiers from the source COBOL to English
- Use English for all XML documentation comments, inline comments, and string literals
- Do NOT preserve Danish, German, or other non-English text from the source

MICROSERVICE ARCHITECTURE (CRITICAL):
- Design output as microservice-ready components
- Decompose by business domain/responsibility (e.g., ValidationService, ProcessingService, DataAccessService)
- Each service should have clear API boundaries and single responsibility
- Use dependency injection patterns for service interactions
- Group related COBOL paragraphs into cohesive service classes

FUNCTIONAL COMPLETENESS (CRITICAL):
- ALL business logic must be preserved as EXECUTABLE CODE
- Every COBOL operation must have corresponding runnable code in the output
- You MAY consolidate small paragraphs or split large ones based on good design
- You MAY inline trivial paragraphs (2-3 lines) into calling methods
- The output must be FUNCTIONALLY EQUIVALENT to the input

ANTI-ABSTRACTION RULES:
- Do NOT represent business logic as DATA (e.g., List<Operation>, Dictionary<string, Action>)
- Business logic must be EXECUTABLE CODE, not configuration or data entries
- Do NOT create generic 'Execute(operationName)' dispatchers
- Each distinct business operation must have its own implementation

Follow these guidelines:
1. Create proper C# class structures from COBOL programs
2. Convert COBOL variables to appropriate C# data types
3. Transform COBOL procedures into C# methods
4. Handle COBOL-specific features (PERFORM, GOTO, etc.) in an idiomatic C# way
5. Implement proper error handling with try-catch blocks
6. Include comprehensive XML documentation comments
7. Apply modern C# best practices (async/await, LINQ, etc.)
8. Use meaningful namespace names based on business domain (e.g., CobolMigration.Payments, CobolMigration.Customers)
9. Return ONLY the C# code without markdown code blocks or additional text
10. Namespace declarations must be single line: 'namespace CobolMigration.Something;'

CLASS NAMING REQUIREMENTS - CRITICAL:
Name the class based on WHAT THE PROGRAM DOES, not the original filename.
Use this pattern: <Domain><Action><Type>

Examples:
- A program that validates payment batches → PaymentBatchValidator
- A program that processes customer onboarding → CustomerOnboardingService  
- A program that reconciles ledger entries → LedgerReconciliationJob
- A program that syncs inventory data → InventorySyncWorker
- A program that sanitizes address data → AddressSanitizer
- A program that generates reports → ReportGenerator
- A program that handles file I/O → FileProcessingService

Common suffixes by program type:
- Service: General business logic
- Validator: Validation/verification logic
- Processor: Data transformation/processing
- Handler: Event/message handling
- Job/Worker: Batch/scheduled tasks
- Repository: Data access
- Calculator: Computation logic
- Generator: Output/report generation

IMPORTANT: The COBOL code may contain placeholder terms for error handling. Convert these to appropriate C# exception handling.

CRITICAL: Your response MUST start with 'namespace' or 'using' and contain ONLY valid C# code. Do NOT include explanations, notes, or markdown code blocks.
";

            // NOTE: Large files are handled by SmartMigrationOrchestrator which routes them
            // to ChunkedMigrationProcess. Files reaching this agent should fit within API limits.
            // If a file is unexpectedly large, log a warning but proceed (chunking should have caught it)
            var contentToConvert = cobolFile.Content;
            var estimatedTokens = TokenHelper.EstimateTokens(contentToConvert);
            if (estimatedTokens > 15000)
            {
                Logger.LogWarning(
                    "⚠️ Large file {FileName} ({Tokens} tokens) reached CSharpConverterAgent. " +
                    "This should have been routed to ChunkedMigrationProcess. Processing anyway...",
                    cobolFile.FileName, estimatedTokens);
            }

            string sanitizedContent = SanitizeCobolContent(contentToConvert);

            var userPrompt = $@"
Convert the following COBOL program to C# with .NET:

```cobol
{sanitizedContent}
```

Here is the analysis of the COBOL program:

{cobolAnalysis.RawAnalysisData}

IMPORTANT REQUIREMENTS:
1. Return ONLY the C# code - NO explanations, NO markdown blocks
2. Start with: namespace CobolMigration.Something; (single line)
3. Your response must be valid, compilable C# code
";

            var (csharpCode, usedFallback, fallbackReason) = await ExecuteWithFallbackAsync(
                systemPrompt,
                userPrompt,
                cobolFile.FileName);

            if (usedFallback)
            {
                return CreateFallbackCodeFile(cobolFile, cobolAnalysis, fallbackReason ?? "Unknown error");
            }

            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "CSHARP_CONVERSION_COMPLETE",
                $"Completed C# conversion of {cobolFile.FileName} in {stopwatch.ElapsedMilliseconds}ms");

            csharpCode = ExtractCSharpCode(csharpCode);

            // Extract AI's semantic class name (based on domain/action/type pattern)
            string aiClassName = ExtractClassNameFromCode(csharpCode);
            string namespaceName = GetNamespaceName(csharpCode);
            
            // Prefer AI-generated semantic name if it's not generic
            // Fall back to filename-derived name only if AI gave a generic name
            string finalClassName;
            if (NamingHelper.IsSemanticClassName(aiClassName))
            {
                // AI generated a good semantic name like "PaymentBatchValidator"
                finalClassName = aiClassName;
                Logger.LogInformation("Using AI-generated semantic class name: {ClassName}", finalClassName);
            }
            else
            {
                // Fall back to filename-derived name
                finalClassName = NamingHelper.DeriveClassNameFromCobolFile(cobolFile.FileName);
                Logger.LogWarning("AI generated generic class name '{AiClass}', using filename-derived: {ClassName}", 
                    aiClassName, finalClassName);
                
                // Update the code to use the new class name
                if (aiClassName != finalClassName)
                {
                    csharpCode = NamingHelper.ReplaceGenericClassName(csharpCode, aiClassName, finalClassName);
                }
            }

            return new CodeFile
            {
                // Use semantic class name for output filename
                FileName = $"{finalClassName}{FileExtension}",
                Content = csharpCode,
                ClassName = finalClassName,
                NamespaceName = namespaceName,
                OriginalCobolFileName = cobolFile.FileName,
                TargetLanguage = TargetLanguage
            };
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("ERROR", "CSHARP_CONVERSION_ERROR",
                $"Failed to convert {cobolFile.FileName}: {ex.Message}", ex);
            Logger.LogError(ex, "Error converting COBOL file to C#: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <inheritdoc/>
    public async Task<List<CodeFile>> ConvertAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null)
    {
        Logger.LogInformation("Converting {Count} COBOL files to C#", cobolFiles.Count);

        var codeFiles = new List<CodeFile>();
        int processedCount = 0;

        for (int i = 0; i < cobolFiles.Count; i++)
        {
            var cobolFile = cobolFiles[i];
            var cobolAnalysis = i < cobolAnalyses.Count ? cobolAnalyses[i] : null;

            if (cobolAnalysis == null)
            {
                Logger.LogWarning("No analysis found for COBOL file: {FileName}", cobolFile.FileName);
                continue;
            }

            var codeFile = await ConvertAsync(cobolFile, cobolAnalysis);
            codeFiles.Add(codeFile);

            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }

        return codeFiles;
    }

    private CodeFile CreateFallbackCodeFile(CobolFile cobolFile, CobolAnalysis cobolAnalysis, string reason)
    {
        var className = NamingHelper.GetFallbackClassName(cobolFile.FileName);
        var namespaceName = "CobolMigration.Fallback";
        var sanitizedReason = reason.Replace("\"", "'");

        var csharpCode = $$"""
namespace {{namespaceName}};

/// <summary>
/// Placeholder implementation generated because the AI conversion service was unavailable.
/// Original COBOL file: {{cobolFile.FileName}}
/// Reason: {{sanitizedReason}}
/// </summary>
public class {{className}}
{
    public void Run()
    {
        throw new NotSupportedException("AI conversion unavailable. Details: {{sanitizedReason}}");
    }
}
""";

        return new CodeFile
        {
            FileName = $"{className}.cs",
            NamespaceName = namespaceName,
            ClassName = className,
            Content = csharpCode,
            OriginalCobolFileName = cobolFile.FileName,
            TargetLanguage = TargetLanguage
        };
    }

    private string ExtractCSharpCode(string input)
    {
        if (input.Contains("```csharp") || input.Contains("```c#"))
        {
            var startMarker = input.Contains("```csharp") ? "```csharp" : "```c#";
            var endMarker = "```";
            int startIndex = input.IndexOf(startMarker);
            if (startIndex >= 0)
            {
                startIndex += startMarker.Length;
                int endIndex = input.IndexOf(endMarker, startIndex);
                if (endIndex >= 0)
                    return input.Substring(startIndex, endIndex - startIndex).Trim();
            }
        }
        return input;
    }

    /// <summary>
    /// Extracts the class name from generated C# code.
    /// </summary>
    private string ExtractClassNameFromCode(string csharpCode)
    {
        try
        {
            var lines = csharpCode.Split('\n');
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine.StartsWith("public class ") || trimmedLine.StartsWith("internal class ") || trimmedLine.StartsWith("class "))
                {
                    var classIndex = trimmedLine.IndexOf("class ", StringComparison.Ordinal);
                    if (classIndex >= 0)
                    {
                        var afterClass = trimmedLine.Substring(classIndex + "class ".Length);
                        var className = afterClass.Split('{', ' ', '\t', '\r', '\n', ':')[0].Trim();
                        if (NamingHelper.IsValidIdentifier(className)) return className;
                    }
                }
            }
        }
        catch (Exception ex)
        {
            Logger.LogWarning(ex, "Error extracting class name from C# code");
        }
        return "ConvertedCobolProgram";
    }

    private string GetNamespaceName(string csharpCode)
    {
        var namespaceIndex = csharpCode.IndexOf("namespace ");
        if (namespaceIndex >= 0)
        {
            var start = namespaceIndex + "namespace ".Length;
            var remaining = csharpCode.Substring(start);
            var end = remaining.IndexOfAny(new[] { ';', '{', '\r', '\n' });
            if (end >= 0) return remaining.Substring(0, end).Trim();
        }
        return "CobolMigration.Legacy";
    }

    private string SanitizeCobolContent(string cobolContent)
    {
        if (string.IsNullOrEmpty(cobolContent)) return cobolContent;

        var sanitizationMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            {"FEJL", "ERROR_CODE"}, {"FEJLMELD", "ERROR_MSG"}, {"FEJL-", "ERROR_"},
            {"FEJLMELD-", "ERROR_MSG_"}, {"INC-FEJLMELD", "INC-ERROR-MSG"},
            {"FEJL VED KALD", "ERROR IN CALL"}, {"KALD", "CALL_OP"}, {"MEDD-TEKST", "MSG_TEXT"},
        };

        string sanitizedContent = cobolContent;
        foreach (var (original, replacement) in sanitizationMap)
        {
            if (sanitizedContent.Contains(original))
                sanitizedContent = sanitizedContent.Replace(original, replacement);
        }
        return sanitizedContent;
    }
}
