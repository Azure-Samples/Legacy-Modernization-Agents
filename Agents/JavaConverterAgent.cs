using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Implementation of the Java converter agent supporting both Responses API (codex) and Chat Completions API.
/// Converts COBOL source files to Java/Quarkus code.
/// </summary>
public class JavaConverterAgent : AgentBase, IJavaConverterAgent, ICodeConverterAgent
{
    /// <inheritdoc/>
    protected override string AgentName => "JavaConverterAgent";

    public string TargetLanguage => "Java";
    public string FileExtension => ".java";

    /// <summary>
    /// Initializes a new instance using Responses API (for codex models like gpt-5.1-codex-mini).
    /// </summary>
    public JavaConverterAgent(
        ResponsesApiClient responsesClient,
        ILogger<JavaConverterAgent> logger,
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
    public JavaConverterAgent(
        IChatClient chatClient,
        ILogger<JavaConverterAgent> logger,
        string modelId,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null)
        : base(chatClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
    }

    /// <inheritdoc/>
    public async Task<JavaFile> ConvertToJavaAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        var stopwatch = Stopwatch.StartNew();

        Logger.LogInformation("Converting COBOL file to Java: {FileName}", cobolFile.FileName);
        EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "JAVA_CONVERSION_START",
            $"Starting Java conversion of {cobolFile.FileName}", cobolFile.FileName);

        try
        {
            // System prompt for Java conversion
            var systemPrompt = @"
You are an expert in converting COBOL programs to Java with Quarkus framework. Your task is to convert COBOL source code to modern, maintainable Java code that runs on the Quarkus framework.

LANGUAGE REQUIREMENT (CRITICAL):
- ALL generated code, comments, variable names, and documentation MUST be in ENGLISH
- Translate any non-English comments or identifiers from the source COBOL to English
- Use English for all Javadoc comments, inline comments, and string literals
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
- Do NOT represent business logic as DATA (e.g., List<Operation>, Map<String, Runnable>)
- Business logic must be EXECUTABLE CODE, not configuration or data entries
- Do NOT create generic 'execute(operationName)' dispatchers
- Each distinct business operation must have its own implementation

Follow these guidelines:
1. Create proper Java class structures from COBOL programs
2. Convert COBOL variables to appropriate Java data types
3. Transform COBOL procedures into Java methods
4. Handle COBOL-specific features (PERFORM, GOTO, etc.) in an idiomatic Java way
5. Implement proper error handling
6. Include comprehensive comments explaining the conversion decisions
7. Make the code compatible with Quarkus framework
8. Apply modern Java best practices, preferably using Java Quarkus features
9. Use ONLY simple lowercase package names based on business domain (e.g., com.example.payments, com.example.customers)
10. Return ONLY the Java code without markdown code blocks or additional text
11. Package declarations must be single line: 'package com.example.something;'

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

IMPORTANT: The COBOL code may contain placeholder terms that replaced Danish or other languages for error handling terminology for content filtering compatibility. 
When you see terms like 'ERROR_CODE', 'ERROR_MSG', or 'ERROR_CALLING', understand these represent standard COBOL error handling patterns.
Convert these to appropriate Java exception handling and logging mechanisms.

CRITICAL: Your response MUST start with 'package' and contain ONLY valid Java code. Do NOT include explanations, notes, or markdown code blocks.
";

            // NOTE: Large files are handled by SmartMigrationOrchestrator which routes them
            // to ChunkedMigrationProcess. Files reaching this agent should fit within API limits.
            // If a file is unexpectedly large, log a warning but proceed (chunking should have caught it)
            var contentToConvert = cobolFile.Content;
            var estimatedTokens = TokenHelper.EstimateTokens(contentToConvert);
            if (estimatedTokens > 15000)
            {
                Logger.LogWarning(
                    "⚠️ Large file {FileName} ({Tokens} tokens) reached JavaConverterAgent. " +
                    "This should have been routed to ChunkedMigrationProcess. Processing anyway...",
                    cobolFile.FileName, estimatedTokens);
            }

            // Sanitize COBOL content for content filtering
            string sanitizedContent = SanitizeCobolContent(contentToConvert);

            // User prompt for Java conversion
            var userPrompt = $@"
Convert the following COBOL program to Java with Quarkus:

```cobol
{sanitizedContent}
```

Here is the analysis of the COBOL program to help you understand its structure:

{cobolAnalysis.RawAnalysisData}

IMPORTANT REQUIREMENTS:
1. Return ONLY the Java code - NO explanations, NO markdown blocks, NO additional text
2. Start with: package com.example.something; (single line, lowercase, no comments)
3. Do NOT include newlines or explanatory text in the package declaration
4. Your response must be valid, compilable Java code starting with 'package' and ending with the class closing brace

Note: The original code contains Danish error handling terms replaced with placeholders.
";

            var (javaCode, usedFallback, fallbackReason) = await ExecuteWithFallbackAsync(
                systemPrompt,
                userPrompt,
                cobolFile.FileName);

            if (usedFallback)
            {
                return CreateFallbackJavaFile(cobolFile, cobolAnalysis, fallbackReason ?? "Unknown error");
            }

            stopwatch.Stop();
            EnhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "JAVA_CONVERSION_COMPLETE",
                $"Completed Java conversion of {cobolFile.FileName} in {stopwatch.ElapsedMilliseconds}ms");

            // Extract the Java code from markdown code blocks if necessary
            javaCode = ExtractJavaCode(javaCode);

            // Extract AI's semantic class name (based on domain/action/type pattern)
            string aiClassName = ExtractClassNameFromCode(javaCode);
            string packageName = GetPackageName(javaCode);
            
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
                    javaCode = NamingHelper.ReplaceGenericClassName(javaCode, aiClassName, finalClassName);
                }
            }

            var javaFile = new JavaFile
            {
                // Use semantic class name for output filename
                FileName = $"{finalClassName}{FileExtension}",
                Content = javaCode,
                ClassName = finalClassName,
                PackageName = packageName,
                OriginalCobolFileName = cobolFile.FileName
            };

            Logger.LogInformation("Completed conversion of COBOL file to Java: {FileName}", cobolFile.FileName);

            return javaFile;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();

            EnhancedLogger?.LogBehindTheScenes("ERROR", "JAVA_CONVERSION_ERROR",
                $"Failed to convert {cobolFile.FileName}: {ex.Message}", ex);

            Logger.LogError(ex, "Error converting COBOL file to Java: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <inheritdoc/>
    async Task<CodeFile> ICodeConverterAgent.ConvertAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        return await ConvertToJavaAsync(cobolFile, cobolAnalysis);
    }

    /// <inheritdoc/>
    async Task<List<CodeFile>> ICodeConverterAgent.ConvertAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback)
    {
        var javaFiles = await ConvertToJavaAsync(cobolFiles, cobolAnalyses, progressCallback);
        return javaFiles.Cast<CodeFile>().ToList();
    }

    /// <inheritdoc/>
    public async Task<List<JavaFile>> ConvertToJavaAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null)
    {
        Logger.LogInformation("Converting {Count} COBOL files to Java", cobolFiles.Count);

        var javaFiles = new List<JavaFile>();
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

            var javaFile = await ConvertToJavaAsync(cobolFile, cobolAnalysis);
            javaFiles.Add(javaFile);

            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }

        Logger.LogInformation("Completed conversion of {Count} COBOL files to Java", cobolFiles.Count);

        return javaFiles;
    }

    private JavaFile CreateFallbackJavaFile(CobolFile cobolFile, CobolAnalysis cobolAnalysis, string reason)
    {
        var className = NamingHelper.GetFallbackClassName(cobolFile.FileName);
        var packageName = "com.example.cobol";
        var sanitizedReason = reason.Replace("\"", "'");

        var javaCode = $$"""
package {{packageName}};

public class {{className}} {
    /**
     * Placeholder implementation generated because the AI conversion service was unavailable.
     * Original COBOL file: {{cobolFile.FileName}}
     * Reason: {{sanitizedReason}}
     */
    public void run() {
        throw new UnsupportedOperationException("AI conversion unavailable. Please supply valid Azure OpenAI credentials and rerun the migration. Details: {{sanitizedReason}}");
    }
}
""";

        return new JavaFile
        {
            FileName = $"{className}.java",
            PackageName = packageName,
            ClassName = className,
            Content = javaCode,
            OriginalCobolFileName = cobolFile.FileName
        };
    }

    private string ExtractJavaCode(string input)
    {
        // If the input contains markdown code blocks, extract the Java code
        if (input.Contains("```java"))
        {
            var startMarker = "```java";
            var endMarker = "```";

            int startIndex = input.IndexOf(startMarker);
            if (startIndex >= 0)
            {
                startIndex += startMarker.Length;
                int endIndex = input.IndexOf(endMarker, startIndex);

                if (endIndex >= 0)
                {
                    return input.Substring(startIndex, endIndex - startIndex).Trim();
                }
            }
        }

        return input;
    }

    /// <summary>
    /// Extracts the class name from generated Java code.
    /// </summary>
    private string ExtractClassNameFromCode(string javaCode)
    {
        try
        {
            var lines = javaCode.Split('\n');
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine.StartsWith("public class ") || trimmedLine.StartsWith("class "))
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
            Logger.LogWarning(ex, "Error extracting class name from Java code");
        }
        return "ConvertedCobolProgram";
    }

    private string GetPackageName(string javaCode)
    {
        var packageIndex = javaCode.IndexOf("package ");
        if (packageIndex >= 0)
        {
            var start = packageIndex + "package ".Length;
            var end = javaCode.IndexOf(";", start);

            if (end >= 0)
            {
                return javaCode.Substring(start, end - start).Trim();
            }
        }

        return "com.example.cobol";
    }

    /// <summary>
    /// Sanitizes COBOL content to avoid Azure OpenAI content filtering issues.
    /// </summary>
    private string SanitizeCobolContent(string cobolContent)
    {
        if (string.IsNullOrEmpty(cobolContent))
            return cobolContent;

        Logger.LogDebug("Sanitizing COBOL content for content filtering compatibility");

        var sanitizationMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            {"FEJL", "ERROR_CODE"},
            {"FEJLMELD", "ERROR_MSG"},
            {"FEJL-", "ERROR_"},
            {"FEJLMELD-", "ERROR_MSG_"},
            {"INC-FEJLMELD", "INC-ERROR-MSG"},
            {"FEJL VED KALD", "ERROR IN CALL"},
            {"FEJL VED KALD AF", "ERROR CALLING"},
            {"FEJL VED KALD BDSDATO", "ERROR CALLING BDSDATO"},
            {"KALD", "CALL_OP"},
            {"MEDD-TEKST", "MSG_TEXT"},
        };

        string sanitizedContent = cobolContent;
        bool contentModified = false;

        foreach (var (original, replacement) in sanitizationMap)
        {
            if (sanitizedContent.Contains(original))
            {
                sanitizedContent = sanitizedContent.Replace(original, replacement);
                contentModified = true;
                Logger.LogDebug("Replaced '{Original}' with '{Replacement}' in COBOL content", original, replacement);
            }
        }

        if (contentModified)
        {
            EnhancedLogger?.LogBehindTheScenes("CONTENT_FILTER", "SANITIZATION_APPLIED",
                "Applied content sanitization to avoid Azure OpenAI content filtering");
        }

        return sanitizedContent;
    }
}
