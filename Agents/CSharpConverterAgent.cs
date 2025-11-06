using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.ChatCompletion;
using Microsoft.SemanticKernel.Connectors.OpenAI;
using CobolModernization.Agents.Interfaces;
using CobolModernization.Models;
using CobolModernization.Helpers;
using System.Diagnostics;

namespace CobolModernization.Agents;

/// <summary>
/// Implementation of the C# converter agent with enhanced API call tracking.
/// </summary>
public class CSharpConverterAgent : ICSharpConverterAgent
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<CSharpConverterAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;

    /// <summary>
    /// Initializes a new instance of the <see cref="CSharpConverterAgent"/> class.
    /// </summary>
    /// <param name="kernelBuilder">The kernel builder.</param>
    /// <param name="logger">The logger.</param>
    /// <param name="modelId">The model ID to use for conversion.</param>
    /// <param name="enhancedLogger">Enhanced logger for API call tracking.</param>
    /// <param name="chatLogger">Chat logger for Azure OpenAI conversation tracking.</param>
    public CSharpConverterAgent(IKernelBuilder kernelBuilder, ILogger<CSharpConverterAgent> logger, string modelId, EnhancedLogger? enhancedLogger = null, ChatLogger? chatLogger = null)
    {
        _kernelBuilder = kernelBuilder;
        _logger = logger;
        _modelId = modelId;
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
    }

    /// <inheritdoc/>
    public async Task<CSharpFile> ConvertToCSharpAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("Converting COBOL file to C#: {FileName}", cobolFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "CSHARP_CONVERSION_START",
            $"Starting C# conversion of {cobolFile.FileName}", cobolFile.FileName);

        var kernel = _kernelBuilder.Build();
        int apiCallId = 0;

        try
        {
            // Create system prompt for C# conversion
            var systemPrompt = @"
You are an expert in converting COBOL programs to modern C# applications. Your task is to convert COBOL source code to clean, maintainable C# code following .NET best practices.

Follow these guidelines:
1. Create proper C# class structures from COBOL programs
2. Convert COBOL variables to appropriate C# data types (use decimal for COBOL numeric types, string for alphanumeric)
3. Transform COBOL procedures into C# methods
4. Handle COBOL-specific features (PERFORM, GOTO, etc.) in an idiomatic C# way using modern control flow
5. Implement proper error handling using exceptions and try-catch blocks
6. Include comprehensive XML documentation comments
7. Use async/await patterns where appropriate
8. Apply modern C# features (LINQ, pattern matching, nullable reference types, records where applicable)
9. Follow C# naming conventions (PascalCase for classes and methods, camelCase for private fields)
10. Use dependency injection patterns where appropriate

IMPORTANT: The COBOL code may contain placeholder terms that replaced Danish or other languages for error handling terminology for content filtering compatibility. 
When you see terms like 'ERROR_CODE', 'ERROR_MSG', or 'ERROR_CALLING', understand these represent standard COBOL error handling patterns.
Convert these to appropriate C# exception handling and logging mechanisms.
";

            // Sanitize COBOL content for content filtering
            string sanitizedContent = SanitizeCobolContent(cobolFile.Content);

            // Create prompt for C# conversion
            var prompt = $@"
Convert the following COBOL program to C#:

```cobol
{sanitizedContent}
```

Here is the analysis of the COBOL program to help you understand its structure:

{cobolAnalysis.RawAnalysisData}

Please provide the complete C# implementation with proper namespace, class structure, and XML documentation comments.
Note: The original code contains Danish error handling terms that have been temporarily replaced with placeholders for processing.
";

            // Log API call start
            apiCallId = _enhancedLogger?.LogApiCallStart(
                "CSharpConverterAgent",
                "ChatCompletion",
                "OpenAI/ConvertToCSharp",
                _modelId,
                $"Converting {cobolFile.FileName} ({cobolFile.Content.Length} chars)"
            ) ?? 0;

            // Log user message to chat logger
            _chatLogger?.LogUserMessage("CSharpConverterAgent", cobolFile.FileName, prompt, systemPrompt);

            _enhancedLogger?.LogBehindTheScenes("API_CALL", "CSHARP_CONVERSION_REQUEST",
                $"Sending conversion request for {cobolFile.FileName} to AI model {_modelId}");

            // Create execution settings
            var executionSettings = new OpenAIPromptExecutionSettings
            {
                MaxTokens = 32768, // Set within model limits
                Temperature = 0.1,
                TopP = 0.5
                // Model ID/deployment name is handled at the kernel level
            };

            // Create the full prompt including system and user message
            var fullPrompt = $"{systemPrompt}\n\n{prompt}";

            // Convert OpenAI settings to kernel arguments
            var kernelArguments = new KernelArguments(executionSettings);

            string csharpCode = string.Empty;
            int maxRetries = 3;
            int retryDelay = 5000; // 5 seconds

            for (int attempt = 1; attempt <= maxRetries; attempt++)
            {
                try
                {
                    _logger.LogInformation("Converting COBOL to C# - Attempt {Attempt}/{MaxRetries} for {FileName}",
                        attempt, maxRetries, cobolFile.FileName);

                    var functionResult = await kernel.InvokePromptAsync(
                        fullPrompt,
                        kernelArguments);

                    csharpCode = functionResult.GetValue<string>() ?? string.Empty;

                    // If we get here, the call was successful
                    break;
                }
                catch (Exception ex) when (attempt < maxRetries && (
                    ex.Message.Contains("canceled") ||
                    ex.Message.Contains("timeout") ||
                    ex.Message.Contains("The request was canceled") ||
                    ex.Message.Contains("content_filter") ||
                    ex.Message.Contains("content filtering") ||
                    ex.Message.Contains("ResponsibleAIPolicyViolation")))
                {
                    _logger.LogWarning("Attempt {Attempt} failed for {FileName}: {Error}. Retrying in {Delay}ms...",
                        attempt, cobolFile.FileName, ex.Message, retryDelay);

                    _enhancedLogger?.LogBehindTheScenes("API_CALL", "RETRY_ATTEMPT",
                        $"Retrying conversion for {cobolFile.FileName} - attempt {attempt}/{maxRetries} (Content filtering or timeout)", ex.Message);

                    await Task.Delay(retryDelay);
                    retryDelay *= 2; // Exponential backoff
                }
                catch (Exception ex)
                {
                    // Log API call failure
                    _enhancedLogger?.LogApiCallEnd(apiCallId, string.Empty, 0, 0);
                    _enhancedLogger?.LogBehindTheScenes("ERROR", "API_CALL_FAILED",
                        $"API call failed for {cobolFile.FileName}: {ex.Message}", ex);

                    _logger.LogError(ex, "Failed to convert COBOL file to C#: {FileName}", cobolFile.FileName);
                    throw;
                }
            }

            if (string.IsNullOrEmpty(csharpCode))
            {
                throw new InvalidOperationException($"Failed to convert {cobolFile.FileName} after {maxRetries} attempts");
            }

            // Log AI response to chat logger
            _chatLogger?.LogAIResponse("CSharpConverterAgent", cobolFile.FileName, csharpCode);

            // Log API call completion
            _enhancedLogger?.LogApiCallEnd(apiCallId, csharpCode, csharpCode.Length / 4, 0.002m); // Rough token estimate
            _enhancedLogger?.LogBehindTheScenes("API_CALL", "CSHARP_CONVERSION_RESPONSE",
                $"Received C# conversion for {cobolFile.FileName} ({csharpCode.Length} chars)");

            // Extract the C# code from markdown code blocks if necessary
            csharpCode = ExtractCSharpCode(csharpCode);

            // Parse file details
            string className = GetClassName(csharpCode);
            string namespaceName = GetNamespace(csharpCode);

            var csharpFile = new CSharpFile
            {
                FileName = $"{className}.cs",
                Content = csharpCode,
                ClassName = className,
                Namespace = namespaceName,
                OriginalCobolFileName = cobolFile.FileName
            };

            stopwatch.Stop();
            _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "CSHARP_CONVERSION_COMPLETE",
                $"Completed C# conversion of {cobolFile.FileName} in {stopwatch.ElapsedMilliseconds}ms", csharpFile);

            _logger.LogInformation("Completed conversion of COBOL file to C#: {FileName}", cobolFile.FileName);

            return csharpFile;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();

            // Log API call error
            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, ex.Message);
            }

            _enhancedLogger?.LogBehindTheScenes("ERROR", "CSHARP_CONVERSION_ERROR",
                $"Failed to convert {cobolFile.FileName}: {ex.Message}", ex);

            _logger.LogError(ex, "Error converting COBOL file to C#: {FileName}", cobolFile.FileName);
            throw;
        }
    }

    /// <inheritdoc/>
    public async Task<List<CSharpFile>> ConvertToCSharpAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Converting {Count} COBOL files to C#", cobolFiles.Count);

        var csharpFiles = new List<CSharpFile>();
        int processedCount = 0;

        for (int i = 0; i < cobolFiles.Count; i++)
        {
            var cobolFile = cobolFiles[i];
            var cobolAnalysis = i < cobolAnalyses.Count ? cobolAnalyses[i] : null;

            if (cobolAnalysis == null)
            {
                _logger.LogWarning("No analysis found for COBOL file: {FileName}", cobolFile.FileName);
                continue;
            }

            var csharpFile = await ConvertToCSharpAsync(cobolFile, cobolAnalysis);
            csharpFiles.Add(csharpFile);

            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }

        _logger.LogInformation("Completed conversion of {Count} COBOL files to C#", cobolFiles.Count);

        return csharpFiles;
    }

    /// <summary>
    /// Extracts C# code from markdown code blocks if present.
    /// </summary>
    /// <param name="input">The input string that may contain markdown code blocks.</param>
    /// <returns>The extracted C# code or the original input if no code blocks are found.</returns>
    private string ExtractCSharpCode(string input)
    {
        // Check for C# code blocks
        if (input.Contains("```csharp") || input.Contains("```cs"))
        {
            var startMarker = input.Contains("```csharp") ? "```csharp" : "```cs";
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

        // If no code blocks or extraction failed, return the original input
        return input;
    }

    /// <summary>
    /// Extracts the class name from C# code.
    /// </summary>
    /// <param name="csharpCode">The C# code to parse.</param>
    /// <returns>The class name or a default value if extraction fails.</returns>
    private string GetClassName(string csharpCode)
    {
        try
        {
            // Look for class declarations (public, private, internal, etc.)
            var lines = csharpCode.Split('\n');
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();

                // Check for class declarations with various access modifiers
                if (trimmedLine.Contains(" class "))
                {
                    // Extract the class name
                    var classKeywordIndex = trimmedLine.IndexOf(" class ");
                    if (classKeywordIndex >= 0)
                    {
                        var start = classKeywordIndex + " class ".Length;
                        var remaining = trimmedLine.Substring(start);

                        // Split on common delimiters: space, colon (for inheritance), opening brace, generic parameters
                        var className = remaining.Split(' ', ':', '{', '<', '\t', '\r', '\n')[0].Trim();

                        // Validate class name (should be alphanumeric + underscore)
                        if (IsValidCSharpIdentifier(className))
                        {
                            return className;
                        }
                    }
                }
            }

            // Fallback: look for record declarations
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine.Contains(" record "))
                {
                    var recordKeywordIndex = trimmedLine.IndexOf(" record ");
                    if (recordKeywordIndex >= 0)
                    {
                        var start = recordKeywordIndex + " record ".Length;
                        var remaining = trimmedLine.Substring(start);
                        var recordName = remaining.Split(' ', ':', '{', '(', '<', '\t', '\r', '\n')[0].Trim();

                        if (IsValidCSharpIdentifier(recordName))
                        {
                            return recordName;
                        }
                    }
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Error extracting class name from C# code");
        }

        // Default to a generic name if extraction fails
        return "ConvertedCobolProgram";
    }

    /// <summary>
    /// Validates if a string is a valid C# identifier.
    /// </summary>
    /// <param name="identifier">The identifier to validate.</param>
    /// <returns>True if the identifier is valid, false otherwise.</returns>
    private bool IsValidCSharpIdentifier(string identifier)
    {
        if (string.IsNullOrWhiteSpace(identifier))
            return false;

        // C# identifier rules: start with letter/underscore, followed by letters/digits/underscores
        if (!char.IsLetter(identifier[0]) && identifier[0] != '_')
            return false;

        return identifier.All(c => char.IsLetterOrDigit(c) || c == '_');
    }

    /// <summary>
    /// Extracts the namespace from C# code.
    /// </summary>
    /// <param name="csharpCode">The C# code to parse.</param>
    /// <returns>The namespace or a default value if extraction fails.</returns>
    private string GetNamespace(string csharpCode)
    {
        try
        {
            // Look for namespace declaration (traditional or file-scoped)
            var lines = csharpCode.Split('\n');
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();

                // Traditional namespace: namespace MyNamespace
                // File-scoped namespace: namespace MyNamespace;
                if (trimmedLine.StartsWith("namespace "))
                {
                    var start = "namespace ".Length;
                    var remaining = trimmedLine.Substring(start);

                    // Remove trailing semicolon or opening brace
                    var namespaceName = remaining.Split(';', '{', ' ', '\t', '\r', '\n')[0].Trim();

                    if (!string.IsNullOrWhiteSpace(namespaceName))
                    {
                        return namespaceName;
                    }
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Error extracting namespace from C# code");
        }

        // Default to a generic namespace if extraction fails
        return "ConvertedCobol";
    }

    /// <summary>
    /// Sanitizes COBOL content to avoid Azure OpenAI content filtering issues.
    /// This method replaces potentially problematic Danish terms with neutral equivalents.
    /// </summary>
    /// <param name="cobolContent">The original COBOL content</param>
    /// <returns>Sanitized COBOL content safe for AI processing</returns>
    private string SanitizeCobolContent(string cobolContent)
    {
        if (string.IsNullOrEmpty(cobolContent))
            return cobolContent;

        _logger.LogDebug("Sanitizing COBOL content for content filtering compatibility");

        // Dictionary of Danish error handling terms that trigger content filtering
        var sanitizationMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            // Danish "FEJL" (error) variations
            {"FEJL", "ERROR_CODE"},
            {"FEJLMELD", "ERROR_MSG"},
            {"FEJL-", "ERROR_"},
            {"FEJLMELD-", "ERROR_MSG_"},
            {"INC-FEJLMELD", "INC-ERROR-MSG"},
            {"FEJL VED KALD", "ERROR IN CALL"},
            {"FEJL VED KALD AF", "ERROR CALLING"},
            {"FEJL VED KALD BDSDATO", "ERROR CALLING BDSDATO"},
            
            // Other potentially problematic terms
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
                _logger.LogDebug("Replaced '{Original}' with '{Replacement}' in COBOL content", original, replacement);
            }
        }

        if (contentModified)
        {
            _enhancedLogger?.LogBehindTheScenes("CONTENT_FILTER", "SANITIZATION_APPLIED",
                "Applied content sanitization to avoid Azure OpenAI content filtering");
        }

        return sanitizedContent;
    }
}
