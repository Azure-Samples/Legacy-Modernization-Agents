using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.ChatCompletion;
using Microsoft.SemanticKernel.Connectors.OpenAI;
using Microsoft.SemanticKernel.Http;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.ClientModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Sockets;
using System.Reflection;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Implementation of the Java converter agent with enhanced API call tracking.
/// </summary>
public class JavaConverterAgent : IJavaConverterAgent, ICodeConverterAgent
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<JavaConverterAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;

    public string TargetLanguage => "Java";
    public string FileExtension => ".java";

    /// <summary>
    /// Initializes a new instance of the <see cref="JavaConverterAgent"/> class.
    /// </summary>
    /// <param name="kernelBuilder">The kernel builder.</param>
    /// <param name="logger">The logger.</param>
    /// <param name="modelId">The model ID to use for conversion.</param>
    /// <param name="enhancedLogger">Enhanced logger for API call tracking.</param>
    /// <param name="chatLogger">Chat logger for Azure OpenAI conversation tracking.</param>
    public JavaConverterAgent(IKernelBuilder kernelBuilder, ILogger<JavaConverterAgent> logger, string modelId, EnhancedLogger? enhancedLogger = null, ChatLogger? chatLogger = null)
    {
        _kernelBuilder = kernelBuilder;
        _logger = logger;
        _modelId = modelId;
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
    }

    /// <inheritdoc/>
    public async Task<JavaFile> ConvertToJavaAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("Converting COBOL file to Java: {FileName}", cobolFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "JAVA_CONVERSION_START",
            $"Starting Java conversion of {cobolFile.FileName}", cobolFile.FileName);

        var kernel = _kernelBuilder.Build();
        int apiCallId = 0;

        try
        {
            // Create system prompt for Java conversion
            var systemPrompt = @"
You are an expert in converting COBOL programs to Java with Quarkus framework. Your task is to convert COBOL source code to modern, maintainable Java code that runs on the Quarkus framework.

Follow these guidelines:
1. Create proper Java class structures from COBOL programs
2. Convert COBOL variables to appropriate Java data types
3. Transform COBOL procedures into Java methods
4. Handle COBOL-specific features (PERFORM, GOTO, etc.) in an idiomatic Java way
5. Implement proper error handling
6. Include comprehensive comments explaining the conversion decisions
7. Make the code compatible with Quarkus framework
8. Apply modern Java best practices, preferably using Java Quarkus features
9. Use ONLY simple lowercase package names (e.g., com.example.cobol, com.example.bdcommit) - NO explanations in package declarations
10. Return ONLY the Java code without markdown code blocks or additional text
11. Package declarations must be single line: 'package com.example.something;'

IMPORTANT: The COBOL code may contain placeholder terms that replaced Danish or other languages for error handling terminology for content filtering compatibility. 
When you see terms like 'ERROR_CODE', 'ERROR_MSG', or 'ERROR_CALLING', understand these represent standard COBOL error handling patterns.
Convert these to appropriate Java exception handling and logging mechanisms.

CRITICAL: Your response MUST start with 'package' and contain ONLY valid Java code. Do NOT include explanations, notes, or markdown code blocks.
";

            // Sanitize COBOL content for content filtering
            string sanitizedContent = SanitizeCobolContent(cobolFile.Content);

            // Create prompt for Java conversion
            var prompt = $@"
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

            // Log API call start
            apiCallId = _enhancedLogger?.LogApiCallStart(
                "JavaConverterAgent",
                "ChatCompletion",
                "OpenAI/ConvertToJava",
                _modelId,
                $"Converting {cobolFile.FileName} ({cobolFile.Content.Length} chars)"
            ) ?? 0;

            // Log user message to chat logger
            _chatLogger?.LogUserMessage("JavaConverterAgent", cobolFile.FileName, prompt, systemPrompt);

            _enhancedLogger?.LogBehindTheScenes("API_CALL", "JAVA_CONVERSION_REQUEST",
                $"Sending conversion request for {cobolFile.FileName} to AI model {_modelId}");

            // Create execution settings
            var executionSettings = new OpenAIPromptExecutionSettings
            {
                // gpt-5-mini only supports default temperature (1) and topP (1)
                // Model ID/deployment name is handled at the kernel level
                ExtensionData = new Dictionary<string, object>
                {
                    ["max_completion_tokens"] = 32768  // gpt-5-mini uses max_completion_tokens
                }
            };

            // Create the full prompt including system and user message
            var fullPrompt = $"{systemPrompt}\n\n{prompt}";

            // Convert OpenAI settings to kernel arguments
            var kernelArguments = new KernelArguments(executionSettings);

            string javaCode = string.Empty;
            int maxRetries = 3;
            int retryDelay = 5000; // 5 seconds

            Exception? lastException = null;

            for (int attempt = 1; attempt <= maxRetries; attempt++)
            {
                try
                {
                    _logger.LogInformation("Converting COBOL to Java - Attempt {Attempt}/{MaxRetries} for {FileName}",
                        attempt, maxRetries, cobolFile.FileName);

                    var functionResult = await kernel.InvokePromptAsync(
                        fullPrompt,
                        kernelArguments);

                    javaCode = functionResult.GetValue<string>() ?? string.Empty;

                    // If we get here, the call was successful
                    break;
                }
                catch (Exception ex) when (ShouldFallback(ex))
                {
                    lastException = ex;
                    var reason = GetFallbackReason(ex);
                    _enhancedLogger?.LogApiCallError(apiCallId, reason);
                    _enhancedLogger?.LogBehindTheScenes("WARNING", "JAVA_CONVERSION_FALLBACK",
                        $"Skipping AI conversion for {cobolFile.FileName}: {reason}", ex.GetType().Name);
                    _logger.LogWarning(ex, "Skipping AI conversion for {FileName}. Using fallback conversion. Reason: {Reason}", cobolFile.FileName, reason);
                    return CreateFallbackJavaFile(cobolFile, cobolAnalysis, reason);
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
                    lastException = ex;
                }
                catch (Exception ex)
                {
                    lastException = ex;
                    // Log API call failure
                    _enhancedLogger?.LogApiCallEnd(apiCallId, string.Empty, 0, 0);
                    _enhancedLogger?.LogBehindTheScenes("ERROR", "API_CALL_FAILED",
                        $"API call failed for {cobolFile.FileName}: {ex.Message}", ex);

                    _logger.LogError(ex, "Failed to convert COBOL file to Java: {FileName}", cobolFile.FileName);
                    throw;
                }
            }

            if (string.IsNullOrEmpty(javaCode))
            {
                if (lastException != null && ShouldFallback(lastException))
                {
                    var reason = GetFallbackReason(lastException);
                    return CreateFallbackJavaFile(cobolFile, cobolAnalysis, reason);
                }

                throw new InvalidOperationException($"Failed to convert {cobolFile.FileName} after {maxRetries} attempts", lastException);
            }

            // Log AI response to chat logger
            _chatLogger?.LogAIResponse("JavaConverterAgent", cobolFile.FileName, javaCode);

            // Log API call completion
            _enhancedLogger?.LogApiCallEnd(apiCallId, javaCode, javaCode.Length / 4, 0.002m); // Rough token estimate
            _enhancedLogger?.LogBehindTheScenes("API_CALL", "JAVA_CONVERSION_RESPONSE",
                $"Received Java conversion for {cobolFile.FileName} ({javaCode.Length} chars)");

            // Extract the Java code from markdown code blocks if necessary
            javaCode = ExtractJavaCode(javaCode);

            // Parse file details
            string className = GetClassName(javaCode);
            string packageName = GetPackageName(javaCode);

            var javaFile = new JavaFile
            {
                FileName = $"{className}.java",
                Content = javaCode,
                ClassName = className,
                PackageName = packageName,
                OriginalCobolFileName = cobolFile.FileName
            };

            stopwatch.Stop();
            _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "JAVA_CONVERSION_COMPLETE",
                $"Completed Java conversion of {cobolFile.FileName} in {stopwatch.ElapsedMilliseconds}ms", javaFile);

            _logger.LogInformation("Completed conversion of COBOL file to Java: {FileName}", cobolFile.FileName);

            return javaFile;
        }
        catch (Exception ex) when (ShouldFallback(ex))
        {
            stopwatch.Stop();
            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, GetFallbackReason(ex));
            }

            _enhancedLogger?.LogBehindTheScenes("WARNING", "JAVA_CONVERSION_FALLBACK",
                $"Skipping AI conversion for {cobolFile.FileName}: {GetFallbackReason(ex)}", ex.GetType().Name);
            _logger.LogWarning(ex, "Skipping AI conversion for {FileName}. Using fallback conversion. Reason: {Reason}", cobolFile.FileName, GetFallbackReason(ex));

            return CreateFallbackJavaFile(cobolFile, cobolAnalysis, GetFallbackReason(ex));
        }
        catch (Exception ex)
        {
            stopwatch.Stop();

            // Log API call error
            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, ex.Message);
            }

            _enhancedLogger?.LogBehindTheScenes("ERROR", "JAVA_CONVERSION_ERROR",
                $"Failed to convert {cobolFile.FileName}: {ex.Message}", ex);

            _logger.LogError(ex, "Error converting COBOL file to Java: {FileName}", cobolFile.FileName);
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

    private JavaFile CreateFallbackJavaFile(CobolFile cobolFile, CobolAnalysis cobolAnalysis, string reason)
    {
        var className = GetFallbackClassName(cobolFile.FileName);
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

    private static string GetFallbackClassName(string cobolFileName)
    {
        var baseName = Path.GetFileNameWithoutExtension(cobolFileName);
        if (string.IsNullOrWhiteSpace(baseName))
        {
            baseName = "ConvertedCobolProgram";
        }

        baseName = new string(baseName.Where(char.IsLetterOrDigit).ToArray());
        if (string.IsNullOrWhiteSpace(baseName))
        {
            baseName = "ConvertedCobolProgram";
        }

        if (!char.IsLetter(baseName[0]))
        {
            baseName = "Converted" + baseName;
        }

        return baseName + "Fallback";
    }

    private static bool ShouldFallback(Exception exception)
    {
        return IsUnauthorizedException(exception) || IsNetworkException(exception);
    }

    private static bool IsNetworkException(Exception exception)
    {
        switch (exception)
        {
            case HttpRequestException:
            case SocketException:
                return true;
            case ClientResultException client when client.InnerException != null:
                return IsNetworkException(client.InnerException);
            case HttpOperationException http when http.InnerException != null:
                return IsNetworkException(http.InnerException);
            case AggregateException aggregate:
                return aggregate.InnerExceptions.Any(IsNetworkException);
            default:
                return exception.InnerException != null && IsNetworkException(exception.InnerException);
        }
    }

    private static string GetFallbackReason(Exception exception)
    {
        var innermost = exception;
        while (innermost.InnerException != null)
        {
            innermost = innermost.InnerException;
        }

        var message = innermost.Message;
        return string.IsNullOrWhiteSpace(message)
            ? exception.Message
            : message.Replace('\r', ' ').Replace('\n', ' ').Trim();
    }

    private static bool IsUnauthorizedException(Exception exception)
    {
        var statusCode = ExtractStatusCode(exception);
        return statusCode is 401 or 403;
    }

    private static int? ExtractStatusCode(Exception exception)
    {
        switch (exception)
        {
            case HttpOperationException httpException when httpException.StatusCode.HasValue:
                return (int)httpException.StatusCode.Value;
            case ClientResultException clientException:
                return clientException.Status;
            case AggregateException aggregateException:
                foreach (var inner in aggregateException.InnerExceptions)
                {
                    var aggregateStatus = ExtractStatusCode(inner);
                    if (aggregateStatus.HasValue)
                    {
                        return aggregateStatus;
                    }
                }
                break;
        }

        var statusCodeProperty = exception.GetType().GetRuntimeProperty("StatusCode");
        if (statusCodeProperty?.GetValue(exception) is HttpStatusCode httpStatus)
        {
            return (int)httpStatus;
        }

        if (statusCodeProperty?.GetValue(exception) is int statusInt)
        {
            return statusInt;
        }

        var statusProperty = exception.GetType().GetRuntimeProperty("Status");
        if (statusProperty?.GetValue(exception) is int status)
        {
            return status;
        }

        return exception.InnerException != null ? ExtractStatusCode(exception.InnerException) : null;
    }

    /// <inheritdoc/>
    public async Task<List<JavaFile>> ConvertToJavaAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Converting {Count} COBOL files to Java", cobolFiles.Count);

        var javaFiles = new List<JavaFile>();
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

            var javaFile = await ConvertToJavaAsync(cobolFile, cobolAnalysis);
            javaFiles.Add(javaFile);

            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }

        _logger.LogInformation("Completed conversion of {Count} COBOL files to Java", cobolFiles.Count);

        return javaFiles;
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

        // If no code blocks or extraction failed, return the original input
        return input;
    }

    private string GetClassName(string javaCode)
    {
        try
        {
            // Look for the class declaration
            var lines = javaCode.Split('\n');
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine.StartsWith("public class "))
                {
                    // Extract just the class name
                    var parts = trimmedLine.Split(' ');
                    if (parts.Length >= 3)
                    {
                        var className = parts[2];
                        // Remove any trailing characters like { or implements
                        className = className.Split('{', ' ', '\t', '\r', '\n')[0];

                        // Validate class name (should be alphanumeric + underscore)
                        if (IsValidJavaIdentifier(className))
                        {
                            return className;
                        }
                    }
                }
            }

            // Fallback: look for any class declaration
            var classIndex = javaCode.IndexOf("class ");
            if (classIndex >= 0)
            {
                var start = classIndex + "class ".Length;
                var remaining = javaCode.Substring(start);

                // Take only the first word after "class"
                var firstWord = remaining.Split(' ', '\t', '\r', '\n', '{')[0].Trim();

                if (IsValidJavaIdentifier(firstWord))
                {
                    return firstWord;
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Error extracting class name from Java code");
        }

        // Default to a generic name if extraction fails
        return "ConvertedCobolProgram";
    }

    /// <summary>
    /// Validates if a string is a valid Java identifier
    /// </summary>
    private bool IsValidJavaIdentifier(string identifier)
    {
        if (string.IsNullOrWhiteSpace(identifier))
            return false;

        // Java identifier rules: start with letter/underscore, followed by letters/digits/underscores
        if (!char.IsLetter(identifier[0]) && identifier[0] != '_')
            return false;

        return identifier.All(c => char.IsLetterOrDigit(c) || c == '_');
    }

    private string GetPackageName(string javaCode)
    {
        // Simple regex-like extraction for package name
        // In a real implementation, this would use proper parsing
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

        // Default to a generic package if extraction fails
        return "com.example.cobol";
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
