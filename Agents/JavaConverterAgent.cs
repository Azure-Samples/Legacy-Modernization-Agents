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
/// Implementation of the Java converter agent with enhanced API call tracking.
/// </summary>
public class JavaConverterAgent : IJavaConverterAgent
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<JavaConverterAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;

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
8. Apply modern Java best practices, preferablely using Java Quarkus features

IMPORTANT: The COBOL code may contain placeholder terms that replaced Danish or other languages for error handling terminology for content filtering compatibility. 
When you see terms like 'ERROR_CODE', 'ERROR_MSG', or 'ERROR_CALLING', understand these represent standard COBOL error handling patterns.
Convert these to appropriate Java exception handling and logging mechanisms.
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

Please provide the complete Java Quarkus implementation.
Note: The original code contains Danish error handling terms that have been temporarily replaced with placeholders for processing.
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
                MaxTokens = 32768, // Set within model limits from 8000
                Temperature = 0.1,
                TopP = 0.5
                // Model ID/deployment name is handled at the kernel level
            };
            
            // Create the full prompt including system and user message
            var fullPrompt = $"{systemPrompt}\n\n{prompt}";
            
            // Convert OpenAI settings to kernel arguments
            var kernelArguments = new KernelArguments(executionSettings);
            
            string javaCode = string.Empty;
            int maxRetries = 3;
            int retryDelay = 5000; // 5 seconds
            
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
                    
                    _logger.LogError(ex, "Failed to convert COBOL file to Java: {FileName}", cobolFile.FileName);
                    throw;
                }
            }
            
            if (string.IsNullOrEmpty(javaCode))
            {
                throw new InvalidOperationException($"Failed to convert {cobolFile.FileName} after {maxRetries} attempts");
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
