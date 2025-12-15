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
/// Implementation of the C# converter agent for COBOL to .NET conversion.
/// </summary>
public class CSharpConverterAgent : ICodeConverterAgent
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<CSharpConverterAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;

    public string TargetLanguage => "CSharp";
    public string FileExtension => ".cs";

    /// <summary>
    /// Initializes a new instance of the <see cref="CSharpConverterAgent"/> class.
    /// </summary>
    public CSharpConverterAgent(IKernelBuilder kernelBuilder, ILogger<CSharpConverterAgent> logger, string modelId, EnhancedLogger? enhancedLogger = null, ChatLogger? chatLogger = null)
    {
        _kernelBuilder = kernelBuilder;
        _logger = logger;
        _modelId = modelId;
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
    }

    /// <inheritdoc/>
    public async Task<CodeFile> ConvertAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("Converting COBOL file to C#: {FileName}", cobolFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "CSHARP_CONVERSION_START",
            $"Starting C# conversion of {cobolFile.FileName}", cobolFile.FileName);

        var kernel = _kernelBuilder.Build();
        int apiCallId = 0;

        try
        {
            var systemPrompt = @"
You are an expert in converting COBOL programs to C# with .NET framework. Your task is to convert COBOL source code to modern, maintainable C# code.

Follow these guidelines:
1. Create proper C# class structures from COBOL programs
2. Convert COBOL variables to appropriate C# data types
3. Transform COBOL procedures into C# methods
4. Handle COBOL-specific features (PERFORM, GOTO, etc.) in an idiomatic C# way
5. Implement proper error handling with try-catch blocks
6. Include comprehensive XML documentation comments
7. Apply modern C# best practices (async/await, LINQ, etc.)
8. Use meaningful namespace names (e.g., CobolMigration.Legacy, CobolMigration.BusinessLogic)
9. Return ONLY the C# code without markdown code blocks or additional text
10. Namespace declarations must be single line: 'namespace CobolMigration.Something;'

IMPORTANT: The COBOL code may contain placeholder terms for error handling. Convert these to appropriate C# exception handling.

CRITICAL: Your response MUST start with 'namespace' or 'using' and contain ONLY valid C# code. Do NOT include explanations, notes, or markdown code blocks.
";

            string sanitizedContent = SanitizeCobolContent(cobolFile.Content);

            var prompt = $@"
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

            apiCallId = _enhancedLogger?.LogApiCallStart(
                "CSharpConverterAgent",
                "ChatCompletion",
                "OpenAI/ConvertToCSharp",
                _modelId,
                $"Converting {cobolFile.FileName} ({cobolFile.Content.Length} chars)"
            ) ?? 0;

            _chatLogger?.LogUserMessage("CSharpConverterAgent", cobolFile.FileName, prompt, systemPrompt);

            var executionSettings = new OpenAIPromptExecutionSettings
            {
                ExtensionData = new Dictionary<string, object>
                {
                    ["max_completion_tokens"] = 32768
                }
            };

            var fullPrompt = $"{systemPrompt}\n\n{prompt}";
            var kernelArguments = new KernelArguments(executionSettings);

            string csharpCode = string.Empty;
            int maxRetries = 3;
            int retryDelay = 5000;
            Exception? lastException = null;

            for (int attempt = 1; attempt <= maxRetries; attempt++)
            {
                try
                {
                    _logger.LogInformation("Converting COBOL to C# - Attempt {Attempt}/{MaxRetries} for {FileName}",
                        attempt, maxRetries, cobolFile.FileName);

                    var functionResult = await kernel.InvokePromptAsync(fullPrompt, kernelArguments);
                    csharpCode = functionResult.GetValue<string>() ?? string.Empty;
                    break;
                }
                catch (Exception ex) when (ShouldFallback(ex))
                {
                    lastException = ex;
                    var reason = GetFallbackReason(ex);
                    _enhancedLogger?.LogApiCallError(apiCallId, reason);
                    return CreateFallbackCodeFile(cobolFile, cobolAnalysis, reason);
                }
                catch (Exception ex) when (attempt < maxRetries && (
                    ex.Message.Contains("canceled") ||
                    ex.Message.Contains("timeout") ||
                    ex.Message.Contains("content_filter")))
                {
                    _logger.LogWarning("Attempt {Attempt} failed: {Error}. Retrying...", attempt, ex.Message);
                    await Task.Delay(retryDelay);
                    retryDelay *= 2;
                    lastException = ex;
                }
                catch (Exception ex)
                {
                    lastException = ex;
                    _enhancedLogger?.LogApiCallEnd(apiCallId, string.Empty, 0, 0);
                    _logger.LogError(ex, "Failed to convert COBOL file to C#: {FileName}", cobolFile.FileName);
                    throw;
                }
            }

            if (string.IsNullOrEmpty(csharpCode))
            {
                if (lastException != null && ShouldFallback(lastException))
                {
                    return CreateFallbackCodeFile(cobolFile, cobolAnalysis, GetFallbackReason(lastException));
                }
                throw new InvalidOperationException($"Failed to convert {cobolFile.FileName} after {maxRetries} attempts", lastException);
            }

            _chatLogger?.LogAIResponse("CSharpConverterAgent", cobolFile.FileName, csharpCode);
            _enhancedLogger?.LogApiCallEnd(apiCallId, csharpCode, csharpCode.Length / 4, 0.002m);

            csharpCode = ExtractCSharpCode(csharpCode);

            string className = GetClassName(csharpCode);
            string namespaceName = GetNamespaceName(csharpCode);

            var codeFile = new CodeFile
            {
                FileName = $"{className}.cs",
                Content = csharpCode,
                ClassName = className,
                NamespaceName = namespaceName,
                OriginalCobolFileName = cobolFile.FileName,
                TargetLanguage = TargetLanguage
            };

            stopwatch.Stop();
            _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "CSHARP_CONVERSION_COMPLETE",
                $"Completed C# conversion of {cobolFile.FileName} in {stopwatch.ElapsedMilliseconds}ms", codeFile);

            return codeFile;
        }
        catch (Exception ex) when (ShouldFallback(ex))
        {
            stopwatch.Stop();
            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, GetFallbackReason(ex));
            }
            return CreateFallbackCodeFile(cobolFile, cobolAnalysis, GetFallbackReason(ex));
        }
    }

    /// <inheritdoc/>
    public async Task<List<CodeFile>> ConvertAsync(List<CobolFile> cobolFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Converting {Count} COBOL files to C#", cobolFiles.Count);

        var codeFiles = new List<CodeFile>();
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

            var codeFile = await ConvertAsync(cobolFile, cobolAnalysis);
            codeFiles.Add(codeFile);

            processedCount++;
            progressCallback?.Invoke(processedCount, cobolFiles.Count);
        }

        return codeFiles;
    }

    private CodeFile CreateFallbackCodeFile(CobolFile cobolFile, CobolAnalysis cobolAnalysis, string reason)
    {
        var className = GetFallbackClassName(cobolFile.FileName);
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
        throw new NotSupportedException("AI conversion unavailable. Please supply valid Azure OpenAI credentials and rerun the migration. Details: {{sanitizedReason}}");
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

    private static string GetFallbackClassName(string cobolFileName)
    {
        var baseName = Path.GetFileNameWithoutExtension(cobolFileName);
        if (string.IsNullOrWhiteSpace(baseName))
            baseName = "ConvertedCobolProgram";

        baseName = new string(baseName.Where(char.IsLetterOrDigit).ToArray());
        if (string.IsNullOrWhiteSpace(baseName))
            baseName = "ConvertedCobolProgram";

        if (!char.IsLetter(baseName[0]))
            baseName = "Converted" + baseName;

        return baseName + "Fallback";
    }

    private static bool ShouldFallback(Exception exception) =>
        IsUnauthorizedException(exception) || IsNetworkException(exception);

    private static bool IsNetworkException(Exception exception) =>
        exception switch
        {
            HttpRequestException or SocketException => true,
            ClientResultException client when client.InnerException != null => IsNetworkException(client.InnerException),
            HttpOperationException http when http.InnerException != null => IsNetworkException(http.InnerException),
            AggregateException aggregate => aggregate.InnerExceptions.Any(IsNetworkException),
            _ => exception.InnerException != null && IsNetworkException(exception.InnerException)
        };

    private static string GetFallbackReason(Exception exception)
    {
        var innermost = exception;
        while (innermost.InnerException != null)
            innermost = innermost.InnerException;

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

    private static int? ExtractStatusCode(Exception exception) =>
        exception switch
        {
            HttpOperationException httpEx when httpEx.StatusCode.HasValue => (int)httpEx.StatusCode.Value,
            ClientResultException clientEx => clientEx.Status,
            AggregateException aggregateEx => aggregateEx.InnerExceptions
                .Select(ExtractStatusCode)
                .FirstOrDefault(s => s.HasValue),
            _ => exception.InnerException != null ? ExtractStatusCode(exception.InnerException) : null
        };

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

    private string GetClassName(string csharpCode)
    {
        try
        {
            var lines = csharpCode.Split('\n');
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine.StartsWith("public class ") || trimmedLine.StartsWith("internal class ") || trimmedLine.StartsWith("class "))
                {
                    var parts = trimmedLine.Split(' ');
                    var classIndex = Array.IndexOf(parts, "class");
                    if (classIndex >= 0 && classIndex + 1 < parts.Length)
                    {
                        var className = parts[classIndex + 1];
                        className = className.Split('{', ' ', '\t', '\r', '\n', ':')[0];

                        if (IsValidCSharpIdentifier(className))
                            return className;
                    }
                }
            }
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Error extracting class name from C# code");
        }

        return "ConvertedCobolProgram";
    }

    private bool IsValidCSharpIdentifier(string identifier)
    {
        if (string.IsNullOrWhiteSpace(identifier))
            return false;

        if (!char.IsLetter(identifier[0]) && identifier[0] != '_')
            return false;

        return identifier.All(c => char.IsLetterOrDigit(c) || c == '_');
    }

    private string GetNamespaceName(string csharpCode)
    {
        var namespaceIndex = csharpCode.IndexOf("namespace ");
        if (namespaceIndex >= 0)
        {
            var start = namespaceIndex + "namespace ".Length;
            var remaining = csharpCode.Substring(start);
            var end = remaining.IndexOfAny(new[] { ';', '{', '\r', '\n' });

            if (end >= 0)
                return remaining.Substring(0, end).Trim();
        }

        return "CobolMigration.Legacy";
    }

    private string SanitizeCobolContent(string cobolContent)
    {
        if (string.IsNullOrEmpty(cobolContent))
            return cobolContent;

        var sanitizationMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            {"FEJL", "ERROR_CODE"},
            {"FEJLMELD", "ERROR_MSG"},
            {"FEJL-", "ERROR_"},
            {"FEJLMELD-", "ERROR_MSG_"},
            {"INC-FEJLMELD", "INC-ERROR-MSG"},
            {"FEJL VED KALD", "ERROR IN CALL"},
            {"KALD", "CALL_OP"},
            {"MEDD-TEKST", "MSG_TEXT"},
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
