using Microsoft.Extensions.AI;
using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Agents.Infrastructure;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Helpers;
using System.Diagnostics;
using System.Text;

namespace CobolToQuarkusMigration.Agents;

/// <summary>
/// Chunk-aware C# converter that processes large files piece by piece
/// while maintaining consistency across chunks. Supports both Responses API (codex) and Chat Completions API.
/// </summary>
public class ChunkAwareCSharpConverter : AgentBase, IChunkAwareConverter
{
    private readonly ConversionSettings _conversionSettings;
    private int? _runId;

    public string TargetLanguage => "CSharp";
    public string FileExtension => ".cs";
    protected override string AgentName => "ChunkAwareCSharpConverter";

    /// <summary>
    /// Sets the Run ID for the current context.
    /// </summary>
    public void SetRunId(int runId)
    {
        _runId = runId;
    }

    /// <summary>
    /// Initializes a new instance using Responses API (for codex models like gpt-5.1-codex-mini).
    /// </summary>
    public ChunkAwareCSharpConverter(
        ResponsesApiClient responsesClient,
        ILogger<ChunkAwareCSharpConverter> logger,
        string modelId,
        ConversionSettings conversionSettings,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null,
        int? runId = null)
        : base(responsesClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
        _conversionSettings = conversionSettings;
    }

    /// <summary>
    /// Initializes a new instance using Chat Completions API (for chat models).
    /// </summary>
    public ChunkAwareCSharpConverter(
        IChatClient chatClient,
        ILogger<ChunkAwareCSharpConverter> logger,
        string modelId,
        ConversionSettings conversionSettings,
        EnhancedLogger? enhancedLogger = null,
        ChatLogger? chatLogger = null,
        RateLimiter? rateLimiter = null,
        AppSettings? settings = null,
        int? runId = null)
        : base(chatClient, logger, modelId, enhancedLogger, chatLogger, rateLimiter, settings)
    {
        _conversionSettings = conversionSettings;
    }

    private int MaxContentChars => Settings?.ChunkingSettings?.AutoChunkCharThreshold ?? 150_000;

    /// <inheritdoc/>
    public async Task<ChunkConversionResult> ConvertChunkAsync(
        ChunkResult chunk,
        ChunkContext context,
        CancellationToken cancellationToken = default)
    {
        var stopwatch = Stopwatch.StartNew();
        Logger.LogInformation(
            "Converting chunk {Index} of {File} to C# (lines {Start}-{End}, {ContentLen} chars)",
            chunk.ChunkIndex, chunk.SourceFile, chunk.StartLine, chunk.EndLine, chunk.Content?.Length ?? 0);

        var contentLength = chunk.Content?.Length ?? 0;
        if (contentLength > MaxContentChars)
        {
            var errorMsg = $"❌ CHUNK TOO LARGE: Chunk {chunk.ChunkIndex} has {contentLength:N0} chars (max: {MaxContentChars:N0}).";
            Logger.LogError(errorMsg);
            
            return new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = false,
                ErrorMessage = errorMsg,
                ProcessingTimeMs = stopwatch.ElapsedMilliseconds
            };
        }

        try
        {
            var systemPrompt = BuildChunkAwareSystemPrompt(chunk, context);
            var userPrompt = BuildChunkAwareUserPrompt(chunk, context);

            var (csharpCode, usedFallback, fallbackReason) = await ExecuteWithFallbackAsync(
                systemPrompt, userPrompt, $"{chunk.SourceFile} chunk {chunk.ChunkIndex}");

            stopwatch.Stop();

            if (usedFallback)
            {
                return new ChunkConversionResult
                {
                    ChunkIndex = chunk.ChunkIndex,
                    SourceFile = chunk.SourceFile,
                    Success = false,
                    ErrorMessage = fallbackReason ?? "Fallback triggered",
                    ProcessingTimeMs = stopwatch.ElapsedMilliseconds
                };
            }

            csharpCode = ExtractCSharpCode(csharpCode);
            var definedMethods = ExtractDefinedMethods(csharpCode);

            return new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = true,
                ConvertedCode = csharpCode,
                DefinedMethods = definedMethods,
                ProcessingTimeMs = stopwatch.ElapsedMilliseconds
            };
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            Logger.LogError(ex, "Error converting chunk {Index} of {File}", chunk.ChunkIndex, chunk.SourceFile);

            return new ChunkConversionResult
            {
                ChunkIndex = chunk.ChunkIndex,
                SourceFile = chunk.SourceFile,
                Success = false,
                ErrorMessage = ex.Message,
                ProcessingTimeMs = stopwatch.ElapsedMilliseconds
            };
        }
    }

    /// <inheritdoc/>
    public async Task<ChunkConversionResult> ApplyCorrectionsAsync(
        ChunkResult originalChunk,
        ChunkConversionResult originalConversion,
        IReadOnlyList<string> corrections,
        CancellationToken cancellationToken = default)
    {
        var stopwatch = Stopwatch.StartNew();
        Logger.LogInformation("Applying {Count} corrections to chunk {Index} of {File}",
            corrections.Count, originalChunk.ChunkIndex, originalChunk.SourceFile);

        try
        {
            var systemPrompt = @"You are an expert C# code reviewer. Apply the following corrections:
" + string.Join("\n", corrections.Select((c, i) => $"{i + 1}. {c}")) + @"

Return ONLY the corrected C# code. No explanations. No markdown blocks.";

            var userPrompt = $@"Apply the corrections to this C# code:

```csharp
{originalConversion.ConvertedCode}
```";

            var (correctedCode, usedFallback, fallbackReason) = await ExecuteWithFallbackAsync(
                systemPrompt, userPrompt, $"{originalChunk.SourceFile} chunk {originalChunk.ChunkIndex} corrections");

            stopwatch.Stop();

            if (usedFallback)
            {
                return new ChunkConversionResult
                {
                    ChunkIndex = originalChunk.ChunkIndex,
                    SourceFile = originalChunk.SourceFile,
                    Success = false,
                    ErrorMessage = fallbackReason ?? "Correction fallback triggered",
                    ProcessingTimeMs = stopwatch.ElapsedMilliseconds
                };
            }

            correctedCode = ExtractCSharpCode(correctedCode);

            return new ChunkConversionResult
            {
                ChunkIndex = originalChunk.ChunkIndex,
                SourceFile = originalChunk.SourceFile,
                Success = true,
                ConvertedCode = correctedCode,
                DefinedMethods = ExtractDefinedMethods(correctedCode),
                ProcessingTimeMs = stopwatch.ElapsedMilliseconds
            };
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            Logger.LogError(ex, "Error applying corrections to chunk {Index}", originalChunk.ChunkIndex);

            return new ChunkConversionResult
            {
                ChunkIndex = originalChunk.ChunkIndex,
                SourceFile = originalChunk.SourceFile,
                Success = false,
                ErrorMessage = ex.Message,
                ProcessingTimeMs = stopwatch.ElapsedMilliseconds
            };
        }
    }

    /// <inheritdoc/>
    public async Task<CodeFile> ConvertAsync(CobolFile cobolFile, CobolAnalysis cobolAnalysis)
    {
        var chunk = new ChunkResult
        {
            ChunkIndex = 0,
            SourceFile = cobolFile.FileName,
            Content = cobolFile.Content,
            StartLine = 1,
            EndLine = cobolFile.Content.Split('\n').Length
        };

        var context = new ChunkContext { TotalChunks = 1 };
        var result = await ConvertChunkAsync(chunk, context);

        return new CodeFile
        {
            // Use centralized NamingHelper for consistent naming
            FileName = NamingHelper.GetOutputFileName(cobolFile.FileName, FileExtension),
            Content = result.ConvertedCode ?? string.Empty,
            ClassName = NamingHelper.DeriveClassNameFromCobolFile(cobolFile.FileName),
            OriginalCobolFileName = cobolFile.FileName,
            TargetLanguage = TargetLanguage
        };
    }

    /// <inheritdoc/>
    public async Task<List<CodeFile>> ConvertAsync(
        List<CobolFile> cobolFiles,
        List<CobolAnalysis> cobolAnalyses,
        Action<int, int>? progressCallback = null)
    {
        var maxParallel = Math.Min(
            Settings?.ChunkingSettings?.MaxParallelConversion ?? 1, cobolFiles.Count);
        var enableParallel = maxParallel > 1 && cobolFiles.Count > 1;

        if (enableParallel)
        {
            Logger.LogInformation(
                "\u26a1 Parallel conversion: {Workers} workers for {Files} files",
                maxParallel, cobolFiles.Count);

            var staggerDelay = Settings?.ChunkingSettings?.ParallelStaggerDelayMs ?? 500;
            using var semaphore = new SemaphoreSlim(maxParallel, maxParallel);
            var completed = 0;

            var tasks = cobolFiles.Select((cobolFile, i) =>
            {
                var analysis = i < cobolAnalyses.Count ? cobolAnalyses[i] : null;
                return Task.Run(async () =>
                {
                    await semaphore.WaitAsync();
                    try
                    {
                        if (analysis == null)
                        {
                            Logger.LogWarning("No analysis found for file: {FileName}", cobolFile.FileName);
                            return (Index: i, Result: (CodeFile?)null);
                        }

                        await Task.Delay(i * staggerDelay);
                        var result = await ConvertAsync(cobolFile, analysis);
                        var done = Interlocked.Increment(ref completed);
                        progressCallback?.Invoke(done, cobolFiles.Count);
                        return (Index: i, Result: (CodeFile?)result);
                    }
                    finally
                    {
                        semaphore.Release();
                    }
                });
            }).ToList();

            var all = await Task.WhenAll(tasks);
            return all.Where(r => r.Result != null)
                      .OrderBy(r => r.Index)
                      .Select(r => r.Result!)
                      .ToList();
        }

        // Sequential fallback
        var results = new List<CodeFile>();
        for (int i = 0; i < cobolFiles.Count; i++)
        {
            var cobolFile = cobolFiles[i];
            var analysis = i < cobolAnalyses.Count ? cobolAnalyses[i] : null;

            if (analysis == null)
            {
                Logger.LogWarning("No analysis found for file: {FileName}", cobolFile.FileName);
                continue;
            }

            var result = await ConvertAsync(cobolFile, analysis);
            results.Add(result);
            progressCallback?.Invoke(i + 1, cobolFiles.Count);
        }

        return results;
    }

    private string BuildChunkAwareSystemPrompt(ChunkResult chunk, ChunkContext context)
    {
        var sb = new StringBuilder();
        
        sb.AppendLine(@"You are an expert COBOL to C#/.NET converter specializing in processing large files in chunks.

CRITICAL: You are converting CHUNK " + (chunk.ChunkIndex + 1) + " of " + context.TotalChunks + @" for this file.

LANGUAGE REQUIREMENT (CRITICAL):
- ALL generated code, comments, variable names, and documentation MUST be in ENGLISH
- Translate any non-English comments or identifiers from the source COBOL to English
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
- The output must be FUNCTIONALLY EQUIVALENT to the input");

        sb.AppendLine(@"
Guidelines:
1. Convert COBOL to modern C# with .NET 8 patterns
2. Use proper C# naming conventions (PascalCase for methods/properties)
3. Handle COBOL-specific features (PERFORM, GOTO) idiomatically
4. Include comprehensive XML documentation comments
5. Return ONLY C# code - no markdown blocks, no explanations
6. Use appropriate namespaces (e.g., CobolMigration)

ANTI-ABSTRACTION RULES:
- Do NOT represent business logic as DATA (e.g., List<Operation>, Dictionary<string, Action>)
- Business logic must be EXECUTABLE CODE, not configuration or data entries
- Do NOT create generic 'Execute(operationName)' dispatchers
- Each distinct business operation must have its own implementation

CHUNK-SPECIFIC INSTRUCTIONS:");

        if (chunk.ChunkIndex == 0)
        {
            sb.AppendLine(@"
- This is the FIRST chunk - include using statements and namespace
- Include class declaration with opening brace
- Do NOT close the class (more chunks follow)
- Initialize any fields needed for the file

CLASS NAMING - CRITICAL:
Name the class based on WHAT THE PROGRAM DOES, not the original filename.
Use pattern: <Domain><Action><Type>
Examples: PaymentBatchValidator, CustomerOnboardingService, LedgerReconciliationJob
Common suffixes: Service, Processor, Handler, Validator, Calculator, Generator, Job, Worker");
        }
        else if (chunk.ChunkIndex == context.TotalChunks - 1)
        {
            sb.AppendLine(@"
- This is the LAST chunk - include closing brace for the class and namespace
- Complete any remaining methods
- Ensure all brackets are balanced");
        }
        else
        {
            sb.AppendLine(@"
- This is a MIDDLE chunk - continue from previous chunk
- Do NOT include using/namespace/class declaration
- Do NOT close the class yet
- Just output method bodies and properties");
        }

        // Add context from previous chunks
        if (context.PreviousSignatures.Any())
        {
            sb.AppendLine("\nMethods defined in previous chunks (maintain consistency):");
            foreach (var sig in context.PreviousSignatures.Take(20))
            {
                sb.AppendLine($"  - {sig.TargetSignature}");
            }
        }

        if (context.PreviousVariables.Any())
        {
            sb.AppendLine("\nVariables defined in previous chunks:");
            foreach (var variable in context.PreviousVariables.Take(30))
            {
                sb.AppendLine($"  - {variable.TargetType} {variable.TargetName}");
            }
        }

        return sb.ToString();
    }

    private string BuildChunkAwareUserPrompt(ChunkResult chunk, ChunkContext context)
    {
        var sb = new StringBuilder();

        sb.AppendLine($"Convert this COBOL chunk (lines {chunk.StartLine}-{chunk.EndLine}) to C#:");
        sb.AppendLine();
        sb.AppendLine("```cobol");
        sb.AppendLine(SanitizeCobolContent(chunk.Content ?? string.Empty));
        sb.AppendLine("```");
        sb.AppendLine();

        if (chunk.SemanticUnitNames.Any())
        {
            sb.AppendLine("Semantic units in this chunk:");
            foreach (var unit in chunk.SemanticUnitNames)
            {
                sb.AppendLine($"  - {unit}");
            }
            sb.AppendLine();
        }

        if (context.PendingForwardReferences.Any())
        {
            sb.AppendLine("References to resolve from previous chunks:");
            foreach (var reference in context.PendingForwardReferences.Take(10))
            {
                sb.AppendLine($"  - {reference.TargetMethod}");
            }
            sb.AppendLine();
        }

        sb.AppendLine("Return ONLY C# code. No markdown blocks. No explanations.");

        return sb.ToString();
    }

    private string ExtractCSharpCode(string input)
    {
        if (string.IsNullOrWhiteSpace(input))
            return input;

        if (input.Contains("```csharp") || input.Contains("```cs"))
        {
            var startMarker = input.Contains("```csharp") ? "```csharp" : "```cs";
            int startIndex = input.IndexOf(startMarker);
            if (startIndex >= 0)
            {
                startIndex += startMarker.Length;
                int endIndex = input.IndexOf("```", startIndex);
                if (endIndex >= 0)
                {
                    return input.Substring(startIndex, endIndex - startIndex).Trim();
                }
            }
        }

        if (input.StartsWith("```"))
        {
            var lines = input.Split('\n');
            var codeLines = new List<string>();
            var inCodeBlock = false;

            foreach (var line in lines)
            {
                if (line.TrimStart().StartsWith("```"))
                {
                    inCodeBlock = !inCodeBlock;
                    continue;
                }
                if (inCodeBlock) codeLines.Add(line);
            }

            if (codeLines.Any())
                return string.Join("\n", codeLines).Trim();
        }

        return input.Trim();
    }

    private List<MethodSignature> ExtractDefinedMethods(string csharpCode)
    {
        var methods = new List<MethodSignature>();
        if (string.IsNullOrWhiteSpace(csharpCode)) return methods;

        var lines = csharpCode.Split('\n');
        foreach (var line in lines)
        {
            var trimmed = line.Trim();
            if ((trimmed.StartsWith("public ") || trimmed.StartsWith("private ") || 
                 trimmed.StartsWith("protected ") || trimmed.StartsWith("internal ") ||
                 trimmed.StartsWith("async ")) &&
                trimmed.Contains("(") && !trimmed.Contains("=") && !trimmed.Contains("new "))
            {
                var parenClose = trimmed.IndexOf(')');
                if (parenClose > 0)
                {
                    var signature = trimmed.Substring(0, parenClose + 1);
                    methods.Add(new MethodSignature
                    {
                        TargetSignature = signature,
                        TargetMethodName = ExtractMethodName(signature)
                    });
                }
            }
        }
        return methods;
    }

    private string ExtractMethodName(string signature)
    {
        var parenIndex = signature.IndexOf('(');
        if (parenIndex <= 0) return string.Empty;
        
        var beforeParen = signature.Substring(0, parenIndex).Trim();
        var parts = beforeParen.Split(' ', StringSplitOptions.RemoveEmptyEntries);
        return parts.Length > 0 ? parts[^1] : string.Empty;
    }

    private string SanitizeCobolContent(string cobolContent)
    {
        if (string.IsNullOrEmpty(cobolContent)) return cobolContent;

        var sanitizationMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            {"FEJL", "ERROR_CODE"},
            {"FEJLMELD", "ERROR_MSG"},
            {"FEJL-", "ERROR_"},
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
