using Microsoft.Extensions.Logging;
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.Connectors.OpenAI;
using CobolModernization.Agents.Interfaces;
using CobolModernization.Models;
using CobolModernization.Helpers;
using System.Diagnostics;
using System.Text;

namespace CobolModernization.Agents;

/// <summary>
/// Implementation of the unit test agent with AI-powered test generation.
/// </summary>
public class UnitTestAgent : IUnitTestAgent
{
    private readonly IKernelBuilder _kernelBuilder;
    private readonly ILogger<UnitTestAgent> _logger;
    private readonly string _modelId;
    private readonly EnhancedLogger? _enhancedLogger;
    private readonly ChatLogger? _chatLogger;

    /// <summary>
    /// Initializes a new instance of the <see cref="UnitTestAgent"/> class.
    /// </summary>
    /// <param name="kernelBuilder">The kernel builder.</param>
    /// <param name="logger">The logger.</param>
    /// <param name="modelId">The model ID to use for test generation.</param>
    /// <param name="enhancedLogger">Enhanced logger for API call tracking.</param>
    /// <param name="chatLogger">Chat logger for Azure OpenAI conversation tracking.</param>
    public UnitTestAgent(IKernelBuilder kernelBuilder, ILogger<UnitTestAgent> logger, string modelId, EnhancedLogger? enhancedLogger = null, ChatLogger? chatLogger = null)
    {
        _kernelBuilder = kernelBuilder;
        _logger = logger;
        _modelId = modelId;
        _enhancedLogger = enhancedLogger;
        _chatLogger = chatLogger;
    }

    /// <inheritdoc/>
    public async Task<JavaFile> GenerateUnitTestsAsync(JavaFile javaFile, CobolAnalysis cobolAnalysis)
    {
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("Generating unit tests for Java file: {FileName}", javaFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "UNIT_TEST_GENERATION_START",
            $"Starting unit test generation for {javaFile.FileName}");

        var kernel = _kernelBuilder.Build();
        int apiCallId = 0;

        try
        {
            var systemPrompt = @"
You are an expert Java testing engineer specializing in JUnit 5, Mockito, and Quarkus testing.
Your task is to generate comprehensive unit tests for Java code converted from COBOL.

Guidelines:
1. Use JUnit 5 (@Test, @BeforeEach, @AfterEach, etc.)
2. Use Mockito for mocking dependencies (@Mock, @InjectMocks)
3. Follow Arrange-Act-Assert pattern
4. Test edge cases, null checks, boundary conditions
5. Include integration tests for database operations if applicable
6. Use Quarkus testing annotations (@QuarkusTest) when testing Quarkus components
7. Ensure tests verify the COBOL business logic is preserved
8. Include meaningful test names that describe what is being tested
9. Aim for high code coverage (>80%)
10. Add comments explaining complex test scenarios

Return ONLY the complete Java test class code, no explanations or markdown.
";

            var prompt = $@"
Generate comprehensive JUnit 5 tests for the following Java class converted from COBOL:

Original COBOL File: {javaFile.OriginalCobolFileName}
Java Class Name: {javaFile.ClassName}
Package: {javaFile.PackageName}

COBOL Analysis (for business logic reference):
{cobolAnalysis.ProgramDescription}

Data Structures from COBOL:
{string.Join("\n", cobolAnalysis.Variables.Take(15).Select(v => $"- {v.Name} ({v.Level}): {v.Type} {v.Size}"))}

Business Logic (Paragraphs/Sections):
{string.Join("\n", cobolAnalysis.Paragraphs.Take(10).Select(p => $"- {p.Name}: {p.Description}"))}

Procedure Divisions:
{string.Join("\n", cobolAnalysis.ProcedureDivisions.Take(5))}

Java Code to Test:
```java
{TruncateForPrompt(javaFile.Content, 15000)}
```

Generate a complete test class with:
1. Setup and teardown methods
2. Tests for each public method
3. Tests for edge cases and error handling
4. Tests that verify COBOL business logic is preserved
5. Mock dependencies where needed
6. Use QuarkusTest if the class uses Quarkus features

Test Class Name: {javaFile.ClassName}Test
Package: {javaFile.PackageName}
";

            // Log API call start
            apiCallId = _enhancedLogger?.LogApiCallStart(
                "UnitTestAgent",
                "ChatCompletion",
                "OpenAI/GenerateTests",
                _modelId,
                $"Generating tests for {javaFile.FileName}"
            ) ?? 0;

            _enhancedLogger?.LogBehindTheScenes("API_CALL", "TEST_GENERATION_REQUEST",
                $"Requesting AI to generate unit tests for {javaFile.ClassName}");

            var executionSettings = new OpenAIPromptExecutionSettings
            {
                MaxTokens = 32768,
                Temperature = 0.3,
                TopP = 0.8
            };

            var kernelArguments = new KernelArguments(executionSettings);
            var fullPrompt = $"{systemPrompt}\n\n{prompt}";

            // Log user message to chat logger
            _chatLogger?.LogUserMessage("UnitTestAgent", javaFile.FileName, prompt, systemPrompt);

            var functionResult = await kernel.InvokePromptAsync(fullPrompt, kernelArguments);
            var testCode = functionResult.GetValue<string>() ?? string.Empty;

            // Log AI response to chat logger
            _chatLogger?.LogAIResponse("UnitTestAgent", javaFile.FileName, testCode);

            // Log API call completion with token estimation
            var tokensUsed = testCode.Length / 4; // Rough estimation
            _enhancedLogger?.LogApiCallEnd(apiCallId, testCode, tokensUsed, 0.001m);
            _enhancedLogger?.LogBehindTheScenes("API_CALL", "TEST_GENERATION_RESPONSE",
                $"Received unit test code ({testCode.Length} chars)");

            // Clean up the test code
            testCode = ExtractJavaCode(testCode);

            var testFile = new JavaFile
            {
                FileName = $"{javaFile.ClassName}Test.java",
                Content = testCode,
                PackageName = javaFile.PackageName,
                ClassName = $"{javaFile.ClassName}Test",
                OriginalCobolFileName = javaFile.OriginalCobolFileName
            };

            stopwatch.Stop();
            _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "UNIT_TEST_GENERATION_COMPLETE",
                $"Completed unit test generation for {javaFile.FileName} in {stopwatch.ElapsedMilliseconds}ms");

            _logger.LogInformation("Successfully generated unit tests for {FileName}", javaFile.FileName);

            return testFile;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();

            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, ex.Message);
            }

            _enhancedLogger?.LogBehindTheScenes("ERROR", "UNIT_TEST_GENERATION_ERROR",
                $"Failed to generate unit tests for {javaFile.FileName} after {stopwatch.ElapsedMilliseconds}ms: {ex.Message}", ex);

            _logger.LogError(ex, "Error generating unit tests for {FileName}", javaFile.FileName);
            throw;
        }
    }

    /// <inheritdoc/>
    public async Task<List<JavaFile>> GenerateUnitTestsAsync(List<JavaFile> javaFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Generating unit tests for {Count} Java files", javaFiles.Count);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "BATCH_TEST_GENERATION_START",
            $"Starting batch unit test generation for {javaFiles.Count} files");

        var testFiles = new List<JavaFile>();
        int completed = 0;

        foreach (var javaFile in javaFiles)
        {
            // Find corresponding COBOL analysis
            var cobolAnalysis = cobolAnalyses.FirstOrDefault(a =>
                a.FileName.Replace(".cbl", "").Equals(javaFile.OriginalCobolFileName.Replace(".cbl", ""), StringComparison.OrdinalIgnoreCase))
                ?? cobolAnalyses.FirstOrDefault();

            if (cobolAnalysis == null)
            {
                _logger.LogWarning("No COBOL analysis found for {FileName}, skipping test generation", javaFile.FileName);
                continue;
            }

            try
            {
                var testFile = await GenerateUnitTestsAsync(javaFile, cobolAnalysis);
                testFiles.Add(testFile);

                completed++;
                progressCallback?.Invoke(completed, javaFiles.Count);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Failed to generate tests for {FileName}, continuing with others", javaFile.FileName);
            }
        }

        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "BATCH_TEST_GENERATION_COMPLETE",
            $"Completed batch test generation: {testFiles.Count}/{javaFiles.Count} successful");

        _logger.LogInformation("Generated {Generated} test files out of {Total} Java files", testFiles.Count, javaFiles.Count);

        return testFiles;
    }

    /// <summary>
    /// Generates unit tests for C# files.
    /// </summary>
    public async Task<CSharpFile> GenerateUnitTestsAsync(CSharpFile csharpFile, CobolAnalysis cobolAnalysis)
    {
        var stopwatch = Stopwatch.StartNew();

        _logger.LogInformation("Generating unit tests for C# file: {FileName}", csharpFile.FileName);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "UNIT_TEST_GENERATION_START",
            $"Starting unit test generation for {csharpFile.FileName}");

        var kernel = _kernelBuilder.Build();
        int apiCallId = 0;

        try
        {
            var systemPrompt = @"
You are an expert C# testing engineer specializing in xUnit, NUnit, and Moq.
Your task is to generate comprehensive unit tests for C# code converted from COBOL.

Guidelines:
1. Use xUnit (@Fact, @Theory, IDisposable for setup/teardown)
2. Use Moq for mocking dependencies
3. Follow Arrange-Act-Assert pattern
4. Test edge cases, null checks, boundary conditions
5. Include integration tests for database operations if applicable
6. Ensure tests verify the COBOL business logic is preserved
7. Include meaningful test names that describe what is being tested
8. Aim for high code coverage (>80%)
9. Add comments explaining complex test scenarios
10. Use FluentAssertions for readable assertions

Return ONLY the complete C# test class code, no explanations or markdown.
";

            var prompt = $@"
Generate comprehensive xUnit tests for the following C# class converted from COBOL:

Original COBOL File: {csharpFile.OriginalCobolFileName}
C# Class Name: {csharpFile.ClassName}
Namespace: {csharpFile.Namespace}

COBOL Analysis (for business logic reference):
{cobolAnalysis.ProgramDescription}

Data Structures from COBOL:
{string.Join("\n", cobolAnalysis.Variables.Take(15).Select(v => $"- {v.Name} ({v.Level}): {v.Type} {v.Size}"))}

Business Logic (Paragraphs/Sections):
{string.Join("\n", cobolAnalysis.Paragraphs.Take(10).Select(p => $"- {p.Name}: {p.Description}"))}

Procedure Divisions:
{string.Join("\n", cobolAnalysis.ProcedureDivisions.Take(5))}

C# Code to Test:
```csharp
{TruncateForPrompt(csharpFile.Content, 15000)}
```

Generate a complete test class with:
1. Constructor and IDisposable for setup/teardown
2. Tests for each public method using [Fact] or [Theory]
3. Tests for edge cases and error handling
4. Tests that verify COBOL business logic is preserved
5. Mock dependencies using Moq where needed
6. Use FluentAssertions for assertions (e.g., result.Should().Be(expected))

Test Class Name: {csharpFile.ClassName}Tests
Namespace: {csharpFile.Namespace}.Tests
";

            // Log API call start
            apiCallId = _enhancedLogger?.LogApiCallStart(
                "UnitTestAgent",
                "ChatCompletion",
                "OpenAI/GenerateTests",
                _modelId,
                $"Generating tests for {csharpFile.FileName}"
            ) ?? 0;

            _enhancedLogger?.LogBehindTheScenes("API_CALL", "TEST_GENERATION_REQUEST",
                $"Requesting AI to generate unit tests for {csharpFile.ClassName}");

            var executionSettings = new OpenAIPromptExecutionSettings
            {
                MaxTokens = 32768,
                Temperature = 0.3,
                TopP = 0.8
            };

            var kernelArguments = new KernelArguments(executionSettings);
            var fullPrompt = $"{systemPrompt}\n\n{prompt}";

            // Log user message to chat logger
            _chatLogger?.LogUserMessage("UnitTestAgent", csharpFile.FileName, prompt, systemPrompt);

            var functionResult = await kernel.InvokePromptAsync(fullPrompt, kernelArguments);
            var testCode = functionResult.GetValue<string>() ?? string.Empty;

            // Log AI response to chat logger
            _chatLogger?.LogAIResponse("UnitTestAgent", csharpFile.FileName, testCode);

            // Log API call completion with token estimation
            var tokensUsed = testCode.Length / 4; // Rough estimation
            _enhancedLogger?.LogApiCallEnd(apiCallId, testCode, tokensUsed, 0.001m);
            _enhancedLogger?.LogBehindTheScenes("API_CALL", "TEST_GENERATION_RESPONSE",
                $"Received unit test code ({testCode.Length} chars)");

            // Clean up the test code
            testCode = ExtractCSharpCode(testCode);

            var testFile = new CSharpFile
            {
                FileName = $"{csharpFile.ClassName}Tests.cs",
                Content = testCode,
                Namespace = $"{csharpFile.Namespace}.Tests",
                ClassName = $"{csharpFile.ClassName}Tests",
                OriginalCobolFileName = csharpFile.OriginalCobolFileName
            };

            stopwatch.Stop();
            _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "UNIT_TEST_GENERATION_COMPLETE",
                $"Completed unit test generation for {csharpFile.FileName} in {stopwatch.ElapsedMilliseconds}ms");

            _logger.LogInformation("Successfully generated unit tests for {FileName}", csharpFile.FileName);

            return testFile;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();

            if (apiCallId > 0)
            {
                _enhancedLogger?.LogApiCallError(apiCallId, ex.Message);
            }

            _enhancedLogger?.LogBehindTheScenes("ERROR", "UNIT_TEST_GENERATION_ERROR",
                $"Failed to generate unit tests for {csharpFile.FileName} after {stopwatch.ElapsedMilliseconds}ms: {ex.Message}", ex);

            _logger.LogError(ex, "Error generating unit tests for {FileName}", csharpFile.FileName);
            throw;
        }
    }

    /// <summary>
    /// Generates unit tests for a collection of C# files.
    /// </summary>
    public async Task<List<CSharpFile>> GenerateUnitTestsAsync(List<CSharpFile> csharpFiles, List<CobolAnalysis> cobolAnalyses, Action<int, int>? progressCallback = null)
    {
        _logger.LogInformation("Generating unit tests for {Count} C# files", csharpFiles.Count);
        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "BATCH_TEST_GENERATION_START",
            $"Starting batch unit test generation for {csharpFiles.Count} files");

        var testFiles = new List<CSharpFile>();
        int completed = 0;

        foreach (var csharpFile in csharpFiles)
        {
            // Find corresponding COBOL analysis
            var cobolAnalysis = cobolAnalyses.FirstOrDefault(a =>
                a.FileName.Replace(".cbl", "").Equals(csharpFile.OriginalCobolFileName.Replace(".cbl", ""), StringComparison.OrdinalIgnoreCase))
                ?? cobolAnalyses.FirstOrDefault();

            if (cobolAnalysis == null)
            {
                _logger.LogWarning("No COBOL analysis found for {FileName}, skipping test generation", csharpFile.FileName);
                continue;
            }

            try
            {
                var testFile = await GenerateUnitTestsAsync(csharpFile, cobolAnalysis);
                testFiles.Add(testFile);

                completed++;
                progressCallback?.Invoke(completed, csharpFiles.Count);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Failed to generate tests for {FileName}, continuing with others", csharpFile.FileName);
            }
        }

        _enhancedLogger?.LogBehindTheScenes("AI_PROCESSING", "BATCH_TEST_GENERATION_COMPLETE",
            $"Completed batch test generation: {testFiles.Count}/{csharpFiles.Count} successful");

        _logger.LogInformation("Generated {Generated} test files out of {Total} C# files", testFiles.Count, csharpFiles.Count);

        return testFiles;
    }

    /// <summary>
    /// Extracts Java code from AI response, removing markdown code blocks if present.
    /// </summary>
    private string ExtractJavaCode(string input)
    {
        if (string.IsNullOrWhiteSpace(input))
            return string.Empty;

        var result = input.Trim();

        // Remove markdown code blocks
        if (result.StartsWith("```java"))
        {
            result = result.Substring("```java".Length);
        }
        else if (result.StartsWith("```"))
        {
            result = result.Substring("```".Length);
        }

        if (result.EndsWith("```"))
        {
            result = result.Substring(0, result.Length - 3);
        }

        return result.Trim();
    }

    /// <summary>
    /// Extracts C# code from AI response, removing markdown code blocks if present.
    /// </summary>
    private string ExtractCSharpCode(string input)
    {
        if (string.IsNullOrWhiteSpace(input))
            return string.Empty;

        var result = input.Trim();

        // Remove markdown code blocks
        if (result.StartsWith("```csharp") || result.StartsWith("```c#"))
        {
            var firstLineEnd = result.IndexOf('\n');
            if (firstLineEnd > 0)
            {
                result = result.Substring(firstLineEnd + 1);
            }
        }
        else if (result.StartsWith("```"))
        {
            result = result.Substring("```".Length);
        }

        if (result.EndsWith("```"))
        {
            result = result.Substring(0, result.Length - 3);
        }

        return result.Trim();
    }

    /// <summary>
    /// Truncates content for prompt to stay within token limits.
    /// </summary>
    private string TruncateForPrompt(string content, int maxChars)
    {
        if (content.Length <= maxChars)
            return content;

        var truncated = content.Substring(0, maxChars);
        return truncated + $"\n\n// ... (truncated {content.Length - maxChars} characters)";
    }
}
