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

    /// <inheritdoc/>
    public async Task<UnitTestReport> GenerateTestReportAsync(List<JavaFile> testFiles, List<JavaFile> sourceFiles, string outputDirectory)
    {
        _logger.LogInformation("Generating unit test report for {TestCount} Java test files", testFiles.Count);
        _enhancedLogger?.LogBehindTheScenes("REPORTING", "TEST_REPORT_GENERATION_START",
            $"Starting test report generation for {testFiles.Count} Java test files");

        var report = new UnitTestReport
        {
            TargetLanguage = "Java",
            TestFramework = "JUnit 5 + Mockito",
            TotalTestFiles = testFiles.Count,
            SourceFilesCount = sourceFiles.Count,
            GeneratedAt = DateTime.UtcNow,
            TestFiles = new List<TestFileInfo>(),
            CoverageAreas = new TestCoverageAreas(),
            Recommendations = new List<string>(),
            RunCommands = new List<string>
            {
                "mvn test",
                "mvn verify",
                "mvn test -Dtest=SpecificTest"
            }
        };

        int totalTestMethods = 0;

        // Analyze each test file
        foreach (var testFile in testFiles)
        {
            var testFileInfo = AnalyzeJavaTestFile(testFile, sourceFiles);
            report.TestFiles.Add(testFileInfo);
            totalTestMethods += testFileInfo.TestMethodCount;

            // Update coverage areas
            foreach (var testType in testFileInfo.TestTypes)
            {
                UpdateCoverageAreas(report.CoverageAreas, testType);
            }
        }

        report.TotalTestMethods = totalTestMethods;

        // Calculate estimated coverage
        report.EstimatedCoverage = CalculateEstimatedCoverage(totalTestMethods, sourceFiles);

        // Generate recommendations
        report.Recommendations = GenerateRecommendations(report);

        // Generate summary
        report.Summary = GenerateSummary(report);

        // Save report to file
        var reportPath = Path.Combine(outputDirectory, "unit-test-report.md");
        var reportContent = FormatReportAsMarkdown(report);
        await File.WriteAllTextAsync(reportPath, reportContent);
        report.ReportPath = reportPath;

        _logger.LogInformation("Unit test report generated: {ReportPath}", reportPath);
        _enhancedLogger?.LogBehindTheScenes("REPORTING", "TEST_REPORT_GENERATION_COMPLETE",
            $"Test report saved to {reportPath} with {totalTestMethods} tests across {testFiles.Count} files");

        return report;
    }

    /// <inheritdoc/>
    public async Task<UnitTestReport> GenerateTestReportAsync(List<CSharpFile> testFiles, List<CSharpFile> sourceFiles, string outputDirectory)
    {
        _logger.LogInformation("Generating unit test report for {TestCount} C# test files", testFiles.Count);
        _enhancedLogger?.LogBehindTheScenes("REPORTING", "TEST_REPORT_GENERATION_START",
            $"Starting test report generation for {testFiles.Count} C# test files");

        var report = new UnitTestReport
        {
            TargetLanguage = "C#",
            TestFramework = "xUnit + Moq",
            TotalTestFiles = testFiles.Count,
            SourceFilesCount = sourceFiles.Count,
            GeneratedAt = DateTime.UtcNow,
            TestFiles = new List<TestFileInfo>(),
            CoverageAreas = new TestCoverageAreas(),
            Recommendations = new List<string>(),
            RunCommands = new List<string>
            {
                "dotnet test",
                "dotnet test --logger \"console;verbosity=detailed\"",
                "dotnet test --filter FullyQualifiedName~SpecificTest"
            }
        };

        int totalTestMethods = 0;

        // Analyze each test file
        foreach (var testFile in testFiles)
        {
            var testFileInfo = AnalyzeCSharpTestFile(testFile, sourceFiles);
            report.TestFiles.Add(testFileInfo);
            totalTestMethods += testFileInfo.TestMethodCount;

            // Update coverage areas
            foreach (var testType in testFileInfo.TestTypes)
            {
                UpdateCoverageAreas(report.CoverageAreas, testType);
            }
        }

        report.TotalTestMethods = totalTestMethods;

        // Calculate estimated coverage
        report.EstimatedCoverage = CalculateEstimatedCoverage(totalTestMethods, sourceFiles);

        // Generate recommendations
        report.Recommendations = GenerateRecommendations(report);

        // Generate summary
        report.Summary = GenerateSummary(report);

        // Save report to file
        var reportPath = Path.Combine(outputDirectory, "unit-test-report.md");
        var reportContent = FormatReportAsMarkdown(report);
        await File.WriteAllTextAsync(reportPath, reportContent);
        report.ReportPath = reportPath;

        _logger.LogInformation("Unit test report generated: {ReportPath}", reportPath);
        _enhancedLogger?.LogBehindTheScenes("REPORTING", "TEST_REPORT_GENERATION_COMPLETE",
            $"Test report saved to {reportPath} with {totalTestMethods} tests across {testFiles.Count} files");

        return report;
    }

    /// <summary>
    /// Analyzes a Java test file to extract test information.
    /// </summary>
    private TestFileInfo AnalyzeJavaTestFile(JavaFile testFile, List<JavaFile> sourceFiles)
    {
        var testFileInfo = new TestFileInfo
        {
            FileName = testFile.FileName,
            TestClassName = ExtractClassName(testFile.Content),
            TestTypes = new List<string>()
        };

        // Find the corresponding source file
        var sourceName = testFile.FileName.Replace("Test.java", ".java").Replace("Tests.java", ".java");
        var sourceFile = sourceFiles.FirstOrDefault(sf => sf.FileName == sourceName);
        if (sourceFile != null)
        {
            testFileInfo.SourceFileName = sourceFile.FileName;
            testFileInfo.OriginalCobolFile = sourceFile.OriginalCobolFileName;
        }

        // Count test methods (look for @Test annotations)
        var lines = testFile.Content.Split('\n');
        int testCount = 0;
        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();
            if (line.StartsWith("@Test") || line.Contains("@Test "))
            {
                testCount++;

                // Identify test type based on method name
                if (i + 1 < lines.Length)
                {
                    var methodLine = lines[i + 1].ToLower();
                    if (methodLine.Contains("edge") || methodLine.Contains("boundary") || methodLine.Contains("limit"))
                        testFileInfo.TestTypes.Add("Edge Case");
                    else if (methodLine.Contains("error") || methodLine.Contains("exception") || methodLine.Contains("invalid"))
                        testFileInfo.TestTypes.Add("Error Handling");
                    else if (methodLine.Contains("valid") || methodLine.Contains("format") || methodLine.Contains("range"))
                        testFileInfo.TestTypes.Add("Data Validation");
                    else if (methodLine.Contains("integration") || methodLine.Contains("end"))
                        testFileInfo.TestTypes.Add("Integration");
                    else if (methodLine.Contains("performance") || methodLine.Contains("load"))
                        testFileInfo.TestTypes.Add("Performance");
                    else
                        testFileInfo.TestTypes.Add("Business Logic");
                }
            }
        }

        testFileInfo.TestMethodCount = testCount;
        return testFileInfo;
    }

    /// <summary>
    /// Analyzes a C# test file to extract test information.
    /// </summary>
    private TestFileInfo AnalyzeCSharpTestFile(CSharpFile testFile, List<CSharpFile> sourceFiles)
    {
        var testFileInfo = new TestFileInfo
        {
            FileName = testFile.FileName,
            TestClassName = ExtractClassName(testFile.Content),
            TestTypes = new List<string>()
        };

        // Find the corresponding source file
        var sourceName = testFile.FileName.Replace("Tests.cs", ".cs").Replace("Test.cs", ".cs");
        var sourceFile = sourceFiles.FirstOrDefault(sf => sf.FileName == sourceName);
        if (sourceFile != null)
        {
            testFileInfo.SourceFileName = sourceFile.FileName;
            testFileInfo.OriginalCobolFile = sourceFile.OriginalCobolFileName;
        }

        // Count test methods (look for [Fact] or [Theory] attributes)
        var lines = testFile.Content.Split('\n');
        int testCount = 0;
        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();
            if (line.StartsWith("[Fact") || line.StartsWith("[Theory") || line.Contains("[Fact]") || line.Contains("[Theory]"))
            {
                testCount++;

                // Identify test type based on method name
                if (i + 1 < lines.Length)
                {
                    var methodLine = lines[i + 1].ToLower();
                    if (methodLine.Contains("edge") || methodLine.Contains("boundary") || methodLine.Contains("limit"))
                        testFileInfo.TestTypes.Add("Edge Case");
                    else if (methodLine.Contains("error") || methodLine.Contains("exception") || methodLine.Contains("invalid"))
                        testFileInfo.TestTypes.Add("Error Handling");
                    else if (methodLine.Contains("valid") || methodLine.Contains("format") || methodLine.Contains("range"))
                        testFileInfo.TestTypes.Add("Data Validation");
                    else if (methodLine.Contains("integration") || methodLine.Contains("end"))
                        testFileInfo.TestTypes.Add("Integration");
                    else if (methodLine.Contains("performance") || methodLine.Contains("load"))
                        testFileInfo.TestTypes.Add("Performance");
                    else
                        testFileInfo.TestTypes.Add("Business Logic");
                }
            }
        }

        testFileInfo.TestMethodCount = testCount;
        return testFileInfo;
    }

    /// <summary>
    /// Extracts the class name from code content.
    /// </summary>
    private string ExtractClassName(string content)
    {
        var lines = content.Split('\n');
        foreach (var line in lines)
        {
            var trimmed = line.Trim();
            if (trimmed.StartsWith("public class ") || trimmed.StartsWith("class "))
            {
                var parts = trimmed.Split(new[] { ' ', '{', ':' }, StringSplitOptions.RemoveEmptyEntries);
                if (parts.Length >= 2)
                {
                    return parts[1];
                }
            }
        }
        return "Unknown";
    }

    /// <summary>
    /// Updates coverage areas based on test type.
    /// </summary>
    private void UpdateCoverageAreas(TestCoverageAreas coverage, string testType)
    {
        switch (testType)
        {
            case "Business Logic":
                coverage.BusinessLogicTests++;
                break;
            case "Edge Case":
                coverage.EdgeCaseTests++;
                break;
            case "Error Handling":
                coverage.ErrorHandlingTests++;
                break;
            case "Data Validation":
                coverage.DataValidationTests++;
                break;
            case "Integration":
                coverage.IntegrationTests++;
                break;
            case "Performance":
                coverage.PerformanceTests++;
                break;
        }
    }

    /// <summary>
    /// Calculates estimated coverage based on test count and source complexity.
    /// </summary>
    private double CalculateEstimatedCoverage(int testMethodCount, dynamic sourceFiles)
    {
        if (sourceFiles.Count == 0)
            return 0.0;

        // Estimate based on tests per source file (heuristic)
        // Ideal: 5-10 tests per source file
        double testsPerFile = (double)testMethodCount / sourceFiles.Count;

        if (testsPerFile >= 10)
            return 85.0; // Excellent coverage
        else if (testsPerFile >= 7)
            return 75.0; // Good coverage
        else if (testsPerFile >= 5)
            return 65.0; // Acceptable coverage
        else if (testsPerFile >= 3)
            return 50.0; // Moderate coverage
        else
            return 35.0; // Needs improvement
    }

    /// <summary>
    /// Generates recommendations based on report analysis.
    /// </summary>
    private List<string> GenerateRecommendations(UnitTestReport report)
    {
        var recommendations = new List<string>();

        // Coverage-based recommendations
        if (report.EstimatedCoverage < 50)
        {
            recommendations.Add("⚠️ Test coverage is below 50%. Consider adding more comprehensive test cases.");
        }
        else if (report.EstimatedCoverage < 70)
        {
            recommendations.Add("✓ Test coverage is acceptable but could be improved with additional edge cases.");
        }

        // Coverage area recommendations
        if (report.CoverageAreas.EdgeCaseTests < report.TotalTestFiles)
        {
            recommendations.Add("Add more edge case tests to validate boundary conditions and unusual inputs.");
        }

        if (report.CoverageAreas.ErrorHandlingTests < report.TotalTestFiles)
        {
            recommendations.Add("Enhance error handling tests to cover exception scenarios and invalid states.");
        }

        if (report.CoverageAreas.DataValidationTests == 0)
        {
            recommendations.Add("Consider adding data validation tests to ensure input constraints are enforced.");
        }

        if (report.CoverageAreas.IntegrationTests == 0)
        {
            recommendations.Add("Add integration tests to verify interactions between components.");
        }

        // Test count recommendations
        var avgTestsPerFile = report.TotalTestFiles > 0 ? (double)report.TotalTestMethods / report.TotalTestFiles : 0;
        if (avgTestsPerFile < 5)
        {
            recommendations.Add($"Average of {avgTestsPerFile:F1} tests per file is low. Aim for 5-10 tests per source file.");
        }

        // General recommendations
        recommendations.Add("Review generated tests and enhance with domain-specific scenarios.");
        recommendations.Add("Run tests regularly during development to catch regressions early.");
        recommendations.Add("Consider adding performance benchmarks for critical business operations.");

        return recommendations;
    }

    /// <summary>
    /// Generates a summary of the test report.
    /// </summary>
    private string GenerateSummary(UnitTestReport report)
    {
        var summary = new StringBuilder();
        summary.AppendLine($"Generated {report.TotalTestMethods} unit tests across {report.TotalTestFiles} test files ");
        summary.AppendLine($"for {report.SourceFilesCount} {report.TargetLanguage} source files using {report.TestFramework}.");
        summary.AppendLine();
        summary.AppendLine($"Estimated code coverage: {report.EstimatedCoverage:F1}%");
        summary.AppendLine();
        summary.AppendLine("Test Distribution:");
        summary.AppendLine($"  • Business Logic Tests: {report.CoverageAreas.BusinessLogicTests}");
        summary.AppendLine($"  • Edge Case Tests: {report.CoverageAreas.EdgeCaseTests}");
        summary.AppendLine($"  • Error Handling Tests: {report.CoverageAreas.ErrorHandlingTests}");
        summary.AppendLine($"  • Data Validation Tests: {report.CoverageAreas.DataValidationTests}");
        summary.AppendLine($"  • Integration Tests: {report.CoverageAreas.IntegrationTests}");
        summary.AppendLine($"  • Performance Tests: {report.CoverageAreas.PerformanceTests}");

        return summary.ToString();
    }

    /// <summary>
    /// Formats the report as markdown.
    /// </summary>
    private string FormatReportAsMarkdown(UnitTestReport report)
    {
        var md = new StringBuilder();

        md.AppendLine("# Unit Test Report");
        md.AppendLine();
        md.AppendLine($"**Generated:** {report.GeneratedAt:yyyy-MM-dd HH:mm:ss UTC}");
        md.AppendLine($"**Target Language:** {report.TargetLanguage}");
        md.AppendLine($"**Test Framework:** {report.TestFramework}");
        md.AppendLine();

        md.AppendLine("## Summary");
        md.AppendLine();
        md.AppendLine(report.Summary);
        md.AppendLine();

        md.AppendLine("## Test Coverage Analysis");
        md.AppendLine();
        md.AppendLine($"| Metric | Value |");
        md.AppendLine("|--------|-------|");
        md.AppendLine($"| Total Test Files | {report.TotalTestFiles} |");
        md.AppendLine($"| Total Test Methods | {report.TotalTestMethods} |");
        md.AppendLine($"| Source Files Covered | {report.SourceFilesCount} |");
        md.AppendLine($"| Estimated Coverage | {report.EstimatedCoverage:F1}% |");
        md.AppendLine($"| Avg Tests/File | {(report.TotalTestFiles > 0 ? (double)report.TotalTestMethods / report.TotalTestFiles : 0):F1} |");
        md.AppendLine();

        md.AppendLine("## Test Files");
        md.AppendLine();
        md.AppendLine("| Test File | Source File | Test Class | Tests | Types Covered |");
        md.AppendLine("|-----------|-------------|------------|-------|---------------|");
        foreach (var testFile in report.TestFiles)
        {
            var types = string.Join(", ", testFile.TestTypes.Distinct());
            md.AppendLine($"| {testFile.FileName} | {testFile.SourceFileName ?? "N/A"} | {testFile.TestClassName} | {testFile.TestMethodCount} | {types} |");
        }
        md.AppendLine();

        md.AppendLine("## Coverage by Test Type");
        md.AppendLine();
        md.AppendLine("```");
        md.AppendLine($"Business Logic Tests:  {report.CoverageAreas.BusinessLogicTests,3} ({GetPercentage(report.CoverageAreas.BusinessLogicTests, report.TotalTestMethods):F1}%)");
        md.AppendLine($"Edge Case Tests:       {report.CoverageAreas.EdgeCaseTests,3} ({GetPercentage(report.CoverageAreas.EdgeCaseTests, report.TotalTestMethods):F1}%)");
        md.AppendLine($"Error Handling Tests:  {report.CoverageAreas.ErrorHandlingTests,3} ({GetPercentage(report.CoverageAreas.ErrorHandlingTests, report.TotalTestMethods):F1}%)");
        md.AppendLine($"Data Validation Tests: {report.CoverageAreas.DataValidationTests,3} ({GetPercentage(report.CoverageAreas.DataValidationTests, report.TotalTestMethods):F1}%)");
        md.AppendLine($"Integration Tests:     {report.CoverageAreas.IntegrationTests,3} ({GetPercentage(report.CoverageAreas.IntegrationTests, report.TotalTestMethods):F1}%)");
        md.AppendLine($"Performance Tests:     {report.CoverageAreas.PerformanceTests,3} ({GetPercentage(report.CoverageAreas.PerformanceTests, report.TotalTestMethods):F1}%)");
        md.AppendLine("```");
        md.AppendLine();

        md.AppendLine("## Recommendations");
        md.AppendLine();
        foreach (var recommendation in report.Recommendations)
        {
            md.AppendLine($"- {recommendation}");
        }
        md.AppendLine();

        md.AppendLine("## Running Tests");
        md.AppendLine();
        md.AppendLine("Execute the following commands to run the generated tests:");
        md.AppendLine();
        md.AppendLine("```bash");
        foreach (var command in report.RunCommands)
        {
            md.AppendLine($"# {command}");
        }
        md.AppendLine("```");
        md.AppendLine();

        md.AppendLine("---");
        md.AppendLine("*This report was automatically generated by the COBOL Modernization Unit Test Agent.*");

        return md.ToString();
    }

    /// <summary>
    /// Calculates percentage with division by zero protection.
    /// </summary>
    private double GetPercentage(int part, int total)
    {
        return total > 0 ? ((double)part / total) * 100.0 : 0.0;
    }
}
