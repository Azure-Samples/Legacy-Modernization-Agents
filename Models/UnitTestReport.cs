namespace CobolModernization.Models;

/// <summary>
/// Represents a report summarizing generated unit tests
/// </summary>
public class UnitTestReport
{
    /// <summary>
    /// Target language of the tests (Java or CSharp)
    /// </summary>
    public string TargetLanguage { get; set; } = string.Empty;

    /// <summary>
    /// Test framework used (JUnit 5, xUnit, etc.)
    /// </summary>
    public string TestFramework { get; set; } = string.Empty;

    /// <summary>
    /// Total number of test files generated
    /// </summary>
    public int TotalTestFiles { get; set; }

    /// <summary>
    /// Total number of test methods/cases generated
    /// </summary>
    public int TotalTestMethods { get; set; }

    /// <summary>
    /// Estimated code coverage percentage
    /// </summary>
    public double EstimatedCoverage { get; set; }

    /// <summary>
    /// Number of source files tested
    /// </summary>
    public int SourceFilesCount { get; set; }

    /// <summary>
    /// List of generated test files with details
    /// </summary>
    public List<TestFileInfo> TestFiles { get; set; } = new();

    /// <summary>
    /// Test coverage areas
    /// </summary>
    public TestCoverageAreas CoverageAreas { get; set; } = new();

    /// <summary>
    /// Recommendations for improving tests
    /// </summary>
    public List<string> Recommendations { get; set; } = new();

    /// <summary>
    /// Summary of test generation
    /// </summary>
    public string Summary { get; set; } = string.Empty;

    /// <summary>
    /// Timestamp when tests were generated
    /// </summary>
    public DateTime GeneratedAt { get; set; } = DateTime.UtcNow;

    /// <summary>
    /// Path to the report file
    /// </summary>
    public string ReportPath { get; set; } = string.Empty;

    /// <summary>
    /// Commands to run the tests
    /// </summary>
    public List<string> RunCommands { get; set; } = new();
}

/// <summary>
/// Information about a single test file
/// </summary>
public class TestFileInfo
{
    /// <summary>
    /// Name of the test file
    /// </summary>
    public string FileName { get; set; } = string.Empty;

    /// <summary>
    /// Name of the source file being tested
    /// </summary>
    public string SourceFileName { get; set; } = string.Empty;

    /// <summary>
    /// Test class name
    /// </summary>
    public string TestClassName { get; set; } = string.Empty;

    /// <summary>
    /// Number of test methods in this file
    /// </summary>
    public int TestMethodCount { get; set; }

    /// <summary>
    /// Types of tests included (unit, integration, edge cases, etc.)
    /// </summary>
    public List<string> TestTypes { get; set; } = new();

    /// <summary>
    /// Original COBOL file name
    /// </summary>
    public string OriginalCobolFile { get; set; } = string.Empty;
}

/// <summary>
/// Areas covered by the generated tests
/// </summary>
public class TestCoverageAreas
{
    /// <summary>
    /// Business logic tests count
    /// </summary>
    public int BusinessLogicTests { get; set; }

    /// <summary>
    /// Edge case tests count
    /// </summary>
    public int EdgeCaseTests { get; set; }

    /// <summary>
    /// Error handling tests count
    /// </summary>
    public int ErrorHandlingTests { get; set; }

    /// <summary>
    /// Data validation tests count
    /// </summary>
    public int DataValidationTests { get; set; }

    /// <summary>
    /// Integration tests count
    /// </summary>
    public int IntegrationTests { get; set; }

    /// <summary>
    /// Performance tests count
    /// </summary>
    public int PerformanceTests { get; set; }
}
