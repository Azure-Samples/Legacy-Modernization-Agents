namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents a dependency relationship between COBOL files.
/// </summary>
public class DependencyRelationship
{
    /// <summary>
    /// Gets or sets the source file name.
    /// </summary>
    public string SourceFile { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the target file name (dependency).
    /// </summary>
    public string TargetFile { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the type of dependency (COPY, CALL, etc.).
    /// </summary>
    public string DependencyType { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the line number where the dependency occurs.
    /// </summary>
    public int LineNumber { get; set; }

    /// <summary>
    /// Gets or sets additional context about the dependency.
    /// </summary>
    public string Context { get; set; } = string.Empty;
}

/// <summary>
/// Represents a comprehensive dependency map for COBOL files.
/// </summary>
public class DependencyMap
{
    /// <summary>
    /// Gets or sets the list of dependency relationships.
    /// </summary>
    public List<DependencyRelationship> Dependencies { get; set; } = new List<DependencyRelationship>();

    /// <summary>
    /// Gets or sets the copybook usage matrix.
    /// Key: COBOL program, Value: List of copybooks used.
    /// </summary>
    public Dictionary<string, List<string>> CopybookUsage { get; set; } = new Dictionary<string, List<string>>();

    /// <summary>
    /// Gets or sets the reverse dependency map.
    /// Key: Copybook, Value: List of programs that use it.
    /// </summary>
    public Dictionary<string, List<string>> ReverseDependencies { get; set; } = new Dictionary<string, List<string>>();

    /// <summary>
    /// Gets or sets the Mermaid diagram representation.
    /// </summary>
    public string MermaidDiagram { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets AI-generated insights about the dependency structure.
    /// </summary>
    public string AnalysisInsights { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the complexity metrics.
    /// </summary>
    public DependencyMetrics Metrics { get; set; } = new DependencyMetrics();

    /// <summary>
    /// Gets or sets the timestamp when the dependency map was created.
    /// </summary>
    public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
}

/// <summary>
/// Represents metrics about the dependency structure.
/// </summary>
public class DependencyMetrics
{
    /// <summary>
    /// Gets or sets the total number of COBOL programs.
    /// </summary>
    public int TotalPrograms { get; set; }

    /// <summary>
    /// Gets or sets the total number of copybooks.
    /// </summary>
    public int TotalCopybooks { get; set; }

    /// <summary>
    /// Gets or sets the total number of dependencies.
    /// </summary>
    public int TotalDependencies { get; set; }

    /// <summary>
    /// Gets or sets the average dependencies per program.
    /// </summary>
    public double AverageDependenciesPerProgram { get; set; }

    /// <summary>
    /// Gets or sets the most used copybook.
    /// </summary>
    public string MostUsedCopybook { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the usage count of the most used copybook.
    /// </summary>
    public int MostUsedCopybookCount { get; set; }

    /// <summary>
    /// Gets or sets the list of circular dependencies (if any).
    /// </summary>
    public List<string> CircularDependencies { get; set; } = new List<string>();
}
