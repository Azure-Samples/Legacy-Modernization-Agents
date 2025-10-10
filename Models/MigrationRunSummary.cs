namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents a summary of a migration run and associated insights.
/// </summary>
public class MigrationRunSummary
{
    public int RunId { get; set; }
    public DateTime StartedAt { get; set; }
    public DateTime? CompletedAt { get; set; }
    public string Status { get; set; } = string.Empty;
    public string CobolSourcePath { get; set; } = string.Empty;
    public string JavaOutputPath { get; set; } = string.Empty;
    public string? Notes { get; set; }
    public DependencyMetrics? Metrics { get; set; }
    public string? AnalysisInsights { get; set; }
    public string? MermaidDiagram { get; set; }
}
