namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents a COBOL source file or copybook.
/// </summary>
public class CobolFile
{
    /// <summary>
    /// Gets or sets the file name.
    /// </summary>
    public string FileName { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the full path to the file.
    /// </summary>
    public string FilePath { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the file content.
    /// </summary>
    public string Content { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets whether this file is a copybook.
    /// </summary>
    public bool IsCopybook { get; set; }
}
