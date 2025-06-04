namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents a generated Java file.
/// </summary>
public class JavaFile
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
    /// Gets or sets the package name.
    /// </summary>
    public string PackageName { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the class name.
    /// </summary>
    public string ClassName { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the original COBOL file name.
    /// </summary>
    public string OriginalCobolFileName { get; set; } = string.Empty;
}
