namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents a generated code file (base class for Java, C#, etc.).
/// </summary>
public class CodeFile
{
    /// <summary>
    /// Gets or sets the file name.
    /// </summary>
    public string FileName { get; set; } = string.Empty;

    /// <summary>
    /// Absolute path the file was written to (set by the save helpers after
    /// the file lands on disk). Empty until then. Used by quality-check
    /// passes that re-open generated files to verify they aren't 0-byte
    /// or stub placeholders.
    /// </summary>
    public string FilePath { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the file content.
    /// </summary>
    public string Content { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the class name.
    /// </summary>
    public string ClassName { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the namespace/package name.
    /// </summary>
    public string NamespaceName { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the original COBOL file name.
    /// </summary>
    public string OriginalCobolFileName { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the target language (Java, CSharp, etc.).
    /// </summary>
    public string TargetLanguage { get; set; } = string.Empty;
}
