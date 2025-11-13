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
