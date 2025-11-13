namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents a generated Java file.
/// </summary>
public class JavaFile : CodeFile
{
    /// <summary>
    /// Gets or sets the full path to the file.
    /// </summary>
    public string FilePath { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the package name (alias for NamespaceName).
    /// </summary>
    public string PackageName
    {
        get => NamespaceName;
        set => NamespaceName = value;
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="JavaFile"/> class.
    /// </summary>
    public JavaFile()
    {
        TargetLanguage = "Java";
    }
}
