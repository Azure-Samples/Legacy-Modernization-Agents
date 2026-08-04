namespace CobolToQuarkusMigration.Models;

public class CodeFile
{
    public string FileName { get; set; } = string.Empty;

    // Populated after the generated file is written to disk.
    public string FilePath { get; set; } = string.Empty;

    public string Content { get; set; } = string.Empty;

    public string ClassName { get; set; } = string.Empty;

    public string NamespaceName { get; set; } = string.Empty;

    public string OriginalCobolFileName { get; set; } = string.Empty;

    public string TargetLanguage { get; set; } = string.Empty;
}
