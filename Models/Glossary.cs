namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents a single glossary term mapping technical COBOL terms to business-friendly translations
/// </summary>
public class GlossaryTerm
{
    /// <summary>
    /// Technical term from COBOL code (e.g., "ARTNR", "KDUTGAVA")
    /// </summary>
    public string Term { get; set; } = string.Empty;

    /// <summary>
    /// Business-friendly translation (e.g., "Article Number", "Release Version")
    /// </summary>
    public string Translation { get; set; } = string.Empty;
}

/// <summary>
/// Collection of glossary terms for business-friendly documentation
/// </summary>
public class Glossary
{
    /// <summary>
    /// List of all glossary terms
    /// </summary>
    public List<GlossaryTerm> Terms { get; set; } = new();
}
