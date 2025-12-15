namespace CobolToQuarkusMigration.Chunking.Interfaces;

/// <summary>
/// Represents a language-specific adapter for parsing and understanding legacy source code.
/// Each supported source language (COBOL, PL/I, FORTRAN) should implement this interface.
/// </summary>
public interface ILanguageAdapter
{
    /// <summary>
    /// Gets the name of the source language this adapter handles.
    /// </summary>
    string LanguageName { get; }

    /// <summary>
    /// Gets the file extensions supported by this adapter.
    /// </summary>
    IReadOnlyList<string> SupportedExtensions { get; }

    /// <summary>
    /// Determines if this adapter can process the given file.
    /// </summary>
    /// <param name="filePath">Path to the source file.</param>
    /// <param name="content">Optional file content for content-based detection.</param>
    /// <returns>True if this adapter can process the file.</returns>
    bool CanProcess(string filePath, string? content = null);

    /// <summary>
    /// Parses the source file and identifies semantic units (sections, paragraphs, procedures).
    /// </summary>
    /// <param name="content">The source code content.</param>
    /// <param name="filePath">Path to the source file (for context).</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>A list of identified semantic units.</returns>
    Task<IReadOnlyList<SemanticUnit>> IdentifySemanticUnitsAsync(
        string content,
        string filePath,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Extracts all variable declarations from the source code.
    /// </summary>
    /// <param name="content">The source code content.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>A list of variable declarations with their types.</returns>
    Task<IReadOnlyList<VariableDeclaration>> ExtractVariablesAsync(
        string content,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Extracts all procedure/paragraph/method calls and their dependencies.
    /// </summary>
    /// <param name="content">The source code content.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>A list of call dependencies.</returns>
    Task<IReadOnlyList<CallDependency>> ExtractCallDependenciesAsync(
        string content,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Identifies copybook/include file references.
    /// </summary>
    /// <param name="content">The source code content.</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>A list of external file references.</returns>
    Task<IReadOnlyList<ExternalReference>> ExtractExternalReferencesAsync(
        string content,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Converts a legacy name to a modern name using deterministic rules.
    /// </summary>
    /// <param name="legacyName">The original legacy name (e.g., VALIDATE-CUSTOMER).</param>
    /// <param name="nameType">The type of name (variable, method, class, etc.).</param>
    /// <returns>The converted modern name (e.g., validateCustomer).</returns>
    string ConvertNameDeterministic(string legacyName, NameType nameType);
}

/// <summary>
/// Represents a semantic unit in the source code (section, paragraph, procedure, etc.).
/// </summary>
public class SemanticUnit
{
    /// <summary>
    /// Unique identifier for this semantic unit within the file.
    /// </summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>
    /// The name of the semantic unit in the source language.
    /// </summary>
    public string LegacyName { get; set; } = string.Empty;

    /// <summary>
    /// The type of semantic unit (Section, Paragraph, Procedure, DataDivision, etc.).
    /// </summary>
    public SemanticUnitType UnitType { get; set; }

    /// <summary>
    /// Starting line number (1-based) in the source file.
    /// </summary>
    public int StartLine { get; set; }

    /// <summary>
    /// Ending line number (1-based) in the source file.
    /// </summary>
    public int EndLine { get; set; }

    /// <summary>
    /// The actual source code content of this unit.
    /// </summary>
    public string Content { get; set; } = string.Empty;

    /// <summary>
    /// List of semantic units this unit depends on (calls, references).
    /// </summary>
    public List<string> Dependencies { get; set; } = new();

    /// <summary>
    /// List of semantic units that depend on this unit.
    /// </summary>
    public List<string> Dependents { get; set; } = new();

    /// <summary>
    /// Parent semantic unit ID (for nested structures).
    /// </summary>
    public string? ParentId { get; set; }

    /// <summary>
    /// Child semantic unit IDs (for nested structures).
    /// </summary>
    public List<string> ChildIds { get; set; } = new();

    /// <summary>
    /// Estimated token count for this unit.
    /// </summary>
    public int EstimatedTokens { get; set; }

    /// <summary>
    /// Line count for this unit.
    /// </summary>
    public int LineCount => EndLine - StartLine + 1;
}

/// <summary>
/// Types of semantic units that can be identified in legacy code.
/// </summary>
public enum SemanticUnitType
{
    /// <summary>Program-level container (entire COBOL program).</summary>
    Program,
    
    /// <summary>Identification division (COBOL).</summary>
    IdentificationDivision,
    
    /// <summary>Environment division (COBOL).</summary>
    EnvironmentDivision,
    
    /// <summary>Data division (COBOL).</summary>
    DataDivision,
    
    /// <summary>Working storage section (COBOL).</summary>
    WorkingStorageSection,
    
    /// <summary>Linkage section (COBOL).</summary>
    LinkageSection,
    
    /// <summary>File section (COBOL).</summary>
    FileSection,
    
    /// <summary>Procedure division (COBOL).</summary>
    ProcedureDivision,
    
    /// <summary>A named section within procedure division.</summary>
    Section,
    
    /// <summary>A paragraph within a section or procedure division.</summary>
    Paragraph,
    
    /// <summary>A copybook include statement.</summary>
    CopybookInclude,
    
    /// <summary>A subroutine or procedure (PL/I, FORTRAN).</summary>
    Subroutine,
    
    /// <summary>A function definition.</summary>
    Function,
    
    /// <summary>A data structure or record definition.</summary>
    DataStructure,
    
    /// <summary>Unknown or custom unit type.</summary>
    Other
}

/// <summary>
/// Types of names in legacy code.
/// </summary>
public enum NameType
{
    /// <summary>Variable or field name.</summary>
    Variable,
    
    /// <summary>Method or paragraph name.</summary>
    Method,
    
    /// <summary>Class or program name.</summary>
    Class,
    
    /// <summary>Constant name.</summary>
    Constant,
    
    /// <summary>Parameter name.</summary>
    Parameter,
    
    /// <summary>Section name.</summary>
    Section,
    
    /// <summary>File name.</summary>
    File
}

/// <summary>
/// Represents a variable declaration in the source code.
/// </summary>
public class VariableDeclaration
{
    /// <summary>The variable name in the source language.</summary>
    public string LegacyName { get; set; } = string.Empty;
    
    /// <summary>The type definition in the source language (e.g., PIC X(10)).</summary>
    public string LegacyType { get; set; } = string.Empty;
    
    /// <summary>The level number (COBOL-specific, e.g., 01, 05, 88).</summary>
    public int? Level { get; set; }
    
    /// <summary>Parent variable name (for hierarchical structures).</summary>
    public string? ParentName { get; set; }
    
    /// <summary>Line number where this variable is declared.</summary>
    public int LineNumber { get; set; }
    
    /// <summary>Initial value if specified.</summary>
    public string? InitialValue { get; set; }
    
    /// <summary>Whether this variable is a group/structure.</summary>
    public bool IsGroup { get; set; }
    
    /// <summary>Whether this is a redefinition of another variable.</summary>
    public bool IsRedefines { get; set; }
    
    /// <summary>Name of the variable being redefined.</summary>
    public string? RedefinesTarget { get; set; }
}

/// <summary>
/// Represents a call dependency between procedures/paragraphs.
/// </summary>
public class CallDependency
{
    /// <summary>The name of the calling procedure/paragraph.</summary>
    public string CallerName { get; set; } = string.Empty;
    
    /// <summary>The name of the called procedure/paragraph.</summary>
    public string CalledName { get; set; } = string.Empty;
    
    /// <summary>Line number of the call.</summary>
    public int LineNumber { get; set; }
    
    /// <summary>Type of call (PERFORM, CALL, GO TO, etc.).</summary>
    public string CallType { get; set; } = string.Empty;
    
    /// <summary>Whether this is an external call to another program.</summary>
    public bool IsExternal { get; set; }
}

/// <summary>
/// Represents a reference to an external file (copybook, include, etc.).
/// </summary>
public class ExternalReference
{
    /// <summary>The name of the external file.</summary>
    public string FileName { get; set; } = string.Empty;
    
    /// <summary>Line number of the reference.</summary>
    public int LineNumber { get; set; }
    
    /// <summary>Type of reference (COPY, INCLUDE, etc.).</summary>
    public string ReferenceType { get; set; } = string.Empty;
    
    /// <summary>Optional library/folder specification.</summary>
    public string? Library { get; set; }
}
