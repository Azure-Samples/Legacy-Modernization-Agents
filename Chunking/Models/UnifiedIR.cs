namespace CobolToQuarkusMigration.Chunking.Models;

/// <summary>
/// Unified Intermediate Representation for legacy code.
/// Provides a language-agnostic view that can be converted to any target language.
/// </summary>
public class UnifiedIR
{
    /// <summary>
    /// The source file this IR was generated from.
    /// </summary>
    public string SourceFile { get; set; } = string.Empty;

    /// <summary>
    /// The source language (COBOL, PL/I, FORTRAN).
    /// </summary>
    public string SourceLanguage { get; set; } = string.Empty;

    /// <summary>
    /// Program/module name.
    /// </summary>
    public string ProgramName { get; set; } = string.Empty;

    /// <summary>
    /// Program description/identification.
    /// </summary>
    public string? ProgramDescription { get; set; }

    /// <summary>
    /// All data structures/records defined in the program.
    /// </summary>
    public List<IRDataStructure> DataStructures { get; set; } = new();

    /// <summary>
    /// All variables/fields defined (flat list).
    /// </summary>
    public List<IRVariable> Variables { get; set; } = new();

    /// <summary>
    /// All procedures/methods/paragraphs.
    /// </summary>
    public List<IRProcedure> Procedures { get; set; } = new();

    /// <summary>
    /// File declarations (for file I/O).
    /// </summary>
    public List<IRFileDeclaration> FileDeclarations { get; set; } = new();

    /// <summary>
    /// External program calls.
    /// </summary>
    public List<IRExternalCall> ExternalCalls { get; set; } = new();

    /// <summary>
    /// Copybook/include references.
    /// </summary>
    public List<string> IncludeReferences { get; set; } = new();

    /// <summary>
    /// Total line count of the original source.
    /// </summary>
    public int TotalLines { get; set; }

    /// <summary>
    /// When this IR was generated.
    /// </summary>
    public DateTime GeneratedAt { get; set; } = DateTime.UtcNow;
}

/// <summary>
/// Represents a data structure (record, group) in the IR.
/// </summary>
public class IRDataStructure
{
    /// <summary>Unique ID for this structure.</summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>Original name in source language.</summary>
    public string LegacyName { get; set; } = string.Empty;

    /// <summary>Suggested target class/struct name.</summary>
    public string? SuggestedTargetName { get; set; }

    /// <summary>Level in hierarchy (COBOL-specific).</summary>
    public int? Level { get; set; }

    /// <summary>Parent structure ID (for nested structures).</summary>
    public string? ParentId { get; set; }

    /// <summary>Child fields/structures.</summary>
    public List<IRVariable> Fields { get; set; } = new();

    /// <summary>Start line in source.</summary>
    public int StartLine { get; set; }

    /// <summary>End line in source.</summary>
    public int EndLine { get; set; }

    /// <summary>Whether this is a redefinition.</summary>
    public bool IsRedefines { get; set; }

    /// <summary>Name of structure being redefined.</summary>
    public string? RedefinesTarget { get; set; }
}

/// <summary>
/// Represents a variable/field in the IR.
/// </summary>
public class IRVariable
{
    /// <summary>Unique ID for this variable.</summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>Original name in source language.</summary>
    public string LegacyName { get; set; } = string.Empty;

    /// <summary>Original type declaration.</summary>
    public string LegacyType { get; set; } = string.Empty;

    /// <summary>Suggested target field name.</summary>
    public string? SuggestedTargetName { get; set; }

    /// <summary>Inferred target type.</summary>
    public string InferredTargetType { get; set; } = string.Empty;

    /// <summary>Level number (COBOL-specific).</summary>
    public int? Level { get; set; }

    /// <summary>Parent variable/structure ID.</summary>
    public string? ParentId { get; set; }

    /// <summary>Initial value if specified.</summary>
    public string? InitialValue { get; set; }

    /// <summary>Whether this is an array/occurs.</summary>
    public bool IsArray { get; set; }

    /// <summary>Array dimension if applicable.</summary>
    public int? ArraySize { get; set; }

    /// <summary>Whether this can be null.</summary>
    public bool IsNullable { get; set; }

    /// <summary>Line number in source.</summary>
    public int LineNumber { get; set; }

    /// <summary>Whether this is a condition name (88-level).</summary>
    public bool IsConditionName { get; set; }

    /// <summary>Condition values (for 88-level items).</summary>
    public List<string> ConditionValues { get; set; } = new();

    /// <summary>Numeric precision (for decimal types).</summary>
    public int? Precision { get; set; }

    /// <summary>Numeric scale (for decimal types).</summary>
    public int? Scale { get; set; }
}

/// <summary>
/// Represents a procedure/method/paragraph in the IR.
/// </summary>
public class IRProcedure
{
    /// <summary>Unique ID for this procedure.</summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>Original name in source language.</summary>
    public string LegacyName { get; set; } = string.Empty;

    /// <summary>Suggested target method name.</summary>
    public string? SuggestedTargetName { get; set; }

    /// <summary>Type of procedure (Section, Paragraph, Subroutine, Function).</summary>
    public string ProcedureType { get; set; } = string.Empty;

    /// <summary>Parent procedure ID (for nested procedures).</summary>
    public string? ParentId { get; set; }

    /// <summary>Parameters (for subroutines/functions).</summary>
    public List<IRParameter> Parameters { get; set; } = new();

    /// <summary>Return type (for functions).</summary>
    public string? ReturnType { get; set; }

    /// <summary>Procedures called from this one.</summary>
    public List<string> CallsTo { get; set; } = new();

    /// <summary>Procedures that call this one.</summary>
    public List<string> CalledBy { get; set; } = new();

    /// <summary>Variables read in this procedure.</summary>
    public List<string> VariablesRead { get; set; } = new();

    /// <summary>Variables modified in this procedure.</summary>
    public List<string> VariablesModified { get; set; } = new();

    /// <summary>Start line in source.</summary>
    public int StartLine { get; set; }

    /// <summary>End line in source.</summary>
    public int EndLine { get; set; }

    /// <summary>The original source code.</summary>
    public string SourceCode { get; set; } = string.Empty;

    /// <summary>Summary of what this procedure does.</summary>
    public string? Summary { get; set; }

    /// <summary>Complexity estimate.</summary>
    public int? CyclomaticComplexity { get; set; }
}

/// <summary>
/// Represents a parameter in the IR.
/// </summary>
public class IRParameter
{
    /// <summary>Parameter name.</summary>
    public string Name { get; set; } = string.Empty;

    /// <summary>Legacy type.</summary>
    public string LegacyType { get; set; } = string.Empty;

    /// <summary>Inferred target type.</summary>
    public string InferredTargetType { get; set; } = string.Empty;

    /// <summary>Whether passed by reference.</summary>
    public bool IsByReference { get; set; }

    /// <summary>Whether this is an output parameter.</summary>
    public bool IsOutput { get; set; }
}

/// <summary>
/// Represents a file declaration in the IR.
/// </summary>
public class IRFileDeclaration
{
    /// <summary>File name/handle.</summary>
    public string FileName { get; set; } = string.Empty;

    /// <summary>File organization (Sequential, Indexed, Relative).</summary>
    public string Organization { get; set; } = string.Empty;

    /// <summary>Access mode (Sequential, Random, Dynamic).</summary>
    public string AccessMode { get; set; } = string.Empty;

    /// <summary>Record structure name.</summary>
    public string RecordName { get; set; } = string.Empty;

    /// <summary>Key fields (for indexed files).</summary>
    public List<string> KeyFields { get; set; } = new();

    /// <summary>Start line in source.</summary>
    public int LineNumber { get; set; }
}

/// <summary>
/// Represents an external program call in the IR.
/// </summary>
public class IRExternalCall
{
    /// <summary>Program being called.</summary>
    public string ProgramName { get; set; } = string.Empty;

    /// <summary>Parameters passed.</summary>
    public List<string> Parameters { get; set; } = new();

    /// <summary>Calling procedure.</summary>
    public string CallingProcedure { get; set; } = string.Empty;

    /// <summary>Line number of the call.</summary>
    public int LineNumber { get; set; }

    /// <summary>Whether this is a dynamic call.</summary>
    public bool IsDynamic { get; set; }
}
