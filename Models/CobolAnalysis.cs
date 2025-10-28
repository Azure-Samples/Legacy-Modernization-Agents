namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents the analysis of a COBOL file.
/// </summary>
public class CobolAnalysis
{
    /// <summary>
    /// Gets or sets the file name.
    /// </summary>
    public string FileName { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the file path.
    /// </summary>
    public string FilePath { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets whether this is a copybook file.
    /// </summary>
    public bool IsCopybook { get; set; }
    
    /// <summary>
    /// Gets or sets the overall description of the COBOL program.
    /// </summary>
    public string ProgramDescription { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the data divisions found in the COBOL program.
    /// </summary>
    public List<string> DataDivisions { get; set; } = new List<string>();
    
    /// <summary>
    /// Gets or sets the procedure divisions found in the COBOL program.
    /// </summary>
    public List<string> ProcedureDivisions { get; set; } = new List<string>();
    
    /// <summary>
    /// Gets or sets the variables found in the COBOL program.
    /// </summary>
    public List<CobolVariable> Variables { get; set; } = new List<CobolVariable>();
    
    /// <summary>
    /// Gets or sets the paragraphs/sections found in the COBOL program.
    /// </summary>
    public List<CobolParagraph> Paragraphs { get; set; } = new List<CobolParagraph>();
    
    /// <summary>
    /// Gets or sets the copybooks referenced in the COBOL program.
    /// </summary>
    public List<string> CopybooksReferenced { get; set; } = new List<string>();
    
    /// <summary>
    /// Gets or sets the raw analysis data.
    /// </summary>
    public string RawAnalysisData { get; set; } = string.Empty;
}

/// <summary>
/// Represents a COBOL variable.
/// </summary>
public class CobolVariable
{
    /// <summary>
    /// Gets or sets the variable name.
    /// </summary>
    public string Name { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the variable level.
    /// </summary>
    public int Level { get; set; }
    
    /// <summary>
    /// Gets or sets the variable type.
    /// </summary>
    public string Type { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the variable size.
    /// </summary>
    public string Size { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets whether this variable is a group item.
    /// </summary>
    public bool IsGroupItem { get; set; }
    
    /// <summary>
    /// Gets or sets the child variables if this is a group item.
    /// </summary>
    public List<CobolVariable> Children { get; set; } = new List<CobolVariable>();
}

/// <summary>
/// Represents a COBOL paragraph or section.
/// </summary>
public class CobolParagraph
{
    /// <summary>
    /// Gets or sets the paragraph name.
    /// </summary>
    public string Name { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the paragraph description.
    /// </summary>
    public string Description { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the paragraph logic.
    /// </summary>
    public string Logic { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the variables used in this paragraph.
    /// </summary>
    public List<string> VariablesUsed { get; set; } = new List<string>();
    
    /// <summary>
    /// Gets or sets the paragraphs called from this paragraph.
    /// </summary>
    public List<string> ParagraphsCalled { get; set; } = new List<string>();
}
