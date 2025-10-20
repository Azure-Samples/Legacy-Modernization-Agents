namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Represents extracted business logic from COBOL code.
/// </summary>
public class BusinessLogic
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
    /// Gets or sets the overall business purpose of the program.
    /// </summary>
    public string BusinessPurpose { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the user stories extracted from the code.
    /// </summary>
    public List<UserStory> UserStories { get; set; } = new List<UserStory>();
    
    /// <summary>
    /// Gets or sets the feature descriptions for batch/calculation processes.
    /// </summary>
    public List<FeatureDescription> Features { get; set; } = new List<FeatureDescription>();
    
    /// <summary>
    /// Gets or sets the business rules identified in the code.
    /// </summary>
    public List<BusinessRule> BusinessRules { get; set; } = new List<BusinessRule>();
    
    /// <summary>
    /// Gets or sets the data entities used in the business logic.
    /// </summary>
    public List<DataEntity> DataEntities { get; set; } = new List<DataEntity>();
}

/// <summary>
/// Represents a user story extracted from COBOL code.
/// </summary>
public class UserStory
{
    /// <summary>
    /// Gets or sets the user story ID.
    /// </summary>
    public string Id { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the title.
    /// </summary>
    public string Title { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the role (As a...).
    /// </summary>
    public string Role { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the action (I want to...).
    /// </summary>
    public string Action { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the benefit (So that...).
    /// </summary>
    public string Benefit { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the acceptance criteria.
    /// </summary>
    public List<string> AcceptanceCriteria { get; set; } = new List<string>();
    
    /// <summary>
    /// Gets or sets the source paragraph or section in COBOL.
    /// </summary>
    public string SourceLocation { get; set; } = string.Empty;
}

/// <summary>
/// Represents a feature description for batch/calculation processes.
/// </summary>
public class FeatureDescription
{
    /// <summary>
    /// Gets or sets the feature ID.
    /// </summary>
    public string Id { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the feature name.
    /// </summary>
    public string Name { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the description.
    /// </summary>
    public string Description { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the business rules for this feature.
    /// </summary>
    public List<string> BusinessRules { get; set; } = new List<string>();
    
    /// <summary>
    /// Gets or sets the inputs.
    /// </summary>
    public List<string> Inputs { get; set; } = new List<string>();
    
    /// <summary>
    /// Gets or sets the outputs.
    /// </summary>
    public List<string> Outputs { get; set; } = new List<string>();
    
    /// <summary>
    /// Gets or sets the processing steps.
    /// </summary>
    public List<string> ProcessingSteps { get; set; } = new List<string>();
    
    /// <summary>
    /// Gets or sets the source paragraph or section in COBOL.
    /// </summary>
    public string SourceLocation { get; set; } = string.Empty;
}

/// <summary>
/// Represents a business rule.
/// </summary>
public class BusinessRule
{
    /// <summary>
    /// Gets or sets the rule ID.
    /// </summary>
    public string Id { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the rule description.
    /// </summary>
    public string Description { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the condition or trigger.
    /// </summary>
    public string Condition { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the action or outcome.
    /// </summary>
    public string Action { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the source location in COBOL.
    /// </summary>
    public string SourceLocation { get; set; } = string.Empty;
}

/// <summary>
/// Represents a data entity in the business domain.
/// </summary>
public class DataEntity
{
    /// <summary>
    /// Gets or sets the entity name.
    /// </summary>
    public string Name { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the business description.
    /// </summary>
    public string Description { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the attributes.
    /// </summary>
    public List<DataAttribute> Attributes { get; set; } = new List<DataAttribute>();
    
    /// <summary>
    /// Gets or sets the source COBOL structure.
    /// </summary>
    public string SourceStructure { get; set; } = string.Empty;
}

/// <summary>
/// Represents an attribute of a data entity.
/// </summary>
public class DataAttribute
{
    /// <summary>
    /// Gets or sets the attribute name.
    /// </summary>
    public string Name { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the business meaning.
    /// </summary>
    public string BusinessMeaning { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets the data type.
    /// </summary>
    public string DataType { get; set; } = string.Empty;
    
    /// <summary>
    /// Gets or sets any validation rules.
    /// </summary>
    public List<string> ValidationRules { get; set; } = new List<string>();
}
