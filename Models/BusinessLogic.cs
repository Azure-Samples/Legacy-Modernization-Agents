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
    /// Gets or sets whether this is a copybook file.
    /// </summary>
    public bool IsCopybook { get; set; }

    /// <summary>
    /// Gets or sets the overall business purpose of the program.
    /// </summary>
    public string BusinessPurpose { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the feature descriptions extracted from the code.
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
}

/// <summary>
/// Represents a feature description extracted from COBOL code.
/// </summary>
public class UserStory
{
    /// <summary>
    /// Gets or sets the feature description ID.
    /// </summary>
    public string Id { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the title.
    /// </summary>
    public string Title { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the trigger or context for this feature.
    /// </summary>
    public string Role { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the description of what this feature does.
    /// </summary>
    public string Action { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the business benefit or outcome.
    /// </summary>
    public string Benefit { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the business rules.
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
