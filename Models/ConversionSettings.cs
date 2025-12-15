using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Defines the naming strategy used during code conversion.
/// </summary>
[JsonConverter(typeof(JsonStringEnumConverter))]
public enum NamingStrategy
{
    /// <summary>
    /// Rule-based naming (VALIDATE-CUST → validateCust).
    /// 100% reproducible - same input always produces same output.
    /// Best for: regulated industries, audits, reproducible builds.
    /// No AI involvement in naming decisions.
    /// </summary>
    Deterministic,

    /// <summary>
    /// AI freely chooses modern names each conversion.
    /// VALIDATE-CUST might become validateCustomerEligibility.
    /// WARNING: Same file converted twice may produce different names.
    /// Only use for: exploratory work, POC, prototyping.
    /// </summary>
    AIGenerated,

    /// <summary>
    /// (DEFAULT) AI suggests modern names on FIRST encounter,
    /// then name is locked in database forever.
    /// VALIDATE-CUST → AI suggests validateCustomerEligibility → cached → 
    /// all future references use cached name.
    /// Best balance of modern naming + consistency.
    /// </summary>
    Hybrid
}

/// <summary>
/// Configuration settings for code conversion behavior.
/// Controls naming strategies, type mappings, and consistency enforcement.
/// </summary>
public class ConversionSettings
{
    /// <summary>
    /// The naming strategy to use for converting legacy names to modern names.
    /// Default: Hybrid (AI suggests once, then cached forever).
    /// </summary>
    public NamingStrategy NamingStrategy { get; set; } = NamingStrategy.Hybrid;

    /// <summary>
    /// Whether to enforce deterministic type mappings.
    /// When true, PIC clauses are always mapped to the same target types.
    /// Default: true.
    /// </summary>
    public bool EnforceDeterministicTypes { get; set; } = true;

    /// <summary>
    /// Whether to allow AI to suggest modern patterns beyond direct translation.
    /// For example, suggesting Builder patterns, using Optional types, etc.
    /// Default: true.
    /// </summary>
    public bool AllowAIToSuggestModernPatterns { get; set; } = true;

    /// <summary>
    /// Whether to preserve original legacy names as comments in converted code.
    /// Useful for traceability and debugging.
    /// Default: true.
    /// </summary>
    public bool PreserveLegacyNamesAsComments { get; set; } = true;

    /// <summary>
    /// Whether to generate mapping documentation showing legacy → modern name mappings.
    /// Default: true.
    /// </summary>
    public bool GenerateMappingDocumentation { get; set; } = true;

    /// <summary>
    /// Prefix to add to all generated class names (e.g., "Legacy" → LegacyCustomerService).
    /// Empty string means no prefix.
    /// Default: empty.
    /// </summary>
    public string ClassNamePrefix { get; set; } = string.Empty;

    /// <summary>
    /// Suffix to add to all generated class names (e.g., "Service" → CustomerDataService).
    /// Empty string means no suffix.
    /// Default: empty.
    /// </summary>
    public string ClassNameSuffix { get; set; } = string.Empty;

    /// <summary>
    /// Whether to run a reconciliation pass after all chunks are converted.
    /// This pass validates cross-chunk consistency and fixes any issues.
    /// Default: true.
    /// </summary>
    public bool EnableReconciliationPass { get; set; } = true;

    /// <summary>
    /// Maximum number of auto-fix attempts for consistency violations.
    /// After this limit, violations are reported as errors.
    /// Default: 3.
    /// </summary>
    public int MaxAutoFixAttempts { get; set; } = 3;

    /// <summary>
    /// Whether to fail the entire conversion if any consistency violations remain.
    /// When false, violations are logged as warnings but conversion continues.
    /// Default: false.
    /// </summary>
    public bool FailOnConsistencyViolation { get; set; } = false;
}
