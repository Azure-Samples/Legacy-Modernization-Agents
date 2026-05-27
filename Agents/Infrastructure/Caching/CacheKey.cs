using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Caching;

/// <summary>
/// Immutable cache key for the deterministic response cache. Every field is
/// required at construction time — callers go through <see cref="Build"/> which
/// validates and throws on missing fields.
/// </summary>
/// <remarks>
/// <para>Schema versioning:</para>
/// <list type="bullet">
///   <item><see cref="KeySchemaVersion"/> bumps when this record gains/loses fields.
///         Old entries are simply unreachable (cache miss); the storage schema is unaffected.</item>
///   <item><see cref="IdentitySchemeVersion"/> is the file-identity scheme (basename vs relative-path).
///         Pinned to <c>"v1-basename"</c> per docs/basename-coupling-map.md.</item>
/// </list>
/// </remarks>
public sealed record CacheKey
{
    /// <summary>Bumped when the set of key fields changes. Old entries become unreachable.</summary>
    public const string KeySchemaVersion = "1";

    /// <summary>Current identity scheme — see docs/basename-coupling-map.md.</summary>
    public const string CurrentIdentitySchemeVersion = "v1-basename";

    public required string ProviderKey { get; init; }
    public required string Model { get; init; }
    public required string SystemPromptHash { get; init; }
    public required string UserPromptHash { get; init; }
    public required string ReasoningEffort { get; init; }
    public required string ResponseFormat { get; init; }
    public required string PromptTemplateId { get; init; }
    public required string PromptTemplateVersion { get; init; }
    public required string TargetLanguage { get; init; }
    public required string FrameworkSettings { get; init; }
    public required string SourceHash { get; init; }
    public required string RektFactsHash { get; init; }

    /// <summary>Generation settings — captured as a single hash so the field set can grow later.</summary>
    public required string GenerationSettingsHash { get; init; }

    public string IdentitySchemeVersion { get; init; } = CurrentIdentitySchemeVersion;

    // Forward-compat identity payload — stored alongside the entry for diagnostics
    // and so a future ProgramKey migration has the info it needs. Not part of the
    // key hash today (basename is the identity); becomes part of the key after the
    // identity migration lands.
    public string? Basename { get; init; }
    public string? RelativePath { get; init; }

    /// <summary>
    /// Validates required fields and returns the key. Use this rather than the
    /// record initialiser to fail fast on missing values.
    /// </summary>
    public static CacheKey Build(CacheKey key)
    {
        ThrowIfEmpty(key.ProviderKey, nameof(ProviderKey));
        ThrowIfEmpty(key.Model, nameof(Model));
        ThrowIfEmpty(key.SystemPromptHash, nameof(SystemPromptHash));
        ThrowIfEmpty(key.UserPromptHash, nameof(UserPromptHash));
        ThrowIfEmpty(key.PromptTemplateId, nameof(PromptTemplateId));
        ThrowIfEmpty(key.PromptTemplateVersion, nameof(PromptTemplateVersion));
        ThrowIfEmpty(key.TargetLanguage, nameof(TargetLanguage));
        ThrowIfEmpty(key.GenerationSettingsHash, nameof(GenerationSettingsHash));
        ThrowIfEmpty(key.IdentitySchemeVersion, nameof(IdentitySchemeVersion));
        // SourceHash, RektFactsHash, ReasoningEffort, ResponseFormat, FrameworkSettings
        // may legitimately be empty strings (e.g. no REKT facts for this call) — only
        // the required-content fields are validated.
        return key;
    }

    /// <summary>
    /// Computes the SHA-256 key hash. Stable across processes and OS — only depends
    /// on the field values, not on field ordering in source or struct layout.
    /// </summary>
    public string Compute() => CanonicalHasher.HashFields(
        // Order matters: any reordering changes every existing key.
        // Bump KeySchemaVersion if you change this list.
        KeySchemaVersion,
        IdentitySchemeVersion,
        ProviderKey,
        Model,
        ReasoningEffort,
        ResponseFormat,
        TargetLanguage,
        FrameworkSettings,
        PromptTemplateId,
        PromptTemplateVersion,
        SystemPromptHash,
        UserPromptHash,
        GenerationSettingsHash,
        SourceHash,
        RektFactsHash);

    private static void ThrowIfEmpty(string value, string name)
    {
        if (string.IsNullOrEmpty(value))
            throw new ArgumentException($"CacheKey.{name} must be non-empty for cache correctness.", name);
    }
}
