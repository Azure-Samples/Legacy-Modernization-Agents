using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Caching;

/// <summary>
/// Builds <see cref="CacheKey"/> instances for the Java converter. Lives outside
/// <c>JavaConverterAgent</c> so the construction logic is testable without
/// instantiating the full agent.
/// </summary>
/// <remarks>
/// All inputs are explicit parameters — no AsyncLocal, no ambient state. The
/// caller assembles them from data it already has in hand.
/// </remarks>
public static class JavaConverterCacheKeys
{
    public const string TemplateId = "java-converter";

    /// <summary>
    /// Bump when the converter's prompt template, REKT injection block, or the
    /// final-output validation rules change in a way that would produce different
    /// Java for the same source. Pinned for an entire prompt-tuning iteration so
    /// the cache is reusable across re-runs.
    /// </summary>
    public const string TemplateVersion = "1";

    public const string Provider = "azure-openai";
    public const string TargetLanguage = "java";
    public const string FrameworkSettings = "quarkus";

    /// <summary>
    /// Hash inputs that combine to make the prompt deterministic. The agent supplies
    /// only what is in scope; <see cref="CacheKey.Build"/> will reject any missing field.
    /// </summary>
    /// <param name="systemPrompt">Verbatim system prompt sent to the model.</param>
    /// <param name="userPrompt">Verbatim user prompt sent to the model (after REKT, business-logic, sanitisation).</param>
    /// <param name="preprocessedSourceBytes">
    /// Exact bytes of the COBOL source as embedded in the prompt — sanitised, post-preprocessor.
    /// Use this rather than the raw file bytes so preprocessor changes correctly invalidate.
    /// </param>
    /// <param name="rektContextBlock">
    /// Exact REKT context text appended to the prompt (empty string if none). Hashing the
    /// embedded text avoids invalidating on incidental REKT-file changes that don't affect the prompt.
    /// </param>
    /// <param name="model">Deployment/model identifier.</param>
    /// <param name="maxOutputTokens">Computed via <c>CalculateTokenSettings</c>.</param>
    /// <param name="reasoningEffort">Computed via <c>CalculateTokenSettings</c>.</param>
    /// <param name="cobolFile">The file being converted — supplies basename + (future) relative path.</param>
    public static CacheKey ForConversion(
        string systemPrompt,
        string userPrompt,
        string preprocessedSourceBytes,
        string rektContextBlock,
        string model,
        int maxOutputTokens,
        string reasoningEffort,
        CobolFile cobolFile)
    {
        var systemHash = CanonicalHasher.HashUtf8(systemPrompt);
        var userHash = CanonicalHasher.HashUtf8(userPrompt);
        var sourceHash = CanonicalHasher.HashUtf8(preprocessedSourceBytes);
        var rektHash = string.IsNullOrEmpty(rektContextBlock)
            ? ""  // empty hash signals "no REKT context" — distinct from a REKT block that happens to hash to something
            : CanonicalHasher.HashUtf8(rektContextBlock);

        // Generation settings — anything that could change the model output without
        // changing the prompt text. top_p, seed, stop sequences are not currently
        // exposed in our ResponsesApiClient path, but include the hashable fields
        // we do control so a future change shows up in the key.
        var generationHash = CanonicalHasher.HashFields(
            "max_output_tokens", maxOutputTokens.ToString(System.Globalization.CultureInfo.InvariantCulture),
            "reasoning_effort", reasoningEffort,
            "response_format", "text",
            "top_p", "default",
            "seed", "none",
            "stop", "none");

        return CacheKey.Build(new CacheKey
        {
            ProviderKey = Provider,
            Model = model,
            SystemPromptHash = systemHash,
            UserPromptHash = userHash,
            ReasoningEffort = reasoningEffort,
            ResponseFormat = "text",
            PromptTemplateId = TemplateId,
            PromptTemplateVersion = TemplateVersion,
            TargetLanguage = TargetLanguage,
            FrameworkSettings = FrameworkSettings,
            SourceHash = sourceHash,
            RektFactsHash = rektHash,
            GenerationSettingsHash = generationHash,
            Basename = cobolFile.FileName,
            // RelativePath is not currently populated (basename identity); leave null.
            // When the ProgramKey migration lands, set this from CobolFile.FilePath
            // and bump IdentitySchemeVersion.
            RelativePath = null,
        });
    }

    /// <summary>
    /// Determines whether the final assembled Java is structurally complete enough
    /// to cache. Mirrors the validity checks the continuation loop uses, so we
    /// never cache code that the agent itself would consider truncated.
    /// </summary>
    public static bool IsCacheableJava(string? javaCode)
    {
        if (string.IsNullOrWhiteSpace(javaCode)) return false;

        var hasPackage = javaCode.Contains("package ", StringComparison.Ordinal);
        var hasClass = javaCode.Contains("class ", StringComparison.Ordinal);
        var opens = javaCode.Count(c => c == '{');
        var closes = javaCode.Count(c => c == '}');

        return hasPackage && hasClass && opens > 0 && opens == closes;
    }
}
