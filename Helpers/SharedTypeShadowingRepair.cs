// A program and a copybook can share a name (INQTRANL.cbl copies INQTRANL.cpy). The copybook's
// type is shared; if the program's conversion declares any type under that name — an entity, a
// nested record — the local type hides the shared one and every use of a copybook field fails
// to compile. The prompt says the name is taken; this catches the conversions that reuse it anyway.

namespace CobolToQuarkusMigration.Helpers;

using Microsoft.Extensions.Logging;

public static class SharedTypeShadowingRepair
{
    /// <summary>Shared copybook type names the converted <paramref name="fileName"/> must reference.</summary>
    public static IReadOnlyList<string> SharedNamesFor(
        string fileName, string? sourceDirectory, string? repositoryRoot, ILogger? logger = null)
    {
        try
        {
            var root = RektPromptInjector.ResolveRepositoryRoot(repositoryRoot);
            if (root is null) return Array.Empty<string>();
            return CopybookOwnershipRegistryHolder
                .GetOrBuild(root.FullName, RektPromptInjector.ResolveSourceFolder(sourceDirectory))
                .ReferencedTypeNames(Path.GetFileName(fileName));
        }
        catch (Exception ex)
        {
            logger?.LogWarning("[SharedTypeShadowing] Could not read copybook ownership for {File}: {Msg}", fileName, ex.Message);
            return Array.Empty<string>();
        }
    }

    /// <summary>
    /// Returns <paramref name="code"/>, or a single corrected conversion when the original hides a
    /// shared type and the correction no longer does. <paramref name="askForCorrection"/> receives the
    /// shadowed names and returns cleaned code, or null when the provider failed.
    /// </summary>
    public static async Task<string> RepairAsync(
        string code,
        IReadOnlyList<string> sharedNames,
        Func<IReadOnlyList<string>, Task<string?>> askForCorrection,
        string fileName,
        ILogger logger)
    {
        if (sharedNames.Count == 0) return code;
        var shadowed = CopybookOwnershipRegistry.ShadowedSharedTypes(code, sharedNames);
        if (shadowed.Count == 0) return code;

        logger.LogWarning(
            "[SharedTypeShadowing] {File} declares {Names}, hiding the shared copybook type(s); asking for a rename",
            fileName, string.Join(", ", shadowed));

        string? corrected;
        try { corrected = await askForCorrection(shadowed); }
        catch (Exception ex)
        {
            logger.LogWarning("[SharedTypeShadowing] Correction request failed for {File}: {Msg}", fileName, ex.Message);
            return code;
        }

        var reason = Rejection(code, corrected, sharedNames);
        if (reason is null)
        {
            logger.LogInformation("[SharedTypeShadowing] {File}: local type renamed, shared {Names} now resolves", fileName, string.Join(", ", shadowed));
            return corrected!;
        }

        logger.LogWarning("[SharedTypeShadowing] Kept the original {File}: correction {Reason}", fileName, reason);
        return code;
    }

    private static string? Rejection(string original, string? corrected, IReadOnlyList<string> sharedNames)
    {
        if (string.IsNullOrWhiteSpace(corrected)) return "was empty";
        if (CopybookOwnershipRegistry.ShadowedSharedTypes(corrected, sharedNames).Count > 0) return "still declares a shared name";
        if (!CopybookOwnershipRegistry.DeclaresAnyType(corrected)) return "declares no type";
        // Compared with the original rather than with zero: braces inside string literals count too.
        if (BraceBalance(corrected) != BraceBalance(original)) return "changed the brace balance";
        return null;
    }

    private static int BraceBalance(string code) => code.Count(c => c == '{') - code.Count(c => c == '}');
}
