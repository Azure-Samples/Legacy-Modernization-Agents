namespace CobolToQuarkusMigration.Helpers;

public static class CopilotProvider
{
    public const string CanonicalServiceType = "GitHubCopilot";

    public static bool IsSdk(string? serviceType) =>
        serviceType?.Trim().ToLowerInvariant() is
            "githubcopilot" or "githubcopilotsdk" or "copilotsdk";

    public static string Canonicalize(string? serviceType) =>
        IsSdk(serviceType) ? CanonicalServiceType : serviceType?.Trim() ?? "AzureOpenAI";
}
