namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// An explicit, opt-in target runtime policy: the deployment choices (datastore, transaction,
/// service-call and terminal mappings) that the source cannot supply and the converter prompts
/// refuse to invent. Set TARGET_RUNTIME_POLICY to a Markdown file, relative to the repository
/// root or absolute. Unset means no policy, and strict conversion keeps blocking on those inputs.
/// </summary>
public static class TargetRuntimePolicy
{
    public const string EnvironmentVariable = "TARGET_RUNTIME_POLICY";

    public static string Block(string? configured, string repositoryRoot)
    {
        if (string.IsNullOrWhiteSpace(configured)) return "";
        var path = Path.IsPathRooted(configured) ? configured : Path.Combine(repositoryRoot, configured);
        // A named policy that cannot be read must stop the run: converting without it would
        // quietly produce the refusals the policy was supplied to resolve, or worse, a mix.
        if (!File.Exists(path))
            throw new FileNotFoundException($"{EnvironmentVariable} names a policy file that does not exist.", path);
        return "\n## Selected target runtime policy (explicit deployment choice)\n" +
            "The following is a deliberate target-port decision supplied by the application owner. " +
            "It is NOT evidence of original mainframe behavior and does not prove behavioral equivalence. " +
            "Where it maps a runtime concern, implement that mapping instead of blocking on it. It never " +
            "supplies missing source: copybook layouts, SQL, paragraphs and callee contracts still come only " +
            "from the source-bound context above.\n\n" +
            File.ReadAllText(path).Trim() + "\n";
    }
}
