// Finds the repository the portal is reporting on.
//
// The portal does not have to run from inside the estate it describes — it is routinely started
// from a separate checkout with REPO_ROOT pointing at the repository that holds source/ and
// output/. Most readers honour that. Several endpoints instead resolved their files relative to
// the portal's own content root, so they looked for the estate's output inside the portal's
// directory and reported it missing — a report that had just been generated, said not to exist.
//
// One definition, so the answer cannot differ between two parts of the same page.

namespace McpChatWeb.Services;

public static class RepositoryRoot
{
    public const string Variable = "REPO_ROOT";

    /// <summary>
    /// The repository root: the configured one when it exists, otherwise the nearest ancestor of
    /// <paramref name="startFrom"/> that contains doctor.sh.
    /// </summary>
    public static string Resolve(string? startFrom = null)
    {
        var configured = Environment.GetEnvironmentVariable(Variable);
        if (!string.IsNullOrWhiteSpace(configured) && Directory.Exists(configured))
            return Path.GetFullPath(configured);

        var start = string.IsNullOrWhiteSpace(startFrom) ? Directory.GetCurrentDirectory() : startFrom;

        var dir = new DirectoryInfo(start);
        while (dir != null && !File.Exists(Path.Combine(dir.FullName, "doctor.sh")))
            dir = dir.Parent;

        return dir?.FullName ?? start;
    }

    /// <summary>A path inside the repository, wherever the portal happens to be running from.</summary>
    public static string PathTo(string? startFrom, params string[] segments) =>
        Path.GetFullPath(Path.Combine(new[] { Resolve(startFrom) }.Concat(segments).ToArray()));
}
