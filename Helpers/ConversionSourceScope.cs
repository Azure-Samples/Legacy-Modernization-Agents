using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// The exact source files a conversion run selected. The copybook-ownership and call-target
/// registries read only these, so estates sharing a source folder, or a superseded copy of a
/// copybook the gate did not bind, cannot make a stem ambiguous and silently drop the contracts
/// for every file. With no scope set, the registries read the whole source folder as before.
/// Flows with the run's execution context, so concurrent runs and tests do not share it.
/// </summary>
public static class ConversionSourceScope
{
    private sealed record Scope(HashSet<string> Paths, string Key);

    private static readonly AsyncLocal<Scope?> Current = new();

    public static string Key => Current.Value?.Key ?? "";

    public static void Set(IEnumerable<CobolFile> files)
    {
        var paths = files.Select(file => Path.GetFullPath(file.FilePath)).ToHashSet(StringComparer.Ordinal);
        Current.Value = new Scope(paths, string.Join('|', paths.Order(StringComparer.Ordinal)));
    }

    public static bool IsSet => Current.Value is not null;

    public static bool Contains(string path) => Current.Value?.Paths.Contains(Path.GetFullPath(path)) == true;

    public static void Clear() => Current.Value = null;

    public static IEnumerable<string> Filter(IEnumerable<string> paths)
    {
        var scope = Current.Value;
        return scope is null ? paths : paths.Where(path => scope.Paths.Contains(Path.GetFullPath(path)));
    }
}
