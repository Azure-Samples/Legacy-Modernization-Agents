using System.Security.Cryptography;
using System.Text.Json;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed record DependencyInputAvailability(
    string Kind, string Name, string Status, string? Path, string Scope, string Reason);

public sealed record DependencyInputInventoryResult(
    IReadOnlyList<DependencyInputAvailability> Items, string? Warning = null);

public sealed class DependencyInputInventory(string sourceRoot)
{
    private readonly Dictionary<string, (string Hash, bool Placeholder)> _contents = new(StringComparer.Ordinal);
    private readonly Lazy<ILookup<string, string>> _candidates = new(() =>
        SourceTypeRegistry.EnumerateCopybookFiles(sourceRoot)
            .Concat(SourceTypeRegistry.EnumerateProgramFiles(sourceRoot))
            .ToLookup(path => System.IO.Path.GetFileNameWithoutExtension(path), StringComparer.OrdinalIgnoreCase));

    public DependencyInputInventoryResult Read(string factsRoot, string program)
    {
        try
        {
            var evidence = DependencyEvidenceGate.LoadEvidence(factsRoot, program);
            if (evidence is null)
                return new([], "Input bindings are not recorded: dependency evidence is missing. This does not mean the program has no dependencies.");
            if (evidence.Definitions is null || evidence.UnresolvedDiagnostics is null
                || evidence.Definitions.Any(definition => definition is null || string.IsNullOrWhiteSpace(definition.Kind)
                    || string.IsNullOrWhiteSpace(definition.Name) || string.IsNullOrWhiteSpace(definition.Path))
                || evidence.UnresolvedDiagnostics.Any(diagnostic => diagnostic is null))
                return new([], "Input inventory could not be completed: dependency evidence contains invalid input bindings or diagnostics. No empty dependency set is inferred.");
            if (evidence.SchemaVersion != 1 || evidence.Producer != "cobol-rekt/dependency" || evidence.RelativePath != program)
                return new([], "Input inventory is unverified: the evidence schema, producer or program identity does not match. Regenerate matching evidence; no empty dependency set is inferred.");
            var items = evidence.Definitions.Distinct().Select(definition => Inspect(definition, program)).ToList();
            foreach (var reference in evidence.UnresolvedDiagnostics.Select(Reference).OfType<(string Kind, string Name)>()
                         .Distinct())
            {
                if (items.Any(item => item.Kind == reference.Kind && SameName(item.Name, reference.Name))) continue;
                var candidates = _candidates.Value[System.IO.Path.GetFileNameWithoutExtension(reference.Name)]
                    .Where(path => reference.Kind == "COPY" ? SourceTypeRegistry.IsCopybook(path) : SourceTypeRegistry.IsCobolProgram(path))
                    .Order(StringComparer.Ordinal).ToArray();
                if (candidates.Length == 0)
                    items.Add(new(reference.Kind, reference.Name, "missing", null, "unknown",
                        "No matching filename was found in the selected source tree. PROGRAM-ID aliases and external SDK/provider libraries are not inventoried by this view."));
                foreach (var path in candidates)
                    items.Add(Inspect(new() { Kind = reference.Kind, Name = reference.Name, Path = path }, program));
            }
            return new(items.OrderBy(item => item.Kind).ThenBy(item => item.Name).ThenBy(item => item.Path).ToArray());
        }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or JsonException or ArgumentException)
        {
            return new([], $"Input inventory could not be completed: {ex.Message}. Inspect the dependency evidence and source access; no empty dependency set is inferred.");
        }
    }

    private DependencyInputAvailability Inspect(DependencyDefinition definition, string program)
    {
        var path = definition.Path;
        var scope = Scope(sourceRoot, program, path);
        DependencyInputAvailability Item(string status, string reason) =>
            new(definition.Kind, definition.Name, status, path, scope, reason);
        if (!System.IO.Path.IsPathFullyQualified(path))
            return Item("unverified", "The definition has no absolute input path.");
        if (definition.Kind is "PLATFORM_COPY" or "PLATFORM_INPUT")
            return Item("configured", "Explicit symbolic platform replacement, not a resolved vendor copybook. Original byte values are not supplied; target event implementation and behavioral tests are still required. Consult the separate strict evidence check.");
        if (!Within(path, sourceRoot))
            return Item("configured", "Outside the selected source tree; consult the configured profile/contract and the separate strict evidence check. This inventory does not open that path.");
        try
        {
            if (!File.Exists(path)) return Item("missing", "The recorded input path is no longer present.");
            if (!_contents.TryGetValue(path, out var content))
            {
                content = (Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path))),
                    DependencyEvidenceGate.IsPlaceholder(path));
                _contents[path] = content;
            }
            if (content.Placeholder)
                return Item("rejected", "A placeholder/stub is present, not a usable dependency definition.");
            if (string.IsNullOrEmpty(definition.Sha256))
                return Item("unverified", "A candidate file is present, but the exporter has not bound it. Check include paths, ambiguity and provenance, then regenerate evidence.");
            return string.Equals(content.Hash, definition.Sha256, StringComparison.OrdinalIgnoreCase)
                ? Item("resolved", "Present; bytes match the recorded input hash. This alone does not establish parser coverage or runtime equivalence.")
                : Item("stale", "Present, but its bytes differ from the recorded hash. Regenerate matching evidence.");
        }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException)
        {
            return Item("unreadable", $"Input could not be read: {ex.Message}");
        }
    }

    public static string Scope(string root, string program, string path)
    {
        if (!System.IO.Path.IsPathFullyQualified(path) || !Within(path, root)) return "configured";
        var relative = SourcePathHelper.NormalizeRelativePath(System.IO.Path.GetRelativePath(root, path));
        if (relative.Split('/').Contains(".preprocessed", StringComparer.Ordinal)) return "derived";
        var parts = SourcePathHelper.NormalizeRelativePath(program).Split('/');
        return parts.Length > 1 && relative.Split('/')[0] != parts[0] ? "shared" : "local";
    }

    private static bool Within(string path, string root)
    {
        var relative = System.IO.Path.GetRelativePath(System.IO.Path.GetFullPath(root), System.IO.Path.GetFullPath(path));
        return !System.IO.Path.IsPathRooted(relative) && relative != ".."
            && !relative.StartsWith(".." + System.IO.Path.DirectorySeparatorChar, StringComparison.Ordinal);
    }

    private static bool SameName(string left, string right) =>
        string.Equals(System.IO.Path.GetFileNameWithoutExtension(left), System.IO.Path.GetFileNameWithoutExtension(right),
            StringComparison.OrdinalIgnoreCase);

    private static (string Kind, string Name)? Reference(string raw)
    {
        while (raw.StartsWith("native:", StringComparison.Ordinal)) raw = raw["native:".Length..];
        if (raw.StartsWith("parser-diagnostic:missing copybook:", StringComparison.Ordinal))
            return ("COPY", raw["parser-diagnostic:missing copybook:".Length..].Split(':')[0].Trim());
        var parts = raw.Split(':', 3);
        if (parts.Length < 2 || string.IsNullOrWhiteSpace(parts[1])) return null;
        if (parts[0] == "callee" && parts.Length == 3) return Reference(parts[2]);
        return parts[0] switch
        {
            "unresolved-copybook" or "unverified-copybook" or "unreadable-copybook" or "copy-not-exported" => ("COPY", parts[1].Trim()),
            "source-not-found" => ("CALL", parts[1].Trim()),
            _ => null
        };
    }
}
