// Decides the package or namespace a converted program belongs to.
//
// Left to the model, each program invents its own root: one estate produced 33 packages under
// com.example, com.bank, com.legacy, com.bbva and com.modernized, and the copybook types shared
// between programs were re-emitted under each of them. A service cannot be assembled from that,
// and a shared record that exists four times is four records that can drift apart.
//
// The decision is therefore made here rather than asked for. A program's service is its folder in
// the source drop, which is how these estates are already organised — one directory per
// application area. Types that come from a copybook used by more than one program belong to
// neither program, so they go to a shared namespace both can reference.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;

public static class ConversionNamespacePolicy
{
    public const string RootVariable = "TARGET_ROOT_NAMESPACE";

    private const string JavaDefaultRoot = "com.modernized";
    private const string CSharpDefaultRoot = "Modernized";

    /// <summary>Programs sitting directly in the source root have no folder to name them.</summary>
    private const string DefaultService = "core";

    private const string SharedSegment = "shared";

    public static bool IsCSharp(string targetLanguage) =>
        targetLanguage.Equals("C#", StringComparison.OrdinalIgnoreCase)
        || targetLanguage.Equals("CSharp", StringComparison.OrdinalIgnoreCase);

    /// <summary>
    /// The root every generated type sits under. Configured once for the estate so that a
    /// second conversion run cannot land the same program somewhere else.
    /// </summary>
    public static string Root(string targetLanguage)
    {
        var configured = Environment.GetEnvironmentVariable(RootVariable);
        var csharp = IsCSharp(targetLanguage);

        if (string.IsNullOrWhiteSpace(configured))
            return csharp ? CSharpDefaultRoot : JavaDefaultRoot;

        var segments = configured
            .Split(new[] { '.', '/', '\\' }, StringSplitOptions.RemoveEmptyEntries)
            .Select(s => Sanitize(s))
            .Where(s => s.Length > 0)
            .ToList();

        if (segments.Count == 0)
            return csharp ? CSharpDefaultRoot : JavaDefaultRoot;

        return string.Join('.', segments.Select(s => csharp ? ToPascalCase(s) : s.ToLowerInvariant()));
    }

    /// <summary>
    /// The service a program belongs to, taken from its directory in the source drop.
    /// Nested directories are flattened onto one segment so the namespace stays shallow.
    /// </summary>
    public static string ServiceSegment(string? sourceRelativePath)
    {
        if (string.IsNullOrWhiteSpace(sourceRelativePath)) return DefaultService;

        var normalized = sourceRelativePath.Replace('\\', '/').Trim('/');
        var lastSlash = normalized.LastIndexOf('/');
        if (lastSlash <= 0) return DefaultService;

        // The deepest directory names the service: FUENTES/SRC/X.cbl is the SRC area of FUENTES,
        // and naming it "src" says nothing, so a generic container yields to its parent.
        var directories = normalized[..lastSlash]
            .Split('/', StringSplitOptions.RemoveEmptyEntries)
            .Where(d => !GenericContainers.Contains(d))
            .ToList();

        if (directories.Count == 0) return DefaultService;

        var service = Sanitize(directories[^1]);
        return service.Length == 0 ? DefaultService : service;
    }

    private static readonly HashSet<string> GenericContainers = new(StringComparer.OrdinalIgnoreCase)
    {
        "src", "source", "sources", "cobol", "programs", "pgm", "cbl",
    };

    /// <summary>The namespace or package for a converted program.</summary>
    public static string ForProgram(string targetLanguage, string? sourceRelativePath)
    {
        var service = ServiceSegment(sourceRelativePath);
        return Join(targetLanguage, Root(targetLanguage), service);
    }

    /// <summary>
    /// Where a type produced from a copybook used by more than one program lives. It belongs to
    /// no single program, so nesting it inside one of them duplicates it into all of them.
    /// </summary>
    public static string ForSharedTypes(string targetLanguage) =>
        Join(targetLanguage, Root(targetLanguage), SharedSegment);

    private static string Join(string targetLanguage, string root, string segment)
    {
        var csharp = IsCSharp(targetLanguage);
        var tail = csharp ? ToPascalCase(segment) : segment.ToLowerInvariant();
        return $"{root}.{tail}";
    }

    /// <summary>Keeps only characters both languages accept in an identifier segment.</summary>
    private static string Sanitize(string value)
    {
        var sb = new StringBuilder(value.Length);
        foreach (var ch in value)
        {
            if (char.IsLetterOrDigit(ch)) sb.Append(ch);
            else if (ch is '-' or '_' or ' ') sb.Append('_');
        }

        var cleaned = sb.ToString().Trim('_');
        // A segment cannot open with a digit in either language.
        return cleaned.Length > 0 && char.IsDigit(cleaned[0]) ? "_" + cleaned : cleaned;
    }

    private static string ToPascalCase(string value)
    {
        var parts = value.Split(new[] { '_', '-', ' ' }, StringSplitOptions.RemoveEmptyEntries);
        var sb = new StringBuilder();
        foreach (var part in parts)
        {
            sb.Append(char.ToUpperInvariant(part[0]));
            if (part.Length > 1) sb.Append(part[1..].ToLowerInvariant());
        }
        return sb.Length == 0 ? value : sb.ToString();
    }
}
