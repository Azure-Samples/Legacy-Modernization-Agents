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
    public static string ForProgram(string targetLanguage, string? sourceRelativePath) =>
        Render(targetLanguage, ServiceSegment(sourceRelativePath), Kind.Program);

    /// <summary>
    /// Where a type produced from a copybook used by more than one program lives. It belongs to
    /// no single program, so nesting it inside one of them duplicates it into all of them.
    /// </summary>
    public static string ForSharedTypes(string targetLanguage) =>
        Render(targetLanguage, SharedSegment, Kind.Shared);

    private enum Kind { Program, Shared }

    /// <summary>
    /// The layout beneath the root. Estates arrive with a house style already decided, so the
    /// structure is chosen rather than imposed; leaving it unset keeps the service-per-folder
    /// layout that was here before.
    /// </summary>
    public const string ArchitectureVariable = "TARGET_ARCHITECTURE";

    public const string TemplateVariable = "TARGET_NAMESPACE_TEMPLATE";

    public static string Architecture()
    {
        var configured = Environment.GetEnvironmentVariable(ArchitectureVariable)?.Trim();
        return string.IsNullOrWhiteSpace(configured) ? "service" : configured.ToLowerInvariant();
    }

    /// <summary>The template a layout resolves to, before its placeholders are filled.</summary>
    public static string TemplateFor(string architecture, bool shared) => architecture switch
    {
        // Every program of a source folder is one deployable unit; shared records sit beside them.
        "service" => shared ? "{root}.{shared}" : "{root}.{service}",

        // Records from copybooks are the domain; converted programs are what acts on it.
        "layered" or "ddd" => shared ? "{root}.domain" : "{root}.{service}.application",

        // One namespace for everything, for an estate small enough not to need dividing.
        "flat" => "{root}",

        "custom" => Environment.GetEnvironmentVariable(TemplateVariable)?.Trim() is { Length: > 0 } t
            ? t
            : "{root}.{service}",

        // An unrecognised value is treated as the default rather than failing a conversion that
        // is otherwise fine; the name it was given appears in the prompt so the typo is visible.
        _ => shared ? "{root}.{shared}" : "{root}.{service}",
    };

    private static string Render(string targetLanguage, string segment, Kind kind)
    {
        var template = TemplateFor(Architecture(), kind == Kind.Shared);
        var csharp = IsCSharp(targetLanguage);

        // Each segment is cased exactly once. Substituting first and casing afterwards would
        // re-case an already-cased value, turning "MyService" into "Myservice".
        var rendered = new List<string>();

        foreach (var part in template.Split('.', StringSplitOptions.RemoveEmptyEntries))
        {
            var token = part.Trim();

            if (Matches(token, "root"))
            {
                // Already cased, and may itself contain dots ("Bankdata.Core").
                rendered.Add(Root(targetLanguage));
            }
            else if (Matches(token, "service"))
            {
                rendered.Add(Case(segment, csharp));
            }
            else if (Matches(token, "shared"))
            {
                rendered.Add(Case(SharedSegment, csharp));
            }
            else
            {
                // A literal the template author wrote, or an unknown placeholder; either way it
                // is emitted as written rather than silently dropped.
                rendered.Add(Case(token.Trim('{', '}'), csharp));
            }
        }

        return string.Join('.', rendered.Where(p => p.Length > 0));
    }

    private static bool Matches(string token, string name) =>
        token.Equals("{" + name + "}", StringComparison.OrdinalIgnoreCase);

    private static string Case(string value, bool csharp) =>
        csharp ? ToPascalCase(value) : value.ToLowerInvariant();

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
