using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Jcl;

public sealed record JclMember(string Name, string Text, string? File);

// Catalogued procedures and INCLUDE members found in the source, by member name. A mainframe
// resolves them through JCLLIB libraries; an export has only file names, so the library is used
// only to choose between two files with the same member name.
public sealed class JclMemberLibrary
{
    private readonly Dictionary<string, List<string>> _files;
    private readonly Dictionary<string, string> _texts;
    private readonly string? _root;

    private JclMemberLibrary(Dictionary<string, List<string>> files, Dictionary<string, string> texts, string? root)
    {
        _files = files;
        _texts = texts;
        _root = root;
    }

    public static JclMemberLibrary Empty { get; } = new([], [], null);

    public static JclMemberLibrary FromDirectory(string root)
    {
        var files = SourceTypeRegistry.EnumerateJclMemberFiles(root)
            .GroupBy(f => Path.GetFileNameWithoutExtension(f).ToUpperInvariant())
            .ToDictionary(g => g.Key, g => g.OrderBy(f => f, StringComparer.Ordinal).ToList(), StringComparer.Ordinal);
        return new JclMemberLibrary(files, [], root);
    }

    public static JclMemberLibrary FromTexts(IReadOnlyDictionary<string, string> members) =>
        new([], members.ToDictionary(m => m.Key.ToUpperInvariant(), m => m.Value, StringComparer.Ordinal), null);

    // The member, or null; candidates is how many files had its name.
    public JclMember? Find(string name, IReadOnlyList<string> libraries, Func<string, bool> accept, out int candidates)
    {
        var key = name.ToUpperInvariant();
        if (_texts.TryGetValue(key, out var text))
        {
            candidates = 1;
            return accept(text) ? new JclMember(key, text, null) : null;
        }

        var matches = new List<JclMember>();
        foreach (var file in _files.GetValueOrDefault(key) ?? [])
        {
            string content;
            try { content = File.ReadAllText(file); }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException) { continue; }
            if (accept(content)) matches.Add(new JclMember(key, content, Relative(file)));
        }

        candidates = matches.Count;
        if (matches.Count <= 1) return matches.FirstOrDefault();

        // A folder named after a JCLLIB library is the closest an export gets to that library.
        foreach (var library in libraries)
        {
            var inLibrary = matches.FirstOrDefault(m => m.File is { } f
                && f.Split('/', '\\').Any(s => s.Equals(library, StringComparison.OrdinalIgnoreCase)));
            if (inLibrary is not null) return inLibrary;
        }

        return matches[0];
    }

    private string Relative(string file) =>
        _root is null ? file : Path.GetRelativePath(_root, file).Replace('\\', '/');
}
