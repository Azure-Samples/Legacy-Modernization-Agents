// Identifies data names that exist only because tools/preprocess-for-rekt.sh synthesised a
// placeholder copybook for an unresolved COPY. They are preprocessing artefacts, so expecting
// them in generated code produces gaps no conversion can ever close.
//
// Names are pooled across the estate rather than tracked per program. A stub exists only where
// the copybook is absent from source/ entirely, so no other program can COPY a real field of
// the same <NAME>-STUB / <NAME>-VAL shape; a collision needs a hand-declared field mirroring
// the generator's naming, which is not worth per-program attribution.

using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents;

public sealed class StubCopybookCatalog
{
    public const string Marker = "AUTO-GENERATED STUB COPYBOOK";

    private static readonly Regex DataName =
        new(@"^\s*\d{2}\s+([A-Za-z0-9][A-Za-z0-9-]*)", RegexOptions.Multiline | RegexOptions.Compiled);

    private readonly HashSet<string> _names;
    private readonly List<string> _copybooks;

    private StubCopybookCatalog(HashSet<string> names, List<string> copybooks)
    {
        _names = names;
        _copybooks = copybooks;
    }

    public static StubCopybookCatalog Empty { get; } =
        new(new HashSet<string>(StringComparer.OrdinalIgnoreCase), []);

    public IReadOnlyCollection<string> Copybooks => _copybooks;

    public bool Contains(string dataName) => _names.Contains(dataName.Trim());

    public static StubCopybookCatalog Load(string repoRoot, string sourceFolder)
    {
        var dir = Path.Combine(repoRoot, sourceFolder, ".preprocessed");
        if (!Directory.Exists(dir)) return Empty;

        var names = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var copybooks = new List<string>();

        foreach (var path in Directory.EnumerateFiles(dir, "*.cpy", SearchOption.AllDirectories))
        {
            string text;
            try { text = File.ReadAllText(path); }
            catch (IOException) { continue; }

            if (!text.Contains(Marker, StringComparison.Ordinal)) continue;

            copybooks.Add(Path.GetFileNameWithoutExtension(path));
            foreach (Match m in DataName.Matches(StripComments(text)))
            {
                names.Add(m.Groups[1].Value);
            }
        }

        return names.Count == 0 ? Empty : new StubCopybookCatalog(names, copybooks);
    }

    private static string StripComments(string text) => string.Join(
        '\n',
        text.Split('\n').Where(line =>
        {
            var t = line.TrimStart();
            return !t.StartsWith("*>", StringComparison.Ordinal)
                && !(line.Length > 6 && (line[6] == '*' || line[6] == '/'));
        }));
}
