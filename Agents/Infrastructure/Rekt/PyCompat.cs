using System.Globalization;
using System.Security.Cryptography;
using System.Text;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

// Small pieces of Python semantics the REKT records depend on (paths, line splitting, timestamps).
public static class PyCompat
{
    public static readonly Encoding Latin1 = Encoding.Latin1;
    public static readonly Encoding Utf8 = new UTF8Encoding(false, false);
    public static readonly Encoding StrictUtf8 = new UTF8Encoding(false, true);

    public static string Digest(string path) => Convert.ToHexString(SHA256.HashData(ReadBytes(path))).ToLowerInvariant();

    // Path.read_bytes(), raising the OSError text Python would embed in a diagnostic.
    public static byte[] ReadBytes(string path)
    {
        if (Directory.Exists(path)) throw new PyError("IsADirectoryError", $"[Errno 21] Is a directory: {PyJson.ReprString(path)}");
        if (!File.Exists(path)) throw PyError.NotFound(path);
        return File.ReadAllBytes(path);
    }

    public static string Digest(byte[] bytes) => Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant();

    // datetime.now(timezone.utc).isoformat(): microseconds are omitted when zero.
    public static string UtcNowIso()
    {
        var now = DateTime.UtcNow;
        var micro = (int)(now.Ticks / 10 % 1_000_000);
        return now.ToString("yyyy-MM-dd'T'HH:mm:ss", CultureInfo.InvariantCulture)
            + (micro == 0 ? "" : "." + micro.ToString("000000", CultureInfo.InvariantCulture)) + "+00:00";
    }

    // Path.resolve(): absolute, with every existing symlink followed (macOS /tmp is /private/tmp).
    public static string Resolve(string path)
    {
        var full = Path.GetFullPath(path);
        var root = Path.GetPathRoot(full)!;
        var current = root;
        var parts = full[root.Length..].Split(Path.DirectorySeparatorChar, StringSplitOptions.RemoveEmptyEntries);
        var hops = 0;
        foreach (var part in parts)
        {
            if (part == ".") continue;
            if (part == "..")
            {
                current = Path.GetDirectoryName(current) ?? root;
                continue;
            }
            var next = Path.Combine(current, part);
            FileSystemInfo info = new FileInfo(next);
            if (!info.Exists) info = new DirectoryInfo(next);
            if (info.Exists && info.LinkTarget is not null && hops++ < 64)
            {
                var target = info.ResolveLinkTarget(returnFinalTarget: true);
                next = target is null ? next : Resolve(target.FullName);
            }
            current = next;
        }
        return current;
    }

    // str.splitlines() boundaries.
    public static List<string> SplitLines(string text, bool keepEnds = false)
    {
        var lines = new List<string>();
        var start = 0;
        var index = 0;
        while (index < text.Length)
        {
            var character = text[index];
            var width = 0;
            if (character == '\r') width = index + 1 < text.Length && text[index + 1] == '\n' ? 2 : 1;
            else if (character is '\n' or '\v' or '\f' or '\x1c' or '\x1d' or '\x1e' or '\x85' or '\u2028' or '\u2029') width = 1;
            if (width == 0) { index++; continue; }
            lines.Add(text[start..(keepEnds ? index + width : index)]);
            index += width;
            start = index;
        }
        if (start < text.Length) lines.Add(text[start..]);
        return lines;
    }

    // Python os.path.relpath with "/" separators as the tools print them.
    public static string RelPosix(string path, string start) =>
        Path.GetRelativePath(Path.GetFullPath(start), Path.GetFullPath(path)).Replace(Path.DirectorySeparatorChar, '/');

    // str.isspace(): unlike char.IsWhiteSpace it includes the information separators \x1c-\x1f.
    public static bool IsSpace(char character) => char.IsWhiteSpace(character) || character is >= '\x1c' and <= '\x1f';

    public static string Strip(string text) => RStrip(LStrip(text));

    public static string LStrip(string text)
    {
        var start = 0;
        while (start < text.Length && IsSpace(text[start])) start++;
        return text[start..];
    }

    public static string RStrip(string text)
    {
        var end = text.Length;
        while (end > 0 && IsSpace(text[end - 1])) end--;
        return text[..end];
    }

    public static string ToOs(string relative) => relative.Replace('/', Path.DirectorySeparatorChar);

    // Python's str.isdigit/isalnum accept non-ASCII digits and letters too.
    public static bool IsDigit(char character) => char.IsDigit(character) || character is '\u00b9' or '\u00b2' or '\u00b3';

    public static bool IsDigits(string text) => text.Length > 0 && text.All(IsDigit);

    public static bool IsAlnum(string text) => text.Length > 0 && text.All(character => char.IsLetterOrDigit(character) || IsDigit(character));

    // Path.stem / Path.suffix: a leading dot is part of the name, not a suffix.
    public static string Stem(string path)
    {
        var name = Path.GetFileName(path.TrimEnd('/', Path.DirectorySeparatorChar));
        var dot = name.LastIndexOf('.');
        return dot > 0 && dot < name.Length - 1 ? name[..dot] : name;
    }

    public static string Suffix(string path)
    {
        var name = Path.GetFileName(path.TrimEnd('/', Path.DirectorySeparatorChar));
        var dot = name.LastIndexOf('.');
        return dot > 0 && dot < name.Length - 1 ? name[dot..] : "";
    }

    // Path.read_text(): universal newlines turn CRLF and lone CR into LF.
    // Python keeps a BOM as text, and its utf-8 codec is strict.
    public static string ReadText(string path, Encoding? encoding = null) =>
        Decode(ReadBytes(path), encoding).Replace("\r\n", "\n").Replace('\r', '\n');

    public static string Decode(byte[] bytes, Encoding? encoding = null)
    {
        if (encoding is not null && encoding != StrictUtf8) return encoding.GetString(bytes);
        ValidateUtf8(bytes);
        return StrictUtf8.GetString(bytes);
    }

    // CPython's UnicodeDecodeError text, which ends up in recorded diagnostics.
    private static void ValidateUtf8(byte[] bytes)
    {
        var index = 0;
        while (index < bytes.Length)
        {
            var lead = bytes[index];
            if (lead < 0x80) { index++; continue; }
            var (need, low, high) = lead switch
            {
                >= 0xC2 and <= 0xDF => (1, 0x80, 0xBF),
                0xE0 => (2, 0xA0, 0xBF),
                0xED => (2, 0x80, 0x9F),
                >= 0xE1 and <= 0xEF => (2, 0x80, 0xBF),
                0xF0 => (3, 0x90, 0xBF),
                >= 0xF1 and <= 0xF3 => (3, 0x80, 0xBF),
                0xF4 => (3, 0x80, 0x8F),
                _ => (0, 0, 0),
            };
            if (need == 0) throw Utf8Error(bytes, index, index + 1, "invalid start byte");
            for (var offset = 1; offset <= need; offset++)
            {
                if (index + offset >= bytes.Length) throw Utf8Error(bytes, index, bytes.Length, "unexpected end of data");
                var next = bytes[index + offset];
                var (min, max) = offset == 1 ? (low, high) : (0x80, 0xBF);
                if (next < min || next > max) throw Utf8Error(bytes, index, index + offset, "invalid continuation byte");
            }
            index += need + 1;
        }
    }

    private static PyError Utf8Error(byte[] bytes, int start, int end, string reason) => new("UnicodeDecodeError",
        end - start == 1
            ? $"'utf-8' codec can't decode byte 0x{bytes[start]:x2} in position {start}: {reason}"
            : $"'utf-8' codec can't decode bytes in position {start}-{end - 1}: {reason}");

    // Python slicing never throws on out-of-range bounds.
    public static string Slice(string text, int start, int? end = null)
    {
        var stop = Math.Min(end ?? text.Length, text.Length);
        start = Math.Min(start, text.Length);
        return stop > start ? text[start..stop] : "";
    }

    // str.expandtabs() with the default tab size of 8.
    public static string ExpandTabs(string text, int size = 8)
    {
        if (!text.Contains('\t')) return text;
        var builder = new StringBuilder();
        var column = 0;
        foreach (var character in text)
        {
            if (character == '\t')
            {
                var spaces = size - column % size;
                builder.Append(' ', spaces);
                column += spaces;
            }
            else
            {
                builder.Append(character);
                column = character is '\n' or '\r' ? 0 : column + 1;
            }
        }
        return builder.ToString();
    }

    // shutil.move into .previous/<UTC stamp>-<uuid>/<relative> so a regenerated record never loses history.
    public static void Preserve(string path, string archiveRoot, string relativeTo)
    {
        var stamp = DateTime.UtcNow.ToString("yyyyMMdd'T'HHmmss", CultureInfo.InvariantCulture) + "-" + Guid.NewGuid().ToString("N");
        var destination = Path.Combine(archiveRoot, ".previous", stamp, Path.GetRelativePath(relativeTo, path));
        Directory.CreateDirectory(Path.GetDirectoryName(destination)!);
        if (Directory.Exists(path)) Directory.Move(path, destination);
        else File.Move(path, destination);
    }

    // os.walk(top) without following directory symlinks; keepDir prunes like assigning dirs[:].
    public static IEnumerable<(string Root, List<string> Files)> Walk(string top, Func<string, bool> keepDir, bool sorted = false)
    {
        if (!Directory.Exists(top)) yield break;
        var pending = new Stack<string>();
        pending.Push(top);
        while (pending.Count > 0)
        {
            var root = pending.Pop();
            var dirs = new List<string>();
            var files = new List<string>();
            try
            {
                foreach (var entry in new DirectoryInfo(root).EnumerateFileSystemInfos())
                {
                    if (Directory.Exists(entry.FullName)) dirs.Add(entry.Name);
                    else files.Add(entry.Name);
                }
            }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException)
            {
                continue;
            }
            yield return (root, files);
            var kept = dirs.Where(keepDir).ToList();
            if (sorted) kept.Sort(StringComparer.Ordinal);
            for (var index = kept.Count - 1; index >= 0; index--)
            {
                var child = Path.Join(root, kept[index]);
                if (new DirectoryInfo(child).LinkTarget is null) pending.Push(child);
            }
        }
    }

    public static string NewHex() => Guid.NewGuid().ToString("N");

    // Write then rename so readers never observe a half-written file.
    public static void AtomicWrite(string path, string content, string pendingSuffix = ".tmp")
    {
        var pending = path + pendingSuffix;
        File.WriteAllText(pending, content, Utf8);
        File.Move(pending, path, overwrite: true);
    }
}

// A Python exception whose str() is embedded verbatim in diagnostics.
public sealed class PyError(string kind, string message) : Exception(message)
{
    public string Kind { get; } = kind;

    public static PyError Key(string key) => new("KeyError", "'" + key + "'");

    public static PyError Value(string message) => new("ValueError", message);

    public static PyError NotFound(string path) => new("FileNotFoundError", $"[Errno 2] No such file or directory: {PyJson.ReprString(path)}");
}
