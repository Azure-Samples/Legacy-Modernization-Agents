using System.Security.Cryptography;
using System.Text;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Deterministic SHA-256 hashing utility. All cache key construction goes through
/// here so the canonical form is one well-tested code path.
/// </summary>
/// <remarks>
/// "Canonical" rules:
/// <list type="bullet">
///   <item>Field separator is the ASCII unit separator (0x1F) — never appears in normal text.</item>
///   <item>Null values become the literal token <c>"\u0001NULL\u0001"</c>; empty strings remain empty.</item>
///   <item>Numeric values use <see cref="System.Globalization.CultureInfo.InvariantCulture"/>.</item>
///   <item>Output is lower-case hex.</item>
/// </list>
/// Keep the encoder simple: collisions are mathematically negligible at SHA-256, and
/// the goal is determinism + diagnosability, not compact representation.
/// </remarks>
public static class CanonicalHasher
{
    private const char FieldSeparator = '\u001F';
    private const string NullToken = "\u0001NULL\u0001";

    /// <summary>
    /// Produces a hex SHA-256 over the supplied fields joined by a field separator.
    /// Null fields become a distinct token (so <c>"a", null, "b"</c> differs from <c>"a", "", "b"</c>).
    /// </summary>
    public static string HashFields(params string?[] fields)
    {
        var sb = new StringBuilder();
        for (int i = 0; i < fields.Length; i++)
        {
            if (i > 0) sb.Append(FieldSeparator);
            sb.Append(fields[i] is null ? NullToken : fields[i]);
        }
        return HashUtf8(sb.ToString());
    }

    /// <summary>Hashes a single UTF-8 string. Convenience for arbitrary content (prompts, REKT context).</summary>
    public static string HashUtf8(string content)
    {
        var bytes = Encoding.UTF8.GetBytes(content);
        var hash = SHA256.HashData(bytes);
        return Convert.ToHexString(hash).ToLowerInvariant();
    }

    /// <summary>Hashes raw bytes. Used for file content (preprocessed source bytes).</summary>
    public static string HashBytes(ReadOnlySpan<byte> bytes)
    {
        var hash = SHA256.HashData(bytes);
        return Convert.ToHexString(hash).ToLowerInvariant();
    }
}
