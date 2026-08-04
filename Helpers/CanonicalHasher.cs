using System.Security.Cryptography;
using System.Text;

namespace CobolToQuarkusMigration.Helpers;

public static class CanonicalHasher
{
    private const char FieldSeparator = '\u001F';
    private const string NullToken = "\u0001NULL\u0001";

    // The separator and null token keep field boundaries and null distinct from empty strings.
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

    public static string HashUtf8(string content)
    {
        var bytes = Encoding.UTF8.GetBytes(content);
        var hash = SHA256.HashData(bytes);
        return Convert.ToHexString(hash).ToLowerInvariant();
    }

    public static string HashBytes(ReadOnlySpan<byte> bytes)
    {
        var hash = SHA256.HashData(bytes);
        return Convert.ToHexString(hash).ToLowerInvariant();
    }
}
