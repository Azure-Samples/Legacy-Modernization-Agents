using System.Globalization;
using System.Security.Cryptography;
using System.Text;

namespace CobolToQuarkusMigration.Persistence;

// The graph populator keys nodes by uuid5(NAMESPACE_URL, "/".join(parts)). The CLI derives the
// same uid so both writers MERGE into one node rather than creating two copies of it.
public static class GraphUid
{
    // RFC 4122 URL namespace, 6ba7b811-9dad-11d1-80b4-00c04fd430c8, in network byte order.
    private static readonly byte[] UrlNamespace =
    {
        0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x4f, 0xd4, 0x30, 0xc8,
    };

    public static string Make(params object[] parts)
    {
        var name = string.Join("/", parts.Select(p => Convert.ToString(p, CultureInfo.InvariantCulture)));
        var nameBytes = Encoding.UTF8.GetBytes(name);

        var input = new byte[UrlNamespace.Length + nameBytes.Length];
        UrlNamespace.CopyTo(input, 0);
        nameBytes.CopyTo(input, UrlNamespace.Length);

        var hash = SHA1.HashData(input);
        hash[6] = (byte)((hash[6] & 0x0F) | 0x50);
        hash[8] = (byte)((hash[8] & 0x3F) | 0x80);

        var hex = Convert.ToHexString(hash, 0, 16).ToLowerInvariant();
        return $"{hex[..8]}-{hex[8..12]}-{hex[12..16]}-{hex[16..20]}-{hex[20..32]}";
    }
}
