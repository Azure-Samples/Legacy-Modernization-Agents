using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using System.Text;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Hashes key caches and run identities; a collision between field layouts would serve a stale entry as a hit.
public class CanonicalHasherTests
{
    [Fact]
    public void HashUtf8_MatchesKnownSha256Vector()
    {
        CanonicalHasher.HashUtf8("abc")
            .Should().Be("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    }

    [Fact]
    public void HashBytes_EqualsHashUtf8ForSameContent()
    {
        CanonicalHasher.HashBytes(Encoding.UTF8.GetBytes("héllo"))
            .Should().Be(CanonicalHasher.HashUtf8("héllo"));
    }

    [Fact]
    public void HashFields_ShiftedFieldBoundariesProduceDifferentHashes()
    {
        CanonicalHasher.HashFields("ab", "c").Should().NotBe(CanonicalHasher.HashFields("a", "bc"));
    }

    [Fact]
    public void HashFields_DistinguishesNullFromEmptyString()
    {
        CanonicalHasher.HashFields("x", null).Should().NotBe(CanonicalHasher.HashFields("x", ""));
        CanonicalHasher.HashFields(new string?[] { null }).Should().NotBe(CanonicalHasher.HashFields(""));
    }

    [Fact]
    public void HashFields_IsDeterministicAndOrderSensitive()
    {
        CanonicalHasher.HashFields("a", "b").Should().Be(CanonicalHasher.HashFields("a", "b"));
        CanonicalHasher.HashFields("a", "b").Should().NotBe(CanonicalHasher.HashFields("b", "a"));
    }
}
