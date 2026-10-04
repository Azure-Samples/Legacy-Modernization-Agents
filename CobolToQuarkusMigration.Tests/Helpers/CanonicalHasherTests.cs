using System.Text;
using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Cache keys are built from these hashes; ambiguous field joins would let a changed
// input collide with a cached entry and be served stale.
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
    public void HashFields_DistinguishesFieldBoundaries()
    {
        CanonicalHasher.HashFields("ab", "c").Should().NotBe(CanonicalHasher.HashFields("a", "bc"));
    }

    [Fact]
    public void HashFields_DistinguishesNullFromEmptyAndMissingFields()
    {
        var withNull = CanonicalHasher.HashFields("a", null);

        withNull.Should().NotBe(CanonicalHasher.HashFields("a", ""));
        withNull.Should().NotBe(CanonicalHasher.HashFields("a"));
    }

    [Fact]
    public void HashFields_IsDeterministicAndOrderSensitive()
    {
        CanonicalHasher.HashFields("x", "y").Should().Be(CanonicalHasher.HashFields("x", "y"));
        CanonicalHasher.HashFields("x", "y").Should().NotBe(CanonicalHasher.HashFields("y", "x"));
    }
}
