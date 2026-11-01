using System.Text;
using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Hashes key cache entries, so two different inputs sharing a hash would serve a stale result.
public class CanonicalHasherTests
{
    [Fact]
    public void HashUtf8_MatchesKnownSha256Vector()
    {
        CanonicalHasher.HashUtf8("abc")
            .Should().Be("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    }

    [Fact]
    public void HashBytes_AgreesWithHashUtf8ForSameContent()
    {
        const string content = "MOVE ÄÖ TO X.";

        CanonicalHasher.HashBytes(Encoding.UTF8.GetBytes(content))
            .Should().Be(CanonicalHasher.HashUtf8(content));
    }

    [Fact]
    public void HashFields_ShiftingCharactersAcrossFieldBoundaryChangesHash()
    {
        CanonicalHasher.HashFields("ab", "c")
            .Should().NotBe(CanonicalHasher.HashFields("a", "bc"));
    }

    [Fact]
    public void HashFields_DistinguishesNullFromEmptyString()
    {
        CanonicalHasher.HashFields("a", null)
            .Should().NotBe(CanonicalHasher.HashFields("a", ""));
    }

    [Fact]
    public void HashFields_IsStableForIdenticalInput()
    {
        CanonicalHasher.HashFields("PROG1", "v2", null)
            .Should().Be(CanonicalHasher.HashFields("PROG1", "v2", null));
    }

    [Fact]
    public void HashFields_DiffersWhenFieldOrderChanges()
    {
        CanonicalHasher.HashFields("x", "y")
            .Should().NotBe(CanonicalHasher.HashFields("y", "x"));
    }
}
