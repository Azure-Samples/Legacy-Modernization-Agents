using System.Text;
using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class CanonicalHasherTests
{
    [Fact]
    public void HashUtf8_MatchesKnownSha256Vector()
    {
        CanonicalHasher.HashUtf8("abc")
            .Should().Be("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    }

    [Fact]
    public void HashBytes_AgreesWithHashUtf8()
    {
        CanonicalHasher.HashBytes(Encoding.UTF8.GetBytes("héllo"))
            .Should().Be(CanonicalHasher.HashUtf8("héllo"));
    }

    [Fact]
    public void HashFields_ShiftedFieldBoundaries_ProduceDifferentHashes()
    {
        CanonicalHasher.HashFields("ab", "c").Should().NotBe(CanonicalHasher.HashFields("a", "bc"));
    }

    [Fact]
    public void HashFields_NullAndEmptyAreDistinct()
    {
        CanonicalHasher.HashFields("a", null).Should().NotBe(CanonicalHasher.HashFields("a", ""));
        CanonicalHasher.HashFields(null, "a").Should().NotBe(CanonicalHasher.HashFields("", "a"));
    }

    [Fact]
    public void HashFields_SameInputs_AreStable()
    {
        CanonicalHasher.HashFields("x", "y").Should().Be(CanonicalHasher.HashFields("x", "y"));
    }
}
