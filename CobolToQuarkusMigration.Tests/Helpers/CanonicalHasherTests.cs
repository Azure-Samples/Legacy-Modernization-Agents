using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class CanonicalHasherTests
{
    [Fact]
    public void Utf8HashMatchesKnownSha256Vector() =>
        CanonicalHasher.HashUtf8("abc").Should().Be("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");

    [Fact]
    public void BytesAndUtf8HashesAgreeForTheSameContent() =>
        CanonicalHasher.HashBytes("héllo"u8).Should().Be(CanonicalHasher.HashUtf8("héllo"));

    [Fact]
    public void FieldBoundariesChangeTheHash() =>
        CanonicalHasher.HashFields("ab", "c").Should().NotBe(CanonicalHasher.HashFields("a", "bc"));

    [Fact]
    public void NullFieldIsDistinctFromEmptyField()
    {
        CanonicalHasher.HashFields("a", null).Should().NotBe(CanonicalHasher.HashFields("a", ""));
        CanonicalHasher.HashFields(null, null).Should().NotBe(CanonicalHasher.HashFields("", ""));
    }

    [Fact]
    public void FieldHashIsStableForIdenticalInput() =>
        CanonicalHasher.HashFields("x", "y").Should().Be(CanonicalHasher.HashFields("x", "y"));
}
