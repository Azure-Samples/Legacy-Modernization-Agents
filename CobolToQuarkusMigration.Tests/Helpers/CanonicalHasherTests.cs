using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class CanonicalHasherTests
{
    // ── HashUtf8 (baseline) ─────────────────────────────────────────────────

    [Fact]
    public void HashUtf8_ReturnsDeterministicLowercaseHex()
    {
        var hash1 = CanonicalHasher.HashUtf8("hello");
        var hash2 = CanonicalHasher.HashUtf8("hello");

        hash1.Should().Be(hash2);
        hash1.Should().MatchRegex("^[0-9a-f]{64}$");
    }

    [Fact]
    public void HashUtf8_DifferentInputsProduceDifferentHashes()
    {
        var hash1 = CanonicalHasher.HashUtf8("hello");
        var hash2 = CanonicalHasher.HashUtf8("world");

        hash1.Should().NotBe(hash2);
    }

    // ── HashFields ──────────────────────────────────────────────────────────

    [Fact]
    public void HashFields_ReturnsDeterministicHash()
    {
        var hash1 = CanonicalHasher.HashFields("a", "b", "c");
        var hash2 = CanonicalHasher.HashFields("a", "b", "c");

        hash1.Should().Be(hash2);
    }

    [Fact]
    public void HashFields_DifferentFieldOrderProducesDifferentHash()
    {
        var hash1 = CanonicalHasher.HashFields("a", "b");
        var hash2 = CanonicalHasher.HashFields("b", "a");

        hash1.Should().NotBe(hash2);
    }

    [Fact]
    public void HashFields_NullAndEmptyStringAreDistinct()
    {
        // Pass a single null element vs a single empty-string element.
        var hashNull = CanonicalHasher.HashFields(new string?[] { null });
        var hashEmpty = CanonicalHasher.HashFields(string.Empty);

        hashNull.Should().NotBe(hashEmpty);
    }

    [Fact]
    public void HashFields_SingleFieldMatchesHashUtf8OfThatField()
    {
        // A single non-null field with no separator should equal HashUtf8 of that field.
        var hashViaFields = CanonicalHasher.HashFields("test-value");
        var hashDirect = CanonicalHasher.HashUtf8("test-value");

        hashViaFields.Should().Be(hashDirect);
    }

    [Fact]
    public void HashFields_ReturnsLowercaseHex()
    {
        var hash = CanonicalHasher.HashFields("x", "y");

        hash.Should().MatchRegex("^[0-9a-f]{64}$");
    }

    [Fact]
    public void HashFields_EmptyParamsProducesStableHash()
    {
        var hash1 = CanonicalHasher.HashFields();
        var hash2 = CanonicalHasher.HashFields();

        hash1.Should().Be(hash2);
        hash1.Should().HaveLength(64);
    }

    // ── HashBytes ───────────────────────────────────────────────────────────

    [Fact]
    public void HashBytes_ReturnsDeterministicHash()
    {
        ReadOnlySpan<byte> bytes = new byte[] { 1, 2, 3, 4 };

        var hash1 = CanonicalHasher.HashBytes(bytes);
        var hash2 = CanonicalHasher.HashBytes(bytes);

        hash1.Should().Be(hash2);
    }

    [Fact]
    public void HashBytes_DifferentBytesProduceDifferentHashes()
    {
        var hash1 = CanonicalHasher.HashBytes(new byte[] { 1, 2, 3 });
        var hash2 = CanonicalHasher.HashBytes(new byte[] { 4, 5, 6 });

        hash1.Should().NotBe(hash2);
    }

    [Fact]
    public void HashBytes_ReturnsLowercaseHex()
    {
        var hash = CanonicalHasher.HashBytes(new byte[] { 0xFF, 0xAB, 0x00 });

        hash.Should().MatchRegex("^[0-9a-f]{64}$");
    }

    [Fact]
    public void HashBytes_EmptySpanProducesStableHash()
    {
        var hash1 = CanonicalHasher.HashBytes(ReadOnlySpan<byte>.Empty);
        var hash2 = CanonicalHasher.HashBytes(ReadOnlySpan<byte>.Empty);

        hash1.Should().Be(hash2);
        hash1.Should().HaveLength(64);
    }

    [Fact]
    public void HashBytes_ConsistentWithHashUtf8ForUtf8EncodedString()
    {
        // HashBytes on UTF-8 bytes of "hello" should match HashUtf8("hello").
        var bytes = System.Text.Encoding.UTF8.GetBytes("hello");
        var hashViaBytes = CanonicalHasher.HashBytes(bytes);
        var hashViaUtf8 = CanonicalHasher.HashUtf8("hello");

        hashViaBytes.Should().Be(hashViaUtf8);
    }
}
