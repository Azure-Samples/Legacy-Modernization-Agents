using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class SharedTypeRegistryTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(),
        $"shared-type-registry-{Guid.NewGuid():N}");

    [Fact]
    public void Scan_FindsNestedCblAndCobPrograms()
    {
        Directory.CreateDirectory(Path.Combine(_root, "finance"));
        Directory.CreateDirectory(Path.Combine(_root, "batch"));
        File.WriteAllText(
            Path.Combine(_root, "finance", "FIRST.cbl"),
            "       COPY CUSTOMER-DATA.");
        File.WriteAllText(
            Path.Combine(_root, "batch", "SECOND.cob"),
            "       COPY CUSTOMER-DATA.");

        var registry = new SharedTypeRegistry();
        registry.Scan(_root);

        registry.IsShared("CUSTOMER-DATA").Should().BeTrue();
        registry.IsShared("CustomerData").Should().BeTrue();
    }

    // A copybook used by several programs describes one layout. Nesting it inside each caller,
    // which is what this prompt used to ask for, produced four copies of the same record that
    // could drift apart independently. It is now declared once, where both callers can reach it.
    [Fact]
    public void ToPromptBlock_PlacesASharedTypeInTheSharedNamespaceRatherThanInsideEachCaller()
    {
        Directory.CreateDirectory(_root);
        File.WriteAllText(Path.Combine(_root, "FIRST.cbl"), "       COPY CUSTOMER-DATA.");
        File.WriteAllText(Path.Combine(_root, "SECOND.cbl"), "       COPY CUSTOMER-DATA.");

        var registry = new SharedTypeRegistry();
        registry.Scan(_root);

        var prompt = registry.ToPromptBlock("C#");

        prompt.Should().Contain(ConversionNamespacePolicy.ForSharedTypes("C#"));
        prompt.Should().Contain("CustomerData");
        prompt.Should().NotContain("as a nested type");
        prompt.Should().NotContain("already exist");
        prompt.Should().NotContain("will be generated");
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
        {
            Directory.Delete(_root, recursive: true);
        }
    }
}
