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

    [Fact]
    public void ToPromptBlock_RequiresNestedRatherThanMissingSharedType()
    {
        Directory.CreateDirectory(_root);
        File.WriteAllText(Path.Combine(_root, "FIRST.cbl"), "       COPY CUSTOMER-DATA.");
        File.WriteAllText(Path.Combine(_root, "SECOND.cbl"), "       COPY CUSTOMER-DATA.");

        var registry = new SharedTypeRegistry();
        registry.Scan(_root);

        var prompt = registry.ToPromptBlock("C#");

        prompt.Should().Contain("define");
        prompt.Should().Contain("as a nested type");
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
