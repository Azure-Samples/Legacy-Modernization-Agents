using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class RektContextLoaderTests : IDisposable
{
    private readonly string _root =
        Path.Combine(Path.GetTempPath(), $"rekt-loader-{Guid.NewGuid():N}");

    [Fact]
    public void HasAnyRektOutput_UsesCustomRektDirectory()
    {
        var customRektDir = Path.Combine(_root, "custom-rekt");
        Directory.CreateDirectory(customRektDir);
        File.WriteAllText(Path.Combine(customRektDir, "flow-ast-TEST.json"), "{}");

        var loader = new RektContextLoader(_root, customRektDir);

        loader.HasAnyRektOutput().Should().BeTrue();
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
