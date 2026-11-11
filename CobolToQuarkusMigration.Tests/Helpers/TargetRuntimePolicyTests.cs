using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class TargetRuntimePolicyTests : IDisposable
{
    private readonly string _root = Path.GetFullPath(Path.Combine(".test-artifacts", $"runtime-policy-{Guid.NewGuid():N}"));

    public TargetRuntimePolicyTests() => Directory.CreateDirectory(_root);

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }

    [Fact]
    public void NoPolicyIsNamedSoStrictConversionStillBlocks()
    {
        TargetRuntimePolicy.Block(null, _root).Should().BeEmpty();
        TargetRuntimePolicy.Block(" ", _root).Should().BeEmpty();
    }

    [Fact]
    public void ANamedPolicyIsInjectedAsADeploymentChoiceNotEvidence()
    {
        File.WriteAllText(Path.Combine(_root, "policy.md"), "LINK maps to an in-process call.");

        var block = TargetRuntimePolicy.Block("policy.md", _root);

        block.Should().Contain("LINK maps to an in-process call.")
            .And.Contain("NOT evidence of original mainframe behavior");
    }

    [Fact]
    public void AMissingPolicyFileStopsTheRun()
    {
        var load = () => TargetRuntimePolicy.Block("absent.md", _root);

        load.Should().Throw<FileNotFoundException>();
    }
}
