using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// The prompt asked every caller to "generate a service interface" for each CALL and named neither
// the interface nor its method. Seven programs calling one module produced seven declarations of
// IBdsmfjlService, and three of them declared three different methods on it. While each program
// had its own package that merely duplicated; once a service shares one namespace it stops
// compiling. A COBOL program has one entry point, so the contract is knowable — and one program
// has to be responsible for declaring it.
public sealed class CallTargetRegistryTests : IDisposable
{
    private readonly string _root = Path.Join(
        Path.GetTempPath(), "call-targets-" + Guid.NewGuid().ToString("N"));

    public CallTargetRegistryTests() => Directory.CreateDirectory(_root);

    public void Dispose()
    {
        try { Directory.Delete(_root, recursive: true); }
        catch (IOException ex) { Console.Error.WriteLine($"Could not remove {_root}: {ex.Message}"); }
    }

    private void Program(string name, params string[] calls)
    {
        var body = "       IDENTIFICATION DIVISION.\n"
                   + $"       PROGRAM-ID. {name}.\n"
                   + string.Join("\n", calls.Select(c => $"           CALL '{c}' USING WS-PARM"));
        File.WriteAllText(Path.Join(_root, name + ".cbl"), body);
    }

    private CallTargetRegistry Build() => CallTargetRegistry.Build(_root);

    [Fact]
    public void AProgramThatCallsNothingHasNoContracts()
    {
        Program("SOLO");

        Build().Contracts.Should().BeEmpty();
    }

    // The callee knows its own contract, so it declares it and every caller references it.
    [Fact]
    public void TheCalledProgramDeclaresItsOwnInterface()
    {
        Program("CALLER1", "PRICING");
        Program("CALLER2", "PRICING");
        Program("PRICING");

        var contract = Build().Contracts.Should().ContainSingle().Subject;

        contract.Target.Should().Be("PRICING");
        contract.InterfaceName.Should().Be("IPricingService");
        contract.MethodName.Should().Be("ExecuteAsync");
        contract.DeclaredBy.Should().Be("PRICING");
        contract.Callers.Should().BeEquivalentTo(new[] { "CALLER1", "CALLER2" });
    }

    // An external module has no program to declare it, so the choice must still be made once and
    // made the same way for every caller.
    [Fact]
    public void AnExternalTargetIsDeclaredByOneCallerChosenStably()
    {
        Program("ZEBRA", "EXTERNAL");
        Program("ALPHA", "EXTERNAL");

        var contract = Build().Contracts.Should().ContainSingle().Subject;

        contract.DeclaredBy.Should().Be("ALPHA");
        contract.Callers.Should().BeEquivalentTo(new[] { "ALPHA", "ZEBRA" });
    }

    [Fact]
    public void TheSameEstateAlwaysProducesTheSameAssignment()
    {
        Program("ZEBRA", "EXTERNAL");
        Program("ALPHA", "EXTERNAL");
        Program("MIDDLE", "EXTERNAL");

        var first = Build().Contracts.Single().DeclaredBy;
        var second = Build().Contracts.Single().DeclaredBy;

        first.Should().Be(second);
    }

    [Fact]
    public void ExactlyOneProgramIsToldToDeclareEachInterface()
    {
        Program("A", "SHARED");
        Program("B", "SHARED");
        Program("C", "SHARED");

        var registry = Build();
        var declarers = new[] { "A", "B", "C" }
            .Where(p => registry.ToPromptBlock(p, "C#").Contains("ISharedService — one method"))
            .ToList();

        declarers.Should().HaveCount(1);
    }

    [Fact]
    public void ACallerIsToldToReferenceButNotDeclare()
    {
        Program("CALLER", "PRICING");
        Program("PRICING");

        var block = Build().ToPromptBlock("CALLER", "C#");

        block.Should().Contain("IPricingService.ExecuteAsync");
        block.Should().Contain("declared by PRICING");
        block.Should().Contain("do NOT declare");
    }

    [Fact]
    public void TheDeclaringProgramIsToldTheSharedNamespace()
    {
        Program("CALLER", "PRICING");
        Program("PRICING");

        Build().ToPromptBlock("PRICING", "C#")
            .Should().Contain(ConversionNamespacePolicy.ForSharedTypes("C#"));
    }

    [Fact]
    public void AProgramWithNoInvolvementGetsNoBlock()
    {
        Program("CALLER", "PRICING");
        Program("PRICING");
        Program("UNRELATED");

        Build().ToPromptBlock("UNRELATED", "C#").Should().BeEmpty();
    }

    // Recursion is legal in neither the COBOL nor the generated interface; a program calling
    // itself must not be handed an interface for itself.
    [Fact]
    public void AProgramCallingItselfIsNotGivenAContract()
    {
        Program("SELFISH", "SELFISH");

        Build().Contracts.Should().BeEmpty();
    }

    [Fact]
    public void ACommentedOutCallIsNotACall()
    {
        File.WriteAllText(Path.Join(_root, "QUIET.cbl"),
            "       IDENTIFICATION DIVISION.\n      * CALL 'GHOST' USING X\n");

        Build().Contracts.Should().BeEmpty();
    }

    [Theory]
    [InlineData("PRICING", "IPricingService")]
    [InlineData("BDSMFJL", "IBdsmfjlService")]
    [InlineData("PAY-RUN", "IPayRunService")]
    public void TheInterfaceNameIsDerivedFromTheTargetNotInvented(string target, string expected)
    {
        Program("CALLER", target);

        Build().Contracts.Single().InterfaceName.Should().Be(expected);
    }
}
