using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// The shared-copybook rule told every file "do NOT define these types, reference them from the
// shared namespace" and told no file to define them. Each still needed the type to compile, so
// each defined it: seven files declared Bdsdatoi. The instruction was not disobeyed, it was
// unsatisfiable. A copybook is converted in its own right, so an owner already exists and only
// has to be named.
public sealed class CopybookOwnershipRegistryTests : IDisposable
{
    private readonly string _root = Path.Join(
        Path.GetTempPath(), "copybook-owner-" + Guid.NewGuid().ToString("N"));

    public CopybookOwnershipRegistryTests() => Directory.CreateDirectory(_root);

    public void Dispose()
    {
        try { Directory.Delete(_root, recursive: true); }
        catch (IOException ex) { Console.Error.WriteLine($"Could not remove {_root}: {ex.Message}"); }
    }

    private void Program(string name, params string[] copies) =>
        File.WriteAllText(Path.Join(_root, name + ".cbl"),
            "       IDENTIFICATION DIVISION.\n"
            + string.Join("\n", copies.Select(c => $"       COPY {c}.")));

    private void Copybook(string name, params string[] copies) =>
        File.WriteAllText(Path.Join(_root, name + ".cpy"),
            "       01 REC.\n" + string.Join("\n", copies.Select(c => $"       COPY {c}.")));

    private CopybookOwnershipRegistry Build() => CopybookOwnershipRegistry.Build(_root);

    [Fact]
    public void ACopybookOwnsTheTypeBuiltFromIt()
    {
        Program("ORDER", "CUSTREC");
        Copybook("CUSTREC");

        var ownership = Build().Ownerships.Should().ContainSingle().Subject;

        ownership.Copybook.Should().Be("CUSTREC");
        ownership.TypeName.Should().Be("Custrec");
        ownership.OwnedBy.Should().Be("CUSTREC");
        ownership.UsedBy.Should().BeEquivalentTo("ORDER");
    }

    // The exact failure measured on the estate: BDSDATOI was declared by its own converted file
    // and by eight others that copy it.
    [Fact]
    public void EveryUserIsToldToReferenceAndOnlyTheOwnerToDeclare()
    {
        Copybook("BDSDATOI");
        foreach (var user in new[] { "BDSDA11", "BDSDA12", "BDSDA23" })
            Program(user, "BDSDATOI");

        var registry = Build();

        registry.ToPromptBlock("BDSDATOI", "C#").Should().Contain("You are the only file that declares it");

        foreach (var user in new[] { "BDSDA11", "BDSDA12", "BDSDA23" })
        {
            var block = registry.ToPromptBlock(user, "C#");
            block.Should().Contain("do NOT declare them");
            block.Should().Contain("declared by the conversion of BDSDATOI");
            block.Should().NotContain("You are the only file that declares it");
        }
    }

    // A copybook copied by another copybook produces a file that would declare the type again.
    [Fact]
    public void ACopybookCopyingAnotherIsAlsoToldToReference()
    {
        Copybook("INNER");
        Copybook("OUTER", "INNER");

        var block = Build().ToPromptBlock("OUTER", "C#");

        block.Should().Contain("Inner");
        block.Should().Contain("do NOT declare them");
    }

    // Naming an arbitrary user as owner would put a layout nobody can see into a type everybody
    // depends on. An absent copybook has no owner to name.
    [Fact]
    public void AMissingCopybookGetsNoOwner()
    {
        Program("ORDER", "ABSENT");

        Build().Ownerships.Should().BeEmpty();
    }

    [Fact]
    public void AFileInvolvedWithNoCopybooksGetsNoBlock()
    {
        Program("LONE");
        Copybook("UNUSED");

        Build().ToPromptBlock("LONE", "C#").Should().BeEmpty();
    }

    [Fact]
    public void ACommentedOutCopyDoesNotCreateAUser()
    {
        File.WriteAllText(Path.Join(_root, "QUIET.cbl"),
            "       IDENTIFICATION DIVISION.\n      * COPY GHOST.\n");
        Copybook("GHOST");

        Build().Ownerships.Should().BeEmpty();
    }

    [Fact]
    public void ACopybookIsNotItsOwnUser()
    {
        Copybook("SELFISH", "SELFISH");

        Build().Ownerships.Should().BeEmpty();
    }

    [Fact]
    public void TheOwnerIsToldTheSharedNamespace()
    {
        Program("ORDER", "CUSTREC");
        Copybook("CUSTREC");

        Build().ToPromptBlock("CUSTREC", "C#")
            .Should().Contain(ConversionNamespacePolicy.ForSharedTypes("C#"));
    }

    [Fact]
    public void ExactlyOneFileIsToldItOwnsEachType()
    {
        Copybook("SHARED");
        foreach (var user in new[] { "A", "B", "C" }) Program(user, "SHARED");

        var registry = Build();
        var owners = new[] { "SHARED", "A", "B", "C" }
            .Where(f => registry.ToPromptBlock(f, "C#").Contains("You are the only file that declares it"))
            .ToList();

        owners.Should().ContainSingle().Which.Should().Be("SHARED");
    }

    [Theory]
    [InlineData("BDSDATOI", "Bdsdatoi")]
    [InlineData("CUSTOMER-DATA", "CustomerData")]
    [InlineData("ERR_CODES", "ErrCodes")]
    public void TheTypeNameIsDerivedNotInvented(string copybook, string expected)
    {
        Program("ORDER", copybook);
        Copybook(copybook);

        Build().Ownerships.Single().TypeName.Should().Be(expected);
    }
}
