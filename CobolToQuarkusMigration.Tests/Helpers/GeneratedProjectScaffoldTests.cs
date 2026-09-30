using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Generated C# opens with a fixed using block — System, System.Collections.Generic, System.Linq —
// and then uses Column, Key, DbSet, ILogger and others that the block does not cover. On a real
// run of five programs and their copybooks that single gap was 1028 of 1140 compiler errors.
// Asking the model for the right usings is the approach that already failed twice on this branch,
// so these are read out of the generated code instead.
public sealed class GeneratedProjectScaffoldTests : IDisposable
{
    private readonly string _run = Path.Join(
        Path.GetTempPath(), "scaffold-" + Guid.NewGuid().ToString("N"));

    public GeneratedProjectScaffoldTests() => Directory.CreateDirectory(_run);

    public void Dispose()
    {
        try { Directory.Delete(_run, recursive: true); }
        catch (IOException ex) { Console.Error.WriteLine($"Could not remove {_run}: {ex.Message}"); }
    }

    private void Source(string name, string body)
    {
        var path = Path.Join(_run, name);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, body);
    }

    private ScaffoldResult Write() => GeneratedProjectScaffold.Write(_run, "Estate");

    private string UsingsText() => File.ReadAllText(Path.Join(_run, GeneratedProjectScaffold.GlobalUsingsFile));

    private string ProjectText() => File.ReadAllText(Path.Join(_run, "Estate.csproj"));

    [Fact]
    public void AnEntityAttributeEarnsItsNamespaceAndPackage()
    {
        Source("Order.cs", "namespace X;\npublic class Order { [Column(\"ID\")] public int Id { get; set; } }");

        var result = Write();

        result.Usings.Should().Contain("System.ComponentModel.DataAnnotations.Schema");
        result.Packages.Should().Contain("System.ComponentModel.Annotations");
        UsingsText().Should().Contain("global using System.ComponentModel.DataAnnotations.Schema;");
        ProjectText().Should().Contain("System.ComponentModel.Annotations");
    }

    [Fact]
    public void EntityFrameworkIsDetectedFromItsTypes()
    {
        Source("Ctx.cs", "namespace X;\npublic class Ctx : DbContext { public DbSet<Order> Orders { get; set; } }");

        Write().Packages.Should().Contain("Microsoft.EntityFrameworkCore");
    }

    [Fact]
    public void LoggingAndConfigurationAreDetectedSeparately()
    {
        Source("A.cs", "namespace X;\npublic class A { ILogger<A> _log; }");
        Source("B.cs", "namespace X;\npublic class B { IConfiguration _cfg; }");

        var result = Write();

        result.Packages.Should().Contain("Microsoft.Extensions.Logging.Abstractions");
        result.Packages.Should().Contain("Microsoft.Extensions.Configuration.Abstractions");
    }

    // An unused package reference is a claim about what the converted code depends on, and a
    // false one. Nothing is included speculatively.
    [Fact]
    public void CodeUsingNoneOfThemDeclaresNoDependencies()
    {
        Source("Plain.cs", "namespace X;\npublic class Plain { public int Value { get; set; } }");

        var result = Write();

        result.Usings.Should().BeEmpty();
        result.Packages.Should().BeEmpty();
        ProjectText().Should().NotContain("PackageReference");
    }

    [Fact]
    public void ADependencyIsDeclaredOnceHoweverManyFilesUseIt()
    {
        Source("A.cs", "namespace X;\npublic class A { [Key] public int Id { get; set; } }");
        Source("B.cs", "namespace X;\npublic class B { [Key] public int Id { get; set; } }");
        Source("C.cs", "namespace X;\npublic class C { [MaxLength(8)] public string S { get; set; } }");

        var result = Write();

        result.Usings.Should().ContainSingle().Which.Should().Be("System.ComponentModel.DataAnnotations");
        result.Packages.Should().ContainSingle();
    }

    [Fact]
    public void FilesAreFoundInNestedNamespaceFolders()
    {
        Source(Path.Join("Modernized", "Banking", "Bd", "Order.cs"),
            "namespace Modernized.Banking.Bd;\npublic class Order { [Table(\"T\")] public int Id { get; set; } }");

        Write().Usings.Should().Contain("System.ComponentModel.DataAnnotations.Schema");
    }

    [Fact]
    public void TheProjectDeclaresALibraryBecauseNoEntryPointIsGenerated()
    {
        Source("A.cs", "namespace X;\npublic class A { }");

        ProjectTextAfterWrite().Should().Contain("<OutputType>Library</OutputType>");
    }

    private string ProjectTextAfterWrite()
    {
        Write();
        return ProjectText();
    }

    [Fact]
    public void RewritingAfterASecondRunDoesNotAccumulate()
    {
        Source("A.cs", "namespace X;\npublic class A { [Key] public int Id { get; set; } }");
        Write();
        Write();

        var occurrences = UsingsText().Split("global using").Length - 1;
        occurrences.Should().Be(1);
    }

    // The generated usings file must not be read back in as a source of markers on a later run.
    [Fact]
    public void ItsOwnOutputIsNotTreatedAsGeneratedCode()
    {
        Source("A.cs", "namespace X;\npublic class A { [Key] public int Id { get; set; } }");
        Write();

        File.Delete(Path.Join(_run, "A.cs"));
        var second = Write();

        second.Usings.Should().BeEmpty();
    }

    [Fact]
    public void AnEmptyRunFolderProducesNothing()
    {
        var result = Write();

        result.WroteAnything.Should().BeFalse();
        File.Exists(Path.Join(_run, GeneratedProjectScaffold.GlobalUsingsFile)).Should().BeFalse();
    }

    [Fact]
    public void AMissingRunFolderIsNotAnError()
    {
        GeneratedProjectScaffold.Write(Path.Join(_run, "absent"), "Estate")
            .WroteAnything.Should().BeFalse();
    }

    [Fact]
    public void TheTargetFrameworkIsConfigurable()
    {
        Source("A.cs", "namespace X;\npublic class A { }");

        GeneratedProjectScaffold.Write(_run, "Estate", "net8.0");

        ProjectText().Should().Contain("<TargetFramework>net8.0</TargetFramework>");
    }
}
