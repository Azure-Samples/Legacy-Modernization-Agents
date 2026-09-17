using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Giving a service's programs one namespace is what makes them a service, but it removes the
// accident that hid this: with a package per program, two programs could each invent a type of the
// same name and nothing would object. Sharing a namespace turns that into a build error, so the
// disagreement has to be found rather than discovered at compile time.
public sealed class GeneratedTypeCollisionsTests : IDisposable
{
    // Join rather than Combine: every segment here is a relative name this test builds, so the
    // argument-dropping Combine performs for an absolute second segment would only hide a mistake.
    private readonly string _root = Path.Join(
        Path.GetTempPath(), "type-collisions-" + Guid.NewGuid().ToString("N"));

    public GeneratedTypeCollisionsTests() => Directory.CreateDirectory(_root);

    public void Dispose()
    {
        // A temp directory the operating system still holds open is not a test failure, but
        // swallowing the reason silently means a leak is never noticed either.
        try
        {
            Directory.Delete(_root, recursive: true);
        }
        catch (IOException ex)
        {
            Console.Error.WriteLine($"Could not remove {_root}: {ex.Message}");
        }
    }

    private void Write(string name, string body) =>
        File.WriteAllText(Path.Join(_root, name), body);

    [Fact]
    public void AnEstateWithNoDuplicatesReportsNothing()
    {
        Write("A.cs", "namespace X;\npublic class Alpha { }\n");
        Write("B.cs", "namespace X;\npublic class Beta { }\n");

        GeneratedTypeCollisions.Find(_root).Should().BeEmpty();
    }

    // Each caller invents its own interface for the same callee, with its own method shape.
    [Fact]
    public void TheSameTypeDeclaredByTwoFilesInOneNamespaceIsReported()
    {
        Write("Caller1.cs", "namespace X;\npublic interface IPricing { Task RunAsync(); }\n");
        Write("Caller2.cs", "namespace X;\npublic interface IPricing { Task ComputeAsync(); }\n");

        var collisions = GeneratedTypeCollisions.Find(_root);

        var collision = collisions.Should().ContainSingle().Subject;
        collision.Namespace.Should().Be("X");
        collision.TypeName.Should().Be("IPricing");
        collision.Files.Should().HaveCount(2);
    }

    // This is what the old package-per-program layout did, and why the duplication went unnoticed.
    [Fact]
    public void TheSameTypeInDifferentNamespacesDoesNotCollide()
    {
        Write("A.cs", "namespace X.One;\npublic class Shared { }\n");
        Write("B.cs", "namespace X.Two;\npublic class Shared { }\n");

        GeneratedTypeCollisions.Find(_root).Should().BeEmpty();
    }

    [Theory]
    [InlineData("class")]
    [InlineData("record")]
    [InlineData("struct")]
    [InlineData("interface")]
    [InlineData("enum")]
    public void EveryTopLevelDeclarationKindIsCounted(string kind)
    {
        Write("A.cs", $"namespace X;\npublic {kind} Thing {{ }}\n");
        Write("B.cs", $"namespace X;\npublic {kind} Thing {{ }}\n");

        GeneratedTypeCollisions.Find(_root).Should().ContainSingle();
    }

    // A nested type is scoped by the type containing it, so two files may both have one.
    [Fact]
    public void ANestedTypeIsNotAToplevelDeclaration()
    {
        Write("A.cs", "namespace X;\npublic class Alpha\n{\n    public class Inner { }\n}\n");
        Write("B.cs", "namespace X;\npublic class Beta\n{\n    public class Inner { }\n}\n");

        GeneratedTypeCollisions.Find(_root).Should().BeEmpty();
    }

    [Fact]
    public void ModifiersDoNotHideADeclaration()
    {
        Write("A.cs", "namespace X;\npublic sealed partial class Thing { }\n");
        Write("B.cs", "namespace X;\ninternal static class Thing { }\n");

        GeneratedTypeCollisions.Find(_root).Should().ContainSingle();
    }

    [Fact]
    public void FilesAreSearchedRecursively()
    {
        Directory.CreateDirectory(Path.Join(_root, "sub"));
        Write("A.cs", "namespace X;\npublic class Thing { }\n");
        File.WriteAllText(Path.Join(_root, "sub", "B.cs"), "namespace X;\npublic class Thing { }\n");

        GeneratedTypeCollisions.Find(_root).Should().ContainSingle();
    }

    [Fact]
    public void TheWorstOffendersAreReportedFirst()
    {
        for (var i = 0; i < 3; i++)
            Write($"Many{i}.cs", "namespace X;\npublic interface IBusy { }\n");
        Write("Pair1.cs", "namespace X;\npublic class Quiet { }\n");
        Write("Pair2.cs", "namespace X;\npublic class Quiet { }\n");

        var collisions = GeneratedTypeCollisions.Find(_root);

        collisions.Should().HaveCount(2);
        collisions[0].TypeName.Should().Be("IBusy");
        collisions[0].Files.Should().HaveCount(3);
    }

    [Fact]
    public void ADirectoryThatDoesNotExistIsNotAnError()
    {
        GeneratedTypeCollisions.Find(Path.Join(_root, "absent")).Should().BeEmpty();
    }

    [Fact]
    public void TheDescriptionNamesTheTypeAndTheFilesThatDeclareIt()
    {
        Write("A.cs", "namespace X;\npublic interface IPricing { }\n");
        Write("B.cs", "namespace X;\npublic interface IPricing { }\n");

        var text = GeneratedTypeCollisions.Describe(GeneratedTypeCollisions.Find(_root), _root);

        text.Should().Contain("X.IPricing");
        text.Should().Contain("A.cs").And.Contain("B.cs");
        text.Should().Contain("will not compile");
    }

    [Fact]
    public void NothingToReportProducesNoReport()
    {
        GeneratedTypeCollisions.Describe(Array.Empty<TypeCollision>(), _root).Should().BeEmpty();
    }

    // Both namespace styles must give the same answer: a file-scoped namespace puts its top-level
    // types at the indentation a block-scoped one uses for nested ones.
    [Fact]
    public void ABlockScopedNamespaceIsUnderstoodToo()
    {
        Write("A.cs", "namespace X\n{\n    public class Thing { }\n}\n");
        Write("B.cs", "namespace X\n{\n    public class Thing { }\n}\n");

        GeneratedTypeCollisions.Find(_root).Should().ContainSingle();
    }

    [Fact]
    public void ANestedTypeInABlockScopedNamespaceIsStillNested()
    {
        Write("A.cs", "namespace X\n{\n    public class Alpha\n    {\n        public class Inner { }\n    }\n}\n");
        Write("B.cs", "namespace X\n{\n    public class Beta\n    {\n        public class Inner { }\n    }\n}\n");

        GeneratedTypeCollisions.Find(_root).Should().BeEmpty();
    }
}
