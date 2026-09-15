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

    [Fact]
    public void Load_DoesNotAnswerANestedProgramFromTheFlatLayout()
    {
        // Only a flat artifact exists, named by basename alone, so nothing in it says whether it
        // describes the billing or the claims copy. Answering either request from it gives both
        // programs the same paragraphs, PERFORM edges and CALL targets.
        var rektDir = Path.Join(_root, "rekt");
        Directory.CreateDirectory(rektDir);
        File.WriteAllText(
            Path.Join(rektDir, "flow-ast-CUSTOMER.json"),
            """
            {
              "type": "PROCEDURE_DIVISION_BODY", "name": "body", "children": [
                { "type": "PARAGRAPH", "name": "BILLING-ONLY-PARAGRAPH", "children": [] }
              ]
            }
            """);

        var loader = new RektContextLoader(_root, rektDir);

        var nested = loader.Load("billing/CUSTOMER.cbl", "source");
        nested.Sections.SelectMany(s => s.Paragraphs).Should().BeEmpty(
            "a flat artifact cannot be attributed to one of several programs sharing its basename");

        // A caller naming only the basename expresses no preference, so the flat layout still
        // answers it and estates that were never nested keep working.
        var flat = loader.Load("CUSTOMER.cbl", "source");
        flat.Sections.SelectMany(s => s.Paragraphs).Select(p => p.Name)
            .Should().Contain("BILLING-ONLY-PARAGRAPH");
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
