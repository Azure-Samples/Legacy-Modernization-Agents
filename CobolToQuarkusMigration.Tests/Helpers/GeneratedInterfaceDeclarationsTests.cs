using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class GeneratedInterfaceDeclarationsTests
{
    [Fact]
    public void RemovesEveryDeclarationOfAGeneratedNameWithItsDocsAndKeepsTheRest()
    {
        const string code = """
            namespace Modernized.Bd
            {
                /// <summary>Guessed shape.</summary>
                public interface IBdsda2fService
                {
                    // one } in a comment
                    Task ExecuteAsync(Bdsdatoi area, CancellationToken cancellationToken = default);
                }

                public interface IBdsda2fServiceFactory { string Name => "}"; }

                public sealed class Caller(IBdsda2fService service) { }
            }
            """;

        var result = GeneratedInterfaceDeclarations.RemoveFrom(code, ["IBdsda2fService"]);

        result.Should().Be("""
            namespace Modernized.Bd
            {

                public interface IBdsda2fServiceFactory { string Name => "}"; }

                public sealed class Caller(IBdsda2fService service) { }
            }
            """);
    }

    [Fact]
    public void ScaffoldWritesTheInterfacesFromTheSourceAndRemovesModelCopies()
    {
        var source = Directory.CreateTempSubdirectory("gi-src").FullName;
        var run = Directory.CreateTempSubdirectory("gi-run").FullName;
        try
        {
            File.WriteAllText(Path.Combine(source, "PARMC.cpy"), "       01  PARMC.\n           05 P PIC X.\n");
            File.WriteAllText(Path.Combine(source, "A.cbl"), "       PROCEDURE DIVISION.\n           CALL 'EXT' USING PARMC.\n");
            File.WriteAllText(Path.Combine(run, "A.cs"),
                "namespace Modernized.Bd;\npublic interface IExtService\n{\n    Task ExecuteAsync(string wrong);\n}\npublic class A { }\n");

            GeneratedProjectScaffold.Write(run, "Modernized",
                callTargets: CallTargetRegistry.Build(source), sharedNamespace: "Modernized.Shared");

            File.ReadAllText(Path.Combine(run, "A.cs")).Should().NotContain("IExtService").And.Contain("public class A");
            File.ReadAllText(Path.Combine(run, GeneratedProjectScaffold.CallTargetContractsFile))
                .Should().Contain("namespace Modernized.Shared")
                .And.Contain("public interface IExtService")
                .And.Contain("Task ExecuteAsync(Parmc parmc, CancellationToken cancellationToken = default);");
        }
        finally
        {
            Directory.Delete(source, true);
            Directory.Delete(run, true);
        }
    }
}
