using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class CompileGateTests : IDisposable
{
    private static readonly CompileGateSettings Limits = new();

    private readonly string _run = Path.Join(Path.GetTempPath(), "compile-gate-" + Guid.NewGuid().ToString("N"));

    public CompileGateTests() => Directory.CreateDirectory(Path.Join(_run, "Modernized", "Bd"));

    public void Dispose()
    {
        if (Directory.Exists(_run)) Directory.Delete(_run, recursive: true);
    }

    [Fact]
    public void CompilerErrorsAreReadRelativeToTheRunEvenThroughASymlinkedPath()
    {
        File.WriteAllText(Path.Join(_run, "Modernized", "Bd", "Rgni656.cs"), "");
        // macOS reports /tmp/... as /private/tmp/...; only the suffix is reliable.
        var output =
            $"/private{_run}/Modernized/Bd/Rgni656.cs(78,59): error CS1519: Invalid token '&' in a member declaration [/x/Run.csproj]\n" +
            $"/private{_run}/Modernized/Bd/Rgni656.cs(78,59): error CS1519: Invalid token '&' in a member declaration [/x/Run.csproj]\n" +
            "/x/Run.csproj : warning NU1603: approximate match\n";

        var (errors, other) = GeneratedBuildRunner.Parse(output, _run);

        errors.Should().ContainSingle().Which.Should().Be(
            new CompilerDiagnostic("Modernized/Bd/Rgni656.cs", 78, 59, "CS1519", "Invalid token '&' in a member declaration"));
        other.Should().BeEmpty();
    }

    [Fact]
    public void RestoreFailuresAreReportedAsNotHavingCompiled()
    {
        var (errors, other) = GeneratedBuildRunner.Parse(
            "/x/Run.csproj : error NU1101: Unable to find package Foo. [/x/Run.csproj]", _run);

        errors.Should().BeEmpty();
        other.Should().ContainSingle().Which.Should().StartWith("NU1101");
    }

    [Fact]
    public void TheIndexRecordsEachTypeWithItsNamespaceAcrossBlockNamespaces()
    {
        var index = GeneratedTypeIndex.FromSources(new Dictionary<string, string>
        {
            ["A.cs"] = "namespace X.Shared\n{\npublic sealed class Bdcommik { }\n}\nnamespace X.Bd\n{\n[Obsolete] public interface IBdcommikHost { }\n}",
        });

        index.Declarations.Should().BeEquivalentTo(new[]
        {
            new TypeDeclaration("Bdcommik", "X.Shared", "A.cs"),
            new TypeDeclaration("IBdcommikHost", "X.Bd", "A.cs"),
        });
    }

    [Fact]
    public void TheFileNamedAfterATypeKeepsIt()
    {
        GeneratedTypeIndex.ChooseOwner("Sysinfor", ["Bd/Rgnb649.cs", "Shared/Sysinfor.cs", "Shared/Bdsiini1.cs"])
            .Should().Be("Shared/Sysinfor.cs");
        GeneratedTypeIndex.ChooseOwner("Sqlca", ["Bd/Rgnb649.cs", "Bd/Bdsmfjl.cs"])
            .Should().Be("Bd/Bdsmfjl.cs");
    }

    private static readonly Dictionary<string, string> TwoSysinfors = new()
    {
        ["Shared/Sysinfor.cs"] = "namespace S;\npublic sealed class Sysinfor\n{\n    public string Job { get; set; } = \"\";\n}\n",
        ["Shared/Bdsiini1.cs"] = "namespace S;\npublic sealed class Sysinfor\n{\n    public string Step { get; set; } = \"\";\n}\npublic sealed class Bdsiini1 { }\n",
    };

    [Fact]
    public void ADuplicateIsRemovedFromEveryFileButTheOwnerWhicheverFileTheCompilerBlamed()
    {
        // The compiler blames the owner here; the repair still goes to the other file.
        var errors = new[]
        {
            new CompilerDiagnostic("Shared/Sysinfor.cs", 2, 21, "CS0101", "The namespace 'S' already contains a definition for 'Sysinfor'"),
        };

        var tasks = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(TwoSysinfors), TwoSysinfors, "S", Limits);

        var repair = tasks.Should().ContainSingle(t => t.MayRemove.Count > 0).Subject;
        repair.File.Should().Be("Shared/Bdsiini1.cs");
        repair.MayRemove.Should().BeEquivalentTo(["Sysinfor"]);
        repair.Declarations.Should().ContainSingle().Which.Should().Contain("public string Job");
        tasks.Should().NotContain(t => t.File == "Shared/Sysinfor.cs", "the owner keeps its declaration and has nothing to repair");
    }

    [Fact]
    public void ATypeDeclaredNowhereIsDeclaredOnceAsAMarkedExternalContract()
    {
        var sources = new Dictionary<string, string>
        {
            ["Bd/Bdsda23.cs"] = "namespace B;\npublic class Bdsda23 { IBdsparmService _p; }\n",
            ["Bd/Bdsparmx.cs"] = "namespace B;\npublic class Bdsparmx { IBdsparmService _p; }\n",
        };
        var errors = sources.Keys.Select(f => new CompilerDiagnostic(f, 2, 23, "CS0246",
            "The type or namespace name 'IBdsparmService' could not be found (are you missing a using directive or an assembly reference?)")).ToList();

        var tasks = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(sources), sources, "B.Shared", Limits);

        var declaring = tasks.Where(t => t.Instructions.Any(i => i.Contains(CompileRepairPlanner.ExternalContractMarker))).ToList();
        declaring.Should().ContainSingle().Which.File.Should().Be("Bd/Bdsda23.cs");
        declaring[0].Instructions.Should().Contain(i => i.Contains("Bd/Bdsparmx.cs:2"));
        tasks.Single(t => t.File == "Bd/Bdsparmx.cs").Instructions
            .Should().ContainSingle(i => i.Contains("declared by Bd/Bdsda23.cs"));
    }

    [Fact]
    public void AConvertedMemberWhoseTypeWentMissingIsDeclaredFromItsCobolNotFromGuesses()
    {
        var sources = new Dictionary<string, string>
        {
            ["Bd/Bdsda2fk.cs"] = "namespace B;\npublic class Bdsda2fkService { Bdsda2fk _a; }\n",
        };
        var errors = new[]
        {
            new CompilerDiagnostic("Bd/Bdsda2fk.cs", 2, 36, "CS0246",
                "The type or namespace name 'Bdsda2fk' could not be found (are you missing a using directive or an assembly reference?)"),
        };

        var task = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(sources), sources, "B.Shared", Limits,
            new Dictionary<string, string> { ["BDSDA2FK"] = "       01 BDSDA2FK-AREA.\n          05 FIK-KD PIC 9." }).Single();

        task.Instructions.Should().ContainSingle().Which.Should()
            .Contain("05 FIK-KD PIC 9.").And.NotContain(CompileRepairPlanner.ExternalContractMarker);
    }

    [Fact]
    public void ARepairThatDropsATypeItWasNotToldToRemoveIsRejected()
    {
        const string before = "namespace S;\npublic class Keep { }\npublic class Dup { }\n";

        CompileRepairAgent.Reject(before, "namespace S;\npublic class Keep { }\n", new HashSet<string> { "Dup" })
            .Should().BeNull();
        CompileRepairAgent.Reject(before, "namespace S;\npublic class Dup { }\n", new HashSet<string> { "Dup" })
            .Should().Contain("Keep");
        CompileRepairAgent.Reject(before, "namespace S;\npublic class Keep { \npublic class Dup { }\n", new HashSet<string>())
            .Should().Contain("unbalanced");
    }

    [Fact]
    public void ATypeDeclarationIsExtractedToItsClosingBrace()
    {
        CompileRepairPlanner.ExtractDeclaration(TwoSysinfors["Shared/Bdsiini1.cs"], "Sysinfor")
            .Should().Be("public sealed class Sysinfor\n{\n    public string Step { get; set; } = \"\";\n}");
        CompileRepairPlanner.ExtractDeclaration("public sealed record Reni307(int Kode);", "Reni307")
            .Should().Be("public sealed record Reni307(int Kode);");
    }

    [Fact]
    public void ConfiguredLimitsBoundDeclarationLength()
    {
        var source = "public class Big\n{\n" + string.Join("\n", Enumerable.Range(0, 10).Select(i => $"    public int F{i};")) + "\n}";

        CompileRepairPlanner.ExtractDeclaration(source, "Big", maxLines: 4)!.Split('\n')
            .Should().HaveCount(5).And.EndWith("    // … truncated");
        CompileRepairPlanner.ExtractDeclaration(source, "Big")!.Should().Be(source);
    }

    [Fact]
    public void ProjectOfARelativeRunFolderIsAnAbsolutePath()
    {
        var folder = Path.Combine(Path.GetTempPath(), "gate-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(folder);
        try
        {
            File.WriteAllText(Path.Combine(folder, "Modernized.csproj"), "<Project />");
            var relative = Path.GetRelativePath(Directory.GetCurrentDirectory(), folder);

            var project = GeneratedBuildRunner.FindProject(relative);

            project.Should().NotBeNull();
            Path.IsPathRooted(project!).Should().BeTrue();
            File.Exists(project).Should().BeTrue();
        }
        finally
        {
            Directory.Delete(folder, recursive: true);
        }
    }

    [Fact]
    public void AMissingInterfaceOfAConvertedClassIsDeclaredBesideIt()
    {
        var sources = new Dictionary<string, string>
        {
            ["Bd/Bdsm043.cs"] = "namespace B.Bd;\npublic sealed class Bdsm043Service : IBdsm043Service\n{\n    public void Run() { }\n}\n",
            ["Shared/Bdsm043k.cs"] = "namespace B.Shared;\npublic class Caller\n{\n    private readonly IBdsm043Service _s;\n}\n",
        };
        var errors = new[]
        {
            new CompilerDiagnostic("Bd/Bdsm043.cs", 2, 38, "CS0246",
                "The type or namespace name 'IBdsm043Service' could not be found (are you missing a using directive or an assembly reference?)"),
            new CompilerDiagnostic("Shared/Bdsm043k.cs", 4, 22, "CS0246",
                "The type or namespace name 'IBdsm043Service' could not be found (are you missing a using directive or an assembly reference?)"),
        };

        var tasks = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(sources), sources, "B.Shared", Limits);

        var owner = tasks.Single(t => t.File == "Bd/Bdsm043.cs");
        owner.Instructions.Should().Contain(i => i.Contains("implemented by `Bdsm043Service`"));
        owner.Instructions.Should().NotContain(i => i.Contains(CompileRepairPlanner.ExternalContractMarker));
        tasks.Single(t => t.File == "Shared/Bdsm043k.cs").Instructions
            .Should().Contain(i => i.Contains("declared by Bd/Bdsm043.cs"));
    }

    [Fact]
    public void ARepairThatMovesATypeToAnotherNamespaceIsRejected()
    {
        const string before = "namespace A.Shared\n{\n    public class Keep { }\n}\n";
        const string after = "namespace A.Bd\n{\n    public class Keep { }\n}\n";

        CompileRepairAgent.Reject(before, after, new HashSet<string>()).Should().Contain("moved type(s)");
        CompileRepairAgent.Reject(before, before.Replace("{ }", "{ public int X; }"), new HashSet<string>()).Should().BeNull();
    }

    [Fact]
    public void ARoundThatAddsErrorsIsReportedAsUndone()
    {
        var result = new CompileGateResult(false, true, null,
            [
                new CompileRound(0, 22, 8, 1, new Dictionary<string, int>()),
                new CompileRound(1, 497, 0, 0, new Dictionary<string, int>(), RolledBack: true),
            ],
            [], []);

        result.ToMarkdown(10).Should().Contain("| 1 | 497 (worse than before; the round's repairs were undone) |");
    }

    [Fact]
    public void ATypeDeclaredInOneOtherNamespaceIsImportedWithoutAModel()
    {
        var sources = new Dictionary<string, string>
        {
            ["Bd/Rgnb649.cs"] = "using System;\nnamespace M.Bd;\npublic sealed class Sqlca { }\n",
            ["Bd/Bdsmfjl.cs"] = "using System;\nusing M.Shared;\n\nnamespace M.Shared\n{\n    public interface ISql { void Run(Sqlca s); }\n}\n",
        };
        var errors = new[]
        {
            new CompilerDiagnostic("Bd/Bdsmfjl.cs", 6, 38, "CS0246", "The type or namespace name 'Sqlca' could not be found (are you missing a using directive or an assembly reference?)"),
            new CompilerDiagnostic("Bd/Bdsmfjl.cs", 6, 10, "CS0246", "The type or namespace name 'Nowhere' could not be found"),
        };

        var (imported, remaining) = CompileRepairPlanner.ImportUniqueNamespaces(errors, GeneratedTypeIndex.FromSources(sources), sources);

        imported.Should().ContainKey("Bd/Bdsmfjl.cs").WhoseValue.Should()
            .StartWith("using System;\nusing M.Shared;\nusing M.Bd;\n\nnamespace M.Shared");
        remaining.Should().ContainSingle().Which.Message.Should().Contain("Nowhere");
    }

    [Fact]
    public void FixingTheLastBlockingErrorIsProgressEvenWhenItRevealsBodyErrors()
    {
        static CompilerDiagnostic E(string code) => new("A.cs", 1, 1, code, "x");
        var blocked = new[] { E("CS0246") };
        var revealed = Enumerable.Repeat(E("CS1061"), 497).ToArray();

        CSharpCompileGate.IsWorse(revealed, blocked).Should().BeFalse();
        CSharpCompileGate.IsWorse(blocked, revealed).Should().BeTrue();
        CSharpCompileGate.IsWorse([E("CS1061"), E("CS1061")], [E("CS1061")]).Should().BeTrue();
        CSharpCompileGate.IsWorse([E("CS0246"), E("CS0246")], [E("CS0246"), E("CS1061"), E("CS1061")]).Should().BeTrue();

        // The run that motivated three phases: two syntax errors hid 22 declaration errors.
        var syntax = new[] { E("CS1002"), E("CS1014") };
        CSharpCompileGate.IsWorse(Enumerable.Repeat(E("CS0246"), 22).ToArray(), syntax).Should().BeFalse();
    }

    [Fact]
    public void AnArgumentErrorShowsTheDeclarationOfTheMethodBeingCalled()
    {
        var sources = new Dictionary<string, string>
        {
            ["CallTargetContracts.g.cs"] =
                "namespace S\n{\n    public interface IBdsda2fService\n    {\n        Task ExecuteAsync(Bdsdatoi a, Bdsmfjli b, CancellationToken c = default);\n    }\n}\n",
            ["Bd/Rgnb649.cs"] =
                "namespace B;\npublic sealed class Rgnb649(IBdsda2fService bdsda2fService)\n{\n    Task Run() => bdsda2fService.ExecuteAsync(_parm, _ct);\n}\n",
        };
        var errors = new[]
        {
            new CompilerDiagnostic("Bd/Rgnb649.cs", 4, 20, "CS1503",
                "Argument 2: cannot convert from 'System.Threading.CancellationToken' to 'S.Bdsmfjli'"),
        };

        var task = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(sources), sources, "S", Limits)
            .Should().ContainSingle().Subject;

        task.Declarations.Should().ContainSingle().Which.Should()
            .Contain("interface IBdsda2fService").And.Contain("ExecuteAsync(Bdsdatoi a, Bdsmfjli b");
    }
}
