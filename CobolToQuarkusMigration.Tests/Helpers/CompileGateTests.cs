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
        File.WriteAllText(Path.Join(_run, "Modernized", "Bd", "Payi656.cs"), "");
        // macOS reports /tmp/... as /private/tmp/...; only the suffix is reliable.
        var output =
            $"/private{_run}/Modernized/Bd/Payi656.cs(78,59): error CS1519: Invalid token '&' in a member declaration [/x/Run.csproj]\n" +
            $"/private{_run}/Modernized/Bd/Payi656.cs(78,59): error CS1519: Invalid token '&' in a member declaration [/x/Run.csproj]\n" +
            "/x/Run.csproj : warning NU1603: approximate match\n";

        var (errors, other) = GeneratedBuildRunner.Parse(output, _run);

        errors.Should().ContainSingle().Which.Should().Be(
            new CompilerDiagnostic("Modernized/Bd/Payi656.cs", 78, 59, "CS1519", "Invalid token '&' in a member declaration"));
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
            ["A.cs"] = "namespace X.Shared\n{\npublic sealed class Batommik { }\n}\nnamespace X.Bd\n{\n[Obsolete] public interface IBatommikHost { }\n}",
        });

        index.Declarations.Should().BeEquivalentTo(new[]
        {
            new TypeDeclaration("Batommik", "X.Shared", "A.cs"),
            new TypeDeclaration("IBatommikHost", "X.Bd", "A.cs"),
        });
    }

    [Fact]
    public void TheFileNamedAfterATypeKeepsIt()
    {
        GeneratedTypeIndex.ChooseOwner("Jobinfo", ["Bd/Payb649.cs", "Shared/Jobinfo.cs", "Shared/Ordiini1.cs"])
            .Should().Be("Shared/Jobinfo.cs");
        GeneratedTypeIndex.ChooseOwner("Sqlca", ["Bd/Payb649.cs", "Bd/Ordmain.cs"])
            .Should().Be("Bd/Ordmain.cs");
    }

    private static readonly Dictionary<string, string> TwoJobinfos = new()
    {
        ["Shared/Jobinfo.cs"] = "namespace S;\npublic sealed class Jobinfo\n{\n    public string Job { get; set; } = \"\";\n}\n",
        ["Shared/Ordiini1.cs"] = "namespace S;\npublic sealed class Jobinfo\n{\n    public string Step { get; set; } = \"\";\n}\npublic sealed class Ordiini1 { }\n",
    };

    [Fact]
    public void ACopyInAnotherNamespaceIsRemovedByTheFileThatCannotConvertIt()
    {
        var sources = new Dictionary<string, string>
        {
            ["Shared/Jobinfo.cs"] = "namespace M.Shared;\npublic sealed class Jobinfo { public string Job { get; set; } = \"\"; }\n",
            ["Bd/Payb649.cs"] = "namespace M.Bd;\npublic sealed class Jobinfo { }\npublic sealed class Payb649 { }\n",
        };
        var errors = new[]
        {
            new CompilerDiagnostic("Bd/Payb649.cs", 3, 44, "CS1503", "Argument 1: cannot convert from 'M.Bd.Jobinfo' to 'M.Shared.Jobinfo'"),
            new CompilerDiagnostic("Bd/Payb649.cs", 9, 58, "CS1503", "Argument 1: cannot convert from 'M.Bd.Jobinfo' to 'M.Shared.Jobinfo'"),
        };

        var tasks = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(sources), sources, "M.Shared", Limits);

        var repair = tasks.Should().ContainSingle().Subject;
        repair.File.Should().Be("Bd/Payb649.cs");
        repair.MayRemove.Should().BeEquivalentTo(["Jobinfo"]);
        repair.Instructions.Should().ContainSingle(i => i.Contains("using M.Shared;"));
        repair.Declarations.Should().ContainSingle().Which.Should().Contain("public string Job");
        CompileRepairAgent.Reject(sources["Bd/Payb649.cs"], "namespace M.Bd;\npublic sealed class Payb649 { }\n", repair.MayRemove)
            .Should().BeNull();
    }

    [Fact]
    public void ADuplicateIsRemovedFromEveryFileButTheOwnerWhicheverFileTheCompilerBlamed()
    {
        // The compiler blames the owner here; the repair still goes to the other file.
        var errors = new[]
        {
            new CompilerDiagnostic("Shared/Jobinfo.cs", 2, 21, "CS0101", "The namespace 'S' already contains a definition for 'Jobinfo'"),
        };

        var tasks = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(TwoJobinfos), TwoJobinfos, "S", Limits);

        var repair = tasks.Should().ContainSingle(t => t.MayRemove.Count > 0).Subject;
        repair.File.Should().Be("Shared/Ordiini1.cs");
        repair.MayRemove.Should().BeEquivalentTo(["Jobinfo"]);
        repair.Declarations.Should().ContainSingle().Which.Should().Contain("public string Job");
        tasks.Should().NotContain(t => t.File == "Shared/Jobinfo.cs", "the owner keeps its declaration and has nothing to repair");
    }

    [Fact]
    public void ATypeDeclaredNowhereIsDeclaredOnceAsAMarkedExternalContract()
    {
        var sources = new Dictionary<string, string>
        {
            ["Bd/Ordda23.cs"] = "namespace B;\npublic class Ordda23 { IOrdparmService _p; }\n",
            ["Bd/Ordparmx.cs"] = "namespace B;\npublic class Ordparmx { IOrdparmService _p; }\n",
        };
        var errors = sources.Keys.Select(f => new CompilerDiagnostic(f, 2, 23, "CS0246",
            "The type or namespace name 'IOrdparmService' could not be found (are you missing a using directive or an assembly reference?)")).ToList();

        var tasks = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(sources), sources, "B.Shared", Limits);

        var declaring = tasks.Where(t => t.Instructions.Any(i => i.Contains(CompileRepairPlanner.ExternalContractMarker))).ToList();
        declaring.Should().ContainSingle().Which.File.Should().Be("Bd/Ordda23.cs");
        declaring[0].Instructions.Should().Contain(i => i.Contains("Bd/Ordparmx.cs:2"));
        tasks.Single(t => t.File == "Bd/Ordparmx.cs").Instructions
            .Should().ContainSingle(i => i.Contains("declared by Bd/Ordda23.cs"));
    }

    [Fact]
    public void AConvertedMemberWhoseTypeWentMissingIsDeclaredFromItsCobolNotFromGuesses()
    {
        var sources = new Dictionary<string, string>
        {
            ["Bd/Ordda2fk.cs"] = "namespace B;\npublic class Ordda2fkService { Ordda2fk _a; }\n",
        };
        var errors = new[]
        {
            new CompilerDiagnostic("Bd/Ordda2fk.cs", 2, 36, "CS0246",
                "The type or namespace name 'Ordda2fk' could not be found (are you missing a using directive or an assembly reference?)"),
        };

        var task = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(sources), sources, "B.Shared", Limits,
            new Dictionary<string, string> { ["ORDDA2FK"] = "       01 ORDDA2FK-AREA.\n          05 FIK-KD PIC 9." }).Single();

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
        CompileRepairPlanner.ExtractDeclaration(TwoJobinfos["Shared/Ordiini1.cs"], "Jobinfo")
            .Should().Be("public sealed class Jobinfo\n{\n    public string Step { get; set; } = \"\";\n}");
        CompileRepairPlanner.ExtractDeclaration("public sealed record Rpti307(int Kode);", "Rpti307")
            .Should().Be("public sealed record Rpti307(int Kode);");
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
            ["Bd/Ordm043.cs"] = "namespace B.Bd;\npublic sealed class Ordm043Service : IOrdm043Service\n{\n    public void Run() { }\n}\n",
            ["Shared/Ordm043k.cs"] = "namespace B.Shared;\npublic class Caller\n{\n    private readonly IOrdm043Service _s;\n}\n",
        };
        var errors = new[]
        {
            new CompilerDiagnostic("Bd/Ordm043.cs", 2, 38, "CS0246",
                "The type or namespace name 'IOrdm043Service' could not be found (are you missing a using directive or an assembly reference?)"),
            new CompilerDiagnostic("Shared/Ordm043k.cs", 4, 22, "CS0246",
                "The type or namespace name 'IOrdm043Service' could not be found (are you missing a using directive or an assembly reference?)"),
        };

        var tasks = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(sources), sources, "B.Shared", Limits);

        var owner = tasks.Single(t => t.File == "Bd/Ordm043.cs");
        owner.Instructions.Should().Contain(i => i.Contains("implemented by `Ordm043Service`"));
        owner.Instructions.Should().NotContain(i => i.Contains(CompileRepairPlanner.ExternalContractMarker));
        tasks.Single(t => t.File == "Shared/Ordm043k.cs").Instructions
            .Should().Contain(i => i.Contains("declared by Bd/Ordm043.cs"));
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
            ["Bd/Payb649.cs"] = "using System;\nnamespace M.Bd;\npublic sealed class Sqlca { }\n",
            ["Bd/Ordmain.cs"] = "using System;\nusing M.Shared;\n\nnamespace M.Shared\n{\n    public interface ISql { void Run(Sqlca s); }\n}\n",
        };
        var errors = new[]
        {
            new CompilerDiagnostic("Bd/Ordmain.cs", 6, 38, "CS0246", "The type or namespace name 'Sqlca' could not be found (are you missing a using directive or an assembly reference?)"),
            new CompilerDiagnostic("Bd/Ordmain.cs", 6, 10, "CS0246", "The type or namespace name 'Nowhere' could not be found"),
        };

        var (imported, remaining) = CompileRepairPlanner.ImportUniqueNamespaces(errors, GeneratedTypeIndex.FromSources(sources), sources);

        imported.Should().ContainKey("Bd/Ordmain.cs").WhoseValue.Should()
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
                "namespace S\n{\n    public interface IOrdda2fService\n    {\n        Task ExecuteAsync(Orddatai a, Ordmaini b, CancellationToken c = default);\n    }\n}\n",
            ["Bd/Payb649.cs"] =
                "namespace B;\npublic sealed class Payb649(IOrdda2fService ordda2fService)\n{\n    Task Run() => ordda2fService.ExecuteAsync(_parm, _ct);\n}\n",
        };
        var errors = new[]
        {
            new CompilerDiagnostic("Bd/Payb649.cs", 4, 20, "CS1503",
                "Argument 2: cannot convert from 'System.Threading.CancellationToken' to 'S.Ordmaini'"),
        };

        var task = CompileRepairPlanner.Plan(errors, GeneratedTypeIndex.FromSources(sources), sources, "S", Limits)
            .Should().ContainSingle().Subject;

        task.Declarations.Should().ContainSingle().Which.Should()
            .Contain("interface IOrdda2fService").And.Contain("ExecuteAsync(Orddatai a, Ordmaini b");
    }
}
