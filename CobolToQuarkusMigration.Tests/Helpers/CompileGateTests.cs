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
        tasks.Single(t => t.File == "Shared/Sysinfor.cs").MayRemove.Should().BeEmpty();
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
}
