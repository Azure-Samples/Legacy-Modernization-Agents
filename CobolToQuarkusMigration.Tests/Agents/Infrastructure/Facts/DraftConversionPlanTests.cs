using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Facts;

public sealed class DraftConversionPlanTests : IDisposable
{
    private readonly string _root = Path.GetFullPath(Path.Combine(".test-artifacts", $"draft-plan-{Guid.NewGuid():N}"));
    private string SourceRoot => Path.Combine(_root, "source");
    private string FactsRoot => Path.Combine(_root, "output", "rekt");

    [Fact]
    public void DraftIsolatesCompleteProgramWithoutRelaxingStrictBatchOrCoverage()
    {
        var good = Source("a/MAIN.cbl");
        var gap = Source("b/MAIN.cbl");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "a/MAIN.cbl");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "b/MAIN.cbl");
        evidence.Coverage["FILE_IO"] = "unsupported";
        evidence.UnresolvedDiagnostics.Add("native:unresolved-copybook:SHARED-REC");
        DependencyEvidenceFixture.Save(_root, evidence);
        var strict = () => ConversionEligibility.Select([good, gap], SourceRoot, FactsRoot,
            Path.Combine(_root, "strict"), new() { DependencyMode = "strict" }, null);
        strict.Should().Throw<InvalidDataException>();
        var output = Path.Combine(_root, "draft");
        var selected = ConversionEligibility.Select([good, gap], SourceRoot, FactsRoot, output,
            new() { DependencyMode = "draft" }, null);
        selected.Should().ContainSingle().Which.Should().BeSameAs(good);
        using var json = JsonDocument.Parse(File.ReadAllText(Path.Combine(output, "draft-conversion-plan.json")));
        json.RootElement.GetProperty("verified").GetBoolean().Should().BeFalse();
        var blocked = json.RootElement.GetProperty("programs")[1];
        blocked.GetProperty("program").GetString().Should().Be("b/MAIN.cbl");
        blocked.GetProperty("blockers").EnumerateArray().Should().Contain(b =>
            b.GetProperty("missingEvidence").GetString()!.Contains("SHARED-REC"));
        blocked.GetProperty("blockers")[0].GetProperty("routine").ValueKind.Should().Be(JsonValueKind.Null);
        Directory.GetFiles(output, "*", SearchOption.AllDirectories).Should().NotContain(p => p.EndsWith(".java") || p.EndsWith(".cs"));
    }

    [Theory]
    [InlineData("source", "artifact-blocker", "SHA256 mismatch")]
    [InlineData("coverage", "coverage-gap", "COPY coverage is partial")]
    [InlineData("invalid", "artifact-blocker", "unreadable or invalid")]
    [InlineData("missing", "artifact-blocker", "dependency-evidence.json")]
    [InlineData("provenance", "artifact-blocker", "native-export-source-unverified")]
    public void BlockedProgramsPersistExactDeficiencyAndActionWithoutInventingRoutineImpact(string damage, string category, string reason)
    {
        var file = Source("MAIN.cbl");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        switch (damage)
        {
            case "source": File.AppendAllText(file.FilePath, "\n           DISPLAY 'changed'."); break;
            case "coverage":
                evidence.Coverage["COPY"] = "partial";
                DependencyEvidenceFixture.Save(_root, evidence);
                break;
            case "invalid": File.WriteAllText(DependencyEvidenceGate.GetEvidencePath(FactsRoot, "MAIN.cbl"), "{"); break;
            case "missing": File.Delete(DependencyEvidenceGate.GetEvidencePath(FactsRoot, "MAIN.cbl")); break;
            case "provenance":
                evidence.UnresolvedDiagnostics.Add("native-export-source-unverified:original hash mismatch");
                DependencyEvidenceFixture.Save(_root, evidence);
                break;
        }
        var output = Path.Combine(_root, "draft");
        var select = () => ConversionEligibility.Select([file], SourceRoot, FactsRoot, output,
            new() { DependencyMode = "draft" }, null);
        select.Should().Throw<InvalidDataException>().WithMessage("*No independently justified whole programs*");
        var plan = DraftConversionPlanner.Assess(new(FactsRoot), FactsRoot, [file], SourceRoot);
        plan.Status.Should().Be("blocked");
        plan.Programs.Single().Blockers.Should().Contain(b => b.Category == category && b.Raw != null && b.Raw.Contains(reason));
        plan.Programs.Single().Blockers.Should().OnlyContain(b => b.Action.Length > 0 && b.Location == null && b.Routine == null);
        File.Exists(Path.Combine(output, "draft-conversion-plan.json")).Should().BeTrue();
    }

    [Fact]
    public void SelectedBlockedCalleeBlocksCallerTransitively()
    {
        var caller = Source("CALLER.cbl", "           CALL 'CALLEE'.");
        var callee = Source("CALLEE.cbl");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "CALLER.cbl", new DependencyDefinition
        {
            Kind = "CALL", Name = "CALLEE.cbl", Path = callee.FilePath, Sha256 = DependencyEvidenceFixture.Hash(callee.FilePath)
        });
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "CALLEE.cbl");
        evidence.Coverage["EXEC_CICS"] = "unsupported";
        DependencyEvidenceFixture.Save(_root, evidence);
        var plan = DraftConversionPlanner.Assess(new(FactsRoot), FactsRoot, [caller, callee], SourceRoot);
        plan.Programs.Should().OnlyContain(p => p.Status == "blocked");
        plan.Programs[0].Blockers.Should().Contain(b => b.MissingEvidence.Contains("CALLEE"));
    }

    [Fact]
    public void DraftRefusesStaleOutputAndInvalidModes()
    {
        var file = Source("MAIN.cbl");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "MAIN.cbl");
        var output = Path.Combine(_root, "draft");
        Directory.CreateDirectory(output);
        File.WriteAllText(Path.Combine(output, "Old.java"), "previous output");
        var select = () => ConversionEligibility.Select([file], SourceRoot, FactsRoot, output,
            new() { DependencyMode = "draft" }, null);
        select.Should().Throw<InvalidDataException>().WithMessage("*fresh empty output*");
        var invalid = () => ConversionEligibility.ValidateMode("best-effort");
        invalid.Should().Throw<ArgumentException>();
    }

    [Fact]
    public async Task DraftProgramReceivesOnlyItsRealHashBoundCopyDefinitionsWithoutStandaloneCopybookConversion()
    {
        var good = Source("GOOD.cbl", "           COPY PAYREC.");
        File.WriteAllText(good.FilePath, """
                   IDENTIFICATION DIVISION.
                   PROGRAM-ID. GOOD.
                   DATA DIVISION.
                   WORKING-STORAGE SECTION.
                       COPY PAYREC.
                   PROCEDURE DIVISION.
                       DISPLAY REAL-AMOUNT.
                       STOP RUN.
            """);
        good.Content = File.ReadAllText(good.FilePath);
        var needed = Source("PAYREC.cpy", "       01 PAY-RECORD.\n          05 REAL-AMOUNT PIC 9(7)V99.");
        var blocked = Source("BLOCKED.cbl");
        var unrelated = Source("UNRELATED.cpy", "       01 UNRELATED-SECRET PIC X.");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "GOOD.cbl", new DependencyDefinition
        {
            Kind = "COPY", Name = "PAYREC.cpy", Path = needed.FilePath,
            Sha256 = DependencyEvidenceFixture.Hash(needed.FilePath)
        });
        var selected = ConversionEligibility.Select([good, needed, blocked, unrelated], SourceRoot, FactsRoot,
            Path.Combine(_root, "draft"), new() { DependencyMode = "draft" }, null);
        selected.Should().ContainSingle().Which.Should().BeSameAs(good);
        var context = new System.Text.StringBuilder();
        await RektPromptInjector.InjectAsync(context, "Java", "GOOD.cbl", "test", null,
            sourceDirectory: SourceRoot, repositoryRoot: _root, verifiedDraftContext: true);
        context.ToString().Should().Contain("COPY PAYREC.cpy").And.Contain("REAL-AMOUNT PIC 9(7)V99")
            .And.Contain(DependencyEvidenceFixture.Hash(needed.FilePath))
            .And.NotContain("UNRELATED-SECRET").And.NotContain("BLOCKED.cbl");
        Directory.GetFiles(Path.Combine(_root, "draft"), "*", SearchOption.AllDirectories)
            .Should().NotContain(p => p.EndsWith(".java") || p.EndsWith(".cs"));
        File.AppendAllText(needed.FilePath, "\n       01 CHANGED PIC X.");
        var inject = () => RektPromptInjector.InjectAsync(new(), "Java", "GOOD.cbl", "test", null,
            sourceDirectory: SourceRoot, repositoryRoot: _root, verifiedDraftContext: true);
        await inject.Should().ThrowAsync<InvalidDataException>().WithMessage("*SHA256 mismatch*");
    }

    [Theory]
    [InlineData("resolved")]
    [InlineData("stale")]
    [InlineData("missing")]
    public void InputInventoryDistinguishesSharedPresenceFromStaleAndMissingBindings(string status)
    {
        var caller = Source("bank/MAIN.cbl", "           COPY SHARED.");
        var copy = Source("common/SHARED.cpy", "       01 SHARED PIC X.");
        DependencyEvidenceFixture.Write(_root, SourceRoot, "bank/MAIN.cbl", new DependencyDefinition
        {
            Kind = "COPY", Name = "SHARED.cpy", Path = copy.FilePath,
            Sha256 = DependencyEvidenceFixture.Hash(copy.FilePath)
        });
        if (status == "stale") File.AppendAllText(copy.FilePath, "\n       01 CHANGED PIC X.");
        if (status == "missing") File.Delete(copy.FilePath);
        var plan = DraftConversionPlanner.Assess(new(FactsRoot), FactsRoot, [caller], SourceRoot);
        var input = plan.Programs.Single().DependencyInputs!.Items.Single();
        input.Status.Should().Be(status);
        input.Scope.Should().Be("shared");
        input.Path.Should().Be(copy.FilePath);
        plan.Programs.Single().Status.Should().Be(status == "resolved" ? "eligible" : "blocked",
            string.Join("; ", plan.Programs.Single().Blockers.Select(blocker => blocker.Raw)));
    }

    [Fact]
    public void InputInventoryShowsUnboundCandidatesAndRealMissingNamesWithoutTreatingParserGapsAsFiles()
    {
        Source("bank/MAIN.cbl");
        Source("common/REAL.cpy", "       01 REAL PIC X.");
        Source("copy-generated/STUB.cpy", "      * AUTO-GENERATED STUB\n       01 STUB-STUB PIC X.");
        var evidence = DependencyEvidenceFixture.Write(_root, SourceRoot, "bank/MAIN.cbl");
        evidence.UnresolvedDiagnostics.AddRange([
            "native:unresolved-copybook:REAL", "native:unresolved-copybook:STUB",
            "native:unresolved-copybook:ABSENT", "native:callee:CHILD:source-not-found:CHILD",
            "native:unresolved-EXEC_CICS-dependency-bindings:Cics_newContext"
        ]);
        DependencyEvidenceFixture.Save(_root, evidence);
        var inputs = new DependencyInputInventory(SourceRoot).Read(FactsRoot, "bank/MAIN.cbl");
        inputs.Warning.Should().BeNull();
        inputs.Items.Should().HaveCount(4);
        inputs.Items.Should().Contain(item => item.Name == "REAL" && item.Status == "unverified");
        inputs.Items.Should().Contain(item => item.Name == "STUB" && item.Status == "rejected");
        inputs.Items.Should().Contain(item => item.Name == "ABSENT" && item.Status == "missing" && item.Path == null);
        inputs.Items.Should().Contain(item => item.Name == "CHILD" && item.Kind == "CALL" && item.Status == "missing");
    }

    [Theory]
    [InlineData(null)]
    [InlineData("{")]
    [InlineData("{}")]
    [InlineData("{\"definitions\":null}")]
    [InlineData("{\"definitions\":[null]}")]
    [InlineData("{\"unresolvedDiagnostics\":[null]}")]
    public void MissingOrInvalidInventoryIsExplicitlyUnknownNotAZeroDependencyClaim(string? json)
    {
        Source("MAIN.cbl");
        if (json is not null)
        {
            var path = DependencyEvidenceGate.GetEvidencePath(FactsRoot, "MAIN.cbl");
            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, json);
        }
        var inventory = new DependencyInputInventory(SourceRoot).Read(FactsRoot, "MAIN.cbl");
        inventory.Items.Should().BeEmpty();
        inventory.Warning.Should().NotBeNullOrEmpty();
    }

    private CobolFile Source(string relative, string body = "           STOP RUN.")
    {
        var path = Path.Combine(SourceRoot, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, SourceTypeRegistry.IsCopybook(Path.GetFileName(path)) ? body : "       PROCEDURE DIVISION.\n" + body);
        return new() { FileName = Path.GetFileName(path), FilePath = path, Content = File.ReadAllText(path) };
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, true);
    }
}
