using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Facts;

public sealed class ProgramSelectionResolverTests : IDisposable
{
    private readonly string _factsDir = Path.Combine(
        AppContext.BaseDirectory,
        "test-artifacts",
        $"program-selection-{Guid.NewGuid():N}");

    private static readonly string[] Estate =
    [
        "finance/ACCOUNTS.cbl",
        "finance/LEDGER.cbl",
        "shared/UTILITY.cbl",
        "batch/NIGHTLY.cbl",
    ];

    [Fact]
    public void Resolve_WithoutClosure_SelectsOnlyTheNamedProgram()
    {
        var resolver = CreateResolver();

        var result = resolver.Resolve(new ProgramSelection
        {
            Programs = ["finance/ACCOUNTS.cbl"],
        });

        result.Programs.Should().ContainSingle().Which.Should().Be("finance/ACCOUNTS.cbl");
    }

    [Fact]
    public void Resolve_WithoutClosure_NeedsNoFactsOnDisk()
    {
        var resolver = new ProgramSelectionResolver(
            new ProgramSourceCatalog(Estate),
            new FactsClosureSource(Path.Combine(_factsDir, "never-created")));

        var result = resolver.Resolve(new ProgramSelection { Programs = ["UTILITY"] });

        result.Programs.Should().ContainSingle().Which.Should().Be("shared/UTILITY.cbl");
    }

    [Fact]
    public void Resolve_RejectsAmbiguousBasenameInsteadOfPickingOne()
    {
        var resolver = new ProgramSelectionResolver(
            new ProgramSourceCatalog(["finance/ACCOUNTS.cbl", "archive/ACCOUNTS.cbl"]),
            new FactsClosureSource(_factsDir));

        var act = () => resolver.Resolve(new ProgramSelection { Programs = ["ACCOUNTS.cbl"] });

        act.Should().Throw<InvalidOperationException>()
            .WithMessage("*Use a source-relative path*");
    }

    [Fact]
    public void Resolve_RejectsEmptySelection()
    {
        var resolver = CreateResolver();

        var act = () => resolver.Resolve(new ProgramSelection());

        act.Should().Throw<InvalidOperationException>()
            .WithMessage("*no program selector*");
    }

    [Fact]
    public void Resolve_IncludeCallees_FollowsTheCallGraphTransitively()
    {
        WriteFacts("finance/ACCOUNTS.cbl", callees: ["finance/LEDGER.cbl"]);
        WriteFacts("finance/LEDGER.cbl", callees: ["shared/UTILITY.cbl"], callers: ["finance/ACCOUNTS.cbl"]);
        WriteFacts("shared/UTILITY.cbl", callers: ["finance/LEDGER.cbl"]);
        var resolver = CreateResolver();

        var result = resolver.Resolve(new ProgramSelection
        {
            Programs = ["finance/ACCOUNTS.cbl"],
            IncludeCallees = true,
        });

        result.Programs.Should().BeEquivalentTo(
            "finance/ACCOUNTS.cbl", "finance/LEDGER.cbl", "shared/UTILITY.cbl");
    }

    [Fact]
    public void Resolve_IncludeCallers_FollowsTheCallGraphInReverse()
    {
        WriteFacts("finance/ACCOUNTS.cbl", callees: ["shared/UTILITY.cbl"]);
        WriteFacts("batch/NIGHTLY.cbl", callees: ["shared/UTILITY.cbl"]);
        WriteFacts("shared/UTILITY.cbl", callers: ["finance/ACCOUNTS.cbl", "batch/NIGHTLY.cbl"]);
        var resolver = CreateResolver();

        var result = resolver.Resolve(new ProgramSelection
        {
            Programs = ["shared/UTILITY.cbl"],
            IncludeCallers = true,
        });

        result.Programs.Should().BeEquivalentTo(
            "shared/UTILITY.cbl", "finance/ACCOUNTS.cbl", "batch/NIGHTLY.cbl");
    }

    [Fact]
    public void Resolve_ClosureTerminatesOnCycles()
    {
        WriteFacts("finance/ACCOUNTS.cbl", callees: ["finance/LEDGER.cbl"], callers: ["finance/LEDGER.cbl"]);
        WriteFacts("finance/LEDGER.cbl", callees: ["finance/ACCOUNTS.cbl"], callers: ["finance/ACCOUNTS.cbl"]);
        var resolver = CreateResolver();

        var result = resolver.Resolve(new ProgramSelection
        {
            Programs = ["finance/ACCOUNTS.cbl"],
            IncludeCallees = true,
        });

        result.Programs.Should().BeEquivalentTo("finance/ACCOUNTS.cbl", "finance/LEDGER.cbl");
    }

    // A closure that silently omits a program is the failure this feature exists to prevent,
    // so missing evidence must refuse rather than return a smaller set.
    [Fact]
    public void Resolve_ClosureWithoutFactsRefusesInsteadOfUnderReporting()
    {
        var resolver = CreateResolver();

        var act = () => resolver.Resolve(new ProgramSelection
        {
            Programs = ["finance/ACCOUNTS.cbl"],
            IncludeCallees = true,
        });

        act.Should().Throw<InvalidOperationException>()
            .WithMessage("*finance/ACCOUNTS.cbl*")
            .WithMessage("*rekt-scan*");
    }

    [Fact]
    public void Resolve_ClosureRefusesWhenAReachedProgramHasNoFacts()
    {
        WriteFacts("finance/ACCOUNTS.cbl", callees: ["finance/LEDGER.cbl"]);
        var resolver = CreateResolver();

        var act = () => resolver.Resolve(new ProgramSelection
        {
            Programs = ["finance/ACCOUNTS.cbl"],
            IncludeCallees = true,
        });

        act.Should().Throw<InvalidOperationException>().WithMessage("*finance/LEDGER.cbl*");
    }

    // Mirrors the dependency-health rule that an unresolved CALL is a finding, not a silent drop.
    [Fact]
    public void Resolve_ReportsUnresolvedCallTargetsRatherThanDroppingThem()
    {
        WriteFacts("finance/ACCOUNTS.cbl", callees: ["PAYROLL"]);
        var resolver = CreateResolver();

        var result = resolver.Resolve(new ProgramSelection
        {
            Programs = ["finance/ACCOUNTS.cbl"],
            IncludeCallees = true,
        });

        result.Programs.Should().ContainSingle().Which.Should().Be("finance/ACCOUNTS.cbl");
        result.UnresolvedCallTargets.Should().ContainSingle().Which.Should().Be("PAYROLL");
    }

    [Fact]
    public void Resolve_RecordsWhyEachProgramWasSelected()
    {
        WriteFacts("finance/ACCOUNTS.cbl", callees: ["finance/LEDGER.cbl"]);
        WriteFacts("finance/LEDGER.cbl", callers: ["finance/ACCOUNTS.cbl"]);
        var resolver = CreateResolver();

        var result = resolver.Resolve(new ProgramSelection
        {
            Programs = ["ACCOUNTS"],
            IncludeCallees = true,
        });

        result.Matches.Should().ContainSingle(m => m.Program == "finance/ACCOUNTS.cbl")
            .Which.Reason.Should().Be("program selector 'ACCOUNTS'");
        result.Matches.Should().ContainSingle(m => m.Program == "finance/LEDGER.cbl")
            .Which.Reason.Should().Be("called by finance/ACCOUNTS.cbl");
    }

    [Fact]
    public void Resolve_OrdersProgramsDeterministically()
    {
        WriteFacts("batch/NIGHTLY.cbl", callees: ["finance/ACCOUNTS.cbl"]);
        WriteFacts("finance/ACCOUNTS.cbl", callers: ["batch/NIGHTLY.cbl"]);
        var resolver = CreateResolver();

        var result = resolver.Resolve(new ProgramSelection
        {
            Programs = ["batch/NIGHTLY.cbl"],
            IncludeCallees = true,
        });

        result.Programs.Should().Equal("batch/NIGHTLY.cbl", "finance/ACCOUNTS.cbl");
    }

    private ProgramSelectionResolver CreateResolver() =>
        new(new ProgramSourceCatalog(Estate), new FactsClosureSource(_factsDir));

    private void WriteFacts(
        string relativePath,
        IReadOnlyList<string>? callees = null,
        IReadOnlyList<string>? callers = null)
    {
        var facts = new ProgramFacts
        {
            Basename = Path.GetFileName(relativePath),
            Stem = Path.GetFileNameWithoutExtension(relativePath),
            RelativePath = relativePath,
            SourceHash = "test",
            Confidence = FactConfidence.High,
            Summary = new ProgramSummary { ProgramId = Path.GetFileNameWithoutExtension(relativePath) },
            Callees = callees ?? Array.Empty<string>(),
            Callers = callers ?? Array.Empty<string>(),
        };

        var path = ProgramFactsArtifactLocator.GetFactsFilePath(_factsDir, relativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, JsonSerializer.Serialize(facts));
    }

    public void Dispose()
    {
        if (Directory.Exists(_factsDir))
            Directory.Delete(_factsDir, recursive: true);
    }
}
