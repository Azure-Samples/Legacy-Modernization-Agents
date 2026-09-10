using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// Pins the staging contract shared by doctor.sh and the portal. Both narrow a conversion by
// copying the selected scope and repointing --source at it, so a disagreement here would convert
// a different set of programs than the operator asked for.
public sealed class ConversionScopeStagerTests : IDisposable
{
    private readonly string _root = Path.Combine(
        AppContext.BaseDirectory,
        "test-artifacts",
        $"scope-stager-{Guid.NewGuid():N}");

    private string SourceDir => Path.Combine(_root, "source");

    private string StagingDir => Path.Combine(SourceDir, ".conversion-staging");

    [Fact]
    public void StagesSelectedProgramsUnderTheirSourceRelativeFolders()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/ACCOUNTS.cbl");

        var staged = Stage("finance/ACCOUNTS.cbl");

        staged.Programs.Should().Equal(["finance/ACCOUNTS.cbl"]);
        File.Exists(Path.Combine(StagingDir, "finance", "ACCOUNTS.cbl")).Should().BeTrue();
    }

    // Flattening programs would collide the two ACCOUNTS.cbl above and convert the wrong one.
    [Fact]
    public void DoesNotStageProgramsOutsideTheSelection()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/ACCOUNTS.cbl");

        Stage("finance/ACCOUNTS.cbl");

        File.Exists(Path.Combine(StagingDir, "archive", "ACCOUNTS.cbl")).Should().BeFalse();
    }

    // COPY statements resolve by basename, so copybooks stage flat regardless of source layout.
    [Fact]
    public void StagesEveryCopybookFlatSoCopyStatementsStillResolve()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteCopybook("shared/copy/CUSTOMER.cpy");

        var staged = Stage("finance/ACCOUNTS.cbl");

        staged.Copybooks.Should().Be(1);
        File.Exists(Path.Combine(StagingDir, "CUSTOMER.cpy")).Should().BeTrue();
    }

    // Without the prune a previous run's staged copies re-enter the catalog and make every
    // basename ambiguous, which would refuse selectors that are genuinely unique in the source.
    [Fact]
    public void IgnoresHiddenDirectoriesSoAPreviousScopeCannotContributeDuplicates()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteCopybook("shared/CUSTOMER.cpy");
        WriteCopybook(".conversion-staging/CUSTOMER.cpy");

        var staged = Stage("finance/ACCOUNTS.cbl");

        staged.Copybooks.Should().Be(1);
    }

    [Fact]
    public void RestagingReplacesThePreviousScopeRatherThanAccumulating()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("finance/LEDGER.cbl");

        Stage("finance/ACCOUNTS.cbl");
        Stage("finance/LEDGER.cbl");

        File.Exists(Path.Combine(StagingDir, "finance", "LEDGER.cbl")).Should().BeTrue();
        File.Exists(Path.Combine(StagingDir, "finance", "ACCOUNTS.cbl")).Should().BeFalse();
    }

    // A refused selector must not leave the previous scope in place, or the next run silently
    // converts the stale selection while reporting the new one.
    [Fact]
    public void RefusedSelectorLeavesNoScopeBehind()
    {
        WriteProgram("finance/ACCOUNTS.cbl");
        WriteProgram("archive/ACCOUNTS.cbl");
        Stage("finance/ACCOUNTS.cbl");

        var refuse = () => Stage("ACCOUNTS.cbl");

        refuse.Should().Throw<InvalidOperationException>();
        Directory.Exists(StagingDir).Should().BeFalse();
    }

    private ConversionScopeStagingResult Stage(string selector) =>
        new ConversionScopeStager(SourceDir, StagingDir).Stage(
            new ProgramSelection { Programs = [selector] },
            Path.Combine(_root, "facts"));

    private void WriteProgram(string relativePath) => Write(relativePath, "       IDENTIFICATION DIVISION.\n");

    private void WriteCopybook(string relativePath) => Write(relativePath, "       01 REC PIC X.\n");

    private void Write(string relativePath, string content)
    {
        var path = Path.Combine(SourceDir, relativePath.Replace('/', Path.DirectorySeparatorChar));
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, content);
    }

    public void Dispose()
    {
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }
}
