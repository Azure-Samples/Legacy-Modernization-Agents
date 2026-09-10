using System.Diagnostics;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// Focused conversion works by staging a subset and repointing --source at it, so staging is the
// step that decides what actually gets converted. These drive doctor.sh's real function.
public sealed class ConversionStagingTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(), "conversion-staging-" + Guid.NewGuid().ToString("N"));

    private string SourceDir => Path.Combine(_root, "source");

    private string StagingDir => Path.Combine(_root, "source", ".conversion-staging");

    private string FactsDir => Path.Combine(_root, "facts");

    private string ManifestPath => Path.Combine(_root, "conversion-selection.json");

    [Fact]
    public void NoSelectorConvertsTheWholeEstateFromTheSourceDirectory()
    {
        WriteProgram("finance/LEDGER.cbl");
        WriteProgram("billing/INVOICE.cbl");

        var result = StageConversionScope(selector: "");

        result.ExitCode.Should().Be(0);
        result.SelectedSourceDir.Should().Be(SourceDir,
            "an unfiltered run must convert from the estate itself, not a staged copy");
        Directory.Exists(StagingDir).Should().BeFalse();
    }

    [Fact]
    public void SelectorStagesOnlyTheSelectedProgramAndKeepsItsRelativePath()
    {
        WriteProgram("finance/LEDGER.cbl");
        WriteProgram("billing/INVOICE.cbl");

        var result = StageConversionScope(selector: "LEDGER");

        result.ExitCode.Should().Be(0);
        result.SelectedSourceDir.Should().Be(StagingDir);
        StagedPrograms().Should().Equal(["finance/LEDGER.cbl"],
            "staging flat would collide same-named programs and lose source identity");
    }

    // Copybooks are COPY context for whichever program was selected, so dropping them would
    // convert a program whose data definitions no longer resolve.
    [Fact]
    public void SelectorStagesEveryCopybookAsCopyContext()
    {
        WriteProgram("finance/LEDGER.cbl");
        WriteProgram("billing/INVOICE.cbl");
        WriteCopybook("ACCOUNT.cpy");
        WriteCopybook("shared/CUSTOMER.cpy");

        var result = StageConversionScope(selector: "LEDGER");

        result.ExitCode.Should().Be(0);
        StagedCopybooks().Should().BeEquivalentTo(["ACCOUNT.cpy", "CUSTOMER.cpy"],
            "COPY targets resolve by basename, so copybooks stage flat at the staging root");
    }

    [Fact]
    public void ClosureStagesCalleesAlongsideTheSelectedProgram()
    {
        WriteProgram("finance/LEDGER.cbl");
        WriteProgram("shared/POSTING.cbl");
        WriteProgram("billing/INVOICE.cbl");
        WriteFacts("finance/LEDGER.cbl", callees: ["shared/POSTING.cbl"]);
        WriteFacts("shared/POSTING.cbl");

        var result = StageConversionScope(selector: "LEDGER", includeCallees: true);

        result.ExitCode.Should().Be(0);
        StagedPrograms().Should().BeEquivalentTo("finance/LEDGER.cbl", "shared/POSTING.cbl");
    }

    [Fact]
    public void AmbiguousSelectorRefusesAndLeavesNoStagedEstateBehind()
    {
        WriteProgram("finance/SHARED.cbl");
        WriteProgram("billing/SHARED.cbl");

        var result = StageConversionScope(selector: "SHARED.cbl");

        result.ExitCode.Should().NotBe(0);
        result.Output.Should().Contain("source-relative path");
        Directory.Exists(StagingDir).Should().BeFalse(
            "a half-staged estate would silently convert whichever program happened to be copied");
    }

    // Re-running with a narrower selector must not convert last run's leftovers.
    [Fact]
    public void RestagingReplacesThePreviousSelectionRatherThanAccumulating()
    {
        WriteProgram("finance/LEDGER.cbl");
        WriteProgram("billing/INVOICE.cbl");

        StageConversionScope(selector: "LEDGER").ExitCode.Should().Be(0);
        StageConversionScope(selector: "INVOICE").ExitCode.Should().Be(0);

        StagedPrograms().Should().Equal("billing/INVOICE.cbl");
    }

    [Fact]
    public void SelectorRunEnablesSelectorModeSoCopybooksAreNotConvertedStandalone()
    {
        WriteProgram("finance/LEDGER.cbl");

        var result = StageConversionScope(selector: "LEDGER");

        result.ExitCode.Should().Be(0);
        result.SelectorMode.Should().Be("true");
    }

    [Fact]
    public void UnfilteredRunLeavesSelectorModeOff()
    {
        WriteProgram("finance/LEDGER.cbl");

        var result = StageConversionScope(selector: "");

        result.ExitCode.Should().Be(0);
        result.SelectorMode.Should().NotBe("true");
    }

    [Fact]
    public void SelectorRunRecordsWhatWasSelectedAndWhy()
    {
        WriteProgram("finance/LEDGER.cbl");
        WriteProgram("shared/POSTING.cbl");
        WriteFacts("finance/LEDGER.cbl", callees: ["POSTING"]);
        WriteFacts("shared/POSTING.cbl");

        var result = StageConversionScope(selector: "LEDGER", includeCallees: true);

        result.ExitCode.Should().Be(0);
        File.Exists(ManifestPath).Should().BeTrue(
            "a focused run is only reproducible if the scope it used is recorded");

        using var manifest = JsonDocument.Parse(File.ReadAllText(ManifestPath));
        var root = manifest.RootElement;

        root.GetProperty("programs").EnumerateArray().Select(p => p.GetString())
            .Should().BeEquivalentTo(["finance/LEDGER.cbl", "shared/POSTING.cbl"]);

        var reasons = root.GetProperty("matches").EnumerateArray()
            .ToDictionary(m => m.GetProperty("program").GetString()!, m => m.GetProperty("reason").GetString()!);

        reasons["finance/LEDGER.cbl"].Should().Contain("LEDGER");
        reasons["shared/POSTING.cbl"].Should().Contain("called by",
            "the manifest has to distinguish what was asked for from what closure pulled in");

        root.GetProperty("selectors").GetProperty("includeCallees").GetBoolean().Should().BeTrue();
    }

    [Fact]
    public void RefusedSelectorWritesNoManifest()
    {
        WriteProgram("finance/LEDGER.cbl");
        WriteProgram("billing/LEDGER.cbl");

        var result = StageConversionScope(selector: "LEDGER");

        result.ExitCode.Should().NotBe(0);
        File.Exists(ManifestPath).Should().BeFalse(
            "a manifest for a scope that was never converted would misreport the run");
    }

    [Fact]
    public void UnfilteredRunWritesNoManifest()
    {
        WriteProgram("finance/LEDGER.cbl");

        var result = StageConversionScope(selector: "");

        result.ExitCode.Should().Be(0);
        File.Exists(ManifestPath).Should().BeFalse(
            "a whole-estate run has no selection to record");
    }

    private IReadOnlyList<string> StagedPrograms() => StagedFiles(".cbl", ".cob");

    private IReadOnlyList<string> StagedCopybooks() => StagedFiles(".cpy");

    private IReadOnlyList<string> StagedFiles(params string[] extensions)
    {
        if (!Directory.Exists(StagingDir)) return Array.Empty<string>();

        return Directory.EnumerateFiles(StagingDir, "*", SearchOption.AllDirectories)
            .Where(path => extensions.Contains(Path.GetExtension(path), StringComparer.OrdinalIgnoreCase))
            .Select(path => Path.GetRelativePath(StagingDir, path).Replace(Path.DirectorySeparatorChar, '/'))
            .OrderBy(path => path, StringComparer.Ordinal)
            .ToList();
    }

    private void WriteProgram(string relativePath) =>
        WriteSourceFile(relativePath, $"       PROGRAM-ID. {Path.GetFileNameWithoutExtension(relativePath)}.\n");

    private void WriteCopybook(string relativePath) =>
        WriteSourceFile(relativePath, "       01 RECORD-AREA PIC X(80).\n");

    private void WriteSourceFile(string relativePath, string content)
    {
        var path = Path.Combine(SourceDir, relativePath.Replace('/', Path.DirectorySeparatorChar));
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, content);
    }

    private void WriteFacts(string relativePath, IReadOnlyList<string>? callees = null)
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
            Callers = Array.Empty<string>(),
        };

        var path = ProgramFactsArtifactLocator.GetFactsFilePath(FactsDir, relativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, JsonSerializer.Serialize(facts));
    }

    private (int ExitCode, string SelectedSourceDir, string SelectorMode, string Output) StageConversionScope(
        string selector, bool includeCallers = false, bool includeCallees = false)
    {
        var repoRoot = LocateRepoRoot();
        var script = $$"""
            set -euo pipefail
            DOCTOR_SOURCE_ONLY=1 source "{{Path.Combine(repoRoot, "doctor.sh")}}"
            stage_conversion_scope "{{SourceDir}}" "{{StagingDir}}" "{{selector}}" \
                "{{includeCallers.ToString().ToLowerInvariant()}}" \
                "{{includeCallees.ToString().ToLowerInvariant()}}" \
                "{{FactsDir}}" "{{ManifestPath}}"
            echo "SELECTOR_MODE=${SELECTOR_MODE:-}"
            """;

        var psi = new ProcessStartInfo("bash")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = repoRoot,
        };
        psi.ArgumentList.Add("-c");
        psi.ArgumentList.Add(script);

        using var process = Process.Start(psi)!;
        var stdout = process.StandardOutput.ReadToEnd();
        var stderr = process.StandardError.ReadToEnd();
        process.WaitForExit(180_000).Should().BeTrue("staging should not hang");

        var lines = stdout.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            .Select(line => line.TrimEnd('\r'))
            .Where(line => line.Length > 0)
            .ToList();

        var selectorMode = lines
            .LastOrDefault(line => line.StartsWith("SELECTOR_MODE=", StringComparison.Ordinal))
            ?["SELECTOR_MODE=".Length..] ?? "";

        var selectedSourceDir = lines
            .LastOrDefault(line => !line.StartsWith("SELECTOR_MODE=", StringComparison.Ordinal)) ?? "";

        return (process.ExitCode, selectedSourceDir, selectorMode, stdout + stderr);
    }

    private static string LocateRepoRoot()
    {
        var dir = new DirectoryInfo(AppContext.BaseDirectory);
        while (dir is not null && !File.Exists(Path.Combine(dir.FullName, "doctor.sh")))
            dir = dir.Parent;

        dir.Should().NotBeNull("the tests should run from inside the repository");
        return dir!.FullName;
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }
}
