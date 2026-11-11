using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Facts;

public sealed class DependencyBlockerDescriptionsTests
{
    [Theory]
    [InlineData("native:UNRESOLVED_CICS_AID_SEMANTIC:DFHAID:453")]
    [InlineData("native-platform-contract-unverified:DFHAID:UNRESOLVED_CICS_AID_SEMANTIC: unsupported standalone DFHAID comparison")]
    public void AmbiguousAidSemanticsRemainUnresolvedWithoutSuggestingAReplacementKey(string diagnostic)
    {
        var blocker = DependencyBlockerDescriptions.Describe("MAIN.cbl", "MAIN.cbl: unresolved dependency: " + diagnostic);
        blocker.MissingEvidence.Should().Be("UNRESOLVED_CICS_AID_SEMANTIC");
        blocker.DiagnosticCategory.Should().Be("unresolved-semantics");
        blocker.Reason.Should().Contain("original condition is preserved");
        blocker.Action.Should().Contain("Do not remove DFHAID").And.Contain("PF12-only");
    }

    [Fact]
    public void UnreadableCopyHashFailureDoesNotPrescribeInstallingAProvider()
    {
        var blocker = DependencyBlockerDescriptions.Describe("MAIN.cbl",
            "MAIN.cbl: unresolved dependency: native:unreadable-copybook:SQLCA:SHA256 hash mismatch");
        blocker.Category.Should().Be("artifact-blocker");
        blocker.MissingEvidence.Should().Contain("SQLCA");
        blocker.Action.Should().Contain("original bytes").And.Contain("SHA256")
            .And.NotContain("provider").And.NotContain("install");
    }

    [Theory]
    [InlineData("native:unresolved-copybook:PAYREC", "Resolved real COPY input PAYREC", "shared fields", "Supply the real COPY PAYREC")]
    [InlineData("native:callee:FORMAT-BALANCE:source-not-found:FORMAT-BALANCE", "CALL target FORMAT-BALANCE", "could not locate", "Supply the real source for FORMAT-BALANCE")]
    [InlineData("native:unreadable-copybook:SQLCA:Provider \"implicit\" not installed", "Readable COPY SQLCA", "not proof that the real file is absent", "install or configure the required provider 'implicit'")]
    [InlineData("native:unsupported-CICS-dependency-contexts:ExecCicsReceiveContext", "EXEC_CICS contexts: ExecCicsReceiveContext", "cannot establish EXEC_CICS", "resource/target bindings")]
    [InlineData("native:unsupported-file-io-dependency-contexts:OpenContext", "FILE_IO contexts: OpenContext", "cannot establish FILE_IO", "SELECT/FD bindings")]
    [InlineData("native:cics-control-transfer-requires-runtime-mapping:RETURN:bare:parser-line=39", "CICS control transfer RETURN:bare:parser-line=39", "not a missing copybook or proof of Spring equivalence", "implement and behavior-test its target-runtime mapping")]
    [InlineData("native:callee:TARGET:cics-control-transfer-requires-runtime-mapping:RETURN:options:parser-line=10", "CALL target TARGET", "returns control to CICS", "Preserve the recorded CICS RETURN control flow and any options")]
    public void DescribesRecordedDependencyProblemSpecificallyAndPreservesRaw(string diagnostic, string missing, string reason, string action)
    {
        var raw = "MAIN.cbl: unresolved dependency: " + diagnostic;
        var blocker = DependencyBlockerDescriptions.Describe("MAIN.cbl", raw);
        blocker.MissingEvidence.Should().Contain(missing).And.NotBe(raw);
        blocker.Reason.Should().Contain(reason);
        blocker.Action.Should().Contain(action);
        blocker.Raw.Should().Be(raw);
        blocker.Routine.Should().BeNull();
        blocker.Location.Should().BeNull();
    }

    [Theory]
    [InlineData("native:unresolved-EXEC_CICS-dependency-bindings:Cics_newContext", "binding-gap", "does not establish a missing file")]
    [InlineData("native:sql-declaration-not-found:SQL_RELATION:CONTROL", "missing-declaration", "catalog file can be present")]
    [InlineData("native:cics-declaration-not-found:CICS_DATA:BUFFER", "missing-declaration", "No matching declaration")]
    [InlineData("native:cics-map-definition-not-found:BNK1CCM:BNK1CCM", "missing-declaration", "does not imply a missing BMS file")]
    [InlineData("native:cics-map-definition-ambiguous:SCREEN:PANEL", "binding-gap", "Multiple supported BMS declarations")]
    [InlineData("native:original-operation-not-exported:DFHVALUE(ABEND)", "unsupported", "not proof that an input file is missing")]
    [InlineData("native:partial-derived-bms-layout:SCREEN.cpy", "derived-layout", "not a missing vendor copybook")]
    [InlineData("dependency-export-failed:1", "export-failure", "does not identify a missing file")]
    [InlineData("native:cics-control-transfer-requires-runtime-mapping:RETURN", "runtime-mapping", "parser recognized")]
    public void PreservesReadableCauseClassificationWithoutChangingEvidencePolicy(string diagnostic, string category, string reason)
    {
        var blocker = DependencyBlockerDescriptions.Describe("MAIN.cbl", "MAIN.cbl: unresolved dependency: " + diagnostic);
        blocker.DiagnosticCategory.Should().Be(category);
        (blocker.Reason + " " + blocker.Action).Should().Contain(reason);
        blocker.Category.Should().Be("coverage-gap");
    }

    [Theory]
    [InlineData("COPY", "nested COPY references")]
    [InlineData("CALL", "transitive callee dependencies")]
    [InlineData("EXEC_SQL", "host-variable bindings")]
    [InlineData("EXEC_CICS", "resource/target bindings")]
    [InlineData("FILE_IO", "SELECT/FD bindings")]
    public void PartialCoverageRequiresDomainSpecificEvidenceNotAnInventedMissingInput(string domain, string required)
    {
        var blocker = DependencyBlockerDescriptions.Describe("MAIN.cbl",
            $"MAIN.cbl: {domain} coverage is partial; explicit complete parser evidence is required.");
        blocker.Category.Should().Be("coverage-gap");
        blocker.MissingEvidence.Should().Contain(domain).And.Contain(required);
        blocker.Action.Should().Contain(required).And.Contain("explicit absence proofs");
        blocker.Action.Should().NotContain("Supply the real COPY").And.NotContain("configure the required provider");
    }

    [Theory]
    [InlineData("original source missing or SHA256 mismatch.", "original source", "recorded SHA256")]
    [InlineData("COPY definition PAYREC missing or SHA256 mismatch.", "PAYREC", "recorded SHA256")]
    [InlineData("source-relative identity or original source path mismatch.", "originalSourcePath", "different source path")]
    [InlineData("missing dependency-evidence.json; legacy High confidence is not dependency coverage.", "MAIN.cbl.report/dependency-evidence.json", "No dependency-evidence artifact")]
    public void ArtifactFailuresHaveArtifactSpecificRemediesNotProviderConfiguration(string diagnostic, string missing, string reason)
    {
        var blocker = DependencyBlockerDescriptions.Describe("MAIN.cbl", "MAIN.cbl: " + diagnostic);
        blocker.Category.Should().Be("artifact-blocker");
        blocker.MissingEvidence.Should().Contain(missing);
        (blocker.MissingEvidence + blocker.Reason).Should().Contain(reason);
        blocker.Action.Should().NotContain("install or configure");
    }
}
