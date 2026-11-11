using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

public class AstEvidenceSummaryTests
{
    [Theory]
    [InlineData(false, "not-recorded")]
    [InlineData(true, "not-established")]
    public void EmptyDiagnosticsNeverEstablishReadinessOrParserSuccess(bool reportAvailable, string readiness)
    {
        var summary = AstEvidenceSummary.FromRecorded(reportAvailable, []);
        Assert.Equal(readiness, summary.DependencyReadiness);
        Assert.Equal("not-established", summary.ParserStatus);
        Assert.Empty(summary.Diagnostics);
        Assert.Equal("requires-local-evidence-assessment", summary.DraftEligibility);
        Assert.Contains("hashes/provenance", summary.MissingDraftEvidence);
        Assert.Contains("Routine-level", summary.DraftAction);
    }

    [Theory]
    [InlineData("native:unresolved-copybook:PAYREC", "missing-copybook", "PAYREC")]
    [InlineData("source-not-found:SUBPROG", "missing-input", "SUBPROG")]
    [InlineData("copy-not-exported:PAYREC", "unverified-input", "PAYREC")]
    [InlineData("native:unsupported-file-io-dependency-contexts:OpenContext", "unsupported", "OpenContext")]
    [InlineData("parser-diagnostic:syntax:orders.cbl:42 unexpected token", "parser", "syntax:orders.cbl:42 unexpected token")]
    [InlineData("parser-diagnostic:missing copybook:ERROR-CODES: Copybook not found", "missing-copybook", "ERROR-CODES")]
    [InlineData("native:parser-diagnostic:missing copybook:ERROR-CODES: Copybook not found", "missing-copybook", "ERROR-CODES")]
    [InlineData("parser-diagnostic:None:Copybook declaration has more than 8 characters for: CUSTOMER-DATA", "parser", "None:Copybook declaration has more than 8 characters for: CUSTOMER-DATA")]
    [InlineData("native-export-source-unverified:hash mismatch", "unverified-input", "hash mismatch")]
    [InlineData("dependency-export-failed:1", "export-failure", null)]
    [InlineData("dependency-analysis-failed:timeout", "analysis-failed", "timeout")]
    [InlineData("exporter-coverage-not-declared:COPY,CALL", "not-recorded", "COPY,CALL")]
    [InlineData("new-exporter-condition:details", "other", "details")]
    public void ClassifiesOnlyRecordedMeaningAndPreservesExactDiagnostic(string raw, string category, string? item)
    {
        var summary = AstEvidenceSummary.FromRecorded(true, [raw]);
        var detail = Assert.Single(summary.Diagnostics);
        Assert.Equal("issues-recorded", summary.DependencyReadiness);
        Assert.Equal(category, detail.Category);
        Assert.Equal(item, detail.Item);
        Assert.Equal(raw, detail.Raw);
        Assert.Null(detail.Location);
        Assert.NotEmpty(detail.Reason);
        Assert.NotEmpty(detail.Action);
    }

    [Fact]
    public void UnreadableCopybookNamesTheInputAndProviderFailureWithoutClaimingTheFileIsAbsent()
    {
        const string raw = "native:unreadable-copybook:SQLCA:Provider \"implicit\" not installed";
        var detail = Assert.Single(AstEvidenceSummary.FromRecorded(true, [raw]).Diagnostics);
        Assert.Equal("unreadable-copybook", detail.Category);
        Assert.Equal("SQLCA", detail.Item);
        Assert.Equal(raw, detail.Raw);
        Assert.Null(detail.Location);
        Assert.Contains("not proof that the real file is absent", detail.Reason);
        Assert.Contains("Provider \"implicit\" not installed", detail.Reason);
        Assert.Contains("install or configure the required provider", detail.Action);
    }
}
