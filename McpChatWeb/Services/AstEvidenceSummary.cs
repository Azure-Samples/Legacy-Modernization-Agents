namespace McpChatWeb.Services;

// A graph projection, not a substitute for the strict dependency evidence gate.
public sealed record AstDiagnosticDetail(
    string Category, string Code, string? Item, string? Location,
    string Reason, string Action, string Raw);

public sealed record AstEvidenceSummary(
    string DependencyReadiness, string ParserStatus, IReadOnlyList<AstDiagnosticDetail> Diagnostics)
{
    public string DraftEligibility => "requires-local-evidence-assessment";
    public string MissingDraftEvidence => "Current original source, export and definition hashes/provenance, complete dependency-domain coverage and selected-callee isolation are not validated by this graph projection.";
    public string DraftAction => "Request /api/estate/draft-plan with this scanRunId. If server-approved snapshot input/evidence binding is unavailable, assessment is refused rather than using configured defaults. Opt-in draft isolates whole justified programs; Routine-level usage/control-flow evidence is not recorded.";

    public static AstEvidenceSummary FromRecorded(bool reportAvailable, IReadOnlyList<string> diagnostics)
        => new(diagnostics.Count > 0 ? "issues-recorded" : reportAvailable ? "not-established" : "not-recorded",
            "not-established", diagnostics.Select(Describe).ToArray());

    private static AstDiagnosticDetail Describe(string raw)
    {
        if (CobolToQuarkusMigration.Agents.Infrastructure.Facts.DependencyBlockerDescriptions.DescribeDiagnostic(raw) is { } shared)
            return new(shared.Category, shared.Code, shared.Item, null, shared.Reason, shared.Action, raw);
        var value = raw.StartsWith("native:", StringComparison.Ordinal) ? raw["native:".Length..] : raw;
        var parts = value.Split(':', 2);
        var code = parts[0];
        var item = parts.Length > 1 && parts[1].Length > 0 ? parts[1] : null;
        var (category, reason, action) = code switch
        {
            "unresolved-copybook" => ("missing-copybook", "COPY input could not be resolved.",
                "Supply the real copybook and configure its include/search path; rerun dependency analysis into a new snapshot."),
            "source-not-found" => ("missing-input", "A required source input was not found.",
                "Supply the real source input in the configured source/search roots and rescan."),
            "copy-not-exported" or "nested-copy-not-exported" or "call-not-exported"
                or "definition-resolution-not-exported" => ("unverified-input", "A source reference has no exported resolution evidence; it is not proof the input is absent.",
                "Check the real input and search paths, then regenerate the dependency export and evidence."),
            "unverified-copybook" or "unverified-staged-copybook" or "native-resolved-definition-unverified"
                or "native-export-source-unverified" or "unverified-staged-source"
                => ("unverified-input", "Input identity or provenance was not verified.",
                    "Verify the original source, staging provenance and hashes; regenerate evidence for these exact inputs."),
            "ambiguous-copybook" or "ambiguous-call-target"
                => ("unverified-input", "More than one input may match this reference.",
                    "Disambiguate the source/search paths and regenerate the dependency evidence."),
            "parser-diagnostic" => ("parser", "The parser recorded a diagnostic; its code and message are preserved below. Severity is not recorded here.",
                "Inspect the parser diagnostic and source location if supplied; fix the syntax, dialect configuration or input indicated, then rescan."),
            "invalid-parser-diagnostics" => ("not-recorded", "The parser diagnostic report could not be read.",
                "Regenerate a valid parser diagnostic report; absence of readable diagnostics is not parser success."),
            "dependency-export-failed" or "dependency-analysis-failed" or "dependency-export-invalid"
                => ("analysis-failed", "Dependency analysis/export failed; this does not establish that the source or its AST is unusable.",
                    "Inspect the scan/export logs, correct the reported parser or exporter failure, then regenerate evidence."),
            "dynamic-CALL-target-unresolved" or "dynamic-call-target-unresolved"
                => ("unverified-input", "A dynamic CALL target is unresolved, not a confirmed missing COPY input.",
                    "Supply verified target resolution evidence for the dynamic call; do not infer the callee from AST structure."),
            "callee" => ("unverified-input", "A callee has a recorded diagnostic; the original diagnostic chain is preserved below.",
                "Inspect the callee and its report in this snapshot, resolve the nested issue and regenerate the caller's dependency evidence."),
            _ when code.StartsWith("unsupported-", StringComparison.Ordinal)
                => ("unsupported", "The exporter recorded unsupported dependency coverage.",
                    "Use a parser/exporter supporting this construct and regenerate evidence; do not replace real definitions with stubs."),
            _ when code.Contains("coverage", StringComparison.Ordinal) || code.Contains("absence-proof", StringComparison.Ordinal)
                || code.Contains("incomplete-callee", StringComparison.Ordinal)
                => ("not-recorded", "Required dependency coverage or absence evidence is incomplete or not declared.",
                    "Regenerate complete dependency evidence, including callees, with a supported exporter."),
            _ => ("other", "A diagnostic was recorded; its meaning has not been classified by the portal.",
                "Inspect the exact diagnostic and dependency-evidence.json / parse-diagnostics.json for this snapshot before retrying.")
        };
        // These graph properties contain diagnostic strings, not structured source locations.
        return new(category, code, item, null, reason, action, raw);
    }
}
