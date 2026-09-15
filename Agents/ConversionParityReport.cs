using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Agents;

// Public so McpChatWeb can deserialise conversion-parity.json; the validator itself is internal.

[JsonConverter(typeof(JsonStringEnumConverter))]
public enum ParityGapKind
{
    // Symbol appears nowhere in the generated file.
    Missing,

    // Symbol appears only in comments or string literals. A renamed or inlined procedure
    // characteristically survives this way, so it is reported but not scored as a loss.
    PossiblyRenamedOrMerged,
}

[JsonConverter(typeof(JsonStringEnumConverter))]
public enum ParityOutcome
{
    Evaluated,
    NotEvaluated,
}

public sealed record ParityGap
{
    [JsonPropertyName("axis")] public required string Axis { get; init; }
    [JsonPropertyName("symbol")] public required string Symbol { get; init; }
    [JsonPropertyName("kind")] public required ParityGapKind Kind { get; init; }
    [JsonPropertyName("detail")] public string? Detail { get; init; }
}

public sealed record ParityAxisResult
{
    [JsonPropertyName("name")] public required string Name { get; init; }
    [JsonPropertyName("weight")] public required double Weight { get; init; }

    // Symbols actually scored: total minus excluded.
    [JsonPropertyName("expected")] public int Expected { get; init; }
    [JsonPropertyName("matchedInCode")] public int MatchedInCode { get; init; }
    [JsonPropertyName("matchedInCommentsOnly")] public int MatchedInCommentsOnly { get; init; }
    [JsonPropertyName("missing")] public int Missing { get; init; }

    // Symbols dropped before scoring (FILLER, level 88, names too short to match safely).
    [JsonPropertyName("excluded")] public int Excluded { get; init; }

    // null when Expected is 0: the axis is absent, not perfect.
    [JsonPropertyName("coverage")] public double? Coverage { get; init; }

    // Expected symbols, none of them in code. Renormalising the weighted score can otherwise
    // leave an all-or-nothing axis failure above the threshold, so the gate reads this too.
    [JsonPropertyName("totalLoss")] public bool IsTotalLoss => Expected > 0 && MatchedInCode == 0;

    [JsonPropertyName("note")] public string? Note { get; init; }
}

public sealed record ProgramParityResult
{
    [JsonPropertyName("program")] public required string Program { get; init; }
    [JsonPropertyName("generatedFile")] public string? GeneratedFile { get; init; }

    [JsonPropertyName("outcome")] public required ParityOutcome Outcome { get; init; }
    [JsonPropertyName("notEvaluatedReason")] public string? NotEvaluatedReason { get; init; }

    [JsonPropertyName("provenance")] public string? Provenance { get; init; }
    [JsonPropertyName("structuralConfidence")] public double? StructuralConfidence { get; init; }

    // null whenever Outcome is NotEvaluated. Never defaulted to a passing value.
    [JsonPropertyName("score")] public double? Score { get; init; }

    [JsonPropertyName("isDiagnosticStub")] public bool IsDiagnosticStub { get; init; }

    // Symbols dropped because the evidence behind them is synthetic. A score computed with these
    // excluded measures less than a clean one, so the omission is reported rather than absorbed.
    [JsonPropertyName("evidenceNotes")] public IReadOnlyList<string> EvidenceNotes { get; init; }
        = Array.Empty<string>();

    // Axes whose symbols are entirely absent from code. A program with any of these has failed
    // parity whatever its weighted score, so it is surfaced next to the score, not buried.
    [JsonPropertyName("lostAxes")] public IReadOnlyList<string> LostAxes =>
        Axes.Where(a => a.IsTotalLoss).Select(a => a.Name).ToList();

    // Stamped by the post-pass once the threshold is known. The portal reads this rather than
    // re-deriving the verdict from the score, which cannot see total axis loss.
    [JsonPropertyName("failed")] public bool Failed { get; init; }

    [JsonPropertyName("axes")] public IReadOnlyList<ParityAxisResult> Axes { get; init; }
        = Array.Empty<ParityAxisResult>();

    [JsonPropertyName("gaps")] public IReadOnlyList<ParityGap> Gaps { get; init; }
        = Array.Empty<ParityGap>();
}

public sealed record ConversionParityReport
{
    public const int CurrentSchemaVersion = 1;

    [JsonPropertyName("schemaVersion")] public int SchemaVersion { get; init; } = CurrentSchemaVersion;
    [JsonPropertyName("generatedAtUtc")] public DateTimeOffset GeneratedAtUtc { get; init; }
    [JsonPropertyName("targetLanguage")] public string TargetLanguage { get; init; } = "";
    [JsonPropertyName("threshold")] public double Threshold { get; init; }
    [JsonPropertyName("onLowScore")] public string OnLowScore { get; init; } = "warn";

    [JsonPropertyName("programs")] public IReadOnlyList<ProgramParityResult> Programs { get; init; }
        = Array.Empty<ProgramParityResult>();

    [JsonPropertyName("evaluatedCount")] public int EvaluatedCount { get; init; }
    [JsonPropertyName("notEvaluatedCount")] public int NotEvaluatedCount { get; init; }
    [JsonPropertyName("belowThresholdCount")] public int BelowThresholdCount { get; init; }

    // null when nothing could be evaluated, so an empty run never reads as a clean run.
    [JsonPropertyName("averageScore")] public double? AverageScore { get; init; }

    // Set by the portal reader, not persisted by the writer. Ignoring it unconditionally would
    // also strip it from the API response, leaving the panel with no report path to show.
    [JsonPropertyName("sourcePath")]
    [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
    public string? SourcePath { get; set; }
}
