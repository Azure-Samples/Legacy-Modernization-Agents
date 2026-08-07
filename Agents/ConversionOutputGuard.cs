namespace CobolToQuarkusMigration.Agents;

using System.Text;

internal static class ConversionOutputGuard
{
    internal static string ExtractFencedCode(string input, params string[] languageMarkers)
    {
        foreach (var marker in languageMarkers)
        {
            var startIndex = input.IndexOf(marker, StringComparison.OrdinalIgnoreCase);
            if (startIndex < 0)
            {
                continue;
            }

            startIndex += marker.Length;
            var endIndex = input.IndexOf("```", startIndex, StringComparison.Ordinal);
            return (endIndex >= 0
                    ? input[startIndex..endIndex]
                    : input[startIndex..])
                .Trim();
        }

        return input;
    }

    internal static bool IsUsableChunk(
        string? code,
        string primaryStructure,
        string secondaryStructure,
        string language,
        bool requireStructure,
        out string reason)
    {
        if (string.IsNullOrWhiteSpace(code))
        {
            reason = "EMPTY_LLM_RESPONSE — model returned no usable output (likely provider timeout / 0-token response). Re-run with ENABLE_REKT_CONTEXT=true and a smaller chunk threshold.";
            return false;
        }

        if (requireStructure)
        {
            var hasPrimaryStructure = code.Contains(primaryStructure, StringComparison.Ordinal);
            var hasSecondaryStructure = code.Contains(secondaryStructure, StringComparison.Ordinal);
            var opens = code.Count(c => c == '{');
            var closes = code.Count(c => c == '}');

            if ((!hasPrimaryStructure && !hasSecondaryStructure) || (opens == 0 && closes == 0))
            {
                var reasonLanguage = language.Equals("C#", StringComparison.OrdinalIgnoreCase)
                    ? "CSHARP"
                    : language.ToUpperInvariant();
                reason = $"NO_{reasonLanguage}_STRUCTURE — model emitted prose or non-{language} content. Re-run with full REKT context enabled.";
                return false;
            }
        }

        reason = string.Empty;
        return true;
    }

    internal static string EscapeBlockCommentContent(string? content)
    {
        return string.IsNullOrWhiteSpace(content)
            ? "(no output)"
            : content.Replace("*/", "* /", StringComparison.Ordinal);
    }

    internal static bool ShouldCreateWholeFileStub(
        string code,
        bool hasClass,
        int openingBraces,
        int closingBraces)
    {
        return !hasClass
            || code.Trim().Length < 40
            || openingBraces != closingBraces;
    }

    internal static string BuildChunkDiagnosticStub(
        string language,
        string sourceFile,
        int chunkIndex,
        int totalChunks,
        int startLine,
        int endLine,
        string reason)
    {
        var reportStem = Path.GetFileNameWithoutExtension(sourceFile);
        var stub = new StringBuilder();
        stub.AppendLine("// ═════════════════════════════════════════════════════════════════════");
        stub.AppendLine($"// ⚠ CHUNK CONVERSION DID NOT PRODUCE USABLE {language}");
        stub.AppendLine("// ═════════════════════════════════════════════════════════════════════");
        stub.AppendLine($"// Source COBOL: {sourceFile}");
        stub.AppendLine($"// Chunk: {chunkIndex + 1}/{totalChunks} (lines {startLine}-{endLine})");
        stub.AppendLine($"// Reason: {reason}");
        stub.AppendLine("//");
        stub.AppendLine("// What to do");
        stub.AppendLine("// ──────────");
        stub.AppendLine("// 1. Verify ENABLE_REKT_CONTEXT=true in the environment.");
        stub.AppendLine("// 2. Confirm full-fidelity REKT artifacts exist under");
        stub.AppendLine($"//    output/rekt/{reportStem}.cbl.report/");
        stub.AppendLine("// 3. Re-run the conversion for just this program.");
        stub.AppendLine("// ═════════════════════════════════════════════════════════════════════");
        return stub.ToString();
    }

    internal static string BuildWholeFileDiagnosticStub(
        string language,
        string reason,
        string originalOutput)
    {
        var stub = new StringBuilder();
        stub.AppendLine("// ═════════════════════════════════════════════════════════════════════");
        stub.AppendLine($"// ⚠ CONVERSION DID NOT PRODUCE USABLE {language}");
        stub.AppendLine("// ═════════════════════════════════════════════════════════════════════");
        stub.AppendLine($"// This file is a placeholder because the model did not return a usable {language} class.");
        stub.AppendLine("// The pipeline keeps this file so the failed conversion remains visible.");
        stub.AppendLine("//");
        stub.AppendLine($"// Reason: {reason}");
        stub.AppendLine("//");
        stub.AppendLine("// What to do");
        stub.AppendLine("// ──────────");
        stub.AppendLine("// 1. Check the 'Unusable Conversions' table in migration-report.md.");
        stub.AppendLine("// 2. Resolve any missing copybooks, run './doctor.sh rekt-full', and re-convert.");
        stub.AppendLine("// 3. For 0-token responses, retry with chunking or a provider with a higher output budget.");
        stub.AppendLine("// 4. Review migration-conversation-log.md for the raw prompt and response.");
        stub.AppendLine("// ═════════════════════════════════════════════════════════════════════");
        stub.AppendLine();
        stub.AppendLine("// Original output is preserved below for debugging.");
        stub.AppendLine("/*");
        stub.AppendLine(EscapeBlockCommentContent(originalOutput));
        stub.AppendLine("*/");
        return stub.ToString();
    }
}
