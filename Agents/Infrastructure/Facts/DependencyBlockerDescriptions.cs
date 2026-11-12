using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed record DependencyDiagnosticExplanation(
    string Category, string Code, string? Item, string MissingEvidence, string Reason, string Action,
    bool ArtifactBlocker = false);

public static class DependencyBlockerDescriptions
{
    public static ConversionBlocker Describe(string program, string issue)
    {
        var value = issue.StartsWith(program + ": ", StringComparison.Ordinal) ? issue[(program.Length + 2)..] : issue;
        ConversionBlocker Block(string missing, string reason, string action, bool hard = true, string? diagnosticCategory = null) =>
            new(hard ? "artifact-blocker" : "coverage-gap", missing, reason, action, Raw: issue, DiagnosticCategory: diagnosticCategory);
        if (value.StartsWith("unresolved dependency: ", StringComparison.Ordinal))
        {
            var diagnostic = value["unresolved dependency: ".Length..];
            if (DescribeDiagnostic(diagnostic) is { } known)
                return Block(known.MissingEvidence, known.Reason, known.Action, known.ArtifactBlocker, known.Category);
            return Block($"Classified resolution/severity evidence for diagnostic '{diagnostic}'",
                "The exporter recorded this condition, but its exact input and impact are not structured in the evidence.",
                $"Inspect '{diagnostic}' in {program}.report/dependency-evidence.json and the corresponding parser/export log; produce its input-resolution and severity evidence before reassessing.");
        }
        var coverage = Regex.Match(value, @"^(COPY|CALL|EXEC_SQL|EXEC_CICS|FILE_IO) coverage is ([^;]+)");
        if (coverage.Success)
        {
            var domain = coverage.Groups[1].Value;
            var required = DomainEvidence(domain);
            return Block($"Complete {domain} coverage: {required}", $"Recorded {domain} coverage is {coverage.Groups[2].Value}; {required} are not established.",
                $"Produce {domain} parser/export evidence recording {required} for {program}, including explicit absence proofs when no such constructs occur; regenerate its dependency evidence with an exporter supporting this domain.", false);
        }
        if (value.Contains("missing dependency-evidence.json", StringComparison.Ordinal))
            return Block($"{program}.report/dependency-evidence.json",
                "No dependency-evidence artifact binds this program's original source, export, resolved definitions and domain coverage.",
                $"Run dependency evidence production for {program} with the real inputs; supply {program}.report/dependency-evidence.json alongside its hash-bound export. Legacy confidence or AST presence cannot replace it.");
        if (value.Contains("source-relative identity or original source path mismatch", StringComparison.Ordinal))
            return Block($"Matching source-relative identity and originalSourcePath for {program}",
                "The evidence identifies a different source path or program identity; attributing it to this program would mix inputs.",
                $"Select the original source root named by the snapshot, or regenerate evidence with relativePath '{program}' and its exact originalSourcePath. Do not rewrite identities to reuse another source's report.");
        var definition = Regex.Match(value, @"^(COPY|CALL) (.+?) has no hash-bound resolved definition");
        if (definition.Success)
        {
            var kind = definition.Groups[1].Value;
            var target = definition.Groups[2].Value;
            return Block($"Resolved {kind} definition {target} with original path and SHA256",
                $"The bound export references {kind} {target}, but no matching hash-bound resolved definition was supplied; this is not proof that the file is absent.",
                $"Resolve {kind} {target} to the real input in the configured include/source roots, record its exact path and SHA256 in the dependency evidence, and regenerate {program}'s export.", false);
        }
        if (value.Contains("SHA256 mismatch", StringComparison.Ordinal))
        {
            var artifact = value.Split(" missing or SHA256 mismatch", 2)[0];
            return Block($"Readable {artifact} matching its recorded SHA256",
                $"The {artifact} is absent or its bytes differ from the hash bound by this program's evidence.",
                $"Restore the exact recorded {artifact}, or regenerate the export/evidence from the intended current bytes and refresh dependent facts. Do not change include providers or waive hash validation.");
        }
        if (value.Contains("unreadable or invalid", StringComparison.Ordinal))
            return Block($"Readable valid dependency-evidence/export JSON for {program}",
                $"The artifact could not be loaded or validated: {value}",
                $"Inspect the exact I/O or JSON error in the raw diagnostic; restore read access or regenerate valid schema-conforming JSON for {program}. Do not interpret an unreadable report as an empty dependency set.");
        if (value.Contains("unsupported dependency evidence schema or producer", StringComparison.Ordinal))
            return Block("Dependency evidence schemaVersion 1 produced by cobol-rekt/dependency",
                "The evidence schema or producer is not supported by the verification gate; its provenance cannot authorize this conversion.",
                $"Regenerate {program}.report/dependency-evidence.json using the supported cobol-rekt/dependency producer and schemaVersion 1, with bound source/export/definition hashes.");
        if (value.Contains("dependency export must be bound", StringComparison.Ordinal))
            return Block("Export path within the selected active REKT artifact directory",
                "The dependency export points outside the selected artifact provenance boundary or into an inactive artifact path.",
                $"Select the matching snapshot artifact root and regenerate {program}'s evidence with its real active export path; do not relabel another snapshot's export.");
        if (value == "dependency exporter did not complete successfully.")
            return Block($"Successful dependency export completion for {program}",
                "The recorded exporter exit status is not successful; a dependency report may be partial despite an AST being available.",
                $"Inspect {program}'s dependency-export command/log and exit code; correct its reported input, dialect or exporter failure and rerun the export before producing coverage evidence.", false);
        return Block($"Valid evidence for {program}: {value}",
            $"The strict evidence gate rejected this specific condition: {value}",
            $"Correct the recorded condition in {program}'s dependency-evidence.json and its bound source/export/definition artifacts; regenerate the affected evidence and facts. No routine-level safety is established.");
    }

    public static DependencyDiagnosticExplanation? DescribeDiagnostic(string raw)
    {
        var value = raw;
        while (value.StartsWith("native:", StringComparison.Ordinal)) value = value["native:".Length..];
        var parts = value.Split(':', 2);
        var code = parts[0];
        var item = parts.Length > 1 ? parts[1] : null;
        DependencyDiagnosticExplanation Explain(string category, string? input, string missing, string reason, string action, bool hard = false) =>
            new(category, code, input, missing, reason, action, hard);
        if (code == "UNRESOLVED_CICS_AID_SEMANTIC"
            || code == "native-platform-contract-unverified"
            && item?.Contains("UNRESOLVED_CICS_AID_SEMANTIC", StringComparison.Ordinal) == true)
            return Explain("unresolved-semantics", item, "UNRESOLVED_CICS_AID_SEMANTIC",
                "The source compares EIBAID to standalone DFHAID. The selected contract defines named AID keys, not a meaning for DFHAID itself. The original condition is preserved.",
                "Establish the intended AID semantics from authoritative application/SDK evidence before changing the condition. Do not remove DFHAID, assume PF12-only behavior, or reinterpret it as ENTER or CLEAR.");
        if (code.StartsWith("platform-contract-", StringComparison.Ordinal) || code == "native-platform-contract-unverified")
            return Explain("platform-replacement", item, $"Supported, hash-bound platform replacement usage: {item}",
                "The explicit symbolic contract cannot account for this use. Aggregate DFHAID, raw bytes, writes and unsupported predicates cannot be interpreted as named terminal events.",
                "Inspect the exact source use and diagnostic. Supply an entitled original SDK definition, or establish a separately reviewed replacement contract preserving its actual semantics. Do not assign guessed bytes or reinterpret aggregate DFHAID as ENTER.");
        if (code == "dependency-export-failed")
            return Explain("export-failure", null, "A successful native dependency export",
                $"The dependency exporter recorded exit {item}. This does not identify a missing file by itself.",
                "Inspect the specific input and parser diagnostics below, fix their causes and rerun the export.");
        if (code == "callee" && item is not null)
        {
            var nested = item.Split(':', 2);
            var cause = nested.Length == 2 ? DescribeDiagnostic(nested[1]) : null;
            return Explain("unverified-input", nested[0], $"CALL target {nested[0]}: {cause?.MissingEvidence ?? "complete callee dependency evidence"}",
                $"Callee {nested[0]} has unresolved dependency evidence. {cause?.Reason}",
                cause is null ? $"Inspect the dependency report for callee {nested[0]}, resolve its recorded diagnostics, and regenerate caller/callee evidence."
                    : $"For callee {nested[0]}: {cause.Action} Then regenerate the caller's evidence.", cause?.ArtifactBlocker ?? false);
        }
        if (code == "unreadable-copybook")
        {
            var input = item?.Split(':', 2);
            var name = input?[0] ?? "name not recorded";
            var failure = input?.Length > 1 ? input[1] : "read failure reason not recorded";
            var provider = Regex.Match(failure, "Provider [\"']([^\"']+)[\"'] not installed", RegexOptions.IgnoreCase);
            var action = provider.Success
                ? $"For COPY {name}, install or configure the required provider '{provider.Groups[1].Value}'; verify it can read the real input, then regenerate dependency evidence."
                : failure.Contains("hash", StringComparison.OrdinalIgnoreCase)
                    ? $"Verify COPY {name}'s original bytes and recorded SHA256; restore the exact input or regenerate provenance-bound evidence from the intended bytes."
                    : $"Check read access, include paths and the recorded read failure for COPY {name}; provide a readable real input and regenerate evidence.";
            return Explain("unreadable-copybook", name, $"Readable COPY {name} ({failure})",
                $"COPY {name} could not be read. Recorded reason: {failure}. This is not proof that the real file is absent.", action,
                failure.Contains("hash", StringComparison.OrdinalIgnoreCase));
        }
        if (code == "unresolved-copybook" || (code == "parser-diagnostic" && item?.StartsWith("missing copybook:", StringComparison.Ordinal) == true))
        {
            var name = code == "unresolved-copybook" ? item : item!["missing copybook:".Length..].Split(':', 2)[0];
            return Explain("missing-copybook", name, $"Resolved real COPY input {name}",
                $"COPY {name} could not be resolved. Its shared fields or executable statements may affect program data and control flow; no routine impact is measured.",
                $"Supply the real COPY {name}, configure its include/search path, and regenerate the parser export and hash-bound definition evidence. Do not invent its layout or replace it with a stub.");
        }
        if (code is "source-not-found" or "incomplete-callee")
            return Explain("missing-input", item, code == "source-not-found" ? $"Real source input for CALL target {item}" : $"Complete dependency evidence for callee {item}",
                code == "source-not-found" ? $"The exporter could not locate required source {item}; its callee behavior and dependencies cannot be inspected."
                    : $"Callee {item}'s dependency coverage is incomplete; the caller's transitive dependency evidence is insufficient.",
                code == "source-not-found" ? $"Supply the real source for {item} in the configured source/search roots, then regenerate callee and caller exports."
                    : $"Resolve the diagnostics in callee {item}'s report and produce complete domain/definition evidence for that callee; regenerate the caller's export.");
        if (code == "cics-control-transfer-requires-runtime-mapping")
            return Explain("runtime-mapping", item, $"Verified target-runtime mapping for CICS control transfer {item}",
                $"The parser recognized CICS control transfer {item}, which returns control to CICS. Its target-runtime behavior is not verified; this is not a missing copybook or proof of Spring equivalence.",
                "Preserve the recorded CICS RETURN control flow and any options; implement and behavior-test its target-runtime mapping, then produce supported export evidence. Do not delete the command, assume a Java return is equivalent, or mark CICS dependencies absent.");
        if (code.StartsWith("unresolved-EXEC_", StringComparison.Ordinal))
            return Explain("binding-gap", item, $"Resolved native dependency bindings for {item}",
                $"The native export has unresolved operation bindings ({code}). This alone does not establish a missing file or an unsupported grammar feature.",
                "Inspect the accompanying declaration, target and resource diagnostics. Resolve real input bindings or extend the unsupported resolution behavior, then regenerate evidence.");
        if (code is "sql-declaration-not-found" or "cics-declaration-not-found")
            return Explain("missing-declaration", item, $"Source-grounded declaration for {item}",
                $"No matching declaration was established for {item}. A source or catalog file can be present without supplying the required declaration.",
                "Check the original operand and its local/shared SQL or CICS declarations, select the real declaration/resource catalog and regenerate bindings. Do not invent a declaration.");
        if (code is "cics-map-definition-not-found" or "cics-map-definition-ambiguous"
            && item?.Split(':', 2) is { Length: 2 } map)
            return Explain(code == "cics-map-definition-not-found" ? "missing-declaration" : "binding-gap", item,
                $"Unique BMS map {map[1]} in mapset {map[0]}",
                code == "cics-map-definition-not-found"
                    ? $"No supported BMS declaration matches map {map[1]} in mapset {map[0]}; this does not imply a missing BMS file."
                    : $"Multiple supported BMS declarations match map {map[1]} in mapset {map[0]}; the exporter cannot select one safely.",
                "Check MAP/MAPSET against the actual DFHMDI/DFHMSD declarations. When MAPSET is omitted it defaults to MAP, not a guessed containing mapset. Supply the intended genuine BMS input or correct the source operands after confirming their intended screen; resolve duplicate definitions and regenerate evidence. Do not invent aliases.");
        if (code == "original-operation-not-exported")
            return Explain("unsupported", item, $"Native parser/exporter support for {item}",
                $"The original operation or its dependency bindings are not fully represented by the native export ({code}). This is a parser/exporter gap, not proof that an input file is missing.",
                "Retain the original operation, extend the parser/exporter binding support and regenerate evidence. Do not remove the statement or substitute a placeholder.");
        if (code == "partial-derived-bms-layout")
            return Explain("derived-layout", item, $"Complete source-grounded layout evidence for {item}",
                $"A derived screen layout is present but is not certified complete for the supported BMS recipe.",
                "Inspect the BMS layout diagnostics, support the recorded layout constructs and regenerate the original/COPY/manifest bindings. A partial layout is not a missing vendor copybook.");
        if (code.StartsWith("unsupported-", StringComparison.Ordinal))
        {
            var domain = code.Contains("cics", StringComparison.OrdinalIgnoreCase) ? "EXEC_CICS"
                : code.Contains("file-io", StringComparison.OrdinalIgnoreCase) ? "FILE_IO"
                : code.Contains("sql", StringComparison.OrdinalIgnoreCase) ? "EXEC_SQL" : "recorded dependency construct";
            return Explain("unsupported", item, $"Exporter support for {domain} contexts: {item ?? "context not recorded"}",
                $"The exporter cannot establish {domain} dependency semantics for the recorded context {item}; this does not prove a missing source file.",
                $"Use a parser/exporter that resolves {domain} contexts {item}, including {DomainEvidence(domain)}; regenerate coverage and hash-bound dependency evidence. Supplying a stub or changing include providers does not add exporter support.");
        }
        if (code.Contains("source-unverified", StringComparison.Ordinal) || code.Contains("definition-unverified", StringComparison.Ordinal)
            || code is "unverified-copybook" or "unverified-staged-copybook" or "unverified-staged-source")
            return Explain("unverified-input", item, $"Verified source/definition identity, hashes and staging provenance for {item ?? code}",
                $"Input identity or provenance was not verified ({code}); recorded detail: {item}.",
                $"Compare the original and staged input paths/bytes for {item ?? code}, validate their recorded hashes, and regenerate the export and evidence from the intended exact input. Provider configuration cannot repair a hash or identity mismatch.", true);
        return null;
    }

    private static string DomainEvidence(string domain) => domain switch
    {
        "COPY" => "resolved include definitions, nested COPY references and hashes",
        "CALL" => "literal/dynamic target resolution and transitive callee dependencies",
        "EXEC_SQL" => "SQL statement contexts, host-variable bindings and external SQL definitions",
        "EXEC_CICS" => "CICS command contexts, resource/target bindings and external definitions",
        "FILE_IO" => "file-operation contexts, SELECT/FD bindings and record definitions",
        _ => "the construct's dependency and external-definition bindings"
    };
}
