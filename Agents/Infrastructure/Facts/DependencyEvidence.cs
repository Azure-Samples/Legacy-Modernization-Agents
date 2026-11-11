using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed record DependencyEvidence
{
    [JsonPropertyName("schemaVersion")] public int SchemaVersion { get; init; }
    [JsonPropertyName("producer")] public string Producer { get; init; } = "";
    [JsonPropertyName("relativePath")] public string RelativePath { get; init; } = "";
    [JsonPropertyName("originalSourcePath")] public string OriginalSourcePath { get; init; } = "";
    [JsonPropertyName("originalSourceSha256")] public string OriginalSourceSha256 { get; init; } = "";
    [JsonPropertyName("exportPath")] public string ExportPath { get; init; } = "";
    [JsonPropertyName("exportSha256")] public string ExportSha256 { get; init; } = "";
    [JsonPropertyName("exportExitCode")] public int? ExportExitCode { get; init; }
    [JsonPropertyName("coverage")] public Dictionary<string, string> Coverage { get; init; } = new();
    [JsonPropertyName("definitions")] public List<DependencyDefinition> Definitions { get; init; } = new();
    [JsonPropertyName("unresolvedDiagnostics")] public List<string> UnresolvedDiagnostics { get; init; } = new();
    [JsonPropertyName("placeholderContaminated")] public bool? PlaceholderContaminated { get; init; }
}

public sealed record DependencyDefinition
{
    [JsonPropertyName("kind")] public string Kind { get; init; } = "";
    [JsonPropertyName("name")] public string Name { get; init; } = "";
    [JsonPropertyName("path")] public string Path { get; init; } = "";
    [JsonPropertyName("sha256")] public string Sha256 { get; init; } = "";
}

/// <summary>
/// Validates local parser exports, not a Neo4j query or a new parser execution.
/// Global parse confidence and a successful process exit are not dependency coverage.
/// </summary>
public sealed partial class DependencyEvidenceGate(string factsDirectory)
{
    public static readonly IReadOnlyList<string> RequiredKinds =
        new[] { "COPY", "CALL", "EXEC_SQL", "EXEC_CICS", "FILE_IO" };

    // One facts-tree walk per gate instead of one per program: the locator's basename fallback
    // otherwise rescans the whole artifact tree for every identity it validates.
    private readonly Lazy<string[]> factsFiles = new(() => Directory.Exists(factsDirectory)
        ? Directory.EnumerateFiles(factsDirectory, "*" + ProgramFactsArtifactLocator.FactsSuffix,
            SearchOption.AllDirectories).ToArray()
        : Array.Empty<string>());

    public static string GetEvidencePath(string factsDirectory, string relativePath) =>
        Path.Combine(factsDirectory, SourcePathHelper.ToOsRelativePath(relativePath) + ".report",
            "dependency-evidence.json");

    public static DependencyEvidence? LoadEvidence(string factsDirectory, string relativePath)
    {
        var path = GetEvidencePath(factsDirectory, relativePath);
        return File.Exists(path)
            ? JsonSerializer.Deserialize<DependencyEvidence>(File.ReadAllText(path))
            : null;
    }

    /// <summary>
    /// Name-based selection can carry same-named copybooks the parser never resolved, such as another
    /// estate's copy or a stale generated map. A copybook is dropped only when no selected program binds
    /// its exact path and a selected program binds that name to a different exact path; every other
    /// unbound copybook stays selected so the gate still rejects it. The gate verifies the bound path.
    /// </summary>
    public List<CobolFile> ExcludeSupersededCopybooks(
        IReadOnlyList<CobolFile> files, string sourceRoot, ICollection<string>? excluded = null)
    {
        var bound = new List<DependencyDefinition>();
        foreach (var program in files.Where(file => SourceTypeRegistry.IsCobolProgram(file.FileName)))
        {
            try
            {
                if (LoadEvidence(factsDirectory, Identity(program, sourceRoot)) is { } evidence)
                    bound.AddRange(evidence.Definitions.Where(definition => definition.Kind is "COPY" or "PLATFORM_INPUT"));
            }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or JsonException)
            {
                return files.ToList();
            }
        }
        var boundPaths = bound.Select(definition => Path.GetFullPath(definition.Path)).ToHashSet(StringComparer.Ordinal);
        var kept = new List<CobolFile>();
        foreach (var file in files)
        {
            var path = Path.GetFullPath(file.FilePath);
            if (SourceTypeRegistry.IsCopybook(file.FileName) && !boundPaths.Contains(path)
                && bound.Any(definition => SameTarget(definition.Name, Path.GetFileNameWithoutExtension(file.FileName))))
            {
                excluded?.Add(Identity(file, sourceRoot));
                continue;
            }
            kept.Add(file);
        }
        return kept;
    }

    public DependencyMap RequireVerified(
        IReadOnlyList<CobolFile> files, string sourceRoot, DependencyMap? reusedMap = null) =>
        RequireVerified(files, sourceRoot, reusedMap, ProgramSourceCatalog.FromStagingDirectory(sourceRoot));

    internal DependencyMap RequireVerified(
        IReadOnlyList<CobolFile> files, string sourceRoot, DependencyMap? reusedMap, ProgramSourceCatalog catalog)
    {
        var issues = new List<string>();
        var map = new DependencyMap();
        var copyContexts = new List<(string Program, DependencyDefinition Definition)>();
        foreach (var file in files.Where(file => SourceTypeRegistry.IsCobolProgram(file.FileName)))
        {
            var identity = Identity(file, sourceRoot);
            try
            {
                var evidence = LoadEvidence(factsDirectory, identity);
                if (evidence is null)
                {
                    issues.Add($"{identity}: missing dependency-evidence.json; legacy High confidence is not dependency coverage.");
                    continue;
                }

                var startIssues = issues.Count;
                Validate(evidence, identity, file.FilePath, issues);
                if (issues.Count != startIssues) continue;

                var tree = ReadExportTree(evidence.ExportPath, identity, requireIdentity: true);
                var export = tree.Direct;
                FindContradictions(file.FilePath, identity, export,
                    tree.RuntimeRequirements.Where(requirement => requirement.Program == identity).ToArray(), issues);
                ValidateProfiles(identity, evidence, tree.RuntimeRequirements, sourceRoot, issues);
                ValidatePlatformCopies(identity, evidence, tree.RuntimeRequirements, issues);
                ValidateTransactions(identity, evidence, tree, sourceRoot, issues);
                ValidateCicsTerminalEvidence(identity, evidence, tree.RuntimeRequirements, sourceRoot, issues);
                ValidateBmsLayouts(identity, evidence, tree.DerivedInputs, sourceRoot, issues);
                ValidateFacts(identity, evidence, export, catalog, issues);
                var copies = export.Where(edge => edge.Kind == "COPY").Select(edge => edge.Name)
                    .Distinct(StringComparer.OrdinalIgnoreCase).Order(StringComparer.OrdinalIgnoreCase).ToList();
                foreach (var copy in tree.Supporting.Where(edge => edge.Kind == "COPY").Select(edge => edge.Name))
                {
                    if (!evidence.Definitions.Any(definition => definition.Kind == "COPY"
                            && SameTarget(definition.Name, copy)))
                        issues.Add($"{identity}: COPY {copy} has no hash-bound resolved definition.");
                }
                foreach (var call in tree.Supporting.Where(edge => edge.Kind == "CALL"))
                {
                    if (!evidence.Definitions.Any(definition => definition.Kind is "CALL" or "RUNTIME_CONTRACT"
                            && SameTarget(definition.Name, call.Name)))
                        issues.Add($"{identity}: CALL {call.Name} has no hash-bound resolved definition; external input remains unresolved.");
                }
                foreach (var definition in evidence.Definitions.Where(definition => definition.Kind is "COPY" or "CALL" or "RUNTIME_CONTRACT"))
                {
                    if (!tree.Supporting.Any(edge => edge.Kind == (definition.Kind == "RUNTIME_CONTRACT" ? "CALL" : definition.Kind)
                            && SameTarget(definition.Name, edge.Name)))
                        issues.Add($"{identity}: resolved {definition.Kind} {definition.Name} is absent from the bound parser export.");
                }
                if (issues.Count == startIssues)
                {
                    copyContexts.AddRange(evidence.Definitions.Where(definition => definition.Kind == "COPY")
                        .Select(definition => (identity, definition)));
                    map.RuntimeRequirements.AddRange(tree.RuntimeRequirements);
                }

                map.CopybookUsage[identity] = copies;
                foreach (var edge in export)
                {
                    var target = edge.Kind == "CALL"
                        ? catalog.ResolveCallTarget(edge.Name) ?? edge.Name
                        : edge.Name;
                    map.Dependencies.Add(new DependencyRelationship
                    {
                        SourceFile = identity,
                        TargetFile = target,
                        DependencyType = edge.Kind,
                        Context = "Verified local REKT dependency export; no Neo4j query or new parser execution."
                    });
                }
            }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or JsonException
                                      or InvalidDataException or ArgumentException or NullReferenceException)
            {
                issues.Add($"{identity}: dependency evidence unreadable or invalid: {ex.Message}");
            }
        }
        ValidateSelectedCopybooks(files, sourceRoot, copyContexts, map, issues);

        if (issues.Count == 0 && reusedMap is not null)
        {
            if (!Edges(map).SequenceEqual(Edges(reusedMap), StringComparer.OrdinalIgnoreCase)
                || !CopyUsage(map).SequenceEqual(CopyUsage(reusedMap), StringComparer.OrdinalIgnoreCase)
                || !RuntimeRequirements(map).SequenceEqual(RuntimeRequirements(reusedMap), StringComparer.Ordinal))
                issues.Add("Reused dependency map differs from the current hash-verified parser export; regenerate the map.");
        }
        if (issues.Count > 0)
            throw new InvalidDataException("Required REKT dependency evidence rejected before model conversion:\n- "
                + string.Join("\n- ", issues));

        map.EvidenceDiagnostics.Add("Dependency coverage and current source/export/definition hashes verified from local " +
            "REKT disk exports. Neo4j was not queried; no parser was executed by this gate.");
        return map;
    }

    private void ValidateSelectedCopybooks(
        IReadOnlyList<CobolFile> files, string sourceRoot,
        IReadOnlyList<(string Program, DependencyDefinition Definition)> contexts,
        DependencyMap map, List<string> issues)
    {
        foreach (var file in files.Where(file => SourceTypeRegistry.IsCopybook(file.FileName)))
        {
            var identity = Identity(file, sourceRoot);
            var path = Path.GetFullPath(file.FilePath);
            var bindings = contexts.Where(context => Path.GetFullPath(context.Definition.Path) == path).ToList();
            if (bindings.Count == 0)
            {
                issues.Add($"{identity}: selected copybook is not an exact-path, hash-bound COPY definition of a validated selected program.");
                continue;
            }
            if (bindings.Any(binding => contexts.Where(context => context.Program == binding.Program
                        && SameTarget(context.Definition.Name, binding.Definition.Name))
                    .Select(context => Path.GetFullPath(context.Definition.Path)).Distinct(StringComparer.Ordinal).Count() > 1))
            {
                issues.Add($"{identity}: ambiguous COPY definition in a selected program context; multiple resolved paths share its target name.");
                continue;
            }
            var startIssues = issues.Count;
            foreach (var binding in bindings)
                CheckHash(path, binding.Definition.Sha256, $"{identity}: selected COPY definition", issues.Add);
            if (File.Exists(path) && IsPlaceholder(path))
                issues.Add($"{identity}: selected COPY definition is placeholder/stub-backed.");
            if (ProgramFactsArtifactLocator.TryLoad(factsDirectory, identity, () => factsFiles.Value) is not null)
                issues.Add($"{identity}: standalone copybook facts lack verified definition-context projection provenance; " +
                    "they must not be injected as program facts.");
            if (issues.Count != startIssues) continue;
            map.EvidenceDiagnostics.Add($"{identity}: selected copybook is a hash-bound COPY definition of " +
                $"{string.Join(", ", bindings.Select(binding => binding.Program).Distinct().Order())}; " +
                "no standalone parser coverage, program facts or COPY self-edge is asserted.");
        }
    }

    private void Validate(DependencyEvidence evidence, string identity, string sourcePath, List<string> issues)
    {
        void Issue(string message) => issues.Add($"{identity}: {message}");
        if (evidence.SchemaVersion != 1 || evidence.Producer != "cobol-rekt/dependency")
            Issue("unsupported dependency evidence schema or producer.");
        if (!string.Equals(evidence.RelativePath, identity, StringComparison.Ordinal)
            || !Path.IsPathFullyQualified(evidence.OriginalSourcePath)
            || Path.GetFullPath(evidence.OriginalSourcePath) != Path.GetFullPath(sourcePath))
            Issue("source-relative identity or original source path mismatch.");
        CheckHash(sourcePath, evidence.OriginalSourceSha256, "original source", Issue);
        if (evidence.ExportExitCode != 0) Issue("dependency exporter did not complete successfully.");
        if (!Path.IsPathFullyQualified(evidence.ExportPath)
            || !IsWithin(evidence.ExportPath, factsDirectory)
            || !ProgramFactsArtifactLocator.IsActiveArtifactPath(evidence.ExportPath, factsDirectory))
            Issue("dependency export must be bound to the selected REKT artifact directory.");
        else
            CheckHash(evidence.ExportPath, evidence.ExportSha256, "dependency export", Issue);
        foreach (var kind in RequiredKinds)
        {
            if (!evidence.Coverage.TryGetValue(kind, out var coverage) || coverage != "complete")
                Issue($"{kind} coverage is {coverage ?? "missing"}; explicit complete parser evidence is required.");
        }
        if (evidence.PlaceholderContaminated != false)
            Issue("placeholder/stub contamination is present or has not been checked.");
        foreach (var diagnostic in evidence.UnresolvedDiagnostics)
            Issue($"unresolved dependency: {diagnostic}");
        foreach (var definition in evidence.Definitions)
        {
            if (!(RequiredKinds.Contains(definition.Kind) || definition.Kind is "BMS" or "COPY_INPUT" or "CICS_FILE"
                    or "ANALYSIS_PROFILE" or "RUNTIME_CONTRACT" or "CICS_PROGRAM" or "CICS_TRANSACTION" or "SQL_DECLARATION" or "BMS_LAYOUT"
                    or "PLATFORM_COPY" or "PLATFORM_INPUT" or "CICS_MAP" or "CICS_MAP_LAYOUT"
                    or "CICS_CONTINUATION" or "CICS_CONTINUATION_SOURCE" or "CICS_DATASET"
                    or "CICS_TDQUEUE" or "COMMAREA_CLOSURE" or "ASSEMBLER_SOURCE")
                || string.IsNullOrWhiteSpace(definition.Name)
                || !Path.IsPathFullyQualified(definition.Path))
            {
                Issue("resolved definition has invalid kind/name/path.");
                continue;
            }
            CheckHash(definition.Path, definition.Sha256, $"{definition.Kind} definition {definition.Name}", Issue);
            if (File.Exists(definition.Path) && IsPlaceholder(definition.Path))
                Issue($"{definition.Kind} definition {definition.Name} is placeholder/stub-backed.");
        }
        if (File.Exists(sourcePath) && IsPlaceholder(sourcePath))
            Issue("original source contains generated placeholder/stub evidence.");
    }

    private void ValidateFacts(
        string identity, DependencyEvidence evidence, IReadOnlyList<(string Kind, string Name)> export,
        ProgramSourceCatalog catalog, List<string> issues)
    {
        var facts = ProgramFactsArtifactLocator.TryLoad(factsDirectory, identity, () => factsFiles.Value);
        if (facts is null) return; // A verified disk dependency export does not require a facts projection.
        var snapshot = facts.DependencyEvidence;
        if (snapshot is null || snapshot.RelativePath != identity
            || snapshot.OriginalSourcePath != evidence.OriginalSourcePath
            || snapshot.OriginalSourceSha256 != evidence.OriginalSourceSha256
            || snapshot.ExportPath != evidence.ExportPath || snapshot.ExportSha256 != evidence.ExportSha256
            || snapshot.PlaceholderContaminated != false || snapshot.UnresolvedDiagnostics.Count > 0
            || RequiredKinds.Any(kind => !snapshot.Coverage.TryGetValue(kind, out var status) || status != "complete")
            || !DefinitionKeys(snapshot).SequenceEqual(DefinitionKeys(evidence), StringComparer.OrdinalIgnoreCase))
            issues.Add($"{identity}: existing program facts are stale or lack matching dependency evidence; regenerate facts from the current verified inputs.");
        var copies = export.Where(edge => edge.Kind == "COPY").Select(edge => edge.Name);
        var calls = export.Where(edge => edge.Kind == "CALL")
            .Select(edge => catalog.ResolveCallTarget(edge.Name) ?? edge.Name);
        if (!Values(copies).SequenceEqual(Values(facts.Data.CopybooksUsed), StringComparer.OrdinalIgnoreCase)
            || !Values(calls).SequenceEqual(Values(facts.Callees), StringComparer.OrdinalIgnoreCase))
            issues.Add($"{identity}: existing program facts dependency entries differ from the bound parser export.");
        if (facts.Warnings.Any(warning => warning.StartsWith("generated-copybook-stub:", StringComparison.OrdinalIgnoreCase)
                || warning.StartsWith("missing-copybook:", StringComparison.OrdinalIgnoreCase)))
            issues.Add($"{identity}: existing program facts contain unresolved/placeholder dependency warnings.");
    }

    private static IEnumerable<string> Values(IEnumerable<string> values) =>
        values.Distinct(StringComparer.OrdinalIgnoreCase).Order(StringComparer.OrdinalIgnoreCase);

    private static IEnumerable<string> DefinitionKeys(DependencyEvidence evidence) =>
        Values(evidence.Definitions.Select(definition =>
            $"{definition.Kind}\0{definition.Name}\0{definition.Path}\0{definition.Sha256}"));

    private static void CheckHash(string path, string? expected, string label, Action<string> issue)
    {
        if (expected is null || expected.Length != 64 || !expected.All(Uri.IsHexDigit) || !File.Exists(path)
            || !string.Equals(Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path))),
                expected, StringComparison.OrdinalIgnoreCase))
            issue($"{label} missing or SHA256 mismatch (stale/unbound evidence): {path}");
    }

    internal static bool IsPlaceholder(string path)
    {
        var text = File.ReadAllText(path);
        return text.Contains("AUTO-GENERATED STUB", StringComparison.OrdinalIgnoreCase)
            || Regex.IsMatch(text, @"^\s*(?:\d{6}\s*)?(?:01|77)\s+[\w-]+-STUB\b",
                RegexOptions.IgnoreCase | RegexOptions.Multiline)
            || text.Contains(".stubcache", StringComparison.OrdinalIgnoreCase)
            || path.Contains(".stubcache", StringComparison.OrdinalIgnoreCase);
    }

    private static bool IsWithin(string path, string root)
    {
        var relative = Path.GetRelativePath(Path.GetFullPath(root), Path.GetFullPath(path));
        return !Path.IsPathRooted(relative) && relative != ".."
            && !relative.StartsWith(".." + Path.DirectorySeparatorChar, StringComparison.Ordinal);
    }

    public static string Identity(CobolFile file, string sourceRoot)
    {
        if (string.IsNullOrWhiteSpace(file.FilePath) || !IsWithin(file.FilePath, sourceRoot))
            throw new InvalidDataException($"Source file is outside the selected source root: {file.FilePath}");
        return SourcePathHelper.NormalizeRelativePath(Path.GetRelativePath(sourceRoot, file.FilePath));
    }

    private static bool SameTarget(string left, string right) =>
        string.Equals(Path.GetFileNameWithoutExtension(left), Path.GetFileNameWithoutExtension(right),
            StringComparison.OrdinalIgnoreCase);

    private static IEnumerable<string> Edges(DependencyMap map) =>
        map.Dependencies.Select(edge => $"{edge.SourceFile}\0{edge.DependencyType}\0{edge.TargetFile}")
            .Distinct(StringComparer.OrdinalIgnoreCase).Order(StringComparer.OrdinalIgnoreCase);

    private static IEnumerable<string> CopyUsage(DependencyMap map) =>
        map.CopybookUsage.Select(pair => $"{pair.Key}\0{string.Join("\0", pair.Value.Order(StringComparer.OrdinalIgnoreCase))}")
            .Order(StringComparer.OrdinalIgnoreCase);

    private static IEnumerable<string> RuntimeRequirements(DependencyMap map) =>
        map.RuntimeRequirements.Select(requirement => JsonSerializer.Serialize(requirement)).Order(StringComparer.Ordinal);

    private static void FindContradictions(
        string sourcePath, string identity, IReadOnlyList<(string Kind, string Name)> export,
        IReadOnlyList<RuntimeImplementationRequirement> runtimeRequirements, List<string> issues)
    {
        // A positive source match can disprove an export. A negative match never proves coverage.
        var lines = File.ReadAllLines(sourcePath);
        var tokens = ContradictionTokens(lines);
        if (lines.Any(line => line.Length > 7 && line.Take(6).All(character => character is ' ' or >= '0' and <= '9')
                && line[6] is 'D' or 'd' && line[7] == ' ')
            && Enumerable.Range(0, Math.Max(0, tokens.Count - 2)).Any(index =>
                !tokens[index].IsLiteral && !tokens[index + 1].IsLiteral && !tokens[index + 2].IsLiteral
                && string.Equals(tokens[index].Text, "WITH", StringComparison.OrdinalIgnoreCase)
                && string.Equals(tokens[index + 1].Text, "DEBUGGING", StringComparison.OrdinalIgnoreCase)
                && string.Equals(tokens[index + 2].Text, "MODE", StringComparison.OrdinalIgnoreCase)))
            issues.Add($"{identity}: active WITH DEBUGGING MODE lines are excluded by static preprocessing; dependency coverage remains unverified.");
        var remaining = runtimeRequirements.ToList();
        var inDialect = false;
        for (var index = 0; index + 1 < tokens.Count; index++)
        {
            var token = tokens[index];
            var operand = tokens[index + 1];
            if (token.IsLiteral) continue;
            var keyword = token.Text.ToUpperInvariant();
            if (keyword == "END-EXEC")
            {
                inDialect = false;
                continue;
            }
            if (keyword is "DFHRESP" or "DFHVALUE" && operand.Text == "(")
            {
                var requirement = remaining.FirstOrDefault(requirement =>
                    requirement.Kind == "EXEC_CICS" && requirement.Operation == keyword);
                if (requirement is null)
                    issues.Add($"{identity}: EXEC_CICS source expression {keyword} contradicts the dependency export.");
                else
                    remaining.Remove(requirement);
                continue;
            }
            if (inDialect) continue;
            if (keyword == "COPY" && operand.Text != ".")
            {
                if (!export.Any(edge => edge.Kind == "COPY" && SameTarget(edge.Name, operand.Text))
                    && !runtimeRequirements.Any(requirement => requirement.Kind == "PLATFORM_COPY"
                        && SameTarget(requirement.Operation, operand.Text)))
                    issues.Add($"{identity}: source COPY {operand.Text} contradicts the dependency export; no scanner edge substituted.");
            }
            else if (keyword == "CALL")
            {
                // A dynamic operand passes only with a verified constant proof on an exported runtime leaf.
                if (!operand.IsLiteral && !runtimeRequirements.Any(requirement => requirement.Kind == "EXTERNAL_RUNTIME"
                        && requirement.Evidence.TryGetProperty("targetResolution", out var resolution)
                        && string.Equals(Text(resolution, "operand"), operand.Text, StringComparison.OrdinalIgnoreCase)))
                    issues.Add($"{identity}: dynamic CALL target remains unresolved; no target was invented.");
                else if (!export.Any(edge => edge.Kind == "CALL" && SameTarget(edge.Name, operand.Text)))
                    issues.Add($"{identity}: source CALL {operand.Text} contradicts the dependency export; no scanner edge substituted.");
            }

            var unsupportedKind = keyword == "EXEC" && !operand.IsLiteral
                ? operand.Text.ToUpperInvariant() switch { "SQL" => "EXEC_SQL", "CICS" => "EXEC_CICS", _ => null }
                : keyword is "OPEN" or "READ" or "WRITE" or "REWRITE" or "CLOSE" or "DELETE" or "START"
                    ? "FILE_IO" : null;
            if (keyword == "EXEC" && unsupportedKind is not null)
            {
                inDialect = true;
                var operation = index + 2 < tokens.Count ? tokens[index + 2].Text : "";
                if (unsupportedKind == "EXEC_SQL"
                    && string.Equals(operation, "INCLUDE", StringComparison.OrdinalIgnoreCase)
                    && index + 4 < tokens.Count && !tokens[index + 3].IsLiteral
                    && string.Equals(tokens[index + 4].Text, "END-EXEC", StringComparison.OrdinalIgnoreCase)
                    && export.Any(edge => edge.Kind == "COPY" && SameTarget(edge.Name, tokens[index + 3].Text)))
                    continue;
                var requirement = remaining.FirstOrDefault(requirement => requirement.Kind == unsupportedKind
                    && string.Equals(requirement.Operation, operation, StringComparison.OrdinalIgnoreCase));
                if (requirement is not null)
                {
                    remaining.Remove(requirement);
                    continue;
                }
            }
            if (unsupportedKind == "FILE_IO")
            {
                // Native FILE_IO statements are bound to their SELECT entry and FD by the parser.
                var requirement = remaining.FirstOrDefault(requirement => requirement.Kind == "FILE_IO"
                    && string.Equals(requirement.Operation, keyword, StringComparison.OrdinalIgnoreCase));
                if (requirement is not null)
                {
                    remaining.Remove(requirement);
                    continue;
                }
            }
            if (unsupportedKind is not null)
                issues.Add($"{identity}: {unsupportedKind} source evidence cannot be represented by this dependency export shape at {keyword}; required inputs remain unresolved.");
        }
    }

    private readonly record struct SourceToken(string Text, bool IsLiteral = false);

    private static List<SourceToken> ContradictionTokens(IEnumerable<string> lines)
    {
        var tokens = new List<SourceToken>();
        var literal = new StringBuilder();
        char? quote = null;
        var identification = false;
        var documentaryParagraph = false;
        foreach (var raw in lines)
        {
            var fixedFormat = raw.Length > 6 && raw.Take(6).All(character => character is ' ' or >= '0' and <= '9')
                && (raw[6] is ' ' or '-' or '*' or '/' || raw[6] is 'D' or 'd' && raw.Length > 7 && raw[7] == ' ');
            if (fixedFormat && raw[6] is '*' or '/' or 'D' or 'd') continue;
            var line = fixedFormat ? raw[7..Math.Min(raw.Length, 72)] : raw;
            if (Regex.IsMatch(line, @"^\s*(?:IDENTIFICATION|ID)(?:\s+DIVISION\b|\s*$)", RegexOptions.IgnoreCase))
                identification = true;
            if (identification && Regex.IsMatch(line, @"^\s*(?:ENVIRONMENT|DATA|PROCEDURE)(?:\s+DIVISION\b|\s*$)", RegexOptions.IgnoreCase))
            {
                identification = false;
                documentaryParagraph = false;
            }
            if (identification && Regex.IsMatch(line,
                    @"^\s*(?:AUTHOR|INSTALLATION|DATE-WRITTEN|DATE-COMPILED|SECURITY|REMARKS)\s*\.", RegexOptions.IgnoreCase))
                documentaryParagraph = true;
            if (documentaryParagraph) continue;
            var index = 0;
            if (quote is not null && fixedFormat && raw[6] == '-')
            {
                // Fixed-format literal continuation repeats its delimiter; it does not close the literal.
                while (index < line.Length && char.IsWhiteSpace(line[index])) index++;
                if (index < line.Length && line[index] == quote) index++;
            }
            while (index < line.Length)
            {
                var character = line[index];
                if (quote is not null)
                {
                    if (character != quote)
                        literal.Append(character);
                    else if (index + 1 < line.Length && line[index + 1] == quote)
                    {
                        literal.Append(character);
                        index++;
                    }
                    else
                    {
                        tokens.Add(new SourceToken(literal.ToString(), IsLiteral: true));
                        literal.Clear();
                        quote = null;
                    }
                    index++;
                    continue;
                }
                if (character == '*' && index + 1 < line.Length && line[index + 1] == '>') break;
                if (character is '\'' or '"')
                {
                    quote = character;
                    index++;
                }
                else if (char.IsLetterOrDigit(character) || character is '_' or '-')
                {
                    var start = index++;
                    while (index < line.Length && (char.IsLetterOrDigit(line[index]) || line[index] is '_' or '-'))
                        index++;
                    tokens.Add(new SourceToken(line[start..index]));
                }
                else
                {
                    if (!char.IsWhiteSpace(character)) tokens.Add(new SourceToken(character.ToString()));
                    index++;
                }
            }
        }
        return tokens;
    }

    internal static IReadOnlyList<(string Kind, string Name)> ReadExport(
        string path, string identity, bool requireIdentity = false) =>
        ReadExportTree(path, identity, requireIdentity).Direct;

    private sealed record ExportTree(
        IReadOnlyList<(string Kind, string Name)> Direct,
        IReadOnlyList<(string Kind, string Name)> Supporting,
        IReadOnlyList<RuntimeImplementationRequirement> RuntimeRequirements,
        IReadOnlyList<JsonElement> DerivedInputs);

    private static ExportTree ReadExportTree(string path, string identity, bool requireIdentity)
    {
        using var document = JsonDocument.Parse(File.ReadAllText(path));
        if (requireIdentity && (!document.RootElement.TryGetProperty("callTarget", out var rootTarget)
            || !rootTarget.TryGetProperty("name", out var rootName)
            || rootName.ValueKind != JsonValueKind.String
            || !(string.Equals(rootName.GetString(), Path.GetFileName(identity), StringComparison.OrdinalIgnoreCase)
                || string.Equals(rootName.GetString(), identity, StringComparison.Ordinal))))
            throw new InvalidDataException($"{identity}: dependency export source identity mismatch.");
        var direct = new List<(string Kind, string Name)>();
        var supporting = new List<(string Kind, string Name)>();
        var runtimeRequirements = new List<RuntimeImplementationRequirement>();
        var derivedInputs = new List<JsonElement>();
        Visit(document.RootElement, depth: 0, identity);
        return new ExportTree(direct.Distinct().ToList(), supporting.Distinct().ToList(),
            runtimeRequirements, derivedInputs);

        void Visit(JsonElement node, int depth, string program)
        {
            var owner = program;
            if (node.ValueKind != JsonValueKind.Object)
                throw new InvalidDataException("unrecognized dependency export node.");
            if (node.TryGetProperty("dependencyDiagnostics", out var diagnostics)
                && (diagnostics.ValueKind != JsonValueKind.Array || diagnostics.GetArrayLength() > 0))
                throw new InvalidDataException($"unresolved parser dependency diagnostics at depth {depth}: {diagnostics}");
            if (node.TryGetProperty("dependencyDerivedInputs", out var derived))
            {
                if (derived.ValueKind != JsonValueKind.Array)
                    throw new InvalidDataException("native derived inputs must be an array.");
                foreach (var binding in derived.EnumerateArray())
                {
                    if (Text(binding, "kind") != "BMS_LAYOUT")
                        throw new InvalidDataException("unsupported native derived input.");
                    derivedInputs.Add(binding.Clone());
                }
            }
            var target = node.TryGetProperty("callTarget", out var callTarget) ? callTarget : node;
            if (depth > 0 && target.TryGetProperty("name", out var programName)
                && programName.ValueKind == JsonValueKind.String)
                program = programName.GetString() ?? program;
            if (node.TryGetProperty("dependencyDialectStatements", out var statements))
            {
                if (statements.ValueKind != JsonValueKind.Array)
                    throw new InvalidDataException("native dialect statements must be an array.");
                foreach (var statement in statements.EnumerateArray())
                {
                    if (statement.ValueKind != JsonValueKind.Object
                        || !statement.TryGetProperty("kind", out var kind)
                        || kind.ValueKind != JsonValueKind.String
                        || kind.GetString() is not ("EXEC_CICS" or "EXEC_SQL" or "FILE_IO")
                        || !statement.TryGetProperty("operation", out var operation)
                        || operation.ValueKind != JsonValueKind.String
                        || string.IsNullOrWhiteSpace(operation.GetString()))
                        throw new InvalidDataException("native dialect statement has invalid kind/operation.");
                    if (!statement.TryGetProperty("bindingStatus", out var binding)
                        || binding.ValueKind != JsonValueKind.String || binding.GetString() != "complete"
                        || !statement.TryGetProperty("grammarRule", out var grammar)
                        || grammar.ValueKind != JsonValueKind.String
                        || !statement.TryGetProperty("sourceText", out var text)
                        || text.ValueKind != JsonValueKind.String || string.IsNullOrWhiteSpace(text.GetString())
                        || !statement.TryGetProperty("syntax", out var syntax)
                        || !ValidSyntax(syntax) || !syntax.TryGetProperty("rule", out var syntaxRule)
                        || syntaxRule.GetString() != grammar.GetString()
                        || kind.GetString() == "EXEC_SQL"
                        && (!statement.TryGetProperty("semanticStatus", out var semanticStatus)
                            || semanticStatus.ValueKind != JsonValueKind.String || semanticStatus.GetString() != "complete"
                            || !statement.TryGetProperty("semanticSyntax", out var semanticSyntax)
                            || !ValidSyntax(semanticSyntax)
                            || !semanticSyntax.TryGetProperty("rule", out var semanticRule)
                            || semanticRule.GetString() != "StartSqlRuleContext"))
                        throw new InvalidDataException("native dialect statement lacks complete parsed syntax/bindings.");
                    runtimeRequirements.Add(new(program, kind.GetString()!, operation.GetString()!, statement.Clone()));
                }
            }
            if (depth > 0)
            {
                if (!target.TryGetProperty("name", out var nameNode) || nameNode.ValueKind != JsonValueKind.String
                    || string.IsNullOrWhiteSpace(nameNode.GetString()))
                    throw new InvalidDataException("dependency export target has no name.");
                var name = nameNode.GetString()!;
                var kind = node.TryGetProperty("dependencyKind", out var kindNode)
                    ? kindNode.GetString()
                    : Path.GetExtension(name).ToLowerInvariant() is ".cpy" or ".cpb" ? "COPY" : "CALL";
                if (kind is not ("COPY" or "CALL" or "EXTERNAL_CONTROL" or "EXTERNAL_RUNTIME" or "CICS_LINK" or "CICS_RUN" or "PLATFORM_COPY"))
                    throw new InvalidDataException($"unsupported explicit dependency kind {kind} for {name}.");
                if (target.TryGetProperty("programReferenceType", out var referenceType)
                    && kind is "CALL" or "EXTERNAL_RUNTIME" or "CICS_LINK" or "CICS_RUN"
                    && referenceType.ToString().Contains("DYNAMIC", StringComparison.OrdinalIgnoreCase))
                    throw new InvalidDataException($"unresolved dynamic CALL {name}; no target was invented.");
                if (kind == "EXTERNAL_RUNTIME")
                {
                    if (Text(target, "programReferenceType") != "STATIC"
                        || !node.TryGetProperty("runtimeContractBindingComplete", out var bound)
                        || bound.ValueKind != JsonValueKind.True
                        || !node.TryGetProperty("dependencyAnalysisComplete", out var analyzed)
                        || analyzed.ValueKind != JsonValueKind.False
                        || node.TryGetProperty("sourcePath", out var body) && body.ValueKind != JsonValueKind.Null
                        || node.TryGetProperty("sourceSha256", out var bodyHash) && bodyHash.ValueKind != JsonValueKind.Null
                        || !node.TryGetProperty("dependencies", out var children)
                        || children.ValueKind != JsonValueKind.Array || children.GetArrayLength() != 0
                        || !node.TryGetProperty("runtimeContract", out var contract)
                        || !ValidRuntimeContract(contract, name))
                        throw new InvalidDataException($"invalid documented platform-service binding for {name}; no COBOL body was parsed.");
                    runtimeRequirements.Add(new(owner, "EXTERNAL_RUNTIME", name, contract.Clone()));
                }
                if (kind == "PLATFORM_COPY")
                    runtimeRequirements.Add(ReadPlatformCopy(node, owner, name));
                if (kind is not ("EXTERNAL_CONTROL" or "PLATFORM_COPY"))
                {
                    var edgeKind = kind is "EXTERNAL_RUNTIME" or "CICS_LINK" or "CICS_RUN" ? "CALL" : kind;
                    supporting.Add((edgeKind, name));
                    if (depth == 1) direct.Add((edgeKind, name));
                }
            }
            if (node.TryGetProperty("dependencies", out var dependencies))
            {
                if (dependencies.ValueKind != JsonValueKind.Array)
                    throw new InvalidDataException("dependency export dependencies must be an array.");
                foreach (var dependency in dependencies.EnumerateArray())
                    Visit(dependency, depth + 1, program);
            }
            else if (depth == 0)
                throw new InvalidDataException($"{identity}: dependency export has no explicit dependency array.");
        }
    }

    private static bool ValidSyntax(JsonElement syntax)
    {
        if (syntax.ValueKind != JsonValueKind.Object) return false;
        if (syntax.TryGetProperty("token", out var token))
            return token.ValueKind == JsonValueKind.String && syntax.EnumerateObject().Count() == 1;
        return syntax.TryGetProperty("rule", out var rule) && rule.ValueKind == JsonValueKind.String
            && syntax.TryGetProperty("children", out var children) && children.ValueKind == JsonValueKind.Array
            && children.EnumerateArray().All(ValidSyntax);
    }

    private static string? Text(JsonElement element, string property) =>
        element.ValueKind == JsonValueKind.Object && element.TryGetProperty(property, out var value)
            && value.ValueKind == JsonValueKind.String ? value.GetString() : null;

    // Omitted DYNAMIC is IBM CICS DYNAMIC(NO); the exporter records that as dynamicProvenance=cics-default.
    private static bool ValidProgramAttributes(JsonElement attributes)
    {
        if (attributes.ValueKind != JsonValueKind.Object
            || Text(attributes, "language") != "COBOL" || Text(attributes, "dynamic") != "NO")
            return false;
        var names = attributes.EnumerateObject().Select(property => property.Name).ToArray();
        // IBM CICS runs COBOL under Language Environment with LANGUAGE(COBOL) or a blank LANGUAGE.
        return names.Length == 2
            || names.Length == 3 && Text(attributes, "dynamicProvenance") == "cics-default"
            || names.Length == 3 && Text(attributes, "languageProvenance") == "cics-blank-language-environment";
    }

    private static void ValidateBmsLayouts(string identity, DependencyEvidence evidence,
        IReadOnlyList<JsonElement> bindings, string sourceRoot, List<string> issues)
    {
        foreach (var source in evidence.Definitions.Where(definition => definition.Kind == "BMS").Distinct())
        {
            // A supplied symbolic map is parsed through its preprocessed COPY_INPUT; a generated map is the COPY.
            var parsed = evidence.Definitions.Where(definition => definition.Kind == "COPY_INPUT"
                && SameTarget(definition.Name, source.Name)).Distinct().ToArray();
            var copies = parsed.Length > 0 ? parsed : evidence.Definitions.Where(definition => definition.Kind == "COPY"
                && SameTarget(definition.Name, source.Name)).Distinct().ToArray();
            var layouts = evidence.Definitions.Where(definition => definition.Kind == "BMS_LAYOUT"
                && SameTarget(definition.Name, source.Name)).Distinct().ToArray();
            if (copies.Length != 1 || layouts.Length != 1
                || !IsWithin(source.Path, sourceRoot) || !IsWithin(layouts[0].Path, sourceRoot)
                || !IsWithin(copies[0].Path, sourceRoot))
            {
                issues.Add($"{identity}: BMS {source.Name} lacks one grounded symbolic layout and COPY.");
                continue;
            }
            using var document = JsonDocument.Parse(File.ReadAllText(layouts[0].Path));
            var proof = document.RootElement;
            if (proof.ValueKind != JsonValueKind.Object
                || !proof.TryGetProperty("schemaVersion", out var version) || version.ValueKind != JsonValueKind.Number
                || !version.TryGetInt32(out var number) || number != 1
                || Text(proof, "producer") != "bms-symbolic-map/layout-v1" || Text(proof, "status") != "complete"
                || Text(proof, "sourceSha256") != source.Sha256 || Text(proof, "outputSha256") != copies[0].Sha256
                || !proof.TryGetProperty("maps", out var maps) || maps.ValueKind != JsonValueKind.Array || maps.GetArrayLength() == 0
                || !bindings.Any(binding => SameTarget(Text(binding, "name") ?? "", source.Name)
                    && Text(binding, "sourceSha256") == source.Sha256
                    && Text(binding, "outputSha256") == copies[0].Sha256
                    && Text(binding, "layoutSha256") == layouts[0].Sha256))
                issues.Add($"{identity}: BMS {source.Name} layout differs from its original source, COPY or native derivation.");
        }
    }

    private const string LanguageEnvironment = "IBM z/OS Language Environment";
    private const string LeServices = "https://www.ibm.com/docs/en/zos/3.1.0?topic=services-";

    // Name -> (platform, required documentation reference, declared parameter count).
    private static readonly Dictionary<string, (string Platform, string Reference, int Count)> RuntimeDocumentation = new()
    {
        ["CEEGMT"] = (LanguageEnvironment, LeServices + "ceegmtget-current-greenwich-mean-time", 3),
        ["CEEDATM"] = (LanguageEnvironment, LeServices + "ceedatmconvert-seconds-character-timestamp", 4),
        ["CEEDAYS"] = (LanguageEnvironment, LeServices + "ceedaysconvert-date-lilian-format", 4),
        ["CEELOCT"] = (LanguageEnvironment, LeServices + "ceeloctget-current-local-date-time", 4),
        ["CEE3ABD"] = (LanguageEnvironment, LeServices + "cee3abdterminate-enclave-abend", 2),
        ["CBLTDLI"] = ("IBM IMS", "https://www.ibm.com/docs/en/SSEPH2_15.4.0/com.ibm.ims154.doc.apg/ims_imsdbcobolapp.htm", 4),
        ["DSNTIAC"] = ("IBM Db2 for z/OS", "https://www.ibm.com/docs/en/db2-for-zos/13.0.0?topic=statements-handling-sql-error-codes-in-cobol-applications", 5),
    };

    private static readonly Dictionary<string, string> AssemblerServices = new()
    {
        ["STIMER"] = "https://www.ibm.com/docs/en/SSLTBW_3.1.0/com.ibm.zos.v3r1.ieaa900/stimer.htm",
    };

    private static readonly HashSet<string> HlasmOperations =
    [
        "START", "CSECT", "DSECT", "USING", "DROP", "EQU", "DS", "DC", "LTORG", "END", "COPY", "MACRO", "MEND",
        "STM", "LM", "L", "ST", "LA", "LR", "SR", "BR", "B", "BE", "BNE", "MVI", "MVC", "CLI", "CLC",
    ];

    private static bool ValidParameters(JsonElement definition) =>
        definition.TryGetProperty("parameters", out var parameters) && parameters.ValueKind == JsonValueKind.Array
        && parameters.EnumerateArray().All(parameter => parameter.ValueKind == JsonValueKind.Object
            && Text(parameter, "passing") == "BY REFERENCE"
            && Text(parameter, "direction") is "in" or "out" or "inout"
            && !string.IsNullOrWhiteSpace(Text(parameter, "name"))
            && !string.IsNullOrWhiteSpace(Text(parameter, "representation")));

    private static bool ValidRuntimeContract(JsonElement binding, string name)
    {
        if (Text(binding, "name") != name
            || Text(binding, "resolutionKind") != "documented-platform-contract"
            || Text(binding, "runtimeMapping") != "unverified"
            || !binding.TryGetProperty("definition", out var definition) || definition.ValueKind != JsonValueKind.Object
            || binding.TryGetProperty("targetResolution", out var resolution) && !ValidDynamicCallResolution(resolution, name))
            return false;
        if (Text(definition, "kind") == "estate-assembler-routine")
            return !RuntimeDocumentation.ContainsKey(name) && AssemblerRoutineProblem(name, definition, null, null) is null;
        return RuntimeDocumentation.TryGetValue(name, out var expected)
            && Text(definition, "kind") == "documented-platform-service"
            && Text(definition, "platform") == expected.Platform
            && definition.TryGetProperty("documentation", out var documentation)
            && documentation.ValueKind == JsonValueKind.Array
            && documentation.EnumerateArray().Any(reference => reference.ValueKind == JsonValueKind.String
                && reference.GetString() == expected.Reference)
            && ValidParameters(definition)
            && definition.GetProperty("parameters").GetArrayLength() == expected.Count;
    }

    private static readonly HashSet<string> CallOperandFollowers =
        ["USING", "RETURNING", "ON", "NOT", "EXCEPTION", "OVERFLOW", "END-CALL", "."];

    /// <summary>CALL identifier proved by the native parser to hold one never-modified program-name constant.</summary>
    private static bool ValidDynamicCallResolution(JsonElement resolution, string name)
    {
        if (resolution.ValueKind != JsonValueKind.Object
            || resolution.EnumerateObject().Select(property => property.Name).Order(StringComparer.Ordinal)
                .SequenceEqual(new[] { "callSyntax", "operand", "targetResolution" }) is false
            || Text(resolution, "operand") is not { } operand || !Regex.IsMatch(operand, "^[A-Z][A-Z0-9-]*$")
            || resolution.GetProperty("callSyntax") is var syntax && (syntax.ValueKind != JsonValueKind.Object
                || Text(syntax, "rule") != "CallStatementContext"))
            return false;
        var tokens = CicsNodes(syntax).Where(node => node.TryGetProperty("token", out _))
            .Select(node => node.GetProperty("token").GetString()!.ToUpperInvariant()).ToList();
        if (tokens.Count < 2 || tokens[0] != "CALL" || tokens[1] != operand
            || tokens.Count > 2 && !CallOperandFollowers.Contains(tokens[2]))
            return false;
        using var synthetic = JsonDocument.Parse(JsonSerializer.Serialize(new
        {
            rule = "Cics_nameContext",
            children = new object[] { new { token = "(" }, new { token = operand }, new { token = ")" } }
        }));
        return CicsConstantOperand(synthetic.RootElement, resolution.GetProperty("targetResolution"), 1, 8)
            is { } value && value.ToUpperInvariant() == name;
    }

    private static IEnumerable<string[]> AssemblerStatements(string text)
    {
        foreach (var raw in text.Split('\n'))
        {
            var line = (raw.Length > 71 ? raw[..71] : raw).TrimEnd('\r', ' ');
            if (line.Trim().Length == 0 || line.StartsWith('*') || line.StartsWith(".*")) continue;
            var fields = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            yield return line[0] == ' ' ? ["", .. fields] : fields;
        }
    }

    /// <summary>Why an estate assembler routine is not a closed, hash-bound leaf; null when it is.</summary>
    private static string? AssemblerRoutineProblem(string name, JsonElement definition, string? sourceRoot, string? estate)
    {
        if (Text(definition, "language") != "IBM High Level Assembler" || Text(definition, "entry") != name
            || !definition.TryGetProperty("includes", out var includes) || includes.ValueKind != JsonValueKind.Array
            || !definition.TryGetProperty("systemServices", out var services) || services.ValueKind != JsonValueKind.Array
            || !ValidParameters(definition) || definition.GetProperty("parameters").GetArrayLength() == 0
            || !definition.TryGetProperty("semantics", out var semantics) || semantics.ValueKind != JsonValueKind.Object
            || !semantics.EnumerateObject().Any()
            || includes.EnumerateArray().Any(item => item.ValueKind != JsonValueKind.Object
                || Text(item, "kind") is not ("macro" or "copy") || string.IsNullOrEmpty(Text(item, "name")))
            || includes.EnumerateArray().Select(item => Text(item, "name")).Distinct().Count() != includes.GetArrayLength()
            || services.EnumerateArray().Any(item => item.ValueKind != JsonValueKind.Object
                || !AssemblerServices.TryGetValue(Text(item, "name") ?? "", out var reference)
                || !item.TryGetProperty("documentation", out var docs) || docs.ValueKind != JsonValueKind.Array
                || docs.GetArrayLength() != 1 || docs[0].GetString() != reference))
            return "assembler routine contract is malformed";
        var files = new[] { definition }.Concat(includes.EnumerateArray())
            .Select(item => (Relative: Text(item, "source"), Sha: Text(item, "sourceSha256"))).ToArray();
        foreach (var (relative, sha) in files)
            if (string.IsNullOrEmpty(relative) || Path.IsPathRooted(relative) || relative.Split('/').Contains("..")
                || sha is null || !System.Text.RegularExpressions.Regex.IsMatch(sha, "^[0-9a-f]{64}$")
                || estate is not null && relative.Split('/')[0] != estate)
                return $"assembler source {relative} is not an estate-relative hash-bound path";
        if (sourceRoot is null) return null;
        var texts = new List<string>();
        foreach (var (relative, sha) in files)
        {
            var path = Path.GetFullPath(Path.Combine(sourceRoot, relative!));
            if (!IsWithin(path, sourceRoot) || !File.Exists(path)
                || Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(File.ReadAllBytes(path)))
                    .ToLowerInvariant() != sha)
                return $"assembler source {relative} differs from its contract hash";
            texts.Add(File.ReadAllText(path, System.Text.Encoding.Latin1));
        }
        var statements = AssemblerStatements(texts[0]).ToList();
        if (!statements.Any(fields => fields[0] == name && fields.Length > 1 && fields[1] is "START" or "CSECT"))
            return $"assembler source does not define entry {name}";
        var used = statements.Where(fields => fields.Length > 1).Select(fields => fields[1]).ToHashSet();
        var index = 1;
        foreach (var item in includes.EnumerateArray())
        {
            var body = AssemblerStatements(texts[index++]).ToList();
            if (Text(item, "kind") == "macro")
            {
                if (body.Count < 3 || body[0] is not ["", "MACRO"] || body[1].Length < 2 || body[1][1] != Text(item, "name")
                    || body[^1].Length < 2 || body[^1][1] != "MEND")
                    return $"macro {Text(item, "name")} does not define its prototype";
                body = body[2..^1];
            }
            used.UnionWith(body.Where(fields => fields.Length > 1).Select(fields => fields[1]));
            statements.AddRange(body);
        }
        var copies = statements.Where(fields => fields.Length > 2 && fields[1] == "COPY").Select(fields => fields[2]).ToHashSet();
        if (!copies.SetEquals(includes.EnumerateArray().Where(item => Text(item, "kind") == "copy").Select(item => Text(item, "name")!)))
            return "assembler COPY members differ from the declared includes";
        var declared = includes.EnumerateArray().Where(item => Text(item, "kind") == "macro").Select(item => Text(item, "name")!)
            .Concat(services.EnumerateArray().Select(item => Text(item, "name")!)).ToHashSet();
        return used.Except(HlasmOperations).ToHashSet().SetEquals(declared)
            ? null : "assembler operations outside the closed contract";
    }

    private static void ValidateTransactions(string identity, DependencyEvidence evidence,
        ExportTree tree, string sourceRoot, List<string> issues)
    {
        foreach (var requirement in tree.RuntimeRequirements.Where(requirement =>
                     requirement.Kind == "EXEC_CICS" && requirement.Operation is "RUN" or "RETURN"))
        {
            var continuation = requirement.Operation == "RETURN";
            if (continuation && CicsOperands(requirement.Evidence.GetProperty("syntax"), "TRANSID").Count == 0)
                continue;
            if (!requirement.Evidence.TryGetProperty("resourceBindings", out var resources)
                || resources.ValueKind != JsonValueKind.Array)
            {
                issues.Add($"{identity}: {requirement.Operation} lacks grounded transaction routing.");
                continue;
            }
            var transactions = resources.EnumerateArray()
                .Where(binding => Text(binding, "kind") == "CICS_TRANSACTION").ToArray();
            if (transactions.Length == 0)
                issues.Add($"{identity}: {requirement.Operation} lacks grounded transaction routing.");
            foreach (var transaction in transactions)
            {
                if (!transaction.TryGetProperty("attributes", out var attributes)
                    || Text(attributes, "dynamic") != "NO" || Text(attributes, "routable") != "NO"
                    || Text(attributes, "status") != "ENABLED"
                    || !Bound(transaction, "CICS_TRANSACTION")
                    || !transaction.TryGetProperty("programBinding", out var program)
                    || Text(program, "kind") != "CICS_PROGRAM"
                    || Text(program, "name") != Text(attributes, "program") || !Bound(program, "CICS_PROGRAM")
                    || !program.TryGetProperty("attributes", out var programAttributes)
                    || !ValidProgramAttributes(programAttributes)
                    || !continuation && !tree.Supporting.Any(edge => edge.Kind == "CALL"
                        && SameTarget(edge.Name, Text(program, "name") ?? ""))
                    || Text(program, "programSourcePath") is not { } programSourcePath
                    || !Path.IsPathFullyQualified(programSourcePath)
                    || !evidence.Definitions.Any(definition => definition.Kind == (continuation ? "CICS_CONTINUATION" : "CALL")
                        && SameTarget(definition.Name, Text(program, "name") ?? "")
                        && definition.Sha256 == Text(program, "programSourceSha256")
                        && Path.GetFileName(definition.Path) == Path.GetFileName(programSourcePath)
                        && IsWithin(definition.Path, sourceRoot))
                    || !transaction.TryGetProperty("targetResolution", out var resolution)
                    || !continuation && Text(resolution, "kind") is not ("literal" or "bounded-varying-string")
                    || continuation && (transactions.Length != 1
                        || Text(transaction, "controlEffect") != "transaction-continuation"
                        || CicsOperands(requirement.Evidence.GetProperty("syntax"), "TRANSID").Count != 1
                        || CicsConstantOperand(CicsOperands(requirement.Evidence.GetProperty("syntax"), "TRANSID")[0],
                            resolution, 4, 4) is not { } continuationName
                        || continuationName != Text(transaction, "name")
                        || !evidence.Definitions.Any(definition => definition.Kind == "CICS_CONTINUATION_SOURCE"
                            && SameTarget(definition.Name, Text(program, "name") ?? "")
                            && IsWithin(definition.Path, sourceRoot))))
                    issues.Add($"{identity}: {requirement.Operation} transaction {Text(transaction, "name")} lacks a local catalog, finite target proof or supplied program source.");
            }
            var bounded = transactions.Where(transaction => transaction.TryGetProperty("targetResolution", out var resolution)
                && Text(resolution, "kind") == "bounded-varying-string").ToArray();
            if (bounded.Length > 0)
            {
                var proof = bounded[0].GetProperty("targetResolution");
                if (bounded.Length != transactions.Length
                    || Text(proof, "usageProof") != "single-native-program:unconditional-string-with-read-only-target-and-counter-uses"
                    || new[] { "definitionSyntax", "counterDefinitionSyntax", "loopHeaderSyntax", "stringSyntax" }
                        .Any(property => !proof.TryGetProperty(property, out var syntax) || !ValidSyntax(syntax))
                    || !proof.TryGetProperty("transactionNames", out var names) || names.ValueKind != JsonValueKind.Array
                    || !names.EnumerateArray().Select(name => name.ValueKind == JsonValueKind.String ? name.GetString() : null)
                        .SequenceEqual(transactions.Select(transaction => Text(transaction, "name")))
                    || bounded.Any(transaction => !JsonElement.DeepEquals(proof, transaction.GetProperty("targetResolution"))))
                    issues.Add($"{identity}: RUN finite transaction set differs from its native construction proof.");
            }
        }

        bool Bound(JsonElement binding, string kind) =>
            !string.IsNullOrWhiteSpace(Text(binding, "sourcePath"))
            && Path.IsPathFullyQualified(Text(binding, "sourcePath")!)
            && evidence.Definitions.Any(definition => definition.Kind == kind
                && definition.Name == Text(binding, "name") && definition.Sha256 == Text(binding, "sourceSha256")
                && IsWithin(definition.Path, sourceRoot)
                && Path.GetFileName(definition.Path) == Path.GetFileName(Text(binding, "sourcePath")));
    }

    private static void ValidateProfiles(string identity, DependencyEvidence evidence,
        IReadOnlyList<RuntimeImplementationRequirement> requirements, string sourceRoot, List<string> issues)
    {
        foreach (var requirement in requirements)
        {
            if (requirement.Kind == "EXTERNAL_RUNTIME")
            {
                Validate(requirement.Evidence, "RUNTIME_CONTRACT", requirement.Operation, runtime: true);
                if (requirement.Evidence.TryGetProperty("definition", out var routine)
                    && Text(routine, "kind") == "estate-assembler-routine")
                {
                    var estate = identity.Replace('\\', '/').Split('/')[0];
                    var problem = AssemblerRoutineProblem(requirement.Operation, routine, sourceRoot, estate);
                    var sources = new[] { routine }.Concat(routine.GetProperty("includes").EnumerateArray());
                    if (problem is not null || !sources.All(source => evidence.Definitions.Any(definition =>
                            definition.Kind == "ASSEMBLER_SOURCE" && definition.Sha256 == Text(source, "sourceSha256")
                            && IsWithin(definition.Path, sourceRoot)
                            && Path.GetFullPath(definition.Path) == Path.GetFullPath(Path.Combine(sourceRoot, Text(source, "source")!)))))
                        issues.Add($"{identity}: assembler routine {requirement.Operation} is not a closed hash-bound estate source: {problem ?? "missing ASSEMBLER_SOURCE evidence"}.");
                }
            }
            else if (requirement.Kind == "EXEC_SQL"
                     && requirement.Evidence.TryGetProperty("resourceBindings", out var bindings)
                     && bindings.ValueKind == JsonValueKind.Array)
                foreach (var binding in bindings.EnumerateArray())
                {
                    if (binding.TryGetProperty("qualification", out var qualification))
                        Validate(qualification, "ANALYSIS_PROFILE", Text(qualification, "profileName"), runtime: false);
                    if (Text(binding, "definitionScope") != "explicit-sql-catalog") continue;
                    var declarations = evidence.Definitions.Where(definition => definition.Kind == "SQL_DECLARATION"
                        && definition.Name == Text(binding, "name")
                        && definition.Sha256 == Text(binding, "sourceSha256")
                        && IsWithin(definition.Path, sourceRoot)).ToArray();
                    if (Text(binding, "kind") != "SQL_RELATION" || declarations.Length != 1
                        || string.IsNullOrWhiteSpace(Text(binding, "sourcePath"))
                        || !Path.IsPathFullyQualified(Text(binding, "sourcePath")!)
                        || declarations.Length == 1
                        && Path.GetFileName(declarations[0].Path) != Path.GetFileName(Text(binding, "sourcePath"))
                        || !binding.TryGetProperty("definitionSyntax", out var declarationSyntax)
                        || Text(declarationSyntax, "rule") != "Dbs_declare_tableContext")
                        issues.Add($"{identity}: SQL declaration {Text(binding, "name")} lacks a hash-bound source catalog.");
                }
        }

        void Validate(JsonElement binding, string kind, string? name, bool runtime)
        {
            var definitions = evidence.Definitions.Where(definition => definition.Kind == kind
                && definition.Name == name && definition.Sha256 == Text(binding, "sourceSha256")).ToArray();
            if (Text(binding, "profileKind") != "target-port" || string.IsNullOrWhiteSpace(Text(binding, "profileName"))
                || definitions.Length != 1)
            {
                issues.Add($"{identity}: {kind} {name} lacks a hash-bound explicit target profile.");
                return;
            }
            using var document = JsonDocument.Parse(File.ReadAllText(definitions[0].Path));
            var profile = document.RootElement;
            var valid = profile.TryGetProperty("schemaVersion", out var version)
                && version.ValueKind == JsonValueKind.Number && version.TryGetInt32(out var number) && number == 1
                && Text(profile, "profileKind") == Text(binding, "profileKind")
                && Text(profile, "profileName") == Text(binding, "profileName");
            if (runtime)
                valid = valid && profile.TryGetProperty("runtimeContracts", out var contracts)
                    && contracts.ValueKind == JsonValueKind.Object && contracts.TryGetProperty(name!, out var definition)
                    && binding.TryGetProperty("definition", out var exported) && JsonElement.DeepEquals(definition, exported);
            else
                valid = valid && !string.IsNullOrWhiteSpace(Text(binding, "schema"))
                    && Text(profile, "sqlDefaultSchema") == Text(binding, "schema");
            if (!valid) issues.Add($"{identity}: {kind} {name} differs from its current configured profile.");
        }
    }
}
