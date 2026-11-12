using System.Globalization;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Agents.Infrastructure.Facts;

public sealed partial class DependencyEvidenceGate
{
    // Mirrors tools/rekt_dependency_evidence.py evidence_current: an unchanged closure file can still be stale
    // when an estate participant was added or changed, so participants and slot proofs are rechecked here.
    private void ValidateCommareaClosures(
        string identity, DependencyEvidence evidence, string exportPath, string sourceRoot, List<string> issues)
    {
        var closures = evidence.Definitions.Where(definition => definition.Kind == "COMMAREA_CLOSURE").ToList();
        using var export = JsonDocument.Parse(File.ReadAllText(exportPath));
        var proofs = ClosedWorldProofs(export.RootElement).ToList();
        if (closures.Count == 0)
        {
            if (proofs.Count > 0)
                issues.Add($"{identity}: closed-world COMMAREA target proof has no hash-bound closure definition.");
            return;
        }
        var estate = identity.Split('/')[0];
        if (closures.Count != 1 || !identity.Contains('/') || closures[0].Name != estate)
        {
            issues.Add($"{identity}: COMMAREA closure definition is not bound to this program's estate.");
            return;
        }
        var problem = ClosureProblem(closures[0], estate, sourceRoot, identity, export.RootElement, proofs);
        if (problem is not null)
            issues.Add($"{identity}: COMMAREA closure is stale against current estate inputs: {problem}.");
    }

    private string? ClosureProblem(DependencyDefinition definition, string estate, string sourceRoot,
        string identity, JsonElement export, IReadOnlyList<JsonElement> proofs)
    {
        using var document = JsonDocument.Parse(File.ReadAllText(definition.Path));
        var closure = document.RootElement;
        if (closure.ValueKind != JsonValueKind.Object
            || !closure.TryGetProperty("schema", out var schema) || schema.ValueKind != JsonValueKind.Number
            || schema.GetInt32() != 1 || Text(closure, "estate") != estate
            || !closure.TryGetProperty("errors", out var errors) || errors.ValueKind != JsonValueKind.Array
            || errors.GetArrayLength() != 0
            || !closure.TryGetProperty("participants", out var participants)
            || participants.ValueKind != JsonValueKind.Array)
            return "closure is malformed or records errors";

        var estateRoot = Path.Combine(Path.GetFullPath(sourceRoot), estate);
        var current = Directory.Exists(estateRoot)
            ? Directory.EnumerateFiles(estateRoot, "*", SearchOption.AllDirectories)
                .Where(path => Path.GetExtension(path).ToLowerInvariant() is ".cbl" or ".cob"
                    && !Path.GetRelativePath(estateRoot, path).Split('/', '\\').Any(part => part.StartsWith('.')))
                .ToDictionary(path => SourcePathHelper.NormalizeRelativePath(
                    Path.GetRelativePath(Path.GetFullPath(sourceRoot), path)), path => path, StringComparer.Ordinal)
            : new Dictionary<string, string>(StringComparer.Ordinal);
        var recorded = participants.EnumerateArray().Select(participant => Text(participant, "relativePath")).ToList();
        if (recorded.Any(path => path is null) || recorded.Count != current.Count
            || !recorded.All(path => current.ContainsKey(path!)))
            return "participant set differs from the current estate sources";
        foreach (var participant in participants.EnumerateArray())
        {
            var relative = Text(participant, "relativePath")!;
            var sourceHash = Sha256(File.ReadAllBytes(current[relative]));
            if (!string.Equals(Text(participant, "sourceSha256"), sourceHash, StringComparison.OrdinalIgnoreCase))
                return $"participant {relative} source hash changed";
            var inventory = CurrentInventory(relative, sourceHash);
            var mode = Text(participant, "mode");
            if (mode == "lexical" ? inventory is not null
                : mode != "inventory" || inventory is null
                  || !string.Equals(Text(participant, "inventorySha256"), Sha256(Encoding.UTF8.GetBytes(PythonJson(inventory.Value))),
                      StringComparison.OrdinalIgnoreCase))
                return $"participant {relative} inventory differs";
        }

        if (proofs.Count == 0) return null;
        var programSha = Text(export, "sourceSha256");
        if (programSha is null || !closure.TryGetProperty("programs", out var programs)
            || programs.ValueKind != JsonValueKind.Object
            || !programs.TryGetProperty(programSha, out var program) || Text(program, "relativePath") != identity
            || !program.TryGetProperty("slots", out var slots) || slots.ValueKind != JsonValueKind.Object)
            return "closure does not describe this program";
        foreach (var proof in proofs)
        {
            if (!string.Equals(Text(proof, "closureSha256"), definition.Sha256, StringComparison.OrdinalIgnoreCase)
                || Text(proof, "programSourceSha256") != programSha
                || Text(proof, "operand") is not { } operand || !slots.TryGetProperty(operand, out var universe)
                || !proof.TryGetProperty("universe", out var proved) || !JsonElement.DeepEquals(universe, proved))
                return $"slot proof {Text(proof, "operand")} differs from the closure";
        }
        return null;
    }

    private JsonElement? CurrentInventory(string relative, string sourceHash)
    {
        try
        {
            var evidence = LoadEvidence(factsDirectory, relative);
            var exportPath = Path.Combine(factsDirectory, SourcePathHelper.ToOsRelativePath(relative) + "-deps.json");
            if (evidence?.OriginalSourceSha256 is not { } recorded
                || !string.Equals(recorded, sourceHash, StringComparison.OrdinalIgnoreCase) || !File.Exists(exportPath))
                return null;
            using var export = JsonDocument.Parse(File.ReadAllText(exportPath));
            return export.RootElement.ValueKind == JsonValueKind.Object
                && export.RootElement.TryGetProperty("commareaInventory", out var inventory)
                && inventory.ValueKind == JsonValueKind.Object
                ? inventory.Clone() : null;
        }
        catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or JsonException)
        {
            return null;
        }
    }

    private static IEnumerable<JsonElement> ClosedWorldProofs(JsonElement export)
    {
        if (export.ValueKind != JsonValueKind.Object
            || !export.TryGetProperty("dependencyDialectStatements", out var statements)
            || statements.ValueKind != JsonValueKind.Array)
            yield break;
        foreach (var statement in statements.EnumerateArray())
        {
            if (statement.ValueKind != JsonValueKind.Object
                || !statement.TryGetProperty("resourceBindings", out var bindings) || bindings.ValueKind != JsonValueKind.Array)
                continue;
            foreach (var binding in bindings.EnumerateArray())
                if (Text(binding, "kind") == "CICS_PROGRAM"
                    && binding.TryGetProperty("targetResolution", out var resolution)
                    && Text(resolution, "kind") == "closed-world-commarea-slot")
                    yield return resolution;
        }
    }

    private static string Sha256(byte[] bytes) => Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant();

    // Python json.dumps(value, sort_keys=True) with default separators and ensure_ascii.
    private static string PythonJson(JsonElement value)
    {
        var builder = new StringBuilder();
        Write(value);
        return builder.ToString();

        void Write(JsonElement element)
        {
            switch (element.ValueKind)
            {
                case JsonValueKind.Object:
                    builder.Append('{');
                    var first = true;
                    foreach (var property in element.EnumerateObject().OrderBy(property => property.Name, StringComparer.Ordinal))
                    {
                        if (!first) builder.Append(", ");
                        first = false;
                        WriteString(property.Name);
                        builder.Append(": ");
                        Write(property.Value);
                    }
                    builder.Append('}');
                    break;
                case JsonValueKind.Array:
                    builder.Append('[');
                    var index = 0;
                    foreach (var item in element.EnumerateArray())
                    {
                        if (index++ > 0) builder.Append(", ");
                        Write(item);
                    }
                    builder.Append(']');
                    break;
                case JsonValueKind.String: WriteString(element.GetString()!); break;
                case JsonValueKind.True: builder.Append("true"); break;
                case JsonValueKind.False: builder.Append("false"); break;
                case JsonValueKind.Null: builder.Append("null"); break;
                default: builder.Append(element.GetRawText()); break;
            }
        }

        void WriteString(string text)
        {
            builder.Append('"');
            foreach (var character in text)
            {
                builder.Append(character switch
                {
                    '"' => "\\\"",
                    '\\' => "\\\\",
                    '\n' => "\\n",
                    '\r' => "\\r",
                    '\t' => "\\t",
                    '\b' => "\\b",
                    '\f' => "\\f",
                    _ when character is < ' ' or > '~' => "\\u" + ((int)character).ToString("x4", CultureInfo.InvariantCulture),
                    _ => character.ToString()
                });
            }
            builder.Append('"');
        }
    }
}
