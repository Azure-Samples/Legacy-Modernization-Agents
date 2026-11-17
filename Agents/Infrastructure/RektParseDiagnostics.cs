using System.Text.Json;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

// Reads what smojol persisted for one program. Its exit code alone is not evidence: it can
// exit 0 without writing the AST or control flow, or after recovering past missing copybooks.
public static class RektParseDiagnostics
{
    public static readonly IReadOnlyList<string> StructureDirectories = ["flow_ast", "cfg"];

    // Throws JsonException for an unreadable artifact so callers can tell it apart from an absent one.
    public static bool HasStructure(string reportDirectory, IEnumerable<string>? directories = null)
    {
        foreach (var directory in directories ?? StructureDirectories)
        {
            var folder = Path.Join(reportDirectory, directory);
            var artifacts = Directory.Exists(folder) ? Directory.GetFiles(folder, "*.json") : [];
            if (artifacts.Length == 0)
                return false;
            foreach (var artifact in artifacts)
            {
                using var stream = File.OpenRead(artifact);
                using var document = JsonDocument.Parse(stream);
                if (!IsNonEmptyContainer(document.RootElement))
                    return false;
            }
        }
        return true;
    }

    public static IReadOnlyList<string> MissingCopybookWarnings(string diagnosticsPath)
    {
        using var stream = File.OpenRead(diagnosticsPath);
        using var document = JsonDocument.Parse(stream);
        if (document.RootElement.ValueKind != JsonValueKind.Array)
            throw new InvalidDataException($"{diagnosticsPath}: parse diagnostics must be a JSON array.");

        var warnings = new SortedSet<string>(StringComparer.Ordinal);
        foreach (var diagnostic in document.RootElement.EnumerateArray())
        {
            if (diagnostic.ValueKind != JsonValueKind.Object)
                throw new InvalidDataException($"{diagnosticsPath}: each diagnostic must be a JSON object.");
            if (!diagnostic.TryGetProperty("code", out var code) || code.ValueKind != JsonValueKind.String
                || code.GetString() != "missing copybook")
                continue;
            if (!diagnostic.TryGetProperty("message", out var message) || message.ValueKind != JsonValueKind.String)
                throw new InvalidDataException($"{diagnosticsPath}: a missing-copybook diagnostic has no message.");
            // doctor.sh joins warnings with '|', so a message must not introduce its own separator.
            warnings.Add($"missing-copybook:{message.GetString()}".Replace('|', '/'));
        }
        return [.. warnings];
    }

    private static bool IsNonEmptyContainer(JsonElement value) => value.ValueKind switch
    {
        JsonValueKind.Object => value.EnumerateObject().Any(),
        JsonValueKind.Array => value.GetArrayLength() > 0,
        _ => false,
    };
}
