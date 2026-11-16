namespace CobolToQuarkusMigration.Persistence.GraphIngest;

// Constraints and indexes for the graph, embedded so the published CLI needs no file beside it.
public static class GraphSchema
{
    public static IReadOnlyList<string> Statements { get; } = Parse(ReadEmbedded());

    internal static IReadOnlyList<string> Parse(string text)
    {
        var withoutComments = string.Join("\n", text.Split('\n')
            .Select(line => line.TrimEnd('\r'))
            .Where(line => !line.Trim().StartsWith("//", StringComparison.Ordinal)));
        return withoutComments.Split(';')
            .Select(statement => statement.Trim())
            .Where(statement => statement.Length > 0)
            .ToList();
    }

    private static string ReadEmbedded()
    {
        using var stream = typeof(GraphSchema).Assembly.GetManifestResourceStream("GraphIngest.schema.cypher")
            ?? throw new InvalidOperationException("The graph schema resource is missing from the build.");
        using var reader = new StreamReader(stream);
        return reader.ReadToEnd();
    }
}
