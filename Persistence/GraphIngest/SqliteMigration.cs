using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Persistence.GraphIngest;

// Reads migration.db rows as sqlite3.Row dicts were read: optional columns fall back to a
// default when absent, required ones fail, and a NULL stays null.
internal sealed class SqliteMigration(SqliteConnection connection)
{
    public sealed class Row(string table, Dictionary<string, object?> values)
    {
        public object? Required(string column) => values.TryGetValue(column, out var value)
            ? value
            : throw new InvalidDataException($"Column '{column}' is missing from the SQLite table '{table}'.");

        public object? Get(string column, object? fallback) => values.TryGetValue(column, out var value) ? value : fallback;
    }

    // Null when the table does not exist; any other SQLite error propagates.
    public List<Row>? Rows(string table)
    {
        using var exists = connection.CreateCommand();
        exists.CommandText = "SELECT 1 FROM sqlite_master WHERE type IN ('table', 'view') AND name = $name";
        exists.Parameters.AddWithValue("$name", table);
        if (exists.ExecuteScalar() is null)
        {
            return table is "runs" or "cobol_files" or "dependencies"
                ? throw new InvalidDataException($"The SQLite database has no '{table}' table.")
                : null;
        }

        using var command = connection.CreateCommand();
        command.CommandText = $"SELECT * FROM \"{table}\"";
        using var reader = command.ExecuteReader();
        var rows = new List<Row>();
        while (reader.Read())
        {
            var values = new Dictionary<string, object?>();
            for (var i = 0; i < reader.FieldCount; i++)
            {
                values[reader.GetName(i)] = reader.IsDBNull(i) ? null : reader.GetValue(i);
            }
            rows.Add(new Row(table, values));
        }
        return rows;
    }

    public static bool Truthy(object? value) => value switch
    {
        null => false,
        long l => l != 0,
        double d => d != 0,
        string s => s.Length > 0,
        byte[] b => b.Length > 0,
        _ => true,
    };
}
