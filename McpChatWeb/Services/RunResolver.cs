// Decides which migration run a chat question is about.
//
// The chat feature was built against a single run and kept that run's number, 43, as the fallback
// wherever the question did not name one. On the estate it was written against that was the
// newest run; on any other it is an old run, and on a fresh database it does not exist at all.
// The answer still came back worded with full confidence, describing programs from whichever run
// happened to be numbered 43.
//
// Resolving the newest run instead makes the default correct as the estate moves on, and
// reporting "no runs" is a better answer than confidently describing someone else's.

namespace McpChatWeb.Services;

using System.Text.RegularExpressions;
using Microsoft.Data.Sqlite;

public static class RunResolver
{
    private static readonly Regex RunInPrompt = new(
        @"\brun\s*(?:id\s*)?#?\s*(\d{1,9})\b",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>The run the question names, or null when it does not name one.</summary>
    public static int? ParseRunFromPrompt(string? prompt)
    {
        if (string.IsNullOrWhiteSpace(prompt)) return null;

        var match = RunInPrompt.Match(prompt);
        if (!match.Success) return null;

        return int.TryParse(match.Groups[1].Value, out var runId) && runId > 0 ? runId : null;
    }

    /// <summary>The newest run recorded in the migration database, or null when there are none.</summary>
    public static async Task<int?> LatestRunIdAsync(
        string databasePath, CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrWhiteSpace(databasePath) || !File.Exists(databasePath)) return null;

        try
        {
            // Read-only so a question asked while a migration is writing cannot block it.
            await using var connection = new SqliteConnection($"Data Source={databasePath};Mode=ReadOnly");
            await connection.OpenAsync(cancellationToken).ConfigureAwait(false);

            await using var command = connection.CreateCommand();
            command.CommandText = "SELECT MAX(id) FROM runs";

            var result = await command.ExecuteScalarAsync(cancellationToken).ConfigureAwait(false);
            if (result is null || result is DBNull) return null;

            var latest = Convert.ToInt32(result);
            return latest > 0 ? latest : null;
        }
        catch (SqliteException)
        {
            // A missing or half-written database is not an error worth failing the question over;
            // the caller reports that no run could be determined.
            return null;
        }
    }

    /// <summary>
    /// The run a question is about: the one it names, otherwise the newest one on record.
    /// </summary>
    public static async Task<int?> ResolveAsync(
        string? prompt, string databasePath, CancellationToken cancellationToken = default)
        => ParseRunFromPrompt(prompt)
           ?? await LatestRunIdAsync(databasePath, cancellationToken).ConfigureAwait(false);
}
