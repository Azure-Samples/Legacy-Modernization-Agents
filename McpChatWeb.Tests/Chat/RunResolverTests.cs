using System;
using System.IO;
using System.Threading.Tasks;
using McpChatWeb.Services;
using Microsoft.Data.Sqlite;
using Xunit;

namespace McpChatWeb.Tests.Chat;

// The chat feature was built against run 43 and kept that number as its fallback. On the estate
// it was written against, 43 was the newest run; on any other it is an old run, and on a fresh
// database it does not exist. The answer came back worded with the same confidence either way,
// describing whichever programs happened to be numbered 43.
public class RunResolverTests : IDisposable
{
    private readonly string _dir = Path.Combine(
        Path.GetTempPath(), "run-resolver-" + Guid.NewGuid().ToString("N"));

    private string DbPath => Path.Combine(_dir, "migration.db");

    public RunResolverTests() => Directory.CreateDirectory(_dir);

    public void Dispose()
    {
        SqliteConnection.ClearAllPools();
        try { Directory.Delete(_dir, recursive: true); } catch (IOException) { }
    }

    private async Task SeedRunsAsync(params int[] runIds)
    {
        await using var connection = new SqliteConnection($"Data Source={DbPath}");
        await connection.OpenAsync();

        await using (var create = connection.CreateCommand())
        {
            create.CommandText = "CREATE TABLE runs (id INTEGER PRIMARY KEY, status TEXT)";
            await create.ExecuteNonQueryAsync();
        }

        foreach (var id in runIds)
        {
            await using var insert = connection.CreateCommand();
            insert.CommandText = "INSERT INTO runs (id, status) VALUES ($id, 'Completed')";
            insert.Parameters.AddWithValue("$id", id);
            await insert.ExecuteNonQueryAsync();
        }
    }

    // ── Reading the run out of the question ─────────────────────────────

    [Theory]
    [InlineData("Show me all dependencies for run 43", 43)]
    [InlineData("what does run 7 contain", 7)]
    [InlineData("run id 12 summary", 12)]
    [InlineData("RUN #99 please", 99)]
    [InlineData("Run   id   5", 5)]
    public void TheRunNamedInTheQuestionIsUsed(string prompt, int expected)
    {
        Assert.Equal(expected, RunResolver.ParseRunFromPrompt(prompt));
    }

    [Theory]
    [InlineData("what programs are in the estate")]
    [InlineData("")]
    [InlineData(null)]
    [InlineData("run the conversion again")]   // no number follows
    public void AQuestionThatNamesNoRunReturnsNothingToParse(string? prompt)
    {
        Assert.Null(RunResolver.ParseRunFromPrompt(prompt));
    }

    [Fact]
    public void ARunNumberOfZeroIsNotARun()
    {
        Assert.Null(RunResolver.ParseRunFromPrompt("run 0"));
    }

    // ── Falling back to the newest run ──────────────────────────────────

    [Fact]
    public async Task TheNewestRunIsTheDefaultRatherThanTheOneThisWasBuiltAgainst()
    {
        await SeedRunsAsync(41, 42, 43, 59, 60);

        Assert.Equal(60, await RunResolver.LatestRunIdAsync(DbPath));
        Assert.Equal(60, await RunResolver.ResolveAsync("what changed in the estate", DbPath));
    }

    [Fact]
    public async Task AnExplicitRunStillWinsOverTheNewestOne()
    {
        await SeedRunsAsync(41, 42, 43, 60);

        Assert.Equal(43, await RunResolver.ResolveAsync("dependencies for run 43", DbPath));
    }

    // Answering "there are no runs" is better than answering about a run that does not exist.
    [Fact]
    public async Task AnEmptyDatabaseResolvesToNoRunRatherThanToAGuess()
    {
        await SeedRunsAsync();

        Assert.Null(await RunResolver.LatestRunIdAsync(DbPath));
        Assert.Null(await RunResolver.ResolveAsync("what happened", DbPath));
    }

    [Fact]
    public async Task AMissingDatabaseResolvesToNoRun()
    {
        Assert.Null(await RunResolver.LatestRunIdAsync(Path.Combine(_dir, "absent.db")));
    }

    [Fact]
    public async Task AnUnreadableDatabaseDoesNotFailTheQuestion()
    {
        await File.WriteAllTextAsync(DbPath, "this is not a sqlite database");

        Assert.Null(await RunResolver.LatestRunIdAsync(DbPath));
    }

    [Fact]
    public async Task ANamedRunIsHonouredEvenWithoutADatabase()
    {
        Assert.Equal(8, await RunResolver.ResolveAsync("run 8", Path.Combine(_dir, "absent.db")));
    }
}
