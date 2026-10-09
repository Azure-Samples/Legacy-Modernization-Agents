using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using FluentAssertions;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// --reuse-re used to take the newest business logic from any run and match it to source files
// by bare file name. Reverse-engineering one estate and then converting another fed the first
// estate's rules into the second one's prompts, and an edited program kept receiving the rules
// of its previous version.
public sealed class BusinessLogicReuseTests : IDisposable
{
    private readonly string _root = Path.Join(
        Path.GetTempPath(), "bl-reuse-" + Guid.NewGuid().ToString("N"));

    public BusinessLogicReuseTests() => Directory.CreateDirectory(_root);

    public void Dispose()
    {
        SqliteConnection.ClearAllPools();
        try { Directory.Delete(_root, recursive: true); } catch (IOException) { }
    }

    private static BusinessLogic Logic(string path) => new()
    {
        FileName = Path.GetFileName(path),
        FilePath = path,
        BusinessPurpose = "purpose of " + path
    };

    [Fact]
    public void Find_prefers_path_and_refuses_ambiguous_name()
    {
        var a = Logic(Path.Join(_root, "a", "ACCT.cbl"));
        var b = Logic(Path.Join(_root, "b", "ACCT.cbl"));
        var list = new List<BusinessLogic> { a, b };

        BusinessLogicReuse.Find(list, "ACCT.cbl", Path.Join(_root, "b", "ACCT.cbl")).Should().BeSameAs(b);
        BusinessLogicReuse.Find(list, "ACCT.cbl").Should().BeNull("two files share the name");
        BusinessLogicReuse.Find(new[] { a }, "acct.cbl").Should().BeSameAs(a);
        BusinessLogicReuse.Find(list, "OTHER.cbl").Should().BeNull();
    }

    [Fact]
    public void FilterStale_drops_changed_and_missing_files()
    {
        var same = Path.Join(_root, "SAME.cbl");
        var edited = Path.Join(_root, "EDITED.cbl");
        var gone = Path.Join(_root, "GONE.cbl");
        File.WriteAllText(same, "LINE1\r\nLINE2\r\n");
        File.WriteAllText(edited, "NEW CONTENT");

        var snapshot = new List<CobolFile>
        {
            new() { FileName = "SAME.cbl", FilePath = same, Content = "LINE1\nLINE2\n" },
            new() { FileName = "EDITED.cbl", FilePath = edited, Content = "OLD CONTENT" },
            new() { FileName = "GONE.cbl", FilePath = gone, Content = "X" },
        };

        var result = BusinessLogicReuse.FilterStale(
            new[] { Logic(same), Logic(edited), Logic(gone) }, snapshot, BusinessLogicReuse.ReadFileOrNull);

        result.Fresh.Select(b => b.FileName).Should().Equal("SAME.cbl");
        result.Changed.Should().Equal("EDITED.cbl");
        result.Missing.Should().Equal("GONE.cbl");
    }

    [Fact]
    public async Task Latest_run_is_scoped_to_the_source_folder()
    {
        var repo = new SqliteMigrationRepository(
            Path.Join(_root, "migration.db"), NullLogger<SqliteMigrationRepository>.Instance);
        await repo.InitializeAsync();

        var estateA = Path.Join(_root, "estate-a");
        var estateB = Path.Join(_root, "estate-b");

        var runA = await repo.StartRunAsync(estateA, "out");
        await repo.SaveBusinessLogicAsync(runA, new[] { Logic(Path.Join(estateA, "P.cbl")) });
        var runB = await repo.StartRunAsync(estateB, "out");
        await repo.SaveBusinessLogicAsync(runB, new[] { Logic(Path.Join(estateB, "P.cbl")) });

        (await repo.GetLatestRunIdWithBusinessLogicAsync()).Should().Be(runB);
        (await repo.GetLatestRunIdWithBusinessLogicAsync(estateA + Path.DirectorySeparatorChar)).Should().Be(runA);
        (await repo.GetLatestRunIdWithBusinessLogicAsync(Path.Join(_root, "estate-c"))).Should().BeNull();
    }
}
