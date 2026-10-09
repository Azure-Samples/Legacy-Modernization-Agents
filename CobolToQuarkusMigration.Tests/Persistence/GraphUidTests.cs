using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Persistence;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Persistence;

public class GraphUidTests
{
    // Expected values come from the populator's _make_uid:
    // uuid.uuid5(uuid.NAMESPACE_URL, "/".join(str(p) for p in parts))
    [Theory]
    [InlineData("261a8559-0a72-54da-9ef9-91bdc5dd4251", 5, "CREACC.cbl")]
    [InlineData("7a142981-c071-59cb-a56b-c3a3324085cd", 5, "CREACC.cbl", "chunk", 0)]
    [InlineData("cbccc17f-16b6-5baa-9a46-10ddfbf1ada3", 12, "X.cbl", "PROC-A")]
    public void Make_MatchesThePopulator(string expected, params object[] parts)
    {
        Assert.Equal(expected, GraphUid.Make(parts));
    }

    [Fact]
    public void SchemaStatements_UseThePopulatorsConstraintNames()
    {
        var schemaPath = Path.Combine(FindRepoRoot(), "tools", "graph-populator", "schema.cypher");
        var schema = File.ReadAllText(schemaPath);

        foreach (var statement in Neo4jMigrationRepository.SchemaStatements)
        {
            var name = Regex.Match(statement, @"CREATE CONSTRAINT (\w+)").Groups[1].Value;
            Assert.Matches(new Regex($@"CREATE CONSTRAINT {name}\s"), schema);
        }
    }

    private static string FindRepoRoot()
    {
        var dir = new DirectoryInfo(AppContext.BaseDirectory);
        while (dir is not null && !File.Exists(Path.Combine(dir.FullName, "docker-compose.yml")))
        {
            dir = dir.Parent;
        }
        return dir?.FullName ?? throw new DirectoryNotFoundException("Repository root not found");
    }
}
