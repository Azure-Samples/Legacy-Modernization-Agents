using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Persistence;
using CobolToQuarkusMigration.Persistence.GraphIngest;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Persistence;

public class GraphUidTests
{
    // Expected values come from the retired Python populator's _make_uid, which graph-ingest preserves:
    // uuid.uuid5(uuid.NAMESPACE_URL, "/".join(str(p) for p in parts))
    [Theory]
    [InlineData("261a8559-0a72-54da-9ef9-91bdc5dd4251", 5, "CREACC.cbl")]
    [InlineData("7a142981-c071-59cb-a56b-c3a3324085cd", 5, "CREACC.cbl", "chunk", 0)]
    [InlineData("cbccc17f-16b6-5baa-9a46-10ddfbf1ada3", 12, "X.cbl", "PROC-A")]
    public void Make_MatchesTheOriginalPopulator(string expected, params object[] parts)
    {
        Assert.Equal(expected, GraphUid.Make(parts));
    }

    [Fact]
    public void SchemaStatements_UseTheGraphIngestConstraintNames()
    {
        var schema = string.Join(";\n", GraphSchema.Statements);

        foreach (var statement in Neo4jMigrationRepository.SchemaStatements)
        {
            var name = Regex.Match(statement, @"CREATE CONSTRAINT (\w+)").Groups[1].Value;
            Assert.Matches(new Regex($@"CREATE CONSTRAINT {name}\s"), schema);
        }
    }
}
