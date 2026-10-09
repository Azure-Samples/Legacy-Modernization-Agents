using CobolToQuarkusMigration.Models;
using CobolToQuarkusMigration.Persistence;
using Microsoft.Extensions.Logging.Abstractions;
using Neo4j.Driver;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Persistence;

public class Neo4jConnectionTests
{
    [Fact]
    public void Describe_AuthenticationFailure_PointsAtThePasswordSetting()
    {
        var message = Neo4jConnection.Describe(new AuthenticationException("bad"), "bolt://localhost:7687");

        Assert.Contains("NEO4J_PASSWORD", message);
        Assert.Contains("bolt://localhost:7687", message);
    }

    [Fact]
    public void Describe_Unreachable_PointsAtStartingTheContainer()
    {
        var message = Neo4jConnection.Describe(new ServiceUnavailableException("down"), "bolt://localhost:7699");

        Assert.Contains("not reachable", message);
        Assert.Contains("NEO4J_BOLT_PORT", message);
    }

    [Fact]
    public async Task TryOpen_WhenDisabled_ReturnsNullWithoutConnecting()
    {
        var repository = await Neo4jConnection.TryOpenAsync(
            new Neo4jSettings { Enabled = false }, NullLoggerFactory.Instance, NullLogger.Instance);

        Assert.Null(repository);
    }

    [Fact]
    public async Task TryOpen_WhenNothingListens_ReturnsNullInsteadOfThrowing()
    {
        var repository = await Neo4jConnection.TryOpenAsync(
            new Neo4jSettings { Enabled = true, Uri = "bolt://127.0.0.1:1", Password = "x", ConnectTimeoutSeconds = 2 },
            NullLoggerFactory.Instance, NullLogger.Instance);

        Assert.Null(repository);
    }
}
