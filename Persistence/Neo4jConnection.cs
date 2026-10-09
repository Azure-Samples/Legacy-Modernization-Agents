using Microsoft.Extensions.Logging;
using Neo4j.Driver;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Persistence;

/// <summary>
/// Opens the migration graph once, up front. Creating a driver does not contact the server, so a
/// wrong password or a stopped container used to surface as a failure on every later save while
/// the run had already reported "connected".
/// </summary>
public static class Neo4jConnection
{
    public static async Task<Neo4jMigrationRepository?> TryOpenAsync(
        Neo4jSettings? settings, ILoggerFactory loggerFactory, Microsoft.Extensions.Logging.ILogger logger)
    {
        if (settings?.Enabled != true)
        {
            return null;
        }

        var driver = GraphDatabase.Driver(
            settings.Uri,
            AuthTokens.Basic(settings.Username, settings.Password),
            o => o.WithConnectionTimeout(TimeSpan.FromSeconds(Math.Max(1, settings.ConnectTimeoutSeconds))));

        try
        {
            await driver.VerifyConnectivityAsync();
            logger.LogInformation("✅ Neo4j graph database connected at {Uri}", settings.Uri);
            var repository = new Neo4jMigrationRepository(driver, loggerFactory.CreateLogger<Neo4jMigrationRepository>());
            await repository.EnsureSchemaAsync();
            return repository;
        }
        catch (Exception ex)
        {
            await driver.DisposeAsync();
            logger.LogWarning("⚠️  {Problem} Continuing with SQLite only; the dependency graph is still saved there.",
                Describe(ex, settings.Uri));
            return null;
        }
    }

    internal static string Describe(Exception ex, string uri) => ex switch
    {
        AuthenticationException =>
            $"NEO4J_PASSWORD in Config/ai-config.local.env does not open the graph at {uri}. " +
            "Neo4j keeps the password it was first started with in its data volume, so set the value that volume " +
            "was created with (./doctor.sh doctor checks this).",
        ServiceUnavailableException =>
            $"Neo4j is not reachable at {uri}. Start it with 'docker compose up -d neo4j', or check NEO4J_BOLT_PORT.",
        _ => $"Neo4j at {uri} could not be opened: {ex.Message}."
    };
}
