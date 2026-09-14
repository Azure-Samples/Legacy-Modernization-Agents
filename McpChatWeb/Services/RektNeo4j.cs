using Neo4j.Driver;

namespace McpChatWeb.Services;

// Cobol-REKT graph on bolt 7688, distinct from the migration graph on 7687. The driver
// owns a pool and must outlive a request; one per request exhausts the connection limit.
public static class RektNeo4j
{
    public const string DefaultUri = "bolt://localhost:7688";
    public const string DefaultUser = "neo4j";

    private static readonly object _gate = new();
    private static IDriver? _driver;

    public static string Uri =>
        Environment.GetEnvironmentVariable("REKT_NEO4J_URI") ?? DefaultUri;

    private static string User =>
        Environment.GetEnvironmentVariable("REKT_NEO4J_USER") ?? DefaultUser;

    private static string? Password =>
        Environment.GetEnvironmentVariable("REKT_NEO4J_PASSWORD")
        ?? Environment.GetEnvironmentVariable("NEO4J_PASSWORD");

    // A password is not required: a local graph may run with auth disabled, which is a normal
    // development setup. Connecting unauthenticated is attempted rather than reported as
    // "not configured", so the banner never blames credentials for a reachable graph.
    public static bool IsConfigured => true;

    public const string NotConfiguredNote =
        "REKT graph is not reachable. Start it with ./doctor.sh rekt-full, and set " +
        "REKT_NEO4J_PASSWORD (or NEO4J_PASSWORD) if the instance requires authentication.";

    // Process-wide shared driver. Never dispose per request.
    public static IDriver Shared
    {
        get
        {
            if (_driver is not null) return _driver;
            lock (_gate)
            {
                if (_driver is not null) return _driver;

                var password = Password;
                var auth = string.IsNullOrEmpty(password)
                    ? AuthTokens.None
                    : AuthTokens.Basic(User, password);

                _driver = GraphDatabase.Driver(
                    Uri,
                    auth,
                    o => o
                        .WithMaxConnectionPoolSize(50)
                        // Fail fast: the default is 60s, long enough to look like a hang.
                        .WithConnectionAcquisitionTimeout(TimeSpan.FromSeconds(15))
                        .WithMaxConnectionLifetime(TimeSpan.FromMinutes(30))
                        .WithConnectionTimeout(TimeSpan.FromSeconds(15)));

                return _driver;
            }
        }
    }

    public static async ValueTask DisposeAsync()
    {
        IDriver? driver;
        lock (_gate) { driver = _driver; _driver = null; }
        if (driver is not null) await driver.DisposeAsync();
    }
}
