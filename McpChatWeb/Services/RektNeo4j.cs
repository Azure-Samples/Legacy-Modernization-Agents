using Neo4j.Driver;

namespace McpChatWeb.Services;

// The single Neo4j instance, which holds the REKT graph next to the migration graph. It is
// reached on the same settings as the migration repository. The driver owns a pool and must
// outlive a request; one per request exhausts the connection limit.
public static class RektNeo4j
{
    public const string DefaultUser = "neo4j";

    public static string DefaultUri =>
        $"bolt://localhost:{FirstSet("NEO4J_BOLT_PORT") ?? "7687"}";

    private static readonly object _gate = new();
    private static IDriver? _driver;

    public static string Uri =>
        FirstSet("ApplicationSettings__Neo4j__Uri", "NEO4J_URI") ?? DefaultUri;

    private static string User =>
        FirstSet("ApplicationSettings__Neo4j__Username", "NEO4J_USER") ?? DefaultUser;

    private static string? Password =>
        FirstSet("ApplicationSettings__Neo4j__Password", "NEO4J_PASSWORD");

    private static string? FirstSet(params string[] names) =>
        names.Select(Environment.GetEnvironmentVariable).FirstOrDefault(v => !string.IsNullOrEmpty(v));

    // A password is not required: a local graph may run with auth disabled, which is a normal
    // development setup. Connecting unauthenticated is attempted rather than reported as
    // "not configured", so the banner never blames credentials for a reachable graph.
    public static bool IsConfigured => true;

    public const string NotConfiguredNote =
        "REKT graph is not reachable. Start it with ./doctor.sh rekt-full, and set " +
        "NEO4J_PASSWORD if the instance requires authentication.";

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
