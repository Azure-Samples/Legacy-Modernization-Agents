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

    // Endpoints check this first so an unconfigured environment returns a note, not a 500.
    public static bool IsConfigured => !string.IsNullOrEmpty(Password);

    public const string NotConfiguredNote =
        "REKT graph credentials are not configured. Set REKT_NEO4J_PASSWORD (or NEO4J_PASSWORD) " +
        "and run ./doctor.sh rekt-full to populate the graph.";

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
                if (string.IsNullOrEmpty(password))
                    throw new InvalidOperationException(NotConfiguredNote);

                _driver = GraphDatabase.Driver(
                    Uri,
                    AuthTokens.Basic(User, password),
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
