using Neo4j.Driver;

namespace McpChatWeb.Services;

/// <summary>
/// Process-wide shared Neo4j driver for the Cobol-REKT graph
/// (the <c>cobol-rekt-neo4j</c> container that <c>doctor.sh</c> starts on
/// bolt port 7688 — distinct from the migration graph on 7687).
///
/// <para>
/// The Neo4j .NET driver owns a TCP connection pool and is designed to be
/// created once per application and shared. Creating a driver per request
/// opens a fresh pool each time and exhausts the server-side connection limit
/// under dashboard auto-refresh, surfacing as
/// "Failed to obtain a connection from pool within 00:01:00". Sessions stay
/// cheap and are still opened and disposed per request; only the driver is
/// shared.
/// </para>
///
/// <para>
/// Credentials come from the environment. <c>doctor.sh</c> writes
/// <c>NEO4J_PASSWORD</c> into <c>Config/ai-config.local.env</c>; set
/// <c>REKT_NEO4J_PASSWORD</c> to point the portal at a different graph. No
/// password is compiled into the assembly — when none is configured,
/// <see cref="IsConfigured"/> is false and callers degrade to an empty result
/// instead of failing the request.
/// </para>
/// </summary>
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

    /// <summary>
    /// True when a password is available. Endpoints check this first so an
    /// unconfigured environment returns an explanatory note rather than a 500.
    /// </summary>
    public static bool IsConfigured => !string.IsNullOrEmpty(Password);

    public const string NotConfiguredNote =
        "REKT graph credentials are not configured. Set REKT_NEO4J_PASSWORD (or NEO4J_PASSWORD) " +
        "and run ./doctor.sh rekt-full to populate the graph.";

    /// <summary>
    /// Lazily-initialised, process-wide shared driver. Never dispose per request.
    /// </summary>
    /// <exception cref="InvalidOperationException">No password is configured.</exception>
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
                        // Bound the pool so a runaway dashboard cannot open unlimited connections.
                        .WithMaxConnectionPoolSize(50)
                        // Fail fast instead of the 60s default so callers surface a clean
                        // error rather than hanging the request for a minute.
                        .WithConnectionAcquisitionTimeout(TimeSpan.FromSeconds(15))
                        .WithMaxConnectionLifetime(TimeSpan.FromMinutes(30))
                        .WithConnectionTimeout(TimeSpan.FromSeconds(15)));

                return _driver;
            }
        }
    }

    /// <summary>Close the shared driver on application shutdown.</summary>
    public static async ValueTask DisposeAsync()
    {
        IDriver? driver;
        lock (_gate) { driver = _driver; _driver = null; }
        if (driver is not null) await driver.DisposeAsync();
    }
}
