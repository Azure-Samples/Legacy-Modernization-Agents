using Neo4j.Driver;

namespace McpChatWeb.Services;

/// <summary>
/// Process-wide shared Neo4j driver for the Cobol-REKT graph.
///
/// The Neo4j .NET driver is expensive and is explicitly designed to be created
/// <b>once per application</b> and shared — each driver instance owns its own
/// TCP connection pool. The portal previously created (and disposed) a brand-new
/// driver on every REKT endpoint call; under the dashboard auto-refresh (which
/// hammers several endpoints at once) this spun up many short-lived pools, each
/// opening fresh connections to Neo4j, eventually exhausting the server-side
/// connection limit and producing:
///   "Failed to obtain a connection from pool within 00:01:00".
///
/// This holder lazily creates a single bounded driver and reuses it for the
/// lifetime of the process. Sessions remain cheap and are still opened/closed
/// per request (via <c>await using</c>); only the driver is shared.
/// </summary>
public static class RektNeo4j
{
    private static readonly object _gate = new();
    private static IDriver? _driver;

    /// <summary>Lazily-initialized, process-wide shared driver. Never dispose per request.</summary>
    public static IDriver Shared
    {
        get
        {
            if (_driver is not null) return _driver;
            lock (_gate)
            {
                if (_driver is not null) return _driver;

                var uri = Environment.GetEnvironmentVariable("REKT_NEO4J_URI") ?? "bolt://localhost:7688";
                var user = Environment.GetEnvironmentVariable("REKT_NEO4J_USER") ?? "neo4j";
                var password = Environment.GetEnvironmentVariable("REKT_NEO4J_PASSWORD") ?? "cobol-rekt-2026";

                _driver = GraphDatabase.Driver(
                    uri,
                    AuthTokens.Basic(user, password),
                    o => o
                        // Bound the pool so a runaway dashboard can't open unlimited connections.
                        .WithMaxConnectionPoolSize(50)
                        // Fail fast (15s) instead of the 60s default so callers surface a clean
                        // error/504 rather than hanging the whole request for a minute.
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
        IDriver? d;
        lock (_gate) { d = _driver; _driver = null; }
        if (d is not null) await d.DisposeAsync();
    }
}
