using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

public class ModernizationEndpointsTests : IClassFixture<Integration.WebAppFactory>
{
    private readonly Integration.WebAppFactory _factory;

    public ModernizationEndpointsTests(Integration.WebAppFactory factory) => _factory = factory;

    [Theory]
    [InlineData("/api/modernization/dependency-health")]
    [InlineData("/api/modernization/topology")]
    [InlineData("/api/modernization/service-chain")]
    [InlineData("/api/modernization/conversion-parity")]
    [InlineData("/api/modernization/program-catalog")]
    [InlineData("/api/modernization/flow/CUSTOMER.cbl")]
    [InlineData("/api/graph/rekt/runs")]
    [InlineData("/api/graph/rekt/architect")]
    [InlineData("/api/graph/rekt/services")]
    public async Task Endpoints_AreMappedAndReturnJson(string url)
    {
        var client = _factory.CreateClient();

        var response = await client.GetAsync(url);

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        Assert.Equal("application/json", response.Content.Headers.ContentType?.MediaType);
    }

    [Fact]
    public async Task DependencyHealth_ExposesFidelityBuckets()
    {
        var client = _factory.CreateClient();

        var payload = await client.GetFromJsonAsync<JsonElement>("/api/modernization/dependency-health");

        Assert.True(payload.TryGetProperty("totalPrograms", out _));
        Assert.True(payload.TryGetProperty("readinessScore", out _));
        Assert.True(payload.TryGetProperty("programs", out var programs));
        Assert.Equal(JsonValueKind.Array, programs.ValueKind);
    }

    [Fact]
    public async Task RektGraph_WithoutNeo4j_DegradesWithNote()
    {
        var client = _factory.CreateClient();

        var payload = await client.GetFromJsonAsync<JsonElement>("/api/graph/rekt/runs");

        Assert.Equal(JsonValueKind.Array, payload.GetProperty("runs").ValueKind);
        Assert.True(payload.TryGetProperty("note", out _));
    }

    [Fact]
    public async Task ConversionParity_ReportsWhichTargetsWereFound()
    {
        var client = _factory.CreateClient();

        var payload = await client.GetFromJsonAsync<JsonElement>("/api/modernization/conversion-parity");

        Assert.Equal(JsonValueKind.Array, payload.GetProperty("reports").ValueKind);
        // Absence must be explicit — an empty reports array alone reads as "everything passed".
        Assert.Equal(JsonValueKind.Array, payload.GetProperty("missingTargets").ValueKind);
        Assert.Equal(JsonValueKind.Array, payload.GetProperty("unreadableTargets").ValueKind);
    }

    [Fact]
    public async Task Flow_AcceptsSourceRelativeIdentityWithSlashes()
    {
        var client = _factory.CreateClient();

        // The route is a catch-all so nested identities survive routing.
        var response = await client.GetAsync("/api/modernization/flow/billing/CUSTOMER.cbl");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var payload = await response.Content.ReadFromJsonAsync<JsonElement>();
        Assert.Equal("billing/CUSTOMER.cbl", payload.GetProperty("identity").GetString());
    }

    [Fact]
    public async Task ProgramCatalog_ExposesSelectableProgramsAndClosureAvailability()
    {
        var client = _factory.CreateClient();

        var payload = await client.GetFromJsonAsync<JsonElement>("/api/modernization/program-catalog");

        Assert.Equal(JsonValueKind.Array, payload.GetProperty("programs").ValueKind);
        // Closure has to declare itself unusable rather than silently offering an empty result.
        Assert.True(payload.TryGetProperty("closureAvailable", out var available));
        Assert.Contains(available.ValueKind, new[] { JsonValueKind.True, JsonValueKind.False });
        Assert.True(payload.TryGetProperty("closureUnavailableReason", out _));
        Assert.Equal(JsonValueKind.Array, payload.GetProperty("deferredSelectors").ValueKind);
    }

    [Fact]
    public async Task ProgramCatalog_SearchNarrowsWithoutInventingEntries()
    {
        var client = _factory.CreateClient();

        var all = await client.GetFromJsonAsync<JsonElement>("/api/modernization/program-catalog");
        var filtered = await client.GetFromJsonAsync<JsonElement>(
            "/api/modernization/program-catalog?q=zzz-no-such-program");

        Assert.Equal(0, filtered.GetProperty("programs").GetArrayLength());
        // The unfiltered count must survive filtering so an empty result is visibly a filter, not a failure.
        Assert.True(all.GetProperty("totalPrograms").GetInt32() >= filtered.GetProperty("programs").GetArrayLength());
        Assert.Equal("zzz-no-such-program", filtered.GetProperty("query").GetString());
    }
}
