using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

/// <summary>
/// Verifies the endpoints are mapped, resolve their dependencies, and degrade
/// to an explanatory payload on a repository with no REKT artifacts. That empty
/// state is the normal state before <c>./doctor.sh rekt-full</c> has run, so it
/// has to render rather than error.
/// </summary>
public class ModernizationEndpointsTests : IClassFixture<Integration.WebAppFactory>
{
    private readonly Integration.WebAppFactory _factory;

    public ModernizationEndpointsTests(Integration.WebAppFactory factory) => _factory = factory;

    [Theory]
    [InlineData("/api/modernization/dependency-health")]
    [InlineData("/api/modernization/topology")]
    [InlineData("/api/modernization/service-chain")]
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
    public async Task Flow_AcceptsSourceRelativeIdentityWithSlashes()
    {
        var client = _factory.CreateClient();

        // The route is a catch-all so nested identities survive routing.
        var response = await client.GetAsync("/api/modernization/flow/billing/CUSTOMER.cbl");

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var payload = await response.Content.ReadFromJsonAsync<JsonElement>();
        Assert.Equal("billing/CUSTOMER.cbl", payload.GetProperty("identity").GetString());
    }
}
