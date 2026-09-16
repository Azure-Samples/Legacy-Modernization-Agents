using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

// Focused conversion (preview). These cover the request-validation path only: the shared test
// host runs against the real checkout, so a test must not reach staging or launch a converter.
// Selector resolution and staging are covered in isolation by ConversionScopeServiceTests.
public class FocusedConvertEndpointTests : IClassFixture<Integration.WebAppFactory>
{
    private readonly Integration.WebAppFactory _factory;

    public FocusedConvertEndpointTests(Integration.WebAppFactory factory) => _factory = factory;

    [Fact]
    public async Task EmptySelectionIsRejected()
    {
        var client = _factory.CreateClient();

        var response = await client.PostAsJsonAsync(
            "/api/runs/convert",
            new { programs = System.Array.Empty<string>() });

        Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);
    }

    [Fact]
    public async Task MissingSelectionIsRejected()
    {
        var client = _factory.CreateClient();

        var response = await client.PostAsJsonAsync("/api/runs/convert", new { });

        Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);
    }

    // A rejected request that still launched a converter would convert the whole estate, which is
    // the exact failure this feature exists to prevent.
    [Fact]
    public async Task RejectedSelectionStartsNoRun()
    {
        var client = _factory.CreateClient();
        var before = await CountRuns(client);

        await client.PostAsJsonAsync("/api/runs/convert", new { programs = System.Array.Empty<string>() });

        Assert.Equal(before, await CountRuns(client));
    }

    private static async Task<int> CountRuns(System.Net.Http.HttpClient client)
    {
        // ProcessManager's own list. /api/runs/all reads the migration database, which a
        // launched-but-not-yet-recorded run would not appear in.
        var payload = await client.GetFromJsonAsync<JsonElement>("/api/runs/managed");
        return payload.GetArrayLength();
    }
}
