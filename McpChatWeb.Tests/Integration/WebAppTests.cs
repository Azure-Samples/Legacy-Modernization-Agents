using System.Collections.Generic;
using System.Net.Http.Json;
using System.Threading;
using System.Threading.Tasks;
using McpChatWeb;
using McpChatWeb.Models;
using McpChatWeb.Services;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using Xunit;

namespace McpChatWeb.Tests.Integration;

public class WebAppTests : IClassFixture<WebAppFactory>
{
    private readonly WebAppFactory _factory;

    public WebAppTests(WebAppFactory factory)
    {
        _factory = factory;
    }

    [Fact]
    public async Task ResourcesEndpoint_ReturnsFakeResources()
    {
        var client = _factory.CreateClient();

        var response = await client.GetAsync("/api/resources");
        response.EnsureSuccessStatusCode();

        var payload = await response.Content.ReadFromJsonAsync<ResourceDto[]>();

        Assert.NotNull(payload);
        Assert.Single(payload!);
        Assert.Equal("demo-resource", payload![0].Name);
        Assert.Equal("urn:demo", payload![0].Uri);
    }

    [Fact]
    public async Task ChatEndpoint_ReturnsFakeResponse()
    {
        var client = _factory.CreateClient();

        var response = await client.PostAsJsonAsync("/api/chat", new ChatRequest("hello"));
        response.EnsureSuccessStatusCode();

        var payload = await response.Content.ReadFromJsonAsync<ChatResponse>();
        Assert.NotNull(payload);
        Assert.Equal("Echo: hello", payload!.Response);
    }
}

public sealed class WebAppFactory : WebApplicationFactory<Program>
{
    protected override void ConfigureWebHost(Microsoft.AspNetCore.Hosting.IWebHostBuilder builder)
    {
        builder.ConfigureServices(services =>
        {
            services.RemoveAll<IMcpClient>();
            services.AddSingleton<IMcpClient, FakeMcpClient>();
        });
    }

    private sealed class FakeMcpClient : IMcpClient
    {
        private static readonly IReadOnlyList<ResourceDto> Resources =
            new List<ResourceDto>
            {
                new("urn:demo", "demo-resource", "Sample resource", "application/json")
            };

        public Task EnsureReadyAsync(CancellationToken cancellationToken = default)
            => Task.CompletedTask;

        public Task<IReadOnlyList<ResourceDto>> ListResourcesAsync(CancellationToken cancellationToken = default)
            => Task.FromResult<IReadOnlyList<ResourceDto>>(Resources);

        public Task<string> ReadResourceAsync(string uri, CancellationToken cancellationToken = default)
            => Task.FromResult($"Resource content for: {uri}");

        public Task<string> SendChatAsync(string prompt, CancellationToken cancellationToken = default)
            => Task.FromResult($"Echo: {prompt}");

        public ValueTask DisposeAsync() => ValueTask.CompletedTask;
    }
}
