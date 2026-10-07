using System;
using System.Collections.Generic;
using System.IO;
using System.Net;
using System.Net.Http.Json;
using System.Text.Json.Nodes;
using System.Threading;
using System.Threading.Tasks;
using McpChatWeb;
using McpChatWeb.Configuration;
using McpChatWeb.Models;
using McpChatWeb.Services;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using Microsoft.Extensions.Options;
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

    [Fact]
    public async Task PromptScoreEndpoint_ValidPromptId_ReturnsSuccess()
    {
        var client = _factory.CreateClient();

        var response = await client.PostAsync("/api/prompts/score/CobolAnalyzer", null);

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
    }

    [Theory]
    [InlineData("../secret")]
    [InlineData("../../secret")]
    [InlineData("..\\secret")]
    [InlineData("/etc/passwd")]
    [InlineData("foo/bar")]
    public async Task PromptScoreEndpoint_PathTraversalPromptIds_ReturnBadRequest(string promptId)
    {
        var client = _factory.CreateClient();
        var encodedPromptId = Uri.EscapeDataString(promptId);

        var response = await client.PostAsync($"/api/prompts/score/{encodedPromptId}", null);

        Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);
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

        public Task<JsonObject> CallToolAsync(string toolName, Dictionary<string, object> arguments, CancellationToken cancellationToken = default)
        {
            var result = new JsonObject
            {
                ["result"] = $"Tool {toolName} executed."
            };
            return Task.FromResult(result);
        }

        public Task RestartAsync(CancellationToken cancellationToken = default)
            => Task.CompletedTask;

        public ValueTask DisposeAsync() => ValueTask.CompletedTask;
    }
}

public class McpServerUnavailableTests
{
    [Fact]
    public async Task ResourcesEndpoint_WhenServerHasNoRuns_Returns503WithReason()
    {
        if (OperatingSystem.IsWindows())
        {
            return;
        }

        var dir = Directory.CreateTempSubdirectory("mcp-noruns-");
        try
        {
            // Stand-in for the MCP server: say why, then exit, as Program.cs does on an empty database.
            var script = Path.Join(dir.FullName, "fake-mcp.sh");
            await File.WriteAllTextAsync(script,
                "echo 'No migration runs available in the database. Run the migration process first.' >&2\nexit 0\n");

            var options = Options.Create(new McpOptions
            {
                DotnetExecutable = "/bin/sh",
                AssemblyPath = script,
                ConfigPath = Path.Join(dir.FullName, "unused.json"),
                WorkingDirectory = dir.FullName
            });

            using var factory = new WebAppFactory().WithWebHostBuilder(builder =>
                builder.ConfigureServices(services =>
                {
                    services.RemoveAll<IMcpClient>();
                    services.AddSingleton<IMcpClient>(_ => new McpProcessClient(options));
                }));

            var response = await factory.CreateClient().GetAsync("/api/resources");

            Assert.Equal(HttpStatusCode.ServiceUnavailable, response.StatusCode);
            var body = await response.Content.ReadFromJsonAsync<JsonObject>();
            Assert.NotNull(body);
            Assert.True(body!["noMigrationRuns"]!.GetValue<bool>());
            Assert.Contains("No migration runs", body["reason"]!.GetValue<string>());
            Assert.Contains("reverse-eng", body["hint"]!.GetValue<string>());
        }
        finally
        {
            dir.Delete(recursive: true);
        }
    }

    [Fact]
    public void Exception_WithoutStderr_StillExplainsExit()
    {
        var ex = new McpServerUnavailableException(3, Array.Empty<string>());

        Assert.False(ex.NoMigrationRuns);
        Assert.Null(ex.Reason);
        Assert.Contains("exit code 3", ex.Message);
    }
}
