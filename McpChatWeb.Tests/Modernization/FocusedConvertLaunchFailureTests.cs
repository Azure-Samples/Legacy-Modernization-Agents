using System;
using System.IO;
using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;
using McpChatWeb.Services;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.DependencyInjection.Extensions;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

// StartRun records a launch failure on the run instead of throwing, so the endpoint has to inspect
// the returned status. Reporting success here would tell an operator a conversion is under way
// that never started, and the modal would show a run that produces nothing.
public sealed class FocusedConvertLaunchFailureTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(),
        $"portal-launch-{Guid.NewGuid():N}");

    private readonly FailingLaunchFactory _factory;

    public FocusedConvertLaunchFailureTests()
    {
        var program = Path.Combine(_root, "estate", "source", "finance", "ACCOUNTS.cbl");
        Directory.CreateDirectory(Path.GetDirectoryName(program)!);
        File.WriteAllText(program, "       IDENTIFICATION DIVISION.\n");

        // Staging resolves against a real estate; the converter is launched from a directory that
        // does not exist, which is the failure StartRun swallows.
        _factory = new FailingLaunchFactory(
            estateRoot: Path.Combine(_root, "estate"),
            launchRoot: Path.Combine(_root, "no-such-checkout"));
    }

    [Fact]
    public async Task FailedLaunchIsNotReportedAsAStartedRun()
    {
        var client = _factory.CreateClient();

        var response = await client.PostAsJsonAsync(
            "/api/runs/convert",
            new { programs = new[] { "finance/ACCOUNTS.cbl" } });

        Assert.NotEqual(HttpStatusCode.OK, response.StatusCode);
    }

    [Fact]
    public async Task FailedLaunchReportsTheRunAsFailed()
    {
        var client = _factory.CreateClient();

        var response = await client.PostAsJsonAsync(
            "/api/runs/convert",
            new { programs = new[] { "finance/ACCOUNTS.cbl" } });

        var payload = await response.Content.ReadFromJsonAsync<JsonElement>();
        Assert.Equal("failed", payload.GetProperty("run").GetProperty("status").GetString());
    }

    public void Dispose()
    {
        _factory.Dispose();
        if (Directory.Exists(_root))
            Directory.Delete(_root, recursive: true);
    }

    private sealed class FailingLaunchFactory : WebApplicationFactory<Program>
    {
        private readonly string _estateRoot;
        private readonly string _launchRoot;

        public FailingLaunchFactory(string estateRoot, string launchRoot)
        {
            _estateRoot = estateRoot;
            _launchRoot = launchRoot;
        }

        protected override void ConfigureWebHost(Microsoft.AspNetCore.Hosting.IWebHostBuilder builder)
        {
            builder.ConfigureServices(services =>
            {
                services.RemoveAll<ConversionScopeService>();
                services.AddSingleton(new ConversionScopeService(_estateRoot));
                services.RemoveAll<ProcessManager>();
                services.AddSingleton(new ProcessManager(_launchRoot));
            });
        }
    }
}
