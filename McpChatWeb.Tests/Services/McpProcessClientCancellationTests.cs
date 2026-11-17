using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using McpChatWeb.Configuration;
using McpChatWeb.Services;
using McpChatWeb.Tests.Fixtures;
using Microsoft.Extensions.Options;
using Xunit;

namespace McpChatWeb.Tests.Services;

public sealed class McpProcessClientCancellationTests : IDisposable
{
    private readonly string _root = Directory.CreateTempSubdirectory("mcp-cancellation-").FullName;

    [Theory]
    [InlineData("initialize")]
    [InlineData("initialize-header")]
    [InlineData("resources-list")]
    public async Task CancelledPartialFrameTerminatesOwnedProcessAndNextRequestRecovers(string phase)
    {
        using var client = new McpProcessClient(Options.Create(new McpOptions
        {
            DotnetExecutable = DotnetHost,
            AssemblyPath = typeof(FakeMcpServer).Assembly.Location,
            ConfigPath = Path.Combine(_root, phase),
            WorkingDirectory = _root
        }));
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(15));
        var pending = client.ListResourcesAsync(cancellation.Token);
        var deadline = DateTime.UtcNow.AddSeconds(10);
        while (!File.Exists(Path.Combine(_root, "partial")))
        {
            Assert.False(pending.IsCompleted, "The fake MCP process ended before the partial response.");
            Assert.True(DateTime.UtcNow < deadline, "The fake MCP process did not produce a partial frame.");
            await Task.Delay(20);
        }
        cancellation.Cancel();
        await Assert.ThrowsAnyAsync<OperationCanceledException>(async () =>
            await pending.WaitAsync(TimeSpan.FromSeconds(5)));
        var recovered = await client.ListResourcesAsync().WaitAsync(TimeSpan.FromSeconds(5));
        Assert.Equal("test://recovered", Assert.Single(recovered).Uri);
    }

    public void Dispose() => Directory.Delete(_root, recursive: true);

    private static string DotnetHost =>
        Environment.GetEnvironmentVariable("DOTNET_HOST_PATH") is { Length: > 0 } host && File.Exists(host) ? host : "dotnet";
}
