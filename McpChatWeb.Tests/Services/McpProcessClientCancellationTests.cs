using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using McpChatWeb.Configuration;
using McpChatWeb.Services;
using Microsoft.Extensions.Options;
using Xunit;

namespace McpChatWeb.Tests.Services;

public sealed class McpProcessClientCancellationTests : IDisposable
{
    private readonly string _root = Directory.CreateTempSubdirectory("mcp-cancellation-").FullName;

    [UnixTheory]
    [InlineData("initialize")]
    [InlineData("initialize-header")]
    [InlineData("resources-list")]
    public async Task CancelledPartialFrameTerminatesOwnedProcessAndNextRequestRecovers(string phase)
    {
        var script = Path.Combine(_root, "fake_mcp.py");
        File.WriteAllText(script, """
            import json, pathlib, sys, time
            root = pathlib.Path(__file__).parent
            phase = pathlib.Path(sys.argv[sys.argv.index("--config") + 1]).name
            first = not (root / "started").exists()
            (root / "started").touch()
            while True:
                headers = {}
                while True:
                    line = sys.stdin.buffer.readline()
                    if not line:
                        sys.exit(0)
                    if line in (b"\n", b"\r\n"):
                        break
                    key, value = line.decode().split(":", 1)
                    headers[key.lower()] = value.strip()
                request = json.loads(sys.stdin.buffer.read(int(headers["content-length"])))
                method = request["method"]
                result = {"resources": [{"uri": "test://recovered", "name": "Recovered"}]} if method == "resources/list" else {}
                body = json.dumps({"jsonrpc": "2.0", "id": request["id"], "result": result}).encode()
                if first and phase == "initialize-header" and method == "initialize":
                    sys.stdout.buffer.write(b"Content-Leng")
                    sys.stdout.buffer.flush()
                    (root / "partial").touch()
                    time.sleep(60)
                sys.stdout.buffer.write(("Content-Length: %d\r\n\r\n" % len(body)).encode())
                if first and method.replace("/", "-") == phase:
                    sys.stdout.buffer.write(body[:5])
                    sys.stdout.buffer.flush()
                    (root / "partial").touch()
                    time.sleep(60)
                else:
                    sys.stdout.buffer.write(body)
                    sys.stdout.buffer.flush()
            """);
        using var client = new McpProcessClient(Options.Create(new McpOptions
        {
            DotnetExecutable = "python3",
            AssemblyPath = script,
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

    private sealed class UnixTheoryAttribute : TheoryAttribute
    {
        public UnixTheoryAttribute()
        {
            if (OperatingSystem.IsWindows()) Skip = "Uses the repository's Unix python3 process fixture.";
        }
    }
}
