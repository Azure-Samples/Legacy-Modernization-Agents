using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json.Nodes;
using System.Threading;

namespace McpChatWeb.Tests.Fixtures;

// The test assembly doubles as a fake MCP server, so process tests need no interpreter and run on every OS.
public static class FakeMcpServer
{
    public static int Main(string[] args)
    {
        var config = Array.IndexOf(args, "--config");
        return args.Length > 0 && args[0] == "mcp" && config > 0 && config + 1 < args.Length
            ? Serve(args[config + 1])
            : 0;
    }

    private static int Serve(string configPath)
    {
        var root = Path.GetDirectoryName(Path.GetFullPath(configPath))!;
        var phase = Path.GetFileName(configPath);
        var first = !File.Exists(Path.Combine(root, "started"));
        File.WriteAllBytes(Path.Combine(root, "started"), []);
        using var stdin = Console.OpenStandardInput();
        using var stdout = Console.OpenStandardOutput();
        while (true)
        {
            var headers = new Dictionary<string, string>(StringComparer.Ordinal);
            while (true)
            {
                var line = ReadLine(stdin);
                if (line is null) return 0;
                if (line is "\n" or "\r\n") break;
                var colon = line.IndexOf(':');
                headers[line[..colon].ToLowerInvariant()] = line[(colon + 1)..].Trim();
            }
            var payload = new byte[int.Parse(headers["content-length"])];
            stdin.ReadExactly(payload);
            var request = JsonNode.Parse(payload)!;
            var method = request["method"]!.GetValue<string>();
            JsonNode result = method == "resources/list"
                ? JsonNode.Parse("""{"resources": [{"uri": "test://recovered", "name": "Recovered"}]}""")!
                : new JsonObject();
            var body = Encoding.UTF8.GetBytes(new JsonObject
            {
                ["jsonrpc"] = "2.0",
                ["id"] = request["id"]!.DeepClone(),
                ["result"] = result,
            }.ToJsonString());
            if (first && phase == "initialize-header" && method == "initialize")
                Stall(stdout, root, Encoding.ASCII.GetBytes("Content-Leng"));
            stdout.Write(Encoding.ASCII.GetBytes($"Content-Length: {body.Length}\r\n\r\n"));
            if (first && method.Replace('/', '-') == phase)
                Stall(stdout, root, body[..5]);
            stdout.Write(body);
            stdout.Flush();
        }
    }

    // Leaves a partial frame on the pipe and hangs until the client kills the process.
    private static void Stall(Stream stdout, string root, byte[] partial)
    {
        stdout.Write(partial);
        stdout.Flush();
        File.WriteAllBytes(Path.Combine(root, "partial"), []);
        Thread.Sleep(TimeSpan.FromSeconds(60));
    }

    private static string? ReadLine(Stream stdin)
    {
        var bytes = new List<byte>();
        int next;
        while ((next = stdin.ReadByte()) >= 0)
        {
            bytes.Add((byte)next);
            if (next == '\n') break;
        }
        return bytes.Count == 0 ? null : Encoding.UTF8.GetString(bytes.ToArray());
    }
}
