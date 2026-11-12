using System.Diagnostics;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// The portal launches `mcp` as a stdio child. When the portal is killed (e.g. OOM), stdin
// reaches EOF and the server must exit; otherwise every portal crash leaks a process.
public class McpServerLifecycleTests
{
    [Fact]
    public void McpServer_ExitsWhenStdinCloses()
    {
        var dll = Path.Join(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll");
        File.Exists(dll).Should().BeTrue($"the CLI assembly should sit beside the test assembly at {dll}");

        var workingDirectory = Path.Join(Path.GetTempPath(), "lma-mcp-eof-" + Guid.NewGuid().ToString("n"));
        Directory.CreateDirectory(workingDirectory);
        try
        {
            var psi = new ProcessStartInfo("dotnet")
            {
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                WorkingDirectory = workingDirectory,
            };
            psi.ArgumentList.Add(dll);
            psi.ArgumentList.Add("mcp");
            psi.ArgumentList.Add("--run-id");
            psi.ArgumentList.Add("1");
            psi.ArgumentList.Add("--config");
            psi.ArgumentList.Add(Path.Join(workingDirectory, "missing-config.json"));
            psi.Environment["MIGRATION_DB_PATH"] = Path.Join(workingDirectory, "migration.db");
            foreach (var key in new[] { "AZURE_OPENAI_API_KEY", "AZURE_OPENAI_SERVICE_TYPE", "GITHUB_TOKEN", "NEO4J_PASSWORD" })
            {
                psi.Environment.Remove(key);
            }
            // Startup validation needs these; no model call is made before stdin closes.
            psi.Environment["AZURE_OPENAI_ENDPOINT"] = "https://contoso-test.openai.azure.com";
            psi.Environment["AZURE_OPENAI_DEPLOYMENT_NAME"] = "gpt-4o";
            psi.Environment["AZURE_OPENAI_MODEL_ID"] = "gpt-4o";

            using var process = Process.Start(psi)!;
            var stdout = process.StandardOutput.ReadToEndAsync();
            var stderr = process.StandardError.ReadToEndAsync();
            process.StandardInput.Close();

            var exited = process.WaitForExit(milliseconds: 60_000);
            if (!exited)
            {
                process.Kill(entireProcessTree: true);
                process.WaitForExit(5_000);
            }

            exited.Should().BeTrue("an MCP server whose stdin is closed has no client left to serve");
            (stdout.Result + stderr.Result).Should().Contain("MCP input closed",
                "the server must reach its request loop and stop there, not fail during startup");
            process.ExitCode.Should().Be(0);
        }
        finally
        {
            try { Directory.Delete(workingDirectory, recursive: true); }
            catch (IOException) { }
            catch (UnauthorizedAccessException) { }
        }
    }
}
