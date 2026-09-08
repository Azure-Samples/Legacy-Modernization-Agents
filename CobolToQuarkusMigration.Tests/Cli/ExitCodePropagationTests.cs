using System.Diagnostics;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// Only a real process can establish an exit code: an int-returning Main overrides
// Environment.ExitCode, so every command that reports failure that way is silently a success
// in-process. The parity gate and the existing Cli/ commands both depend on this.
public class ExitCodePropagationTests
{
    [Fact]
    public void FailingCommand_PropagatesEnvironmentExitCodeToTheProcess()
    {
        var result = RunCli("program-facts", "extract", "/nonexistent-staging-directory");

        result.ExitCode.Should().Be(2, "Cli/ProgramFactsCommand reports a missing input with Environment.ExitCode = 2");
    }

    [Fact]
    public void SucceedingCommand_ExitsZero()
    {
        var result = RunCli("--help");

        result.ExitCode.Should().Be(0);
    }

    // The AI config gate runs before command dispatch, so it can block commands that never
    // make a model call. Config/ai-config.env used to hide this by supplying placeholders.
    [Theory]
    [InlineData("--help")]
    [InlineData("program-facts,extract,/nonexistent-staging-directory")]
    [InlineData("rekt-scan-cache,plan,/nonexistent-staging-directory")]
    public void DeterministicCommands_RunWithoutAiCredentials(string commaSeparatedArgs)
    {
        var result = RunCli(commaSeparatedArgs.Split(','), withoutAiCredentials: true);

        result.Output.Should().NotContain(
            "Configuration Validation Failed",
            "file-only commands must not require AZURE_OPENAI_* settings");
    }

    [Fact]
    public void AiCommands_StillRequireCredentials()
    {
        var result = RunCli(new[] { "--source", "source" }, withoutAiCredentials: true);

        result.Output.Should().Contain("Configuration Validation Failed");
        result.ExitCode.Should().Be(1);
    }

    // The guard checked for "your-resource" while the template shipped "your-endpoint", so an
    // unedited copy validated successfully and failed later as a DNS error. Pin both literals.
    [Theory]
    [InlineData("https://your-endpoint.cognitiveservices.azure.com")]
    [InlineData("https://your-resource.openai.azure.com")]
    public void UneditedTemplateEndpoint_IsRejected(string endpoint)
    {
        var result = RunCli(
            new[] { "--source", "source" },
            withoutAiCredentials: true,
            env: new Dictionary<string, string>
            {
                ["AZURE_OPENAI_ENDPOINT"] = endpoint,
                ["AZURE_OPENAI_DEPLOYMENT_NAME"] = "gpt-4o",
                ["AZURE_OPENAI_MODEL_ID"] = "gpt-4o",
            });

        result.Output.Should().Contain("template placeholder");
        result.ExitCode.Should().Be(1);
    }

    [Fact]
    public void RealEndpoint_IsNotMistakenForATemplate()
    {
        var result = RunCli(
            new[] { "--help" },
            withoutAiCredentials: true,
            env: new Dictionary<string, string>
            {
                ["AZURE_OPENAI_ENDPOINT"] = "https://contoso-prod.openai.azure.com",
                ["AZURE_OPENAI_DEPLOYMENT_NAME"] = "gpt-4o",
                ["AZURE_OPENAI_MODEL_ID"] = "gpt-4o",
            });

        result.Output.Should().NotContain("template placeholder");
    }

    private static (int ExitCode, string Output) RunCli(params string[] args)
        => RunCli(args, withoutAiCredentials: false);

    private static (int ExitCode, string Output) RunCli(
        string[] args,
        bool withoutAiCredentials,
        IDictionary<string, string>? env = null)
    {
        var dll = Path.Combine(AppContext.BaseDirectory, "CobolToQuarkusMigration.dll");
        File.Exists(dll).Should().BeTrue($"the CLI assembly should sit beside the test assembly at {dll}");

        var psi = new ProcessStartInfo("dotnet")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = AppContext.BaseDirectory,
        };
        psi.ArgumentList.Add(dll);
        foreach (var a in args) psi.ArgumentList.Add(a);

        if (withoutAiCredentials)
        {
            // Inherited shell exports would otherwise satisfy the gate and hide a regression.
            foreach (var key in new[]
                     {
                         "AZURE_OPENAI_ENDPOINT", "AZURE_OPENAI_API_KEY", "AZURE_OPENAI_MODEL_ID",
                         "AZURE_OPENAI_DEPLOYMENT_NAME", "AZURE_OPENAI_SERVICE_TYPE", "GITHUB_TOKEN",
                     })
            {
                psi.Environment.Remove(key);
            }
        }

        if (env is not null)
        {
            foreach (var (key, value) in env) psi.Environment[key] = value;
        }

        using var process = Process.Start(psi)!;
        var stdout = process.StandardOutput.ReadToEnd();
        var stderr = process.StandardError.ReadToEnd();
        process.WaitForExit(milliseconds: 120_000).Should().BeTrue("the CLI should not hang");

        return (process.ExitCode, stdout + stderr);
    }
}
