using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using GitHub.Copilot;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class CopilotRoutingTests
{
    [Theory]
    [InlineData("github.com", "github.com")]
    [InlineData(" HTTPS://Tenant.GHE.com/ ", "tenant.ghe.com")]
    [InlineData("tenant.ghe.com/", "tenant.ghe.com")]
    public void Normalize_AcceptsHostnameOrHttpsRoot(string input, string expected)
    {
        CopilotRouting.Normalize(input).Should().Be(expected);
    }

    [Theory]
    [InlineData("")]
    [InlineData("http://github.com")]
    [InlineData("https://user:pass@github.com")]
    [InlineData("https://github.com/path")]
    [InlineData("https://github.com?x=1")]
    [InlineData("https://github.com#fragment")]
    [InlineData("github.com:443")]
    [InlineData("tenant..ghe.com")]
    [InlineData("-tenant.ghe.com")]
    [InlineData("ftp://github.com")]
    public void Normalize_RejectsUnsafeOrMalformedInput(string input)
    {
        var act = () => CopilotRouting.Normalize(input);
        act.Should().Throw<ArgumentException>();
    }

    [Fact]
    public void Resolve_UsesDocumentedPrecedence()
    {
        var values = new Dictionary<string, string?>
        {
            ["COPILOT_GH_HOST"] = "copilot.ghe.com",
            ["GITHUB_HOST"] = "legacy.ghe.com",
            ["GH_HOST"] = "repo.example.com"
        };

        CopilotRouting.Resolve("explicit.ghe.com", values.GetValueOrDefault).Source.Should().Be("explicit");
        CopilotRouting.Resolve(null, values.GetValueOrDefault).Hostname.Should().Be("copilot.ghe.com");
        values["COPILOT_GH_HOST"] = null;
        CopilotRouting.Resolve(null, values.GetValueOrDefault).Hostname.Should().Be("legacy.ghe.com");
        values["GITHUB_HOST"] = null;
        CopilotRouting.Resolve(null, values.GetValueOrDefault).Hostname.Should().Be("repo.example.com");
    }

    [Fact]
    public void Resolve_DefaultsToGitHubDotCom()
    {
        CopilotRouting.Resolve(getEnvironmentVariable: _ => null).Hostname.Should().Be("github.com");
    }

    [Fact]
    public void ResolveToken_UsesCurrentCliPrecedenceBeforeLegacyValue()
    {
        var values = new Dictionary<string, string?>
        {
            ["COPILOT_GITHUB_TOKEN"] = "copilot-token",
            ["GH_TOKEN"] = "gh-token",
            ["GITHUB_TOKEN"] = "github-token",
            ["GITHUB_COPILOT_TOKEN"] = "legacy-token"
        };

        CopilotRouting.ResolveToken(values.GetValueOrDefault).Should().Be("copilot-token");
        values["COPILOT_GITHUB_TOKEN"] = null;
        CopilotRouting.ResolveToken(values.GetValueOrDefault).Should().Be("gh-token");
        values["GH_TOKEN"] = null;
        CopilotRouting.ResolveToken(values.GetValueOrDefault).Should().Be("github-token");
        values["GITHUB_TOKEN"] = null;
        CopilotRouting.ResolveToken(values.GetValueOrDefault).Should().Be("legacy-token");
    }

    [Fact]
    public void GetBundledCliPath_UsesCurrentPlatformRidLayout()
    {
        var path = CopilotRouting.GetBundledCliPath("/application");
        var expectedRid = OperatingSystem.IsWindows()
            ? System.Runtime.InteropServices.RuntimeInformation.OSArchitecture == System.Runtime.InteropServices.Architecture.Arm64 ? "win-arm64" : "win-x64"
            : OperatingSystem.IsMacOS()
                ? System.Runtime.InteropServices.RuntimeInformation.OSArchitecture == System.Runtime.InteropServices.Architecture.Arm64 ? "osx-arm64" : "osx-x64"
                : System.Runtime.InteropServices.RuntimeInformation.OSArchitecture == System.Runtime.InteropServices.Architecture.Arm64 ? "linux-arm64" : "linux-x64";

        path.Should().Be(Path.Combine(
            "/application",
            "runtimes",
            expectedRid,
            "native",
            OperatingSystem.IsWindows() ? "copilot.exe" : "copilot"));
    }

    [Fact]
    public void ApplyTo_PreservesInheritedEnvironmentAndAddsCanonicalHost()
    {
        var options = new CopilotClientOptions
        {
            Environment = new Dictionary<string, string> { ["CUSTOM_TEST_VALUE"] = "preserved" }
        };

        CopilotRouting.ApplyTo(options, "tenant.ghe.com");

        options.Environment.Should().ContainKey("CUSTOM_TEST_VALUE").WhoseValue.Should().Be("preserved");
        options.Environment.Should().ContainKey("COPILOT_GH_HOST").WhoseValue.Should().Be("tenant.ghe.com");
        if (!string.IsNullOrEmpty(Environment.GetEnvironmentVariable("PATH")))
            options.Environment.Should().ContainKey("PATH");
    }

    [Fact]
    public void ApplyTo_PreservesAnExplicitHostAlreadyStoredInOptions()
    {
        var options = new CopilotClientOptions
        {
            Environment = new Dictionary<string, string>
            {
                ["COPILOT_GH_HOST"] = "tenant.ghe.com"
            }
        };

        var route = CopilotRouting.ApplyTo(options);

        route.Hostname.Should().Be("tenant.ghe.com");
        options.Environment["COPILOT_GH_HOST"].Should().Be("tenant.ghe.com");
    }

    [Fact]
    public async Task RunBoundedAsync_CancelsHungDiscovery()
    {
        var act = async () => await CopilotModelDiagnostics.RunBoundedAsync(
            async cancellationToken => 
            {
                await Task.Delay(Timeout.InfiniteTimeSpan, cancellationToken);
                return Array.Empty<string>();
            },
            TimeSpan.FromMilliseconds(25));

        await act.Should().ThrowAsync<OperationCanceledException>();
    }

    [Theory]
    [InlineData("GitHubCopilot")]
    [InlineData("GitHubCopilotSDK")]
    [InlineData("CopilotSDK")]
    public void ProviderAliases_AreCanonicalized(string value)
    {
        CopilotProvider.IsSdk(value).Should().BeTrue();
        CopilotProvider.Canonicalize(value).Should().Be(CopilotProvider.CanonicalServiceType);
    }
}
