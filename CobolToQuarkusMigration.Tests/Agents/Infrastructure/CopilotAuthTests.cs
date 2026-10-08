using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Agents.Infrastructure;
using GitHub.Copilot;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure;

public class CopilotAuthTests
{
    private const string FineGrained = "github_pat_example";
    private const string Classic = "ghp_example";

    private static Func<string, string?> Env(params (string Name, string Value)[] values)
    {
        var map = values.ToDictionary(v => v.Name, v => v.Value);
        return name => map.TryGetValue(name, out var v) ? v : null;
    }

    [Fact]
    public void Unset_NoTokens_UsesCopilotLogin()
    {
        var auth = CopilotAuth.Resolve(Env());

        auth.Mode.Should().Be(CopilotAuth.Mode.Auto);
        auth.Token.Should().BeNull();
        auth.HiddenVariables.Should().BeEmpty();
        auth.Description.Should().Be("copilot login");
    }

    [Fact]
    public void Unset_CopilotToken_IsPassedExplicitly()
    {
        var auth = CopilotAuth.Resolve(Env(("COPILOT_GITHUB_TOKEN", FineGrained), ("GH_TOKEN", "gho_other")));

        auth.Mode.Should().Be(CopilotAuth.Mode.Token);
        auth.Token.Should().Be(FineGrained);
        auth.TokenSource.Should().Be("COPILOT_GITHUB_TOKEN");
    }

    [Fact]
    public void LegacyVariable_IsStillRead()
    {
        var auth = CopilotAuth.Resolve(Env(("GITHUB_COPILOT_TOKEN", FineGrained)));

        auth.Token.Should().Be(FineGrained);
        auth.TokenSource.Should().Be("GITHUB_COPILOT_TOKEN");
    }

    [Fact]
    public void NewVariable_WinsOverLegacy()
    {
        var auth = CopilotAuth.Resolve(Env(("GITHUB_COPILOT_TOKEN", "github_pat_old"), ("COPILOT_GITHUB_TOKEN", FineGrained)));

        auth.Token.Should().Be(FineGrained);
    }

    [Fact]
    public void ClassicToken_IsRejectedWithGuidance()
    {
        var act = () => CopilotAuth.Resolve(Env(("COPILOT_GITHUB_TOKEN", Classic)));

        act.Should().Throw<InvalidOperationException>()
            .WithMessage("*COPILOT_GITHUB_TOKEN*ghp_*fine-grained*Copilot Requests*");
    }

    [Fact]
    public void LoginMode_HidesEveryTokenVariable_AndIgnoresConfiguredToken()
    {
        var auth = CopilotAuth.Resolve(Env(
            ("COPILOT_AUTH", "login"),
            ("COPILOT_GITHUB_TOKEN", FineGrained),
            ("GH_TOKEN", "gho_x"),
            ("GITHUB_TOKEN", Classic)));

        auth.Mode.Should().Be(CopilotAuth.Mode.Login);
        auth.Token.Should().BeNull();
        auth.HiddenVariables.Should().BeEquivalentTo("COPILOT_GITHUB_TOKEN", "GH_TOKEN", "GITHUB_TOKEN");
        auth.Description.Should().StartWith("copilot login");
    }

    [Fact]
    public void TokenMode_WithoutToken_Fails()
    {
        var act = () => CopilotAuth.Resolve(Env(("COPILOT_AUTH", "token")));

        act.Should().Throw<InvalidOperationException>().WithMessage("*COPILOT_GITHUB_TOKEN is not set*");
    }

    [Fact]
    public void Unset_ClassicAmbientToken_IsHidden_SoLoginCanWin()
    {
        var auth = CopilotAuth.Resolve(Env(("GITHUB_TOKEN", Classic)));

        auth.HiddenVariables.Should().BeEquivalentTo("GITHUB_TOKEN");
        auth.Description.Should().Be("copilot login (ignoring classic token in GITHUB_TOKEN)");
    }

    [Fact]
    public void Unset_UsableAmbientToken_IsLeftToTheCli()
    {
        var auth = CopilotAuth.Resolve(Env(("GH_TOKEN", "gho_x")));

        auth.HiddenVariables.Should().BeEmpty();
        auth.Description.Should().Be("GH_TOKEN, else copilot login");
    }

    [Theory]
    [InlineData("bogus")]
    [InlineData("oauth")]
    public void InvalidMode_Fails(string mode)
    {
        var act = () => CopilotAuth.Resolve(Env(("COPILOT_AUTH", mode)));

        act.Should().Throw<InvalidOperationException>().WithMessage("*COPILOT_AUTH*");
    }

    [Theory]
    [InlineData("LOGIN", CopilotAuth.Mode.Login)]
    [InlineData(" pat ", CopilotAuth.Mode.Token)]
    [InlineData("auto", CopilotAuth.Mode.Auto)]
    public void ParseMode_IsLenient(string value, CopilotAuth.Mode expected)
    {
        CopilotAuth.ParseMode(value).Should().Be(expected);
    }

    [Fact]
    public void Apply_Token_SetsGitHubToken_AndKeepsEnvironment()
    {
        var options = CopilotAuth.CreateClientOptions(Env(("COPILOT_GITHUB_TOKEN", FineGrained)));

        options.Mode.Should().Be(CopilotClientMode.CopilotCli);
        options.GitHubToken.Should().Be(FineGrained);
        options.Environment.Should().BeNull();
    }

    [Fact]
    public void Apply_Login_PassesEnvironmentWithoutHiddenVariables()
    {
        const string marker = "LMA_COPILOT_AUTH_TEST_MARKER";
        var originalGhToken = Environment.GetEnvironmentVariable("GH_TOKEN");
        Environment.SetEnvironmentVariable(marker, "kept");
        Environment.SetEnvironmentVariable("GH_TOKEN", "gho_hidden");
        try
        {
            var resolution = new CopilotAuth.Resolution(CopilotAuth.Mode.Login, null, null, new[] { "GH_TOKEN" }, "copilot login");
            var options = CopilotAuth.Apply(new CopilotClientOptions(), resolution);

            options.GitHubToken.Should().BeNull();
            options.Environment.Should().NotBeNull();
            options.Environment!.Should().ContainKey(marker);
            options.Environment!.Keys.Should().NotContain("GH_TOKEN");
        }
        finally
        {
            Environment.SetEnvironmentVariable(marker, null);
            Environment.SetEnvironmentVariable("GH_TOKEN", originalGhToken);
        }
    }

    [Theory]
    [InlineData("github_pat_abc", true)]
    [InlineData("gho_abc", true)]
    [InlineData("ghu_abc", true)]
    [InlineData("ghp_abc", false)]
    public void ValidateToken_OnlyRejectsClassic(string token, bool valid)
    {
        (CopilotAuth.ValidateToken(token) is null).Should().Be(valid);
    }
}
