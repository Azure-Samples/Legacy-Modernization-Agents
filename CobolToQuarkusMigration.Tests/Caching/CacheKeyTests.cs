using CobolToQuarkusMigration.Agents.Infrastructure.Caching;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Caching;

public class CacheKeyTests
{
    private static CacheKey MakeValid() => CacheKey.Build(new CacheKey
    {
        ProviderKey = "azure-openai",
        Model = "gpt-5.3-codex",
        SystemPromptHash = "abc",
        UserPromptHash = "def",
        ReasoningEffort = "high",
        ResponseFormat = "text",
        PromptTemplateId = "java-converter",
        PromptTemplateVersion = "1",
        TargetLanguage = "java",
        FrameworkSettings = "quarkus",
        SourceHash = "src-hash",
        RektFactsHash = "rekt-hash",
        GenerationSettingsHash = "gen-hash",
        Basename = "PROG.cbl",
    });

    [Fact]
    public void Compute_IsDeterministicForIdenticalKey()
    {
        MakeValid().Compute().Should().Be(MakeValid().Compute());
    }

    [Theory]
    [InlineData("ProviderKey")]
    [InlineData("Model")]
    [InlineData("SystemPromptHash")]
    [InlineData("UserPromptHash")]
    [InlineData("ReasoningEffort")]
    [InlineData("ResponseFormat")]
    [InlineData("PromptTemplateId")]
    [InlineData("PromptTemplateVersion")]
    [InlineData("TargetLanguage")]
    [InlineData("FrameworkSettings")]
    [InlineData("SourceHash")]
    [InlineData("RektFactsHash")]
    [InlineData("GenerationSettingsHash")]
    [InlineData("IdentitySchemeVersion")]
    public void Compute_ChangesWhenAnyKeyFieldChanges(string fieldName)
    {
        var baseKey = MakeValid();
        var changed = fieldName switch
        {
            "ProviderKey"             => baseKey with { ProviderKey = "github-copilot-sdk" },
            "Model"                   => baseKey with { Model = "claude-opus-4.7" },
            "SystemPromptHash"        => baseKey with { SystemPromptHash = "xxx" },
            "UserPromptHash"          => baseKey with { UserPromptHash = "yyy" },
            "ReasoningEffort"         => baseKey with { ReasoningEffort = "low" },
            "ResponseFormat"          => baseKey with { ResponseFormat = "json" },
            "PromptTemplateId"        => baseKey with { PromptTemplateId = "csharp-converter" },
            "PromptTemplateVersion"   => baseKey with { PromptTemplateVersion = "2" },
            "TargetLanguage"          => baseKey with { TargetLanguage = "csharp" },
            "FrameworkSettings"       => baseKey with { FrameworkSettings = "dotnet" },
            "SourceHash"              => baseKey with { SourceHash = "different" },
            "RektFactsHash"           => baseKey with { RektFactsHash = "different" },
            "GenerationSettingsHash"  => baseKey with { GenerationSettingsHash = "different" },
            "IdentitySchemeVersion"   => baseKey with { IdentitySchemeVersion = "v2-relative-path" },
            _ => throw new ArgumentOutOfRangeException(nameof(fieldName)),
        };

        changed.Compute().Should().NotBe(baseKey.Compute(),
            $"changing {fieldName} must produce a different cache key");
    }

    [Theory]
    [InlineData("ProviderKey", "")]
    [InlineData("Model", "")]
    [InlineData("SystemPromptHash", "")]
    [InlineData("UserPromptHash", "")]
    [InlineData("PromptTemplateId", "")]
    [InlineData("PromptTemplateVersion", "")]
    [InlineData("TargetLanguage", "")]
    [InlineData("GenerationSettingsHash", "")]
    public void Build_RejectsEmptyRequiredFields(string fieldName, string value)
    {
        var template = new CacheKey
        {
            ProviderKey = fieldName == "ProviderKey" ? value : "p",
            Model = fieldName == "Model" ? value : "m",
            SystemPromptHash = fieldName == "SystemPromptHash" ? value : "s",
            UserPromptHash = fieldName == "UserPromptHash" ? value : "u",
            ReasoningEffort = "low",
            ResponseFormat = "text",
            PromptTemplateId = fieldName == "PromptTemplateId" ? value : "t",
            PromptTemplateVersion = fieldName == "PromptTemplateVersion" ? value : "1",
            TargetLanguage = fieldName == "TargetLanguage" ? value : "java",
            FrameworkSettings = "",
            SourceHash = "",
            RektFactsHash = "",
            GenerationSettingsHash = fieldName == "GenerationSettingsHash" ? value : "g",
        };

        var act = () => CacheKey.Build(template);
        act.Should().Throw<ArgumentException>().WithMessage($"*{fieldName}*");
    }

    [Fact]
    public void Compute_TreatsNullAndEmptyDifferently()
    {
        // Specifically: a key with FrameworkSettings = "" must differ from a key
        // where FrameworkSettings sat at its default. Since required = string, the
        // two would be the same if both default to ""; this is a smoke check that
        // a real-world divergence (null vs "") is preserved by CanonicalHasher.
        // We verify via direct hasher use because the public CacheKey contract
        // requires non-null strings.
        var withEmpty = CobolToQuarkusMigration.Helpers.CanonicalHasher.HashFields("a", "", "b");
        var withNull = CobolToQuarkusMigration.Helpers.CanonicalHasher.HashFields("a", null, "b");
        withEmpty.Should().NotBe(withNull);
    }
}
