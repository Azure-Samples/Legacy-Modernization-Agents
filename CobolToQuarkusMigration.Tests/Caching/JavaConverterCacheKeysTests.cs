using CobolToQuarkusMigration.Agents.Infrastructure.Caching;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Caching;

public class JavaConverterCacheKeysTests
{
    private static CobolFile MakeFile(string name = "PROG.cbl", string content = "       PROGRAM-ID. PROG.") => new()
    {
        FileName = name,
        FilePath = "/source/" + name,
        Content = content,
        IsCopybook = false,
    };

    private static CacheKey Build(
        string systemPrompt = "sys",
        string userPrompt = "usr",
        string source = "PROGRAM-ID. PROG.",
        string rekt = "REKT STRUCTURAL CONTEXT: …",
        string model = "gpt-5.3-codex",
        int maxTokens = 16000,
        string effort = "high",
        CobolFile? file = null) =>
        JavaConverterCacheKeys.ForConversion(
            systemPrompt, userPrompt, source, rekt, model, maxTokens, effort,
            file ?? MakeFile());

    [Fact]
    public void IdenticalInputs_ProduceIdenticalKey()
    {
        Build().Compute().Should().Be(Build().Compute());
    }

    [Fact]
    public void SourceChange_ProducesDifferentKey()
    {
        Build(source: "A").Compute().Should().NotBe(Build(source: "B").Compute());
    }

    [Fact]
    public void RektContextChange_ProducesDifferentKey()
    {
        Build(rekt: "rekt-v1").Compute().Should().NotBe(Build(rekt: "rekt-v2").Compute());
    }

    [Fact]
    public void EmptyRekt_IsDistinctFromAnyRektContent()
    {
        Build(rekt: "").Compute().Should().NotBe(Build(rekt: "anything").Compute());
    }

    [Fact]
    public void ModelChange_ProducesDifferentKey()
    {
        Build(model: "gpt-5.3-codex").Compute().Should().NotBe(Build(model: "claude-opus-4.7").Compute());
    }

    [Fact]
    public void MaxTokensChange_ProducesDifferentKey_ViaGenerationSettings()
    {
        Build(maxTokens: 16_000).Compute().Should().NotBe(Build(maxTokens: 32_000).Compute());
    }

    [Fact]
    public void ReasoningEffortChange_ProducesDifferentKey()
    {
        Build(effort: "high").Compute().Should().NotBe(Build(effort: "medium").Compute());
    }

    [Fact]
    public void TemplateVersionBump_ProducesDifferentKey()
    {
        // Simulate template-version bump by directly constructing two keys with
        // different versions. This guards the contract that templating bumps
        // invalidate cleanly.
        var k1 = Build();
        var k2 = k1 with { PromptTemplateVersion = "2" };
        k2.Compute().Should().NotBe(k1.Compute());
    }

    [Fact]
    public void IdentitySchemeBump_ProducesDifferentKey()
    {
        // Simulates the future basename→relativePath migration.
        var k1 = Build();
        var k2 = k1 with { IdentitySchemeVersion = "v2-relative-path" };
        k2.Compute().Should().NotBe(k1.Compute());
    }

    [Fact]
    public void ProviderNamespace_IsolatesKeys()
    {
        // Same prompt, different provider → different cache entries.
        var k1 = Build();
        var k2 = k1 with { ProviderKey = "github-copilot-sdk" };
        k2.Compute().Should().NotBe(k1.Compute());
    }

    [Fact]
    public void BasenamePopulated_OnEveryKey()
    {
        Build().Basename.Should().Be("PROG.cbl");
    }

    [Fact]
    public void RelativePath_NullToday_ForwardCompat()
    {
        // Stays null until ProgramKey migration; documented in basename-coupling-map.md.
        Build().RelativePath.Should().BeNull();
    }

    [Theory]
    [InlineData("package x; class C {}", true)]
    [InlineData("package x; class C { void f() { return; } }", true)]
    [InlineData("package x; class C {", false)]                    // unbalanced
    [InlineData("class C {}", false)]                              // no package
    [InlineData("package x; interface I {}", false)]               // no class
    [InlineData("", false)]
    [InlineData("   ", false)]
    public void IsCacheableJava_GatesIncompleteOutput(string code, bool expected)
    {
        JavaConverterCacheKeys.IsCacheableJava(code).Should().Be(expected);
    }
}
