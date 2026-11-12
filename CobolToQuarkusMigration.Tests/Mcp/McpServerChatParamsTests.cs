using System.Text.Json;
using CobolToQuarkusMigration.Mcp;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Mcp;

public class McpServerChatParamsTests
{
    private static JsonElement Params(string json) => JsonDocument.Parse(json).RootElement;

    [Theory]
    [InlineData("""{"context":"report"}""", true)]
    [InlineData("""{"context":"REPORT"}""", true)]
    [InlineData("""{"context":"run"}""", false)]
    [InlineData("""{"context":1}""", false)]
    [InlineData("""{}""", false)]
    public void IsReportContext_OnlyForReportString(string json, bool expected)
        => McpServer.IsReportContext(Params(json)).Should().Be(expected);

    [Theory]
    [InlineData("""{"maxOutputTokens":1200}""", 1200)]
    [InlineData("""{"maxOutputTokens":0}""", 4000)]
    [InlineData("""{"maxOutputTokens":"900"}""", 4000)]
    [InlineData("""{}""", 4000)]
    public void ReadMaxOutputTokens_UsesPositiveNumberElseDefault(string json, int expected)
        => McpServer.ReadMaxOutputTokens(Params(json)).Should().Be(expected);
}
