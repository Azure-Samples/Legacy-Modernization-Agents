using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

[Collection("EnvironmentSensitive")]
public sealed class MigrationPathPrecedenceTests
{
    [Theory]
    [InlineData(null, null, "env-source", "env-output")]
    [InlineData("cli-source", "cli-output", "cli-source", "cli-output")]
    public void ExplicitPathsTakePrecedenceWithoutChangingEnvironment(
        string? source, string? output, string expectedSource, string expectedOutput)
    {
        var beforeSource = Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER");
        var beforeOutput = Environment.GetEnvironmentVariable("JAVA_OUTPUT_FOLDER");
        try
        {
            Environment.SetEnvironmentVariable("COBOL_SOURCE_FOLDER", "env-source");
            Environment.SetEnvironmentVariable("JAVA_OUTPUT_FOLDER", "env-output");
            var settings = new AppSettings();
            Program.OverrideSettingsFromEnvironment(settings, source, output);
            settings.ApplicationSettings.CobolSourceFolder.Should().Be(expectedSource);
            settings.ApplicationSettings.JavaOutputFolder.Should().Be(expectedOutput);
            Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER").Should().Be("env-source");
            Environment.GetEnvironmentVariable("JAVA_OUTPUT_FOLDER").Should().Be("env-output");
        }
        finally
        {
            Environment.SetEnvironmentVariable("COBOL_SOURCE_FOLDER", beforeSource);
            Environment.SetEnvironmentVariable("JAVA_OUTPUT_FOLDER", beforeOutput);
        }
    }
}
