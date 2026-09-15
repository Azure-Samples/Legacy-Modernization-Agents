using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// Deleting Config/ai-config.env removed the placeholder values that used to satisfy validation
// for every command, so any command that runs before configuration exists must be whitelisted
// explicitly or it now fails before its handler runs.
public class RequiresAiSettingsTests
{
    [Theory]
    [InlineData("program-facts")]
    [InlineData("rekt-scan-cache")]
    [InlineData("conversation")]
    public void FileOnlyCommands_DoNotRequireAiSettings(string command)
    {
        Program.RequiresAiSettings([command]).Should().BeFalse();
    }

    [Fact]
    // doctor.sh runs this during setup to populate the model picker, before any config is
    // written. Its handler uses the Copilot SDK and never reads the Azure settings.
    public void ListModels_DoesNotRequireAiSettings()
    {
        Program.RequiresAiSettings(["list-models"]).Should().BeFalse();
    }

    [Theory]
    [InlineData("--help")]
    [InlineData("-h")]
    [InlineData("--version")]
    public void HelpAndVersion_DoNotRequireAiSettings(string flag)
    {
        Program.RequiresAiSettings([flag]).Should().BeFalse();
    }

    [Fact]
    public void ConvertingCommands_StillRequireAiSettings()
    {
        Program.RequiresAiSettings(["migrate"]).Should().BeTrue();
    }
}
