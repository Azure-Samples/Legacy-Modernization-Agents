using System.CommandLine;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

namespace CobolToQuarkusMigration.Cli;

// preprocess-for-rekt.sh projects platform copybooks (DFHAID, DFHBMSCA, ...) the estate does not supply.
public static class RektPlatformContractsCommand
{
    public static Command Build()
    {
        var cmd = new Command("rekt-platform-contracts",
            "Generate symbolic platform copybooks from the contracts named by $REKT_PLATFORM_CONTRACTS; does nothing when unset.");
        var sourceRoot = new Argument<string>("source-root", "Original source tree.");
        var outputRoot = new Argument<string>("output-root", "Preprocessed output directory.");
        cmd.AddArgument(sourceRoot);
        cmd.AddArgument(outputRoot);
        cmd.SetHandler(context =>
        {
            try
            {
                PlatformCopybookContracts.Generate(context.ParseResult.GetValueForArgument(sourceRoot),
                    context.ParseResult.GetValueForArgument(outputRoot), Environment.GetEnvironmentVariable("REKT_PLATFORM_CONTRACTS"), Console.Out);
            }
            catch (Exception error) when (RektEvidence.Caught(error))
            {
                Console.Error.WriteLine($"rekt-platform-contracts: {error.Message}");
                context.ExitCode = 1;
            }
        });
        return cmd;
    }
}
