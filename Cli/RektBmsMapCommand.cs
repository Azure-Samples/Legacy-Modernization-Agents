using System.CommandLine;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

namespace CobolToQuarkusMigration.Cli;

// preprocess-for-rekt.sh generates the symbolic map copybooks z/OS builds produce from BMS but repositories omit.
public static class RektBmsMapCommand
{
    public static Command Build()
    {
        var cmd = new Command("rekt-bms-map", "Generate COBOL symbolic map copybooks and screen definitions from CICS BMS sources.");
        var sourceDir = new Argument<string>("source-dir", "Original source tree holding *.bms files.");
        var outputDir = new Argument<string>("output-dir", "Preprocessed output directory.");
        cmd.AddArgument(sourceDir);
        cmd.AddArgument(outputDir);
        cmd.SetHandler((string source, string output) =>
        {
            var (generated, warnings) = BmsSymbolicMap.Generate(source, output);
            foreach (var warning in warnings) Console.Error.Write($"  ⚠️  BMS: {warning}\n");
            if (generated.Count > 0)
                Console.Out.Write($"  Generated {generated.Count} symbolic map copybook(s) from BMS: {string.Join(", ", generated)}\n");
        }, sourceDir, outputDir);
        return cmd;
    }
}
