using System.CommandLine;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

namespace CobolToQuarkusMigration.Cli;

// doctor.sh runs this before staging; exit 1 means same-named sources would lose their identity in REKT's flat staging.
public static class RektSourceIdentityCommand
{
    public static Command Build()
    {
        var cmd = new Command("rekt-source-identity", RektSourceIdentity.Summary);
        var sourceRoot = new Argument<string>("source-root", "Original source tree.");
        var preprocessed = new Argument<string>("preprocessed-dir", "Preprocessed source directory.");
        var shadowed = new Argument<string?>("shadowed-copybooks-file", () => null,
            "Generated stand-in copybooks a real copybook replaces, one absolute path per line.");
        cmd.AddArgument(sourceRoot);
        cmd.AddArgument(preprocessed);
        cmd.AddArgument(shadowed);
        cmd.SetHandler(context =>
        {
            var parse = context.ParseResult;
            context.ExitCode = RektSourceIdentity.Run(parse.GetValueForArgument(sourceRoot), parse.GetValueForArgument(preprocessed),
                parse.GetValueForArgument(shadowed), Console.Out);
        });
        return cmd;
    }
}
