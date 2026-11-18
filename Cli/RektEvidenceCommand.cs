using System.CommandLine;
using System.CommandLine.Invocation;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;

namespace CobolToQuarkusMigration.Cli;

// doctor.sh and preprocess-for-rekt.sh bind every staged, preprocessed and parsed file to its original bytes
// through this command; it is offline and needs no AI settings.
public static class RektEvidenceCommand
{
    public static readonly string[] Actions =
    [
        "write", "validate", "quarantine", "reset-preprocessed", "provenance", "stage", "validate-definition",
        "dependency-search-view", "commarea-closure", "shadowed-copybooks",
    ];

    public static Command Build()
    {
        var cmd = new Command("rekt-evidence", "Write, validate and stage hash-bound REKT dependency evidence.");
        var action = new Argument<string>("action", "What to do.").FromAmong(Actions);
        var sourceRoot = new Option<string?>("--source-root", "Original source tree.");
        var relativePath = new Option<string?>("--relative-path", "Program path relative to the source root.");
        var outputRoot = new Option<string?>("--output-root", "REKT output (or preprocessed) directory.");
        var stagingRoot = new Option<string?>("--staging-root", "Flat REKT staging directory.");
        var parserSourceRoot = new Option<string?>("--parser-source-root", "Directory the parser actually read.");
        var dependencyProfile = new Option<string?>("--dependency-profile", () => NonEmpty("REKT_DEPENDENCY_PROFILE"),
            "Dependency profile JSON (default: $REKT_DEPENDENCY_PROFILE).");
        var sqlDeclarations = new Option<string?>("--sql-declarations", () => NonEmpty("REKT_SQL_DECLARATIONS"),
            "SQL declaration files separated by the path separator (default: $REKT_SQL_DECLARATIONS).");
        var exportExitCode = new Option<int>("--export-exit-code", () => 1, "Exit code of the dependency exporter.");
        var source = new Option<string?>("--source", "Original file.");
        var target = new Option<string?>("--target", "Staged or preprocessed file.");
        var transformed = new Option<string?>("--transformed", "Preprocessed file the target was copied from.");
        var derived = new Option<bool>("--derived", "The target is derived from, not a copy of, the source.");
        var estate = new Option<string?>("--estate", "Estate folder under the output root.");
        cmd.AddArgument(action);
        foreach (var option in new Option[]
                 {
                     sourceRoot, relativePath, outputRoot, stagingRoot, parserSourceRoot, dependencyProfile, sqlDeclarations,
                     exportExitCode, source, target, transformed, derived, estate,
                 })
            cmd.AddOption(option);

        cmd.SetHandler(context =>
        {
            var parse = context.ParseResult;
            string Required(Option<string?> option) =>
                parse.GetValueForOption(option) ?? throw PyError.Value($"{option.Name} is required for {parse.GetValueForArgument(action)}");
            try
            {
                context.ExitCode = parse.GetValueForArgument(action) switch
                {
                    "commarea-closure" => CommareaClosure(Required(sourceRoot), Required(outputRoot), Required(estate)),
                    "dependency-search-view" => Done(() => RektEvidence.DependencySearchView(Required(stagingRoot), Required(relativePath))),
                    "validate-definition" => RektEvidence.DefinitionProblem(Required(source)) is { Length: > 0 } ? 1 : 0,
                    "provenance" => Done(() => RektEvidence.Provenance(Required(source), Required(target))),
                    "stage" => Done(() => RektEvidence.Stage(Required(source), Required(target), parse.GetValueForOption(transformed),
                        parse.GetValueForOption(derived), Console.Error)),
                    "reset-preprocessed" => Done(() => RektEvidence.ResetPreprocessed(Required(outputRoot))),
                    "quarantine" => Done(() => RektEvidence.Quarantine(Required(outputRoot), Required(relativePath))),
                    "validate" => RektEvidence.EvidenceCurrent(Required(sourceRoot), Required(relativePath), Required(outputRoot)) ? 0 : 1,
                    "shadowed-copybooks" => ShadowedCopybooks(Required(sourceRoot)),
                    _ => RektEvidence.Write(Required(sourceRoot), Required(relativePath), Required(outputRoot),
                        parse.GetValueForOption(exportExitCode), parse.GetValueForOption(stagingRoot),
                        parse.GetValueForOption(parserSourceRoot), parse.GetValueForOption(dependencyProfile),
                        parse.GetValueForOption(sqlDeclarations)),
                };
            }
            catch (Exception error) when (RektEvidence.Caught(error) || BmsSymbolicMap.Caught(error))
            {
                Console.Error.WriteLine($"REKT evidence: {error.Message}");
                context.ExitCode = 1;
            }
        });
        return cmd;
    }

    private static string? NonEmpty(string variable) =>
        Environment.GetEnvironmentVariable(variable) is { Length: > 0 } value ? value : null;

    private static int Done(Action action)
    {
        action();
        return 0;
    }

    // Placeholder or unverified copybooks a genuine definition must replace before staging.
    private static int ShadowedCopybooks(string sourceRoot)
    {
        foreach (var path in RektEvidence.OriginalFiles(sourceRoot))
            if (PyCompat.Suffix(path).ToLowerInvariant() is ".cpy" or ".cpb" && RektEvidence.DefinitionProblem(path) is { Length: > 0 })
                Console.Out.Write(path + "\n");
        return 0;
    }

    private static int CommareaClosure(string sourceRoot, string outputRoot, string estate)
    {
        var closure = RektEvidence.WriteCommareaClosure(sourceRoot, outputRoot, estate);
        var errors = closure["errors"]!.AsArray().Select(error => PyJson.PyStr(error)).ToList();
        if (errors.Count > 0)
            Console.Error.WriteLine($"  ⚠ COMMAREA closure not applied for {estate}: {string.Join("; ", errors.Select(DescribeClosureError))} (details: .commarea-closure.json)");
        return 0;
    }

    // The raw tokens stay in .commarea-closure.json; the console gets one sentence a person can act on.
    internal static string DescribeClosureError(string error)
    {
        var parts = error.Split(':');
        string At(int i) => i < parts.Length ? parts[i] : "?";
        return parts[0] switch
        {
            "commarea-closure-carrier-unsupported" => $"{At(1)} passes {At(2)} in a form the closure cannot follow",
            "commarea-closure-send-unsupported" => $"{At(1)} sends {At(2)} in a form the closure cannot follow",
            "commarea-closure-foreign-send" => $"{At(1)} sends {At(2)} (line {At(3)}) to a program outside the estate",
            "commarea-closure-incompatible-layouts" => $"{At(1)} different COMMAREA layouts share the record",
            "commarea-closure-participant-unavailable" => $"no dependency evidence yet for {At(1)}",
            "commarea-closure-participant-invalid" => $"unreadable dependency evidence for {At(1)}",
            _ => error,
        };
    }
}
