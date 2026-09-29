// Builds the generated C# with the real compiler and, when it fails, repairs the files the errors
// point at and builds again, for a bounded number of rounds. The outcome is recorded either way:
// a run is never reported as compiling unless the compiler said so.

using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Helpers;

public sealed record CompileRound(
    int Round, int Errors, int FilesRepaired, int RepairsRejected, IReadOnlyDictionary<string, int> ByCode,
    bool RolledBack = false);

public sealed record CompileGateResult(
    bool Compiled,
    bool Measured,
    string? FailureReason,
    IReadOnlyList<CompileRound> Rounds,
    IReadOnlyList<CompilerDiagnostic> RemainingErrors,
    IReadOnlyList<string> ExternalContracts)
{
    public string ToMarkdown(int maxErrorsShown)
    {
        var sb = new StringBuilder();
        sb.AppendLine("## 🧱 Compile Status");
        sb.AppendLine();
        if (!Measured)
        {
            sb.AppendLine($"**Not measured:** {FailureReason}");
            sb.AppendLine();
            return sb.ToString();
        }

        sb.AppendLine(Compiled
            ? "**The generated project compiles** (`dotnet build`, 0 errors)."
            : $"**The generated project does not compile:** {RemainingErrors.Count} error(s) remain{(FailureReason is null ? "" : $" ({FailureReason})")}.");
        sb.AppendLine();
        sb.AppendLine("| Round | Errors | Files repaired | Repairs rejected |");
        sb.AppendLine("|---|---|---|---|");
        foreach (var r in Rounds)
        {
            var errors = r.RolledBack ? $"{r.Errors} (worse than before; the round's repairs were undone)" : r.Errors.ToString();
            sb.AppendLine($"| {r.Round} | {errors} | {r.FilesRepaired} | {r.RepairsRejected} |");
        }
        sb.AppendLine();

        if (ExternalContracts.Count > 0)
        {
            sb.AppendLine($"**External contracts ({ExternalContracts.Count}):** types the code uses whose COBOL was not part of this " +
                          "conversion, declared from their uses and marked `// " + CompileRepairPlanner.ExternalContractMarker +
                          "`. They compile; they are not converted logic.");
            foreach (var c in ExternalContracts) sb.AppendLine($"- `{c}`");
            sb.AppendLine();
        }

        if (RemainingErrors.Count > 0)
        {
            sb.AppendLine("```");
            foreach (var e in RemainingErrors.Take(maxErrorsShown)) sb.AppendLine(e.ToString());
            if (RemainingErrors.Count > maxErrorsShown) sb.AppendLine($"… and {RemainingErrors.Count - maxErrorsShown} more (see compile-status.json)");
            sb.AppendLine("```");
            sb.AppendLine();
        }

        return sb.ToString();
    }
}

public static class CSharpCompileGate
{
    public const string StatusFile = "compile-status.json";

    public static async Task<CompileGateResult> RunAsync(
        string runFolder,
        string rootNamespace,
        string sharedNamespace,
        CompileRepairAgent? repairAgent,
        CompileGateSettings settings,
        ILogger logger,
        IReadOnlyDictionary<string, string>? cobolByName = null,
        CancellationToken cancellationToken = default)
    {
        var timeout = TimeSpan.FromSeconds(Math.Max(1, settings.BuildTimeoutSeconds));
        var maxRounds = Math.Max(0, settings.MaxRepairRounds);
        var rounds = new List<CompileRound>();
        BuildOutcome outcome;
        BuildOutcome? previous = null;
        var undo = new Dictionary<string, string>(StringComparer.Ordinal);
        var round = 0;

        while (true)
        {
            outcome = await GeneratedBuildRunner.BuildAsync(runFolder, timeout, cancellationToken);
            if (!outcome.Ran)
                return Finish(runFolder, new CompileGateResult(false, false, outcome.FailureReason, rounds, [], []), logger);

            if (previous is not null && IsWorse(outcome.Errors, previous.Errors))
            {
                // A round must not leave the code worse than it found it. Its files are restored and
                // repair stops, since the same plan would be tried again.
                logger.LogWarning("[CompileGate] Round {Round}: {Errors} error(s), more than {Before}; undoing its repairs",
                    round, outcome.Errors.Count, previous.Errors.Count);
                foreach (var (file, text) in undo)
                    await File.WriteAllTextAsync(Path.Join(runFolder, file), text, cancellationToken);
                GeneratedProjectScaffold.Write(runFolder, rootNamespace);
                rounds = [.. rounds, new CompileRound(round, outcome.Errors.Count, 0, 0,
                    outcome.Errors.GroupBy(e => e.Code).ToDictionary(g => g.Key, g => g.Count()), RolledBack: true)];
                outcome = previous;
                break;
            }

            logger.LogInformation("[CompileGate] Round {Round}: {Errors} compiler error(s)", round, outcome.Errors.Count);

            if (outcome.NonCompilerErrors.Count > 0)
            {
                // Restore or SDK failure: the compiler never saw the code, so there is nothing to repair.
                return Finish(runFolder, new CompileGateResult(false, true,
                    "build failed before compiling: " + string.Join("; ", outcome.NonCompilerErrors.Take(3)),
                    Append(rounds, round, outcome, 0, 0), outcome.Errors, ExternalContracts(runFolder)), logger);
            }

            if (outcome.Succeeded || repairAgent is null || round >= maxRounds) break;

            var sources = ReadSources(runFolder);
            var typeIndex = GeneratedTypeIndex.FromSources(sources);
            undo = new Dictionary<string, string>(StringComparer.Ordinal);

            var (imported, toRepair) = CompileRepairPlanner.ImportUniqueNamespaces(outcome.Errors, typeIndex, sources);
            foreach (var (file, text) in imported)
            {
                undo[file] = sources[file];
                sources[file] = text;
                await File.WriteAllTextAsync(Path.Join(runFolder, file), text, cancellationToken);
                logger.LogInformation("[CompileGate] Round {Round}: imported a missing namespace in {File}", round, file);
            }

            var tasks = CompileRepairPlanner.Plan(toRepair, typeIndex, sources, sharedNamespace, settings, cobolByName);

            var repaired = imported.Count;
            var rejected = 0;
            using var gate = new SemaphoreSlim(Math.Max(1, settings.MaxConcurrentRepairs));
            await Task.WhenAll(tasks.Select(async task =>
            {
                await gate.WaitAsync(cancellationToken);
                try
                {
                    var result = await repairAgent.RepairAsync(task, sources[task.File]);
                    if (result is null) { Interlocked.Increment(ref rejected); return; }
                    lock (undo) undo.TryAdd(task.File, sources[task.File]);
                    await File.WriteAllTextAsync(Path.Join(runFolder, task.File), result, cancellationToken);
                    logger.LogInformation("[CompileGate] Round {Round}: repaired {File}", round, task.File);
                    Interlocked.Increment(ref repaired);
                }
                finally
                {
                    gate.Release();
                }
            }));

            rounds = Append(rounds, round, outcome, repaired, rejected);
            if (repaired == 0) break; // Nothing changed, so another build would report the same.

            // A repair can introduce a framework type or a second namespace; scaffold again so the
            // next build judges the code, not stale scaffolding.
            GeneratedProjectScaffold.Write(runFolder, rootNamespace);
            previous = outcome;
            round++;
        }

        if (rounds.Count == 0 || (rounds[^1].Round != round && !rounds[^1].RolledBack)) rounds = Append(rounds, round, outcome, 0, 0);

        return Finish(runFolder, new CompileGateResult(
            outcome.Succeeded, true,
            outcome.Succeeded ? null : $"{rounds.Count(r => r.Round > 0)} repair round(s) did not converge",
            rounds, outcome.Errors, ExternalContracts(runFolder)), logger);
    }

    // The compiler works in phases and reports a later phase only once the earlier ones are clean:
    // a syntax error hides every declaration error, and a declaration error hides method bodies.
    // Fixing the last error of a phase can therefore reveal many that were always there.
    private static readonly HashSet<string> SyntaxPhase = new(StringComparer.Ordinal)
    {
        "CS1001", "CS1002", "CS1003", "CS1014", "CS1022", "CS1026", "CS1031", "CS1513", "CS1514", "CS1519", "CS1525",
        "CS1529", "CS1733",
    };

    private static readonly HashSet<string> DeclarationPhase = new(StringComparer.Ordinal)
    {
        "CS0101", "CS0102", "CS0111", "CS0115", "CS0234", "CS0246", "CS0260", "CS0263", "CS0506", "CS0508", "CS0527",
        "CS0535", "CS0542", "CS0738",
    };

    /// <summary>
    /// Whether a round left the code worse, comparing the earliest phase first: fewer syntax errors is
    /// progress whatever it reveals, then fewer declaration errors, then fewer errors overall.
    /// </summary>
    public static bool IsWorse(IReadOnlyList<CompilerDiagnostic> now, IReadOnlyList<CompilerDiagnostic> before)
    {
        static (int Syntax, int Declarations, int All) Rank(IReadOnlyList<CompilerDiagnostic> errors) => (
            errors.Count(e => SyntaxPhase.Contains(e.Code)),
            errors.Count(e => DeclarationPhase.Contains(e.Code)),
            errors.Count);

        var (s, d, a) = Rank(now);
        var (sb, db, ab) = Rank(before);
        return s != sb ? s > sb : d != db ? d > db : a > ab;
    }

    private static List<CompileRound> Append(List<CompileRound> rounds, int round, BuildOutcome outcome, int repaired, int rejected) =>
    [
        .. rounds,
        new CompileRound(round, outcome.Errors.Count, repaired, rejected,
            outcome.Errors.GroupBy(e => e.Code).OrderByDescending(g => g.Count()).ToDictionary(g => g.Key, g => g.Count())),
    ];

    private static Dictionary<string, string> ReadSources(string runFolder) =>
        Directory.EnumerateFiles(runFolder, "*.cs", SearchOption.AllDirectories)
            .Select(p => (Path: p, Relative: Path.GetRelativePath(runFolder, p).Replace('\\', '/')))
            .Where(f => !f.Relative.StartsWith("bin/", StringComparison.Ordinal) && !f.Relative.StartsWith("obj/", StringComparison.Ordinal))
            .ToDictionary(f => f.Relative, f => File.ReadAllText(f.Path), StringComparer.Ordinal);

    private static IReadOnlyList<string> ExternalContracts(string runFolder)
    {
        var sources = ReadSources(runFolder);
        var marker = "// " + CompileRepairPlanner.ExternalContractMarker + ": ";
        return sources
            .SelectMany(s => s.Value.Split('\n')
                .Select(l => l.Trim())
                .Where(l => l.StartsWith(marker, StringComparison.Ordinal))
                .Select(l => l[marker.Length..].Split(' ')[0] + " (" + s.Key + ")"))
            .Distinct()
            .OrderBy(x => x, StringComparer.Ordinal)
            .ToList();
    }

    private static CompileGateResult Finish(string runFolder, CompileGateResult result, ILogger logger)
    {
        try
        {
            File.WriteAllText(Path.Join(runFolder, StatusFile),
                JsonSerializer.Serialize(result, new JsonSerializerOptions { WriteIndented = true }));
        }
        catch (IOException ex)
        {
            logger.LogWarning("[CompileGate] Could not write {File}: {Message}", StatusFile, ex.Message);
        }

        if (result.Compiled)
            logger.LogInformation("[CompileGate] Generated project compiles after {Rounds} round(s)", result.Rounds.Count - 1);
        else
            logger.LogWarning("[CompileGate] Generated project does not compile: {Reason}; {Errors} error(s) remain",
                result.FailureReason, result.RemainingErrors.Count);

        return result;
    }
}
