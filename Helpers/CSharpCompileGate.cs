// Builds the generated C# with the real compiler and, when it fails, repairs the files the errors
// point at and builds again, for a bounded number of rounds. The outcome is recorded either way:
// a run is never reported as compiling unless the compiler said so.

using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Helpers;

public sealed record CompileRound(int Round, int Errors, int FilesRepaired, int RepairsRejected, IReadOnlyDictionary<string, int> ByCode);

public sealed record CompileGateResult(
    bool Compiled,
    bool Measured,
    string? FailureReason,
    IReadOnlyList<CompileRound> Rounds,
    IReadOnlyList<CompilerDiagnostic> RemainingErrors,
    IReadOnlyList<string> ExternalContracts)
{
    public string ToMarkdown()
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
        foreach (var r in Rounds) sb.AppendLine($"| {r.Round} | {r.Errors} | {r.FilesRepaired} | {r.RepairsRejected} |");
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
            foreach (var e in RemainingErrors.Take(50)) sb.AppendLine(e.ToString());
            if (RemainingErrors.Count > 50) sb.AppendLine($"… and {RemainingErrors.Count - 50} more (see compile-status.json)");
            sb.AppendLine("```");
            sb.AppendLine();
        }

        return sb.ToString();
    }
}

public static class CSharpCompileGate
{
    public const string StatusFile = "compile-status.json";

    public static int MaxRoundsFromEnvironment() =>
        int.TryParse(Environment.GetEnvironmentVariable("COMPILE_REPAIR_MAX_ROUNDS"), out var n) && n >= 0 ? n : 3;

    public static async Task<CompileGateResult> RunAsync(
        string runFolder,
        string rootNamespace,
        string sharedNamespace,
        CompileRepairAgent? repairAgent,
        int maxRounds,
        ILogger logger,
        IReadOnlyDictionary<string, string>? cobolByName = null,
        CancellationToken cancellationToken = default)
    {
        var timeout = TimeSpan.FromMinutes(5);
        var rounds = new List<CompileRound>();
        BuildOutcome outcome;
        var round = 0;

        while (true)
        {
            outcome = await GeneratedBuildRunner.BuildAsync(runFolder, timeout, cancellationToken);
            if (!outcome.Ran)
                return Finish(runFolder, new CompileGateResult(false, false, outcome.FailureReason, rounds, [], []), logger);

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
            var tasks = CompileRepairPlanner.Plan(outcome.Errors, GeneratedTypeIndex.FromSources(sources), sources, sharedNamespace, cobolByName);

            var repaired = 0;
            var rejected = 0;
            using var gate = new SemaphoreSlim(4);
            await Task.WhenAll(tasks.Select(async task =>
            {
                await gate.WaitAsync(cancellationToken);
                try
                {
                    var result = await repairAgent.RepairAsync(task, sources[task.File]);
                    if (result is null) { Interlocked.Increment(ref rejected); return; }
                    await File.WriteAllTextAsync(Path.Join(runFolder, task.File), result, cancellationToken);
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
            round++;
        }

        if (rounds.Count == 0 || rounds[^1].Round != round) rounds = Append(rounds, round, outcome, 0, 0);

        return Finish(runFolder, new CompileGateResult(
            outcome.Succeeded, true,
            outcome.Succeeded ? null : $"{rounds.Count - 1} repair round(s) did not converge",
            rounds, outcome.Errors, ExternalContracts(runFolder)), logger);
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
