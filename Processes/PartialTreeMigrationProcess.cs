using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Agents.Interfaces;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Processes;

public sealed class PartialTreeMigrationProcess(IJavaConverterAgent converter, AppSettings settings, ILogger logger)
{
    public sealed record Result(PartialProgram Selection, string Status, string? JavaFile = null, string? Error = null);

    public async Task RunAsync(IReadOnlyList<CobolFile> sources, string sourceRoot, string outputFolder,
        Func<ParityRepairRequest, Task<IReadOnlyDictionary<string, string>?>>? repair = null,
        CancellationToken cancellationToken = default)
    {
        if (settings.ApplicationSettings.TargetLanguage != TargetLanguage.Java
            || JavaGeneration.Framework(settings.ConversionSettings) != "Spring")
            throw new InvalidDataException("Source-isolated partial conversion currently supports Java Spring only.");
        if (Directory.Exists(outputFolder) && Directory.EnumerateFileSystemEntries(outputFolder).Any())
            throw new InvalidDataException("Partial conversion requires a fresh output directory.");
        Directory.CreateDirectory(outputFolder);
        var programs = sources.Where(f => SourceTypeRegistry.IsCobolProgram(f.FileName)).ToArray();
        var results = programs.Select(p => new Result(PartialTreePlan.Assess(p, sourceRoot), "pending")).ToArray();
        var generated = new List<CodeFile>();
        var sync = new object();
        void Save()
        {
            var path = Path.Combine(outputFolder, "partial-tree-plan.json");
            File.WriteAllText(path + ".tmp", JsonSerializer.Serialize(new
            {
                Mode = "partial",
                Verified = false,
                Scope = "Only listed source-isolated routines; omitted routines and application entry points are not implemented.",
                Programs = results
            }, new JsonSerializerOptions { WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase }));
            File.Move(path + ".tmp", path, overwrite: true);
        }
        Save();
        File.WriteAllText(Path.Combine(outputFolder, "PARTIAL-NOT-VERIFIED.txt"),
            "Source-isolated partial Spring draft. NOT a complete application. See partial-tree-plan.json for exact included " +
            "and excluded routines, source hashes and reasons. Compilation is not behavioral equivalence. " +
            "Native whole-program dependency coverage is not claimed by source-level selection.");

        await Parallel.ForEachAsync(Enumerable.Range(0, results.Length),
            new ParallelOptions { MaxDegreeOfParallelism = 2, CancellationToken = cancellationToken },
            async (index, token) =>
            {
                var plan = results[index].Selection;
                var selected = plan.Selected.ToArray();
                lock (sync)
                {
                    results[index] = results[index] with { Status = selected.Length == 0 ? "blocked" : "converting" };
                    Save();
                }
                var stage = $"{index + 1}/{programs.Length} {plan.Program}: {selected.Length} selected, " +
                    $"{plan.Routines.Count - selected.Length} excluded routines";
                AiLoopEvents.StageStarted(index + 1, programs.Length + 1, stage);
                logger.LogInformation("[PARTIAL] {Stage}", stage);
                foreach (var routine in plan.Routines.Where(r => r.Blockers.Count > 0))
                    logger.LogInformation("[EXCLUDED] {Program}:{Line} {Routine}: {Reason}",
                        plan.Program, routine.StartLine, routine.Name, string.Join("; ", routine.Blockers));
                if (selected.Length == 0) return;
                try
                {
                    var source = programs[index];
                    RequireUnchanged(source.FilePath, plan);
                    var package = ConversionNamespacePolicy.Root("Java") + ".partial.p" +
                        Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(plan.Program)))[..12].ToLowerInvariant();
                    var input = JsonSerializer.Serialize(new
                    {
                        ExpectedPackage = package,
                        Selection = JsonDocument.Parse(PartialTreePlan.ConversionInput(plan)).RootElement
                    });
                    var candidate = await converter.ConvertToJavaAsync(new CobolFile
                    {
                        FileName = plan.Program,
                        FilePath = source.FilePath,
                        Content = input
                    }, new CobolAnalysis
                    {
                        RawAnalysisData = "Convert only the selected routines with supplied local declarations. " +
                            "Do not generate excluded routines or a whole-program entry point. ExpectedPackage=" + package
                    }).WaitAsync(token);
                    RequireUnchanged(source.FilePath, plan);
                    RequireCandidate(candidate, plan, package);
                    var folder = Path.Combine(outputFolder, package.Replace('.', Path.DirectorySeparatorChar));
                    Directory.CreateDirectory(folder);
                    candidate.FilePath = Path.Combine(folder, candidate.FileName);
                    await File.WriteAllTextAsync(candidate.FilePath, candidate.Content, token);
                    lock (sync)
                    {
                        generated.Add(candidate);
                        results[index] = results[index] with
                        {
                            Status = "generated-not-verified",
                            JavaFile = Path.GetRelativePath(outputFolder, candidate.FilePath)
                        };
                        Save();
                    }
                    logger.LogInformation("[GENERATED] {Program}: {Count} source-isolated routines -> {JavaFile}",
                        plan.Program, selected.Length, candidate.FilePath);
                }
                catch (Exception ex) when (ex is InvalidDataException or IOException or InvalidOperationException or HttpRequestException)
                {
                    logger.LogError(ex, "[PARTIAL FAILED] {Program}; other independent programs continue", plan.Program);
                    lock (sync)
                    {
                        results[index] = results[index] with { Status = "failed", Error = ex.Message };
                        Save();
                    }
                }
            });
        foreach (var (program, index) in programs.Select((p, i) => (p, i)))
            RequireUnchanged(program.FilePath, results[index].Selection);
        if (generated.Count == 0)
            throw new InvalidDataException("No source-isolated routine output generated; see partial-tree-plan.json.");
        AiLoopEvents.StageStarted(programs.Length + 1, programs.Length + 1, "Compiling source-isolated Spring subset");
        JavaProjectScaffold.Write(outputFolder, ConversionNamespacePolicy.Root("Java"), framework: "Spring");
        var byName = results.ToDictionary(r => r.Selection.Program, r => r.Selection);
        var build = await JavaCompileGate.RunAsync(outputFolder, settings.CompileGate, logger,
            cancellationToken, repair: repair is null ? null : request =>
                repair(request with { Source = PartialTreePlan.ConversionInput(byName[request.Program]) }),
            generatedFiles: generated);
        logger.LogWarning("[PARTIAL RESULT] {Generated}/{Total} program slices generated; {Routines} routines. " +
            "Maven compiled={Compiled}. This is NOT a complete or behaviorally verified application.",
            generated.Count, programs.Length,
            results.Where(r => r.JavaFile is not null).Sum(r => r.Selection.Selected.Count()), build.Compiled);
        if (!build.Compiled || results.Any(r => r.Status == "failed"))
            Environment.ExitCode = JavaCompileGate.FailureExitCode;
    }

    private static void RequireUnchanged(string path, PartialProgram plan)
    {
        var actual = Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path))).ToLowerInvariant();
        if (actual != plan.SourceSha256)
            throw new InvalidDataException("Source changed during partial conversion: " + plan.Program);
        foreach (var definition in plan.Definitions)
            if (Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(definition.Path))).ToLowerInvariant() != definition.Sha256)
                throw new InvalidDataException("COPY definition changed during partial conversion: " + definition.Path);
    }

    internal static void RequireCandidate(JavaFile candidate, PartialProgram plan, string package)
    {
        if (candidate.PackageName != package || !Regex.IsMatch(candidate.FileName, @"^[A-Za-z_$][A-Za-z0-9_$]*\.java$")
            || !ConversionOutputGuard.DeclaresType(candidate.Content)
            || Regex.IsMatch(candidate.Content, @"\b(TODO|UnsupportedOperationException|System\.exit)\b"))
            throw new InvalidDataException("Partial response has invalid identity, non-code output or unavailable-implementation stubs.");
        foreach (var routine in plan.Selected)
            if (!Regex.IsMatch(candidate.Content, @"public\s+void\s+" + Regex.Escape(PartialTreePlan.MethodName(routine.Name)) +
                @"\s*\(\s*(?:final\s+)?State\s+\w+\s*\)"))
                throw new InvalidDataException("Selected routine missing its public State method: " + routine.Name);
    }
}
