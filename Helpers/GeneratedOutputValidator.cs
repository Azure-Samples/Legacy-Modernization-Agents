using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Helpers;

public sealed record GeneratedOutputValidationResult(
    bool Accepted, string Markdown, JavaCompileResult? JavaBuild, CompileGateResult? CSharpBuild);

public sealed class MigrationExecutionOptions
{
    public string? RepositoryRoot { get; init; }
    public string LogDirectory { get; init; } = "Logs";
    public string MavenExecutable { get; init; } = "mvn";
}

public static class GeneratedOutputValidator
{
    public static async Task<GeneratedOutputValidationResult> RunAsync(
        IReadOnlyList<CodeFile> files, IReadOnlyList<CobolFile> sources,
        string sourceRoot, string outputFolder, TargetLanguage targetLanguage,
        CompileGateSettings settings, ILogger logger,
        Func<ParityRepairRequest, Task<IReadOnlyDictionary<string, string>?>>? repair = null,
        MigrationExecutionOptions? execution = null, CancellationToken cancellationToken = default,
        ConversionSettings? conversionSettings = null)
    {
        var java = targetLanguage == TargetLanguage.Java;
        var language = java ? "Java" : "C#";
        var repositoryRoot = execution?.RepositoryRoot
            ?? Environment.GetEnvironmentVariable("REPO_ROOT") ?? Directory.GetCurrentDirectory();
        var requirementsBySource = new Dictionary<string, IReadOnlyList<RuntimeImplementationRequirement>>(StringComparer.Ordinal);
        foreach (var source in sources.Where(source => SourceTypeRegistry.IsCobolProgram(source.FileName)))
            requirementsBySource[SourcePathHelper.ForFile(source)] = VerifiedRuntimeContext.Load(
                ConversionEligibility.FactsDirectory(repositoryRoot), sourceRoot,
                SourcePathHelper.RelativeToSource(SourcePathHelper.ForFile(source), sourceRoot));
        var runtimeRequirements = requirementsBySource.Values.SelectMany(requirements => requirements).ToArray();
        if (repair is not null)
        {
            var originalRepair = repair;
            repair = request => originalRepair(request with
            {
                RuntimeRequirements = files.Where(file => request.Files.ContainsKey(file.FilePath))
                    .SelectMany(file => requirementsBySource.GetValueOrDefault(file.OriginalSourcePath) ?? [])
                    .ToArray()
            });
        }
        Directory.CreateDirectory(outputFolder);
        await File.WriteAllTextAsync(Path.Combine(outputFolder, "runtime-implementation-requirements.json"),
            JsonSerializer.Serialize(new
            {
                behaviorVerified = false,
                requirements = runtimeRequirements
            }, new JsonSerializerOptions { WriteIndented = true }), cancellationToken);
        if (java) JavaProjectScaffold.Write(outputFolder, ConversionNamespacePolicy.Root("Java"),
            framework: JavaGeneration.Framework(conversionSettings));
        var markdown = await ConversionParityPostPass.RunAsync(files, outputFolder, language, logger,
            sources.Select(SourcePathHelper.ForFile), repositoryRoot: repositoryRoot,
            repair: repair, sourceDirectory: sourceRoot, applyGate: false, cancellationToken: cancellationToken);
        JavaCompileResult? javaBuild = null;
        CompileGateResult? csharpBuild = null;
        if (settings.Enabled)
        {
            if (java)
                javaBuild = await JavaCompileGate.RunAsync(outputFolder, settings, logger, cancellationToken,
                    execution?.MavenExecutable ?? "mvn", repair, files);
            else
                csharpBuild = await CSharpCompileGate.RunAsync(outputFolder,
                    ConversionNamespacePolicy.Root("C#"), ConversionNamespacePolicy.ForSharedTypes("C#"),
                    null, settings, logger, cancellationToken: cancellationToken);
            foreach (var file in files)
                if (File.Exists(file.FilePath)) file.Content = await File.ReadAllTextAsync(file.FilePath, cancellationToken);
            markdown = await ConversionParityPostPass.RunAsync(files, outputFolder, language, logger,
                sources.Select(SourcePathHelper.ForFile), repositoryRoot: repositoryRoot,
                sourceDirectory: sourceRoot, applyGate: false, preserveRepairHistory: true, cancellationToken: cancellationToken);
        }
        var artifact = Path.Combine(outputFolder, ConversionParityPostPass.ArtifactName);
        var parity = File.Exists(artifact)
            ? JsonSerializer.Deserialize<ConversionParityReport>(await File.ReadAllTextAsync(artifact, cancellationToken))
            : null;
        var complete = sources.All(source => files.Any(file =>
            file.OriginalSourcePath == SourcePathHelper.ForFile(source) && File.Exists(file.FilePath)));
        var invalidOutput = parity?.Programs.Any(program => program.IsDiagnosticStub
            || program.Gaps.Any(gap => gap.Axis == "file")
            || program.NotEvaluatedReason?.Contains("multiple source identities", StringComparison.Ordinal) == true) == true;
        var strict = ConversionParityPostPass.ReadGate(logger) == ConversionParityPostPass.ParityGate.Stop;
        var parityAccepted = !strict || parity is { EvaluatedCount: > 0, BelowThresholdCount: 0, NotEvaluatedCount: 0 };
        var buildAccepted = !settings.Enabled || (java ? javaBuild?.Compiled == true : csharpBuild?.Compiled == true);
        var accepted = complete && !invalidOutput && parityAccepted && buildAccepted;
        if (MetricsSink.CurrentRunId is not null)
            MetricsSink.EmitAmbient(new
            {
                Event = "validation_completed",
                Accepted = accepted,
                OutputComplete = complete && !invalidOutput,
                CompileEnabled = settings.Enabled,
                CompileMeasured = java ? javaBuild?.Measured : csharpBuild?.Measured,
                Compiled = java ? javaBuild?.Compiled : csharpBuild?.Compiled,
                ParityEvaluated = parity?.EvaluatedCount,
                ParityBelowThreshold = parity?.BelowThresholdCount,
                ParityNotEvaluated = parity?.NotEvaluatedCount,
                StrictParityGate = strict,
            });
        if (!accepted && Environment.ExitCode == 0)
            Environment.ExitCode = buildAccepted ? ConversionParityPostPass.LowScoreExitCode : JavaCompileGate.FailureExitCode;
        if (javaBuild is not null)
        {
            markdown += javaBuild.Compiled
                ? "\n## Java compilation\n\nThe generated project compiles with Maven. Compilation does not establish conversion completeness or behavioral equivalence.\n"
                : $"\n## Java compilation\n\n**Not accepted:** {javaBuild.FailureReason} Compiler diagnostics: {javaBuild.Errors.Count}.\n";
            markdown += "\n| Round | Compiled | Maven exit | Diagnostics | Rolled back |\n|---|---|---|---|---|\n";
            foreach (var round in javaBuild.Rounds)
                markdown += $"| {round.Round} | {round.Compiled} | {round.ExitCode?.ToString() ?? "not measured"} | {round.Errors.Count} | {round.RolledBack} |\n";
        }
        if (csharpBuild is not null) markdown += csharpBuild.ToMarkdown(settings.MaxErrorsInReport);
        if (runtimeRequirements.Length > 0)
            markdown += "\n## Runtime implementation requirements\n\n" +
                "The source-backed CICS/SQL operations were passed to code generation, not rejected because target " +
                "adapters did not already exist. See runtime-implementation-requirements.json for the original " +
                "operations and bindings. **Runtime behavior is not verified by compilation or name-based parity.** " +
                "Exercise persistence, SQL results, status/error paths, transactions and control transfers before " +
                "claiming application equivalence.\n";
        if (!complete || invalidOutput)
            markdown += "\n**Conversion incomplete:** missing, diagnostic-only or conflicting generated output remains.\n";
        return new(accepted, markdown, javaBuild, csharpBuild);
    }
}
