using System.Diagnostics;
using System.Text.Json;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;

namespace CobolToQuarkusMigration.Helpers;

public sealed record JavaCompileResult(bool Compiled, bool Measured, int? ExitCode,
    string? FailureReason, IReadOnlyList<CompilerDiagnostic> Errors)
{
    public int RepairRounds { get; init; }
    public IReadOnlyList<JavaCompileRound> Rounds { get; init; } = [];
}

public sealed record JavaCompileRound(int Round, bool Compiled, bool Measured, int? ExitCode,
    string? FailureReason, IReadOnlyList<CompilerDiagnostic> Errors, bool RolledBack = false);

public static class JavaCompileGate
{
    public const int FailureExitCode = 5;

    public static async Task<JavaCompileResult> RunAsync(string folder, CompileGateSettings settings,
        ILogger logger, CancellationToken cancellationToken = default, string executable = "mvn",
        Func<ParityRepairRequest, Task<IReadOnlyDictionary<string, string>?>>? repair = null,
        IReadOnlyList<CodeFile>? generatedFiles = null)
    {
        Directory.CreateDirectory(folder);
        JavaCompileResult result;
        var round = 0;
        var rounds = new List<JavaCompileRound>();
        void Record(JavaCompileResult build, bool rolledBack = false)
        {
            rounds.Add(new(round, build.Compiled, build.Measured, build.ExitCode, build.FailureReason, build.Errors, rolledBack));
            var log = Path.Combine(folder, "java-build.log");
            if (File.Exists(log)) File.Copy(log, Path.Combine(folder, $"java-build.round-{round}.log"), overwrite: true);
        }
        try
        {
            result = await BuildAsync(folder, Timeout(settings), executable, cancellationToken);
            Record(result);
            while (!result.Compiled && result.Measured && result.Errors.Count > 0
                   && repair is not null && round < Math.Max(0, settings.MaxRepairRounds))
            {
                var startedRound = false;
                var originals = new Dictionary<string, string>();
                var candidates = new Dictionary<string, string>();
                foreach (var group in result.Errors.GroupBy(error => error.File))
                {
                    cancellationToken.ThrowIfCancellationRequested();
                    var path = Path.GetFullPath(Path.Combine(folder, group.Key));
                    var mapped = (generatedFiles ?? []).Where(file => !string.IsNullOrWhiteSpace(file.FilePath)
                        && Path.GetFullPath(file.FilePath) == path).ToList();
                    if (!IsInside(folder, path) || mapped.Count != 1 || !File.Exists(mapped[0].OriginalSourcePath))
                    {
                        logger.LogWarning("Compiler repair skipped {File}: no unique authoritative source mapping.", group.Key);
                        continue;
                    }
                    var file = mapped[0];
                    var before = await File.ReadAllTextAsync(path, cancellationToken);
                    var related = new Dictionary<string, string>();
                    var pom = Path.Combine(folder, JavaProjectScaffold.PomFile);
                    related[pom] = await File.ReadAllTextAsync(pom, cancellationToken);
                    foreach (var peer in (generatedFiles ?? []).Where(peer => peer.FilePath != path
                        && File.Exists(peer.FilePath) && PackageOf(peer.Content) == PackageOf(before))
                        .Take(Math.Max(0, settings.MaxDeclarationsPerRepair)))
                    {
                        var code = await File.ReadAllTextAsync(peer.FilePath, cancellationToken);
                        related[peer.FilePath] = string.Join('\n', code.Split('\n')
                            .Take(Math.Max(0, settings.MaxDeclarationLines)));
                    }
                    IReadOnlyDictionary<string, string>? candidate;
                    if (!startedRound)
                    {
                        round++;
                        startedRound = true;
                    }
                    try
                    {
                        candidate = await repair(new(file.OriginalCobolFileName, "Java",
                            await File.ReadAllTextAsync(file.OriginalSourcePath, cancellationToken),
                            new Dictionary<string, string> { [path] = before },
                            new ProgramParityResult { Program = file.OriginalCobolFileName, Outcome = ParityOutcome.NotEvaluated },
                            group.ToList(), related)).WaitAsync(cancellationToken);
                    }
                    catch (Exception ex) when (ex is not OperationCanceledException)
                    {
                        logger.LogWarning(ex, "Compiler repair failed for {File}; retaining original output.", group.Key);
                        continue;
                    }
                    if (candidate is null || candidate.Count != 1 || !candidate.TryGetValue(path, out var after)
                        || string.IsNullOrWhiteSpace(after) || before == after
                        || RejectCandidate(before, after, preserveAllTypes: true) is not null)
                    {
                        logger.LogWarning("Compiler repair rejected for {File}: absent candidate or changed public identity.", group.Key);
                        continue;
                    }
                    originals[path] = before;
                    candidates[path] = after;
                }
                if (originals.Count == 0) break;
                JavaCompileResult measured;
                try
                {
                    foreach (var candidate in candidates)
                        await File.WriteAllTextAsync(candidate.Key, candidate.Value, cancellationToken);
                    measured = await BuildAsync(folder, Timeout(settings), executable, cancellationToken);
                }
                catch
                {
                    await RestoreAsync(originals);
                    throw;
                }
                if (!measured.Compiled && (!measured.Measured || measured.Errors.Count == 0
                    || measured.Errors.Count >= result.Errors.Count))
                {
                    Record(measured, rolledBack: true);
                    await RestoreAsync(originals);
                    logger.LogWarning("Java compile repair round {Round} rolled back: compiler failures did not decrease.", round);
                    result = await BuildAsync(folder, Timeout(settings), executable, cancellationToken);
                    break;
                }
                Record(measured);
                result = measured;
                foreach (var file in generatedFiles ?? [])
                    if (candidates.TryGetValue(file.FilePath, out var content)) file.Content = content;
            }
        }
        catch (OperationCanceledException)
        {
            await PersistAsync(folder, new(false, false, null, "Java validation was cancelled.", [])
                { RepairRounds = round, Rounds = rounds });
            Environment.ExitCode = FailureExitCode;
            throw;
        }
        result = result with { RepairRounds = round, Rounds = rounds };
        await PersistAsync(folder, result);
        if (!result.Compiled)
        {
            logger.LogError("Generated Java build failed: {Reason}; {Count} compiler diagnostics",
                result.FailureReason, result.Errors.Count);
            Environment.ExitCode = FailureExitCode;
        }
        return result;
    }

    private static TimeSpan Timeout(CompileGateSettings settings) =>
        TimeSpan.FromSeconds(Math.Max(1, settings.BuildTimeoutSeconds));

    private static bool IsInside(string folder, string path)
    {
        var relative = Path.GetRelativePath(Path.GetFullPath(folder), path);
        return !Path.IsPathRooted(relative) && relative != ".."
            && !relative.StartsWith(".." + Path.DirectorySeparatorChar, StringComparison.Ordinal);
    }

    private static async Task RestoreAsync(IReadOnlyDictionary<string, string> originals)
    {
        foreach (var original in originals)
            await File.WriteAllTextAsync(original.Key, original.Value, CancellationToken.None);
    }

    private static Task PersistAsync(string folder, JavaCompileResult result) =>
        File.WriteAllTextAsync(Path.Combine(folder, "java-compile-status.json"),
            JsonSerializer.Serialize(result, new JsonSerializerOptions { WriteIndented = true }), CancellationToken.None);

    internal static string? RejectCandidate(string before, string after, bool preserveAllTypes = false)
    {
        if (string.IsNullOrWhiteSpace(after)) return "empty output";
        var original = ConversionParityValidator.Tokenizer.Split(before).Code;
        var candidate = ConversionParityValidator.Tokenizer.Split(after).Code;
        if (PackageOf(before) != PackageOf(after)) return "package changed";
        var types = preserveAllTypes
            ? @"\b(?:class|interface|enum|record)\s+(\w+)"
            : @"\bpublic\s+(?:(?:abstract|final|static)\s+)*(?:class|interface|enum|record)\s+(\w+)";
        HashSet<string> Names(string code) => Regex.Matches(code, types)
            .Select(match => match.Groups[1].Value).ToHashSet(StringComparer.Ordinal);
        if (!Names(original).IsSubsetOf(Names(candidate))) return "type declarations lost";
        const string publicTypes = @"\bpublic\s+(?:(?:abstract|final|static)\s+)*(?:class|interface|enum|record)\s+(\w+)";
        var publicBefore = Regex.Matches(original, publicTypes).Select(match => match.Groups[1].Value).ToHashSet(StringComparer.Ordinal);
        var publicAfter = Regex.Matches(candidate, publicTypes).Select(match => match.Groups[1].Value).ToHashSet(StringComparer.Ordinal);
        if (!publicBefore.IsSubsetOf(publicAfter)) return "public type visibility lost";
        const string methods = @"\b(?:public|protected)\s+(?:(?:static|final|synchronized|abstract|native|default)\s+)*(?:[\w.$<>\[\]?,]+\s+)?\w+\s*\([^)]*\)";
        HashSet<string> Signatures(string code) => Regex.Matches(code, methods)
            .Select(match => Regex.Replace(match.Value, @"\s+", " ").Trim()).ToHashSet(StringComparer.Ordinal);
        if (!Signatures(original).IsSubsetOf(Signatures(candidate))) return "public or protected APIs lost";
        const string fields = @"\b(?:public|protected)\s+(?:(?:static|final|volatile|transient)\s+)*[\w.$<>\[\]?,]+\s+\w+(?=\s*[=;])";
        HashSet<string> Fields(string code) => Regex.Matches(code, fields)
            .Select(match => Regex.Replace(match.Value, @"\s+", " ").Trim()).ToHashSet(StringComparer.Ordinal);
        return Fields(original).IsSubsetOf(Fields(candidate)) ? null : "public or protected fields lost";
    }

    private static string PackageOf(string source) =>
        Regex.Match(ConversionParityValidator.Tokenizer.Split(source).Code,
            @"(?m)^\s*package\s+([\w.]+)\s*;").Groups[1].Value;

    internal static async Task<JavaCompileResult> BuildAsync(string folder, TimeSpan timeout,
        string executable = "mvn", CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();
        if (!File.Exists(Path.Combine(folder, "pom.xml")))
        {
            Directory.CreateDirectory(folder);
            await File.WriteAllTextAsync(Path.Combine(folder, "java-build.log"),
                "No pom.xml in the generated project.", cancellationToken);
            return new(false, false, null, "No pom.xml in the generated project.", []);
        }
        var arguments = new[] { "-B", "-q", "-Dstyle.color=never", "-DskipTests", "compile" };
        var commandScript = OperatingSystem.IsWindows() && !executable.EndsWith(".exe", StringComparison.OrdinalIgnoreCase);
        var start = new ProcessStartInfo(commandScript ? "cmd.exe" : executable)
        {
            WorkingDirectory = Path.GetFullPath(folder),
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false
        };
        if (commandScript)
        {
            var command = executable == "mvn" ? "mvn.cmd" : executable;
            start.Arguments = $"/d /s /c \"\"{command}\" {string.Join(' ', arguments)}\"";
        }
        else
            foreach (var argument in arguments) start.ArgumentList.Add(argument);
        Process process;
        try { process = Process.Start(start) ?? throw new InvalidOperationException("Maven did not start."); }
        catch (Exception ex) when (ex is System.ComponentModel.Win32Exception or InvalidOperationException)
        {
            await File.WriteAllTextAsync(Path.Combine(folder, "java-build.log"),
                $"Could not start Maven: {ex.Message}", cancellationToken);
            return new(false, false, null, $"Could not start Maven: {ex.Message}", []);
        }
        using (process)
        {
            var stdout = process.StandardOutput.ReadToEndAsync();
            var stderr = process.StandardError.ReadToEndAsync();
            using var timer = CancellationTokenSource.CreateLinkedTokenSource(cancellationToken);
            timer.CancelAfter(timeout);
            try { await process.WaitForExitAsync(timer.Token); }
            catch (OperationCanceledException)
            {
                try { if (!process.HasExited) process.Kill(entireProcessTree: true); }
                catch (InvalidOperationException) { }
                await process.WaitForExitAsync(CancellationToken.None);
                await Task.WhenAll(stdout, stderr);
                await File.WriteAllTextAsync(Path.Combine(folder, "java-build.log"),
                    await stdout + "\n" + await stderr, CancellationToken.None);
                cancellationToken.ThrowIfCancellationRequested();
                return new(false, false, null, $"Maven build exceeded {timeout.TotalSeconds:0} seconds.", []);
            }
            var output = await stdout + "\n" + await stderr;
            await File.WriteAllTextAsync(Path.Combine(folder, "java-build.log"), output, CancellationToken.None);
            cancellationToken.ThrowIfCancellationRequested();
            return new(process.ExitCode == 0, true, process.ExitCode,
                process.ExitCode == 0 ? null : $"Maven exited {process.ExitCode}; see java-build.log.", Parse(output, folder));
        }
    }

    internal static IReadOnlyList<CompilerDiagnostic> Parse(string output, string folder) =>
        Regex.Matches(output, @"(?m)^\[ERROR\]\s+(?<file>.+?\.java):\[(?<line>\d+),(?<column>\d+)\]\s*(?<message>.+)$")
            .Select(match => new CompilerDiagnostic(RelativeFile(match.Groups["file"].Value, folder),
                int.Parse(match.Groups["line"].Value), int.Parse(match.Groups["column"].Value),
                "javac", match.Groups["message"].Value.Trim())).Distinct().ToList();

    private static string RelativeFile(string path, string folder)
    {
        var normalized = path.Replace('\\', '/');
        return Directory.EnumerateFiles(folder, "*.java", SearchOption.AllDirectories)
            .Select(file => Path.GetRelativePath(folder, file).Replace('\\', '/'))
            .FirstOrDefault(file => normalized == file || normalized.EndsWith("/" + file, StringComparison.Ordinal))
            ?? path;
    }
}
