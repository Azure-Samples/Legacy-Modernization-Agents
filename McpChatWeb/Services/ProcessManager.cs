using System.Collections.Concurrent;
using System.Diagnostics;
using System.Text;
using System.Text.RegularExpressions;
using System.Text.Json;
using System.Text.Json.Serialization;
using CobolToQuarkusMigration.Helpers;

namespace McpChatWeb.Services;

public sealed class ManagedRunValidationException(ManagedRun run, ArgumentException cause)
    : ArgumentException(cause.Message, cause)
{
    public ManagedRun Run { get; } = run;
}

/// <summary>
/// Represents a single managed run launched from the portal.
/// Wraps a non-interactive migration CLI subprocess with start/stop/pause and durable log capture.
/// </summary>
public class ManagedRun
{
    public string RunId { get; init; } = Guid.NewGuid().ToString("N")[..8];
    public string Name { get; set; } = "";
    public string Command { get; init; } = "";
    public string TargetLanguage { get; set; } = "Java";
    public string SpeedProfile { get; set; } = "balanced";
    public string DependencyMode { get; set; } = "strict";
    public string Status { get; set; } = "pending";  // pending | running | paused | completed | failed | stopped
    public DateTime StartedAt { get; set; } = DateTime.UtcNow;
    public DateTime? CompletedAt { get; set; }
    public DateTime? RecoveredAt { get; set; }
    public int? ExitCode { get; set; }
    public int? ProcessId { get; set; }
    [JsonInclude] public string? Error { get; internal set; }
    [JsonIgnore] public string? LogPath { get; internal set; }
    [JsonInclude] public string? OutputFolder { get; internal set; }
    public string? HistoryWarning { get; set; }
    internal Action? Save { get; set; }
    // The --programs selection the run converts; empty means the whole estate.
    public IReadOnlyList<string> Programs { get; set; } = [];

    // Circular buffer for last N lines of output
    private readonly List<string> _logLines = new();
    private readonly object _logLock = new();
    private const int MaxLogLines = 2000;
    private StreamWriter? _logWriter;

    internal void OpenLog(string path)
    {
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        _logWriter = new StreamWriter(new FileStream(path, FileMode.CreateNew, FileAccess.Write, FileShare.ReadWrite))
        {
            AutoFlush = true
        };
        LogPath = path;
    }

    internal void CloseLog()
    {
        lock (_logLock)
        {
            try
            {
                _logWriter?.Dispose();
            }
            catch (IOException ex)
            {
                RecordLogFailure(ex);
            }
            finally
            {
                _logWriter = null;
            }
        }
    }

    private void RecordLogFailure(IOException ex)
    {
        Error = $"Could not persist the process log: {ex.Message}";
        _logLines.Add($"[PORTAL] ERROR: {Error}");
        if (Status == "completed") Status = "failed";
    }

    public void AppendLog(string line)
    {
        lock (_logLock)
        {
            _logLines.Add(line);
            if (_logLines.Count > MaxLogLines)
                _logLines.RemoveAt(0);
            try
            {
                _logWriter?.WriteLine(line);
            }
            catch (IOException ex)
            {
                RecordLogFailure(ex);
                CloseLog();
            }
        }
    }

    public string[] GetLogLines(int? lastN = null)
    {
        lock (_logLock)
        {
            if (_logWriter is null && LogPath is not null && File.Exists(LogPath))
            {
                try
                {
                    using var stream = new FileStream(LogPath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
                    using var reader = new StreamReader(stream);
                    var tail = new Queue<string>();
                    var limit = Math.Clamp(lastN ?? MaxLogLines, 1, MaxLogLines);
                    while (reader.ReadLine() is { } line)
                    {
                        tail.Enqueue(line);
                        if (tail.Count > limit) tail.Dequeue();
                    }
                    return tail.ToArray();
                }
                catch (IOException) { /* The buffered log remains available. */ }
            }
            if (lastN.HasValue && lastN.Value < _logLines.Count)
                return _logLines.Skip(_logLines.Count - lastN.Value).ToArray();
            return _logLines.ToArray();
        }
    }

    internal Process? Process { get; set; }
    internal Task Completion { get; set; } = Task.CompletedTask;
}

/// <summary>
/// Manages migration CLI subprocesses launched from the portal.
/// Provides start/stop/pause/status for migration runs.
/// </summary>
public class ProcessManager : IDisposable
{
    private readonly ConcurrentDictionary<string, ManagedRun> _runs = new();
    private readonly string _repoRoot;
    private readonly string _dotnetExecutable;
    private readonly string? _cliAssembly;
    private DependencyReadinessCache? _readinessCache;
    private static readonly object OutputFolderLock = new();

    private static readonly string[] AllowedCommands = { "migrate", "run", "full", "reverse-engineer", "reverse", "re", "convert-only", "convert", "resume" };
    private static readonly Regex SafePathPattern = new(@"^[a-zA-Z0-9_\-./]+$", RegexOptions.Compiled);
    // A program selector is a file name or source-relative path; COBOL names may carry # @ $.
    private static readonly Regex ProgramSelectorPattern = new(@"^[A-Za-z0-9#@$_\-./]+$", RegexOptions.Compiled);
    private static readonly string[] CommandsTakingPrograms = { "migrate", "run", "full", "convert-only", "convert" };

    public ProcessManager(string repoRoot) : this(repoRoot, "dotnet") { }

    public async Task<DependencyReadiness> AssessDependenciesAsync(
        string? sourceFolder = null, string? factsFolder = null, IReadOnlyList<string>? programs = null,
        CancellationToken ct = default)
    {
        var sourceRoot = ResolveSourceRoot(sourceFolder);
        var readiness = await DependencyReadiness.AssessAsync(sourceRoot, ResolveFactsRoot(factsFolder),
            programs is null ? null : ValidatePrograms("convert-only", programs), ct);
        return WithSourceFolderId(readiness, sourceRoot);
    }

    // Whole-estate readiness for display only; launches call AssessDependenciesAsync for a fresh check.
    // refresh discards the cached assessment, so an explicit refresh always re-reads every input.
    public async Task<DependencyReadiness> GetEstateReadinessAsync(
        string? sourceFolder = null, CancellationToken ct = default, bool refresh = false)
    {
        var sourceRoot = ResolveSourceRoot(sourceFolder);
        var factsRoot = ResolveFactsRoot(null);
        _readinessCache ??= new DependencyReadinessCache([Path.Combine(_repoRoot, "tools")]);
        if (refresh) _readinessCache.Invalidate(sourceRoot, factsRoot);
        var readiness = await _readinessCache.GetAsync(sourceRoot, factsRoot,
            token => DependencyReadiness.AssessAsync(sourceRoot, factsRoot, null, token), ct,
            DependencyReadiness.ReferencedInputs);
        return WithSourceFolderId(readiness, sourceRoot);
    }

    private DependencyReadiness WithSourceFolderId(DependencyReadiness readiness, string sourceRoot) =>
        readiness with { SourceFolderId = Path.GetRelativePath(_repoRoot, sourceRoot).Replace('\\', '/') };

    public static bool RequiresDependencyEvidence(string? command) =>
        command?.ToLowerInvariant() is "migrate" or "run" or "full" or "convert-only" or "convert"
            or "reverse-engineer" or "reverse" or "re";

    internal ProcessManager(string repoRoot, string dotnetExecutable, string? cliAssembly = null)
    {
        _repoRoot = Path.GetFullPath(repoRoot);
        _dotnetExecutable = dotnetExecutable;
        _cliAssembly = cliAssembly ?? Environment.GetEnvironmentVariable("MIGRATION_CLI_ASSEMBLY");
        if (_cliAssembly is not null && (!Path.IsPathRooted(_cliAssembly)
            || !File.Exists(_cliAssembly) || Path.GetExtension(_cliAssembly) != ".dll"))
            throw new ArgumentException("MIGRATION_CLI_ASSEMBLY must name an existing absolute .dll path; no source-build fallback is allowed.");
        RestoreRuns();
    }

    /// <summary>
    /// Start a migration command as a managed subprocess.
    /// </summary>
    public ManagedRun StartRun(
        string command,
        string name,
        string targetLanguage = "Java",
        string speedProfile = "balanced",
        string? sourceFolder = null,
        string? provider = null,
        string? modelId = null,
        Dictionary<string, string>? extraEnv = null,
        IReadOnlyList<string>? programs = null)
    {
        var run = new ManagedRun
        {
            Command = AllowedCommands.Contains(command?.ToLowerInvariant()) ? command! : "invalid",
            Name = string.IsNullOrWhiteSpace(name) ? $"launch-{DateTime.UtcNow:HHmmss}" : name,
            TargetLanguage = targetLanguage,
            SpeedProfile = speedProfile,
            DependencyMode = extraEnv?.GetValueOrDefault("CONVERSION_DEPENDENCY_MODE") ?? "strict",
        };
        _runs[run.RunId] = run;
        run.Save = () => PersistRun(run);
        // Persist before validation or spawning: rejected and preflight attempts are history too.
        if (!PersistRun(run))
        {
            run.Status = "failed";
            run.Error = "Launch refused because managed history could not be persisted. Restore write access to Logs/portal-history before retrying.";
            run.CompletedAt = DateTime.UtcNow;
            return run;
        }
        try
        {
            return StartValidatedRun(run, command!, targetLanguage, speedProfile, sourceFolder,
                provider, modelId, extraEnv, programs);
        }
        catch (ArgumentException ex)
        {
            run.Status = "failed";
            run.Error = ex.Message;
            run.CompletedAt = DateTime.UtcNow;
            run.AppendLog($"[PORTAL] Validation failed: {ex.Message}");
            PersistRun(run);
            throw new ManagedRunValidationException(run, ex);
        }
    }

    public static void ValidateLaunchSettings(string command, string targetLanguage,
        string speedProfile, string? provider = null, IReadOnlyList<string>? programs = null)
    {
        if (string.IsNullOrWhiteSpace(command) || !AllowedCommands.Contains(command.ToLowerInvariant()))
            throw new ArgumentException($"Invalid command. Allowed: {string.Join(", ", AllowedCommands)}");
        if (targetLanguage is not ("Java" or "CSharp"))
            throw new ArgumentException("Target language must be Java or CSharp.");
        if (speedProfile is not ("balanced" or "fast" or "turbo" or "thorough"))
            throw new ArgumentException("Speed profile must be balanced, fast, turbo or thorough.");
        ValidatePrograms(command, programs);
        NormalizeProvider(provider);
    }

    private ManagedRun StartValidatedRun(ManagedRun run, string command, string targetLanguage,
        string speedProfile, string? sourceFolder, string? provider, string? modelId,
        Dictionary<string, string>? extraEnv, IReadOnlyList<string>? programs)
    {
        ValidateLaunchSettings(command, targetLanguage, speedProfile, provider, programs);
        var selection = ValidatePrograms(command, programs);
        var serviceType = NormalizeProvider(provider);

        ResolveSourceRoot(sourceFolder);
        if (extraEnv?.TryGetValue("JAVA_FRAMEWORK", out var framework) == true)
            JavaGeneration.ValidateFramework(framework);
        if (extraEnv?.TryGetValue("REKT_FACTS_FOLDER", out var factsFolder) == true)
            ResolveFactsRoot(factsFolder);

        run.Programs = selection;

        // Build the dotnet command directly instead of going through doctor.sh
        // (doctor.sh is interactive — we bypass it for non-interactive portal use)
        var (executable, arguments) = BuildCommand(command, targetLanguage, speedProfile, sourceFolder, selection);

        var psi = new ProcessStartInfo
        {
            FileName = executable,
            WorkingDirectory = _repoRoot,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };

        if (!OperatingSystem.IsWindows())
        {
            // A sh parent lets pause stop every .NET process in the run without stopping the
            // portal's own child (see ProcessTreeSuspender). Arguments pass through "$@" untouched.
            psi.FileName = "/bin/sh";
            psi.ArgumentList.Add("-c");
            psi.ArgumentList.Add("\"$@\"; exit $?");
            psi.ArgumentList.Add("migration-run");
            psi.ArgumentList.Add(executable);
        }

        foreach (var argument in arguments)
        {
            psi.ArgumentList.Add(argument);
        }

        // Set environment variables
        psi.Environment["TARGET_LANGUAGE"] = targetLanguage;
        psi.Environment["MIGRATION_DB_PATH"] = Path.Combine(_repoRoot, "Data", "migration.db");
        psi.Environment["COBOL_SOURCE_FOLDER"] = sourceFolder ?? "source";

        // Speed profile env vars
        ApplySpeedProfile(psi.Environment, speedProfile);

        // Extra env vars
        if (extraEnv != null)
        {
            foreach (var (k, v) in extraEnv)
                psi.Environment[k] = v;
        }
        psi.Environment["AI_LOOP_MANAGED_RUN_ID"] = run.RunId;

        // ── Apply provider/model selection from portal UI ──
        // The portal's own environment (set by ./doctor.sh setup or the setup modal) is the
        // default; an explicit provider from the UI overrides only the service type.
        if (serviceType is not null)
            psi.Environment["AZURE_OPENAI_SERVICE_TYPE"] = serviceType;

        // Only an explicit UI selection overrides per-agent models. Otherwise let the CLI
        // resolve inherited environment, local settings and appsettings without inventing a model.
        if (!string.IsNullOrWhiteSpace(modelId))
        {
            foreach (var key in new[]
            {
                "AZURE_OPENAI_MODEL_ID", "AZURE_OPENAI_DEPLOYMENT_NAME",
                "AZURE_OPENAI_COBOL_ANALYZER_MODEL", "AZURE_OPENAI_JAVA_CONVERTER_MODEL",
                "AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL", "AZURE_OPENAI_UNIT_TEST_MODEL",
                "AZURE_OPENAI_CHAT_MODEL_ID", "AZURE_OPENAI_CHAT_DEPLOYMENT_NAME",
                "AISETTINGS__MODELID", "AISETTINGS__DEPLOYMENTNAME",
                "AISETTINGS__COBOLANALYZERMODELID", "AISETTINGS__JAVACONVERTERMODELID",
                "AISETTINGS__DEPENDENCYMAPPERMODELID", "AISETTINGS__UNITTESTMODELID",
                "AISETTINGS__CHATMODELID", "AISETTINGS__CHATDEPLOYMENTNAME"
            })
                psi.Environment[key] = modelId.Trim();
        }

        _runs[run.RunId] = run;
        try
        {
            run.OpenLog(Path.Combine(_repoRoot, "Logs", "portal", $"{run.RunId}.log"));
            run.OutputFolder = ReserveOutputFolder(targetLanguage);
            psi.Environment[targetLanguage == "CSharp" ? "CSHARP_OUTPUT_FOLDER" : "JAVA_OUTPUT_FOLDER"] = run.OutputFolder;
            run.AppendLog($"[PORTAL] Starting managed run {run.RunId}: {executable} {string.Join(" ", arguments)}");
            run.AppendLog($"[PORTAL] Output: {run.OutputFolder}");
            if (!PersistRun(run))
                throw new IOException("Managed history could not be persisted before process launch.");
            var process = Process.Start(psi);
            if (process == null)
                throw new InvalidOperationException("Failed to start process.");

            run.Process = process;
            run.ProcessId = process.Id;
            run.Status = "running";
            if (!PersistRun(run))
                throw new IOException("Managed history could not be persisted after process launch.");
            run.AppendLog($"[PORTAL] PID: {process.Id} | Target: {targetLanguage} | Speed: {speedProfile}");
            run.Completion = MonitorProcessAsync(run,
                CaptureOutputAsync(process.StandardOutput, run),
                CaptureOutputAsync(process.StandardError, run));

            Console.WriteLine($"🚀 Run '{run.Name}' started (PID: {process.Id}, command: {command})");
        }
        catch (Exception ex) when (ex is System.ComponentModel.Win32Exception or IOException
            or UnauthorizedAccessException or InvalidOperationException)
        {
            if (run.Process is { HasExited: false } child)
                child.Kill(entireProcessTree: true);
            run.Status = "failed";
            run.CompletedAt = DateTime.UtcNow;
            run.Error = ex.Message;
            run.AppendLog($"ERROR: {ex.Message}");
            run.CloseLog();
            PersistRun(run);
        }
        return run;
    }

    private string ResolveSourceRoot(string? sourceFolder)
    {
        if (sourceFolder != null)
        {
            if (sourceFolder.Contains('\0'))
                throw new ArgumentException("Source folder contains invalid characters.");
            if (sourceFolder.Contains(".."))
                throw new ArgumentException("Source folder must not contain path traversal sequences.");
            if (!SafePathPattern.IsMatch(sourceFolder))
                throw new ArgumentException("Source folder contains disallowed characters. Only alphanumeric, dash, underscore, dot, and forward slash are permitted.");
        }
        // Return the exact path that passed the containment check, never a recomputed one.
        var resolvedPath = Path.GetFullPath(Path.Combine(_repoRoot, sourceFolder ?? "source"));
        if (resolvedPath == _repoRoot)
            return _repoRoot;
        if (!resolvedPath.StartsWith(_repoRoot + Path.DirectorySeparatorChar, StringComparison.Ordinal))
            throw new ArgumentException("Source folder must resolve to a path within the repository root.");
        if (!Directory.Exists(resolvedPath))
            throw new ArgumentException("Source folder does not exist.");
        return resolvedPath;
    }

    private string ResolveFactsRoot(string? factsFolder)
    {
        if (factsFolder is not null)
        {
            if (string.IsNullOrWhiteSpace(factsFolder) || Path.IsPathRooted(factsFolder)
                || !SafePathPattern.IsMatch(factsFolder) || factsFolder.Split('/').Any(p => p is ".." or "."))
                throw new ArgumentException("REKT facts folder must be a source-independent repository-relative artifact directory.");
            var directory = Path.GetFullPath(Path.Combine(_repoRoot, factsFolder));
            if (!directory.StartsWith(_repoRoot + Path.DirectorySeparatorChar, StringComparison.Ordinal)
                || !Directory.Exists(directory))
                throw new ArgumentException("REKT facts folder must exist inside this repository.");
            for (var current = new DirectoryInfo(directory); current.FullName != _repoRoot; current = current.Parent!)
                if ((current.Attributes & FileAttributes.ReparsePoint) != 0)
                    throw new ArgumentException("Linked REKT facts folders cannot establish a run's artifact binding.");
            return directory;
        }
        return ConversionEligibility.FactsDirectory(_repoRoot);
    }

    private string ReserveOutputFolder(string targetLanguage)
    {
        // Keep the existing reader-compatible timestamp layout, but reserve it before launch:
        // two clicks within one second must never share generated files or validation evidence.
        lock (OutputFolderLock)
        {
            var when = DateTimeOffset.Now;
            while (true)
            {
                var relative = $"output/{(targetLanguage == "CSharp" ? "csharp" : "java")}/{ConversionOutputFolder.NewRunName(when)}";
                var absolute = Path.Combine(_repoRoot, relative);
                if (!Directory.Exists(absolute) && !File.Exists(absolute))
                {
                    Directory.CreateDirectory(absolute);
                    return relative;
                }
                when = when.AddSeconds(1);
            }
        }
    }

    /// <summary>
    /// Stop (kill) a running process.
    /// </summary>
    public bool StopRun(string runId)
    {
        if (!_runs.TryGetValue(runId, out var run)) return false;
        if (run.Process == null || run.Process.HasExited) return false;

        try
        {
            run.Process.Kill(entireProcessTree: true);
            run.Status = "stopped";
            run.CompletedAt = DateTime.UtcNow;
            run.AppendLog("[PORTAL] Process stopped by user");
            run.Error = "Stopped by user. Process termination does not establish conversion or verification success.";
            PersistRun(run);
            Console.WriteLine($"🛑 Run '{run.Name}' stopped");
            return true;
        }
        catch (Exception ex)
        {
            run.AppendLog($"[PORTAL] Failed to stop: {ex.Message}");
            return false;
        }
    }

    /// <summary>
    /// Pause a running process and the processes it started.
    /// </summary>
    public bool PauseRun(string runId)
    {
        if (!_runs.TryGetValue(runId, out var run)) return false;
        if (run.Process == null || run.Process.HasExited) return false;
        if (run.Status == "paused") return true;

        try
        {
            ProcessTreeSuspender.Suspend(run.Process.Id, includeRoot: OperatingSystem.IsWindows());
            run.Status = "paused";
            PersistRun(run);
            run.AppendLog("[PORTAL] Process paused by user");
            Console.WriteLine($"⏸️ Run '{run.Name}' paused");
            return true;
        }
        catch (Exception ex)
        {
            run.AppendLog($"[PORTAL] Failed to pause: {ex.Message}");
            return false;
        }
    }

    /// <summary>
    /// Resume a paused process and the processes it started.
    /// </summary>
    public bool ResumeRun(string runId)
    {
        if (!_runs.TryGetValue(runId, out var run)) return false;
        if (run.Process == null || run.Process.HasExited) return false;
        if (run.Status != "paused") return false;

        try
        {
            ProcessTreeSuspender.Resume(run.Process.Id, includeRoot: OperatingSystem.IsWindows());
            run.Status = "running";
            PersistRun(run);
            run.AppendLog("[PORTAL] Process resumed by user");
            Console.WriteLine($"▶️ Run '{run.Name}' resumed");
            return true;
        }
        catch (Exception ex)
        {
            run.AppendLog($"[PORTAL] Failed to resume: {ex.Message}");
            return false;
        }
    }

    /// <summary>
    /// Get all managed runs.
    /// </summary>
    public IReadOnlyCollection<ManagedRun> GetAllRuns()
    {
        return _runs.Values.OrderByDescending(r => r.StartedAt).ToList();
    }

    /// <summary>
    /// Get a specific run.
    /// </summary>
    public ManagedRun? GetRun(string runId)
    {
        return _runs.TryGetValue(runId, out var run) ? run : null;
    }

    public ManagedRun RecordRejectedLaunch(string name, string reason, string targetLanguage = "Java",
        string speedProfile = "balanced", string status = "failed")
    {
        var run = new ManagedRun
        {
            Name = name,
            Command = "convert-only",
            TargetLanguage = targetLanguage,
            SpeedProfile = speedProfile,
            Status = status,
            Error = reason,
            CompletedAt = DateTime.UtcNow
        };
        _runs[run.RunId] = run;
        PersistRun(run);
        return run;
    }

    // Selectors reach the CLI as one --programs argument, so each must be a plain name or path:
    // no separators the CLI would split on, no option-looking values, nothing outside the source.
    public static IReadOnlyList<string> ValidatePrograms(string command, IReadOnlyList<string>? programs)
    {
        if (programs is null || programs.Count == 0) return [];
        if (!CommandsTakingPrograms.Contains(command.ToLowerInvariant()))
            throw new ArgumentException($"Command '{command}' does not take a program selection.");

        var selection = new List<string>();
        foreach (var raw in programs)
        {
            var p = raw?.Trim() ?? "";
            if (p.Length == 0)
                throw new ArgumentException("Program selectors must not be empty.");
            if (p.Length > 256 || !ProgramSelectorPattern.IsMatch(p) || p.StartsWith('-') || p.StartsWith('/')
                || p.Split('/').Any(seg => seg is ".." or "."))
                throw new ArgumentException("Invalid program selector. Use a file name or a source-relative path.");
            if (!selection.Contains(p, StringComparer.OrdinalIgnoreCase)) selection.Add(p);
        }
        return selection;
    }

    private (string executable, string[] arguments) BuildCommand(
        string command, string targetLang, string speedProfile, string? sourceFolder,
        IReadOnlyList<string>? programs = null)
    {
        var (executable, arguments) = BaseCommand(command, sourceFolder);
        return programs is { Count: > 0 }
            ? (executable, [.. arguments, "--programs", string.Join(',', programs)])
            : (executable, arguments);
    }

    private (string executable, string[] arguments) BaseCommand(string command, string? sourceFolder)
    {
        var dotnet = _dotnetExecutable;
        var source = Path.GetFullPath(Path.Combine(_repoRoot, sourceFolder ?? "source"));
        var project = Path.Combine(_repoRoot, "CobolToQuarkusMigration.csproj");

        string[] arguments = command.ToLowerInvariant() switch
        {
            "migrate" or "run" or "full" =>
                ["--source", source],

            "reverse-engineer" or "reverse" or "re" =>
                ["reverse-engineer", "--source", source, "--output", "output"],

            "convert-only" or "convert" =>
                ["--source", source, "--skip-reverse-engineering"],

            "resume" =>
                ["--source", source, "--resume"],

            _ => ["--source", source]
        };
        return _cliAssembly is not null ? (dotnet, [_cliAssembly, .. arguments])
            : (dotnet, ["run", "--project", project, "--", .. arguments]);
    }

    private static void ApplySpeedProfile(IDictionary<string, string?> env, string profile)
    {
        switch (profile.ToLowerInvariant())
        {
            case "turbo":
                env["AI_LOW_REASONING_EFFORT"] = "low";
                env["AI_MEDIUM_REASONING_EFFORT"] = "low";
                env["AI_HIGH_REASONING_EFFORT"] = "low";
                env["AI_MAX_OUTPUT_TOKENS"] = "65000";
                env["AI_MAX_PARALLEL_CONVERSION"] = "4";
                env["AI_STAGGER_DELAY_MS"] = "200";
                env["AI_RATE_LIMIT_SAFETY_FACTOR"] = "0.85";
                break;
            case "fast":
                env["AI_LOW_REASONING_EFFORT"] = "low";
                env["AI_MEDIUM_REASONING_EFFORT"] = "low";
                env["AI_HIGH_REASONING_EFFORT"] = "medium";
                env["AI_MAX_OUTPUT_TOKENS"] = "32768";
                env["AI_MAX_PARALLEL_CONVERSION"] = "3";
                env["AI_STAGGER_DELAY_MS"] = "500";
                break;
            case "thorough":
                env["AI_LOW_REASONING_EFFORT"] = "high";
                env["AI_MEDIUM_REASONING_EFFORT"] = "high";
                env["AI_HIGH_REASONING_EFFORT"] = "high";
                env["AI_MAX_PARALLEL_CONVERSION"] = "2";
                env["AI_STAGGER_DELAY_MS"] = "1500";
                break;
            default: // balanced
                env["AI_MAX_PARALLEL_CONVERSION"] = "2";
                env["AI_STAGGER_DELAY_MS"] = "1000";
                break;
        }
    }

    private static async Task CaptureOutputAsync(StreamReader reader, ManagedRun run)
    {
        string? line;
        while ((line = await reader.ReadLineAsync()) != null)
            run.AppendLog(line);
    }

    private static async Task MonitorProcessAsync(ManagedRun run, Task stdout, Task stderr)
    {
        if (run.Process == null) return;

        try
        {
            await run.Process.WaitForExitAsync();
            await Task.WhenAll(stdout, stderr);
            run.ExitCode = run.Process.ExitCode;
            run.CompletedAt = DateTime.UtcNow;

            if (run.Status is "running" or "paused")
            {
                run.Status = run.Process.ExitCode == 0 && run.Error is null
                    ? run.DependencyMode is "draft" or "partial" ? "partial" : "completed" : "failed";
            }

            if (run.Process.ExitCode != 0 && run.Status is not ("stopped" or "interrupted"))
                run.Error = $"Process exited with code {run.Process.ExitCode}. See the process log for details.";
            run.AppendLog($"[PORTAL] Process exited with code {run.Process.ExitCode}");
            Console.WriteLine($"✅ Run '{run.Name}' finished (exit: {run.Process.ExitCode})");
        }
        catch (Exception ex) when (ex is IOException or InvalidOperationException)
        {
            run.Status = "failed";
            run.CompletedAt = DateTime.UtcNow;
            run.Error = ex.Message;
            run.AppendLog($"[PORTAL] Monitor error: {ex.Message}");
        }
        finally
        {
            run.CloseLog();
            run.Save?.Invoke();
        }
    }

    public void Dispose()
    {
        _readinessCache?.Dispose();
        foreach (var run in _runs.Values)
        {
            if (run.Process != null && !run.Process.HasExited)
            {
                try
                {
                    run.Status = "interrupted";
                    run.Error = "Portal shut down while this process was active. Completion was not verified.";
                    run.CompletedAt = DateTime.UtcNow;
                    run.Process.Kill(entireProcessTree: true);
                    PersistRun(run);
                }
                catch (InvalidOperationException) { }
            }
        }
    }

    private readonly object _historyLock = new();
    private string HistoryFolder => Path.Combine(_repoRoot, "Logs", "portal-history");

    private bool PersistRun(ManagedRun run)
    {
        lock (_historyLock)
        {
            try
            {
                Directory.CreateDirectory(HistoryFolder);
                var path = Path.Combine(HistoryFolder, run.RunId + ".json");
                File.WriteAllText(path + ".pending", JsonSerializer.Serialize(run));
                File.Move(path + ".pending", path, overwrite: true);
                return true;
            }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException)
            {
                run.HistoryWarning = "Managed history could not be saved. This displayed update may not survive restart; restore write access to Logs/portal-history.";
                return false;
            }
        }
    }

    private void RestoreRuns()
    {
        if (Directory.Exists(HistoryFolder))
            foreach (var file in Directory.EnumerateFiles(HistoryFolder, "*.json"))
            {
                try
                {
                    var run = JsonSerializer.Deserialize<ManagedRun>(File.ReadAllText(file));
                    if (run is null || !AiLoopReader.IsValidRunId(run.RunId)) continue;
                    // Never signal a reused PID or claim a previous portal's child is still managed.
                    run.ProcessId = null;
                    run.LogPath = File.Exists(Path.Combine(_repoRoot, "Logs", "portal", run.RunId + ".log"))
                        ? Path.Combine(_repoRoot, "Logs", "portal", run.RunId + ".log") : null;
                    if (run.Status is "pending" or "running" or "paused")
                    {
                        run.Status = "interrupted";
                        run.Error = "Portal restarted without a recorded process completion. The previous child may still be running; inspect its artifacts before relaunching.";
                        run.CompletedAt = null;
                        run.RecoveredAt = DateTime.UtcNow;
                        run.HistoryWarning = $"Process exit time was not recorded; recovery detection time: {run.RecoveredAt:O}.";
                        PersistRun(run);
                    }
                    run.Save = () => PersistRun(run);
                    _runs[run.RunId] = run;
                }
                catch (Exception ex) when (ex is IOException or JsonException)
                {
                    Console.Error.WriteLine($"Could not read managed history {Path.GetFileName(file)}: {ex.GetType().Name}");
                }
            }
        var logs = Path.Combine(_repoRoot, "Logs", "portal");
        if (!Directory.Exists(logs)) return;
        foreach (var file in Directory.EnumerateFiles(logs, "*.log"))
        {
            var id = Path.GetFileNameWithoutExtension(file);
            if (!AiLoopReader.IsValidRunId(id) || _runs.ContainsKey(id)) continue;
            _runs[id] = new ManagedRun
            {
                RunId = id,
                Name = "Historical process log",
                Status = "unknown",
                StartedAt = File.GetCreationTimeUtc(file),
                LogPath = file,
                HistoryWarning = "Only the historical log is available. Timestamp is log creation time; launch settings, process outcome and migration association were not recorded."
            };
        }
    }

    /// <summary>
    /// Maps a portal provider value to AZURE_OPENAI_SERVICE_TYPE, or null to keep the portal's
    /// configured provider. There is no GitHub Models client, so "GitHubModels" is rejected
    /// instead of silently running on the Copilot SDK.
    /// </summary>
    public static string? NormalizeProvider(string? provider)
    {
        if (string.IsNullOrWhiteSpace(provider))
            return null;

        return provider.Trim().ToLowerInvariant() switch
        {
            "azureopenai" => "AzureOpenAI",
            "githubcopilot" or "githubcopilotsdk" or "copilotsdk" => "GitHubCopilot",
            "openai" => "OpenAI",
            _ => throw new ArgumentException(
                "Unsupported provider. Use AzureOpenAI, GitHubCopilot or OpenAI.")
        };
    }
}
