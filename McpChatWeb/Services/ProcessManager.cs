using System.Collections.Concurrent;
using System.Diagnostics;
using System.Text;
using System.Text.RegularExpressions;

namespace McpChatWeb.Services;

/// <summary>
/// Represents a single managed run launched from the portal.
/// Wraps a doctor.sh subprocess with start/stop/pause and live log capture.
/// </summary>
public class ManagedRun
{
    public string RunId { get; init; } = Guid.NewGuid().ToString("N")[..8];
    public string Name { get; set; } = "";
    public string Command { get; init; } = "";
    public string TargetLanguage { get; set; } = "Java";
    public string SpeedProfile { get; set; } = "balanced";
    public string Status { get; set; } = "pending";  // pending | running | paused | completed | failed | stopped
    public DateTime StartedAt { get; set; } = DateTime.UtcNow;
    public DateTime? CompletedAt { get; set; }
    public int? ExitCode { get; set; }
    public int? ProcessId { get; set; }

    // Circular buffer for last N lines of output
    private readonly List<string> _logLines = new();
    private readonly object _logLock = new();
    private const int MaxLogLines = 2000;

    public void AppendLog(string line)
    {
        lock (_logLock)
        {
            _logLines.Add(line);
            if (_logLines.Count > MaxLogLines)
                _logLines.RemoveAt(0);
        }
    }

    public string[] GetLogLines(int? lastN = null)
    {
        lock (_logLock)
        {
            if (lastN.HasValue && lastN.Value < _logLines.Count)
                return _logLines.Skip(_logLines.Count - lastN.Value).ToArray();
            return _logLines.ToArray();
        }
    }

    internal Process? Process { get; set; }
}

/// <summary>
/// Manages doctor.sh subprocesses launched from the portal.
/// Provides start/stop/pause/status for migration runs.
/// </summary>
public class ProcessManager : IDisposable
{
    private readonly ConcurrentDictionary<string, ManagedRun> _runs = new();
    private readonly string _repoRoot;
    private readonly string _doctorShPath;

    private static readonly string[] AllowedCommands = { "migrate", "run", "full", "reverse-engineer", "reverse", "re", "convert-only", "convert", "resume" };
    private static readonly Regex SafePathPattern = new(@"^[a-zA-Z0-9_\-./]+$", RegexOptions.Compiled);

    public ProcessManager(string repoRoot)
    {
        _repoRoot = Path.GetFullPath(repoRoot);
        _doctorShPath = Path.Combine(_repoRoot, "doctor.sh");
    }

    /// <summary>
    /// Start a doctor.sh command as a managed subprocess.
    /// </summary>
    public ManagedRun StartRun(
        string command,
        string name,
        string targetLanguage = "Java",
        string speedProfile = "balanced",
        string? sourceFolder = null,
        string provider = "AzureOpenAI",
        string? modelId = null,
        Dictionary<string, string>? extraEnv = null)
    {
        // Validate command against allowlist
        if (!AllowedCommands.Contains(command.ToLowerInvariant()))
            throw new ArgumentException($"Invalid command: '{command}'. Allowed: {string.Join(", ", AllowedCommands)}");

        // Validate sourceFolder to prevent path traversal and argument injection
        if (sourceFolder != null)
        {
            if (sourceFolder.Contains('\0'))
                throw new ArgumentException("Source folder contains invalid characters.");
            if (sourceFolder.Contains(".."))
                throw new ArgumentException("Source folder must not contain path traversal sequences.");
            if (!SafePathPattern.IsMatch(sourceFolder))
                throw new ArgumentException("Source folder contains disallowed characters. Only alphanumeric, dash, underscore, dot, and forward slash are permitted.");

            var resolvedPath = Path.GetFullPath(Path.Combine(_repoRoot, sourceFolder));
            if (!resolvedPath.StartsWith(_repoRoot + Path.DirectorySeparatorChar) && resolvedPath != _repoRoot)
                throw new ArgumentException("Source folder must resolve to a path within the repository root.");
        }

        var run = new ManagedRun
        {
            Command = command,
            Name = string.IsNullOrWhiteSpace(name) ? $"{command}-{DateTime.Now:HHmmss}" : name,
            TargetLanguage = targetLanguage,
            SpeedProfile = speedProfile,
            Status = "running",
            StartedAt = DateTime.UtcNow
        };

        // Build the dotnet command directly instead of going through doctor.sh
        // (doctor.sh is interactive — we bypass it for non-interactive portal use)
        var (executable, arguments) = BuildCommand(command, targetLanguage, speedProfile, sourceFolder);

        var psi = new ProcessStartInfo
        {
            FileName = executable,
            Arguments = arguments,
            WorkingDirectory = _repoRoot,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };

        // Set environment variables
        psi.Environment["TARGET_LANGUAGE"] = targetLanguage;
        psi.Environment["MIGRATION_DB_PATH"] = Path.Combine(_repoRoot, "Data", "migration.db");
        psi.Environment["COBOL_SOURCE_FOLDER"] = sourceFolder ?? "source";

        if (targetLanguage.Equals("CSharp", StringComparison.OrdinalIgnoreCase))
            psi.Environment["CSHARP_OUTPUT_FOLDER"] = "output/csharp";
        else
            psi.Environment["JAVA_OUTPUT_FOLDER"] = "output/java";

        // Speed profile env vars
        ApplySpeedProfile(psi.Environment, speedProfile);

        // Extra env vars
        if (extraEnv != null)
        {
            foreach (var (k, v) in extraEnv)
                psi.Environment[k] = v;
        }

        // ── Apply provider/model selection from portal UI ──
        var effectiveModel = modelId ?? Environment.GetEnvironmentVariable("AZURE_OPENAI_MODEL_ID") ?? "gpt-5.1-codex-mini";

        switch (provider)
        {
            case "GitHubModels":
            {
                var ghToken = Environment.GetEnvironmentVariable("GITHUB_TOKEN") ?? "";
                // Try gh auth token if no env var
                if (string.IsNullOrEmpty(ghToken))
                {
                    try
                    {
                        var proc = Process.Start(new ProcessStartInfo("gh", "auth token")
                        {
                            RedirectStandardOutput = true, UseShellExecute = false, CreateNoWindow = true
                        });
                        ghToken = proc?.StandardOutput.ReadToEnd().Trim() ?? "";
                        proc?.WaitForExit(5000);
                    }
                    catch { /* gh not available */ }
                }

                if (string.IsNullOrEmpty(ghToken))
                {
                    run.Status = "failed";
                    run.AppendLog("ERROR: No GitHub token found. Set GITHUB_TOKEN env var or run 'gh auth login'.");
                    _runs[run.RunId] = run;
                    return run;
                }

                psi.Environment["AZURE_OPENAI_SERVICE_TYPE"] = "GitHubCopilot";
                psi.Environment["AZURE_OPENAI_ENDPOINT"] = "https://models.github.ai/inference";
                psi.Environment["AZURE_OPENAI_API_KEY"] = ghToken;
                psi.Environment["GITHUB_TOKEN"] = ghToken;
                psi.Environment["AZURE_OPENAI_CHAT_API_KEY"] = ghToken;
                break;
            }

            case "CopilotSDK":
            {
                psi.Environment["AZURE_OPENAI_SERVICE_TYPE"] = "GitHubCopilotSDK";
                // Force sequential — Copilot SDK stdio deadlocks with concurrent sessions
                psi.Environment["AI_MAX_PARALLEL_CONVERSION"] = "1";
                psi.Environment["AI_MAX_PARALLEL_ANALYSIS"] = "1";
                psi.Environment["AI_MAX_PARALLEL_CHUNKS"] = "1";
                break;
            }

            default: // AzureOpenAI
            {
                // Propagate existing Azure env vars
                foreach (var key in new[] {
                    "AZURE_OPENAI_ENDPOINT", "AZURE_OPENAI_API_KEY",
                    "AZURE_OPENAI_SERVICE_TYPE",
                    "AZURE_OPENAI_CHAT_ENDPOINT", "AZURE_OPENAI_CHAT_API_KEY" })
                {
                    var val = Environment.GetEnvironmentVariable(key);
                    if (!string.IsNullOrEmpty(val))
                        psi.Environment[key] = val;
                }
                break;
            }
        }

        // Set model for ALL agents
        psi.Environment["AZURE_OPENAI_MODEL_ID"] = effectiveModel;
        psi.Environment["AZURE_OPENAI_DEPLOYMENT_NAME"] = effectiveModel;
        psi.Environment["AZURE_OPENAI_COBOL_ANALYZER_MODEL"] = effectiveModel;
        psi.Environment["AZURE_OPENAI_JAVA_CONVERTER_MODEL"] = effectiveModel;
        psi.Environment["AZURE_OPENAI_DEPENDENCY_MAPPER_MODEL"] = effectiveModel;
        psi.Environment["AZURE_OPENAI_UNIT_TEST_MODEL"] = effectiveModel;
        psi.Environment["AZURE_OPENAI_CHAT_MODEL_ID"] = effectiveModel;
        psi.Environment["AZURE_OPENAI_CHAT_DEPLOYMENT_NAME"] = effectiveModel;

        try
        {
            var process = Process.Start(psi);
            if (process == null)
            {
                run.Status = "failed";
                run.AppendLog("ERROR: Failed to start process");
                _runs[run.RunId] = run;
                return run;
            }

            run.Process = process;
            run.ProcessId = process.Id;
            _runs[run.RunId] = run;

            // Capture stdout/stderr asynchronously
            _ = CaptureOutputAsync(process.StandardOutput, run);
            _ = CaptureOutputAsync(process.StandardError, run);

            // Monitor completion
            _ = MonitorProcessAsync(run);

            run.AppendLog($"[PORTAL] Started: {executable} {arguments}");
            run.AppendLog($"[PORTAL] PID: {process.Id} | Target: {targetLanguage} | Speed: {speedProfile}");

            Console.WriteLine($"🚀 Run '{run.Name}' started (PID: {process.Id}, command: {command})");
        }
        catch (Exception ex)
        {
            run.Status = "failed";
            run.AppendLog($"ERROR: {ex.Message}");
            _runs[run.RunId] = run;
        }

        return run;
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
    /// Pause a running process (SIGSTOP on Unix).
    /// </summary>
    public bool PauseRun(string runId)
    {
        if (!_runs.TryGetValue(runId, out var run)) return false;
        if (run.Process == null || run.Process.HasExited) return false;
        if (run.Status == "paused") return true;

        try
        {
            // Send SIGSTOP (19) on Unix
            var killProc = Process.Start("kill", $"-STOP {run.Process.Id}");
            killProc?.WaitForExit(3000);
            run.Status = "paused";
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
    /// Resume a paused process (SIGCONT on Unix).
    /// </summary>
    public bool ResumeRun(string runId)
    {
        if (!_runs.TryGetValue(runId, out var run)) return false;
        if (run.Process == null || run.Process.HasExited) return false;
        if (run.Status != "paused") return false;

        try
        {
            var killProc = Process.Start("kill", $"-CONT {run.Process.Id}");
            killProc?.WaitForExit(3000);
            run.Status = "running";
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

    private (string executable, string arguments) BuildCommand(
        string command, string targetLang, string speedProfile, string? sourceFolder)
    {
        var dotnet = "dotnet";
        var source = sourceFolder ?? "source";
        var project = Path.Combine(_repoRoot, "CobolToQuarkusMigration.csproj");

        return command.ToLowerInvariant() switch
        {
            "migrate" or "run" or "full" =>
                (dotnet, $"run --project \"{project}\" -- --source ./{source}"),

            "reverse-engineer" or "reverse" or "re" =>
                (dotnet, $"run --project \"{project}\" -- reverse-engineer --source ./{source} --output output"),

            "convert-only" or "convert" =>
                (dotnet, $"run --project \"{project}\" -- --source ./{source} --skip-reverse-engineering"),

            "resume" =>
                (dotnet, $"run --project \"{project}\" -- --source ./{source} --resume"),

            _ => (dotnet, $"run --project \"{project}\" -- --source ./{source}")
        };
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

    private async Task CaptureOutputAsync(StreamReader reader, ManagedRun run)
    {
        try
        {
            string? line;
            while ((line = await reader.ReadLineAsync()) != null)
            {
                run.AppendLog(line);
            }
        }
        catch { /* Process ended */ }
    }

    private async Task MonitorProcessAsync(ManagedRun run)
    {
        if (run.Process == null) return;

        try
        {
            await run.Process.WaitForExitAsync();
            run.ExitCode = run.Process.ExitCode;
            run.CompletedAt = DateTime.UtcNow;

            if (run.Status == "running")
            {
                run.Status = run.Process.ExitCode == 0 ? "completed" : "failed";
            }

            run.AppendLog($"[PORTAL] Process exited with code {run.Process.ExitCode}");
            Console.WriteLine($"✅ Run '{run.Name}' finished (exit: {run.Process.ExitCode})");
        }
        catch (Exception ex)
        {
            run.Status = "failed";
            run.AppendLog($"[PORTAL] Monitor error: {ex.Message}");
        }
    }

    public void Dispose()
    {
        foreach (var run in _runs.Values)
        {
            if (run.Process != null && !run.Process.HasExited)
            {
                try { run.Process.Kill(entireProcessTree: true); } catch { }
            }
        }
    }
}
