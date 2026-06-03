using System.Collections.Concurrent;
using System.Diagnostics;
using System.Text;

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

    /// <summary>
    /// Immutable, per-run output folder (relative to repo root). Computed
    /// when the run starts. Every run writes here instead of the shared
    /// <c>output/java/</c> or <c>output/csharp/</c> so prior runs are never
    /// overwritten and each run's output is independently inspectable.
    /// Pattern: <c>output/runs/{localTimestamp}_{runId}-{targetLang}-{slug}-{utcTimestamp}/</c>
    /// (local timestamp first so the folder is easy to spot in
    /// chronological listings.)
    /// </summary>
    public string OutputFolder { get; set; } = "";

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

    public ProcessManager(string repoRoot)
    {
        _repoRoot = repoRoot;
        _doctorShPath = Path.Combine(repoRoot, "doctor.sh");
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
        var run = new ManagedRun
        {
            Command = command,
            Name = string.IsNullOrWhiteSpace(name) ? $"{command}-{DateTime.Now:HHmmss}" : name,
            TargetLanguage = targetLanguage,
            SpeedProfile = speedProfile,
            Status = "running",
            StartedAt = DateTime.UtcNow
        };

        // ── Per-run immutable output folder ──────────────────────────────
        // Every conversion writes into its own isolated directory so prior
        // runs are never overwritten. This is the CORE addressability change:
        // a Convert click + 2 weeks later you can still inspect exactly what
        // that run produced. The shared output/java and output/csharp folders
        // are no longer used for portal runs.
        //
        // Folder name puts a LOCAL-TIME human-readable timestamp at the FRONT
        // so `ls output/runs/` sorts chronologically by glance and users can
        // find the folder they just kicked off without scanning UTC stamps.
        var langSlug = targetLanguage.Equals("CSharp", StringComparison.OrdinalIgnoreCase) ? "csharp" : "java";
        var nameSlug = Slug(run.Name);
        var localStamp = run.StartedAt.ToLocalTime().ToString("yyyy-MM-dd_HH-mm-ss");
        var utcStamp = run.StartedAt.ToString("yyyyMMddTHHmmssZ");
        run.OutputFolder = $"output/runs/{localStamp}_{run.RunId}-{langSlug}-{nameSlug}-{utcStamp}";
        var absOutputFolder = Path.Combine(_repoRoot, run.OutputFolder);
        try { Directory.CreateDirectory(absOutputFolder); }
        catch (Exception ex)
        {
            // If we can't create the folder, surface clearly and bail.
            run.Status = "failed";
            run.ExitCode = -1;
            run.CompletedAt = DateTime.UtcNow;
            run.AppendLog($"❌ Could not create per-run output folder {absOutputFolder}: {ex.Message}");
            return run;
        }

        // Build the dotnet command directly instead of going through doctor.sh
        // (doctor.sh is interactive — we bypass it for non-interactive portal use)
        var (executable, arguments) = BuildCommand(command, targetLanguage, speedProfile, sourceFolder);

        // ── Rebuild guard ─────────────────────────────────────────────
        // BuildCommand uses --no-build for speed. If any *.cs file under
        // the main project is newer than the compiled DLL the user thinks
        // they're running the latest converter code but they aren't —
        // empty Java files, missing stub-writer logic, stale agents.
        // Detect that staleness here and rebuild once before the run.
        EnsureMainProjectFresh(run);

        var psi = new ProcessStartInfo
        {
            FileName = executable,
            WorkingDirectory = _repoRoot,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };

        foreach (var argument in arguments)
        {
            psi.ArgumentList.Add(argument);
        }

        // Set environment variables
        psi.Environment["TARGET_LANGUAGE"] = targetLanguage;
        psi.Environment["MIGRATION_DB_PATH"] = Path.Combine(_repoRoot, "Data", "migration.db");
        psi.Environment["COBOL_SOURCE_FOLDER"] = sourceFolder ?? "source";
        // Default-ON: REKT context injection (matches doctor.sh behaviour)
        psi.Environment["ENABLE_REKT_CONTEXT"] = "true";

        if (targetLanguage.Equals("CSharp", StringComparison.OrdinalIgnoreCase))
            psi.Environment["CSHARP_OUTPUT_FOLDER"] = run.OutputFolder;
        else
            psi.Environment["JAVA_OUTPUT_FOLDER"] = run.OutputFolder;

        // Load AI config env files (same ones doctor.sh sources)
        // Order: local first (higher priority), then template for defaults.
        LoadEnvFile(psi.Environment, Path.Combine(_repoRoot, "Config", "ai-config.local.env"));
        LoadEnvFile(psi.Environment, Path.Combine(_repoRoot, "Config", "ai-config.env"));

        // Speed profile env vars
        ApplySpeedProfile(psi.Environment, speedProfile);

        // Extra env vars (from Convert modal: SELECTOR_MODE, COPILOT_SAFE_MODE, etc.)
        if (extraEnv != null)
        {
            foreach (var (k, v) in extraEnv)
                if (!string.IsNullOrEmpty(v)) psi.Environment[k] = v;
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

            run.AppendLog($"[PORTAL] Started: {executable} {string.Join(" ", arguments)}");
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
            // Send SIGSTOP on Unix via ArgumentList to avoid shell injection
            var killPsi = new ProcessStartInfo
            {
                FileName = "kill",
                UseShellExecute = false,
                CreateNoWindow = true
            };
            killPsi.ArgumentList.Add("-STOP");
            killPsi.ArgumentList.Add(run.Process.Id.ToString());

            var killProc = Process.Start(killPsi);
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
            // Send SIGCONT on Unix via ArgumentList to avoid shell injection
            var killPsi = new ProcessStartInfo
            {
                FileName = "kill",
                UseShellExecute = false,
                CreateNoWindow = true
            };
            killPsi.ArgumentList.Add("-CONT");
            killPsi.ArgumentList.Add(run.Process.Id.ToString());

            var killProc = Process.Start(killPsi);
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

    private (string executable, string[] arguments) BuildCommand(
        string command, string targetLang, string speedProfile, string? sourceFolder)
    {
        // Always use direct dotnet invocation with --no-build from the portal.
        // Using doctor.sh caused two critical issues:
        //   1. doctor.sh calls `dotnet run` (without --no-build) which rebuilds
        //      the project, including the portal's DLL → macOS file lock kills
        //      the running portal → portal restarts → loses all in-memory state.
        //   2. doctor.sh's interactive prompts + run_via_portal() created an
        //      infinite process-spawn loop (portal → doctor.sh → portal → ...).
        //
        // Instead, ProcessManager sets ALL the env vars that doctor.sh would
        // (ENABLE_REKT_CONTEXT, SELECTOR_MODE, COPILOT_SAFE_MODE, etc.) so
        // the dotnet process gets the same configuration. The preprocessor and
        // REKT staging are handled by the Convert modal's staging logic which
        // already runs before this point.
        var dotnet = "dotnet";
        var source = $"./{sourceFolder ?? "source"}";
        var project = Path.Combine(_repoRoot, "CobolToQuarkusMigration.csproj");

        return command.ToLowerInvariant() switch
        {
            "migrate" or "run" or "full" =>
                (dotnet, new[] { "run", "--no-build", "--project", project, "--", "--source", source }),

            "reverse-engineer" or "reverse" or "re" =>
                (dotnet, new[] { "run", "--no-build", "--project", project, "--", "reverse-engineer", "--source", source, "--output", "output" }),

            "convert-only" or "convert" =>
                (dotnet, new[] { "run", "--no-build", "--project", project, "--", "--source", source, "--skip-reverse-engineering" }),

            "resume" =>
                (dotnet, new[] { "run", "--no-build", "--project", project, "--", "--source", source, "--resume" }),

            _ => (dotnet, new[] { "run", "--no-build", "--project", project, "--", "--source", source })
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

    /// <summary>
    /// Load a KEY=VALUE env file (same format doctor.sh uses) into the process
    /// environment. Skips comments, blank lines, and keys already set.
    /// </summary>
    private static void LoadEnvFile(IDictionary<string, string?> env, string path)
    {
        if (!File.Exists(path)) return;
        foreach (var rawLine in File.ReadAllLines(path))
        {
            var line = rawLine.Trim();
            if (string.IsNullOrEmpty(line) || line.StartsWith('#')) continue;
            var eq = line.IndexOf('=');
            if (eq <= 0) continue;
            var key = line[..eq].Trim();
            var val = line[(eq + 1)..].Trim().Trim('"');
            // Don't overwrite values already set (local.env loaded after template)
            if (!env.ContainsKey(key) || string.IsNullOrEmpty(env[key]))
                env[key] = val;
        }
    }

    /// <summary>
    /// Ensure the main CobolToQuarkusMigration project is built with all
    /// current source changes before the run starts. We use `--no-build`
    /// for speed, so a stale DLL would silently ship old converter code
    /// (typical symptom: 0-byte Java files because the stub-writer fix
    /// landed in source but never made it into the running binary).
    ///
    /// Rebuild only when ANY *.cs file under the main project is newer
    /// than the compiled DLL — keeps the fast path fast and adds ~3-8 s
    /// on the rare run that needs it.
    /// </summary>
    private void EnsureMainProjectFresh(ManagedRun run)
    {
        try
        {
            var project = Path.Combine(_repoRoot, "CobolToQuarkusMigration.csproj");
            var dll = Path.Combine(_repoRoot, "bin", "Debug", "net10.0", "CobolToQuarkusMigration.dll");
            if (!File.Exists(project)) return;

            DateTime dllStamp = File.Exists(dll)
                ? File.GetLastWriteTimeUtc(dll)
                : DateTime.MinValue;

            // Scan top-level *.cs source directories for any file newer
            // than the DLL. Skip generated/build folders.
            var sourceDirs = new[] { "Agents", "Helpers", "Processes", "Models", "Services", "Prompts", "Telemetry" };
            bool stale = false;
            foreach (var rel in sourceDirs)
            {
                var dir = Path.Combine(_repoRoot, rel);
                if (!Directory.Exists(dir)) continue;
                foreach (var f in Directory.EnumerateFiles(dir, "*.cs", SearchOption.AllDirectories))
                {
                    if (File.GetLastWriteTimeUtc(f) > dllStamp) { stale = true; break; }
                }
                if (stale) break;
            }
            // Also include the root .cs files (Program.cs etc.)
            if (!stale)
            {
                foreach (var f in Directory.EnumerateFiles(_repoRoot, "*.cs", SearchOption.TopDirectoryOnly))
                {
                    if (File.GetLastWriteTimeUtc(f) > dllStamp) { stale = true; break; }
                }
            }

            if (!stale) return;

            run.AppendLog("🔨 Source code newer than CobolToQuarkusMigration.dll — rebuilding before run...");
            var build = new ProcessStartInfo
            {
                FileName = "dotnet",
                Arguments = $"build \"{project}\" -c Debug --nologo -v minimal",
                WorkingDirectory = _repoRoot,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true
            };
            using var p = Process.Start(build);
            if (p == null) { run.AppendLog("⚠️  dotnet build did not start; continuing with possibly-stale DLL"); return; }
            var stdout = p.StandardOutput.ReadToEnd();
            var stderr = p.StandardError.ReadToEnd();
            p.WaitForExit();
            if (p.ExitCode != 0)
            {
                run.AppendLog($"❌ dotnet build failed (exit {p.ExitCode}); aborting run.");
                if (!string.IsNullOrWhiteSpace(stderr)) run.AppendLog(stderr.TrimEnd());
                if (!string.IsNullOrWhiteSpace(stdout)) run.AppendLog(stdout.TrimEnd());
                throw new InvalidOperationException($"dotnet build of CobolToQuarkusMigration.csproj failed with exit code {p.ExitCode}.");
            }
            run.AppendLog("✅ Rebuild complete.");
        }
        catch (InvalidOperationException) { throw; }
        catch (Exception ex)
        {
            // Don't block the run on detection errors — log and proceed.
            run.AppendLog($"⚠️  Could not verify build freshness: {ex.Message}");
        }
    }

    /// <summary>
    /// Generates a URL/folder-safe slug from a free-form run name.
    /// </summary>
    private static string Slug(string s)
    {
        if (string.IsNullOrWhiteSpace(s)) return "run";
        var sb = new System.Text.StringBuilder();
        bool lastWasDash = false;
        foreach (var c in s.ToLowerInvariant())
        {
            if (char.IsLetterOrDigit(c))
            {
                sb.Append(c);
                lastWasDash = false;
            }
            else if (!lastWasDash && sb.Length > 0)
            {
                sb.Append('-');
                lastWasDash = true;
            }
        }
        var slug = sb.ToString().Trim('-');
        if (slug.Length > 40) slug = slug.Substring(0, 40).TrimEnd('-');
        return string.IsNullOrEmpty(slug) ? "run" : slug;
    }
}
