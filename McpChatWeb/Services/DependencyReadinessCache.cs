namespace McpChatWeb.Services;

// Holds the last whole-estate readiness so the portal can colour program rings without re-hashing
// every input per page load. Any file event under a watched root, a watcher error or MaxAge forces
// a fresh assessment, so a cached "eligible" never outlives a change to its evidence or inputs.
// Inputs an assessment references outside those roots (profiles, contracts, exports) get their own
// directory watchers; one that cannot be watched leaves the result uncached.
// Run launch never reads this cache; it always reassesses.
public sealed class DependencyReadinessCache : IDisposable
{
    public static readonly TimeSpan MaxAge = TimeSpan.FromMinutes(15);

    private readonly object _lock = new();
    private readonly Dictionary<string, Entry> _entries = new(StringComparer.Ordinal);
    private readonly IReadOnlyList<string> _extraRoots;
    private bool _disposed;

    public DependencyReadinessCache(IEnumerable<string>? extraRoots = null) =>
        _extraRoots = (extraRoots ?? []).ToList();

    public Task<DependencyReadiness> GetAsync(
        string sourceRoot, string factsRoot,
        Func<CancellationToken, Task<DependencyReadiness>> assess, CancellationToken ct = default,
        Func<DependencyReadiness, IEnumerable<string>>? referencedInputs = null)
    {
        Task<DependencyReadiness> task;
        lock (_lock)
        {
            ObjectDisposedException.ThrowIf(_disposed, this);
            var key = sourceRoot + "\n" + factsRoot;
            if (!_entries.TryGetValue(key, out var entry))
            {
                entry = Entry.TryCreate([sourceRoot, factsRoot], _extraRoots);
                if (entry is null) return assess(ct);
                _entries[key] = entry;
            }
            task = entry.Get(assess, referencedInputs);
        }
        return task.WaitAsync(ct);
    }

    public void Invalidate(string sourceRoot, string factsRoot)
    {
        lock (_lock)
        {
            if (_entries.TryGetValue(sourceRoot + "\n" + factsRoot, out var entry)) entry.Invalidate();
        }
    }

    public void Dispose()
    {
        lock (_lock)
        {
            _disposed = true;
            foreach (var entry in _entries.Values) entry.Dispose();
            _entries.Clear();
        }
    }

    private sealed class Entry : IDisposable
    {
        private readonly List<FileSystemWatcher> _watchers = [];
        private readonly List<string> _watchedRoots = [];
        private readonly HashSet<string> _watchedDirectories = new(StringComparer.Ordinal);
        private long _generation;
        private bool _disposed;
        private (Task<DependencyReadiness> Task, long Generation, DateTime StartedAt)? _current;

        // Every required root must be watchable: one created later (output/rekt after a first scan)
        // raises no event on its parent's watchers, so caching without it could serve stale readiness.
        public static Entry? TryCreate(IReadOnlyList<string> requiredRoots, IEnumerable<string> optionalRoots)
        {
            if (requiredRoots.Any(root => !Directory.Exists(root))) return null;
            var entry = new Entry();
            try
            {
                foreach (var root in requiredRoots.Concat(optionalRoots).Select(Path.GetFullPath).Distinct(StringComparer.Ordinal))
                {
                    if (!Directory.Exists(root)) continue;
                    entry.Watch(root, includeSubdirectories: true);
                    entry._watchedRoots.Add(root);
                }
            }
            catch (Exception ex) when (ex is IOException or ArgumentException or PlatformNotSupportedException
                or UnauthorizedAccessException)
            {
                entry.Dispose();
                return null;
            }
            return entry;
        }

        private void Watch(string directory, bool includeSubdirectories)
        {
            var watcher = new FileSystemWatcher(directory)
            {
                IncludeSubdirectories = includeSubdirectories,
                NotifyFilter = NotifyFilters.FileName | NotifyFilters.DirectoryName | NotifyFilters.LastWrite
                    | NotifyFilters.Size | NotifyFilters.CreationTime,
            };
            watcher.Changed += Invalidate;
            watcher.Created += Invalidate;
            watcher.Deleted += Invalidate;
            watcher.Renamed += Invalidate;
            watcher.Error += (_, _) => Invalidate();
            watcher.EnableRaisingEvents = true;
            _watchers.Add(watcher);
        }

        private void Invalidate(object sender, FileSystemEventArgs e) => Invalidate();

        public void Invalidate() => Interlocked.Increment(ref _generation);

        // Called once an assessment finishes. A newly watched directory may have changed while the
        // assessment read it, so adding one invalidates this result and the next read reassesses
        // with the watcher in place; an unwatchable input invalidates every result that uses it.
        private void WatchExternalInputs(IEnumerable<string> paths)
        {
            lock (_watchers)
            {
                if (_disposed) return;
                foreach (var path in paths)
                {
                    string directory;
                    try
                    {
                        var full = Path.GetFullPath(path);
                        if (_watchedRoots.Any(root => full.StartsWith(root + Path.DirectorySeparatorChar, StringComparison.Ordinal)))
                            continue;
                        directory = Path.GetDirectoryName(full) ?? "";
                    }
                    catch (Exception ex) when (ex is ArgumentException or NotSupportedException or PathTooLongException)
                    {
                        Invalidate();
                        continue;
                    }
                    if (_watchedDirectories.Contains(directory)) continue;
                    Invalidate();
                    if (!Directory.Exists(directory)) continue;
                    try
                    {
                        Watch(directory, includeSubdirectories: false);
                        _watchedDirectories.Add(directory);
                    }
                    catch (Exception ex) when (ex is IOException or ArgumentException or PlatformNotSupportedException
                        or UnauthorizedAccessException) { }
                }
            }
        }

        public Task<DependencyReadiness> Get(
            Func<CancellationToken, Task<DependencyReadiness>> assess,
            Func<DependencyReadiness, IEnumerable<string>>? referencedInputs)
        {
            var generation = Interlocked.Read(ref _generation);
            if (_current is { } current && current.Generation == generation
                && !current.Task.IsFaulted && !current.Task.IsCanceled
                && DateTime.UtcNow - current.StartedAt < MaxAge)
                return current.Task;
            // Shared by concurrent callers, so one caller's cancellation must not cancel it.
            var task = Task.Run(async () =>
            {
                var result = await assess(CancellationToken.None);
                if (referencedInputs is not null) WatchExternalInputs(referencedInputs(result));
                return result;
            });
            _current = (task, generation, DateTime.UtcNow);
            return task;
        }

        public void Dispose()
        {
            lock (_watchers)
            {
                _disposed = true;
                foreach (var watcher in _watchers) watcher.Dispose();
                _watchers.Clear();
            }
        }
    }
}
