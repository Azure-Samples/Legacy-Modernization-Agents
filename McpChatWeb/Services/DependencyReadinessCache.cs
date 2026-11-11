namespace McpChatWeb.Services;

// Holds the last whole-estate readiness so the portal can colour program rings without re-hashing
// every input per page load. Any file event under a watched root, a watcher error or MaxAge forces
// a fresh assessment, so a cached "eligible" never outlives a change to its evidence or inputs.
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
        Func<CancellationToken, Task<DependencyReadiness>> assess, CancellationToken ct = default)
    {
        Task<DependencyReadiness> task;
        lock (_lock)
        {
            ObjectDisposedException.ThrowIf(_disposed, this);
            var key = sourceRoot + "\n" + factsRoot;
            if (!_entries.TryGetValue(key, out var entry))
            {
                entry = Entry.TryCreate(new[] { sourceRoot, factsRoot }.Concat(_extraRoots));
                if (entry is null) return assess(ct);
                _entries[key] = entry;
            }
            task = entry.Get(assess);
        }
        return task.WaitAsync(ct);
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
        private long _generation;
        private (Task<DependencyReadiness> Task, long Generation, DateTime StartedAt)? _current;

        public static Entry? TryCreate(IEnumerable<string> roots)
        {
            var entry = new Entry();
            try
            {
                foreach (var root in roots.Select(Path.GetFullPath).Distinct(StringComparer.Ordinal))
                {
                    if (!Directory.Exists(root)) continue;
                    var watcher = new FileSystemWatcher(root)
                    {
                        IncludeSubdirectories = true,
                        NotifyFilter = NotifyFilters.FileName | NotifyFilters.DirectoryName | NotifyFilters.LastWrite
                            | NotifyFilters.Size | NotifyFilters.CreationTime,
                    };
                    watcher.Changed += entry.Invalidate;
                    watcher.Created += entry.Invalidate;
                    watcher.Deleted += entry.Invalidate;
                    watcher.Renamed += entry.Invalidate;
                    watcher.Error += (_, _) => Interlocked.Increment(ref entry._generation);
                    watcher.EnableRaisingEvents = true;
                    entry._watchers.Add(watcher);
                }
            }
            catch (Exception ex) when (ex is IOException or ArgumentException or PlatformNotSupportedException
                or UnauthorizedAccessException)
            {
                entry.Dispose();
                return null;
            }
            // The source and facts roots must be watched, or a change there could go unnoticed.
            return entry._watchers.Count >= 2 ? entry : Disposed(entry);
        }

        private static Entry? Disposed(Entry entry) { entry.Dispose(); return null; }

        private void Invalidate(object sender, FileSystemEventArgs e) => Interlocked.Increment(ref _generation);

        public Task<DependencyReadiness> Get(Func<CancellationToken, Task<DependencyReadiness>> assess)
        {
            var generation = Interlocked.Read(ref _generation);
            if (_current is { } current && current.Generation == generation
                && !current.Task.IsFaulted && !current.Task.IsCanceled
                && DateTime.UtcNow - current.StartedAt < MaxAge)
                return current.Task;
            // Shared by concurrent callers, so one caller's cancellation must not cancel it.
            var task = Task.Run(() => assess(CancellationToken.None));
            _current = (task, generation, DateTime.UtcNow);
            return task;
        }

        public void Dispose()
        {
            foreach (var watcher in _watchers) watcher.Dispose();
            _watchers.Clear();
        }
    }
}
