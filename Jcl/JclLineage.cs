namespace CobolToQuarkusMigration.Jcl;

public sealed record JclDatasetUse(string Job, string Step, JclDatasetAccess Access, string? Generation, string Via);

public sealed record JclDatasetLineage(string Dataset, bool Temporary, IReadOnlyList<JclDatasetUse> Uses);

public sealed record JclJobDependency(string Upstream, string Downstream, IReadOnlyList<string> Datasets);

// Which jobs and steps create, read, append to and delete each dataset, and the job order that
// follows from it. The order is inferred from data only; a scheduler may add or enforce more. It
// can contain cycles: generation data groups are written by one run and read by the next.
public sealed record JclEstateLineage(
    IReadOnlyList<JclDatasetLineage> Datasets,
    IReadOnlyList<JclJobDependency> Dependencies,
    IReadOnlyList<string> ExternalInputs)
{
    // Program and catalog libraries: what a step loads code or catalog entries from, not data it processes.
    private static readonly HashSet<string> LibraryDds = new(StringComparer.OrdinalIgnoreCase)
    {
        "STEPLIB", "JOBLIB", "STEPCAT", "JOBCAT",
    };

    private static readonly JclDatasetAccess[] Writes = [JclDatasetAccess.Create, JclDatasetAccess.Append];
    private static readonly JclDatasetAccess[] Reads = [JclDatasetAccess.Read, JclDatasetAccess.Exclusive];

    public static JclEstateLineage Build(IEnumerable<JclJob> jobs)
    {
        var uses = new SortedDictionary<string, (bool Temporary, List<JclDatasetUse> Uses)>(StringComparer.Ordinal);

        void Add(JclJob job, JclStep step, JclDataset dataset, JclDatasetAccess access, string via)
        {
            // A temporary dataset exists only within its job, so the same name in two jobs is two datasets.
            var key = dataset.Temporary ? $"{job.Name}:{dataset.Name}" : dataset.Name;
            if (!uses.TryGetValue(key, out var entry)) uses[key] = entry = (dataset.Temporary, []);
            entry.Uses.Add(new JclDatasetUse(job.Name, step.Name, access, dataset.Generation, via));
        }

        foreach (var job in jobs)
        foreach (var step in job.Steps)
        {
            foreach (var dd in step.Dds.SelectMany(d => d.Concatenated.Prepend(d)))
            {
                if (dd.Dataset is not { } dataset || dd.Access is not { } access) continue;
                if (LibraryDds.Contains(dd.Name.Split('.')[^1])) continue;
                Add(job, step, dataset, access, dd.Name);
                if (dd.NormalDisposition == "DELETE") Add(job, step, dataset, JclDatasetAccess.Delete, dd.Name);
            }

            foreach (var effect in step.Effects) Add(job, step, effect.Dataset, effect.Access, step.Program ?? "");
        }

        var datasets = uses.Select(u => new JclDatasetLineage(u.Key, u.Value.Temporary, u.Value.Uses)).ToList();

        var dependencies = datasets
            .Where(d => !d.Temporary)
            .SelectMany(d => d.Uses.Where(u => Writes.Contains(u.Access)).Select(w => w.Job).Distinct()
                .SelectMany(up => d.Uses.Where(u => Reads.Contains(u.Access) && u.Job != up).Select(r => r.Job).Distinct()
                    .Select(down => (Up: up, Down: down, d.Dataset))))
            .GroupBy(x => (x.Up, x.Down))
            .OrderBy(g => g.Key.Up, StringComparer.Ordinal).ThenBy(g => g.Key.Down, StringComparer.Ordinal)
            .Select(g => new JclJobDependency(g.Key.Up, g.Key.Down, g.Select(x => x.Dataset).Distinct().OrderBy(x => x, StringComparer.Ordinal).ToList()))
            .ToList();

        // Read before anything in the estate writes it: data that arrives from outside these jobs.
        var external = datasets
            .Where(d => !d.Temporary && d.Uses.Any(u => Reads.Contains(u.Access)) && !d.Uses.Any(u => Writes.Contains(u.Access)))
            .Select(d => d.Dataset)
            .ToList();

        return new JclEstateLineage(datasets, dependencies, external);
    }
}
