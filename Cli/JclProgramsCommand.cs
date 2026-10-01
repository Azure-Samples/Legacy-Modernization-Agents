using System.CommandLine;
using CobolToQuarkusMigration.Jcl;

namespace CobolToQuarkusMigration.Cli;

// Lets a conversion be scoped by job: doctor.sh --job asks which programs a job runs and converts those.
public static class JclProgramsCommand
{
    public static Command Build()
    {
        var cmd = new Command("jcl-programs",
            "List the estate programs each JCL job runs, one job per line: job, file, then the programs comma-separated.");

        var sourceArg = new Argument<string>("source-dir", "Directory containing the JCL, and any catalogued procedures and INCLUDE members.");
        cmd.AddArgument(sourceArg);

        var jobsOption = new Option<string?>("--jobs",
            "Comma-separated job names or member names to list. Lists every job when omitted.")
        { Arity = ArgumentArity.ZeroOrOne };
        cmd.AddOption(jobsOption);

        cmd.SetHandler((string sourceDir, string? jobs) =>
        {
            if (!Directory.Exists(sourceDir))
            {
                Console.Error.WriteLine($"Source dir not found: {sourceDir}");
                Environment.ExitCode = 2;
                return;
            }

            var names = (jobs ?? "").Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
            var selected = Select(JclEstate.Parse(sourceDir), names, out var unknown);
            foreach (var name in unknown)
                Console.Error.WriteLine($"No JCL job is named {name}.");
            if (unknown.Count > 0)
            {
                Environment.ExitCode = 2;
                return;
            }

            foreach (var job in selected)
                Console.WriteLine($"{job.Name}\t{job.File}\t{string.Join(',', JclEstate.Programs(job))}");
        }, sourceArg, jobsOption);

        return cmd;
    }

    // A name matches a job by its JOB statement name or by its member name, since either is what a
    // user sees: the scheduler shows the first, the source folder the second.
    public static IReadOnlyList<JclJob> Select(IReadOnlyList<JclJob> jobs, IReadOnlyList<string> names, out List<string> unknown)
    {
        unknown = [];
        if (names.Count == 0) return jobs;

        var selected = new List<JclJob>();
        foreach (var name in names)
        {
            var matches = jobs.Where(j =>
                j.Name.Equals(name, StringComparison.OrdinalIgnoreCase) ||
                Path.GetFileNameWithoutExtension(j.File).Equals(name, StringComparison.OrdinalIgnoreCase)).ToList();
            if (matches.Count == 0) unknown.Add(name);
            selected.AddRange(matches.Where(m => !selected.Contains(m)));
        }
        return selected;
    }
}
