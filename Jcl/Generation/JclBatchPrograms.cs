using System.Text;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Jcl.Generation;

// Which programs the estate's JCL runs, and how, so their conversion can be told to implement the
// batch-program contract the generated jobs call.
public sealed class JclBatchPrograms
{
    public const string EnabledVariable = "JCL_JOBS_ENABLED";
    public const string SourceVariable = "JCL_SOURCE_FOLDER";

    private readonly Dictionary<string, List<string>> _invocations;

    private JclBatchPrograms(Dictionary<string, List<string>> invocations) => _invocations = invocations;

    public IReadOnlyCollection<string> Programs => _invocations.Keys;

    public static JclBatchPrograms Build(string sourceRoot)
    {
        var invocations = new Dictionary<string, List<string>>(StringComparer.OrdinalIgnoreCase);
        if (!Directory.Exists(sourceRoot)) return new JclBatchPrograms(invocations);

        void Add(string program, string line)
        {
            if (!invocations.TryGetValue(program, out var list)) invocations[program] = list = [];
            if (!list.Contains(line)) list.Add(line);
        }

        foreach (var job in JclEstate.Parse(sourceRoot))
        {
            foreach (var step in job.Steps)
            {
                var dds = Describe(step.Dds);
                if (step.Kind == JclStepKind.Program && step.Program is not null)
                    Add(step.Program, $"  • job {job.Name}, step {step.Name}: {dds}{Parm(step.Parm)}");
                foreach (var run in step.Runs)
                    Add(run.Program, $"  • job {job.Name}, step {step.Name} (under the TSO monitor{(run.Plan is null ? "" : ", Db2 plan " + run.Plan)}): {dds}{Parm(run.Parms)}");
            }
        }
        return new JclBatchPrograms(invocations);
    }

    public string ToPromptBlock(string programStem, string targetLanguage)
    {
        if (!_invocations.TryGetValue(programStem, out var lines)) return string.Empty;
        var csharp = ConversionNamespacePolicy.IsCSharp(targetLanguage);
        return Environment.NewLine + PromptLoader.LoadSectionValidated(
            "RektContext", csharp ? "CSharpBatchProgram" : "JavaBatchProgram", new Dictionary<string, string>
            {
                ["Invocations"] = string.Join(Environment.NewLine, lines),
                ["JobsNamespace"] = ConversionNamespacePolicy.ForJobs(targetLanguage),
                ["ProgramId"] = programStem.ToUpperInvariant(),
            });
    }

    private static string Describe(IReadOnlyList<JclDd> dds)
    {
        var described = dds
            .Where(dd => !JclEstateLineage.IsLibraryDd(dd.Name))
            .Select(dd => dd.Name + " (" + (dd.Dummy ? "dummy"
                : dd.SysOut is not null ? "sysout"
                : dd.InStream is not null ? "in-stream data"
                : dd.Access switch
                {
                    JclDatasetAccess.Read => "read",
                    JclDatasetAccess.Create => "written, new",
                    JclDatasetAccess.Append => "appended to",
                    JclDatasetAccess.Exclusive => "read or written",
                    _ => "dataset",
                }) + ")")
            .ToList();
        return described.Count == 0 ? "no DDs" : "DDs " + string.Join(", ", described);
    }

    private static string Parm(string? parm) =>
        string.IsNullOrEmpty(parm) ? "" : "; PARM " + parm;
}

public static class JclBatchProgramsHolder
{
    private static readonly object Lock = new();
    private static readonly Dictionary<string, JclBatchPrograms> Cache = new(StringComparer.OrdinalIgnoreCase);

    // Off only for the literal "false", like the other structural context.
    public static bool Enabled =>
        !string.Equals(Environment.GetEnvironmentVariable(JclBatchPrograms.EnabledVariable)?.Trim(), "false", StringComparison.OrdinalIgnoreCase);

    public static JclBatchPrograms GetOrBuild(string repoRoot, string sourceFolder)
    {
        var configured = Environment.GetEnvironmentVariable(JclBatchPrograms.SourceVariable);
        var folder = string.IsNullOrWhiteSpace(configured) ? sourceFolder : configured;
        var key = Path.IsPathRooted(folder) ? folder : Path.Join(repoRoot, folder);
        lock (Lock)
        {
            if (Cache.TryGetValue(key, out var existing)) return existing;
            return Cache[key] = JclBatchPrograms.Build(key);
        }
    }
}
