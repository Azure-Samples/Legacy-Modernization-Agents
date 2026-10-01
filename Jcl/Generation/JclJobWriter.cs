using System.Globalization;
using System.Reflection;
using System.Text;
using System.Text.Json;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Jcl.Generation;

public sealed record JclGeneratedFile(string RelativePath, string Content);

public sealed record JclJobManifestEntry(
    string Job,
    string File,
    string Type,
    IReadOnlyList<string> Programs,
    IReadOnlyList<string> ProgramsImplemented,
    IReadOnlyList<string> ProgramsMissing,
    IReadOnlyList<string> StepsNotRunnable,
    IReadOnlyList<JclDiagnostic> Diagnostics);

// Renders job plans as code in the target language, over a runtime that ships with them.
public static class JclJobWriter
{
    public const string ManifestFile = "jobs-manifest.json";
    public const string CSharpRuntimeFile = "Jobs/JclJobRuntime.g.cs";
    public const string CSharpContract = "IBatchProgram";
    public const string JavaContract = "BatchProgram";

    private static readonly Regex CSharpProgramId = new(
        @"\bProgramId\s*(?:=>|\{\s*get\s*(?:=>|\{\s*return))\s*""([^""]+)""", RegexOptions.Compiled);

    private static readonly Regex JavaProgramId = new(
        @"\bprogramId\s*\(\s*\)\s*\{\s*return\s*""([^""]+)""", RegexOptions.Compiled);

    public static IReadOnlyList<JclJobPlan> Plan(IEnumerable<JclJob> jobs)
    {
        var plans = jobs.Select(JclJobPlan.From).ToList();
        // Two members can hold the same job name. Each needs its own identity to be found and launched,
        // so a repeat runs as NAME_2 and says so.
        var names = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        for (var i = 0; i < plans.Count; i++)
        {
            var name = plans[i].JobName;
            for (var n = 2; !names.Add(name); n++) name = plans[i].JobName + "_" + n.ToString(CultureInfo.InvariantCulture);
            if (name == plans[i].JobName) continue;
            plans[i] = plans[i] with
            {
                JobName = name,
                TypeName = JclJobPlan.ToTypeName(name),
                Diagnostics = [.. plans[i].Diagnostics, new(0, "DUPLICATE_JOB_NAME",
                    $"{plans[i].File} names job {plans[i].JobName}, as another member does; it runs as {name}.")],
            };
        }

        // Distinct names can still collapse to one type name.
        var used = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        for (var i = 0; i < plans.Count; i++)
        {
            var type = plans[i].TypeName;
            for (var n = 2; !used.Add(type); n++) type = plans[i].TypeName + "_" + n.ToString(CultureInfo.InvariantCulture);
            if (type != plans[i].TypeName) plans[i] = plans[i] with { TypeName = type };
        }
        return plans;
    }

    // Writes a job per JCL job in the source, its runtime and the manifest under outputRoot, and
    // removes jobs an earlier run generated that the source no longer has. Returns the jobs written.
    public static int WriteTo(string sourceRoot, string outputRoot, string targetLanguage)
    {
        var jobs = Directory.Exists(sourceRoot) ? JclEstate.Parse(sourceRoot) : [];
        var ns = ConversionNamespacePolicy.ForJobs(targetLanguage);
        var csharp = ConversionNamespacePolicy.IsCSharp(targetLanguage);
        var jobsDir = Path.Join(outputRoot, csharp ? "Jobs" : ns.Replace('.', '/'));

        if (Directory.Exists(jobsDir))
            foreach (var stale in Directory.EnumerateFiles(jobsDir, csharp ? "*.g.cs" : "*.java").Where(IsGenerated))
                File.Delete(stale);

        var manifest = Path.Join(outputRoot, ManifestFile);
        if (jobs.Count == 0)
        {
            if (File.Exists(manifest)) File.Delete(manifest);
            return 0;
        }

        var plans = Plan(jobs);
        foreach (var file in Write(plans, targetLanguage, ns))
        {
            var path = Path.Join(outputRoot, file.RelativePath);
            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, file.Content);
        }
        File.WriteAllText(manifest, Manifest(plans, ImplementedPrograms(outputRoot, targetLanguage)));
        // C# gets its project file from the conversion's scaffold; the Spring Boot build is the jobs'.
        if (!csharp) JavaProjectScaffold.Write(outputRoot, ConversionNamespacePolicy.Root("Java"), ns + ".JclBatchApplication");
        return plans.Count;
    }

    private static bool IsGenerated(string path)
    {
        using var reader = new StreamReader(path);
        return reader.ReadLine()?.StartsWith(Header("//").TrimEnd('\n'), StringComparison.Ordinal) == true;
    }

    public static IReadOnlyList<JclGeneratedFile> Write(IReadOnlyList<JclJobPlan> plans, string targetLanguage, string @namespace) =>
        ConversionNamespacePolicy.IsCSharp(targetLanguage) ? CSharp(plans, @namespace) : Java(plans, @namespace);

    // Program ids the converted code already answers to through the batch-program contract.
    public static IReadOnlySet<string> ImplementedPrograms(string outputRoot, string targetLanguage)
    {
        var found = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        if (!Directory.Exists(outputRoot)) return found;
        var csharp = ConversionNamespacePolicy.IsCSharp(targetLanguage);
        var pattern = csharp ? CSharpProgramId : JavaProgramId;
        foreach (var file in Directory.EnumerateFiles(outputRoot, csharp ? "*.cs" : "*.java", SearchOption.AllDirectories))
            foreach (Match match in pattern.Matches(File.ReadAllText(file)))
                found.Add(match.Groups[1].Value.Trim());
        return found;
    }

    public static string Manifest(IReadOnlyList<JclJobPlan> plans, IReadOnlySet<string> implemented)
    {
        var entries = plans.Select(plan => new JclJobManifestEntry(
            plan.JobName,
            plan.File,
            plan.TypeName,
            plan.Programs,
            plan.Programs.Where(implemented.Contains).ToList(),
            plan.Programs.Where(p => !implemented.Contains(p)).ToList(),
            plan.Steps.Where(s => s.Kind == JclPlanStepKind.Unresolved || s.GuardError is not null).Select(s => s.Name).ToList(),
            plan.Diagnostics)).ToList();
        return JsonSerializer.Serialize(entries, JclEstate.JsonOptions);
    }

    public static string Template(string name)
    {
        using var stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("Jcl.Templates." + name)
            ?? throw new InvalidOperationException($"Template {name} is not embedded in the build.");
        using var reader = new StreamReader(stream);
        return reader.ReadToEnd();
    }

    private static IReadOnlyList<JclGeneratedFile> CSharp(IReadOnlyList<JclJobPlan> plans, string ns)
    {
        var files = new List<JclGeneratedFile>
        {
            new(CSharpRuntimeFile, Template("JclJobRuntime.cs.txt").Replace("{{Namespace}}", ns)),
        };
        foreach (var plan in plans)
            files.Add(new($"Jobs/{plan.TypeName}Job.g.cs", CSharpJob(plan, ns)));

        var sb = new StringBuilder();
        sb.Append(Header("//")).Append("#nullable enable\n");
        sb.Append("namespace ").Append(ns).Append("\n{\n");
        sb.Append("    public static class JclJobs\n    {\n");
        sb.Append("        public static IReadOnlyList<JclJob> All { get; } =\n        [\n");
        foreach (var plan in plans) sb.Append("            new ").Append(plan.TypeName).Append("Job(),\n");
        sb.Append("        ];\n\n");
        sb.Append("        public static JclJob? Find(string name) =>\n");
        sb.Append("            All.FirstOrDefault(j => string.Equals(j.Name, name, StringComparison.OrdinalIgnoreCase));\n");
        sb.Append("    }\n}\n");
        files.Add(new("Jobs/JclJobs.g.cs", sb.ToString()));
        return files;
    }

    private static string CSharpJob(JclJobPlan plan, string ns)
    {
        var sb = new StringBuilder();
        sb.Append(Header("//")).Append("// Source: ").Append(Comment(plan.File)).Append('\n');
        sb.Append("#nullable enable\n");
        sb.Append("namespace ").Append(ns).Append("\n{\n");
        sb.Append("    public sealed class ").Append(plan.TypeName).Append("Job : JclJob\n    {\n");
        sb.Append("        public ").Append(plan.TypeName).Append("Job() : base(").Append(Str(plan.JobName))
            .Append(", ").Append(CSharpCond(plan.JobCond)).Append(") { }\n\n");
        sb.Append("        public override IReadOnlyList<JclStepDefinition> Steps { get; } =\n        [\n");
        for (var i = 0; i < plan.Steps.Count; i++) sb.Append("            Step").Append(i + 1).Append("(),\n");
        sb.Append("        ];\n");

        for (var i = 0; i < plan.Steps.Count; i++)
        {
            var step = plan.Steps[i];
            sb.Append('\n');
            if (step.GuardSource is not null) sb.Append("        // IF ").Append(Comment(step.GuardSource)).Append('\n');
            sb.Append("        private static JclStepDefinition Step").Append(i + 1).Append("() => new(")
                .Append(Str(step.Name)).Append(", JclStepKind.").Append(step.Kind).Append(")\n        {\n");
            var p = "            ";
            if (step.Program is not null) sb.Append(p).Append("Program = ").Append(Str(step.Program)).Append(",\n");
            if (step.Procedure is not null) sb.Append(p).Append("Procedure = ").Append(Str(step.Procedure)).Append(",\n");
            if (step.Parm is not null) sb.Append(p).Append("Parm = ").Append(Str(step.Parm)).Append(",\n");
            if (step.Runs.Count > 0)
            {
                sb.Append(p).Append("Runs =\n").Append(p).Append("[\n");
                foreach (var run in step.Runs)
                    sb.Append(p).Append("    new(").Append(Str(run.Program)).Append(", ").Append(Str(run.Plan))
                        .Append(", ").Append(Str(run.Subsystem)).Append(", ").Append(Str(run.Parms)).Append("),\n");
                sb.Append(p).Append("],\n");
            }
            if (step.Guard is not null)
            {
                sb.Append(p).Append("Guard = s => ").Append(Guard(step.Guard, true)).Append(",\n");
                if (JclConditions.TestsAbend(step.Guard)) sb.Append(p).Append("GuardTestsAbend = true,\n");
            }
            if (step.GuardError is not null) sb.Append(p).Append("GuardError = ").Append(Str(step.GuardError)).Append(",\n");
            if (step.Cond is not null) sb.Append(p).Append("Cond = ").Append(CSharpCond(step.Cond)).Append(",\n");
            if (step.Dds.Count > 0)
            {
                sb.Append(p).Append("Dds =\n").Append(p).Append("[\n");
                foreach (var dd in step.Dds) CSharpDd(sb, dd, p + "    ");
                sb.Append(p).Append("],\n");
            }
            if (step.ControlStatements.Count > 0)
            {
                sb.Append(p).Append("ControlStatements =\n").Append(p).Append("[\n");
                foreach (var statement in step.ControlStatements) sb.Append(p).Append("    ").Append(Str(statement)).Append(",\n");
                sb.Append(p).Append("],\n");
            }
            sb.Append("        };\n");
        }
        sb.Append("    }\n}\n");
        return sb.ToString();
    }

    private static void CSharpDd(StringBuilder sb, JclDd dd, string indent)
    {
        sb.Append(indent).Append("new(").Append(Str(dd.Name)).Append(")\n").Append(indent).Append("{\n");
        var p = indent + "    ";
        if (dd.Dataset is { } ds)
        {
            sb.Append(p).Append("Dataset = ").Append(Str(ds.Name)).Append(",\n");
            if (ds.Member is not null) sb.Append(p).Append("Member = ").Append(Str(ds.Member)).Append(",\n");
            if (ds.Generation is not null) sb.Append(p).Append("Generation = ").Append(Str(ds.Generation)).Append(",\n");
            if (ds.Temporary) sb.Append(p).Append("Temporary = true,\n");
        }
        if (dd.Status is not null) sb.Append(p).Append("Status = ").Append(Str(dd.Status)).Append(",\n");
        if (dd.NormalDisposition is not null) sb.Append(p).Append("NormalDisposition = ").Append(Str(dd.NormalDisposition)).Append(",\n");
        if (dd.AbnormalDisposition is not null) sb.Append(p).Append("AbnormalDisposition = ").Append(Str(dd.AbnormalDisposition)).Append(",\n");
        if (dd.SysOut is not null) sb.Append(p).Append("SysOut = ").Append(Str(dd.SysOut)).Append(",\n");
        if (dd.Dummy) sb.Append(p).Append("Dummy = true,\n");
        if (dd.InStream is not null)
        {
            sb.Append(p).Append("InStream =\n").Append(p).Append("[\n");
            foreach (var line in dd.InStream) sb.Append(p).Append("    ").Append(Str(line)).Append(",\n");
            sb.Append(p).Append("],\n");
        }
        if (dd.Concatenated.Count > 0)
        {
            sb.Append(p).Append("Concatenated =\n").Append(p).Append("[\n");
            foreach (var next in dd.Concatenated) CSharpDd(sb, next, p + "    ");
            sb.Append(p).Append("],\n");
        }
        sb.Append(indent).Append("},\n");
    }

    private static string CSharpCond(JclCondParameter? cond) => cond is null ? "null" :
        "new([" + string.Join(", ", cond.Tests.Select(t =>
            $"new({t.Code.ToString(CultureInfo.InvariantCulture)}, JclCompare.{t.Op}, {Str(t.Step)})")) + "]"
        + (cond.Even ? ", Even: true" : "") + (cond.Only ? ", Only: true" : "") + ")";

    private static IReadOnlyList<JclGeneratedFile> Java(IReadOnlyList<JclJobPlan> plans, string package)
    {
        var dir = package.Replace('.', '/') + "/";
        var files = new List<JclGeneratedFile>();
        foreach (var (name, content) in SplitJavaTemplate(Template("JclJobRuntime.java.txt").Replace("{{Package}}", package).Replace("{{ProgramsPackage}}", ConversionNamespacePolicy.Root("Java"))))
            files.Add(new(dir + name, content));
        foreach (var plan in plans)
            files.Add(new(dir + plan.TypeName + "Job.java", JavaJob(plan, package)));

        var sb = new StringBuilder();
        sb.Append(Header("//"));
        sb.Append("package ").Append(package).Append(";\n\n");
        sb.Append("import java.util.List;\n\n");
        sb.Append("public final class JclJobs {\n");
        sb.Append("    private static final List<JclJob> ALL = List.of(");
        sb.Append(string.Join(",", plans.Select(p => "\n            new " + p.TypeName + "Job()")));
        sb.Append(");\n\n    private JclJobs() { }\n\n");
        sb.Append("    public static List<JclJob> all() { return ALL; }\n\n");
        sb.Append("    public static JclJob find(String name) {\n");
        sb.Append("        return ALL.stream().filter(j -> j.name().equalsIgnoreCase(name)).findFirst().orElse(null);\n");
        sb.Append("    }\n}\n");
        files.Add(new(dir + "JclJobs.java", sb.ToString()));
        return files;
    }

    private static IEnumerable<(string Name, string Content)> SplitJavaTemplate(string template)
    {
        const string marker = "//// FILE: ";
        var parts = template.Split(marker, StringSplitOptions.RemoveEmptyEntries);
        foreach (var part in parts)
        {
            var newline = part.IndexOf('\n');
            if (newline < 0) continue;
            yield return (part[..newline].Trim(), part[(newline + 1)..]);
        }
    }

    private static string JavaJob(JclJobPlan plan, string package)
    {
        var sb = new StringBuilder();
        sb.Append(Header("//")).Append("// Source: ").Append(Comment(plan.File)).Append('\n');
        sb.Append("package ").Append(package).Append(";\n\n");
        sb.Append("import java.util.List;\n\n");
        sb.Append("public final class ").Append(plan.TypeName).Append("Job extends JclJob {\n");
        sb.Append("    private static final List<JclStepDefinition> STEPS = List.of(");
        sb.Append(string.Join(", ", Enumerable.Range(1, plan.Steps.Count).Select(i => "step" + i + "()")));
        sb.Append(");\n\n");
        sb.Append("    public ").Append(plan.TypeName).Append("Job() { super(").Append(Str(plan.JobName)).Append(", ")
            .Append(JavaCond(plan.JobCond)).Append("); }\n\n");
        sb.Append("    @Override\n    public List<JclStepDefinition> steps() { return STEPS; }\n");

        for (var i = 0; i < plan.Steps.Count; i++)
        {
            var step = plan.Steps[i];
            sb.Append('\n');
            if (step.GuardSource is not null) sb.Append("    // IF ").Append(Comment(step.GuardSource)).Append('\n');
            sb.Append("    private static JclStepDefinition step").Append(i + 1).Append("() {\n");
            sb.Append("        return JclStepDefinition.of(").Append(Str(step.Name)).Append(", JclStepKind.")
                .Append(step.Kind.ToString().ToUpperInvariant()).Append(')');
            var p = "\n            ";
            if (step.Program is not null) sb.Append(p).Append(".program(").Append(Str(step.Program)).Append(')');
            if (step.Procedure is not null) sb.Append(p).Append(".procedure(").Append(Str(step.Procedure)).Append(')');
            if (step.Parm is not null) sb.Append(p).Append(".parm(").Append(Str(step.Parm)).Append(')');
            if (step.Runs.Count > 0)
                sb.Append(p).Append(".runs(").Append(string.Join(", ", step.Runs.Select(r =>
                    $"new JclDb2Run({Str(r.Program)}, {Str(r.Plan)}, {Str(r.Subsystem)}, {Str(r.Parms)})"))).Append(')');
            if (step.Guard is not null)
                sb.Append(p).Append(".guard(s -> ").Append(Guard(step.Guard, false)).Append(", ")
                    .Append(JclConditions.TestsAbend(step.Guard) ? "true" : "false").Append(')');
            if (step.GuardError is not null) sb.Append(p).Append(".guardError(").Append(Str(step.GuardError)).Append(')');
            if (step.Cond is not null) sb.Append(p).Append(".cond(").Append(JavaCond(step.Cond)).Append(')');
            if (step.Dds.Count > 0)
                sb.Append(p).Append(".dds(").Append(string.Join(",", step.Dds.Select(d => "\n                " + JavaDd(d, "                ")))).Append(')');
            if (step.ControlStatements.Count > 0)
                sb.Append(p).Append(".controlStatements(").Append(string.Join(",", step.ControlStatements.Select(c => "\n                " + Str(c)))).Append(')');
            sb.Append(";\n    }\n");
        }
        sb.Append("}\n");
        return sb.ToString();
    }

    private static string JavaDd(JclDd dd, string indent)
    {
        var sb = new StringBuilder("JclDd.of(").Append(Str(dd.Name)).Append(')');
        if (dd.Dataset is { } ds)
        {
            sb.Append(".dataset(").Append(Str(ds.Name)).Append(')');
            if (ds.Member is not null) sb.Append(".member(").Append(Str(ds.Member)).Append(')');
            if (ds.Generation is not null) sb.Append(".generation(").Append(Str(ds.Generation)).Append(')');
            if (ds.Temporary) sb.Append(".temporary()");
        }
        if (dd.Status is not null) sb.Append(".status(").Append(Str(dd.Status)).Append(')');
        if (dd.NormalDisposition is not null) sb.Append(".normalDisposition(").Append(Str(dd.NormalDisposition)).Append(')');
        if (dd.AbnormalDisposition is not null) sb.Append(".abnormalDisposition(").Append(Str(dd.AbnormalDisposition)).Append(')');
        if (dd.SysOut is not null) sb.Append(".sysOut(").Append(Str(dd.SysOut)).Append(')');
        if (dd.Dummy) sb.Append(".dummy()");
        // With no arguments inStream() is the getter, so empty in-stream data needs an explicit array.
        if (dd.InStream is { Count: 0 }) sb.Append(".inStream(new String[0])");
        else if (dd.InStream is not null)
            sb.Append(".inStream(").Append(string.Join(",", dd.InStream.Select(l => "\n" + indent + "    " + Str(l)))).Append(')');
        if (dd.Concatenated.Count > 0)
            sb.Append(".concatenate(").Append(string.Join(",", dd.Concatenated.Select(c => "\n" + indent + "    " + JavaDd(c, indent + "    ")))).Append(')');
        return sb.ToString();
    }

    private static string JavaCond(JclCondParameter? cond) => cond is null ? "null" :
        "new JclCond(List.of(" + string.Join(", ", cond.Tests.Select(t =>
            $"new JclCondCheck({t.Code.ToString(CultureInfo.InvariantCulture)}, JclCompare.{t.Op.ToString().ToUpperInvariant()}, {Str(t.Step)})"))
        + "), " + (cond.Even ? "true" : "false") + ", " + (cond.Only ? "true" : "false") + ")";

    // Both languages share the operators and the call shape; only case differs.
    private static string Guard(JclExpr expression, bool csharp) => expression switch
    {
        JclNot n => "!(" + Guard(n.Operand, csharp) + ")",
        JclLogical l => "(" + Guard(l.Left, csharp) + (l.IsAnd ? " && " : " || ") + Guard(l.Right, csharp) + ")",
        JclReturnCodeTest t => $"s.{(csharp ? "Rc" : "rc")}({Str(t.Step)}, JclCompare.{Op(t.Op, csharp)}, {t.Value.ToString(CultureInfo.InvariantCulture)})",
        JclAbendCodeTest t => $"s.{(csharp ? "AbendCc" : "abendCc")}({Str(t.Step)}, JclCompare.{Op(t.Op, csharp)}, {Str(t.Code)})",
        JclAbendTest t => $"s.{(csharp ? "Abended" : "abended")}({Str(t.Step)})",
        JclRunTest t => $"s.{(csharp ? "Ran" : "ran")}({Str(t.Step)})",
        _ => throw new NotSupportedException(expression.GetType().Name),
    };

    private static string Op(JclCompare op, bool csharp) => csharp ? op.ToString() : op.ToString().ToUpperInvariant();

    private static string Header(string comment) =>
        comment + " Generated from the estate's JCL. Rewritten on every run; do not edit.\n";

    // Java reads \u as an escape even inside a comment, so backslashes in paths become slashes.
    private static string Comment(string text) => text.Replace('\r', ' ').Replace('\n', ' ').Replace('\\', '/');

    // A literal both C# and Java read the same way.
    private static string Str(string? value)
    {
        if (value is null) return "null";
        var sb = new StringBuilder("\"");
        foreach (var c in value)
        {
            switch (c)
            {
                case '\\': sb.Append("\\\\"); break;
                case '"': sb.Append("\\\""); break;
                case '\n': sb.Append("\\n"); break;
                case '\r': sb.Append("\\r"); break;
                case '\t': sb.Append("\\t"); break;
                default:
                    if (char.IsControl(c) || char.IsSurrogate(c) || c > '\u007e')
                        sb.Append("\\u").Append(((int)c).ToString("x4", CultureInfo.InvariantCulture));
                    else sb.Append(c);
                    break;
            }
        }
        return sb.Append('"').ToString();
    }
}
