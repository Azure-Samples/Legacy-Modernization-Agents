using System.Text;
using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Rekt;

// Evidence and platform contracts read REKT_* variables from the process, so these classes never run in parallel.
[CollectionDefinition(Name, DisableParallelization = true)]
public sealed class RektEvidenceCollection
{
    public const string Name = "rekt-evidence";
}

// One program with one copybook and one static callee: the smallest estate every evidence rule can be judged on.
public abstract class RektEvidenceFixture : IDisposable
{
    protected readonly string Work;
    protected readonly string Source;
    protected readonly string Output;
    protected readonly string Relative = "estate/PROG.cbl";
    protected readonly string ProgramPath;
    protected string DefinitionPath;
    protected readonly string ExportPath;

    protected RektEvidenceFixture()
    {
        // Evidence records resolved paths; resolving the root keeps macOS /var -> /private/var out of comparisons.
        Work = Path.Join(PyCompat.Resolve(Path.GetTempPath()), $"rekt-evidence-{Guid.NewGuid():N}");
        Source = Path.Join(Work, "source");
        Output = Path.Join(Work, "output");
        Directory.CreateDirectory(Source);
        Directory.CreateDirectory(Output);
        ProgramPath = Write("estate/PROG.cbl", "       COPY REAL.\n           CALL 'SUBPROG'.\n");
        DefinitionPath = Write("estate/copy/REAL.cpy", "       01 REAL-FIELD PIC X.\n");
        ExportPath = Path.Join(Output, Relative + "-deps.json");
        Directory.CreateDirectory(Path.GetDirectoryName(ExportPath)!);
        File.WriteAllText(ExportPath, PyJson.Dumps(J("""
            {"callTarget": {"name": "PROG.cbl", "programReferenceType": "STATIC"},
             "dependencies": [{"name": "REAL.cpy"}, {"callTarget": {"name": "SUBPROG"}, "dependencies": []}]}
            """)));
    }

    public virtual void Dispose()
    {
        if (Directory.Exists(Work)) Directory.Delete(Work, recursive: true);
    }

    protected static JsonNode J(string json) => PyJson.Parse(json)!;

    protected static JsonObject O(string json) => J(json).AsObject();

    protected static JsonNode? Clone(JsonNode? node) => node?.DeepClone();

    protected static string Digest(string path) => PyCompat.Digest(path);

    protected static JsonObject AllKinds(string status) =>
        new(RektEvidence.Kinds.Select(kind => KeyValuePair.Create(kind, (JsonNode?)status)));

    protected static JsonObject AbsenceProofs() =>
        new(RektEvidence.AbsenceProofs.Select(pair => KeyValuePair.Create(pair.Key, (JsonNode?)pair.Value)));

    protected static List<string> Strings(JsonNode? array) =>
        array!.AsArray().Select(item => item!.GetValue<string>()).ToList();

    protected static List<JsonObject> Objects(JsonNode? array) =>
        array!.AsArray().Select(item => item!.AsObject()).ToList();

    protected static string Text(string path) => File.ReadAllText(path);

    protected static void Save(string path, string text)
    {
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, text);
    }

    protected static void Save(string path, JsonNode? document) => Save(path, PyJson.Dumps(document));

    protected string Write(string relative, string text)
    {
        var path = Path.Join(Source, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllBytes(path, Encoding.Latin1.GetBytes(text));
        return path;
    }

    protected JsonObject Build(int exitCode = 0, string? staging = null) =>
        RektEvidence.BuildEvidence(Source, Relative, Output, exitCode, staging);

    protected string ReportPath => Path.Join(Output, Relative + ".report", "dependency-evidence.json");

    protected bool Current() => RektEvidence.EvidenceCurrent(Source, Relative, Output);

    protected void WriteReport(JsonNode result) => RektEvidence.WriteJson(ReportPath, result);

    protected static void Stage(string source, string target, string? transformed = null, bool derived = false) =>
        RektEvidence.Stage(source, target, transformed, derived, TextWriter.Null);

    protected static T WithEnvironment<T>(string key, string? value, Func<T> action)
    {
        var previous = Environment.GetEnvironmentVariable(key);
        Environment.SetEnvironmentVariable(key, value);
        try { return action(); }
        finally { Environment.SetEnvironmentVariable(key, previous); }
    }

    protected static string RepoRoot()
    {
        var directory = AppContext.BaseDirectory;
        while (!File.Exists(Path.Join(directory, "doctor.sh"))) directory = Path.GetDirectoryName(directory)!;
        return directory;
    }

    // Runs a process with a timeout; the native parser tests drive docker and the dependency gate through this.
    protected static (int ExitCode, string Output) Run(string file, IEnumerable<string> args, int timeoutSeconds = 180)
    {
        var psi = new System.Diagnostics.ProcessStartInfo(file) { RedirectStandardOutput = true, RedirectStandardError = true };
        foreach (var arg in args) psi.ArgumentList.Add(arg);
        using var process = System.Diagnostics.Process.Start(psi)!;
        var stdErr = process.StandardError.ReadToEndAsync();
        var stdOut = process.StandardOutput.ReadToEndAsync();
        if (!process.WaitForExit(timeoutSeconds * 1000))
        {
            process.Kill(entireProcessTree: true);
            throw new TimeoutException(file + " " + string.Join(' ', args));
        }
        return (process.ExitCode, stdOut.Result + stdErr.Result);
    }

    protected JsonObject NativeExport()
    {
        var document = J(Text(ExportPath)).AsObject();
        document["dependencyExportSchemaVersion"] = 1;
        document["sourcePath"] = ProgramPath;
        document["sourceSha256"] = Digest(ProgramPath);
        var coverage = AllKinds("unavailable");
        coverage["COPY"] = "complete";
        coverage["CALL"] = "complete";
        document["dependencyCoverage"] = coverage;
        document["dependencyDiagnostics"] = new JsonArray();
        var callee = Write("estate/SUBPROG.cbl", "           GOBACK.\n");
        document["dependencies"] = new JsonArray(
            new JsonObject
            {
                ["callTarget"] = O("""{"name": "REAL.cpy", "programReferenceType": "STATIC"}"""),
                ["dependencyKind"] = "COPY", ["sourcePath"] = DefinitionPath,
                ["sourceSha256"] = Digest(DefinitionPath), ["dependencies"] = new JsonArray(),
            },
            new JsonObject
            {
                ["callTarget"] = O("""{"name": "SUBPROG", "programReferenceType": "STATIC"}"""),
                ["dependencyKind"] = "CALL", ["sourcePath"] = callee, ["sourceSha256"] = Digest(callee),
                ["dependencies"] = new JsonArray(), ["dependencyAnalysisComplete"] = true,
                ["dependencyCoverage"] = AllKinds("complete"), ["dependencyAbsenceEvidence"] = AbsenceProofs(),
                ["dependencyDiagnostics"] = new JsonArray(),
            });
        Save(ExportPath, document);
        return document;
    }
}
