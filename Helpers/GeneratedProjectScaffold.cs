// Writes the two files that stand between a conversion's output and a compiler.
//
// Generated C# opens with a fixed using block — System, System.Collections.Generic, System.Linq —
// and then uses Column, Key, Table, MaxLength, DbSet, DbContext, ILogger, IConfiguration and
// IServiceCollection, none of which that block covers. Measured on a real run of five programs and
// their copybooks, that single gap accounted for 1028 of 1140 compiler errors. Nothing was wrong
// with the converted logic; there was simply no way to resolve the types it named.
//
// Asking the model for the right usings is the approach that has already failed twice on this
// branch for shared types and call targets. This does not ask. It reads the generated code, sees
// which of a known set of types appear, and writes exactly the usings and package references those
// require — so a dependency is never declared for code that does not use it.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;
using CobolToQuarkusMigration.Jcl.Generation;

/// <summary>A namespace the generated code needs, and the package that supplies it.</summary>
public sealed record GeneratedDependency(string Marker, string Namespace, string Package, string Version);

public sealed record ScaffoldResult(
    IReadOnlyList<string> Usings,
    IReadOnlyList<string> Packages,
    string? GlobalUsingsPath,
    string? ProjectPath,
    IReadOnlyList<string>? NormalizedFiles = null)
{
    public bool WroteAnything => GlobalUsingsPath is not null;

    /// <summary>Generated files rewritten from several file-scoped namespaces to block-scoped ones.</summary>
    public IReadOnlyList<string> Normalized => NormalizedFiles ?? [];
}

public static class GeneratedProjectScaffold
{
    public const string GlobalUsingsFile = "GlobalUsings.g.cs";
    public const string CallTargetContractsFile = "CallTargetContracts.g.cs";

    /// <summary>
    /// Only what has actually been observed in generated output. A marker earns its dependency;
    /// nothing is included speculatively, because an unused package reference is a lie about what
    /// the converted code depends on.
    /// </summary>
    public static readonly GeneratedDependency[] Known =
    [
        new("[Column", "System.ComponentModel.DataAnnotations.Schema", "System.ComponentModel.Annotations", "5.0.0"),
        new("[Table", "System.ComponentModel.DataAnnotations.Schema", "System.ComponentModel.Annotations", "5.0.0"),
        new("[DatabaseGenerated", "System.ComponentModel.DataAnnotations.Schema", "System.ComponentModel.Annotations", "5.0.0"),
        new("[Key", "System.ComponentModel.DataAnnotations", "System.ComponentModel.Annotations", "5.0.0"),
        new("[MaxLength", "System.ComponentModel.DataAnnotations", "System.ComponentModel.Annotations", "5.0.0"),
        new("[Required", "System.ComponentModel.DataAnnotations", "System.ComponentModel.Annotations", "5.0.0"),
        new("DbSet<", "Microsoft.EntityFrameworkCore", "Microsoft.EntityFrameworkCore", "9.0.0"),
        new("DbContext", "Microsoft.EntityFrameworkCore", "Microsoft.EntityFrameworkCore", "9.0.0"),
        new("ModelBuilder", "Microsoft.EntityFrameworkCore", "Microsoft.EntityFrameworkCore", "9.0.0"),
        new("ILogger", "Microsoft.Extensions.Logging", "Microsoft.Extensions.Logging.Abstractions", "9.0.0"),
        new("IConfiguration", "Microsoft.Extensions.Configuration", "Microsoft.Extensions.Configuration.Abstractions", "9.0.0"),
        new("IServiceCollection", "Microsoft.Extensions.DependencyInjection", "Microsoft.Extensions.DependencyInjection.Abstractions", "9.0.0"),
    ];

    /// <summary>
    /// Writes a global-usings file and a project file into a completed run folder.
    /// </summary>
    /// <remarks>
    /// Both are generated artefacts and are overwritten on each run. Neither contains converted
    /// logic, so regenerating them can lose nothing. Before writing them, any generated file that
    /// declares several file-scoped namespaces is rewritten to block-scoped ones — a syntax-only
    /// change, see <see cref="FileScopedNamespaceNormalizer"/>.
    /// </remarks>
    public static ScaffoldResult Write(
        string runFolder, string assemblyName, string targetFramework = "net10.0",
        CallTargetRegistry? callTargets = null, string? sharedNamespace = null)
    {
        if (!Directory.Exists(runFolder))
            return new ScaffoldResult([], [], null, null);

        if (callTargets is not null && sharedNamespace is not null)
            WriteCallTargetContracts(runFolder, callTargets, sharedNamespace);

        // Jobs generated from JCL declare the batch-program contract; a program that declared its
        // own copy would implement a different interface than the one the job runner looks for.
        var jobsNamespace = File.Exists(Path.Join(runFolder, JclJobWriter.CSharpRuntimeFile))
            ? ConversionNamespacePolicy.ForJobs("C#") : null;
        if (jobsNamespace is not null)
            StripDeclarations(runFolder, [JclJobWriter.CSharpContract]);

        var sources = Directory
            .EnumerateFiles(runFolder, "*.cs", SearchOption.AllDirectories)
            .Where(f => !Path.GetFileName(f).Equals(GlobalUsingsFile, StringComparison.OrdinalIgnoreCase))
            .ToList();

        if (sources.Count == 0) return new ScaffoldResult([], [], null, null);

        var normalized = FileScopedNamespaceNormalizer.NormalizeFolder(runFolder)
            .Where(f => !Path.GetFileName(f).Equals(GlobalUsingsFile, StringComparison.OrdinalIgnoreCase))
            .ToList();

        var needed = Detect(sources);

        var usings = needed
            .Select(d => d.Namespace)
            .Concat(jobsNamespace is null ? [] : [jobsNamespace])
            .Distinct(StringComparer.Ordinal)
            .OrderBy(n => n, StringComparer.Ordinal)
            .ToList();

        var packages = needed
            .Select(d => (d.Package, d.Version))
            .Distinct()
            .OrderBy(p => p.Package, StringComparer.Ordinal)
            .ToList();

        var usingsPath = Path.Join(runFolder, GlobalUsingsFile);
        File.WriteAllText(usingsPath, RenderUsings(usings));

        var projectPath = Path.Join(runFolder, assemblyName + ".csproj");
        File.WriteAllText(projectPath, RenderProject(packages, targetFramework));

        return new ScaffoldResult(
            usings,
            packages.Select(p => p.Package).ToList(),
            usingsPath,
            projectPath,
            normalized);
    }

    private static void WriteCallTargetContracts(string runFolder, CallTargetRegistry callTargets, string sharedNamespace)
    {
        var path = Path.Join(runFolder, CallTargetContractsFile);
        var names = callTargets.GeneratedInterfaces("C#").Select(c => c.InterfaceName).ToList();
        if (names.Count == 0)
        {
            File.Delete(path);
            return;
        }

        foreach (var file in Directory.EnumerateFiles(runFolder, "*.cs", SearchOption.AllDirectories))
        {
            var name = Path.GetFileName(file);
            if (name.Equals(CallTargetContractsFile, StringComparison.OrdinalIgnoreCase)
                || name.Equals(GlobalUsingsFile, StringComparison.OrdinalIgnoreCase)
                || IsBuildOutput(runFolder, file)) continue;
            var text = File.ReadAllText(file);
            var stripped = GeneratedInterfaceDeclarations.RemoveFrom(text, names);
            if (!ReferenceEquals(text, stripped) && text != stripped) File.WriteAllText(file, stripped);
        }
        File.WriteAllText(path, callTargets.RenderCSharpInterfaces(sharedNamespace));
    }

    private static void StripDeclarations(string runFolder, IReadOnlyCollection<string> names)
    {
        foreach (var file in Directory.EnumerateFiles(runFolder, "*.cs", SearchOption.AllDirectories))
        {
            if (file.EndsWith(".g.cs", StringComparison.OrdinalIgnoreCase) || IsBuildOutput(runFolder, file)) continue;
            var text = File.ReadAllText(file);
            var stripped = GeneratedInterfaceDeclarations.RemoveFrom(text, names);
            if (text != stripped) File.WriteAllText(file, stripped);
        }
    }

    private static bool IsBuildOutput(string runFolder, string file)
    {
        var first = Path.GetRelativePath(runFolder, file).Split(Path.DirectorySeparatorChar)[0];
        return first is "bin" or "obj";
    }

    /// <summary>The dependencies the given sources actually reference.</summary>
    public static IReadOnlyList<GeneratedDependency> Detect(IEnumerable<string> sourceFiles)
    {
        var found = new List<GeneratedDependency>();

        foreach (var file in sourceFiles)
        {
            string text;
            try { text = File.ReadAllText(file); }
            catch (IOException) { continue; }

            foreach (var dependency in Known)
            {
                if (found.Contains(dependency)) continue;
                if (text.Contains(dependency.Marker, StringComparison.Ordinal))
                    found.Add(dependency);
            }
        }

        return found;
    }

    private static string RenderUsings(IReadOnlyList<string> namespaces)
    {
        var sb = new StringBuilder();
        sb.AppendLine("// Generated alongside the converted sources in this folder.");
        sb.AppendLine("// The converter emits a fixed using block that does not cover the framework");
        sb.AppendLine("// types it goes on to use; these make those names resolvable without editing");
        sb.AppendLine("// every generated file. Overwritten on each conversion run.");
        sb.AppendLine();

        foreach (var ns in namespaces) sb.AppendLine($"global using {ns};");

        return sb.ToString();
    }

    private static string RenderProject(
        IReadOnlyList<(string Package, string Version)> packages, string targetFramework)
    {
        var sb = new StringBuilder();
        sb.AppendLine("<!-- Generated alongside the converted sources in this folder, so the run can be");
        sb.AppendLine("     compiled without a project file being written by hand. It declares only the");
        sb.AppendLine("     packages the generated code was observed to use. Overwritten on each run. -->");
        sb.AppendLine("<Project Sdk=\"Microsoft.NET.Sdk\">");
        sb.AppendLine();
        sb.AppendLine("  <PropertyGroup>");
        sb.AppendLine($"    <TargetFramework>{targetFramework}</TargetFramework>");
        sb.AppendLine("    <Nullable>disable</Nullable>");
        sb.AppendLine("    <ImplicitUsings>enable</ImplicitUsings>");
        sb.AppendLine("    <!-- Converted programs are libraries; no entry point is generated. -->");
        sb.AppendLine("    <OutputType>Library</OutputType>");
        sb.AppendLine("  </PropertyGroup>");

        if (packages.Count > 0)
        {
            sb.AppendLine();
            sb.AppendLine("  <ItemGroup>");
            foreach (var (package, version) in packages)
                sb.AppendLine($"    <PackageReference Include=\"{package}\" Version=\"{version}\" />");
            sb.AppendLine("  </ItemGroup>");
        }

        sb.AppendLine();
        sb.AppendLine("</Project>");
        return sb.ToString();
    }
}
