// Writes the Maven build that lets a Java run folder with jobs in it be compiled and run.
//
// The jobs generated from JCL are Spring Batch jobs, so the build is a Spring Boot application
// whose main class is the generated JclBatchApplication. The converted programs beside them
// import Jakarta APIs and Quarkus Panache; as with the C# scaffold, a dependency is declared only
// when the generated code imports it.

namespace CobolToQuarkusMigration.Helpers;

using System.Text;

/// <summary>An import prefix the generated Java uses, and the artifact that supplies it.</summary>
public sealed record JavaDependency(string ImportPrefix, string GroupId, string ArtifactId, string? Version, string? Scope = null);

public static class JavaProjectScaffold
{
    public const string PomFile = "pom.xml";
    public const string PropertiesFile = "application.properties";
    public const string SpringBootVersion = "3.5.16";
    private const string Marker = "Generated beside the code in this run folder";

    public static readonly JavaDependency[] Known =
    [
        new("org.springframework.jdbc", "org.springframework.boot", "spring-boot-starter-jdbc", null),
        new("org.springframework.transaction", "org.springframework.boot", "spring-boot-starter-jdbc", null),
        new("org.springframework.web", "org.springframework.boot", "spring-boot-starter-web", null),
        new("org.springframework.data.jpa", "org.springframework.boot", "spring-boot-starter-data-jpa", null),
        new("org.springframework.validation", "org.springframework.boot", "spring-boot-starter-validation", null),
        new("jakarta.validation", "org.springframework.boot", "spring-boot-starter-validation", null),
        new("org.springframework", "org.springframework.boot", "spring-boot-starter", null),
        new("org.h2", "com.h2database", "h2", "2.3.232"),
        new("jakarta.persistence", "jakarta.persistence", "jakarta.persistence-api", "3.1.0"),
        new("jakarta.transaction", "jakarta.transaction", "jakarta.transaction-api", "2.0.1"),
        new("jakarta.enterprise", "jakarta.enterprise", "jakarta.enterprise.cdi-api", "4.0.1"),
        new("jakarta.inject", "jakarta.inject", "jakarta.inject-api", "2.0.1"),
        new("jakarta.annotation", "jakarta.annotation", "jakarta.annotation-api", "2.1.1"),
        new("jakarta.ws.rs", "jakarta.ws.rs", "jakarta.ws.rs-api", "3.1.0"),
        // Compile only: Panache's runtime is Quarkus, not the Spring Boot application the jobs run in.
        new("io.quarkus.hibernate.orm.panache", "io.quarkus", "quarkus-hibernate-orm-panache", "3.40.1", "provided"),
    ];

    /// <summary>
    /// Writes pom.xml and application.properties into runFolder. A file of either name that this
    /// scaffold did not write is left as it is. Returns the paths written.
    /// </summary>
    public static IReadOnlyList<string> Write(string runFolder, string groupId, string? mainClass = null,
        string framework = "Quarkus")
    {
        JavaGeneration.ValidateFramework(framework);
        var written = new List<string>();
        if (!Directory.Exists(runFolder)) return written;

        var pom = Path.Join(runFolder, PomFile);
        var batchApplication = mainClass is not null;
        var keepBatchBuild = mainClass is null && File.Exists(pom)
            && File.ReadAllText(pom).Contains("<artifactId>spring-boot-starter-batch</artifactId>", StringComparison.Ordinal);
        if (framework == "Spring" && mainClass is null && !keepBatchBuild && Ours(pom))
        {
            mainClass = groupId + ".ModernizedApplication";
            var bootstrap = Path.Join(runFolder, groupId.Replace('.', Path.DirectorySeparatorChar), "ModernizedApplication.java");
            if (Ours(bootstrap))
            {
                Directory.CreateDirectory(Path.GetDirectoryName(bootstrap)!);
                File.WriteAllText(bootstrap,
                    $"// {Marker}.\npackage {groupId};\n\n" +
                    "import org.springframework.boot.SpringApplication;\n" +
                    "import org.springframework.boot.autoconfigure.SpringBootApplication;\n\n" +
                    "@SpringBootApplication\npublic class ModernizedApplication {\n" +
                    "    public static void main(String[] args) {\n" +
                    "        SpringApplication.run(ModernizedApplication.class, args);\n    }\n}\n");
                written.Add(bootstrap);
            }
        }
        if (Ours(pom) && !keepBatchBuild)
        {
            File.WriteAllText(pom, RenderPom(groupId, mainClass, Detect(runFolder), batchApplication));
            written.Add(pom);
        }

        var properties = Path.Join(runFolder, PropertiesFile);
        if (batchApplication && Ours(properties))
        {
            File.WriteAllText(properties,
                $"# {Marker}; overwritten on each run unless this line is removed.\n" +
                "# Datasets the jobs name resolve to files under this folder (docs/jcl-jobs.md).\n" +
                "jcl.datasets.root=datasets\n");
            written.Add(properties);
        }
        return written;
    }

    /// <summary>The dependencies the Java sources under runFolder import.</summary>
    public static IReadOnlyList<JavaDependency> Detect(string runFolder)
    {
        var found = new List<JavaDependency>();
        foreach (var file in Directory.EnumerateFiles(runFolder, "*.java", SearchOption.AllDirectories)
                     .Where(f => Path.GetRelativePath(runFolder, f).Split(Path.DirectorySeparatorChar)[0] != "target"))
        {
            string text;
            try { text = File.ReadAllText(file); }
            catch (IOException) { continue; }
            found.AddRange(Known.Where(d => !found.Contains(d)
                && text.Contains("import " + d.ImportPrefix + ".", StringComparison.Ordinal)));
        }
        return found.DistinctBy(d => (d.GroupId, d.ArtifactId)).ToArray();
    }

    private static bool Ours(string path) =>
        !File.Exists(path) || File.ReadLines(path).Take(3).Any(l => l.Contains(Marker, StringComparison.Ordinal));

    private static string RenderPom(string groupId, string? mainClass, IReadOnlyList<JavaDependency> dependencies,
        bool batchApplication)
    {
        var spring = mainClass is not null || dependencies.Any(d => d.GroupId == "org.springframework.boot");
        var sb = new StringBuilder();
        sb.Append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        sb.Append("<!-- ").Append(Marker).Append("; overwritten on each run unless this line is removed.\n");
        sb.Append("     Build with mvn compile. -->\n");
        sb.Append("<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
        sb.Append("         xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd\">\n");
        sb.Append("  <modelVersion>4.0.0</modelVersion>\n\n");
        if (spring)
        {
            sb.Append("  <parent>\n    <groupId>org.springframework.boot</groupId>\n    <artifactId>spring-boot-starter-parent</artifactId>\n");
            sb.Append("    <version>").Append(SpringBootVersion).Append("</version>\n    <relativePath/>\n  </parent>\n\n");
        }
        sb.Append("  <groupId>").Append(groupId).Append("</groupId>\n");
        sb.Append("  <artifactId>modernized</artifactId>\n  <version>1.0.0-SNAPSHOT</version>\n\n");
        sb.Append("  <properties>\n    <java.version>17</java.version>\n    <maven.compiler.release>17</maven.compiler.release>\n    <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>\n  </properties>\n\n");
        sb.Append("  <dependencies>\n");
        if (batchApplication)
        {
            Dependency(sb, "org.springframework.boot", "spring-boot-starter-batch", null, null);
            if (!dependencies.Any(dependency => dependency.GroupId == "com.h2database" && dependency.ArtifactId == "h2"))
                Dependency(sb, "com.h2database", "h2", null, "runtime");
        }
        foreach (var d in dependencies.Where(d => !batchApplication || d.ArtifactId != "spring-boot-starter"))
            Dependency(sb, d.GroupId, d.ArtifactId, d.Version, d.Scope);
        sb.Append("  </dependencies>\n\n");
        sb.Append("  <build>\n");
        sb.Append("    <!-- The run folder is the source root: generated code is laid out by package from here. -->\n");
        sb.Append("    <sourceDirectory>${project.basedir}</sourceDirectory>\n");
        sb.Append("    <plugins>\n");
        sb.Append("      <plugin>\n        <groupId>org.apache.maven.plugins</groupId>\n        <artifactId>maven-compiler-plugin</artifactId>\n");
        sb.Append("        <version>3.14.1</version>\n");
        sb.Append("        <configuration>\n          <excludes>\n            <exclude>target/**</exclude>\n          </excludes>\n        </configuration>\n      </plugin>\n");
        if (mainClass is not null)
        {
            sb.Append("      <plugin>\n        <groupId>org.springframework.boot</groupId>\n        <artifactId>spring-boot-maven-plugin</artifactId>\n");
            sb.Append("        <configuration>\n          <mainClass>").Append(mainClass).Append("</mainClass>\n        </configuration>\n      </plugin>\n");
        }
        sb.Append("    </plugins>\n  </build>\n</project>\n");
        return sb.ToString();
    }

    private static void Dependency(StringBuilder sb, string groupId, string artifactId, string? version, string? scope)
    {
        sb.Append("    <dependency>\n      <groupId>").Append(groupId).Append("</groupId>\n");
        sb.Append("      <artifactId>").Append(artifactId).Append("</artifactId>\n");
        if (version is not null) sb.Append("      <version>").Append(version).Append("</version>\n");
        if (scope is not null) sb.Append("      <scope>").Append(scope).Append("</scope>\n");
        sb.Append("    </dependency>\n");
    }
}
