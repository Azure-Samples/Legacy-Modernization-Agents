using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class JavaProjectScaffoldTests : IDisposable
{
    private readonly string _run = Path.Join(Path.GetTempPath(), "java-scaffold-" + Guid.NewGuid().ToString("N"));

    public JavaProjectScaffoldTests() => Directory.CreateDirectory(_run);

    public void Dispose()
    {
        if (Directory.Exists(_run)) Directory.Delete(_run, recursive: true);
    }

    private void Write(string relative, string content)
    {
        var path = Path.Join(_run, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, content);
    }

    [Fact]
    public void OrdinaryConversionWritesCompilerBuildWithoutIntroducingBatchRuntime()
    {
        Write("com/x/A.java", "package com.x; public class A {}");
        JavaProjectScaffold.Write(_run, "com.x").Should().ContainSingle()
            .Which.Should().Be(Path.Join(_run, "pom.xml"));
        var pom = File.ReadAllText(Path.Join(_run, "pom.xml"));
        pom.Should().Contain("<maven.compiler.release>17</maven.compiler.release>")
            .And.Contain("<sourceDirectory>${project.basedir}</sourceDirectory>")
            .And.NotContain("spring-boot")
            .And.NotContain("<mainClass>");
        File.Exists(Path.Join(_run, "application.properties")).Should().BeFalse();
    }

    [Fact]
    public void OrdinaryConversionPreservesExistingBatchBuildAndUserPom()
    {
        JavaProjectScaffold.Write(_run, "com.x", "com.x.jobs.JclBatchApplication");
        var batch = File.ReadAllText(Path.Join(_run, "pom.xml"));
        JavaProjectScaffold.Write(_run, "com.x").Should().BeEmpty();
        File.ReadAllText(Path.Join(_run, "pom.xml")).Should().Be(batch);
        Write("pom.xml", "<project>custom</project>");
        JavaProjectScaffold.Write(_run, "com.x").Should().BeEmpty();
        File.ReadAllText(Path.Join(_run, "pom.xml")).Should().Be("<project>custom</project>");
    }

    [Fact]
    public void DeclaresOnlyTheDependenciesTheCodeImports()
    {
        Write("com/x/A.java", "package com.x;\nimport jakarta.persistence.Entity;\nimport io.quarkus.hibernate.orm.panache.PanacheEntity;\n");
        Write("target/generated-sources/B.java", "import jakarta.ws.rs.GET;\n");

        JavaProjectScaffold.Write(_run, "com.x", "com.x.jobs.JclBatchApplication");

        var pom = File.ReadAllText(Path.Join(_run, "pom.xml"));
        pom.Should().Contain("<artifactId>spring-boot-starter-batch</artifactId>")
            .And.Contain("<artifactId>jakarta.persistence-api</artifactId>")
            .And.Contain("<artifactId>quarkus-hibernate-orm-panache</artifactId>")
            .And.Contain("<mainClass>com.x.jobs.JclBatchApplication</mainClass>")
            .And.Contain("<exclude>target/**</exclude>")
            .And.NotContain("jakarta.ws.rs-api")
            .And.NotContain("jakarta.inject-api");
        File.ReadAllText(Path.Join(_run, "application.properties")).Should().Contain("jcl.datasets.root=");
    }

    [Fact]
    public void LeavesAHandWrittenBuildAlone()
    {
        Write("pom.xml", "<project>mine</project>");
        Write("application.properties", "jcl.datasets.root=/data\n");

        JavaProjectScaffold.Write(_run, "com.x", "com.x.jobs.JclBatchApplication").Should().BeEmpty();

        File.ReadAllText(Path.Join(_run, "pom.xml")).Should().Be("<project>mine</project>");
        File.ReadAllText(Path.Join(_run, "application.properties")).Should().Be("jcl.datasets.root=/data\n");
    }

    [Fact]
    public void SpringTargetWritesBootApplicationAndUpdatesItsBuildWithoutIntroducingBatchOrQuarkus()
    {
        JavaProjectScaffold.Write(_run, "com.x", framework: "Spring");
        Write("com/x/AccountService.java", """
            package com.x;
            import org.springframework.stereotype.Service;
            import org.springframework.jdbc.core.JdbcTemplate;
            import org.springframework.transaction.annotation.Transactional;
            import org.springframework.web.bind.annotation.RestController;
            """);
        JavaProjectScaffold.Write(_run, "com.x", framework: "Spring");

        var pom = File.ReadAllText(Path.Join(_run, "pom.xml"));
        pom.Should().Contain("spring-boot-starter-parent")
            .And.Contain("spring-boot-starter-web")
            .And.Contain("<mainClass>com.x.ModernizedApplication</mainClass>")
            .And.NotContain("spring-boot-starter-batch")
            .And.NotContain("quarkus")
            .And.NotContain("<artifactId>h2</artifactId>");
        var document = System.Xml.Linq.XDocument.Parse(pom);
        var ns = document.Root!.Name.Namespace;
        document.Descendants(ns + "artifactId").Count(n => n.Value == "spring-boot-starter-jdbc")
            .Should().Be(1);
        File.ReadAllText(Path.Join(_run, "com/x/ModernizedApplication.java"))
            .Should().Contain("@SpringBootApplication").And.Contain("SpringApplication.run");
        File.Exists(Path.Join(_run, "application.properties")).Should().BeFalse();
    }

    [Fact]
    public void ImportedH2DriverIsACompileDependencyWithoutDuplicatingTheBatchRuntimeDependency()
    {
        Write("com/x/A.java", "package com.x;\nimport org.h2.jdbcx.JdbcDataSource;\npublic class A {}");
        foreach (var mainClass in new string?[] { null, "com.x.jobs.JclBatchApplication" })
        {
            JavaProjectScaffold.Write(_run, "com.x", mainClass, framework: "Spring");
            var document = System.Xml.Linq.XDocument.Parse(File.ReadAllText(Path.Join(_run, "pom.xml")));
            var ns = document.Root!.Name.Namespace;
            var dependency = document.Descendants(ns + "dependency")
                .Where(element => element.Element(ns + "artifactId")?.Value == "h2")
                .Should().ContainSingle().Subject;
            dependency.Element(ns + "scope").Should().BeNull();
            dependency.Element(ns + "version")!.Value.Should().Be("2.3.232");
        }
    }

    [Fact]
    public void SpringTargetDoesNotModifyAnExistingUserProject()
    {
        Write("pom.xml", "<project>custom</project>");
        JavaProjectScaffold.Write(_run, "com.x", framework: "Spring").Should().BeEmpty();
        File.ReadAllText(Path.Join(_run, "pom.xml")).Should().Be("<project>custom</project>");
        File.Exists(Path.Join(_run, "com/x/ModernizedApplication.java")).Should().BeFalse();
    }

    [Fact]
    public void RewritesItsOwnBuild()
    {
        JavaProjectScaffold.Write(_run, "com.x", "com.x.jobs.JclBatchApplication");
        Write("com/x/A.java", "import jakarta.inject.Inject;\n");

        JavaProjectScaffold.Write(_run, "com.x", "com.x.jobs.JclBatchApplication").Should().HaveCount(2);

        File.ReadAllText(Path.Join(_run, "pom.xml")).Should().Contain("jakarta.inject-api");
    }
}
