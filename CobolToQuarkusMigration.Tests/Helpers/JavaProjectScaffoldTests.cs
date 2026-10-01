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
    public void RewritesItsOwnBuild()
    {
        JavaProjectScaffold.Write(_run, "com.x", "com.x.jobs.JclBatchApplication");
        Write("com/x/A.java", "import jakarta.inject.Inject;\n");

        JavaProjectScaffold.Write(_run, "com.x", "com.x.jobs.JclBatchApplication").Should().HaveCount(2);

        File.ReadAllText(Path.Join(_run, "pom.xml")).Should().Contain("jakarta.inject-api");
    }
}
