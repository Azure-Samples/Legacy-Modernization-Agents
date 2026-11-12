using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

[Collection("EnvironmentSensitive")]
public sealed class JavaCompileGateTests : IDisposable
{
    private readonly string _root = Path.Combine(Path.GetTempPath(), $"java-gate-{Guid.NewGuid():N}");
    private readonly int _originalExit = Environment.ExitCode;

    [Fact]
    public void Parse_KeepsUniqueCompilerDiagnosticsAndSourceLocations()
    {
        var file = Path.Combine(_root, "com", "example", "Record.java");
        Directory.CreateDirectory(Path.GetDirectoryName(file)!);
        File.WriteAllText(file, "class Record {}");
        var output = $"[ERROR] {file}:[12,4] duplicate class: example.Record\n" +
            $"[ERROR] {file}:[12,4] duplicate class: example.Record\n";
        JavaCompileGate.Parse(output, _root).Should().Equal(
            new CompilerDiagnostic("com/example/Record.java", 12, 4, "javac", "duplicate class: example.Record"));
    }

    [Fact]
    public async Task Build_MissingProjectOrMavenIsNotAccepted()
    {
        Directory.CreateDirectory(_root);
        var missingProject = await JavaCompileGate.BuildAsync(_root, TimeSpan.FromSeconds(1));
        missingProject.Compiled.Should().BeFalse();
        missingProject.Measured.Should().BeFalse();
        missingProject.ExitCode.Should().BeNull();
        missingProject.FailureReason.Should().Be("No pom.xml in the generated project.");
        await File.WriteAllTextAsync(Path.Combine(_root, "pom.xml"), "<project/>");
        var missingMaven = await JavaCompileGate.BuildAsync(_root, TimeSpan.FromSeconds(1),
            Path.Combine(_root, "missing-maven"));
        missingMaven.Compiled.Should().BeFalse();
        missingMaven.FailureReason.Should().NotBeNullOrWhiteSpace();
    }

    [Theory]
    [InlineData(0, true)]
    [InlineData(9, false)]
    public async Task Build_UsesActualExitEvenWithoutJavacDiagnostics(int exitCode, bool compiles)
    {
        var executable = await FakeMavenAsync($"printf 'build output\\n'\nexit {exitCode}\n",
            $"echo build output\r\nexit /b {exitCode}\r\n");
        var result = await JavaCompileGate.BuildAsync(_root, TimeSpan.FromSeconds(5), executable);
        result.Compiled.Should().Be(compiles);
        result.Measured.Should().BeTrue();
        result.ExitCode.Should().Be(exitCode);
        result.Errors.Should().BeEmpty();
        (await File.ReadAllTextAsync(Path.Combine(_root, "java-build.log"))).Should().Contain("build output");
    }

    [Fact]
    public async Task Build_TimeoutAndCancellationDoNotBecomeSuccess()
    {
        var executable = await FakeMavenAsync("sleep 20\n", "ping -n 21 127.0.0.1 >nul\r\n");
        var timeout = await JavaCompileGate.BuildAsync(_root, TimeSpan.FromMilliseconds(100), executable);
        timeout.Compiled.Should().BeFalse();
        timeout.Measured.Should().BeFalse();
        timeout.FailureReason.Should().Contain("exceeded");
        using var cancellation = new CancellationTokenSource(TimeSpan.FromMilliseconds(100));
        await FluentActions.Awaiting(() => JavaCompileGate.RunAsync(_root, new(), NullLogger.Instance,
            cancellation.Token, executable)).Should().ThrowAsync<OperationCanceledException>();
        var persisted = JsonSerializer.Deserialize<JavaCompileResult>(
            await File.ReadAllTextAsync(Path.Combine(_root, "java-compile-status.json")))!;
        persisted.Compiled.Should().BeFalse();
        persisted.FailureReason.Should().Contain("cancelled");
        Environment.ExitCode.Should().Be(JavaCompileGate.FailureExitCode);
    }

    [Theory]
    [InlineData("package other; public class Record { public int getValue() { return 1; } }")]
    [InlineData("package example; class Record { public int getValue() { return 1; } }")]
    [InlineData("package example; public class Record { /* public int getValue() { return 1; } */ }")]
    public void Guard_RejectsPackageTypeOrApiLoss(string candidate)
    {
        JavaCompileGate.RejectCandidate(
            "package example; public class Record { public int getValue() { return 1; } }",
            candidate, preserveAllTypes: true).Should().NotBeNull();
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Run_RemeasuresRepairsAndRollsBackWithoutCompilerImprovement(bool improves)
    {
        var original = "public class Record { int value = broken; }";
        var candidate = "public class Record { int value = 1; }";
        var executable = await FakeMavenAsync(
            (improves ? "if ! grep -q broken Record.java; then exit 0; fi\n" : "") +
            "printf '[ERROR] Record.java:[1,1] cannot find symbol: broken\\n'\nexit 1\n",
            (improves ? "findstr broken Record.java >nul\r\nif errorlevel 1 exit /b 0\r\n" : "") +
            "echo [ERROR] Record.java:[1,1] cannot find symbol: broken\r\nexit /b 1\r\n");
        var source = Path.Combine(_root, "source.cpy");
        var generated = Path.Combine(_root, "Record.java");
        await File.WriteAllTextAsync(source, "       01 RECORD.\n          05 VALUE PIC 9 VALUE 1.");
        await File.WriteAllTextAsync(generated, original);
        var calls = 0;
        var result = await JavaCompileGate.RunAsync(_root, new CompileGateSettings { MaxRepairRounds = 1 },
            NullLogger.Instance, executable: executable,
            generatedFiles: [new CodeFile { FilePath = generated, OriginalSourcePath = source, OriginalCobolFileName = "source.cpy" }],
            repair: request =>
            {
                request.Source.Should().Contain("VALUE 1");
                request.RelatedFiles.Should().ContainKey(Path.Combine(_root, "pom.xml"));
                request.CompilerDiagnostics.Should().ContainSingle()
                    .Which.Message.Should().Be("cannot find symbol: broken");
                calls++;
                return Task.FromResult<IReadOnlyDictionary<string, string>?>(
                    new Dictionary<string, string> { [generated] = candidate });
            });
        calls.Should().Be(1);
        result.Compiled.Should().Be(improves);
        result.Measured.Should().BeTrue();
        result.RepairRounds.Should().Be(1);
        (await File.ReadAllTextAsync(generated)).Should().Be(improves ? candidate : original);
        var persisted = JsonSerializer.Deserialize<JavaCompileResult>(
            await File.ReadAllTextAsync(Path.Combine(_root, "java-compile-status.json")))!;
        persisted.Compiled.Should().Be(improves);
        persisted.ExitCode.Should().Be(improves ? 0 : 1);
        persisted.Rounds.Should().HaveCount(2);
        persisted.Rounds[0].Errors.Should().ContainSingle()
            .Which.Message.Should().Be("cannot find symbol: broken");
        persisted.Rounds[1].RolledBack.Should().Be(!improves);
        (await File.ReadAllTextAsync(Path.Combine(_root, "java-build.round-0.log")))
            .Should().Contain("cannot find symbol: broken");
    }

    [Fact]
    public void Guard_RejectsRemovalOfPublicField()
    {
        JavaCompileGate.RejectCandidate("public class Record { public int value; }",
            "public class Record {}").Should().Be("public or protected fields lost");
    }

    [Fact]
    public async Task Run_CancellationDuringCandidateBuildRestoresOriginalAndPersistsCancellation()
    {
        var executable = await FakeMavenAsync(
            "if grep -q candidate Record.java; then printf started > candidate-started; sleep 20; fi\n" +
            "printf '[ERROR] Record.java:[1,1] original failure\\n'\nexit 1\n",
            "findstr candidate Record.java >nul\r\nif not errorlevel 1 (\r\necho started > candidate-started\r\n" +
            "ping -n 21 127.0.0.1 >nul\r\n)\r\necho [ERROR] Record.java:[1,1] original failure\r\nexit /b 1\r\n");
        var generated = Path.Combine(_root, "Record.java");
        var source = Path.Combine(_root, "RECORD.cpy");
        const string original = "public class Record {}";
        await File.WriteAllTextAsync(generated, original);
        await File.WriteAllTextAsync(source, "       01 RECORD.");
        using var cancellation = new CancellationTokenSource(TimeSpan.FromSeconds(10));
        var run = JavaCompileGate.RunAsync(_root, new CompileGateSettings { MaxRepairRounds = 1 },
            NullLogger.Instance, cancellation.Token, executable,
            repair: _ => Task.FromResult<IReadOnlyDictionary<string, string>?>(
                new Dictionary<string, string> { [generated] = "public class Record { int candidate; }" }),
            generatedFiles: [new CodeFile { FilePath = generated, OriginalSourcePath = source }]);
        while (!File.Exists(Path.Combine(_root, "candidate-started")))
        {
            cancellation.Token.ThrowIfCancellationRequested();
            await Task.Delay(20, cancellation.Token);
        }
        cancellation.Cancel();
        await FluentActions.Awaiting(() => run).Should().ThrowAsync<OperationCanceledException>();
        (await File.ReadAllTextAsync(generated)).Should().Be(original);
        var status = JsonSerializer.Deserialize<JavaCompileResult>(
            await File.ReadAllTextAsync(Path.Combine(_root, "java-compile-status.json")))!;
        status.Compiled.Should().BeFalse();
        status.FailureReason.Should().Contain("cancelled");
        status.RepairRounds.Should().Be(1);
        status.Rounds.Should().ContainSingle().Which.Errors.Should().ContainSingle()
            .Which.Message.Should().Be("original failure");
    }

    [Fact]
    public async Task Run_DependencyResolutionFailureIsNotRepairableOrSuccessful()
    {
        var executable = await FakeMavenAsync("printf 'Could not resolve dependencies\\n'\nexit 1\n",
            "echo Could not resolve dependencies\r\nexit /b 1\r\n");
        var result = await JavaCompileGate.RunAsync(_root, new(), NullLogger.Instance, executable: executable,
            repair: _ => throw new InvalidOperationException("Repair must not run for dependency resolution."));
        result.Compiled.Should().BeFalse();
        result.ExitCode.Should().Be(1);
        result.RepairRounds.Should().Be(0);
        Environment.ExitCode.Should().Be(JavaCompileGate.FailureExitCode);
    }

    private async Task<string> FakeMavenAsync(string unix, string windows)
    {
        Directory.CreateDirectory(_root);
        await File.WriteAllTextAsync(Path.Combine(_root, "pom.xml"), "<project/>");
        var executable = Path.Combine(_root, OperatingSystem.IsWindows() ? "fake-maven.cmd" : "fake-maven");
        await File.WriteAllTextAsync(executable, OperatingSystem.IsWindows()
            ? "@echo off\r\n" + windows : "#!/bin/sh\n" + unix);
        if (!OperatingSystem.IsWindows())
            File.SetUnixFileMode(executable, UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute);
        return executable;
    }

    public void Dispose()
    {
        Environment.ExitCode = _originalExit;
        if (Directory.Exists(_root)) Directory.Delete(_root, true);
    }
}
