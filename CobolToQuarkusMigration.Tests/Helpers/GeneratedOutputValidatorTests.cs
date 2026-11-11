using System.Text.Json;
using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

[Collection("EnvironmentSensitive")]
public sealed class GeneratedOutputValidatorTests : IDisposable
{
    private readonly string _root = Path.Combine(Path.GetTempPath(), $"output-validation-{Guid.NewGuid():N}");
    private readonly int _originalExit = Environment.ExitCode;
    private readonly string? _originalGate = Environment.GetEnvironmentVariable("ON_LOW_SCORE");
    private readonly string? _originalThreshold = Environment.GetEnvironmentVariable("MIN_PROGRAM_SCORE");
    private readonly string? _originalRounds = Environment.GetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS");
    private string SourceRoot => Path.Combine(_root, "source");
    private string OutputRoot => Path.Combine(_root, "generated");

    public GeneratedOutputValidatorTests()
    {
        Directory.CreateDirectory(SourceRoot);
        Directory.CreateDirectory(OutputRoot);
        Environment.ExitCode = 0;
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "stop");
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", "0.75");
        Environment.SetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS", "0");
    }

    [Fact]
    public async Task Run_OrdinaryJavaScaffoldsBuildsAndMapsActualWriterPath()
    {
        var (source, file) = await WriteAsync("package authoritative.path;\npublic class Record { int neededValue = 1; }");
        var executable = await MavenAsync("exit 0\n", "exit /b 0\r\n");
        var result = await ValidateAsync([source], [file], executable);
        result.Accepted.Should().BeTrue();
        result.JavaBuild!.Compiled.Should().BeTrue();
        file.FilePath.Should().Be(Path.Combine(OutputRoot, "authoritative", "path", "Record.java"));
        var pom = await File.ReadAllTextAsync(Path.Combine(OutputRoot, "pom.xml"));
        pom.Should().Contain("<maven.compiler.release>17</maven.compiler.release>").And.NotContain("spring-boot");
        using var map = JsonDocument.Parse(await File.ReadAllTextAsync(Path.Combine(OutputRoot, "generated-source-map.json")));
        var row = map.RootElement.EnumerateArray().Should().ContainSingle().Which;
        row.GetProperty("source").GetString().Should().Be("RECORD.cpy");
        row.GetProperty("generated").GetString().Should().Be("authoritative/path/Record.java");
        var report = await ReadParityAsync();
        report.Programs.Should().ContainSingle().Which.Score.Should().Be(1);
        result.Markdown.Should().Contain("Compilation does not establish conversion completeness");
    }

    [Fact]
    public async Task Run_DefaultExecutionUsesExplicitRepositoryRootInsteadOfSkippingParity()
    {
        var originalRoot = Environment.GetEnvironmentVariable("REPO_ROOT");
        try
        {
            Environment.SetEnvironmentVariable("REPO_ROOT", _root);
            var (source, file) = await WriteAsync("public class Record { int neededValue = 1; }");
            var executable = await MavenAsync("exit 0\n", "exit /b 0\r\n");
            var result = await GeneratedOutputValidator.RunAsync([file], [source], SourceRoot, OutputRoot,
                TargetLanguage.Java, new CompileGateSettings { MaxRepairRounds = 0 }, NullLogger.Instance,
                execution: new MigrationExecutionOptions { MavenExecutable = executable });
            result.Accepted.Should().BeTrue();
            (await ReadParityAsync()).Programs.Should().ContainSingle().Which.Score.Should().Be(1);
        }
        finally
        {
            Environment.SetEnvironmentVariable("REPO_ROOT", originalRoot);
        }
    }

    [Fact]
    public async Task Run_CompileSuccessCannotConcealMissingConversionEvenWithWarnPolicy()
    {
        var (source, file) = await WriteAsync("public class Record { int neededValue = 1; }");
        var missing = new CobolFile { FileName = "MISSING.cpy", FilePath = Path.Combine(SourceRoot, "MISSING.cpy") };
        await File.WriteAllTextAsync(missing.FilePath, "       01 MISSING.\n          05 MISSING-VALUE PIC 9.");
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", "warn");
        var executable = await MavenAsync("exit 0\n", "exit /b 0\r\n");
        var result = await ValidateAsync([source, missing], [file], executable);
        result.JavaBuild!.Compiled.Should().BeTrue();
        result.Accepted.Should().BeFalse();
        result.Markdown.Should().Contain("Conversion incomplete");
        var missingResult = (await ReadParityAsync()).Programs.Single(program => program.Program == "MISSING.cpy");
        missingResult.Score.Should().Be(0);
        missingResult.Gaps.Should().ContainSingle().Which.Axis.Should().Be("file");
        Environment.ExitCode.Should().Be(ConversionParityPostPass.LowScoreExitCode);
    }

    [Fact]
    public async Task Run_FinalParityRemeasuresCompilerRepairRatherThanReportingOldScore()
    {
        var (source, file) = await WriteAsync("public class Record { int neededValue = broken; }");
        var executable = await MavenAsync(
            "if ! grep -q broken com/example/Record.java; then exit 0; fi\n" +
            "printf '[ERROR] com/example/Record.java:[1,1] cannot find symbol: broken\\n'\nexit 1\n",
            "findstr broken com\\example\\Record.java >nul\r\nif errorlevel 1 exit /b 0\r\n" +
            "echo [ERROR] com/example/Record.java:[1,1] cannot find symbol: broken\r\nexit /b 1\r\n");
        var calls = 0;
        var result = await ValidateAsync([source], [file], executable, request =>
        {
            request.CompilerDiagnostics.Should().ContainSingle();
            request.Source.Should().Contain("NEEDED-VALUE");
            calls++;
            return Task.FromResult<IReadOnlyDictionary<string, string>?>(
                new Dictionary<string, string> { [file.FilePath] = "public class Record { int replacement = 1; }" });
        });
        calls.Should().Be(1);
        result.JavaBuild!.Compiled.Should().BeTrue();
        result.Accepted.Should().BeFalse();
        var report = await ReadParityAsync();
        report.Programs.Should().ContainSingle().Which.Score.Should().Be(0);
        report.Programs.Single().Failed.Should().BeTrue();
        report.Programs.Single().LostAxes.Should().Contain("dataFields");
        (await File.ReadAllTextAsync(file.FilePath)).Should().Contain("replacement").And.NotContain("neededValue");
    }

    private async Task<(CobolFile Source, CodeFile File)> WriteAsync(string code)
    {
        var sourcePath = Path.Combine(SourceRoot, "RECORD.cpy");
        var content = "       01 RECORD.\n          05 NEEDED-VALUE PIC 9 VALUE 1.\n";
        await File.WriteAllTextAsync(sourcePath, content);
        var source = new CobolFile { FileName = "RECORD.cpy", FilePath = sourcePath, Content = content };
        var file = new JavaFile
        {
            FileName = "Record.java", ClassName = "Record", PackageName = "com.example", Content = code,
            OriginalCobolFileName = source.FileName, OriginalSourcePath = sourcePath
        };
        file.FilePath = await new FileHelper(NullLogger<FileHelper>.Instance).SaveJavaFileAsync(file, OutputRoot);
        return (source, file);
    }

    private Task<GeneratedOutputValidationResult> ValidateAsync(IReadOnlyList<CobolFile> sources,
        IReadOnlyList<CodeFile> files, string executable,
        Func<ParityRepairRequest, Task<IReadOnlyDictionary<string, string>?>>? repair = null) =>
        GeneratedOutputValidator.RunAsync(files, sources, SourceRoot, OutputRoot, TargetLanguage.Java,
            new CompileGateSettings { MaxRepairRounds = 1 }, NullLogger.Instance, repair,
            new MigrationExecutionOptions { RepositoryRoot = _root, MavenExecutable = executable });

    private async Task<ConversionParityReport> ReadParityAsync() =>
        JsonSerializer.Deserialize<ConversionParityReport>(
            await File.ReadAllTextAsync(Path.Combine(OutputRoot, ConversionParityPostPass.ArtifactName)))!;

    private async Task<string> MavenAsync(string unix, string windows)
    {
        var path = Path.Combine(OutputRoot, OperatingSystem.IsWindows() ? "fake-maven.cmd" : "fake-maven");
        await File.WriteAllTextAsync(path, OperatingSystem.IsWindows() ? "@echo off\r\n" + windows : "#!/bin/sh\n" + unix);
        if (!OperatingSystem.IsWindows())
            File.SetUnixFileMode(path, UnixFileMode.UserRead | UnixFileMode.UserWrite | UnixFileMode.UserExecute);
        return path;
    }

    public void Dispose()
    {
        Environment.ExitCode = _originalExit;
        Environment.SetEnvironmentVariable("ON_LOW_SCORE", _originalGate);
        Environment.SetEnvironmentVariable("MIN_PROGRAM_SCORE", _originalThreshold);
        Environment.SetEnvironmentVariable("PARITY_REPAIR_MAX_ROUNDS", _originalRounds);
        if (Directory.Exists(_root)) Directory.Delete(_root, true);
    }
}
