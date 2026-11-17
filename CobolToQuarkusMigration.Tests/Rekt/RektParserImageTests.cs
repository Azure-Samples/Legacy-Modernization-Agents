using System.Diagnostics;
using System.Text.Json.Nodes;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Rekt;

// Runs the real parser image, so it only runs when REKT_TEST_IMAGE names one.
public sealed class RektParserImageTests : IDisposable
{
    private static readonly string? Image = Environment.GetEnvironmentVariable("REKT_TEST_IMAGE");
    private readonly string _root = Directory.CreateTempSubdirectory("rekt-parser-").FullName;

    public void Dispose() => Directory.Delete(_root, recursive: true);

    [ImageTheory]
    [InlineData("INQUIRE")]
    [InlineData("SET")]
    public void TerminalCommandsPreserveSurroundingControlFlow(string verb)
    {
        var name = $"TERMINAL-{verb}";
        var (source, output) = Folders();
        File.WriteAllText(Path.Join(source, $"{name}.cbl"), string.Join("\n",
            "       IDENTIFICATION DIVISION.",
            $"       PROGRAM-ID. {name}.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01 TERMINAL-ID PIC X(4).",
            "       01 TRANSLATION-MODE PIC S9(8) COMP.",
            "       01 RESPONSE-CODE PIC S9(8) COMP.",
            "       01 DETAIL-CODE PIC S9(8) COMP.",
            "       PROCEDURE DIVISION.",
            "       MAIN-LOGIC.",
            "           IF TRANSLATION-MODE = 1",
            $"              EXEC CICS {verb} TERMINAL(TERMINAL-ID)",
            "                   UCTRANST(TRANSLATION-MODE)",
            "                   RESP(RESPONSE-CODE)",
            "                   RESP2(DETAIL-CODE)",
            "              END-EXEC",
            "              MOVE 2 TO TRANSLATION-MODE",
            "           END-IF.",
            "           GOBACK.",
            ""));

        var (exitCode, log) = Parse(source, output, $"{name}.cbl", "BUILD_BASE_ANALYSIS WRITE_FLOW_AST", recovery: false);

        exitCode.Should().Be(0, log);
        var artifacts = FlowAsts(Path.Join(output, $"{name}.cbl.report"));
        artifacts.Should().ContainSingle(log);
        var nodes = Objects(JsonNode.Parse(File.ReadAllText(artifacts[0]))).ToList();
        nodes.Should().Contain(node => (string?)node["type"] == "PARAGRAPH");
        var texts = nodes.Select(node => (node["originalText"] is JsonValue v ? v.GetValue<string>() : "").ToUpperInvariant()).ToList();
        texts.Should().Contain(text => text.Contains($"EXEC CICS {verb}") && text.Contains("UCTRANST"));
        texts.Should().Contain(text => text.Contains("MOVE 2 TO TRANSLATION-MODE"));
        texts.Should().Contain(text => text.Contains("IF TRANSLATION-MODE"));
    }

    [ImageFact]
    public void RecoversKnownControlFlowAndPreservesMissingDiagnostics()
    {
        var (exitCode, log, report, source) = RunRecoveryFixture();

        exitCode.Should().Be(0, log);
        var diagnostics = JsonNode.Parse(File.ReadAllText(Path.Join(report, "parse-diagnostics.json")))!.AsArray();
        diagnostics.Should().Contain(d => (string?)d!["code"] == "missing copybook" && ((string?)d["message"] ?? "").Contains("ABSENTCB"));
        var artifacts = FlowAsts(report);
        artifacts.Should().ContainSingle();
        var ast = File.ReadAllText(artifacts[0]);
        ast.Should().Contain("KNOWN-VALUE").And.Contain("MOVE 2").And.Contain("\"IF");
        var data = Directory.GetFiles(Path.Join(report, "data_structures"), "*.json");
        data.Should().ContainSingle();
        File.ReadAllText(data[0]).Should().NotContain("ABSENTCB-STUB").And.NotContain("ABSENTCB-VAL");
        Directory.GetFileSystemEntries(source).Select(Path.GetFileName).Should().Equal("RECOVERY.cbl");
    }

    [ImageFact]
    public void StrictModeStillRejectsMissingDefinitions()
    {
        var (exitCode, _, report, _) = RunRecoveryFixture(recovery: false);

        exitCode.Should().NotBe(0);
        FlowAsts(report).Should().BeEmpty();
        File.Exists(Path.Join(report, "parse-diagnostics.json")).Should().BeTrue();
    }

    [ImageFact]
    public void RecoveryDoesNotAcceptSyntaxErrors()
    {
        var (exitCode, _, report, _) = RunRecoveryFixture(invalid: true);

        exitCode.Should().NotBe(0);
        FlowAsts(report).Should().BeEmpty();
    }

    [ImageFact]
    public void UnknownLayoutDoesNotBecomeOneByte()
    {
        var (exitCode, log, report, _) = RunRecoveryFixture(unknownLayout: true);

        exitCode.Should().NotBe(0);
        log.Should().Contain("Unknown data layout");
        var data = Path.Join(report, "data_structures");
        (Directory.Exists(data) ? Directory.GetFiles(data, "*.json") : []).Should().BeEmpty();
    }

    private (int ExitCode, string Log, string Report, string Source) RunRecoveryFixture(
        bool recovery = true, bool invalid = false, bool unknownLayout = false)
    {
        var (source, output) = Folders();
        var lines = new List<string>
        {
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. RECOVERY.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01 KNOWN-VALUE PIC 9.",
            "       COPY ABSENTCB.",
            "       PROCEDURE DIVISION.",
            "       MAIN-LOGIC.",
            "           IF KNOWN-VALUE = 1",
            "               MOVE 2 TO KNOWN-VALUE",
            "           END-IF.",
            "           GOBACK.",
        };
        if (invalid) lines.Insert(lines.Count - 1, "           MOVE TO.");
        if (unknownLayout) lines.Insert(5, "       01 UNKNOWN-LAYOUT.");
        File.WriteAllText(Path.Join(source, "RECOVERY.cbl"), string.Join("\n", lines) + "\n");

        var (exitCode, log) = Parse(source, output, "RECOVERY.cbl",
            "BUILD_BASE_ANALYSIS WRITE_FLOW_AST WRITE_CFG WRITE_DATA_STRUCTURES", recovery);
        return (exitCode, log, Path.Join(output, "RECOVERY.cbl.report"), source);
    }

    private (string Source, string Output) Folders() =>
        (Directory.CreateDirectory(Path.Join(_root, "source")).FullName,
         Directory.CreateDirectory(Path.Join(_root, "output")).FullName);

    private static (int ExitCode, string Log) Parse(string source, string output, string program, string commands, bool recovery)
    {
        var psi = new ProcessStartInfo("docker") { RedirectStandardOutput = true, RedirectStandardError = true };
        foreach (var arg in new[] { "run", "--rm", "--entrypoint", "java", "-v", $"{source}:/source:ro", "-v", $"{output}:/output" })
            psi.ArgumentList.Add(arg);
        if (recovery)
        {
            psi.ArgumentList.Add("-e");
            psi.ArgumentList.Add("REKT_ALLOW_MISSING_COPYBOOKS=true");
        }
        foreach (var arg in new[] { Image!, "-jar", "/app/smojol-cli.jar", "run", program, $"--commands={commands}",
                     "--srcDir=/source", "--copyBooksDir=/source", "--reportDir=/output", "--generation=PROGRAM" })
            psi.ArgumentList.Add(arg);

        using var process = Process.Start(psi)!;
        var stderr = process.StandardError.ReadToEndAsync();
        var stdout = process.StandardOutput.ReadToEnd();
        if (!process.WaitForExit(TimeSpan.FromSeconds(120)))
        {
            process.Kill(entireProcessTree: true);
            throw new TimeoutException($"docker run did not finish for {program}");
        }
        return (process.ExitCode, stdout + stderr.Result);
    }

    private static string[] FlowAsts(string report) =>
        Directory.Exists(report) ? Directory.GetFiles(report, "flow-ast-*.json", SearchOption.AllDirectories) : [];

    private static IEnumerable<JsonObject> Objects(JsonNode? node)
    {
        if (node is JsonObject obj)
        {
            yield return obj;
            foreach (var (_, child) in obj)
                foreach (var nested in Objects(child)) yield return nested;
        }
        else if (node is JsonArray array)
            foreach (var child in array)
                foreach (var nested in Objects(child)) yield return nested;
    }

    private sealed class ImageFactAttribute : FactAttribute
    {
        public ImageFactAttribute()
        {
            if (string.IsNullOrEmpty(Image)) Skip = "Set REKT_TEST_IMAGE to run parser integration tests";
        }
    }

    private sealed class ImageTheoryAttribute : TheoryAttribute
    {
        public ImageTheoryAttribute()
        {
            if (string.IsNullOrEmpty(Image)) Skip = "Set REKT_TEST_IMAGE to run parser integration tests";
        }
    }
}
