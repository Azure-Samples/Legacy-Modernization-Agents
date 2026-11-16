using System.Text;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Rekt;

// Local preprocess rules were written as Python regular expressions; existing files must keep their meaning.
public sealed class RektPreprocessorTests : IDisposable
{
    private readonly string _work = Path.Join(Path.GetTempPath(), $"rekt-preprocess-{Guid.NewGuid():N}");

    public RektPreprocessorTests() => Directory.CreateDirectory(Path.Join(_work, "out"));

    public void Dispose() => Directory.Delete(_work, recursive: true);

    [Theory]
    [InlineData(@"(A)(B)", @"\2\1", "AB", "BA")]
    [InlineData(@"(?P<first>A)(B)", @"\g<first>-\2-\g<2>-\1", "AB", "A-B-B-A")]
    [InlineData(@"(A)", @"\g<0>$1 costs $5", "A", "A$1 costs $5")]
    [InlineData(@"(A)", @"\1\n\t\\x\&", "A", "A\n\t\\x\\&")]
    [InlineData(@"(A)(B)(C)(D)(E)(F)(G)(H)(I)(J)", @"\10\g<1>0", "ABCDEFGHIJ", "JA0")]
    [InlineData(@"(?P<a>A)(?P=a)", @"<\g<a>>", "AA", "<A>")]
    [InlineData("(?x) A  # letter (not a group)\n (B)", @"[\1]", "AB", "[B]")]
    public void PythonReplacementTemplates_KeepTheirMeaning(string find, string replace, string input, string expected)
    {
        var result = PyRegex.Compile(find).Replace(input, PyRegex.TranslateReplacement(replace, find));

        result.Should().Be(expected);
    }

    [Fact]
    public void PythonOnlyPatternSyntax_IsTranslated()
    {
        PyRegex.Compile(@"AB\Z").IsMatch("AB\n").Should().BeFalse("Python's \\Z is the very end of the text");
        PyRegex.Compile(@"^A{,2}$").IsMatch("AA").Should().BeTrue();
        PyRegex.Compile(@"A\sB").IsMatch("A\u001cB").Should().BeTrue("Python's \\s includes the information separators");
        PyRegex.Flags(["MULTILINE", "IGNORECASE", "DOTALL", "VERBOSE"]).Should().Be(
            RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Singleline | RegexOptions.IgnorePatternWhitespace);
    }

    [Theory]
    [InlineData(@"(A)", @"\2", null)]
    [InlineData(@"(A)", @"\g<missing>", null)]
    [InlineData(@"(A)", @"\q", null)]
    [InlineData(@"(A)", @"\101", null)]
    [InlineData(@"\p{L}", "x", null)]
    [InlineData(@"A*+", "x", null)]
    [InlineData(@"(A)", "x", "ASCII")]
    [InlineData(@"(A)", "x", "multiline")]
    public void UntranslatableRules_AreReportedAndSkipped(string find, string replace, string? flag)
    {
        var rules = Path.Join(_work, "rules.json");
        var flags = flag is null ? "" : $", \"flags\": [\"{flag}\"]";
        File.WriteAllText(rules, "{\"programRewrites\": [" +
            $"{{\"find\": {Json(find)}, \"replace\": {Json(replace)}{flags}}}, " +
            "{\"find\": \"KEEP\", \"replace\": \"KEPT\", \"literal\": true}]}");
        var problems = new List<string>();

        var loaded = RektPreprocessor.LoadRules(rules, problems.Add);

        problems.Should().ContainSingle().Which.Should().StartWith("rule 1:");
        loaded.Should().ContainSingle().Which.Replace.Should().Be("KEPT");
    }

    [Fact]
    public void ProgramRules_RunAtTheirStage()
    {
        var rules = Path.Join(_work, "rules.json");
        File.WriteAllText(rules, """
            {"programRewrites": [
              {"find": "(?P<verb>move)\\s+(OLD-NAME)", "replace": "\\g<verb> NEW-NAME", "flags": ["IGNORECASE"]},
              {"find": "SPACES TO Z", "replace": "SPACE TO Z", "literal": true, "stage": "late"},
              {"find": "spaces TO Y", "replace": "spaces TO Z", "literal": true}
            ]}
            """);

        var result = RektPreprocessor.Program("           MOVE OLD-NAME TO X.\n           MOVE spaces TO Y.\n",
            _work, RektPreprocessor.LoadRules(rules));

        // Early rules see the source spelling; late rules see the normalised figurative constant.
        result.Should().Be("           MOVE NEW-NAME TO X.\n           MOVE SPACE TO Z.\n");
    }

    [Fact]
    public void CopybookCli_WritesOnlyRewrittenFiles_AndReadsCrlfAsUnchanged()
    {
        var unchanged = Write("PLAIN.cpy", "       01 WS-A PIC X.\r\n       01 WS-B PIC 9.\r\n");
        var anonymous = Write("ANON.cpy", "       01 WS-A.\n           05      PIC X.\n");

        Cli("copybook", "--source", unchanged, "--output-root", Path.Join(_work, "out")).ExitCode.Should().Be(1);
        Cli("copybook", "--source", anonymous, "--output-root", Path.Join(_work, "out")).ExitCode.Should().Be(0);

        Directory.GetFiles(Path.Join(_work, "out")).Select(Path.GetFileName).Should().Equal("ANON.cpy");
        File.ReadAllText(Path.Join(_work, "out", "ANON.cpy")).Should().Be("       01 WS-A.\n           05 FILLER PIC X.\n");
    }

    [Fact]
    public void ProgramCli_InsertsContinueBeforeAnElseThatFollowsOnlyComments()
    {
        var program = Write("estate/PROG.cbl", "           IF WS-A = 1\n      * nothing here\n           ELSE\n" +
                                               "               DISPLAY 'X'\n           END-IF.\n");

        var (code, _, _) = Cli("program", "--source", program, "--source-root", _work, "--output-root", Path.Join(_work, "out"));

        code.Should().Be(0);
        File.ReadAllText(Path.Join(_work, "out", "estate", "PROG.cbl")).Should().Be(
            "           IF WS-A = 1\n           CONTINUE\n      * nothing here\n           ELSE\n" +
            "               DISPLAY 'X'\n           END-IF.\n");
    }

    [Fact]
    public void CheckRulesCli_ReportsBadRules_AndRejectsFilesWithoutAProgramRewritesList()
    {
        var rules = Write("rules.json", "{\"programRewrites\": [{\"find\": \"(A)\", \"replace\": \"\\\\3\"}]}");
        var broken = Write("broken.json", "{\"rules\": []}");

        var (code, stdout, _) = Cli("check-rules", "--rules", rules);

        code.Should().Be(0);
        stdout.Should().Be($"  ⚠️  Ignoring preprocess rule 1: invalid group reference 3 in {rules}\n");
        Cli("check-rules", "--rules", broken).ExitCode.Should().Be(1);
    }

    private (int ExitCode, string StdOut, string StdErr) Cli(params string[] args) =>
        LmaCliProcess.Run(_work, null, ["rekt-preprocess", .. args]);

    private string Write(string relative, string text)
    {
        var path = Path.Join(_work, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, text, Encoding.Latin1);
        return path;
    }

    private static string Json(string text) => System.Text.Json.JsonSerializer.Serialize(text);
}
