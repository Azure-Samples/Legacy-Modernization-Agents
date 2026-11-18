using System.Text;
using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Tests.Doctor;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Rekt;

// Drives tools/preprocess-for-rekt.sh end to end, so the shell wrapper and the CLI it forwards to are pinned together.
public sealed class PreprocessForRektScriptTests : IDisposable
{
    // Mirrors StubCopybookCatalog.Marker and REKT_STUB_MARKER in doctor.sh.
    private const string Marker = "AUTO-GENERATED STUB COPYBOOK";
    private static readonly Encoding Latin1 = Encoding.Latin1;

    private readonly DirectoryInfo _work = Directory.CreateTempSubdirectory("rekt-preprocess-");
    private readonly string _source;

    public PreprocessForRektScriptTests()
    {
        _source = Path.Join(_work.FullName, "source with 'quotes and spaces");
        Directory.CreateDirectory(_source);
    }

    public void Dispose()
    {
        try { _work.Delete(recursive: true); }
        catch (IOException) { }
        catch (UnauthorizedAccessException) { }
    }

    [Fact]
    public void HandlesQuotesAndSpacesInProgramAndCopybookPaths()
    {
        const string copybookName = "quoted ' copybook.cpy";
        const string programName = "quoted ' program.cbl";
        var copybook = Write($"copybooks 'quoted'/{copybookName}", "       01  SAMPLE-FIELD COMP-2.\n");
        Write($"programs 'quoted'/{programName}", Lines(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. QUOTEDP.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-FLAG PIC 9.",
            "       PROCEDURE DIVISION.",
            "           MOVE 0(1) TO WS-FLAG.",
            "           GOBACK."));

        var result = RunPreprocessor();

        result.StdOut.Should().Contain("Preprocessed 1 program(s) and 0 copybook(s)");
        File.Exists(Path.Join(_source, ".preprocessed", copybookName)).Should().BeFalse();
        Read(copybook).Should().Contain("SAMPLE-FIELD COMP-2.");
        Preprocessed($"programs 'quoted'/{programName}").Should().Contain("MOVE 0 TO WS-FLAG.");
    }

    [Fact]
    public void PreservesFloatingPointUsageInProgramsAndCopybooks()
    {
        const string declarations =
            "       01 SHORT-FLOAT COMP-1.\n" +
            "       01 LONG-FLOAT COMP-2.\n" +
            "       01 SHORT-ALIAS COMPUTATIONAL-1.\n" +
            "       01 LONG-ALIAS COMPUTATIONAL-2.\n";
        Write("FLOATS.cbl",
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. FLOATS.\n" +
            "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n" +
            declarations + "       01 WS-FLAG PIC 9.\n" +
            "       PROCEDURE DIVISION.\n           MOVE 0(1) TO WS-FLAG.\n           GOBACK.\n");
        Write("FLOATS.cpy", declarations);

        RunPreprocessor();

        foreach (var filename in new[] { "FLOATS.cbl", "FLOATS.cpy" })
        {
            var output = Staged(filename);
            foreach (var declaration in declarations.Split('\n', StringSplitOptions.RemoveEmptyEntries))
                output.Should().Contain(declaration);
            output.Should().NotContain("PIC X(4)").And.NotContain("PIC X(8)");
        }
    }

    [Fact]
    public void DuplicateProgramRewrites_PreserveSourceRelativeIdentity()
    {
        foreach (var area in new[] { "one", "two" })
            Write($"{area}/SAMPLE.cbl",
                "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. SAMPLE.\n" +
                "       PROCEDURE DIVISION.\n           MOVE 0(1) TO WS-FLAG.\n" +
                $"           DISPLAY '{area}'.\n           GOBACK.\n");

        RunPreprocessor();

        foreach (var area in new[] { "one", "two" })
        {
            var output = Path.Join(_source, ".preprocessed", area, "SAMPLE.cbl");
            Read(output).Should().Contain("MOVE 0 TO WS-FLAG.").And.Contain($"DISPLAY '{area}'.");
            DoctorFixture.ShouldBeSamePath(OriginalSourcePath(output),
                DoctorFixture.RealPath(Path.Join(_source, area, "SAMPLE.cbl")));
        }
        File.Exists(Path.Join(_source, ".preprocessed/SAMPLE.cbl")).Should().BeFalse();
        var checkedIdentity = LmaCliProcess.Run(Path.Join(_work.FullName, "cli"), null,
            "rekt-source-identity", _source, Path.Join(_source, ".preprocessed"));
        checkedIdentity.ExitCode.Should().Be(0, checkedIdentity.StdOut + checkedIdentity.StdErr);
    }

    [Fact]
    public void SqlCursorDeclarationAndItsQuery_AreNotCommentedOut()
    {
        const string declaration =
            "           EXEC SQL DECLARE TEST-CURSOR CURSOR FOR\n" +
            "               SELECT CUSTOMER_NUMBER FROM CUSTOMER\n" +
            "               WHERE CUSTOMER_NUMBER = :WS-CUSTOMER\n" +
            "           END-EXEC.\n";
        const string original =
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. CURSORP.\n" +
            "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n" +
            "       01 WS-CUSTOMER PIC X(10).\n" + declaration +
            "       PROCEDURE DIVISION.\n" +
            "           EXEC SQL OPEN TEST-CURSOR END-EXEC.\n           GOBACK.\n";
        var source = Write("cursor.cbl", original);

        RunPreprocessor();

        var transformed = Staged("cursor.cbl");
        transformed.Should().Contain(declaration).And.NotContain("*SQL>");
        Read(source).Should().Be(original);
    }

    [Fact]
    public void AsyncCicsCommandsAndContainerLengths_ArePreserved()
    {
        const string commands =
            "           EXEC CICS PUT CONTAINER(CONT-NAME) FROM(PAYLOAD)\n" +
            "               FLENGTH(DATA-LENGTH) CHANNEL(CHAN-NAME) END-EXEC.\n" +
            "           EXEC CICS RUN TRANSID(TRAN-NAME) CHANNEL(CHAN-NAME)\n" +
            "               CHILD(CHILD-TOKEN) END-EXEC.\n" +
            "           EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n" +
            "           EXEC CICS FETCH ANY(CHILD-TOKEN) CHANNEL(CHAN-NAME)\n" +
            "               NOSUSPEND COMPSTATUS(CHILD-STATUS) END-EXEC.\n" +
            "           EXEC CICS GET CONTAINER(CONT-NAME) INTO(PAYLOAD)\n" +
            "               FLENGTH(DATA-LENGTH) CHANNEL(CHAN-NAME) END-EXEC.\n" +
            "           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)\n" +
            "               YYMMDD(FORMAT-DATE) DATESEP('.') END-EXEC.\n" +
            "           IF CHILD-STATUS = DFHVALUE(NORMAL) CONTINUE END-IF.\n" +
            "           IF CHILD-STATUS = DFHVALUE(ABEND) CONTINUE END-IF.\n" +
            "           IF CHILD-STATUS = DFHVALUE(SECERROR) CONTINUE END-IF.\n";
        const string original =
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. ASYNCP.\n" +
            "       PROCEDURE DIVISION.\n" + commands +
            "           MOVE 0(1) TO WS-FLAG.\n           GOBACK.\n";
        var source = Write("async.cbl", original);

        RunPreprocessor();

        Preprocessed("async.cbl").Should().Contain(commands);
        Read(source).Should().Be(original);
    }

    [Fact]
    public void CicsInquireAssociationAndOriginFields_ArePreserved()
    {
        const string command =
            "           EXEC CICS INQUIRE ASSOCIATION(EIBTASKN)\n" +
            "               ODAPPLID(SUBPGM-APPLID)\n" +
            "               ODUSERID(SUBPGM-USERID)\n" +
            "               ODFACILNAME(SUBPGM-FACILITY-NAME)\n" +
            "               ODNETWORKID(SUBPGM-NETWRK-ID)\n" +
            "               ODFACILTYPE(SUBPGM-FACILTYPE)\n" +
            "           END-EXEC.\n";
        const string original =
            "       IDENTIFICATION DIVISION.\n" +
            "       PROGRAM-ID. ASSOCIATIONP.\n" +
            "       PROCEDURE DIVISION.\n" + command +
            "           MOVE 0(1) TO WS-FLAG.\n" +
            "           GOBACK.\n";
        var source = Write("association.cbl", original);

        RunPreprocessor();

        Preprocessed("association.cbl").Should().Contain(command);
        Read(source).Should().Be(original);
    }

    [Fact]
    public void PreservesUnsupportedInConditionsAsOpaque()
    {
        const string arithmeticIf = "           IF WS-VALUE IN WS-GROUP + 1";
        const string arithmeticAnd = "              AND WS-OTHER = 1";
        const string comparisonIf = "           IF WS-VALUE IN WS-GROUP > 1";
        const string opaqueIf = "           IF REKT-OPAQUE-IN-CONDITION";
        Write("unsupported-in.cbl", Lines(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. INQUAL.",
            "       PROCEDURE DIVISION.",
            arithmeticIf,
            arithmeticAnd,
            "               DISPLAY 'ARITH'.",
            "           END-IF.",
            comparisonIf,
            "               DISPLAY 'COMPARE'.",
            "           END-IF.",
            "           GOBACK."));

        RunPreprocessor();
        var output = Preprocessed("unsupported-in.cbl");

        output.Should().NotContain("IF TRUE");
        Count(output, opaqueIf).Should().Be(2);
        output.Should().Contain(string.Join('\n', AsCobolComment(arithmeticIf), AsCobolComment(arithmeticAnd), opaqueIf));
        output.Should().Contain(string.Join('\n', AsCobolComment(comparisonIf), opaqueIf));
    }

    [Fact]
    public void MissingCopybooks_AreNotSynthesisedEvenWithLegacyOptIn()
    {
        Write("sqlprog.cbl", Lines(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. SQLPROG.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "           EXEC SQL INCLUDE SQLCA END-EXEC.",
            "       COPY ABSENTCB.",
            "       PROCEDURE DIVISION.",
            "           GOBACK."));

        RunPreprocessor(stubs: true);

        Preprocessed("SQLCA.cpy").Should().NotContain(Marker).And.Contain("SQLCODE");
        File.Exists(Path.Join(_source, ".preprocessed", "ABSENTCB.cpy")).Should().BeFalse();
    }

    [Fact]
    public void UnverifiedGeneratedCandidate_DoesNotSuppressBundledDefinition()
    {
        var candidate = Write("copy-generated/SQLCA.cpy", "       01 SQLCA PIC X.\n");

        RunPreprocessor();

        var bundled = Path.Join(DoctorFixture.RepoRoot, "tools", "system-copybooks", "SQLCA.cpy");
        var output = Path.Join(_source, ".preprocessed", "SQLCA.cpy");
        File.ReadAllBytes(output).Should().Equal(File.ReadAllBytes(bundled));
        DoctorFixture.ShouldBeSamePath(OriginalSourcePath(output), DoctorFixture.RealPath(bundled));
        Read(candidate).Should().Be("       01 SQLCA PIC X.\n");
    }

    [Fact]
    public void SuppliedDefinition_TakesPrecedenceOverBundledDefinition()
    {
        var definition = Write("copy/SQLCA.cpy", "       01 SQLCA.\n           05 CUSTOM-SQLCODE PIC S9(9).\n");

        RunPreprocessor();

        File.Exists(Path.Join(_source, ".preprocessed", "SQLCA.cpy")).Should().BeFalse();
        Read(definition).Should().Contain("CUSTOM-SQLCODE");
    }

    [Fact]
    public void QuarantinesLegacyShadows_WithoutTouchingSuppliedSource()
    {
        var generated = Path.Join(_source, ".preprocessed");
        var stub = $"      * {Marker}\n       01 INVENTED PIC X.\n";
        var placeholder = Write(".preprocessed/ABSENTCB.cpy", stub);
        var sidecar = Write(".preprocessed/ABSENTCB.cpy.preprocess.json", "{}");
        var genuine = Write(".preprocessed/REALCB.cpy", "       01 REAL-FIELD PIC X.\n");
        var supplied = Write("SUPPLIED-STUB.cpy", stub);

        RunPreprocessor(stubs: true);

        File.Exists(placeholder).Should().BeFalse();
        File.Exists(sidecar).Should().BeFalse();
        File.Exists(genuine).Should().BeFalse();
        var previous = Path.Join(generated, ".previous");
        var archived = Directory.GetFiles(previous, "REALCB.cpy", SearchOption.AllDirectories);
        archived.Should().ContainSingle();
        Read(archived[0]).Should().Be("       01 REAL-FIELD PIC X.\n");
        Directory.GetFiles(previous, "ABSENTCB.cpy", SearchOption.AllDirectories).Should().ContainSingle();
        Read(supplied).Should().Be(stub);
        File.Exists(Path.Join(generated, "SUPPLIED-STUB.cpy")).Should().BeFalse();
        File.Exists(Path.Join(generated, "aliases", "SUPPLIED.cpy")).Should().BeFalse();
    }

    [Fact]
    public void InsertsContinue_WhenThenBranchHoldsOnlyComments()
    {
        WriteProgram("empty-then.cbl",
            "           IF WS-FILE-OK",
            "      *        nothing to do yet",
            "           ELSE",
            "               DISPLAY 'FAILED'",
            "           END-IF",
            "           IF WS-OTHER-OK",
            "               DISPLAY 'OK'",
            "           ELSE",
            "               DISPLAY 'NOT OK'",
            "           END-IF");

        RunPreprocessor();
        var output = Preprocessed("empty-then.cbl");

        output.Should().Contain(string.Join('\n',
            "           IF WS-FILE-OK",
            "           CONTINUE",
            "      *        nothing to do yet",
            "           ELSE"));
        Count(output, "CONTINUE").Should().Be(1);
    }

    [Fact]
    public void LengthOf_IsPreservedInProcedureStatements()
    {
        WriteProgram("table-length.cbl",
            "           COMPUTE TOTAL = 48 + (COUNT * (LENGTH OF ENTRY(1))).",
            "           COMPUTE TOTAL = LENGTH OF ENTRY(INDEX) OF GROUP.",
            "           MOVE A TO B(LENGTH OF C + 1:LENGTH OF A)");

        RunPreprocessor();

        Staged("table-length.cbl").Should()
            .Contain("COMPUTE TOTAL = 48 + (COUNT * (LENGTH OF ENTRY(1))).")
            .And.Contain("COMPUTE TOTAL = LENGTH OF ENTRY(INDEX) OF GROUP.")
            .And.Contain("MOVE A TO B(LENGTH OF C + 1:LENGTH OF A)");
    }

    [Fact]
    public void RewrittenPaddedLines_StayWithinColumn80AndKeepConditions()
    {
        WriteProgram("padded.cbl",
            "           IF  FLG-FILTER-BLANK".PadRight(80),
            "           AND FLG-REENTER".PadRight(80),
            "               MOVE LENGTH OF WS-REC TO WS-LEN".PadRight(80),
            "           END-IF".PadRight(80),
            "           EXEC DLI SCHD PSB((PSB-NAME)) END-EXEC".PadRight(80));

        RunPreprocessor();
        var lines = Preprocessed("padded.cbl").Split('\n');

        lines.Where(line => line.Length > 80).Should().BeEmpty();
        var stripped = string.Join('\n', lines.Select(line => line.TrimEnd()));
        stripped.Should()
            .Contain("           IF  FLG-FILTER-BLANK\n           AND FLG-REENTER\n")
            .And.Contain("               MOVE LENGTH OF WS-REC TO WS-LEN")
            .And.Contain("      *IMS>     EXEC DLI SCHD PSB((PSB-NAME)) END-EXEC");
    }

    [Fact]
    public void PreservesInspectForAllSyntax()
    {
        WriteProgram("inspect-count.cbl",
            "           INSPECT INPUT-VALUE TALLYING",
            "              DIGIT-COUNT FOR ALL '0'",
            "              POINT-COUNT for ALL '.'.",
            "           INSPECT INPUT-VALUE REPLACING ALL '_' BY SPACE.",
            "           INSPECT INPUT-VALUE REPLACING",
            "              ALL '_' BY SPACE",
            "              ALL '%' BY SPACE.",
            "           MOVE ALL '%' TO OUTPUT-VALUE.");

        RunPreprocessor();

        Preprocessed("inspect-count.cbl").Should()
            .Contain("DIGIT-COUNT FOR ALL '0'")
            .And.Contain("POINT-COUNT for ALL '.'")
            .And.Contain("REPLACING ALL '_' BY SPACE.")
            .And.Contain("              ALL '_' BY SPACE")
            .And.Contain("              ALL '%' BY SPACE.")
            .And.Contain("MOVE '%' TO OUTPUT-VALUE.");
    }

    [Fact]
    public void DropsIdentificationAreaText_InsteadOfPullingItIntoCode()
    {
        // Columns 73-80 are ignored by the compiler; shifting their text left turns it into a token
        // and the parser then writes no AST.
        static string Fixed(string code, string ident) => code.PadRight(73) + ident;
        Write("idarea.cbl", Lines(
            Fixed("       IDENTIFICATION DIVISION.", "0002000"),
            Fixed("       PROGRAM-ID. TESTPGM.", "0003000"),
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-TS          PIC X(10).",
            Fixed("       01  FILLER REDEFINES WS-TS.", ""),
            Fixed("           06 WS-YYYY      PIC X(004).", "E"),
            Fixed("           06 WS-SEP       PIC X.", "-"),
            "       PROCEDURE DIVISION.",
            Fixed("           GOBACK.", "AB12CD34")));
        Write("idarea.cpy", Fixed("       01  CPY-FLD        PIC X(004).", "E") + "\n");

        RunPreprocessor();

        foreach (var name in new[] { "idarea.cbl", "idarea.cpy" })
            foreach (var line in Preprocessed(name).Split('\n'))
            {
                var trimmed = line.TrimEnd();
                trimmed.Length.Should().BeLessThanOrEqualTo(72, "{0}: {1}", name, line);
                foreach (var ident in new[] { "0002000", "0003000", "AB12CD34" })
                    line.Should().NotContain(ident, "{0}: {1}", name, line);
                (trimmed.EndsWith(" E", StringComparison.Ordinal) || trimmed.EndsWith(" -", StringComparison.Ordinal))
                    .Should().BeFalse("{0}: {1}", name, line);
            }
        Preprocessed("idarea.cbl").Should().Contain("PIC X(004).");
    }

    [Fact]
    public void PreservesQualifiedLengthOf()
    {
        WriteProgram("length-of.cbl",
            "           PERFORM VARYING WS-IDX",
            "                   FROM LENGTH OF OPTIONI OF MAPAI BY -1 UNTIL",
            "                   WS-IDX = 1",
            "           END-PERFORM");

        RunPreprocessor();

        Staged("length-of.cbl").Should().Contain("FROM LENGTH OF OPTIONI OF MAPAI BY -1 UNTIL");
    }

    [Fact]
    public void SkipsGeneratedCopybook_ThatARealCopybookReplaces()
    {
        // Both need rewriting (column 73+ text), so each would otherwise be written to .preprocessed/.
        Write("real/SHARED.cpy", "       01  REAL-FIELD     PIC X(004).".PadRight(73) + "E\n");
        var generated = Write("app/copy-generated/SHARED.cpy", "       01  STUB-FIELD     PIC X(004).".PadRight(73) + "E\n");
        var shadowed = Path.Join(_work.FullName, "shadowed.txt");
        File.WriteAllText(shadowed, generated + "\n");

        RunPreprocessor(shadowed: shadowed);

        Preprocessed("SHARED.cpy").Should().Contain("REAL-FIELD").And.NotContain("STUB-FIELD");
    }

    [Fact]
    public void AppliesLocalRulesFileByStage()
    {
        WriteProgram("local-rules.cbl", "           PERFORM 100-STEP UNTIL WS-A-DONE AND WS-B-DONE");
        var rules = Path.Join(_work.FullName, "rules.json");
        File.WriteAllText(rules, new JsonObject
        {
            ["programRewrites"] = new JsonArray(
                new JsonObject
                {
                    ["stage"] = "early",
                    ["literal"] = true,
                    ["find"] = "UNTIL WS-A-DONE AND WS-B-DONE",
                    ["replace"] = "UNTIL WS-A = 'Y' AND WS-B = 'Y'",
                },
                new JsonObject
                {
                    ["stage"] = "late",
                    ["find"] = @"PERFORM (\d+)-STEP",
                    ["replace"] = @"PERFORM \1-NEXT",
                }),
        }.ToJsonString());

        var result = RunPreprocessor(rules: rules);

        result.StdOut.Should().Contain("Using local preprocess rules");
        Preprocessed("local-rules.cbl").Should().Contain("PERFORM 100-NEXT UNTIL WS-A = 'Y' AND WS-B = 'Y'");
    }

    [Fact]
    public void ReportsAndIgnoresAMalformedRulesFile()
    {
        WriteProgram("plain.cbl", "           MOVE 0(1) TO WS-FLAG");
        var rules = Path.Join(_work.FullName, "broken.json");
        File.WriteAllText(rules, "{not json");

        var result = RunPreprocessor(rules: rules);

        result.StdOut.Should().Contain("Ignoring").And.Contain("Preprocessed 1 program(s)");
    }

    [Fact]
    public void Messages_ShowPathsRelativeToTheCallersFolder()
    {
        var script = File.ReadAllText(Path.Join(DoctorFixture.RepoRoot, "tools", "preprocess-for-rekt.sh"));
        var from = script.IndexOf("display_path() {", StringComparison.Ordinal);
        var function = script[from..(script.IndexOf("\n}\n", from, StringComparison.Ordinal) + 3)];

        var result = DoctorFixture.RunProcess(DoctorFixture.Bash,
            ["-c", function + "cd /tmp && display_path /tmp/repo/tools/../Config/rules.json && echo && display_path /elsewhere/x"],
            DoctorFixture.RepoRoot, DoctorFixture.ScrubbedEnvironment(), TimeSpan.FromSeconds(30));

        result.ShouldSucceed();
        result.StdOut.ReplaceLineEndings("\n").Should().Be("repo/Config/rules.json\n/elsewhere/x");
    }

    private DoctorRun RunPreprocessor(bool stubs = false, string? rules = null, string? shadowed = null)
    {
        var env = DoctorFixture.ScrubbedEnvironment();
        if (shadowed is not null) env["REKT_SHADOWED_COPYBOOKS_FILE"] = shadowed;
        env["REKT_NO_STUB_COPYBOOKS"] = stubs ? "false" : "true";
        // Reuse the built CLI instead of building it per test.
        env["MIGRATION_CLI_ASSEMBLY"] = DoctorFixture.CliAssembly;
        env["DOTNET_CMD"] = DoctorFixture.Posix(DoctorFixture.RealDotnet);
        // Never pick up a developer's local estate rules from Config/.
        env["REKT_PREPROCESS_RULES"] = rules ?? Path.Join(_work.FullName, "no-rules.json");

        var result = DoctorFixture.RunProcess(DoctorFixture.Bash,
            [DoctorFixture.Posix(Path.Join(DoctorFixture.RepoRoot, "tools", "preprocess-for-rekt.sh")), _source],
            DoctorFixture.RepoRoot, env, TimeSpan.FromSeconds(90));
        result.ShouldSucceed();
        return result;
    }

    private string Write(string relative, string text)
    {
        var path = Path.Join(_source, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, text, Latin1);
        return path;
    }

    private void WriteProgram(string name, params string[] procedureLines) =>
        Write(name, Lines([
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TESTPGM.",
            "       PROCEDURE DIVISION.",
            .. procedureLines,
            "           GOBACK."]));

    private static string Lines(params string[] lines) => string.Join('\n', lines) + "\n";

    // Matches Python's universal-newline reads so a CRLF checkout cannot mask a rewrite.
    private static string Read(string path) => File.ReadAllText(path, Latin1).ReplaceLineEndings("\n");

    private string Preprocessed(string relative) => Read(Path.Join(_source, ".preprocessed", relative));

    // Unchanged programs get no .preprocessed copy; the parser reads the source itself.
    private string Staged(string relative)
    {
        var preprocessed = Path.Join(_source, ".preprocessed", relative);
        return Read(File.Exists(preprocessed) ? preprocessed : Path.Join(_source, relative));
    }

    private static string OriginalSourcePath(string output) =>
        JsonNode.Parse(File.ReadAllText(output + ".preprocess.json"))!["originalSourcePath"]!.GetValue<string>();

    private static string AsCobolComment(string line) => line.Length >= 7 ? line[..6] + "*" + line[7..] : "*" + line;

    private static int Count(string text, string value)
    {
        var count = 0;
        for (var at = text.IndexOf(value, StringComparison.Ordinal); at >= 0; at = text.IndexOf(value, at + value.Length, StringComparison.Ordinal))
            count++;
        return count;
    }
}
