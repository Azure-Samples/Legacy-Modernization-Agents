using System.Text;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Rekt;

// REKT stages every source into one flat folder, so same-named sources must be proven harmless before staging.
public sealed class RektSourceIdentityTests : IDisposable
{
    private readonly string _work = Path.Join(Path.GetTempPath(), $"rekt-identity-{Guid.NewGuid():N}");
    private readonly string _source;

    public RektSourceIdentityTests()
    {
        _source = Path.Join(_work, "source");
        Directory.CreateDirectory(_source);
    }

    public void Dispose() => Directory.Delete(_work, recursive: true);

    [Fact]
    public void IdenticalCopiesOfAUsedCopybook_DoNotBlock()
    {
        Write("a/copy/SHARED.cpy", "       01 WS-A PIC X.\n");
        Write("b/copy/SHARED.cpy", "       01  WS-A   PIC X.   \n");
        Program("a/cobol/PROGA.cbl", "           COPY SHARED.");

        var (code, rows) = Check();

        code.Should().Be(0);
        Kinds(rows).Should().Equal(("copybook-duplicate-identical", "shared.cpy"));
    }

    [Fact]
    public void DifferingCopiesThatNothingUses_DoNotBlock()
    {
        Write("api/one/COMMAREA.cpy", "       01 WS-A PIC X.\n");
        Write("api/two/COMMAREA.cpy", "       01 WS-B PIC 9.\n");
        Program("base/PROGA.cbl", "           COPY OTHER.");
        Write("base/OTHER.cpy", "       01 WS-C PIC X.\n");

        var (code, rows) = Check();

        code.Should().Be(0);
        Kinds(rows).Should().Equal(("copybook-duplicate-unused", "commarea.cpy"));
        rows[0][2..].Should().Equal("api/one/COMMAREA.cpy", "api/two/COMMAREA.cpy");
    }

    [Fact]
    public void DifferingCopiesThatAProgramUses_BlockAndNameTheUserThroughTheCli()
    {
        Write("one/COMMAREA.cpy", "       01 WS-A PIC X.\n");
        Write("two/COMMAREA.cpy", "       01 WS-B PIC 9.\n");
        Program("one/PROGA.cbl", "           COPY 'COMMAREA'.");

        var result = LmaCliProcess.Run(Path.Join(_work, "cwd"), null,
            "rekt-source-identity", _source, Path.Join(_source, ".preprocessed"));

        result.ExitCode.Should().Be(1);
        var rows = Rows(result.StdOut);
        Kinds(rows).Should().Equal(("copybook-collision", "commarea.cpy"), ("copybook-collision-users", "commarea.cpy"));
        rows[1][2..].Should().Equal("one/PROGA.cbl");
    }

    [Fact]
    public void ApostrophesOutsideProgramText_DoNotHideALaterCopy()
    {
        Write("one/SHARED.cpy", "       01 WS-A PIC X.\n");
        Write("two/SHARED.cpy", "       01 WS-B PIC 9.\n");
        Write("one/AUTHORED.cbl",
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. AUTHORED.\n       AUTHOR. O'CONNOR.\n"
            + "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n           COPY SHARED.\n");
        Write("two/SEQUENCED.cbl",
            "000100 IDENTIFICATION DIVISION.                                         ID'00001\n"
            + "000200 PROGRAM-ID. SEQUENCED.\n000300 DATA DIVISION.\n000400 WORKING-STORAGE SECTION.\n"
            + "000500     COPY SHARED.\n");

        var (code, rows) = Check();

        code.Should().Be(1);
        rows[0][0].Should().Be("copybook-collision");
        rows[1][2..].Order().Should().Equal("one/AUTHORED.cbl", "two/SEQUENCED.cbl");
    }

    [Fact]
    public void CommentLinesAndSqlComments_DoNotHideALaterCopy()
    {
        Write("one/SHARED.cpy", "       01 WS-A PIC X.\n");
        Write("two/SHARED.cpy", "       01 WS-B PIC 9.\n");
        Write("one/COMMENTED.cbl",
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. COMMENTED.\n      * DATA DIVISION.\n"
            + "       AUTHOR. O'CONNOR.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n           COPY SHARED.\n");
        Write("two/SQLNOTE.cbl",
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. SQLNOTE.\n       PROCEDURE DIVISION.\n"
            + "           EXEC SQL\n             COMMIT -- don't hold locks\n           END-EXEC.\n           COPY SHARED.\n");

        var (code, rows) = Check();

        code.Should().Be(1);
        rows[0][0].Should().Be("copybook-collision");
        rows[1][2..].Order().Should().Equal("one/COMMENTED.cbl", "two/SQLNOTE.cbl");
    }

    [Fact]
    public void UseThroughTheEightCharacterAlias_Counts()
    {
        Write("one/INPUT-AREA_request_0.cpy", "       01 WS-A PIC X.\n");
        Write("two/INPUT-AREA_request_0.cpy", "       01 WS-B PIC 9.\n");
        Program("one/PROGA.cbl", "           COPY INPUT-AR.");

        var (code, rows) = Check();

        code.Should().Be(1);
        rows[0][0].Should().Be("copybook-collision");
    }

    [Fact]
    public void UseFromANestedCopybookOrSqlInclude_Counts()
    {
        Write("one/RECORD.cpy", "       01 WS-A PIC X.\n");
        Write("two/RECORD.cpy", "       01 WS-B PIC 9.\n");
        Write("one/WRAPPER.cpy", "           EXEC SQL INCLUDE RECORD END-EXEC.\n");
        Program("one/PROGA.cbl", "           COPY WRAPPER.");

        var (code, rows) = Check();

        code.Should().Be(1);
        rows[1][2..].Should().Equal("one/WRAPPER.cpy");
    }

    [Fact]
    public void CommentedOutCopy_DoesNotCountAsUse()
    {
        Write("one/COMMAREA.cpy", "       01 WS-A PIC X.\n");
        Write("two/COMMAREA.cpy", "       01 WS-B PIC 9.\n");
        Program("one/PROGA.cbl", "      *    COPY COMMAREA.", "           MOVE 1 TO WS-X. *> COPY COMMAREA.");

        var (code, rows) = Check();

        code.Should().Be(0);
        rows[0][0].Should().Be("copybook-duplicate-unused");
    }

    [Fact]
    public void HyphenatedIdentifierEndingInCopy_IsNotAReference()
    {
        Write("one/COMMAREA.cpy", "       01 WS-A PIC X.\n");
        Write("two/COMMAREA.cpy", "       01 WS-B PIC 9.\n");
        Program("one/PROGA.cbl", "           MOVE WS-COPY COMMAREA-LEN.");

        Check().Code.Should().Be(0);
    }

    [Fact]
    public void PreprocessedDuplicateProgram_Blocks()
    {
        Program("one/PROGA.cbl");
        Program("two/PROGA.cbl");
        Write(".preprocessed/PROGA.cbl", "       IDENTIFICATION DIVISION.\n");

        var (code, rows) = Check();

        code.Should().Be(1);
        Kinds(rows).Should().Equal(("program-duplicate", "proga.cbl"), ("preprocessed-program-collision", "proga.cbl"));
    }

    [Fact]
    public void StagingFolders_AreIgnored()
    {
        Write("one/SHARED.cpy", "       01 WS-A PIC X.\n");
        Write(".rekt-staging/SHARED.cpy", "       01 WS-B PIC 9.\n");
        Write(".convert-123/SHARED.cpy", "       01 WS-C PIC 9.\n");
        Program("one/PROGA.cbl", "           COPY SHARED.");

        var (code, rows) = Check();

        code.Should().Be(0);
        rows.Should().BeEmpty();
    }

    [Fact]
    public void ShadowedGeneratedCopybook_IsLeftOut()
    {
        Write("copy-generated/CEEIGZCT.cpy", "       01 STUB PIC X.\n");
        Write("lib/CEEIGZCT.cpy", "       01 REAL-FIELD PIC 9(4).\n");
        Program("lib/PROGA.cbl", "           COPY CEEIGZCT.");
        var shadowed = Path.Join(_work, "shadowed.txt");
        File.WriteAllText(shadowed, Path.Join(_source, "copy-generated", "CEEIGZCT.cpy") + "\n");

        var blocked = Check().Code;
        var (code, rows) = Check(shadowed);

        blocked.Should().Be(1);
        code.Should().Be(0);
        rows.Should().BeEmpty();
    }

    private void Write(string relative, string text)
    {
        var path = Path.Join(_source, relative);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, text, Encoding.Latin1);
    }

    private void Program(string relative, params string[] copyLines) =>
        Write(relative,
            "       IDENTIFICATION DIVISION.\n"
            + $"       PROGRAM-ID. {Path.GetFileNameWithoutExtension(relative).ToUpperInvariant()}.\n"
            + "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n"
            + string.Concat(copyLines.Select(line => line + "\n"))
            + "       PROCEDURE DIVISION.\n           GOBACK.\n");

    private (int Code, List<string[]> Rows) Check(string? shadowed = null)
    {
        var output = new StringWriter();
        var code = RektSourceIdentity.Run(_source, Path.Join(_source, ".preprocessed"), shadowed, output);
        return (code, Rows(output.ToString()));
    }

    private static List<string[]> Rows(string stdout) =>
        stdout.Split('\n').Where(line => line.Length > 0).Select(line => line.Split('\t')).ToList();

    private static List<(string, string)> Kinds(List<string[]> rows) => rows.Select(row => (row[0], row[1])).ToList();
}
