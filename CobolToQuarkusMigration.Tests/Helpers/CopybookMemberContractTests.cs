using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class CopybookMemberContractTests
{
    private const string Tagged =
        "000010 01  BAT-SEQ-:XXXX:.\n" +
        "000020     03  BAT-:XXXX:-DDNAME.\n" +
        "000030      04 BAT-GSAM-DDNAME         PIC X(04) VALUE 'GSAM'.\n" +
        "000040      04 BAT-FILE-DDNAME         PIC X(04) VALUE :'XXXX':.\n" +
        "000050*    03  BAT-COMMENTED           PIC X(01).\n" +
        "000060     03  BAT-:XXXX:-REQUEST-TYPE PIC X(06) VALUE 'OPEN  '.\n" +
        "000070     03  BAT-:XXXX:-RETURN-CODE  PIC X(03) VALUE LOW-VALUES.\n" +
        "000080       88  BAT-:XXXX:-OK                   VALUE '   '.\n" +
        "000090     03  BAT-:XXXX:-BLEN         PIC S9(04) COMP.\n" +
        "000100     03  BAT-:XXXX:-AMOUNT       PIC S9(07)V99 COMP-3.\n" +
        "000110     03  BAT-:XXXX:-TABLE        OCCURS 5 TIMES.\n" +
        "000120         05 BAT-:XXXX:-ENTRY     PIC X(10).\n" +
        "000130         05 FILLER               PIC X(02).\n" +
        "000140     03  BAT-:XXXX:-SEQ-NO       PIC 9(12).\n" +
        "000150     03  BAT-:XXXX:-EDITED       PIC ZZ9.99-.\n";

    [Fact]
    public void MembersAreTheFullDataNamesFlatWithPlaceholdersDropped()
    {
        var members = CopybookMemberContract.Parse(Tagged, "Batseqoi");

        members.Select(m => (m.Name, m.CSharpType)).Should().Equal(
            ("BatDdname", "string"),
            ("BatGsamDdname", "string"),
            ("BatFileDdname", "string"),
            ("BatRequestType", "string"),
            ("BatReturnCode", "string"),
            ("BatOk", "bool"),
            ("BatBlen", "int"),
            ("BatAmount", "decimal"),
            ("BatTable", "string[]"),
            ("BatEntry", "string[]"),
            ("BatSeqNo", "long"),
            ("BatEdited", "string"));
    }

    [Fact]
    public void JavaGetsCamelCaseAndJavaTypes()
    {
        var rendered = CopybookMemberContract.Render(CopybookMemberContract.Parse(Tagged, "Batseqoi"), "Java", "");

        rendered.Should().Contain("java.math.BigDecimal: batAmount");
        rendered.Should().Contain("boolean: batOk");
        rendered.Should().StartWith("String: batDdname, batGsamDdname, batFileDdname, batRequestType");
    }

    [Fact]
    public void SeveralRecordsAreEachAMemberAndANameEqualToTheTypeIsSuffixed()
    {
        const string text = "       01  REC-A.\n           05 A-FIELD PIC X.\n       01  REC-B PIC 9(3).\n       01  HOLDER PIC X.\n";

        var names = CopybookMemberContract.Parse(text, "Holder").Select(m => m.Name);

        names.Should().Equal("RecA", "AField", "RecB", "HolderValue");
    }

    [Fact]
    public void ANestedCopyIsPartOfTheRecordWithItsReplacingApplied()
    {
        const string outer = "000100 03 ORDDATAC.\n000200    05 ORDDATA-KD PIC X(2).\n000300    COPY ORDDFDT REPLACING ==:P:== BY ==DATO==.\n000400    05 ORDDATA-END PIC X.\n";
        const string inner = "000100    05 :P:-YYYY-MM-DD PIC X(10).\n000200    COPY ORDDATAI.\n";
        var copybooks = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase) { ["ORDDATAI"] = outer, ["ORDDFDT"] = inner };

        var names = CopybookMemberContract.Parse(outer, "Orddatai", n => copybooks.GetValueOrDefault(n)).Select(m => m.Name);

        names.Should().Equal("Orddatac", "OrddataKd", "DatoYyyyMmDd", "OrddataEnd");
    }
}
