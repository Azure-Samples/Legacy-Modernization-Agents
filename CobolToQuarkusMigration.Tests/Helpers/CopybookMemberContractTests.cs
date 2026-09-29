using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class CopybookMemberContractTests
{
    private const string Tagged =
        "000010 01  BDC-SEQ-:XXXX:.\n" +
        "000020     03  BDC-:XXXX:-DDNAME.\n" +
        "000030      04 BDC-GSAM-DDNAME         PIC X(04) VALUE 'GSAM'.\n" +
        "000040      04 BDC-FILE-DDNAME         PIC X(04) VALUE :'XXXX':.\n" +
        "000050*    03  BDC-COMMENTED           PIC X(01).\n" +
        "000060     03  BDC-:XXXX:-REQUEST-TYPE PIC X(06) VALUE 'OPEN  '.\n" +
        "000070     03  BDC-:XXXX:-RETURN-CODE  PIC X(03) VALUE LOW-VALUES.\n" +
        "000080       88  BDC-:XXXX:-OK                   VALUE '   '.\n" +
        "000090     03  BDC-:XXXX:-BLEN         PIC S9(04) COMP.\n" +
        "000100     03  BDC-:XXXX:-AMOUNT       PIC S9(07)V99 COMP-3.\n" +
        "000110     03  BDC-:XXXX:-TABLE        OCCURS 5 TIMES.\n" +
        "000120         05 BDC-:XXXX:-ENTRY     PIC X(10).\n" +
        "000130         05 FILLER               PIC X(02).\n" +
        "000140     03  BDC-:XXXX:-SEQ-NO       PIC 9(12).\n" +
        "000150     03  BDC-:XXXX:-EDITED       PIC ZZ9.99-.\n";

    [Fact]
    public void MembersAreTheFullDataNamesFlatWithPlaceholdersDropped()
    {
        var members = CopybookMemberContract.Parse(Tagged, "Bdcseqoi");

        members.Select(m => (m.Name, m.CSharpType)).Should().Equal(
            ("BdcDdname", "string"),
            ("BdcGsamDdname", "string"),
            ("BdcFileDdname", "string"),
            ("BdcRequestType", "string"),
            ("BdcReturnCode", "string"),
            ("BdcOk", "bool"),
            ("BdcBlen", "int"),
            ("BdcAmount", "decimal"),
            ("BdcTable", "string[]"),
            ("BdcEntry", "string[]"),
            ("BdcSeqNo", "long"),
            ("BdcEdited", "string"));
    }

    [Fact]
    public void JavaGetsCamelCaseAndJavaTypes()
    {
        var rendered = CopybookMemberContract.Render(CopybookMemberContract.Parse(Tagged, "Bdcseqoi"), "Java", "");

        rendered.Should().Contain("java.math.BigDecimal: bdcAmount");
        rendered.Should().Contain("boolean: bdcOk");
        rendered.Should().StartWith("String: bdcDdname, bdcGsamDdname, bdcFileDdname, bdcRequestType");
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
        const string outer = "000100 03 BDSDATOC.\n000200    05 BDSDATO-KD PIC X(2).\n000300    COPY BDSDFDT REPLACING ==:P:== BY ==DATO==.\n000400    05 BDSDATO-END PIC X.\n";
        const string inner = "000100    05 :P:-YYYY-MM-DD PIC X(10).\n000200    COPY BDSDATOI.\n";
        var copybooks = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase) { ["BDSDATOI"] = outer, ["BDSDFDT"] = inner };

        var names = CopybookMemberContract.Parse(outer, "Bdsdatoi", n => copybooks.GetValueOrDefault(n)).Select(m => m.Name);

        names.Should().Equal("Bdsdatoc", "BdsdatoKd", "DatoYyyyMmDd", "BdsdatoEnd");
    }
}
