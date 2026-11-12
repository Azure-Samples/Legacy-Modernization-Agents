using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class NamingBoundaryTests
{
    [Fact]
    public void RenamingPrimaryClassPreservesNestedTypesAndTheirConstructors()
    {
        var source = """
            public class Record {
                public Record() {}
                static class RecordId {
                    public RecordId() {}
                }
                Record create() { return new Record(); }
                RecordId key() { return new RecordId(); }
                String label = "Record";
            }
            """;
        NamingHelper.ReplaceGenericClassName(source, "Record", "ConvertedRecord").Should().Be("""
            public class ConvertedRecord {
                public ConvertedRecord() {}
                static class RecordId {
                    public RecordId() {}
                }
                ConvertedRecord create() { return new ConvertedRecord(); }
                RecordId key() { return new RecordId(); }
                String label = "Record";
            }
            """);
    }

    [Theory]
    [InlineData("var s = $\"{new Record()}\";", "var s = $\"{new ConvertedRecord()}\";")]
    [InlineData("var s = $\"Record {{Record}} {Record.Max:N0} {(x ? Record.A : 1)}\";",
        "var s = $\"Record {{Record}} {ConvertedRecord.Max:N0} {(x ? ConvertedRecord.A : 1)}\";")]
    [InlineData("var p = @\"C:\\Record\\\"; var r = new Record();", "var p = @\"C:\\Record\\\"; var r = new ConvertedRecord();")]
    [InlineData("var p = @\"say \"\"Record\"\"\"; Record r;", "var p = @\"say \"\"Record\"\"\"; ConvertedRecord r;")]
    [InlineData("var e = \"\\\"Record\"; Record r;", "var e = \"\\\"Record\"; ConvertedRecord r;")]
    [InlineData("var j = $$\"\"\"{Record} {{new Record()}}\"\"\"; Record r;",
        "var j = $$\"\"\"{Record} {{new ConvertedRecord()}}\"\"\"; ConvertedRecord r;")]
    [InlineData("var e = \"\"; char q = '\"'; Record r; // Record", "var e = \"\"; char q = '\"'; ConvertedRecord r; // Record")]
    public void RenamingVisitsInterpolationHolesAndRespectsStringBoundaries(string source, string expected) =>
        NamingHelper.ReplaceGenericClassName(source, "Record", "ConvertedRecord").Should().Be(expected);
}
