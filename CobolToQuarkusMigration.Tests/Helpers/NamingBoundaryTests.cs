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
}
