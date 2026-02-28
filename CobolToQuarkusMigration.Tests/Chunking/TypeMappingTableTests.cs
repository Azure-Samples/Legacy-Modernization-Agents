using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Chunking.Core;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;
using Moq;
using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Tests.Chunking;

/// <summary>
/// Tests for TypeMappingTable covering the InferTargetType pure-logic method
/// and the DB-backed CRUD operations.
/// </summary>
public class TypeMappingTableTests : IDisposable
{
    private readonly string _databasePath;
    private readonly TypeMappingTable _table;

    public TypeMappingTableTests()
    {
        _databasePath = Path.Combine(Path.GetTempPath(), $"test_typemappings_{Guid.NewGuid()}.db");
        var loggerMock = new Mock<ILogger<TypeMappingTable>>();
        InitializeTestDatabase();
        _table = new TypeMappingTable(_databasePath, loggerMock.Object);
    }

    public void Dispose()
    {
        try
        {
            SqliteConnection.ClearAllPools();
            if (File.Exists(_databasePath))
                File.Delete(_databasePath);
        }
        catch { /* ignore cleanup errors */ }
    }

    private void InitializeTestDatabase()
    {
        using var connection = new SqliteConnection($"Data Source={_databasePath}");
        connection.Open();
        using var command = connection.CreateCommand();
        command.CommandText = @"
            CREATE TABLE IF NOT EXISTS type_mappings (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id INTEGER NOT NULL,
                source_file TEXT NOT NULL,
                legacy_variable TEXT NOT NULL,
                legacy_type TEXT NOT NULL,
                target_type TEXT NOT NULL,
                target_field_name TEXT NOT NULL,
                is_nullable INTEGER NOT NULL DEFAULT 0,
                default_value TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                UNIQUE(run_id, source_file, legacy_variable)
            );";
        command.ExecuteNonQuery();
    }

    #region InferTargetType – COBOL PIC clauses

    [Theory]
    [InlineData("PIC X(10)", TargetLanguage.CSharp, "string")]
    [InlineData("PIC X(10)", TargetLanguage.Java, "String")]
    [InlineData("PIC A(5)", TargetLanguage.CSharp, "string")]
    [InlineData("PIC A(5)", TargetLanguage.Java, "String")]
    public void InferTargetType_CobolPicAlphanumeric_ReturnsStringType(string legacyType, TargetLanguage lang, string expected)
    {
        _table.InferTargetType(legacyType, lang).Should().Be(expected);
    }

    [Theory]
    [InlineData("PIC 9(4)", TargetLanguage.CSharp, "short")]
    [InlineData("PIC 9(4)", TargetLanguage.Java, "short")]
    [InlineData("PIC 9(9)", TargetLanguage.CSharp, "int")]
    [InlineData("PIC 9(9)", TargetLanguage.Java, "int")]
    [InlineData("PIC 9(18)", TargetLanguage.CSharp, "long")]
    [InlineData("PIC 9(18)", TargetLanguage.Java, "long")]
    [InlineData("PIC 9(19)", TargetLanguage.CSharp, "decimal")]
    [InlineData("PIC 9(19)", TargetLanguage.Java, "BigDecimal")]
    public void InferTargetType_CobolPicNumeric_ReturnsCorrectIntegerType(string legacyType, TargetLanguage lang, string expected)
    {
        _table.InferTargetType(legacyType, lang).Should().Be(expected);
    }

    [Theory]
    [InlineData("PIC 9(7)V99", TargetLanguage.CSharp, "decimal")]
    [InlineData("PIC 9(7)V99", TargetLanguage.Java, "BigDecimal")]
    [InlineData("PIC S9(5)V9(2)", TargetLanguage.CSharp, "decimal")]
    public void InferTargetType_CobolPicWithDecimalV_ReturnsDecimalType(string legacyType, TargetLanguage lang, string expected)
    {
        _table.InferTargetType(legacyType, lang).Should().Be(expected);
    }

    #endregion

    #region InferTargetType – PL/I types

    [Theory]
    [InlineData("FIXED BINARY", TargetLanguage.CSharp, "int")]
    [InlineData("FIXED BINARY", TargetLanguage.Java, "int")]
    [InlineData("FIXED DECIMAL", TargetLanguage.CSharp, "decimal")]
    [InlineData("FIXED DECIMAL", TargetLanguage.Java, "BigDecimal")]
    [InlineData("FLOAT", TargetLanguage.CSharp, "double")]
    [InlineData("FLOAT", TargetLanguage.Java, "double")]
    [InlineData("CHAR(10)", TargetLanguage.CSharp, "string")]
    [InlineData("CHAR(10)", TargetLanguage.Java, "String")]
    [InlineData("BIT(1)", TargetLanguage.CSharp, "bool")]
    [InlineData("BIT(1)", TargetLanguage.Java, "boolean")]
    public void InferTargetType_PliTypes_ReturnsCorrectType(string legacyType, TargetLanguage lang, string expected)
    {
        _table.InferTargetType(legacyType, lang).Should().Be(expected);
    }

    #endregion

    #region InferTargetType – FORTRAN types

    [Theory]
    [InlineData("INTEGER", TargetLanguage.CSharp, "int")]
    [InlineData("INTEGER*8", TargetLanguage.CSharp, "long")]
    [InlineData("INTEGER*8", TargetLanguage.Java, "long")]
    [InlineData("INTEGER*2", TargetLanguage.CSharp, "short")]
    [InlineData("REAL", TargetLanguage.CSharp, "float")]
    [InlineData("REAL*8", TargetLanguage.CSharp, "double")]
    [InlineData("DOUBLE PRECISION", TargetLanguage.CSharp, "double")]
    [InlineData("CHARACTER", TargetLanguage.CSharp, "string")]
    [InlineData("CHARACTER", TargetLanguage.Java, "String")]
    [InlineData("LOGICAL", TargetLanguage.CSharp, "bool")]
    [InlineData("LOGICAL", TargetLanguage.Java, "boolean")]
    public void InferTargetType_FortranTypes_ReturnsCorrectType(string legacyType, TargetLanguage lang, string expected)
    {
        _table.InferTargetType(legacyType, lang).Should().Be(expected);
    }

    #endregion

    #region InferTargetType – fallback

    [Theory]
    [InlineData("UNKNOWN_TYPE", TargetLanguage.CSharp, "string")]
    [InlineData("UNKNOWN_TYPE", TargetLanguage.Java, "String")]
    public void InferTargetType_UnknownType_ReturnsStringFallback(string legacyType, TargetLanguage lang, string expected)
    {
        _table.InferTargetType(legacyType, lang).Should().Be(expected);
    }

    #endregion

    #region DB-backed operations

    [Fact]
    public async Task RegisterMappingAsync_NewMapping_ReturnsSavedMapping()
    {
        var mapping = new TypeMapping
        {
            LegacyVariable = "WS-AMOUNT",
            LegacyType = "PIC 9(9)V99",
            TargetType = "decimal",
            TargetFieldName = "wsAmount",
            IsNullable = false,
            DefaultValue = null
        };

        var result = await _table.RegisterMappingAsync(1, "payment.cbl", mapping);

        result.Should().NotBeNull();
        result.LegacyVariable.Should().Be("WS-AMOUNT");
        result.TargetType.Should().Be("decimal");
    }

    [Fact]
    public async Task RegisterMappingAsync_DuplicateMapping_ReturnsExisting()
    {
        var mapping = new TypeMapping
        {
            LegacyVariable = "WS-NAME",
            LegacyType = "PIC X(30)",
            TargetType = "string",
            TargetFieldName = "wsName",
            IsNullable = true
        };

        await _table.RegisterMappingAsync(1, "customer.cbl", mapping);

        var duplicate = new TypeMapping { LegacyVariable = "WS-NAME", LegacyType = "PIC X(30)", TargetType = "object", TargetFieldName = "wsName", IsNullable = true };
        var result = await _table.RegisterMappingAsync(1, "customer.cbl", duplicate);

        // Should return the original, not the duplicate with "object"
        result.TargetType.Should().Be("string");
    }

    [Fact]
    public async Task GetMappingAsync_ExistingMapping_ReturnsMappingData()
    {
        var mapping = new TypeMapping
        {
            LegacyVariable = "WS-CODE",
            LegacyType = "PIC X(4)",
            TargetType = "string",
            TargetFieldName = "wsCode",
            IsNullable = false
        };
        await _table.RegisterMappingAsync(1, "code.cbl", mapping);

        var result = await _table.GetMappingAsync(1, "code.cbl", "WS-CODE");

        result.Should().NotBeNull();
        result!.LegacyVariable.Should().Be("WS-CODE");
        result.TargetFieldName.Should().Be("wsCode");
    }

    [Fact]
    public async Task GetMappingAsync_NonExistent_ReturnsNull()
    {
        var result = await _table.GetMappingAsync(99, "nonexistent.cbl", "NO-SUCH-VAR");
        result.Should().BeNull();
    }

    [Fact]
    public async Task GetMappingAsync_NullSourceFile_SearchesAcrossFiles()
    {
        var mapping = new TypeMapping
        {
            LegacyVariable = "GLOBAL-VAR",
            LegacyType = "PIC 9(5)",
            TargetType = "int",
            TargetFieldName = "globalVar",
            IsNullable = false
        };
        await _table.RegisterMappingAsync(1, "global.cbl", mapping);

        // Search without specifying source file
        var result = await _table.GetMappingAsync(1, null, "GLOBAL-VAR");

        result.Should().NotBeNull();
        result!.TargetType.Should().Be("int");
    }

    [Fact]
    public async Task GetAllMappingsAsync_WithSourceFile_ReturnsOnlyFileMappings()
    {
        var m1 = new TypeMapping { LegacyVariable = "VAR-A", LegacyType = "PIC X", TargetType = "string", TargetFieldName = "varA", IsNullable = false };
        var m2 = new TypeMapping { LegacyVariable = "VAR-B", LegacyType = "PIC 9", TargetType = "int", TargetFieldName = "varB", IsNullable = false };
        var m3 = new TypeMapping { LegacyVariable = "VAR-C", LegacyType = "PIC X", TargetType = "string", TargetFieldName = "varC", IsNullable = false };

        await _table.RegisterMappingAsync(2, "file1.cbl", m1);
        await _table.RegisterMappingAsync(2, "file2.cbl", m2);
        await _table.RegisterMappingAsync(2, "file1.cbl", m3);

        var result = await _table.GetAllMappingsAsync(2, "file1.cbl");

        result.Should().HaveCount(2);
        result.Select(m => m.LegacyVariable).Should().BeEquivalentTo(new[] { "VAR-A", "VAR-C" });
    }

    [Fact]
    public async Task GetAllMappingsAsync_WithoutSourceFile_ReturnsAllForRun()
    {
        var m1 = new TypeMapping { LegacyVariable = "ALL-A", LegacyType = "PIC X", TargetType = "string", TargetFieldName = "allA", IsNullable = false };
        var m2 = new TypeMapping { LegacyVariable = "ALL-B", LegacyType = "PIC 9", TargetType = "int", TargetFieldName = "allB", IsNullable = false };

        await _table.RegisterMappingAsync(3, "fileA.cbl", m1);
        await _table.RegisterMappingAsync(3, "fileB.cbl", m2);

        var result = await _table.GetAllMappingsAsync(3);

        result.Should().HaveCount(2);
    }

    [Fact]
    public async Task BulkRegisterMappingsAsync_EmptyList_ReturnsZero()
    {
        var result = await _table.BulkRegisterMappingsAsync(1, "test.cbl", Array.Empty<TypeMapping>());
        result.Should().Be(0);
    }

    [Fact]
    public async Task BulkRegisterMappingsAsync_NewMappings_ReturnsCountOfInserted()
    {
        var mappings = new[]
        {
            new TypeMapping { LegacyVariable = "BULK-A", LegacyType = "PIC X", TargetType = "string", TargetFieldName = "bulkA", IsNullable = false },
            new TypeMapping { LegacyVariable = "BULK-B", LegacyType = "PIC 9", TargetType = "int", TargetFieldName = "bulkB", IsNullable = false },
            new TypeMapping { LegacyVariable = "BULK-C", LegacyType = "PIC X", TargetType = "string", TargetFieldName = "bulkC", IsNullable = true }
        };

        var count = await _table.BulkRegisterMappingsAsync(4, "bulk.cbl", mappings);

        count.Should().Be(3);
    }

    [Fact]
    public async Task BulkRegisterMappingsAsync_WithDuplicates_SkipsDuplicates()
    {
        var first = new TypeMapping { LegacyVariable = "DUP-VAR", LegacyType = "PIC X", TargetType = "string", TargetFieldName = "dupVar", IsNullable = false };
        await _table.RegisterMappingAsync(5, "dup.cbl", first);

        // Attempt to bulk insert including the already-existing variable
        var mappings = new[]
        {
            new TypeMapping { LegacyVariable = "DUP-VAR", LegacyType = "PIC 9", TargetType = "int", TargetFieldName = "dupVar2", IsNullable = false },
            new TypeMapping { LegacyVariable = "NEW-VAR", LegacyType = "PIC X", TargetType = "string", TargetFieldName = "newVar", IsNullable = false }
        };

        var count = await _table.BulkRegisterMappingsAsync(5, "dup.cbl", mappings);

        // Only NEW-VAR should be inserted (DUP-VAR conflicts)
        count.Should().Be(1);
    }

    #endregion
}
