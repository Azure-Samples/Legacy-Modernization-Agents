using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Chunking.Core;
using CobolToQuarkusMigration.Chunking.Interfaces;
using Microsoft.Extensions.Logging;
using Moq;
using Microsoft.Data.Sqlite;

namespace CobolToQuarkusMigration.Tests.Chunking;

public class SignatureRegistryTests : IDisposable
{
    private readonly string _databasePath;
    private readonly Mock<ILogger<SignatureRegistry>> _loggerMock;
    private readonly SignatureRegistry _registry;

    public SignatureRegistryTests()
    {
        _databasePath = Path.Combine(Path.GetTempPath(), $"test_signatures_{Guid.NewGuid()}.db");
        _loggerMock = new Mock<ILogger<SignatureRegistry>>();
        
        InitializeTestDatabase();
        _registry = new SignatureRegistry(_databasePath, _loggerMock.Object);
    }

    public void Dispose()
    {
        try
        {
            SqliteConnection.ClearAllPools();
            if (File.Exists(_databasePath))
            {
                File.Delete(_databasePath);
            }
        }
        catch
        {
            // Ignore cleanup errors
        }
    }

    private void InitializeTestDatabase()
    {
        using var connection = new SqliteConnection($"Data Source={_databasePath}");
        connection.Open();
        
        using var command = connection.CreateCommand();
        command.CommandText = @"
            CREATE TABLE IF NOT EXISTS signatures (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id INTEGER NOT NULL,
                source_file TEXT NOT NULL,
                chunk_index INTEGER NOT NULL,
                legacy_name TEXT NOT NULL,
                target_method_name TEXT NOT NULL,
                target_signature TEXT NOT NULL,
                return_type TEXT NOT NULL,
                parameters TEXT,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                UNIQUE(run_id, source_file, legacy_name)
            );
        ";
        command.ExecuteNonQuery();
    }

    [Fact]
    public async Task RegisterSignatureAsync_NewSignature_ReturnsRegistered()
    {
        // Arrange
        var signature = new MethodSignature
        {
            LegacyName = "VALIDATE-CUSTOMER",
            TargetMethodName = "ValidateCustomer",
            TargetSignature = "bool ValidateCustomer(string customerId)",
            ReturnType = "bool",
            Parameters = new List<MethodParameter>
            {
                new() { Name = "customerId", Type = "string" }
            }
        };

        // Act
        var result = await _registry.RegisterSignatureAsync(1, "customer.cbl", 0, signature);

        // Assert
        result.Should().NotBeNull();
        result.LegacyName.Should().Be("VALIDATE-CUSTOMER");
        result.TargetMethodName.Should().Be("ValidateCustomer");
    }

    [Fact]
    public async Task RegisterSignatureAsync_DuplicateLegacyName_ReturnsExisting()
    {
        // Arrange
        var signature1 = new MethodSignature
        {
            LegacyName = "PROCESS-ORDER",
            TargetMethodName = "ProcessOrder",
            TargetSignature = "void ProcessOrder()",
            ReturnType = "void"
        };

        var signature2 = new MethodSignature
        {
            LegacyName = "PROCESS-ORDER", // Same legacy name
            TargetMethodName = "ProcessOrderV2", // Different target
            TargetSignature = "bool ProcessOrderV2()",
            ReturnType = "bool"
        };

        // Act
        await _registry.RegisterSignatureAsync(1, "order.cbl", 0, signature1);
        var result = await _registry.RegisterSignatureAsync(1, "order.cbl", 1, signature2);

        // Assert - should return the original signature, not the new one
        result.TargetMethodName.Should().Be("ProcessOrder");
        result.ReturnType.Should().Be("void");
    }

    [Fact]
    public async Task GetSignatureAsync_ExistingSignature_ReturnsSignature()
    {
        // Arrange
        var signature = new MethodSignature
        {
            LegacyName = "CALCULATE-TOTAL",
            TargetMethodName = "CalculateTotal",
            TargetSignature = "decimal CalculateTotal()",
            ReturnType = "decimal"
        };
        await _registry.RegisterSignatureAsync(1, "calc.cbl", 0, signature);

        // Act
        var result = await _registry.GetSignatureAsync(1, "calc.cbl", "CALCULATE-TOTAL");

        // Assert
        result.Should().NotBeNull();
        result!.TargetMethodName.Should().Be("CalculateTotal");
        result.ReturnType.Should().Be("decimal");
    }

    [Fact]
    public async Task GetSignatureAsync_NonExistent_ReturnsNull()
    {
        // Act
        var result = await _registry.GetSignatureAsync(1, null, "NON-EXISTENT");

        // Assert
        result.Should().BeNull();
    }

    [Fact]
    public async Task GetAllSignaturesAsync_ReturnsAllForRun()
    {
        // Arrange
        var signatures = new[]
        {
            new MethodSignature { LegacyName = "METHOD-A", TargetMethodName = "MethodA", TargetSignature = "void MethodA()", ReturnType = "void" },
            new MethodSignature { LegacyName = "METHOD-B", TargetMethodName = "MethodB", TargetSignature = "void MethodB()", ReturnType = "void" },
            new MethodSignature { LegacyName = "METHOD-C", TargetMethodName = "MethodC", TargetSignature = "void MethodC()", ReturnType = "void" }
        };

        foreach (var sig in signatures)
        {
            await _registry.RegisterSignatureAsync(1, "test.cbl", 0, sig);
        }

        // Act
        var result = await _registry.GetAllSignaturesAsync(1);

        // Assert
        result.Should().HaveCount(3);
        result.Select(s => s.LegacyName).Should().BeEquivalentTo(new[] { "METHOD-A", "METHOD-B", "METHOD-C" });
    }

    [Fact]
    public async Task GetAllSignaturesAsync_FilterByFile_ReturnsOnlyFileSignatures()
    {
        // Arrange
        await _registry.RegisterSignatureAsync(1, "file1.cbl", 0, 
            new MethodSignature { LegacyName = "FILE1-METHOD", TargetMethodName = "File1Method", TargetSignature = "void File1Method()", ReturnType = "void" });
        await _registry.RegisterSignatureAsync(1, "file2.cbl", 0, 
            new MethodSignature { LegacyName = "FILE2-METHOD", TargetMethodName = "File2Method", TargetSignature = "void File2Method()", ReturnType = "void" });

        // Act
        var result = await _registry.GetAllSignaturesAsync(1, "file1.cbl");

        // Assert
        result.Should().HaveCount(1);
        result[0].LegacyName.Should().Be("FILE1-METHOD");
    }

    [Fact]
    public async Task SignatureExistsAsync_ExistingSignature_ReturnsTrue()
    {
        // Arrange
        var signature = new MethodSignature
        {
            LegacyName = "EXISTS-TEST",
            TargetMethodName = "ExistsTest",
            TargetSignature = "void ExistsTest()",
            ReturnType = "void"
        };
        await _registry.RegisterSignatureAsync(1, "test.cbl", 0, signature);

        // Act
        var exists = await _registry.SignatureExistsAsync(1, "EXISTS-TEST");

        // Assert
        exists.Should().BeTrue();
    }

    [Fact]
    public async Task SignatureExistsAsync_NonExistentSignature_ReturnsFalse()
    {
        // Act
        var exists = await _registry.SignatureExistsAsync(1, "NON-EXISTENT");

        // Assert
        exists.Should().BeFalse();
    }

    [Fact]
    public async Task ValidateSignatureAsync_MatchingSignature_ReturnsValid()
    {
        // Arrange
        var original = new MethodSignature
        {
            LegacyName = "VALIDATE-TEST",
            TargetMethodName = "ValidateTest",
            TargetSignature = "bool ValidateTest(string input)",
            ReturnType = "bool",
            Parameters = new List<MethodParameter> { new() { Name = "input", Type = "string" } }
        };
        await _registry.RegisterSignatureAsync(1, "test.cbl", 0, original);

        var toValidate = new MethodSignature
        {
            LegacyName = "VALIDATE-TEST",
            TargetMethodName = "ValidateTest",
            ReturnType = "bool",
            Parameters = new List<MethodParameter> { new() { Name = "input", Type = "string" } }
        };

        // Act
        var result = await _registry.ValidateSignatureAsync(1, toValidate);

        // Assert
        result.IsValid.Should().BeTrue();
        result.Discrepancies.Should().BeEmpty();
    }

    [Fact]
    public async Task ValidateSignatureAsync_DifferentReturnType_ReturnsInvalid()
    {
        // Arrange
        var original = new MethodSignature
        {
            LegacyName = "MISMATCH-TEST",
            TargetMethodName = "MismatchTest",
            TargetSignature = "void MismatchTest()",
            ReturnType = "void",
            Parameters = new List<MethodParameter>()
        };
        await _registry.RegisterSignatureAsync(1, "test.cbl", 0, original);

        var toValidate = new MethodSignature
        {
            LegacyName = "MISMATCH-TEST",
            TargetMethodName = "MismatchTest",
            ReturnType = "bool", // Different!
            Parameters = new List<MethodParameter>()
        };

        // Act
        var result = await _registry.ValidateSignatureAsync(1, toValidate);

        // Assert
        result.IsValid.Should().BeFalse();
        result.Discrepancies.Should().Contain(d => d.Field == "ReturnType");
    }
}
