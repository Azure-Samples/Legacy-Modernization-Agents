using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Chunking.Validation;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Chunking.Models;
using CobolToQuarkusMigration.Models;
using Microsoft.Extensions.Logging;
using Moq;

namespace CobolToQuarkusMigration.Tests.Chunking;

public class ConversionValidatorTests
{
    private readonly Mock<ISignatureRegistry> _signatureRegistryMock;
    private readonly Mock<ITypeMappingTable> _typeMappingTableMock;
    private readonly Mock<ILogger<ConversionValidator>> _loggerMock;
    private readonly ConversionSettings _settings;
    private readonly ConversionValidator _validator;

    public ConversionValidatorTests()
    {
        _signatureRegistryMock = new Mock<ISignatureRegistry>();
        _typeMappingTableMock = new Mock<ITypeMappingTable>();
        _loggerMock = new Mock<ILogger<ConversionValidator>>();
        _settings = new ConversionSettings
        {
            EnableReconciliationPass = true,
            MaxAutoFixAttempts = 10
        };

        _validator = new ConversionValidator(
            _signatureRegistryMock.Object,
            _typeMappingTableMock.Object,
            _settings,
            _loggerMock.Object);
    }

    #region ValidateChunkAsync Tests

    [Fact]
    public async Task ValidateChunkAsync_ValidSignatures_ReturnsValidReport()
    {
        // Arrange
        var result = new ChunkConversionResult
        {
            ChunkIndex = 0,
            SourceFile = "test.cbl",
            Success = true,
            ConvertedCode = "public class Test { }",
            DefinedMethods = new List<MethodSignature>
            {
                new() { LegacyName = "PROCESS-ORDER", TargetMethodName = "ProcessOrder", ReturnType = "void" }
            },
            TypeMappings = new List<TypeMapping>()
        };

        _signatureRegistryMock
            .Setup(r => r.ValidateSignatureAsync(1, It.IsAny<MethodSignature>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new SignatureValidationResult { IsValid = true });

        // Act
        var report = await _validator.ValidateChunkAsync(1, "test.cbl", 0, result);

        // Assert
        report.IsValid.Should().BeTrue();
        report.ValidChunks.Should().Be(1);
        report.Statistics.TotalMethods.Should().Be(1);
        report.Statistics.ConsistentMethods.Should().Be(1);
        report.SignatureIssues.Should().BeEmpty();
    }

    [Fact]
    public async Task ValidateChunkAsync_SignatureDiscrepancy_ReturnsInvalidWithIssues()
    {
        // Arrange
        var result = new ChunkConversionResult
        {
            ChunkIndex = 0,
            SourceFile = "test.cbl",
            Success = true,
            ConvertedCode = "public class Test { }",
            DefinedMethods = new List<MethodSignature>
            {
                new() { LegacyName = "CALC-TOTAL", TargetMethodName = "CalcTotal", ReturnType = "int" }
            },
            TypeMappings = new List<TypeMapping>()
        };

        _signatureRegistryMock
            .Setup(r => r.ValidateSignatureAsync(1, It.IsAny<MethodSignature>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new SignatureValidationResult
            {
                IsValid = false,
                Discrepancies = new List<SignatureDiscrepancy>
                {
                    new() { Field = "ReturnType", ExpectedValue = "decimal", ActualValue = "int", Severity = DiscrepancySeverity.Error }
                }
            });

        // Act
        var report = await _validator.ValidateChunkAsync(1, "test.cbl", 0, result);

        // Assert
        report.IsValid.Should().BeFalse();
        report.ValidChunks.Should().Be(0);
        report.SignatureIssues.Should().HaveCount(1);
        report.SignatureIssues[0].IssueType.Should().Be(ConsistencyIssueType.ReturnTypeMismatch);
        report.SignatureIssues[0].EntityName.Should().Be("CALC-TOTAL");
    }

    [Fact]
    public async Task ValidateChunkAsync_TypeMappingMismatch_ReturnsIssue()
    {
        // Arrange
        var result = new ChunkConversionResult
        {
            ChunkIndex = 0,
            SourceFile = "test.cbl",
            Success = true,
            ConvertedCode = "public class Test { }",
            DefinedMethods = new List<MethodSignature>(),
            TypeMappings = new List<TypeMapping>
            {
                new() { LegacyVariable = "WS-COUNTER", TargetType = "int", TargetFieldName = "wsCounter" }
            }
        };

        _typeMappingTableMock
            .Setup(t => t.GetMappingAsync(1, "test.cbl", "WS-COUNTER", It.IsAny<CancellationToken>()))
            .ReturnsAsync(new TypeMapping { LegacyVariable = "WS-COUNTER", TargetType = "decimal", TargetFieldName = "wsCounter" });

        // Act
        var report = await _validator.ValidateChunkAsync(1, "test.cbl", 0, result);

        // Assert
        report.IsValid.Should().BeFalse();
        report.TypeMappingIssues.Should().HaveCount(1);
        report.TypeMappingIssues[0].IssueType.Should().Be(ConsistencyIssueType.VariableTypeMismatch);
        report.TypeMappingIssues[0].Expected.Should().Be("decimal");
        report.TypeMappingIssues[0].Actual.Should().Be("int");
    }

    [Fact]
    public async Task ValidateChunkAsync_NewTypeMapping_NoIssue()
    {
        // Arrange
        var result = new ChunkConversionResult
        {
            ChunkIndex = 0,
            SourceFile = "test.cbl",
            Success = true,
            ConvertedCode = "public class Test { }",
            DefinedMethods = new List<MethodSignature>(),
            TypeMappings = new List<TypeMapping>
            {
                new() { LegacyVariable = "WS-NEW-VAR", TargetType = "string", TargetFieldName = "wsNewVar" }
            }
        };

        _typeMappingTableMock
            .Setup(t => t.GetMappingAsync(1, "test.cbl", "WS-NEW-VAR", It.IsAny<CancellationToken>()))
            .ReturnsAsync((TypeMapping?)null);

        // Act
        var report = await _validator.ValidateChunkAsync(1, "test.cbl", 0, result);

        // Assert
        report.IsValid.Should().BeTrue();
        report.TypeMappingIssues.Should().BeEmpty();
        report.Statistics.ConsistentVariables.Should().Be(1);
    }

    [Fact]
    public async Task ValidateChunkAsync_FieldNameMismatch_ReturnsWarning()
    {
        // Arrange
        var result = new ChunkConversionResult
        {
            ChunkIndex = 0,
            SourceFile = "test.cbl",
            Success = true,
            ConvertedCode = "public class Test { }",
            DefinedMethods = new List<MethodSignature>(),
            TypeMappings = new List<TypeMapping>
            {
                new() { LegacyVariable = "WS-FIELD", TargetType = "string", TargetFieldName = "wsFieldNew" }
            }
        };

        _typeMappingTableMock
            .Setup(t => t.GetMappingAsync(1, "test.cbl", "WS-FIELD", It.IsAny<CancellationToken>()))
            .ReturnsAsync(new TypeMapping { LegacyVariable = "WS-FIELD", TargetType = "string", TargetFieldName = "wsField" });

        // Act
        var report = await _validator.ValidateChunkAsync(1, "test.cbl", 0, result);

        // Assert
        report.TypeMappingIssues.Should().HaveCount(1);
        report.TypeMappingIssues[0].IssueType.Should().Be(ConsistencyIssueType.VariableNameMismatch);
        report.TypeMappingIssues[0].Severity.Should().Be(IssueSeverity.Warning);
        report.TypeMappingIssues[0].CanAutoFix.Should().BeTrue();
    }

    [Fact]
    public async Task ValidateChunkAsync_TracksForwardReferences()
    {
        // Arrange
        var result = new ChunkConversionResult
        {
            ChunkIndex = 0,
            SourceFile = "test.cbl",
            Success = true,
            ConvertedCode = "public class Test { }",
            DefinedMethods = new List<MethodSignature>(),
            TypeMappings = new List<TypeMapping>(),
            ForwardReferences = new List<ForwardReference>
            {
                new() { CallerMethod = "MAIN", TargetMethod = "HELPER-1" },
                new() { CallerMethod = "MAIN", TargetMethod = "HELPER-2" }
            }
        };

        // Act
        var report = await _validator.ValidateChunkAsync(1, "test.cbl", 0, result);

        // Assert
        report.Statistics.TotalForwardReferences.Should().Be(2);
    }

    #endregion

    #region ReconcileFileAsync Tests

    [Fact]
    public async Task ReconcileFileAsync_AllResolved_ReturnsValid()
    {
        // Arrange
        var history = new FileConversionHistory
        {
            SourceFile = "test.cbl",
            RunId = 1,
            ChunkResults = new List<ChunkConversionResult>
            {
                new() { ChunkIndex = 0, Success = true, ConvertedCode = "public void ProcessOrder() { }" },
                new() { ChunkIndex = 1, Success = true, ConvertedCode = "public void CalcTotal() { }" }
            },
            AllSignatures = new List<MethodSignature>
            {
                new() { LegacyName = "PROCESS-ORDER", TargetMethodName = "ProcessOrder" },
                new() { LegacyName = "CALC-TOTAL", TargetMethodName = "CalcTotal" }
            },
            AllTypeMappings = new List<TypeMapping>(),
            UnresolvedReferences = new List<ForwardReference>()
        };

        // Act
        var report = await _validator.ReconcileFileAsync(1, "test.cbl", history);

        // Assert
        report.IsValid.Should().BeTrue();
        report.ValidChunks.Should().Be(2);
        report.TotalChunks.Should().Be(2);
        report.UnresolvedReferences.Should().BeEmpty();
    }

    [Fact]
    public async Task ReconcileFileAsync_UnresolvedForwardReference_ReturnsInvalid()
    {
        // Arrange
        var history = new FileConversionHistory
        {
            SourceFile = "test.cbl",
            RunId = 1,
            ChunkResults = new List<ChunkConversionResult>
            {
                new() { ChunkIndex = 0, Success = true, ConvertedCode = "Helper(); // forward ref" }
            },
            AllSignatures = new List<MethodSignature>
            {
                new() { LegacyName = "MAIN", TargetMethodName = "Main" }
            },
            AllTypeMappings = new List<TypeMapping>(),
            UnresolvedReferences = new List<ForwardReference>
            {
                new() { CallerMethod = "MAIN", TargetMethod = "HELPER", PredictedSignature = "void Helper()" }
            }
        };

        // Act
        var report = await _validator.ReconcileFileAsync(1, "test.cbl", history);

        // Assert
        report.IsValid.Should().BeFalse();
        report.UnresolvedReferences.Should().HaveCount(1);
        report.UnresolvedReferences[0].TargetMethod.Should().Be("HELPER");
        report.SignatureIssues.Should().Contain(i => i.IssueType == ConsistencyIssueType.UnresolvedForwardReference);
    }

    [Fact]
    public async Task ReconcileFileAsync_CountsResolvedReferences()
    {
        // Arrange
        var history = new FileConversionHistory
        {
            SourceFile = "test.cbl",
            RunId = 1,
            ChunkResults = new List<ChunkConversionResult>
            {
                new() { ChunkIndex = 0, Success = true, ConvertedCode = "public void Test() { }" }
            },
            AllSignatures = new List<MethodSignature>
            {
                new() { LegacyName = "METHOD-A", TargetMethodName = "MethodA" },
                new() { LegacyName = "METHOD-B", TargetMethodName = "MethodB" },
                new() { LegacyName = "METHOD-C", TargetMethodName = "MethodC" }
            },
            AllTypeMappings = new List<TypeMapping>(),
            UnresolvedReferences = new List<ForwardReference>()
        };

        // Act
        var report = await _validator.ReconcileFileAsync(1, "test.cbl", history);

        // Assert
        report.Statistics.ResolvedReferences.Should().Be(3); // One for each signature
    }

    [Fact]
    public async Task ReconcileFileAsync_DetectsLegacyNameInConvertedCode()
    {
        // Arrange
        var history = new FileConversionHistory
        {
            SourceFile = "test.cbl",
            RunId = 1,
            ChunkResults = new List<ChunkConversionResult>
            {
                new() 
                { 
                    ChunkIndex = 0, 
                    Success = true, 
                    // Simulates converted code that still has legacy pattern
                    ConvertedCode = "// Call PROCESSORDER to process\npublic void Test() { }" 
                }
            },
            AllSignatures = new List<MethodSignature>
            {
                new() { LegacyName = "PROCESS-ORDER", TargetMethodName = "ProcessOrder" }
            },
            AllTypeMappings = new List<TypeMapping>(),
            UnresolvedReferences = new List<ForwardReference>()
        };

        // Act
        var report = await _validator.ReconcileFileAsync(1, "test.cbl", history);

        // Assert
        report.SignatureIssues.Should().Contain(i => 
            i.IssueType == ConsistencyIssueType.MethodNameMismatch &&
            i.CanAutoFix == true);
    }

    [Fact]
    public async Task ReconcileFileAsync_DetectsLegacyVariableInCode()
    {
        // Arrange
        var history = new FileConversionHistory
        {
            SourceFile = "test.cbl",
            RunId = 1,
            ChunkResults = new List<ChunkConversionResult>
            {
                new() 
                { 
                    ChunkIndex = 0, 
                    Success = true,
                    ConvertedCode = "int WS-COUNTER = 0;" // Legacy name still in code
                }
            },
            AllSignatures = new List<MethodSignature>(),
            AllTypeMappings = new List<TypeMapping>
            {
                new() { LegacyVariable = "WS-COUNTER", TargetType = "int", TargetFieldName = "wsCounter" }
            },
            UnresolvedReferences = new List<ForwardReference>()
        };

        // Act
        var report = await _validator.ReconcileFileAsync(1, "test.cbl", history);

        // Assert
        report.TypeMappingIssues.Should().Contain(i => 
            i.IssueType == ConsistencyIssueType.VariableNameMismatch &&
            i.EntityName == "WS-COUNTER");
    }

    [Fact]
    public async Task ReconcileFileAsync_AppliesAutoFixes_WhenEnabled()
    {
        // Arrange
        _settings.EnableReconciliationPass = true;
        _settings.MaxAutoFixAttempts = 5;

        var history = new FileConversionHistory
        {
            SourceFile = "test.cbl",
            RunId = 1,
            ChunkResults = new List<ChunkConversionResult>
            {
                new() 
                { 
                    ChunkIndex = 0, 
                    Success = true,
                    ConvertedCode = "// Uses PROCESSORDER style"
                }
            },
            AllSignatures = new List<MethodSignature>
            {
                new() { LegacyName = "PROCESS-ORDER", TargetMethodName = "ProcessOrder" }
            },
            AllTypeMappings = new List<TypeMapping>(),
            UnresolvedReferences = new List<ForwardReference>()
        };

        // Act
        var report = await _validator.ReconcileFileAsync(1, "test.cbl", history);

        // Assert
        report.AutoFixes.Should().NotBeEmpty();
        report.Statistics.AutoFixedIssues.Should().BeGreaterThan(0);
    }

    [Fact]
    public async Task ReconcileFileAsync_LimitsAutoFixes_ToMaxAttempts()
    {
        // Arrange
        _settings.MaxAutoFixAttempts = 2;

        var history = new FileConversionHistory
        {
            SourceFile = "test.cbl",
            RunId = 1,
            ChunkResults = new List<ChunkConversionResult>
            {
                new() 
                { 
                    ChunkIndex = 0, 
                    Success = true,
                    ConvertedCode = "METHODA METHODB METHODC METHODD METHODE"
                }
            },
            AllSignatures = Enumerable.Range(1, 5)
                .Select(i => new MethodSignature { LegacyName = $"METHOD-{(char)('A' + i - 1)}", TargetMethodName = $"Method{(char)('A' + i - 1)}" })
                .ToList(),
            AllTypeMappings = new List<TypeMapping>(),
            UnresolvedReferences = new List<ForwardReference>()
        };

        // Act
        var report = await _validator.ReconcileFileAsync(1, "test.cbl", history);

        // Assert
        report.AutoFixes.Count.Should().BeLessOrEqualTo(2);
    }

    [Fact]
    public async Task ReconcileFileAsync_CountsValidChunks()
    {
        // Arrange
        var history = new FileConversionHistory
        {
            SourceFile = "test.cbl",
            RunId = 1,
            ChunkResults = new List<ChunkConversionResult>
            {
                new() { ChunkIndex = 0, Success = true, ConvertedCode = "" },
                new() { ChunkIndex = 1, Success = false, ErrorMessage = "API error", ConvertedCode = "" },
                new() { ChunkIndex = 2, Success = true, ConvertedCode = "" }
            },
            AllSignatures = new List<MethodSignature>(),
            AllTypeMappings = new List<TypeMapping>(),
            UnresolvedReferences = new List<ForwardReference>()
        };

        // Act
        var report = await _validator.ReconcileFileAsync(1, "test.cbl", history);

        // Assert
        report.ValidChunks.Should().Be(2);
        report.TotalChunks.Should().Be(3);
    }

    #endregion
}
