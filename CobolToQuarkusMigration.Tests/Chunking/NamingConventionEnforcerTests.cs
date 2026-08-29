using CobolToQuarkusMigration.Chunking.Core;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Chunking;

public sealed class NamingConventionEnforcerTests
{
    private static ConversionSettings DefaultSettings() => new();

    private static NamingConventionEnforcer Create(ConversionSettings? settings = null)
        => new(settings ?? DefaultSettings());

    // ──────────────────────────────────────────────────────────────
    // ConvertNameDeterministic – empty / whitespace inputs
    // ──────────────────────────────────────────────────────────────

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void ConvertNameDeterministic_EmptyOrWhitespace_ReturnsEmpty(string input)
    {
        var sut = Create();
        sut.ConvertNameDeterministic(input, NameKind.ClassName, TargetLanguage.Java)
           .Should().BeEmpty();
    }

    // ──────────────────────────────────────────────────────────────
    // COBOL prefix stripping
    // ──────────────────────────────────────────────────────────────

    [Theory]
    [InlineData("WS-CUSTOMER-DATA", NameKind.ClassName, "CustomerData")]
    [InlineData("LS-ORDER-STATUS",  NameKind.ClassName, "OrderStatus")]
    [InlineData("WK-ACCOUNT-BALANCE", NameKind.ClassName, "AccountBalance")]
    [InlineData("LK-PAYMENT-AMOUNT",  NameKind.ClassName, "PaymentAmount")]
    [InlineData("FD-INPUT-FILE",      NameKind.ClassName, "InputFile")]
    [InlineData("SD-SORT-FILE",       NameKind.ClassName, "SortFile")]
    public void ConvertNameDeterministic_StripsCobolPrefixes(
        string legacyName, NameKind nameKind, string expected)
    {
        var sut = Create();
        sut.ConvertNameDeterministic(legacyName, nameKind, TargetLanguage.Java)
           .Should().Be(expected);
    }

    // ──────────────────────────────────────────────────────────────
    // ClassName → PascalCase
    // ──────────────────────────────────────────────────────────────

    [Theory]
    [InlineData("VALIDATE-CUSTOMER-DATA", "ValidateCustomerData")]
    [InlineData("PROCESS-PAYMENT",        "ProcessPayment")]
    [InlineData("SINGLE",                 "Single")]
    public void ConvertNameDeterministic_ClassName_ProducesPascalCase(
        string legacyName, string expected)
    {
        var sut = Create();
        sut.ConvertNameDeterministic(legacyName, NameKind.ClassName, TargetLanguage.Java)
           .Should().Be(expected);
    }

    // ──────────────────────────────────────────────────────────────
    // MethodName / FieldName / ParameterName → camelCase
    // ──────────────────────────────────────────────────────────────

    [Theory]
    [InlineData(NameKind.MethodName)]
    [InlineData(NameKind.FieldName)]
    [InlineData(NameKind.ParameterName)]
    public void ConvertNameDeterministic_CamelCaseKinds_StartWithLowerCase(NameKind kind)
    {
        var sut = Create();
        var result = sut.ConvertNameDeterministic("PROCESS-PAYMENT", kind, TargetLanguage.Java);
        result.Should().Be("processPayment");
    }

    // ──────────────────────────────────────────────────────────────
    // PropertyName → PascalCase
    // ──────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_PropertyName_ProducesPascalCase()
    {
        var sut = Create();
        sut.ConvertNameDeterministic("ACCOUNT-BALANCE", NameKind.PropertyName, TargetLanguage.Java)
           .Should().Be("AccountBalance");
    }

    // ──────────────────────────────────────────────────────────────
    // ConstantName → UPPER_SNAKE_CASE
    // ──────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_ConstantName_ProducesUpperSnakeCase()
    {
        var sut = Create();
        sut.ConvertNameDeterministic("MAX-RETRY-COUNT", NameKind.ConstantName, TargetLanguage.Java)
           .Should().Be("MAX_RETRY_COUNT");
    }

    // ──────────────────────────────────────────────────────────────
    // EnumMemberName → PascalCase
    // ──────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_EnumMemberName_ProducesPascalCase()
    {
        var sut = Create();
        sut.ConvertNameDeterministic("STATUS-ACTIVE", NameKind.EnumMemberName, TargetLanguage.Java)
           .Should().Be("StatusActive");
    }

    // ──────────────────────────────────────────────────────────────
    // ClassNameSuffix and ClassNamePrefix
    // ──────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_WithClassNameSuffix_AppendsSuffix()
    {
        var settings = new ConversionSettings { ClassNameSuffix = "Service" };
        var sut = Create(settings);
        sut.ConvertNameDeterministic("PAYMENT", NameKind.ClassName, TargetLanguage.Java)
           .Should().Be("PaymentService");
    }

    [Fact]
    public void ConvertNameDeterministic_WithClassNamePrefix_PrependsPrefixForClassName()
    {
        var settings = new ConversionSettings { ClassNamePrefix = "Legacy" };
        var sut = Create(settings);
        sut.ConvertNameDeterministic("PAYMENT", NameKind.ClassName, TargetLanguage.Java)
           .Should().Be("LegacyPayment");
    }

    [Fact]
    public void ConvertNameDeterministic_WithClassNamePrefix_DoesNotApplyToNonClassNames()
    {
        var settings = new ConversionSettings { ClassNamePrefix = "Legacy" };
        var sut = Create(settings);
        // MethodName should not receive the class prefix
        var result = sut.ConvertNameDeterministic("PAYMENT", NameKind.MethodName, TargetLanguage.Java);
        result.Should().NotStartWith("Legacy");
    }

    // ──────────────────────────────────────────────────────────────
    // Reserved word escaping
    // ──────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_CSharpReservedWord_EscapesWithAtSign()
    {
        var sut = Create();
        // "CLASS" → ClassName PascalCase → "Class" → reserved in C#
        var result = sut.ConvertNameDeterministic("CLASS", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("@Class");
    }

    [Fact]
    public void ConvertNameDeterministic_JavaReservedWord_EscapesWithUnderscore()
    {
        var sut = Create();
        // "CLASS" → ClassName PascalCase → "Class" → reserved in Java
        var result = sut.ConvertNameDeterministic("CLASS", NameKind.ClassName, TargetLanguage.Java);
        result.Should().Be("Class_");
    }

    [Fact]
    public void ConvertNameDeterministic_NonReservedWord_IsNotEscaped()
    {
        var sut = Create();
        var result = sut.ConvertNameDeterministic("INVOICE", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("Invoice");
        result.Should().NotStartWith("@");
    }

    // ──────────────────────────────────────────────────────────────
    // Underscore separators (not just hyphens)
    // ──────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_UnderscoreSeparator_WorksLikeHyphen()
    {
        var sut = Create();
        sut.ConvertNameDeterministic("GET_ACCOUNT_BALANCE", NameKind.MethodName, TargetLanguage.Java)
           .Should().Be("getAccountBalance");
    }

    // ──────────────────────────────────────────────────────────────
    // ValidateName
    // ──────────────────────────────────────────────────────────────

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void ValidateName_EmptyOrWhitespace_ReturnsFalse(string name)
    {
        Create().ValidateName(name, NameKind.ClassName).Should().BeFalse();
    }

    [Theory]
    [InlineData("ValidClass", NameKind.ClassName, true)]
    [InlineData("validClass", NameKind.ClassName, false)]   // must start uppercase
    [InlineData("validMethod", NameKind.MethodName, true)]
    [InlineData("ValidMethod", NameKind.MethodName, false)]  // must start lowercase
    [InlineData("ValidProp", NameKind.PropertyName, true)]
    [InlineData("validProp", NameKind.PropertyName, false)]
    [InlineData("validField", NameKind.FieldName, true)]
    [InlineData("ValidField", NameKind.FieldName, false)]
    [InlineData("UPPER_CONSTANT", NameKind.ConstantName, true)]
    [InlineData("lower_constant", NameKind.ConstantName, false)]
    public void ValidateName_VariousNameTypes_ReturnsExpected(
        string name, NameKind kind, bool expected)
    {
        Create().ValidateName(name, kind).Should().Be(expected);
    }

    [Fact]
    public void ValidateName_UnknownNameKind_ReturnsTrue()
    {
        // Default branch returns true for unrecognised kinds
        Create().ValidateName("anythingGoes", (NameKind)999).Should().BeTrue();
    }

    // ──────────────────────────────────────────────────────────────
    // SuggestCorrectedName (delegates to ConvertNameDeterministic)
    // ──────────────────────────────────────────────────────────────

    [Fact]
    public void SuggestCorrectedName_DelegatesToConvertName()
    {
        var sut = Create();
        var suggested = sut.SuggestCorrectedName("PROCESS-ORDER", NameKind.MethodName, TargetLanguage.Java);
        suggested.Should().Be(
            sut.ConvertNameDeterministic("PROCESS-ORDER", NameKind.MethodName, TargetLanguage.Java));
    }
}
