using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Chunking.Core;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Tests.Chunking;

public class NamingConventionEnforcerTests
{
    private static NamingConventionEnforcer CreateEnforcer(
        string classNamePrefix = "",
        string classNameSuffix = "") =>
        new NamingConventionEnforcer(new ConversionSettings
        {
            ClassNamePrefix = classNamePrefix,
            ClassNameSuffix = classNameSuffix
        });

    // ────────────────────────────────────────────────────────────────
    // ConvertNameDeterministic – NameKind.ClassName
    // ────────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_ClassName_CSharp_ReturnsPascalCase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("VALIDATE-CUSTOMER-DATA", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("ValidateCustomerData");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassName_Java_ReturnsPascalCase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("PROCESS-ORDER", NameKind.ClassName, TargetLanguage.Java);
        result.Should().Be("ProcessOrder");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassName_WithSuffix_AppendsSuffix()
    {
        var enforcer = CreateEnforcer(classNameSuffix: "Service");
        var result = enforcer.ConvertNameDeterministic("CUSTOMER", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("CustomerService");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassName_WithPrefix_PrependsPrefix()
    {
        var enforcer = CreateEnforcer(classNamePrefix: "Legacy");
        var result = enforcer.ConvertNameDeterministic("CUSTOMER", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("LegacyCustomer");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassName_WithPrefixAndSuffix_BothApplied()
    {
        var enforcer = CreateEnforcer(classNamePrefix: "Legacy", classNameSuffix: "Processor");
        var result = enforcer.ConvertNameDeterministic("PAYROLL", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("LegacyPayrollProcessor");
    }

    // ────────────────────────────────────────────────────────────────
    // ConvertNameDeterministic – NameKind.MethodName
    // ────────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_MethodName_ReturnsCamelCase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("VALIDATE-CUSTOMER-DATA", NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().Be("validateCustomerData");
    }

    [Fact]
    public void ConvertNameDeterministic_MethodName_SingleWord_ReturnsLowercase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("CALCULATE", NameKind.MethodName, TargetLanguage.Java);
        result.Should().Be("calculate");
    }

    // ────────────────────────────────────────────────────────────────
    // ConvertNameDeterministic – NameKind.PropertyName / FieldName / ParameterName
    // ────────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_PropertyName_ReturnsPascalCase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("CUSTOMER-ID", NameKind.PropertyName, TargetLanguage.CSharp);
        result.Should().Be("CustomerId");
    }

    [Fact]
    public void ConvertNameDeterministic_FieldName_ReturnsCamelCase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("ACCOUNT-BALANCE", NameKind.FieldName, TargetLanguage.CSharp);
        result.Should().Be("accountBalance");
    }

    [Fact]
    public void ConvertNameDeterministic_ParameterName_ReturnsCamelCase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("INPUT-RECORD", NameKind.ParameterName, TargetLanguage.Java);
        result.Should().Be("inputRecord");
    }

    // ────────────────────────────────────────────────────────────────
    // ConvertNameDeterministic – NameKind.ConstantName
    // ────────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_ConstantName_ReturnsUpperSnakeCase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("MAX-RETRY-COUNT", NameKind.ConstantName, TargetLanguage.CSharp);
        result.Should().Be("MAX_RETRY_COUNT");
    }

    [Fact]
    public void ConvertNameDeterministic_ConstantName_SingleWord_ReturnsUppercase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("LIMIT", NameKind.ConstantName, TargetLanguage.Java);
        result.Should().Be("LIMIT");
    }

    // ────────────────────────────────────────────────────────────────
    // ConvertNameDeterministic – NameKind.EnumMemberName
    // ────────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_EnumMemberName_ReturnsPascalCase()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("STATUS-ACTIVE", NameKind.EnumMemberName, TargetLanguage.CSharp);
        result.Should().Be("StatusActive");
    }

    // ────────────────────────────────────────────────────────────────
    // ConvertNameDeterministic – COBOL prefix stripping
    // ────────────────────────────────────────────────────────────────

    [Theory]
    [InlineData("WS-CUSTOMER-ID")]
    [InlineData("LS-CUSTOMER-ID")]
    [InlineData("WK-CUSTOMER-ID")]
    [InlineData("LK-CUSTOMER-ID")]
    [InlineData("FD-CUSTOMER-ID")]
    [InlineData("SD-CUSTOMER-ID")]
    public void ConvertNameDeterministic_StripsCommonCOBOLPrefixes(string legacyName)
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic(legacyName, NameKind.MethodName, TargetLanguage.CSharp);
        // The prefix should be stripped; "CUSTOMER-ID" → "customerId"
        result.Should().Be("customerId");
    }

    // ────────────────────────────────────────────────────────────────
    // ConvertNameDeterministic – reserved word escaping
    // ────────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_CSharpReservedWord_ClassKind_EscapesWithAtPrefix()
    {
        var enforcer = CreateEnforcer();
        // "CLASS" as a ClassName in C# should be escaped
        var result = enforcer.ConvertNameDeterministic("CLASS", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("@Class");
    }

    [Fact]
    public void ConvertNameDeterministic_JavaReservedWord_ClassKind_EscapesWithUnderscore()
    {
        var enforcer = CreateEnforcer();
        // "CLASS" as a ClassName in Java should be escaped with underscore
        var result = enforcer.ConvertNameDeterministic("CLASS", NameKind.ClassName, TargetLanguage.Java);
        result.Should().Be("Class_");
    }

    [Fact]
    public void ConvertNameDeterministic_NonReservedWord_NotEscaped()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("CUSTOMER", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("Customer");
    }

    // ────────────────────────────────────────────────────────────────
    // ConvertNameDeterministic – edge cases
    // ────────────────────────────────────────────────────────────────

    [Fact]
    public void ConvertNameDeterministic_EmptyInput_ReturnsEmptyString()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic(string.Empty, NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().BeEmpty();
    }

    [Fact]
    public void ConvertNameDeterministic_WhitespaceInput_ReturnsEmptyString()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("   ", NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().BeEmpty();
    }

    [Fact]
    public void ConvertNameDeterministic_UnderscoreSeparated_ConvertsCorrectly()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("ACCOUNT_BALANCE_TOTAL", NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().Be("accountBalanceTotal");
    }

    // ────────────────────────────────────────────────────────────────
    // ValidateName
    // ────────────────────────────────────────────────────────────────

    [Theory]
    [InlineData("CustomerService")]
    [InlineData("PaymentProcessor")]
    [InlineData("A")]
    public void ValidateName_ValidClassName_ReturnsTrue(string name)
    {
        var enforcer = CreateEnforcer();
        enforcer.ValidateName(name, NameKind.ClassName).Should().BeTrue();
    }

    [Theory]
    [InlineData("customerService")]  // starts with lowercase
    [InlineData("Customer-Service")] // contains hyphen
    [InlineData("123Customer")]       // starts with digit
    public void ValidateName_InvalidClassName_ReturnsFalse(string name)
    {
        var enforcer = CreateEnforcer();
        enforcer.ValidateName(name, NameKind.ClassName).Should().BeFalse();
    }

    [Theory]
    [InlineData("validateCustomer")]
    [InlineData("processOrder")]
    [InlineData("a")]
    public void ValidateName_ValidMethodName_ReturnsTrue(string name)
    {
        var enforcer = CreateEnforcer();
        enforcer.ValidateName(name, NameKind.MethodName).Should().BeTrue();
    }

    [Theory]
    [InlineData("ValidateCustomer")] // starts with uppercase
    [InlineData("process-order")]     // contains hyphen
    public void ValidateName_InvalidMethodName_ReturnsFalse(string name)
    {
        var enforcer = CreateEnforcer();
        enforcer.ValidateName(name, NameKind.MethodName).Should().BeFalse();
    }

    [Theory]
    [InlineData("MAX_RETRY_COUNT")]
    [InlineData("LIMIT")]
    [InlineData("A")]
    public void ValidateName_ValidConstantName_ReturnsTrue(string name)
    {
        var enforcer = CreateEnforcer();
        enforcer.ValidateName(name, NameKind.ConstantName).Should().BeTrue();
    }

    [Theory]
    [InlineData("maxRetryCount")]   // camelCase
    [InlineData("Max_Retry")]       // mixed case
    public void ValidateName_InvalidConstantName_ReturnsFalse(string name)
    {
        var enforcer = CreateEnforcer();
        enforcer.ValidateName(name, NameKind.ConstantName).Should().BeFalse();
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void ValidateName_NullOrWhitespace_ReturnsFalse(string name)
    {
        var enforcer = CreateEnforcer();
        enforcer.ValidateName(name, NameKind.ClassName).Should().BeFalse();
    }

    // ────────────────────────────────────────────────────────────────
    // SuggestCorrectedName
    // ────────────────────────────────────────────────────────────────

    [Fact]
    public void SuggestCorrectedName_InvalidCamelCase_ReturnsCorrectedCamelCase()
    {
        var enforcer = CreateEnforcer();
        // "ValidateCustomer" (PascalCase) is invalid for a method; should become "validateCustomer"
        var result = enforcer.SuggestCorrectedName("ValidateCustomer", NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().MatchRegex("^[a-z]");
    }

    [Fact]
    public void SuggestCorrectedName_InvalidPascalCase_ReturnsCorrectedPascalCase()
    {
        var enforcer = CreateEnforcer();
        // "validate-customer" is not valid for ClassName; should become PascalCase
        var result = enforcer.SuggestCorrectedName("validate-customer", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().MatchRegex("^[A-Z]");
    }

    [Fact]
    public void SuggestCorrectedName_WithCOBOLHyphens_ReturnsValidName()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.SuggestCorrectedName("CALC-TAX-AMOUNT", NameKind.MethodName, TargetLanguage.Java);
        result.Should().Be("calcTaxAmount");
        enforcer.ValidateName(result, NameKind.MethodName).Should().BeTrue();
    }
}
