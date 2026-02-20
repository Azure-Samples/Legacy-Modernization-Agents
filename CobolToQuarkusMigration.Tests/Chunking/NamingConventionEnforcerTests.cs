using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Chunking.Core;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Tests.Chunking;

public class NamingConventionEnforcerTests
{
    private readonly ConversionSettings _defaultSettings;
    private readonly NamingConventionEnforcer _enforcer;

    public NamingConventionEnforcerTests()
    {
        _defaultSettings = new ConversionSettings
        {
            ClassNamePrefix = string.Empty,
            ClassNameSuffix = string.Empty
        };
        _enforcer = new NamingConventionEnforcer(_defaultSettings);
    }

    // --- ConvertNameDeterministic: ClassName ---

    [Fact]
    public void ConvertNameDeterministic_ClassName_ReturnsPascalCase()
    {
        var result = _enforcer.ConvertNameDeterministic("VALIDATE-CUSTOMER-DATA", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("ValidateCustomerData");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassNameWithSuffix_AppendsSuffix()
    {
        var settings = new ConversionSettings { ClassNameSuffix = "Service" };
        var enforcer = new NamingConventionEnforcer(settings);

        var result = enforcer.ConvertNameDeterministic("PROCESS-ORDER", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("ProcessOrderService");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassNameWithPrefix_PrependsPrefix()
    {
        var settings = new ConversionSettings { ClassNamePrefix = "Legacy" };
        var enforcer = new NamingConventionEnforcer(settings);

        var result = enforcer.ConvertNameDeterministic("CUSTOMER-ACCOUNT", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("LegacyCustomerAccount");
    }

    // --- ConvertNameDeterministic: MethodName ---

    [Fact]
    public void ConvertNameDeterministic_MethodName_ReturnsCamelCase()
    {
        var result = _enforcer.ConvertNameDeterministic("CALCULATE-TOTAL-AMOUNT", NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().Be("calculateTotalAmount");
    }

    [Fact]
    public void ConvertNameDeterministic_ParameterName_ReturnsCamelCase()
    {
        var result = _enforcer.ConvertNameDeterministic("CUSTOMER-ID", NameKind.ParameterName, TargetLanguage.CSharp);
        result.Should().Be("customerId");
    }

    [Fact]
    public void ConvertNameDeterministic_FieldName_ReturnsCamelCase()
    {
        var result = _enforcer.ConvertNameDeterministic("ACCOUNT-BALANCE", NameKind.FieldName, TargetLanguage.CSharp);
        result.Should().Be("accountBalance");
    }

    // --- ConvertNameDeterministic: PropertyName ---

    [Fact]
    public void ConvertNameDeterministic_PropertyName_ReturnsPascalCase()
    {
        var result = _enforcer.ConvertNameDeterministic("ORDER-DATE", NameKind.PropertyName, TargetLanguage.CSharp);
        result.Should().Be("OrderDate");
    }

    // --- ConvertNameDeterministic: ConstantName ---

    [Fact]
    public void ConvertNameDeterministic_ConstantName_ReturnsUpperSnakeCase()
    {
        var result = _enforcer.ConvertNameDeterministic("MAX-RETRY-COUNT", NameKind.ConstantName, TargetLanguage.CSharp);
        result.Should().Be("MAX_RETRY_COUNT");
    }

    // --- ConvertNameDeterministic: EnumMemberName ---

    [Fact]
    public void ConvertNameDeterministic_EnumMemberName_ReturnsPascalCase()
    {
        var result = _enforcer.ConvertNameDeterministic("STATUS-ACTIVE", NameKind.EnumMemberName, TargetLanguage.CSharp);
        result.Should().Be("StatusActive");
    }

    // --- ConvertNameDeterministic: COBOL prefix stripping ---

    [Theory]
    [InlineData("WS-CUSTOMER-ID", "customerId")]
    [InlineData("LS-ORDER-AMOUNT", "orderAmount")]
    [InlineData("WK-RESULT", "result")]
    [InlineData("LK-ACCOUNT-TYPE", "accountType")]
    [InlineData("FD-INPUT-FILE", "inputFile")]
    [InlineData("SD-SORT-FILE", "sortFile")]
    public void ConvertNameDeterministic_StripsCOBOLPrefixes_WhenPresent(string input, string expected)
    {
        var result = _enforcer.ConvertNameDeterministic(input, NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().Be(expected);
    }

    // --- ConvertNameDeterministic: empty input ---

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void ConvertNameDeterministic_EmptyOrWhitespace_ReturnsEmpty(string input)
    {
        var result = _enforcer.ConvertNameDeterministic(input, NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().BeEmpty();
    }

    // --- ConvertNameDeterministic: underscore separator ---

    [Fact]
    public void ConvertNameDeterministic_UnderscoreSeparator_SplitsCorrectly()
    {
        var result = _enforcer.ConvertNameDeterministic("validate_customer_data", NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().Be("validateCustomerData");
    }

    // --- ConvertNameDeterministic: reserved word escaping (CSharp) ---

    [Fact]
    public void ConvertNameDeterministic_CSharpReservedWord_EscapesWithAtPrefix()
    {
        // "CLASS" converts to "class" as camelCase method → reserved word in C#
        var result = _enforcer.ConvertNameDeterministic("CLASS", NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().Be("@class");
    }

    [Fact]
    public void ConvertNameDeterministic_JavaReservedWord_EscapesWithUnderscore()
    {
        // "CLASS" converts to "class" as camelCase method → reserved word in Java
        var result = _enforcer.ConvertNameDeterministic("CLASS", NameKind.MethodName, TargetLanguage.Java);
        result.Should().Be("class_");
    }

    [Fact]
    public void ConvertNameDeterministic_NonReservedWord_NoEscaping()
    {
        var result = _enforcer.ConvertNameDeterministic("PROCESS-PAYMENT", NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().Be("processPayment");
    }

    // --- ValidateName ---

    [Theory]
    [InlineData("ValidateCustomer", NameKind.ClassName, true)]
    [InlineData("validateCustomer", NameKind.ClassName, false)]
    [InlineData("123Invalid", NameKind.ClassName, false)]
    public void ValidateName_ClassName_ValidatesCorrectly(string name, NameKind kind, bool expected)
    {
        var result = _enforcer.ValidateName(name, kind);
        result.Should().Be(expected);
    }

    [Theory]
    [InlineData("processPayment", NameKind.MethodName, true)]
    [InlineData("ProcessPayment", NameKind.MethodName, false)]
    [InlineData("", NameKind.MethodName, false)]
    public void ValidateName_MethodName_ValidatesCorrectly(string name, NameKind kind, bool expected)
    {
        var result = _enforcer.ValidateName(name, kind);
        result.Should().Be(expected);
    }

    [Theory]
    [InlineData("MAX_RETRY_COUNT", NameKind.ConstantName, true)]
    [InlineData("maxRetryCount", NameKind.ConstantName, false)]
    public void ValidateName_ConstantName_ValidatesCorrectly(string name, NameKind kind, bool expected)
    {
        var result = _enforcer.ValidateName(name, kind);
        result.Should().Be(expected);
    }

    [Fact]
    public void ValidateName_NullOrWhitespace_ReturnsFalse()
    {
        _enforcer.ValidateName(string.Empty, NameKind.ClassName).Should().BeFalse();
        _enforcer.ValidateName("   ", NameKind.ClassName).Should().BeFalse();
    }

    [Fact]
    public void ValidateName_UnknownNameKind_ReturnsTrue()
    {
        // Default case returns true
        _enforcer.ValidateName("anything123", (NameKind)999).Should().BeTrue();
    }

    // --- SuggestCorrectedName ---

    [Fact]
    public void SuggestCorrectedName_InvalidLegacyName_ReturnsConvertedName()
    {
        var result = _enforcer.SuggestCorrectedName("VALIDATE-CUSTOMER", NameKind.MethodName, TargetLanguage.CSharp);
        result.Should().Be("validateCustomer");
    }

    [Fact]
    public void SuggestCorrectedName_IsDeterministic_SameInputSameOutput()
    {
        var result1 = _enforcer.SuggestCorrectedName("PROCESS-ORDER-DATA", NameKind.ClassName, TargetLanguage.Java);
        var result2 = _enforcer.SuggestCorrectedName("PROCESS-ORDER-DATA", NameKind.ClassName, TargetLanguage.Java);
        result1.Should().Be(result2);
    }
}
