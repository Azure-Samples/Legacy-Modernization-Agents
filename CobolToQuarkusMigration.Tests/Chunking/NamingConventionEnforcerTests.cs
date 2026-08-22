using CobolToQuarkusMigration.Chunking.Core;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;
// TargetLanguage and ConversionSettings are both in CobolToQuarkusMigration.Models

namespace CobolToQuarkusMigration.Tests.Chunking;

public sealed class NamingConventionEnforcerTests
{
    private static NamingConventionEnforcer CreateEnforcer(
        string classNamePrefix = "",
        string classNameSuffix = "")
    {
        var settings = new ConversionSettings
        {
            ClassNamePrefix = classNamePrefix,
            ClassNameSuffix = classNameSuffix
        };
        return new NamingConventionEnforcer(settings);
    }

    // ------------------------------------------------------------------
    // ConvertNameDeterministic – ClassName
    // ------------------------------------------------------------------

    [Fact]
    public void ConvertNameDeterministic_ClassName_WithHyphenatedCobolName_ReturnsPascalCase()
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(
            "VALIDATE-CUSTOMER-DATA", NameKind.ClassName, TargetLanguage.CSharp);

        result.Should().Be("ValidateCustomerData");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassName_WithClassNameSuffix_AppendsSuffix()
    {
        var enforcer = CreateEnforcer(classNameSuffix: "Service");

        var result = enforcer.ConvertNameDeterministic(
            "CUSTOMER-DATA", NameKind.ClassName, TargetLanguage.CSharp);

        result.Should().Be("CustomerDataService");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassName_WithClassNamePrefix_PrependsPrefix()
    {
        var enforcer = CreateEnforcer(classNamePrefix: "Legacy");

        var result = enforcer.ConvertNameDeterministic(
            "PAYMENT", NameKind.ClassName, TargetLanguage.CSharp);

        result.Should().Be("LegacyPayment");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassName_WithPrefixAndSuffix_AppliesBoth()
    {
        var enforcer = CreateEnforcer(classNamePrefix: "My", classNameSuffix: "Handler");

        var result = enforcer.ConvertNameDeterministic(
            "ORDER", NameKind.ClassName, TargetLanguage.CSharp);

        result.Should().Be("MyOrderHandler");
    }

    // ------------------------------------------------------------------
    // ConvertNameDeterministic – MethodName / FieldName / ParameterName
    // ------------------------------------------------------------------

    [Theory]
    [InlineData(NameKind.MethodName)]
    [InlineData(NameKind.FieldName)]
    [InlineData(NameKind.ParameterName)]
    public void ConvertNameDeterministic_CamelCaseKinds_ReturnsCamelCase(NameKind nameType)
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(
            "VALIDATE-CUSTOMER", nameType, TargetLanguage.CSharp);

        result.Should().Be("validateCustomer");
    }

    [Fact]
    public void ConvertNameDeterministic_PropertyName_ReturnsPascalCase()
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(
            "ACCOUNT-BALANCE", NameKind.PropertyName, TargetLanguage.CSharp);

        result.Should().Be("AccountBalance");
    }

    [Fact]
    public void ConvertNameDeterministic_ConstantName_ReturnsUpperSnakeCase()
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(
            "MAX-RETRY-COUNT", NameKind.ConstantName, TargetLanguage.CSharp);

        result.Should().Be("MAX_RETRY_COUNT");
    }

    [Fact]
    public void ConvertNameDeterministic_EnumMemberName_ReturnsPascalCase()
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(
            "STATUS-ACTIVE", NameKind.EnumMemberName, TargetLanguage.CSharp);

        result.Should().Be("StatusActive");
    }

    // ------------------------------------------------------------------
    // ConvertNameDeterministic – COBOL prefix stripping
    // ------------------------------------------------------------------

    [Theory]
    [InlineData("WS-ACCOUNT-NAME", "AccountName")]
    [InlineData("LS-ACCOUNT-NAME", "AccountName")]
    [InlineData("WK-COUNTER", "Counter")]
    [InlineData("LK-PARAM", "Param")]
    [InlineData("FD-FILE", "File")]
    [InlineData("SD-SORT", "Sort")]
    public void ConvertNameDeterministic_StripsCommonCobolPrefixes(string input, string expected)
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(
            input, NameKind.ClassName, TargetLanguage.CSharp);

        result.Should().Be(expected);
    }

    [Fact]
    public void ConvertNameDeterministic_UnknownPrefix_IsNotStripped()
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(
            "XX-ACCOUNT", NameKind.ClassName, TargetLanguage.CSharp);

        // "XX-ACCOUNT" → "XxAccount" (no stripping)
        result.Should().Be("XxAccount");
    }

    // ------------------------------------------------------------------
    // ConvertNameDeterministic – reserved word escaping
    // ------------------------------------------------------------------

    [Theory]
    [InlineData("CLASS", TargetLanguage.CSharp, "@Class")]
    [InlineData("INT", TargetLanguage.CSharp, "@Int")]
    [InlineData("STRING", TargetLanguage.CSharp, "@String")]
    public void ConvertNameDeterministic_CSharpReservedWord_AddsPrefixEscape(
        string input, TargetLanguage lang, string expected)
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(input, NameKind.ClassName, lang);

        result.Should().Be(expected);
    }

    [Theory]
    [InlineData("CLASS", TargetLanguage.Java, "Class_")]
    [InlineData("INT", TargetLanguage.Java, "Int_")]
    public void ConvertNameDeterministic_JavaReservedWord_AddsUnderscoreSuffix(
        string input, TargetLanguage lang, string expected)
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(input, NameKind.ClassName, lang);

        result.Should().Be(expected);
    }

    // ------------------------------------------------------------------
    // ConvertNameDeterministic – edge cases
    // ------------------------------------------------------------------

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void ConvertNameDeterministic_EmptyOrWhitespace_ReturnsEmptyString(string input)
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(input, NameKind.ClassName, TargetLanguage.CSharp);

        result.Should().BeEmpty();
    }

    [Fact]
    public void ConvertNameDeterministic_SingleWord_ConvertsCorrectly()
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic("ACCOUNT", NameKind.ClassName, TargetLanguage.CSharp);

        result.Should().Be("Account");
    }

    [Fact]
    public void ConvertNameDeterministic_UnderscoreSeparated_SplitsOnUnderscore()
    {
        var enforcer = CreateEnforcer();

        var result = enforcer.ConvertNameDeterministic(
            "GET_ACCOUNT_BALANCE", NameKind.MethodName, TargetLanguage.CSharp);

        result.Should().Be("getAccountBalance");
    }

    // ------------------------------------------------------------------
    // ValidateName
    // ------------------------------------------------------------------

    [Theory]
    [InlineData("MyClass", NameKind.ClassName, true)]
    [InlineData("myClass", NameKind.ClassName, false)]
    [InlineData("My_Class", NameKind.ClassName, false)]
    public void ValidateName_ClassName_ValidatesUpperCamelCase(string name, NameKind kind, bool expected)
    {
        var enforcer = CreateEnforcer();

        enforcer.ValidateName(name, kind).Should().Be(expected);
    }

    [Theory]
    [InlineData("myMethod", NameKind.MethodName, true)]
    [InlineData("MyMethod", NameKind.MethodName, false)]
    [InlineData("my_method", NameKind.MethodName, false)]
    public void ValidateName_MethodName_ValidatesLowerCamelCase(string name, NameKind kind, bool expected)
    {
        var enforcer = CreateEnforcer();

        enforcer.ValidateName(name, kind).Should().Be(expected);
    }

    [Theory]
    [InlineData("MAX_RETRIES", NameKind.ConstantName, true)]
    [InlineData("MaxRetries", NameKind.ConstantName, false)]
    [InlineData("max_retries", NameKind.ConstantName, false)]
    public void ValidateName_ConstantName_ValidatesUpperSnakeCase(string name, NameKind kind, bool expected)
    {
        var enforcer = CreateEnforcer();

        enforcer.ValidateName(name, kind).Should().Be(expected);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void ValidateName_EmptyOrWhitespace_ReturnsFalse(string name)
    {
        var enforcer = CreateEnforcer();

        enforcer.ValidateName(name, NameKind.ClassName).Should().BeFalse();
    }

    [Theory]
    [InlineData(NameKind.FieldName)]
    [InlineData(NameKind.ParameterName)]
    public void ValidateName_FieldAndParameterName_ValidatesLowerCamelCase(NameKind kind)
    {
        var enforcer = CreateEnforcer();

        enforcer.ValidateName("myField", kind).Should().BeTrue();
        enforcer.ValidateName("MyField", kind).Should().BeFalse();
    }

    [Fact]
    public void ValidateName_PropertyName_ValidatesUpperCamelCase()
    {
        var enforcer = CreateEnforcer();

        enforcer.ValidateName("AccountBalance", NameKind.PropertyName).Should().BeTrue();
        enforcer.ValidateName("accountBalance", NameKind.PropertyName).Should().BeFalse();
    }

    // ------------------------------------------------------------------
    // SuggestCorrectedName
    // ------------------------------------------------------------------

    [Fact]
    public void SuggestCorrectedName_InvalidName_ReturnsDeterministicallyConvertedName()
    {
        var enforcer = CreateEnforcer();

        var suggestion = enforcer.SuggestCorrectedName(
            "VALIDATE-DATA", NameKind.ClassName, TargetLanguage.CSharp);

        // Should apply the same conversion as ConvertNameDeterministic
        var expected = enforcer.ConvertNameDeterministic(
            "VALIDATE-DATA", NameKind.ClassName, TargetLanguage.CSharp);

        suggestion.Should().Be(expected);
    }

    [Fact]
    public void SuggestCorrectedName_ForMethodName_ReturnsCamelCase()
    {
        var enforcer = CreateEnforcer();

        var suggestion = enforcer.SuggestCorrectedName(
            "PROCESS-BATCH", NameKind.MethodName, TargetLanguage.Java);

        suggestion.Should().Be("processBatch");
    }
}
