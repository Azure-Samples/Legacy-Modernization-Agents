using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Chunking.Core;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Tests.Chunking.Core;

public class NamingConventionEnforcerTests
{
    private static NamingConventionEnforcer CreateEnforcer(string classPrefix = "", string classSuffix = "")
        => new(new ConversionSettings { ClassNamePrefix = classPrefix, ClassNameSuffix = classSuffix });

    // -------------------------------------------------------------------------
    // ConvertNameDeterministic – empty / whitespace input
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void ConvertNameDeterministic_EmptyOrWhitespace_ReturnsEmptyString(string input)
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic(input, NameKind.MethodName, TargetLanguage.Java);
        result.Should().BeEmpty();
    }

    // -------------------------------------------------------------------------
    // ConvertNameDeterministic – Pascal case (ClassName, PropertyName, EnumMemberName)
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("VALIDATE-CUSTOMER-DATA", NameKind.ClassName, "ValidateCustomerData")]
    [InlineData("PROCESS-ORDER", NameKind.ClassName, "ProcessOrder")]
    [InlineData("CUSTOMER-ID", NameKind.PropertyName, "CustomerId")]
    [InlineData("ORDER-STATUS", NameKind.EnumMemberName, "OrderStatus")]
    public void ConvertNameDeterministic_PascalCaseKinds_ReturnsPascalCase(
        string legacyName, NameKind nameKind, string expected)
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic(legacyName, nameKind, TargetLanguage.Java);
        result.Should().Be(expected);
    }

    // -------------------------------------------------------------------------
    // ConvertNameDeterministic – camelCase (MethodName, FieldName, ParameterName, default)
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("VALIDATE-CUSTOMER-DATA", NameKind.MethodName, "validateCustomerData")]
    [InlineData("PROCESS-ORDER", NameKind.FieldName, "processOrder")]
    [InlineData("CUSTOMER-ID", NameKind.ParameterName, "customerId")]
    public void ConvertNameDeterministic_CamelCaseKinds_ReturnsCamelCase(
        string legacyName, NameKind nameKind, string expected)
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic(legacyName, nameKind, TargetLanguage.Java);
        result.Should().Be(expected);
    }

    // -------------------------------------------------------------------------
    // ConvertNameDeterministic – UPPER_SNAKE_CASE (ConstantName)
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("MAX-RETRY-COUNT", "MAX_RETRY_COUNT")]
    [InlineData("DEFAULT-TIMEOUT", "DEFAULT_TIMEOUT")]
    [InlineData("PI", "PI")]
    public void ConvertNameDeterministic_ConstantName_ReturnsUpperSnakeCase(string legacyName, string expected)
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic(legacyName, NameKind.ConstantName, TargetLanguage.Java);
        result.Should().Be(expected);
    }

    // -------------------------------------------------------------------------
    // ConvertNameDeterministic – stripping well-known COBOL prefixes
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("WS-CUSTOMER-ID", "customerId")]
    [InlineData("LS-ORDER-DATE", "orderDate")]
    [InlineData("WK-TOTAL-AMOUNT", "totalAmount")]
    [InlineData("LK-BATCH-SIZE", "batchSize")]
    [InlineData("FD-MASTER-FILE", "masterFile")]
    [InlineData("SD-SORT-KEY", "sortKey")]
    public void ConvertNameDeterministic_WithCobolPrefix_StripsPrefix(string legacyName, string expected)
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic(legacyName, NameKind.FieldName, TargetLanguage.Java);
        result.Should().Be(expected);
    }

    [Fact]
    public void ConvertNameDeterministic_NoKnownPrefix_DoesNotStrip()
    {
        var enforcer = CreateEnforcer();
        // "XX-" is not a known COBOL prefix
        var result = enforcer.ConvertNameDeterministic("XX-CUSTOMER", NameKind.FieldName, TargetLanguage.Java);
        result.Should().Be("xxCustomer");
    }

    // -------------------------------------------------------------------------
    // ConvertNameDeterministic – underscore separators
    // -------------------------------------------------------------------------

    [Fact]
    public void ConvertNameDeterministic_UnderscoreSeparated_SplitsCorrectly()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("VALIDATE_ORDER_DATA", NameKind.MethodName, TargetLanguage.Java);
        result.Should().Be("validateOrderData");
    }

    // -------------------------------------------------------------------------
    // ConvertNameDeterministic – reserved word escaping
    // -------------------------------------------------------------------------

    [Fact]
    public void ConvertNameDeterministic_CSharpReservedWord_PrefixedWithAt()
    {
        var enforcer = CreateEnforcer();
        // "CLASS" → className → "Class" which is a C# reserved word
        var result = enforcer.ConvertNameDeterministic("CLASS", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().Be("@Class");
    }

    [Fact]
    public void ConvertNameDeterministic_JavaReservedWord_SuffixedWithUnderscore()
    {
        var enforcer = CreateEnforcer();
        // "CLASS" → className → "Class" which is a Java reserved word
        var result = enforcer.ConvertNameDeterministic("CLASS", NameKind.ClassName, TargetLanguage.Java);
        result.Should().Be("Class_");
    }

    [Fact]
    public void ConvertNameDeterministic_NonReservedWord_NotEscaped()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("VALIDATE-ORDER", NameKind.ClassName, TargetLanguage.CSharp);
        // "ValidateOrder" is not a reserved word
        result.Should().Be("ValidateOrder");
        result.Should().NotStartWith("@");
    }

    // -------------------------------------------------------------------------
    // ConvertNameDeterministic – class name prefix / suffix from settings
    // -------------------------------------------------------------------------

    [Fact]
    public void ConvertNameDeterministic_WithClassNameSuffix_AppendsSuffix()
    {
        var enforcer = CreateEnforcer(classSuffix: "Service");
        var result = enforcer.ConvertNameDeterministic("CUSTOMER-DATA", NameKind.ClassName, TargetLanguage.Java);
        result.Should().Be("CustomerDataService");
    }

    [Fact]
    public void ConvertNameDeterministic_WithClassNamePrefix_PrependsPrefix()
    {
        var enforcer = CreateEnforcer(classPrefix: "Legacy");
        var result = enforcer.ConvertNameDeterministic("CUSTOMER-DATA", NameKind.ClassName, TargetLanguage.Java);
        result.Should().Be("LegacyCustomerData");
    }

    [Fact]
    public void ConvertNameDeterministic_ClassPrefixAndSuffix_AppliesBoth()
    {
        var enforcer = CreateEnforcer(classPrefix: "Legacy", classSuffix: "Svc");
        var result = enforcer.ConvertNameDeterministic("ORDER", NameKind.ClassName, TargetLanguage.Java);
        result.Should().Be("LegacyOrderSvc");
    }

    [Fact]
    public void ConvertNameDeterministic_PrefixAndSuffix_NotAppliedToNonClassKinds()
    {
        var enforcer = CreateEnforcer(classPrefix: "Legacy", classSuffix: "Service");
        // prefix/suffix should only apply to ClassName
        var result = enforcer.ConvertNameDeterministic("CUSTOMER", NameKind.MethodName, TargetLanguage.Java);
        result.Should().Be("customer");
        result.Should().NotContain("Legacy");
        result.Should().NotContain("Service");
    }

    // -------------------------------------------------------------------------
    // ConvertNameDeterministic – input is lower-case (normalised to upper before processing)
    // -------------------------------------------------------------------------

    [Fact]
    public void ConvertNameDeterministic_LowerCaseInput_NormalisedCorrectly()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.ConvertNameDeterministic("validate-order", NameKind.MethodName, TargetLanguage.Java);
        result.Should().Be("validateOrder");
    }

    // -------------------------------------------------------------------------
    // ValidateName
    // -------------------------------------------------------------------------

    [Fact]
    public void ValidateName_EmptyString_ReturnsFalse()
    {
        var enforcer = CreateEnforcer();
        enforcer.ValidateName("", NameKind.ClassName).Should().BeFalse();
        enforcer.ValidateName("   ", NameKind.ClassName).Should().BeFalse();
    }

    [Theory]
    [InlineData("CustomerData", NameKind.ClassName, true)]
    [InlineData("customerData", NameKind.ClassName, false)]   // must start upper
    [InlineData("Customer-Data", NameKind.ClassName, false)]  // hyphens not allowed
    [InlineData("validateOrder", NameKind.MethodName, true)]
    [InlineData("ValidateOrder", NameKind.MethodName, false)]  // must start lower
    [InlineData("CustomerName", NameKind.PropertyName, true)]
    [InlineData("customerName", NameKind.PropertyName, false)]
    [InlineData("customerName", NameKind.FieldName, true)]
    [InlineData("CustomerName", NameKind.FieldName, false)]
    [InlineData("param", NameKind.ParameterName, true)]
    [InlineData("Param", NameKind.ParameterName, false)]
    [InlineData("MAX_RETRY", NameKind.ConstantName, true)]
    [InlineData("max_retry", NameKind.ConstantName, false)]    // must be upper
    [InlineData("MAX-RETRY", NameKind.ConstantName, false)]    // hyphens not allowed
    public void ValidateName_VariousInputs_ReturnsExpectedResult(
        string name, NameKind kind, bool expected)
    {
        var enforcer = CreateEnforcer();
        enforcer.ValidateName(name, kind).Should().Be(expected);
    }

    [Fact]
    public void ValidateName_UnknownNameKind_ReturnsTrue()
    {
        var enforcer = CreateEnforcer();
        // The switch default case returns true for unrecognised kinds
        enforcer.ValidateName("anything", (NameKind)999).Should().BeTrue();
    }

    // -------------------------------------------------------------------------
    // SuggestCorrectedName – delegates to ConvertNameDeterministic
    // -------------------------------------------------------------------------

    [Fact]
    public void SuggestCorrectedName_InvalidName_ReturnsConvertedName()
    {
        var enforcer = CreateEnforcer();
        var suggested = enforcer.SuggestCorrectedName("VALIDATE-CUSTOMER", NameKind.MethodName, TargetLanguage.Java);
        // Should produce same result as ConvertNameDeterministic
        var expected = enforcer.ConvertNameDeterministic("VALIDATE-CUSTOMER", NameKind.MethodName, TargetLanguage.Java);
        suggested.Should().Be(expected);
    }

    [Fact]
    public void SuggestCorrectedName_EmptyInput_ReturnsEmptyString()
    {
        var enforcer = CreateEnforcer();
        var result = enforcer.SuggestCorrectedName("", NameKind.ClassName, TargetLanguage.CSharp);
        result.Should().BeEmpty();
    }

    // -------------------------------------------------------------------------
    // Round-trip: name converted and then validated
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("VALIDATE-CUSTOMER-DATA", NameKind.ClassName, TargetLanguage.Java)]
    [InlineData("PROCESS-ORDER", NameKind.MethodName, TargetLanguage.Java)]
    [InlineData("TOTAL-AMOUNT", NameKind.PropertyName, TargetLanguage.CSharp)]
    [InlineData("batch-counter", NameKind.FieldName, TargetLanguage.CSharp)]
    [InlineData("MAX-RETRY-COUNT", NameKind.ConstantName, TargetLanguage.Java)]
    public void ConvertNameDeterministic_ThenValidate_ProducesValidName(
        string legacyName, NameKind kind, TargetLanguage lang)
    {
        var enforcer = CreateEnforcer();
        var converted = enforcer.ConvertNameDeterministic(legacyName, kind, lang);

        // Converted names must not be empty
        converted.Should().NotBeNullOrEmpty();
        // When not a reserved-word escape, the result should pass validation
        var isEscaped = converted.StartsWith("@") || converted.EndsWith("_");
        if (!isEscaped)
        {
            enforcer.ValidateName(converted, kind).Should().BeTrue(
                $"converted name '{converted}' should be valid for {kind}");
        }
    }
}
