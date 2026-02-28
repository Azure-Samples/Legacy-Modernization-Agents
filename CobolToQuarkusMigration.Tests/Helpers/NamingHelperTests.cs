using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class NamingHelperTests
{
    #region ToPascalCase

    [Theory]
    [InlineData("my_var", "MyVar")]
    [InlineData("ABC-DEF", "AbcDef")]
    [InlineData("hello world", "HelloWorld")]
    [InlineData("already", "Already")]
    [InlineData("PROCESS-ORDER", "ProcessOrder")]
    [InlineData("some.thing", "SomeThing")]
    public void ToPascalCase_VariousInputs_ReturnsExpectedPascalCase(string input, string expected)
    {
        NamingHelper.ToPascalCase(input).Should().Be(expected);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    [InlineData(null)]
    public void ToPascalCase_EmptyOrWhitespace_ReturnsEmptyString(string? input)
    {
        NamingHelper.ToPascalCase(input!).Should().BeEmpty();
    }

    [Fact]
    public void ToPascalCase_SingleCharacter_ReturnsUppercased()
    {
        NamingHelper.ToPascalCase("a").Should().Be("A");
    }

    [Fact]
    public void ToPascalCase_OnlySpecialChars_ReturnsEmpty()
    {
        NamingHelper.ToPascalCase("---___").Should().BeEmpty();
    }

    #endregion

    #region DeriveClassNameFromCobolFile

    [Theory]
    [InlineData("RGNB649.cbl", "Rgnb649")]
    [InlineData("synthetic_50k_loc_cobol.cbl", "Synthetic50kLocCobol")]
    [InlineData("PAYMENT-BATCH.cbl", "PaymentBatch")]
    [InlineData("CUSTOMER_ACCOUNT.CBL", "CustomerAccount")]
    public void DeriveClassNameFromCobolFile_ValidFilenames_ReturnsExpectedClassName(string filename, string expected)
    {
        NamingHelper.DeriveClassNameFromCobolFile(filename).Should().Be(expected);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    [InlineData(".cbl")]
    public void DeriveClassNameFromCobolFile_EmptyOrExtensionOnly_ReturnsFallback(string filename)
    {
        NamingHelper.DeriveClassNameFromCobolFile(filename).Should().Be("ConvertedCobolProgram");
    }

    [Fact]
    public void DeriveClassNameFromCobolFile_FilenameStartsWithDigit_PrefixedWithCobol()
    {
        // File "123abc.cbl" → base "123abc" → starts with digit → "Cobol123abc"
        var result = NamingHelper.DeriveClassNameFromCobolFile("123abc.cbl");
        result.Should().StartWith("Cobol");
    }

    [Fact]
    public void DeriveClassNameFromCobolFile_PathIncluded_UsesFilenameOnly()
    {
        var result = NamingHelper.DeriveClassNameFromCobolFile("/some/path/RGNB649.cbl");
        result.Should().Be("Rgnb649");
    }

    #endregion

    #region GetFallbackClassName

    [Fact]
    public void GetFallbackClassName_ValidFilename_AppendsFallbackSuffix()
    {
        var result = NamingHelper.GetFallbackClassName("PAYMENT.cbl");
        result.Should().Be("PaymentFallback");
    }

    [Fact]
    public void GetFallbackClassName_EmptyFilename_ReturnsFallbackSuffix()
    {
        var result = NamingHelper.GetFallbackClassName(".cbl");
        result.Should().Be("ConvertedCobolProgramFallback");
    }

    #endregion

    #region GetOutputFileName

    [Theory]
    [InlineData("RGNB649.cbl", ".cs", "Rgnb649.cs")]
    [InlineData("PAYMENT-BATCH.cbl", ".java", "PaymentBatch.java")]
    [InlineData("MY_SERVICE.cbl", ".cs", "MyService.cs")]
    public void GetOutputFileName_ValidInputs_ReturnsCorrectFilename(string cobolFile, string extension, string expected)
    {
        NamingHelper.GetOutputFileName(cobolFile, extension).Should().Be(expected);
    }

    #endregion

    #region IsValidIdentifier

    [Theory]
    [InlineData("ValidName", true)]
    [InlineData("_underscoreStart", true)]
    [InlineData("CamelCase123", true)]
    [InlineData("123StartsWithDigit", false)]
    [InlineData("has-hyphen", false)]
    [InlineData("has space", false)]
    [InlineData("has.dot", false)]
    [InlineData("", false)]
    [InlineData("   ", false)]
    public void IsValidIdentifier_VariousInputs_ReturnsExpected(string identifier, bool expected)
    {
        NamingHelper.IsValidIdentifier(identifier).Should().Be(expected);
    }

    [Fact]
    public void IsValidIdentifier_NullInput_ReturnsFalse()
    {
        NamingHelper.IsValidIdentifier(null!).Should().BeFalse();
    }

    #endregion

    #region IsSemanticClassName

    [Theory]
    [InlineData("PaymentBatchValidator", true)]
    [InlineData("CustomerAccountProcessor", true)]
    [InlineData("OrderService", true)]
    [InlineData("DataManager", true)]
    [InlineData("LoanCalculator", true)]
    public void IsSemanticClassName_SemanticNames_ReturnsTrue(string className, bool expected)
    {
        NamingHelper.IsSemanticClassName(className).Should().Be(expected);
    }

    [Theory]
    [InlineData("ConvertedCobolProgram", false)]
    [InlineData("CobolProgram", false)]
    [InlineData("Program", false)]
    [InlineData("Main", false)]
    [InlineData("Rgnb649", false)] // Short name with number = not semantic
    public void IsSemanticClassName_GenericOrShortNames_ReturnsFalse(string className, bool expected)
    {
        NamingHelper.IsSemanticClassName(className).Should().Be(expected);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    [InlineData(null)]
    public void IsSemanticClassName_EmptyOrNull_ReturnsFalse(string? className)
    {
        NamingHelper.IsSemanticClassName(className!).Should().BeFalse();
    }

    [Fact]
    public void IsSemanticClassName_LongPascalCaseNoNumbers_ReturnsTrue()
    {
        // Multi-word PascalCase with 2+ capitals, 10+ chars, no numbers
        NamingHelper.IsSemanticClassName("CustomerAccount").Should().BeTrue();
    }

    #endregion

    #region ExtractCSharpClassName

    [Fact]
    public void ExtractCSharpClassName_CodeWithValidClass_ReturnsExtractedName()
    {
        var code = @"
public class PaymentProcessor
{
    public void Process() { }
}";
        var result = NamingHelper.ExtractCSharpClassName(code, "PAYMENT.cbl");
        result.Should().Be("PaymentProcessor");
    }

    [Fact]
    public void ExtractCSharpClassName_CodeWithGenericClass_FallsBackToFilename()
    {
        var code = @"
public class ConvertedCobolProgram
{
    public void Run() { }
}";
        var result = NamingHelper.ExtractCSharpClassName(code, "RGNB649.cbl");
        result.Should().Be("Rgnb649");
    }

    [Fact]
    public void ExtractCSharpClassName_EmptyCode_FallsBackToFilename()
    {
        var result = NamingHelper.ExtractCSharpClassName("", "PAYMENT.cbl");
        result.Should().Be("Payment");
    }

    [Fact]
    public void ExtractCSharpClassName_CodeWithInvalidIdentifier_FallsBackToFilename()
    {
        var code = "class 123Invalid { }";
        var result = NamingHelper.ExtractCSharpClassName(code, "MY_FILE.cbl");
        result.Should().Be("MyFile");
    }

    #endregion

    #region ExtractJavaClassName

    [Fact]
    public void ExtractJavaClassName_CodeWithPublicClass_ReturnsExtractedName()
    {
        var code = @"
public class OrderService {
    public void process() {}
}";
        var result = NamingHelper.ExtractJavaClassName(code, "ORDER.cbl");
        result.Should().Be("OrderService");
    }

    [Fact]
    public void ExtractJavaClassName_CodeWithPackageScopedClass_ReturnsExtractedName()
    {
        var code = @"
class CustomerHandler {
    void handle() {}
}";
        var result = NamingHelper.ExtractJavaClassName(code, "CUSTOMER.cbl");
        result.Should().Be("CustomerHandler");
    }

    [Fact]
    public void ExtractJavaClassName_CodeWithGenericName_FallsBackToFilename()
    {
        var code = @"public class ConvertedCobolProgram { }";
        var result = NamingHelper.ExtractJavaClassName(code, "RGNB649.cbl");
        result.Should().Be("Rgnb649");
    }

    [Fact]
    public void ExtractJavaClassName_EmptyCode_FallsBackToFilename()
    {
        var result = NamingHelper.ExtractJavaClassName("", "PAYMENT.cbl");
        result.Should().Be("Payment");
    }

    #endregion

    #region ReplaceGenericClassName

    [Fact]
    public void ReplaceGenericClassName_SameNames_ReturnsUnmodifiedCode()
    {
        var code = "class MyClass { new MyClass() }";
        var result = NamingHelper.ReplaceGenericClassName(code, "MyClass", "MyClass");
        result.Should().Be(code);
    }

    [Fact]
    public void ReplaceGenericClassName_DifferentNames_ReplacesAll()
    {
        var code = "class ConvertedCobolProgram { new ConvertedCobolProgram() ConvertedCobolProgram( }";
        var result = NamingHelper.ReplaceGenericClassName(code, "ConvertedCobolProgram", "PaymentProcessor");
        result.Should().Contain("class PaymentProcessor");
        result.Should().Contain("new PaymentProcessor()");
        result.Should().Contain("PaymentProcessor(");
        result.Should().NotContain("ConvertedCobolProgram");
    }

    #endregion
}
