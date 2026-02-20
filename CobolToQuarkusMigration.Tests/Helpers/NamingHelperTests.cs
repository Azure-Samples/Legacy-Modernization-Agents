using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class NamingHelperTests
{
    // --- ToPascalCase ---

    [Theory]
    [InlineData("my_var", "MyVar")]
    [InlineData("ABC-DEF", "AbcDef")]
    [InlineData("hello world", "HelloWorld")]
    [InlineData("singleword", "Singleword")]
    public void ToPascalCase_ValidInput_ReturnsPascalCase(string input, string expected)
    {
        NamingHelper.ToPascalCase(input).Should().Be(expected);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void ToPascalCase_EmptyOrWhitespace_ReturnsEmpty(string input)
    {
        NamingHelper.ToPascalCase(input).Should().BeEmpty();
    }

    // --- DeriveClassNameFromCobolFile ---

    [Theory]
    [InlineData("RGNB649.cbl", "Rgnb649")]
    [InlineData("synthetic_50k_loc_cobol.cbl", "Synthetic50kLocCobol")]
    [InlineData("CUSTOMER-ACCOUNT.cbl", "CustomerAccount")]
    [InlineData("my.program.cbl", "MyProgram")]
    public void DeriveClassNameFromCobolFile_VariousFilenames_ReturnsExpectedClassName(string filename, string expected)
    {
        NamingHelper.DeriveClassNameFromCobolFile(filename).Should().Be(expected);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    [InlineData(".cbl")]
    public void DeriveClassNameFromCobolFile_EmptyOrNoBasename_ReturnsFallback(string filename)
    {
        NamingHelper.DeriveClassNameFromCobolFile(filename).Should().Be("ConvertedCobolProgram");
    }

    [Fact]
    public void DeriveClassNameFromCobolFile_FilenameStartsWithDigit_PrependsCobol()
    {
        // Filename like "123program.cbl" → "Cobol123program"
        var result = NamingHelper.DeriveClassNameFromCobolFile("123program.cbl");
        result.Should().StartWith("Cobol");
    }

    // --- GetFallbackClassName ---

    [Fact]
    public void GetFallbackClassName_ValidFilename_AppendsFallbackSuffix()
    {
        var result = NamingHelper.GetFallbackClassName("CUSTOMER.cbl");
        result.Should().Be("CustomerFallback");
    }

    // --- GetOutputFileName ---

    [Theory]
    [InlineData("CUSTOMER.cbl", ".cs", "Customer.cs")]
    [InlineData("CUSTOMER.cbl", ".java", "Customer.java")]
    [InlineData("ORDER_PROCESS.cbl", ".cs", "OrderProcess.cs")]
    public void GetOutputFileName_ValidInputs_ReturnsCorrectFilename(string cobolFile, string extension, string expected)
    {
        NamingHelper.GetOutputFileName(cobolFile, extension).Should().Be(expected);
    }

    // --- IsValidIdentifier ---

    [Theory]
    [InlineData("ValidName", true)]
    [InlineData("_privateName", true)]
    [InlineData("name123", true)]
    [InlineData("123invalid", false)]
    [InlineData("has-hyphen", false)]
    [InlineData("has space", false)]
    [InlineData("", false)]
    [InlineData("   ", false)]
    public void IsValidIdentifier_VariousInputs_ReturnsExpected(string identifier, bool expected)
    {
        NamingHelper.IsValidIdentifier(identifier).Should().Be(expected);
    }

    // --- IsSemanticClassName ---

    [Theory]
    [InlineData("PaymentBatchValidator", true)]
    [InlineData("CustomerAccountService", true)]
    [InlineData("OrderProcessor", true)]
    [InlineData("ConvertedCobolProgram", false)]
    [InlineData("CobolProgram", false)]
    [InlineData("Program", false)]
    [InlineData("Rgnb649", false)]    // has digit
    [InlineData("", false)]
    public void IsSemanticClassName_VariousNames_ReturnsExpected(string className, bool expected)
    {
        NamingHelper.IsSemanticClassName(className).Should().Be(expected);
    }

    // --- ExtractCSharpClassName ---

    [Fact]
    public void ExtractCSharpClassName_ValidCSharpCode_ExtractsClassName()
    {
        var code = "public class CustomerValidator : IValidator\n{\n}";
        var result = NamingHelper.ExtractCSharpClassName(code, "CUSTOMER.cbl");
        result.Should().Be("CustomerValidator");
    }

    [Fact]
    public void ExtractCSharpClassName_GenericClassName_FallsBackToFilename()
    {
        var code = "public class ConvertedCobolProgram\n{\n}";
        var result = NamingHelper.ExtractCSharpClassName(code, "RGNB649.cbl");
        result.Should().Be("Rgnb649");
    }

    [Fact]
    public void ExtractCSharpClassName_NoClassDeclaration_FallsBackToFilename()
    {
        // No "class " keyword present at all
        var code = "// this is just a comment";
        var result = NamingHelper.ExtractCSharpClassName(code, "ORDER.cbl");
        result.Should().Be("Order");
    }

    // --- ExtractJavaClassName ---

    [Fact]
    public void ExtractJavaClassName_ValidJavaCode_ExtractsClassName()
    {
        var code = "public class PaymentService {\n}";
        var result = NamingHelper.ExtractJavaClassName(code, "PAYMENT.cbl");
        result.Should().Be("PaymentService");
    }

    [Fact]
    public void ExtractJavaClassName_FallsBackToNonPublicClass()
    {
        var code = "class OrderHandler {\n}";
        var result = NamingHelper.ExtractJavaClassName(code, "ORDER.cbl");
        result.Should().Be("OrderHandler");
    }

    [Fact]
    public void ExtractJavaClassName_GenericClassName_FallsBackToFilename()
    {
        var code = "public class ConvertedCobolProgram {\n}";
        var result = NamingHelper.ExtractJavaClassName(code, "ACCOUNT.cbl");
        result.Should().Be("Account");
    }

    // --- ReplaceGenericClassName ---

    [Fact]
    public void ReplaceGenericClassName_ReplacesClassDeclarationAndUsages()
    {
        var code = "class ConvertedCobolProgram\nnew ConvertedCobolProgram()\nConvertedCobolProgram()";
        var result = NamingHelper.ReplaceGenericClassName(code, "ConvertedCobolProgram", "CustomerValidator");
        result.Should().Contain("class CustomerValidator");
        result.Should().Contain("new CustomerValidator()");
        result.Should().NotContain("ConvertedCobolProgram");
    }

    [Fact]
    public void ReplaceGenericClassName_SameNames_ReturnsUnchanged()
    {
        var code = "class MyClass { new MyClass() }";
        var result = NamingHelper.ReplaceGenericClassName(code, "MyClass", "MyClass");
        result.Should().Be(code);
    }
}
