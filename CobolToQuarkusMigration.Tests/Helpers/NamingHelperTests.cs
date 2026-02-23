using Xunit;
using FluentAssertions;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class NamingHelperTests
{
    // -------------------------------------------------------------------------
    // ToPascalCase
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("my_var", "MyVar")]
    [InlineData("ABC-DEF", "AbcDef")]
    [InlineData("hello world", "HelloWorld")]
    [InlineData("already", "Already")]
    [InlineData("a", "A")]
    public void ToPascalCase_WithValidInput_ReturnsExpectedPascalCase(string input, string expected)
    {
        NamingHelper.ToPascalCase(input).Should().Be(expected);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    [InlineData(null)]
    public void ToPascalCase_WithNullOrWhitespace_ReturnsEmpty(string? input)
    {
        NamingHelper.ToPascalCase(input!).Should().Be(string.Empty);
    }

    [Fact]
    public void ToPascalCase_WithOnlySeparators_ReturnsEmpty()
    {
        NamingHelper.ToPascalCase("---").Should().Be(string.Empty);
    }

    // -------------------------------------------------------------------------
    // DeriveClassNameFromCobolFile
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("RGNB649.cbl", "Rgnb649")]
    [InlineData("synthetic_50k_loc_cobol.cbl", "Synthetic50kLocCobol")]
    [InlineData("MY-PROGRAM.cob", "MyProgram")]
    [InlineData("simple.CBL", "Simple")]
    public void DeriveClassNameFromCobolFile_WithValidFilename_ReturnsExpectedClassName(
        string filename, string expected)
    {
        NamingHelper.DeriveClassNameFromCobolFile(filename).Should().Be(expected);
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    public void DeriveClassNameFromCobolFile_WithEmptyOrWhitespaceName_ReturnsFallback(string filename)
    {
        NamingHelper.DeriveClassNameFromCobolFile(filename).Should().Be("ConvertedCobolProgram");
    }

    [Fact]
    public void DeriveClassNameFromCobolFile_WhenBaseNameStartsWithDigit_PrefixesCobol()
    {
        // Extension-only filename so the basename begins with a digit after stripping
        var result = NamingHelper.DeriveClassNameFromCobolFile("123program.cbl");
        result.Should().StartWith("Cobol");
    }

    [Fact]
    public void DeriveClassNameFromCobolFile_WithFileOnlyContainingExtension_ReturnsFallback()
    {
        // Path.GetFileNameWithoutExtension(".cbl") returns "" on some platforms, falls back
        var result = NamingHelper.DeriveClassNameFromCobolFile(".cbl");
        result.Should().NotBeNullOrEmpty();
    }

    // -------------------------------------------------------------------------
    // GetFallbackClassName
    // -------------------------------------------------------------------------

    [Fact]
    public void GetFallbackClassName_ReturnsClassNameWithFallbackSuffix()
    {
        var result = NamingHelper.GetFallbackClassName("RGNB649.cbl");
        result.Should().Be("Rgnb649Fallback");
    }

    // -------------------------------------------------------------------------
    // GetOutputFileName
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("RGNB649.cbl", ".cs", "Rgnb649.cs")]
    [InlineData("MY-PROG.cbl", ".java", "MyProg.java")]
    public void GetOutputFileName_WithValidInputs_ReturnsCorrectFilename(
        string cobolFile, string extension, string expected)
    {
        NamingHelper.GetOutputFileName(cobolFile, extension).Should().Be(expected);
    }

    // -------------------------------------------------------------------------
    // IsValidIdentifier
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("MyClass", true)]
    [InlineData("_myField", true)]
    [InlineData("abc123", true)]
    [InlineData("123abc", false)]
    [InlineData("my-class", false)]
    [InlineData("my class", false)]
    [InlineData("", false)]
    [InlineData("   ", false)]
    [InlineData(null, false)]
    public void IsValidIdentifier_ReturnsExpectedResult(string? identifier, bool expected)
    {
        NamingHelper.IsValidIdentifier(identifier!).Should().Be(expected);
    }

    // -------------------------------------------------------------------------
    // IsSemanticClassName
    // -------------------------------------------------------------------------

    [Theory]
    [InlineData("PaymentBatchValidator", true)]
    [InlineData("CustomerAccountService", true)]
    [InlineData("InvoiceProcessor", true)]
    [InlineData("DataHandler", true)]
    [InlineData("ConvertedCobolProgram", false)]
    [InlineData("CobolProgram", false)]
    [InlineData("Program", false)]
    [InlineData("", false)]
    [InlineData(null, false)]
    public void IsSemanticClassName_ReturnsExpectedResult(string? className, bool expected)
    {
        NamingHelper.IsSemanticClassName(className!).Should().Be(expected);
    }

    [Fact]
    public void IsSemanticClassName_ShortNameWithoutSemanticSuffix_ReturnsFalse()
    {
        // "Rgnb649" — short, has number, no semantic suffix → false
        NamingHelper.IsSemanticClassName("Rgnb649").Should().BeFalse();
    }

    // -------------------------------------------------------------------------
    // ExtractCSharpClassName
    // -------------------------------------------------------------------------

    [Fact]
    public void ExtractCSharpClassName_WithValidClassDeclaration_ReturnsAiClassName()
    {
        var code = "public class PaymentService : IService { }";
        NamingHelper.ExtractCSharpClassName(code, "RGNB649.cbl").Should().Be("PaymentService");
    }

    [Fact]
    public void ExtractCSharpClassName_WithGenericAiClassName_FallsBackToFilename()
    {
        var code = "public class ConvertedCobolProgram { }";
        NamingHelper.ExtractCSharpClassName(code, "RGNB649.cbl").Should().Be("Rgnb649");
    }

    [Fact]
    public void ExtractCSharpClassName_WithNoClassDeclaration_FallsBackToFilename()
    {
        NamingHelper.ExtractCSharpClassName("// just a comment", "MY_PROG.cbl").Should().Be("MyProg");
    }

    // -------------------------------------------------------------------------
    // ExtractJavaClassName
    // -------------------------------------------------------------------------

    [Fact]
    public void ExtractJavaClassName_WithPublicClassDeclaration_ReturnsAiClassName()
    {
        var code = "public class InvoiceProcessor { }";
        NamingHelper.ExtractJavaClassName(code, "RGNB649.cbl").Should().Be("InvoiceProcessor");
    }

    [Fact]
    public void ExtractJavaClassName_WithGenericAiClassName_FallsBackToFilename()
    {
        var code = "public class ConvertedCobolProgram { }";
        NamingHelper.ExtractJavaClassName(code, "RGNB649.cbl").Should().Be("Rgnb649");
    }

    [Fact]
    public void ExtractJavaClassName_WithNoClassDeclaration_FallsBackToFilename()
    {
        NamingHelper.ExtractJavaClassName("// no class", "MY_PROG.cbl").Should().Be("MyProg");
    }

    // -------------------------------------------------------------------------
    // ReplaceGenericClassName
    // -------------------------------------------------------------------------

    [Fact]
    public void ReplaceGenericClassName_ReplacesClassDeclarationAndConstructorCalls()
    {
        var code = "class ConvertedCobolProgram { new ConvertedCobolProgram(); ConvertedCobolProgram(); }";
        var result = NamingHelper.ReplaceGenericClassName(code, "ConvertedCobolProgram", "PaymentService");
        result.Should().Contain("class PaymentService");
        result.Should().Contain("new PaymentService()");
        result.Should().Contain("PaymentService()");
        result.Should().NotContain("ConvertedCobolProgram");
    }

    [Fact]
    public void ReplaceGenericClassName_WhenGenericAndUniqueAreSame_ReturnsCodeUnchanged()
    {
        var code = "class MyClass { }";
        NamingHelper.ReplaceGenericClassName(code, "MyClass", "MyClass").Should().Be(code);
    }
}
