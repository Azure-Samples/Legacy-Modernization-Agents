using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class NamingHelperTests
{
    [Theory]
    [InlineData("PAYB649.cbl", "Payb649")]
    [InlineData("synthetic_50k_loc_cobol.cbl", "Synthetic50kLocCobol")]
    [InlineData("/src/cobol/ORD-MAIN.cob", "OrdMain")]
    [InlineData("649PAY.cbl", "Cobol649pay")]
    [InlineData(".cbl", "ConvertedCobolProgram")]
    [InlineData("", "ConvertedCobolProgram")]
    public void DerivedClassNameIsAValidIdentifierUniquePerSourceFile(string file, string expected)
    {
        var name = NamingHelper.DeriveClassNameFromCobolFile(file);

        name.Should().Be(expected);
        NamingHelper.IsValidIdentifier(name).Should().BeTrue();
    }

    [Fact]
    public void OutputFileNameCombinesDerivedClassNameAndExtension()
    {
        NamingHelper.GetOutputFileName("PAY-RUN.cbl", ".java").Should().Be("PayRun.java");
        NamingHelper.GetFallbackClassName("PAY-RUN.cbl").Should().Be("PayRunFallback");
    }

    [Theory]
    [InlineData("my_var", "MyVar")]
    [InlineData("ABC-DEF", "AbcDef")]
    [InlineData("  ", "")]
    public void PascalCaseSplitsOnNonAlphanumerics(string input, string expected) =>
        NamingHelper.ToPascalCase(input).Should().Be(expected);

    [Theory]
    [InlineData("_x1", true)]
    [InlineData("Abc9", true)]
    [InlineData("9abc", false)]
    [InlineData("a-b", false)]
    [InlineData("", false)]
    public void IdentifierValidationFollowsLeadingCharacterAndBodyRules(string id, bool valid) =>
        NamingHelper.IsValidIdentifier(id).Should().Be(valid);

    [Theory]
    [InlineData("PaymentBatchValidator", true)]
    [InlineData("CustomerAccount", true)]
    [InlineData("Payb649", false)]
    [InlineData("ConvertedCobolProgram", false)]
    [InlineData("Program", false)]
    [InlineData("", false)]
    public void SemanticClassNamesAreDistinguishedFromGenericAndFilenameDerivedNames(string name, bool semantic) =>
        NamingHelper.IsSemanticClassName(name).Should().Be(semantic);

    [Fact]
    public void ExtractedCSharpClassNameIsPreferredOverFilenameDerivedName()
    {
        const string code = "namespace X;\npublic class PaymentBatchValidator : Base\n{\n}";

        NamingHelper.ExtractCSharpClassName(code, "PAYB649.cbl").Should().Be("PaymentBatchValidator");
    }

    [Theory]
    [InlineData("public class ConvertedCobolProgram {}")]
    [InlineData("no declaration here")]
    public void CSharpClassNameFallsBackToFilenameWhenGenericOrMissing(string code) =>
        NamingHelper.ExtractCSharpClassName(code, "PAYB649.cbl").Should().Be("Payb649");

    [Fact]
    public void JavaClassNameIsReadFromPublicOrPackageLevelDeclaration()
    {
        NamingHelper.ExtractJavaClassName("package a;\npublic class OrderService {\n}", "X.cbl").Should().Be("OrderService");
        NamingHelper.ExtractJavaClassName("package a;\nclass OrderService {\n}", "X.cbl").Should().Be("OrderService");
        NamingHelper.ExtractJavaClassName("package a;\n", "ORD-MAIN.cbl").Should().Be("OrdMain");
    }

    [Fact]
    public void GenericClassNameReplacementUpdatesDeclarationAndConstructorCalls()
    {
        const string code = "class Gen { Gen() {} }\nvar g = new Gen();";

        NamingHelper.ReplaceGenericClassName(code, "Gen", "Payb649")
            .Should().Be("class Payb649 { Payb649() {} }\nvar g = new Payb649();");
        NamingHelper.ReplaceGenericClassName(code, "Gen", "Gen").Should().Be(code);
    }
}
