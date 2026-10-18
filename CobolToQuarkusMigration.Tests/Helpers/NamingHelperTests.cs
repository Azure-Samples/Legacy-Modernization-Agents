using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public class NamingHelperTests
{
    [Theory]
    [InlineData("PAYB649.cbl", "Payb649")]
    [InlineData("synthetic_50k_loc_cobol.cbl", "Synthetic50kLocCobol")]
    [InlineData("source/sub/CUST-ACCT.cob", "CustAcct")]
    [InlineData("649PAY.cbl", "Cobol649pay")]
    [InlineData(".cbl", "ConvertedCobolProgram")]
    [InlineData("___.cbl", "ConvertedCobolProgram")]
    public void DeriveClassNameFromCobolFile_ProducesValidPascalCaseName(string fileName, string expected)
    {
        var name = NamingHelper.DeriveClassNameFromCobolFile(fileName);

        name.Should().Be(expected);
        NamingHelper.IsValidIdentifier(name).Should().BeTrue();
    }

    [Fact]
    public void GetOutputFileName_UsesDerivedClassNameWithExtension()
    {
        NamingHelper.GetOutputFileName("PAY-RUN.cbl", ".java").Should().Be("PayRun.java");
        NamingHelper.GetFallbackClassName("PAY-RUN.cbl").Should().Be("PayRunFallback");
    }

    [Theory]
    [InlineData("", false)]
    [InlineData("1abc", false)]
    [InlineData("a-b", false)]
    [InlineData("_ok1", true)]
    [InlineData("Valid", true)]
    public void IsValidIdentifier_AcceptsOnlyLegalIdentifiers(string identifier, bool expected)
    {
        NamingHelper.IsValidIdentifier(identifier).Should().Be(expected);
    }

    [Theory]
    [InlineData("PaymentBatchValidator", true)]
    [InlineData("CustomerAccount", true)]
    [InlineData("Payb649", false)]
    [InlineData("ConvertedCobolProgram", false)]
    [InlineData("program", false)]
    [InlineData("", false)]
    public void IsSemanticClassName_SeparatesDomainNamesFromGenericOnes(string name, bool expected)
    {
        NamingHelper.IsSemanticClassName(name).Should().Be(expected);
    }

    [Fact]
    public void ExtractCSharpClassName_UsesDeclaredClass()
    {
        var code = "namespace X;\npublic sealed class LedgerPoster : Base\n{\n}";

        NamingHelper.ExtractCSharpClassName(code, "PAYB649.cbl").Should().Be("LedgerPoster");
    }

    [Theory]
    [InlineData("namespace X;\npublic class ConvertedCobolProgram { }")]
    [InlineData("// no type declared here")]
    public void ExtractCSharpClassName_GenericOrMissingClass_FallsBackToFileName(string code)
    {
        NamingHelper.ExtractCSharpClassName(code, "PAYB649.cbl").Should().Be("Payb649");
    }

    [Fact]
    public void ExtractJavaClassName_HandlesPublicAndPackagePrivateClasses()
    {
        NamingHelper.ExtractJavaClassName("package a;\npublic class Ledger {\n}", "X.cbl").Should().Be("Ledger");
        NamingHelper.ExtractJavaClassName("package a;\nclass Helper {\n}", "X.cbl").Should().Be("Helper");
        NamingHelper.ExtractJavaClassName("package a;", "PAY-RUN.cbl").Should().Be("PayRun");
    }

    [Fact]
    public void ReplaceGenericClassName_RewritesDeclarationConstructionAndCalls()
    {
        var code = "public class Generic { Generic() {} }\nvar x = new Generic();";

        var result = NamingHelper.ReplaceGenericClassName(code, "Generic", "Payb649");

        result.Should().Be("public class Payb649 { Payb649() {} }\nvar x = new Payb649();");
    }

    [Fact]
    public void ReplaceGenericClassName_SameName_ReturnsInputUnchanged()
    {
        NamingHelper.ReplaceGenericClassName("class A {}", "A", "A").Should().Be("class A {}");
    }
}
