using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Output file names come from the COBOL file name; two programs mapping to the same name
// would overwrite each other's converted output without any error.
public class NamingHelperTests
{
    [Theory]
    [InlineData("RGNB649.cbl", "Rgnb649")]
    [InlineData("synthetic_50k_loc_cobol.cbl", "Synthetic50kLocCobol")]
    [InlineData("pay-roll.calc.cbl", "PayRollCalc")]
    [InlineData("/data/src/ACCT01.cbl", "Acct01")]
    [InlineData("649ABC.cbl", "Cobol649abc")]
    [InlineData("", "ConvertedCobolProgram")]
    [InlineData("___.cbl", "ConvertedCobolProgram")]
    public void DeriveClassNameFromCobolFile_ProducesValidUniqueFriendlyName(string fileName, string expected)
    {
        var name = NamingHelper.DeriveClassNameFromCobolFile(fileName);

        name.Should().Be(expected);
        NamingHelper.IsValidIdentifier(name).Should().BeTrue();
    }

    [Fact]
    public void GetOutputFileName_AppendsExtensionToDerivedClassName()
    {
        NamingHelper.GetOutputFileName("CUST-UPD.cbl", ".java").Should().Be("CustUpd.java");
        NamingHelper.GetFallbackClassName("CUST-UPD.cbl").Should().Be("CustUpdFallback");
    }

    [Theory]
    [InlineData("Valid_name1", true)]
    [InlineData("_x", true)]
    [InlineData("1abc", false)]
    [InlineData("has-dash", false)]
    [InlineData("has space", false)]
    [InlineData("", false)]
    public void IsValidIdentifier_AcceptsOnlyLegalIdentifiers(string identifier, bool expected)
    {
        NamingHelper.IsValidIdentifier(identifier).Should().Be(expected);
    }

    [Theory]
    [InlineData("PaymentBatchValidator", true)]
    [InlineData("CustomerAccountUpdate", true)]
    [InlineData("Rgnb649", false)]
    [InlineData("ConvertedCobolProgram", false)]
    [InlineData("program", false)]
    [InlineData("", false)]
    public void IsSemanticClassName_RejectsGenericAndFilenameDerivedNames(string className, bool expected)
    {
        NamingHelper.IsSemanticClassName(className).Should().Be(expected);
    }

    [Fact]
    public void ExtractCSharpClassName_UsesDeclaredClassWhenNotGeneric()
    {
        var code = "namespace X;\npublic class PaymentProcessor : IDisposable\n{\n}";

        NamingHelper.ExtractCSharpClassName(code, "RGNB649.cbl").Should().Be("PaymentProcessor");
    }

    [Fact]
    public void ExtractCSharpClassName_FallsBackToFileNameForGenericOrMissingClass()
    {
        NamingHelper.ExtractCSharpClassName("public class ConvertedCobolProgram\n{\n}", "RGNB649.cbl")
            .Should().Be("Rgnb649");
        NamingHelper.ExtractCSharpClassName("// no declarations", "RGNB649.cbl")
            .Should().Be("Rgnb649");
    }

    [Fact]
    public void ExtractJavaClassName_HandlesPackagePrivateAndPublicClasses()
    {
        NamingHelper.ExtractJavaClassName("package a;\npublic class OrderService {\n}", "X.cbl")
            .Should().Be("OrderService");
        NamingHelper.ExtractJavaClassName("package a;\nclass OrderLoader{\n}", "X.cbl")
            .Should().Be("OrderLoader");
        NamingHelper.ExtractJavaClassName("package a;", "ORD-1.cbl")
            .Should().Be("Ord1");
    }

    [Fact]
    public void ReplaceGenericClassName_RewritesDeclarationConstructionAndCalls()
    {
        var code = "class Generic { Generic() {} }\nvar g = new Generic();\nGeneric(1);";

        var result = NamingHelper.ReplaceGenericClassName(code, "Generic", "Acct01");

        result.Should().Be("class Acct01 { Acct01() {} }\nvar g = new Acct01();\nAcct01(1);");
        NamingHelper.ReplaceGenericClassName(code, "Generic", "Generic").Should().Be(code);
    }
}
