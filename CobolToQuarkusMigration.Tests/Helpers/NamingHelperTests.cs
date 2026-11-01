using CobolToQuarkusMigration.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

// Output file names derive from the class name; a collision or invalid identifier overwrites or breaks generated code.
public class NamingHelperTests
{
    [Theory]
    [InlineData("PAYB649.cbl", "Payb649")]
    [InlineData("synthetic_50k_loc_cobol.cbl", "Synthetic50kLocCobol")]
    [InlineData("src/legacy/my-prog.cob", "MyProg")]
    [InlineData("649PAY.cbl", "Cobol649pay")]
    public void DeriveClassNameFromCobolFile_ProducesValidPascalCaseName(string file, string expected)
    {
        var name = NamingHelper.DeriveClassNameFromCobolFile(file);

        name.Should().Be(expected);
        NamingHelper.IsValidIdentifier(name).Should().BeTrue();
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    [InlineData(".cbl")]
    public void DeriveClassNameFromCobolFile_FallsBackWhenNothingUsableRemains(string file)
    {
        NamingHelper.DeriveClassNameFromCobolFile(file).Should().Be("ConvertedCobolProgram");
    }

    [Fact]
    public void GetOutputFileName_AppendsExtensionToDerivedName()
    {
        NamingHelper.GetOutputFileName("PAYB649.cbl", ".java").Should().Be("Payb649.java");
        NamingHelper.GetFallbackClassName("PAYB649.cbl").Should().Be("Payb649Fallback");
    }

    [Fact]
    public void ExtractCSharpClassName_PrefersGeneratedClassOverFilenameDerivedName()
    {
        var code = "namespace X;\npublic class PaymentBatchValidator : Base\n{\n}";

        NamingHelper.ExtractCSharpClassName(code, "PAYB649.cbl").Should().Be("PaymentBatchValidator");
    }

    [Theory]
    [InlineData("public class ConvertedCobolProgram {")]
    [InlineData("// no type declared here")]
    public void ExtractCSharpClassName_FallsBackToFilenameForGenericOrMissingClass(string code)
    {
        NamingHelper.ExtractCSharpClassName(code, "PAYB649.cbl").Should().Be("Payb649");
    }

    [Fact]
    public void ExtractJavaClassName_FindsClassWithoutPublicModifier()
    {
        var code = "package a;\nclass OrderSync {\n}";

        NamingHelper.ExtractJavaClassName(code, "ORD1.cbl").Should().Be("OrderSync");
    }

    [Theory]
    [InlineData("PaymentBatchValidator", true)]
    [InlineData("Payb649", false)]
    [InlineData("Program", false)]
    [InlineData("", false)]
    public void IsSemanticClassName_SeparatesDomainNamesFromGenericOrFilenameNames(string name, bool expected)
    {
        NamingHelper.IsSemanticClassName(name).Should().Be(expected);
    }
}
