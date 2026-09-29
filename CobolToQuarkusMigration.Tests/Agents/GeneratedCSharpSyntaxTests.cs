using CobolToQuarkusMigration.Agents;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

public class GeneratedCSharpSyntaxTests
{
    [Fact]
    public void AnAccessorEndedWithACommaEndsWithASemicolon()
    {
        const string code = "    public string V\n    {\n        get => A + B + C,\n    }\n    public int W\n    {\n        get => FromCode(_w),\n        private set => _w = value,\n    }\n";

        GeneratedCSharpSyntax.FixAccessorTerminators(code).Should().Be(
            "    public string V\n    {\n        get => A + B + C;\n    }\n    public int W\n    {\n        get => FromCode(_w);\n        private set => _w = value;\n    }\n");
    }

    [Fact]
    public void CommasThatBelongToTheCodeAreKept()
    {
        const string code = "        get => Combine(a,\n            b);\n    var x = new { A = 1,\n        B = 2 };\n    var t = new[]\n    {\n        1,\n    };\n";

        GeneratedCSharpSyntax.FixAccessorTerminators(code).Should().Be(code);
    }
}
