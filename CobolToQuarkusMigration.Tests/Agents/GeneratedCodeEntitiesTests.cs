using CobolToQuarkusMigration.Agents;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

public class GeneratedCodeEntitiesTests
{
    [Fact]
    public void EntitiesInCodeAreDecoded()
    {
        // Rgni656.cs:78 as generated.
        const string code = "public bool NettingSortUgyldig => !NettingSortJa &amp;&amp; !NettingSortNej;";

        GeneratedCodeEntities.DecodeInCode(code)
            .Should().Be("public bool NettingSortUgyldig => !NettingSortJa && !NettingSortNej;");
    }

    [Fact]
    public void GenericsAndComparisonsAreDecoded()
    {
        GeneratedCodeEntities.DecodeInCode("List&lt;int&gt; xs; if (a &lt;= b) { }")
            .Should().Be("List<int> xs; if (a <= b) { }");
    }

    [Theory]
    [InlineData("/// <summary>YY &lt; 50 =&gt; 20YY</summary>")]
    [InlineData("// job &amp; step")]
    [InlineData("/* a &lt; b */")]
    [InlineData("var s = \"R&amp;D\";")]
    [InlineData("var s = @\"C:\\x \"\"&lt;\"\"\";")]
    [InlineData("var c = '&';")]
    [InlineData("var t = \"\"\"\n&amp;\n\"\"\";")]
    public void CommentsAndLiteralsAreLeftAlone(string code)
    {
        GeneratedCodeEntities.DecodeInCode(code).Should().BeSameAs(code);
    }

    [Fact]
    public void CodeAfterACommentOrLiteralIsStillDecoded()
    {
        const string code = "/// a &lt; b\nvar s = \"&amp;\"; var ok = x &amp;&amp; y;";

        GeneratedCodeEntities.DecodeInCode(code)
            .Should().Be("/// a &lt; b\nvar s = \"&amp;\"; var ok = x && y;");
    }

    [Fact]
    public void AnUnterminatedStringDoesNotHideTheNextLine()
    {
        GeneratedCodeEntities.DecodeInCode("var s = \"open\nvar ok = x &amp;&amp; y;")
            .Should().EndWith("x && y;");
    }

    [Fact]
    public void QuoteEntitiesAreNotDecoded()
    {
        const string code = "var q = &quot;x&quot;;";

        GeneratedCodeEntities.DecodeInCode(code).Should().BeSameAs(code);
    }
}
