using CobolToQuarkusMigration.Chunking;
using CobolToQuarkusMigration.Chunking.Interfaces;
using CobolToQuarkusMigration.Processes;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Processes;

public class ChunkedOutputAssemblerTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Assemble_OrdersRealContinuationFragmentsWithoutRewritingOrLosingAnyContent(bool java)
    {
        var first = java
            ? "package bank.accounts;\nimport java.util.List;\npublic final class Account<T> extends Base {\nprivate int balance;\nvoid run() {"
            : "using System;\nnamespace Bank.Accounts;\npublic sealed class Account<T> : Base {\nprivate int balance;\nvoid Run() {";
        var middle = "balance++;\n// } class Fake { }\nvar text = \"a } { // not a comment\";\n";
        var last = "balance += 2;\n}\nprivate class Nested { int value; }\n}\nclass Additional { int other; }";
        var input = Create(first, middle, last);
        input.Result.ChunkResults.Reverse();

        var file = ChunkedOutputAssembler.Assemble(input, java).Should().ContainSingle().Which;

        input.Success.Should().BeTrue();
        input.AssemblyErrors.Should().BeEmpty();
        file.Content.Should().Be(first + "\n" + middle + "\n" + last);
        file.FileName.Should().Be(java ? "Account.java" : "Account.cs");
        file.OriginalSourcePath.Should().Be("estate/bank/ACCOUNTS.cbl");
        file.OriginalCobolFileName.Should().Be("ACCOUNTS.cbl");
    }

    [Theory]
    [InlineData("namespace Bank;\npublic class Account {", "int Value(int n) { return n; }\nstring Value(string n) { return n; }", "}", false)]
    [InlineData("package bank;\npublic class Account {", "int value(int n) { return n; }\nString value(String n) { return n; }", "}", true)]
    [InlineData("namespace Bank { public class Account<T> {", "T Map<U>(System.Collections.Generic.Dictionary<string, U> values, T other) { return other; }", "} }", false)]
    [InlineData("package bank; public class Account<T> {", "<U> T map(java.util.Map<String, U> values, T other) { return other; }", "}", true)]
    [InlineData("public class Account {", "int Value(int[] n) { return n[0]; }\nint Value(int n) { return n; }", "}", false)]
    [InlineData("public class Account {", "string text = @\"} \"\" {\";\nchar c = '}';\n/* class Lost { } */ int value;", "}", false)]
    [InlineData("public class Account {", "String text = \"\"\"\n} { //\n\"\"\";\nchar c = '}';", "}", true)]
    public void Assemble_PreservesOverloadsGenericsNamespacesAndLexicalBraces(string first, string middle, string last, bool java)
    {
        var input = Create(first, middle, last);

        var file = ChunkedOutputAssembler.Assemble(input, java).Should().ContainSingle().Which;

        file.Content.Should().Be(first + "\n" + middle + "\n" + last);
        input.Success.Should().BeTrue();
    }

    [Theory]
    [InlineData("int Value(int n) { return 1; }\nint Value(int renamed) { return 2; }")]
    [InlineData("int Value(int n) { return 1; }\nint Value(int n) { return 1; }")]
    [InlineData("T Map<T>(T n) { return n; }\nU Map<U>(U renamed) { return renamed; }")]
    [InlineData("int balance = 1;\nint balance = 2;")]
    [InlineData("class Nested { int first; }\nclass Nested { int second; }")]
    [InlineData("int Value => 1;\nint Value => 2;")]
    [InlineData("int Value;\nint Value() { return 1; }")]
    [InlineData("int Value(int n) => Other(n);\nint Value(int renamed) => Different(renamed);")]
    public void Assemble_RejectsDifferingAndExactIdenticalMemberOverlapsWithoutSelectingAWinner(string body)
    {
        var input = Create("public class Account {", body, "}");
        input.Success = true;

        ChunkedOutputAssembler.Assemble(input, false).Should().BeEmpty();

        input.Success.Should().BeFalse();
        input.AssemblyErrors.Should().ContainSingle().Which.Should().ContainEquivalentOf("conflicting");
        ChunkedMigrationProcess.CreateFailedChunkDiagnosticFiles(input, false)
            .Select(d => d.Content).Should().Equal("public class Account {", body, "}");
    }

    [Theory]
    [InlineData("<T> T map(T n) { return n; }\n<U> U map(U renamed) { return renamed; }")]
    [InlineData("int map(java.util.List<String> n) { return 1; }\nint map(java.util.List<Integer> n) { return 2; }")]
    public void Assemble_RejectsJavaGenericAndErasedSignatureConflicts(string body)
    {
        var input = Create("public class Account {", body, "}");

        ChunkedOutputAssembler.Assemble(input, true).Should().BeEmpty();

        input.Success.Should().BeFalse();
        input.AssemblyErrors.Should().ContainSingle().Which.Should().Contain("conflicting");
    }

    [Fact]
    public void Assemble_PreservesLegitimateDifferentArityTypesAndNestedTypesInOneCSharpUnit()
    {
        const string code = "public class Account<T> { class Nested {} }\npublic class Account<T,U> { class Nested {} }";
        var input = Create(code);

        ChunkedOutputAssembler.Assemble(input, false).Should().ContainSingle().Which.Content.Should().Be(code);

        input.Success.Should().BeTrue();
    }

    [Fact]
    public void Assemble_PreservesSupportedNestedTypeKindsAndAdditionalTopLevelTypes()
    {
        const string code = """
            namespace Bank;
            public class Account<T> {
                public struct Amount { public int Value; }
                public record Row(int Id) { }
                public enum State { Ready, Done }
                public interface IRunner { void Run(); }
            }
            internal class Companion { string value = "class Phantom { }"; }
            """;
        var input = Create(code);

        ChunkedOutputAssembler.Assemble(input, false).Should().ContainSingle().Which.Content.Should().Be(code);

        input.Success.Should().BeTrue();
    }

    [Theory]
    [InlineData("public class Account { int first; }", "public class Account { int second; }", false)]
    [InlineData("public class Account { int first; }", "public class Different { int second; }", false)]
    [InlineData("package first; public class Account {", "package second; int second; }", true)]
    [InlineData("namespace First; public class Account {", "namespace Second; int second; }", false)]
    [InlineData("public class Account {", "using System; }", false)]
    [InlineData("public class Account {", "import java.util.List; }", true)]
    [InlineData("public class Account {}", "}", false)]
    public void Assemble_RejectsIncompatibleUnitsAndHeaders(string first, string last, bool java)
    {
        var input = Create(first, last);
        ChunkedOutputAssembler.Assemble(input, java).Should().BeEmpty();

        input.Success.Should().BeFalse();
        input.AssemblyErrors.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("")]
    [InlineData("// no code")]
    [InlineData("int member;")]
    [InlineData("public class Account {")]
    [InlineData("public class Account {} trailing prose")]
    [InlineData("public class Account { string text = \"unterminated; }")]
    [InlineData("public class Account { /* unterminated }")]
    [InlineData("public class Account { string text = $\"{value}\"; }")]
    [InlineData("#if DEBUG\npublic class Account {}\n#endif")]
    [InlineData("public class Account { int x, y; }")]
    [InlineData("public class Account { int x }")]
    [InlineData("public class Account { arbitrary prose }")]
    [InlineData("public class Account { int Value(int n] { return n; } }")]
    public void Assemble_RejectsEmptyNoTypeMalformedAndUnknownOutput(string code)
    {
        var input = Create(code);

        ChunkedOutputAssembler.Assemble(input, false).Should().BeEmpty();

        input.Success.Should().BeFalse();
        input.AssemblyErrors.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("missing")]
    [InlineData("duplicate")]
    [InlineData("extra")]
    [InlineData("failed")]
    [InlineData("conversion-failed")]
    [InlineData("empty")]
    [InlineData("comments")]
    [InlineData("total")]
    [InlineData("inconsistent")]
    [InlineData("response-index")]
    [InlineData("plan-indices")]
    [InlineData("source")]
    public void Assemble_RejectsCoverageHolesEvenWhenRemainingChunksWouldCompile(string failure)
    {
        var input = Create("public class Account {", "int value;", "}");
        switch (failure)
        {
            case "missing": input.Result.ChunkResults.RemoveAt(1); break;
            case "duplicate": input.Result.ChunkResults.Add(input.Result.ChunkResults[1]); break;
            case "extra": input.Result.ChunkResults[1].ChunkIndex = 9; break;
            case "failed": input.Result.ChunkResults[1].Success = false; break;
            case "conversion-failed": input.Result.ChunkResults[1].ConversionResult.Success = false; break;
            case "empty": input.Result.ChunkResults[1].ConversionResult.ConvertedCode = ""; break;
            case "comments": input.Result.ChunkResults[1].ConversionResult.ConvertedCode = "// omitted"; break;
            case "total": input.Result.TotalChunks = 4; break;
            case "inconsistent": input.Result.IsConsistent = false; break;
            case "response-index": input.Result.ChunkResults[1].ConversionResult.ChunkIndex = 7; break;
            case "plan-indices": input.Plan.Chunks = [new ChunkResult { ChunkIndex = 8 }]; break;
            case "source": input.SourceFile = ""; break;
        }

        ChunkedOutputAssembler.Assemble(input, false).Should().BeEmpty();

        input.Success.Should().BeFalse();
        var completion = () => ChunkedMigrationProcess.EnsureAssemblyComplete([input]);
        completion.Should().Throw<InvalidDataException>().WithMessage("*incomplete*");
    }

    [Fact]
    public async Task Diagnostics_RetainEveryRejectedArrivalAndErrorsOutsideCompilableSource()
    {
        var directory = Path.GetFullPath(Path.Combine(".test-artifacts", "assembly-" + Guid.NewGuid()));
        try
        {
            var input = Create("public class Account {", "int first;", "}");
            input.Result.ChunkResults[1].Success = false;
            input.Result.ChunkResults[1].ConversionResult.ErrorMessage = "Provider refused, without sentinel";
            input.Result.ChunkResults.Add(new ChunkProcessingResult
            {
                ChunkIndex = 1,
                Success = false,
                ConversionResult = new ChunkConversionResult { ConvertedCode = "int second;", ErrorMessage = "Retry also failed" }
            });
            ChunkedOutputAssembler.Assemble(input, false).Should().BeEmpty();

            await ChunkedMigrationProcess.SaveAssemblyDiagnosticsAsync([input], directory);

            Directory.GetFiles(directory, "*.cs", SearchOption.AllDirectories).Should().BeEmpty();
            Directory.GetFiles(directory, "*.java", SearchOption.AllDirectories).Should().BeEmpty();
            var chunks = Directory.GetFiles(directory, "*.rejected.txt", SearchOption.AllDirectories);
            chunks.Should().HaveCount(4);
            (await Task.WhenAll(chunks.Select(path => File.ReadAllTextAsync(path))))
                .Should().BeEquivalentTo("public class Account {", "int first;", "}", "int second;");
            var errors = await File.ReadAllTextAsync(Directory.GetFiles(directory, "assembly-errors.txt", SearchOption.AllDirectories).Single());
            errors.Should().Contain("estate/bank/ACCOUNTS.cbl").And.Contain("Expected chunks: 3")
                .And.Contain("Received indices: 0, 1, 2, 1").And.Contain("Provider refused, without sentinel");
        }
        finally
        {
            if (Directory.Exists(directory)) Directory.Delete(directory, true);
        }
    }

    internal static ChunkedFileResult Create(params string[] code) => new()
    {
        SourceFile = "estate/bank/ACCOUNTS.cbl",
        Plan = new ChunkingPlan { ChunkCount = code.Length },
        Result = new ChunkingResult
        {
            TotalChunks = code.Length,
            SuccessfulChunks = code.Length,
            IsConsistent = true,
            ChunkResults = code.Select((text, index) => new ChunkProcessingResult
            {
                ChunkIndex = index,
                Success = true,
                ConversionResult = new ChunkConversionResult
                {
                    ChunkIndex = index,
                    Success = true,
                    ConvertedCode = text
                }
            }).ToList()
        }
    };
}
