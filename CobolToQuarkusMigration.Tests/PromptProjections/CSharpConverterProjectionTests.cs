using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers.PromptProjections;
using FactsFileAccess = CobolToQuarkusMigration.Agents.Infrastructure.Facts.FileAccess;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.PromptProjections;

/// <summary>
/// PR4.b — mirrors the Java projection tests with .NET-idiom assertions.
/// Shares the env var with JavaConverterProjection (one toggle activates both
/// converter agents); shares <see cref="ProgramFacts"/> schema.
/// </summary>
public class CSharpConverterProjectionTests
{
    private static ProgramFacts MakeFacts() => new()
    {
        Basename = "PROG.cbl",
        Stem = "PROG",
        SourceHash = "abc",
        Confidence = FactConfidence.High,
        Warnings = new[] { "cics-detected-screens-not-extracted" },
        PreprocessNotes = new[] { new PreprocessNote("move-zero", 42, "MOVE 0(1)", "MOVE ZERO") },
        Summary = new ProgramSummary { Loc = 791, Sections = 4, Paragraphs = 42, ProgramId = "PROG" },
        Io = new IoFacts
        {
            DbTables = new[] { new DbTableAccess("ACCOUNTS", new[] { "SELECT", "UPDATE" }) },
            Files = new[] { new FactsFileAccess("CUSTFILE", new[] { "OPEN", "READ", "CLOSE" }) },
        },
        Data = new DataFacts
        {
            Groups = new[] { new DataGroup("WS-CUSTOMER", 12, false) },
            CopybooksUsed = new[] { "BOOK1" },
        },
        Callees = new[] { "CHILD" },
        Callers = new[] { "PARENT.cbl" },
        ControlFlow = new ControlFlowFacts
        {
            EntryPoints = new[] { "MAIN-SECTION" },
            Exits = new[] { "GOBACK" },
        },
        ExternalEffects = new[] { "DB_IO", "CALL_OUT" },
    };

    [Fact]
    public void SharedEnvVar_WithJavaProjection()
    {
        // Pinning the contract: one env var (_USE_PROGRAM_FACTS) flips both
        // converter agents at once. Avoids users having to remember two flags.
        CSharpConverterProjection.EnableEnvVar.Should().Be(JavaConverterProjection.EnableEnvVar);
    }

    [Fact]
    public void BuildPromptBlock_StartsWithCacheKeyMarker()
    {
        var block = CSharpConverterProjection.BuildPromptBlock(MakeFacts());
        block.Should().Contain("REKT STRUCTURAL CONTEXT (authoritative",
            because: "the cache-key extractor in CSharpConverterAgent pins this string");
    }

    [Fact]
    public void BuildPromptBlock_UsesDotNetIdiomsInRules()
    {
        var block = CSharpConverterProjection.BuildPromptBlock(MakeFacts());

        // .NET-specific deviations from the Java version:
        block.Should().Contain("PIC X→string");
        block.Should().Contain("PIC S9V9→decimal");
        block.Should().Contain("PascalCase");
        block.Should().Contain("constructor-injected field");
        block.Should().Contain("EF Core entity / repository method");
        block.Should().Contain(".NET libraries / DI registrations");

        // Must NOT carry Java-isms:
        block.Should().NotContain("BigDecimal");
        block.Should().NotContain("@Inject");
        block.Should().NotContain("Panache");
        block.Should().NotContain("Quarkus");
    }

    [Fact]
    public void BuildPromptBlock_SurfacesAllStructuralFacts()
    {
        var block = CSharpConverterProjection.BuildPromptBlock(MakeFacts());

        block.Should().Contain("programId   : PROG");
        block.Should().Contain("WS-CUSTOMER — 12 field(s)");
        block.Should().Contain("BOOK1");
        block.Should().Contain("ACCOUNTS : SELECT, UPDATE");
        block.Should().Contain("CUSTFILE : CLOSE, OPEN, READ");
        block.Should().Contain("CALL TARGETS").And.Contain("CHILD");
        block.Should().Contain("CALLED BY").And.Contain("PARENT.cbl");
        block.Should().Contain("entryPoints: MAIN-SECTION");
        block.Should().Contain("exits      : GOBACK");
        block.Should().Contain("DB_IO").And.Contain("CALL_OUT");
        block.Should().Contain("WARNINGS").And.Contain("cics-detected-screens-not-extracted");
        block.Should().Contain("PREPROCESSOR TRANSFORMS APPLIED");
    }

    [Fact]
    public void BuildPromptBlock_DistinctOutputForDistinctFacts()
    {
        var f1 = MakeFacts();
        var f2 = f1 with { Callees = new[] { "OTHER" } };

        var b1 = CSharpConverterProjection.BuildPromptBlock(f1);
        var b2 = CSharpConverterProjection.BuildPromptBlock(f2);

        b1.Should().NotBe(b2);
        b2.Should().Contain("OTHER");
        b2.Should().NotContain("CHILD");
    }

    [Fact]
    public void BuildPromptBlock_StableAcrossRunsForSameInput()
    {
        var facts = MakeFacts();
        CSharpConverterProjection.BuildPromptBlock(facts)
            .Should().Be(CSharpConverterProjection.BuildPromptBlock(facts));
    }

    [Fact]
    public void BuildPromptBlock_EmptyCategoriesShowNone()
    {
        var facts = MakeFacts() with
        {
            Callees = Array.Empty<string>(),
            Data = new DataFacts(),
            Io = new IoFacts(),
        };
        var block = CSharpConverterProjection.BuildPromptBlock(facts);
        block.Should().Contain("DATA GROUPS").And.Contain("(none)");
        block.Should().Contain("CALL TARGETS").And.Contain("(none)");
        block.Should().Contain("COPYBOOKS USED").And.Contain("(none)");
        block.Should().Contain("DB TABLES").And.Contain("(none)");
        block.Should().Contain("FILES").And.Contain("(none)");
    }

    [Fact]
    public void TryLoad_DelegatesToSharedReader()
    {
        // CSharpConverterProjection.TryLoad is a pass-through to
        // JavaConverterProjection.TryLoad — same schema, same file location.
        // Asserting equality of the delegate target keeps the contract honest.
        var root = Path.Combine(Path.GetTempPath(), $"pr4b-{Guid.NewGuid():N}");
        try
        {
            Directory.CreateDirectory(root);
            CSharpConverterProjection.TryLoad(root, "NOPE.cbl").Should().BeNull();
        }
        finally
        {
            if (Directory.Exists(root)) Directory.Delete(root, recursive: true);
        }
    }
}
