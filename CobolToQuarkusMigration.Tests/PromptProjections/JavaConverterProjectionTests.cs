using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Helpers.PromptProjections;
using FactsFileAccess = CobolToQuarkusMigration.Agents.Infrastructure.Facts.FileAccess;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.PromptProjections;

public class JavaConverterProjectionTests : IDisposable
{
    private readonly string _root;
    private readonly string? _prevEnv;

    public JavaConverterProjectionTests()
    {
        _root = Path.Combine(Path.GetTempPath(), $"pr4a-{Guid.NewGuid():N}");
        Directory.CreateDirectory(_root);
        _prevEnv = Environment.GetEnvironmentVariable(JavaConverterProjection.EnableEnvVar);
    }

    public void Dispose()
    {
        Environment.SetEnvironmentVariable(JavaConverterProjection.EnableEnvVar, _prevEnv);
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }

    private static ProgramFacts MakeFacts(string basename = "PROG.cbl") => new()
    {
        Basename = basename,
        Stem = Path.GetFileNameWithoutExtension(basename),
        SourceHash = "abc",
        Confidence = FactConfidence.High,
        Warnings = new[] { "cics-detected-screens-not-extracted" },
        PreprocessNotes = new[] { new PreprocessNote("move-zero", 42, "MOVE 0(1)", "MOVE ZERO") },
        Summary = new ProgramSummary
        {
            Loc = 791, Sections = 4, Paragraphs = 42, IsCopybook = false, ProgramId = "PROG",
        },
        Io = new IoFacts
        {
            DbTables = new[] { new DbTableAccess("ACCOUNTS", new[] { "SELECT", "UPDATE" }) },
            Files = new[] { new FactsFileAccess("CUSTFILE", new[] { "OPEN", "READ", "CLOSE" }) },
        },
        Data = new DataFacts
        {
            Groups = new[]
            {
                new DataGroup("WS-CUSTOMER", 12, Redefines: false),
                new DataGroup("WS-VARIANT",  3, Redefines: true),
            },
            CopybooksUsed = new[] { "BOOK1", "BOOK2" },
        },
        Callees = new[] { "CHILD" },
        Callers = new[] { "PARENT.cbl" },
        ControlFlow = new ControlFlowFacts
        {
            EntryPoints = new[] { "MAIN-SECTION" },
            PerformChains = new[] { new[] { "MAIN-PARA", "READ-CUST", "WRITE-LOG" } },
            Exits = new[] { "GOBACK" },
        },
        ExternalEffects = new[] { "FILE_IO", "DB_IO", "CALL_OUT" },
    };

    [Fact]
    public void IsEnabled_DefaultsOff()
    {
        Environment.SetEnvironmentVariable(JavaConverterProjection.EnableEnvVar, null);
        JavaConverterProjection.IsEnabled().Should().BeFalse();
    }

    [Fact]
    public void IsEnabled_TrueOnlyForExactTrueString()
    {
        Environment.SetEnvironmentVariable(JavaConverterProjection.EnableEnvVar, "true");
        JavaConverterProjection.IsEnabled().Should().BeTrue();
        Environment.SetEnvironmentVariable(JavaConverterProjection.EnableEnvVar, "TRUE");
        JavaConverterProjection.IsEnabled().Should().BeTrue();
        Environment.SetEnvironmentVariable(JavaConverterProjection.EnableEnvVar, "1");
        JavaConverterProjection.IsEnabled().Should().BeFalse("env-var convention is explicit 'true' only");
        Environment.SetEnvironmentVariable(JavaConverterProjection.EnableEnvVar, "false");
        JavaConverterProjection.IsEnabled().Should().BeFalse();
    }

    [Fact]
    public void TryLoad_ReturnsNullWhenMissing()
    {
        JavaConverterProjection.TryLoad(_root, "NOPE.cbl").Should().BeNull();
    }

    [Fact]
    public void TryLoad_ReturnsNullOnCorruptJson()
    {
        File.WriteAllText(Path.Combine(_root, "X.facts.json"), "{ not valid json");
        JavaConverterProjection.TryLoad(_root, "X.cbl").Should().BeNull();
    }

    [Fact]
    public void TryLoad_RoundTripsValidFile()
    {
        var facts = MakeFacts();
        File.WriteAllText(Path.Combine(_root, "PROG.facts.json"), JsonSerializer.Serialize(facts));
        var loaded = JavaConverterProjection.TryLoad(_root, "PROG.cbl");
        loaded.Should().NotBeNull();
        loaded!.Basename.Should().Be("PROG.cbl");
        loaded.Confidence.Should().Be(FactConfidence.High);
    }

    [Fact]
    public void BuildPromptBlock_StartsWithCacheKeyMarker()
    {
        // ExtractRektContextBlock in JavaConverterAgent looks for this exact
        // marker — keep it stable so the response-cache key still extracts
        // and hashes the projection content.
        var block = JavaConverterProjection.BuildPromptBlock(MakeFacts());
        block.Should().Contain("REKT STRUCTURAL CONTEXT (authoritative",
            because: "the cache-key extractor pins this string");
    }

    [Fact]
    public void BuildPromptBlock_SurfacesAllStructuralFacts()
    {
        var block = JavaConverterProjection.BuildPromptBlock(MakeFacts());

        // Summary
        block.Should().Contain("programId   : PROG");
        block.Should().Contain("loc         : 791");

        // Data groups (with REDEFINES annotation)
        block.Should().Contain("WS-CUSTOMER — 12 field(s)");
        block.Should().Contain("WS-VARIANT").And.Contain("[REDEFINES");

        // Copybooks
        block.Should().Contain("BOOK1").And.Contain("BOOK2");

        // IO
        block.Should().Contain("ACCOUNTS : SELECT, UPDATE");
        block.Should().Contain("CUSTFILE : CLOSE, OPEN, READ");

        // Callees / callers
        block.Should().Contain("CALL TARGETS").And.Contain("CHILD");
        block.Should().Contain("CALLED BY").And.Contain("PARENT.cbl");

        // Control flow
        block.Should().Contain("entryPoints: MAIN-SECTION");
        block.Should().Contain("MAIN-PARA → READ-CUST → WRITE-LOG");
        block.Should().Contain("exits      : GOBACK");

        // External effects + warnings + preprocess notes
        block.Should().Contain("FILE_IO").And.Contain("DB_IO").And.Contain("CALL_OUT");
        block.Should().Contain("WARNINGS").And.Contain("cics-detected-screens-not-extracted");
        block.Should().Contain("PREPROCESSOR TRANSFORMS APPLIED");
        block.Should().Contain("move-zero @line 42: MOVE 0(1) → MOVE ZERO");
    }

    [Fact]
    public void BuildPromptBlock_PreservesFactLockingRules()
    {
        // The "(none)" markers + the rules are what stop the LLM from inventing
        // fields/calls. Pin them so a careless refactor cannot silently weaken
        // the constraint.
        var facts = MakeFacts() with
        {
            Callees = Array.Empty<string>(),
            Data = new DataFacts(),
            Io = new IoFacts(),
        };
        var block = JavaConverterProjection.BuildPromptBlock(facts);

        block.Should().Contain("FACT-LOCKING RULES");
        block.Should().Contain("Never invent new fields, methods, classes, SQL operations, or CALL targets");
        // Empty sections get explicit "(none)" so the LLM can see "do not generate".
        block.Should().Contain("DATA GROUPS").And.Contain("(none)");
        block.Should().Contain("CALL TARGETS").And.Contain("(none)");
        block.Should().Contain("COPYBOOKS USED").And.Contain("(none)");
        block.Should().Contain("DB TABLES").And.Contain("(none)");
        block.Should().Contain("FILES").And.Contain("(none)");
    }

    [Fact]
    public void BuildPromptBlock_DistinctOutputForDistinctFacts()
    {
        // Pin: when facts change, the block changes — so the cache key (which
        // hashes ExtractRektContextBlock output) invalidates.
        var f1 = MakeFacts();
        var f2 = f1 with { Callees = new[] { "OTHER" } };

        var b1 = JavaConverterProjection.BuildPromptBlock(f1);
        var b2 = JavaConverterProjection.BuildPromptBlock(f2);

        b1.Should().NotBe(b2, "different callees must produce a different block → different cache key");
        b1.Should().Contain("CHILD");
        b2.Should().Contain("OTHER");
        b2.Should().NotContain("CHILD");
    }

    [Fact]
    public void BuildPromptBlock_StableAcrossRunsForSameInput()
    {
        var facts = MakeFacts();
        var first = JavaConverterProjection.BuildPromptBlock(facts);
        var second = JavaConverterProjection.BuildPromptBlock(facts);
        second.Should().Be(first, "the block must be deterministic for cache-key stability");
    }

    [Fact]
    public void BuildPromptBlock_SchemaVersionVisibleToHumanReaders()
    {
        var block = JavaConverterProjection.BuildPromptBlock(MakeFacts());
        block.Should().Contain($"program-facts.json schema {ProgramFacts.CurrentSchemaVersion}");
        block.Should().Contain($"identity {ProgramFacts.CurrentIdentitySchemeVersion}");
    }
}
