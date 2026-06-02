using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FactsFileAccess = CobolToQuarkusMigration.Agents.Infrastructure.Facts.FileAccess;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Facts;

public class ProgramFactsSchemaTests
{
    [Fact]
    public void RoundTrip_PreservesAllFields()
    {
        var facts = new ProgramFacts
        {
            Basename = "PROG.cbl",
            Stem = "PROG",
            RelativePath = "sources/src/PROG.cbl",
            SourceHash = "deadbeef",
            Confidence = FactConfidence.High,
            Warnings = new[] { "w1", "w2" },
            PreprocessNotes = new[]
            {
                new PreprocessNote("move-zero", 42, "MOVE 0(1)", "MOVE ZERO"),
            },
            Summary = new ProgramSummary
            {
                Loc = 791, Paragraphs = 42, Sections = 4, IsCopybook = false, ProgramId = "PROG",
            },
            Io = new IoFacts
            {
                Files = new[] { new FactsFileAccess("CUSTFILE", new[] { "OPEN", "READ" }) },
                DbTables = new[] { new DbTableAccess("ACCOUNTS", new[] { "SELECT", "UPDATE" }) },
            },
            Data = new DataFacts
            {
                Groups = new[] { new DataGroup("WS-CUSTOMER", 12, false) },
                CopybooksUsed = new[] { "BOOK1", "BOOK2" },
            },
            Callers = new[] { "PARENT.cbl" },
            Callees = new[] { "CHILD1.cbl", "CHILD2.cbl" },
            ControlFlow = new ControlFlowFacts
            {
                EntryPoints = new[] { "MAIN-SECTION" },
                PerformChains = new[] { new[] { "MAIN-PARA", "READ-CUST", "WRITE-LOG" } },
                Exits = new[] { "GOBACK" },
            },
            ExternalEffects = new[] { "FILE_IO", "DB_IO", "CALL_OUT" },
        };

        var json = JsonSerializer.Serialize(facts);
        var parsed = JsonSerializer.Deserialize<ProgramFacts>(json);

        parsed.Should().NotBeNull();
        parsed!.SchemaVersion.Should().Be(1);
        parsed.IdentitySchemeVersion.Should().Be("v1-basename");
        parsed.Basename.Should().Be("PROG.cbl");
        parsed.RelativePath.Should().Be("sources/src/PROG.cbl");
        parsed.SourceHash.Should().Be("deadbeef");
        parsed.Confidence.Should().Be(FactConfidence.High);
        parsed.Warnings.Should().BeEquivalentTo(new[] { "w1", "w2" });
        parsed.PreprocessNotes.Should().HaveCount(1);
        parsed.PreprocessNotes[0].Rule.Should().Be("move-zero");
        parsed.Summary.Loc.Should().Be(791);
        parsed.Summary.ProgramId.Should().Be("PROG");
        parsed.Io.Files.Should().ContainSingle().Which.Name.Should().Be("CUSTFILE");
        parsed.Io.DbTables.Should().ContainSingle().Which.Operations.Should().Contain("SELECT");
        parsed.Data.Groups.Should().HaveCount(1);
        parsed.Data.CopybooksUsed.Should().BeEquivalentTo(new[] { "BOOK1", "BOOK2" });
        parsed.Callees.Should().BeEquivalentTo(new[] { "CHILD1.cbl", "CHILD2.cbl" });
        parsed.Callers.Should().BeEquivalentTo(new[] { "PARENT.cbl" });
        parsed.ControlFlow.Exits.Should().Contain("GOBACK");
        parsed.ExternalEffects.Should().BeEquivalentTo(new[] { "FILE_IO", "DB_IO", "CALL_OUT" });
    }

    [Fact]
    public void Defaults_AreSafeAndDocumented()
    {
        var facts = new ProgramFacts
        {
            Basename = "A.cbl",
            Stem = "A",
            SourceHash = "h",
            Confidence = FactConfidence.None,
            Summary = new ProgramSummary(),
        };
        facts.SchemaVersion.Should().Be(ProgramFacts.CurrentSchemaVersion);
        facts.IdentitySchemeVersion.Should().Be("v1-basename");
        facts.RelativePath.Should().BeNull("forward-compat — populated when ProgramKey migration ships");
        facts.Warnings.Should().BeEmpty();
        facts.PreprocessNotes.Should().BeEmpty();
        facts.Io.Files.Should().BeEmpty();
        facts.Data.Groups.Should().BeEmpty();
        facts.Callees.Should().BeEmpty();
        facts.Callers.Should().BeEmpty();
    }

    [Fact]
    public void Json_UsesCamelCasePerSpec()
    {
        var facts = new ProgramFacts
        {
            Basename = "A.cbl",
            Stem = "A",
            SourceHash = "h",
            Confidence = FactConfidence.High,
            Summary = new ProgramSummary { Loc = 10 },
        };
        var json = JsonSerializer.Serialize(facts);
        json.Should().Contain("\"schemaVersion\":1");
        json.Should().Contain("\"identitySchemeVersion\":\"v1-basename\"");
        json.Should().Contain("\"basename\":\"A.cbl\"");
        json.Should().Contain("\"sourceHash\":\"h\"");
        json.Should().Contain("\"externalEffects\"");
        json.Should().NotContain("\"Basename\"", "fields must be camelCase per spec");
    }
}
