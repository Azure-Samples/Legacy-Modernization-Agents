using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Rekt;

// Programs COPY the symbolic map the BMS assembler would generate; a wrong layout silently corrupts screen fields.
public sealed class BmsSymbolicMapTests : IDisposable
{
    // One assembler card: columns 1-71, then a continuation mark in column 72.
    private static string Card(string text, bool continued = false) => text.PadRight(71) + (continued ? "*" : "");

    private static readonly string Mapset = string.Join("\n",
        "* comment line",
        Card("ORDSET   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,TIOAPFX=YES,", true),
        Card("               DSATTS=(COLOR,HILIGHT)"),
        Card("ORDMAP   DFHMDI SIZE=(24,80)"),
        Card("         DFHMDF POS=(1,1),LENGTH=7,INITIAL='ORDMAP '"),
        Card("TITLE    DFHMDF POS=(1,10),LENGTH=30,INITIAL='Order entry, page 1 - A l", true),
        Card("               ong title'"),
        Card("ORDNO    DFHMDF POS=(3,10),LENGTH=8,ATTRB=(NORM,NUM)"),
        Card("RATE     DFHMDF POS=(4,10),LENGTH=7,PICOUT='9999.99'"),
        Card("LINE     DFHMDF POS=(6,1),LENGTH=40,OCCURS=3"),
        Card("         DFHMSD TYPE=FINAL"),
        "         END");

    private readonly string _work = Path.Join(Path.GetTempPath(), $"bms-map-{Guid.NewGuid():N}");
    private readonly string _source;
    private readonly string _out;
    private readonly string _copybook = BmsSymbolicMap.RenderCopybook(BmsSymbolicMap.ParseBms(Mapset), "ORDSET.bms");

    public BmsSymbolicMapTests()
    {
        _source = Path.Join(_work, "source");
        _out = Path.Join(_source, ".preprocessed");
        Directory.CreateDirectory(Path.Join(_source, "bms"));
        File.WriteAllText(Path.Join(_source, "bms", "ordset.bms"), Mapset);
    }

    public void Dispose() => Directory.Delete(_work, recursive: true);

    [Fact]
    public void Continuation_JoinsQuotedInitialWithoutDroppingOperands()
    {
        var statements = BmsSymbolicMap.LogicalLines(Mapset).Select(line => BmsSymbolicMap.ParseStatement(line))
            .OfType<BmsSymbolicMap.Statement>().ToList();
        var title = statements.First(statement => statement.Label == "TITLE");
        title.Operands["INITIAL"].Should().Be("Order entry, page 1 - A long title");
        title.Operands["LENGTH"].Should().Be("30");
        statements.First(statement => statement.Macro == "DFHMSD").Operands["DSATTS"]
            .Should().BeEquivalentTo(new[] { "COLOR", "HILIGHT" }, options => options.WithStrictOrdering());
    }

    [Fact]
    public void UnnamedFields_AreNotInTheMap()
    {
        var mapset = BmsSymbolicMap.ParseBms(Mapset).Single();
        mapset.Maps[0].Fields.Select(field => field.Name).Should().Equal("TITLE", "ORDNO", "RATE", "LINE");
    }

    [Fact]
    public void ExtattYes_ImpliesDefaultAttributes()
    {
        var text = string.Join("\n",
            Card("S        DFHMSD TYPE=MAP,MODE=OUT,LANG=COBOL,EXTATT=YES"),
            Card("M        DFHMDI SIZE=(24,80)"),
            Card("F        DFHMDF POS=(1,1),LENGTH=2"));
        BmsSymbolicMap.ParseBms(text).Single().Maps[0].Attributes.Should().Equal("COLOR", "PS", "HILIGHT", "VALIDN");
    }

    [Fact]
    public void ScreenMetadata_KeepsUnnamedLabelsPositionsAndInputConstraints()
    {
        var screen = BmsSymbolicMap.ParseBms(Mapset)[0].Maps[0];
        screen.Size.Should().Equal(24, 80);
        screen.ScreenFields[0].Name.Should().BeEmpty();
        screen.ScreenFields[0].Initial.Should().Be("ORDMAP ");
        screen.ScreenFields[0].Position.Should().Equal(1, 1);
        var orderNumber = screen.ScreenFields.Single(field => field.Name == "ORDNO");
        orderNumber.Attributes.Should().Equal("NORM", "NUM");
        orderNumber.Length.Should().Be(8);
    }

    [Theory]
    [InlineData("LENGTH=8", "LENGTH=UNKNOWN")]
    [InlineData("LENGTH=8,", "")]
    [InlineData("POS=(3,10)", "POS=(0,10)")]
    public void InvalidOrMissingLengthsAndCoordinates_AreExplicitlyUnsupported(string from, string to)
    {
        var act = () => BmsSymbolicMap.ParseBms(Mapset.Replace(from, to));
        act.Should().Throw<PyError>().Which.Kind.Should().Be("ValueError");
    }

    [Fact]
    public void ZeroLengthUnnamedAttributeBoundary_IsRetainedWithoutFakeData()
    {
        var screen = BmsSymbolicMap.ParseBms(Mapset.Replace("LENGTH=7,INITIAL='ORDMAP '", "LENGTH=0"))[0].Maps[0];
        screen.ScreenFields[0].Length.Should().Be(0);
        screen.ScreenFields[0].Name.Should().BeEmpty();
        screen.Fields.Should().NotContain(screen.ScreenFields[0]);
    }

    [Fact]
    public void InputAndOutputStructures_ShareStorage()
    {
        _copybook.Should().Contain("       01  ORDMAPI.").And.Contain("       01  ORDMAPO REDEFINES ORDMAPI.");
    }

    [Fact]
    public void MapStorageOverlayAndAutoPrefix_FollowTheOriginalMapset()
    {
        var extra = string.Join("\n", Card("SECOND   DFHMDI SIZE=(24,80)"), Card("OTHER    DFHMDF POS=(1,1),LENGTH=4"));
        var final = Card("         DFHMSD TYPE=FINAL");
        var text = Mapset.Replace(final, extra + "\n" + final);
        BmsSymbolicMap.RenderCopybook(BmsSymbolicMap.ParseBms(text), "ORDSET.bms")
            .Should().Contain("01  SECONDI REDEFINES ORDMAPI.");

        var automatic = BmsSymbolicMap.RenderCopybook(BmsSymbolicMap.ParseBms(text.Replace("TIOAPFX=YES", "STORAGE=AUTO")), "ORDSET.bms");
        automatic.Should().Contain("01  SECONDI.").And.NotContain("01  SECONDI REDEFINES");
        Occurrences(automatic, "02  FILLER PIC X(12).").Should().Be(4);
    }

    [Fact]
    public void FieldSuffixes_FollowCicsLayout()
    {
        foreach (var line in new[] { "ORDNOL    COMP  PIC  S9(4).", "ORDNOF    PICTURE X.", "ORDNOA    PICTURE X.",
                     "ORDNOI  PIC X(8).", "ORDNOC    PICTURE X.", "ORDNOH    PICTURE X.", "ORDNOO  PIC X(8)." })
            _copybook.Should().Contain(line);
        _copybook.Should().NotContain("ORDNOP", "PS is not in DSATTS");
    }

    [Fact]
    public void InputAndOutputEntries_AreTheSameLength()
    {
        // L(2) + F(1) + attributes(2) + data == 3 + attributes(2) + data, so I and O line up.
        _copybook.Should().Contain("02  FILLER   PICTURE X(2).");
        Occurrences(_copybook, "02  FILLER PIC X(12).").Should().Be(2);
    }

    [Fact]
    public void PicoutAndOccurs()
    {
        foreach (var line in new[] { "RATEO  PIC 9999.99.", "RATEI  PIC X(7).", "02  DFHMS1 OCCURS 3 TIMES.",
                     "03  LINEI  PIC X(40).", "03  LINEO  PIC X(40)." })
            _copybook.Should().Contain(line);
    }

    [Fact]
    public void Copybook_FitsFixedFormatColumns()
    {
        _copybook.Split('\n').Should().OnlyContain(line => line.Length <= 72);
    }

    [Fact]
    public void Copybook_IsNamedAfterTheSourceFile()
    {
        var result = RunCli();
        File.Exists(Path.Join(_out, "ORDSET.cpy")).Should().BeTrue();
        result.StdOut.Should().Contain("ORDSET");
    }

    [Fact]
    public void Manifest_BindsScreenFieldsToTheActualBmsSource()
    {
        RunCli();
        var definition = Json(Path.Join(_out, "bms-screen-definitions.json"))["definitions"]![0]!;
        PyJson.Str(definition["path"]).Should().Be("bms/ordset.bms");
        PyJson.Str(definition["resourceName"]).Should().Be("ORDSET");
        PyJson.Str(definition["sha256"]).Should().Be(
            PyJson.Str(BmsSymbolicMap.ScreenDefinition(Path.Join(_source, "bms", "ordset.bms"), _source)["sha256"]));
        var screen = definition["mapsets"]![0]!["maps"]![0]!;
        PyJson.Dumps(screen["size"]).Should().Be("[24, 80]");
        PyJson.Str(screen["screen_fields"]![0]!["initial"]).Should().Be("ORDMAP ");
    }

    [Fact]
    public void SupportedLayout_RecordsExactOffsetsAndRejectsAChangedCopybook()
    {
        RunCli();
        var copybook = Path.Join(_out, "ORDSET.cpy");
        var proof = Json(copybook + ".bms-layout.json");
        PyJson.Str(proof["status"]).Should().Be("complete");
        proof["maps"]![0]!["byteLength"]!.GetValue<int>().Should().Be(207);
        var order = proof["maps"]![0]!["fields"]!.AsArray().Single(field => PyJson.Str(field!["name"]) == "ORDNO")!;
        (order["offset"]!.GetValue<int>(), order["dataOffset"]!.GetValue<int>(), order["stride"]!.GetValue<int>())
            .Should().Be((47, 52, 13));
        var bms = Path.Join(_source, "bms", "ordset.bms");
        PyJson.Dumps(BmsSymbolicMap.CompleteLayoutEvidence(bms, copybook)).Should().Be(PyJson.Dumps(proof));

        File.WriteAllText(copybook, File.ReadAllText(copybook).Replace("ORDNOI  PIC X(8)", "ORDNOI  PIC X(9)"));
        var act = () => BmsSymbolicMap.CompleteLayoutEvidence(bms, copybook);
        act.Should().Throw<PyError>().WithMessage("*differs*");
    }

    [Theory]
    [InlineData("LENGTH=8", "LENGTH=8,GRPNAME=GROUP")]
    [InlineData("PICOUT='9999.99'", "PICOUT='999.99'")]
    [InlineData("INITIAL='ORDMAP '", "INITIAL='ORDMAP ")]
    [InlineData(null, null)]
    public void UnsupportedOrMalformedLayouts_RemainExplicitlyPartial(string? from, string? to)
    {
        var changed = from is null
            ? Mapset[..Mapset.LastIndexOf('\n')] + "\n" + Card("         END", true)
            : Mapset.Replace(from, to);
        File.WriteAllText(Path.Join(_source, "bms", "ordset.bms"), changed);

        var result = RunCli();

        var proof = Json(Path.Join(_out, "ORDSET.cpy.bms-layout.json"));
        PyJson.Str(proof["status"]).Should().Be("partial");
        proof["diagnostics"]!.AsArray().Should().NotBeEmpty();
        result.StdErr.Should().Contain("remains partial");
    }

    [Fact]
    public void RealCopybookInSource_Wins()
    {
        Directory.CreateDirectory(Path.Join(_source, "cpy"));
        File.WriteAllText(Path.Join(_source, "cpy", "ORDSET.cpy"), "       01  REAL-MAP PIC X.\n");
        RunCli();
        File.Exists(Path.Join(_out, "ORDSET.cpy")).Should().BeFalse();
    }

    [Fact]
    public void ReplacesAStubButNotOtherContent()
    {
        Directory.CreateDirectory(_out);
        var copybook = Path.Join(_out, "ORDSET.cpy");
        File.WriteAllText(copybook, $"      *> {BmsSymbolicMap.StubMarker} for ORDSET\n");
        RunCli();
        File.ReadAllText(copybook).Should().Contain(BmsSymbolicMap.GeneratedMarker);

        File.WriteAllText(copybook, "       01  HAND-WRITTEN PIC X.\n");
        RunCli();
        File.ReadAllText(copybook).Should().Contain("HAND-WRITTEN");
    }

    [Fact]
    public void MissingArguments_FailAndPrintUsage()
    {
        var result = LmaCliProcess.Run(Path.Join(_work, "cwd"), null, "rekt-bms-map");
        result.ExitCode.Should().NotBe(0);
        (result.StdOut + result.StdErr).Should().Contain("rekt-bms-map <source-dir> <output-dir>");
    }

    [Fact]
    public void NonCobolMapsets_AreSkipped()
    {
        File.WriteAllText(Path.Join(_source, "bms", "ordset.bms"), Mapset.Replace("LANG=COBOL", "LANG=ASM  "));
        RunCli();
        File.Exists(Path.Join(_out, "ORDSET.cpy")).Should().BeFalse();
    }

    [Fact]
    public void UnprovenGeneratedCopy_DoesNotOverrideCurrentGroundedBms()
    {
        Directory.CreateDirectory(Path.Join(_source, "copy-generated"));
        File.WriteAllText(Path.Join(_source, "copy-generated", "ORDSET.cpy"), "       01 UNPROVEN-SUBSET PIC X.\n");
        RunCli();
        var copybook = Path.Join(_out, "ORDSET.cpy");
        File.ReadAllText(copybook).Should().Contain("ORDNOI");
        var record = Json(copybook + ".preprocess.json");
        PyJson.Str(record["producer"]).Should().Be("bms-symbolic-map");
        PyJson.Str(record["originalSourcePath"]).Should().Be(PyCompat.Resolve(Path.Join(_source, "bms", "ordset.bms")));
    }

    private (int ExitCode, string StdOut, string StdErr) RunCli()
    {
        var result = LmaCliProcess.Run(Path.Join(_work, "cwd"), null, "rekt-bms-map", _source, _out);
        result.ExitCode.Should().Be(0, result.StdErr);
        return result;
    }

    private static JsonNode Json(string path) => PyJson.Parse(File.ReadAllText(path))!;

    private static int Occurrences(string text, string value) => text.Split(value).Length - 1;
}
