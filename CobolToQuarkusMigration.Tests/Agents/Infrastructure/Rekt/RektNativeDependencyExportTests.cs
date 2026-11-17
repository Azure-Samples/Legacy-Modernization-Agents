using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Rekt;

// Runs the real native parser against tiny programs and judges its export with the evidence rules.
// Set REKT_TEST_IMAGE to a cobol-rekt image to enable; REKT_PLATFORM_GATE_ASSEMBLY adds the dependency-plan gate.
[Collection(RektEvidenceCollection.Name)]
public sealed partial class RektNativeDependencyExportTests : RektEvidenceFixture
{
    private const string Pilot = "PILOT.cbl";
    private static string? Image => Environment.GetEnvironmentVariable("REKT_TEST_IMAGE") is { Length: > 0 } image ? image : null;
    private static string? GateAssembly => Environment.GetEnvironmentVariable("REKT_PLATFORM_GATE_ASSEMBLY") is { Length: > 0 } gate ? gate : null;

    public RektNativeDependencyExportTests()
    {
        Directory.Delete(Path.Join(Source, "estate"), recursive: true);
        Directory.Delete(Path.Join(Output, "estate"), recursive: true);
        Directory.CreateDirectory(Path.Join(Output, "java-work"));
    }

    private string ExportFile => Path.Join(Output, Pilot + "-deps.json");
    private string Report => Path.Join(Output, Pilot + ".report", "dependency-evidence.json");

    private sealed record Parsed(int Code, string Log, JsonObject Exported)
    {
        public List<string> Diagnostics => Strings(Exported["dependencyDiagnostics"]);
        public JsonArray Statements => Exported["dependencyDialectStatements"]!.AsArray();
        public string Why => Exported["dependencyDiagnostics"] is JsonArray { Count: > 0 } issues ? issues.ToJsonString() : Log;
    }

    private Parsed Parse(string body, bool missing = false, bool fileIo = false, bool callee = true, string? calleeDialect = null,
        bool emptyFileSection = false, string? cicsResources = null, string declarations = "", JsonNode? profile = null,
        string? profileText = null, bool profileOutsideSource = false, string? sqlDeclarations = null,
        string cicsResourcesFile = "resources.yaml", IReadOnlyList<string>? vsamDefinitions = null,
        IReadOnlyDictionary<string, string>? environment = null)
    {
        Save(Path.Join(Source, Pilot),
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PILOT.\n"
            + (fileIo ? "       ENVIRONMENT DIVISION.\n       INPUT-OUTPUT SECTION.\n       FILE-CONTROL.\n"
                        + "           SELECT INPUT-FILE ASSIGN TO 'fixture.dat'.\n" : "")
            + "       DATA DIVISION.\n"
            + (fileIo ? "       FILE SECTION.\n       FD INPUT-FILE.\n       01 INPUT-RECORD PIC X(10).\n"
                : emptyFileSection ? "       FILE SECTION.\n" : "")
            + "       WORKING-STORAGE SECTION.\n"
            + (missing || body.Contains("PAY-AMOUNT") ? "       COPY PAYREC.\n" : "")
            + "       01 WS-TARGET PIC X(8) VALUE 'PAYHELP'.\n"
            + declarations + "       PROCEDURE DIVISION.\n" + body + "           GOBACK.\n");
        if (!missing) Save(Path.Join(Source, "PAYREC.cpy"), "       01 PAY-AMOUNT PIC 9(4).\n");
        if (callee)
            Save(Path.Join(Source, "PAYHELP.cbl"),
                "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYHELP.\n"
                + "       DATA DIVISION.\n       LINKAGE SECTION.\n       01 AMOUNT PIC 9(4).\n"
                + "       PROCEDURE DIVISION USING AMOUNT.\n"
                + calleeDialect switch
                {
                    "options" => "           EXEC CICS RETURN TRANSID('NEXT') END-EXEC.\n",
                    null => "",
                    _ => "           EXEC CICS RETURN END-EXEC.\n",
                }
                + "           GOBACK.\n");
        var cidFile = Path.Join(Output, "container-id");
        var options = new List<string>();
        if (cicsResources is not null)
        {
            Save(Path.Join(Source, cicsResourcesFile), cicsResources);
            options.AddRange(["-e", $"REKT_CICS_RESOURCE_DEFINITIONS=/source/{cicsResourcesFile}"]);
        }
        if (sqlDeclarations is not null) options.AddRange(["-e", $"REKT_SQL_DECLARATIONS={sqlDeclarations}"]);
        if (vsamDefinitions is not null)
        {
            var names = new List<string>();
            for (var index = 0; index < vsamDefinitions.Count; index++)
            {
                Save(Path.Join(Source, $"VSAM{index}.jcl"), vsamDefinitions[index]);
                names.Add($"/source/VSAM{index}.jcl");
            }
            options.AddRange(["-e", "REKT_VSAM_DEFINITIONS=" + string.Join(':', names)]);
        }
        foreach (var (key, value) in environment ?? new Dictionary<string, string>()) options.AddRange(["-e", $"{key}={value}"]);
        if (profile is not null || profileText is not null)
        {
            var profilePath = PyCompat.Resolve(Path.Join(profileOutsideSource ? Work : Source, "profile.json"));
            Save(profilePath, profileText ?? PyJson.Dumps(profile));
            var parserProfile = profileOutsideSource ? profilePath : "/source/profile.json";
            options.AddRange(["-e", $"REKT_DEPENDENCY_PROFILE={parserProfile}"]);
            if (profileOutsideSource) options.AddRange(["-v", $"{profilePath}:{parserProfile}:ro"]);
        }
        List<string> command =
        [
            "run", "--rm", .. options, "--memory=1g", "--entrypoint=java", "--workdir=/output", "--cidfile", cidFile,
            "-e", "JAVA_TOOL_OPTIONS=-Xmx512m -XX:-UsePerfData -Djava.io.tmpdir=/output/java-work",
            "-v", $"{Source}:/source:ro", "-v", $"{Output}:/output",
            Image!, "-jar", "/app/smojol-cli.jar", "dependency", Pilot,
            "--srcDir=/source", "--copyBooksDir=/source", "--dialectJarPath=/app/dialect-idms.jar",
            $"--export=/output/{Pilot}-deps.json",
        ];
        (int Code, string Log) result;
        try { result = Run("docker", command, 120); }
        finally
        {
            if (File.Exists(cidFile))
            {
                Run("docker", ["rm", "-f", File.ReadAllText(cidFile).Trim()], 20);
                File.Delete(cidFile);
            }
        }
        return new Parsed(result.Code, result.Log, J(Text(ExportFile)).AsObject());
    }

    private static JsonObject TargetProfile() =>
        PyJson.ReadFile(Path.Join(RepoRoot(), "tools/cobol-rekt/contracts/bank-of-z-spring-h2-v2.json"))!.AsObject();

    private const string ScreenBms = "SCREEN   DFHMSD TYPE=DSECT,LANG=COBOL,MODE=INOUT\n"
        + "PANEL    DFHMDI SIZE=(24,80)\n"
        + "FIELD1   DFHMDF POS=(1,1),LENGTH=4\n"
        + "         DFHMSD TYPE=FINAL\n         END\n";

    private void TerminalMap()
    {
        Save(Path.Join(Source, "SCREEN.bms"), ScreenBms);
        var generated = Path.Join(Source, ".preprocessed");
        BmsSymbolicMap.Generate(Source, generated).Warnings.Should().BeEmpty();
        Stage(Path.Join(generated, "SCREEN.cpy"), Path.Join(Source, "SCREEN.cpy"), derived: true);
    }

    private void CheckTerminalGate(bool allowed, object index)
    {
        if (GateAssembly is not { } gate) return;
        var target = Path.Join(Work, $"terminal-gate-{index}");
        var (code, output) = Run("dotnet", [gate, "dependency-plan", "--source", Source, "--facts", Output, "--output", target], 60);
        code.Should().Be(0, output);
        var pilot = Objects(PyJson.ReadFile(Path.Join(target, "draft-conversion-plan.json"))!["programs"])
            .First(program => (string?)program["program"] == Pilot);
        ((string?)pilot["status"]).Should().Be(allowed ? "eligible" : "blocked", pilot.ToJsonString());
    }

    private JsonObject Bound(string? sqlDeclarations = null) =>
        RektEvidence.BuildEvidence(Source, Pilot, Output, 0, parserSourceRoot: "/source", sqlDeclarations: sqlDeclarations);

    private bool IsCurrent() => RektEvidence.EvidenceCurrent(Source, Pilot, Output);

    private void Persist(JsonNode report) => RektEvidence.WriteJson(Report, report);

    private void SaveExport(JsonNode document) => File.WriteAllText(ExportFile, PyJson.Dumps(document));

    // Rewrites the export and re-seals the report to it, so only the export's own content can make it stale.
    private void Forge(JsonNode document, JsonNode bound)
    {
        SaveExport(document);
        var forged = bound.DeepClone();
        forged["exportSha256"] = Digest(ExportFile);
        Persist(forged);
    }

    private static List<JsonObject> Bindings(JsonNode statements) =>
        Objects(statements).SelectMany(statement => statement["resourceBindings"] is JsonArray found ? Objects(found) : []).ToList();

    private static bool AllBound(JsonNode statements) => Objects(statements).All(RektEvidence.NativeCicsStatementBound);

    private static string Kind(JsonNode? node) => (string)node!["kind"]!;

    private static List<string> Unresolved(JsonObject bound) => Strings(bound["unresolvedDiagnostics"]);

    [Fact]
    public void TerminalBindingsAndContinuations_PassConsumers_AndRejectTampering()
    {
        if (Image is null) return;
        TerminalMap();
        const string body =
            "           EXEC CICS SEND CONTROL ERASE FREEKB END-EXEC.\n"
            + "           EXEC CICS SEND TEXT FROM(MESSAGE-TEXT) ERASE\n"
            + "               RESP(RESPONSE-CODE) RESP2(RESPONSE-TWO) END-EXEC.\n"
            + "           EXEC CICS SEND MAP('PANEL') MAPSET('SCREEN')\n"
            + "               FROM(PANELO) DATAONLY FREEKB END-EXEC.\n"
            + "           EXEC CICS SEND MAP('PANEL') MAPSET('SCREEN')\n"
            + "               MAPONLY ERASE END-EXEC.\n"
            + "           EXEC CICS RECEIVE MAP('PANEL') MAPSET('SCREEN')\n"
            + "               INTO(PANELI) TERMINAL ASIS\n"
            + "               RESP(RESPONSE-CODE) END-EXEC.\n"
            + "           EXEC CICS INQUIRE TERMINAL(EIBTRMID)\n"
            + "               UCTRANST(TRANSLATION) RESP(RESPONSE-CODE) END-EXEC.\n"
            + "           EXEC CICS INQUIRE ASSOCIATION(EIBTASKN)\n"
            + "               ODAPPLID(MESSAGE-TEXT) ODUSERID(MESSAGE-TEXT)\n"
            + "               ODFACILNAME(MESSAGE-TEXT)\n"
            + "               ODNETWORKID(MESSAGE-TEXT)\n"
            + "               ODFACILTYPE(TRANSLATION) END-EXEC.\n"
            + "           EXEC CICS SET TERMINAL(EIBTRMID)\n"
            + "               UCTRANST(TRANSLATION) RESP2(RESPONSE-TWO) END-EXEC.\n"
            + "           IF TRANSLATION = DFHVALUE(UCTRAN)\n"
            + "               CONTINUE END-IF.\n"
            + "           IF TRANSLATION = DFHVALUE(TRANIDONLY)\n"
            + "               CONTINUE END-IF.\n"
            + "           IF TRANSLATION = DFHVALUE(NOUCTRAN)\n"
            + "               CONTINUE END-IF.\n"
            + "           EXEC CICS HANDLE ABEND CANCEL END-EXEC.\n"
            + "           EXEC CICS BIF DEEDIT FIELD(FIELD1I)\n"
            + "               LENGTH(4) RESP(RESPONSE-CODE) END-EXEC.\n"
            + "           EXEC CICS RETURN TRANSID('NEXT')\n"
            + "               COMMAREA(MESSAGE-TEXT) LENGTH(8) IMMEDIATE\n"
            + "               RESP(RESPONSE-CODE) RESP2(RESPONSE-TWO) END-EXEC.\n";
        const string resources = "resourceDefinitions:\n"
            + "  - program:\n      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n"
            + "  - transaction:\n      name: NEXT\n      program: PAYHELP\n"
            + "      dynamic: 'NO'\n      routable: 'NO'\n      status: ENABLED\n";
        var result = Parse(body, declarations:
            "       COPY SCREEN.\n       01 MESSAGE-TEXT PIC X(8).\n"
            + "       01 RESPONSE-CODE PIC S9(8) COMP.\n       01 RESPONSE-TWO PIC S9(8) COMP.\n"
            + "       01 TRANSLATION PIC S9(8) COMP.\n", cicsResources: resources);
        result.Code.Should().Be(0, result.Why);
        AllBound(result.Statements).Should().BeTrue();
        Objects(result.Exported["dependencies"]).Select(child => (string)child["dependencyKind"]!).Should().Equal("COPY");
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Objects(bound["definitions"]).Should().Contain(item => Kind(item) == "CICS_CONTINUATION");
        Persist(bound);
        IsCurrent().Should().BeTrue();
        CheckTerminalGate(true, 0);
        var index = 0;
        foreach (var damage in new[] { "source-text", "map", "context", "continuation", "data", "missing-map" })
        {
            index++;
            var document = result.Exported.DeepClone();
            var statements = document["dependencyDialectStatements"]!;
            JsonObject First(string kind) => Bindings(statements).First(item => Kind(item) == kind);
            switch (damage)
            {
                case "source-text": statements[0]!["sourceText"] = (string)statements[0]!["sourceText"]! + " ALARM"; break;
                case "map": First("CICS_MAP")["mapset"] = "OTHER"; break;
                case "context": First("CICS_CONTEXT")["role"] = "invented-storage"; break;
                case "continuation": First("CICS_TRANSACTION")["controlEffect"] = "synchronous-call"; break;
                case "data": First("CICS_DATA")["definitionSyntax"]!["rule"] = "UnknownContext"; break;
                default:
                    var receive = Objects(statements).First(statement => (string?)statement["operation"] == "RECEIVE");
                    receive["resourceBindings"] = new JsonArray(Objects(receive["resourceBindings"])
                        .Where(item => Kind(item) != "CICS_MAP").Select(item => (JsonNode)item.DeepClone()).ToArray());
                    break;
            }
            AllBound(statements).Should().BeFalse(damage);
            Forge(document, bound);
            IsCurrent().Should().BeFalse(damage);
            CheckTerminalGate(false, index);
        }
        Forge(result.Exported, bound);
        var screen = Path.Join(Source, "SCREEN.bms");
        File.WriteAllText(screen, Text(screen).Replace("LENGTH=4", "LENGTH=5"));
        IsCurrent().Should().BeFalse();
        CheckTerminalGate(false, 7);
    }

    private const string ConstantDeclarations =
        "       COPY SCREEN.\n       01 MESSAGE-TEXT PIC X(8).\n"
        + "       01 WS-LITERALS.\n"
        + "          05 LIT-TRAN PIC X(4) VALUE 'NEXT'.\n"
        + "          05 LIT-MAP PIC X(7) VALUE 'PANEL'.\n"
        + "          05 LIT-MAPSET PIC X(8) VALUE 'SCREEN'.\n";

    private const string ConstantBody =
        "           MOVE LIT-TRAN TO MESSAGE-TEXT.\n"
        + "           IF MESSAGE-TEXT = LIT-TRAN CONTINUE END-IF.\n"
        + "           EXEC CICS RECEIVE MAP(LIT-MAP) MAPSET(LIT-MAPSET)\n"
        + "               INTO(PANELI) END-EXEC.\n"
        + "           EXEC CICS SEND MAP(LIT-MAP) MAPSET(LIT-MAPSET)\n"
        + "               FROM(PANELO) DATAONLY END-EXEC.\n"
        + "           EXEC CICS RETURN TRANSID(LIT-TRAN) END-EXEC.\n";

    // DFHCSDUP form as shipped with CardDemo; PAYHELP omits LANGUAGE like several CardDemo programs.
    private const string ConstantCsd =
        " DEFINE PROGRAM(PAYHELP) GROUP(PILOT)\n DESCRIPTION(PILOT HELPER)\n"
        + "        RELOAD(NO) STATUS(ENABLED) DYNAMIC(NO) TRANSID(NEXT)\n"
        + " DEFINE TRANSACTION(NEXT) GROUP(PILOT)\n"
        + "        PROGRAM(PAYHELP) STATUS(ENABLED) DYNAMIC(NO) ROUTABLE(NO)\n";

    [Fact]
    public void ReadOnlyConstants_ResolveCicsNamesFromACsd_AndRejectTampering()
    {
        if (Image is null) return;
        TerminalMap();
        var result = Parse(ConstantBody, declarations: ConstantDeclarations, cicsResources: ConstantCsd, cicsResourcesFile: "PILOT.CSD");
        result.Code.Should().Be(0, result.Why);
        AllBound(result.Statements).Should().BeTrue();
        var bindings = Bindings(result.Statements);
        var transaction = bindings.First(item => Kind(item) == "CICS_TRANSACTION");
        ((string?)transaction["name"]).Should().Be("NEXT");
        ((string?)transaction["targetResolution"]!["kind"]).Should().Be("immutable-working-storage-literal");
        ((string?)transaction["programBinding"]!["attributes"]!["languageProvenance"]).Should().Be("cics-blank-language-environment");
        bindings.Count(item => Kind(item) == "CICS_MAP" && (string?)item["targetResolution"]!["mapset"]!["operand"] == "LIT-MAPSET")
            .Should().Be(2);
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Persist(bound);
        CheckTerminalGate(true, 0);
        var index = 0;
        foreach (var damage in new[] { "transaction-value", "map-ancestor", "literal-claim" })
        {
            index++;
            var document = result.Exported.DeepClone();
            var found = Bindings(document["dependencyDialectStatements"]!);
            switch (damage)
            {
                case "transaction-value":
                    var proof = found.First(item => Kind(item) == "CICS_TRANSACTION")["targetResolution"]!;
                    foreach (var node in RektEvidence.SyntaxNodes(proof["definitionSyntax"]))
                        if ((string?)node["token"] == "'NEXT'") node["token"] = "'NEXX'";
                    break;
                case "map-ancestor":
                    found.First(item => Kind(item) == "CICS_MAP")["targetResolution"]!["map"]!["ancestorSyntax"] = new JsonArray();
                    break;
                default:
                    found.First(item => Kind(item) == "CICS_TRANSACTION")["targetResolution"] = O("""{"kind": "literal"}""");
                    break;
            }
            AllBound(document["dependencyDialectStatements"]!).Should().BeFalse(damage);
            Forge(document, bound);
            CheckTerminalGate(false, index);
        }
    }

    private const string CardDemoForms =
        "           INITIALIZE WS-WORK-AREA.\n"
        + "           MOVE LIT-MAP TO WS-MAP.\n"
        + "           EXEC CICS HANDLE CONDITION MAPFAIL(FAILURE)\n"
        + "               ERROR(FAILURE) END-EXEC.\n"
        + "           EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n"
        + "               MAPONLY ERASE END-EXEC.\n"
        + "           EXEC CICS SEND FROM(MESSAGE-TEXT) LENGTH(8)\n"
        + "               WAIT RESP(RESPONSE-CODE) END-EXEC.\n"
        + "           EXEC CICS SEND FROM(MESSAGE-TEXT)\n"
        + "               LENGTH(LENGTH OF MESSAGE-TEXT) NOHANDLE ERASE END-EXEC.\n"
        + "           EXEC CICS WRITEQ TD QUEUE('JOBS') FROM(MESSAGE-TEXT)\n"
        + "               LENGTH(8) RESP(RESPONSE-CODE) END-EXEC.\n"
        + "           GOBACK.\n       FAILURE.\n";

    private const string CardDemoDeclarations =
        "       COPY SCREEN.\n       01 MESSAGE-TEXT PIC X(8).\n"
        + "       01 WS-WORK-AREAS.\n         05 WS-WORK-AREA.\n"
        + "           10 WS-MAP PIC X(7).\n"
        + "       01 LIT-MAP PIC X(7) VALUE 'PANEL'.\n"
        + "       01 RESPONSE-CODE PIC S9(8) COMP.\n";

    // DFHCSDUP form of CardDemo's internal-reader queue.
    private const string CardDemoTdq =
        " DEFINE TDQUEUE(JOBS) GROUP(PILOT)\n"
        + "        TYPE(EXTRA) DDNAME(INREADER) TYPEFILE(OUTPUT)\n"
        + "        RECORDSIZE(80) RECORDFORMAT(FIXED)\n";

    [Fact]
    public void CardDemoScreenQueueAndConditionForms_BindExactly()
    {
        if (Image is null) return;
        TerminalMap();
        var result = Parse(CardDemoForms, declarations: CardDemoDeclarations, cicsResources: CardDemoTdq, cicsResourcesFile: "PILOT.CSD");
        result.Code.Should().Be(0, result.Why);
        AllBound(result.Statements).Should().BeTrue();
        var bindings = Bindings(result.Statements);
        bindings.Where(item => Kind(item) == "CICS_MAP").Select(item => (string)item["targetResolution"]!["map"]!["kind"]!)
            .Should().Equal("structured-reaching-assignments");
        var queue = bindings.First(item => Kind(item) == "CICS_TDQUEUE");
        ((string?)queue["name"]).Should().Be("JOBS");
        ((string?)queue["attributes"]!["ddname"]).Should().Be("INREADER");
        bindings.Count(item => Kind(item) == "CICS_LABEL").Should().Be(2);
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Persist(bound);
        CheckTerminalGate(true, 0);
        var damaged = Objects(result.Statements).First(statement => (string?)statement["operation"] == "WRITEQ").DeepClone();
        Objects(damaged["resourceBindings"]).First(item => Kind(item) == "CICS_TDQUEUE")["attributes"]!["typefile"] = "INPUT";
        RektEvidence.NativeCicsStatementBound(damaged).Should().BeFalse();
        foreach (var (name, body, csd, diagnostic) in new[]
        {
            ("two-maps", CardDemoForms.Replace("           EXEC CICS SEND MAP", "           IF RESPONSE-CODE = 0\n"
                + "               MOVE 'OTHER' TO WS-MAP END-IF.\n           EXEC CICS SEND MAP"), CardDemoTdq,
                "dynamic-cics-map-binding-unresolved"),
            ("handler-reaches-map", CardDemoForms + "           EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n"
                + "               MAPONLY END-EXEC.\n", CardDemoTdq, "dynamic-cics-map-binding-unresolved"),
            ("missing-queue", CardDemoForms, CardDemoTdq.Replace("JOBS", "LOGS"), "cics-tdqueue-definition-not-found:JOBS"),
            ("intra-queue", CardDemoForms, CardDemoTdq.Replace("EXTRA", "INTRA"), "cics-tdqueue-definition-invalid:"),
        })
        {
            var rejected = Parse(body, declarations: CardDemoDeclarations, cicsResources: csd, cicsResourcesFile: "PILOT.CSD");
            rejected.Code.Should().NotBe(0, name);
            rejected.Diagnostics.Should().Contain(issue => issue.Contains(diagnostic), name);
        }
    }

    private const string StraightLineForms =
        "           MOVE LIT-MAP TO WS-MAP.\n"
        + "           GO TO SHOW-SCREEN.\n"
        + "       SHOW-SCREEN.\n"
        + "           MOVE LIT-MAP TO WS-MAP\n"
        + "           SET IS-READY TO TRUE\n"
        + "           EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n"
        + "               MAPONLY ERASE END-EXEC.\n"
        + "           GOBACK.\n";

    private const string StraightLineDeclarations = CardDemoDeclarations + "       01 READY-FLAG PIC X.\n         88 IS-READY VALUE 'Y'.\n";

    [Fact]
    public void StraightLineAssignment_BindsMapDespiteGoTo()
    {
        if (Image is null) return;
        TerminalMap();
        var result = Parse(StraightLineForms, declarations: StraightLineDeclarations);
        result.Code.Should().Be(0, result.Why);
        AllBound(result.Statements).Should().BeTrue();
        var send = Objects(result.Statements).First(statement => (string?)statement["operation"] == "SEND");
        var proof = Objects(send["resourceBindings"]).First(item => Kind(item) == "CICS_MAP")["targetResolution"]!["map"]!;
        ((string?)proof["usageProof"]).Should().Be("single-native-program:paragraph-straight-line-assignment");
        Strings(proof["reachingValues"]).Should().Equal("PANEL");
        proof["assignments"]!.AsArray().Should().HaveCount(1);
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Persist(bound);
        CheckTerminalGate(true, 0);
        var doubled = send.DeepClone();
        var mapProof = Objects(doubled["resourceBindings"]).First(item => Kind(item) == "CICS_MAP")["targetResolution"]!["map"]!;
        mapProof["assignments"]!.AsArray().Add(mapProof["assignments"]![0]!.DeepClone());
        RektEvidence.NativeCicsStatementBound(doubled).Should().BeFalse();
        foreach (var (name, body, declarations) in new[]
        {
            ("nested-send", StraightLineForms.Replace(
                "           EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n               MAPONLY ERASE END-EXEC.\n",
                "           IF RESPONSE-CODE = 0\n               MOVE 'OTHER' TO WS-MAP\n"
                + "               EXEC CICS SEND MAP(WS-MAP) MAPSET('SCREEN')\n"
                + "                   MAPONLY ERASE END-EXEC\n           END-IF.\n"), StraightLineDeclarations),
            ("condition-on-item", StraightLineForms.Replace("IS-READY", "IS-PANEL"),
                ReplaceFirst(StraightLineDeclarations, "PIC X(7).\n", "PIC X(7).\n             88 IS-PANEL VALUE 'X'.\n")),
            ("truncated", StraightLineForms, StraightLineDeclarations.Replace("10 WS-MAP PIC X(7).", "10 WS-MAP PIC X(4).")),
        })
        {
            var rejected = Parse(body, declarations: declarations);
            rejected.Code.Should().NotBe(0, name);
            rejected.Diagnostics.Should().Contain("dynamic-cics-map-binding-unresolved", name);
        }
    }

    private static string ReplaceFirst(string text, string old, string replacement)
    {
        var at = text.IndexOf(old, StringComparison.Ordinal);
        return at < 0 ? text : text[..at] + replacement + text[(at + old.Length)..];
    }

    [Fact]
    public void ConstantsThatMayChange_StayUnresolved()
    {
        if (Image is null) return;
        TerminalMap();
        foreach (var (name, declarations, body) in new[]
        {
            ("moved-into", ConstantDeclarations, "           MOVE 'XXXX' TO LIT-TRAN.\n"),
            ("group-initialized", ConstantDeclarations, "           INITIALIZE WS-LITERALS.\n"),
            ("redefined", ConstantDeclarations + "       01 OTHER-VIEW REDEFINES WS-LITERALS PIC X(19).\n", ""),
            ("condition-name", ConstantDeclarations.Replace("VALUE 'NEXT'.", "VALUE 'NEXT'.\n             88 IS-NEXT VALUE 'NEXT'."), ""),
            ("occurs-group", ConstantDeclarations.Replace("01 WS-LITERALS.\n", "01 WS-LITERALS.\n          03 WS-ROW OCCURS 2.\n"), ""),
            ("passed-by-reference", ConstantDeclarations, "           CALL 'PAYHELP' USING LIT-TRAN.\n"),
        })
        {
            var result = Parse(body + ConstantBody, declarations: declarations, cicsResources: ConstantCsd, cicsResourcesFile: "PILOT.CSD");
            Objects(result.Statements).SelectMany(statement => statement["bindingDiagnostics"] is JsonArray found ? Strings(found) : [])
                .Should().Contain("dynamic-cics-return-transaction-unresolved:(LIT-TRAN)", name);
        }
    }

    [Fact]
    public void TerminalMissingResourcesDeclarationsAndUnsupportedForms_StayBlocked()
    {
        if (Image is null) return;
        TerminalMap();
        foreach (var (body, diagnostic) in new[]
        {
            ("EXEC CICS BIF DEEDIT FIELD(MISSING-FIELD) END-EXEC.", "cics-declaration-not-found"),
            ("EXEC CICS RETURN TRANSID('NEXT') END-EXEC.", "cics-transaction-definition-not-found"),
            ("EXEC CICS SEND MAP('MISSING') MAPSET('SCREEN') MAPONLY END-EXEC.", "cics-map-definition-not-found"),
            ("EXEC CICS SEND MAP('PANEL') MAPONLY END-EXEC.", "cics-map-definition-not-found"),
            ("EXEC CICS HANDLE ABEND RESET END-EXEC.", "cics-handle-operation-unsupported"),
            ("EXEC CICS BIF DIGEST RECORD('X') RESULT(WS-TARGET) END-EXEC.", "cics-terminal-variant-unsupported"),
            ("EXEC CICS INQUIRE ASSOCIATION(EIBTASKN)\n               ODAPPLID(MISSING-FIELD) END-EXEC.", "cics-declaration-not-found"),
            ("EXEC CICS INQUIRE ASSOCIATION(EIBTASKN)\n               ODAPPLID(WS-TARGET)\n"
                + "               ODAPPLID(WS-TARGET) END-EXEC.", "cics-option-ambiguous"),
        })
        {
            var result = Parse("           " + body.Replace(" END-EXEC.", "\n               END-EXEC.") + "\n");
            result.Code.Should().NotBe(0, body);
            result.Diagnostics.Should().Contain(issue => issue.Contains(diagnostic), body);
        }
        foreach (var suffix in new[] { "", ".bms-source", ".bms-layout.json" })
            File.Copy(Path.Join(Source, "SCREEN.cpy" + suffix), Path.Join(Source, "OTHER.cpy" + suffix));
        var ambiguous = Parse("           EXEC CICS SEND MAP('PANEL') MAPSET('SCREEN')\n               MAPONLY END-EXEC.\n");
        ambiguous.Code.Should().NotBe(0);
        ambiguous.Diagnostics.Should().Contain("cics-map-definition-ambiguous:SCREEN:PANEL");
    }

    [Fact]
    public void ChannelFetchDelayAndSymbolicCompletionValues_KeepAllOperands()
    {
        if (Image is null) return;
        const string body =
            "           EXEC CICS PUT CONTAINER(CONT-NAME) FROM(PAYLOAD)\n"
            + "               FLENGTH(DATA-LENGTH) CHANNEL(CHAN-NAME) END-EXEC.\n"
            + "           EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n"
            + "           EXEC CICS FETCH ANY(CHILD-TOKEN) CHANNEL(CHAN-NAME)\n"
            + "               NOSUSPEND COMPSTATUS(CHILD-STATUS) ABCODE(ABEND-CODE)\n"
            + "               RESP(RESPONSE-CODE) END-EXEC.\n"
            + "           EXEC CICS GET CONTAINER(CONT-NAME) CHANNEL(CHAN-NAME)\n"
            + "               INTO(PAYLOAD) FLENGTH(DATA-LENGTH) END-EXEC.\n"
            + "           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)\n"
            + "               YYMMDD(FORMAT-DATE) DATESEP('.') END-EXEC.\n"
            + "           IF CHILD-STATUS = DFHVALUE(NORMAL) CONTINUE END-IF.\n"
            + "           IF CHILD-STATUS = DFHVALUE(ABEND) CONTINUE END-IF.\n"
            + "           IF CHILD-STATUS = DFHVALUE(SECERROR) CONTINUE END-IF.\n";
        const string declarations =
            "       01 CONT-NAME PIC X(16).\n       01 CHAN-NAME PIC X(16).\n"
            + "       01 PAYLOAD PIC X(8).\n       01 DATA-LENGTH PIC S9(9) COMP.\n"
            + "       01 CHILD-TOKEN PIC X(16).\n       01 CHILD-STATUS PIC S9(9) COMP.\n"
            + "       01 ABS-TIME PIC S9(15) COMP-3.\n       01 FORMAT-DATE PIC X(8).\n"
            + "       01 ABEND-CODE PIC X(4).\n       01 RESPONSE-CODE PIC S9(9) COMP.\n";
        var result = Parse(body, declarations: declarations);
        result.Code.Should().Be(0, result.Why);
        Objects(result.Statements).Select(item => (string)item["operation"]!)
            .Should().Equal("PUT", "DELAY", "FETCH", "GET", "FORMATTIME", "DFHVALUE", "DFHVALUE", "DFHVALUE");
        Unresolved(Bound()).Should().BeEmpty();
    }

    [Fact]
    public void AsyncTransactionTargets_RequireACompleteFiniteLoop_AndRealCatalogSources()
    {
        if (Image is null) return;
        const string declarations = "       01 LOOP-NUM PIC 9 VALUE 0.\n       01 TRAN-ID PIC X(4) VALUE SPACES.\n"
            + "       01 CHAN-NAME PIC X(16).\n       01 CHILD-TOKEN PIC X(16).\n";
        const string body = "           PERFORM VARYING LOOP-NUM FROM 1 BY 1 UNTIL LOOP-NUM > 2\n"
            + "               STRING 'TST' DELIMITED BY SIZE\n"
            + "                   LOOP-NUM DELIMITED BY SIZE INTO TRAN-ID END-STRING\n"
            + "               EXEC CICS RUN TRANSID(TRAN-ID) CHANNEL(CHAN-NAME)\n"
            + "                   CHILD(CHILD-TOKEN) END-EXEC\n"
            + "           END-PERFORM.\n";
        const string catalog = "resourceDefinitions:\n"
            + "  - program:\n      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n"
            + "  - transaction:\n      name: TST1\n      program: PAYHELP\n"
            + "      dynamic: 'NO'\n      routable: 'NO'\n      status: ENABLED\n"
            + "  - transaction:\n      name: TST2\n      program: PAYHELP\n"
            + "      dynamic: 'NO'\n      routable: 'NO'\n      status: ENABLED\n";
        foreach (var damage in new[] { "", "target-write", "counter-write", "missing-route", "remote", "test-after" })
        {
            var prepared = damage switch
            {
                "target-write" => body.Replace("               EXEC CICS", "               MOVE 'BADX' TO TRAN-ID\n               EXEC CICS"),
                "counter-write" => body.Replace("               EXEC CICS", "               ADD 1 TO LOOP-NUM\n               EXEC CICS"),
                "test-after" => body.Replace("PERFORM VARYING", "PERFORM WITH TEST AFTER VARYING"),
                _ => body,
            };
            var resources = damage switch
            {
                "missing-route" => catalog[..catalog.IndexOf("  - transaction:\n      name: TST2", StringComparison.Ordinal)],
                "remote" => catalog.Replace("routable: 'NO'", "routable: 'YES'"),
                _ => catalog,
            };
            var result = Parse(prepared, declarations: declarations, cicsResources: resources);
            (result.Code == 0).Should().Be(damage == "", damage + " " + result.Why);
            if (damage != "") continue;
            var run = Objects(result.Statements).Should().ContainSingle().Subject;
            var transactions = Objects(run["resourceBindings"]).Where(item => Kind(item) == "CICS_TRANSACTION").ToList();
            transactions.Select(item => (string)item["name"]!).Should().Equal("TST1", "TST2");
            Objects(result.Exported["dependencies"]).Select(item => (string)item["dependencyKind"]!).Should().Equal("CICS_RUN");
            var bound = Bound();
            Unresolved(bound).Should().BeEmpty();
            Persist(bound);
            IsCurrent().Should().BeTrue();
            transactions[0]["targetResolution"]!["transactionNames"] = new JsonArray("TST1");
            Forge(result.Exported, bound);
            IsCurrent().Should().BeFalse();
        }
    }

    [Fact]
    public void BmsLayout_RequiresTheExactSupportedSourceCopyAndManifest()
    {
        if (Image is null) return;
        var screen = Path.Join(Source, "SCREEN.bms");
        Save(screen, ScreenBms);
        var generated = Path.Join(Work, "derived");
        BmsSymbolicMap.Generate(Source, generated).Warnings.Should().BeEmpty();
        var copybook = Path.Join(Source, "SCREEN.cpy");
        Stage(Path.Join(generated, "SCREEN.cpy"), copybook, derived: true);
        const string body = "           MOVE 'TEST' TO FIELD1O.\n";
        var result = Parse(body, declarations: "       COPY SCREEN.\n");
        result.Code.Should().Be(0, result.Why);
        ((string?)Objects(result.Exported["dependencyDerivedInputs"]).Should().ContainSingle().Subject["kind"]).Should().Be("BMS_LAYOUT");
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Objects(bound["definitions"]).Select(Kind).ToHashSet().Should().BeEquivalentTo(["BMS", "BMS_LAYOUT", "COPY"]);
        Persist(bound);
        IsCurrent().Should().BeTrue();
        var originalExport = Text(ExportFile);
        result.Exported["dependencyDerivedInputs"] = new JsonArray();
        Forge(result.Exported, bound);
        IsCurrent().Should().BeFalse();
        File.WriteAllText(ExportFile, originalExport);
        File.WriteAllText(copybook + ".bms-source", Text(screen).Replace("LENGTH=4", "LENGTH=5"));
        var rejected = Parse(body, declarations: "       COPY SCREEN.\n");
        rejected.Code.Should().NotBe(0);
        rejected.Diagnostics.Should().Contain("partial-derived-bms-layout:SCREEN.cpy");
    }

    private const string ControlDeclaration = "           EXEC SQL DECLARE STTESTER.CONTROL TABLE\n"
        + "               (CONTROL_VALUE_NUM INTEGER)\n           END-EXEC.\n";
    private const string ControlSelect = "           EXEC SQL SELECT CONTROL_VALUE_NUM INTO :RESULT-VALUE\n"
        + "               FROM CONTROL END-EXEC.\n";
    private const string ResultHost = "       01 RESULT-VALUE PIC S9(9) COMP.\n";

    [Fact]
    public void SharedSqlCatalog_IsExplicitHashBound_AndDoesNotReplaceLocalDeclarations()
    {
        if (Image is null) return;
        var catalog = Path.Join(Source, "catalog.cpy");
        Save(catalog, ControlDeclaration);
        var result = Parse(ControlSelect, declarations: ResultHost, profile: TargetProfile(), sqlDeclarations: "/source/catalog.cpy");
        result.Code.Should().Be(0, result.Why);
        var binding = Objects(result.Statements[0]!["resourceBindings"]).Where(item => Kind(item) == "SQL_RELATION")
            .Should().ContainSingle().Subject;
        ((string?)binding["definitionScope"]).Should().Be("explicit-sql-catalog");
        ((string?)binding["sourcePath"]).Should().Be("/source/catalog.cpy");
        var bound = Bound("/source/catalog.cpy");
        Unresolved(bound).Should().BeEmpty();
        var declaration = Objects(bound["definitions"]).Where(item => Kind(item) == "SQL_DECLARATION").Should().ContainSingle().Subject;
        ((string?)declaration["path"]).Should().Be(PyCompat.Resolve(catalog));
        Persist(bound);
        IsCurrent().Should().BeTrue();
        bound["definitions"]!.AsArray().Remove(declaration);
        Persist(bound);
        IsCurrent().Should().BeFalse();
        Unresolved(Bound()).Should().Contain(item => item.Contains("native-sql-catalog-unverified"));
        File.WriteAllText(catalog, Text(catalog).Replace("INTEGER", "CHAR(12)"));
        Unresolved(Bound("/source/catalog.cpy")).Should().Contain(item => item.Contains("native-sql-catalog-unverified"));

        var unscoped = Parse(ControlSelect, declarations: ResultHost, profile: TargetProfile());
        unscoped.Code.Should().NotBe(0);
        unscoped.Diagnostics.Should().Contain("sql-declaration-not-found:SQL_RELATION:CONTROL");
        var local = Parse(ControlSelect, declarations: ResultHost + ControlDeclaration, profile: TargetProfile(),
            sqlDeclarations: "/source/catalog.cpy");
        local.Code.Should().Be(0, local.Why);
        ((string?)Bindings(local.Statements).Where(item => Kind(item) == "SQL_RELATION").Should().ContainSingle()
            .Subject["definitionScope"]).Should().Be("expanded-parser-input");
    }

    [Fact]
    public void SharedSqlCatalog_RejectsAmbiguousDeclarations_AndNonDeclarationSql()
    {
        if (Image is null) return;
        foreach (var name in new[] { "first.cpy", "second.cpy" }) Save(Path.Join(Source, name), ControlDeclaration);
        var result = Parse(ControlSelect, declarations: ResultHost, profile: TargetProfile(),
            sqlDeclarations: "/source/first.cpy:/source/second.cpy");
        result.Code.Should().NotBe(0);
        result.Diagnostics.Should().Contain("sql-declaration-ambiguous:SQL_RELATION:CONTROL");
        Save(Path.Join(Source, "first.cpy"), "           EXEC SQL COMMIT END-EXEC.\n");
        result = Parse(ControlSelect, declarations: ResultHost, profile: TargetProfile(), sqlDeclarations: "/source/first.cpy");
        result.Code.Should().NotBe(0);
        result.Log.Should().Contain("Only complete DECLARE TABLE");
    }

    [Fact]
    public void LilianAndLocalTimeServices_BindDocumentedContracts_NotCobolStubs()
    {
        if (Image is null) return;
        var result = Parse(
            "           CALL 'CEEDAYS' USING INPUT-DATE DATE-PICTURE\n"
            + "               LILIAN-DAYS FC.\n"
            + "           CALL 'CEELOCT' USING LILIAN-DAYS LOCAL-SECONDS\n"
            + "               LOCAL-TEXT FC.\n",
            declarations: "       01 INPUT-DATE.\n"
                + "          05 DATE-LENGTH PIC S9(4) COMP VALUE 10.\n"
                + "          05 DATE-TEXT PIC X(10) VALUE '2020-01-02'.\n"
                + "       01 DATE-PICTURE.\n"
                + "          05 PICTURE-LENGTH PIC S9(4) COMP VALUE 10.\n"
                + "          05 PICTURE-TEXT PIC X(10) VALUE 'YYYY-MM-DD'.\n"
                + "       01 LILIAN-DAYS PIC S9(8) COMP.\n"
                + "       01 LOCAL-SECONDS COMP-2.\n"
                + "       01 LOCAL-TEXT PIC X(17).\n"
                + "       01 FC PIC X(12).\n", profile: TargetProfile());
        result.Code.Should().Be(0, result.Why);
        var leaves = Objects(result.Exported["dependencies"]).Where(node => (string?)node["dependencyKind"] == "EXTERNAL_RUNTIME").ToList();
        leaves.Select(node => (string)node["callTarget"]!["name"]!).Should().BeEquivalentTo("CEEDAYS", "CEELOCT");
        leaves.Should().OnlyContain(node => PyJson.IsBool(node["dependencyAnalysisComplete"]) && !PyJson.IsTrue(node["dependencyAnalysisComplete"])
            && (string?)node["runtimeContract"]!["runtimeMapping"] == "unverified" && !node.ContainsKey("sourcePath"));
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Objects(bound["definitions"]).Where(item => Kind(item) == "RUNTIME_CONTRACT").Select(item => (string)item["name"]!)
            .Should().BeEquivalentTo("CEEDAYS", "CEELOCT");
    }

    [Fact]
    public void QualifiedCicsFieldsLocksAndResponseSymbols_BindWithoutRuntimeClaims()
    {
        if (Image is null) return;
        var body =
            "           EXEC CICS ASKTIME ABSTIME(ABS-TIME OF TIME-AREA)\n"
            + "               END-EXEC.\n"
            + "           EXEC CICS FORMATTIME ABSTIME(ABS-TIME OF TIME-AREA)\n"
            + "               TIME(PRINT-TIME OF TIME-AREA) END-EXEC.\n"
            + "           EXEC CICS ENQ RESOURCE(LOCK-KEY) LENGTH(8)\n"
            + "               RESP(RESPONSE-CODE) END-EXEC.\n"
            + "           EXEC CICS DEQ RESOURCE(LOCK-KEY) LENGTH(8)\n"
            + "               RESP(RESPONSE-CODE) END-EXEC.\n";
        foreach (var symbol in new[] { "NORMAL", "INVREQ", "MAPFAIL", "NOTFINISHED", "NOTFND", "ENDFILE", "DUPREC", "DUPKEY" })
            body += $"           IF RESPONSE-CODE = DFHRESP({symbol})\n               CONTINUE END-IF.\n";
        var result = Parse(body, declarations: "       01 TIME-AREA.\n"
            + "          05 ABS-TIME PIC S9(15) COMP-3.\n"
            + "          05 PRINT-TIME PIC X(6).\n"
            + "       01 OTHER-AREA.\n"
            + "          05 PRINT-TIME PIC X(6).\n"
            + "       01 LOCK-KEY PIC X(8).\n"
            + "       01 RESPONSE-CODE PIC S9(8) COMP.\n");
        result.Code.Should().Be(0, result.Why);
        Unresolved(Bound()).Should().BeEmpty();
        var statements = Objects(result.Statements);
        var locks = statements.Where(statement => (string?)statement["operation"] is "ENQ" or "DEQ").ToList();
        locks.Should().HaveCount(2);
        locks.Should().OnlyContain(statement => (string?)statement["bindingBasis"] == "parsed-cics-lock-operation");
        statements.Should().OnlyContain(statement => (string?)statement["runtimeMapping"] == "unverified");
        var qualified = Bindings(result.Statements).First(binding => (string?)binding["name"] == "PRINT-TIMEOFTIME-AREA");
        qualified["qualificationPath"]!.AsArray().Should().HaveCount(2);
        qualified["qualificationPath"] = new JsonArray(qualified["definitionSyntax"]!.DeepClone());
        SaveExport(result.Exported);
        ((string?)Bound()["coverage"]!["EXEC_CICS"]).Should().NotBe("complete");
    }

    [Fact]
    public void QualifiedCicsFields_RejectMissingQualifiersAndAmbiguousBareNames()
    {
        if (Image is null) return;
        foreach (var operand in new[] { "PRINT-TIME OF MISSING-AREA", "PRINT-TIME" })
        {
            var result = Parse(
                "           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)\n"
                + $"               TIME({operand}) END-EXEC.\n",
                declarations: "       01 ABS-TIME PIC S9(15) COMP-3.\n"
                    + "       01 TIME-AREA.\n          05 PRINT-TIME PIC X(6).\n"
                    + "       01 OTHER-AREA.\n          05 PRINT-TIME PIC X(6).\n");
            result.Code.Should().NotBe(0, operand);
            result.Diagnostics.Should().Contain(issue => issue.Contains("cics-declaration-"), operand);
        }
    }

    private const string PayhelpCatalog = "resourceDefinitions:\n  - program:\n"
        + "      name: PAYHELP\n      language: COBOL\n      dynamic: 'NO'\n";

    [Fact]
    public void CicsProgramLiterals_AllowOnlyTrailingPaddingWithinEightBytes()
    {
        if (Image is null) return;
        foreach (var (operand, declarations, allowed) in new[]
        {
            ("'PAYHELP '", "", true),
            ("WS-PGM", "       01 WS-PGM PIC X(8) VALUE 'PAYHELP '.\n", true),
            ("'PAY HELP'", "", false),
            ("'PAYHELP  '", "", false),
        })
        {
            var result = Parse($"           EXEC CICS LINK PROGRAM({operand}) END-EXEC.\n", declarations: declarations,
                cicsResources: PayhelpCatalog);
            (result.Code == 0).Should().Be(allowed, operand + " " + result.Log);
            var bound = RektEvidence.BuildEvidence(Source, Pilot, Output, result.Code, parserSourceRoot: "/source");
            (Unresolved(bound).Count == 0).Should().Be(allowed, string.Join('\n', Unresolved(bound)));
        }
    }

    [Theory]
    [InlineData("           EXEC CICS ENQ NOSUSPEND END-EXEC.\n")]
    [InlineData("           IF 0 = DFHRESP(NOTREAL) CONTINUE END-IF.\n")]
    public void CicsLockRequiresAResource_AndResponseSymbolsMustBeSupported(string body)
    {
        if (Image is null) return;
        var result = Parse(body);
        result.Code.Should().NotBe(0);
        PyJson.IsTrue(result.Exported["dependencyAnalysisComplete"]).Should().BeFalse();
    }

    [Fact]
    public void ExternalProfile_RequiresExactConfigurationAndCurrentHash()
    {
        if (Image is null) return;
        var result = Parse(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET\n"
            + "               FROM CONTROL END-EXEC.\n"
            + "           CALL 'CEEGMT' USING GMT-LILIAN GMT-SECONDS FC.\n"
            + "           EXEC CICS WRITE FILE('TESTFILE') FROM(WS-TARGET)\n"
            + "               END-EXEC.\n",
            declarations: "           EXEC SQL DECLARE STTESTER.CONTROL TABLE\n"
                + "               (COL1 CHAR(8)) END-EXEC.\n"
                + "       01 GMT-LILIAN PIC S9(8) BINARY.\n"
                + "       01 GMT-SECONDS COMP-2.\n"
                + "       01 FC PIC X(12).\n",
            profile: TargetProfile(), profileOutsideSource: true,
            cicsResources: "resourceDefinitions:\n  - file:\n      name: TESTFILE\n"
                + "      dsname: TEST.DATA\n      recordsize: 80\n      keylength: 8\n");
        result.Code.Should().Be(0, result.Log);
        PyJson.IsTrue(result.Exported["dependencyAnalysisComplete"]).Should().BeTrue();
        var profilePath = PyCompat.Resolve(Path.Join(Work, "profile.json"));
        var profileText = Text(profilePath);
        var otherPath = Path.Join(Work, "other-profile.json");
        File.WriteAllText(otherPath, profileText);
        JsonObject Profiled(string? configured) => RektEvidence.BuildEvidence(Source, Pilot, Output, 0,
            parserSourceRoot: "/source", dependencyProfile: configured);
        foreach (var configured in new[] { null, otherPath })
        {
            var bound = Profiled(configured);
            Unresolved(bound).Should().Contain(issue => issue.StartsWith("native-sql-profile-unverified:"), configured);
            Unresolved(bound).Should().Contain(issue => issue.StartsWith("native-runtime-contract-unverified:CEEGMT:"), configured);
            PyJson.PyEquals(bound["coverage"], AllKinds("complete")).Should().BeFalse(configured);
        }
        var accepted = Profiled(profilePath);
        Unresolved(accepted).Should().BeEmpty();
        Objects(accepted["definitions"]).Select(Kind).ToHashSet().Should().BeEquivalentTo(["ANALYSIS_PROFILE", "RUNTIME_CONTRACT", "CICS_FILE"]);
        Objects(accepted["definitions"]).Where(item => Kind(item) is "ANALYSIS_PROFILE" or "RUNTIME_CONTRACT")
            .Should().OnlyContain(item => (string?)item["path"] == profilePath);
        Persist(accepted);
        IsCurrent().Should().BeTrue();
        var changed = TargetProfile();
        changed["sqlDefaultSchema"] = "OTHER";
        File.WriteAllText(profilePath, PyJson.Dumps(changed));
        IsCurrent().Should().BeFalse();
        Unresolved(Profiled(profilePath)).Should().NotBeEmpty();
        File.WriteAllText(profilePath, profileText);
        IsCurrent().Should().BeTrue();
        var cicsBinding = Bindings(result.Statements).First(binding => Kind(binding) == "CICS_FILE");
        cicsBinding["sourcePath"] = profilePath;
        cicsBinding["sourceSha256"] = Digest(profilePath);
        SaveExport(result.Exported);
        Unresolved(Profiled(profilePath)).Should().Contain(issue => issue.StartsWith("native-cics-resource-definition-unverified:")
            && issue.Contains("outside declared parser source root"));
    }

    private const string Col1Select = "           EXEC SQL SELECT COL1 INTO :WS-TARGET\n               FROM CONTROL END-EXEC.\n";
    private const string Col1Declaration = "           EXEC SQL DECLARE STTESTER.CONTROL TABLE\n               (COL1 CHAR(8)) END-EXEC.\n";

    [Fact]
    public void ExplicitTargetSchema_BindsOnlyItsActualQualifiedDeclaration()
    {
        if (Image is null) return;
        var result = Parse(Col1Select, declarations: Col1Declaration, profile: TargetProfile());
        result.Code.Should().Be(0, result.Log);
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeTrue();
        var binding = Objects(result.Statements[^1]!["resourceBindings"]).First(item => Kind(item) == "SQL_RELATION");
        ((string?)binding["qualification"]!["schema"]).Should().Be("STTESTER");
        ((string?)binding["qualification"]!["profileKind"]).Should().Be("target-port");
        ((string?)binding["qualification"]!["sourceSha256"]).Should().Be(Digest(Path.Join(Source, "profile.json")));
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Objects(bound["definitions"]).Should().Contain(definition => Kind(definition) == "ANALYSIS_PROFILE");
    }

    [Fact]
    public void WrongExplicitSchema_DoesNotGuessAnotherDeclaredSchema()
    {
        if (Image is null) return;
        var profile = TargetProfile();
        profile["sqlDefaultSchema"] = "OTHER";
        var result = Parse(Col1Select, declarations: Col1Declaration, profile: profile);
        result.Code.Should().NotBe(0);
        string.Join('\n', result.Diagnostics).Should().Contain("sql-declaration-not-found:SQL_RELATION:CONTROL");
    }

    [Fact]
    public void DocumentedPlatformCalls_BindContracts_NotInventedCobolSources()
    {
        if (Image is null) return;
        var result = Parse(
            "           CALL 'CEEGMT' USING GMT-LILIAN GMT-SECONDS FC.\n"
            + "           CALL 'CEEDATM' USING GMT-SECONDS DATM-PICTURE\n"
            + "               DATM-CONV FC.\n",
            declarations: "       01 GMT-LILIAN PIC S9(8) BINARY.\n"
                + "       01 GMT-SECONDS COMP-2.\n"
                + "       01 FC PIC X(12).\n"
                + "       01 DATM-PICTURE.\n"
                + "          05 DATM-LENGTH PIC 9999 COMP.\n"
                + "          05 DATM-FORMAT PIC X(14).\n"
                + "       01 DATM-CONV PIC X(80).\n", profile: TargetProfile());
        result.Code.Should().Be(0, result.Log);
        var leaves = Objects(result.Exported["dependencies"]).Where(node => (string?)node["dependencyKind"] == "EXTERNAL_RUNTIME").ToList();
        leaves.Should().HaveCount(2);
        foreach (var leaf in leaves)
        {
            PyJson.Truthy(leaf["dependencyAnalysisComplete"]).Should().BeFalse();
            PyJson.Truthy(leaf["runtimeContractBindingComplete"]).Should().BeTrue();
            leaf.ContainsKey("sourcePath").Should().BeFalse();
            PyJson.Truthy(leaf["dependencyAbsenceEvidence"]).Should().BeFalse();
            RektEvidence.NativeRuntimeContractBound(leaf, TargetProfile()).Should().BeTrue();
        }
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Objects(bound["definitions"]).Where(item => Kind(item) == "RUNTIME_CONTRACT").Select(item => (string)item["name"]!)
            .Should().BeEquivalentTo("CEEGMT", "CEEDATM");
        Save(Report, bound);
        IsCurrent().Should().BeTrue();
        var changed = TargetProfile();
        changed["sqlDefaultSchema"] = "OTHER";
        Save(Path.Join(Source, "profile.json"), changed);
        IsCurrent().Should().BeFalse();
    }

    [Fact]
    public void UnknownPlatformCall_StillRequiresARealDefinition()
    {
        if (Image is null) return;
        var result = Parse("           CALL 'NOTREAL'.\n", profile: TargetProfile());
        result.Code.Should().NotBe(0);
        string.Join('\n', result.Diagnostics).Should().Contain("source-not-found:NOTREAL");
    }

    [Fact]
    public void SqlReadOnlyClause_IsNotAHostIndicatorAndAnotherFetch()
    {
        if (Image is null) return;
        var result = Parse(
            "           EXEC SQL DECLARE TEST-CURSOR CURSOR FOR\n"
            + "               SELECT COL1 FROM TESTTAB\n"
            + "               WHERE COL1 = :WS-TARGET FOR FETCH ONLY\n"
            + "               END-EXEC.\n"
            + "           EXEC SQL OPEN TEST-CURSOR END-EXEC.\n",
            declarations: "           EXEC SQL DECLARE TESTTAB TABLE\n               (COL1 CHAR(8)) END-EXEC.\n");
        result.Code.Should().Be(0, result.Log);
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeTrue();
        var cursor = Objects(result.Statements).First(statement => Objects(statement["resourceReferences"])
            .Any(reference => (string?)reference["name"] == "TEST-CURSOR" && (string?)reference["role"] == "declaration"));
        Objects(cursor["resourceReferences"]).Select(reference => (string)reference["name"]!).ToHashSet()
            .Should().BeEquivalentTo(["TEST-CURSOR", "TESTTAB", "WS-TARGET"]);
    }

    [Fact]
    public void SqlNullIndicator_IsRetainedAndBoundAsARealHostVariable()
    {
        if (Image is null) return;
        var result = Parse(
            "           EXEC SQL SELECT COL1 INTO :WS-TARGET\n"
            + "               INDICATOR :WS-INDICATOR FROM TESTTAB END-EXEC.\n",
            declarations: "       01 WS-INDICATOR PIC S9(4) COMP.\n"
                + "           EXEC SQL DECLARE TESTTAB TABLE\n               (COL1 CHAR(8)) END-EXEC.\n");
        result.Code.Should().Be(0, result.Log);
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeTrue();
        var select = Objects(result.Statements).First(statement => (string?)statement["operation"] == "SELECT");
        Objects(select["resourceBindings"]).Where(binding => Kind(binding) == "SQL_HOST_VARIABLE").Select(binding => (string)binding["name"]!)
            .ToHashSet().Should().BeEquivalentTo(["WS-TARGET", "WS-INDICATOR"]);
    }

    [Fact]
    public void CurrentTaskCicsCommands_BindActualDataAndLocalAbendLabel()
    {
        if (Image is null) return;
        var result = Parse(
            "           EXEC CICS HANDLE ABEND LABEL(FAILURE) END-EXEC.\n"
            + "           EXEC CICS ASSIGN APPLID(WS-TARGET) END-EXEC.\n"
            + "           EXEC CICS ASKTIME ABSTIME(WS-CLOCK) END-EXEC.\n"
            + "           EXEC CICS FORMATTIME ABSTIME(WS-CLOCK)\n"
            + "               YYYYMMDD(WS-TARGET) END-EXEC.\n"
            + "           EXEC CICS SYNCPOINT ROLLBACK END-EXEC.\n"
            + "           GOBACK.\n       FAILURE SECTION.\n"
            + "           EXEC CICS ABEND ABCODE('TEST') NODUMP END-EXEC.\n",
            declarations: "       01 WS-CLOCK PIC S9(15) COMP-3.\n");
        result.Code.Should().Be(0, result.Log);
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_CICS").Should().BeTrue();
        RektEvidence.NativeDomainEvidence(result.Exported, "FILE_IO").Should().BeTrue();
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Objects(bound["definitions"]).Should().NotContain(definition => Kind(definition) == "CICS_FILE");
    }

    [Fact]
    public void CicsMissingLocalDataOrLabel_StaysUnresolved()
    {
        if (Image is null) return;
        var result = Parse(
            "           EXEC CICS HANDLE ABEND LABEL(NOT-THERE) END-EXEC.\n"
            + "           EXEC CICS ASKTIME ABSTIME(NOT-DECLARED) END-EXEC.\n");
        result.Code.Should().NotBe(0);
        var diagnostics = string.Join('\n', result.Diagnostics);
        diagnostics.Should().Contain("cics-declaration-not-found:CICS_LABEL:NOT-THERE");
        diagnostics.Should().Contain("cics-declaration-not-found:CICS_DATA:NOT-DECLARED");
    }

    [Fact]
    public void LocalCicsLink_BindsCatalogAndParsedCalleeSource()
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS LINK PROGRAM('PAYHELP')\n               COMMAREA(WS-TARGET) END-EXEC.\n",
            cicsResources: PayhelpCatalog);
        result.Code.Should().Be(0, result.Log);
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_CICS").Should().BeTrue();
        var callee = Objects(result.Exported["dependencies"]).First(node => (string?)node["dependencyKind"] == "CICS_LINK");
        ((string?)callee["callTarget"]!["name"]).Should().Be("PAYHELP");
        PyJson.IsTrue(callee["dependencyAnalysisComplete"]).Should().BeTrue();
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Objects(bound["definitions"]).Should().Contain(definition => Kind(definition) == "CALL" && (string?)definition["name"] == "PAYHELP");
        Objects(bound["definitions"]).Should().Contain(definition => Kind(definition) == "CICS_PROGRAM");
        Save(Report, bound);
        IsCurrent().Should().BeTrue();
        File.WriteAllText(Path.Join(Source, "resources.yaml"), "resourceDefinitions: []\n");
        IsCurrent().Should().BeFalse();
    }

    private const string HelpProgramDeclarations = "       01 WS-PGMS.\n         05 WS-HELP-PGM PIC X(08) VALUE 'PAYHELP'.\n";

    [Fact]
    public void CicsLengthOfOperand_BindsTheNamedItem()
    {
        if (Image is null) return;
        const string body = "           EXEC CICS LINK PROGRAM(WS-HELP-PGM)\n"
            + "               COMMAREA(WS-TARGET)\n"
            + "               LENGTH(LENGTH OF WS-TARGET) END-EXEC.\n";
        var result = Parse(body, declarations: HelpProgramDeclarations, cicsResources: PayhelpCatalog);
        result.Code.Should().Be(0, result.Why);
        RektEvidence.NativeCicsStatementBound(result.Statements[^1]).Should().BeTrue();
        var rejected = Parse(body.Replace("OF WS-TARGET", "OF NOT-THERE"), declarations: HelpProgramDeclarations, cicsResources: PayhelpCatalog);
        rejected.Code.Should().NotBe(0);
        rejected.Diagnostics.Should().Contain(issue => issue.Contains("cics-declaration-not-found:CICS_DATA:"));
    }

    private void ClearOutputFiles()
    {
        foreach (var path in Directory.GetFiles(Output)) File.Delete(path);
    }

    private static List<string> LastBindingDiagnostics(Parsed result) => Strings(result.Statements[^1]!["bindingDiagnostics"]);

    private string AllDiagnostics(Parsed result) => string.Join('\n', result.Diagnostics);

    private bool CoverageComplete(JsonObject exported) => PyJson.PyEquals(exported["dependencyCoverage"], AllKinds("complete"));

    private bool AbsenceProven(JsonObject exported) => PyJson.PyEquals(exported["dependencyAbsenceEvidence"], AbsenceProofs());

    [Fact]
    public void CicsLink_BindsAReadOnlyGroupMemberConstant()
    {
        if (Image is null) return;
        const string body = "           EXEC CICS LINK PROGRAM(WS-HELP-PGM)\n               COMMAREA(WS-TARGET) END-EXEC.\n";
        var result = Parse(body, declarations: HelpProgramDeclarations, cicsResources: PayhelpCatalog);
        result.Code.Should().Be(0, result.Why);
        var statement = result.Statements[^1]!;
        Objects(statement["resourceBindings"]).Where(item => Kind(item) == "CICS_PROGRAM")
            .Select(item => ((string?)item["name"], (string?)item["controlEffect"])).Should().Equal(("PAYHELP", "call"));
        RektEvidence.NativeCicsStatementBound(statement).Should().BeTrue();
        ((string?)Objects(result.Exported["dependencies"]).First(node => (string?)node["dependencyKind"] == "CICS_LINK")["callTarget"]!["name"])
            .Should().Be("PAYHELP");
        Unresolved(Bound()).Should().BeEmpty();
        var damaged = statement.DeepClone();
        Objects(damaged["resourceBindings"]).First(item => Kind(item) == "CICS_PROGRAM")["name"] = "PAYMORE";
        RektEvidence.NativeCicsStatementBound(damaged).Should().BeFalse();
        ClearOutputFiles();
        // A literal that reaches the LINK is the target; one without a CSD definition still blocks.
        var undefined = Parse("           MOVE 'PAYMORE' TO WS-HELP-PGM.\n" + body, declarations: HelpProgramDeclarations, cicsResources: PayhelpCatalog);
        undefined.Code.Should().NotBe(0);
        LastBindingDiagnostics(undefined).Should().Contain("cics-program-definition-not-found:PAYMORE");
        ClearOutputFiles();
        var dynamic = Parse("           MOVE WS-TARGET TO WS-HELP-PGM.\n" + body, declarations: HelpProgramDeclarations, cicsResources: PayhelpCatalog);
        dynamic.Code.Should().NotBe(0);
        LastBindingDiagnostics(dynamic).Should().Contain("dynamic-cics-program-binding-unresolved:WS-HELP-PGM");
    }

    [Fact]
    public void CicsLink_DoesNotHideAnIncompleteCallee()
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS LINK PROGRAM('PAYHELP') END-EXEC.\n", calleeDialect: "options", cicsResources: PayhelpCatalog);
        result.Code.Should().NotBe(0);
        PyJson.Truthy(result.Exported["dependencyAnalysisComplete"]).Should().BeFalse();
        PyJson.Truthy(Objects(result.Exported["dependencies"]).First(node => (string?)node["dependencyKind"] == "CICS_LINK")["dependencyAnalysisComplete"])
            .Should().BeFalse();
    }

    [Fact]
    public void CicsLink_RequiresARealProgramCatalogEvenWhenSourceExists()
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS LINK PROGRAM('PAYHELP') END-EXEC.\n");
        result.Code.Should().NotBe(0);
        AllDiagnostics(result).Should().Contain("cics-program-definition-not-found:PAYHELP");
    }

    [Theory]
    [InlineData("", """{"language": "COBOL", "dynamic": "NO", "dynamicProvenance": "cics-default"}""", null)]
    [InlineData("      dynamic: 'NO'\n", """{"language": "COBOL", "dynamic": "NO"}""", null)]
    [InlineData("      dynamic: 'YES'\n", null, "Dynamic CICS program resources are not supported: PAYHELP")]
    [InlineData("      dynamic: 'MAYBE'\n", null, "Invalid CICS program DYNAMIC value")]
    public void CicsProgramDynamic_DefaultsToNoOnlyWhenOmitted(string dynamic, string? attributes, string? error)
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS LINK PROGRAM('PAYHELP') END-EXEC.\n",
            cicsResources: "resourceDefinitions:\n  - program:\n      name: PAYHELP\n      language: COBOL\n" + dynamic);
        if (error is not null)
        {
            result.Code.Should().NotBe(0);
            (result.Log + (result.Exported["dependencyDiagnostics"] is JsonArray ? AllDiagnostics(result) : "")).Should().Contain(error);
            return;
        }
        result.Code.Should().Be(0, result.Log);
        var binding = Bindings(result.Statements).First(item => Kind(item) == "CICS_PROGRAM");
        PyJson.PyEquals(binding["attributes"], J(attributes!)).Should().BeTrue(binding["attributes"]!.ToJsonString());
        Unresolved(Bound()).Should().BeEmpty();
    }

    [Fact]
    public void RemoteCicsLink_DoesNotBindTheLocalProgram()
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS LINK PROGRAM('PAYHELP') SYSID('RMT1') END-EXEC.\n", cicsResources: PayhelpCatalog);
        result.Code.Should().NotBe(0);
        AllDiagnostics(result).Should().Contain("cics-link-variant-unsupported");
    }

    [Theory]
    [InlineData("", "immutable-working-storage-literal", false)]
    [InlineData("       LOCAL-STORAGE SECTION.\n", "immutable-local-storage-literal", true)]
    public void CicsProgramInitialValue_RequiresAWholeProgramNoWriteProof(string section, string kind, bool localStorage)
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS LINK PROGRAM(WS-PGM)\n               COMMAREA(WS-TARGET) END-EXEC.\n",
            declarations: section + "       01 WS-PGM PIC X(8) VALUE 'PAYHELP'.\n"
                + (localStorage ? "" : "       01 UNRELATED-PTR USAGE POINTER.\n"),
            cicsResources: PayhelpCatalog);
        result.Code.Should().Be(0, result.Log);
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_CICS").Should().BeTrue();
        if (localStorage) RektEvidence.NativeDomainEvidence(result.Exported, "FILE_IO").Should().BeTrue();
        var link = Objects(result.Statements).First(statement => (string?)statement["operation"] == "LINK");
        ((string?)Objects(link["resourceBindings"]).First(binding => Kind(binding) == "CICS_PROGRAM")["targetResolution"]!["kind"])
            .Should().Be(kind);
        Unresolved(Bound()).Should().BeEmpty();
    }

    [Theory]
    [InlineData("           MOVE 'OTHER' TO WS-PGM.\n", "", "               COMMAREA(WS-TARGET)", true)]
    [InlineData("           MOVE 'OTHER' TO WS-PGM.\n", "       LOCAL-STORAGE SECTION.\n", "", true)]
    [InlineData("           EXEC SQL SELECT COL1 INTO :WS-PGM FROM CONTROL END-EXEC.\n", "", "", false)]
    [InlineData("           SET WS-PTR TO ADDRESS OF WS-PGM.\n", "", "", false)]
    public void MutatedCicsProgramInitialValue_IsNotAStaticTarget(string write, string section, string commarea, bool noLink)
    {
        if (Image is null) return;
        var link = commarea == "" ? "           EXEC CICS LINK PROGRAM(WS-PGM) END-EXEC.\n"
            : "           EXEC CICS LINK PROGRAM(WS-PGM)\n" + commarea + " END-EXEC.\n";
        var declarations = section + "       01 WS-PGM PIC X(8) VALUE 'PAYHELP'.\n"
            + (write.Contains("EXEC SQL") ? "           EXEC SQL DECLARE CONTROL TABLE (COL1 CHAR(8)) END-EXEC.\n" : "")
            + (write.Contains("ADDRESS OF") ? "       01 WS-PTR USAGE POINTER.\n" : "");
        var result = Parse(write + link, declarations: declarations, cicsResources: PayhelpCatalog);
        result.Code.Should().NotBe(0);
        AllDiagnostics(result).Should().Contain("dynamic-cics-program-binding-unresolved:WS-PGM");
        if (noLink) Objects(result.Exported["dependencies"]).Should().NotContain(node => (string?)node["dependencyKind"] == "CICS_LINK");
    }

    [Fact]
    public void CopyAndStaticCallExport_AreHashBoundToActualInputs()
    {
        if (Image is null) return;
        var result = Parse("           CALL 'PAYHELP' USING PAY-AMOUNT.\n");
        result.Code.Should().Be(0, result.Log);
        var exported = result.Exported;
        ((int)exported["dependencyExportSchemaVersion"]!).Should().Be(1);
        ((string?)exported["sourceSha256"]).Should().Be(Digest(Path.Join(Source, Pilot)));
        var nodes = RektEvidence.ExportedNodes(exported, Pilot);
        var copy = nodes.First(node => (string?)node["dependencyKind"] == "COPY");
        var call = nodes.First(node => (string?)node["dependencyKind"] == "CALL");
        ((string?)copy["callTarget"]!["name"]).Should().Be("PAYREC.cpy");
        ((string?)copy["sourceSha256"]).Should().Be(Digest(Path.Join(Source, "PAYREC.cpy")));
        ((string?)copy["sourcePath"]).Should().Be("/source/PAYREC.cpy");
        ((string?)call["callTarget"]!["name"]).Should().Be("PAYHELP");
        ((string?)call["sourceSha256"]).Should().Be(Digest(Path.Join(Source, "PAYHELP.cbl")));
        CoverageComplete(exported).Should().BeTrue();
        AbsenceProven(exported).Should().BeTrue();
    }

    [Fact]
    public void DynamicCall_DoesNotClaimResolvedStaticCoverage()
    {
        if (Image is null) return;
        var result = Parse("           CALL WS-TARGET.\n");
        result.Code.Should().NotBe(0, result.Log);
        ((string?)result.Exported["dependencyCoverage"]!["CALL"]).Should().Be("partial");
        var call = RektEvidence.ExportedNodes(result.Exported, Pilot).First(node => (string?)node["dependencyKind"] == "CALL");
        ((string?)call["callTarget"]!["programReferenceType"]).Should().Be("DYNAMIC");
        call.ContainsKey("sourceSha256").Should().BeFalse();
    }

    [Fact]
    public void MissingCopybook_IsFailure_NotSuccessfulEmptyExport()
    {
        if (Image is null) return;
        var result = Parse("           DISPLAY 'KNOWN'.\n", missing: true);
        result.Code.Should().NotBe(0);
        ((string?)result.Exported["dependencyCoverage"]!["COPY"]).Should().NotBe("complete");
        result.Diagnostics.Should().NotBeEmpty();
        File.Exists(Path.Join(Source, "PAYREC.cpy")).Should().BeFalse();
    }

    [Fact]
    public void ZeroReferenceCoverage_ComesFromActualSuccessfulParser()
    {
        if (Image is null) return;
        var result = Parse("           DISPLAY 'NO DEPENDENCIES'.\n");
        result.Code.Should().Be(0, result.Log);
        result.Exported["dependencies"]!.AsArray().Should().BeEmpty();
        CoverageComplete(result.Exported).Should().BeTrue();
        AbsenceProven(result.Exported).Should().BeTrue();
    }

    [Fact]
    public void FileIo_IsBoundPresence_NeverZeroFileIo()
    {
        if (Image is null) return;
        var result = Parse("           OPEN INPUT INPUT-FILE.\n           CLOSE INPUT-FILE.\n", fileIo: true);
        result.Code.Should().Be(0, result.Log);
        result.Exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("FILE_IO").Should().BeFalse();
        ((string?)result.Exported["dependencyPresenceEvidence"]!["FILE_IO"]).Should().Be("antlr-tree:bound-file-control-resources");
        RektEvidence.NativeDomainEvidence(result.Exported, "FILE_IO").Should().BeTrue();
    }

    [Fact]
    public void EmptyFileSection_IsNotAFileDependency()
    {
        if (Image is null) return;
        var result = Parse("           DISPLAY 'NO FILE OPERATIONS'.\n", emptyFileSection: true);
        result.Code.Should().Be(0, result.Log);
        CoverageComplete(result.Exported).Should().BeTrue();
        AbsenceProven(result.Exported).Should().BeTrue();
    }

    [Fact]
    public void FileDeclarationWithoutIoStatements_StillBlocksAbsence()
    {
        if (Image is null) return;
        var result = Parse("           DISPLAY 'DECLARED FILE'.\n", fileIo: true);
        result.Code.Should().NotBe(0);
        ((string?)result.Exported["dependencyCoverage"]!["FILE_IO"]).Should().NotBe("complete");
        result.Exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("FILE_IO").Should().BeFalse();
        result.Diagnostics.Should().Contain(item => item.Contains("FileDescriptionEntryContext"));
    }

    [Fact]
    public void BareCicsReturn_HasPositiveCoverage_WithoutClaimingRuntimeMapping()
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS RETURN END-EXEC.\n", emptyFileSection: true);
        result.Code.Should().Be(0, result.Log);
        var exported = result.Exported;
        CoverageComplete(exported).Should().BeTrue();
        ((string?)exported["dependencyAbsenceEvidence"]!["EXEC_SQL"]).Should().Be("antlr-tree:no-sql-contexts");
        exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("EXEC_CICS").Should().BeFalse();
        ((string?)exported["dependencyPresenceEvidence"]!["EXEC_CICS"]).Should().Be("antlr-tree:resource-free-cics-return");
        RektEvidence.NativeDomainEvidence(exported, "EXEC_CICS").Should().BeTrue();
        RektEvidence.NativeDomainEvidence(exported, "EXEC_SQL").Should().BeTrue();
        var statement = Objects(result.Statements).Should().ContainSingle().Subject;
        ((string?)statement["operation"]).Should().Be("RETURN");
        ((string?)statement["runtimeMapping"]).Should().Be("unverified");
        var transfer = Objects(exported["dependencyControlTransfers"]).Should().ContainSingle().Subject;
        ((string?)transfer["variant"]).Should().Be("bare");
        ((string?)transfer["coordinateSpace"]).Should().Be("expanded-parser-input");
        ((string)transfer["sourceText"]!).ToUpperInvariant().Should().Contain("RETURN");
        var sourceLine = PyCompat.SplitLines(Text(Path.Join(Source, Pilot)))[int.Parse(PyJson.PyStr(transfer["line"])) - 1];
        sourceLine[int.Parse(PyJson.PyStr(transfer["column"]))..].Split(' ', StringSplitOptions.RemoveEmptyEntries)[0].Should().Be("RETURN");
        ((string?)transfer["runtimeMapping"]).Should().Be("unverified");
        result.Diagnostics.Should().BeEmpty();
    }

    [Fact]
    public void OptionedCicsReturn_IsNotCollapsedToBareReturn()
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS RETURN TRANSID('NEXT') END-EXEC.\n");
        result.Code.Should().NotBe(0);
        var exported = result.Exported;
        var transfer = Objects(exported["dependencyControlTransfers"]).Should().ContainSingle().Subject;
        ((string?)transfer["variant"]).Should().Be("options");
        ((string)transfer["sourceText"]!).ToUpperInvariant().Should().Contain("TRANSID");
        ((string)transfer["sourceText"]!).Should().Contain("NEXT");
        exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("EXEC_CICS").Should().BeFalse();
        ((string?)exported["dependencyCoverage"]!["EXEC_CICS"]).Should().NotBe("complete");
        ((string?)exported["dependencyAbsenceEvidence"]!["EXEC_SQL"]).Should().Be("antlr-tree:no-sql-contexts");
        RektEvidence.NativeDomainEvidence(exported, "EXEC_CICS").Should().BeFalse();
    }

    [Fact]
    public void LowercaseReturn_RetainsValidPositiveEvidence()
    {
        if (Image is null) return;
        var result = Parse("           exec cics return end-exec.\n");
        result.Code.Should().Be(0, result.Log);
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_CICS").Should().BeTrue();
    }

    [Fact]
    public void EmptyCicsBlock_IsNotMistakenForCicsAbsence()
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS END-EXEC.\n");
        result.Code.Should().NotBe(0);
        result.Exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("EXEC_CICS").Should().BeFalse();
        ((string?)result.Exported["dependencyCoverage"]!["EXEC_CICS"]).Should().NotBe("complete");
        result.Statements.Should().NotBeEmpty();
    }

    [Fact]
    public void CicsLink_RetainsItsGrammarOperands_WithoutInventingResourceBindings()
    {
        if (Image is null) return;
        var result = Parse("           EXEC CICS LINK PROGRAM('PAYHELP')\n               COMMAREA(WS-TARGET) LENGTH(8) END-EXEC.\n");
        result.Code.Should().NotBe(0);
        var statement = Objects(result.Statements).Should().ContainSingle().Subject;
        ((string?)statement["kind"]).Should().Be("EXEC_CICS");
        ((string?)statement["operation"]).Should().Be("LINK");
        ((string)statement["sourceText"]!).Should().Contain("PAYHELP");
        PyJson.Dumps(statement["syntax"]).Should().Contain("WS-TARGET");
        ((string?)statement["bindingStatus"]).Should().Be("unresolved");
        ((string?)result.Exported["dependencyCoverage"]!["EXEC_SQL"]).Should().Be("complete");
        ((string?)result.Exported["dependencyCoverage"]!["EXEC_CICS"]).Should().NotBe("complete");
    }

    private const string TestFileCatalog = "resourceDefinitions:\n  - file:\n      name: TESTFILE\n"
        + "      dsname: TEST.DATA\n      recordsize: 80\n      keylength: 8\n";

    [Fact]
    public void CicsWrite_BindsTheSuppliedFile_AndRetainsRuntimeRequirement()
    {
        if (Image is null) return;
        var result = Parse(
            "           EXEC CICS WRITE FILE('TESTFILE') FROM(WS-TARGET)\n"
            + "               RESP(PAY-AMOUNT) END-EXEC.\n"
            + "           IF PAY-AMOUNT NOT = DFHRESP(NORMAL)\n"
            + "               EXEC CICS RETURN END-EXEC\n           END-IF.\n", cicsResources: TestFileCatalog);
        result.Code.Should().Be(0, result.Log);
        CoverageComplete(result.Exported).Should().BeTrue();
        result.Exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("FILE_IO").Should().BeFalse();
        RektEvidence.NativeDomainEvidence(result.Exported, "FILE_IO").Should().BeTrue();
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_CICS").Should().BeTrue();
        var write = Objects(result.Statements).First(item => (string?)item["operation"] == "WRITE");
        var binding = Objects(write["resourceBindings"]).Where(item => Kind(item) == "CICS_FILE").Should().ContainSingle().Subject;
        ((string?)binding["name"]).Should().Be("TESTFILE");
        ((string?)binding["sourceSha256"]).Should().Be(Digest(Path.Join(Source, "resources.yaml")));
        ((string?)write["runtimeMapping"]).Should().Be("unverified");
    }

    [Fact]
    public void CicsWrite_RejectsMissingDuplicateDynamicAndRemoteFileBindings()
    {
        if (Image is null) return;
        foreach (var (operand, suffix, catalog, expected) in new[]
        {
            ("'MISSING'", "", TestFileCatalog, "cics-file-definition-not-found:MISSING"),
            ("WS-TARGET", "", TestFileCatalog, "dynamic-cics-file-binding-unresolved:WS-TARGET"),
            ("'TESTFILE'", " SYSID(WS-TARGET)", TestFileCatalog, "remote-cics-file-binding-unresolved:"),
            ("'TESTFILE'", "", TestFileCatalog + TestFileCatalog["resourceDefinitions:\n".Length..], "Ambiguous CICS file definition"),
        })
        {
            var result = Parse($"           EXEC CICS WRITE FILE({operand}) FROM(WS-TARGET)\n"
                + $"               RESP(PAY-AMOUNT){suffix} END-EXEC.\n", cicsResources: catalog);
            result.Code.Should().NotBe(0, operand + suffix);
            result.Diagnostics.Should().Contain(issue => issue.Contains(expected), operand + suffix);
            ((string?)result.Exported["dependencyCoverage"]!["FILE_IO"]).Should().NotBe("complete");
            ClearOutputFiles();
        }
    }

    // IDCAMS as shipped with CardDemo: base KSDS clusters plus an alternate-index path.
    private const string Idcams =
        "//PILOTV   JOB 'DEFINE PILOT FILES'\n"
        + "//STEP10   EXEC PGM=IDCAMS\n"
        + "//SYSPRINT DD   SYSOUT=*\n"
        + "//SYSIN    DD   *\n"
        + "   DEFINE CLUSTER (NAME(PILOT.ACCT.KSDS) -\n"
        + "          KEYS(11 0) RECORDSIZE(300 300) INDEXED) -\n"
        + "          DATA (NAME(PILOT.ACCT.KSDS.DATA))\n"
        + "   DEFINE CLUSTER (NAME(PILOT.CARD.KSDS) KEYS(16 0) -\n"
        + "          RECORDSIZE(150 150) INDEXED)\n"
        + "   DEFINE ALTERNATEINDEX (NAME(PILOT.CARD.AIX) -\n"
        + "          RELATE(PILOT.CARD.KSDS) KEYS(11 16) NONUNIQUEKEY)\n"
        + "   DEFINE PATH (NAME(PILOT.CARD.AIX.PATH) PATHENTRY(PILOT.CARD.AIX))\n"
        + "/*\n";

    private const string FileCsd =
        " DEFINE FILE(ACCTDAT) GROUP(PILOT)\n"
        + "        DSNAME(PILOT.ACCT.KSDS) RECORDFORMAT(V) ADD(YES)\n"
        + "        BROWSE(YES) DELETE(YES) READ(YES) UPDATE(YES)\n"
        + " DEFINE FILE(CARDAIX) GROUP(PILOT)\n"
        + "        DSNAME(PILOT.CARD.AIX.PATH) BROWSE(YES) READ(YES)\n";

    private const string FileDeclarations = "       01 WS-REC PIC X(300).\n       01 WS-KEY PIC X(16).\n"
        + "       01 LIT-PGM PIC X(8) VALUE 'PAYHELP'.\n";

    private const string FileBody =
        "           EXEC CICS READ DATASET('ACCTDAT') INTO(WS-REC)\n"
        + "               RIDFLD(WS-KEY) KEYLENGTH(11) UPDATE END-EXEC.\n"
        + "           EXEC CICS REWRITE DATASET('ACCTDAT') FROM(WS-REC) END-EXEC.\n"
        + "           EXEC CICS WRITE DATASET('ACCTDAT') FROM(WS-REC)\n"
        + "               RIDFLD(WS-KEY) END-EXEC.\n"
        + "           EXEC CICS DELETE DATASET('ACCTDAT') END-EXEC.\n"
        + "           EXEC CICS STARTBR FILE('CARDAIX') RIDFLD(WS-KEY)\n"
        + "               GTEQ END-EXEC.\n"
        + "           EXEC CICS READNEXT FILE('CARDAIX') INTO(WS-REC)\n"
        + "               RIDFLD(WS-KEY) END-EXEC.\n"
        + "           EXEC CICS READPREV FILE('CARDAIX') INTO(WS-REC)\n"
        + "               RIDFLD(WS-KEY) END-EXEC.\n"
        + "           EXEC CICS ENDBR FILE('CARDAIX') END-EXEC.\n"
        + "           EXEC CICS XCTL PROGRAM(LIT-PGM) END-EXEC.\n";

    private static string Attributes(JsonObject binding, params string[] keys) =>
        string.Join(',', keys.Select(key => PyJson.Repr(binding["attributes"]![key])));

    [Fact]
    public void CsdFiles_BindSuppliedIdcamsDefinitions_AndXctlConstants()
    {
        if (Image is null) return;
        var result = Parse(FileBody, declarations: FileDeclarations, cicsResources: ConstantCsd + FileCsd,
            cicsResourcesFile: "PILOT.CSD", vsamDefinitions: [Idcams]);
        result.Code.Should().Be(0, result.Why);
        var statements = result.Statements;
        Objects(statements).Select(statement => (string)statement["operation"]!)
            .Should().Equal("READ", "REWRITE", "WRITE", "DELETE", "STARTBR", "READNEXT", "READPREV", "ENDBR", "XCTL");
        AllBound(statements).Should().BeTrue();
        var files = new Dictionary<string, JsonObject>();
        foreach (var binding in Bindings(statements).Where(item => Kind(item) == "CICS_FILE")) files[(string)binding["name"]!] = binding;
        Attributes(files["ACCTDAT"], "recordsize", "keylength", "keyOffset", "accessPath").Should().Be("300,11,0,'base-cluster'");
        Attributes(files["CARDAIX"], "recordsize", "keylength", "keyOffset", "uniqueKeys", "accessPath")
            .Should().Be("150,11,16,False,'alternate-index-path'");
        var program = Objects(statements[^1]!["resourceBindings"]).First(item => Kind(item) == "CICS_PROGRAM");
        ((string?)program["name"]).Should().Be("PAYHELP");
        ((string?)program["controlEffect"]).Should().Be("transfer-control");
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Objects(bound["definitions"]).Where(item => Kind(item) == "CICS_DATASET").Select(item => (string)item["name"]!).ToHashSet()
            .Should().BeEquivalentTo(["PILOT.ACCT.KSDS", "PILOT.CARD.KSDS", "PILOT.CARD.AIX", "PILOT.CARD.AIX.PATH"]);
        foreach (var damage in new[] { "keylength", "service", "dataset-source", "xctl-effect" })
        {
            var document = result.Exported.DeepClone();
            var damaged = document["dependencyDialectStatements"]!;
            JsonObject FileAt(int index) => Objects(damaged[index]!["resourceBindings"]).First(binding => Kind(binding) == "CICS_FILE");
            switch (damage)
            {
                case "keylength": FileAt(0)["attributes"]!["keylength"] = 301; break;
                case "service": FileAt(3)["attributes"]!["services"]!["delete"] = "NO"; break;
                case "dataset-source":
                    var definitions = FileAt(4)["datasetDefinitions"]!.AsArray();
                    definitions.RemoveAt(definitions.Count - 1);
                    break;
                default:
                    Objects(damaged.AsArray()[^1]!["resourceBindings"]).First(binding => Kind(binding) == "CICS_PROGRAM")["controlEffect"] = "call";
                    break;
            }
            AllBound(damaged).Should().BeFalse(damage);
        }
    }

    private static List<string> BindingDiagnostics(Parsed result) =>
        Objects(result.Statements).SelectMany(statement => statement["bindingDiagnostics"] is JsonArray found ? Strings(found) : []).ToList();

    [Fact]
    public void CsdFilesWithoutMatchingIdcamsOrServices_StayUnresolved()
    {
        if (Image is null) return;
        var conflicting = Idcams.Replace("/*\n", "   DEFINE CLUSTER (NAME(PILOT.ACCT.KSDS) -\n"
            + "          KEYS(11 0) RECORDSIZE(200 300))\n/*\n");
        var noDelete = FileCsd.Replace("DELETE(YES)", "DELETE(NO)");
        foreach (var (name, body, csd, idcams, expected) in new[]
        {
            ("no-idcams", "           EXEC CICS ENDBR FILE('CARDAIX') END-EXEC.\n", FileCsd, Array.Empty<string>(),
                "CICS file dataset has no supplied IDCAMS definition"),
            ("conflicting", "           EXEC CICS DELETE DATASET('ACCTDAT') END-EXEC.\n", FileCsd, [conflicting],
                "Conflicting IDCAMS CLUSTER definitions"),
            ("service", "           EXEC CICS DELETE DATASET('ACCTDAT') END-EXEC.\n", noDelete, [Idcams],
                "cics-file-operation-not-permitted:ACCTDAT:delete"),
            ("alternate-write", "           EXEC CICS WRITE FILE('CARDAIX') FROM(WS-REC)\n"
                + "               RIDFLD(WS-KEY) END-EXEC.\n", FileCsd, [Idcams], "cics-file-operation-not-permitted:CARDAIX:add"),
            ("dynamic-xctl", "           MOVE 'OTHER' TO LIT-PGM.\n"
                + "           EXEC CICS XCTL PROGRAM(LIT-PGM) END-EXEC.\n", FileCsd, [Idcams], "cics-program-definition-not-found:OTHER"),
        })
        {
            var result = Parse(body, declarations: FileDeclarations, cicsResources: ConstantCsd + csd,
                cicsResourcesFile: "PILOT.CSD", vsamDefinitions: idcams);
            result.Code.Should().NotBe(0, name);
            BindingDiagnostics(result).Should().Contain(diagnostic => diagnostic.Contains(expected), name);
            RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_CICS").Should().BeFalse(name);
        }
    }

    private const string MenuDeclarations =
        "       01 MENU-OPTIONS.\n"
        + "         05 MENU-DATA.\n"
        + "           10 FILLER PIC 9(02) VALUE 1.\n"
        + "           10 FILLER PIC X(08) VALUE 'PAYHELP'.\n"
        + "           10 FILLER PIC 9(02) VALUE 2.\n"
        + "           10 FILLER PIC X(08) VALUE 'PAYMORE'.\n"
        + "         05 MENU-TABLE REDEFINES MENU-DATA.\n"
        + "           10 MENU-OPT OCCURS 3 TIMES.\n"
        + "             15 MENU-NUM PIC 9(02).\n"
        + "             15 MENU-PGM PIC X(08).\n"
        + "       01 WS-OPTION PIC 9(02) VALUE 1.\n";

    private const string MenuBody =
        "           IF MENU-PGM(WS-OPTION) = 'PAYHELP'\n"
        + "               EXEC CICS INQUIRE PROGRAM(MENU-PGM(WS-OPTION))\n"
        + "                   NOHANDLE END-EXEC\n"
        + "           END-IF.\n"
        + "           EXEC CICS XCTL PROGRAM(MENU-PGM(WS-OPTION)) END-EXEC.\n";

    private const string MenuCsd = " DEFINE PROGRAM(PAYMORE) GROUP(PILOT)\n        RELOAD(NO) STATUS(ENABLED) DYNAMIC(NO)\n";

    private void WritePaymore() => Save(Path.Join(Source, "PAYMORE.cbl"),
        "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PAYMORE.\n       PROCEDURE DIVISION.\n           GOBACK.\n");

    private static List<JsonObject> Programs(JsonNode statement) =>
        Objects(statement["resourceBindings"]).Where(item => Kind(item) == "CICS_PROGRAM").ToList();

    [Fact]
    public void ReadOnlyRedefinedMenuTables_BindEveryDefinedProgram()
    {
        if (Image is null) return;
        WritePaymore();
        var result = Parse(MenuBody, declarations: MenuDeclarations, cicsResources: ConstantCsd + MenuCsd, cicsResourcesFile: "PILOT.CSD");
        result.Code.Should().Be(0, result.Why);
        var statements = Objects(result.Statements);
        statements.Select(statement => (string)statement["operation"]!).Should().Equal("INQUIRE", "XCTL");
        AllBound(result.Statements).Should().BeTrue();
        foreach (var (statement, effect) in statements.Zip(new[] { "program-definition-inquiry", "transfer-control" }))
        {
            var programs = Programs(statement);
            programs.Select(item => (string)item["name"]!).Should().Equal("PAYHELP", "PAYMORE");
            programs.Select(item => (string?)item["controlEffect"]).Distinct().Should().Equal(effect);
            PyJson.PyEquals(programs[0]["targetResolution"]!["occurrenceValues"], J("""["PAYHELP", "PAYMORE", null]""")).Should().BeTrue();
        }
        var bound = Bound();
        Unresolved(bound).Should().BeEmpty();
        Persist(bound);
        CheckTerminalGate(true, "menu");
        foreach (var damage in new[] { "occurrence", "dropped-target", "value-syntax", "subscript" })
        {
            var statement = result.Exported.DeepClone()["dependencyDialectStatements"]!.AsArray()[^1]!;
            var programs = Programs(statement);
            switch (damage)
            {
                case "occurrence":
                    foreach (var item in programs) item["targetResolution"]!["occurrenceValues"]![2] = "PAYHELP";
                    break;
                case "dropped-target": statement["resourceBindings"]!.AsArray().Remove(programs[^1]); break;
                case "value-syntax":
                    foreach (var item in programs)
                    {
                        var syntax = item["targetResolution"]!["valueSyntax"]!.AsArray();
                        syntax.RemoveAt(syntax.Count - 1);
                    }
                    break;
                default:
                    Objects(statement["resourceBindings"]).First(item => Kind(item) == "CICS_DATA").Remove("subscriptSyntax").Should().BeTrue();
                    break;
            }
            RektEvidence.NativeCicsStatementBound(statement).Should().BeFalse(damage);
        }
        foreach (var (name, change) in new[] { ("table-write", "           MOVE SPACES TO MENU-DATA.\n"),
                     ("member-write", "           MOVE 'OTHER' TO MENU-PGM(2).\n") })
        {
            ClearOutputFiles();
            var rejected = Parse(change + MenuBody, declarations: MenuDeclarations, cicsResources: ConstantCsd + MenuCsd, cicsResourcesFile: "PILOT.CSD");
            rejected.Code.Should().NotBe(0, name);
            BindingDiagnostics(rejected).Should().Contain("dynamic-cics-program-binding-unresolved:MENU-PGM(WS-OPTION)", name);
        }
    }

    private const string ReachingDeclarations =
        "       01 WS-COMMAREA.\n"
        + "         05 WS-INFO.\n"
        + "           10 WS-TO-PGM PIC X(08).\n"
        + "           10 WS-FLAG PIC X.\n"
        + "       01 WS-OTHER PIC X(08).\n"
        + "       01 WS-SIGNON PIC X(08) VALUE 'PAYMORE'.\n";

    private const string ReachingBody =
        "           IF WS-FLAG = 'Y'\n"
        + "               MOVE 'PAYHELP' TO WS-TO-PGM\n"
        + "               PERFORM GO-PREV\n"
        + "           END-IF\n"
        + "           MOVE WS-SIGNON TO WS-TO-PGM\n"
        + "           PERFORM GO-PREV\n"
        + "           EXEC CICS RETURN END-EXEC.\n"
        + "       GO-PREV.\n"
        + "           IF WS-TO-PGM = SPACES\n"
        + "               MOVE 'PAYHELP' TO WS-TO-PGM\n"
        + "           END-IF\n"
        + "           EXEC CICS XCTL PROGRAM(WS-TO-PGM)\n"
        + "               COMMAREA(WS-COMMAREA) END-EXEC.\n";

    private const string BeforeReturn = "           PERFORM GO-PREV\n           EXEC CICS RETURN";

    [Fact]
    public void Xctl_BindsEveryStructuredReachingAssignment()
    {
        if (Image is null) return;
        WritePaymore();
        var result = Parse(ReachingBody, declarations: ReachingDeclarations, cicsResources: ConstantCsd + MenuCsd, cicsResourcesFile: "PILOT.CSD");
        result.Code.Should().Be(0, result.Why);
        var statement = result.Statements[^1]!;
        ((string?)statement["operation"]).Should().Be("XCTL");
        RektEvidence.NativeCicsStatementBound(statement).Should().BeTrue();
        var programs = Programs(statement);
        programs.Select(item => (string)item["name"]!).Should().Equal("PAYHELP", "PAYMORE");
        var proof = programs[0]["targetResolution"]!;
        ((string?)proof["kind"]).Should().Be("structured-reaching-assignments");
        PyJson.Truthy(proof["initialReaches"]).Should().BeFalse();
        proof["assignments"]!.AsArray().Should().HaveCount(3);
        Unresolved(Bound()).Should().BeEmpty();
        foreach (var damage in new[] { "values", "assignment", "constant" })
        {
            var damaged = result.Exported.DeepClone()["dependencyDialectStatements"]!.AsArray()[^1]!;
            foreach (var item in Programs(damaged))
            {
                var resolution = item["targetResolution"]!;
                switch (damage)
                {
                    case "values":
                        var reversed = resolution["reachingValues"]!.AsArray().Select(value => value?.DeepClone()).Reverse().ToArray();
                        resolution["reachingValues"] = new JsonArray(reversed);
                        break;
                    case "assignment": resolution["assignments"]!.AsArray().RemoveAt(1); break;
                    default: resolution["assignments"]![1]!.AsObject().Remove("sourceResolution").Should().BeTrue(); break;
                }
            }
            RektEvidence.NativeCicsStatementBound(damaged).Should().BeFalse(damage);
        }
        foreach (var (name, change) in new[] { ("dynamic-source", "           MOVE WS-OTHER TO WS-TO-PGM\n"),
                     ("group-write", "           MOVE WS-OTHER TO WS-INFO\n") })
        {
            ClearOutputFiles();
            var rejected = Parse(ReachingBody.Replace(BeforeReturn, change + BeforeReturn), declarations: ReachingDeclarations,
                cicsResources: ConstantCsd + MenuCsd, cicsResourcesFile: "PILOT.CSD");
            rejected.Code.Should().NotBe(0, name);
            BindingDiagnostics(rejected).Should().Contain("dynamic-cics-program-binding-unresolved:WS-TO-PGM", name);
        }
    }

    [Fact]
    public void Xctl_FallsBackToEverySlotWrite_WhenControlFlowIsUnstructured()
    {
        if (Image is null) return;
        WritePaymore();
        var body = ReachingBody.Replace(BeforeReturn, "           GO TO GO-PREV\n" + BeforeReturn);
        var result = Parse(body, declarations: ReachingDeclarations, cicsResources: ConstantCsd + MenuCsd, cicsResourcesFile: "PILOT.CSD");
        result.Code.Should().Be(0, result.Why);
        var statement = result.Statements[^1]!;
        var programs = Programs(statement);
        programs.Select(item => (string)item["name"]!).ToHashSet().Should().BeEquivalentTo(["PAYHELP", "PAYMORE"]);
        ((string?)programs[0]["targetResolution"]!["kind"]).Should().Be("flow-insensitive-slot-writes");
        RektEvidence.NativeCicsStatementBound(statement).Should().BeTrue();
        foreach (var damage in new[] { "extra-value", "dropped-write" })
        {
            var damaged = result.Exported.DeepClone()["dependencyDialectStatements"]!.AsArray()[^1]!;
            foreach (var item in Programs(damaged))
            {
                if (damage == "extra-value")
                {
                    item["targetResolution"]!["universe"]!.AsArray().Add("OTHER");
                    continue;
                }
                var writes = item["targetResolution"]!["slots"]!["WS-TO-PGM"]!["writes"]!.AsArray();
                writes.Remove(Objects(writes).First(write => (string?)write["value"] == "PAYMORE"));
            }
            RektEvidence.NativeCicsStatementBound(damaged).Should().BeFalse(damage);
        }
    }

    [Fact]
    public void XctlOfAnIncomingCommareaSlot_BindsOnlyThroughTheEstateClosure()
    {
        if (Image is null) return;
        const string declarations = ReachingDeclarations + "       LINKAGE SECTION.\n       01 DFHCOMMAREA PIC X(100).\n";
        const string body = "           MOVE DFHCOMMAREA(1:EIBCALEN) TO WS-COMMAREA\n"
            + "           EXEC CICS XCTL PROGRAM(WS-TO-PGM)\n"
            + "               COMMAREA(WS-COMMAREA) END-EXEC.\n";
        var pending = Parse(body, declarations: declarations, cicsResources: ConstantCsd, cicsResourcesFile: "PILOT.CSD");
        pending.Code.Should().NotBe(0);
        BindingDiagnostics(pending).Should().Contain("cics-commarea-closure-pending:WS-TO-PGM");
        var record = pending.Exported["commareaInventory"]!["records"]![0]!;
        ((string?)record["name"]).Should().Be("WS-COMMAREA");
        PyJson.IsTrue(record["loaded"]).Should().BeTrue();
        PyJson.Truthy(record["slots"]!["WS-TO-PGM"]!["entry"]).Should().BeTrue();
        var closure = new JsonObject
        {
            ["schema"] = 1,
            ["programs"] = new JsonObject
            {
                [(string)pending.Exported["sourceSha256"]!] = O("""{"relativePath": "PILOT.cbl", "slots": {"WS-TO-PGM": [null, "PAYHELP"]}}"""),
            },
        };
        File.WriteAllText(Path.Join(Output, "closure.json"), PyJson.Dumps(closure));
        var result = Parse(body, declarations: declarations, cicsResources: ConstantCsd, cicsResourcesFile: "PILOT.CSD",
            environment: new Dictionary<string, string> { ["REKT_COMMAREA_CLOSURE"] = "/output/closure.json" });
        result.Code.Should().Be(0, result.Why);
        var statement = result.Statements[^1]!;
        var programs = Programs(statement);
        programs.Select(item => (string)item["name"]!).Should().Equal("PAYHELP");
        ((string?)programs[0]["targetResolution"]!["kind"]).Should().Be("closed-world-commarea-slot");
        RektEvidence.NativeCicsStatementBound(statement).Should().BeTrue();
        var tampered = statement.DeepClone();
        foreach (var item in Programs(tampered)) item["targetResolution"]!["slots"]!["WS-TO-PGM"]!["entry"] = false;
        RektEvidence.NativeCicsStatementBound(tampered).Should().BeFalse();
    }

    [Fact]
    public void SqlCursorDeclaration_IsRetainedFromDataDivision()
    {
        if (Image is null) return;
        var result = Parse("           EXEC SQL OPEN TEST-CURSOR END-EXEC.\n",
            declarations: "           EXEC SQL DECLARE TEST-CURSOR CURSOR FOR\n"
                + "               SELECT 1 FROM TESTSCHEMA.UNDECLARED END-EXEC.\n");
        result.Code.Should().NotBe(0);
        Objects(result.Statements).Select(statement => (string)statement["operation"]!).Should().Equal("DECLARE", "OPEN");
        ((string)result.Statements[0]!["sourceText"]!).Should().Contain("TESTSCHEMA.UNDECLARED");
        result.Diagnostics.Should().NotContain(issue => issue.Contains("parser-diagnostic:"));
    }

    [Fact]
    public void Sql_BindsDeclaredTableCursorAndHostVariable_WithoutRuntimeClaim()
    {
        if (Image is null) return;
        var result = Parse(
            "           EXEC SQL OPEN TEST-CURSOR END-EXEC.\n"
            + "           EXEC SQL FETCH TEST-CURSOR INTO :WS-TARGET END-EXEC.\n"
            + "           EXEC SQL CLOSE TEST-CURSOR END-EXEC.\n"
            + "           EXEC SQL COMMIT WORK END-EXEC.\n",
            declarations: "           EXEC SQL DECLARE TESTTAB TABLE\n"
                + "               (COL1 CHAR(8)) END-EXEC.\n"
                + "           EXEC SQL DECLARE TEST-CURSOR CURSOR FOR\n"
                + "               SELECT COL1 FROM TESTTAB\n"
                + "               WHERE COL1 = :WS-TARGET END-EXEC.\n");
        result.Code.Should().Be(0, result.Log);
        var exported = result.Exported;
        CoverageComplete(exported).Should().BeTrue();
        RektEvidence.NativeDomainEvidence(exported, "EXEC_SQL").Should().BeTrue();
        RektEvidence.NativeDomainEvidence(exported, "FILE_IO").Should().BeTrue();
        exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("EXEC_SQL").Should().BeFalse();
        exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("FILE_IO").Should().BeFalse();
        ((string?)exported["dependencyPresenceEvidence"]!["FILE_IO"]).Should().Be("antlr-tree:bound-sql-resources");
        result.Statements.Should().HaveCount(6);
        Objects(result.Statements).Should().OnlyContain(statement => (string?)statement["runtimeMapping"] == "unverified");
        foreach (var change in new[] { "omit-table-declaration", "wrong-binding", "omit-host-reference" })
        {
            var tampered = exported.DeepClone();
            var statements = tampered["dependencyDialectStatements"]!.AsArray();
            switch (change)
            {
                case "omit-table-declaration": statements.RemoveAt(0); break;
                case "wrong-binding": statements[1]!["resourceBindings"]![0]!["name"] = "OTHER"; break;
                default: statements[3]!["resourceReferences"] = new JsonArray(); break;
            }
            RektEvidence.NativeDomainEvidence(tampered, "EXEC_SQL").Should().BeFalse(change);
            RektEvidence.NativeDomainEvidence(tampered, "FILE_IO").Should().BeFalse(change);
        }
    }

    private const string TestTab = "           EXEC SQL DECLARE TESTTAB TABLE\n               (COL1 CHAR(8)) END-EXEC.\n";

    [Fact]
    public void SqlDuplicateTableDeclarations_RemainAmbiguous()
    {
        if (Image is null) return;
        var result = Parse("           EXEC SQL SELECT COL1 INTO :WS-TARGET\n               FROM TESTTAB END-EXEC.\n",
            declarations: TestTab + TestTab);
        result.Code.Should().NotBe(0);
        AllDiagnostics(result).Should().Contain("sql-declaration-ambiguous:SQL_RELATION:TESTTAB");
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeFalse();
    }

    [Fact]
    public void SqlUnknownHostAndImplicitSchema_AreNotSilentlyBound()
    {
        if (Image is null) return;
        var result = Parse("           EXEC SQL SELECT COL1 INTO :MISSING-HOST\n               FROM TESTTAB END-EXEC.\n",
            declarations: "           EXEC SQL DECLARE OWNER.TESTTAB TABLE\n               (COL1 CHAR(8)) END-EXEC.\n");
        result.Code.Should().NotBe(0);
        AllDiagnostics(result).Should().Contain("sql-declaration-not-found:SQL_RELATION:TESTTAB")
            .And.Contain("sql-declaration-not-found:SQL_HOST_VARIABLE:MISSING-HOST");
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeFalse();
    }

    [Fact]
    public void SqlExternalFunction_CannotHideBehindKnownLocalTableAndHost()
    {
        if (Image is null) return;
        var result = Parse("           EXEC SQL SELECT MYFUNC(COL1) INTO :WS-TARGET\n               FROM TESTTAB END-EXEC.\n",
            declarations: TestTab);
        result.Code.Should().NotBe(0);
        result.Diagnostics.Should().Contain(issue => issue.Contains("SQL_FUNCTION:MYFUNC"));
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeFalse();
    }

    [Fact]
    public void SqlBuiltinTypesAndFunctions_AreNotExternalDependencies()
    {
        if (Image is null) return;
        var result = Parse("           EXEC SQL SELECT ABS(ABS) INTO :PAY-AMOUNT\n               FROM TESTTAB END-EXEC.\n",
            declarations: "           EXEC SQL DECLARE TESTTAB TABLE\n"
                + "               (ABS INTEGER, COL2 SMALLINT,\n"
                + "                COL3 DATE) END-EXEC.\n");
        result.Code.Should().Be(0, result.Why);
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeTrue();
        Objects(result.Statements).SelectMany(statement => Objects(statement["resourceReferences"]))
            .Should().NotContain(reference => Kind(reference) == "SQL_TYPE" || Kind(reference) == "SQL_FUNCTION");
    }

    [Theory]
    [InlineData("TIMESTAMP_FORMAT", "           EXEC SQL DECLARE TESTTAB TABLE\n               (COL1 TIMESTAMP) END-EXEC.\n",
        "           EXEC SQL SELECT COL1 INTO :WS-TARGET FROM TESTTAB\n               WHERE COL1 = {0}(:WS-TARGET,\n               'YYYY-MM-DD') END-EXEC.\n",
        "TIMESTAMP_FORMATX", "sql-declaration-not-found:SQL_FUNCTION:TIMESTAMP_FORMATX")]
    [InlineData("SYSIBM.SYSDUMMY1", "           EXEC SQL DECLARE TESTTAB TABLE\n               (COL1 CHAR(8)) END-EXEC.\n",
        "           EXEC SQL SELECT CURRENT DATE INTO :WS-TARGET\n               FROM {0} END-EXEC.\n",
        "SYSDUMMY1", "sql-declaration-not-found:SQL_RELATION:SYSDUMMY1")]
    public void SqlDocumentedDb2Builtins_AreNotExternal_ButLookalikesAre(string builtin, string declarations, string template,
        string lookalike, string diagnostic)
    {
        if (Image is null) return;
        var result = Parse(string.Format(template, builtin), declarations: declarations);
        result.Code.Should().Be(0, result.Why);
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeTrue();
        ClearOutputFiles();
        var rejected = Parse(string.Format(template, lookalike), declarations: declarations);
        rejected.Code.Should().NotBe(0);
        AllDiagnostics(rejected).Should().Contain(diagnostic);
    }

    [Fact]
    public void SqlCustomType_IsNotMistakenForABuiltin()
    {
        if (Image is null) return;
        var result = Parse("           EXEC SQL SELECT COL1 INTO :WS-TARGET\n               FROM TESTTAB END-EXEC.\n",
            declarations: "           EXEC SQL DECLARE TESTTAB TABLE\n               (COL1 MONEY) END-EXEC.\n");
        result.Code.Should().NotBe(0);
        AllDiagnostics(result).Should().Contain("sql-declaration-not-found:SQL_TYPE:MONEY");
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeFalse();
    }

    [Fact]
    public void SqlInvalidQuery_KeepsOriginalParserFailure()
    {
        if (Image is null) return;
        var result = Parse("           EXEC SQL SELECT 1 INTO :WS-TARGET FROM END-EXEC.\n");
        result.Code.Should().NotBe(0);
        PyJson.Truthy(result.Exported["dependencyAnalysisComplete"]).Should().BeFalse();
        result.Diagnostics.Should().Contain(issue => issue.StartsWith("parser-diagnostic:"));
        RektEvidence.NativeDomainEvidence(result.Exported, "EXEC_SQL").Should().BeFalse();
    }

    [Fact]
    public void Sql_RetainsQueryAndHostVariableSyntax_WithoutClaimingCicsPresence()
    {
        if (Image is null) return;
        var result = Parse("           EXEC SQL SELECT 1 INTO :WS-TARGET\n               FROM TESTSCHEMA.UNDECLARED END-EXEC.\n");
        result.Code.Should().NotBe(0);
        var statement = Objects(result.Statements).Should().ContainSingle().Subject;
        ((string?)statement["kind"]).Should().Be("EXEC_SQL");
        PyJson.Dumps(statement["syntax"]).Should().Contain("WS-TARGET").And.Contain("UNDECLARED");
        ((string?)statement["bindingStatus"]).Should().Be("unresolved");
        ((string?)result.Exported["dependencyCoverage"]!["EXEC_CICS"]).Should().Be("complete");
        ((string?)result.Exported["dependencyAbsenceEvidence"]!["EXEC_CICS"]).Should().Be("antlr-tree:no-cics-contexts");
        ((string?)result.Exported["dependencyCoverage"]!["EXEC_SQL"]).Should().NotBe("complete");
    }

    [Fact]
    public void MixedDialectsAndRepeatedReturns_AreNotLostOrDeduplicatedByText()
    {
        if (Image is null) return;
        var result = Parse(
            "           EXEC CICS RETURN END-EXEC.\n"
            + "           EXEC SQL SELECT 1 INTO :WS-TARGET\n"
            + "               FROM TESTSCHEMA.UNDECLARED END-EXEC.\n"
            + "           EXEC CICS RETURN END-EXEC.\n");
        result.Code.Should().NotBe(0);
        var statements = Objects(result.Statements);
        statements.Select(Kind).Should().Equal("EXEC_CICS", "EXEC_SQL", "EXEC_CICS");
        PyJson.PyEquals(statements[0]["line"], statements[2]["line"]).Should().BeFalse();
        result.Exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("EXEC_SQL").Should().BeFalse();
        result.Exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("EXEC_CICS").Should().BeFalse();
    }

    [Fact]
    public void ResourceFreeReturnInCallee_KeepsDependencyCoverageComplete()
    {
        if (Image is null) return;
        var result = Parse("           CALL 'PAYHELP' USING PAY-AMOUNT.\n", calleeDialect: "return");
        result.Code.Should().Be(0, result.Log);
        var child = Objects(result.Exported["dependencies"]).First(node => (string?)node["dependencyKind"] == "CALL");
        RektEvidence.NativeDomainEvidence(child, "EXEC_CICS").Should().BeTrue();
        PyJson.Truthy(result.Exported["dependencyAnalysisComplete"]).Should().BeTrue();
    }

    [Fact]
    public void CicsReturn_DoesNotHideRealFileOperations()
    {
        if (Image is null) return;
        var result = Parse("           OPEN INPUT INPUT-FILE.\n           CLOSE INPUT-FILE.\n           EXEC CICS RETURN END-EXEC.\n", fileIo: true);
        result.Code.Should().Be(0, result.Log);
        result.Exported["dependencyAbsenceEvidence"]!.AsObject().ContainsKey("FILE_IO").Should().BeFalse();
        result.Exported["dependencyControlTransfers"]!.AsArray().Should().HaveCount(1);
        Objects(result.Statements).SelectMany(statement => statement["fileOperands"] is JsonArray operands ? Strings(operands) : [])
            .Should().Equal("INPUT-FILE", "INPUT-FILE");
        RektEvidence.NativeDomainEvidence(result.Exported, "FILE_IO").Should().BeTrue();
    }

    [Fact]
    public void MissingCallee_PropagatesIncompleteStatusToRoot()
    {
        if (Image is null) return;
        var result = Parse("           CALL 'PAYHELP' USING PAY-AMOUNT.\n", callee: false);
        result.Code.Should().NotBe(0);
        PyJson.Truthy(result.Exported["dependencyAnalysisComplete"]).Should().BeFalse();
        ((string?)result.Exported["dependencyCoverage"]!["CALL"]).Should().Be("partial");
        result.Diagnostics.Should().Contain(item => item.Contains("incomplete-callee:PAYHELP"));
    }

    [Fact]
    public void UnsupportedCalleeDomain_CannotBeHiddenByCleanParent()
    {
        if (Image is null) return;
        var result = Parse("           CALL 'PAYHELP' USING PAY-AMOUNT.\n", calleeDialect: "options");
        result.Code.Should().NotBe(0);
        PyJson.Truthy(result.Exported["dependencyAnalysisComplete"]).Should().BeFalse();
        ((string?)result.Exported["dependencyCoverage"]!["EXEC_CICS"]).Should().NotBe("complete");
        result.Diagnostics.Should().Contain(item => item.Contains("callee:PAYHELP:"));
    }
}
