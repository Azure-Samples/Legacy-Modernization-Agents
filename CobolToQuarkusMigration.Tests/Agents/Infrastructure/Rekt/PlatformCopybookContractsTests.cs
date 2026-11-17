using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Rekt;

// A platform copybook may only be replaced by an explicitly selected, reviewed symbolic contract, never by guessed source bytes.
[Collection(RektEvidenceCollection.Name)]
public sealed class PlatformCopybookContractsTests : RektEvidenceFixture
{
    private const string PlatformVariable = "REKT_PLATFORM_CONTRACTS";

    private static JsonObject Profile() => new()
    {
        ["schemaVersion"] = 1, ["profileKind"] = "platform-replacement", ["profileName"] = "test-terminal-events",
        ["copybooks"] = new JsonObject
        {
            ["DFHAID"] = new JsonObject
            {
                ["recipe"] = "symbolic-terminal-events-v1", ["selector"] = "EIBAID",
                ["symbols"] = new JsonObject(PlatformCopybookContracts.Symbols.Select(pair => KeyValuePair.Create(pair.Key, (JsonNode?)pair.Value))),
                ["documentation"] = new JsonArray(PlatformCopybookContracts.Documentation),
                ["targetSemantics"] = "named-events-preserve-source-branches",
            },
        },
    };

    private static void Generate(string source, string output, string? configured) =>
        PlatformCopybookContracts.Generate(source, output, configured, TextWriter.Null);

    private static bool Uncommented(string line) => line.Length <= 6 || line[6] != '*';

    [Fact]
    public void ExplicitOptIn_AndGenuineSuppliedDefinitionPrecedence()
    {
        var output = Path.Join(Source, ".preprocessed");
        Directory.CreateDirectory(output);
        var configured = Path.Join(Work, "platform.json");
        Save(configured, Profile());
        Generate(Source, output, null);
        var projected = Path.Join(output, "DFHAID.cpy");
        File.Exists(projected).Should().BeFalse();
        Generate(Source, output, configured);
        RektEvidence.CurrentTransform(projected).Should().Be(PyCompat.Resolve(configured));
        File.ReadAllLines(projected).Where(Uncommented).Should().NotContain(line => line.Contains(" VALUE "));
        File.Delete(projected);
        Write("sdk/DFHAID.cpy", "       01 ENTITLED-TEST-DEFINITION PIC X.\n");
        Generate(Source, output, configured);
        File.Exists(projected).Should().BeFalse();
    }

    [Theory]
    [InlineData("selector", "\"ACCOUNT\"")]
    [InlineData("symbols", """{"DFHENTER": 125}""")]
    [InlineData("targetSemantics", "\"ENTER-CREATES-ACCOUNT\"")]
    [InlineData("documentation", """["https://example.test/guessed"]""")]
    public void Rejects_SourceBytesUnknownSymbolsSelectorsAndBusinessActions(string field, string value)
    {
        var profile = Profile();
        profile["copybooks"]!["DFHAID"]![field] = J(value);
        var act = () => PlatformCopybookContracts.Validate(profile);
        act.Should().Throw<PyError>().Which.Kind.Should().Be("ValueError");
    }

    [Fact]
    public void BmsProjection_RequiresExplicitReviewedRecipeAndNeverUsesGeneratedConstants()
    {
        var profile = Profile();
        profile["copybooks"]!["DFHBMSCA"] = PlatformCopybookContracts.BmsContract();
        var configured = Path.Join(Work, "platform.json");
        Save(configured, profile);
        var output = Path.Join(Source, ".preprocessed");
        Directory.CreateDirectory(output);
        Write("copy-generated/DFHBMSCA.cpy", "       01 DFHBMSCA.\n           02 DFHBMASK PIC X VALUE X'00'.\n");
        Generate(Source, output, configured);
        var projected = Text(Path.Join(output, "DFHBMSCA.cpy"));
        projected.Should().Contain("02 DFHBMASK PIC X.");
        projected.Split('\n').Where(Uncommented).Should().NotContain(line => line.Contains(" VALUE "));
        profile["copybooks"]!["DFHBMSCA"]!["symbols"]!["DFHBMASK"] = "A:UNPROTECTED";
        var act = () => PlatformCopybookContracts.Validate(profile);
        act.Should().Throw<PyError>().Which.Kind.Should().Be("ValueError");
    }

    private (string Configured, JsonObject Profile, string Staging, JsonObject Node) Fixture()
    {
        File.WriteAllText(ProgramPath,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROG.\n"
            + "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       COPY DFHAID.\n"
            + "       PROCEDURE DIVISION.\n           IF EIBAID = DFHENTER\n"
            + "               CONTINUE\n           END-IF.\n           GOBACK.\n");
        // Preprocessing snapshots the profile next to the estate's projections.
        var output = Path.Join(Source, ".preprocessed");
        Directory.CreateDirectory(output);
        var configured = Path.Join(output, "platform-contracts.json");
        var profile = Profile();
        Save(configured, profile);
        Generate(Source, output, configured);
        var staging = Path.Join(Source, ".rekt-staging");
        StageProgram(staging);
        Stage(Path.Join(output, "DFHAID.cpy"), Path.Join(staging, "DFHAID.cpy"), derived: true);
        var binding = new JsonObject
        {
            ["name"] = "DFHAID", ["profileName"] = profile["profileName"]!.DeepClone(), ["profileKind"] = "platform-replacement",
            ["sourcePath"] = configured, ["sourceSha256"] = Digest(configured),
            ["outputPath"] = "/source/.rekt-staging/DFHAID.cpy", ["outputSha256"] = Digest(Path.Join(staging, "DFHAID.cpy")),
            ["definition"] = profile["copybooks"]!["DFHAID"]!.DeepClone(), ["resolutionKind"] = "symbolic-platform-replacement",
            ["runtimeMapping"] = "implementation-required", ["sourceValues"] = "not-supplied", ["bindingStatus"] = "complete",
            ["usages"] = J("""
                [{"grammarRule": "ConditionContext",
                  "syntax": {"rule": "ConditionContext", "children": [
                     {"rule": "SimpleConditionContext", "children": [{"token": "EIBAID"}, {"token": "="}, {"token": "DFHENTER"}]}]},
                  "sourceText": "EIBAID = DFHENTER", "line": 8, "column": 14, "coordinateSpace": "expanded-parser-input"}]
                """),
        };
        var node = new JsonObject
        {
            ["dependencyKind"] = "PLATFORM_COPY", ["callTarget"] = O("""{"name": "DFHAID", "programReferenceType": "STATIC"}"""),
            ["runtimeContractBindingComplete"] = true, ["dependencyAnalysisComplete"] = false, ["runtimeContract"] = binding,
            ["dependencies"] = new JsonArray(),
        };
        var document = new JsonObject
        {
            ["dependencyExportSchemaVersion"] = 1, ["callTarget"] = O("""{"name": "PROG.cbl", "programReferenceType": "STATIC"}"""),
            ["sourcePath"] = "/source/.rekt-staging/.parser-input/" + Relative + "/PROG.cbl",
            ["sourceSha256"] = Digest(ProgramPath),
            ["dependencyAnalysisComplete"] = true, ["dependencyDiagnostics"] = new JsonArray(),
            ["dependencyCoverage"] = AllKinds("complete"),
            ["dependencyAbsenceEvidence"] = AbsenceProofs(), ["dependencies"] = new JsonArray(node.DeepClone()),
        };
        Save(ExportPath, document);
        return (configured, profile, staging, node);
    }

    private void StageProgram(string staging)
    {
        Stage(ProgramPath, Path.Join(staging, Relative));
        Stage(ProgramPath, Path.Join(staging, ".parser-input", Relative, "PROG.cbl"));
    }

    [Fact]
    public void RecordsDistinctReplacementNotVendorCopy_AndRejectsTampering()
    {
        var (configured, profile, staging, node) = Fixture();
        WithEnvironment(PlatformVariable, configured, () =>
        {
            var result = Build(staging: staging);
            Strings(result["unresolvedDiagnostics"]).Should().BeEmpty();
            Objects(result["definitions"]).Select(item => (string?)item["kind"]).Distinct()
                .Should().BeEquivalentTo("PLATFORM_COPY", "PLATFORM_INPUT");
            WriteReport(result);
            Current().Should().BeTrue();
            foreach (var name in new[] { "source", "projection", "profile" })
            {
                var damaged = node.DeepClone();
                if (name == "source") damaged["sourcePath"] = ProgramPath;
                else if (name == "projection") damaged["runtimeContract"]!["outputSha256"] = new string('0', 64);
                else damaged["runtimeContract"]!["definition"]!["symbols"]!["DFHENTER"] = "CREATE-CUSTOMER";
                RektEvidence.NativePlatformContractBound(damaged, profile).Should().BeFalse(name);
            }
            File.WriteAllText(Path.Join(staging, "DFHAID.cpy"), "       01 DFHAID PIC X VALUE SPACE.\n");
            Strings(Build(staging: staging)["unresolvedDiagnostics"])
                .Should().Contain(error => error.Contains("native-platform-contract-unverified"));
            return 0;
        });
    }

    [Theory]
    [InlineData("none")]
    [InlineData("whole condition")]
    [InlineData("unknown role")]
    [InlineData("two events")]
    [InlineData("application tokens")]
    public void ApplicationAndConjunctUsage_IsBounded(string damage)
    {
        var (_, profile, _, node) = Fixture();
        var usage = node["runtimeContract"]!["usages"]![0]!.AsObject();
        var conjunct = usage.DeepClone().AsObject();
        conjunct["grammarRule"] = "SimpleConditionContext";
        conjunct["conditionRole"] = "application-and-conjunct";
        conjunct["syntax"] = usage["syntax"]!["children"]![0]!.DeepClone();
        switch (damage)
        {
            case "whole condition": conjunct["syntax"] = usage["syntax"]!.DeepClone(); break;
            case "unknown role": conjunct["conditionRole"] = "application-or-conjunct"; break;
            case "two events":
                conjunct["syntax"] = J("""
                    {"rule": "SimpleConditionContext", "children": [{"token": "EIBAID"}, {"token": "="}, {"token": "DFHENTER"}, {"token": "DFHPF3"}]}
                    """);
                break;
            case "application tokens":
                conjunct["syntax"] = J("""
                    {"rule": "SimpleConditionContext", "children": [{"token": "WS-FLAG"}, {"token": "="}, {"token": "DFHENTER"}]}
                    """);
                break;
        }
        node["runtimeContract"]!["usages"] = new JsonArray(conjunct);
        RektEvidence.NativePlatformContractBound(node, profile).Should().Be(damage == "none");
    }

    private static JsonObject EvaluateUsage(string role, string rule, string text, params string[] tokens) => new()
    {
        ["grammarRule"] = rule, ["conditionRole"] = role, ["sourceText"] = text, ["line"] = 5, ["column"] = 12,
        ["coordinateSpace"] = "expanded-parser-input",
        ["syntax"] = new JsonObject
        {
            ["rule"] = rule, ["children"] = new JsonArray(tokens.Select(token => (JsonNode?)new JsonObject { ["token"] = token }).ToArray()),
        },
    };

    [Theory]
    [InlineData("none")]
    [InlineData("subject without when")]
    [InlineData("when without subject")]
    [InlineData("application subject")]
    [InlineData("unknown event")]
    [InlineData("two events")]
    [InlineData("wrong rule")]
    public void EvaluateEibaidUsages_AreOrderedAndBounded(string damage)
    {
        var (_, profile, _, node) = Fixture();
        var subject = EvaluateUsage("evaluate-subject", "EvaluateSelectContext", "EIBAID", "EIBAID");
        var enter = EvaluateUsage("evaluate-when", "EvaluateWhenContext", "WHEN DFHENTER", "WHEN", "DFHENTER");
        var pf3 = EvaluateUsage("evaluate-when", "EvaluateWhenContext", "WHEN DFHPF3", "WHEN", "DFHPF3");
        JsonObject[] usages = damage switch
        {
            "subject without when" => [subject],
            "when without subject" => [enter, pf3],
            "application subject" => [With(subject, "syntax", J("""{"rule": "EvaluateSelectContext", "children": [{"token": "WS-KEY"}]}""")), enter],
            "unknown event" => [subject, EvaluateUsage("evaluate-when", "EvaluateWhenContext", "WHEN DFHPF99", "WHEN", "DFHPF99")],
            "two events" => [subject, EvaluateUsage("evaluate-when", "EvaluateWhenContext", "WHEN DFHPF3 DFHPF4", "WHEN", "DFHPF3", "DFHPF4")],
            "wrong rule" => [subject, With(enter, "grammarRule", "EvaluateSelectContext")],
            _ => [subject, enter, pf3],
        };
        node["runtimeContract"]!["usages"] = new JsonArray(usages.Select(item => (JsonNode?)item.DeepClone()).ToArray());
        RektEvidence.NativePlatformContractBound(node, profile).Should().Be(damage == "none");
    }

    private static JsonObject With(JsonObject item, string key, JsonNode? value)
    {
        var copy = item.DeepClone().AsObject();
        copy[key] = value;
        return copy;
    }

    private static JsonObject Tokens(string rule, IEnumerable<string> tokens) => new()
    {
        ["rule"] = rule, ["children"] = new JsonArray(tokens.Select(token => (JsonNode?)new JsonObject { ["token"] = token }).ToArray()),
    };

    [Theory]
    [InlineData("none")]
    [InlineData("missing binding")]
    [InlineData("foreign group")]
    [InlineData("class mismatch")]
    [InlineData("single shape for qualified")]
    [InlineData("both shapes")]
    public void QualifiedMultiTargetBmsMove_BindsEveryTarget(string damage)
    {
        static JsonObject Field(string name, string suffix) => new()
        {
            ["name"] = name + suffix, ["field"] = name, ["suffix"] = suffix, ["map"] = "CACTVWA", ["mapset"] = "COACTVW",
            ["layoutPath"] = "x.bms-layout.json", ["layoutSha256"] = "a", ["outputPath"] = "x", ["outputSha256"] = "b",
        };
        var declaration = O("""{"rule": "DataDescriptionEntryFormat1Context", "children": []}""");
        string[] tokens = ["MOVE", "DFHBMDAR", "TO", "ACCTSIDA", "OF", "CACTVWAI", "CUSTIDA", "OF", "CACTVWAI"];
        var usage = new JsonObject
        {
            ["grammarRule"] = "MoveStatementContext", ["coordinateSpace"] = "expanded-parser-input", ["line"] = 9,
            ["sourceText"] = string.Join(' ', tokens), ["valueKind"] = "named-attribute",
            ["syntax"] = Tokens("MoveStatementContext", tokens),
            ["fieldBindings"] = new JsonArray(Field("ACCTSID", "A"), Field("CUSTID", "A")),
            ["definitionSyntaxes"] = new JsonArray(declaration.DeepClone(), declaration.DeepClone()),
        };
        var damaged = usage.DeepClone().AsObject();
        switch (damage)
        {
            case "missing binding":
                damaged["fieldBindings"] = new JsonArray(Field("ACCTSID", "A"));
                damaged["definitionSyntaxes"] = new JsonArray(declaration.DeepClone());
                break;
            case "foreign group":
                damaged["sourceText"] = ReplaceFirst(string.Join(' ', tokens), "CACTVWAI", "OTHERI");
                damaged["syntax"] = Tokens("MoveStatementContext", tokens.Select((token, index) => index == 5 ? "OTHERI" : token));
                break;
            case "class mismatch":
                damaged["sourceText"] = string.Join(' ', tokens).Replace("DFHBMDAR", "DFHRED");
                damaged["syntax"] = Tokens("MoveStatementContext", tokens.Select((token, index) => index == 1 ? "DFHRED" : token));
                break;
            case "single shape for qualified":
                damaged.Remove("fieldBindings");
                damaged.Remove("definitionSyntaxes");
                damaged["fieldBinding"] = Field("ACCTSID", "A");
                damaged["definitionSyntax"] = declaration.DeepClone();
                break;
            case "both shapes":
                damaged["fieldBinding"] = Field("ACCTSID", "A");
                damaged["definitionSyntax"] = declaration.DeepClone();
                break;
        }
        PlatformCopybookContracts.BmsUsageBound(damaged).Should().Be(damage == "none");
    }

    private static string ReplaceFirst(string text, string old, string replacement)
    {
        var index = text.IndexOf(old, StringComparison.Ordinal);
        return text[..index] + replacement + text[(index + old.Length)..];
    }

    [Theory]
    [InlineData("none")]
    [InlineData("shifted occurrence")]
    [InlineData("layout changed")]
    [InlineData("unsupported member")]
    [InlineData("attribute class")]
    public void BmsOverlayTableMove_BindsEachOccurrenceToTheProvedOffset(string damage)
    {
        static JsonObject Entry(params string[] tokens) => Tokens("DataDescriptionEntryFormat1Context", tokens);
        var record = Entry("01", "FIL-R005", "REDEFINES", "MAPAI", ".");
        JsonObject[] members =
        [
            Entry("05", "FILLER", "PIC", "X(12)", "."), Entry("05", "ROWS", "OCCURS", "3", "TIMES", "."),
            Entry("10", "SELL", "PIC", "S9(4)", "COMP", "."), Entry("10", "SELF", "PIC", "X", "."),
            Entry("10", "FIL-R006", "REDEFINES", "SELF", "."), Entry("15", "SELA", "PIC", "X", "."),
            Entry("10", "SELI", "PIC", "X(2)", "."),
        ];
        static List<string> Words(JsonObject entry) => entry["children"]!.AsArray().Select(child => (string)child!["token"]!).ToList();
        DependencyEvidenceGate.BmsOverlayLayout(Words(record), members.Select(Words).ToList(), 5)
            .Should().Be(("MAPAI", 14, 5, 3));
        string[] tokens = ["MOVE", "DFHBMPRO", "TO", "SELA", "(", "I", ")"];
        var binding = new JsonObject
        {
            ["name"] = "SELA", ["subscript"] = "I", ["map"] = "MAPA", ["mapset"] = "MAPS", ["suffix"] = "A",
            ["layoutPath"] = "x.bms-layout.json", ["layoutSha256"] = "a", ["outputPath"] = "x", ["outputSha256"] = "b",
            ["recordSyntax"] = record, ["memberSyntaxes"] = new JsonArray(members.Select(item => (JsonNode?)item.DeepClone()).ToArray()),
            ["definitionSyntax"] = members[5].DeepClone(),
            ["occurrences"] = new JsonArray(Enumerable.Range(0, 3)
                .Select(row => (JsonNode?)new JsonObject { ["field"] = $"SEL{row}", ["offset"] = 14 + 5 * row }).ToArray()),
        };
        var usage = new JsonObject
        {
            ["grammarRule"] = "MoveStatementContext", ["coordinateSpace"] = "expanded-parser-input", ["line"] = 9,
            ["sourceText"] = string.Join(' ', tokens), ["valueKind"] = "named-attribute",
            ["syntax"] = Tokens("MoveStatementContext", tokens), ["overlayBindings"] = new JsonArray(binding),
        };
        switch (damage)
        {
            case "shifted occurrence": binding["occurrences"]![1]!["offset"] = 20; break;
            case "layout changed": binding["memberSyntaxes"]![0] = Entry("05", "FILLER", "PIC", "X(13)", "."); break;
            case "unsupported member": binding["memberSyntaxes"]![6] = Entry("10", "SELI", "PIC", "9(3)", "COMP-3", "."); break;
            case "attribute class":
                usage["syntax"]!["children"]![1]!["token"] = "DFHRED";
                usage["sourceText"] = ((string)usage["sourceText"]!).Replace("DFHBMPRO", "DFHRED");
                break;
        }
        PlatformCopybookContracts.BmsUsageBound(usage).Should().Be(damage == "none");
    }

    [Fact]
    public void UnselectedContractAndMissingBusinessCopy_StillBlock()
    {
        var (configured, _, staging, _) = Fixture();
        WithEnvironment(PlatformVariable, "", () => Strings(Build(staging: staging)["unresolvedDiagnostics"])).Should().NotBeEmpty();
        File.AppendAllText(ProgramPath, "       COPY BUSINESS.\n");
        StageProgram(staging);
        var document = J(Text(ExportPath));
        document["sourceSha256"] = Digest(ProgramPath);
        Save(ExportPath, document);
        WithEnvironment(PlatformVariable, configured, () => Strings(Build(staging: staging)["unresolvedDiagnostics"]))
            .Should().Contain("copy-not-exported:BUSINESS");
    }

    [Fact]
    public void BmsSourceUses_IncludeHashBoundProcedureCopybooksWithTagReplacing()
    {
        var layout = Path.Join(Work, "MAP.json");
        Save(layout, """{"maps": [{"fields": [{"name": "STATUS"}]}]}""");
        var book = Path.Join(Work, "SETATTR.cpy");
        Save(book, "           IF (VAR1) = SPACES\n               MOVE DFHRED TO (SCRN)C OF MAPO\n           END-IF.\n");
        var data = Path.Join(Work, "DATA.cpy");
        Save(data, "       01 DATA-ONLY.\n           05 OTHERC PIC X.\n");
        const string source =
            "000100 DATA DIVISION.                                                   00010000\n"
            + "000200     COPY DATA.                                                   00020000\n"
            + "000300 PROCEDURE DIVISION.                                              00030000\n"
            + "000400     COPY SETATTR REPLACING                                       00040000\n"
            + "000500       ==(VAR1)== BY ==WS-STATUS==                                00050000\n"
            + "000600       ==(SCRN)== BY ==STATUS== .                                 00060000\n";
        List<JsonNode?> Definitions(string? tampered = null) =>
        [
            new JsonObject { ["kind"] = "BMS_LAYOUT", ["name"] = "MAP", ["path"] = layout, ["sha256"] = Digest(layout) },
            new JsonObject { ["kind"] = "COPY", ["name"] = "SETATTR.cpy", ["path"] = book, ["sha256"] = tampered ?? Digest(book) },
            new JsonObject { ["kind"] = "COPY", ["name"] = "DATA.cpy", ["path"] = data, ["sha256"] = Digest(data) },
        ];
        var binding = J("""{"usages": [{"sourceText": "MOVE DFHRED TO STATUSC OF MAPO"}]}""");
        PlatformCopybookContracts.BmsSourceUsesBound(binding, source, Definitions()).Should().BeTrue();
        PlatformCopybookContracts.BmsSourceUsesBound(J("""{"usages": []}"""), source, Definitions()).Should().BeFalse();
        PlatformCopybookContracts.BmsSourceUsesBound(binding, source, Definitions(new string('0', 64))).Should().BeFalse();
        PlatformCopybookContracts.BmsSourceUsesBound(binding, source,
            Definitions().Where(item => (string?)item!["name"] != "SETATTR.cpy").ToList()).Should().BeFalse();
        foreach (var unsupported in new[] { "COPY SETATTR OF LIB REPLACING", "COPY SETATTR REPLACING ==(VAR1) X== BY ==Y== ==(Z)== BY" })
            PlatformCopybookContracts.BmsSourceUsesBound(binding, source.Replace("COPY SETATTR REPLACING", unsupported), Definitions())
                .Should().BeFalse(unsupported);
    }

    // The two tests below run the real native parser; set REKT_NATIVE_PLATFORM_IMAGE to a cobol-rekt image to enable them.
    private static string? NativeImage => Environment.GetEnvironmentVariable("REKT_NATIVE_PLATFORM_IMAGE") is { Length: > 0 } image ? image : null;

    private static string? GateAssembly => Environment.GetEnvironmentVariable("REKT_PLATFORM_GATE_ASSEMBLY") is { Length: > 0 } gate ? gate : null;

    private List<string> PlanStatuses(string target)
    {
        var (code, output) = Run("dotnet", [GateAssembly!, "dependency-plan", "--source", Source, "--facts", Output, "--output", target], 60);
        code.Should().Be(0, output);
        return Objects(PyJson.ReadFile(Path.Join(target, "draft-conversion-plan.json"))!["programs"])
            .Select(program => (string)program["status"]!).ToList();
    }

    [Fact]
    public void NativeBmsAttributes_PreserveFieldBindingsAndLiteralAssignments()
    {
        if (NativeImage is not { } image) return;
        File.WriteAllText(ProgramPath,
            "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. PROG.\n"
            + "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n"
            + "       COPY SCREEN.\n       COPY DFHBMSCA.\n"
            + "       PROCEDURE DIVISION.\n"
            + "           MOVE DFHBMASK TO FIELD1A.\n"
            + "           MOVE DFHGREEN TO FIELD1C.\n"
            + "           MOVE DFHUNDLN TO FIELD1H.\n"
            + "           MOVE 'A' TO FIELD1A.\n"
            + "           MOVE HIGH-VALUES TO FIELD1H.\n"
            + "           MOVE DFHBMPRF TO FIELD1A.\n"
            + "           MOVE DFHBMFSE TO FIELD1A.\n"
            + "           MOVE DFHNEUTR TO FIELD1C.\n"
            + "           GOBACK.\n");
        Write("SCREEN.bms",
            "SCREEN   DFHMSD TYPE=DSECT,LANG=COBOL,MODE=INOUT,".PadRight(71) + "X\n"
            + "               DSATTS=(COLOR,HILIGHT),MAPATTS=(COLOR,HILIGHT)\n"
            + "PANEL    DFHMDI SIZE=(24,80)\n"
            + "FIELD1   DFHMDF POS=(1,1),LENGTH=4\n"
            + "         DFHMSD TYPE=FINAL\n         END\n");
        var generated = Path.Join(Source, ".preprocessed");
        Directory.CreateDirectory(generated);
        // Same layout as doctor.sh: the profile sits beside its transformed copybooks.
        var configured = Path.Join(generated, "platform-contracts.json");
        var profile = Profile();
        profile["copybooks"]!["DFHBMSCA"] = PlatformCopybookContracts.BmsContract();
        Save(configured, profile);
        BmsSymbolicMap.Generate(Source, generated).Warnings.Should().BeEmpty();
        Generate(Source, generated, configured);
        var staging = Path.Join(Source, ".rekt-staging");
        StageProgram(staging);
        foreach (var name in new[] { "SCREEN", "DFHBMSCA" })
            Stage(Path.Join(generated, name + ".cpy"), Path.Join(staging, name + ".cpy"), derived: true);
        string[] command =
        [
            "run", "--rm", "--memory=1g", "--network", "none", "--entrypoint", "java",
            "-e", "JAVA_TOOL_OPTIONS=-Xmx512m -XX:-UsePerfData",
            "-v", $"{Source}:/source:ro", "-v", $"{Output}:/output",
            "-v", $"{configured}:{configured}:ro", "-e", $"{PlatformVariable}={configured}",
            image, "-jar", "/app/smojol-cli.jar",
            "dependency", "PROG.cbl", "--srcDir=/source/.rekt-staging/.parser-input/estate/PROG.cbl",
            "--copyBooksDir=/source/.rekt-staging", "--dialectJarPath=/app/dialect-idms.jar",
            $"--export=/output/{Relative}-deps.json",
        ];
        var (code, log) = Run("docker", command);
        var document = J(Text(ExportPath));
        code.Should().Be(0, log);
        var leaf = Objects(document["dependencies"]).First(node => (string?)node["dependencyKind"] == "PLATFORM_COPY");
        RektEvidence.NativePlatformContractBound(leaf, profile).Should().BeTrue();
        var usages = Objects(leaf["runtimeContract"]!["usages"]);
        usages.Should().HaveCount(8);
        usages.Select(usage => (string)usage["valueKind"]!).Distinct()
            .Should().BeEquivalentTo("named-attribute", "source-character-literal", "figurative-constant");
        var bound = WithEnvironment(PlatformVariable, configured, () => Build(staging: staging));
        Strings(bound["unresolvedDiagnostics"]).Should().BeEmpty();
        WriteReport(bound);
        Current().Should().BeTrue();
        if (GateAssembly is not null) PlanStatuses(Path.Join(Work, "bms-gate-valid")).Should().Equal("eligible");
        foreach (var damage in new[] { "mapset", "field", "literal", "omitted-use" })
        {
            var forged = document.DeepClone();
            var contract = Objects(forged["dependencies"]).First(node => (string?)node["dependencyKind"] == "PLATFORM_COPY")["runtimeContract"]!;
            switch (damage)
            {
                case "mapset": contract["usages"]![0]!["fieldBinding"]!["mapset"] = "OTHER"; break;
                case "field": contract["usages"]![0]!["fieldBinding"]!["field"] = "OTHER"; break;
                case "literal": contract["usages"]![3]!["sourceText"] = "MOVE 'B' TO FIELD1A"; break;
                default: contract["usages"]!.AsArray().RemoveAt(contract["usages"]!.AsArray().Count - 1); break;
            }
            Save(ExportPath, forged);
            var stale = bound.DeepClone();
            stale["exportSha256"] = Digest(ExportPath);
            WriteReport(stale);
            Current().Should().BeFalse(damage);
            if (GateAssembly is not null) PlanStatuses(Path.Join(Work, $"bms-gate-{damage}")).Should().Equal("blocked");
        }
        var original = Text(ProgramPath);
        foreach (var value in new[] { "DFHGREEN", "'B'" })
        {
            File.WriteAllText(ProgramPath, original.Replace("MOVE DFHBMASK TO FIELD1A", $"MOVE {value} TO FIELD1A"));
            StageProgram(staging);
            Run("docker", command).ExitCode.Should().NotBe(0);
            Strings(J(Text(ExportPath))["dependencyDiagnostics"])
                .Should().Contain(issue => issue.Contains("platform-contract-unsupported-bms-value:"));
        }
    }

    [Theory]
    [InlineData("           IF EIBAID = DFHENTER CONTINUE END-IF.\n", true)]
    [InlineData("           IF EIBAID = DFHPF12 CONTINUE END-IF.\n", true)]
    [InlineData("           IF EIBAID = DFHENTER AND WS-FLAG NOT = 'Y'\n               CONTINUE END-IF.\n", true)]
    [InlineData("           IF EIBAID = DFHENTER OR WS-FLAG = 'Y'\n               CONTINUE END-IF.\n", false)]
    [InlineData("           IF WS-FLAG = 'Y' AND DFHPF3 CONTINUE END-IF.\n", false)]
    [InlineData("           IF EIBAID = DFHENTER AND WS-FLAG CONTINUE END-IF.\n", false)]
    [InlineData("           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3 CONTINUE END-IF.\n", true)]
    [InlineData("           IF (EIBAID = DFHENTER)\n               OR (EIBAID = DFHPF3)\n               CONTINUE END-IF.\n", true)]
    [InlineData("           IF EIBAID = DFHAID CONTINUE END-IF.\n", false)]
    [InlineData("           IF EIBAID = DFHAID OR DFHPF12\n               CONTINUE END-IF.\n", false)]
    [InlineData("           MOVE DFHENTER TO EIBAID.\n", false)]
    [InlineData("           IF DFHPF1 = DFHPF2 OR EIBAID = DFHPF3 CONTINUE END-IF.\n", false)]
    public void NativeSymbolicPredicates_AndUnsupportedAggregateWritesAndComparisons(string body, bool supported)
    {
        if (NativeImage is not { } image) return;
        var (configured, profile, staging, _) = Fixture();
        var prefix = Text(ProgramPath).Split("       PROCEDURE DIVISION.")[0] + "       01 WS-FLAG PIC X.\n";
        File.WriteAllText(ProgramPath, prefix + "       PROCEDURE DIVISION.\n" + body + "           GOBACK.\n");
        StageProgram(staging);
        var native = Path.Join(Output, "native.json");
        var (code, log) = Run("docker",
        [
            "run", "--rm", "--network", "none", "--entrypoint", "java",
            "-v", $"{Source}:/source:ro", "-v", $"{Output}:/output",
            "-v", $"{configured}:{configured}:ro", "-e", $"{PlatformVariable}={configured}",
            image, "-jar", "/app/smojol-cli.jar",
            "dependency", "PROG.cbl", "--srcDir=/source/.rekt-staging/.parser-input/estate/PROG.cbl",
            "--copyBooksDir=/source/.rekt-staging", "--dialectJarPath=/app/dialect-idms.jar",
            "--export=/output/native.json",
        ]);
        (code == 0).Should().Be(supported, log);
        var document = J(Text(native));
        var leaf = Objects(document["dependencies"]).First(node => (string?)node["dependencyKind"] == "PLATFORM_COPY");
        RektEvidence.NativePlatformContractBound(leaf, profile).Should().Be(supported);
        var diagnostics = Strings(document["dependencyDiagnostics"]);
        if (!supported)
        {
            diagnostics.Should().Contain(issue => issue.Contains("platform-contract-") || issue.Contains("UNRESOLVED_CICS_AID_SEMANTIC:"));
            if (body.Contains("EIBAID = DFHAID"))
                diagnostics.Count(issue => issue.StartsWith("UNRESOLVED_CICS_AID_SEMANTIC:")).Should().Be(1);
            diagnostics.Should().NotContain("incomplete-callee:DFHAID");
            return;
        }
        leaf["runtimeContract"]!["usages"]!.AsArray().Should().NotBeEmpty();
        File.Copy(native, ExportPath, overwrite: true);
        var produced = WithEnvironment(PlatformVariable, configured, () => Build(staging: staging));
        Strings(produced["unresolvedDiagnostics"]).Should().BeEmpty();
        WriteReport(produced);
        if (GateAssembly is not null) PlanStatuses(Path.Join(Work, "strict-plan")).Should().Equal("eligible");
    }
}
