using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Rekt;

// A COMMAREA slot may only be resolved from the estate-wide union of every program that can send it.
[Collection(RektEvidenceCollection.Name)]
public sealed class RektCommareaClosureTests : RektEvidenceFixture
{
    private static readonly string[] Layout = ["01CARDDEMO-COMMAREA.", "05CDEMO-INFO.", "10CDEMO-TO-PROGRAMPICX(8)."];

    private static string Key(string name) => name.PadRight(64, '0');

    private void Participant(string name, IEnumerable<JsonNode>? records, IEnumerable<JsonNode>? sends = null,
        IEnumerable<JsonNode>? carriers = null, string? source = null)
    {
        var relative = $"est/{name}.cbl";
        var path = Write(relative, source ?? "       PROCEDURE DIVISION.\n           GOBACK.\n");
        if (records is null) return;
        var inventory = new JsonObject
        {
            ["schema"] = 1, ["records"] = Array(records), ["carriers"] = Array(carriers ?? []), ["sends"] = Array(sends ?? []),
        };
        Save(Path.Join(Output, relative + "-deps.json"), new JsonObject { ["sourceSha256"] = Key(name), ["commareaInventory"] = inventory });
        Save(Path.Join(Output, relative + ".report", "dependency-evidence.json"), new JsonObject { ["originalSourceSha256"] = Digest(path) });
    }

    private static JsonArray Array(IEnumerable<JsonNode> items) => new(items.Select(item => (JsonNode?)item.DeepClone()).ToArray());

    private static JsonObject Record(string?[] values, bool entry, bool? loaded = null, string[]? layout = null) => new()
    {
        ["name"] = "CARDDEMO-COMMAREA", ["layout"] = new JsonArray((layout ?? Layout).Select(item => (JsonNode?)item).ToArray()),
        ["loaded"] = loaded ?? entry, ["issues"] = new JsonArray(),
        ["slots"] = new JsonObject
        {
            ["CDEMO-TO-PROGRAM"] = new JsonObject
            {
                ["length"] = 8, ["values"] = new JsonArray(values.Select(item => (JsonNode?)item).ToArray()), ["entry"] = entry,
                ["copies"] = new JsonArray(), ["unsupported"] = new JsonArray(),
            },
        },
    };

    private static JsonObject Send(string kind = "record", string? extra = null)
    {
        var send = O($$"""
            {"command": "XCTL", "operand": "CARDDEMO-COMMAREA", "line": 1, "column": 0, "kind": "{{kind}}", "record": "CARDDEMO-COMMAREA"}
            """);
        if (extra is not null)
            foreach (var (key, value) in O(extra)) send[key] = value?.DeepClone();
        return send;
    }

    private JsonObject Closure() => RektEvidence.CommareaClosure(Source, Output, "est");

    [Fact]
    public void Closure_UnionsEverySenderIntoEachLoader()
    {
        Participant("SIGNON", [Record([null, "MENU"], false)], [Send()]);
        Participant("MENU", [Record([null, "LIST"], true)], [Send()]);
        Participant("LIST", [Record([null], true, layout: [.. Layout, "05EXTRAPICX(4)."])],
            [Send("carrier", """{"carrier": "WS-COMMAREA"}""")],
            [O("""{"name": "WS-COMMAREA", "record": "CARDDEMO-COMMAREA", "issues": []}""")]);
        Participant("MQREAD", null, source: "       PROCEDURE DIVISION.\n           CALL 'MQGET'.\n");
        var closure = Closure();
        closure["errors"]!.AsArray().Should().BeEmpty();
        closure["programs"]![Key("LIST")]!["slots"]!["CDEMO-TO-PROGRAM"]!.AsArray()
            .Select(item => (string?)item).Should().BeEquivalentTo(new string?[] { null, "MENU", "LIST" });
        Objects(closure["participants"]).Where(item => ((string)item["relativePath"]!).Contains("MQREAD"))
            .Select(item => (string?)item["mode"]).Should().Equal("lexical");
    }

    [Fact]
    public void Closure_IgnoresLayoutsLinkedOnlyToProgramsLoadingNoRecord()
    {
        string[] fraud = ["01WS-FRAUD-DATA.", "05FRAUD-KEYPICX(8)."];
        var link = Send(extra: """
            {"command": "LINK", "targets": ["HELPER"], "bindingStatus": "complete", "operand": "WS-FRAUD-DATA", "record": "WS-FRAUD-DATA"}
            """);
        var fraudRecord = Record([null], false, layout: fraud);
        fraudRecord["name"] = "WS-FRAUD-DATA";
        Participant("MENU", [Record([null, "LIST"], true)], [Send()]);
        Participant("SENDER", [Record([null], false), fraudRecord], [Send(), link]);
        Participant("HELPER", []);
        Closure()["errors"]!.AsArray().Should().BeEmpty();
        Participant("HELPER", [Record([null], true)]);
        Closure()["errors"]!.AsArray().Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("unsupported-write")]
    [InlineData("foreign-to-loader")]
    [InlineData("carrier-issue")]
    [InlineData("unparsed-sender")]
    public void Closure_RefusesAnOpenWorld(string name)
    {
        Participant("MENU", [Record([null, "LIST"], true)], [Send()]);
        switch (name)
        {
            case "unsupported-write":
                var record = Record([null], false);
                record["slots"]!["CDEMO-TO-PROGRAM"]!["unsupported"] = new JsonArray("sending-expression");
                Participant("BAD", [record], [Send()]);
                break;
            case "foreign-to-loader":
                Participant("LINKER", [], [Send("foreign", """{"operand": "WS-OTHER", "targets": ["MENU"]}""")]);
                break;
            case "carrier-issue":
                Participant("CARRIER", [Record([null], false)], [Send("carrier", """{"carrier": "WS-COMMAREA"}""")],
                    [O("""{"name": "WS-COMMAREA", "record": "CARDDEMO-COMMAREA", "issues": ["carrier-write:Other"]}""")]);
                break;
            default:
                Participant("LOST", null, source: "       PROCEDURE DIVISION.\n           EXEC CICS XCTL COMMAREA(X) END-EXEC.\n");
                break;
        }
        var closure = Closure();
        if (name == "unsupported-write")
        {
            closure["programs"]![Key("MENU")]!["slots"]!.AsObject().ContainsKey("CDEMO-TO-PROGRAM").Should().BeFalse();
            return;
        }
        closure["errors"]!.AsArray().Should().NotBeEmpty();
        closure["programs"]!.AsObject().Should().BeEmpty();
    }
}

// The parser searches a per-program view; evidence must still bind the current originals behind every view copy.
[Collection(RektEvidenceCollection.Name)]
public sealed class RektDependencySearchViewTests : RektEvidenceFixture
{
    private string _callee = "";
    private string _staging = "";
    private string _view = "";

    private (JsonObject Document, string Actual) Prepare(bool transformed = false)
    {
        var document = NativeExport();
        _callee = Path.Join(Source, "estate", "SUBPROG.cbl");
        _staging = Path.Join(Source, ".rekt-staging");
        var actual = _callee;
        if (transformed)
        {
            actual = Path.Join(Source, ".preprocessed", "SUBPROG.cbl");
            Save(actual, "           DISPLAY 'NORMALIZED'.\n           GOBACK.\n");
            RektEvidence.Provenance(_callee, actual);
        }
        Stage(ProgramPath, Path.Join(_staging, Relative));
        Stage(DefinitionPath, Path.Join(_staging, "REAL.cpy"));
        Stage(_callee, Path.Join(_staging, "estate", "SUBPROG.cbl"), transformed ? actual : null);
        var isolated = Path.Join(_staging, ".parser-input", Relative, "PROG.cbl");
        Directory.CreateDirectory(Path.GetDirectoryName(isolated)!);
        File.Copy(Path.Join(_staging, Relative), isolated);
        _view = RektEvidence.DependencySearchView(_staging, Relative);
        document["sourcePath"] = Path.Join(_view, "PROG.cbl");
        document["dependencyAnalysisComplete"] = true;
        document["dependencyCoverage"] = AllKinds("complete");
        document["dependencyAbsenceEvidence"] = AbsenceProofs();
        string[] paths = [Path.Join(_staging, "REAL.cpy"), Path.Join(_view, "SUBPROG.cbl")];
        for (var index = 0; index < paths.Length; index++)
        {
            document["dependencies"]![index]!["sourcePath"] = paths[index];
            document["dependencies"]![index]!["sourceSha256"] = Digest(paths[index]);
        }
        Save(ExportPath, document);
        return (document, actual);
    }

    private JsonObject BuildView() => RektEvidence.BuildEvidence(Source, Relative, Output, 0, _staging, Source);

    [Fact]
    public void TransformedCallee_BindsCurrentOriginalAndPersistentActualInput()
    {
        var (_, transformed) = Prepare(transformed: true);
        var result = BuildView();
        Strings(result["unresolvedDiagnostics"]).Should().BeEmpty();
        Objects(result["definitions"]).Where(item => (string?)item["kind"] == "CALL")
            .ToDictionary(item => (string)item["path"]!, item => (string)item["sha256"]!)
            .Should().BeEquivalentTo(new Dictionary<string, string>
            {
                [_callee] = Digest(_callee), [transformed] = Digest(transformed),
            });
        WriteReport(result);
        Current().Should().BeTrue();
        File.WriteAllText(transformed, "changed actual parser representation");
        Current().Should().BeFalse();
    }

    [Fact]
    public void ViewCopy_CannotChangeEvenIfNativeHashIsUpdated()
    {
        var (document, _) = Prepare();
        var callee = Path.Join(_view, "SUBPROG.cbl");
        File.WriteAllText(callee, "           DISPLAY 'DIFFERENT'.\n");
        document["dependencies"]![1]!["sourceSha256"] = Digest(callee);
        Save(ExportPath, document);
        var result = BuildView();
        Strings(result["unresolvedDiagnostics"]).Should().Contain(item => item.Contains("callee search view provenance invalid"));
        Objects(result["definitions"]).Should().NotContain(item => (string?)item["kind"] == "CALL");
    }

    [Fact]
    public void ChangedOriginalCallee_IsNotHiddenByUnchangedViewCopy()
    {
        Prepare();
        File.WriteAllText(_callee, "           DISPLAY 'CHANGED ORIGINAL'.\n");
        Strings(BuildView()["unresolvedDiagnostics"]).Should().Contain(item => item.Contains("staged definition provenance invalid"));
    }

    [Fact]
    public void SameNamedRootWithIdenticalBytes_CannotReplaceSelectedEntry()
    {
        var (document, _) = Prepare();
        document["sourcePath"] = Write("other/PROG.cbl", Text(ProgramPath));
        Save(ExportPath, document);
        Strings(BuildView()["unresolvedDiagnostics"]).Should().Contain(item => item.StartsWith("native-export-source-unverified:"));
    }
}

// Shipped profiles may only declare documented runtime contracts, bound to exactly what the program calls.
public sealed class RektRuntimeContractProfileTests : RektEvidenceFixture
{
    private static readonly string Repo = RepoRoot();

    private static string CardDemoProfile => Path.Join(Repo, "tools", "cobol-rekt", "contracts", "estates", "carddemo.json");

    private static JsonObject Contract(string name, JsonNode profile) => new()
    {
        ["dependencyKind"] = "EXTERNAL_RUNTIME", ["runtimeContractBindingComplete"] = true,
        ["dependencyAnalysisComplete"] = false, ["sourcePath"] = null, ["sourceSha256"] = null,
        ["callTarget"] = new JsonObject { ["name"] = name, ["programReferenceType"] = "STATIC" },
        ["runtimeContract"] = new JsonObject
        {
            ["profileName"] = "carddemo-v1", ["profileKind"] = "target-port", ["sourcePath"] = CardDemoProfile,
            ["sourceSha256"] = new string('0', 64), ["name"] = name, ["resolutionKind"] = "documented-platform-contract",
            ["runtimeMapping"] = "unverified", ["definition"] = profile["runtimeContracts"]![name]!.DeepClone(),
        },
    };

    [Fact]
    public void CardDemoProfile_DeclaresOnlyDocumentedLanguageEnvironmentContracts()
    {
        var profile = PyJson.ReadFile(CardDemoProfile)!;
        profile["runtimeContracts"]!.AsObject().Select(pair => pair.Key)
            .Should().BeEquivalentTo("CEE3ABD", "CEEDAYS", "CBLTDLI", "DSNTIAC", "MVSWAIT", "COBDATFT");
        var bank = PyJson.ReadFile(Path.Join(Repo, "tools", "cobol-rekt", "contracts", "bank-of-z-spring-h2-v2.json"))!;
        PyJson.PyEquals(bank["runtimeContracts"]!["CEEDAYS"], profile["runtimeContracts"]!["CEEDAYS"]).Should().BeTrue();
        profile.AsObject().ContainsKey("sqlDefaultSchema").Should().BeFalse();
        var node = Contract("CEE3ABD", profile);
        RektEvidence.NativeRuntimeContractBound(node, profile).Should().BeTrue();
        var parameters = node["runtimeContract"]!["definition"]!["parameters"]!.AsArray();
        var extra = parameters[0]!.DeepClone();
        extra["name"] = "fc";
        parameters.Add(extra);
        RektEvidence.NativeRuntimeContractBound(node).Should().BeFalse();
    }

    [Theory]
    [InlineData("none")]
    [InlineData("other constant")]
    [InlineData("other operand")]
    [InlineData("usage not proved")]
    [InlineData("subscripted operand")]
    [InlineData("extra key")]
    public void DynamicCallResolution_BindsDocumentedRuntimeToReadOnlyConstant(string damage)
    {
        var profile = PyJson.ReadFile(CardDemoProfile)!;
        static JsonObject Entry(params string[] tokens) => new()
        {
            ["rule"] = "DataDescriptionEntryFormat1Context",
            ["children"] = new JsonArray(tokens.Select(token => (JsonNode?)new JsonObject { ["token"] = token }).ToArray()),
        };
        string[] call = ["CALL", "LIT-DSNTIAC", "USING", "DFHEIBLK", "DFHCOMMAREA", "SQLCA", "WS-MSG", "WS-LRECL"];
        var node = Contract("DSNTIAC", profile);
        var resolution = new JsonObject
        {
            ["operand"] = "LIT-DSNTIAC",
            ["targetResolution"] = new JsonObject
            {
                ["kind"] = "immutable-working-storage-literal", ["operand"] = "LIT-DSNTIAC",
                ["definitionSyntax"] = Entry("05", "LIT-DSNTIAC", "PIC", "X(7)", "VALUE", "'DSNTIAC'", "."),
                ["ancestorSyntax"] = new JsonArray(Entry("01", "WS-LITERALS", ".")),
                ["usageProof"] = RektEvidence.ConstantOperandProof,
            },
            ["callSyntax"] = new JsonObject
            {
                ["rule"] = "CallStatementContext",
                ["children"] = new JsonArray(call.Select(token => (JsonNode?)new JsonObject { ["token"] = token }).ToArray()),
            },
        };
        node["runtimeContract"]!["targetResolution"] = resolution;
        switch (damage)
        {
            case "other constant":
                resolution["targetResolution"]!["definitionSyntax"] = Entry("05", "LIT-DSNTIAC", "PIC", "X(7)", "VALUE", "'DSNTIAR'", ".");
                break;
            case "other operand": resolution["callSyntax"]!["children"]![1]!["token"] = "WS-PGM"; break;
            case "usage not proved": resolution["targetResolution"]!["usageProof"] = "assumed"; break;
            case "subscripted operand": resolution["callSyntax"]!["children"]!.AsArray().Insert(2, new JsonObject { ["token"] = "(" }); break;
            case "extra key": resolution["note"] = "x"; break;
        }
        RektEvidence.NativeRuntimeContractBound(node, profile).Should().Be(damage == "none");
    }
}
