using System.Text.Json.Nodes;
using CobolToQuarkusMigration.Agents.Infrastructure.Rekt;
using CobolToQuarkusMigration.Tests.Helpers;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents.Infrastructure.Rekt;

// Evidence may only claim what the parser actually exported for the current original bytes.
[Collection(RektEvidenceCollection.Name)]
public sealed class RektEvidenceTests : RektEvidenceFixture
{
    [Fact]
    public void CicsProgramAttributes_AcceptOnlyExplicitOrDefaultNo()
    {
        RektEvidence.CicsProgramAttributesValid(J("""{"language": "COBOL", "dynamic": "NO"}""")).Should().BeTrue();
        RektEvidence.CicsProgramAttributesValid(
            J("""{"language": "COBOL", "dynamic": "NO", "dynamicProvenance": "cics-default"}""")).Should().BeTrue();
        foreach (var attributes in new[]
                 {
                     """{"language": "COBOL", "dynamic": "YES"}""",
                     """{"language": "COBOL", "dynamic": "MAYBE"}""",
                     """{"language": "COBOL"}""",
                     """{"language": "COBOL", "dynamic": "YES", "dynamicProvenance": "cics-default"}""",
                     """{"language": "COBOL", "dynamic": "NO", "dynamicProvenance": "guessed"}""",
                     """{"language": "COBOL", "dynamic": "NO", "dynamicProvenance": "cics-default", "extra": 1}""",
                 })
            RektEvidence.CicsProgramAttributesValid(J(attributes)).Should().BeFalse(attributes);
    }

    [Fact]
    public void HandleAbendCancel_BindsWithoutALabelButNotResetOrFabricatedResources()
    {
        var statement = O("""
            {"kind": "EXEC_CICS", "operation": "HANDLE", "grammarRule": "Cics_handleContext",
             "sourceText": "HANDLE ABEND CANCEL", "bindingStatus": "complete",
             "bindingBasis": "parsed-cics-control-binding", "resourceReferences": [], "resourceBindings": [],
             "syntax": {"rule": "Cics_handleContext", "children": [
                {"token": "HANDLE"}, {"rule": "Cics_handle_abendContext", "children": [
                    {"token": "ABEND"}, {"token": "CANCEL"}]}]}}
            """);
        RektEvidence.NativeCicsStatementBound(statement).Should().BeTrue();
        var damaged = Clone(statement)!;
        damaged["syntax"]!["children"]![1]!["children"]![1]!["token"] = "RESET";
        damaged["sourceText"] = "HANDLE ABEND RESET";
        RektEvidence.NativeCicsStatementBound(damaged).Should().BeFalse();
        statement["resourceBindings"] = J("""[{"kind": "CICS_LABEL", "name": "INVENTED"}]""");
        RektEvidence.NativeCicsStatementBound(statement).Should().BeFalse();
    }

    private JsonObject CicsReturnExport()
    {
        File.WriteAllText(ProgramPath, "       COPY REAL.\n           CALL 'SUBPROG'.\n           EXEC CICS RETURN END-EXEC.\n");
        var document = NativeExport();
        document["dependencyAnalysisComplete"] = true;
        document["dependencyCoverage"] = AllKinds("complete");
        document["dependencyAbsenceEvidence"] = new JsonObject
        {
            ["EXEC_SQL"] = "antlr-tree:no-sql-contexts", ["FILE_IO"] = RektEvidence.AbsenceProofs["FILE_IO"],
        };
        document["dependencyPresenceEvidence"] = O("""{"EXEC_CICS": "antlr-tree:resource-free-cics-return"}""");
        document["dependencyDialectStatements"] = J("""
            [{"kind": "EXEC_CICS", "operation": "RETURN", "grammarRule": "Cics_returnContext",
              "sourceText": "RETURN", "bindingStatus": "complete", "runtimeMapping": "unverified",
              "syntax": {"rule": "Cics_returnContext", "children": [{"token": "RETURN"}]}}]
            """);
        return document;
    }

    [Fact]
    public void PositiveNativeReturnEvidence_IsNotAnAbsenceOrRuntimeMappingClaim()
    {
        var document = CicsReturnExport();
        Save(ExportPath, document);
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().BeEmpty();
        PyJson.PyEquals(result["coverage"], AllKinds("complete")).Should().BeTrue();
        WriteReport(result);
        Current().Should().BeTrue();
    }

    [Theory]
    [InlineData("           EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n", true)]
    [InlineData("           DISPLAY 'EXEC CICS DELAY'.\n", false)]
    [InlineData("           DISPLAY 'Example\n      -    ' EXEC CICS DELAY'.\n", false)]
    [InlineData("      D    EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n", false)]
    [InlineData("       SOURCE-COMPUTER. IBM WITH DEBUGGING MODE.\n      D    EXEC CICS DELAY FOR SECONDS(3) END-EXEC.\n", true)]
    public void OriginalOperationsRemovedByPreprocessing_BlockEvidenceButLiteralsAndDebugLinesDoNot(string extra, bool blocked)
    {
        var document = CicsReturnExport();
        File.AppendAllText(ProgramPath, extra);
        document["sourceSha256"] = Digest(ProgramPath);
        Save(ExportPath, document);
        var result = Build();
        if (!blocked)
        {
            Strings(result["unresolvedDiagnostics"]).Should().BeEmpty();
            return;
        }
        Strings(result["unresolvedDiagnostics"]).Should().Contain("original-operation-not-exported:EXEC_CICS:DELAY:1");
        result["coverage"]!.AsObject().Select(pair => pair.Value!.GetValue<string>()).Should().OnlyContain(value => value == "partial");
        result["coverage"] = Clone(document["dependencyCoverage"]);
        result["unresolvedDiagnostics"] = new JsonArray();
        WriteReport(result);
        Current().Should().BeFalse();
    }

    [Fact]
    public void MissingSymbolicCicsExpressions_RejectProductionAndOldCache()
    {
        var document = CicsReturnExport();
        File.AppendAllText(ProgramPath, "           IF STATUS = DFHVALUE(ABEND) CONTINUE END-IF.\n"
                                        + "           IF STATUS = DFHRESP(NORMAL) CONTINUE END-IF.\n");
        document["sourceSha256"] = Digest(ProgramPath);
        Save(ExportPath, document);
        var result = Build();
        foreach (var operation in new[] { "DFHVALUE", "DFHRESP" })
            Strings(result["unresolvedDiagnostics"]).Should().Contain($"original-operation-not-exported:EXEC_CICS:{operation}:1");
        result["coverage"] = Clone(document["dependencyCoverage"]);
        result["unresolvedDiagnostics"] = new JsonArray();
        WriteReport(result);
        Current().Should().BeFalse();
    }

    [Theory]
    [InlineData("""{"sourceText": "RETURN TRANSID('NEXT')"}""")]
    [InlineData("""{"syntax": null}""")]
    [InlineData("""{"syntax": {"rule": "Cics_returnContext", "children": [{"token": "RETURN"}, {"token": "TRANSID"}]}}""")]
    public void NativeReturnPresence_CannotApproveAnOptionedReturnOrMalformedSyntax(string change)
    {
        var document = CicsReturnExport();
        var statement = document["dependencyDialectStatements"]![0]!.AsObject();
        foreach (var (key, value) in O(change)) statement[key] = Clone(value);
        Save(ExportPath, document);
        var result = Build();
        result["coverage"]!["EXEC_CICS"]!.GetValue<string>().Should().NotBe("complete");
        Strings(result["unresolvedDiagnostics"]).Should().Contain("native-absence-proof-missing:EXEC_CICS");
    }

    [Fact]
    public void NativeDialectPresence_ContradictsALegacyNoDialectAttestation()
    {
        var document = CicsReturnExport();
        document["dependencyAbsenceEvidence"] = AbsenceProofs();
        Save(ExportPath, document);
        var result = Build();
        result["coverage"]!["EXEC_CICS"]!.GetValue<string>().Should().NotBe("complete");
        Strings(result["unresolvedDiagnostics"]).Should().Contain("native-absence-proof-missing:EXEC_CICS");
    }

    private (JsonObject Document, string Catalog) BoundCicsFileExport()
    {
        var document = CicsReturnExport();
        var catalog = Write("cics/resources.yaml", "resourceDefinitions:\n"
                                                   + "  - file:\n      name: TESTFILE\n      dsname: TEST.DATA\n"
                                                   + "      recordsize: 80\n      keylength: 8\n");
        var statement = O("""
            {"kind": "EXEC_CICS", "operation": "WRITE", "grammarRule": "Cics_writeContext",
             "sourceText": "WRITE FILE('TESTFILE') FROM(REAL-FIELD)",
             "bindingStatus": "complete", "runtimeMapping": "unverified",
             "resourceBindings": [{"kind": "CICS_FILE", "name": "TESTFILE", "sourcePath": null, "sourceSha256": null,
                                   "attributes": {"dsname": "TEST.DATA", "recordsize": 80, "keylength": 8}}],
             "syntax": {"rule": "Cics_writeContext", "children": [
                {"token": "WRITE"},
                {"rule": "Cics_write_fileContext", "children": [
                    {"rule": "Cics_file_nameContext", "children": [
                        {"token": "FILE"}, {"rule": "Cics_nameContext", "children": [
                            {"token": "("}, {"rule": "NameContext", "children": [{"token": "'TESTFILE'"}]},
                            {"token": ")"}]}]}]}]}}
            """);
        statement["resourceBindings"]![0]!["sourcePath"] = catalog;
        statement["resourceBindings"]![0]!["sourceSha256"] = Digest(catalog);
        document["dependencyDialectStatements"]!.AsArray().Insert(0, statement);
        document["dependencyPresenceEvidence"] = O("""
            {"EXEC_CICS": "antlr-tree:bound-cics-resources", "FILE_IO": "antlr-tree:bound-cics-file-resources"}
            """);
        document["dependencyAbsenceEvidence"]!.AsObject().Remove("FILE_IO");
        return (document, catalog);
    }

    [Fact]
    public void BoundCicsFile_IsPositiveFileIoEvidenceAndRechecksItsRealDefinition()
    {
        var (document, catalog) = BoundCicsFileExport();
        Save(ExportPath, document);
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().BeEmpty();
        PyJson.PyEquals(result["coverage"], AllKinds("complete")).Should().BeTrue();
        Objects(result["definitions"]).Should().Contain(item =>
            (string?)item["kind"] == "CICS_FILE" && (string?)item["path"] == catalog);
        WriteReport(result);
        Current().Should().BeTrue();
        result["definitions"] = new JsonArray(Objects(result["definitions"])
            .Where(item => (string?)item["kind"] != "CICS_FILE").Select(item => (JsonNode?)item.DeepClone()).ToArray());
        WriteReport(result);
        Current().Should().BeFalse();
        File.WriteAllText(catalog, "changed");
        Strings(Build()["unresolvedDiagnostics"]).Should().Contain(issue =>
            issue.Contains("native CICS resource definition identity/hash mismatch"));
    }

    [Theory]
    [InlineData("absence")]
    [InlineData("wrong-name")]
    [InlineData("missing-binding")]
    [InlineData("sysid")]
    [InlineData("malformed-syntax")]
    public void BoundCicsFile_CannotClaimAbsenceOrUseTheWrongResourceOrRemoteSysid(string change)
    {
        var (document, _) = BoundCicsFileExport();
        var statement = document["dependencyDialectStatements"]![0]!;
        switch (change)
        {
            case "absence": document["dependencyAbsenceEvidence"]!["FILE_IO"] = RektEvidence.AbsenceProofs["FILE_IO"]; break;
            case "wrong-name": statement["resourceBindings"]![0]!["name"] = "OTHER"; break;
            case "missing-binding": statement["resourceBindings"] = new JsonArray(); break;
            case "sysid": statement["syntax"]!["children"]!.AsArray().Add(O("""{"token": "SYSID"}""")); break;
            default: statement["syntax"]!["children"] = null; break;
        }
        RektEvidence.NativeDomainEvidence(document, "FILE_IO").Should().BeFalse();
    }

    private JsonObject NativeFileControlExport()
    {
        var document = NativeExport();
        document["dependencyDialectStatements"] = J("""
            [{"kind": "FILE_IO", "operation": "READ", "grammarRule": "ReadStatementContext",
              "sourceText": "READ IN-FILE", "bindingStatus": "complete", "runtimeMapping": "unverified",
              "fileOperands": ["IN-FILE"],
              "resourceBindings": [{
                 "kind": "COBOL_FILE", "name": "IN-FILE", "assignment": "INFILE", "organization": "SEQUENTIAL",
                 "accessMode": "SEQUENTIAL", "records": ["IN-REC"], "definitionScope": "expanded-parser-input",
                 "runtimeMapping": "unverified",
                 "definitionSyntax": {"rule": "FileControlEntryContext", "children": [
                    {"rule": "SelectClauseContext", "children": [{"token": "SELECT"},
                        {"rule": "FileNameContext", "children": [{"rule": "CobolWordContext", "children": [{"token": "IN-FILE"}]}]}]}]},
                 "descriptionSyntax": {"rule": "FileDescriptionEntryClausesContext", "children": [
                    {"token": "FD"}, {"rule": "CobolWordContext", "children": [{"token": "IN-FILE"}]}]}}],
              "syntax": {"rule": "ReadStatementContext", "children": [{"token": "READ"},
                 {"rule": "FileNameContext", "children": [{"rule": "CobolWordContext", "children": [{"token": "IN-FILE"}]}]}]}}]
            """);
        document["dependencyCoverage"] = AllKinds("complete");
        document["dependencyAnalysisComplete"] = true;
        document["dependencyAbsenceEvidence"] = O("""{"EXEC_SQL": "antlr-tree:no-sql-contexts", "EXEC_CICS": "antlr-tree:no-cics-contexts"}""");
        document["dependencyPresenceEvidence"] = O("""{"FILE_IO": "antlr-tree:bound-file-control-resources"}""");
        return document;
    }

    [Fact]
    public void NativeSelectAndFdBinding_IsPositiveFileIoEvidence()
    {
        Save(ExportPath, NativeFileControlExport());
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().BeEmpty();
        PyJson.PyEquals(result["coverage"], AllKinds("complete")).Should().BeTrue();
    }

    [Theory]
    [InlineData("absence")]
    [InlineData("wrong-select")]
    [InlineData("unknown-operand")]
    [InlineData("no-binding")]
    [InlineData("unresolved")]
    [InlineData("cics-file")]
    public void NativeFileBinding_RejectsUnboundOrMismatchedDeclarations(string change)
    {
        var document = NativeFileControlExport();
        var statement = document["dependencyDialectStatements"]![0]!;
        var binding = statement["resourceBindings"]![0]!;
        switch (change)
        {
            case "absence": document["dependencyAbsenceEvidence"]!["FILE_IO"] = RektEvidence.AbsenceProofs["FILE_IO"]; break;
            case "wrong-select":
                binding["definitionSyntax"]!["children"]![0]!["children"]![1]!["children"]![0]!["children"] = J("""[{"token": "OTHER-FILE"}]""");
                break;
            case "unknown-operand": statement["fileOperands"] = J("""["OTHER-FILE"]"""); break;
            case "no-binding": statement["resourceBindings"] = new JsonArray(); break;
            case "unresolved": statement["bindingStatus"] = "unresolved"; break;
            default: document["dependencyPresenceEvidence"]!["FILE_IO"] = "antlr-tree:bound-cics-file-resources"; break;
        }
        RektEvidence.NativeDomainEvidence(document, "FILE_IO").Should().BeFalse();
    }

    [Fact]
    public void IncompleteCics_DoesNotMisreportAValidSqlAbsenceProofAsMissing()
    {
        var document = CicsReturnExport();
        document["dependencyAnalysisComplete"] = false;
        document["dependencyCoverage"]!["EXEC_CICS"] = "partial";
        document["dependencyDiagnostics"] = J("""["cics-file-definition-not-found:TESTFILE"]""");
        Save(ExportPath, document);
        var result = Build(1);
        Strings(result["unresolvedDiagnostics"]).Should().NotContain("native-absence-proof-missing:EXEC_SQL")
            .And.Contain("native:cics-file-definition-not-found:TESTFILE");
        result["coverage"]!["EXEC_SQL"]!.GetValue<string>().Should().NotBe("complete");
    }

    [Fact]
    public void SuccessfulExport_BindsOriginalAndActualExportWithoutClaimingFullCoverage()
    {
        var result = Build();
        ((string?)result["originalSourceSha256"]).Should().Be(Digest(ProgramPath));
        ((string?)result["originalSourcePath"]).Should().Be(PyCompat.Resolve(ProgramPath));
        ((string?)result["relativePath"]).Should().Be(Relative);
        ((string?)result["exportSha256"]).Should().Be(Digest(ExportPath));
        result["definitions"]!.AsArray().Should().BeEmpty();
        var issues = Strings(result["unresolvedDiagnostics"]);
        issues.Should().Contain("definition-resolution-not-exported:REAL").And.Contain("exporter-coverage-not-declared:COPY,CALL");
        ((string?)result["coverage"]!["COPY"]).Should().Be("partial");
        ((string?)result["coverage"]!["CALL"]).Should().Be("partial");
        ((string?)result["coverage"]!["EXEC_SQL"]).Should().Be("unavailable");
    }

    [Fact]
    public void EmptyCopyExport_CannotContradictOriginalOrBeFilledBySourceScanner()
    {
        var exported = J(Text(ExportPath));
        exported["dependencies"] = new JsonArray();
        Save(ExportPath, exported);
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().Contain("empty-COPY-export-contradiction").And.Contain("copy-not-exported:REAL");
        result["definitions"]!.AsArray().Should().BeEmpty();
        J(Text(ExportPath))["dependencies"]!.AsArray().Should().BeEmpty();
    }

    [Fact]
    public void FailedAndMalformedExports_RetainTruthfulEvidence()
    {
        File.Delete(ExportPath);
        var result = Build(17);
        ((int?)result["exportExitCode"]).Should().Be(17);
        result["exportSha256"].Should().BeNull();
        Strings(result["unresolvedDiagnostics"]).Should().Contain("dependency-export-failed:17");
        ((string?)result["coverage"]!["COPY"]).Should().Be("unavailable");
        File.WriteAllText(ExportPath, """{"dependencies":[]}""");
        Strings(Build()["unresolvedDiagnostics"]).Should().Contain(item => item.Contains("identity-mismatch"));
    }

    [Fact]
    public void UnresolvedNestedCopybookAndParserDiagnostics_ArePreserved()
    {
        File.WriteAllText(DefinitionPath, "       COPY ABSENT.\n");
        Save(Path.Join(Output, Relative + ".report", "parse-diagnostics.json"),
            """[{"code": "missing copybook", "message": "ABSENT definition required"}]""");
        var issues = Strings(Build()["unresolvedDiagnostics"]);
        issues.Should().Contain("nested-copy-not-exported:ABSENT").And.Contain("unresolved-copybook:ABSENT");
        issues.Should().Contain(item => item.Contains("ABSENT definition required"));
        Directory.EnumerateFiles(Source, "*.cpy", SearchOption.AllDirectories)
            .Should().NotContain(path => Path.GetFileNameWithoutExtension(path) == "ABSENT");
    }

    [Fact]
    public void CopyNameLengthInfo_IsDroppedOnlyForABoundCopybook()
    {
        const string note = "Copybook declaration has more than 8 characters for: ";
        var document = NativeExport();
        document["dependencyDiagnostics"] = new JsonArray("parser-diagnostic::" + note + "REAL", "parser-diagnostic::" + note + "ABSENT");
        Save(ExportPath, document);
        Save(Path.Join(Output, Relative + ".report", "parse-diagnostics.json"), new JsonArray(
            new JsonObject { ["severity"] = "INFO", ["message"] = note + "REAL" },
            new JsonObject { ["severity"] = "INFO", ["message"] = note + "ABSENT" },
            new JsonObject { ["severity"] = "WARNING", ["message"] = note + "REAL " }));
        var bound = Build();
        Objects(bound["definitions"]).Where(item => (string?)item["kind"] == "COPY").Select(item => (string?)item["name"])
            .Should().Contain("REAL");
        var issues = Strings(bound["unresolvedDiagnostics"]);
        issues.Should().NotContain("parser-diagnostic:None:" + note + "REAL")
            .And.NotContain("native:parser-diagnostic::" + note + "REAL")
            .And.Contain("parser-diagnostic:None:" + note + "ABSENT")
            .And.Contain("native:parser-diagnostic::" + note + "ABSENT")
            .And.Contain("parser-diagnostic:None:" + note + "REAL ");

        File.Delete(DefinitionPath);
        Strings(Build()["unresolvedDiagnostics"]).Should().Contain("parser-diagnostic:None:" + note + "REAL")
            .And.Contain("native:parser-diagnostic::" + note + "REAL");
    }

    [Fact]
    public void PlaceholderAndUnprovenGeneratedDefinitions_AreNotResolved()
    {
        File.WriteAllText(DefinitionPath, "      * AUTO-GENERATED STUB COPYBOOK\n       01 REAL-STUB PIC X.\n");
        var result = Build();
        ((bool?)result["placeholderContaminated"]).Should().BeTrue();
        result["definitions"]!.AsArray().Should().BeEmpty();
        File.Delete(DefinitionPath);
        Write("copy-generated/REAL.cpy", "       01 REAL-FIELD PIC X.\n");
        result = Build();
        // Unknown provenance, not a proven stub.
        ((bool?)result["placeholderContaminated"]).Should().BeFalse();
        Strings(result["unresolvedDiagnostics"]).Should().Contain("unverified-copybook:REAL");
        result["definitions"]!.AsArray().Should().BeEmpty();
    }

    [Fact]
    public void ExplicitStubInParserArtifact_ContaminatesOtherwiseCleanInputs()
    {
        Save(Path.Join(Output, Relative + ".report", "data.json"), """{"name":"DFHAID-STUB"}""");
        ((bool?)Build()["placeholderContaminated"]).Should().BeTrue();
    }

    [Fact]
    public void StalePreprocessedShadow_CannotOverrideCurrentOriginal()
    {
        var transformed = Path.Join(Source, ".preprocessed", "REAL.cpy");
        Save(transformed, "       01 OLD-FIELD PIC 9.\n");
        RektEvidence.Provenance(DefinitionPath, transformed);
        File.WriteAllText(DefinitionPath, "       01 CURRENT-FIELD PIC X(2).\n");
        var staged = Path.Join(Work, "staging", "REAL.cpy");
        Stage(DefinitionPath, staged, transformed);
        File.ReadAllBytes(staged).Should().Equal(File.ReadAllBytes(DefinitionPath));
    }

    [Fact]
    public void WrongOriginalSourceIdentity_CannotReuseSameNamedTransformation()
    {
        var other = Write("other/PROG.cbl", "           DISPLAY 'OTHER'.\n");
        var transformed = Path.Join(Source, ".preprocessed", "PROG.cbl");
        Save(transformed, "           DISPLAY 'TRANSFORMED OTHER'.\n");
        RektEvidence.Provenance(other, transformed);
        var staged = Path.Join(Work, "staging", "PROG.cbl");
        Stage(ProgramPath, staged, transformed);
        File.ReadAllBytes(staged).Should().Equal(File.ReadAllBytes(ProgramPath));
    }

    [Fact]
    public void StagedCopyHash_MustMatchOriginalAndActualParserInput()
    {
        var staging = Path.Join(Work, "staging");
        Stage(ProgramPath, Path.Join(staging, Relative));
        Stage(DefinitionPath, Path.Join(staging, "REAL.cpy"));
        File.WriteAllText(Path.Join(staging, "REAL.cpy"), "       01 STALE-FIELD PIC X.\n");
        var result = Build(staging: staging);
        Strings(result["unresolvedDiagnostics"]).Should().Contain("unverified-staged-copybook:REAL");
        result["definitions"]!.AsArray().Should().BeEmpty();
    }

    [Fact]
    public void OldCompleteClaim_CannotHideChangedDefinitionOrEmptyCopyExport()
    {
        NativeExport();
        var result = Build();
        // Simulate legacy persisted claims, not a new producer assertion of completeness.
        result["coverage"] = AllKinds("complete");
        result["unresolvedDiagnostics"] = new JsonArray();
        WriteReport(result);
        File.WriteAllText(DefinitionPath, "       01 CHANGED PIC X.\n");
        Current().Should().BeFalse();
        result["definitions"]![0]!["sha256"] = Digest(DefinitionPath);
        var export = J(Text(ExportPath));
        export["dependencies"] = new JsonArray();
        Save(ExportPath, export);
        result["exportSha256"] = Digest(ExportPath);
        WriteReport(result);
        Current().Should().BeFalse();
    }

    [Fact]
    public void NativeParserCoverage_RequiresActualResolvedInputHashes()
    {
        NativeExport();
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().BeEmpty();
        ((string?)result["coverage"]!["COPY"]).Should().Be("complete");
        ((string?)result["coverage"]!["CALL"]).Should().Be("complete");
        ((string?)result["coverage"]!["EXEC_CICS"]).Should().Be("unavailable");
        Objects(result["definitions"]).Select(item => (string?)item["kind"]).Distinct().Should().BeEquivalentTo("COPY", "CALL");
        File.WriteAllText(DefinitionPath, "       01 CHANGED-SINCE-PARSE PIC X.\n");
        result = Build();
        ((string?)result["coverage"]!["COPY"]).Should().Be("partial");
        Strings(result["unresolvedDiagnostics"]).Should().Contain(item => item.Contains("native resolved input hash mismatch"));
        Objects(result["definitions"]).Should().NotContain(item => (string?)item["kind"] == "COPY");
    }

    [Fact]
    public void FailedNativeExport_RetainsHashBoundPositiveEvidenceWithoutCompleteStatus()
    {
        NativeExport();
        var result = Build(1);
        Strings(result["unresolvedDiagnostics"]).Should().Contain("dependency-export-failed:1");
        ((string?)result["coverage"]!["COPY"]).Should().Be("partial");
        Objects(result["definitions"]).Select(item => (string?)item["kind"]).Distinct().Should().BeEquivalentTo("COPY", "CALL");
    }

    [Fact]
    public void NativeClaimForSameNamedOtherOriginal_IsRejected()
    {
        var document = NativeExport();
        document["sourcePath"] = Write("other/PROG.cbl", Text(ProgramPath));
        Save(ExportPath, document);
        var result = Build();
        ((string?)result["coverage"]!["COPY"]).Should().Be("partial");
        Strings(result["unresolvedDiagnostics"]).Should().Contain(item => item.Contains("source identity/hash mismatch"));
    }

    [Fact]
    public void NativeDynamicCall_StaysUnresolvedNotConcrete()
    {
        var document = NativeExport();
        File.WriteAllText(ProgramPath, "       COPY REAL.\n           CALL WS-TARGET.\n");
        document["sourceSha256"] = Digest(ProgramPath);
        document["dependencies"]![1] = O("""
            {"callTarget": {"name": "WS-TARGET", "programReferenceType": "DYNAMIC"}, "dependencyKind": "CALL", "dependencies": []}
            """);
        document["dependencyCoverage"]!["CALL"] = "partial";
        Save(ExportPath, document);
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().Contain("dynamic-call-target-unresolved:WS-TARGET");
        Objects(result["definitions"]).Should().NotContain(item => (string?)item["kind"] == "CALL");
        ((string?)result["coverage"]!["CALL"]).Should().NotBe("complete");
    }

    [Fact]
    public void LiteralsAndLiteralCommentMarkers_AreNotDependencyContradictions()
    {
        File.WriteAllText(ProgramPath,
            "       COPY 'REAL'.\n"
            + "           DISPLAY 'COPY ABSENT. CALL WS-FAKE *> not a comment'.\n"
            + "           DISPLAY 'It''s COPY WRONG'.\n"
            + "           CALL 'SUBPROG'. *> COPY COMMENTED.\n");
        NativeExport();
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().BeEmpty();
        ((string?)result["coverage"]!["COPY"]).Should().Be("complete");
        ((string?)result["coverage"]!["CALL"]).Should().Be("complete");
    }

    [Fact]
    public void HyphenatedParagraphEndingInCall_IsNotADynamicCall()
    {
        File.WriteAllText(ProgramPath,
            "       COPY REAL.\n           PERFORM 3200-INSERT-IMS-CALL THRU 3200-EXIT.\n           CALL 'SUBPROG'.\n");
        NativeExport();
        Strings(Build()["unresolvedDiagnostics"]).Should().NotContain("dynamic-CALL-target-unresolved")
            .And.NotContain("call-not-exported:INSERT-IMS-CALL");
        File.WriteAllText(ProgramPath, "       COPY REAL.\n           CALL WS-TARGET.\n           CALL 'SUBPROG'.\n");
        NativeExport();
        Strings(Build()["unresolvedDiagnostics"]).Should().Contain("dynamic-CALL-target-unresolved");
    }

    [Fact]
    public void GroundedBmsHashes_DoNotAttestCompleteSymbolicLayout()
    {
        var bms = Write("bms/REAL.bms", "MAP DFHMSD LANG=COBOL\n");
        RektEvidence.Provenance(bms, DefinitionPath, producer: "bms-symbolic-map");
        NativeExport();
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().Contain("partial-derived-bms-layout:REAL.cpy");
        ((string?)result["coverage"]!["COPY"]).Should().Be("partial");
        Objects(result["definitions"]).Should().Contain(item => (string?)item["kind"] == "BMS" && (string?)item["path"] == bms);
    }

    [Fact]
    public void NativeSupportedAbsence_CanCompleteAllKindsAndReuseCurrentEvidence()
    {
        var document = NativeExport();
        document["dependencyCoverage"] = AllKinds("complete");
        document["dependencyAnalysisComplete"] = true;
        document["dependencyAbsenceEvidence"] = AbsenceProofs();
        Save(ExportPath, document);
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().BeEmpty();
        PyJson.PyEquals(result["coverage"], AllKinds("complete")).Should().BeTrue();
        WriteReport(result);
        Current().Should().BeTrue();
    }

    [Fact]
    public void NativeUnsupportedKind_CannotBeCompleteWithoutFullTreeAbsenceAttestation()
    {
        var document = NativeExport();
        document["dependencyCoverage"] = AllKinds("complete");
        Save(ExportPath, document);
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().Contain("native-absence-proof-missing:EXEC_CICS")
            .And.Contain("native-absence-proof-missing:EXEC_SQL").And.Contain("native-absence-proof-missing:FILE_IO");
        result["coverage"]!.AsObject().Should().NotContain(pair => (string?)pair.Value == "complete");
    }

    [Fact]
    public void OldCachedCompleteClaimWithoutNativeAbsenceProof_IsNotReusable()
    {
        var document = NativeExport();
        document["dependencyCoverage"] = AllKinds("complete");
        document["dependencyAnalysisComplete"] = true;
        Save(ExportPath, document);
        var result = Build();
        result["coverage"] = AllKinds("complete");
        result["unresolvedDiagnostics"] = new JsonArray();
        WriteReport(result);
        Current().Should().BeFalse();
    }

    [Fact]
    public void NativePresentOrUnknownContexts_RemainPartialWithExplicitDiagnostics()
    {
        var document = NativeExport();
        document["dependencyAnalysisComplete"] = true;
        foreach (var kind in RektEvidence.AbsenceProofs.Keys) document["dependencyCoverage"]![kind] = "partial";
        document["dependencyDiagnostics"] = J("""
            ["unsupported-dialect-dependency-contexts:DialectStatementContext", "unsupported-file-io-dependency-contexts:opaque-dialect"]
            """);
        Save(ExportPath, document);
        var result = Build();
        foreach (var kind in new[] { "EXEC_SQL", "EXEC_CICS", "FILE_IO" })
            ((string?)result["coverage"]![kind]).Should().Be("partial");
        Strings(result["unresolvedDiagnostics"]).Should().Contain(item => item.Contains("DialectStatementContext"));
    }

    [Fact]
    public void ChildCopy_CannotHideMissingRootCopyExportOrCreateRootCallEdges()
    {
        var document = NativeExport();
        var dependencies = document["dependencies"]!.AsArray();
        var childCopy = dependencies[0]!;
        dependencies.RemoveAt(0);
        dependencies[0]!["dependencies"] = new JsonArray(childCopy, O("""
            {"callTarget": {"name": "THIRD", "programReferenceType": "STATIC"}, "dependencyKind": "CALL", "dependencies": []}
            """));
        Save(ExportPath, document);
        var (copies, calls) = RektEvidence.ExportedNames(document, Path.GetFileName(ProgramPath));
        copies.Should().BeEmpty();
        calls.Should().BeEquivalentTo("SUBPROG");
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().Contain("copy-not-exported:REAL").And.Contain("empty-COPY-export-contradiction");
        ((string?)result["coverage"]!["COPY"]).Should().Be("partial");
    }

    [Fact]
    public void NativeRoot_CannotHideIncompleteCalleeBehindCompleteParentMetadata()
    {
        var document = NativeExport();
        document["dependencyAnalysisComplete"] = true;
        document["dependencyCoverage"] = AllKinds("complete");
        document["dependencyAbsenceEvidence"] = AbsenceProofs();
        document["dependencies"]![1]!["dependencyCoverage"]!["FILE_IO"] = "partial";
        document["dependencies"]![1]!["dependencyAnalysisComplete"] = false;
        Save(ExportPath, document);
        var result = Build();
        Strings(result["unresolvedDiagnostics"]).Should().Contain("native-incomplete-callee:SUBPROG");
        result["coverage"]!.AsObject().Should().NotContain(pair => (string?)pair.Value == "complete");
    }

    [Fact]
    public void PlaceholderInSupportingCalleeCopy_ContaminatesRootEvidence()
    {
        var document = NativeExport();
        var placeholder = Write("copy/CHILD.cpy", "      * AUTO-GENERATED STUB COPYBOOK\n       01 CHILD-STUB PIC X.\n");
        document["dependencies"]![1]!["dependencies"] = new JsonArray(new JsonObject
        {
            ["callTarget"] = O("""{"name": "CHILD.cpy", "programReferenceType": "STATIC"}"""), ["dependencyKind"] = "COPY",
            ["sourcePath"] = placeholder, ["sourceSha256"] = Digest(placeholder), ["dependencies"] = new JsonArray(),
        });
        Save(ExportPath, document);
        var result = Build();
        ((bool?)result["placeholderContaminated"]).Should().BeTrue();
        Objects(result["definitions"]).Should().NotContain(item => (string?)item["name"] == "CHILD");
        ((string?)result["coverage"]!["COPY"]).Should().Be("partial");
    }

    [Fact]
    public void Quarantine_PreservesOnlyTargetIdentity()
    {
        var untouched = Path.Join(Output, "other", "PROG.cbl-deps.json");
        Save(untouched, "unrelated");
        var (code, stdOut, stdErr) = LmaCliProcess.Run(Path.Join(Work, "cwd"), null,
            "rekt-evidence", "quarantine", "--output-root", Output, "--relative-path", Relative);
        code.Should().Be(0, stdOut + stdErr);
        File.Exists(ExportPath).Should().BeFalse();
        Directory.EnumerateFiles(Path.Join(Output, ".previous"), "PROG.cbl-deps.json", SearchOption.AllDirectories).Should().HaveCount(1);
        Text(untouched).Should().Be("unrelated");
    }

    [Fact]
    public void Validate_ExitsZeroOnlyForCurrentEvidence()
    {
        var document = NativeExport();
        document["dependencyCoverage"] = AllKinds("complete");
        document["dependencyAnalysisComplete"] = true;
        document["dependencyAbsenceEvidence"] = AbsenceProofs();
        Save(ExportPath, document);
        string[] args = ["rekt-evidence", "validate", "--source-root", Source, "--relative-path", Relative, "--output-root", Output];
        LmaCliProcess.Run(Path.Join(Work, "cwd"), null, args).ExitCode.Should().Be(1);
        WriteReport(Build());
        var (code, stdOut, stdErr) = LmaCliProcess.Run(Path.Join(Work, "cwd"), null, args);
        code.Should().Be(0, stdOut + stdErr);
    }
}
