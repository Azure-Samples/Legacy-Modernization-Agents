using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class PartialTreePlanTests : IDisposable
{
    private readonly string _root = Path.Combine(Path.GetTempPath(), "partial-tree-" + Guid.NewGuid().ToString("N"));

    private PartialProgram Assess(string data, string procedure)
    {
        Directory.CreateDirectory(_root);
        var path = Path.Combine(_root, "TEST.cbl");
        File.WriteAllText(path, "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST.\n" +
            "       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n" + data +
            "\n       PROCEDURE DIVISION.\n" + procedure);
        return PartialTreePlan.Assess(new CobolFile { FileName = "TEST.cbl", FilePath = path }, _root);
    }

    [Fact]
    public void SelectsIndependentRoutineWithoutAdmittingMissingLayoutOrItsCaller()
    {
        var plan = Assess("       01 W-COUNT PIC 9(4) VALUE ZERO.\n       77 W-TEXT PIC X(12).",
            "       MAIN SECTION.\n           PERFORM BLOCKED.\n" +
            "       RESET SECTION.\n           MOVE ZERO TO W-COUNT.\n           MOVE SPACE TO W-TEXT.\n" +
            "       BLOCKED SECTION.\n           MOVE ZERO TO MISSING-FIELD.\n");
        Assert.Equal("RESET", Assert.Single(plan.Selected).Name);
        Assert.Contains(plan.Routines.Single(r => r.Name == "MAIN").Blockers, b => b.Contains("BLOCKED"));
        Assert.Contains(plan.Routines.Single(r => r.Name == "BLOCKED").Blockers, b => b.Contains("MISSING-FIELD"));
        Assert.Contains("77 W-TEXT PIC X(12).", PartialTreePlan.ConversionInput(plan));
    }

    [Fact]
    public void RejectsDynamicCallAndBoundariesButRetainsRealLocalLogic()
    {
        var plan = Assess("       01 W-COUNT PIC 9(4).",
            "       LOCAL SECTION.\n           ADD 1 TO W-COUNT.\n" +
            "       EXTERNAL SECTION.\n           CALL 'NOT-PRESENT'.\n" +
            "       FINISH SECTION.\n           GOBACK.\n");
        Assert.Equal("LOCAL", Assert.Single(plan.Selected).Name);
        Assert.All(plan.Routines.Where(r => r.Name != "LOCAL"), r => Assert.NotEmpty(r.Blockers));
    }

    [Fact]
    public void ExcludesRedefinedOriginalAndArrayFieldsNotJustTheirAliases()
    {
        var plan = Assess("       01 ORIGINAL PIC X(4).\n       01 ALIAS REDEFINES ORIGINAL PIC 9(4).\n" +
            "       01 TABLE-ROOT.\n          05 ITEM PIC 9 OCCURS 4.\n       01 LOCAL PIC 9.",
            "       UNSAFE SECTION.\n           MOVE ZERO TO ORIGINAL ITEM.\n" +
            "       SAFE SECTION.\n           MOVE ZERO TO LOCAL.\n");
        Assert.Equal("SAFE", Assert.Single(plan.Selected).Name);
        Assert.DoesNotContain(plan.Fields, f => f.Name is "ORIGINAL" or "ALIAS" or "ITEM");
    }

    [Fact]
    public void DoesNotTrustCachedContentOrTreatDebugLiteralsAsDependencies()
    {
        var plan = Assess("       01 W-COUNT PIC 9(4).",
            "       RESET SECTION.\n      D    DISPLAY 'CALL MISSING'.\n           MOVE ZERO TO W-COUNT.\n");
        Assert.Single(plan.Selected);
        Assert.DoesNotContain("CALL", Assert.Single(plan.Selected).Source);
    }

    [Fact]
    public void ExcludesNestedParagraphFallthroughAndContinuationRecords()
    {
        var plan = Assess("       01 W-TEXT PIC X(12).",
            "       PARENT SECTION.\n       CHILD.\n           MOVE 'X' TO W-TEXT.\n" +
            "       CONTINUED SECTION.\n           MOVE 'HELLO\n      -    'WORLD' TO W-TEXT.\n");
        Assert.DoesNotContain(plan.Selected, r => r.Kind == "routine");
        Assert.All(plan.Routines.Where(r => r.Kind == "routine"), r => Assert.NotEmpty(r.Blockers));
    }

    [Fact]
    public void UsesCompleteRawGroupLayoutButDoesNotCountEmptyExitMethods()
    {
        var plan = Assess("       01 FILE-STATUS.\n          05 STATUS-1 PIC X.\n          05 STATUS-2 PIC X.\n" +
            "       01 RESULT-CODE PIC X(2).",
            "       COPY-STATUS.\n           MOVE FILE-STATUS TO RESULT-CODE.\n       END-MARKER.\n           EXIT.\n");
        Assert.Equal("COPY-STATUS", Assert.Single(plan.Selected).Name);
        Assert.Equal("raw-alphanumeric-group", plan.Fields.Single(f => f.Name == "FILE-STATUS").Kind);
        Assert.NotEmpty(plan.Routines.Single(r => r.Name == "END-MARKER").Blockers);
    }

    [Fact]
    public void BindsRealCopyAndSelectsOnlyPrefixBeforeCicsWithoutChangingLiterals()
    {
        Directory.CreateDirectory(_root);
        File.WriteAllText(Path.Combine(_root, "LAYOUT.cpy"), "       01 ORIGINAL PIC X(20) VALUE 'ORIGINAL'.\n");
        var plan = Assess("           COPY LAYOUT REPLACING ==ORIGINAL== BY ==LOCAL-NAME==.",
            "       MAIN SECTION.\n       A010.\n           MOVE 'ORIGINAL' TO LOCAL-NAME.\n" +
            "           EXEC CICS RETURN END-EXEC.\n           GOBACK.\n");
        var prefix = Assert.Single(plan.Selected);
        Assert.Equal("straight-line-prefix", prefix.Kind);
        Assert.Contains("MOVE 'ORIGINAL' TO LOCAL-NAME", prefix.Source);
        Assert.DoesNotContain("CICS", prefix.Source);
        Assert.Contains("VALUE 'ORIGINAL'", Assert.Single(plan.Fields).Declaration);
        Assert.Single(plan.Definitions);
        Assert.NotEmpty(plan.Routines.Single(r => r.Kind == "routine").Blockers);
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }
}
