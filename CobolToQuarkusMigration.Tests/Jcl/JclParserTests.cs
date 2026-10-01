using CobolToQuarkusMigration.Jcl;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Jcl;

public class JclParserTests
{
    private static JclJob Parse(string jcl, IReadOnlyDictionary<string, string>? members = null) =>
        new JclParser(members is null ? null : JclMemberLibrary.FromTexts(members)).Parse(jcl, "JOBS/TEST.jcl");

    [Fact]
    public void ContinuationsAreJoinedAndCommentsAndSequenceNumbersDropped()
    {
        var job = Parse("""
            //PAYJOB   JOB (ACCT),'PAYROLL',CLASS=A,                                00000100
            //         COND=(4,LT)                                                  00000200
            //* a comment card
            //STEP1    EXEC PGM=PAYCALC,PARM='A B'   the rest is a comment          00000300
            //OUT      DD DSN=PAY.MASTER,                                           00000400
            //            DISP=(NEW,CATLG,DELETE),                                  00000500
            //            DCB=(RECFM=FB,LRECL=120,BLKSIZE=0)                        00000600
            """);

        job.Name.Should().Be("PAYJOB");
        job.JobClass.Should().Be("A");
        job.Cond.Should().Be("(4,LT)");
        var step = job.Steps.Should().ContainSingle().Subject;
        step.Program.Should().Be("PAYCALC");
        step.Kind.Should().Be(JclStepKind.Program);
        step.Parm.Should().Be("'A B'");
        var dd = step.Dds.Should().ContainSingle().Subject;
        dd.Dataset!.Name.Should().Be("PAY.MASTER");
        dd.Status.Should().Be("NEW");
        dd.NormalDisposition.Should().Be("CATLG");
        dd.RecordFormat.Should().Be("FB");
        dd.RecordLength.Should().Be(120);
        dd.Access.Should().Be(JclDatasetAccess.Create);
    }

    [Fact]
    public void AQuotedValueContinuesInColumnSixteen()
    {
        var job = Parse(
            "//J        JOB\n" +
            "//S        EXEC PGM=P,PARM='FIRST PART OF A LONG PARAMETER THAT RUNS TO\n" +
            "//             SECOND'\n");

        job.Steps.Single().Parm.Should().Be("'FIRST PART OF A LONG PARAMETER THAT RUNS TOSECOND'");
    }

    [Fact]
    public void InStreamDataEndsAtTheDelimiterOrTheNextStatement()
    {
        var job = Parse("""
            //J        JOB
            //S1       EXEC PGM=P1
            //IN1      DD *
            LINE ONE
            //IN2      DD DATA,DLM=$$
            //NOT A STATEMENT
            $$
            //S2       EXEC PGM=P2
            IMPLICIT SYSIN
            /*
            """);

        job.Steps[0].Dds.Single(d => d.Name == "IN1").InStream.Should().Equal("LINE ONE");
        job.Steps[0].Dds.Single(d => d.Name == "IN2").InStream.Should().Equal("//NOT A STATEMENT");
        job.Steps[1].Dds.Single(d => d.Name == "SYSIN").InStream.Should().Equal("IMPLICIT SYSIN");
    }

    [Fact]
    public void SymbolsAreSubstitutedAndTemporaryNamesAreNot()
    {
        var job = Parse("""
            //J        JOB
            //         SET HLQ=PROD,ENV='X1'
            //S1       EXEC PGM=P
            //A        DD DSN=&HLQ..&ENV..DATA,DISP=SHR
            //B        DD DSN=&&WORK,DISP=(NEW,PASS)
            //C        DD DSN=&UNSET..FILE,DISP=SHR
            """);

        var dds = job.Steps.Single().Dds;
        dds[0].Dataset!.Name.Should().Be("PROD.X1.DATA");
        dds[1].Dataset!.Name.Should().Be("&&WORK");
        dds[1].Dataset!.Temporary.Should().BeTrue();
        job.UnresolvedSymbols.Should().Equal("UNSET");
        job.Symbols.Should().Contain("HLQ", "PROD");
    }

    [Fact]
    public void AnInStreamProcedureIsExpandedWithItsOverrides()
    {
        var job = Parse("""
            //J        JOB
            //MYPROC   PROC OUT=DEFAULT.OUT
            //STEPA    EXEC PGM=PGMA,PARM=OLD
            //INPUT    DD DSN=A.IN,DISP=SHR
            //OUTPUT   DD DSN=&OUT,DISP=(NEW,CATLG),UNIT=SYSDA
            //STEPB    EXEC PGM=PGMB,PARM=KEEP
            //INPUT    DD DSN=B.IN,DISP=SHR
            //         PEND
            //RUN1     EXEC MYPROC,OUT=CUSTOM.OUT,PARM=NEW
            //STEPA.OUTPUT DD UNIT=
            //EXTRA    DD DSN=A.EXTRA,DISP=SHR
            //STEPB.INPUT DD DSN=B.OVERRIDE
            //ADDED    DD DUMMY
            """);

        job.Steps.Select(s => s.Name).Should().Equal("RUN1.STEPA", "RUN1.STEPB");
        var a = job.Steps[0];
        a.Program.Should().Be("PGMA");
        a.Procedure.Should().Be("MYPROC");
        a.ProcedureStep.Should().Be("STEPA");
        a.Parm.Should().Be("NEW");
        a.Dds.Select(d => d.Name).Should().Equal("INPUT", "OUTPUT", "EXTRA");
        a.Dds[1].Dataset!.Name.Should().Be("CUSTOM.OUT");
        a.Dds[1].Operands.Has("UNIT").Should().BeFalse();

        var b = job.Steps[1];
        b.Parm.Should().Be("", "a PARM without a step name nullifies it on every step after the first");
        b.Dds.Single(d => d.Name == "INPUT").Dataset!.Name.Should().Be("B.OVERRIDE");
        b.Dds.Single(d => d.Name == "INPUT").Status.Should().Be("SHR");
        b.Dds.Should().Contain(d => d.Name == "ADDED" && d.Dummy, "an unqualified DD goes to the step the previous override named");
    }

    [Fact]
    public void ProcedureDefaultsResolveTheSymbolsTheyName()
    {
        var job = Parse("""
            //J        JOB
            //         SET ENV=PROD
            //MYPROC   PROC HLQ=&ENV,DS=&HLQ..DATA
            //S1       EXEC PGM=P
            //IN       DD DSN=&DS,DISP=SHR
            //         PEND
            //RUN1     EXEC MYPROC
            //RUN2     EXEC MYPROC,HLQ=TEST
            """);

        job.Steps.Select(s => s.Dds.Single().Dataset!.Name).Should().Equal("PROD.DATA", "TEST.DATA");
        job.UnresolvedSymbols.Should().BeEmpty();
    }

    [Theory]
    [InlineData("//PAYPROC  PROC\n//CALC     EXEC PGM=PAYCALC", true)]
    [InlineData("//SYSOUT   DD SYSOUT=*", true)]
    [InlineData("//CALC     EXEC PGM=PAYCALC", false)]
    [InlineData("//J        JOB\n//P        PROC\n//S EXEC PGM=X\n//  PEND", false)]
    public void AMemberSavedAsJclIsNotAJob(string text, bool member) =>
        JclParser.IsMember(text).Should().Be(member);

    [Fact]
    public void CataloguedProceduresAndIncludesComeFromTheSourceAndMissingOnesAreReported()
    {
        var members = new Dictionary<string, string>
        {
            ["LIBPROC"] = "//LIBPROC  PROC\n//S1       EXEC PGM=FROMLIB\n",
            ["COMMON"] = "//S0       EXEC PGM=FROMINC\n",
        };

        var job = Parse("""
            //J        JOB
            //LIBS     JCLLIB ORDER=('MY.PROCLIB')
            //         INCLUDE MEMBER=COMMON
            //         INCLUDE MEMBER=ABSENT
            //RUN1     EXEC PROC=LIBPROC
            //RUN2     EXEC GONE
            //P1.SYSTSIN1 DD *
             DSN SYSTEM(DB0A)
             RUN PROGRAM(HIDDEN) PLAN(HIDPLAN)
            """, members);

        job.ProcLibraries.Should().Equal("MY.PROCLIB");
        job.Includes.Should().Equal("COMMON", "ABSENT");
        job.Steps.Select(s => (s.Name, s.Kind)).Should().Equal(
            ("S0", JclStepKind.Program), ("RUN1.S1", JclStepKind.Program), ("RUN2", JclStepKind.UnresolvedProcedure));
        job.Diagnostics.Select(d => d.Code).Should().BeEquivalentTo(["INCLUDE_NOT_FOUND", "PROC_NOT_FOUND"]);
        job.Steps[2].Runs.Should().ContainSingle().Which.Should().Be(new JclDb2Run("HIDDEN", "HIDPLAN", "DB0A", null));
        JclEstate.Programs(job).Should().Equal("FROMINC", "FROMLIB", "HIDDEN");
    }

    [Fact]
    public void StepsRecordTheIfConditionsTheyRunUnder()
    {
        var job = Parse("""
            //J        JOB
            //CHECK    EXEC PGM=CHECKER
            //IF1      IF CHECK.RC = 0 THEN
            //GOOD     EXEC PGM=GOOD
            //IF2      IF GOOD.RC > 4 THEN
            //WARN     EXEC PGM=WARN
            //IF2      ENDIF
            //         ELSE
            //BAD      EXEC PGM=BAD
            //         ENDIF
            //AFTER    EXEC PGM=AFTER,COND=(8,LT)
            """);

        job.Steps.Single(s => s.Name == "GOOD").Conditions.Should().Equal(new JclCondition("CHECK.RC = 0", false));
        job.Steps.Single(s => s.Name == "WARN").Conditions.Should().Equal(
            new JclCondition("CHECK.RC = 0", false), new JclCondition("GOOD.RC > 4", false));
        job.Steps.Single(s => s.Name == "BAD").Conditions.Should().Equal(new JclCondition("CHECK.RC = 0", true));
        var after = job.Steps.Single(s => s.Name == "AFTER");
        after.Conditions.Should().BeEmpty();
        after.Cond.Should().Be("(8,LT)");
        job.Diagnostics.Should().BeEmpty();
    }

    [Fact]
    public void TheTsoMonitorRunsTheProgramNamedInSystsin()
    {
        var job = Parse("""
            //J        JOB
            //DB2STEP  EXEC PGM=IKJEFT01
            //SYSTSIN  DD *
             DSN SYSTEM(DB0A)
             RUN PROGRAM(PAYDB2) PLAN(PAYPLAN) -
                 PARMS('2024')
             END
            """);

        var step = job.Steps.Single();
        step.Kind.Should().Be(JclStepKind.TsoBatch);
        step.ProgramsRun.Should().Equal("PAYDB2");
        step.Runs.Single().Should().Be(new JclDb2Run("PAYDB2", "PAYPLAN", "DB0A", "2024"));
    }

    [Fact]
    public void IdcamsCommandsBecomeDatasetEffects()
    {
        var job = Parse("""
            //J        JOB
            //CLEAN    EXEC PGM=IDCAMS
            //IN       DD DSN=SRC.DATA,DISP=SHR
            //SYSIN    DD *
              DELETE OLD.DATA PURGE
              REPRO INFILE(IN) -
                    OUTDATASET(COPY.DATA)
              DEFINE CLUSTER (NAME(NEW.KSDS) INDEXED)
              SET MAXCC = 0
            /*
            """);

        var step = job.Steps.Single();
        step.Kind.Should().Be(JclStepKind.Utility);
        step.ProgramsRun.Should().BeEmpty();
        step.Effects.Select(e => (e.Access, e.Dataset.Name)).Should().Equal(
            (JclDatasetAccess.Delete, "OLD.DATA"),
            (JclDatasetAccess.Read, "SRC.DATA"),
            (JclDatasetAccess.Create, "COPY.DATA"),
            (JclDatasetAccess.Create, "NEW.KSDS"));
    }

    [Fact]
    public void ABackwardReferenceResolvesToTheEarlierDataset()
    {
        var job = Parse("""
            //J        JOB
            //S1       EXEC PGM=A
            //OUT      DD DSN=GDG.BASE(+1),DISP=(NEW,CATLG)
            //S2       EXEC PGM=B
            //IN       DD DSN=*.S1.OUT,DISP=SHR
            //MISSING  DD DSN=*.S9.OUT,DISP=SHR
            """);

        var input = job.Steps[1].Dds[0].Dataset!;
        input.Name.Should().Be("GDG.BASE");
        input.Generation.Should().Be("+1");
        job.Diagnostics.Should().ContainSingle(d => d.Code == "BACKREF_UNRESOLVED");
    }

    [Fact]
    public void LineageOrdersJobsByTheDataTheyShare()
    {
        var parser = new JclParser();
        var producer = parser.Parse("""
            //PRODUCE  JOB
            //S1       EXEC PGM=MAKER
            //STEPLIB  DD DSN=LOAD.LIB,DISP=SHR
            //IN       DD DSN=OUTSIDE.FEED,DISP=SHR
            //OUT      DD DSN=DAILY.GDG(+1),DISP=(NEW,CATLG)
            //TMP      DD DSN=&&WORK,DISP=(NEW,PASS)
            """, "a.jcl");
        var consumer = parser.Parse("""
            //CONSUME  JOB
            //S1       EXEC PGM=USER
            //IN       DD DSN=DAILY.GDG(0),DISP=SHR
            //TMP      DD DSN=&&WORK,DISP=SHR
            //SCRATCH  DD DSN=MY.SCRATCH,DISP=(OLD,DELETE)
            """, "b.jcl");

        var lineage = JclEstateLineage.Build([producer, consumer]);

        lineage.Dependencies.Should().ContainSingle().Which.Should().BeEquivalentTo(
            new JclJobDependency("PRODUCE", "CONSUME", ["DAILY.GDG"]));
        lineage.ExternalInputs.Should().Equal("MY.SCRATCH", "OUTSIDE.FEED");
        lineage.Datasets.Select(d => d.Dataset).Should().NotContain("LOAD.LIB");
        lineage.Datasets.Where(d => d.Temporary).Select(d => d.Dataset).Should().Equal("CONSUME:&&WORK", "PRODUCE:&&WORK");
        lineage.Datasets.Single(d => d.Dataset == "MY.SCRATCH").Uses.Select(u => u.Access)
            .Should().Equal(JclDatasetAccess.Exclusive, JclDatasetAccess.Delete);
    }
}
