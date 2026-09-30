using System.Diagnostics;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Jcl;
using CobolToQuarkusMigration.Jcl.Generation;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Jcl;

public class JclJobGenerationTests
{
    private static JclJob Parse(string jcl, IReadOnlyDictionary<string, string>? members = null) =>
        new JclParser(members is null ? null : JclMemberLibrary.FromTexts(members)).Parse(jcl, "JOBS/TEST.jcl");

    private static JclExpr Expression(string text)
    {
        var parsed = JclConditions.ParseExpression(text, out var error);
        error.Should().BeNull();
        return parsed!;
    }

    [Fact]
    public void AndAndOrAreReadLeftToRightWithoutPrecedence()
    {
        Expression("RC = 0 OR S1.RC > 4 AND ABEND").Should().Be(
            new JclLogical(
                new JclLogical(new JclReturnCodeTest(null, JclCompare.Eq, 0), false, new JclReturnCodeTest("S1", JclCompare.Gt, 4)),
                true,
                new JclAbendTest(null)));
    }

    [Theory]
    [InlineData("¬ABEND")]
    [InlineData("NOT ABEND")]
    [InlineData("ABEND = FALSE")]
    [InlineData("ABEND ¬= TRUE")]
    public void NegationsOfAbendAllReadAsNot(string text) =>
        Expression(text).Should().Be(new JclNot(new JclAbendTest(null)));

    [Theory]
    [InlineData("S1.RC ¬= 4", JclCompare.Ne)]
    [InlineData("S1.RC NE 4", JclCompare.Ne)]
    [InlineData("S1.RC ¬> 4", JclCompare.Le)]
    [InlineData("S1.RC NL 4", JclCompare.Ge)]
    [InlineData("S1.RC >= 4", JclCompare.Ge)]
    public void ComparisonsAcceptWordsAndSymbols(string text, JclCompare op) =>
        Expression(text).Should().Be(new JclReturnCodeTest("S1", op, 4));

    [Theory]
    [InlineData("RUN")]
    [InlineData("S1.RC = X")]
    [InlineData("(RC = 0")]
    [InlineData("S1.FOO = 1")]
    public void UnreadableExpressionsReportAnError(string text)
    {
        JclConditions.ParseExpression(text, out var error).Should().BeNull();
        error.Should().NotBeNullOrEmpty();
    }

    [Fact]
    public void CondListsCarryEvenAndOnly()
    {
        JclConditions.ParseCond("(4,LT)", out var e1).Should().BeEquivalentTo(
            new JclCondParameter([new JclCondTest(4, JclCompare.Lt, null)], false, false));
        e1.Should().BeNull();

        JclConditions.ParseCond("((0,NE,S1),(8,LE),EVEN)", out var e2).Should().BeEquivalentTo(
            new JclCondParameter([new JclCondTest(0, JclCompare.Ne, "S1"), new JclCondTest(8, JclCompare.Le, null)], true, false));
        e2.Should().BeNull();

        JclConditions.ParseCond("ONLY", out _)!.Only.Should().BeTrue();
        JclConditions.ParseCond("(4,XX)", out var e3).Should().BeNull();
        e3.Should().NotBeNull();
    }

    [Fact]
    public void ElseRunsWhenTheIfIsFalseAndReferencesInsideAProcedureNameTheirSiblings()
    {
        var job = Parse("""
            //J        JOB
            //S1       EXEC PGM=P1
            //         IF (S1.RC = 0) THEN
            //S2       EXEC PGM=P2
            //         ELSE
            //S3       EXEC PGM=P3
            //         ENDIF
            //CALL     EXEC MYPROC
            """, new Dictionary<string, string>
        {
            ["MYPROC"] = """
                //MYPROC   PROC
                //A        EXEC PGM=PA
                //B        EXEC PGM=PB,COND=(4,LT,A)
                //         PEND
                """,
        });

        var plan = JclJobPlan.From(job);
        plan.Steps.Single(s => s.Name == "S2").Guard.Should().Be(new JclReturnCodeTest("S1", JclCompare.Eq, 0));
        plan.Steps.Single(s => s.Name == "S3").Guard.Should().Be(new JclNot(new JclReturnCodeTest("S1", JclCompare.Eq, 0)));
        plan.Steps.Single(s => s.Name == "CALL.B").Cond!.Tests.Single().Step.Should().Be("CALL.A");
        plan.Diagnostics.Should().NotContain(d => d.Code == "STEP_REFERENCE_MISSING");
    }

    [Fact]
    public void AStepOfAMissingProcedureResolvesToTheCallingStep()
    {
        var plan = JclJobPlan.From(Parse("""
            //J        JOB
            //V        EXEC MISSING
            //         IF (V.P21.RC = 0) THEN
            //S2       EXEC PGM=P2
            //         ENDIF
            //S3       EXEC PGM=P3,COND=(0,NE,NOSUCH)
            """));

        plan.Steps.Single(s => s.Name == "V").Kind.Should().Be(JclPlanStepKind.Unresolved);
        plan.Steps.Single(s => s.Name == "S2").Guard.Should().Be(new JclReturnCodeTest("V", JclCompare.Eq, 0));
        plan.Diagnostics.Where(d => d.Code == "STEP_REFERENCE_MISSING").Should().ContainSingle()
            .Which.Message.Should().Contain("NOSUCH");
    }

    [Fact]
    public void AnUnreadableConditionMakesTheStepFailRatherThanRun()
    {
        var plan = JclJobPlan.From(Parse("""
            //J        JOB
            //S1       EXEC PGM=P1
            //         IF (S1.WHAT = 0) THEN
            //S2       EXEC PGM=P2
            //         ENDIF
            """));

        var step = plan.Steps.Single(s => s.Name == "S2");
        step.Guard.Should().BeNull();
        step.GuardError.Should().NotBeNull();
        plan.Diagnostics.Should().Contain(d => d.Code == "IF_UNREADABLE");
    }

    [Fact]
    public void TypeNamesAreUniqueAndValid()
    {
        var plans = JclJobWriter.Plan([Parse("//JOB-A    JOB\n//S EXEC PGM=P\n"), Parse("//JOB_A    JOB\n//S EXEC PGM=P\n"), Parse("//1ST      JOB\n//S EXEC PGM=P\n")]);

        plans.Select(p => p.TypeName).Should().Equal("Job_a", "Job_a_2", "J1st");
    }

    [Fact]
    public void LiteralsAreEscapedForBothLanguages()
    {
        var plans = JclJobWriter.Plan([Parse("""
            //J        JOB
            //S1       EXEC PGM=P1,PARM='A"B\C ¬'
            //IN       DD *
            /*
            """)]);

        foreach (var language in new[] { "C#", "Java" })
        {
            var all = string.Concat(JclJobWriter.Write(plans, language, "X.Jobs").Select(f => f.Content));
            all.Should().Contain("\"A\\\"B\\\\C \\u00ac\"");
        }
        string.Concat(JclJobWriter.Write(plans, "Java", "x.jobs").Select(f => f.Content))
            .Should().Contain(".inStream(new String[0])");
    }

    [Fact]
    public void GuardsRenderAsLambdasOverTheJobState()
    {
        var plans = JclJobWriter.Plan([Parse("""
            //J        JOB
            //S1       EXEC PGM=P1
            //         IF (S1.RC > 4 | S1.ABENDCC = U0100) THEN
            //S2       EXEC PGM=P2
            //         ENDIF
            """)]);

        var cs = JclJobWriter.Write(plans, "C#", "X.Jobs").Single(f => f.RelativePath == "Jobs/JJob.g.cs").Content;
        cs.Should().Contain("Guard = s => (s.Rc(\"S1\", JclCompare.Gt, 4) || s.AbendCc(\"S1\", JclCompare.Eq, \"U0100\"))")
            .And.Contain("GuardTestsAbend = true");

        var java = JclJobWriter.Write(plans, "Java", "x.jobs").Single(f => f.RelativePath == "x/jobs/JJob.java").Content;
        java.Should().Contain(".guard(s -> (s.rc(\"S1\", JclCompare.GT, 4) || s.abendCc(\"S1\", JclCompare.EQ, \"U0100\")), true)");
        JclJobWriter.Write(plans, "Java", "x.jobs").Should().Contain(f => f.RelativePath == "x/jobs/JJobConfiguration.java");
    }

    [Fact]
    public void WriteToReplacesItsOwnFilesAndReportsImplementedPrograms()
    {
        var source = Directory.CreateTempSubdirectory("jj-src").FullName;
        var run = Directory.CreateTempSubdirectory("jj-run").FullName;
        try
        {
            File.WriteAllText(Path.Join(source, "J.jcl"), "//J        JOB\n//S1       EXEC PGM=P1\n//S2       EXEC PGM=P2\n");
            Directory.CreateDirectory(Path.Join(run, "Jobs"));
            File.WriteAllText(Path.Join(run, "Jobs", "OldJob.g.cs"), "// Generated from the estate's JCL. Rewritten on every run; do not edit.\n");
            File.WriteAllText(Path.Join(run, "Jobs", "Mine.g.cs"), "// hand-written\n");
            File.WriteAllText(Path.Join(run, "P1.cs"), "class P1 : IBatchProgram { public string ProgramId => \"P1\"; }");

            JclJobWriter.WriteTo(source, run, "C#").Should().Be(1);

            File.Exists(Path.Join(run, "Jobs", "OldJob.g.cs")).Should().BeFalse();
            File.Exists(Path.Join(run, "Jobs", "Mine.g.cs")).Should().BeTrue();
            File.Exists(Path.Join(run, JclJobWriter.CSharpRuntimeFile)).Should().BeTrue();
            File.ReadAllText(Path.Join(run, "Jobs", "JJob.g.cs")).Should().Contain("namespace " + ConversionNamespacePolicy.ForJobs("C#"));
            var manifest = File.ReadAllText(Path.Join(run, JclJobWriter.ManifestFile));
            manifest.Should().Contain("\"programsImplemented\": [\n      \"P1\"").And.Contain("\"programsMissing\": [\n      \"P2\"");
        }
        finally
        {
            Directory.Delete(source, true);
            Directory.Delete(run, true);
        }
    }

    [Fact]
    public void ScaffoldRemovesModelCopiesOfTheContractAndImportsTheJobsNamespace()
    {
        var run = Directory.CreateTempSubdirectory("jj-scaffold").FullName;
        try
        {
            Directory.CreateDirectory(Path.Join(run, "Jobs"));
            File.WriteAllText(Path.Join(run, JclJobWriter.CSharpRuntimeFile), "namespace X { public interface IBatchProgram { } }\n");
            File.WriteAllText(Path.Join(run, "P1.cs"), "namespace Modernized.Bd\n{\n    public interface IBatchProgram\n    {\n        string ProgramId { get; }\n    }\n    public class P1 { }\n}\n");

            GeneratedProjectScaffold.Write(run, "Modernized");

            File.ReadAllText(Path.Join(run, "P1.cs")).Should().NotContain("interface IBatchProgram").And.Contain("class P1");
            File.ReadAllText(Path.Join(run, JclJobWriter.CSharpRuntimeFile)).Should().Contain("interface IBatchProgram");
            File.ReadAllText(Path.Join(run, GeneratedProjectScaffold.GlobalUsingsFile))
                .Should().Contain("global using " + ConversionNamespacePolicy.ForJobs("C#") + ";");
        }
        finally
        {
            Directory.Delete(run, true);
        }
    }

    [Fact]
    public void ProgramsTheJclRunsAreToldTheContractWithTheirDds()
    {
        var source = Directory.CreateTempSubdirectory("jj-prompt").FullName;
        try
        {
            File.WriteAllText(Path.Join(source, "J.jcl"), """
                //J        JOB
                //S1       EXEC PGM=PAY01,PARM='X'
                //STEPLIB  DD DSN=LOAD.LIB,DISP=SHR
                //IN       DD DSN=PAY.IN,DISP=SHR
                //OUT      DD DSN=PAY.OUT,DISP=(NEW,CATLG)
                """);

            var programs = JclBatchPrograms.Build(source);
            var block = programs.ToPromptBlock("PAY01", "C#");
            block.Should().Contain("job J, step S1: DDs IN (read), OUT (written, new); PARM 'X'")
                .And.Contain("ProgramId => \"PAY01\"")
                .And.NotContain("STEPLIB");
            programs.ToPromptBlock("PAY01", "Java").Should().Contain("programId() returning \"PAY01\"");
            programs.ToPromptBlock("OTHER", "C#").Should().BeEmpty();
        }
        finally
        {
            Directory.Delete(source, true);
        }
    }

    // Builds the generated C# and runs a job through it, so the IF/COND/abend rules are checked
    // by executing them rather than by reading the generated text.
    [Fact]
    public async Task AGeneratedCSharpJobRunsByTheJclRules()
    {
        var root = Directory.CreateTempSubdirectory("jj-exec").FullName;
        try
        {
            var source = Directory.CreateDirectory(Path.Join(root, "src")).FullName;
            var app = Directory.CreateDirectory(Path.Join(root, "app")).FullName;
            File.WriteAllText(Path.Join(source, "T.jcl"), """
                //T        JOB
                //STEP1    EXEC PGM=PROGA,PARM='HELLO'
                //IN       DD *
                line1
                line2
                /*
                //OUT      DD DSN=TEST.OUT,DISP=(NEW,CATLG,DELETE)
                //STEP2    EXEC PGM=PROGB,COND=(0,NE,STEP1)
                //IN       DD DSN=TEST.OUT,DISP=SHR
                //         IF (STEP2.RC = 4) THEN
                //STEP3    EXEC PGM=PROGC
                //         ELSE
                //STEP4    EXEC PGM=PROGC
                //         ENDIF
                //STEP5    EXEC PGM=PROGD
                //STEP6    EXEC PGM=PROGC
                //STEP7    EXEC PGM=PROGC,COND=EVEN
                //         IF ABEND THEN
                //STEP8    EXEC PGM=PROGC
                //         ENDIF
                //STEP9    EXEC PGM=IDCAMS,COND=EVEN
                //SYSIN    DD *
                  DELETE ( TEST.GONE ) PURGE
                /*
                """);
            JclJobWriter.WriteTo(source, app, "C#").Should().Be(1);
            var ns = ConversionNamespacePolicy.ForJobs("C#");

            File.WriteAllText(Path.Join(app, "App.csproj"), """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net10.0</TargetFramework><ImplicitUsings>enable</ImplicitUsings></PropertyGroup>
                </Project>
                """);
            File.WriteAllText(Path.Join(app, "Program.cs"), $$"""
                using {{ns}};

                var data = Path.Join(Path.GetTempPath(), "jj-data-" + Guid.NewGuid().ToString("N"));
                var runner = new JclJobRunner([new ProgA(), new ProgB(), new Rc("PROGC", 0), new ProgD()], new DirectoryDatasetCatalog(data));
                var result = await runner.RunAsync(JclJobs.Find("T")!);
                foreach (var s in result.Steps) Console.WriteLine($"{s.Step} {s.Ran} {s.ReturnCode} {s.AbendCode}");
                Console.WriteLine("OUT " + File.ReadAllText(Directory.GetFiles(data, "*", SearchOption.AllDirectories).Single()).ReplaceLineEndings("|"));
                Directory.Delete(data, true);

                class ProgA : IBatchProgram
                {
                    public string ProgramId => "PROGA";
                    public async Task<int> RunAsync(BatchStepContext c, CancellationToken ct = default)
                    {
                        using var r = new StreamReader(c.OpenRead("IN"));
                        using var w = new StreamWriter(c.OpenWrite("OUT"));
                        await w.WriteLineAsync(c.Parm);
                        while (await r.ReadLineAsync() is { } l) await w.WriteLineAsync(l.ToUpperInvariant());
                        return 0;
                    }
                }
                class ProgB : IBatchProgram
                {
                    public string ProgramId => "PROGB";
                    public Task<int> RunAsync(BatchStepContext c, CancellationToken ct = default)
                    {
                        using var r = new StreamReader(c.OpenRead("IN"));
                        return Task.FromResult(r.ReadToEnd().Split('\n', StringSplitOptions.RemoveEmptyEntries).Length == 3 ? 4 : 0);
                    }
                }
                class Rc(string id, int rc) : IBatchProgram
                {
                    public string ProgramId => id;
                    public Task<int> RunAsync(BatchStepContext c, CancellationToken ct = default) => Task.FromResult(rc);
                }
                class ProgD : IBatchProgram
                {
                    public string ProgramId => "PROGD";
                    public Task<int> RunAsync(BatchStepContext c, CancellationToken ct = default) => throw new BatchAbendException("U0100", "boom");
                }
                """);

            var psi = new ProcessStartInfo("dotnet", "run")
            {
                WorkingDirectory = app,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
            };
            using var process = Process.Start(psi)!;
            var stderr = process.StandardError.ReadToEndAsync();
            var output = await process.StandardOutput.ReadToEndAsync();
            await process.WaitForExitAsync();
            process.ExitCode.Should().Be(0, output + await stderr);

            output.Split('\n', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries).Should().Equal(
                "STEP1 True 0",
                "STEP2 True 4",
                "STEP3 True 0",
                "STEP4 False",
                "STEP5 True  U0100",
                "STEP6 False",
                "STEP7 True 0",
                "STEP8 True 0",
                "STEP9 True 8",
                "OUT HELLO|LINE1|LINE2|");
        }
        finally
        {
            Directory.Delete(root, true);
        }
    }
}
