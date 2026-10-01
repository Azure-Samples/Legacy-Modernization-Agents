using CobolToQuarkusMigration.Cli;
using CobolToQuarkusMigration.Jcl;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Jcl;

public class JclProgramsCommandTests
{
    private static readonly IReadOnlyList<JclJob> Jobs =
    [
        new() { Name = "PAYJOB", File = "JCL/PROD_PAYJOB.jcl" },
        new() { Name = "PAYJOB", File = "JCL/TEST_PAYJOB.jcl" },
        new() { Name = "BILLING", File = "JCL/BILL01.jcl" },
    ];

    [Fact]
    public void NoNamesSelectsEveryJob()
    {
        JclProgramsCommand.Select(Jobs, [], out var unknown).Should().HaveCount(3);
        unknown.Should().BeEmpty();
    }

    [Fact]
    public void AJobIsFoundByItsJobNameOrItsMemberName()
    {
        JclProgramsCommand.Select(Jobs, ["billing"], out _).Single().File.Should().Be("JCL/BILL01.jcl");
        JclProgramsCommand.Select(Jobs, ["bill01"], out _).Single().Name.Should().Be("BILLING");
    }

    [Fact]
    public void AJobNameSeveralMembersShareSelectsAllOfThem()
    {
        JclProgramsCommand.Select(Jobs, ["PAYJOB"], out _).Select(j => j.File)
            .Should().Equal("JCL/PROD_PAYJOB.jcl", "JCL/TEST_PAYJOB.jcl");
    }

    [Fact]
    public void AJobNamedTwiceIsSelectedOnce()
    {
        JclProgramsCommand.Select(Jobs, ["BILLING", "BILL01"], out _).Should().ContainSingle();
    }

    [Fact]
    public void AnUnknownNameIsReported()
    {
        JclProgramsCommand.Select(Jobs, ["BILLING", "NOPE"], out var unknown);
        unknown.Should().Equal("NOPE");
    }
}
