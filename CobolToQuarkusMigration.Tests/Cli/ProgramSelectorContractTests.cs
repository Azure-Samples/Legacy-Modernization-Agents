using System.Diagnostics;
using System.Text;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Cli;

// The REKT scan resolves program selectors in Python because it must run without .NET, while
// conversion resolves them in C#. Two implementations of one rule drift silently, so this pins
// them to the same answers by extracting doctor.sh's actual heredoc rather than a copy of it.
public sealed class ProgramSelectorContractTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(), "selector-contract-" + Guid.NewGuid().ToString("N"));

    public static TheoryData<string> AgreedSelectors() =>
    [
        "finance/LEDGER.cbl",
        "LEDGER.cbl",
        "LEDGER",
        "ledger",
        "finance\\LEDGER.cbl",
        "./finance/LEDGER.cbl",
        "shared/UTIL.cob",
        "UTIL",
        "finance/LEDGER.cbl,shared/UTIL.cob",
        "LEDGER,LEDGER",
    ];

    [Theory]
    [MemberData(nameof(AgreedSelectors))]
    public void BothResolversSelectTheSamePrograms(string selector)
    {
        var staging = CreateUnambiguousEstate();

        var csharp = ProgramSourceCatalog.FromStagingDirectory(staging).ResolveSelectors(selector);
        var python = RunDoctorResolver(staging, selector);

        python.ExitCode.Should().Be(0, "the shell resolver should accept '{0}': {1}", selector, python.Output);
        python.Lines.Should().Equal(csharp,
            "both resolvers must agree on which programs '{0}' names", selector);
    }

    public static TheoryData<string, string> RefusedSelectors() =>
        new()
        {
            { "SHARED.cbl", "basename" },
            { "SHARED", "stem" },
            { "NOSUCHPROGRAM", "did not match" },
        };

    [Theory]
    [MemberData(nameof(RefusedSelectors))]
    public void BothResolversRefuseTheSameSelectors(string selector, string expectedReason)
    {
        var staging = CreateAmbiguousEstate();

        var csharpFailure = Record.Exception(
            () => ProgramSourceCatalog.FromStagingDirectory(staging).ResolveSelectors(selector));
        var python = RunDoctorResolver(staging, selector);

        csharpFailure.Should().BeOfType<InvalidOperationException>(
            "'{0}' is not resolvable to exactly one program", selector);
        python.ExitCode.Should().Be(1,
            "the shell resolver must refuse '{0}' too, not pick a candidate", selector);

        csharpFailure!.Message.Should().Contain(expectedReason);
        python.Output.Should().Contain(expectedReason);
        python.Lines.Should().NotContain(line => line.EndsWith(".cbl", StringComparison.OrdinalIgnoreCase),
            "refusing must not also emit a chosen program");
    }

    private string CreateUnambiguousEstate()
    {
        var staging = Path.Combine(_root, "unambiguous");
        WriteProgram(staging, "finance/LEDGER.cbl");
        WriteProgram(staging, "shared/UTIL.cob");
        WriteProgram(staging, "billing/INVOICE.cbl");
        return staging;
    }

    private string CreateAmbiguousEstate()
    {
        var staging = Path.Combine(_root, "ambiguous");
        WriteProgram(staging, "finance/SHARED.cbl");
        WriteProgram(staging, "billing/SHARED.cbl");
        return staging;
    }

    private static void WriteProgram(string staging, string relativePath)
    {
        var full = Path.Combine(staging, relativePath.Replace('/', Path.DirectorySeparatorChar));
        Directory.CreateDirectory(Path.GetDirectoryName(full)!);
        File.WriteAllText(full, $"       PROGRAM-ID. {Path.GetFileNameWithoutExtension(full)}.\n");
    }

    private (int ExitCode, string Output, IReadOnlyList<string> Lines) RunDoctorResolver(
        string stagingDir, string selectors)
    {
        var scriptPath = Path.Combine(_root, "resolver.py");
        Directory.CreateDirectory(_root);
        File.WriteAllText(scriptPath, ExtractResolverHeredoc(), new UTF8Encoding(false));

        var psi = new ProcessStartInfo("python3")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = _root,
        };
        psi.ArgumentList.Add(scriptPath);
        psi.ArgumentList.Add(stagingDir);
        psi.ArgumentList.Add(selectors);

        using var process = Process.Start(psi)!;
        var stdout = process.StandardOutput.ReadToEnd();
        var stderr = process.StandardError.ReadToEnd();
        process.WaitForExit(60_000).Should().BeTrue("the resolver should not hang");

        var lines = stdout.Split('\n', StringSplitOptions.RemoveEmptyEntries)
            .Select(line => line.TrimEnd('\r'))
            .Where(line => line.Length > 0)
            .ToList();

        return (process.ExitCode, stdout + stderr, lines);
    }

    // Read doctor.sh's bytes rather than a copy, so editing the heredoc runs the edited resolver here.
    private static string ExtractResolverHeredoc()
    {
        var doctor = File.ReadAllText(Path.Combine(LocateRepoRoot(), "doctor.sh"));

        const string opener = "\"$_REKT_PROGRAM_FILTER\" <<'PYEOF'\n";
        var start = doctor.IndexOf(opener, StringComparison.Ordinal);
        start.Should().BeGreaterThan(-1, "doctor.sh should still resolve _REKT_PROGRAM_FILTER via a Python heredoc");

        var bodyStart = start + opener.Length;
        var end = doctor.IndexOf("\nPYEOF", bodyStart, StringComparison.Ordinal);
        end.Should().BeGreaterThan(bodyStart, "the heredoc should be terminated");

        return doctor[bodyStart..end];
    }

    private static string LocateRepoRoot()
    {
        var dir = new DirectoryInfo(AppContext.BaseDirectory);
        while (dir is not null && !File.Exists(Path.Combine(dir.FullName, "doctor.sh")))
            dir = dir.Parent;

        dir.Should().NotBeNull("the tests should run from inside the repository");
        return dir!.FullName;
    }

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, recursive: true);
    }
}
