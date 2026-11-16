using System.Text.RegularExpressions;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Samples;

public sealed class BankOfZSourceCorrectionTests
{
    private static readonly string[] Pf12Programs = ["BNK1CAC", "BNK1CCA", "BNK1CRA", "BNK1DCS", "BNK1TFN", "BNK1UAC"];

    [BankOfZFact]
    public void NoAggregateDfhaidComparisonRemains()
    {
        foreach (var path in Directory.GetFiles(BankOfZFactAttribute.Cobol!, "*.cbl").Order(StringComparer.Ordinal))
            foreach (var line in CodeLines(path))
                Regex.IsMatch(line, @"=\s*DFHAID\b").Should().BeFalse($"{Path.GetFileNameWithoutExtension(path)}: {line}");
    }

    [BankOfZFact]
    public void Pf12AloneReachesEachTerminationBranch()
    {
        foreach (var program in Pf12Programs)
        {
            var lines = CodeLines(Path.Join(BankOfZFactAttribute.Cobol!, $"{program}.cbl"));
            var index = lines.FindIndex(line => line.Trim() == "WHEN EIBAID = DFHPF12");
            index.Should().BeGreaterThanOrEqualTo(0, program);
            var branch = lines.FindIndex(index + 1, line => line.Trim().StartsWith("WHEN ", StringComparison.Ordinal));
            var body = string.Join(" ", lines.Skip(index + 1).Take(branch - index - 1));
            body.Should().Contain("PERFORM SEND-TERMINATION-MSG", program);
            body.Should().MatchRegex(@"EXEC CICS\s+RETURN\s+END-EXEC", program);
        }
    }

    [Fact]
    public void GroupComparisonWithIbmDfhaidPrefixIsNeverTrue()
    {
        // IBM DFHAID starts DFHNULL (space), DFHENTER (quote), DFHCLEAR (underscore).
        byte[] group = " '_"u8.ToArray();
        for (var value = 0; value < 256; value++)
            new byte[] { (byte)value, (byte)' ', (byte)' ' }.Should().NotEqual(group);
    }

    // Columns 7-72 of non-comment lines, the area the compiler reads.
    private static List<string> CodeLines(string path) =>
        File.ReadAllLines(path)
            .Where(line => line.Length > 6 && line[6] is not ('*' or '/'))
            .Select(line => line[6..Math.Min(line.Length, 72)])
            .ToList();

    private sealed class BankOfZFactAttribute : FactAttribute
    {
        // The sample is not committed; it is only checked where it has been copied into source/.
        public static readonly string? Cobol = FindCobol();

        public BankOfZFactAttribute()
        {
            if (Cobol is null) Skip = "Bank-of-Z sample not present under source/";
        }

        private static string? FindCobol()
        {
            for (var dir = new DirectoryInfo(AppContext.BaseDirectory); dir is not null; dir = dir.Parent)
                if (File.Exists(Path.Join(dir.FullName, "doctor.sh")))
                {
                    var cobol = Path.Join(dir.FullName, "source", "bank-of-z", "cobol");
                    return Directory.Exists(cobol) ? cobol : null;
                }
            return null;
        }
    }
}
