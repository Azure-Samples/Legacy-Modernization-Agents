using System.Security.Cryptography;
using System.Text;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

public static class VerifiedCopybookContext
{
    public static string Build(string factsDirectory, string sourceRoot, string program) =>
        Build(factsDirectory, sourceRoot, program, unconvertedOnly: false);

    /// <summary>
    /// For a strict run: the bound COPY inputs that no file in the run converts into a shared
    /// type, such as BMS symbolic maps generated during preprocessing and system copybooks. The
    /// shared types already reach the model as owned members; these otherwise reached it as a
    /// name only, and the model rightly refused to invent their layouts.
    /// </summary>
    public static string BuildUnconverted(string factsDirectory, string sourceRoot, string program) =>
        ConversionSourceScope.IsSet ? Build(factsDirectory, sourceRoot, program, unconvertedOnly: true) : "";

    private static string Build(string factsDirectory, string sourceRoot, string program, bool unconvertedOnly)
    {
        var sourcePath = Path.Combine(sourceRoot, SourcePathHelper.ToOsRelativePath(program));
        var source = new CobolFile { FileName = Path.GetFileName(sourcePath), FilePath = sourcePath };
        new DependencyEvidenceGate(factsDirectory).RequireVerified([source], sourceRoot);
        var evidence = DependencyEvidenceGate.LoadEvidence(factsDirectory, program)!;
        var definitions = evidence.Definitions.Where(d => d.Kind == "COPY")
            .Where(d => !unconvertedOnly || !ConversionSourceScope.Contains(d.Path))
            .DistinctBy(d => d.Path).OrderBy(d => d.Path, StringComparer.Ordinal).ToList();
        if (unconvertedOnly && definitions.Count == 0) return "";
        var context = new StringBuilder();
        if (unconvertedOnly)
        {
            context.AppendLine("## Hash-bound COPY definitions without a shared owner");
            context.AppendLine("These exact resolved definitions are not converted as shared types in this run. Where this program uses one, declare it as a nested type inside this program's class from its real fields; do not invent layouts or substitute stubs. Definitions used only by supporting callees are context for their contracts. No routine safety or equivalence is asserted.");
        }
        else
        {
            context.AppendLine("## Required hash-bound COPY definitions (context, not standalone conversion units)");
            context.AppendLine("These are the exact resolved definitions of this eligible program, including nested COPY. Use their real fields/statements for required model declarations; do not invent layouts, substitute stubs or generate an unrelated program. No routine safety or equivalence is asserted.");
        }
        foreach (var definition in definitions)
        {
            var bytes = File.ReadAllBytes(definition.Path);
            if (!string.Equals(Convert.ToHexString(SHA256.HashData(bytes)), definition.Sha256, StringComparison.OrdinalIgnoreCase))
                throw new InvalidDataException($"Required COPY {definition.Name} changed after eligibility validation; regenerate evidence.");
            context.AppendLine($"### COPY {definition.Name}; original path {definition.Path}; SHA256 {definition.Sha256}");
            context.AppendLine(CobolSourceReader.Decode(bytes));
        }
        return context.ToString();
    }
}
