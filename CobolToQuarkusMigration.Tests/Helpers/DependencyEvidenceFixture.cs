using System;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;

namespace CobolToQuarkusMigration.Tests.Helpers;

internal static class DependencyEvidenceFixture
{
    public static object DialectStatement(string kind, string operation, string runtimeMapping = "unverified")
    {
        var grammar = kind == "EXEC_SQL" ? "SqlCodeContext"
            : kind == "FILE_IO" ? "ReadStatementContext"
            : operation == "WRITE" ? "Cics_writeContext"
            : operation == "LINK" ? "Cics_linkContext" : "Cics_returnContext";
        return new
        {
            kind,
            operation,
            runtimeMapping,
            bindingStatus = "complete",
            grammarRule = grammar,
            sourceText = operation,
            syntax = new { rule = grammar, children = new[] { new { token = operation } } },
            semanticStatus = "complete",
            semanticSyntax = new { rule = "StartSqlRuleContext", children = new[] { new { token = operation } } }
        };
    }

    // Synthetic parser evidence belongs only to isolated tests, never to production/pilot artifacts.
    public static DependencyEvidence Write(
        string repositoryRoot, string sourceRoot, string relativePath, params DependencyDefinition[] definitions)
    {
        var factsRoot = Path.Combine(repositoryRoot, "output", "rekt");
        var evidencePath = DependencyEvidenceGate.GetEvidencePath(factsRoot, relativePath);
        Directory.CreateDirectory(Path.GetDirectoryName(evidencePath)!);
        var exportPath = Path.Combine(factsRoot, relativePath + "-deps.json");
        File.WriteAllText(exportPath, JsonSerializer.Serialize(new
        {
            callTarget = new { name = relativePath },
            dependencies = definitions.Select(definition => new { name = definition.Name }).ToArray()
        }));
        var sourcePath = Path.Combine(sourceRoot, relativePath);
        var evidence = new DependencyEvidence
        {
            SchemaVersion = 1,
            Producer = "cobol-rekt/dependency",
            RelativePath = relativePath,
            OriginalSourcePath = Path.GetFullPath(sourcePath),
            OriginalSourceSha256 = Hash(sourcePath),
            ExportPath = Path.GetFullPath(exportPath),
            ExportSha256 = Hash(exportPath),
            ExportExitCode = 0,
            PlaceholderContaminated = false,
            Coverage = DependencyEvidenceGate.RequiredKinds.ToDictionary(kind => kind, _ => "complete"),
            Definitions = definitions.ToList()
        };
        Save(repositoryRoot, evidence);
        return evidence;
    }

    public static void Save(string repositoryRoot, DependencyEvidence evidence) =>
        File.WriteAllText(DependencyEvidenceGate.GetEvidencePath(
            Path.Combine(repositoryRoot, "output", "rekt"), evidence.RelativePath), JsonSerializer.Serialize(evidence));

    public static DependencyEvidence BindExport(string repositoryRoot, DependencyEvidence evidence, string export)
    {
        File.WriteAllText(evidence.ExportPath, export);
        evidence = evidence with { ExportSha256 = Hash(evidence.ExportPath) };
        Save(repositoryRoot, evidence);
        return evidence;
    }

    public static string Hash(string path) => Convert.ToHexString(SHA256.HashData(File.ReadAllBytes(path)));
}
