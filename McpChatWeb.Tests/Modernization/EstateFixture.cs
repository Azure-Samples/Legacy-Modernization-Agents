using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;

namespace McpChatWeb.Tests.Modernization;

// Fixtures are generated per test rather than committed: the REKT artifact layout is
// derived from the source tree, so a committed fixture would silently rot when it changes.
internal sealed class EstateFixture : IDisposable
{
    public EstateFixture()
    {
        RepoRoot = Path.Combine(Path.GetTempPath(), "rekt-estate-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(SourceRoot);
        Directory.CreateDirectory(RektDir);
        Directory.CreateDirectory(Path.Combine(RepoRoot, "Data"));

        // The reader walks up for doctor.sh when no root is injected; create it
        // so an accidental discovery lands here rather than in the real repo.
        File.WriteAllText(Path.Combine(RepoRoot, "doctor.sh"), "#!/usr/bin/env bash\n");
    }

    public string RepoRoot { get; }
    public string SourceRoot => Path.Combine(RepoRoot, "source");
    public string RektDir => Path.Combine(RepoRoot, "output", "rekt");
    public string ScanDbPath => Path.Combine(RepoRoot, "Data", "rekt-scan.db");

    public EstateFixture AddProgram(string relativePath, string? body = null)
    {
        var full = Path.Combine(SourceRoot, relativePath.Replace('/', Path.DirectorySeparatorChar));
        Directory.CreateDirectory(Path.GetDirectoryName(full)!);
        File.WriteAllText(full, body ?? DefaultProgramBody(Path.GetFileNameWithoutExtension(relativePath)));
        return this;
    }

    public EstateFixture AddJcl(string relativePath, string body)
    {
        var full = Path.Combine(SourceRoot, relativePath.Replace('/', Path.DirectorySeparatorChar));
        Directory.CreateDirectory(Path.GetDirectoryName(full)!);
        File.WriteAllText(full, body);
        return this;
    }

    // Nested source-relative location the production locator reads from.
    public EstateFixture AddFacts(
        string relativePath,
        int confidence,
        IEnumerable<string>? callees = null,
        IEnumerable<string>? copybooks = null,
        int loc = 42)
    {
        var normalized = relativePath.Replace('\\', '/');
        var full = Path.Combine(RektDir, normalized.Replace('/', Path.DirectorySeparatorChar) + ".facts.json");
        Directory.CreateDirectory(Path.GetDirectoryName(full)!);

        var json = $$"""
        {
          "schemaVersion": 1,
          "identitySchemeVersion": "v2-source-relative",
          "basename": "{{Path.GetFileName(normalized)}}",
          "stem": "{{Path.GetFileNameWithoutExtension(normalized)}}",
          "relativePath": "{{normalized}}",
          "sourceHash": "deadbeef",
          "confidence": {{confidence}},
          "warnings": [],
          "preprocessNotes": [],
          "externalEffects": [],
          "callers": [],
          "callees": [{{Join(callees)}}],
          "summary": {
            "loc": {{loc}},
            "paragraphs": 3,
            "sections": 1,
            "isCopybook": false,
            "programId": "{{Path.GetFileNameWithoutExtension(normalized).ToUpperInvariant()}}"
          },
          "data": { "copybooksUsed": [{{Join(copybooks)}}] },
          "io": { "files": [], "screens": [], "dbTables": [], "queues": [] },
          "controlFlow": { "entryPoints": [], "performChains": [], "exits": [] }
        }
        """;
        File.WriteAllText(full, json);
        return this;
    }

    public EstateFixture AddDeps(string stem, params string[] dependencyNames)
    {
        var items = string.Join(",", Array.ConvertAll(dependencyNames, n => $$"""{"name":"{{n}}"}"""));
        File.WriteAllText(
            Path.Combine(RektDir, $"{stem}-deps.json"),
            $$"""{"dependencies":[{{items}}]}""");
        return this;
    }

    public EstateFixture AddReportDirectory(string stem)
    {
        var dir = Path.Combine(RektDir, $"{stem}.report");
        Directory.CreateDirectory(dir);
        File.WriteAllText(Path.Combine(dir, "index.html"), "<html></html>");
        return this;
    }

    // Flat smojol layout: the loader reads output/rekt/flow-ast-STEM.json with no report directory.
    public EstateFixture AddFlowAst(string stem)
    {
        File.WriteAllText(Path.Combine(RektDir, $"flow-ast-{stem}.json"), """
        {
          "nodeType": "SECTION",
          "name": "MAIN-SECTION",
          "startLine": 10,
          "endLine": 60,
          "children": [
            { "nodeType": "PARAGRAPH", "name": "MAIN-PARA", "startLine": 11, "endLine": 20 },
            { "nodeType": "PARAGRAPH", "name": "READ-PARA", "startLine": 21, "endLine": 30 },
            { "nodeType": "PERFORM", "target": "READ-PARA", "conditional": false, "startLine": 15 },
            { "nodeType": "DIALECT", "text": "EXEC SQL SELECT NAME FROM CUSTOMER_TBL END-EXEC", "startLine": 25 }
          ]
        }
        """);
        return this;
    }

    public EstateFixture AddMissingCopybooks(string content)
    {
        File.WriteAllText(Path.Combine(RektDir, "missing-copybooks.txt"), content);
        return this;
    }

    // Seeded through the production writer: the cache drops and recreates scan_entry when
    // PRAGMA user_version does not match, so externally seeded rows would be discarded.
    public async Task<EstateFixture> AddScanEntryAsync(
        string basename,
        RektParseOutcome parseOutcome,
        RektScanConfidence confidence,
        string? relativePath = null,
        string identityScheme = "v1-basename")
    {
        var cache = new SqliteRektScanCache(ScanDbPath);
        await cache.UpsertAsync(new RektScanEntry
        {
            Basename = basename,
            IdentitySchemeVersion = identityScheme,
            RelativePath = relativePath,
            PreprocessedHash = "hash",
            SourceHash = "srchash",
            ParseOutcome = parseOutcome,
            Confidence = confidence,
            ParsedAtUtc = DateTime.UtcNow,
        });
        return this;
    }

    private static string Join(IEnumerable<string>? values)
    {
        if (values is null) return "";
        var builder = new StringBuilder();
        foreach (var value in values)
        {
            if (builder.Length > 0) builder.Append(',');
            builder.Append('"').Append(value).Append('"');
        }
        return builder.ToString();
    }

    private static string DefaultProgramBody(string programId) => $"""
               IDENTIFICATION DIVISION.
               PROGRAM-ID. {programId}.
               PROCEDURE DIVISION.
                   DISPLAY 'HELLO'.
                   STOP RUN.
        """;

    public void Dispose()
    {
        try
        {
            if (Directory.Exists(RepoRoot)) Directory.Delete(RepoRoot, recursive: true);
        }
        catch (IOException)
        {
            // A locked SQLite handle must not fail an otherwise passing test.
        }
    }
}
