using System;
using System.IO;
using System.Linq;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

namespace McpChatWeb.Tests.Integration;

[CollectionDefinition("Source inventory working directory", DisableParallelization = true)]
public sealed class SourceInventoryCollection;

[Collection("Source inventory working directory")]
public sealed class SourceInventoryTests
{
    [Fact]
    public async Task SourceAnalysis_WithOnlyParserCaches_ReturnsEmptyAnalysis()
    {
        var originalDirectory = Directory.GetCurrentDirectory();
        var root = Path.Combine(originalDirectory, "output", "source-inventory-test-" + Guid.NewGuid().ToString("N"));
        try
        {
            var cacheDirectory = Path.Combine(root, "source", ".preprocessed");
            Directory.CreateDirectory(cacheDirectory);
            File.WriteAllText(Path.Combine(cacheDirectory, "MAIN.cbl"), "EXEC CICS SEND MAP\n");

            using var factory = new WebAppFactory();
            using var client = factory.CreateClient();
            Directory.SetCurrentDirectory(root);
            var analysis = await client.GetFromJsonAsync<JsonElement>("/api/source/analyze");

            Assert.True(analysis.GetProperty("isEmpty").GetBoolean());
            Assert.Equal(0, analysis.GetProperty("totalFiles").GetInt32());
            Assert.Equal(0, analysis.GetProperty("totalLines").GetInt32());
            Assert.Equal("general", analysis.GetProperty("architecturePattern").GetString());
            Assert.Empty(analysis.GetProperty("files").EnumerateArray());
            Assert.Empty(analysis.GetProperty("detectedFeatures").EnumerateArray());
        }
        finally
        {
            Directory.SetCurrentDirectory(originalDirectory);
            if (Directory.Exists(root)) Directory.Delete(root, recursive: true);
        }
    }

    [Fact]
    public async Task SourceFilesAndAnalysis_ExcludeParserCachesWithoutHidingSimilarSourceFolderNames()
    {
        var originalDirectory = Directory.GetCurrentDirectory();
        var root = Path.Combine(originalDirectory, "output", "source-inventory-test-" + Guid.NewGuid().ToString("N"));
        try
        {
            foreach (var relative in new[]
            {
                "bank/MAIN.cbl", "bank/RECORD.cpy", "legacy.copy", "embedded.pco",
                "nested/.preprocessed/MAIN.cbl", ".preprocessed/RECORD.cpy",
                ".rekt-staging/PROGRAM.cob", ".rekt-staging/INCLUDE.copy",
                "nested/.convert-job/PROGRAM.cbl", ".convert-job/embedded.sqb",
                "preprocessed/REAL.CBL", "nested/.preprocessed-original/REAL.cpy"
            })
            {
                var path = Path.Combine(root, "source", relative);
                Directory.CreateDirectory(Path.GetDirectoryName(path)!);
                File.WriteAllText(path, relative.Contains(".preprocessed/", StringComparison.Ordinal)
                    || relative.Contains(".rekt-staging/", StringComparison.Ordinal)
                    || relative.Contains(".convert-job/", StringComparison.Ordinal)
                        ? "EXEC CICS SEND MAP\ncache\n"
                        : "source\n");
            }

            using var factory = new WebAppFactory();
            using var client = factory.CreateClient();
            Directory.SetCurrentDirectory(root);
            var payload = await client.GetFromJsonAsync<JsonElement>("/api/source/files");

            Assert.False(payload.GetProperty("isEmpty").GetBoolean());
            var summary = payload.GetProperty("summary");
            Assert.Equal(6, summary.GetProperty("total").GetInt32());
            Assert.Equal(2, summary.GetProperty("programs").GetInt32());
            Assert.Equal(3, summary.GetProperty("copybooks").GetInt32());
            Assert.Equal(1, summary.GetProperty("other").GetInt32());
            var paths = payload.GetProperty("files").EnumerateArray()
                .Select(file => file.GetProperty("relativePath").GetString()!.Replace('\\', '/'))
                .OrderBy(path => path, StringComparer.Ordinal).ToArray();
            Assert.Equal(new[]
            {
                "source/bank/MAIN.cbl", "source/bank/RECORD.cpy", "source/embedded.pco",
                "source/legacy.copy", "source/nested/.preprocessed-original/REAL.cpy",
                "source/preprocessed/REAL.CBL"
            }, paths);

            var analysis = await client.GetFromJsonAsync<JsonElement>("/api/source/analyze");
            Assert.False(analysis.GetProperty("isEmpty").GetBoolean());
            Assert.Equal(summary.GetProperty("total").GetInt32(), analysis.GetProperty("totalFiles").GetInt32());
            Assert.Equal(6, analysis.GetProperty("totalLines").GetInt32());
            Assert.Equal("general", analysis.GetProperty("architecturePattern").GetString());
            Assert.Empty(analysis.GetProperty("detectedFeatures").EnumerateArray());
            var analyzedFiles = analysis.GetProperty("files").EnumerateArray().ToArray();
            Assert.Equal(paths.Select(Path.GetFileName).OrderBy(name => name, StringComparer.Ordinal),
                analyzedFiles.Select(file => file.GetProperty("fileName").GetString()).OrderBy(name => name, StringComparer.Ordinal));
            Assert.All(analyzedFiles, file =>
            {
                Assert.Equal(1, file.GetProperty("lineCount").GetInt32());
                Assert.Equal("low", file.GetProperty("complexity").GetString());
                Assert.Equal("service", file.GetProperty("suggestedTarget").GetString());
                Assert.Empty(file.GetProperty("features").EnumerateArray());
            });
        }
        finally
        {
            Directory.SetCurrentDirectory(originalDirectory);
            if (Directory.Exists(root)) Directory.Delete(root, recursive: true);
        }
    }
}
