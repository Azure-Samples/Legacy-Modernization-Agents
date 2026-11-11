using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Agents.Infrastructure.RektCache;
using CobolToQuarkusMigration.Helpers;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Microsoft.Extensions.Logging.Abstractions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Helpers;

public sealed class SourceOutputSafetyTests : IDisposable
{
    private readonly string _root = Path.Combine(AppContext.BaseDirectory, "test-artifacts", "source-output-" + Guid.NewGuid().ToString("N"));
    private static FileHelper Writer() => new(NullLogger<FileHelper>.Instance);

    [Theory]
    [InlineData("", "")]
    [InlineData("\u001a\u001a", "")]
    [InlineData("A\u001a\n", "A\u001a\n")]
    [InlineData("A\u001a \u001a", "A\u001a ")]
    public void OnlyConsecutiveTerminalEofBytesAreRemoved(string source, string expected)
    {
        CobolSourceReader.Decode(Encoding.UTF8.GetBytes(source)).Should().Be(expected);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    public async Task SourceReadersPreserveLiteralsAndInteriorEofButRemoveTrailingEof(bool bom, bool latin1)
    {
        var source = Path.Combine(_root, "source");
        Directory.CreateDirectory(source);
        const string expected = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. SAMPLE.\n       DISPLAY 'ÅÄÖ\u001a'.\n";
        var bytes = (latin1 ? Encoding.Latin1 : Encoding.UTF8).GetBytes(expected);
        if (bom) bytes = new byte[] { 0xEF, 0xBB, 0xBF }.Concat(bytes).ToArray();
        bytes = bytes.Concat(new byte[] { 0x1A, 0x1A }).ToArray();
        var path = Path.Combine(source, "SAMPLE.cbl");
        await File.WriteAllBytesAsync(path, bytes);

        CobolSourceReader.Read(path).Should().Be(expected);
        (await CobolSourceReader.ReadAsync(path)).Should().Be(expected);
        var files = await Writer().ScanDirectoryForCobolFilesAsync(source);
        files.Should().ContainSingle().Which.Content.Should().Be(expected);
        files[0].FilePath.Should().Be(path);

        string? observedSource = null;
        var provider = new StructuralContextProvider(_root, "source", true, (_, text) =>
        {
            observedSource = text;
            return Task.FromResult<RektContext?>(null);
        });
        await provider.GetAsync("SAMPLE.cbl");
        observedSource.Should().Be(expected);

        var factsRoot = Path.Combine(_root, "facts");
        var extractor = new ProgramFactsExtractor(_root, source, Path.Combine(_root, "rekt"));
        (await extractor.ExtractAllAsync(new[] { "SAMPLE.cbl" }, factsRoot)).Should().Be(1);
        using var facts = JsonDocument.Parse(await File.ReadAllTextAsync(Path.Combine(factsRoot, "SAMPLE.cbl.facts.json")));
        facts.RootElement.GetProperty("sourceHash").GetString().Should().Be(CanonicalHasher.HashUtf8(expected));
    }

    [Theory]
    [InlineData("SAME.cbl", "SAME.cpy")]
    [InlineData("one/SAME.cbl", "two/SAME.cbl")]
    [InlineData("one/SAME.cbl", "one/same.cbl")]
    public async Task ConflictingOwnersNeverOverwriteIncludingReversedInputAndReload(string firstOwner, string secondOwner)
    {
        foreach (var reverse in new[] { false, true })
        {
            var output = Path.Combine(_root, reverse ? "reverse" : "forward");
            var first = FileFor(reverse ? secondOwner : firstOwner, "first");
            var second = FileFor(reverse ? firstOwner : secondOwner, "second");
            var saved = await Writer().SaveJavaFileAsync(first, output);
            first.FilePath.Should().Be(saved);
            var saveConflict = () => Writer().SaveJavaFileAsync(second, output);
            await saveConflict.Should().ThrowAsync<InvalidOperationException>().WithMessage("*collision*");
            (await File.ReadAllTextAsync(saved)).Should().Be(first.Content.ReplaceLineEndings());

            first.Content = first.Content.Replace("first", "retry");
            (await Writer().SaveJavaFileAsync(first, output)).Should().Be(saved);
            (await File.ReadAllTextAsync(saved)).Should().Be(first.Content.ReplaceLineEndings());
            await saveConflict.Should().ThrowAsync<InvalidOperationException>();
        }
    }

    [Fact]
    public async Task CaseOnlyOutputCollisionAndUnclaimedExistingFileFailWithoutModification()
    {
        var first = FileFor("A.cbl", "first");
        var path = await Writer().SaveJavaFileAsync(first, _root);
        var other = FileFor("B.cbl", "second");
        other.FileName = "sample.java";
        await ((Func<Task>)(async () => await Writer().SaveJavaFileAsync(other, _root)))
            .Should().ThrowAsync<InvalidOperationException>();
        (await File.ReadAllTextAsync(path)).Should().Contain("first");

        var external = Path.Combine(_root, "unclaimed", "p");
        Directory.CreateDirectory(external);
        var unclaimed = Path.Combine(external, "Sample.java");
        await File.WriteAllTextAsync(unclaimed, "owned by somebody else");
        await ((Func<Task>)(async () => await Writer().SaveJavaFileAsync(first, Path.Combine(_root, "unclaimed"))))
            .Should().ThrowAsync<InvalidOperationException>().WithMessage("*without a source ownership record*");
        (await File.ReadAllTextAsync(unclaimed)).Should().Be("owned by somebody else");
    }

    [Fact]
    public async Task DifferentSourceOwnersCanUseSameBasenameWhenPackagesAreDistinct()
    {
        var program = FileFor("dir/SAME.cbl", "program");
        var copybook = FileFor("dir/SAME.cpy", "copybook");
        copybook.Content = copybook.Content.Replace("package p;", "package shared;");
        var firstPath = await Writer().SaveJavaFileAsync(program, _root);
        var secondPath = await Writer().SaveJavaFileAsync(copybook, _root);
        firstPath.Should().NotBe(secondPath);
        (await File.ReadAllTextAsync(firstPath)).Should().Contain("program");
        (await File.ReadAllTextAsync(secondPath)).Should().Contain("copybook");
    }

    [Fact]
    public async Task ConcurrentOwnersCannotBothSucceed()
    {
        var first = FileFor("one/SAME.cbl", "first");
        var second = FileFor("two/SAME.cbl", "second");
        async Task<bool> TryWrite(JavaFile file)
        {
            try { await Writer().SaveJavaFileAsync(file, _root); return true; }
            catch (InvalidOperationException) { return false; }
        }
        var results = await Task.WhenAll(TryWrite(first), TryWrite(second));
        results.Count(result => result).Should().Be(1);
        var winner = results[0] ? first : second;
        (await File.ReadAllTextAsync(winner.FilePath)).Should().Be(winner.Content.ReplaceLineEndings());
    }

    [Fact]
    public async Task UnidentifiedOutputCannotBeReusedAsAFictitiousSharedOwner()
    {
        var first = FileFor("", "first");
        first.OriginalCobolFileName = "SAME.cbl";
        var path = await Writer().SaveJavaFileAsync(first, _root);
        var second = FileFor("", "second");
        second.OriginalCobolFileName = "SAME.cbl";
        await ((Func<Task>)(async () => await Writer().SaveJavaFileAsync(second, _root)))
            .Should().ThrowAsync<InvalidOperationException>().WithMessage("*collision*");
        (await File.ReadAllTextAsync(path)).Should().Contain("first");
    }

    [Fact]
    public async Task CSharpOutputUsesTheSamePersistentOwnershipContract()
    {
        var first = new CodeFile
        {
            FileName = "Sample.cs", OriginalSourcePath = "one/SAMPLE.cbl",
            Content = "namespace Example.Output;\npublic class Sample {}"
        };
        var saved = await Writer().SaveCodeFileAsync(first, _root);
        saved.Should().Be(first.FilePath);
        var second = new CodeFile
        {
            FileName = "Sample.cs", OriginalSourcePath = "two/SAMPLE.cbl",
            Content = first.Content
        };
        await ((Func<Task>)(async () => await Writer().SaveCodeFileAsync(second, _root)))
            .Should().ThrowAsync<InvalidOperationException>();
        (await File.ReadAllTextAsync(saved)).Should().Be(first.Content);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void StemOnlyRegistriesRejectAmbiguousFilesBeforeAssigningFictitiousOwnership(bool reverse)
    {
        var paths = new[] { "a/SAME.cbl", "b/same.cob", "a/DATA.cpy", "b/data.cpy" };
        foreach (var relative in reverse ? paths.Reverse() : paths)
        {
            var path = Path.Combine(_root, relative);
            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, "       COPY DATA.\n       CALL 'TARGET'.");
        }
        Action calls = () => CallTargetRegistry.Build(_root);
        Action copies = () => CopybookOwnershipRegistry.Build(_root);
        calls.Should().Throw<InvalidOperationException>().WithMessage("*Ambiguous source stem*");
        copies.Should().Throw<InvalidOperationException>().WithMessage("*Ambiguous source stem*");

        // A run that selected one estate reads only its files, so the other estate's copies of
        // the same stems no longer make the registries throw.
        ConversionSourceScope.Set(new[] { "a/SAME.cbl", "a/DATA.cpy" }
            .Select(relative => new CobolFile { FileName = Path.GetFileName(relative), FilePath = Path.Combine(_root, relative) }));
        try
        {
            CopybookOwnershipRegistry.Build(_root).Ownerships.Should().ContainSingle();
            CallTargetRegistry.Build(_root);
        }
        finally
        {
            ConversionSourceScope.Clear();
        }
    }

    [Fact]
    public void ProgramAndCopybookSharingStemRemainDifferentRegistryOwners()
    {
        Directory.CreateDirectory(_root);
        File.WriteAllText(Path.Combine(_root, "SAME.cbl"), "       COPY SAME.\n       CALL 'TARGET'.");
        File.WriteAllText(Path.Combine(_root, "SAME.cpy"), "       01 DATA-AREA.\n          05 FIELD PIC X.");
        var ownership = CopybookOwnershipRegistry.Build(_root).Ownerships.Should().ContainSingle().Subject;
        ownership.IsOwnedBy("SAME", isCopybook: false).Should().BeFalse();
        ownership.IsOwnedBy("SAME", isCopybook: true).Should().BeTrue();
        CallTargetRegistry.Build(_root).Contracts.Should().ContainSingle().Which.DeclaredBy.Should().Be("SAME");
    }

    [Fact]
    public void SharedRegistryRescanDoesNotReuseRemovedReferences()
    {
        Directory.CreateDirectory(_root);
        File.WriteAllBytes(Path.Combine(_root, "FIRST.cbl"), Encoding.Latin1.GetBytes("      * Å\n       COPY DATA."));
        File.WriteAllText(Path.Combine(_root, "SECOND.cbl"), "       COPY DATA.");
        var registry = new SharedTypeRegistry();
        registry.Scan(_root);
        registry.IsShared("DATA").Should().BeTrue();
        File.Delete(Path.Combine(_root, "SECOND.cbl"));
        registry.Scan(_root);
        registry.IsShared("DATA").Should().BeFalse();
    }

    private static JavaFile FileFor(string source, string marker) => new()
    {
        FileName = "Sample.java",
        OriginalSourcePath = source,
        Content = $"package p;\npublic class Sample {{ String value = \"{marker}\"; }}",
        PackageName = "p"
    };

    public void Dispose()
    {
        if (Directory.Exists(_root)) Directory.Delete(_root, true);
    }
}
