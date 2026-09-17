using System;
using System.IO;
using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Chat;

// Chat accepts a report path from the browser and reads that file into the model's context.
// Containment was tested with a string prefix, which is not a containment test.
public class RepositoryPathTests : IDisposable
{
    private readonly string _root = Path.Combine(
        Path.GetTempPath(), "repo-path-" + Guid.NewGuid().ToString("N"), "repo");

    public RepositoryPathTests()
    {
        Directory.CreateDirectory(Path.Combine(_root, "output"));
        // The sibling that a prefix test would have accepted.
        Directory.CreateDirectory(_root + "-notes");
    }

    public void Dispose()
    {
        try { Directory.Delete(Path.GetDirectoryName(_root)!, recursive: true); }
        catch (IOException) { }
    }

    [Fact]
    public void APathInsideTheRepositoryResolves()
    {
        var resolved = RepositoryPath.ResolveInside(_root, "output/report.md");

        Assert.NotNull(resolved);
        Assert.StartsWith(_root, resolved);
    }

    // "<repo>-notes" starts with "<repo>" but is not inside it.
    [Fact]
    public void ASiblingDirectorySharingThePrefixIsRefused()
    {
        Assert.Null(RepositoryPath.ResolveInside(_root, "../repo-notes/secret.md"));
        Assert.False(RepositoryPath.IsInside(_root, _root + "-notes/secret.md"));
    }

    [Theory]
    [InlineData("../../etc/passwd")]
    [InlineData("output/../../../etc/passwd")]
    [InlineData("..")]
    public void TraversalOutOfTheRepositoryIsRefused(string candidate)
    {
        Assert.Null(RepositoryPath.ResolveInside(_root, candidate));
    }

    // Path.Combine discards the root when the second argument is absolute, so the result must
    // still be checked rather than trusted for having been combined.
    [Fact]
    public void AnAbsolutePathOutsideTheRepositoryIsRefused()
    {
        var outside = OperatingSystem.IsWindows() ? @"C:\Windows\win.ini" : "/etc/passwd";

        Assert.Null(RepositoryPath.ResolveInside(_root, outside));
    }

    [Fact]
    public void AnAbsolutePathInsideTheRepositoryIsAccepted()
    {
        var inside = Path.Combine(_root, "output", "report.md");

        Assert.NotNull(RepositoryPath.ResolveInside(_root, inside));
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("   ")]
    public void AnEmptyPathResolvesToNothing(string? candidate)
    {
        Assert.Null(RepositoryPath.ResolveInside(_root, candidate));
    }

    [Fact]
    public void TheRepositoryRootItselfCountsAsInside()
    {
        Assert.True(RepositoryPath.IsInside(_root, _root));
        Assert.True(RepositoryPath.IsInside(_root, _root + Path.DirectorySeparatorChar));
    }
}
