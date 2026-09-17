using System;
using System.IO;
using McpChatWeb.Services;
using Xunit;

namespace McpChatWeb.Tests.Modernization;

// The portal is routinely started from a separate checkout with REPO_ROOT pointing at the
// repository that holds source/ and output/. Several endpoints resolved their files relative to
// the portal's own directory instead, so a reverse-engineering report that had just been
// generated was reported as not existing.
public sealed class RepositoryRootTests : IDisposable
{
    private readonly string? _original = Environment.GetEnvironmentVariable(RepositoryRoot.Variable);

    private readonly string _estate = Path.Combine(
        Path.GetTempPath(), "repo-root-" + Guid.NewGuid().ToString("N"));

    public RepositoryRootTests()
    {
        Directory.CreateDirectory(Path.Combine(_estate, "output"));
        File.WriteAllText(Path.Combine(_estate, "doctor.sh"), "#!/bin/bash\n");
    }

    public void Dispose()
    {
        Environment.SetEnvironmentVariable(RepositoryRoot.Variable, _original);
        try { Directory.Delete(_estate, recursive: true); } catch (IOException) { }
    }

    [Fact]
    public void TheConfiguredRootIsUsed()
    {
        Environment.SetEnvironmentVariable(RepositoryRoot.Variable, _estate);

        Assert.Equal(Path.GetFullPath(_estate), RepositoryRoot.Resolve());
    }

    // The exact failure: the portal running from elsewhere looked for the estate's output inside
    // its own directory.
    [Fact]
    public void APathResolvesIntoTheConfiguredRootNotTheCallersDirectory()
    {
        Environment.SetEnvironmentVariable(RepositoryRoot.Variable, _estate);
        var elsewhere = Path.Combine(Path.GetTempPath(), "some-other-checkout", "McpChatWeb");

        var resolved = RepositoryRoot.PathTo(elsewhere, "output", "reverse-engineering-details.md");

        Assert.StartsWith(Path.GetFullPath(_estate), resolved);
        Assert.EndsWith("reverse-engineering-details.md", resolved);
    }

    [Fact]
    public void AConfiguredRootThatDoesNotExistIsIgnored()
    {
        Environment.SetEnvironmentVariable(
            RepositoryRoot.Variable, Path.Combine(_estate, "no-such-directory"));

        var resolved = RepositoryRoot.Resolve(_estate);

        Assert.Equal(Path.GetFullPath(_estate), Path.GetFullPath(resolved));
    }

    [Fact]
    public void WithoutConfigurationItWalksUpToTheRepository()
    {
        Environment.SetEnvironmentVariable(RepositoryRoot.Variable, null);
        var nested = Path.Combine(_estate, "McpChatWeb", "bin");
        Directory.CreateDirectory(nested);

        Assert.Equal(Path.GetFullPath(_estate), Path.GetFullPath(RepositoryRoot.Resolve(nested)));
    }

    [Fact]
    public void WithNothingToFindItReturnsWhereItStarted()
    {
        Environment.SetEnvironmentVariable(RepositoryRoot.Variable, null);
        var orphan = Path.Combine(Path.GetTempPath(), "orphan-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(orphan);

        try
        {
            Assert.Equal(orphan, RepositoryRoot.Resolve(orphan));
        }
        finally
        {
            Directory.Delete(orphan, recursive: true);
        }
    }
}
