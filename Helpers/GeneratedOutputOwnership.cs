using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

internal static class GeneratedOutputOwnership
{
    private sealed record Claim(string Target, string? Source);

    internal static async Task WriteAsync(string root, string target, CodeFile file, Func<Task> write)
    {
        var relative = Path.GetRelativePath(root, target).Replace('\\', '/');
        var source = string.IsNullOrWhiteSpace(file.OriginalSourcePath)
            ? null
            : file.OriginalSourcePath.Replace('\\', '/');
        var claimsDirectory = Path.Combine(root, ".generated-owners");
        Directory.CreateDirectory(claimsDirectory);
        var key = Convert.ToHexString(SHA256.HashData(Encoding.UTF8.GetBytes(relative.ToUpperInvariant())));
        var claimPath = Path.Combine(claimsDirectory, key + ".json");
        var lockPath = Path.Combine(claimsDirectory, key + ".lock");

        await using var lease = await AcquireAsync(lockPath);
        if (File.Exists(claimPath))
        {
            var existing = JsonSerializer.Deserialize<Claim>(await File.ReadAllTextAsync(claimPath))
                ?? throw new InvalidOperationException($"Invalid generated output ownership record for '{relative}'.");
            if (source is null || existing.Source is null
                || !string.Equals(existing.Source, source, StringComparison.Ordinal)
                || !string.Equals(existing.Target, relative, StringComparison.Ordinal))
                throw new InvalidOperationException($"Generated output collision at '{relative}': conflicting source owners.");
        }
        else
        {
            if (HasExistingTarget(root, relative))
                throw new InvalidOperationException($"Generated output '{relative}' exists without a source ownership record.");
            // Claim before writing, so a failed write can be retried only by its original source.
            var pending = claimPath + ".pending";
            await File.WriteAllTextAsync(pending, JsonSerializer.Serialize(new Claim(relative, source)));
            File.Move(pending, claimPath, overwrite: true);
        }
        await write();
    }

    private static bool HasExistingTarget(string root, string relative)
    {
        IEnumerable<string> candidates = [root];
        foreach (var segment in relative.Split('/'))
            candidates = candidates.Where(Directory.Exists)
                .SelectMany(Directory.EnumerateFileSystemEntries)
                .Where(path => string.Equals(Path.GetFileName(path), segment, StringComparison.OrdinalIgnoreCase))
                .ToArray();
        return candidates.Any();
    }

    private static async Task<FileStream> AcquireAsync(string path)
    {
        for (var attempt = 0; ; attempt++)
        {
            try { return new FileStream(path, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None); }
            catch (IOException) when (attempt < 100) { await Task.Delay(20); }
        }
    }
}
