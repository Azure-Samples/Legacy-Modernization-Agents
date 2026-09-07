using System.Collections;
using GitHub.Copilot;

namespace CobolToQuarkusMigration.Helpers;

public sealed record CopilotHostRoute(string Hostname, string LoginUrl, string Source);

public static class CopilotRouting
{
    public const string DefaultHost = "github.com";
    public const string HostEnvironmentVariable = "COPILOT_GH_HOST";

    public static CopilotHostRoute Resolve(
        string? explicitHost = null,
        Func<string, string?>? getEnvironmentVariable = null)
    {
        getEnvironmentVariable ??= Environment.GetEnvironmentVariable;

        var candidates = new[]
        {
            (Value: explicitHost, Source: "explicit"),
            (Value: getEnvironmentVariable(HostEnvironmentVariable), Source: HostEnvironmentVariable),
            (Value: getEnvironmentVariable("GITHUB_HOST"), Source: "GITHUB_HOST"),
            (Value: getEnvironmentVariable("GH_HOST"), Source: "GH_HOST"),
            (Value: DefaultHost, Source: "default")
        };

        foreach (var candidate in candidates)
        {
            if (string.IsNullOrWhiteSpace(candidate.Value))
                continue;

            var hostname = Normalize(candidate.Value);
            return new CopilotHostRoute(hostname, $"https://{hostname}", candidate.Source);
        }

        throw new ArgumentException("A GitHub host is required.", nameof(explicitHost));
    }

    public static string Normalize(string input)
    {
        if (string.IsNullOrWhiteSpace(input))
            throw new ArgumentException("GitHub host cannot be empty.", nameof(input));

        var value = input.Trim();
        var hasScheme = value.Contains("://", StringComparison.Ordinal);
        if (hasScheme && !value.StartsWith("https://", StringComparison.OrdinalIgnoreCase))
            throw new ArgumentException("GitHub host URLs must use HTTPS.", nameof(input));
        var authorityInput = hasScheme ? value["https://".Length..] : value;
        authorityInput = authorityInput.TrimEnd('/');
        if (authorityInput.Contains(':'))
            throw new ArgumentException("GitHub host must not contain a port.", nameof(input));

        if (!Uri.TryCreate(hasScheme ? value : $"https://{value}", UriKind.Absolute, out var uri))
            throw new ArgumentException("GitHub host is not a valid hostname or HTTPS URL.", nameof(input));

        if (!uri.Scheme.Equals(Uri.UriSchemeHttps, StringComparison.OrdinalIgnoreCase) ||
            string.IsNullOrWhiteSpace(uri.Host))
            throw new ArgumentException("GitHub host is not a valid HTTPS hostname.", nameof(input));

        if (!string.IsNullOrEmpty(uri.UserInfo))
            throw new ArgumentException("GitHub host must not contain credentials.", nameof(input));
        if (uri.AbsolutePath is not "" and not "/")
            throw new ArgumentException("GitHub host must not contain a path.", nameof(input));
        if (!string.IsNullOrEmpty(uri.Query) || !string.IsNullOrEmpty(uri.Fragment))
            throw new ArgumentException("GitHub host must not contain a query or fragment.", nameof(input));
        if (!uri.Authority.Equals(uri.Host, StringComparison.OrdinalIgnoreCase))
            throw new ArgumentException("GitHub host must not contain a port.", nameof(input));
        if (Uri.CheckHostName(uri.Host) == UriHostNameType.Unknown ||
            uri.Host.Split('.').Any(label =>
                string.IsNullOrEmpty(label) || label.StartsWith('-') || label.EndsWith('-')))
            throw new ArgumentException("GitHub host contains an invalid hostname.", nameof(input));

        return uri.IdnHost.ToLowerInvariant();
    }

    public static CopilotHostRoute ApplyToCurrentProcess(string? explicitHost = null)
    {
        var route = Resolve(explicitHost);
        Environment.SetEnvironmentVariable(HostEnvironmentVariable, route.Hostname);
        return route;
    }

    public static string? ResolveToken(Func<string, string?>? getEnvironmentVariable = null)
    {
        getEnvironmentVariable ??= Environment.GetEnvironmentVariable;
        return new[]
        {
            "COPILOT_GITHUB_TOKEN",
            "GH_TOKEN",
            "GITHUB_TOKEN",
            "GITHUB_COPILOT_TOKEN"
        }
        .Select(getEnvironmentVariable)
        .FirstOrDefault(value => !string.IsNullOrWhiteSpace(value));
    }

    public static CopilotHostRoute ApplyTo(
        CopilotClientOptions options,
        string? explicitHost = null)
    {
        ArgumentNullException.ThrowIfNull(options);

        string? optionHost = null;
        if (options.Environment is not null &&
            options.Environment.TryGetValue(HostEnvironmentVariable, out var configuredHost))
        {
            optionHost = configuredHost;
        }

        var route = Resolve(explicitHost ?? optionHost);
        var environment = new Dictionary<string, string>(GetEnvironmentComparer());

        foreach (DictionaryEntry entry in Environment.GetEnvironmentVariables())
        {
            if (entry.Key is string key && entry.Value is string value)
                environment[key] = value;
        }

        if (options.Environment is not null)
        {
            foreach (var pair in options.Environment)
                environment[pair.Key] = pair.Value;
        }

        environment[HostEnvironmentVariable] = route.Hostname;
        options.Environment = environment;
        if (options.Connection is null && ResolveCliPath() is { } cliPath)
            options.Connection = RuntimeConnection.ForStdio(cliPath, null);
        return route;
    }

    public static string? ResolveCliPath(string? bundledBaseDirectory = null)
    {
        var overridePath = Environment.GetEnvironmentVariable("COPILOT_CLI_PATH");
        if (!string.IsNullOrWhiteSpace(overridePath) && File.Exists(overridePath))
            return overridePath;

        if (!string.IsNullOrWhiteSpace(bundledBaseDirectory))
        {
            var bundledPath = GetBundledCliPath(bundledBaseDirectory);
            if (File.Exists(bundledPath))
                return bundledPath;
        }

        var binaryName = OperatingSystem.IsWindows() ? "copilot.exe" : "copilot";
        var separator = OperatingSystem.IsWindows() ? ';' : ':';
        foreach (var directory in (Environment.GetEnvironmentVariable("PATH") ?? "")
                     .Split(separator, StringSplitOptions.RemoveEmptyEntries))
        {
            try
            {
                var candidate = Path.Combine(directory.Trim(), binaryName);
                if (File.Exists(candidate))
                    return candidate;
            }
            catch
            {
                // Ignore malformed PATH entries.
            }
        }

        var home = Environment.GetEnvironmentVariable("HOME") ?? "";
        return new[]
        {
            "/opt/homebrew/bin/copilot",
            "/usr/local/bin/copilot",
            "/usr/bin/copilot",
            Path.Combine(home, ".local/bin/copilot"),
            Path.Combine(home, ".npm-global/bin/copilot")
        }.FirstOrDefault(File.Exists);
    }

    internal static string GetBundledCliPath(string baseDirectory)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(baseDirectory);

        var architecture = System.Runtime.InteropServices.RuntimeInformation.OSArchitecture;
        var rid = OperatingSystem.IsWindows()
            ? architecture == System.Runtime.InteropServices.Architecture.Arm64 ? "win-arm64" : "win-x64"
            : OperatingSystem.IsMacOS()
                ? architecture == System.Runtime.InteropServices.Architecture.Arm64 ? "osx-arm64" : "osx-x64"
                : architecture == System.Runtime.InteropServices.Architecture.Arm64 ? "linux-arm64" : "linux-x64";
        var binaryName = OperatingSystem.IsWindows() ? "copilot.exe" : "copilot";
        return Path.Combine(baseDirectory, "runtimes", rid, "native", binaryName);
    }

    private static StringComparer GetEnvironmentComparer() =>
        OperatingSystem.IsWindows() ? StringComparer.OrdinalIgnoreCase : StringComparer.Ordinal;
}
