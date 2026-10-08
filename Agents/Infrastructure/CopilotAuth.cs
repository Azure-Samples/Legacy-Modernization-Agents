using GitHub.Copilot;

namespace CobolToQuarkusMigration.Agents.Infrastructure;

/// <summary>
/// Decides which GitHub credential the Copilot runtime uses.
///
/// The Copilot CLI picks the first of <c>COPILOT_GITHUB_TOKEN</c>, <c>GH_TOKEN</c>,
/// <c>GITHUB_TOKEN</c>, the keychain token from <c>copilot login</c>, and <c>gh auth token</c>.
/// An unrelated <c>GH_TOKEN</c> or <c>GITHUB_TOKEN</c> in the shell therefore silently beats
/// <c>copilot login</c>. <c>COPILOT_AUTH</c> makes the choice explicit:
/// <list type="bullet">
///   <item><c>login</c> — use <c>copilot login</c>; token variables are hidden from the runtime.</item>
///   <item><c>token</c> — use the token in <c>COPILOT_GITHUB_TOKEN</c> and nothing else.</item>
///   <item>unset or <c>auto</c> — a <c>COPILOT_GITHUB_TOKEN</c> wins, otherwise the CLI's own order,
///   minus classic <c>ghp_</c> tokens, which Copilot never accepts.</item>
/// </list>
/// </summary>
public static class CopilotAuth
{
    public const string ModeVariable = "COPILOT_AUTH";
    public const string TokenVariable = "COPILOT_GITHUB_TOKEN";

    /// <summary>Written by earlier versions of setup; read only as a fallback.</summary>
    public const string LegacyTokenVariable = "GITHUB_COPILOT_TOKEN";

    private static readonly string[] AmbientTokenVariables = { TokenVariable, "GH_TOKEN", "GITHUB_TOKEN", LegacyTokenVariable };

    public enum Mode { Auto, Login, Token }

    public sealed record Resolution(Mode Mode, string? Token, string? TokenSource, IReadOnlyList<string> HiddenVariables, string Description);

    public static Mode ParseMode(string? value) => (value ?? "").Trim().ToLowerInvariant() switch
    {
        "" or "auto" => Mode.Auto,
        "login" or "copilot-login" or "cli" => Mode.Login,
        "token" or "pat" => Mode.Token,
        _ => throw new InvalidOperationException(
            $"{ModeVariable}='{value}' is not valid. Use 'login' (copilot login), 'token' ({TokenVariable}) or leave it unset.")
    };

    /// <summary>
    /// Null when the token can be used with Copilot, otherwise why not.
    /// </summary>
    public static string? ValidateToken(string token)
    {
        if (token.StartsWith("ghp_", StringComparison.Ordinal))
        {
            return "Classic personal access tokens (ghp_) do not work with GitHub Copilot. " +
                   "Create a fine-grained token (github_pat_) with the 'Copilot Requests' account permission, " +
                   "or use 'copilot login'.";
        }
        return null;
    }

    public static Resolution Resolve(Func<string, string?>? getEnvironment = null)
    {
        getEnvironment ??= Environment.GetEnvironmentVariable;
        string? Get(string name) => getEnvironment(name) is { Length: > 0 } v ? v.Trim() : null;

        var mode = ParseMode(Get(ModeVariable));
        var (token, source) = Get(TokenVariable) is { } t ? (t, TokenVariable)
            : Get(LegacyTokenVariable) is { } legacy ? (legacy, LegacyTokenVariable)
            : ((string?)null, (string?)null);

        if (mode == Mode.Login)
        {
            var hidden = AmbientTokenVariables.Where(v => Get(v) is not null).ToList();
            return new Resolution(mode, null, null, hidden,
                "copilot login" + (hidden.Count > 0 ? $" (ignoring {string.Join(", ", hidden)})" : ""));
        }

        if (token is not null)
        {
            if (ValidateToken(token) is { } problem)
                throw new InvalidOperationException($"{source}: {problem}");
            return new Resolution(Mode.Token, token, source, Array.Empty<string>(), $"token from {source}");
        }

        if (mode == Mode.Token)
        {
            throw new InvalidOperationException(
                $"{ModeVariable}=token but {TokenVariable} is not set. Run './doctor.sh setup' or set {TokenVariable}.");
        }

        var classic = new[] { "GH_TOKEN", "GITHUB_TOKEN" }
            .Where(v => Get(v) is { } value && ValidateToken(value) is not null)
            .ToList();
        var ambient = new[] { "GH_TOKEN", "GITHUB_TOKEN" }.FirstOrDefault(v => Get(v) is not null && !classic.Contains(v));
        var description = ambient is not null ? $"{ambient}, else copilot login" : "copilot login";
        if (classic.Count > 0)
            description += $" (ignoring classic token in {string.Join(", ", classic)})";
        return new Resolution(Mode.Auto, null, ambient, classic, description);
    }

    /// <summary>
    /// Client options for the Copilot runtime with the resolved credential applied.
    /// </summary>
    public static CopilotClientOptions CreateClientOptions(Func<string, string?>? getEnvironment = null)
        => Apply(new CopilotClientOptions { Mode = CopilotClientMode.CopilotCli }, Resolve(getEnvironment));

    public static CopilotClientOptions Apply(CopilotClientOptions options, Resolution resolution)
    {
        if (resolution.Token is not null)
        {
            options.GitHubToken = resolution.Token;
            return options;
        }

        if (resolution.HiddenVariables.Count > 0)
        {
            // Setting Environment replaces the runtime's whole environment, so pass everything else through.
            var environment = new Dictionary<string, string>(StringComparer.Ordinal);
            foreach (System.Collections.DictionaryEntry entry in Environment.GetEnvironmentVariables())
            {
                if (entry.Key is string key && entry.Value is string value && !resolution.HiddenVariables.Contains(key, StringComparer.OrdinalIgnoreCase))
                    environment[key] = value;
            }
            options.Environment = environment;
        }
        return options;
    }
}
