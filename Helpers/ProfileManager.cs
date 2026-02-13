using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;

namespace CobolToQuarkusMigration.Helpers;

public class GenerationProfile
{
    public string Id { get; set; } = string.Empty;
    public string Name { get; set; } = string.Empty;
    public string Language { get; set; } = string.Empty;
    public string Framework { get; set; } = string.Empty;
    public string FrameworkVersion { get; set; } = string.Empty;
    public List<string> Features { get; set; } = new();
    public NamingConvention NamingConvention { get; set; } = new();
    public PromptSettings Prompts { get; set; } = new();
}

public class NamingConvention
{
    public string NamespaceBase { get; set; } = string.Empty;
    public string DtoSuffix { get; set; } = string.Empty;
    public string ServiceSuffix { get; set; } = string.Empty;
}

public class PromptSettings
{
    public string SystemPrompt { get; set; } = string.Empty;
}

public class ProfileConfig
{
    public List<GenerationProfile> Profiles { get; set; } = new();
    public string DefaultProfileId { get; set; } = string.Empty;
}

public class ProfileManager
{
    private readonly string _configPath;
    private ProfileConfig? _cachedConfig;

    public ProfileManager(string configPath = "Config/GenerationProfiles.json")
    {
        _configPath = configPath;
    }

    public async Task<GenerationProfile?> GetProfileAsync(string profileId)
    {
        var config = await GetConfigAsync();
        return config.Profiles.FirstOrDefault(p => p.Id == profileId);
    }

    public async Task<GenerationProfile> GetDefaultProfileAsync()
    {
        var config = await GetConfigAsync();
        return config.Profiles.FirstOrDefault(p => p.Id == config.DefaultProfileId) 
               ?? config.Profiles.FirstOrDefault() 
               ?? new GenerationProfile { Id = "default", Name = "Default" };
    }
    
    public async Task<List<GenerationProfile>> GetAllProfilesAsync()
    {
        var config = await GetConfigAsync();
        return config.Profiles;
    }

    private async Task<ProfileConfig> GetConfigAsync()
    {
        if (_cachedConfig != null) return _cachedConfig;

        if (!File.Exists(_configPath))
        {
            // Fallback default
            return new ProfileConfig 
            { 
                DefaultProfileId = "default",
                Profiles = new List<GenerationProfile> { new GenerationProfile { Id = "default", Name = "Fallback Default" } } 
            };
        }

        var json = await File.ReadAllTextAsync(_configPath);
        var options = new JsonSerializerOptions { PropertyNameCaseInsensitive = true };
        _cachedConfig = JsonSerializer.Deserialize<ProfileConfig>(json, options) ?? new ProfileConfig();
        return _cachedConfig;
    }
}
