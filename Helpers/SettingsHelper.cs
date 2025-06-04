using Microsoft.Extensions.Logging;
using System.Text.Json;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Helper class for application settings.
/// </summary>
public class SettingsHelper
{
    private readonly ILogger<SettingsHelper> _logger;

    /// <summary>
    /// Initializes a new instance of the <see cref="SettingsHelper"/> class.
    /// </summary>
    /// <param name="logger">The logger.</param>
    public SettingsHelper(ILogger<SettingsHelper> logger)
    {
        _logger = logger;
    }

    /// <summary>
    /// Loads settings from a JSON file.
    /// </summary>
    /// <typeparam name="T">The type of settings to load.</typeparam>
    /// <param name="filePath">The path to the settings file.</param>
    /// <returns>The loaded settings.</returns>
    public async Task<T> LoadSettingsAsync<T>(string filePath) where T : new()
    {
        _logger.LogInformation("Loading settings from file: {FilePath}", filePath);
        
        if (!File.Exists(filePath))
        {
            _logger.LogWarning("Settings file not found: {FilePath}", filePath);
            return new T();
        }
        
        try
        {
            var json = await File.ReadAllTextAsync(filePath);
            var settings = JsonSerializer.Deserialize<T>(json, new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            });
            
            if (settings == null)
            {
                _logger.LogWarning("Failed to deserialize settings from file: {FilePath}", filePath);
                return new T();
            }
            
            _logger.LogInformation("Successfully loaded settings from file: {FilePath}", filePath);
            
            return settings;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading settings from file: {FilePath}", filePath);
            return new T();
        }
    }

    /// <summary>
    /// Saves settings to a JSON file.
    /// </summary>
    /// <typeparam name="T">The type of settings to save.</typeparam>
    /// <param name="settings">The settings to save.</param>
    /// <param name="filePath">The path to the settings file.</param>
    /// <returns>A task representing the asynchronous operation.</returns>
    public async Task SaveSettingsAsync<T>(T settings, string filePath)
    {
        _logger.LogInformation("Saving settings to file: {FilePath}", filePath);
        
        try
        {
            var json = JsonSerializer.Serialize(settings, new JsonSerializerOptions
            {
                WriteIndented = true
            });
            
            var directory = Path.GetDirectoryName(filePath);
            if (!string.IsNullOrEmpty(directory) && !Directory.Exists(directory))
            {
                Directory.CreateDirectory(directory);
            }
            
            await File.WriteAllTextAsync(filePath, json);
            
            _logger.LogInformation("Successfully saved settings to file: {FilePath}", filePath);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error saving settings to file: {FilePath}", filePath);
            throw;
        }
    }
}
