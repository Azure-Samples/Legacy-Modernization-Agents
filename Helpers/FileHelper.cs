using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Helper class for file operations.
/// </summary>
public class FileHelper
{
    private readonly ILogger<FileHelper> _logger;

    /// <summary>
    /// Initializes a new instance of the <see cref="FileHelper"/> class.
    /// </summary>
    /// <param name="logger">The logger.</param>
    public FileHelper(ILogger<FileHelper> logger)
    {
        _logger = logger;
    }

    /// <summary>
    /// Scans a directory for COBOL files.
    /// </summary>
    /// <param name="directory">The directory to scan.</param>
    /// <returns>A list of COBOL files.</returns>
    public async Task<List<CobolFile>> ScanDirectoryForCobolFilesAsync(string directory)
    {
        _logger.LogInformation("Scanning directory for COBOL files: {Directory}", directory);
        
        if (!Directory.Exists(directory))
        {
            _logger.LogError("Directory not found: {Directory}", directory);
            throw new DirectoryNotFoundException($"Directory not found: {directory}");
        }
        
        var cobolFiles = new List<CobolFile>();
        
        // Get all .cbl files (COBOL programs)
        var cblFiles = Directory.GetFiles(directory, "*.cbl", SearchOption.AllDirectories);
        foreach (var filePath in cblFiles)
        {
            var content = await File.ReadAllTextAsync(filePath);
            cobolFiles.Add(new CobolFile
            {
                FileName = Path.GetFileName(filePath),
                FilePath = filePath,
                Content = content,
                IsCopybook = false
            });
        }
        
        // Get all .cpy files (COBOL copybooks)
        var cpyFiles = Directory.GetFiles(directory, "*.cpy", SearchOption.AllDirectories);
        foreach (var filePath in cpyFiles)
        {
            var content = await File.ReadAllTextAsync(filePath);
            cobolFiles.Add(new CobolFile
            {
                FileName = Path.GetFileName(filePath),
                FilePath = filePath,
                Content = content,
                IsCopybook = true
            });
        }
        
        _logger.LogInformation("Found {Count} COBOL files ({CblCount} programs, {CpyCount} copybooks)", 
            cobolFiles.Count, cblFiles.Length, cpyFiles.Length);
        
        return cobolFiles;
    }

    /// <summary>
    /// Saves a Java file to disk.
    /// </summary>
    /// <param name="javaFile">The Java file to save.</param>
    /// <param name="outputDirectory">The output directory.</param>
    /// <returns>The full path to the saved file.</returns>
    public async Task<string> SaveJavaFileAsync(JavaFile javaFile, string outputDirectory)
    {
        // Validate and sanitize the filename
        var sanitizedFileName = SanitizeFileName(javaFile.FileName);
        if (string.IsNullOrEmpty(sanitizedFileName))
        {
            // Extract class name from content if filename is invalid
            sanitizedFileName = ExtractClassNameFromContent(javaFile.Content) + ".java";
            _logger.LogWarning("Invalid filename '{OriginalFileName}' replaced with '{SanitizedFileName}'", 
                javaFile.FileName, sanitizedFileName);
        }
        
        _logger.LogInformation("Saving Java file: {FileName}", sanitizedFileName);
        
        if (!Directory.Exists(outputDirectory))
        {
            _logger.LogInformation("Creating output directory: {Directory}", outputDirectory);
            Directory.CreateDirectory(outputDirectory);
        }
        
        // Create package directory structure
        var packagePath = javaFile.PackageName.Replace('.', Path.DirectorySeparatorChar);
        var packageDirectory = Path.Combine(outputDirectory, packagePath);
        
        if (!Directory.Exists(packageDirectory))
        {
            _logger.LogInformation("Creating package directory: {Directory}", packageDirectory);
            Directory.CreateDirectory(packageDirectory);
        }
        
        var filePath = Path.Combine(packageDirectory, sanitizedFileName);
        await File.WriteAllTextAsync(filePath, javaFile.Content);
        
        _logger.LogInformation("Saved Java file: {FilePath}", filePath);
        
        return filePath;
    }

    /// <summary>
    /// Sanitizes a filename by removing invalid characters and content
    /// </summary>
    private string SanitizeFileName(string fileName)
    {
        if (string.IsNullOrWhiteSpace(fileName))
            return string.Empty;
            
        // Remove any content that looks like Java code or comments
        var lines = fileName.Split('\n', StringSplitOptions.RemoveEmptyEntries);
        var firstLine = lines[0].Trim();
        
        // If the first line contains Java keywords or symbols, it's not a valid filename
        if (firstLine.Contains("public class") || firstLine.Contains("*/") || 
            firstLine.Contains("@") || firstLine.Contains("{") || firstLine.Contains("}"))
        {
            return string.Empty;
        }
        
        // Remove invalid path characters
        var invalidChars = Path.GetInvalidFileNameChars();
        var sanitized = new string(firstLine.Where(c => !invalidChars.Contains(c)).ToArray());
        
        // Ensure it ends with .java
        if (!sanitized.EndsWith(".java", StringComparison.OrdinalIgnoreCase))
        {
            if (sanitized.EndsWith("."))
                sanitized = sanitized.TrimEnd('.') + ".java";
            else if (!string.IsNullOrEmpty(sanitized))
                sanitized += ".java";
        }
        
        return sanitized;
    }

    /// <summary>
    /// Extracts the class name from Java content
    /// </summary>
    private string ExtractClassNameFromContent(string content)
    {
        var lines = content.Split('\n');
        foreach (var line in lines)
        {
            var trimmedLine = line.Trim();
            if (trimmedLine.StartsWith("public class "))
            {
                var parts = trimmedLine.Split(' ');
                if (parts.Length >= 3)
                {
                    var className = parts[2];
                    // Remove any trailing characters like { or implements
                    className = className.Split('{', ' ')[0];
                    return className;
                }
            }
        }
        
        // Fallback to a default name
        return "GeneratedClass";
    }

    /// <summary>
    /// Saves a dependency map to disk as JSON.
    /// </summary>
    /// <param name="dependencyMap">The dependency map to save.</param>
    /// <param name="filePath">The file path to save to.</param>
    public async Task SaveDependencyMapAsync(DependencyMap dependencyMap, string filePath)
    {
        _logger.LogInformation("Saving dependency map: {FilePath}", filePath);
        
        var directory = Path.GetDirectoryName(filePath);
        if (!string.IsNullOrEmpty(directory) && !Directory.Exists(directory))
        {
            _logger.LogInformation("Creating directory: {Directory}", directory);
            Directory.CreateDirectory(directory);
        }
        
        var json = System.Text.Json.JsonSerializer.Serialize(dependencyMap, new System.Text.Json.JsonSerializerOptions
        {
            WriteIndented = true,
            PropertyNamingPolicy = System.Text.Json.JsonNamingPolicy.CamelCase
        });
        
        await File.WriteAllTextAsync(filePath, json);
        _logger.LogInformation("Dependency map saved: {FilePath}", filePath);
    }
}
