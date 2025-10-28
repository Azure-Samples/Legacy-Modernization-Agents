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
    /// Scans a directory for COBOL files including both programs (.cbl) and copybooks (.cpy).
    /// </summary>
    /// <param name="directory">The directory to scan.</param>
    /// <returns>A list of COBOL files including both programs and copybooks.</returns>
    public async Task<List<CobolFile>> ScanDirectoryForCobolFilesAsync(string directory)
    {
        _logger.LogInformation("Scanning directory for COBOL files and copybooks: {Directory}", directory);
        
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
        
        // Sanitize and validate package name
        var sanitizedPackageName = SanitizePackageName(javaFile.PackageName);
        if (string.IsNullOrEmpty(sanitizedPackageName))
        {
            // Extract package from content if package name is invalid
            sanitizedPackageName = ExtractPackageNameFromContent(javaFile.Content);
            _logger.LogWarning("Invalid package name '{OriginalPackage}' replaced with '{SanitizedPackage}'", 
                javaFile.PackageName, sanitizedPackageName);
        }
        
        // Create package directory structure
        var packagePath = sanitizedPackageName.Replace('.', Path.DirectorySeparatorChar);
        var packageDirectory = Path.Combine(outputDirectory, packagePath);
        
        // Validate the full path length before creating
        var potentialFilePath = Path.Combine(packageDirectory, sanitizedFileName);
        if (potentialFilePath.Length > 240) // Leave some margin before the 260 limit
        {
            _logger.LogWarning("Path too long ({Length} chars), using shortened package name", potentialFilePath.Length);
            // Use just the last part of the package name
            var parts = sanitizedPackageName.Split('.');
            sanitizedPackageName = parts.Length > 2 
                ? string.Join('.', parts.TakeLast(2))
                : sanitizedPackageName;
            packagePath = sanitizedPackageName.Replace('.', Path.DirectorySeparatorChar);
            packageDirectory = Path.Combine(outputDirectory, packagePath);
            potentialFilePath = Path.Combine(packageDirectory, sanitizedFileName);
        }
        
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
    /// Sanitizes a package name by removing invalid characters and content
    /// </summary>
    private string SanitizePackageName(string packageName)
    {
        if (string.IsNullOrWhiteSpace(packageName))
            return "com.example.generated";
            
        // Take only the first line and remove whitespace
        var firstLine = packageName.Split('\n', StringSplitOptions.RemoveEmptyEntries)[0].Trim();
        
        // If it contains any invalid characters for a package name, reject it
        if (firstLine.Any(c => !char.IsLetterOrDigit(c) && c != '.' && c != '_'))
        {
            return "com.example.generated";
        }
        
        // Remove any leading/trailing dots
        firstLine = firstLine.Trim('.');
        
        // Ensure it doesn't have consecutive dots
        while (firstLine.Contains(".."))
        {
            firstLine = firstLine.Replace("..", ".");
        }
        
        // Validate it looks like a package name (only lowercase letters, dots, numbers)
        if (string.IsNullOrEmpty(firstLine) || !firstLine.Contains('.'))
        {
            return "com.example.generated";
        }
        
        return firstLine.ToLowerInvariant();
    }

    /// <summary>
    /// Extracts the package name from Java content
    /// </summary>
    private string ExtractPackageNameFromContent(string content)
    {
        var lines = content.Split('\n');
        foreach (var line in lines)
        {
            var trimmedLine = line.Trim();
            if (trimmedLine.StartsWith("package ") && trimmedLine.EndsWith(";"))
            {
                var packageName = trimmedLine.Substring(8, trimmedLine.Length - 9).Trim();
                return SanitizePackageName(packageName);
            }
        }
        
        return "com.example.generated";
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
            if (trimmedLine.StartsWith("public class ") || trimmedLine.StartsWith("class "))
            {
                var parts = trimmedLine.Split(' ');
                var classIndex = Array.IndexOf(parts, "class") + 1;
                if (classIndex > 0 && classIndex < parts.Length)
                {
                    var className = parts[classIndex];
                    // Remove any trailing characters like { or implements
                    className = className.Split('{', ' ', '<')[0];
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
