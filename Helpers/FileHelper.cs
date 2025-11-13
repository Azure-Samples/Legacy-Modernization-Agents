using Microsoft.Extensions.Logging;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Helper class for cross-platform file operations.
/// 
/// Handles OS-specific concerns:
/// - Windows MAX_PATH (260 character limit) with automatic path shortening
/// - Windows reserved filenames (CON, PRN, AUX, NUL, COM1-9, LPT1-9)
/// - Invalid characters for filenames on all platforms
/// - Windows file locking with retry logic
/// - UTF-8 encoding without BOM for maximum compatibility
/// - Platform-specific line endings (CRLF on Windows, LF on Unix/Mac)
/// - Directory creation with proper error handling
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
    /// Saves a Java file to disk with cross-platform compatibility.
    /// Handles Windows path length limits, invalid characters, and encoding issues.
    /// </summary>
    /// <param name="javaFile">The Java file to save.</param>
    /// <param name="outputDirectory">The output directory.</param>
    /// <returns>The full path to the saved file.</returns>
    public async Task<string> SaveJavaFileAsync(JavaFile javaFile, string outputDirectory)
    {
        return await SaveCodeFileAsync(javaFile, outputDirectory, ".java");
    }

    /// <summary>
    /// Saves a code file to disk with cross-platform compatibility.
    /// Handles Windows path length limits, invalid characters, and encoding issues.
    /// Supports multiple languages (Java, C#, etc.).
    /// </summary>
    /// <param name="codeFile">The code file to save.</param>
    /// <param name="outputDirectory">The output directory.</param>
    /// <param name="defaultExtension">Default extension if file doesn't have one (e.g., ".java", ".cs").</param>
    /// <returns>The full path to the saved file.</returns>
    public async Task<string> SaveCodeFileAsync(CodeFile codeFile, string outputDirectory, string defaultExtension = ".cs")
    {
        try
        {
            // Ensure default extension starts with dot
            if (!string.IsNullOrEmpty(defaultExtension) && !defaultExtension.StartsWith("."))
            {
                defaultExtension = "." + defaultExtension;
            }

            // Validate and sanitize the filename
            var sanitizedFileName = SanitizeFileName(codeFile.FileName);
            if (string.IsNullOrEmpty(sanitizedFileName))
            {
                // Extract class name from content if filename is invalid
                sanitizedFileName = ExtractClassNameFromContent(codeFile.Content) + defaultExtension;
                _logger.LogWarning("Invalid filename '{OriginalFileName}' replaced with '{SanitizedFileName}'",
                    codeFile.FileName, sanitizedFileName);
            }

            // Ensure filename has correct extension
            if (!string.IsNullOrEmpty(defaultExtension) && !sanitizedFileName.EndsWith(defaultExtension, StringComparison.OrdinalIgnoreCase))
            {
                var currentExt = Path.GetExtension(sanitizedFileName);
                if (string.IsNullOrEmpty(currentExt))
                {
                    // No extension, add the default one
                    sanitizedFileName += defaultExtension;
                }
                else
                {
                    // Has wrong extension, replace it with the correct one
                    sanitizedFileName = Path.GetFileNameWithoutExtension(sanitizedFileName) + defaultExtension;
                }
            }

            _logger.LogInformation("Saving code file: {FileName}", sanitizedFileName);

            // Normalize output directory path for current OS
            outputDirectory = Path.GetFullPath(outputDirectory);

            // Create output directory with retry logic for Windows
            EnsureDirectoryExists(outputDirectory);

            // Sanitize and validate namespace/package name
            var sanitizedNamespace = SanitizePackageName(codeFile.NamespaceName);
            if (string.IsNullOrEmpty(sanitizedNamespace))
            {
                // Extract namespace/package from content if invalid
                sanitizedNamespace = ExtractPackageNameFromContent(codeFile.Content);
                _logger.LogWarning("Invalid namespace '{OriginalNamespace}' replaced with '{SanitizedNamespace}'",
                    codeFile.NamespaceName, sanitizedNamespace);
            }

            // Create namespace/package directory structure using OS-specific separator
            var namespacePath = sanitizedNamespace.Replace('.', Path.DirectorySeparatorChar);
            var namespaceDirectory = Path.Combine(outputDirectory, namespacePath);

            // Windows MAX_PATH is 260 characters, but we need to handle long paths
            // Check if we're on Windows and path is too long
            var potentialFilePath = Path.Combine(namespaceDirectory, sanitizedFileName);
            var isWindows = Environment.OSVersion.Platform == PlatformID.Win32NT ||
                           Environment.OSVersion.Platform == PlatformID.Win32Windows ||
                           Environment.OSVersion.Platform == PlatformID.Win32S;

            if (isWindows && potentialFilePath.Length > 240) // Leave margin for Windows MAX_PATH (260)
            {
                _logger.LogWarning("Path too long for Windows ({Length} chars), using shortened namespace", potentialFilePath.Length);
                // Use just the last part of the namespace
                var parts = sanitizedNamespace.Split('.');
                sanitizedNamespace = parts.Length > 2
                    ? string.Join('.', parts.TakeLast(2))
                    : sanitizedNamespace;
                namespacePath = sanitizedNamespace.Replace('.', Path.DirectorySeparatorChar);
                namespaceDirectory = Path.Combine(outputDirectory, namespacePath);
                potentialFilePath = Path.Combine(namespaceDirectory, sanitizedFileName);

                // If still too long, use flat structure
                if (potentialFilePath.Length > 240)
                {
                    _logger.LogWarning("Path still too long, using flat structure in output directory");
                    namespaceDirectory = outputDirectory;
                    potentialFilePath = Path.Combine(namespaceDirectory, sanitizedFileName);
                }
            }

            // Create namespace directory with retry logic
            EnsureDirectoryExists(namespaceDirectory);

            var filePath = Path.Combine(namespaceDirectory, sanitizedFileName);

            // Write file with explicit UTF-8 encoding (no BOM) and proper line endings
            // Use cross-platform line endings (Environment.NewLine)
            var normalizedContent = NormalizeLineEndings(codeFile.Content);

            // Write with retry logic for Windows file locking issues
            await WriteFileWithRetryAsync(filePath, normalizedContent);

            _logger.LogInformation("Saved code file: {FilePath}", filePath);

            return filePath;
        }
        catch (PathTooLongException ex)
        {
            _logger.LogError(ex, "Path too long for file system. File: {FileName}, Namespace: {Namespace}",
                codeFile.FileName, codeFile.NamespaceName);
            throw new InvalidOperationException(
                $"Cannot save file '{codeFile.FileName}' - path exceeds OS limit. Try using a shorter output directory or namespace.", ex);
        }
        catch (UnauthorizedAccessException ex)
        {
            _logger.LogError(ex, "Access denied writing file: {FileName}. Check directory permissions.", codeFile.FileName);
            throw new InvalidOperationException(
                $"Access denied writing file '{codeFile.FileName}'. Ensure the output directory has write permissions.", ex);
        }
        catch (IOException ex)
        {
            _logger.LogError(ex, "I/O error writing file: {FileName}. File may be locked by another process.", codeFile.FileName);
            throw new InvalidOperationException(
                $"Cannot write file '{codeFile.FileName}'. The file may be open in another program or the disk may be full.", ex);
        }
    }

    /// <summary>
    /// Ensures a directory exists, creating it with retry logic for Windows.
    /// </summary>
    private void EnsureDirectoryExists(string directoryPath)
    {
        if (Directory.Exists(directoryPath))
            return;

        const int maxRetries = 3;
        const int retryDelayMs = 100;

        for (int attempt = 1; attempt <= maxRetries; attempt++)
        {
            try
            {
                _logger.LogDebug("Creating directory: {Directory} (attempt {Attempt}/{MaxRetries})",
                    directoryPath, attempt, maxRetries);
                Directory.CreateDirectory(directoryPath);
                return;
            }
            catch (IOException ex) when (attempt < maxRetries)
            {
                _logger.LogWarning(ex, "Failed to create directory on attempt {Attempt}, retrying...", attempt);
                Thread.Sleep(retryDelayMs);
            }
            catch (UnauthorizedAccessException ex)
            {
                _logger.LogError(ex, "Access denied creating directory: {Directory}", directoryPath);
                throw new InvalidOperationException(
                    $"Cannot create directory '{directoryPath}'. Check that you have write permissions to the parent directory.", ex);
            }
        }
    }

    /// <summary>
    /// Writes a file with retry logic to handle Windows file locking issues.
    /// </summary>
    private async Task WriteFileWithRetryAsync(string filePath, string content)
    {
        const int maxRetries = 3;
        const int retryDelayMs = 100;

        for (int attempt = 1; attempt <= maxRetries; attempt++)
        {
            try
            {
                // Use UTF-8 encoding without BOM for maximum compatibility
                var encoding = new System.Text.UTF8Encoding(false);
                await File.WriteAllTextAsync(filePath, content, encoding);
                return;
            }
            catch (IOException ex) when (attempt < maxRetries &&
                (ex.Message.Contains("being used by another process") ||
                 ex.Message.Contains("locked")))
            {
                _logger.LogWarning("File locked on attempt {Attempt}, retrying in {Delay}ms...", attempt, retryDelayMs);
                await Task.Delay(retryDelayMs);
            }
        }
    }

    /// <summary>
    /// Normalizes line endings to the current platform's standard.
    /// </summary>
    private string NormalizeLineEndings(string content)
    {
        if (string.IsNullOrEmpty(content))
            return content;

        // Replace all line ending variations with the platform-specific one
        return content.Replace("\r\n", "\n").Replace("\r", "\n").Replace("\n", Environment.NewLine);
    }

    /// <summary>
    /// Sanitizes a filename by removing invalid characters and handling Windows reserved names.
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

        // Remove any leading/trailing spaces or dots (Windows doesn't allow these)
        sanitized = sanitized.Trim(' ', '.');

        // Check for Windows reserved names (CON, PRN, AUX, NUL, COM1-9, LPT1-9)
        var reservedNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "CON", "PRN", "AUX", "NUL",
            "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9",
            "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9"
        };

        var nameWithoutExtension = Path.GetFileNameWithoutExtension(sanitized);
        if (reservedNames.Contains(nameWithoutExtension))
        {
            // Prefix with underscore to make it safe
            sanitized = "_" + sanitized;
            _logger.LogWarning("Filename '{Original}' is reserved on Windows, renamed to '{Sanitized}'",
                nameWithoutExtension, sanitized);
        }

        // Final validation - ensure filename isn't empty after sanitization
        if (string.IsNullOrWhiteSpace(sanitized))
        {
            return string.Empty;
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
