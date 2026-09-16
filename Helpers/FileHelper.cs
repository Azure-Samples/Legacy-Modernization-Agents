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
    /// Gets or sets the programs the run is restricted to. Empty means the whole estate.
    /// Applied here rather than at each caller because every process reaches its source
    /// files through <see cref="ScanDirectoryForCobolFilesAsync"/>; a caller that forgot to
    /// filter would silently convert everything.
    /// </summary>
    public IReadOnlyList<string> ProgramSelection { get; set; } = Array.Empty<string>();

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

        // SourceTypeRegistry decides which extensions count as programs (.cbl,
        // .cob) versus copybooks (.cpy); recursion behaviour is unchanged.
        var cblFiles = SourceTypeRegistry.EnumerateProgramFiles(directory).ToArray();
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

        // In selector mode the user asked for specific programs, so skip the
        // per-copybook analyzer calls; copybooks are still read as COPY context.
        var selectorMode = string.Equals(
            Environment.GetEnvironmentVariable("SELECTOR_MODE"),
            "true",
            StringComparison.OrdinalIgnoreCase);
        var allCopybooks = SourceTypeRegistry.EnumerateCopybookFiles(directory).ToArray();
        var cpyFiles = selectorMode ? Array.Empty<string>() : allCopybooks;
        if (selectorMode)
        {
            _logger.LogInformation("[FileHelper] SELECTOR_MODE=true — skipping standalone .cpy analysis ({Skipped} copybooks will only be used as COPY context)",
                allCopybooks.Length);
        }
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

        if (ProgramSelection.Count > 0)
        {
            cobolFiles = Helpers.ProgramSelection.Apply(cobolFiles, ProgramSelection, out var unmatched);

            foreach (var name in unmatched)
            {
                _logger.LogWarning(
                    "Selected program '{Program}' matched no source file under {Directory}", name, directory);
            }

            _logger.LogInformation(
                "Program selection applied: {Count} files retained ({ProgramCount} programs)",
                cobolFiles.Count, cobolFiles.Count(f => !f.IsCopybook));
        }

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

            // The declaration inside the file decides where the file goes. NamespaceName is what
            // the converter intended; the declaration is what the compiler will read, and when the
            // two disagree it is the declaration that has to be honoured. Java will not build a
            // file whose package and folder differ, and every generated file was landing in the
            // fallback directory while declaring something else entirely.
            var declaredNamespace = ExtractPackageNameFromContent(codeFile.Content);
            var sanitizedNamespace = SanitizePackageName(codeFile.NamespaceName);

            if (string.IsNullOrEmpty(sanitizedNamespace))
            {
                sanitizedNamespace = declaredNamespace;
                _logger.LogWarning("Invalid namespace '{OriginalNamespace}' replaced with '{SanitizedNamespace}'",
                    codeFile.NamespaceName, sanitizedNamespace);
            }
            else if (!string.Equals(sanitizedNamespace, declaredNamespace, StringComparison.Ordinal)
                     && declaredNamespace != "com.example.generated")
            {
                _logger.LogWarning(
                    "{FileName} declares '{Declared}' but was assigned '{Assigned}'; filing it under the declaration so it compiles",
                    sanitizedFileName, declaredNamespace, sanitizedNamespace);
                sanitizedNamespace = declaredNamespace;
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

        if (string.IsNullOrEmpty(firstLine) || !firstLine.Contains('.'))
        {
            return "com.example.generated";
        }

        // Case is preserved rather than lowered. Lowercasing is a Java package convention, and
        // applying it to C# turns Contoso.Billing into contoso.billing, which then disagrees with
        // the namespace the file declares — reintroducing the mismatch this is meant to remove.
        return firstLine;
    }

    /// <summary>
    /// Extracts the package name from Java content
    /// </summary>
    /// <summary>
    /// Reads the package or namespace the file declares for itself.
    ///
    /// The declaration is what a compiler reads, so it decides where the file has to live. Java
    /// requires the two to agree: a file declaring <c>com.example.billing</c> must sit in
    /// <c>com/example/billing</c>, and one that does not will not build, however correct its
    /// contents. C# does not require it, but a namespace that disagrees with its folder is still
    /// the kind of thing a reviewer stops on.
    /// </summary>
    private string ExtractPackageNameFromContent(string content)
    {
        foreach (var line in content.Split('\n'))
        {
            var trimmedLine = line.Trim();

            // Java, and C#'s file-scoped form: `package x.y;` / `namespace X.Y;`
            if (trimmedLine.StartsWith("package ", StringComparison.Ordinal) && trimmedLine.EndsWith(";", StringComparison.Ordinal))
                return SanitizePackageName(trimmedLine[8..^1].Trim());

            if (trimmedLine.StartsWith("namespace ", StringComparison.Ordinal))
            {
                // `namespace X.Y;`, `namespace X.Y {`, or `namespace X.Y` with the brace below.
                var declared = trimmedLine[10..].TrimEnd();
                var brace = declared.IndexOf('{');
                if (brace >= 0) declared = declared[..brace];
                declared = declared.TrimEnd(';').Trim();

                if (!string.IsNullOrWhiteSpace(declared))
                    return SanitizePackageName(declared);
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

    /// <summary>Writes dependency-map.json and dependency-diagram.md to <paramref name="outputFolder"/>.</summary>
    public async Task SaveDependencyOutputsAsync(DependencyMap dependencyMap, string outputFolder)
    {
        Directory.CreateDirectory(outputFolder);
        await SaveDependencyMapAsync(dependencyMap, Path.Join(outputFolder, "dependency-map.json"));
        await File.WriteAllTextAsync(
            Path.Join(outputFolder, "dependency-diagram.md"),
            $"# COBOL Dependency Diagram\n\n```mermaid\n{dependencyMap.MermaidDiagram}\n```");
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

    /// <summary>
    /// Loads the glossary from disk.
    /// </summary>
    /// <returns>The glossary or null if not found.</returns>
    public async Task<Glossary?> LoadGlossaryAsync()
    {
        var glossaryPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "Data", "glossary.json");
        if (!File.Exists(glossaryPath))
        {
            _logger.LogWarning("Glossary file not found at {Path}", glossaryPath);
            return null;
        }

        try
        {
            var content = await File.ReadAllTextAsync(glossaryPath);
            return System.Text.Json.JsonSerializer.Deserialize<Glossary>(content, new System.Text.Json.JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to load glossary from {Path}", glossaryPath);
            return null;
        }
    }
}
