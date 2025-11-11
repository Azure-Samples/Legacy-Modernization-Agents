# Windows Compatibility Guide

**Last Updated:** October 24, 2025  
**Version:** 1.3.1

## Overview

This document describes the cross-platform file writing improvements made to ensure the Java Converter Agent works reliably on Windows OS, in addition to macOS and Linux.

## Problems Identified

### 1. Windows MAX_PATH Limitation (260 Characters)
**Issue:** Windows has a historical 260-character path length limit (MAX_PATH) that can cause file writes to fail when Java package structures create deep directory hierarchies.

**Example:**
```
C:\Users\YourName\Projects\Legacy-Modernization-Agents\java-output\com\example\generated\cobol\bankdata\programs\SomeVeryLongClassName.java
```

This path can easily exceed 260 characters when combined with long package names.

### 2. Windows Reserved Filenames
**Issue:** Windows reserves certain filenames that cannot be used, regardless of extension:
- `CON`, `PRN`, `AUX`, `NUL`
- `COM1` through `COM9`
- `LPT1` through `LPT9`

If a Java class is named any of these (e.g., `AUX.java`), Windows will fail to create the file.

### 3. File Locking Issues
**Issue:** Windows has stricter file locking than Unix systems. Antivirus software, Windows Defender, or other processes may briefly lock files during creation, causing transient write failures.

### 4. Invalid Filename Characters
**Issue:** Windows disallows more characters in filenames than Unix systems:
- `< > : " / \ | ? *`
- Control characters (0-31)
- Leading/trailing spaces or dots

### 5. Line Ending Differences
**Issue:** Windows uses CRLF (`\r\n`) while Unix/Mac use LF (`\n`). Generated Java files need consistent line endings for the target platform.

### 6. UTF-8 BOM (Byte Order Mark)
**Issue:** Some Windows text editors add a UTF-8 BOM to files. Java compilers may have issues with BOM in source files.

## Solutions Implemented

### 1. Path Length Handling

#### Detection
```csharp
var isWindows = Environment.OSVersion.Platform == PlatformID.Win32NT || 
               Environment.OSVersion.Platform == PlatformID.Win32Windows ||
               Environment.OSVersion.Platform == PlatformID.Win32S;

if (isWindows && potentialFilePath.Length > 240)
```

#### Automatic Path Shortening
When a path exceeds 240 characters (leaving margin below 260), the system automatically:

1. **First attempt:** Shorten package name to last 2 components
   ```
   com.example.generated.cobol.bankdata.programs
   → programs
   ```

2. **Second attempt:** Use flat structure (no package directories)
   ```
   java-output/SomeClassName.java
   ```

#### Logging
```
[Warning] Path too long for Windows (255 chars), using shortened package name
[Warning] Path still too long, using flat structure in output directory
```

### 2. Reserved Filename Detection

```csharp
var reservedNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
{
    "CON", "PRN", "AUX", "NUL",
    "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9",
    "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9"
};

if (reservedNames.Contains(nameWithoutExtension))
{
    sanitized = "_" + sanitized; // Prefix with underscore
}
```

**Example:**
- `AUX.java` → `_AUX.java`
- `CON.java` → `_CON.java`

### 3. File Locking Retry Logic

```csharp
private async Task WriteFileWithRetryAsync(string filePath, string content)
{
    const int maxRetries = 3;
    const int retryDelayMs = 100;
    
    for (int attempt = 1; attempt <= maxRetries; attempt++)
    {
        try
        {
            await File.WriteAllTextAsync(filePath, content, encoding);
            return;
        }
        catch (IOException ex) when (attempt < maxRetries && 
            (ex.Message.Contains("being used by another process") || 
             ex.Message.Contains("locked")))
        {
            await Task.Delay(retryDelayMs);
        }
    }
}
```

**Benefits:**
- Handles transient locks from antivirus scans
- Handles Windows Defender real-time protection
- Handles temporary locks from indexing services

### 4. Comprehensive Filename Sanitization

```csharp
// Remove invalid path characters
var invalidChars = Path.GetInvalidFileNameChars();
var sanitized = new string(firstLine.Where(c => !invalidChars.Contains(c)).ToArray());

// Remove leading/trailing spaces or dots (Windows requirement)
sanitized = sanitized.Trim(' ', '.');
```

**Removed characters:**
- `< > : " / \ | ? *` (Windows invalid chars)
- Control characters (0-31)
- Leading/trailing spaces and dots

### 5. Line Ending Normalization

```csharp
private string NormalizeLineEndings(string content)
{
    // Replace all variations with platform-specific
    return content.Replace("\r\n", "\n")
                  .Replace("\r", "\n")
                  .Replace("\n", Environment.NewLine);
}
```

**Result:**
- Windows: `\r\n` (CRLF)
- Unix/Mac: `\n` (LF)

### 6. UTF-8 Without BOM

```csharp
var encoding = new System.Text.UTF8Encoding(false); // false = no BOM
await File.WriteAllTextAsync(filePath, content, encoding);
```

**Benefits:**
- Java compilers work consistently
- Git diffs are cleaner
- Cross-platform file sharing works better

## Error Handling

### Enhanced Error Messages

#### Path Too Long
```
Cannot save file 'SomeFile.java' - path exceeds OS limit. 
Try using a shorter output directory or package name.
```

#### Access Denied
```
Access denied writing file 'SomeFile.java'. 
Ensure the output directory has write permissions.
```

#### File Locked
```
Cannot write file 'SomeFile.java'. 
The file may be open in another program or the disk may be full.
```

### Logging Levels

```csharp
_logger.LogDebug("Creating directory: {Directory} (attempt {Attempt}/{MaxRetries})", ...);
_logger.LogWarning("Path too long for Windows ({Length} chars), using shortened package name");
_logger.LogError(ex, "Access denied creating directory: {Directory}");
```

## Testing on Windows

### Test Scenarios

1. **Long Paths**
   ```bash
   # Create deep directory structure
   dotnet run -- --cobol-source ./cobol-source --java-output "C:\Very\Long\Path\With\Many\Nested\Directories\And\A\Complex\Package\Structure"
   ```

2. **Reserved Names**
   ```cobol
   * Create COBOL program named AUX.cbl or CON.cbl
   * Verify it converts to _AUX.java or _CON.java
   ```

3. **Concurrent Access**
   ```bash
   # Run migration while antivirus is active
   # Open java-output folder in Windows Explorer during migration
   # Monitor for successful writes despite locks
   ```

4. **Special Characters**
   ```cobol
   * Create COBOL program with special characters in comments
   * Verify filename sanitization removes them
   ```

5. **Line Endings**
   ```bash
   # After migration, check Java files have CRLF
   file java-output/com/example/*.java
   # Should show: "CRLF line terminators"
   ```

### Expected Results

✅ All Java files written successfully  
✅ No "Path too long" errors  
✅ No "Access denied" errors  
✅ No "File is being used" errors  
✅ Reserved names prefixed with underscore  
✅ Line endings match Windows standard (CRLF)  
✅ UTF-8 encoding without BOM  
✅ All directory structures created correctly  

## Performance Impact

| Operation | Before | After | Impact |
|-----------|--------|-------|--------|
| **File Write** | 1 attempt | Up to 3 attempts | +200ms worst case (transient locks) |
| **Path Validation** | None | Windows check + length check | +1ms per file |
| **Directory Creation** | 1 attempt | Up to 3 attempts | +200ms worst case |
| **Line Ending Normalization** | None | String replacement | +2-5ms per file |

**Total Impact:** Negligible for typical migrations (<1% overhead)

## Configuration

### Disable Windows-Specific Handling (Not Recommended)

If you need to disable Windows-specific logic for testing:

```csharp
// In FileHelper.cs, force isWindows = false
var isWindows = false; // Disable Windows path length checking
```

**Warning:** This may cause failures on actual Windows systems.

### Adjust Retry Settings

```csharp
// In FileHelper.cs WriteFileWithRetryAsync method
const int maxRetries = 5;      // Default: 3
const int retryDelayMs = 200;  // Default: 100
```

## Troubleshooting

### "Path too long" errors persist

**Solution 1:** Use shorter output directory
```bash
dotnet run -- --java-output "C:\out"
```

**Solution 2:** Enable Windows Long Path support (Windows 10 1607+)
```
1. Win + R → gpedit.msc
2. Computer Configuration → Administrative Templates → System → Filesystem
3. Enable "Enable Win32 long paths"
4. Reboot
```

**Solution 3:** Use UNC path prefix
```bash
dotnet run -- --java-output "\\?\C:\very\long\path"
```

### Files still locked by antivirus

**Solution:** Add exclusion for java-output directory
```
Windows Security → Virus & threat protection → Manage settings
→ Exclusions → Add folder → Select java-output
```

### Line endings wrong after Git clone

**Solution:** Configure Git for Windows
```bash
git config --global core.autocrlf true
```

## Best Practices

1. **Short Output Paths**
   ```bash
   # Good
   --java-output "C:\migration\output"
   
   # Bad
   --java-output "C:\Users\Very Long Username\Documents\Projects\Legacy Modernization\Migration Output Files"
   ```

2. **Simple Package Names**
   ```java
   // Good
   package com.bankdata.cobol;
   
   // Bad
   package com.example.generated.cobol.bankdata.migration.v1.programs.batch.mainframe;
   ```

3. **Avoid Reserved Names**
   - Don't name COBOL programs: `CON.cbl`, `AUX.cbl`, `NUL.cbl`
   - The system will handle it, but avoid for clarity

4. **Monitor Logs**
   ```bash
   # Watch for warnings during migration
   tail -f Logs/migration-*.log | grep -i "warning\|error"
   ```

## Known Limitations

1. **Path Length:** Even with improvements, extremely deep package hierarchies may still fail on Windows. Maximum safe path is ~240 characters.

2. **Network Drives:** Writing to UNC network paths (e.g., `\\server\share`) may have additional latency and locking issues.

3. **Symbolic Links:** Windows symbolic links require admin privileges. The migration doesn't use them, but be aware if customizing.

4. **Case Sensitivity:** Windows is case-insensitive but case-preserving. `File.java` and `file.java` are the same file.

## Version History

| Version | Date | Changes |
|---------|------|---------|
| **1.3.1** | 2025-10-24 | Initial Windows compatibility improvements |
| | | - Path length validation and shortening |
| | | - Reserved filename detection |
| | | - File locking retry logic |
| | | - Line ending normalization |
| | | - UTF-8 without BOM |
| | | - Enhanced error messages |

## Additional Resources

- [Windows MAX_PATH Limitation](https://learn.microsoft.com/en-us/windows/win32/fileio/maximum-file-path-limitation)
- [Naming Files, Paths, and Namespaces](https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file)
- [UTF-8 Encoding in .NET](https://learn.microsoft.com/en-us/dotnet/api/system.text.utf8encoding)

---

**Questions or Issues?** Please open an issue on GitHub with:
- Operating System and version
- Full error message
- Path length of failing operation
- Whether antivirus is active
