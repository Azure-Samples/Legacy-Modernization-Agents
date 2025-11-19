# Configuration Path Fix - COBOL Source Folder

## Issue

The migration tool was failing with "No COBOL files found" error because the configuration files pointed to the wrong source directory.

### Error Message
```
info: CobolToQuarkusMigration.Helpers.FileHelper[0]
      Scanning directory for COBOL files: source
info: CobolToQuarkusMigration.Helpers.FileHelper[0]
      Found 0 COBOL files (0 programs, 0 copybooks)
✅ Found 0 COBOL files
⚠️  Warning: No COBOL files found. Nothing to reverse engineer.
❌ Migration process failed (exit code 1).
```

## Root Cause

Configuration files had incorrect path for COBOL source folder:
- **Incorrect**: `COBOL_SOURCE_FOLDER="source"`
- **Correct**: `COBOL_SOURCE_FOLDER="cobol-source"`

The actual COBOL files are located in `/workspaces/Legacy-Modernization-Agents/cobol-source/` directory.

## Files Modified

### 1. Config/ai-config.local.env
**Changed:**
```diff
  # Application Settings
- COBOL_SOURCE_FOLDER="source"
- JAVA_OUTPUT_FOLDER="output"
+ COBOL_SOURCE_FOLDER="cobol-source"
+ JAVA_OUTPUT_FOLDER="output/java-output"
+ CSHARP_OUTPUT_FOLDER="output/csharp-output"
  TEST_OUTPUT_FOLDER="TestOutput"
```

### 2. Config/ai-config.env.example
**Changed:**
```diff
  # Application Settings
- COBOL_SOURCE_FOLDER="source"
- JAVA_OUTPUT_FOLDER="output"
+ COBOL_SOURCE_FOLDER="cobol-source"
+ JAVA_OUTPUT_FOLDER="output/java-output"
+ CSHARP_OUTPUT_FOLDER="output/csharp-output"
  TEST_OUTPUT_FOLDER="TestOutput"
```

### 3. Config/ai-config.local.env.template
**Changed:**
```diff
  # Application Settings
- COBOL_SOURCE_FOLDER="source"
- JAVA_OUTPUT_FOLDER="output"
+ COBOL_SOURCE_FOLDER="cobol-source"
+ JAVA_OUTPUT_FOLDER="output/java-output"
+ CSHARP_OUTPUT_FOLDER="output/csharp-output"
  TEST_OUTPUT_FOLDER="TestOutput"
```

## Changes Summary

### Primary Fix
- **COBOL_SOURCE_FOLDER**: Changed from `"source"` to `"cobol-source"` to match actual directory structure

### Additional Improvements
- **JAVA_OUTPUT_FOLDER**: Changed from `"output"` to `"output/java-output"` for better organization
- **CSHARP_OUTPUT_FOLDER**: Added new setting `"output/csharp-output"` to support dual-language conversion feature

## Directory Structure

```
/workspaces/Legacy-Modernization-Agents/
├── cobol-source/           # ✅ COBOL source files (.cbl, .cpy)
│   ├── CUSTOMER-DATA.cpy
│   ├── CUSTOMER-DISPLAY.cbl
│   ├── CUSTOMER-INQUIRY.cbl
│   ├── ERROR-CODES.cpy
│   ├── FORMAT-BALANCE.cbl
│   └── TEST-PROGRAM.cbl
├── output/                 # ✅ Generated output files
│   ├── java-output/        # Java conversion results
│   └── csharp-output/      # C# conversion results
└── source/                 # ❌ Empty/unused directory
```

## Impact

### Before Fix
- Migration tool scanned wrong directory (`source/`)
- Found 0 COBOL files
- Migration failed immediately

### After Fix
- Migration tool scans correct directory (`cobol-source/`)
- Finds 6 COBOL files (4 programs + 2 copybooks)
- Migration proceeds successfully
- Proper output folder organization for multi-language support

## Testing

To verify the fix works:

```bash
# Validate configuration
./doctor.sh

# Run migration
./doctor.sh run
```

Expected output:
```
info: CobolToQuarkusMigration.Helpers.FileHelper[0]
      Scanning directory for COBOL files: cobol-source
info: CobolToQuarkusMigration.Helpers.FileHelper[0]
      Found 6 COBOL files (4 programs, 2 copybooks)
✅ Found 6 COBOL files
```

## Notes

- **TARGET_LANGUAGE setting**: Not required in config files. The `doctor.sh` script prompts for language choice at runtime and sets it as an environment variable.
- **Config file precedence**: `ai-config.local.env` > `ai-config.env` (local overrides template)
- **Template files**: Updated to ensure new users get correct defaults

## Related Files

- [ai-config.local.env](Config/ai-config.local.env) - Active configuration (gitignored)
- [ai-config.env.example](Config/ai-config.env.example) - Example configuration (committed)
- [ai-config.local.env.template](Config/ai-config.local.env.template) - Template for local config (committed)
- [ai-config.env](Config/ai-config.env) - Base configuration (committed)

## Commit Message Template

```
fix(config): correct COBOL source folder path and output structure

- Change COBOL_SOURCE_FOLDER from "source" to "cobol-source"
- Update JAVA_OUTPUT_FOLDER to "output/java-output"
- Add CSHARP_OUTPUT_FOLDER as "output/csharp-output"
- Apply changes to all config templates for consistency

Fixes migration failure where tool couldn't find COBOL files
because it was scanning the wrong directory.

Updated files:
- Config/ai-config.local.env
- Config/ai-config.env.example
- Config/ai-config.local.env.template
```

## Prevention

To prevent this issue in future:

1. **Documentation**: Update setup guides to clearly specify `cobol-source` as the source directory
2. **Validation**: The `doctor.sh` script already validates that COBOL files exist in the configured folder
3. **Consistent naming**: All configuration templates now use the same correct paths
4. **Example files**: Provide sample COBOL files in `cobol-source/` directory by default

## References

- Issue: Migration tool scanning wrong directory
- Solution: Update configuration files to point to `cobol-source/` directory
- Related: [SHELL_SCRIPT_FIXES.md](SHELL_SCRIPT_FIXES.md) - Previous configuration fixes
