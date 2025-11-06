# Output Folder Fix - C# Migration Reports

## Issue
When converting COBOL to C# only, the migration report (`migration-report.md`) was being written to the `java-output` folder instead of the `csharp-output` folder.

## Root Cause
The code was using a `primaryOutputFolder` variable that defaulted to `javaOutputFolder ?? csharpOutputFolder`, which always preferred the Java output folder even when only converting to C#.

## Solution
Modified `MigrationProcess.cs` to intelligently route output files based on the target language:

### Changes Made

#### 1. Dependency Map & Mermaid Diagram Export
Now saves to the appropriate folder(s) based on conversion target:
- **Java only**: Saves to `java-output/`
- **C# only**: Saves to `csharp-output/`
- **Both**: Saves to both folders

#### 2. Migration Report Generation
Now generates separate reports for each language:
- **Java only**: `java-output/migration-report.md` (Java-specific report)
- **C# only**: `csharp-output/migration-report.md` (C#-specific report)
- **Both**: Separate reports in each folder with language-specific content

#### 3. Conversation Log Export
Mirrors the report logic:
- **Java only**: `java-output/migration-conversation-log.md`
- **C# only**: `csharp-output/migration-conversation-log.md`
- **Both**: Copies to both folders

## Code Changes Summary

### Before
```csharp
var primaryOutputFolder = javaOutputFolder ?? csharpOutputFolder ?? cobolSourceFolder;
var dependencyMapPath = Path.Combine(primaryOutputFolder, "dependency-map.json");
// ... always used primaryOutputFolder
```

### After
```csharp
if (convertToJava && convertToCSharp)
{
    // Save to both folders
    await GenerateMigrationReportAsync(..., javaOutputFolder!, ...);
    await GenerateMigrationReportAsync(..., csharpOutputFolder!, ...);
}
else if (convertToJava)
{
    await GenerateMigrationReportAsync(..., javaOutputFolder!, ...);
}
else if (convertToCSharp)
{
    await GenerateMigrationReportAsync(..., csharpOutputFolder!, ...);
}
```

## Testing
Run the following commands to verify:

```bash
# C# only conversion - report should go to csharp-output/
./doctor.sh run --target csharp

# Java only conversion - report should go to java-output/
./doctor.sh run --target java

# Both conversions - separate reports in each folder
./doctor.sh run --target both
```

## Expected Output Locations

### Java Only (`--target java`)
```
java-output/
├── migration-report.md           ✅ Java-specific report
├── dependency-map.json
├── dependency-diagram.md
├── migration-conversation-log.md
└── org/example/*.java
```

### C# Only (`--target csharp`)
```
csharp-output/
├── migration-report.md           ✅ C#-specific report
├── dependency-map.json
├── dependency-diagram.md
├── migration-conversation-log.md
└── namespace/folders/*.cs
```

### Both (`--target both`)
```
java-output/
├── migration-report.md           ✅ Java-specific report
├── dependency-map.json
├── dependency-diagram.md
├── migration-conversation-log.md
└── org/example/*.java

csharp-output/
├── migration-report.md           ✅ C#-specific report
├── dependency-map.json
├── dependency-diagram.md
├── migration-conversation-log.md
└── namespace/folders/*.cs
```

## Build Verification
✅ Build succeeded with no errors
✅ All changes compile successfully
✅ No breaking changes to existing functionality
