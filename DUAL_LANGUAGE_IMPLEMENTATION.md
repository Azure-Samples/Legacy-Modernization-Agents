# Migration Tool Enhancement: Dual Language Support

## Summary

The COBOL migration tool has been successfully enhanced to support **dual-language conversion**, allowing users to convert COBOL programs to **Java Quarkus**, **C# .NET**, or **both languages simultaneously**.

## Changes Made

### 1. **New Models and Interfaces**
- ✅ `CSharpFile` model already existed
- ✅ `ICSharpConverterAgent` interface already existed
- ✅ `CSharpConverterAgent` implementation already existed

### 2. **Settings Configuration**
**File**: `Models/Settings.cs`
- Added `CSharpOutputFolder` property to `ApplicationSettings`
- Added `TargetLanguage` property to specify conversion target (Java, CSharp, or Both)

**File**: `Config/appsettings.json`
- Added `CSharpOutputFolder` setting (default: "csharp-output")
- Added `TargetLanguage` setting (default: "Java")

### 3. **File Helper Updates**
**File**: `Helpers/FileHelper.cs`
- Added `SaveCSharpFileAsync()` method to save C# files with proper namespace structure
- Added `SanitizeCSharpFileName()` helper method
- Added `ExtractCSharpClassNameFromContent()` helper method

### 4. **Migration Process Updates**
**File**: `MigrationProcess.cs`

**Agent Initialization:**
- Updated `InitializeAgents()` to create 4 agents (was 3):
  1. CobolAnalyzerAgent
  2. JavaConverterAgent
  3. **CSharpConverterAgent** (new)
  4. DependencyMapperAgent

**Main Migration Logic:**
- Updated `RunAsync()` method to accept optional `csharpOutputFolder` parameter
- Added `targetLanguage` parameter to control conversion behavior
- Refactored conversion logic to support:
  - **Java only**: Converts to Java Quarkus
  - **C# only**: Converts to C# .NET
  - **Both**: Converts to both languages using shared COBOL analysis

**Report Generation:**
- Updated `GenerateMigrationReportAsync()` to handle both Java and C# files
- Enhanced report to show separate file mappings for each language
- Added language-specific next steps in the report

### 5. **Program.cs Updates**
**Command Line Interface:**
- Updated tool description: "COBOL Migration Tool - Convert COBOL to Java and/or C#"
- Added new command-line options:
  - `--csharp-output` / `-cs`: Path to C# output folder
  - `--target` / `-t`: Target language (Java, CSharp, or Both)

**Configuration Management:**
- Updated `OverrideSettingsFromEnvironment()` to handle:
  - `CSHARP_OUTPUT_FOLDER` environment variable
  - `TARGET_LANGUAGE` environment variable

**Validation Logic:**
- Added validation for target language parameter
- Added conditional validation for output folders based on target language
- Enhanced error messages for missing configuration

### 6. **Documentation**
**New File**: `DUAL_LANGUAGE_GUIDE.md`
- Comprehensive guide for dual-language conversion
- Usage examples for all three modes (Java, C#, Both)
- Command-line option reference
- Output structure documentation
- Conversion feature comparison
- Best practices for choosing target language
- Troubleshooting section

**New Directory**: `csharp-output/`
- Created output directory for C# conversions

## Architecture Benefits

### Efficiency Through Shared Analysis
The architecture is optimized for dual-language conversion:
1. **COBOL Analysis**: Performed once and shared between both converters
2. **Dependency Mapping**: Performed once and shared
3. **Independent Conversion**: Java and C# conversions happen independently using shared analysis
4. **Optimized for "Both"**: When converting to both languages, analysis overhead is minimized

### Agent Separation
Each agent has a specific responsibility:
- **CobolAnalyzerAgent**: Language-agnostic COBOL analysis
- **JavaConverterAgent**: Java-specific conversion with Quarkus patterns
- **CSharpConverterAgent**: C#-specific conversion with .NET patterns
- **DependencyMapperAgent**: Language-agnostic dependency analysis

## Usage Examples

### Convert to Java Only (Default)
```bash
dotnet run -- --cobol-source cobol-source --java-output java-output
```

### Convert to C# Only
```bash
dotnet run -- --cobol-source cobol-source --csharp-output csharp-output --target CSharp
```

### Convert to Both Languages
```bash
dotnet run -- \
  --cobol-source cobol-source \
  --java-output java-output \
  --csharp-output csharp-output \
  --target Both
```

### Using Environment Variables
```bash
export TARGET_LANGUAGE="Both"
export CSHARP_OUTPUT_FOLDER="my-csharp-output"
dotnet run
```

## Output Structure

### Java Output
```
java-output/
├── org/example/
│   └── ConvertedProgram.java
├── dependency-map.json
├── dependency-diagram.md
├── migration-report.md
└── migration-conversation-log.md
```

### C# Output
```
csharp-output/
├── ConvertedCobol/
│   └── ConvertedProgram.cs
├── dependency-map.json
├── dependency-diagram.md
├── migration-report.md
└── migration-conversation-log.md
```

## Key Features

### Language-Specific Conversions

**Java Quarkus:**
- Jakarta EE annotations (@ApplicationScoped, @Inject)
- Quarkus application patterns
- RESTful web services
- Modern Java features (streams, lambdas)

**C# .NET:**
- Async/await patterns
- XML documentation comments
- Dependency injection
- Modern C# features (records, pattern matching, nullable references)
- LINQ for data operations

### Shared Features
- Content sanitization for Azure OpenAI content filtering
- Retry logic for API calls
- Comprehensive error handling
- API call tracking and analytics
- Detailed migration reports

## Migration Report Enhancements

The enhanced migration report includes:
- **Language-specific file mappings**: Separate tables for Java and C# conversions
- **Multi-language overview**: Shows counts for both Java and C# files
- **Targeted next steps**: Different recommendations for Java vs C#
- **Unified dependency analysis**: Shared across all conversions

## Testing the Changes

1. **Build the project**:
   ```bash
   dotnet build
   ```

2. **Test Java conversion** (existing functionality):
   ```bash
   dotnet run -- --cobol-source cobol-source --java-output java-output-test
   ```

3. **Test C# conversion** (new functionality):
   ```bash
   dotnet run -- --cobol-source cobol-source --csharp-output csharp-output-test --target CSharp
   ```

4. **Test dual conversion** (new functionality):
   ```bash
   dotnet run -- \
     --cobol-source cobol-source \
     --java-output java-test \
     --csharp-output csharp-test \
     --target Both
   ```

## Backward Compatibility

✅ **Fully backward compatible**
- Default behavior remains Java-only conversion
- Existing command-line usage works without changes
- Configuration files with old schema still work
- All existing features remain functional

## Performance Considerations

- **Java only**: Same performance as before
- **C# only**: Similar performance to Java conversion
- **Both languages**: Approximately 2x conversion time (analysis is shared)
- **API costs**: Doubled when using `--target Both` due to two conversion passes

## Future Enhancements

Potential improvements for future versions:
1. **Parallel conversion**: Run Java and C# conversions in parallel
2. **Language comparison**: Automated analysis of conversion quality differences
3. **Hybrid output**: Single application using both Java and C# components
4. **Additional languages**: Python, Go, TypeScript support
5. **Conversion preferences**: Fine-tune conversion strategies per language

## Conclusion

The migration tool now provides **flexible, user-driven language selection** for COBOL modernization, allowing organizations to:
- Evaluate multiple target platforms
- Support diverse infrastructure needs
- Compare AI conversion quality across languages
- Make informed decisions about modernization strategy

All changes maintain **full backward compatibility** while providing powerful new capabilities for dual-language migration scenarios.
