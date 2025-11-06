# COBOL to Java/C# Dual-Language Migration Guide

## 📋 Table of Contents
- [Overview](#overview)
- [What Changed](#what-changed)
- [Architecture Changes](#architecture-changes)
- [File-by-File Changes](#file-by-file-changes)
- [Configuration Updates](#configuration-updates)
- [Usage Guide](#usage-guide)
- [Migration Workflow](#migration-workflow)
- [Output Structure](#output-structure)
- [Troubleshooting](#troubleshooting)

---

## Overview

This guide documents the complete transformation of the COBOL migration tool from a **Java-only** converter to a **dual-language** converter that supports:
- ✅ Java Quarkus microservices
- ✅ C# .NET applications
- ✅ Simultaneous conversion to both languages

### Original State
The application only converted COBOL programs to Java Quarkus with a fixed output folder structure.

### Current State
The application now supports user-selectable conversion targets (Java, C#, or Both) with intelligent output folder routing, separate migration reports, and language-specific optimizations.

---

## What Changed

### High-Level Changes
1. **Added C# Conversion Support** - Full C# .NET code generation capability
2. **User Language Selection** - Interactive menu and command-line options
3. **Dual Output Folders** - Separate `java-output/` and `csharp-output/` directories
4. **Smart Report Generation** - Language-specific migration reports
5. **Enhanced CLI** - New `--target` and `--csharp-output` parameters
6. **Updated Bash Automation** - Interactive language selection in `doctor.sh`

### Key Benefits
- 🎯 **Flexibility**: Choose the target language that fits your needs
- 📊 **Comparison**: Generate both outputs to evaluate which works better
- 🔄 **Backward Compatible**: Existing Java-only workflows still work
- 📁 **Organized**: Clean separation of Java and C# outputs

---

## Architecture Changes

### Agent System Enhancement

#### Before
```
CobolAnalyzerAgent → JavaConverterAgent → Output to java-output/
                  ↓
            DependencyMapperAgent
```

#### After
```
CobolAnalyzerAgent → JavaConverterAgent → Output to java-output/
                  ↓                     ↘
            DependencyMapperAgent        CSharpConverterAgent → Output to csharp-output/
```

### New Agent: CSharpConverterAgent
**Location**: `Agents/CSharpConverterAgent.cs`

**Purpose**: Converts analyzed COBOL code to modern C# .NET applications

**Key Features**:
- Converts COBOL data structures to C# classes
- Translates COBOL procedures to C# methods
- Generates namespace-based folder structure
- Supports modern C# features (nullable types, LINQ, async/await)
- Creates migration metadata in comments

**Interface**: `Agents/Interfaces/ICSharpConverterAgent.cs`

---

## File-by-File Changes

### 1. Models/Settings.cs

**Purpose**: Application configuration model

**Changes Added**:
```csharp
public class ApplicationSettings
{
    // EXISTING PROPERTIES
    public string CobolSourceFolder { get; set; } = "cobol-source";
    public string JavaOutputFolder { get; set; } = "java-output";
    public string UnitTestOutputFolder { get; set; } = "unit-tests";
    
    // NEW PROPERTIES
    public string CSharpOutputFolder { get; set; } = "csharp-output";
    public string TargetLanguage { get; set; } = "Java"; // Java, CSharp, or Both
}
```

**Impact**: Enables configuration of C# output location and target language selection

---

### 2. Helpers/FileHelper.cs

**Purpose**: File I/O operations for migration outputs

**New Methods Added**:

#### SaveCSharpFileAsync
```csharp
public async Task SaveCSharpFileAsync(CSharpFile csharpFile, string outputFolder)
```
- Saves C# files with namespace-based directory structure
- Creates nested folders matching namespace hierarchy
- Example: `MyApp.Services.Customer` → `outputFolder/MyApp/Services/Customer.cs`

#### SanitizeCSharpFileName
```csharp
public string SanitizeCSharpFileName(string fileName)
```
- Converts COBOL file names to valid C# file names
- Handles special characters and naming conventions
- Example: `customer-master-update.cbl` → `CustomerMasterUpdate.cs`

#### ExtractCSharpClassNameFromContent
```csharp
public string ExtractCSharpClassNameFromContent(string content)
```
- Parses C# code to extract the primary class name
- Handles various class declaration patterns
- Fallback to filename if class not found

**Impact**: Provides complete C# file management capabilities matching Java file operations

---

### 3. MigrationProcess.cs

**Purpose**: Main migration orchestration logic

**Major Refactoring**:

#### Added C# Agent Initialization
```csharp
public void InitializeAgents()
{
    // Existing agents
    _cobolAnalyzerAgent = new CobolAnalyzerAgent(...);
    _javaConverterAgent = new JavaConverterAgent(...);
    _dependencyMapperAgent = new DependencyMapperAgent(...);
    
    // NEW AGENT
    _csharpConverterAgent = new CSharpConverterAgent(
        _kernelBuilder,
        loggerFactory.CreateLogger<CSharpConverterAgent>(),
        _settings.AISettings.JavaConverterModelId, // Reuses same AI model
        _enhancedLogger,
        _chatLogger);
}
```

#### Dual-Language RunAsync Method
**New Signature**:
```csharp
public async Task RunAsync(
    string cobolSourceFolder,
    string? javaOutputFolder,           // Now nullable
    string? csharpOutputFolder,         // NEW parameter
    string targetLanguage = "Java",     // NEW parameter
    Action<string, int, int>? progressCallback = null)
```

**Language Selection Logic**:
```csharp
var convertToJava = targetLanguage.Equals("Java", StringComparison.OrdinalIgnoreCase) ||
                   targetLanguage.Equals("Both", StringComparison.OrdinalIgnoreCase);
                   
var convertToCSharp = targetLanguage.Equals("CSharp", StringComparison.OrdinalIgnoreCase) ||
                     targetLanguage.Equals("Both", StringComparison.OrdinalIgnoreCase);
```

#### C# Conversion Flow
```csharp
// Convert to C# if requested
if (convertToCSharp && csharpOutputFolder != null)
{
    currentStep++;
    _enhancedLogger.ShowStep(currentStep, totalSteps, "C# Conversion", 
        "Converting to C# .NET applications");
    
    csharpFiles = await _csharpConverterAgent!.ConvertToCSharpAsync(
        cobolFiles,
        cobolAnalyses,
        progressCallback);
    
    // Save C# files
    currentStep++;
    for (int i = 0; i < csharpFiles.Count; i++)
    {
        await _fileHelper.SaveCSharpFileAsync(csharpFiles[i], csharpOutputFolder);
    }
}
```

#### Smart Output Folder Routing
**Dependency Maps & Diagrams**:
```csharp
// Save to appropriate folder(s) based on target language
if (convertToJava && convertToCSharp)
{
    // Save to BOTH folders
    await _fileHelper.SaveDependencyMapAsync(dependencyMap, 
        Path.Combine(javaOutputFolder!, "dependency-map.json"));
    await _fileHelper.SaveDependencyMapAsync(dependencyMap, 
        Path.Combine(csharpOutputFolder!, "dependency-map.json"));
}
else if (convertToJava)
{
    // Java folder only
    await _fileHelper.SaveDependencyMapAsync(dependencyMap, 
        Path.Combine(javaOutputFolder!, "dependency-map.json"));
}
else if (convertToCSharp)
{
    // C# folder only
    await _fileHelper.SaveDependencyMapAsync(dependencyMap, 
        Path.Combine(csharpOutputFolder!, "dependency-map.json"));
}
```

#### Language-Specific Migration Reports
```csharp
// Generate separate reports for each language
if (convertToJava && convertToCSharp)
{
    // Java-specific report
    await GenerateMigrationReportAsync(cobolFiles, javaFiles, null, 
        dependencyMap, javaOutputFolder!, startTime, "Java");
    
    // C#-specific report
    await GenerateMigrationReportAsync(cobolFiles, null, csharpFiles, 
        dependencyMap, csharpOutputFolder!, startTime, "CSharp");
}
else if (convertToJava)
{
    await GenerateMigrationReportAsync(cobolFiles, javaFiles, null, 
        dependencyMap, javaOutputFolder!, startTime, "Java");
}
else if (convertToCSharp)
{
    await GenerateMigrationReportAsync(cobolFiles, null, csharpFiles, 
        dependencyMap, csharpOutputFolder!, startTime, "CSharp");
}
```

#### Updated Report Generation Method
**New Signature**:
```csharp
private async Task GenerateMigrationReportAsync(
    List<CobolFile> cobolFiles,
    List<JavaFile>? javaFiles,      // Now nullable
    List<CSharpFile>? csharpFiles,  // NEW parameter
    DependencyMap dependencyMap,
    string outputFolder,
    DateTime startTime,
    string targetLanguage)
```

**Report Content Adaptation**:
- Shows Java file mapping only when Java files are generated
- Shows C# file mapping only when C# files are generated
- Adjusts "Next Steps" section based on target language
- Includes language-specific configuration instructions

**Impact**: Complete dual-language orchestration with intelligent routing and reporting

---

### 4. Program.cs

**Purpose**: Application entry point and CLI

**New Command-Line Options**:

#### --csharp-output Option
```csharp
var csharpOutputOption = new Option<string?>(
    name: "--csharp-output",
    description: "Output folder for C# files")
{
    IsRequired = false
};
rootCommand.AddOption(csharpOutputOption);
```

#### --target Option
```csharp
var targetOption = new Option<string?>(
    name: "--target",
    description: "Target language: Java, CSharp, or Both (default: Java)")
{
    IsRequired = false
};
rootCommand.AddOption(targetOption);
```

#### Enhanced Validation Logic
```csharp
// Determine target language
var targetLanguage = target ?? settings.ApplicationSettings.TargetLanguage;

// Validation: At least one output folder required
var hasJavaOutput = !string.IsNullOrWhiteSpace(javaOutput);
var hasCSharpOutput = !string.IsNullOrWhiteSpace(csharpOutput);

if (!hasJavaOutput && !hasCSharpOutput)
{
    logger.LogError("At least one output folder (--java-output or --csharp-output) must be specified");
    return 1;
}

// Validation: Target language consistency
if (targetLanguage.Equals("Java", StringComparison.OrdinalIgnoreCase) && !hasJavaOutput)
{
    logger.LogError("Target is Java but --java-output not specified");
    return 1;
}

if (targetLanguage.Equals("CSharp", StringComparison.OrdinalIgnoreCase) && !hasCSharpOutput)
{
    logger.LogError("Target is CSharp but --csharp-output not specified");
    return 1;
}

if (targetLanguage.Equals("Both", StringComparison.OrdinalIgnoreCase) && 
    (!hasJavaOutput || !hasCSharpOutput))
{
    logger.LogError("Target is Both but not all output folders specified");
    return 1;
}
```

#### Environment Variable Support
```csharp
private static void OverrideSettingsFromEnvironment(AppSettings settings)
{
    // Existing overrides
    if (!string.IsNullOrEmpty(Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER")))
        settings.ApplicationSettings.CobolSourceFolder = 
            Environment.GetEnvironmentVariable("COBOL_SOURCE_FOLDER")!;
    
    if (!string.IsNullOrEmpty(Environment.GetEnvironmentVariable("JAVA_OUTPUT_FOLDER")))
        settings.ApplicationSettings.JavaOutputFolder = 
            Environment.GetEnvironmentVariable("JAVA_OUTPUT_FOLDER")!;
    
    // NEW OVERRIDES
    if (!string.IsNullOrEmpty(Environment.GetEnvironmentVariable("CSHARP_OUTPUT_FOLDER")))
        settings.ApplicationSettings.CSharpOutputFolder = 
            Environment.GetEnvironmentVariable("CSHARP_OUTPUT_FOLDER")!;
    
    if (!string.IsNullOrEmpty(Environment.GetEnvironmentVariable("TARGET_LANGUAGE")))
        settings.ApplicationSettings.TargetLanguage = 
            Environment.GetEnvironmentVariable("TARGET_LANGUAGE")!;
}
```

#### Updated Migration Process Invocation
```csharp
await migrationProcess.RunAsync(
    cobolSourceFolder,
    hasJavaOutput ? javaOutputFolder : null,    // Nullable
    hasCSharpOutput ? csharpOutputFolder : null, // NEW parameter
    targetLanguage,                              // NEW parameter
    (message, current, total) => {
        logger.LogInformation("[{Current}/{Total}] {Message}", current, total, message);
    });
```

**Impact**: Full CLI support for dual-language conversion with comprehensive validation

---

### 5. Config/appsettings.json

**Purpose**: Default application settings

**Changes**:
```json
{
  "ApplicationSettings": {
    "CobolSourceFolder": "cobol-source",
    "JavaOutputFolder": "java-output",
    "CSharpOutputFolder": "csharp-output",     // NEW
    "UnitTestOutputFolder": "unit-tests",
    "TargetLanguage": "Java"                   // NEW
  },
  "AISettings": {
    // ... existing AI configuration
  }
}
```

**Impact**: Default configuration supports both output folders

---

### 6. doctor.sh

**Purpose**: Bash automation script for setup/test/run workflows

**Major Updates**:

#### Updated show_usage()
```bash
show_usage() {
    echo -e "${BOLD}${BLUE}🧠 COBOL Migration Tool - Multi-Language Support${NC}"
    echo -e "${BLUE}=================================================${NC}"
    echo
    echo -e "${BOLD}Available Commands:${NC}"
    echo -e "  ${GREEN}run${NC}             Start the migration process (interactive language selection)"
    echo
    echo -e "${BOLD}Run Command Options:${NC}"
    echo -e "  --target java     Convert to Java Quarkus"
    echo -e "  --target csharp   Convert to C# .NET"
    echo -e "  --target both     Convert to both languages"
    echo
    echo -e "${BOLD}Examples:${NC}"
    echo -e "  $0 run          ${CYAN}# Start migration (interactive language selection)${NC}"
    echo -e "  $0 run --target java     ${CYAN}# Convert to Java only${NC}"
    echo -e "  $0 run --target csharp   ${CYAN}# Convert to C# only${NC}"
    echo -e "  $0 run --target both     ${CYAN}# Convert to both languages${NC}"
}
```

#### Interactive Language Selection in run_migration()
```bash
run_migration() {
    # Parse command line arguments for --target option
    TARGET_LANG=""
    shift # Remove the 'run' command
    while [[ $# -gt 0 ]]; do
        case $1 in
            --target)
                TARGET_LANG="$2"
                shift 2
                ;;
            *)
                echo -e "${RED}❌ Unknown option: $1${NC}"
                return 1
                ;;
        esac
    done

    # If no target specified, show interactive menu
    if [ -z "$TARGET_LANG" ]; then
        echo -e "${CYAN}${BOLD}Select Target Language for Conversion:${NC}"
        echo "======================================"
        echo
        echo "1) Java Quarkus    - Convert to modern Java microservices"
        echo "2) C# .NET         - Convert to modern C# applications"
        echo "3) Both            - Convert to both Java and C# (parallel evaluation)"
        echo
        read -p "Enter your choice (1-3): " choice

        case $choice in
            1)
                TARGET_LANG="java"
                echo -e "${GREEN}✅ Selected: Java Quarkus${NC}"
                ;;
            2)
                TARGET_LANG="csharp"
                echo -e "${GREEN}✅ Selected: C# .NET${NC}"
                ;;
            3)
                TARGET_LANG="both"
                echo -e "${GREEN}✅ Selected: Both Languages${NC}"
                ;;
            *)
                echo -e "${RED}❌ Invalid choice. Defaulting to Java.${NC}"
                TARGET_LANG="java"
                ;;
        esac
        echo
    fi

    # Normalize target language
    TARGET_LANG=$(echo "$TARGET_LANG" | tr '[:upper:]' '[:lower:]')

    # Set output folders based on target language
    case $TARGET_LANG in
        "java")
            echo -e "${BLUE}🚀 Starting COBOL to Java Quarkus Migration...${NC}"
            dotnet run --project "$PROJECT_DIR/CobolModernization.csproj" -- \
                --cobol-source ./cobol-source \
                --java-output ./java-output \
                --target Java
            ;;
        "csharp"|"cs")
            echo -e "${BLUE}🚀 Starting COBOL to C# .NET Migration...${NC}"
            dotnet run --project "$PROJECT_DIR/CobolModernization.csproj" -- \
                --cobol-source ./cobol-source \
                --csharp-output ./csharp-output \
                --target CSharp
            ;;
        "both")
            echo -e "${BLUE}🚀 Starting COBOL to Java & C# Migration...${NC}"
            echo -e "${YELLOW}⚠️  Note: This will take approximately 2x the time and API costs${NC}"
            dotnet run --project "$PROJECT_DIR/CobolModernization.csproj" -- \
                --cobol-source ./cobol-source \
                --java-output ./java-output \
                --csharp-output ./csharp-output \
                --target Both
            ;;
        *)
            echo -e "${RED}❌ Invalid target language: $TARGET_LANG${NC}"
            return 1
            ;;
    esac

    # Show completion message
    echo
    echo -e "${GREEN}✅ Migration completed!${NC}"
    case $TARGET_LANG in
        "java")
            echo "📁 Output location: ./java-output/"
            echo "📄 Migration report: ./java-output/migration-report.md"
            ;;
        "csharp"|"cs")
            echo "📁 Output location: ./csharp-output/"
            echo "📄 Migration report: ./csharp-output/migration-report.md"
            ;;
        "both")
            echo "📁 Java output: ./java-output/"
            echo "📁 C# output: ./csharp-output/"
            echo "📄 Migration reports in each folder"
            ;;
    esac
}
```

#### Updated run_test()
```bash
run_test() {
    # ... existing test logic ...
    
    # Check output directories
    echo ""
    echo "Checking output directories..."
    
    # Check Java output
    if [ -d "$SCRIPT_DIR/java-output" ]; then
        java_files=$(find "$SCRIPT_DIR/java-output" -name "*.java" 2>/dev/null | wc -l)
        if [ "$java_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found previous Java output ($java_files files)${NC}"
        fi
    fi

    # NEW: Check C# output
    if [ -d "$SCRIPT_DIR/csharp-output" ]; then
        csharp_files=$(find "$SCRIPT_DIR/csharp-output" -name "*.cs" 2>/dev/null | wc -l)
        if [ "$csharp_files" -gt 0 ]; then
            echo -e "${GREEN}✅ Found previous C# output ($csharp_files files)${NC}"
        fi
    fi
    
    echo ""
    echo -e "${GREEN}🚀 Ready to run migration!${NC}"
    echo ""
    echo "Migration Options:"
    echo "  Interactive:  ./doctor.sh run"
    echo "  Java:         ./doctor.sh run --target java"
    echo "  C#:           ./doctor.sh run --target csharp"
    echo "  Both:         ./doctor.sh run --target both"
}
```

#### Updated run_validate()
```bash
run_validate() {
    # ... existing validation logic ...
    
    # Check directories
    for dir in "cobol-source" "java-output" "csharp-output"; do  # Added csharp-output
        if [ -d "$SCRIPT_DIR/$dir" ]; then
            echo -e "${GREEN}✅ Directory: $dir${NC}"
        else
            echo -e "${YELLOW}⚠️  Creating directory: $dir${NC}"
            mkdir -p "$SCRIPT_DIR/$dir"
        fi
    done
}
```

#### Updated main()
```bash
main() {
    # Create required directories if they don't exist
    mkdir -p "$SCRIPT_DIR/cobol-source" \
             "$SCRIPT_DIR/java-output" \
             "$SCRIPT_DIR/csharp-output" \   # NEW
             "$SCRIPT_DIR/Logs"

    case "${1:-doctor}" in
        "run")
            shift # Remove 'run' from arguments
            run_migration "$@"  # Pass remaining arguments
            ;;
        # ... other commands ...
    esac
}
```

**Impact**: Complete bash script automation with interactive language selection and parameter support

---

## Configuration Updates

### Environment Variables

#### New Variables Added
```bash
# In Config/ai-config.local.env or environment
export CSHARP_OUTPUT_FOLDER="csharp-output"
export TARGET_LANGUAGE="Java"  # Options: Java, CSharp, Both
```

#### All Available Variables
```bash
# Azure OpenAI Configuration (Required)
export AZURE_OPENAI_ENDPOINT="https://your-resource.openai.azure.com/"
export AZURE_OPENAI_API_KEY="your-api-key"
export AZURE_OPENAI_DEPLOYMENT_NAME="gpt-4"

# Folder Configuration
export COBOL_SOURCE_FOLDER="cobol-source"
export JAVA_OUTPUT_FOLDER="java-output"
export CSHARP_OUTPUT_FOLDER="csharp-output"        # NEW
export UNIT_TEST_OUTPUT_FOLDER="unit-tests"

# Migration Configuration
export TARGET_LANGUAGE="Java"                       # NEW

# AI Model Configuration
export COBOL_ANALYZER_MODEL_ID="gpt-4"
export JAVA_CONVERTER_MODEL_ID="gpt-4"
export DEPENDENCY_MAPPER_MODEL_ID="gpt-4"
```

---

## Usage Guide

### Quick Start

#### Interactive Mode (Recommended)
```bash
# Run with interactive language selection menu
./doctor.sh run
```

This will display:
```
Select Target Language for Conversion:
======================================

1) Java Quarkus    - Convert to modern Java microservices
2) C# .NET         - Convert to modern C# applications
3) Both            - Convert to both Java and C# (parallel evaluation)

Enter your choice (1-3):
```

#### Command-Line Mode

**Java Only**:
```bash
./doctor.sh run --target java
```

**C# Only**:
```bash
./doctor.sh run --target csharp
```

**Both Languages**:
```bash
./doctor.sh run --target both
```

### Direct .NET CLI Usage

**Java Only**:
```bash
dotnet run --project CobolModernization.csproj -- \
    --cobol-source ./cobol-source \
    --java-output ./java-output \
    --target Java
```

**C# Only**:
```bash
dotnet run --project CobolModernization.csproj -- \
    --cobol-source ./cobol-source \
    --csharp-output ./csharp-output \
    --target CSharp
```

**Both Languages**:
```bash
dotnet run --project CobolModernization.csproj -- \
    --cobol-source ./cobol-source \
    --java-output ./java-output \
    --csharp-output ./csharp-output \
    --target Both
```

### Environment Variable Configuration

Create or edit `Config/ai-config.local.env`:
```bash
# Set default target language
export TARGET_LANGUAGE="CSharp"  # or "Java" or "Both"

# Set default output folders
export JAVA_OUTPUT_FOLDER="java-output"
export CSHARP_OUTPUT_FOLDER="csharp-output"
```

Then run:
```bash
./doctor.sh run
```

---

## Migration Workflow

### Step-by-Step Process

#### 1. File Discovery
```
📁 Scanning for COBOL programs and copybooks
```
- Scans `cobol-source/` directory
- Identifies `.cbl` (programs) and `.cpy` (copybooks) files
- Reports file count

#### 2. Dependency Analysis
```
🔗 Mapping COBOL relationships and dependencies
```
- Analyzes CALL statements
- Maps COPY/INCLUDE relationships
- Generates dependency graph
- Creates Mermaid diagram

#### 3. COBOL Analysis
```
🧠 AI-powered code structure analysis
```
- Uses Azure OpenAI to analyze each file
- Identifies data structures, procedures, logic
- Maps COBOL constructs to modern patterns

#### 4. Language Conversion (Java)
```
☕ Converting to Java Quarkus microservices
```
- Converts data structures to Java classes
- Translates procedures to Java methods
- Generates Quarkus annotations
- Creates package structure

#### 5. Language Conversion (C#)
```
🎯 Converting to C# .NET applications
```
- Converts data structures to C# classes
- Translates procedures to C# methods
- Uses modern C# features (nullable types, LINQ)
- Creates namespace structure

#### 6. File Generation
```
💾 Writing output files to disk
```
- Java: Package-based folder structure
- C#: Namespace-based folder structure
- Preserves logical organization

#### 7. Report Generation
```
📊 Creating migration summary and metrics
```
- Language-specific reports
- File mapping tables
- Dependency analysis
- Migration metrics
- Next steps guidance

---

## Output Structure

### Java Only Migration

```
java-output/
├── migration-report.md              # Java-specific migration report
├── dependency-map.json              # Dependency analysis
├── dependency-diagram.md            # Mermaid visualization
├── migration-conversation-log.md   # AI conversation log
└── org/
    └── example/
        ├── CustomerMasterUpdate.java
        ├── PayrollProcessor.java
        └── models/
            ├── CustomerRecord.java
            └── PaymentRecord.java
```

### C# Only Migration

```
csharp-output/
├── migration-report.md              # C#-specific migration report
├── dependency-map.json              # Dependency analysis
├── dependency-diagram.md            # Mermaid visualization
├── migration-conversation-log.md   # AI conversation log
└── YourApp/
    ├── CustomerMasterUpdate.cs
    ├── PayrollProcessor.cs
    └── Models/
        ├── CustomerRecord.cs
        └── PaymentRecord.cs
```

### Both Languages Migration

```
java-output/
├── migration-report.md              # Java-specific report
├── dependency-map.json
├── dependency-diagram.md
├── migration-conversation-log.md
└── org/example/
    └── *.java

csharp-output/
├── migration-report.md              # C#-specific report
├── dependency-map.json
├── dependency-diagram.md
├── migration-conversation-log.md
└── YourApp/
    └── *.cs
```

### Migration Report Contents

Each `migration-report.md` includes:

1. **📊 Migration Overview**
   - Source file count
   - Generated file count (language-specific)
   - Dependency metrics
   - Migration time

2. **🗂️ File Mapping** (Language-specific)
   - COBOL → Java/C# mapping table
   - File type classification

3. **🔗 Dependency Analysis**
   - Circular dependencies (if any)
   - Most used copybooks
   - Dependency relationships

4. **📈 Migration Metrics**
   - Files per minute
   - Average file size
   - Total lines of code

5. **🚀 Next Steps**
   - Review checklist
   - Testing recommendations
   - Configuration guidance (language-specific)

6. **📁 Generated Files**
   - List of all output artifacts

---

## Troubleshooting

### Common Issues

#### Issue: "At least one output folder must be specified"
**Cause**: No output folder provided
**Solution**: 
```bash
# Specify at least one output folder
./doctor.sh run --target java
# OR
dotnet run -- --cobol-source ./cobol-source --java-output ./java-output --target Java
```

#### Issue: "Target is CSharp but --csharp-output not specified"
**Cause**: Mismatch between target and output folder
**Solution**:
```bash
# Correct usage
./doctor.sh run --target csharp
# OR
dotnet run -- --cobol-source ./cobol-source --csharp-output ./csharp-output --target CSharp
```

#### Issue: "Target is Both but not all output folders specified"
**Cause**: Missing one of the output folders when using "Both"
**Solution**:
```bash
# Must provide both folders for "Both" target
./doctor.sh run --target both
# OR
dotnet run -- --cobol-source ./cobol-source \
              --java-output ./java-output \
              --csharp-output ./csharp-output \
              --target Both
```

#### Issue: Migration report in wrong folder
**Cause**: Fixed in latest update
**Solution**: Update to latest version - reports now go to correct language-specific folders

#### Issue: C# files not generating
**Cause**: Agent not initialized or target not set correctly
**Solution**:
1. Check `appsettings.json` has `CSharpOutputFolder` setting
2. Verify `--target CSharp` or `--target Both` is specified
3. Check logs for agent initialization errors

### Validation Commands

**Test Configuration**:
```bash
./doctor.sh test
```

**Validate System**:
```bash
./doctor.sh validate
```

**Check Dependencies**:
```bash
dotnet build CobolModernization.csproj
```

### Performance Considerations

#### API Costs
- **Java Only**: 1x API calls
- **C# Only**: 1x API calls  
- **Both**: 2x API calls (approximately double the cost)

#### Time Estimates
For 10 COBOL files:
- **Java Only**: ~2-3 minutes
- **C# Only**: ~2-3 minutes
- **Both**: ~4-6 minutes

---

## Summary of Changes

### Files Modified
1. ✅ `Models/Settings.cs` - Added C# configuration properties
2. ✅ `Helpers/FileHelper.cs` - Added C# file operations
3. ✅ `MigrationProcess.cs` - Dual-language orchestration
4. ✅ `Program.cs` - Enhanced CLI with new options
5. ✅ `Config/appsettings.json` - Default C# settings
6. ✅ `doctor.sh` - Interactive language selection

### Files Created
1. ✅ `Agents/CSharpConverterAgent.cs` - C# conversion logic
2. ✅ `Agents/Interfaces/ICSharpConverterAgent.cs` - C# agent interface
3. ✅ `Models/CSharpFile.cs` - C# file model (if not existing)

### Backward Compatibility
- ✅ Existing Java-only workflows unchanged
- ✅ Default behavior remains Java conversion
- ✅ All previous command-line options still work
- ✅ Configuration file structure compatible

### Testing Status
- ✅ Build: Success
- ✅ Java-only conversion: Tested
- ✅ C#-only conversion: Tested
- ✅ Both languages: Tested
- ✅ Interactive menu: Tested
- ✅ Command-line parameters: Tested
- ✅ Output folder routing: Fixed and tested
- ✅ Report generation: Fixed and tested

---

## Next Steps

1. **Try Different Targets**: Test with your COBOL files using all three modes
2. **Compare Outputs**: Review Java vs C# to determine best fit
3. **Customize Configuration**: Adjust `appsettings.json` for your needs
4. **Review Reports**: Check migration reports for insights
5. **Validate Code**: Test generated code in your target environment

---

## Support

For issues or questions:
1. Check `migration-report.md` in output folder
2. Review `migration-conversation-log.md` for AI interactions
3. Check `Logs/` directory for detailed logging
4. Run `./doctor.sh doctor` for system diagnostics

---

**Last Updated**: November 6, 2025
**Version**: 2.0 (Dual-Language Support)
**Compatibility**: .NET 8.0+, Azure OpenAI GPT-4+
