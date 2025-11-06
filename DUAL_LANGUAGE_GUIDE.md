# Dual Language Conversion Guide: Java & C#

This guide explains how to use the COBOL migration tool to convert COBOL programs to **both Java Quarkus and C# .NET** applications.

## Overview

The migration tool now supports converting COBOL programs to:
- **Java** (Quarkus framework) - Modern Java microservices
- **C#** (.NET) - Modern C# applications
- **Both** - Convert to both languages simultaneously

## Usage

### Command Line Options

```bash
# Convert to Java only (default)
dotnet run -- --cobol-source cobol-source --java-output java-output

# Convert to C# only
dotnet run -- --cobol-source cobol-source --csharp-output csharp-output --target CSharp

# Convert to both Java and C#
dotnet run -- --cobol-source cobol-source --java-output java-output --csharp-output csharp-output --target Both
```

### Available Command Line Options

- `--cobol-source` / `-s` : Path to COBOL source files folder
- `--java-output` / `-j` : Path to Java output folder
- `--csharp-output` / `-cs` : Path to C# output folder
- `--target` / `-t` : Target language (Java, CSharp, or Both)
- `--config` / `-c` : Path to configuration file (default: Config/appsettings.json)

### Configuration File

You can also set default values in `Config/appsettings.json`:

```json
{
  "ApplicationSettings": {
    "CobolSourceFolder": "cobol-source",
    "JavaOutputFolder": "java-output",
    "CSharpOutputFolder": "csharp-output",
    "TargetLanguage": "Java"
  }
}
```

### Environment Variables

You can override settings using environment variables:

```bash
export COBOL_SOURCE_FOLDER="my-cobol-files"
export JAVA_OUTPUT_FOLDER="my-java-output"
export CSHARP_OUTPUT_FOLDER="my-csharp-output"
export TARGET_LANGUAGE="Both"
```

## Examples

### Example 1: Convert to Java Only

```bash
dotnet run -- \
  --cobol-source ./legacy-cobol \
  --java-output ./modernized/java \
  --target Java
```

**Output:**
- Java files in `./modernized/java/`
- Migration report: `./modernized/java/migration-report.md`
- Dependency diagram: `./modernized/java/dependency-diagram.md`

### Example 2: Convert to C# Only

```bash
dotnet run -- \
  --cobol-source ./legacy-cobol \
  --csharp-output ./modernized/csharp \
  --target CSharp
```

**Output:**
- C# files in `./modernized/csharp/`
- Migration report: `./modernized/csharp/migration-report.md`
- Dependency diagram: `./modernized/csharp/dependency-diagram.md`

### Example 3: Convert to Both Languages

```bash
dotnet run -- \
  --cobol-source ./legacy-cobol \
  --java-output ./modernized/java \
  --csharp-output ./modernized/csharp \
  --target Both
```

**Output:**
- Java files in `./modernized/java/`
- C# files in `./modernized/csharp/`
- Migration report: `./modernized/java/migration-report.md` (includes both languages)
- Dependency diagram: `./modernized/java/dependency-diagram.md`

## Output Structure

### Java Output
```
java-output/
├── org/
│   └── example/
│       ├── CustomerMasterUpdate.java
│       └── ...
├── dependency-map.json
├── dependency-diagram.md
├── migration-report.md
└── migration-conversation-log.md
```

### C# Output
```
csharp-output/
├── ConvertedCobol/
│   ├── CustomerMasterUpdate.cs
│   └── ...
├── dependency-map.json
├── dependency-diagram.md
├── migration-report.md
└── migration-conversation-log.md
```

## Conversion Features

### Java Conversion
- **Framework**: Quarkus microservices
- **Features**:
  - Jakarta EE annotations
  - Dependency injection with CDI
  - RESTful web services
  - JPA for database access
  - Modern Java patterns (streams, lambdas)

### C# Conversion
- **Framework**: .NET 8.0+
- **Features**:
  - Async/await patterns
  - Dependency injection
  - XML documentation comments
  - Modern C# features (records, pattern matching)
  - LINQ for data operations
  - Nullable reference types

## Migration Report

The migration report includes:
- Overview of converted files (Java and/or C#)
- File mapping (COBOL → Java/C#)
- Dependency analysis
- Migration metrics
- Next steps for each language
- API call statistics

## Best Practices

### Converting to Both Languages

1. **Start with Analysis**: Run COBOL analysis first to understand dependencies
2. **Convert to Both**: Use `--target Both` to leverage the same analysis for both conversions
3. **Compare Results**: Review both Java and C# outputs to choose the best fit
4. **Iterative Refinement**: Test and refine both versions independently

### Choosing Target Language

**Choose Java if:**
- You have existing Java infrastructure
- You need Quarkus/Jakarta EE features
- Your team is more familiar with Java
- You're targeting JVM-based deployments

**Choose C# if:**
- You have existing .NET infrastructure
- You want to leverage Azure services
- Your team is more familiar with C#
- You're targeting Windows or Azure deployments

**Choose Both if:**
- You're evaluating which language fits better
- You want to compare AI conversion quality
- You have multi-language infrastructure
- You need to support different deployment targets

## Troubleshooting

### Missing Output Folder Error

If you see "Java output folder not specified" or "C# output folder not specified":

1. Check your `--target` parameter matches your output folders
2. For `--target Java`: Ensure `--java-output` is specified
3. For `--target CSharp`: Ensure `--csharp-output` is specified
4. For `--target Both`: Ensure both output folders are specified

### Performance Considerations

- Converting to both languages takes approximately 2x the time
- Each language conversion is independent (parallelization possible)
- API call costs are doubled when using `--target Both`

## AI Agent Architecture

The tool uses specialized AI agents:

1. **CobolAnalyzerAgent**: Analyzes COBOL structure (shared for both languages)
2. **JavaConverterAgent**: Converts to Java Quarkus
3. **CSharpConverterAgent**: Converts to C# .NET
4. **DependencyMapperAgent**: Maps dependencies (shared for both languages)

When using `--target Both`, the analysis is performed once and reused for both conversions, optimizing performance.

## Support

For issues or questions:
- Open an issue in the GitHub repository
- Check the main README.md for general documentation
- Review the SEMANTIC_KERNEL_ARCHITECTURE.md for technical details
