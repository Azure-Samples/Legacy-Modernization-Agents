# C# Converter Agent Usage Guide

## Overview

The `CSharpConverterAgent` is a new agent that converts COBOL programs to modern C# code. It follows the same architecture as the `JavaConverterAgent` but generates C# code with .NET best practices.

## Files Created

### 1. **Models/CSharpFile.cs**
Represents a generated C# file with properties:
- `FileName` - The C# file name (e.g., "ConvertedProgram.cs")
- `FilePath` - Full path to the file
- `Content` - The C# source code
- `Namespace` - The C# namespace
- `ClassName` - The main class name
- `OriginalCobolFileName` - Reference to the source COBOL file

### 2. **Agents/Interfaces/ICSharpConverterAgent.cs**
Interface defining the contract for C# conversion:
- `ConvertToCSharpAsync(CobolFile, CobolAnalysis)` - Converts a single COBOL file
- `ConvertToCSharpAsync(List<CobolFile>, List<CobolAnalysis>, Action<int, int>?)` - Converts multiple files with progress tracking

### 3. **Agents/CSharpConverterAgent.cs**
The main implementation with features:
- AI-powered COBOL to C# conversion
- Content sanitization for Azure OpenAI content filtering
- Retry logic with exponential backoff
- Enhanced logging and API call tracking
- Markdown code block extraction
- C# namespace and class name parsing

## Key Features

### C#-Specific Conversion Guidelines
The agent converts COBOL to C# following these principles:
- **Data Types**: COBOL numeric types → `decimal`, alphanumeric → `string`
- **Control Flow**: COBOL PERFORM/GOTO → modern C# control structures
- **Error Handling**: COBOL error codes → C# exceptions and try-catch blocks
- **Documentation**: Comprehensive XML documentation comments
- **Naming**: PascalCase for public members, camelCase for private fields
- **Modern Features**: async/await, LINQ, pattern matching, nullable reference types
- **Architecture**: Dependency injection patterns where appropriate

### Content Sanitization
Like the Java converter, it sanitizes Danish error handling terms that may trigger content filters:
- `FEJL` → `ERROR_CODE`
- `FEJLMELD` → `ERROR_MSG`
- `KALD` → `CALL_OP`

### Retry Logic
Automatically retries failed conversions up to 3 times with exponential backoff for:
- Timeouts
- Content filtering issues
- Transient network errors

## Usage Example

```csharp
// Create the agent
var csharpConverter = new CSharpConverterAgent(
    kernelBuilder,
    logger,
    modelId,
    enhancedLogger,
    chatLogger
);

// Convert a single COBOL file
CobolFile cobolFile = /* ... */;
CobolAnalysis analysis = /* ... */;

CSharpFile csharpFile = await csharpConverter.ConvertToCSharpAsync(
    cobolFile, 
    analysis
);

// Save the generated C# file
File.WriteAllText(csharpFile.FileName, csharpFile.Content);
```

## Integration with Migration Process

To integrate this agent into your migration workflow:

1. **Add to dependency injection** in `Program.cs`:
```csharp
var csharpConverterAgent = new CSharpConverterAgent(
    kernelBuilder, 
    loggerFactory.CreateLogger<CSharpConverterAgent>(), 
    settings.AzureOpenAI.ModelId,
    enhancedLogger,
    chatLogger
);
```

2. **Use in migration process**:
```csharp
// After COBOL analysis
var csharpFiles = await csharpConverterAgent.ConvertToCSharpAsync(
    cobolFiles,
    cobolAnalyses,
    progressCallback: (completed, total) => 
    {
        Console.WriteLine($"Converting to C#: {completed}/{total}");
    }
);

// Save C# files
foreach (var csharpFile in csharpFiles)
{
    var outputPath = Path.Combine(csharpOutputFolder, csharpFile.FileName);
    await File.WriteAllTextAsync(outputPath, csharpFile.Content);
}
```

## Logging and Monitoring

The agent provides comprehensive logging at multiple levels:

### Standard Logging
```
[INFO] Converting COBOL file to C#: PROGRAM.cbl
[INFO] Converting COBOL to C# - Attempt 1/3 for PROGRAM.cbl
[INFO] Completed conversion of COBOL file to C#: PROGRAM.cbl
```

### Enhanced Logging (Behind-the-Scenes)
- `CSHARP_CONVERSION_START` - Conversion initiated
- `CSHARP_CONVERSION_REQUEST` - AI API request sent
- `CSHARP_CONVERSION_RESPONSE` - AI response received
- `CSHARP_CONVERSION_COMPLETE` - Conversion finished with timing
- `RETRY_ATTEMPT` - Retry triggered due to error
- `ERROR` - Conversion failed

### Chat Logging
Full conversation logs with:
- System prompts
- User messages (COBOL code + analysis)
- AI responses (generated C# code)

## Differences from JavaConverterAgent

1. **Target Language**: Generates C# instead of Java
2. **Namespace vs Package**: Uses C# namespaces instead of Java packages
3. **File Extension**: `.cs` instead of `.java`
4. **Conversion Guidelines**: C#-specific best practices and idioms
5. **Framework**: .NET ecosystem instead of Quarkus/Java

## Next Steps

To make this agent production-ready, consider:

1. **Add configuration** for C# output folder in `appsettings.json`
2. **Create command-line options** to choose between Java and C# conversion
3. **Implement unit tests** for the C# converter agent
4. **Add C# project file generation** (`.csproj` files)
5. **Include NuGet package recommendations** based on COBOL features

## Testing

Build verification:
```bash
dotnet build CobolModernization.csproj
```

All files compile successfully with no errors or warnings.
