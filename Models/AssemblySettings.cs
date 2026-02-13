using System.Text.Json.Serialization;

namespace CobolToQuarkusMigration.Models;

/// <summary>
/// Configuration settings for assembling converted code into output files.
/// Controls how chunks are combined and split into classes/files for both Java and C#.
/// 
/// These settings determine the final structure of the generated code, including:
/// - How many files are created (single file vs. one per class)
/// - Package/namespace naming conventions
/// - Whether to generate interfaces alongside implementations
/// - Architectural layer organization (Service/Repository/Model patterns)
/// </summary>
public class AssemblySettings
{
    /// <summary>
    /// Settings for Java file assembly.
    /// Configures how COBOL code is transformed into Java classes, packages, and files.
    /// </summary>
    public JavaAssemblySettings Java { get; set; } = new();

    /// <summary>
    /// Settings for C# file assembly.
    /// Configures how COBOL code is transformed into C# classes, namespaces, and files.
    /// </summary>
    public CSharpAssemblySettings CSharp { get; set; } = new();
}

/// <summary>
/// Strategy for splitting output into files.
/// Choose based on your project's organization preferences and IDE capabilities.
/// </summary>
[JsonConverter(typeof(JsonStringEnumConverter))]
public enum FileSplitStrategy
{
    /// <summary>
    /// All converted code assembled into a single file.
    /// Best for: Small COBOL programs, simple migrations, or when you prefer manual refactoring.
    /// Output: One large file containing all classes.
    /// </summary>
    SingleFile,

    /// <summary>
    /// Detect class declarations and create one file per class.
    /// Best for: Standard Java/C# projects following convention of one class per file.
    /// Output: Multiple files, each containing a single class (e.g., CustomerService.java, CustomerRepository.java).
    /// This is the DEFAULT and recommended strategy.
    /// </summary>
    ClassPerFile,

    /// <summary>
    /// Create one file per processing chunk.
    /// Best for: Very large COBOL programs where you want to preserve chunk boundaries.
    /// Output: Multiple partial class files (C#) or multiple classes (Java) per chunk.
    /// </summary>
    FilePerChunk,

    /// <summary>
    /// Split by architectural layer (Service/Repository/Model).
    /// Best for: Enterprise applications following clean architecture patterns.
    /// Output: Organized folder structure with Services/, Repositories/, Models/, Utils/ subfolders.
    /// </summary>
    LayeredArchitecture
}

/// <summary>
/// Java-specific assembly settings.
/// Configure how converted COBOL code is packaged and organized for Java/Quarkus projects.
/// </summary>
public class JavaAssemblySettings
{
    /// <summary>
    /// Strategy for splitting Java output files.
    /// 
    /// Options:
    /// - SingleFile: All classes in one .java file (not recommended for large programs)
    /// - ClassPerFile: One .java file per detected class (RECOMMENDED)
    /// - FilePerChunk: One file per processing chunk
    /// - LayeredArchitecture: Separate packages for Service/Repository/Model layers
    /// 
    /// Default: ClassPerFile
    /// </summary>
    public FileSplitStrategy SplitStrategy { get; set; } = FileSplitStrategy.ClassPerFile;

    /// <summary>
    /// Base package name for generated Java classes.
    /// This becomes the root package, with class-specific sub-packages appended.
    /// 
    /// Examples:
    /// - "com.example.generated" → com/example/generated/CustomerService.java
    /// - "com.mycompany.cobol" → com/mycompany/cobol/InventoryManager.java
    /// - "legacy.migration" → legacy/migration/PayrollProcessor.java
    /// 
    /// The folder structure mirrors the package name (Java convention).
    /// Default: "com.example.generated"
    /// </summary>
    public string PackagePrefix { get; set; } = "com.example.generated";

    /// <summary>
    /// Suffix to add to the main service class name.
    /// Useful for distinguishing migrated code or following naming conventions.
    /// 
    /// Examples:
    /// - "" (empty): CustomerInquiry.java
    /// - "Service": CustomerInquiryService.java
    /// - "Legacy": CustomerInquiryLegacy.java
    /// 
    /// Default: "" (no suffix)
    /// </summary>
    public string MainClassSuffix { get; set; } = "";

    /// <summary>
    /// Default annotations to add to all generated classes.
    /// These are added at the class level for dependency injection, transactions, etc.
    /// 
    /// Common Quarkus/CDI annotations:
    /// - @ApplicationScoped: CDI bean with application lifecycle
    /// - @Transactional: Wrap methods in database transactions
    /// - @Singleton: Single instance across the application
    /// 
    /// Example: ["@ApplicationScoped", "@Transactional"]
    /// Default: ["@ApplicationScoped"]
    /// </summary>
    public List<string> DefaultAnnotations { get; set; } = new() { "@ApplicationScoped" };

    /// <summary>
    /// Whether to generate interface files alongside implementation classes.
    /// Useful for dependency injection, testing, and clean architecture.
    /// 
    /// When true:
    /// - Creates ICustomerService.java (interface)
    /// - Creates CustomerService.java (implements ICustomerService)
    /// 
    /// Default: false
    /// </summary>
    public bool GenerateInterfaces { get; set; } = false;

    /// <summary>
    /// Prefix for interface names when GenerateInterfaces is true.
    /// 
    /// Java Convention: Interface has descriptive name, implementation has "Impl" suffix.
    /// - Interface: CustomerService.java
    /// - Implementation: CustomerServiceImpl.java
    /// 
    /// Set to "" (empty) for Java convention.
    /// Default: "" (Java convention)
    /// </summary>
    public string InterfacePrefix { get; set; } = "";

    /// <summary>
    /// Suffix added to implementation class names when GenerateInterfaces is true.
    /// 
    /// Java Convention: Implementation classes have "Impl" suffix.
    /// - Interface: CustomerService.java
    /// - Implementation: CustomerServiceImpl.java
    /// 
    /// Default: "Impl"
    /// </summary>
    public string ImplementationSuffix { get; set; } = "Impl";

    /// <summary>
    /// Layer configuration when using LayeredArchitecture strategy.
    /// Defines how code is organized into Service, Repository, Model, and Utils layers.
    /// </summary>
    public LayerSettings Layers { get; set; } = new();
}

/// <summary>
/// C#-specific assembly settings.
/// Configure how converted COBOL code is organized for .NET projects.
/// </summary>
public class CSharpAssemblySettings
{
    /// <summary>
    /// Strategy for splitting C# output files.
    /// 
    /// Options:
    /// - SingleFile: All classes in one .cs file
    /// - ClassPerFile: One .cs file per detected class (RECOMMENDED)
    /// - FilePerChunk: One partial class file per processing chunk
    /// - LayeredArchitecture: Separate folders for Service/Repository/Model layers
    /// 
    /// Default: ClassPerFile
    /// </summary>
    public FileSplitStrategy SplitStrategy { get; set; } = FileSplitStrategy.ClassPerFile;

    /// <summary>
    /// Base namespace for generated C# classes.
    /// Sub-namespaces are appended based on the source file name.
    /// 
    /// Examples:
    /// - "CobolMigration" → namespace CobolMigration.CustomerInquiry
    /// - "Legacy.Cobol" → namespace Legacy.Cobol.InventoryManager
    /// - "MyCompany.Migration" → namespace MyCompany.Migration.PayrollProcessor
    /// 
    /// Default: "CobolMigration"
    /// </summary>
    public string NamespacePrefix { get; set; } = "CobolMigration";

    /// <summary>
    /// Whether to use file-scoped namespace declarations (C# 10+ feature).
    /// 
    /// When true (C# 10+):
    ///   namespace CobolMigration.Customer;
    ///   public class CustomerService { }
    /// 
    /// When false (traditional):
    ///   namespace CobolMigration.Customer
    ///   {
    ///       public class CustomerService { }
    ///   }
    /// 
    /// Default: true (modern C# style)
    /// </summary>
    public bool UseFileScopedNamespace { get; set; } = true;

    /// <summary>
    /// Whether to generate partial classes.
    /// Partial classes allow splitting a single class across multiple files.
    /// 
    /// When true:
    /// - Enables FilePerChunk strategy to work properly
    /// - Allows manual extensions in separate files
    /// - Generated code includes 'partial' keyword
    /// 
    /// Default: true
    /// </summary>
    public bool UsePartialClasses { get; set; } = true;

    /// <summary>
    /// Suffix to add to the main class name.
    /// 
    /// Examples:
    /// - "" (empty): CustomerInquiry.cs
    /// - "Service": CustomerInquiryService.cs
    /// - "Migrated": CustomerInquiryMigrated.cs
    /// 
    /// Default: "" (no suffix)
    /// </summary>
    public string MainClassSuffix { get; set; } = "";

    /// <summary>
    /// Whether to generate interface files alongside implementation classes.
    /// 
    /// When true:
    /// - Creates ICustomerService.cs (interface)
    /// - Creates CustomerService.cs (implements ICustomerService)
    /// 
    /// Useful for dependency injection in ASP.NET Core.
    /// Default: false
    /// </summary>
    public bool GenerateInterfaces { get; set; } = false;

    /// <summary>
    /// Prefix for interface names when GenerateInterfaces is true.
    /// 
    /// C# Convention: Interfaces use "I" prefix, implementation has no suffix.
    /// - Interface: ICustomerService.cs
    /// - Implementation: CustomerService.cs
    /// 
    /// Default: "I" (C# convention)
    /// </summary>
    public string InterfacePrefix { get; set; } = "I";

    /// <summary>
    /// Suffix added to implementation class names when GenerateInterfaces is true.
    /// 
    /// C# Convention: Implementation classes have no suffix (interface has "I" prefix instead).
    /// - Interface: ICustomerService.cs
    /// - Implementation: CustomerService.cs
    /// 
    /// Default: "" (C# convention)
    /// </summary>
    public string ImplementationSuffix { get; set; } = "";

    /// <summary>
    /// Layer configuration when using LayeredArchitecture strategy.
    /// Defines how code is organized into Services, Repositories, Models, and Extensions folders.
    /// </summary>
    public LayerSettings Layers { get; set; } = new();

    /// <summary>
    /// File name pattern when using FilePerChunk strategy.
    /// Supports placeholders that are replaced at generation time.
    /// 
    /// Available placeholders:
    /// - {SourceFileName}: Original COBOL file name (e.g., "CUSTINQ")
    /// - {ChunkIndex}: Chunk number (e.g., "1", "2", "3")
    /// - {ClassName}: Detected class name
    /// 
    /// Examples:
    /// - "{SourceFileName}.Part{ChunkIndex}.cs" → CUSTINQ.Part1.cs, CUSTINQ.Part2.cs
    /// - "{ClassName}.Chunk{ChunkIndex}.cs" → CustomerService.Chunk1.cs
    /// 
    /// Default: "{SourceFileName}.Part{ChunkIndex}.cs"
    /// </summary>
    public string ChunkFileNamePattern { get; set; } = "{SourceFileName}.Part{ChunkIndex}.cs";
}

/// <summary>
/// Layer configuration for LayeredArchitecture split strategy.
/// Defines folder structure and naming conventions for clean architecture patterns.
/// 
/// This creates a project structure like:
/// output/
/// ├── Services/
/// │   └── CustomerService.cs
/// ├── Repositories/
/// │   └── CustomerRepository.cs
/// ├── Models/
/// │   └── Customer.cs
/// └── Utils/
///     └── DateUtils.cs
/// </summary>
public class LayerSettings
{
    /// <summary>
    /// Service layer configuration.
    /// Contains business logic and orchestration code.
    /// Typically annotated with @ApplicationScoped (Java) or registered as scoped service (C#).
    /// </summary>
    public LayerConfig Service { get; set; } = new()
    {
        Suffix = "Service",
        Subfolder = "Services",
        Annotations = new() { "@ApplicationScoped", "@Transactional" },
        Attributes = new()
    };

    /// <summary>
    /// Repository/Data access layer configuration.
    /// Contains database access and data persistence code.
    /// Implements repository pattern for data operations.
    /// </summary>
    public LayerConfig Repository { get; set; } = new()
    {
        Suffix = "Repository",
        Subfolder = "Repositories",
        Annotations = new() { "@ApplicationScoped" },
        Attributes = new()
    };

    /// <summary>
    /// Model/Entity layer configuration.
    /// Contains data transfer objects, entities, and value objects.
    /// Typically plain classes without special annotations.
    /// </summary>
    public LayerConfig Model { get; set; } = new()
    {
        Suffix = "",
        Subfolder = "Models",
        Annotations = new(),
        Attributes = new()
    };

    /// <summary>
    /// Utilities layer configuration.
    /// Contains helper methods, extension methods, and static utilities.
    /// Often converted from COBOL copybooks or common paragraphs.
    /// </summary>
    public LayerConfig Utils { get; set; } = new()
    {
        Suffix = "Utils",
        Subfolder = "Utils",
        Annotations = new(),
        Attributes = new(),
        IsStatic = true
    };
}

/// <summary>
/// Configuration for a single architectural layer.
/// Defines naming conventions, folder location, and decorators for classes in this layer.
/// </summary>
public class LayerConfig
{
    /// <summary>
    /// Suffix to add to class names in this layer.
    /// 
    /// Examples:
    /// - "Service" → CustomerService
    /// - "Repository" → CustomerRepository
    /// - "" (empty) → Customer (for models)
    /// </summary>
    public string Suffix { get; set; } = "";

    /// <summary>
    /// Subfolder within the output directory for this layer.
    /// Creates organized folder structure for the project.
    /// 
    /// Examples:
    /// - "Services" → output/csharp/Services/CustomerService.cs
    /// - "repositories" → output/java/repositories/CustomerRepository.java
    /// </summary>
    public string Subfolder { get; set; } = "";

    /// <summary>
    /// Java annotations to add to classes in this layer.
    /// Added at the class level in generated Java code.
    /// 
    /// Common annotations:
    /// - @ApplicationScoped: CDI application-scoped bean
    /// - @Transactional: Database transaction management
    /// - @Singleton: Single instance bean
    /// - @RequestScoped: Per-request lifecycle
    /// </summary>
    public List<string> Annotations { get; set; } = new();

    /// <summary>
    /// C# attributes to add to classes in this layer.
    /// Added at the class level in generated C# code.
    /// 
    /// Common attributes:
    /// - [Serializable]: Mark class as serializable
    /// - [Table("TableName")]: Entity Framework table mapping
    /// </summary>
    public List<string> Attributes { get; set; } = new();

    /// <summary>
    /// Whether classes in this layer should be static (C# only).
    /// Useful for utility/helper classes with extension methods.
    /// 
    /// When true: public static class DateUtils { }
    /// When false: public class DateUtils { }
    /// 
    /// Default: false
    /// </summary>
    public bool IsStatic { get; set; } = false;
}
