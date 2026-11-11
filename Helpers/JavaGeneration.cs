using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Helpers;

public static class JavaGeneration
{
    public static string ValidateFramework(string framework) =>
        framework is "Quarkus" or "Spring" ? framework
            : throw new ArgumentException("Java framework must be 'Quarkus' or 'Spring'.");

    public static string Framework(ConversionSettings? settings) =>
        ValidateFramework(settings?.JavaFramework ?? "Quarkus");

    public static string ConverterPrompt(ConversionSettings? settings, bool chunked = false) =>
        Framework(settings) == "Spring" ? settings?.DependencyMode == "partial"
            ? "PartialJavaSpringConverter" : "JavaSpringConverter"
            : chunked ? "ChunkAwareJavaConverter" : "JavaConverter";

    public static string TargetName(ConversionSettings settings) =>
        Framework(settings) == "Spring" ? "Java Spring Boot" : "Java Quarkus";
}
