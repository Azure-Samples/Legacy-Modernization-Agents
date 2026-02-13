using System.Text;
using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Models;

namespace CobolToQuarkusMigration.Chunking.Core;

/// <summary>
/// Enforces naming conventions during conversion based on the configured strategy.
/// </summary>
public class NamingConventionEnforcer
{
    private readonly ConversionSettings _settings;

    public NamingConventionEnforcer(ConversionSettings settings)
    {
        _settings = settings;
    }

    /// <summary>
    /// Converts a legacy name to a modern name using deterministic rules.
    /// </summary>
    /// <param name="legacyName">The legacy name (e.g., VALIDATE-CUSTOMER-DATA).</param>
    /// <param name="nameType">The type of name being converted.</param>
    /// <param name="targetLanguage">The target language.</param>
    /// <returns>The converted name.</returns>
    public string ConvertNameDeterministic(string legacyName, NameKind nameType, TargetLanguage targetLanguage)
    {
        if (string.IsNullOrWhiteSpace(legacyName))
            return string.Empty;

        var normalized = legacyName.Trim().ToUpperInvariant();

        // Remove common COBOL prefixes that shouldn't be in modern code
        normalized = StripCommonPrefixes(normalized);

        // Split by separators (hyphens are common in COBOL)
        var parts = SplitName(normalized);

        // Apply naming convention based on type
        var baseName = nameType switch
        {
            NameKind.ClassName => ToPascalCase(parts) + _settings.ClassNameSuffix,
            NameKind.MethodName => ToCamelCase(parts),
            NameKind.PropertyName => ToPascalCase(parts),
            NameKind.FieldName => ToCamelCase(parts),
            NameKind.ParameterName => ToCamelCase(parts),
            NameKind.ConstantName => ToUpperSnakeCase(parts),
            NameKind.EnumMemberName => ToPascalCase(parts),
            _ => ToCamelCase(parts)
        };

        // Add prefix if configured
        if (!string.IsNullOrEmpty(_settings.ClassNamePrefix) && nameType == NameKind.ClassName)
        {
            baseName = _settings.ClassNamePrefix + baseName;
        }

        // Handle reserved words
        baseName = EscapeReservedWord(baseName, targetLanguage);

        return baseName;
    }

    /// <summary>
    /// Validates that a name follows the expected conventions.
    /// </summary>
    /// <param name="modernName">The modern name to validate.</param>
    /// <param name="nameType">The expected name type.</param>
    /// <returns>True if valid, false otherwise.</returns>
    public bool ValidateName(string modernName, NameKind nameType)
    {
        if (string.IsNullOrWhiteSpace(modernName))
            return false;

        return nameType switch
        {
            NameKind.ClassName => Regex.IsMatch(modernName, @"^[A-Z][a-zA-Z0-9]*$"),
            NameKind.MethodName => Regex.IsMatch(modernName, @"^[a-z][a-zA-Z0-9]*$"),
            NameKind.PropertyName => Regex.IsMatch(modernName, @"^[A-Z][a-zA-Z0-9]*$"),
            NameKind.FieldName => Regex.IsMatch(modernName, @"^[a-z][a-zA-Z0-9]*$"),
            NameKind.ParameterName => Regex.IsMatch(modernName, @"^[a-z][a-zA-Z0-9]*$"),
            NameKind.ConstantName => Regex.IsMatch(modernName, @"^[A-Z][A-Z0-9_]*$"),
            _ => true
        };
    }

    /// <summary>
    /// Suggests a corrected name if the provided name doesn't follow conventions.
    /// </summary>
    /// <param name="invalidName">The name that doesn't follow conventions.</param>
    /// <param name="nameType">The expected name type.</param>
    /// <param name="targetLanguage">The target language.</param>
    /// <returns>A corrected name suggestion.</returns>
    public string SuggestCorrectedName(string invalidName, NameKind nameType, TargetLanguage targetLanguage)
    {
        // Treat it as a legacy name and convert
        return ConvertNameDeterministic(invalidName, nameType, targetLanguage);
    }

    private static string StripCommonPrefixes(string name)
    {
        // Common COBOL prefixes that are redundant in modern code
        var prefixes = new[] { "WS-", "LS-", "WK-", "LK-", "FD-", "SD-" };

        foreach (var prefix in prefixes)
        {
            if (name.StartsWith(prefix))
            {
                name = name.Substring(prefix.Length);
                break;
            }
        }

        return name;
    }

    private static string[] SplitName(string name)
    {
        // Split by hyphens and underscores
        return name.Split(new[] { '-', '_' }, StringSplitOptions.RemoveEmptyEntries);
    }

    private static string ToPascalCase(string[] parts)
    {
        var sb = new StringBuilder();
        foreach (var part in parts)
        {
            if (part.Length > 0)
            {
                sb.Append(char.ToUpperInvariant(part[0]));
                if (part.Length > 1)
                {
                    sb.Append(part.Substring(1).ToLowerInvariant());
                }
            }
        }
        return sb.ToString();
    }

    private static string ToCamelCase(string[] parts)
    {
        var sb = new StringBuilder();
        for (int i = 0; i < parts.Length; i++)
        {
            var part = parts[i];
            if (part.Length == 0) continue;

            if (i == 0)
            {
                sb.Append(part.ToLowerInvariant());
            }
            else
            {
                sb.Append(char.ToUpperInvariant(part[0]));
                if (part.Length > 1)
                {
                    sb.Append(part.Substring(1).ToLowerInvariant());
                }
            }
        }
        return sb.ToString();
    }

    private static string ToUpperSnakeCase(string[] parts)
    {
        return string.Join("_", parts.Select(p => p.ToUpperInvariant()));
    }

    private static string EscapeReservedWord(string name, TargetLanguage targetLanguage)
    {
        var reservedWords = targetLanguage == TargetLanguage.CSharp
            ? CSharpReservedWords
            : JavaReservedWords;

        if (reservedWords.Contains(name.ToLowerInvariant()))
        {
            // Language-specific escape strategy:
            // - C#: use @ prefix (e.g., @class)
            // - Java: append underscore (e.g., class_)
            return targetLanguage == TargetLanguage.CSharp
                ? "@" + name
                : name + "_";
        }

        return name;
    }

    private static readonly HashSet<string> CSharpReservedWords = new(StringComparer.OrdinalIgnoreCase)
    {
        "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char",
        "checked", "class", "const", "continue", "decimal", "default", "delegate",
        "do", "double", "else", "enum", "event", "explicit", "extern", "false",
        "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit",
        "in", "int", "interface", "internal", "is", "lock", "long", "namespace",
        "new", "null", "object", "operator", "out", "override", "params", "private",
        "protected", "public", "readonly", "ref", "return", "sbyte", "sealed",
        "short", "sizeof", "stackalloc", "static", "string", "struct", "switch",
        "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked",
        "unsafe", "ushort", "using", "virtual", "void", "volatile", "while"
    };

    private static readonly HashSet<string> JavaReservedWords = new(StringComparer.OrdinalIgnoreCase)
    {
        "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char",
        "class", "const", "continue", "default", "do", "double", "else", "enum",
        "extends", "final", "finally", "float", "for", "goto", "if", "implements",
        "import", "instanceof", "int", "interface", "long", "native", "new", "null",
        "package", "private", "protected", "public", "return", "short", "static",
        "strictfp", "super", "switch", "synchronized", "this", "throw", "throws",
        "transient", "try", "void", "volatile", "while", "true", "false"
    };
}

/// <summary>
/// Types of names in converted code.
/// </summary>
public enum NameKind
{
    /// <summary>Class or type name.</summary>
    ClassName,

    /// <summary>Method name.</summary>
    MethodName,

    /// <summary>Property name (C#).</summary>
    PropertyName,

    /// <summary>Field name.</summary>
    FieldName,

    /// <summary>Parameter name.</summary>
    ParameterName,

    /// <summary>Constant name.</summary>
    ConstantName,

    /// <summary>Enum member name.</summary>
    EnumMemberName
}
