using System.Text;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Centralized helper for consistent naming across all converters.
/// Ensures each COBOL file produces a uniquely named output file.
/// </summary>
public static class NamingHelper
{
    /// <summary>
    /// Converts a string to PascalCase (e.g. "my_var" -> "MyVar", "ABC-DEF" -> "AbcDef")
    /// </summary>
    public static string ToPascalCase(string input)
    {
        if (string.IsNullOrWhiteSpace(input))
            return string.Empty;

        var sb = new StringBuilder();
        bool capitalizeNext = true;
        
        foreach (char c in input)
        {
            if (char.IsLetterOrDigit(c))
            {
                sb.Append(capitalizeNext ? char.ToUpper(c) : char.ToLower(c));
                capitalizeNext = false;
            }
            else
            {
                capitalizeNext = true;
            }
        }
        
        return sb.ToString();
    }

    /// <summary>
    /// Derives a unique, valid class name from a COBOL filename.
    /// This ensures each COBOL file produces a uniquely named output file.
    /// </summary>
    /// <param name="cobolFileName">The COBOL filename (e.g., "PAYB649.cbl" or "synthetic_50k_loc_cobol.cbl")</param>
    /// <returns>A valid C#/Java class name (e.g., "Payb649" or "Synthetic50kLocCobol")</returns>
    public static string DeriveClassNameFromCobolFile(string cobolFileName)
    {
        // Get just the filename without path and extension
        var baseName = Path.GetFileNameWithoutExtension(cobolFileName);
        
        if (string.IsNullOrWhiteSpace(baseName))
            return "ConvertedCobolProgram";
        
        // Convert to PascalCase and remove invalid characters
        // COBOL names like "PAYB649" become "Payb649"
        // Names like "synthetic_50k_loc_cobol" become "Synthetic50kLocCobol"
        var sb = new StringBuilder();
        bool capitalizeNext = true;
        
        foreach (char c in baseName)
        {
            if (char.IsLetterOrDigit(c))
            {
                sb.Append(capitalizeNext ? char.ToUpper(c) : char.ToLower(c));
                capitalizeNext = false;
            }
            else if (c == '_' || c == '-' || c == '.')
            {
                capitalizeNext = true; // Capitalize after separator
            }
        }
        
        var className = sb.ToString();
        
        // Ensure starts with letter (not digit)
        if (className.Length > 0 && !char.IsLetter(className[0]))
        {
            className = "Cobol" + className;
        }
        
        // Ensure we have a valid name
        if (string.IsNullOrWhiteSpace(className))
            return "ConvertedCobolProgram";
            
        return className;
    }

    /// <summary>
    /// Creates a fallback class name when AI conversion fails.
    /// </summary>
    public static string GetFallbackClassName(string cobolFileName)
    {
        var baseName = DeriveClassNameFromCobolFile(cobolFileName);
        return baseName + "Fallback";
    }

    /// <summary>
    /// Gets the output filename for a converted COBOL file.
    /// </summary>
    /// <param name="cobolFileName">The original COBOL filename</param>
    /// <param name="extension">The target extension (e.g., ".cs" or ".java")</param>
    public static string GetOutputFileName(string cobolFileName, string extension)
    {
        var className = DeriveClassNameFromCobolFile(cobolFileName);
        return className + extension;
    }

    /// <summary>
    /// Checks if a string is a valid C#/Java identifier.
    /// </summary>
    public static bool IsValidIdentifier(string identifier)
    {
        if (string.IsNullOrWhiteSpace(identifier))
            return false;

        if (!char.IsLetter(identifier[0]) && identifier[0] != '_')
            return false;

        return identifier.All(c => char.IsLetterOrDigit(c) || c == '_');
    }

    /// <summary>
    /// Common suffixes that indicate a semantic, domain-based class name.
    /// </summary>
    private static readonly string[] SemanticSuffixes = new[]
    {
        "Service", "Processor", "Handler", "Validator", "Calculator",
        "Generator", "Manager", "Controller", "Repository", "Factory",
        "Builder", "Worker", "Job", "Task", "Runner", "Executor",
        "Parser", "Formatter", "Converter", "Transformer", "Mapper",
        "Reader", "Writer", "Loader", "Exporter", "Importer",
        "Scheduler", "Dispatcher", "Notifier", "Reporter", "Analyzer",
        "Sanitizer", "Reconciler", "Aggregator", "Batcher", "Sync"
    };

    /// <summary>
    /// Generic class names that indicate the AI didn't provide a semantic name.
    /// </summary>
    private static readonly string[] GenericClassNames = new[]
    {
        "ConvertedCobolProgram", "CobolProgram", "Program", "MainProgram",
        "CobolConverter", "ConvertedProgram", "LegacyProgram", "MigratedProgram",
        "Application", "Main", "Entry", "Cobol"
    };

    /// <summary>
    /// Determines if the class name appears to be a semantic, domain-based name
    /// following the pattern: Domain + Action + Type (e.g., PaymentBatchValidator)
    /// </summary>
    public static bool IsSemanticClassName(string className)
    {
        if (string.IsNullOrWhiteSpace(className))
            return false;

        // Check if it's a known generic name
        if (GenericClassNames.Any(g => g.Equals(className, StringComparison.OrdinalIgnoreCase)))
            return false;

        // Check if it ends with a semantic suffix (strong indicator)
        if (SemanticSuffixes.Any(s => className.EndsWith(s, StringComparison.Ordinal)))
            return true;

        // Check for multi-word PascalCase with at least 2 capital letters
        // (indicates Domain+Action pattern like "PaymentBatch" or "CustomerAccount")
        int capitalCount = className.Count(char.IsUpper);
        if (capitalCount >= 2 && className.Length >= 10)
        {
            // Additional check: not just a filename converted to PascalCase
            // Filenames often have numbers or are short like "Payb649"
            bool hasNumber = className.Any(char.IsDigit);
            if (!hasNumber)
                return true;
        }

        return false;
    }

    /// <summary>
    /// Extracts a class name from generated C# code.
    /// Falls back to deriving from COBOL filename if extraction fails.
    /// </summary>
    public static string ExtractCSharpClassName(string csharpCode, string cobolFileName)
    {
        var aiClassName = ExtractClassNameFromCode(csharpCode, "class ");
        
        // If AI generated a reasonable class name (not generic), use it
        if (!string.IsNullOrEmpty(aiClassName) && 
            aiClassName != "ConvertedCobolProgram" && 
            IsValidIdentifier(aiClassName))
        {
            return aiClassName;
        }
        
        // Fall back to deriving from COBOL filename
        return DeriveClassNameFromCobolFile(cobolFileName);
    }

    /// <summary>
    /// Extracts a class name from generated Java code.
    /// Falls back to deriving from COBOL filename if extraction fails.
    /// </summary>
    public static string ExtractJavaClassName(string javaCode, string cobolFileName)
    {
        var aiClassName = ExtractClassNameFromCode(javaCode, "public class ");
        
        // Also try without public modifier
        if (string.IsNullOrEmpty(aiClassName) || aiClassName == "ConvertedCobolProgram")
        {
            aiClassName = ExtractClassNameFromCode(javaCode, "class ");
        }
        
        // If AI generated a reasonable class name (not generic), use it
        if (!string.IsNullOrEmpty(aiClassName) && 
            aiClassName != "ConvertedCobolProgram" && 
            IsValidIdentifier(aiClassName))
        {
            return aiClassName;
        }
        
        // Fall back to deriving from COBOL filename
        return DeriveClassNameFromCobolFile(cobolFileName);
    }

    private static string? ExtractClassNameFromCode(string code, string classKeyword)
    {
        try
        {
            var lines = code.Split('\n');
            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                var classIndex = trimmedLine.IndexOf(classKeyword, StringComparison.Ordinal);
                if (classIndex >= 0)
                {
                    var afterClass = trimmedLine.Substring(classIndex + classKeyword.Length);
                    var className = afterClass.Split(' ', '\t', '\r', '\n', '{', ':')[0].Trim();
                    
                    if (IsValidIdentifier(className))
                        return className;
                }
            }
        }
        catch
        {
            // Ignore parsing errors
        }
        
        return null;
    }

    /// <summary>
    /// Updates generic class names in generated code to use the unique filename-derived name.
    /// </summary>
    public static string ReplaceGenericClassName(string code, string genericName, string uniqueName)
    {
        if (genericName == uniqueName)
            return code;
            
        // Rename identifier tokens, not prefixes or mentions in comments and literals. Interpolation
        // holes are code, so their identifiers are renamed too.
        var output = new System.Text.StringBuilder(code.Length);
        var index = 0;
        RenameCode(code, ref index, output, genericName, uniqueName, inHole: false);
        return output.ToString();
    }

    private static void RenameCode(string code, ref int i, System.Text.StringBuilder output,
        string genericName, string uniqueName, bool inHole)
    {
        var depth = 0;
        while (i < code.Length)
        {
            var c = code[i];
            if (c == '/' && At(code, i + 1, '/'))
            {
                var end = code.IndexOf('\n', i);
                Copy(code, ref i, end < 0 ? code.Length : end, output);
            }
            else if (c == '/' && At(code, i + 1, '*'))
            {
                var end = code.IndexOf("*/", i + 2, StringComparison.Ordinal);
                Copy(code, ref i, end < 0 ? code.Length : end + 2, output);
            }
            else if (c == '\'')
            {
                var end = i + 1;
                while (end < code.Length && code[end] != '\'' && code[end] != '\n')
                    end += code[end] == '\\' ? 2 : 1;
                Copy(code, ref i, Math.Min(end + 1, code.Length), output);
            }
            else if (StringPrefix(code, i) is { } prefix)
                RenameString(code, ref i, prefix, output, genericName, uniqueName);
            else if (char.IsLetter(c) || c == '_')
            {
                var end = i;
                while (end < code.Length && (char.IsLetterOrDigit(code[end]) || code[end] == '_')) end++;
                var word = code[i..end];
                output.Append(word == genericName ? uniqueName : word);
                i = end;
            }
            else if (inHole && depth == 0 && (c == '}' || c == ':'))
                return;
            else
            {
                if (c is '{' or '(' or '[') depth++;
                else if (c is '}' or ')' or ']') depth--;
                output.Append(c);
                i++;
            }
        }
    }

    // Returns (dollars, verbatim, prefix length) when a string literal starts at i.
    private static (int Dollars, bool Verbatim, int Length)? StringPrefix(string code, int i)
    {
        var start = i;
        var dollars = 0;
        var verbatim = false;
        while (i < code.Length && (code[i] == '$' || code[i] == '@'))
        {
            if (code[i] == '$') dollars++; else verbatim = true;
            i++;
        }
        if (!At(code, i, '"') || start > 0 && (char.IsLetterOrDigit(code[start - 1]) || code[start - 1] == '_'))
            return null;
        return (dollars, verbatim, i - start);
    }

    private static void RenameString(string code, ref int i, (int Dollars, bool Verbatim, int Length) prefix,
        System.Text.StringBuilder output, string genericName, string uniqueName)
    {
        Copy(code, ref i, i + prefix.Length, output);
        var quotes = 0;
        while (At(code, i + quotes, '"')) quotes++;
        var raw = quotes >= 3 && !prefix.Verbatim;
        if (!raw) quotes = 1;
        if (!raw && quotes == 1 && At(code, i + 1, '"') && !At(code, i + 2, '"'))
        {
            Copy(code, ref i, i + 2, output); // empty string ""
            return;
        }
        Copy(code, ref i, i + quotes, output);
        var holeBraces = raw ? prefix.Dollars : Math.Min(prefix.Dollars, 1);
        while (i < code.Length)
        {
            var c = code[i];
            if (raw && c == '"' && string.CompareOrdinal(code, i, new string('"', quotes), 0, quotes) == 0)
            {
                Copy(code, ref i, i + quotes, output);
                return;
            }
            if (!raw && c == '"')
            {
                if (prefix.Verbatim && At(code, i + 1, '"'))
                {
                    Copy(code, ref i, i + 2, output);
                    continue;
                }
                Copy(code, ref i, i + 1, output);
                return;
            }
            if (!raw && !prefix.Verbatim && c == '\\')
            {
                Copy(code, ref i, Math.Min(i + 2, code.Length), output);
                continue;
            }
            if (holeBraces > 0 && c == '{')
            {
                var run = 0;
                while (At(code, i + run, '{')) run++;
                if (!raw && run >= 2)
                {
                    Copy(code, ref i, i + 2, output); // escaped {{
                    continue;
                }
                if (run < holeBraces)
                {
                    Copy(code, ref i, i + run, output);
                    continue;
                }
                Copy(code, ref i, i + run, output);
                RenameCode(code, ref i, output, genericName, uniqueName, inHole: true);
                if (At(code, i, ':'))
                {
                    var end = code.IndexOf('}', i);
                    Copy(code, ref i, end < 0 ? code.Length : end, output);
                }
                var closing = 0;
                while (closing < holeBraces && At(code, i, '}'))
                {
                    output.Append('}');
                    i++;
                    closing++;
                }
                continue;
            }
            Copy(code, ref i, i + 1, output);
        }
    }

    private static bool At(string code, int index, char expected) => index < code.Length && code[index] == expected;

    private static void Copy(string code, ref int i, int end, System.Text.StringBuilder output)
    {
        output.Append(code, i, end - i);
        i = end;
    }
}
