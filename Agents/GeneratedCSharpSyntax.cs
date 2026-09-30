// Models sometimes end an expression-bodied accessor with a comma, as if it were an initializer
// member (`get => A + B,`). That is one character, always the same one, and while it stands the
// compiler reports nothing else in the project, so it is corrected here rather than by a repair.

using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents;

internal static class GeneratedCSharpSyntax
{
    // The accessor's line ends in a comma and the next line closes the property or starts the other
    // accessor. A comma continuing the expression onto the next line is not followed by either.
    private static readonly Regex AccessorComma = new(
        @"^(?<head>[ \t]*(?:(?:private|protected|internal)[ \t]+)?(?:get|set|init)[ \t]*=>[^\r\n]*?),[ \t]*(?=\r?\n[ \t]*(?:\}|(?:(?:private|protected|internal)[ \t]+)?(?:get|set|init)\b))",
        RegexOptions.Multiline | RegexOptions.Compiled);

    public static string FixAccessorTerminators(string code) =>
        string.IsNullOrEmpty(code) ? code : AccessorComma.Replace(code, "${head};");
}
