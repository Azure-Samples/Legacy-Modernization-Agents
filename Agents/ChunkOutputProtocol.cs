using System.Text.RegularExpressions;

namespace CobolToQuarkusMigration.Agents;

internal static class ChunkOutputProtocol
{
    internal static string ReadResponse(string response, string language)
    {
        if (!response.TrimStart().StartsWith("```", StringComparison.Ordinal))
        {
            if (Regex.IsMatch(response, @"(?m)^\s*```"))
                throw new InvalidDataException("A chunk response must contain only source, not prose followed by fenced blocks.");
            return response;
        }

        var fence = Regex.Match(response.Trim(), @"\A```(?<tag>[A-Za-z]*)[ \t]*\r?\n(?<code>[\s\S]*)\r?\n```\z");
        var tag = fence.Groups["tag"].Value;
        if (!fence.Success || !(tag.Length == 0 || tag.Equals(language, StringComparison.OrdinalIgnoreCase)
            || language == "csharp" && tag.Equals("cs", StringComparison.OrdinalIgnoreCase))
            || Regex.IsMatch(fence.Groups["code"].Value, @"(?m)^[ \t]*```"))
            throw new InvalidDataException("Only one whole-response code fence is supported; multiple blocks or surrounding prose cannot be discarded.");
        return fence.Groups["code"].Value;
    }
}
