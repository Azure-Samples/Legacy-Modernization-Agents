using System.Text.RegularExpressions;
using CobolToQuarkusMigration.Chunking.Interfaces;

namespace CobolToQuarkusMigration.Chunking.Adapters;

/// <summary>
/// Language adapter for COBOL source code.
/// Implements parsing and semantic unit identification for COBOL programs.
/// </summary>
public class CobolAdapter : ILanguageAdapter
{
    public string LanguageName => "COBOL";

    public IReadOnlyList<string> SupportedExtensions { get; } = new[] { ".cbl", ".cob", ".cobol", ".cpy" };

    // Regex patterns for COBOL structure identification
    private static readonly Regex IdentificationDivisionRegex = new(
        @"^\s*IDENTIFICATION\s+DIVISION\s*\.",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex EnvironmentDivisionRegex = new(
        @"^\s*ENVIRONMENT\s+DIVISION\s*\.",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex DataDivisionRegex = new(
        @"^\s*DATA\s+DIVISION\s*\.",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex ProcedureDivisionRegex = new(
        @"^\s*PROCEDURE\s+DIVISION",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex WorkingStorageRegex = new(
        @"^\s*WORKING-STORAGE\s+SECTION\s*\.",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex LinkageSectionRegex = new(
        @"^\s*LINKAGE\s+SECTION\s*\.",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex FileSectionRegex = new(
        @"^\s*FILE\s+SECTION\s*\.",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex SectionRegex = new(
        @"^\s{6}\s*([A-Z0-9][-A-Z0-9]*)\s+SECTION\s*\.",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex ParagraphRegex = new(
        @"^\s{6}\s*([A-Z0-9][-A-Z0-9]*)\s*\.\s*$",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex VariableRegex = new(
        @"^\s{6}\s*(\d{2})\s+([A-Z0-9][-A-Z0-9]*)\s+(PIC|PICTURE)\s+(.+?)(?:\s+VALUE\s+(.+?))?\s*\.",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex GroupVariableRegex = new(
        @"^\s{6}\s*(\d{2})\s+([A-Z0-9][-A-Z0-9]*)\s*\.",
        RegexOptions.Multiline | RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex PerformRegex = new(
        @"PERFORM\s+([A-Z0-9][-A-Z0-9]*)",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex CallRegex = new(
        @"CALL\s+['""]?([A-Z0-9][-A-Z0-9]*)['""]?",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex GoToRegex = new(
        @"GO\s+TO\s+([A-Z0-9][-A-Z0-9]*)",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    private static readonly Regex CopyRegex = new(
        @"COPY\s+([A-Z0-9][-A-Z0-9.]*)",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    public bool CanProcess(string filePath, string? content = null)
    {
        var extension = Path.GetExtension(filePath).ToLowerInvariant();
        if (SupportedExtensions.Contains(extension))
            return true;

        // Content-based detection
        if (!string.IsNullOrEmpty(content))
        {
            return IdentificationDivisionRegex.IsMatch(content) ||
                   DataDivisionRegex.IsMatch(content) ||
                   ProcedureDivisionRegex.IsMatch(content);
        }

        return false;
    }

    public Task<IReadOnlyList<SemanticUnit>> IdentifySemanticUnitsAsync(
        string content,
        string filePath,
        CancellationToken cancellationToken = default)
    {
        var units = new List<SemanticUnit>();
        var lines = content.Split('\n');
        var lineIndex = 0;

        // Track current position for each division/section
        int? identificationStart = null;
        int? environmentStart = null;
        int? dataStart = null;
        int? procedureStart = null;
        int? workingStorageStart = null;
        int? linkageStart = null;
        int? fileStart = null;

        var sections = new List<(string Name, int StartLine, int EndLine)>();
        var paragraphs = new List<(string Name, int StartLine, int EndLine, string? SectionName)>();

        string? currentSection = null;
        int? currentSectionStart = null;
        string? currentParagraph = null;
        int? currentParagraphStart = null;

        // First pass: identify divisions and sections
        foreach (var line in lines)
        {
            lineIndex++;
            var trimmedLine = line.TrimStart();

            if (IdentificationDivisionRegex.IsMatch(line))
            {
                identificationStart = lineIndex;
            }
            else if (EnvironmentDivisionRegex.IsMatch(line))
            {
                if (identificationStart.HasValue)
                {
                    units.Add(CreateDivisionUnit("IDENTIFICATION-DIVISION", SemanticUnitType.IdentificationDivision,
                        identificationStart.Value, lineIndex - 1, lines));
                }
                environmentStart = lineIndex;
            }
            else if (DataDivisionRegex.IsMatch(line))
            {
                if (environmentStart.HasValue)
                {
                    units.Add(CreateDivisionUnit("ENVIRONMENT-DIVISION", SemanticUnitType.EnvironmentDivision,
                        environmentStart.Value, lineIndex - 1, lines));
                }
                dataStart = lineIndex;
            }
            else if (ProcedureDivisionRegex.IsMatch(line))
            {
                // Close any open data sections
                CloseDataSections(units, lines, lineIndex - 1,
                    ref workingStorageStart, ref linkageStart, ref fileStart);

                if (dataStart.HasValue)
                {
                    units.Add(CreateDivisionUnit("DATA-DIVISION", SemanticUnitType.DataDivision,
                        dataStart.Value, lineIndex - 1, lines));
                }
                procedureStart = lineIndex;
            }
            else if (WorkingStorageRegex.IsMatch(line) && dataStart.HasValue)
            {
                workingStorageStart = lineIndex;
            }
            else if (LinkageSectionRegex.IsMatch(line) && dataStart.HasValue)
            {
                if (workingStorageStart.HasValue)
                {
                    units.Add(CreateSectionUnit("WORKING-STORAGE", SemanticUnitType.WorkingStorageSection,
                        workingStorageStart.Value, lineIndex - 1, lines, "DATA-DIVISION"));
                    workingStorageStart = null;
                }
                linkageStart = lineIndex;
            }
            else if (FileSectionRegex.IsMatch(line) && dataStart.HasValue)
            {
                fileStart = lineIndex;
            }
            else if (procedureStart.HasValue)
            {
                // Check for section
                var sectionMatch = SectionRegex.Match(line);
                if (sectionMatch.Success)
                {
                    // Close previous paragraph and section
                    if (currentParagraph != null && currentParagraphStart.HasValue)
                    {
                        paragraphs.Add((currentParagraph, currentParagraphStart.Value, lineIndex - 1, currentSection));
                        currentParagraph = null;
                        currentParagraphStart = null;
                    }

                    if (currentSection != null && currentSectionStart.HasValue)
                    {
                        sections.Add((currentSection, currentSectionStart.Value, lineIndex - 1));
                    }

                    currentSection = sectionMatch.Groups[1].Value;
                    currentSectionStart = lineIndex;
                    continue;
                }

                // Check for paragraph (but not if it looks like a section)
                var paragraphMatch = ParagraphRegex.Match(line);
                if (paragraphMatch.Success && !line.ToUpperInvariant().Contains("SECTION"))
                {
                    // Close previous paragraph
                    if (currentParagraph != null && currentParagraphStart.HasValue)
                    {
                        paragraphs.Add((currentParagraph, currentParagraphStart.Value, lineIndex - 1, currentSection));
                    }

                    currentParagraph = paragraphMatch.Groups[1].Value;
                    currentParagraphStart = lineIndex;
                }
            }
        }

        // Close remaining open structures
        if (currentParagraph != null && currentParagraphStart.HasValue)
        {
            paragraphs.Add((currentParagraph, currentParagraphStart.Value, lines.Length, currentSection));
        }

        if (currentSection != null && currentSectionStart.HasValue)
        {
            sections.Add((currentSection, currentSectionStart.Value, lines.Length));
        }

        CloseDataSections(units, lines, lines.Length,
            ref workingStorageStart, ref linkageStart, ref fileStart);

        if (procedureStart.HasValue)
        {
            units.Add(CreateDivisionUnit("PROCEDURE-DIVISION", SemanticUnitType.ProcedureDivision,
                procedureStart.Value, lines.Length, lines));
        }

        // Add sections
        foreach (var (name, start, end) in sections)
        {
            units.Add(CreateSectionUnit(name, SemanticUnitType.Section, start, end, lines, "PROCEDURE-DIVISION"));
        }

        // Add paragraphs
        foreach (var (name, start, end, section) in paragraphs)
        {
            var parentId = section != null ? $"SECTION:{section}" : "PROCEDURE-DIVISION";
            units.Add(CreateParagraphUnit(name, start, end, lines, parentId));
        }

        // Identify dependencies between paragraphs
        IdentifyDependencies(units, content);

        // Estimate tokens for each unit
        foreach (var unit in units)
        {
            unit.EstimatedTokens = EstimateTokens(unit.Content);
        }

        return Task.FromResult<IReadOnlyList<SemanticUnit>>(units);
    }

    public Task<IReadOnlyList<VariableDeclaration>> ExtractVariablesAsync(
        string content,
        CancellationToken cancellationToken = default)
    {
        var variables = new List<VariableDeclaration>();
        var lines = content.Split('\n');
        var currentParent = new Stack<(int Level, string Name)>();

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            var lineNumber = i + 1;

            // Try variable with PIC clause
            var varMatch = VariableRegex.Match(line);
            if (varMatch.Success)
            {
                var level = int.Parse(varMatch.Groups[1].Value);
                var name = varMatch.Groups[2].Value;
                var picClause = varMatch.Groups[4].Value.Trim();
                var initialValue = varMatch.Groups[5].Success ? varMatch.Groups[5].Value.Trim() : null;

                // Update parent stack
                while (currentParent.Count > 0 && currentParent.Peek().Level >= level)
                {
                    currentParent.Pop();
                }

                variables.Add(new VariableDeclaration
                {
                    LegacyName = name,
                    LegacyType = $"PIC {picClause}",
                    Level = level,
                    ParentName = currentParent.Count > 0 ? currentParent.Peek().Name : null,
                    LineNumber = lineNumber,
                    InitialValue = initialValue,
                    IsGroup = false
                });

                continue;
            }

            // Try group variable (no PIC clause)
            var groupMatch = GroupVariableRegex.Match(line);
            if (groupMatch.Success && !line.ToUpperInvariant().Contains("PIC"))
            {
                var level = int.Parse(groupMatch.Groups[1].Value);
                var name = groupMatch.Groups[2].Value;

                // Skip if this looks like a paragraph name or reserved word
                if (name.ToUpperInvariant() is "FILLER" or "EXIT" or "STOP" or "END-PROGRAM")
                    continue;

                // Update parent stack
                while (currentParent.Count > 0 && currentParent.Peek().Level >= level)
                {
                    currentParent.Pop();
                }

                variables.Add(new VariableDeclaration
                {
                    LegacyName = name,
                    LegacyType = "GROUP",
                    Level = level,
                    ParentName = currentParent.Count > 0 ? currentParent.Peek().Name : null,
                    LineNumber = lineNumber,
                    IsGroup = true
                });

                currentParent.Push((level, name));
            }
        }

        return Task.FromResult<IReadOnlyList<VariableDeclaration>>(variables);
    }

    public Task<IReadOnlyList<CallDependency>> ExtractCallDependenciesAsync(
        string content,
        CancellationToken cancellationToken = default)
    {
        var dependencies = new List<CallDependency>();
        var lines = content.Split('\n');
        string? currentParagraph = null;

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            var lineNumber = i + 1;

            // Track current paragraph
            var paragraphMatch = ParagraphRegex.Match(line);
            if (paragraphMatch.Success && !line.ToUpperInvariant().Contains("SECTION"))
            {
                currentParagraph = paragraphMatch.Groups[1].Value;
            }

            if (currentParagraph == null) continue;

            // PERFORM statements
            foreach (Match match in PerformRegex.Matches(line))
            {
                dependencies.Add(new CallDependency
                {
                    CallerName = currentParagraph,
                    CalledName = match.Groups[1].Value,
                    LineNumber = lineNumber,
                    CallType = "PERFORM",
                    IsExternal = false
                });
            }

            // CALL statements
            foreach (Match match in CallRegex.Matches(line))
            {
                dependencies.Add(new CallDependency
                {
                    CallerName = currentParagraph,
                    CalledName = match.Groups[1].Value,
                    LineNumber = lineNumber,
                    CallType = "CALL",
                    IsExternal = true
                });
            }

            // GO TO statements
            foreach (Match match in GoToRegex.Matches(line))
            {
                dependencies.Add(new CallDependency
                {
                    CallerName = currentParagraph,
                    CalledName = match.Groups[1].Value,
                    LineNumber = lineNumber,
                    CallType = "GO TO",
                    IsExternal = false
                });
            }
        }

        return Task.FromResult<IReadOnlyList<CallDependency>>(dependencies);
    }

    public Task<IReadOnlyList<ExternalReference>> ExtractExternalReferencesAsync(
        string content,
        CancellationToken cancellationToken = default)
    {
        var references = new List<ExternalReference>();
        var lines = content.Split('\n');

        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            var lineNumber = i + 1;

            foreach (Match match in CopyRegex.Matches(line))
            {
                var copyName = match.Groups[1].Value;
                // Check for library specification
                string? library = null;
                if (line.ToUpperInvariant().Contains("OF ") || line.ToUpperInvariant().Contains("IN "))
                {
                    var libMatch = Regex.Match(line, @"(?:OF|IN)\s+([A-Z0-9][-A-Z0-9]*)", RegexOptions.IgnoreCase);
                    if (libMatch.Success)
                    {
                        library = libMatch.Groups[1].Value;
                    }
                }

                references.Add(new ExternalReference
                {
                    FileName = copyName,
                    LineNumber = lineNumber,
                    ReferenceType = "COPY",
                    Library = library
                });
            }
        }

        return Task.FromResult<IReadOnlyList<ExternalReference>>(references);
    }

    public string ConvertNameDeterministic(string legacyName, NameType nameType)
    {
        if (string.IsNullOrEmpty(legacyName))
            return string.Empty;

        // Normalize the input
        var normalized = legacyName.Trim().ToUpperInvariant();

        // Split by hyphens (COBOL convention)
        var parts = normalized.Split('-', StringSplitOptions.RemoveEmptyEntries);

        // Convert to camelCase or PascalCase based on name type
        var result = nameType switch
        {
            NameType.Class or NameType.Section => ToPascalCase(parts),
            NameType.Method or NameType.Variable or NameType.Parameter => ToCamelCase(parts),
            NameType.Constant => ToConstantCase(parts),
            _ => ToCamelCase(parts)
        };

        return result;
    }

    private static string ToCamelCase(string[] parts)
    {
        if (parts.Length == 0) return string.Empty;

        var result = parts[0].ToLowerInvariant();
        for (int i = 1; i < parts.Length; i++)
        {
            result += CapitalizeFirst(parts[i]);
        }
        return result;
    }

    private static string ToPascalCase(string[] parts)
    {
        return string.Join("", parts.Select(CapitalizeFirst));
    }

    private static string ToConstantCase(string[] parts)
    {
        return string.Join("_", parts.Select(p => p.ToUpperInvariant()));
    }

    private static string CapitalizeFirst(string s)
    {
        if (string.IsNullOrEmpty(s)) return s;
        return char.ToUpperInvariant(s[0]) + s.Substring(1).ToLowerInvariant();
    }

    private static SemanticUnit CreateDivisionUnit(string name, SemanticUnitType type, int startLine, int endLine, string[] lines)
    {
        return new SemanticUnit
        {
            Id = $"DIVISION:{name}",
            LegacyName = name,
            UnitType = type,
            StartLine = startLine,
            EndLine = endLine,
            Content = ExtractContent(lines, startLine, endLine)
        };
    }

    private static SemanticUnit CreateSectionUnit(string name, SemanticUnitType type, int startLine, int endLine, string[] lines, string parentId)
    {
        return new SemanticUnit
        {
            Id = $"SECTION:{name}",
            LegacyName = name,
            UnitType = type,
            StartLine = startLine,
            EndLine = endLine,
            Content = ExtractContent(lines, startLine, endLine),
            ParentId = parentId
        };
    }

    private static SemanticUnit CreateParagraphUnit(string name, int startLine, int endLine, string[] lines, string parentId)
    {
        return new SemanticUnit
        {
            Id = $"PARAGRAPH:{name}",
            LegacyName = name,
            UnitType = SemanticUnitType.Paragraph,
            StartLine = startLine,
            EndLine = endLine,
            Content = ExtractContent(lines, startLine, endLine),
            ParentId = parentId
        };
    }

    private static string ExtractContent(string[] lines, int startLine, int endLine)
    {
        var start = Math.Max(0, startLine - 1);
        var end = Math.Min(lines.Length, endLine);
        return string.Join("\n", lines.Skip(start).Take(end - start));
    }

    private static void CloseDataSections(List<SemanticUnit> units, string[] lines, int endLine,
        ref int? workingStorageStart, ref int? linkageStart, ref int? fileStart)
    {
        if (workingStorageStart.HasValue)
        {
            units.Add(CreateSectionUnit("WORKING-STORAGE", SemanticUnitType.WorkingStorageSection,
                workingStorageStart.Value, endLine, lines, "DATA-DIVISION"));
            workingStorageStart = null;
        }

        if (linkageStart.HasValue)
        {
            units.Add(CreateSectionUnit("LINKAGE", SemanticUnitType.LinkageSection,
                linkageStart.Value, endLine, lines, "DATA-DIVISION"));
            linkageStart = null;
        }

        if (fileStart.HasValue)
        {
            units.Add(CreateSectionUnit("FILE", SemanticUnitType.FileSection,
                fileStart.Value, endLine, lines, "DATA-DIVISION"));
            fileStart = null;
        }
    }

    private void IdentifyDependencies(List<SemanticUnit> units, string content)
    {
        var paragraphUnits = units.Where(u => u.UnitType == SemanticUnitType.Paragraph).ToList();
        var paragraphNames = paragraphUnits.Select(p => p.LegacyName.ToUpperInvariant()).ToHashSet();

        foreach (var unit in paragraphUnits)
        {
            // Find PERFORM references in this paragraph's content
            foreach (Match match in PerformRegex.Matches(unit.Content))
            {
                var targetName = match.Groups[1].Value.ToUpperInvariant();
                if (paragraphNames.Contains(targetName) && targetName != unit.LegacyName.ToUpperInvariant())
                {
                    if (!unit.Dependencies.Contains(targetName))
                    {
                        unit.Dependencies.Add(targetName);
                    }

                    // Also update the target's dependents
                    var targetUnit = paragraphUnits.FirstOrDefault(p =>
                        p.LegacyName.Equals(targetName, StringComparison.OrdinalIgnoreCase));
                    if (targetUnit != null && !targetUnit.Dependents.Contains(unit.LegacyName))
                    {
                        targetUnit.Dependents.Add(unit.LegacyName);
                    }
                }
            }
        }
    }

    private static int EstimateTokens(string content)
    {
        // Rough estimation: ~4 characters per token on average for COBOL
        return content.Length / 4;
    }
}
