using System.Text;
using CobolToQuarkusMigration.Agents.Infrastructure.Facts;
using CobolToQuarkusMigration.Helpers;

namespace CobolToQuarkusMigration.Agents;

// Deterministic check that COBOL structure survived conversion. No LLM involvement: a
// validator that guesses is worse than no validator.
internal static class ConversionParityValidator
{
    internal const string ProceduresAxis = "procedures";
    internal const string DataFieldsAxis = "dataFields";
    internal const string CallTargetsAxis = "callTargets";
    internal const string SqlTablesAxis = "sqlTables";

    // Preview's published weighting, kept so scores stay comparable across branches.
    private const double ProceduresWeight = 0.40;
    private const double DataFieldsWeight = 0.25;
    private const double CallTargetsWeight = 0.20;
    private const double SqlTablesWeight = 0.15;

    // Below this length a name matches too much to mean anything.
    private const int MinimumComparableLength = 3;

    // Compact matching ignores word boundaries, so it needs a longer name to stay honest.
    private const int MinimumCompactLength = 4;

    // Weight of a symbol found only in comments or string literals. Not zero, because renaming
    // is legitimate; not one, because a comment is not converted code.
    private const double CommentEvidenceWeight = 0.5;

    // Storage-section prefixes carry no meaning a converter is expected to preserve.
    private static readonly HashSet<string> ScopePrefixes =
        new(StringComparer.Ordinal) { "ws", "ls", "lk", "fd", "sd" };

    private static readonly HashSet<string> PerformKeywords = new(StringComparer.OrdinalIgnoreCase)
    {
        "THRU", "THROUGH", "UNTIL", "VARYING", "TIMES", "TEST", "BEFORE", "AFTER",
        "FROM", "BY", "WITH", "AND", "OR", "NOT", "GIVING", "END-PERFORM", "PERFORM",
    };

    private static readonly string[] StubMarkers =
    {
        "CONVERSION DID NOT PRODUCE USABLE",
        "CHUNK CONVERSION DID NOT PRODUCE USABLE",
    };

    internal static ProgramParityResult Evaluate(
        string program,
        string? generatedFile,
        string generatedCode,
        StructuralContext? structuralContext,
        ProgramFacts? facts,
        StubCopybookCatalog? stubCopybooks = null)
    {
        stubCopybooks ??= StubCopybookCatalog.Empty;
        var symbols = new SourceSymbols(generatedCode);
        var isStub = StubMarkers.Any(m => generatedCode.Contains(m, StringComparison.Ordinal));

        // A guard stub is direct evidence the conversion failed, so it is a measured zero even
        // without structural context. Reporting it as unmeasurable would hide a known failure.
        if (structuralContext is null || structuralContext.Provenance == StructuralProvenance.None)
        {
            if (!isStub)
            {
                return new ProgramParityResult
                {
                    Program = program,
                    GeneratedFile = generatedFile,
                    Outcome = ParityOutcome.NotEvaluated,
                    NotEvaluatedReason =
                        "No structural context available; parity cannot be measured. Run './doctor.sh rekt-full'.",
                    Provenance = structuralContext?.Provenance.ToString(),
                    StructuralConfidence = structuralContext?.Confidence,
                };
            }

            return new ProgramParityResult
            {
                Program = program,
                GeneratedFile = generatedFile,
                Outcome = ParityOutcome.Evaluated,
                Score = 0,
                IsDiagnosticStub = true,
                Provenance = structuralContext?.Provenance.ToString(),
                StructuralConfidence = structuralContext?.Confidence,
                Gaps = [new ParityGap
                {
                    Axis = "file",
                    Symbol = generatedFile ?? program,
                    Kind = ParityGapKind.Missing,
                    Detail = "File is a ConversionOutputGuard diagnostic stub; no converted code was produced.",
                }],
            };
        }

        // A stub or a file with no code at all is not a conversion, so a name surviving in its
        // comments is not evidence that anything was converted.
        var creditComments = !isStub && symbols.HasCode;

        var fieldNames = CollectDataFields(structuralContext.Context, stubCopybooks, out var stubbed);
        var expectedByAxis = new Dictionary<string, List<ExpectedSymbol>>
        {
            [ProceduresAxis] = CollectProcedures(structuralContext.Context, fieldNames),
            [DataFieldsAxis] = fieldNames,
            [CallTargetsAxis] = CollectCallTargets(structuralContext.Context, facts),
            [SqlTablesAxis] = CollectSqlTables(structuralContext.Context, facts),
        };

        var weights = new Dictionary<string, double>
        {
            [ProceduresAxis] = ProceduresWeight,
            [DataFieldsAxis] = DataFieldsWeight,
            [CallTargetsAxis] = CallTargetsWeight,
            [SqlTablesAxis] = SqlTablesWeight,
        };

        var axes = new List<ParityAxisResult>();
        var gaps = new List<ParityGap>();

        foreach (var (axisName, expected) in expectedByAxis)
        {
            axes.Add(ScoreAxis(axisName, weights[axisName], expected, symbols, creditComments, gaps));
        }

        var evidenceNotes = stubbed.Count == 0
            ? Array.Empty<string>()
            : new[]
            {
                $"{stubbed.Count} data field(s) excluded as artefacts of auto-generated stub copybooks: "
                + string.Join(", ", stubbed.OrderBy(n => n, StringComparer.OrdinalIgnoreCase))
                + ". The real copybooks were not found, so field-level parity is measured against "
                + "incomplete evidence.",
            };

        var scored = axes.Where(a => a.Coverage.HasValue).ToList();
        if (scored.Count == 0)
        {
            return new ProgramParityResult
            {
                Program = program,
                GeneratedFile = generatedFile,
                Outcome = ParityOutcome.NotEvaluated,
                NotEvaluatedReason =
                    "Structural context contains no comparable symbols on any axis.",
                Provenance = structuralContext.Provenance.ToString(),
                StructuralConfidence = structuralContext.Confidence,
                IsDiagnosticStub = isStub,
                EvidenceNotes = evidenceNotes,
                Axes = axes,
            };
        }

        // Renormalise over present axes so an absent axis neither rewards nor penalises.
        var totalWeight = scored.Sum(a => a.Weight);
        var score = scored.Sum(a => a.Weight * a.Coverage!.Value) / totalWeight;

        if (isStub)
        {
            gaps.Insert(0, new ParityGap
            {
                Axis = "file",
                Symbol = Path.GetFileName(generatedFile ?? program),
                Kind = ParityGapKind.Missing,
                Detail = "File is a ConversionOutputGuard diagnostic stub; no converted code was produced.",
            });
        }

        return new ProgramParityResult
        {
            Program = program,
            GeneratedFile = generatedFile,
            Outcome = ParityOutcome.Evaluated,
            Provenance = structuralContext.Provenance.ToString(),
            StructuralConfidence = structuralContext.Confidence,
            Score = Math.Round(score, 4),
            IsDiagnosticStub = isStub,
            EvidenceNotes = evidenceNotes,
            Axes = axes,
            Gaps = gaps,
        };
    }

    private static ParityAxisResult ScoreAxis(
        string axisName,
        double weight,
        List<ExpectedSymbol> expected,
        SourceSymbols symbols,
        bool creditComments,
        List<ParityGap> gaps)
    {
        var comparable = expected.Where(e => e.IsComparable).ToList();
        var excluded = expected.Count - comparable.Count;

        if (comparable.Count == 0)
        {
            return new ParityAxisResult
            {
                Name = axisName,
                Weight = weight,
                Expected = 0,
                Excluded = excluded,
                Coverage = null,
                Note = expected.Count == 0
                    ? "No symbols of this kind in the structural context."
                    : $"All {excluded} symbol(s) excluded as too short or non-comparable.",
            };
        }

        int inCode = 0, inCommentsOnly = 0, missing = 0;

        foreach (var symbol in comparable)
        {
            if (symbols.MatchesCode(symbol))
            {
                inCode++;
                continue;
            }

            if (symbols.MatchesComments(symbol))
            {
                if (creditComments)
                {
                    inCommentsOnly++;
                    gaps.Add(new ParityGap
                    {
                        Axis = axisName,
                        Symbol = symbol.Original,
                        Kind = ParityGapKind.PossiblyRenamedOrMerged,
                        Detail = symbol.Detail ?? "Present in comments or string literals only.",
                    });
                    continue;
                }

                missing++;
                gaps.Add(new ParityGap
                {
                    Axis = axisName,
                    Symbol = symbol.Original,
                    Kind = ParityGapKind.Missing,
                    Detail = symbol.Detail ?? "Present in comments only, in a file containing no converted code.",
                });
                continue;
            }

            missing++;
            gaps.Add(new ParityGap
            {
                Axis = axisName,
                Symbol = symbol.Original,
                Kind = ParityGapKind.Missing,
                Detail = symbol.Detail,
            });
        }

        // Comment-only evidence is weaker than code: a renamed procedure characteristically
        // survives as a comment, but a file that only names its symbols in comments converted
        // nothing. Half credit keeps a handful of renames passing while a hollow file fails.
        var covered = (inCode + CommentEvidenceWeight * inCommentsOnly) / comparable.Count;

        return new ParityAxisResult
        {
            Name = axisName,
            Weight = weight,
            Expected = comparable.Count,
            MatchedInCode = inCode,
            MatchedInCommentsOnly = inCommentsOnly,
            Missing = missing,
            Excluded = excluded,
            Coverage = covered,
        };
    }

    private static List<ExpectedSymbol> CollectProcedures(RektContext ctx, List<ExpectedSymbol> fields)
    {
        var fieldNames = fields
            .Select(f => f.Original)
            .ToHashSet(StringComparer.OrdinalIgnoreCase);

        var names = new List<string>();

        foreach (var section in ctx.Sections)
        {
            // The loader synthesises "(implicit)" for paragraphs with no enclosing section.
            if (!string.IsNullOrWhiteSpace(section.Name) && section.Name != "(implicit)")
                names.Add(section.Name);

            foreach (var paragraph in section.Paragraphs)
            {
                if (!string.IsNullOrWhiteSpace(paragraph.Name))
                    names.Add(paragraph.Name);
            }
        }

        // PERFORM targets are the only source of procedure names when a program's paragraphs
        // were not recorded, but a PERFORM ... UNTIL condition names a data item, not a procedure.
        foreach (var edge in ctx.PerformGraph)
        {
            foreach (var word in SplitPerformTarget(edge.To))
            {
                if (fieldNames.Contains(word)) continue;
                names.Add(word);
            }
        }

        return Distinct(names).Select(n => new ExpectedSymbol(n)).ToList();
    }

    private static IEnumerable<string> SplitPerformTarget(string target)
    {
        if (string.IsNullOrWhiteSpace(target)) yield break;

        foreach (var word in target.Split(
            new[] { ' ', '\t', ',', '(', ')' }, StringSplitOptions.RemoveEmptyEntries))
        {
            var trimmed = word.Trim().Trim('\'', '"', '.');
            if (trimmed.Length == 0) continue;
            if (PerformKeywords.Contains(trimmed)) continue;
            if (!trimmed.Any(char.IsLetter)) continue;
            yield return trimmed;
        }
    }

    private static List<ExpectedSymbol> CollectDataFields(
        RektContext ctx, StubCopybookCatalog stubCopybooks, out List<string> stubbed)
    {
        var found = new List<(string Name, string? Section)>();
        foreach (var item in ctx.DataStructure) FlattenDataItem(item, found, null);

        var sections = new Dictionary<string, string?>(StringComparer.OrdinalIgnoreCase);
        foreach (var (name, section) in found) sections.TryAdd(name, section);

        var distinct = Distinct(found.Select(f => f.Name));
        stubbed = distinct.Where(stubCopybooks.Contains).ToList();

        return distinct
            .Where(n => !stubCopybooks.Contains(n))
            .Select(n => new ExpectedSymbol(n, detail: DescribeSection(sections.GetValueOrDefault(n))))
            .ToList();
    }

    // A LINKAGE field is supplied by the caller, so a program that only reads it through a
    // condition name legitimately never spells it out. Saying where it came from stops the gap
    // being read as a dropped field.
    private static string? DescribeSection(string? section) =>
        string.Equals(section, "LINKAGE", StringComparison.OrdinalIgnoreCase)
            ? "Declared in the LINKAGE SECTION; supplied by the caller."
            : null;

    private static void FlattenDataItem(
        RektDataItem item, List<(string Name, string? Section)> into, string? inheritedSection)
    {
        var name = item.Name?.Trim() ?? "";
        var section = item.SourceSection ?? inheritedSection;
        var keep =
            name.Length > 0
            && item.Level >= 0
            && item.Level != 88   // condition names are booleans over another field, not storage
            && item.Level != 66   // RENAMES aliases an existing field
            && !IsProcedureRegister(item)
            && !name.Equals("FILLER", StringComparison.OrdinalIgnoreCase)
            && !name.StartsWith("TypedRecord", StringComparison.Ordinal);

        if (keep) into.Add((name, section));

        foreach (var child in item.Children) FlattenDataItem(child, into, section);
    }

    // A data node attributed to the PROCEDURE DIVISION is a special register smojol injected
    // (WHEN-COMPILED, TALLY), not declared storage, so no conversion can be expected to carry it.
    private static bool IsProcedureRegister(RektDataItem item) =>
        string.Equals(item.SourceSection, "PROCEDURE_DIVISION", StringComparison.OrdinalIgnoreCase);

    private static readonly string[] CobolSourceExtensions = { ".cbl", ".cob", ".cpy", ".ccp", ".cobol" };

    private static List<ExpectedSymbol> CollectCallTargets(RektContext ctx, ProgramFacts? facts)
    {
        // Dynamic targets name a variable resolved at runtime, so no generated identifier can
        // be expected to carry them.
        var names = facts?.Callees is { Count: > 0 } callees
            ? callees.ToList()
            : ctx.CallTargets.Where(c => !c.IsDynamic).Select(c => c.TargetProgram).ToList();

        return Distinct(names)
            .Select(n => new ExpectedSymbol(n, comparable: ComparableProgramName(n)))
            .ToList();
    }

    // Facts record a callee as a source-relative path so identity stays unique across duplicate
    // basenames; only the program stem can plausibly appear in generated code.
    private static string ComparableProgramName(string name)
    {
        var trimmed = name.Trim().Trim('\'', '"').Replace('\\', '/');
        if (trimmed.Length == 0) return name;

        var segment = trimmed[(trimmed.LastIndexOf('/') + 1)..];
        if (segment.Length == 0) return trimmed;

        foreach (var ext in CobolSourceExtensions)
        {
            if (segment.EndsWith(ext, StringComparison.OrdinalIgnoreCase))
                return segment[..^ext.Length];
        }

        return segment;
    }

    private static List<ExpectedSymbol> CollectSqlTables(RektContext ctx, ProgramFacts? facts)
    {
        // Operations are carried into the gap detail but not scored: SELECT has no predictable
        // textual form in JPA or JDBC, so matching on it would invent failures.
        if (facts?.Io.DbTables is { Count: > 0 } tables)
        {
            return tables
                .Where(t => !string.IsNullOrWhiteSpace(t.Name))
                .GroupBy(t => t.Name, StringComparer.OrdinalIgnoreCase)
                .Select(g => new ExpectedSymbol(
                    g.Key,
                    $"SQL operations on this table: {string.Join(", ", g.SelectMany(t => t.Operations).Distinct())}"))
                .ToList();
        }

        var fromContext = ctx.SqlStatements
            .SelectMany(s => s.Tables.Select(t => (Table: t, s.Operation)))
            .Where(x => !string.IsNullOrWhiteSpace(x.Table))
            .GroupBy(x => x.Table, StringComparer.OrdinalIgnoreCase)
            .Select(g => new ExpectedSymbol(
                g.Key,
                $"SQL operations on this table: {string.Join(", ", g.Select(x => x.Operation).Distinct())}"))
            .ToList();

        return fromContext;
    }

    private static List<string> Distinct(IEnumerable<string> names) => names
        .Where(n => !string.IsNullOrWhiteSpace(n))
        .Select(n => n.Trim())
        .Distinct(StringComparer.OrdinalIgnoreCase)
        .ToList();

    internal sealed class ExpectedSymbol
    {
        internal ExpectedSymbol(string original, string? detail = null, string? comparable = null)
        {
            Original = original;
            Detail = detail;

            var all = Tokenizer.SplitIdentifier(comparable ?? original);
            Tokens = all.Where(t => !t.All(char.IsDigit)).ToList();

            CoreTokens = Tokens.Count > 1 && ScopePrefixes.Contains(Tokens[0])
                ? Tokens.Skip(1).ToList()
                : Tokens;

            Compact = string.Concat(Tokens);
            CompactCore = string.Concat(CoreTokens);
        }

        internal string Original { get; }
        internal string? Detail { get; }
        internal List<string> Tokens { get; }
        internal List<string> CoreTokens { get; }
        internal string Compact { get; }
        internal string CompactCore { get; }

        internal bool IsComparable => CompactCore.Length >= MinimumComparableLength;
    }

    // Identifiers from a generated file, kept in two buckets so a name that survives only
    // inside a comment is distinguishable from one that survives in code.
    internal sealed class SourceSymbols
    {
        private readonly List<List<string>> _codeTokens = new();
        private readonly List<List<string>> _commentTokens = new();
        private readonly List<string> _codeCompact = new();
        private readonly List<string> _commentCompact = new();

        internal SourceSymbols(string source)
        {
            var (code, comments) = Tokenizer.SplitCodeAndComments(source);
            Load(code, _codeTokens, _codeCompact, joinHyphens: false);
            // Comments carry original COBOL names verbatim, so hyphens there join a single
            // symbol rather than separating operands as they would in code.
            Load(comments, _commentTokens, _commentCompact, joinHyphens: true);
        }

        internal bool HasCode => _codeTokens.Count > 0;

        private static void Load(string text, List<List<string>> tokens, List<string> compact, bool joinHyphens)
        {
            foreach (var identifier in Tokenizer.ExtractIdentifiers(text, joinHyphens))
            {
                var parts = Tokenizer.SplitIdentifier(identifier);
                if (parts.Count == 0) continue;
                tokens.Add(parts);
                compact.Add(string.Concat(parts.Where(p => !p.All(char.IsDigit))));
            }
        }

        internal bool MatchesCode(ExpectedSymbol symbol) => Matches(symbol, _codeTokens, _codeCompact);

        internal bool MatchesComments(ExpectedSymbol symbol) => Matches(symbol, _commentTokens, _commentCompact);

        private static bool Matches(ExpectedSymbol symbol, List<List<string>> tokens, List<string> compact)
        {
            foreach (var candidate in tokens)
            {
                if (ContainsSequence(candidate, symbol.Tokens)) return true;
                if (!ReferenceEquals(symbol.CoreTokens, symbol.Tokens)
                    && ContainsSequence(candidate, symbol.CoreTokens)) return true;
            }

            // COBOL names are unhyphenated inside a word (SUBPGM01), so word boundaries alone
            // miss conversions that reintroduce them (SubPgm01Service).
            foreach (var candidate in compact)
            {
                if (symbol.Compact.Length >= MinimumCompactLength
                    && candidate.Contains(symbol.Compact, StringComparison.Ordinal)) return true;
                if (symbol.CompactCore.Length >= MinimumCompactLength
                    && candidate.Contains(symbol.CompactCore, StringComparison.Ordinal)) return true;
            }

            return false;
        }

        private static bool ContainsSequence(List<string> haystack, List<string> needle)
        {
            if (needle.Count == 0 || needle.Count > haystack.Count) return false;

            for (var start = 0; start <= haystack.Count - needle.Count; start++)
            {
                var matched = true;
                for (var i = 0; i < needle.Count; i++)
                {
                    if (!string.Equals(haystack[start + i], needle[i], StringComparison.Ordinal))
                    {
                        matched = false;
                        break;
                    }
                }

                if (matched) return true;
            }

            return false;
        }
    }

    internal static class Tokenizer
    {
        // Ambiguity resolves toward the comment bucket: under-counting code produces a visible
        // gap, whereas leaking comment text into code produces a silent false pass.
        internal static (string Code, string Comments) SplitCodeAndComments(string source)
        {
            var code = new StringBuilder(source.Length);
            var comments = new StringBuilder();
            var i = 0;

            while (i < source.Length)
            {
                var c = source[i];
                var next = i + 1 < source.Length ? source[i + 1] : '\0';

                if (c == '/' && next == '/')
                {
                    i = Consume(source, i + 2, comments, s => s == '\n');
                    continue;
                }

                if (c == '/' && next == '*')
                {
                    i = ConsumeUntil(source, i + 2, comments, "*/");
                    continue;
                }

                if (c == '"' && next == '"' && i + 2 < source.Length && source[i + 2] == '"')
                {
                    i = ConsumeUntil(source, i + 3, comments, "\"\"\"");
                    continue;
                }

                if (c == '@' && next == '"')
                {
                    i = ConsumeVerbatim(source, i + 2, comments);
                    continue;
                }

                if (c is '"' or '\'')
                {
                    i = ConsumeQuoted(source, i + 1, comments, c);
                    continue;
                }

                code.Append(c);
                i++;
            }

            return (code.ToString(), comments.ToString());
        }

        private static int Consume(string source, int start, StringBuilder sink, Func<char, bool> stop)
        {
            var i = start;
            while (i < source.Length && !stop(source[i])) sink.Append(source[i++]);
            sink.Append(' ');
            return i;
        }

        private static int ConsumeUntil(string source, int start, StringBuilder sink, string terminator)
        {
            var end = source.IndexOf(terminator, start, StringComparison.Ordinal);
            if (end < 0) end = source.Length;
            sink.Append(source, start, end - start);
            sink.Append(' ');
            return Math.Min(end + terminator.Length, source.Length);
        }

        private static int ConsumeQuoted(string source, int start, StringBuilder sink, char quote)
        {
            var i = start;
            while (i < source.Length)
            {
                if (source[i] == '\\') { i += 2; continue; }
                if (source[i] == quote) { i++; break; }
                if (source[i] == '\n') break;   // unterminated literal; do not swallow the file
                sink.Append(source[i++]);
            }

            sink.Append(' ');
            return i;
        }

        private static int ConsumeVerbatim(string source, int start, StringBuilder sink)
        {
            var i = start;
            while (i < source.Length)
            {
                if (source[i] == '"')
                {
                    if (i + 1 < source.Length && source[i + 1] == '"') { i += 2; continue; }
                    i++;
                    break;
                }

                sink.Append(source[i++]);
            }

            sink.Append(' ');
            return i;
        }

        internal static IEnumerable<string> ExtractIdentifiers(string text, bool joinHyphens = false)
        {
            var current = new StringBuilder();
            foreach (var c in text)
            {
                if (char.IsLetterOrDigit(c) || c == '_' || c == '$'
                    || (joinHyphens && c == '-' && current.Length > 0))
                {
                    current.Append(c);
                    continue;
                }

                if (current.Length > 0)
                {
                    yield return current.ToString();
                    current.Clear();
                }
            }

            if (current.Length > 0) yield return current.ToString();
        }

        internal static List<string> SplitIdentifier(string identifier)
        {
            var parts = new List<string>();
            var current = new StringBuilder();
            var previous = '\0';

            void Flush()
            {
                if (current.Length == 0) return;
                parts.Add(current.ToString().ToLowerInvariant());
                current.Clear();
            }

            for (var i = 0; i < identifier.Length; i++)
            {
                var c = identifier[i];
                if (!char.IsLetterOrDigit(c))
                {
                    Flush();
                    previous = '\0';
                    continue;
                }

                if (previous != '\0')
                {
                    var boundary =
                        (char.IsLower(previous) && char.IsUpper(c))
                        || char.IsDigit(previous) != char.IsDigit(c)
                        // SQLQuery: break before the last capital of a run followed by lowercase.
                        || (char.IsUpper(previous) && char.IsUpper(c)
                            && i + 1 < identifier.Length && char.IsLower(identifier[i + 1]));

                    if (boundary) Flush();
                }

                current.Append(c);
                previous = c;
            }

            Flush();
            return parts;
        }
    }
}
