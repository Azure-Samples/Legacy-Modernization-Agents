using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace McpChatWeb.Services;

// Focused conversion (preview): projects the REKT estate into the choices the portal can honestly
// offer. Anything the scan did not measure is reported as absent, never as zero.
public sealed class ProgramCatalogService
{
    private readonly RektEstateReader _reader;

    public ProgramCatalogService(RektEstateReader reader) => _reader = reader;

    public async Task<ProgramCatalog> BuildCatalogAsync(CancellationToken cancellationToken = default)
    {
        var estate = await _reader.ReadAsync(cancellationToken);

        var programs = estate.Programs
            .Where(p => !p.IsCopybook)
            .Select(p => ToCatalogProgram(p, estate.MissingCopybooks))
            .OrderBy(p => p.RelativePath, StringComparer.OrdinalIgnoreCase)
            .ToList();

        var closureAvailable = programs.Any(p => p.HasClosureEvidence);

        return new ProgramCatalog
        {
            SourceRoot = estate.SourceRoot,
            Programs = programs,
            ClosureAvailable = closureAvailable,
            ClosureUnavailableReason = closureAvailable
                ? ""
                : "No call facts found. Run ./doctor.sh rekt-scan to record them before selecting a dependency closure.",
            DeferredSelectors = ProgramCatalog.NotYetShippedSelectors,
        };
    }

    public IReadOnlyList<CatalogProgram> Search(ProgramCatalog catalog, string? query)
    {
        if (string.IsNullOrWhiteSpace(query)) return catalog.Programs;

        var trimmed = query.Trim();
        return catalog.Programs
            .Where(p => p.RelativePath.Contains(trimmed, StringComparison.OrdinalIgnoreCase)
                || p.Basename.Contains(trimmed, StringComparison.OrdinalIgnoreCase))
            .ToList();
    }

    private static CatalogProgram ToCatalogProgram(
        RektProgramRecord record, IReadOnlyList<MissingCopybookRow> missingCopybooks)
    {
        // Facts are the only record of who calls whom; without them a count would be a guess.
        var hasClosureEvidence = record.HasFacts;

        return new CatalogProgram
        {
            RelativePath = record.RelativePath,
            Basename = record.Basename,
            LinesOfCode = record.LinesOfCode,
            ParseFidelity = record.ParseFidelity,
            FidelitySource = record.FidelitySource,
            AmbiguousBasename = record.AmbiguousBasename,
            HasClosureEvidence = hasClosureEvidence,
            CalleeCount = hasClosureEvidence ? record.Callees.Count : null,
            CallerCount = hasClosureEvidence ? record.Callers.Count : null,
            MissingCopybookCount = missingCopybooks.Count(row =>
                row.ReferencedBy.Contains(record.Basename, StringComparer.OrdinalIgnoreCase)
                || row.ReferencedBy.Contains(record.Stem, StringComparer.OrdinalIgnoreCase)),
        };
    }
}

public sealed class ProgramCatalog
{
    // Selector kinds preview offered that this release does not implement. Named here so the UI
    // can say so outright instead of rendering an empty filter that looks like a real result.
    public static readonly IReadOnlyList<string> NotYetShippedSelectors = ["transaction", "wave", "component"];

    public string SourceRoot { get; set; } = "";
    public IReadOnlyList<CatalogProgram> Programs { get; set; } = [];
    public bool ClosureAvailable { get; set; }
    public string ClosureUnavailableReason { get; set; } = "";
    public IReadOnlyList<string> DeferredSelectors { get; set; } = [];
}

public sealed class CatalogProgram
{
    public string RelativePath { get; set; } = "";
    public string Basename { get; set; } = "";
    public int LinesOfCode { get; set; }
    public string ParseFidelity { get; set; } = Services.ParseFidelity.NotParsed;
    public string FidelitySource { get; set; } = FidelitySources.None;
    public bool AmbiguousBasename { get; set; }
    public bool HasClosureEvidence { get; set; }
    public int? CalleeCount { get; set; }
    public int? CallerCount { get; set; }
    public int MissingCopybookCount { get; set; }
}
