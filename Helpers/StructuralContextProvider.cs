// Resolves structural context from REKT, deterministic readers, or the optional LLM fallback.

using System.Collections.Concurrent;
using System.Text.Json;

namespace CobolToQuarkusMigration.Helpers;

public sealed class StructuralContextProvider
{
    private readonly string _repoRoot;
    private readonly string _sourceFolder;
    private readonly RektContextLoader _loader;
    private readonly Func<string, string, Task<RektContext?>>? _llmFallback;
    private readonly ConcurrentDictionary<string, StructuralContext> _cache = new(StringComparer.OrdinalIgnoreCase);
    private readonly bool _fallbackToAi;
    private readonly string _llmCacheDir;

    public StructuralContextProvider(
        string repoRoot,
        string sourceFolder,
        bool fallbackToAi,
        Func<string, string, Task<RektContext?>>? llmFallback = null)
    {
        _repoRoot = repoRoot;
        _sourceFolder = sourceFolder;
        _loader = new RektContextLoader(repoRoot);
        _llmFallback = llmFallback;
        _fallbackToAi = fallbackToAi;
        _llmCacheDir = Path.Combine(repoRoot, "output", "rekt", "llm-derived");
        Directory.CreateDirectory(_llmCacheDir);
    }

    public RektContextLoader Loader => _loader;

    public async Task<StructuralContext> GetAsync(string programFileName)
    {
        if (_cache.TryGetValue(programFileName, out var cached)) return cached;

        var sc = await ResolveAsync(programFileName);
        _cache[programFileName] = sc;
        return sc;
    }

    private async Task<StructuralContext> ResolveAsync(string programFileName)
    {
        // 1) Non-COBOL artefacts: dispatch to deterministic readers.
        var ext = Path.GetExtension(programFileName).ToLowerInvariant();
        var srcPath = Path.Combine(_repoRoot, _sourceFolder, programFileName);
        if (ext is ".bms")
        {
            var mapset = BmsReader.ParseFile(srcPath);
            if (mapset != null)
            {
                return new StructuralContext
                {
                    Program = programFileName,
                    Provenance = StructuralProvenance.RektNative,
                    Confidence = 0.95,
                    Context = BmsReader.ToRektContext(mapset),
                    Notes = $"BMS mapset '{mapset.Name}' with {mapset.Maps.Count} map(s).",
                };
            }
        }
        if (ext is ".dbd")
        {
            var dbd = ImsDbdReader.ParseFile(srcPath);
            if (dbd != null)
            {
                return new StructuralContext
                {
                    Program = programFileName,
                    Provenance = StructuralProvenance.RektNative,
                    Confidence = 0.95,
                    Context = ImsDbdReader.ToRektContext(dbd),
                    Notes = $"IMS DBD '{dbd.Name}' with {dbd.Segments.Count} segment(s).",
                };
            }
        }
        if (ext is ".psb")
        {
            var psb = ImsPsbReader.ParseFile(srcPath);
            if (psb != null)
            {
                var ctx = new RektContext
                {
                    Program = programFileName,
                    IsCopybook = false,
                };
                foreach (var pcb in psb.Pcbs)
                {
                    ctx.CallTargets.Add(new RektCallTarget { TargetProgram = pcb.DbdName });
                    foreach (var s in pcb.Sensegs)
                        ctx.Sections.Add(new RektSection { Name = s.Name });
                }
                return new StructuralContext
                {
                    Program = programFileName,
                    Provenance = StructuralProvenance.RektNative,
                    Confidence = 0.9,
                    Context = ctx,
                    Notes = $"IMS PSB '{psb.Name}' with {psb.Pcbs.Count} PCB(s).",
                };
            }
        }

        // 2) COBOL — try REKT JSON.
        var rekt = _loader.Load(programFileName, _sourceFolder);
        var hasFullAst = rekt.Sections.Count > 0;
        var hasDeps = rekt.CallTargets.Count > 0 || rekt.CopybookUsage.Count > 0;

        if (hasFullAst)
        {
            return new StructuralContext
            {
                Program = programFileName,
                Provenance = StructuralProvenance.RektNative,
                Confidence = 0.95,
                Context = rekt,
            };
        }

        if (hasDeps && !_fallbackToAi)
        {
            return new StructuralContext
            {
                Program = programFileName,
                Provenance = StructuralProvenance.RektPartial,
                Confidence = 0.55,
                Context = rekt,
                Notes = "REKT parsed dependencies only (AST writer NPE). Structure missing — converter should derive from source.",
            };
        }

        // 3) LLM fallback — opt-in.
        if (_fallbackToAi && _llmFallback != null)
        {
            // Disk cache check
            var cachePath = Path.Combine(_llmCacheDir, programFileName.Replace('/', '_') + ".json");
            RektContext? extracted = null;
            if (File.Exists(cachePath))
            {
                try
                {
                    extracted = JsonSerializer.Deserialize<RektContext>(File.ReadAllText(cachePath));
                }
                catch { /* cache corrupted — re-run */ }
            }
            if (extracted is null && File.Exists(srcPath))
            {
                var source = await File.ReadAllTextAsync(srcPath);
                extracted = await _llmFallback(programFileName, source);
                if (extracted != null)
                {
                    try { File.WriteAllText(cachePath, JsonSerializer.Serialize(extracted, new JsonSerializerOptions { WriteIndented = true })); } catch { }
                }
            }
            if (extracted != null)
            {
                // Merge with deps if we had them
                if (hasDeps)
                {
                    extracted.CallTargets = extracted.CallTargets
                        .Concat(rekt.CallTargets.Where(c =>
                            !extracted.CallTargets.Any(e => e.TargetProgram == c.TargetProgram)))
                        .ToList();
                    extracted.CopybookUsage = extracted.CopybookUsage
                        .Concat(rekt.CopybookUsage.Where(c => !extracted.CopybookUsage.Contains(c, StringComparer.OrdinalIgnoreCase)))
                        .ToList();
                }
                extracted.Program = programFileName;
                extracted.TargetPlan = rekt.TargetPlan;
                return new StructuralContext
                {
                    Program = programFileName,
                    Provenance = StructuralProvenance.LlmExtracted,
                    Confidence = 0.45,
                    Context = extracted,
                    Notes = "Structure extracted by StructuralExtractorAgent — treat as hypothesis, verify against source.",
                };
            }
        }

        // 4) Nothing.
        return new StructuralContext
        {
            Program = programFileName,
            Provenance = hasDeps ? StructuralProvenance.RektPartial : StructuralProvenance.None,
            Confidence = hasDeps ? 0.4 : 0.1,
            Context = rekt,
            Notes = hasDeps
                ? "Only dependency edges available. Converter should re-derive sections/paragraphs from source."
                : "No structural context — converter operates on raw source only.",
        };
    }
}
