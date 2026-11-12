using McpChatWeb.Services;
using Neo4j.Driver;
using System.Text.Json;

namespace McpChatWeb.Endpoints;

public sealed record EstateConvertRequest(
    string ClusterId,
    string TargetLanguage = "Java",
    bool IncludeNeeds = true,
    string SpeedProfile = "balanced",
    string? Provider = null,
    string? ModelId = null,
    string? Name = null,
    string DependencyMode = "strict");

public static class EstateEndpoints
{
    public static void MapEstateEndpoints(this WebApplication app)
    {
        var group = app.MapGroup("/api/estate").WithTags("Estate Mission Control");
        group.MapGet("/draft-plan", AssessDraftPlanAsync)
            .WithSummary("Read-only draft eligibility from current local hash-bound evidence; no model calls or graph writes.");

        group.MapGet("/summary", async (EstateGraphService service, CancellationToken ct) =>
                Results.Ok(await service.GetSummaryAsync(ct)))
            .WithName("GetEstateSummary")
            .WithSummary("Clusters, waves, hubs and per-program status, without the edges.");

        group.MapGet("/graph", async (string? cluster, EstateGraphService service, CancellationToken ct) =>
                await service.GetGraphViewAsync(cluster, ct) is { } view
                    ? Results.Ok(view)
                    : Results.NotFound(new { error = $"No cluster '{cluster}'." }))
            .WithName("GetEstateGraph")
            .WithSummary("Nodes and edges of the estate, or of one cluster and its neighbours.");

        group.MapGet("/mission", async (EstateGraphService service, CancellationToken ct) =>
                Results.Ok(await service.GetMissionAsync(ct)))
            .WithName("GetEstateMission")
            .WithSummary("Mission Control: nodes with kind, technology, business function and status; carve-out clusters with owned and shared data; KPIs.");

        group.MapPost("/rebuild", async (EstateGraphService service, CancellationToken ct) =>
            {
                var (graph, warning) = await service.RebuildAsync(ct);
                return Results.Ok(new { graph.GeneratedAtUtc, graph.Counts, warning });
            })
            .WithName("RebuildEstateGraph")
            .WithSummary("Rescan the source and rebuild the estate graph now.");

        // Node ids carry a kind prefix and may carry a source-relative path.
        group.MapGet("/node/{**id}", async (string id, EstateGraphService service, CancellationToken ct) =>
                await service.GetNodeAsync(id, ct) is { } node
                    ? Results.Ok(node)
                    : Results.NotFound(new { error = $"No node '{id}'." }))
            .WithName("GetEstateNode")
            .WithSummary("One node with every edge in and out of it and the source lines behind each.");

        group.MapGet("/cluster/{id}/slice", async (string id, bool? includeNeeds, EstateGraphService service, CancellationToken ct) =>
                await service.GetSliceAsync(id, includeNeeds ?? true, ct) is { } slice
                    ? Results.Ok(slice)
                    : Results.NotFound(new { error = $"No cluster '{id}'." }))
            .WithName("GetEstateSlice")
            .WithSummary("What converting a cluster takes: its programs, what they call, what is missing, which jobs can run.");

        group.MapPost("/slice/convert", ConvertSliceAsync)
            .WithName("ConvertEstateSlice")
            .WithSummary("Start a convert-only run over one cluster's slice.");
    }

    internal static async Task<IResult> AssessDraftPlanAsync(
        RektEstateReader reader, CancellationToken ct, string? scanRunId = null, ILoggerFactory? loggerFactory = null)
    {
        ct.ThrowIfCancellationRequested();
        SnapshotEvidenceBinding? binding = null;
        if (!string.IsNullOrEmpty(scanRunId))
        {
            IResult Unavailable(string reason) => Results.Conflict(new
            {
                code = "snapshot-evidence-binding-unavailable",
                scanRunId,
                missingEvidence = $"Server-approved exact source/artifact binding for selected scan {scanRunId}",
                reason = reason + " No default-path assessment was performed.",
                action = "Restore the selected scan's recorded manifest and original/prepared source and artifact directories with matching ingestion paths. Alternatively run dependency-plan offline against that scan's exact inputs. No default-path assessment or conversion was performed."
            });
            if (!long.TryParse(scanRunId, out var id) || id < 1)
                return Unavailable("A positive numeric named graph snapshot ID is required; latest/all cannot identify exact evidence.");
            if (!RektNeo4j.IsConfigured)
                return Unavailable("The REKT graph connection is not configured, so its recorded ingestion binding cannot be read.");
            try
            {
                await using var session = RektNeo4j.Shared.AsyncSession();
                var cursor = await session.RunAsync(
                    "MATCH (s:RektScan {id:$id}) RETURN s.manifestPath AS manifest, " +
                    "s.sourceDirectory AS source, s.outputDirectory AS output, s.ingestionStatus AS status",
                    new { id });
                ct.ThrowIfCancellationRequested();
                if (!await cursor.FetchAsync())
                    return Unavailable("No named scan metadata is recorded for this graph snapshot.");
                var row = cursor.Current;
                var manifest = row["manifest"].As<string?>();
                var source = row["source"].As<string?>();
                var output = row["output"].As<string?>();
                if (row["status"].As<string?>() != "complete" || manifest is null || source is null || output is null)
                    return Unavailable("This snapshot has no complete ingestion record with manifest, prepared-source and artifact paths.");
                binding = SnapshotEvidenceBinding.Resolve(reader.RepoRoot, manifest, source, output);
            }
            catch (OperationCanceledException) when (ct.IsCancellationRequested) { throw; }
            catch (Exception ex) when (ex is IOException or UnauthorizedAccessException or JsonException
                                       or InvalidDataException or ArgumentException or Neo4jException)
            {
                (loggerFactory?.CreateLogger("SnapshotDraftAssessment")
                    ?? Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance)
                    .LogWarning(ex, "Cannot bind selected REKT snapshot {ScanRunId} for draft assessment", scanRunId);
                return Unavailable($"Selected-snapshot evidence binding failed: {ex.Message}");
            }
        }
        var sourceRoot = binding?.SourceRoot ?? reader.SourceRoot;
        var factsRoot = binding?.FactsRoot ?? reader.RektDir;
        var helper = new CobolToQuarkusMigration.Helpers.FileHelper(
            Microsoft.Extensions.Logging.Abstractions.NullLogger<CobolToQuarkusMigration.Helpers.FileHelper>.Instance);
        var files = (await helper.ScanDirectoryForCobolFilesAsync(sourceRoot))
            .Where(file => CobolToQuarkusMigration.Helpers.SourceTypeRegistry.IsCobolProgram(file.FileName)).ToList();
        if (binding is not null)
        {
            // Frozen COPY search inputs can include programs outside the recorded scan scope.
            files = files.Where(file => binding.Programs.Contains(
                Path.GetRelativePath(sourceRoot, file.FilePath).Replace('\\', '/'))).ToList();
            if (files.Count != binding.Programs.Count)
                return Results.Conflict(new
                {
                    code = "snapshot-source-scope-unavailable",
                    scanRunId,
                    missingEvidence = "All recorded original programs in the selected native scan",
                    reason = "The current readable source inventory does not match the selected manifest's exact program scope. No default-path assessment was performed.",
                    action = "Restore the exact recorded snapshot inputs before reassessing; no conversion was started."
                });
        }
        ct.ThrowIfCancellationRequested();
        var plan = CobolToQuarkusMigration.Agents.Infrastructure.Facts.DraftConversionPlanner.Assess(
            new(factsRoot), factsRoot, files, sourceRoot);
        if (binding is null) return Results.Ok(plan);
        return Results.Ok(new
        {
            plan.Mode,
            plan.Verified,
            plan.Status,
            plan.Isolation,
            plan.Programs,
            scanRunId,
            manifestPath = binding is null ? null : Path.GetRelativePath(reader.RepoRoot, binding.ManifestPath)
        });
    }

    internal static async Task<IResult> ConvertSliceAsync(
        EstateConvertRequest request, EstateGraphService service, ProcessManager pm, CancellationToken ct)
    {
        IResult Rejected(string reason, int status)
        {
            var rejected = pm.RecordRejectedLaunch(request.Name ?? "Estate conversion validation",
                reason, request.TargetLanguage, request.SpeedProfile);
            return Results.Json(new { error = reason, runId = rejected.RunId }, statusCode: status);
        }
        if (string.IsNullOrWhiteSpace(request.ClusterId))
            return Rejected("clusterId is required.", 400);
        try
        {
            ProcessManager.ValidateLaunchSettings("convert-only", request.TargetLanguage,
                request.SpeedProfile, request.Provider);
            CobolToQuarkusMigration.Helpers.ConversionEligibility.ValidateMode(request.DependencyMode);
        }
        catch (ArgumentException ex) { return Rejected(ex.Message, 400); }
        EstateSliceView? slice;
        try
        {
            ct.ThrowIfCancellationRequested();
            slice = await service.GetSliceAsync(request.ClusterId, request.IncludeNeeds, ct);
        }
        catch (OperationCanceledException)
        {
            var cancelled = pm.RecordRejectedLaunch(request.Name ?? "Estate conversion preparation",
                "Launch preparation was cancelled before a conversion process started.", request.TargetLanguage, request.SpeedProfile,
                status: "cancelled");
            return Results.Json(new { error = cancelled.Error, runId = cancelled.RunId }, statusCode: 409);
        }
        catch (Exception ex) when (ex is IOException or InvalidOperationException)
        {
            return Rejected("Could not resolve the selected source slice. Refresh the estate inventory and inspect portal diagnostics before retrying.", 503);
        }
        if (slice is null)
            return Rejected("The selected cluster no longer exists. Refresh Estate Mission Control before retrying.", 404);
        if (slice.Selectors.Count == 0)
            return Rejected("The slice has no programs to convert.", 400);
        if (request.DependencyMode != "partial" && slice.Readiness?.Allows(request.DependencyMode) != true)
        {
            var message = slice.Readiness?.BlockedMessage ?? "Dependency readiness is unavailable; no conversion started.";
            var rejected = pm.RecordRejectedLaunch(request.Name ?? "Estate dependency preflight",
                message, request.TargetLanguage, request.SpeedProfile);
            return Results.Conflict(new { error = message, runId = rejected.RunId, readiness = slice.Readiness });
        }

        ManagedRun run;
        try
        {
            run = pm.StartRun("convert-only",
                request.Name ?? $"slice-{slice.Slice.ClusterId}-{DateTime.Now:HHmmss}",
                request.TargetLanguage, request.SpeedProfile, service.SourceFolderForRun,
                request.Provider, request.ModelId,
                extraEnv: new() { [CobolToQuarkusMigration.Helpers.ConversionEligibility.EnvironmentVariable] = request.DependencyMode },
                programs: slice.Selectors);
        }
        catch (ArgumentException ex)
        {
            return Results.BadRequest(new { error = ex.Message, runId = (ex as ManagedRunValidationException)?.Run.RunId });
        }

        var response = new
        {
            run.RunId,
            run.Name,
            run.Status,
            run.Programs,
            slice.Slice.ClusterId,
            slice.Slice.Missing,
            slice.Slice.Jobs,
            run.Error,
            run.OutputFolder,
            request.DependencyMode,
            Verified = false,
            LogPath = run.LogPath is null ? null : $"Logs/portal/{run.RunId}.log"
        };
        return run.Status == "failed"
            ? Results.Json(response, statusCode: StatusCodes.Status503ServiceUnavailable)
            : Results.Ok(response);
    }
}
