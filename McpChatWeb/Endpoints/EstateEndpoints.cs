using McpChatWeb.Services;

namespace McpChatWeb.Endpoints;

public sealed record EstateConvertRequest(
    string ClusterId,
    string TargetLanguage = "Java",
    bool IncludeNeeds = true,
    string SpeedProfile = "balanced",
    string? Provider = null,
    string? ModelId = null,
    string? Name = null);

public static class EstateEndpoints
{
    public static void MapEstateEndpoints(this WebApplication app)
    {
        var group = app.MapGroup("/api/estate").WithTags("Estate Mission Control");

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

        group.MapPost("/slice/convert", async (EstateConvertRequest request, EstateGraphService service, ProcessManager pm, CancellationToken ct) =>
            {
                if (string.IsNullOrWhiteSpace(request.ClusterId))
                    return Results.BadRequest(new { error = "clusterId is required." });
                var slice = await service.GetSliceAsync(request.ClusterId, request.IncludeNeeds, ct);
                if (slice is null)
                    return Results.NotFound(new { error = $"No cluster '{request.ClusterId}'." });
                if (slice.Selectors.Count == 0)
                    return Results.BadRequest(new { error = "The slice has no programs to convert." });

                ManagedRun run;
                try
                {
                    run = pm.StartRun("convert-only",
                        request.Name ?? $"slice-{slice.Slice.ClusterId}-{DateTime.Now:HHmmss}",
                        request.TargetLanguage, request.SpeedProfile, service.SourceFolderForRun,
                        request.Provider, request.ModelId, programs: slice.Selectors);
                }
                catch (ArgumentException ex)
                {
                    return Results.BadRequest(new { error = ex.Message });
                }

                return Results.Ok(new
                {
                    run.RunId,
                    run.Name,
                    run.Status,
                    run.Programs,
                    slice.Slice.ClusterId,
                    slice.Slice.Missing,
                    slice.Slice.Jobs,
                });
            })
            .WithName("ConvertEstateSlice")
            .WithSummary("Start a convert-only run over one cluster's slice.");
    }
}
