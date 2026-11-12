using McpChatWeb.Services;

namespace McpChatWeb.Endpoints;

public static class AiLoopEndpoints
{
    public static void MapAiLoopEndpoints(this WebApplication app)
    {
        var group = app.MapGroup("/api/ai-loop").WithTags("AI Loop");
        group.AddEndpointFilter(async (context, next) =>
        {
            context.HttpContext.Response.Headers.CacheControl = "no-store";
            return await next(context);
        });

        group.MapGet("/runs", async (int? offset, AiLoopReader reader, HttpContext context, CancellationToken ct) =>
            {
                var skip = Math.Max(0, offset ?? 0);
                var size = reader.PageSize;
                var manager = context.RequestServices.GetService<ProcessManager>();
                var managedPage = manager?.GetAllRuns().OrderByDescending(r => r.CompletedAt ?? r.StartedAt)
                    .Skip(skip).Take(size + 1).ToList() ?? [];
                var managedRuns = managedPage.Take(size)
                    .Select(r => new
                    {
                        runId = r.RunId,
                        activityKind = "managed-launch",
                        activityId = "managed:" + r.RunId,
                        lastEventAt = r.CompletedAt ?? r.StartedAt,
                        r.Name,
                        r.Status,
                        r.TargetLanguage,
                        r.DependencyMode,
                        r.StartedAt,
                        r.CompletedAt,
                        r.ExitCode,
                        r.Error,
                        r.OutputFolder,
                        r.HistoryWarning
                    }).ToArray() ?? [];
                var page = await reader.ListRunsAsync(ct, skip, size + 1);
                var runs = page.Take(size).Select(r =>
                    WithProcessSummary(r, r.ManagedRunId is { } id ? manager?.GetRun(id) : null)).ToList();
                return Results.Ok(new
                {
                    runs,
                    managedRuns,
                    pollSeconds = reader.PollSeconds,
                    nextOffset = skip + size,
                    hasMore = page.Count > size || managedPage.Count > size
                });
            })
            .WithName("ListAiLoopRuns")
            .WithSummary("Recorded native REKT activity, AI Loop attempts and managed launches, including launches without telemetry.");

        group.MapGet("/managed/{managedRunId}", async (string managedRunId, AiLoopReader reader, HttpContext context, CancellationToken ct) =>
            {
                if (!AiLoopReader.IsValidRunId(managedRunId))
                    return Results.BadRequest(new { error = "Invalid managed run id." });
                var detail = await reader.GetManagedRunAsync(managedRunId, ct);
                var process = context.RequestServices.GetService<ProcessManager>()?.GetRun(managedRunId);
                detail = WithProcessStatus(detail, process);
                return detail is null
                    ? Results.NotFound(new { error = "No AI Loop attempt recorded for this managed run yet." })
                    : Results.Ok(detail);
            })
            .WithName("GetManagedAiLoopRun")
            .WithSummary("Resolve a launched process to its explicitly associated AI Loop attempt; missing telemetry is not success.");

        group.MapGet("/{runId}/native-log", async (string runId, AiLoopReader reader, CancellationToken ct) =>
            {
                var result = await reader.ReadNativeLogAsync(runId, ct);
                return result.Log is { } log ? Results.Ok(log)
                    : Results.Json(new { error = result.Error }, statusCode: result.StatusCode);
            })
            .WithSummary("Bounded read-only tail of the selected native manifest's original native.log; no arbitrary paths.");

        group.MapGet("/{runId}", async (string runId, string? attemptId, int? attemptIndex, AiLoopReader reader, HttpContext context, CancellationToken ct) =>
            {
                if (!AiLoopReader.IsValidRunId(runId) || (attemptId is not null && !AiLoopReader.IsValidRunId(attemptId))
                    || attemptIndex < 0)
                    return Results.BadRequest(new { error = "Invalid run id." });
                var detail = await reader.GetRunAsync(runId, ct, attemptId, attemptIndex);
                if (detail?.Summary.ManagedRunId is { } managedId)
                    detail = WithProcessStatus(detail, context.RequestServices.GetService<ProcessManager>()?.GetRun(managedId));
                return detail is null ? Results.NotFound(new { error = $"No metrics for run '{runId}'." }) : Results.Ok(detail);
            })
            .WithName("GetAiLoopRun")
            .WithSummary("Stages, per-agent model calls, retries, fallbacks and quality gates for one run.");
    }

    private static AiLoopRunDetail? WithProcessStatus(AiLoopRunDetail? detail, ManagedRun? process)
    {
        if (detail is null) return null;
        var summary = WithProcessSummary(detail.Summary, process);
        if (ReferenceEquals(summary, detail.Summary)) return detail;
        var active = summary.Status is "running" or "paused";

        return detail with
        {
            Summary = summary,
            Requests = detail.Requests.Select(r => r with { Status = active
                ? summary.Status == "paused" ? "paused" : "in-flight" : "unknown" }).ToList(),
            Gates = active ? detail.Gates : detail.Gates.Select(g => g.Id == "model" ? g with
            {
                Status = "warn",
                Headline = "The process ended without a recorded attempt finish",
            } : g).ToList(),
        };
    }

    private static AiLoopRunSummary WithProcessSummary(AiLoopRunSummary summary, ManagedRun? process)
    {
        // Silence is not termination when the explicitly associated process is still tracked alive.
        // Do not revive superseded attempts, database termination or a recorded terminal event.
        if (summary.FinishedAt is null && summary.FailureReason is null
            && summary.Status is "running" or "interrupted"
            && process?.Status is "running" or "paused")
            return summary.Status == process.Status ? summary : summary with { Status = process.Status };
        if (summary.FinishedAt is null && process?.Status == "interrupted")
            return summary with { Status = "interrupted", FailureReason = process.Error };
        if (summary.FinishedAt is not null || process?.CompletedAt is not { } end
            || process.Status is not ("completed" or "partial" or "failed" or "stopped" or "interrupted"))
            return summary;

        // Process exit establishes termination, not successful conversion or passing gates.
        return summary with
        {
            Status = process.Status == "completed" ? "unknown" : process.Status == "stopped" ? "cancelled" : process.Status,
            FailureReason = process.Error,
            FinishedAt = end,
            DurationMs = summary.StartedAt is { } start ? Math.Max(0, (long)(end - start).TotalMilliseconds) : null,
        };
    }
}
