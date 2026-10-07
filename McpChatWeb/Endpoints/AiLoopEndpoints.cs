using McpChatWeb.Services;

namespace McpChatWeb.Endpoints;

public static class AiLoopEndpoints
{
    public static void MapAiLoopEndpoints(this WebApplication app)
    {
        var group = app.MapGroup("/api/ai-loop").WithTags("AI Loop");

        group.MapGet("/runs", async (AiLoopReader reader, CancellationToken ct) =>
                Results.Ok(new { runs = await reader.ListRunsAsync(ct), pollSeconds = reader.PollSeconds }))
            .WithName("ListAiLoopRuns")
            .WithSummary("Runs that recorded AI loop events, newest first.");

        group.MapGet("/{runId}", async (string runId, AiLoopReader reader, CancellationToken ct) =>
            {
                if (!AiLoopReader.IsValidRunId(runId))
                    return Results.BadRequest(new { error = "Invalid run id." });
                var detail = await reader.GetRunAsync(runId, ct);
                return detail is null ? Results.NotFound(new { error = $"No metrics for run '{runId}'." }) : Results.Ok(detail);
            })
            .WithName("GetAiLoopRun")
            .WithSummary("Stages, per-agent model calls, retries, fallbacks and quality gates for one run.");
    }
}
