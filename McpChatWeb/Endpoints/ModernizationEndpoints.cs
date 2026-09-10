using McpChatWeb.Services;

namespace McpChatWeb.Endpoints;

public static class ModernizationEndpoints
{
    public static void MapModernizationEndpoints(this WebApplication app)
    {
        var group = app.MapGroup("/api/modernization")
            .WithTags("Modernization Intelligence");

        group.MapGet("/dependency-health", async (
            ModernizationIntelligenceService service,
            CancellationToken cancellationToken) =>
                Results.Ok(await service.GetDependencyHealthAsync(cancellationToken)))
            .WithName("GetDependencyHealth")
            .WithSummary("Parse fidelity, missing copybooks and blocked programs across the estate.");

        group.MapGet("/topology", async (
            ModernizationIntelligenceService service,
            CancellationToken cancellationToken) =>
                Results.Ok(await service.GetTopologyAsync(cancellationToken)))
            .WithName("GetTopology")
            .WithSummary("Programs and copybooks with their CALL and COPY edges.");

        // Identity may be a source-relative path with separators, not just a basename.
        group.MapGet("/flow/{**identity}", async (
            string identity,
            ModernizationIntelligenceService service,
            CancellationToken cancellationToken) =>
                string.IsNullOrWhiteSpace(identity)
                    ? Results.BadRequest(new { error = "A program identity is required." })
                    : Results.Ok(await service.GetProgramFlowAsync(identity, cancellationToken)))
            .WithName("GetProgramFlow")
            .WithSummary("Procedural flow artifacts available for one program.");

        group.MapGet("/service-chain", async (
            string? job,
            string? program,
            bool? includeUtilities,
            ModernizationIntelligenceService service,
            CancellationToken cancellationToken) =>
                Results.Ok(await service.GetServiceChainAsync(
                    job, program, includeUtilities ?? false, cancellationToken)))
            .WithName("GetServiceChain")
            .WithSummary("JCL to program to copybook chain, with a Mermaid rendering.");

        group.MapGet("/conversion-parity", async (
            ConversionParityReader reader,
            CancellationToken cancellationToken) =>
                Results.Ok(await reader.ReadAsync(cancellationToken)))
            .WithName("GetConversionParity")
            .WithSummary("Structural coverage of generated code against the COBOL it came from.");

        group.MapGet("/program-catalog", async (
            string? q,
            ProgramCatalogService service,
            CancellationToken cancellationToken) =>
        {
            var catalog = await service.BuildCatalogAsync(cancellationToken);
            var matches = service.Search(catalog, q);

            return Results.Ok(new
            {
                sourceRoot = catalog.SourceRoot,
                // The unfiltered total travels with every response so an empty filtered list
                // reads as a narrow query rather than an empty estate.
                totalPrograms = catalog.Programs.Count,
                query = q ?? "",
                programs = matches,
                closureAvailable = catalog.ClosureAvailable,
                closureUnavailableReason = catalog.ClosureUnavailableReason,
                deferredSelectors = catalog.DeferredSelectors,
            });
        })
            .WithName("GetProgramCatalog")
            .WithSummary("Programs selectable for a focused conversion, with closure availability (preview).");
    }
}
