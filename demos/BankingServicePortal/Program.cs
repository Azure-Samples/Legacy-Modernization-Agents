// The converted banking service, running.
//
// Five function modules from the COBOL estate are exposed as HTTP endpoints so the modernised
// behaviour can be exercised directly rather than described. Every capability names the program
// it came from, which is what makes this useful in a review: the question is always "is this
// still the same behaviour", and that needs the origin to be visible.

using Modernized.Banking.Portal.Domain;
using Modernized.Banking.Portal.Services;

var builder = WebApplication.CreateBuilder(args);

// Enums cross the wire by name. As numbers the browser receives `1` for a classification and a
// severity, which it cannot label, colour or explain — and the value silently changes meaning if
// a member is ever inserted.
builder.Services.ConfigureHttpJsonOptions(options =>
    options.SerializerOptions.Converters.Add(
        new System.Text.Json.Serialization.JsonStringEnumConverter()));

builder.Services.AddSingleton<IBankingCalendar, BankingCalendar>();
builder.Services.AddSingleton<IThroughputReporter, ThroughputReporter>();
builder.Services.AddSingleton<ISqlDiagnostics, SqlDiagnostics>();
builder.Services.AddSingleton<IRateReconciler, RateReconciler>();

builder.Services.AddCors(options => options.AddDefaultPolicy(policy =>
    policy.AllowAnyOrigin().AllowAnyMethod().AllowAnyHeader()));

var app = builder.Build();

app.UseCors();
app.UseDefaultFiles();
app.UseStaticFiles();

// ── Catalogue ───────────────────────────────────────────────────────────────────────────────

var catalogue = new CapabilityOrigin[]
{
    new("Banking calendar", "BDSDA23", "source/bd/BDSDA23.cbl",
        "Resolves the applicable banking date, shifting past weekends and holidays.",
        "/api/bankdate/resolve"),
    new("Date validation", "BDSDA2F", "source/bd/BDSDA2F.cbl",
        "Turns a rejected date into a coded, readable explanation.",
        "/api/bankdate/validate"),
    new("Batch throughput", "BDSM043", "source/bd/BDSM043.cbl",
        "Reports how many logical units a batch run handles per minute.",
        "/api/batch/throughput"),
    new("Database diagnostics", "BDSMFJL", "source/bd/BDSMFJL.cbl",
        "Classifies a database return code and says whether it is worth retrying.",
        "/api/diagnostics/sql"),
    new("Rate reconciliation", "RGNB649", "source/bd/RGNB649.cbl",
        "Reconciles two sets of interest rates, rejecting fictitious and out-of-category entries.",
        "/api/rates/reconcile"),
};

app.MapGet("/api/catalog", () => Results.Ok(catalogue))
   .WithName("Catalog");

app.MapGet("/api/health", () => Results.Ok(new
{
    status = "healthy",
    service = "Modernized.Banking.Portal",
    capabilities = catalogue.Length,
    utc = DateTime.UtcNow,
}));

// ── Banking calendar ────────────────────────────────────────────────────────────────────────

app.MapPost("/api/bankdate/resolve", (BankingDateRequest request, IBankingCalendar calendar) =>
{
    var asOf = request.AsOf ?? DateOnly.FromDateTime(DateTime.UtcNow);
    return Results.Ok(calendar.Resolve(asOf, request.OffsetDays));
});

app.MapGet("/api/bankdate/current", (IBankingCalendar calendar) =>
    Results.Ok(calendar.Resolve(DateOnly.FromDateTime(DateTime.UtcNow), 0)));

app.MapPost("/api/bankdate/validate", (DateValidationRequest request, IBankingCalendar calendar) =>
{
    var result = calendar.Validate(request.Value);
    return result.IsValid ? Results.Ok(result) : Results.BadRequest(result);
});

app.MapGet("/api/bankdate/holidays/{year:int}", (int year, IBankingCalendar calendar) =>
    year is < 1900 or > 2200
        ? Results.BadRequest(new { message = $"Year {year} is outside the supported range 1900-2200." })
        : Results.Ok(calendar.HolidaysIn(year)));

// ── Batch throughput ────────────────────────────────────────────────────────────────────────

app.MapPost("/api/batch/throughput", (ThroughputRequest request, IThroughputReporter reporter) =>
    request.ElapsedSeconds < 0
        ? Results.BadRequest(new { message = "Elapsed seconds cannot be negative." })
        : Results.Ok(reporter.Measure(request)));

// ── Database diagnostics ────────────────────────────────────────────────────────────────────

app.MapPost("/api/diagnostics/sql", (SqlDiagnosticRequest request, ISqlDiagnostics diagnostics) =>
    Results.Ok(diagnostics.Diagnose(request)));

// ── Rate reconciliation ─────────────────────────────────────────────────────────────────────

app.MapPost("/api/rates/reconcile", (ReconcileRequest request, IRateReconciler reconciler) =>
{
    if (request.Primary is null || request.Secondary is null)
        return Results.BadRequest(new { message = "Both primary and secondary rate sets are required." });

    return Results.Ok(reconciler.Reconcile(request));
});

app.Run();

/// <summary>Exposed so the test host can reference this assembly.</summary>
public partial class Program;
