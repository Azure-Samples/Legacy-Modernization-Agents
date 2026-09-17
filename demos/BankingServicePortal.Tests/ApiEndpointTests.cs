using System.Net;
using System.Net.Http.Json;
using System.Text.Json;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;
using Xunit;

namespace Modernized.Banking.Portal.Tests;

// Exercises the service over HTTP rather than through the classes directly. The domain tests can
// all pass while the app still fails to serve anything: routing, binding, serialisation and the
// static UI are separate ways for this to be broken in a demo.
public class ApiEndpointTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly HttpClient _client;

    private static readonly JsonSerializerOptions Json =
        new(JsonSerializerDefaults.Web);

    public ApiEndpointTests(WebApplicationFactory<Program> factory) =>
        _client = factory.CreateClient();

    private async Task<JsonElement> GetJson(string path)
    {
        var resp = await _client.GetAsync(path);
        resp.EnsureSuccessStatusCode();
        return JsonSerializer.Deserialize<JsonElement>(await resp.Content.ReadAsStringAsync(), Json);
    }

    private async Task<(HttpStatusCode Status, JsonElement Body)> PostJson(string path, object payload)
    {
        var resp = await _client.PostAsJsonAsync(path, payload, Json);
        var text = await resp.Content.ReadAsStringAsync();
        var body = string.IsNullOrWhiteSpace(text)
            ? default
            : JsonSerializer.Deserialize<JsonElement>(text, Json);
        return (resp.StatusCode, body);
    }

    // ── Plumbing ────────────────────────────────────────────────────────

    [Fact]
    public async Task TheServiceReportsItselfHealthy()
    {
        var body = await GetJson("/api/health");

        body.GetProperty("status").GetString().Should().Be("healthy");
        body.GetProperty("capabilities").GetInt32().Should().Be(5);
    }

    [Fact]
    public async Task EveryCatalogueEntryNamesTheProgramItCameFrom()
    {
        var body = await GetJson("/api/catalog");

        body.GetArrayLength().Should().Be(5);
        foreach (var entry in body.EnumerateArray())
        {
            entry.GetProperty("program").GetString().Should().NotBeNullOrWhiteSpace();
            entry.GetProperty("sourcePath").GetString().Should().StartWith("source/bd/");
            entry.GetProperty("endpoint").GetString().Should().StartWith("/api/");
        }
    }

    // Every endpoint the catalogue advertises must actually answer, or the UI built from it
    // will offer the user a capability that 404s.
    [Fact]
    public async Task EveryAdvertisedEndpointExists()
    {
        var catalog = await GetJson("/api/catalog");

        var endpoints = catalog.EnumerateArray()
            .Select(entry => entry.GetProperty("endpoint").GetString()!)
            .ToList();

        foreach (var endpoint in endpoints)
        {
            var resp = await _client.PostAsJsonAsync(endpoint, new { }, Json);
            resp.StatusCode.Should().NotBe(HttpStatusCode.NotFound,
                $"{endpoint} is advertised in the catalogue");
        }
    }

    [Fact]
    public async Task TheUserInterfaceIsServed()
    {
        var resp = await _client.GetAsync("/");

        resp.StatusCode.Should().Be(HttpStatusCode.OK);
        resp.Content.Headers.ContentType!.MediaType.Should().Be("text/html");
        (await resp.Content.ReadAsStringAsync()).Should().Contain("Modernized Banking Services");
    }

    [Theory]
    [InlineData("/app.js")]
    [InlineData("/styles.css")]
    public async Task TheUserInterfaceAssetsAreServed(string path)
    {
        (await _client.GetAsync(path)).StatusCode.Should().Be(HttpStatusCode.OK);
    }

    // ── Banking calendar ────────────────────────────────────────────────

    [Fact]
    public async Task ResolvingABankingDateReturnsTheShiftedDay()
    {
        var (status, body) = await PostJson("/api/bankdate/resolve",
            new { asOf = "2026-09-19", offsetDays = 0 });

        status.Should().Be(HttpStatusCode.OK);
        body.GetProperty("bankingDate").GetString().Should().Be("2026-09-21");
        body.GetProperty("classification").GetString().Should().Be("Weekend");
    }

    [Fact]
    public async Task ResolvingWithNoDateUsesToday()
    {
        var (status, body) = await PostJson("/api/bankdate/resolve", new { asOf = (string?)null, offsetDays = 0 });

        status.Should().Be(HttpStatusCode.OK);
        body.GetProperty("requested").GetString()
            .Should().Be(DateTime.UtcNow.ToString("yyyy-MM-dd"));
    }

    [Fact]
    public async Task TheCurrentBankingDateIsAvailableWithoutABody()
    {
        var body = await GetJson("/api/bankdate/current");

        body.TryGetProperty("bankingDate", out _).Should().BeTrue();
    }

    [Fact]
    public async Task AValidDateIsAccepted()
    {
        var (status, body) = await PostJson("/api/bankdate/validate", new { value = "20260916" });

        status.Should().Be(HttpStatusCode.OK);
        body.GetProperty("isValid").GetBoolean().Should().BeTrue();
    }

    // The UI renders the rejection reason, so the code and message must survive the 400.
    [Fact]
    public async Task AnInvalidDateIsRejectedWithItsReasonInTheBody()
    {
        var (status, body) = await PostJson("/api/bankdate/validate", new { value = "nonsense" });

        status.Should().Be(HttpStatusCode.BadRequest);
        body.GetProperty("code").GetString().Should().Be("DATE_FORMAT");
        body.GetProperty("message").GetString().Should().NotBeNullOrWhiteSpace();
    }

    [Fact]
    public async Task HolidaysAreListedForAYear()
    {
        var body = await GetJson("/api/bankdate/holidays/2026");

        body.GetArrayLength().Should().BeGreaterThan(5);
    }

    [Fact]
    public async Task AYearOutsideTheSupportedRangeIsRefused()
    {
        var resp = await _client.GetAsync("/api/bankdate/holidays/1500");

        resp.StatusCode.Should().Be(HttpStatusCode.BadRequest);
    }

    // ── Throughput ──────────────────────────────────────────────────────

    [Fact]
    public async Task ThroughputIsCalculatedOverHttp()
    {
        var (status, body) = await PostJson("/api/batch/throughput",
            new { label = "POSTING", unitsProcessed = 6000, elapsedSeconds = 60 });

        status.Should().Be(HttpStatusCode.OK);
        body.GetProperty("unitsPerMinute").GetDouble().Should().Be(6000);
        body.GetProperty("displayLine").GetString().Should().Contain("POSTING");
    }

    [Fact]
    public async Task ANegativeElapsedTimeIsRefused()
    {
        var (status, _) = await PostJson("/api/batch/throughput",
            new { label = "X", unitsProcessed = 1, elapsedSeconds = -5 });

        status.Should().Be(HttpStatusCode.BadRequest);
    }

    // ── Diagnostics ─────────────────────────────────────────────────────

    [Fact]
    public async Task ADeadlockIsDiagnosedAsRetryable()
    {
        var (status, body) = await PostJson("/api/diagnostics/sql", new { sqlCode = -911 });

        status.Should().Be(HttpStatusCode.OK);
        body.GetProperty("isRetryable").GetBoolean().Should().BeTrue();
        body.GetProperty("sqlState").GetString().Should().Be("40001");
    }

    [Fact]
    public async Task ADuplicateKeyIsDiagnosedAsAFault()
    {
        var (_, body) = await PostJson("/api/diagnostics/sql", new { sqlCode = -803 });

        body.GetProperty("isRetryable").GetBoolean().Should().BeFalse();
        body.GetProperty("condition").GetString().Should().Be("Duplicate key");
    }

    // ── Reconciliation ──────────────────────────────────────────────────

    [Fact]
    public async Task RatesAreReconciledOverHttp()
    {
        var (status, body) = await PostJson("/api/rates/reconcile", new
        {
            primary = new[]
            {
                new { key = "K1", rate = 2.25m, category = "KRD", fictitious = false },
                new { key = "K2", rate = 3.10m, category = "KRD", fictitious = false },
            },
            secondary = new[]
            {
                new { key = "K1", rate = 2.25m, category = "KRD", fictitious = false },
                new { key = "K2", rate = 3.15m, category = "KRD", fictitious = false },
            },
            requiredCategory = "KRD",
        });

        status.Should().Be(HttpStatusCode.OK);
        body.GetProperty("matched").GetArrayLength().Should().Be(1);
        body.GetProperty("differing").GetArrayLength().Should().Be(1);
    }

    [Fact]
    public async Task ReconcilingWithoutBothSetsIsRefused()
    {
        var (status, _) = await PostJson("/api/rates/reconcile",
            new { primary = (object?)null, secondary = (object?)null, requiredCategory = "KRD" });

        status.Should().Be(HttpStatusCode.BadRequest);
    }

    // The sample the UI ships must actually work, or the first click of the demo fails.
    [Fact]
    public async Task TheSampleTheInterfaceShipsWithProducesEveryOutcome()
    {
        var (status, body) = await PostJson("/api/rates/reconcile", new
        {
            primary = new object[]
            {
                new { key = "FI01-0001", rate = 2.25m, category = "KRD", fictitious = false },
                new { key = "FI01-0002", rate = 3.10m, category = "KRD", fictitious = false },
                new { key = "FI01-0003", rate = 1.75m, category = "KRD", fictitious = false },
                new { key = "FI01-0004", rate = 4.00m, category = "KRD", fictitious = false },
                new { key = "FI01-0005", rate = 9.99m, category = "KRD", fictitious = true },
                new { key = "FI01-0006", rate = 2.50m, category = "DEP", fictitious = false },
            },
            secondary = new object[]
            {
                new { key = "FI01-0001", rate = 2.25m, category = "KRD", fictitious = false },
                new { key = "FI01-0002", rate = 3.15m, category = "KRD", fictitious = false },
                new { key = "FI01-0003", rate = 1.75m, category = "KRD", fictitious = false },
            },
            requiredCategory = "KRD",
        });

        status.Should().Be(HttpStatusCode.OK);
        body.GetProperty("matched").GetArrayLength().Should().Be(2);
        body.GetProperty("differing").GetArrayLength().Should().Be(1);
        body.GetProperty("missingCounterpart").GetArrayLength().Should().Be(1);
        body.GetProperty("rejected").GetArrayLength().Should().Be(2);
    }
}
