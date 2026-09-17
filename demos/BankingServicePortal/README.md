# Modernized Banking Services — demo portal

A running .NET service built from five COBOL function modules in the estate under `source/bd`.
Its purpose is to make a modernization review concrete: instead of reading generated code and
asking whether the behaviour survived, you can call it.

## What it exposes

| Capability | Origin program | Endpoint |
|---|---|---|
| Banking calendar — resolves the applicable banking date past weekends and holidays | `BDSDA23` | `POST /api/bankdate/resolve` |
| Date validation — turns a rejected date into a coded, readable reason | `BDSDA2F` | `POST /api/bankdate/validate` |
| Batch throughput — units handled per minute, with the original job-log line | `BDSM043` | `POST /api/batch/throughput` |
| Database diagnostics — classifies a return code and says whether to retry | `BDSMFJL` | `POST /api/diagnostics/sql` |
| Rate reconciliation — reconciles two rate sets, rejecting fictitious entries | `RGNB649` | `POST /api/rates/reconcile` |

Supporting endpoints: `GET /api/catalog`, `GET /api/health`,
`GET /api/bankdate/current`, `GET /api/bankdate/holidays/{year}`.

The browser UI is built from `/api/catalog`, so a capability that is not served does not appear.

## Running it

```bash
cd demos/BankingServicePortal
dotnet run
```

Then open the URL printed in the console. To pin the port:

```bash
ASPNETCORE_URLS=http://localhost:5210 dotnet run
```

## Testing it

```bash
cd demos/BankingServicePortal.Tests
dotnet test
```

55 tests: domain behaviour (calendar arithmetic across holiday runs, retry classification,
reconciliation of unsorted input) and HTTP-level checks that every advertised endpoint answers,
that rejections carry their reason in the body, and that the sample the UI ships with works.

## What is deliberately not here

**No customer code.** `source/` and `output/` are gitignored and stay that way. This project
contains the *service shape* the estate's modules describe — a banking date, a diagnosed database
error, a throughput reading — implemented cleanly. It does not contain the customer's record
layouts, field names, or business constants, and it is not a substitute for the converted output
the pipeline generates into `output/`.

Two places where the modernised behaviour deliberately differs from the original, both noted in
the code:

- The reconciliation batch required both inputs sorted and one-to-one, which is a property of the
  job that produced them rather than of the data. Matching on the key means an unsorted input
  reconciles correctly instead of silently producing wrong output.
- A zero-length batch run returns a throughput of zero rather than dividing by zero.

The holiday set is the common Western European bank-holiday pattern, not any customer's calendar.
A real deployment would load that from configuration.
