// Turns a database return code into a diagnosis.
//
// Modelled on the estate's shared error-handling module, which every program called after a
// database operation to decide whether the failure was fatal, expected, or worth retrying. That
// decision was duplicated as inline code in several programs; expressing it once is most of the
// value of modernising it.

namespace Modernized.Banking.Portal.Services;

using Modernized.Banking.Portal.Domain;

public sealed class SqlDiagnostics : ISqlDiagnostics
{
    public SqlDiagnosticResult Diagnose(SqlDiagnosticRequest request)
    {
        var state = string.IsNullOrWhiteSpace(request.SqlState)
            ? DeriveState(request.SqlCode)
            : request.SqlState.Trim().ToUpperInvariant();

        return request.SqlCode switch
        {
            0 when request.RowsAffected == 0 => Build(request, state, "Info", "Success, no rows",
                "The statement succeeded but matched no rows.",
                "Confirm the selection criteria if rows were expected.", false),

            0 => Build(request, state, "Info", "Success",
                $"The statement succeeded and affected {request.RowsAffected} row(s).",
                "No action required.", false),

            100 => Build(request, state, "Info", "Not found",
                "No row satisfied the request; the cursor reached the end of the result set.",
                "Treat as an expected end-of-data condition, not an error.", false),

            -803 => Build(request, state, "Error", "Duplicate key",
                "The insert or update would have created a duplicate value in a unique index.",
                "Read the existing row and decide between update and skip.", false),

            -904 => Build(request, state, "Error", "Resource unavailable",
                "A required table space, index or other resource was unavailable.",
                "Retry after a short delay; escalate if it persists.", true),

            -911 or -913 => Build(request, state, "Warning", "Deadlock or timeout",
                "The unit of work was rolled back after a deadlock or lock timeout.",
                "Retry the whole unit of work; it is expected under contention.", true),

            -180 or -181 => Build(request, state, "Error", "Invalid datetime",
                "A date or timestamp value was not valid in the format supplied.",
                "Validate the date before binding it into the statement.", false),

            -407 => Build(request, state, "Error", "Null into non-nullable",
                "A null was supplied for a column defined as NOT NULL.",
                "Supply a value or make the column nullable.", false),

            -502 => Build(request, state, "Error", "Cursor already open",
                "An OPEN was issued against a cursor that was already open.",
                "Close the cursor before reopening it.", false),

            -805 => Build(request, state, "Error", "Package not found",
                "The package or plan needed by the program was not found in the catalogue.",
                "Rebind the package for this environment.", false),

            < 0 => Build(request, state, "Error", "Unhandled negative code",
                $"The database reported SQLCODE {request.SqlCode}, which this service does not classify.",
                "Consult the platform's message reference for this code.", false),

            _ => Build(request, state, "Warning", "Unhandled positive code",
                $"The database reported SQLCODE {request.SqlCode}, a warning this service does not classify.",
                "Review whether the warning is acceptable for this operation.", false),
        };
    }

    private static SqlDiagnosticResult Build(
        SqlDiagnosticRequest request, string state, string severity, string condition,
        string explanation, string action, bool retryable)
    {
        if (!string.IsNullOrWhiteSpace(request.Statement))
            explanation += $" Statement: {request.Statement.Trim()}.";

        return new SqlDiagnosticResult(
            request.SqlCode, state, severity, condition, explanation, action, retryable);
    }

    private static string DeriveState(int sqlCode) => sqlCode switch
    {
        0 => "00000",
        100 => "02000",
        -803 => "23505",
        -904 => "57011",
        -911 or -913 => "40001",
        -180 or -181 => "22007",
        -407 => "23502",
        -502 => "24502",
        -805 => "51002",
        _ => "HY000",
    };
}
