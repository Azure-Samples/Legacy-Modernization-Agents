using System;
using System.Data;
using System.Data.Common;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace Insurance.Tracking
{
    /// <summary>
    /// Represents the input parameters for the tracking operation.
    /// </summary>
    public record TrackingOperationInput(
        string OperationType,
        string ProcessDate,
        string PolicyNumber
    );

    /// <summary>
    /// Represents the output/result of the tracking operation.
    /// </summary>
    public record TrackingOperationResult(
        int SqlCode
    );

    /// <summary>
    /// Represents a tracking record in the INSURNCE.TTRAKING table.
    /// </summary>
    public record TrackingRecord(
        string PolicyNumber,
        string NotifyDate,
        string Status,
        DateTime AddTimestamp,
        DateTime UpdateTimestamp
    );

    /// <summary>
    /// Interface for tracking repository abstraction.
    /// </summary>
    public interface ITrackingRepository
    {
        /// <summary>
        /// Attempts to retrieve a tracking record by policy number.
        /// </summary>
        /// <param name="policyNumber">The policy number to search for.</param>
        /// <returns>The tracking record if found; otherwise, null.</returns>
        Task<TrackingRecord?> GetTrackingRecordAsync(string policyNumber);

        /// <summary>
        /// Inserts a new tracking record.
        /// </summary>
        /// <param name="record">The tracking record to insert.</param>
        /// <returns>The SQLCODE from the operation.</returns>
        Task<int> InsertTrackingRecordAsync(TrackingRecord record);

        /// <summary>
        /// Updates an existing tracking record.
        /// </summary>
        /// <param name="record">The tracking record to update.</param>
        /// <returns>The SQLCODE from the operation.</returns>
        Task<int> UpdateTrackingRecordAsync(TrackingRecord record);
    }

    /// <summary>
    /// Concrete implementation of <see cref="ITrackingRepository"/> using ADO.NET.
    /// </summary>
    public class TrackingRepository : ITrackingRepository
    {
        private readonly DbConnection _connection;
        private readonly ILogger<TrackingRepository> _logger;

        /// <summary>
        /// Initializes a new instance of the <see cref="TrackingRepository"/> class.
        /// </summary>
        /// <param name="connection">The database connection.</param>
        /// <param name="logger">The logger instance.</param>
        public TrackingRepository(DbConnection connection, ILogger<TrackingRepository> logger)
        {
            _connection = connection ?? throw new ArgumentNullException(nameof(connection));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <inheritdoc/>
        public async Task<TrackingRecord?> GetTrackingRecordAsync(string policyNumber)
        {
            const string sql = @"
                SELECT TR_POLICY_NUMBER, TR_NOTIFY_DATE, TR_STATUS, TR_ADD_TIMESTAMP, TR_UPDATE_TIMESTAMP
                FROM INSURNCE.TTRAKING
                WHERE TR_POLICY_NUMBER = @PolicyNumber
            ";

            await using var cmd = _connection.CreateCommand();
            cmd.CommandText = sql;
            cmd.CommandType = CommandType.Text;
            var param = cmd.CreateParameter();
            param.ParameterName = "@PolicyNumber";
            param.DbType = DbType.String;
            param.Value = policyNumber;
            cmd.Parameters.Add(param);

            try
            {
                if (_connection.State != ConnectionState.Open)
                    await _connection.OpenAsync();

                await using var reader = await cmd.ExecuteReaderAsync();
                if (await reader.ReadAsync())
                {
                    return new TrackingRecord(
                        reader.GetString(0),
                        reader.GetString(1),
                        reader.GetString(2),
                        reader.GetDateTime(3),
                        reader.GetDateTime(4)
                    );
                }
                else
                {
                    // SQLCODE 100: Not found
                    return null;
                }
            }
            catch (DbException ex)
            {
                _logger.LogError(ex, "Error selecting TTRACKING for PolicyNumber: {PolicyNumber}", policyNumber);
                throw new DataException("Error selecting TTRACKING record.", ex);
            }
        }

        /// <inheritdoc/>
        public async Task<int> InsertTrackingRecordAsync(TrackingRecord record)
        {
            const string sql = @"
                INSERT INTO INSURNCE.TTRAKING (
                    TR_POLICY_NUMBER,
                    TR_NOTIFY_DATE,
                    TR_STATUS,
                    TR_ADD_TIMESTAMP,
                    TR_UPDATE_TIMESTAMP
                ) VALUES (
                    @PolicyNumber,
                    @NotifyDate,
                    @Status,
                    CURRENT_TIMESTAMP,
                    CURRENT_TIMESTAMP
                )
            ";

            await using var cmd = _connection.CreateCommand();
            cmd.CommandText = sql;
            cmd.CommandType = CommandType.Text;

            cmd.Parameters.Add(CreateParameter(cmd, "@PolicyNumber", DbType.String, record.PolicyNumber));
            cmd.Parameters.Add(CreateParameter(cmd, "@NotifyDate", DbType.String, record.NotifyDate));
            cmd.Parameters.Add(CreateParameter(cmd, "@Status", DbType.String, record.Status));

            try
            {
                if (_connection.State != ConnectionState.Open)
                    await _connection.OpenAsync();

                var affected = await cmd.ExecuteNonQueryAsync();
                // SQLCODE 0: Success, else error
                return affected > 0 ? 0 : -1;
            }
            catch (DbException ex)
            {
                _logger.LogError(ex, "Error inserting into TTRACKING for PolicyNumber: {PolicyNumber}", record.PolicyNumber);
                return ex.ErrorCode != 0 ? ex.ErrorCode : -1;
            }
        }

        /// <inheritdoc/>
        public async Task<int> UpdateTrackingRecordAsync(TrackingRecord record)
        {
            const string sql = @"
                UPDATE INSURNCE.TTRAKING
                SET
                    TR_NOTIFY_DATE = @NotifyDate,
                    TR_STATUS = @Status,
                    TR_UPDATE_TIMESTAMP = CURRENT_TIMESTAMP
                WHERE TR_POLICY_NUMBER = @PolicyNumber
            ";

            await using var cmd = _connection.CreateCommand();
            cmd.CommandText = sql;
            cmd.CommandType = CommandType.Text;

            cmd.Parameters.Add(CreateParameter(cmd, "@NotifyDate", DbType.String, record.NotifyDate));
            cmd.Parameters.Add(CreateParameter(cmd, "@Status", DbType.String, record.Status));
            cmd.Parameters.Add(CreateParameter(cmd, "@PolicyNumber", DbType.String, record.PolicyNumber));

            try
            {
                if (_connection.State != ConnectionState.Open)
                    await _connection.OpenAsync();

                var affected = await cmd.ExecuteNonQueryAsync();
                return affected > 0 ? 0 : -1;
            }
            catch (DbException ex)
            {
                _logger.LogError(ex, "Error updating TTRACKING for PolicyNumber: {PolicyNumber}", record.PolicyNumber);
                return ex.ErrorCode != 0 ? ex.ErrorCode : -1;
            }
        }

        private static DbParameter CreateParameter(DbCommand cmd, string name, DbType type, object value)
        {
            var param = cmd.CreateParameter();
            param.ParameterName = name;
            param.DbType = type;
            param.Value = value ?? DBNull.Value;
            return param;
        }
    }

    /// <summary>
    /// Service responsible for processing tracking operations (insert/update).
    /// </summary>
    public class TrackingService
    {
        private readonly ITrackingRepository _repository;
        private readonly ILogger<TrackingService> _logger;

        /// <summary>
        /// Initializes a new instance of the <see cref="TrackingService"/> class.
        /// </summary>
        /// <param name="repository">The tracking repository.</param>
        /// <param name="logger">The logger instance.</param>
        public TrackingService(ITrackingRepository repository, ILogger<TrackingService> logger)
        {
            _repository = repository ?? throw new ArgumentNullException(nameof(repository));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Processes the tracking operation based on the input parameters.
        /// </summary>
        /// <param name="input">The input parameters for the operation.</param>
        /// <returns>The result of the operation, including the SQLCODE.</returns>
        public async Task<TrackingOperationResult> ProcessTrackingOperationAsync(TrackingOperationInput input)
        {
            if (input is null)
                throw new ArgumentNullException(nameof(input));

            int sqlCode = 0;

            try
            {
                // Normalize operation type
                var operationType = input.OperationType?.Trim().ToUpperInvariant();

                switch (operationType)
                {
                    case "INSERT":
                    case "UPDATE":
                        sqlCode = await InsertOrUpdateTrackingAsync(input, operationType);
                        break;

                    default:
                        _logger.LogWarning("Invalid operation type: {OperationType}", input.OperationType);
                        sqlCode = -1;
                        break;
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Unhandled exception in ProcessTrackingOperationAsync");
                sqlCode = -99999; // Arbitrary error code for unhandled exceptions
            }

            return new TrackingOperationResult(sqlCode);
        }

        /// <summary>
        /// Handles the logic for inserting or updating a tracking record.
        /// </summary>
        /// <param name="input">The input parameters.</param>
        /// <param name="operationType">The normalized operation type.</param>
        /// <returns>The SQLCODE from the operation.</returns>
        private async Task<int> InsertOrUpdateTrackingAsync(TrackingOperationInput input, string operationType)
        {
            // Step 1: Check if record exists
            var existingRecord = await _repository.GetTrackingRecordAsync(input.PolicyNumber);

            // Step 2: Populate tracking record fields
            var trackingRecord = new TrackingRecord(
                PolicyNumber: input.PolicyNumber,
                NotifyDate: input.ProcessDate,
                Status: "A",
                AddTimestamp: DateTime.UtcNow, // Will be set by DB
                UpdateTimestamp: DateTime.UtcNow // Will be set by DB
            );

            // Step 3: Insert or Update
            if (existingRecord is null)
            {
                // Not present in tracking: INSERT
                var insertCode = await _repository.InsertTrackingRecordAsync(trackingRecord);
                if (insertCode != 0)
                {
                    _logger.LogError("Error inserting into TTRACKING. SQLCODE: {SqlCode}", insertCode);
                }
                return insertCode;
            }
            else
            {
                // Present in tracking: UPDATE
                var updateCode = await _repository.UpdateTrackingRecordAsync(trackingRecord);
                if (updateCode != 0)
                {
                    _logger.LogError("Error updating TTRACKING. SQLCODE: {SqlCode}", updateCode);
                }
                return updateCode;
            }
        }
    }

    /// <summary>
    /// Example of how to wire up the service and repository using dependency injection.
    /// </summary>
    public static class ServiceRegistration
    {
        /// <summary>
        /// Registers tracking services and repository in the DI container.
        /// </summary>
        /// <param name="services">The service collection.</param>
        /// <param name="connectionFactory">A factory for creating DbConnection instances.</param>
        public static void AddTrackingServices(this IServiceCollection services, Func<IServiceProvider, DbConnection> connectionFactory)
        {
            services.AddScoped<ITrackingRepository>(provider =>
            {
                var connection = connectionFactory(provider);
                var logger = provider.GetRequiredService<ILogger<TrackingRepository>>();
                return new TrackingRepository(connection, logger);
            });

            services.AddScoped<TrackingService>();
        }
    }
}