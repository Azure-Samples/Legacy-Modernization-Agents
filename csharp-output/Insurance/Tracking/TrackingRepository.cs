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
    /// Represents the output result of the tracking operation.
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
    /// Interface for tracking repository to abstract database operations.
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
        /// <returns>The SQL code from the operation.</returns>
        Task<int> InsertTrackingRecordAsync(TrackingRecord record);

        /// <summary>
        /// Updates an existing tracking record.
        /// </summary>
        /// <param name="record">The tracking record to update.</param>
        /// <returns>The SQL code from the operation.</returns>
        Task<int> UpdateTrackingRecordAsync(TrackingRecord record);
    }

    /// <summary>
    /// Implements the tracking repository using a DbConnection.
    /// </summary>
    public class TrackingRepository : ITrackingRepository
    {
        private readonly DbConnection _connection;

        /// <summary>
        /// Initializes a new instance of the <see cref="TrackingRepository"/> class.
        /// </summary>
        /// <param name="connection">The database connection.</param>
        public TrackingRepository(DbConnection connection)
        {
            _connection = connection ?? throw new ArgumentNullException(nameof(connection));
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

            return null;
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

            await _connection.OpenAsync();

            try
            {
                var affected = await cmd.ExecuteNonQueryAsync();
                return affected == 1 ? 0 : -1; // 0 for success, -1 for failure
            }
            catch (DbException ex)
            {
                // Map DB exception to SQLCODE if needed
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
                WHERE
                    TR_POLICY_NUMBER = @PolicyNumber
            ";

            await using var cmd = _connection.CreateCommand();
            cmd.CommandText = sql;
            cmd.CommandType = CommandType.Text;

            cmd.Parameters.Add(CreateParameter(cmd, "@NotifyDate", DbType.String, record.NotifyDate));
            cmd.Parameters.Add(CreateParameter(cmd, "@Status", DbType.String, record.Status));
            cmd.Parameters.Add(CreateParameter(cmd, "@PolicyNumber", DbType.String, record.PolicyNumber));

            await _connection.OpenAsync();

            try
            {
                var affected = await cmd.ExecuteNonQueryAsync();
                return affected == 1 ? 0 : -1;
            }
            catch (DbException ex)
            {
                return ex.ErrorCode != 0 ? ex.ErrorCode : -1;
            }
        }

        private static DbParameter CreateParameter(DbCommand cmd, string name, DbType type, object value)
        {
            var param = cmd.CreateParameter();
            param.ParameterName = name;
            param.DbType = type;
            param.Value = value;
            return param;
        }
    }

    /// <summary>
    /// Service for processing insurance tracking operations.
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
        /// Processes the tracking operation (INSERT or UPDATE) for a given policy.
        /// </summary>
        /// <param name="input">The input parameters for the operation.</param>
        /// <returns>The result containing the SQL code.</returns>
        /// <exception cref="ArgumentException">Thrown when the operation type is invalid.</exception>
        public async Task<TrackingOperationResult> ProcessTrackingOperationAsync(TrackingOperationInput input)
        {
            if (input is null)
                throw new ArgumentNullException(nameof(input));

            int sqlCode = 0;

            try
            {
                switch (input.OperationType?.Trim().ToUpperInvariant())
                {
                    case "INSERT":
                    case "UPDATE":
                        sqlCode = await InsertOrUpdateTrackingAsync(input);
                        break;

                    default:
                        _logger.LogError("Invalid operation type: {OperationType}", input.OperationType);
                        sqlCode = -1;
                        break;
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Exception during tracking operation for policy {PolicyNumber}", input.PolicyNumber);
                sqlCode = -1;
            }

            return new TrackingOperationResult(sqlCode);
        }

        /// <summary>
        /// Inserts or updates a tracking record based on its existence.
        /// </summary>
        /// <param name="input">The input parameters.</param>
        /// <returns>The SQL code from the operation.</returns>
        private async Task<int> InsertOrUpdateTrackingAsync(TrackingOperationInput input)
        {
            // Step 1: SELECT tracking record
            TrackingRecord? existingRecord = null;
            try
            {
                existingRecord = await _repository.GetTrackingRecordAsync(input.PolicyNumber);
            }
            catch (DbException ex)
            {
                _logger.LogError(ex, "Error selecting TTRAKING for policy {PolicyNumber}", input.PolicyNumber);
                return ex.ErrorCode != 0 ? ex.ErrorCode : -1;
            }

            // Step 2: Populate tracking record
            var trackingRecord = new TrackingRecord(
                PolicyNumber: input.PolicyNumber,
                NotifyDate: input.ProcessDate,
                Status: "A",
                AddTimestamp: DateTime.UtcNow,
                UpdateTimestamp: DateTime.UtcNow
            );

            // Step 3: INSERT or UPDATE
            if (existingRecord is null)
            {
                // Not present: INSERT
                try
                {
                    var insertCode = await _repository.InsertTrackingRecordAsync(trackingRecord);
                    if (insertCode != 0)
                    {
                        _logger.LogError("Error inserting into TTRAKING for policy {PolicyNumber}, SQLCODE: {SqlCode}", input.PolicyNumber, insertCode);
                    }
                    return insertCode;
                }
                catch (DbException ex)
                {
                    _logger.LogError(ex, "Error inserting into TTRAKING for policy {PolicyNumber}", input.PolicyNumber);
                    return ex.ErrorCode != 0 ? ex.ErrorCode : -1;
                }
            }
            else
            {
                // Present: UPDATE
                try
                {
                    var updateCode = await _repository.UpdateTrackingRecordAsync(trackingRecord);
                    if (updateCode != 0)
                    {
                        _logger.LogError("Error updating TTRAKING for policy {PolicyNumber}, SQLCODE: {SqlCode}", input.PolicyNumber, updateCode);
                    }
                    return updateCode;
                }
                catch (DbException ex)
                {
                    _logger.LogError(ex, "Error updating TTRAKING for policy {PolicyNumber}", input.PolicyNumber);
                    return ex.ErrorCode != 0 ? ex.ErrorCode : -1;
                }
            }
        }
    }

    /// <summary>
    /// Example of how to configure dependency injection for the tracking service.
    /// </summary>
    public static class ServiceCollectionExtensions
    {
        /// <summary>
        /// Adds tracking services to the service collection.
        /// </summary>
        /// <param name="services">The service collection.</param>
        /// <param name="connectionFactory">A factory for creating DbConnection instances.</param>
        /// <returns>The updated service collection.</returns>
        public static IServiceCollection AddTrackingServices(
            this IServiceCollection services,
            Func<DbConnection> connectionFactory)
        {
            services.AddScoped<ITrackingRepository>(provider =>
                new TrackingRepository(connectionFactory()));

            services.AddScoped<TrackingService>();

            return services;
        }
    }
}