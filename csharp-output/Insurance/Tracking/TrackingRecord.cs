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
    public class TrackingRecord
    {
        public string PolicyNumber { get; set; } = string.Empty;
        public string NotifyDate { get; set; } = string.Empty;
        public string Status { get; set; } = string.Empty;
        public DateTime AddTimestamp { get; set; }
        public DateTime UpdateTimestamp { get; set; }
    }

    /// <summary>
    /// Provides data access methods for the INSURNCE.TTRAKING table.
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
        /// <returns>The SQLCODE result.</returns>
        Task<int> InsertTrackingRecordAsync(TrackingRecord record);

        /// <summary>
        /// Updates an existing tracking record.
        /// </summary>
        /// <param name="record">The tracking record to update.</param>
        /// <returns>The SQLCODE result.</returns>
        Task<int> UpdateTrackingRecordAsync(TrackingRecord record);
    }

    /// <summary>
    /// Implements the tracking repository using ADO.NET.
    /// </summary>
    public class TrackingRepository : ITrackingRepository
    {
        private readonly DbConnection _connection;

        public TrackingRepository(DbConnection connection)
        {
            _connection = connection ?? throw new ArgumentNullException(nameof(connection));
        }

        public async Task<TrackingRecord?> GetTrackingRecordAsync(string policyNumber)
        {
            const string sql = @"
                SELECT TR_POLICY_NUMBER, TR_NOTIFY_DATE, TR_STATUS, TR_ADD_TIMESTAMP, TR_UPDATE_TIMESTAMP
                FROM INSURNCE.TTRAKING
                WHERE TR_POLICY_NUMBER = @PolicyNumber
            ";

            using var cmd = _connection.CreateCommand();
            cmd.CommandText = sql;
            cmd.CommandType = CommandType.Text;
            var param = cmd.CreateParameter();
            param.ParameterName = "@PolicyNumber";
            param.DbType = DbType.String;
            param.Value = policyNumber;
            cmd.Parameters.Add(param);

            if (_connection.State != ConnectionState.Open)
                await _connection.OpenAsync();

            using var reader = await cmd.ExecuteReaderAsync();
            if (await reader.ReadAsync())
            {
                return new TrackingRecord
                {
                    PolicyNumber = reader.GetString(0),
                    NotifyDate = reader.GetString(1),
                    Status = reader.GetString(2),
                    AddTimestamp = reader.GetDateTime(3),
                    UpdateTimestamp = reader.GetDateTime(4)
                };
            }
            return null;
        }

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

            using var cmd = _connection.CreateCommand();
            cmd.CommandText = sql;
            cmd.CommandType = CommandType.Text;

            cmd.Parameters.Add(CreateParameter(cmd, "@PolicyNumber", DbType.String, record.PolicyNumber));
            cmd.Parameters.Add(CreateParameter(cmd, "@NotifyDate", DbType.String, record.NotifyDate));
            cmd.Parameters.Add(CreateParameter(cmd, "@Status", DbType.String, record.Status));

            if (_connection.State != ConnectionState.Open)
                await _connection.OpenAsync();

            try
            {
                var affected = await cmd.ExecuteNonQueryAsync();
                return affected == 1 ? 0 : -1; // 0 for success, -1 for failure
            }
            catch (DbException ex)
            {
                // Map DB2 SQLCODE if possible, otherwise -1
                return ex.ErrorCode != 0 ? ex.ErrorCode : -1;
            }
        }

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

            using var cmd = _connection.CreateCommand();
            cmd.CommandText = sql;
            cmd.CommandType = CommandType.Text;

            cmd.Parameters.Add(CreateParameter(cmd, "@NotifyDate", DbType.String, record.NotifyDate));
            cmd.Parameters.Add(CreateParameter(cmd, "@Status", DbType.String, record.Status));
            cmd.Parameters.Add(CreateParameter(cmd, "@PolicyNumber", DbType.String, record.PolicyNumber));

            if (_connection.State != ConnectionState.Open)
                await _connection.OpenAsync();

            try
            {
                var affected = await cmd.ExecuteNonQueryAsync();
                return affected == 1 ? 0 : -1; // 0 for success, -1 for failure
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
            param.Value = value ?? DBNull.Value;
            return param;
        }
    }

    /// <summary>
    /// Service for processing insurance tracking operations (insert/update).
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
        /// Processes the tracking operation (insert or update) based on the input parameters.
        /// </summary>
        /// <param name="input">The input parameters for the operation.</param>
        /// <returns>The result containing the SQLCODE.</returns>
        public async Task<TrackingOperationResult> ProcessTrackingOperationAsync(TrackingOperationInput input)
        {
            if (input is null)
                throw new ArgumentNullException(nameof(input));

            int sqlCode = 0;

            try
            {
                // Validate operation type
                var operationType = input.OperationType?.Trim().ToUpperInvariant();
                if (operationType is not ("INSERT" or "UPDATE"))
                {
                    _logger.LogError("Invalid operation type: {OperationType}", input.OperationType);
                    sqlCode = -1;
                    return new TrackingOperationResult(sqlCode);
                }

                // Step 1: SELECT tracking record
                var existingRecord = await _repository.GetTrackingRecordAsync(input.PolicyNumber);

                // Step 2: Populate tracking record
                var trackingRecord = new TrackingRecord
                {
                    PolicyNumber = input.PolicyNumber,
                    NotifyDate = input.ProcessDate,
                    Status = "A"
                };

                // Step 3: INSERT or UPDATE
                if (existingRecord is null)
                {
                    sqlCode = await _repository.InsertTrackingRecordAsync(trackingRecord);
                    if (sqlCode != 0)
                    {
                        _logger.LogError("Error inserting into TTRAKING. SQLCODE: {SqlCode}", sqlCode);
                    }
                }
                else
                {
                    sqlCode = await _repository.UpdateTrackingRecordAsync(trackingRecord);
                    if (sqlCode != 0)
                    {
                        _logger.LogError("Error updating TTRAKING. SQLCODE: {SqlCode}", sqlCode);
                    }
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Exception occurred during tracking operation.");
                sqlCode = -1;
            }

            return new TrackingOperationResult(sqlCode);
        }
    }

    /// <summary>
    /// Example of dependency injection setup and usage.
    /// </summary>
    public static class ServiceRegistration
    {
        /// <summary>
        /// Registers tracking services and repository in the DI container.
        /// </summary>
        /// <param name="services">The service collection.</param>
        /// <param name="connectionFactory">A factory for creating DB connections.</param>
        public static void AddTrackingServices(this IServiceCollection services, Func<DbConnection> connectionFactory)
        {
            services.AddScoped<ITrackingRepository>(provider =>
                new TrackingRepository(connectionFactory()));
            services.AddScoped<TrackingService>();
        }
    }
}