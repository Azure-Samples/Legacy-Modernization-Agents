using System;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;

namespace Insurance.Tracking
{
    /// <summary>
    /// Represents the input and output parameters for the tracking operation.
    /// </summary>
    public interface ILinkArea
    {
        /// <summary>
        /// Input parameters.
        /// </summary>
        LinkInputArea Input { get; }

        /// <summary>
        /// Output parameters.
        /// </summary>
        LinkOutputArea Output { get; }
    }

    /// <summary>
    /// Concrete implementation of ILinkArea.
    /// </summary>
    public class LinkArea : ILinkArea
    {
        public LinkInputArea Input { get; init; } = new();
        public LinkOutputArea Output { get; init; } = new();
    }

    /// <summary>
    /// Input parameters for the tracking operation.
    /// </summary>
    public record LinkInputArea
    {
        public string OperationType { get; init; } = string.Empty; // 'INSERT' or 'UPDATE'
        public string ProcessDate { get; init; } = string.Empty;   // Format: 'YYYY-MM-DD'
        public string PolicyNumber { get; init; } = string.Empty;
    }

    /// <summary>
    /// Output parameters for the tracking operation.
    /// </summary>
    public record LinkOutputArea
    {
        public int SqlCode { get; set; }
    }

    /// <summary>
    /// Represents a record in the INSURNCE.TTRAKING table.
    /// </summary>
    public record TrackingRecord
    {
        public string PolicyNumber { get; init; } = string.Empty;
        public string NotifyDate { get; init; } = string.Empty;
        public string Status { get; init; } = "A";
        public DateTime AddTimestamp { get; init; }
        public DateTime UpdateTimestamp { get; init; }
    }

    /// <summary>
    /// Abstraction for tracking repository operations.
    /// </summary>
    public interface ITrackingRepository
    {
        /// <summary>
        /// Gets a tracking record by policy number.
        /// </summary>
        Task<TrackingRecord?> GetTrackingRecordAsync(string policyNumber);

        /// <summary>
        /// Inserts a new tracking record.
        /// </summary>
        Task<int> InsertTrackingRecordAsync(TrackingRecord record);

        /// <summary>
        /// Updates an existing tracking record.
        /// </summary>
        Task<int> UpdateTrackingRecordAsync(TrackingRecord record);
    }

    /// <summary>
    /// Implements the main logic for inserting or updating tracking records.
    /// </summary>
    public class TrackingService
    {
        private readonly ITrackingRepository _repository;
        private readonly ILogger<TrackingService> _logger;

        /// <summary>
        /// Initializes a new instance of <see cref="TrackingService"/>.
        /// </summary>
        /// <param name="repository">Tracking repository.</param>
        /// <param name="logger">Logger instance.</param>
        public TrackingService(ITrackingRepository repository, ILogger<TrackingService> logger)
        {
            _repository = repository ?? throw new ArgumentNullException(nameof(repository));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Processes the tracking operation based on the input parameters.
        /// </summary>
        /// <param name="linkArea">Input/output area.</param>
        /// <returns>Task representing the asynchronous operation.</returns>
        public async Task ProcessAsync(ILinkArea linkArea)
        {
            if (linkArea == null) throw new ArgumentNullException(nameof(linkArea));

            var operationType = linkArea.Input.OperationType?.Trim().ToUpperInvariant();
            var processDate = linkArea.Input.ProcessDate?.Trim();
            var policyNumber = linkArea.Input.PolicyNumber?.Trim();

            int sqlCode = 0;

            try
            {
                switch (operationType)
                {
                    case "INSERT":
                    case "UPDATE":
                        sqlCode = await InsertOrUpdateTrackingAsync(policyNumber, processDate);
                        break;

                    default:
                        _logger.LogError("Invalid operation type: {OperationType}", operationType);
                        sqlCode = -1;
                        break;
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Exception during tracking operation for policy {PolicyNumber}", policyNumber);
                sqlCode = -99999; // Custom error code for unexpected exceptions
            }

            linkArea.Output.SqlCode = sqlCode;
        }

        /// <summary>
        /// Inserts or updates a tracking record for the given policy number.
        /// </summary>
        /// <param name="policyNumber">Policy number.</param>
        /// <param name="processDate">Process date.</param>
        /// <returns>SQLCODE: 0 for success, non-zero for DB error.</returns>
        private async Task<int> InsertOrUpdateTrackingAsync(string policyNumber, string processDate)
        {
            // Step 1: Check if record exists
            TrackingRecord? existingRecord = null;
            int sqlCode = 0;

            try
            {
                existingRecord = await _repository.GetTrackingRecordAsync(policyNumber);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error selecting TTRAKING for policy {PolicyNumber}", policyNumber);
                return -2; // Custom error code for select failure
            }

            // Step 2: Populate record
            var record = new TrackingRecord
            {
                PolicyNumber = policyNumber,
                NotifyDate = processDate,
                Status = "A",
                AddTimestamp = DateTime.UtcNow,
                UpdateTimestamp = DateTime.UtcNow
            };

            // Step 3: Insert or Update
            if (existingRecord is null)
            {
                try
                {
                    sqlCode = await _repository.InsertTrackingRecordAsync(record);
                    if (sqlCode != 0)
                    {
                        _logger.LogError("Error inserting into TTRAKING. SQLCODE: {SqlCode}", sqlCode);
                        return sqlCode;
                    }
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex, "Exception inserting into TTRAKING for policy {PolicyNumber}", policyNumber);
                    return -3; // Custom error code for insert failure
                }
            }
            else
            {
                try
                {
                    sqlCode = await _repository.UpdateTrackingRecordAsync(record);
                    if (sqlCode != 0)
                    {
                        _logger.LogError("Error updating TTRAKING. SQLCODE: {SqlCode}", sqlCode);
                        return sqlCode;
                    }
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex, "Exception updating TTRAKING for policy {PolicyNumber}", policyNumber);
                    return -4; // Custom error code for update failure
                }
            }

            return 0; // Success
        }
    }

    // Example repository implementation (for illustration; replace with actual DB code)
    public class TrackingRepository : ITrackingRepository
    {
        // Inject your DbContext or IDbConnection here

        public async Task<TrackingRecord?> GetTrackingRecordAsync(string policyNumber)
        {
            // Replace with actual DB SELECT logic
            // Return null if not found, or TrackingRecord if found
            await Task.Delay(10); // Simulate async DB call
            return null;
        }

        public async Task<int> InsertTrackingRecordAsync(TrackingRecord record)
        {
            // Replace with actual DB INSERT logic
            await Task.Delay(10); // Simulate async DB call
            return 0; // 0 = success
        }

        public async Task<int> UpdateTrackingRecordAsync(TrackingRecord record)
        {
            // Replace with actual DB UPDATE logic
            await Task.Delay(10); // Simulate async DB call
            return 0; // 0 = success
        }
    }
}