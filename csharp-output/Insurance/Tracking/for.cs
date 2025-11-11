using System;

namespace Insurance.Tracking
{
    /// <summary>
    /// Represents a tracking record for an insurance policy, mapping to the DB2 table INSURNCE.TTRAKING.
    /// </summary>
    /// <remarks>
    /// This record is generated from the COBOL declaration and DB2 table definition.
    /// All properties are non-nullable and correspond to table columns.
    /// </remarks>
    public record TrackingRecord
    {
        /// <summary>
        /// Gets the policy number associated with this tracking record.
        /// Maps to TR_POLICY_NUMBER CHAR(10) NOT NULL.
        /// </summary>
        public string PolicyNumber { get; init; }

        /// <summary>
        /// Gets the notification date for the policy.
        /// Maps to TR_NOTIFY_DATE DATE NOT NULL.
        /// </summary>
        public DateTime NotifyDate { get; init; }

        /// <summary>
        /// Gets the status of the tracking record.
        /// Maps to TR_STATUS CHAR(1) NOT NULL.
        /// </summary>
        public string Status { get; init; }

        /// <summary>
        /// Gets the timestamp when the record was added.
        /// Maps to TR_ADD_TIMESTAMP TIMESTAMP NOT NULL WITH DEFAULT.
        /// </summary>
        public DateTime AddTimestamp { get; init; }

        /// <summary>
        /// Gets the timestamp when the record was last updated.
        /// Maps to TR_UPDATE_TIMESTAMP TIMESTAMP NOT NULL WITH DEFAULT.
        /// </summary>
        public DateTime UpdateTimestamp { get; init; }

        /// <summary>
        /// Initializes a new instance of the <see cref="TrackingRecord"/> record.
        /// </summary>
        /// <param name="policyNumber">The policy number (max 10 characters).</param>
        /// <param name="notifyDate">The notification date.</param>
        /// <param name="status">The status (single character).</param>
        /// <param name="addTimestamp">The timestamp when the record was added.</param>
        /// <param name="updateTimestamp">The timestamp when the record was last updated.</param>
        /// <exception cref="ArgumentNullException">Thrown if any required parameter is null.</exception>
        /// <exception cref="ArgumentException">Thrown if any parameter does not meet length requirements.</exception>
        public TrackingRecord(
            string policyNumber,
            DateTime notifyDate,
            string status,
            DateTime addTimestamp,
            DateTime updateTimestamp)
        {
            PolicyNumber = policyNumber ?? throw new ArgumentNullException(nameof(policyNumber));
            Status = status ?? throw new ArgumentNullException(nameof(status));

            if (policyNumber.Length > 10)
                throw new ArgumentException("PolicyNumber must be at most 10 characters.", nameof(policyNumber));
            if (status.Length != 1)
                throw new ArgumentException("Status must be a single character.", nameof(status));

            NotifyDate = notifyDate;
            AddTimestamp = addTimestamp;
            UpdateTimestamp = updateTimestamp;
        }
    }
}