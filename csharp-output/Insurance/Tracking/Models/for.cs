using System;

namespace Insurance.Tracking.Models
{
    /// <summary>
    /// Represents a tracking record for an insurance policy.
    /// Maps to the DB2 table INSURNCE.TTRAKING.
    /// </summary>
    public record TrackingRecord
    {
        /// <summary>
        /// Gets or sets the policy number.
        /// Maps to TR_POLICY_NUMBER CHAR(10) NOT NULL.
        /// </summary>
        public string PolicyNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the notification date.
        /// Maps to TR_NOTIFY_DATE DATE NOT NULL.
        /// </summary>
        public DateTime NotifyDate { get; init; }

        /// <summary>
        /// Gets or sets the status.
        /// Maps to TR_STATUS CHAR(1) NOT NULL.
        /// </summary>
        public string Status { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp when the record was added.
        /// Maps to TR_ADD_TIMESTAMP TIMESTAMP NOT NULL WITH DEFAULT.
        /// </summary>
        public DateTime AddTimestamp { get; init; }

        /// <summary>
        /// Gets or sets the timestamp when the record was last updated.
        /// Maps to TR_UPDATE_TIMESTAMP TIMESTAMP NOT NULL WITH DEFAULT.
        /// </summary>
        public DateTime UpdateTimestamp { get; init; }
    }
}