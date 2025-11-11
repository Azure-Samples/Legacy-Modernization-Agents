using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

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
        /// Maps to TR_POLICY_NUMBER (CHAR(10) NOT NULL).
        /// </summary>
        [Key]
        [Column("TR_POLICY_NUMBER")]
        [Required]
        [StringLength(10)]
        public string PolicyNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the notification date.
        /// Maps to TR_NOTIFY_DATE (DATE NOT NULL).
        /// </summary>
        [Column("TR_NOTIFY_DATE")]
        [Required]
        public DateTime NotifyDate { get; init; }

        /// <summary>
        /// Gets or sets the status of the tracking record.
        /// Maps to TR_STATUS (CHAR(1) NOT NULL).
        /// </summary>
        [Column("TR_STATUS")]
        [Required]
        [StringLength(1)]
        public string Status { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp when the record was added.
        /// Maps to TR_ADD_TIMESTAMP (TIMESTAMP NOT NULL WITH DEFAULT).
        /// </summary>
        [Column("TR_ADD_TIMESTAMP")]
        [Required]
        public DateTime AddTimestamp { get; init; } = DateTime.UtcNow;

        /// <summary>
        /// Gets or sets the timestamp when the record was last updated.
        /// Maps to TR_UPDATE_TIMESTAMP (TIMESTAMP NOT NULL WITH DEFAULT).
        /// </summary>
        [Column("TR_UPDATE_TIMESTAMP")]
        [Required]
        public DateTime UpdateTimestamp { get; init; } = DateTime.UtcNow;
    }
}