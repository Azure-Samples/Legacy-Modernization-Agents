using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Insurance.Tracking.Models
{
    /// <summary>
    /// Represents a tracking record for an insurance policy.
    /// Maps to the DB2 table INSURNCE.TTRAKING.
    /// </summary>
    /// <remarks>
    /// This class is generated from a COBOL DECLGEN definition.
    /// It provides a strongly-typed model for use with Entity Framework or other ORMs.
    /// </remarks>
    [Table("TTRAKING", Schema = "INSURNCE")]
    public class TrackingRecord
    {
        /// <summary>
        /// Gets or sets the policy number.
        /// </summary>
        /// <remarks>
        /// COBOL: TR-POLICY-NUMBER PIC X(10)
        /// DB2: CHAR(10) NOT NULL
        /// </remarks>
        [Key]
        [Column("TR_POLICY_NUMBER")]
        [Required]
        [StringLength(10)]
        public string PolicyNumber { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the notification date.
        /// </summary>
        /// <remarks>
        /// COBOL: TR-NOTIFY-DATE PIC X(10)
        /// DB2: DATE NOT NULL
        /// </remarks>
        [Column("TR_NOTIFY_DATE")]
        [Required]
        public DateTime NotifyDate { get; set; }

        /// <summary>
        /// Gets or sets the status code.
        /// </summary>
        /// <remarks>
        /// COBOL: TR-STATUS PIC X(1)
        /// DB2: CHAR(1) NOT NULL
        /// </remarks>
        [Column("TR_STATUS")]
        [Required]
        [StringLength(1)]
        public string Status { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp when the record was added.
        /// </summary>
        /// <remarks>
        /// COBOL: TR-ADD-TIMESTAMP PIC X(26)
        /// DB2: TIMESTAMP NOT NULL WITH DEFAULT
        /// </remarks>
        [Column("TR_ADD_TIMESTAMP")]
        [Required]
        public DateTime AddTimestamp { get; set; }

        /// <summary>
        /// Gets or sets the timestamp when the record was last updated.
        /// </summary>
        /// <remarks>
        /// COBOL: TR-UPDATE-TIMESTAMP PIC X(26)
        /// DB2: TIMESTAMP NOT NULL WITH DEFAULT
        /// </remarks>
        [Column("TR_UPDATE_TIMESTAMP")]
        [Required]
        public DateTime UpdateTimestamp { get; set; }
    }
}