using System;
using System.ComponentModel.DataAnnotations;

namespace Insurance.Data.Models
{
    /// <summary>
    /// Represents a coverage record in the INSURNCE.TCOVERAG table.
    /// </summary>
    /// <remarks>
    /// This class maps to the DB2 table INSURNCE.TCOVERAG as defined in the COBOL DECLGEN output.
    /// </remarks>
    public record CoverageRecord
    {
        /// <summary>
        /// Gets or sets the policy number associated with the coverage.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-POL-NUM (PIC X(10))
        /// DB2: COVERAGE_POL_NUM CHAR(10) NOT NULL
        /// </remarks>
        [Required]
        [MaxLength(10)]
        public string CoveragePolicyNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the status of the coverage.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-STATUS (PIC X(10))
        /// DB2: COVERAGE_STATUS CHAR(10) NOT NULL
        /// </remarks>
        [Required]
        [MaxLength(10)]
        public string CoverageStatus { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the start date of the coverage.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-START-DT (PIC X(10))
        /// DB2: COVERAGE_START_DT CHAR(10) NOT NULL
        /// Format: Expected to be 'yyyy-MM-dd' or similar.
        /// </remarks>
        [Required]
        [MaxLength(10)]
        public string CoverageStartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the end date of the coverage.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-END-DT (PIC X(10))
        /// DB2: COVERAGE_END_DT CHAR(10) NOT NULL
        /// Format: Expected to be 'yyyy-MM-dd' or similar.
        /// </remarks>
        [Required]
        [MaxLength(10)]
        public string CoverageEndDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp when the coverage record was added.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-ADD-TS (PIC X(26))
        /// DB2: COVERAGE_ADD_TS TIMESTAMP NOT NULL WITH DEFAULT
        /// </remarks>
        [Required]
        public DateTime CoverageAddedTimestamp { get; init; }

        /// <summary>
        /// Gets or sets the timestamp when the coverage record was last updated.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-UPDATE-TS (PIC X(26))
        /// DB2: COVERAGE_UPDATE_TS TIMESTAMP NOT NULL WITH DEFAULT
        /// </remarks>
        [Required]
        public DateTime CoverageUpdatedTimestamp { get; init; }
    }
}