using System;

namespace Insurance.Data.Models
{
    /// <summary>
    /// Represents a coverage record in the INSURNCE.TCOVERAG table.
    /// </summary>
    /// <remarks>
    /// This class maps the COBOL group variable DCLCOVGE and the DB2 table structure.
    /// </remarks>
    public record Coverage
    {
        /// <summary>
        /// Gets or sets the policy number associated with the coverage.
        /// </summary>
        /// <remarks>
        /// Corresponds to COVERAGE_POL_NUM CHAR(10) NOT NULL.
        /// </remarks>
        public string CoveragePolicyNumber { get; init; }

        /// <summary>
        /// Gets or sets the status of the coverage.
        /// </summary>
        /// <remarks>
        /// Corresponds to COVERAGE_STATUS CHAR(10) NOT NULL.
        /// </remarks>
        public string CoverageStatus { get; init; }

        /// <summary>
        /// Gets or sets the start date of the coverage (format: yyyy-MM-dd).
        /// </summary>
        /// <remarks>
        /// Corresponds to COVERAGE_START_DT CHAR(10) NOT NULL.
        /// </remarks>
        public string CoverageStartDate { get; init; }

        /// <summary>
        /// Gets or sets the end date of the coverage (format: yyyy-MM-dd).
        /// </summary>
        /// <remarks>
        /// Corresponds to COVERAGE_END_DT CHAR(10) NOT NULL.
        /// </remarks>
        public string CoverageEndDate { get; init; }

        /// <summary>
        /// Gets or sets the timestamp when the coverage record was added.
        /// </summary>
        /// <remarks>
        /// Corresponds to COVERAGE_ADD_TS TIMESTAMP NOT NULL WITH DEFAULT.
        /// </remarks>
        public DateTime CoverageAddedTimestamp { get; init; }

        /// <summary>
        /// Gets or sets the timestamp when the coverage record was last updated.
        /// </summary>
        /// <remarks>
        /// Corresponds to COVERAGE_UPDATE_TS TIMESTAMP NOT NULL WITH DEFAULT.
        /// </remarks>
        public DateTime CoverageUpdatedTimestamp { get; init; }
    }
}