#nullable enable

using System;

namespace Insurance.Data.Models
{
    /// <summary>
    /// Represents a row in the INSURNCE.TCOVERAG DB2 table.
    /// </summary>
    /// <remarks>
    /// This record maps the COBOL structure DCLCOVGE and the DB2 table columns.
    /// </remarks>
    public record Coverage
    {
        /// <summary>
        /// Gets the policy number associated with the coverage.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-POL-NUM PIC X(10)
        /// DB2: COVERAGE_POL_NUM CHAR(10) NOT NULL
        /// </remarks>
        public string CoveragePolicyNumber { get; init; }

        /// <summary>
        /// Gets the status of the coverage.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-STATUS PIC X(10)
        /// DB2: COVERAGE_STATUS CHAR(10) NOT NULL
        /// </remarks>
        public string CoverageStatus { get; init; }

        /// <summary>
        /// Gets the start date of the coverage, as a string (format: yyyy-MM-dd or as stored).
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-START-DT PIC X(10)
        /// DB2: COVERAGE_START_DT CHAR(10) NOT NULL
        /// </remarks>
        public string CoverageStartDate { get; init; }

        /// <summary>
        /// Gets the end date of the coverage, as a string (format: yyyy-MM-dd or as stored).
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-END-DT PIC X(10)
        /// DB2: COVERAGE_END_DT CHAR(10) NOT NULL
        /// </remarks>
        public string CoverageEndDate { get; init; }

        /// <summary>
        /// Gets the timestamp when the coverage was added.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-ADD-TS PIC X(26)
        /// DB2: COVERAGE_ADD_TS TIMESTAMP NOT NULL WITH DEFAULT
        /// </remarks>
        public DateTime CoverageAddedTimestamp { get; init; }

        /// <summary>
        /// Gets the timestamp when the coverage was last updated.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-UPDATE-TS PIC X(26)
        /// DB2: COVERAGE_UPDATE_TS TIMESTAMP NOT NULL WITH DEFAULT
        /// </remarks>
        public DateTime CoverageUpdatedTimestamp { get; init; }

        /// <summary>
        /// Initializes a new instance of the <see cref="Coverage"/> record.
        /// </summary>
        /// <param name="coveragePolicyNumber">Policy number (max 10 chars).</param>
        /// <param name="coverageStatus">Coverage status (max 10 chars).</param>
        /// <param name="coverageStartDate">Coverage start date (max 10 chars).</param>
        /// <param name="coverageEndDate">Coverage end date (max 10 chars).</param>
        /// <param name="coverageAddedTimestamp">Timestamp when coverage was added.</param>
        /// <param name="coverageUpdatedTimestamp">Timestamp when coverage was updated.</param>
        /// <exception cref="ArgumentException">Thrown if any string parameter exceeds its max length or is null/empty.</exception>
        public Coverage(
            string coveragePolicyNumber,
            string coverageStatus,
            string coverageStartDate,
            string coverageEndDate,
            DateTime coverageAddedTimestamp,
            DateTime coverageUpdatedTimestamp)
        {
            CoveragePolicyNumber = ValidateString(coveragePolicyNumber, 10, nameof(coveragePolicyNumber));
            CoverageStatus = ValidateString(coverageStatus, 10, nameof(coverageStatus));
            CoverageStartDate = ValidateString(coverageStartDate, 10, nameof(coverageStartDate));
            CoverageEndDate = ValidateString(coverageEndDate, 10, nameof(coverageEndDate));
            CoverageAddedTimestamp = coverageAddedTimestamp;
            CoverageUpdatedTimestamp = coverageUpdatedTimestamp;
        }

        /// <summary>
        /// Validates that a string is not null, not empty, and does not exceed the specified maximum length.
        /// </summary>
        /// <param name="value">The string value to validate.</param>
        /// <param name="maxLength">The maximum allowed length.</param>
        /// <param name="paramName">The parameter name for error reporting.</param>
        /// <returns>The validated string.</returns>
        /// <exception cref="ArgumentException">Thrown if validation fails.</exception>
        private static string ValidateString(string value, int maxLength, string paramName)
        {
            if (string.IsNullOrWhiteSpace(value))
                throw new ArgumentException($"{paramName} cannot be null or empty.", paramName);
            if (value.Length > maxLength)
                throw new ArgumentException($"{paramName} cannot exceed {maxLength} characters.", paramName);
            return value;
        }
    }
}