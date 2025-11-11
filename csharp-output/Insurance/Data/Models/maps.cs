using System;

namespace Insurance.Data.Models
{
    /// <summary>
    /// Represents a coverage record from the INSURNCE.TCOVERAG table.
    /// </summary>
    /// <remarks>
    /// This class maps to the DB2 table INSURNCE.TCOVERAG.
    /// <para>
    /// COBOL source: DCLCOVGE group structure.
    /// </para>
    /// </remarks>
    public record Coverage
    {
        /// <summary>
        /// Gets the policy number associated with the coverage.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-POL-NUM (PIC X(10))
        /// DB2: COVERAGE_POL_NUM CHAR(10) NOT NULL
        /// </remarks>
        public string CoveragePolNum { get; init; }

        /// <summary>
        /// Gets the status of the coverage.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-STATUS (PIC X(10))
        /// DB2: COVERAGE_STATUS CHAR(10) NOT NULL
        /// </remarks>
        public string CoverageStatus { get; init; }

        /// <summary>
        /// Gets the start date of the coverage, as a string (format: yyyy-MM-dd).
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-START-DT (PIC X(10))
        /// DB2: COVERAGE_START_DT CHAR(10) NOT NULL
        /// </remarks>
        public string CoverageStartDt { get; init; }

        /// <summary>
        /// Gets the end date of the coverage, as a string (format: yyyy-MM-dd).
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-END-DT (PIC X(10))
        /// DB2: COVERAGE_END_DT CHAR(10) NOT NULL
        /// </remarks>
        public string CoverageEndDt { get; init; }

        /// <summary>
        /// Gets the timestamp when the coverage record was added.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-ADD-TS (PIC X(26))
        /// DB2: COVERAGE_ADD_TS TIMESTAMP NOT NULL WITH DEFAULT
        /// </remarks>
        public DateTime CoverageAddTs { get; init; }

        /// <summary>
        /// Gets the timestamp when the coverage record was last updated.
        /// </summary>
        /// <remarks>
        /// COBOL: COVERAGE-UPDATE-TS (PIC X(26))
        /// DB2: COVERAGE_UPDATE_TS TIMESTAMP NOT NULL WITH DEFAULT
        /// </remarks>
        public DateTime CoverageUpdateTs { get; init; }

        /// <summary>
        /// Initializes a new instance of the <see cref="Coverage"/> record.
        /// </summary>
        /// <param name="coveragePolNum">Policy number (max 10 chars).</param>
        /// <param name="coverageStatus">Coverage status (max 10 chars).</param>
        /// <param name="coverageStartDt">Coverage start date (string, format: yyyy-MM-dd).</param>
        /// <param name="coverageEndDt">Coverage end date (string, format: yyyy-MM-dd).</param>
        /// <param name="coverageAddTs">Timestamp when record was added.</param>
        /// <param name="coverageUpdateTs">Timestamp when record was last updated.</param>
        public Coverage(
            string coveragePolNum,
            string coverageStatus,
            string coverageStartDt,
            string coverageEndDt,
            DateTime coverageAddTs,
            DateTime coverageUpdateTs)
        {
            CoveragePolNum = coveragePolNum ?? throw new ArgumentNullException(nameof(coveragePolNum));
            CoverageStatus = coverageStatus ?? throw new ArgumentNullException(nameof(coverageStatus));
            CoverageStartDt = coverageStartDt ?? throw new ArgumentNullException(nameof(coverageStartDt));
            CoverageEndDt = coverageEndDt ?? throw new ArgumentNullException(nameof(coverageEndDt));
            CoverageAddTs = coverageAddTs;
            CoverageUpdateTs = coverageUpdateTs;
        }
    }
}