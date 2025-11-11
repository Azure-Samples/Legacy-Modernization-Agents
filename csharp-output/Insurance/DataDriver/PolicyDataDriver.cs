using System;
using System.Collections.Generic;
using System.Data;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;

namespace Insurance.DataDriver
{
    /// <summary>
    /// Represents the input parameters for the PolicyDataDriver.
    /// </summary>
    public record PolicyDriverRequest
    {
        /// <summary>
        /// The operation type: "OPEN", "FETCH", or "CLOSE".
        /// </summary>
        public string OperationType { get; init; } = string.Empty;

        /// <summary>
        /// The process date in yyyy-MM-dd format.
        /// </summary>
        public string ProcessDate { get; init; } = string.Empty;
    }

    /// <summary>
    /// Represents the output result from the PolicyDataDriver.
    /// </summary>
    public record PolicyDriverResponse
    {
        /// <summary>
        /// The SQL code returned from the operation.
        /// </summary>
        public int SqlCode { get; init; }

        /// <summary>
        /// The serialized policy data (if any).
        /// </summary>
        public string? PolicyData { get; init; }
    }

    /// <summary>
    /// Represents a policy record as returned by the database.
    /// </summary>
    public record PolicyRecord
    {
        public string PolicyNumber { get; init; } = string.Empty;
        public string PolicyHolderFirstName { get; init; } = string.Empty;
        public string PolicyHolderMiddleName { get; init; } = string.Empty;
        public string PolicyHolderLastName { get; init; } = string.Empty;
        public string PolicyBeneficiaryName { get; init; } = string.Empty;
        public string PolicyBeneficiaryRelation { get; init; } = string.Empty;
        public string PolicyHolderAddress1 { get; init; } = string.Empty;
        public string PolicyHolderAddress2 { get; init; } = string.Empty;
        public string PolicyHolderCity { get; init; } = string.Empty;
        public string PolicyHolderState { get; init; } = string.Empty;
        public string PolicyHolderZipCode { get; init; } = string.Empty;
        public DateTime? PolicyHolderDateOfBirth { get; init; }
        public string PolicyHolderGender { get; init; } = string.Empty;
        public string PolicyHolderPhone { get; init; } = string.Empty;
        public string PolicyHolderEmail { get; init; } = string.Empty;
        public string PolicyPaymentFrequency { get; init; } = string.Empty;
        public string PolicyPaymentMethod { get; init; } = string.Empty;
        public string PolicyUnderwriter { get; init; } = string.Empty;
        public string PolicyTermsConditions { get; init; } = string.Empty;
        public bool PolicyClaimed { get; init; }
        public string PolicyDiscountCode { get; init; } = string.Empty;
        public decimal PolicyPremiumAmount { get; init; }
        public decimal PolicyCoverageAmount { get; init; }
        public string PolicyType { get; init; } = string.Empty;
        public DateTime PolicyStartDate { get; init; }
        public DateTime PolicyExpiryDate { get; init; }
        public string PolicyStatus { get; init; } = string.Empty;
        public string PolicyAgentCode { get; init; } = string.Empty;
        public bool PolicyNotifyFlag { get; init; }
        public DateTime PolicyAddTimestamp { get; init; }
        public DateTime PolicyUpdateTimestamp { get; init; }
    }

    /// <summary>
    /// Provides operations for opening, fetching, and closing the insurance policy data cursor.
    /// </summary>
    public interface IPolicyDataDriver
    {
        /// <summary>
        /// Processes the requested operation on the policy data cursor.
        /// </summary>
        /// <param name="request">The input parameters.</param>
        /// <returns>The output result.</returns>
        Task<PolicyDriverResponse> ProcessAsync(PolicyDriverRequest request);
    }

    /// <summary>
    /// Implementation of the insurance policy data driver, converting COBOL cursor logic to modern C#.
    /// </summary>
    public class PolicyDataDriver : IPolicyDataDriver
    {
        private readonly IDbConnection _dbConnection;
        private readonly ILogger<PolicyDataDriver> _logger;
        private IDataReader? _policyCursor;

        /// <summary>
        /// Initializes a new instance of the <see cref="PolicyDataDriver"/> class.
        /// </summary>
        /// <param name="dbConnection">The database connection.</param>
        /// <param name="logger">The logger instance.</param>
        public PolicyDataDriver(IDbConnection dbConnection, ILogger<PolicyDataDriver> logger)
        {
            _dbConnection = dbConnection ?? throw new ArgumentNullException(nameof(dbConnection));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <inheritdoc />
        public async Task<PolicyDriverResponse> ProcessAsync(PolicyDriverRequest request)
        {
            if (request is null)
                throw new ArgumentNullException(nameof(request));

            int sqlCode = 0;
            string? policyData = null;

            try
            {
                switch (request.OperationType?.Trim().ToUpperInvariant())
                {
                    case "OPEN":
                        sqlCode = await OpenPolicyCursorAsync(request.ProcessDate);
                        break;

                    case "FETCH":
                        (sqlCode, policyData) = await FetchPolicyCursorAsync();
                        break;

                    case "CLOSE":
                        sqlCode = await ClosePolicyCursorAsync();
                        break;

                    default:
                        _logger.LogWarning("Invalid operation type: {OperationType}", request.OperationType);
                        sqlCode = -1;
                        break;
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Exception during PolicyDataDriver operation: {OperationType}", request.OperationType);
                sqlCode = -99999; // Custom error code for unhandled exceptions
            }

            return new PolicyDriverResponse
            {
                SqlCode = sqlCode,
                PolicyData = policyData
            };
        }

        /// <summary>
        /// Opens the policy data cursor for subsequent fetch operations.
        /// </summary>
        /// <param name="processDate">The process date as a string.</param>
        /// <returns>The SQL code (0 for success, non-zero for error).</returns>
        private async Task<int> OpenPolicyCursorAsync(string processDate)
        {
            try
            {
                // Parse process date
                if (!DateTime.TryParse(processDate, out var processDateValue))
                {
                    _logger.LogError("Invalid process date format: {ProcessDate}", processDate);
                    return -2;
                }

                // Build SQL command
                var sql = @"
SELECT 
    POLICY_NUMBER,
    POLICY_HOLDER_FNAME,
    POLICY_HOLDER_MNAME,
    POLICY_HOLDER_LNAME,
    POLICY_BENEF_NAME,
    POLICY_BENEF_RELATION,
    POLICY_HOLDER_ADDR_1,
    POLICY_HOLDER_ADDR_2,
    POLICY_HOLDER_CITY,
    POLICY_HOLDER_STATE,
    POLICY_HOLDER_ZIP_CD,
    POLICY_HOLDER_DOB,
    POLICY_HOLDER_GENDER,
    POLICY_HOLDER_PHONE,
    POLICY_HOLDER_EMAIL,
    POLICY_PAYMENT_FREQUENCY,
    POLICY_PAYMENT_METHOD,
    POLICY_UNDERWRITER,
    POLICY_TERMS_CONDITIONS,
    POLICY_CLAIMED,
    POLICY_DISCOUNT_CODE,
    POLICY_PREMIUM_AMOUNT,
    POLICY_COVERAGE_AMOUNT,
    POLICY_TYPE,
    POLICY_START_DATE,
    POLICY_EXPIRY_DATE,
    POLICY_STATUS,
    POLICY_AGENT_CODE,
    POLICY_NOTIFY_FLAG,
    POLICY_ADD_TIMESTAMP,
    POLICY_UPDATE_TIMESTAMP
FROM INSURNCE.TPOLICY p
INNER JOIN INSURNCE.TCOVERAG c ON p.POLICY_NUMBER = c.COVERAGE_POL_NUM
WHERE p.POLICY_STATUS = 'A'
  AND p.POLICY_HOLDER_STATE IN ('CA', 'MN', 'NY')
  AND DATEDIFF(DAY, @ProcessDate, p.POLICY_EXPIRY_DATE) BETWEEN 30 AND 35
  AND p.POLICY_TYPE = 'HEALTH'
  AND c.COVERAGE_STATUS = 'ACTIVE'
  AND c.COVERAGE_TYPE = 'CHARGEABLE'
  AND p.POLICY_NUMBER NOT IN (
        SELECT TR_POLICY_NUMBER
        FROM INSURNCE.TTRAKING
        WHERE TR_POLICY_NUMBER = p.POLICY_NUMBER
          AND TR_STATUS = 'A'
    )
ORDER BY p.POLICY_HOLDER_STATE, p.POLICY_AGENT_CODE
";

                var command = _dbConnection.CreateCommand();
                command.CommandText = sql;
                command.CommandType = CommandType.Text;

                var param = command.CreateParameter();
                param.ParameterName = "@ProcessDate";
                param.Value = processDateValue;
                command.Parameters.Add(param);

                // Open connection if not already open
                if (_dbConnection.State != ConnectionState.Open)
                    await _dbConnection.OpenAsync();

                // Open cursor (data reader)
                _policyCursor = await command.ExecuteReaderAsync();

                return 0; // Success
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error opening policy cursor.");
                return -3; // Error code for open failure
            }
        }

        /// <summary>
        /// Fetches the next policy record from the cursor.
        /// </summary>
        /// <returns>A tuple of SQL code and serialized policy data.</returns>
        private async Task<(int SqlCode, string? PolicyData)> FetchPolicyCursorAsync()
        {
            if (_policyCursor == null)
            {
                _logger.LogError("Policy cursor is not open.");
                return (-4, null);
            }

            try
            {
                if (await _policyCursor.ReadAsync())
                {
                    var record = MapPolicyRecord(_policyCursor);
                    // Serialize to string (e.g., JSON, or fixed-length as in COBOL)
                    var serialized = SerializePolicyRecord(record);
                    return (0, serialized);
                }
                else
                {
                    // End of cursor
                    return (100, null);
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error fetching from policy cursor.");
                return (-5, null);
            }
        }

        /// <summary>
        /// Closes the policy data cursor.
        /// </summary>
        /// <returns>The SQL code (0 for success, non-zero for error).</returns>
        private async Task<int> ClosePolicyCursorAsync()
        {
            try
            {
                if (_policyCursor != null)
                {
                    await _policyCursor.DisposeAsync();
                    _policyCursor = null;
                }
                return 0;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error closing policy cursor.");
                return -6;
            }
        }

        /// <summary>
        /// Maps a data reader row to a <see cref="PolicyRecord"/>.
        /// </summary>
        /// <param name="reader">The data reader.</param>
        /// <returns>The mapped policy record.</returns>
        private static PolicyRecord MapPolicyRecord(IDataReader reader)
        {
            // Use pattern matching and null checks for robust mapping
            return new PolicyRecord
            {
                PolicyNumber = reader["POLICY_NUMBER"] as string ?? string.Empty,
                PolicyHolderFirstName = reader["POLICY_HOLDER_FNAME"] as string ?? string.Empty,
                PolicyHolderMiddleName = reader["POLICY_HOLDER_MNAME"] as string ?? string.Empty,
                PolicyHolderLastName = reader["POLICY_HOLDER_LNAME"] as string ?? string.Empty,
                PolicyBeneficiaryName = reader["POLICY_BENEF_NAME"] as string ?? string.Empty,
                PolicyBeneficiaryRelation = reader["POLICY_BENEF_RELATION"] as string ?? string.Empty,
                PolicyHolderAddress1 = reader["POLICY_HOLDER_ADDR_1"] as string ?? string.Empty,
                PolicyHolderAddress2 = reader["POLICY_HOLDER_ADDR_2"] as string ?? string.Empty,
                PolicyHolderCity = reader["POLICY_HOLDER_CITY"] as string ?? string.Empty,
                PolicyHolderState = reader["POLICY_HOLDER_STATE"] as string ?? string.Empty,
                PolicyHolderZipCode = reader["POLICY_HOLDER_ZIP_CD"] as string ?? string.Empty,
                PolicyHolderDateOfBirth = reader["POLICY_HOLDER_DOB"] as DateTime?,
                PolicyHolderGender = reader["POLICY_HOLDER_GENDER"] as string ?? string.Empty,
                PolicyHolderPhone = reader["POLICY_HOLDER_PHONE"] as string ?? string.Empty,
                PolicyHolderEmail = reader["POLICY_HOLDER_EMAIL"] as string ?? string.Empty,
                PolicyPaymentFrequency = reader["POLICY_PAYMENT_FREQUENCY"] as string ?? string.Empty,
                PolicyPaymentMethod = reader["POLICY_PAYMENT_METHOD"] as string ?? string.Empty,
                PolicyUnderwriter = reader["POLICY_UNDERWRITER"] as string ?? string.Empty,
                PolicyTermsConditions = reader["POLICY_TERMS_CONDITIONS"] as string ?? string.Empty,
                PolicyClaimed = reader["POLICY_CLAIMED"] is bool b ? b : (reader["POLICY_CLAIMED"]?.ToString() == "Y"),
                PolicyDiscountCode = reader["POLICY_DISCOUNT_CODE"] as string ?? string.Empty,
                PolicyPremiumAmount = reader["POLICY_PREMIUM_AMOUNT"] is decimal d1 ? d1 : 0m,
                PolicyCoverageAmount = reader["POLICY_COVERAGE_AMOUNT"] is decimal d2 ? d2 : 0m,
                PolicyType = reader["POLICY_TYPE"] as string ?? string.Empty,
                PolicyStartDate = reader["POLICY_START_DATE"] is DateTime dt1 ? dt1 : DateTime.MinValue,
                PolicyExpiryDate = reader["POLICY_EXPIRY_DATE"] is DateTime dt2 ? dt2 : DateTime.MinValue,
                PolicyStatus = reader["POLICY_STATUS"] as string ?? string.Empty,
                PolicyAgentCode = reader["POLICY_AGENT_CODE"] as string ?? string.Empty,
                PolicyNotifyFlag = reader["POLICY_NOTIFY_FLAG"] is bool b2 ? b2 : (reader["POLICY_NOTIFY_FLAG"]?.ToString() == "Y"),
                PolicyAddTimestamp = reader["POLICY_ADD_TIMESTAMP"] is DateTime dt3 ? dt3 : DateTime.MinValue,
                PolicyUpdateTimestamp = reader["POLICY_UPDATE_TIMESTAMP"] is DateTime dt4 ? dt4 : DateTime.MinValue
            };
        }

        /// <summary>
        /// Serializes a <see cref="PolicyRecord"/> to a fixed-length string (as in COBOL), or JSON.
        /// </summary>
        /// <param name="record">The policy record.</param>
        /// <returns>The serialized string.</returns>
        private static string SerializePolicyRecord(PolicyRecord record)
        {
            // For demonstration, serialize as JSON. For COBOL compatibility, use fixed-length formatting.
            // return JsonSerializer.Serialize(record);

            // Example: Fixed-length serialization (truncated/padded to 466 chars as in COBOL)
            var fields = new List<string>
            {
                record.PolicyNumber.PadRight(20),
                record.PolicyHolderFirstName.PadRight(30),
                record.PolicyHolderMiddleName.PadRight(30),
                record.PolicyHolderLastName.PadRight(30),
                record.PolicyBeneficiaryName.PadRight(30),
                record.PolicyBeneficiaryRelation.PadRight(20),
                record.PolicyHolderAddress1.PadRight(40),
                record.PolicyHolderAddress2.PadRight(40),
                record.PolicyHolderCity.PadRight(20),
                record.PolicyHolderState.PadRight(2),
                record.PolicyHolderZipCode.PadRight(10),
                record.PolicyHolderDateOfBirth?.ToString("yyyy-MM-dd").PadRight(10) ?? "".PadRight(10),
                record.PolicyHolderGender.PadRight(1),
                record.PolicyHolderPhone.PadRight(15),
                record.PolicyHolderEmail.PadRight(40),
                record.PolicyPaymentFrequency.PadRight(10),
                record.PolicyPaymentMethod.PadRight(10),
                record.PolicyUnderwriter.PadRight(20),
                record.PolicyTermsConditions.PadRight(40),
                (record.PolicyClaimed ? "Y" : "N").PadRight(1),
                record.PolicyDiscountCode.PadRight(10),
                record.PolicyPremiumAmount.ToString("F2").PadLeft(12),
                record.PolicyCoverageAmount.ToString("F2").PadLeft(12),
                record.PolicyType.PadRight(10),
                record.PolicyStartDate.ToString("yyyy-MM-dd").PadRight(10),
                record.PolicyExpiryDate.ToString("yyyy-MM-dd").PadRight(10),
                record.PolicyStatus.PadRight(1),
                record.PolicyAgentCode.PadRight(10),
                (record.PolicyNotifyFlag ? "Y" : "N").PadRight(1),
                record.PolicyAddTimestamp.ToString("yyyy-MM-dd HH:mm:ss").PadRight(19),
                record.PolicyUpdateTimestamp.ToString("yyyy-MM-dd HH:mm:ss").PadRight(19)
            };

            var result = string.Concat(fields);
            return result.Length > 466 ? result.Substring(0, 466) : result.PadRight(466);
        }
    }
}