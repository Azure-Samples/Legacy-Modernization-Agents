using System;
using System.Data;
using System.Data.Common;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;

namespace Insurance.DataAccess
{
    /// <summary>
    /// Represents the input parameters for the PolicyCursorDriver.
    /// </summary>
    public record PolicyCursorRequest(
        string OperationType,
        string ProcessDate // Format: "yyyy-MM-dd"
    );

    /// <summary>
    /// Represents the output/result of the PolicyCursorDriver.
    /// </summary>
    public record PolicyCursorResponse(
        int SqlCode,
        PolicyRecord? PolicyData
    );

    /// <summary>
    /// Represents a policy record as returned by the cursor.
    /// </summary>
    public record PolicyRecord(
        string PolicyNumber,
        string PolicyHolderFirstName,
        string PolicyHolderMiddleName,
        string PolicyHolderLastName,
        string PolicyBeneficiaryName,
        string PolicyBeneficiaryRelation,
        string PolicyHolderAddress1,
        string PolicyHolderAddress2,
        string PolicyHolderCity,
        string PolicyHolderState,
        string PolicyHolderZipCode,
        DateTime PolicyHolderDateOfBirth,
        string PolicyHolderGender,
        string PolicyHolderPhone,
        string PolicyHolderEmail,
        string PolicyPaymentFrequency,
        string PolicyPaymentMethod,
        string PolicyUnderwriter,
        string PolicyTermsConditions,
        bool PolicyClaimed,
        string PolicyDiscountCode,
        decimal PolicyPremiumAmount,
        decimal PolicyCoverageAmount,
        string PolicyType,
        DateTime PolicyStartDate,
        DateTime PolicyExpiryDate,
        string PolicyStatus,
        string PolicyAgentCode,
        bool PolicyNotifyFlag,
        DateTime PolicyAddTimestamp,
        DateTime PolicyUpdateTimestamp
    );

    /// <summary>
    /// Provides operations for opening, fetching, and closing a policy cursor.
    /// </summary>
    public class PolicyCursorDriver : IAsyncDisposable
    {
        private readonly DbConnection _dbConnection;
        private readonly ILogger<PolicyCursorDriver> _logger;
        private DbDataReader? _policyCursorReader;

        /// <summary>
        /// Initializes a new instance of the <see cref="PolicyCursorDriver"/> class.
        /// </summary>
        /// <param name="dbConnection">The database connection to use.</param>
        /// <param name="logger">The logger for error and info messages.</param>
        public PolicyCursorDriver(DbConnection dbConnection, ILogger<PolicyCursorDriver> logger)
        {
            _dbConnection = dbConnection ?? throw new ArgumentNullException(nameof(dbConnection));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Executes the requested operation (OPEN, FETCH, CLOSE) on the policy cursor.
        /// </summary>
        /// <param name="request">The request containing operation type and process date.</param>
        /// <returns>The response containing SQL code and policy data (if applicable).</returns>
        public async Task<PolicyCursorResponse> ExecuteAsync(PolicyCursorRequest request)
        {
            if (request is null)
                throw new ArgumentNullException(nameof(request));

            int sqlCode = 0;
            PolicyRecord? policyData = null;

            try
            {
                switch (request.OperationType?.ToUpperInvariant())
                {
                    case "OPEN":
                        sqlCode = await OpenPolicyCursorAsync(request.ProcessDate);
                        break;
                    case "FETCH":
                        var fetchResult = await FetchPolicyCursorAsync();
                        sqlCode = fetchResult.SqlCode;
                        policyData = fetchResult.PolicyData;
                        break;
                    case "CLOSE":
                        sqlCode = await ClosePolicyCursorAsync();
                        break;
                    default:
                        _logger.LogError("Invalid operation type: {OperationType}", request.OperationType);
                        sqlCode = -1;
                        break;
                }
            }
            catch (DbException dbEx)
            {
                _logger.LogError(dbEx, "Database error during {OperationType}", request.OperationType);
                sqlCode = dbEx.ErrorCode;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Unexpected error during {OperationType}", request.OperationType);
                sqlCode = -99999; // Arbitrary error code for unexpected errors
            }

            return new PolicyCursorResponse(sqlCode, policyData);
        }

        /// <summary>
        /// Opens the policy cursor for subsequent fetch operations.
        /// </summary>
        /// <param name="processDate">The process date used in the query filter.</param>
        /// <returns>The SQL code (0 for success, non-zero for error).</returns>
        private async Task<int> OpenPolicyCursorAsync(string processDate)
        {
            try
            {
                if (_policyCursorReader != null && !_policyCursorReader.IsClosed)
                {
                    await _policyCursorReader.DisposeAsync();
                    _policyCursorReader = null;
                }

                if (_dbConnection.State != ConnectionState.Open)
                    await _dbConnection.OpenAsync();

                var command = _dbConnection.CreateCommand();
                command.CommandText = @"
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
                    FROM INSURNCE.TPOLICY
                    INNER JOIN INSURNCE.TCOVERAG
                        ON POLICY_NUMBER = COVERAGE_POL_NUM
                    WHERE POLICY_STATUS = 'A'
                        AND POLICY_HOLDER_STATE IN ('CA', 'MN', 'NY')
                        AND DATEDIFF(DAY, @ProcessDate, POLICY_EXPIRY_DATE) BETWEEN 30 AND 35
                        AND POLICY_TYPE = 'HEALTH'
                        AND COVERAGE_STATUS = 'ACTIVE'
                        AND COVERAGE_TYPE = 'CHARGEABLE'
                        AND POLICY_NUMBER NOT IN (
                            SELECT TR_POLICY_NUMBER
                            FROM INSURNCE.TTRAKING
                            WHERE TR_POLICY_NUMBER = POLICY_NUMBER
                              AND TR_STATUS = 'A'
                        )
                    ORDER BY POLICY_HOLDER_STATE, POLICY_AGENT_CODE
                ";
                var processDateParam = command.CreateParameter();
                processDateParam.ParameterName = "@ProcessDate";
                processDateParam.DbType = DbType.Date;
                processDateParam.Value = DateTime.Parse(processDate);
                command.Parameters.Add(processDateParam);

                _policyCursorReader = await command.ExecuteReaderAsync(CommandBehavior.SequentialAccess);
                return 0; // Success
            }
            catch (DbException dbEx)
            {
                _logger.LogError(dbEx, "Error opening policy cursor.");
                return dbEx.ErrorCode;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Unexpected error opening policy cursor.");
                return -99999;
            }
        }

        /// <summary>
        /// Fetches the next policy record from the cursor.
        /// </summary>
        /// <returns>A tuple containing SQL code and the fetched policy record.</returns>
        private async Task<(int SqlCode, PolicyRecord? PolicyData)> FetchPolicyCursorAsync()
        {
            if (_policyCursorReader == null)
            {
                _logger.LogError("Policy cursor is not open.");
                return (-2, null);
            }

            try
            {
                if (await _policyCursorReader.ReadAsync())
                {
                    var policy = MapPolicyRecord(_policyCursorReader);
                    return (0, policy);
                }
                else
                {
                    // End of data (SQLCODE 100 in COBOL)
                    return (100, null);
                }
            }
            catch (DbException dbEx)
            {
                _logger.LogError(dbEx, "Error fetching from policy cursor.");
                return (dbEx.ErrorCode, null);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Unexpected error fetching from policy cursor.");
                return (-99999, null);
            }
        }

        /// <summary>
        /// Closes the policy cursor.
        /// </summary>
        /// <returns>The SQL code (0 for success, non-zero for error).</returns>
        private async Task<int> ClosePolicyCursorAsync()
        {
            try
            {
                if (_policyCursorReader != null)
                {
                    await _policyCursorReader.DisposeAsync();
                    _policyCursorReader = null;
                }
                return 0;
            }
            catch (DbException dbEx)
            {
                _logger.LogError(dbEx, "Error closing policy cursor.");
                return dbEx.ErrorCode;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Unexpected error closing policy cursor.");
                return -99999;
            }
        }

        /// <summary>
        /// Maps a data reader row to a <see cref="PolicyRecord"/>.
        /// </summary>
        /// <param name="reader">The data reader.</param>
        /// <returns>The mapped policy record.</returns>
        private static PolicyRecord MapPolicyRecord(DbDataReader reader)
        {
            return new PolicyRecord(
                reader.GetString(0),
                reader.GetString(1),
                reader.GetString(2),
                reader.GetString(3),
                reader.GetString(4),
                reader.GetString(5),
                reader.GetString(6),
                reader.GetString(7),
                reader.GetString(8),
                reader.GetString(9),
                reader.GetString(10),
                reader.GetDateTime(11),
                reader.GetString(12),
                reader.GetString(13),
                reader.GetString(14),
                reader.GetString(15),
                reader.GetString(16),
                reader.GetString(17),
                reader.GetString(18),
                reader.GetBoolean(19),
                reader.GetString(20),
                reader.GetDecimal(21),
                reader.GetDecimal(22),
                reader.GetString(23),
                reader.GetDateTime(24),
                reader.GetDateTime(25),
                reader.GetString(26),
                reader.GetString(27),
                reader.GetBoolean(28),
                reader.GetDateTime(29),
                reader.GetDateTime(30)
            );
        }

        /// <summary>
        /// Disposes the policy cursor driver and closes any open resources.
        /// </summary>
        public async ValueTask DisposeAsync()
        {
            if (_policyCursorReader != null)
            {
                await _policyCursorReader.DisposeAsync();
                _policyCursorReader = null;
            }
            if (_dbConnection.State == ConnectionState.Open)
            {
                await _dbConnection.CloseAsync();
            }
        }
    }
}