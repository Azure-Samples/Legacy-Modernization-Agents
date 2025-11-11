using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace Insurance.DataAccess
{
    /// <summary>
    /// Represents the input parameters for the PolicyDriver operation.
    /// </summary>
    public record PolicyDriverInput
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
    /// Represents the output from the PolicyDriver operation.
    /// </summary>
    public record PolicyDriverOutput
    {
        /// <summary>
        /// The SQL return code. 0 = success, 100 = no more rows, negative = error.
        /// </summary>
        public int SqlCode { get; init; }

        /// <summary>
        /// The policy data as a serialized string (for compatibility with legacy interface).
        /// </summary>
        public string? PolicyData { get; init; }
    }

    /// <summary>
    /// Represents a single policy record returned from the database.
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
        public string PolicyClaimed { get; init; } = string.Empty;
        public string PolicyDiscountCode { get; init; } = string.Empty;
        public decimal? PolicyPremiumAmount { get; init; }
        public decimal? PolicyCoverageAmount { get; init; }
        public string PolicyType { get; init; } = string.Empty;
        public DateTime? PolicyStartDate { get; init; }
        public DateTime? PolicyExpiryDate { get; init; }
        public string PolicyStatus { get; init; } = string.Empty;
        public string PolicyAgentCode { get; init; } = string.Empty;
        public string PolicyNotifyFlag { get; init; } = string.Empty;
        public DateTime? PolicyAddTimestamp { get; init; }
        public DateTime? PolicyUpdateTimestamp { get; init; }
    }

    /// <summary>
    /// Provides database access for insurance policy operations, emulating the COBOL DBDRIVR1 program.
    /// </summary>
    public class PolicyDriver : IAsyncDisposable
    {
        private readonly DbConnection dbConnection;
        private readonly ILogger<PolicyDriver> logger;
        private DbCommand? policyCursorCommand;
        private DbDataReader? policyCursorReader;
        private string? processDate;

        /// <summary>
        /// Initializes a new instance of the <see cref="PolicyDriver"/> class.
        /// </summary>
        /// <param name="dbConnection">The database connection to use.</param>
        /// <param name="logger">The logger instance.</param>
        public PolicyDriver(DbConnection dbConnection, ILogger<PolicyDriver> logger)
        {
            this.dbConnection = dbConnection ?? throw new ArgumentNullException(nameof(dbConnection));
            this.logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Executes the requested operation ("OPEN", "FETCH", "CLOSE") on the policy cursor.
        /// </summary>
        /// <param name="input">The input parameters.</param>
        /// <returns>The output result, including SQL code and policy data if applicable.</returns>
        public async Task<PolicyDriverOutput> ExecuteAsync(PolicyDriverInput input)
        {
            if (input is null)
                throw new ArgumentNullException(nameof(input));

            int sqlCode = 0;
            string? policyData = null;

            try
            {
                switch (input.OperationType.ToUpperInvariant())
                {
                    case "OPEN":
                        processDate = input.ProcessDate;
                        sqlCode = await OpenPolicyCursorAsync(processDate);
                        break;

                    case "FETCH":
                        (sqlCode, policyData) = await FetchPolicyCursorAsync();
                        break;

                    case "CLOSE":
                        sqlCode = await ClosePolicyCursorAsync();
                        break;

                    default:
                        logger.LogError("Invalid operation type: {OperationType}", input.OperationType);
                        sqlCode = -1;
                        break;
                }
            }
            catch (DbException dbEx)
            {
                logger.LogError(dbEx, "Database error during {OperationType}", input.OperationType);
                sqlCode = dbEx.ErrorCode != 0 ? dbEx.ErrorCode : -2;
            }
            catch (Exception ex)
            {
                logger.LogError(ex, "Unexpected error during {OperationType}", input.OperationType);
                sqlCode = -999;
            }

            return new PolicyDriverOutput
            {
                SqlCode = sqlCode,
                PolicyData = policyData
            };
        }

        /// <summary>
        /// Opens the policy cursor for subsequent fetch operations.
        /// </summary>
        /// <param name="processDate">The process date as a string (yyyy-MM-dd).</param>
        /// <returns>The SQL code: 0 for success, negative for error.</returns>
        private async Task<int> OpenPolicyCursorAsync(string? processDate)
        {
            if (policyCursorReader != null)
            {
                await policyCursorReader.DisposeAsync();
                policyCursorReader = null;
            }

            if (policyCursorCommand != null)
            {
                await policyCursorCommand.DisposeAsync();
                policyCursorCommand = null;
            }

            // Prepare SQL with parameters
            string sql = @"
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
                  AND DATEDIFF(DAY, @ProcessDate, p.POLICY_EXPIRY_DATE) >= 30
                  AND DATEDIFF(DAY, @ProcessDate, p.POLICY_EXPIRY_DATE) <= 35
                  AND p.POLICY_TYPE = 'HEALTH'
                  AND c.COVERAGE_STATUS = 'ACTIVE'
                  AND c.COVERAGE_TYPE = 'CHARGEABLE'
                  AND p.POLICY_NUMBER NOT IN (
                        SELECT t.TR_POLICY_NUMBER
                        FROM INSURNCE.TTRAKING t
                        WHERE t.TR_POLICY_NUMBER = p.POLICY_NUMBER
                          AND t.TR_STATUS = 'A'
                  )
                ORDER BY p.POLICY_HOLDER_STATE, p.POLICY_AGENT_CODE
            ";

            policyCursorCommand = dbConnection.CreateCommand();
            policyCursorCommand.CommandText = sql;
            policyCursorCommand.CommandType = CommandType.Text;

            var processDateParam = policyCursorCommand.CreateParameter();
            processDateParam.ParameterName = "@ProcessDate";
            processDateParam.DbType = DbType.Date;
            processDateParam.Value = DateTime.TryParse(processDate, out var dt) ? dt : DBNull.Value;
            policyCursorCommand.Parameters.Add(processDateParam);

            try
            {
                if (dbConnection.State != ConnectionState.Open)
                    await dbConnection.OpenAsync();

                policyCursorReader = await policyCursorCommand.ExecuteReaderAsync(CommandBehavior.SequentialAccess);
                logger.LogInformation("Policy cursor opened successfully.");
                return 0;
            }
            catch (DbException dbEx)
            {
                logger.LogError(dbEx, "Error opening policy cursor.");
                return dbEx.ErrorCode != 0 ? dbEx.ErrorCode : -2;
            }
        }

        /// <summary>
        /// Fetches the next policy record from the cursor.
        /// </summary>
        /// <returns>
        /// A tuple containing the SQL code (0 = success, 100 = no more rows, negative = error) and the serialized policy data.
        /// </returns>
        private async Task<(int sqlCode, string? policyData)> FetchPolicyCursorAsync()
        {
            if (policyCursorReader == null)
            {
                logger.LogWarning("Policy cursor is not open.");
                return (-3, null);
            }

            try
            {
                if (await policyCursorReader.ReadAsync())
                {
                    var record = MapPolicyRecord(policyCursorReader);
                    // For legacy compatibility, serialize to fixed-length string (simulate COBOL PIC X(466))
                    string serialized = SerializePolicyRecord(record);
                    return (0, serialized);
                }
                else
                {
                    // No more rows
                    await policyCursorReader.DisposeAsync();
                    policyCursorReader = null;
                    logger.LogInformation("No more policy records to fetch.");
                    return (100, null);
                }
            }
            catch (DbException dbEx)
            {
                logger.LogError(dbEx, "Error fetching from policy cursor.");
                return (dbEx.ErrorCode != 0 ? dbEx.ErrorCode : -2, null);
            }
        }

        /// <summary>
        /// Closes the policy cursor and releases resources.
        /// </summary>
        /// <returns>The SQL code: 0 for success, negative for error.</returns>
        private async Task<int> ClosePolicyCursorAsync()
        {
            try
            {
                if (policyCursorReader != null)
                {
                    await policyCursorReader.DisposeAsync();
                    policyCursorReader = null;
                }
                if (policyCursorCommand != null)
                {
                    await policyCursorCommand.DisposeAsync();
                    policyCursorCommand = null;
                }
                logger.LogInformation("Policy cursor closed successfully.");
                return 0;
            }
            catch (DbException dbEx)
            {
                logger.LogError(dbEx, "Error closing policy cursor.");
                return dbEx.ErrorCode != 0 ? dbEx.ErrorCode : -2;
            }
        }

        /// <summary>
        /// Maps a data reader row to a <see cref="PolicyRecord"/>.
        /// </summary>
        /// <param name="reader">The data reader.</param>
        /// <returns>The mapped policy record.</returns>
        private static PolicyRecord MapPolicyRecord(DbDataReader reader)
        {
            // Use pattern matching and null checks for safety
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
                PolicyClaimed = reader["POLICY_CLAIMED"] as string ?? string.Empty,
                PolicyDiscountCode = reader["POLICY_DISCOUNT_CODE"] as string ?? string.Empty,
                PolicyPremiumAmount = reader["POLICY_PREMIUM_AMOUNT"] as decimal?,
                PolicyCoverageAmount = reader["POLICY_COVERAGE_AMOUNT"] as decimal?,
                PolicyType = reader["POLICY_TYPE"] as string ?? string.Empty,
                PolicyStartDate = reader["POLICY_START_DATE"] as DateTime?,
                PolicyExpiryDate = reader["POLICY_EXPIRY_DATE"] as DateTime?,
                PolicyStatus = reader["POLICY_STATUS"] as string ?? string.Empty,
                PolicyAgentCode = reader["POLICY_AGENT_CODE"] as string ?? string.Empty,
                PolicyNotifyFlag = reader["POLICY_NOTIFY_FLAG"] as string ?? string.Empty,
                PolicyAddTimestamp = reader["POLICY_ADD_TIMESTAMP"] as DateTime?,
                PolicyUpdateTimestamp = reader["POLICY_UPDATE_TIMESTAMP"] as DateTime?
            };
        }

        /// <summary>
        /// Serializes a <see cref="PolicyRecord"/> to a fixed-length string for legacy compatibility.
        /// </summary>
        /// <param name="record">The policy record.</param>
        /// <returns>A fixed-length string representation (max 466 chars).</returns>
        private static string SerializePolicyRecord(PolicyRecord record)
        {
            // For demonstration, concatenate fields and pad/truncate to 466 chars.
            // In production, use a proper mapping to match COBOL PIC X(466) layout.
            var fields = new List<string>
            {
                record.PolicyNumber,
                record.PolicyHolderFirstName,
                record.PolicyHolderMiddleName,
                record.PolicyHolderLastName,
                record.PolicyBeneficiaryName,
                record.PolicyBeneficiaryRelation,
                record.PolicyHolderAddress1,
                record.PolicyHolderAddress2,
                record.PolicyHolderCity,
                record.PolicyHolderState,
                record.PolicyHolderZipCode,
                record.PolicyHolderDateOfBirth?.ToString("yyyy-MM-dd") ?? string.Empty,
                record.PolicyHolderGender,
                record.PolicyHolderPhone,
                record.PolicyHolderEmail,
                record.PolicyPaymentFrequency,
                record.PolicyPaymentMethod,
                record.PolicyUnderwriter,
                record.PolicyTermsConditions,
                record.PolicyClaimed,
                record.PolicyDiscountCode,
                record.PolicyPremiumAmount?.ToString("F2") ?? string.Empty,
                record.PolicyCoverageAmount?.ToString("F2") ?? string.Empty,
                record.PolicyType,
                record.PolicyStartDate?.ToString("yyyy-MM-dd") ?? string.Empty,
                record.PolicyExpiryDate?.ToString("yyyy-MM-dd") ?? string.Empty,
                record.PolicyStatus,
                record.PolicyAgentCode,
                record.PolicyNotifyFlag,
                record.PolicyAddTimestamp?.ToString("yyyy-MM-dd HH:mm:ss") ?? string.Empty,
                record.PolicyUpdateTimestamp?.ToString("yyyy-MM-dd HH:mm:ss") ?? string.Empty
            };

            string concatenated = string.Join('|', fields);
            if (concatenated.Length > 466)
                return concatenated.Substring(0, 466);
            else
                return concatenated.PadRight(466, ' ');
        }

        /// <summary>
        /// Disposes resources asynchronously.
        /// </summary>
        public async ValueTask DisposeAsync()
        {
            if (policyCursorReader != null)
            {
                await policyCursorReader.DisposeAsync();
                policyCursorReader = null;
            }
            if (policyCursorCommand != null)
            {
                await policyCursorCommand.DisposeAsync();
                policyCursorCommand = null;
            }
        }
    }

    /// <summary>
    /// Service registration for dependency injection.
    /// </summary>
    public static class PolicyDriverServiceCollectionExtensions
    {
        /// <summary>
        /// Adds the <see cref="PolicyDriver"/> to the service collection.
        /// </summary>
        /// <param name="services">The service collection.</param>
        /// <returns>The updated service collection.</returns>
        public static IServiceCollection AddPolicyDriver(this IServiceCollection services)
        {
            services.AddScoped<PolicyDriver>();
            return services;
        }
    }
}