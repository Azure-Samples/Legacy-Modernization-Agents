using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace Insurance.DataDriver
{
    /// <summary>
    /// Represents the input parameters for the PolicyDataDriver.
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
    /// Represents the output results from the PolicyDataDriver.
    /// </summary>
    public record PolicyDriverOutput
    {
        /// <summary>
        /// The SQL status code. 0 = success, 100 = no more rows, negative = error.
        /// </summary>
        public int SqlCode { get; init; }

        /// <summary>
        /// The serialized policy data (if any).
        /// </summary>
        public string? PolicyData { get; init; }
    }

    /// <summary>
    /// Represents a policy record as returned by the cursor.
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
    /// Provides data access logic for insurance policy cursor operations.
    /// </summary>
    public interface IPolicyDataDriver : IAsyncDisposable
    {
        /// <summary>
        /// Processes the requested operation (OPEN, FETCH, CLOSE) on the policy cursor.
        /// </summary>
        /// <param name="input">The input parameters.</param>
        /// <returns>The output result, including SQL code and policy data if applicable.</returns>
        Task<PolicyDriverOutput> ProcessAsync(PolicyDriverInput input);
    }

    /// <summary>
    /// Implements the insurance policy data driver logic, converting COBOL cursor operations to C#.
    /// </summary>
    public class PolicyDataDriver : IPolicyDataDriver
    {
        private readonly DbConnection _connection;
        private readonly ILogger<PolicyDataDriver> _logger;
        private DbCommand? _policyCursorCommand;
        private DbDataReader? _policyCursorReader;
        private string? _processDate;

        /// <summary>
        /// Initializes a new instance of the <see cref="PolicyDataDriver"/> class.
        /// </summary>
        /// <param name="connection">The database connection.</param>
        /// <param name="logger">The logger instance.</param>
        public PolicyDataDriver(DbConnection connection, ILogger<PolicyDataDriver> logger)
        {
            _connection = connection ?? throw new ArgumentNullException(nameof(connection));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <inheritdoc />
        public async Task<PolicyDriverOutput> ProcessAsync(PolicyDriverInput input)
        {
            if (input is null)
                throw new ArgumentNullException(nameof(input));

            var operationType = input.OperationType?.Trim().ToUpperInvariant();
            _processDate = input.ProcessDate;

            try
            {
                switch (operationType)
                {
                    case "OPEN":
                        return await OpenPolicyCursorAsync();
                    case "FETCH":
                        return await FetchPolicyCursorAsync();
                    case "CLOSE":
                        return await ClosePolicyCursorAsync();
                    default:
                        _logger.LogWarning("Invalid operation type: {OperationType}", operationType);
                        return new PolicyDriverOutput { SqlCode = -1, PolicyData = null };
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Exception during {OperationType} operation.", operationType);
                return new PolicyDriverOutput { SqlCode = -2, PolicyData = null };
            }
        }

        /// <summary>
        /// Opens the policy cursor for subsequent fetch operations.
        /// </summary>
        /// <returns>The output result with SQL code.</returns>
        private async Task<PolicyDriverOutput> OpenPolicyCursorAsync()
        {
            try
            {
                if (_connection.State != ConnectionState.Open)
                    await _connection.OpenAsync();

                _policyCursorCommand = _connection.CreateCommand();
                _policyCursorCommand.CommandText = GetPolicyCursorSql();
                _policyCursorCommand.CommandType = CommandType.Text;

                // Add process date parameter
                var processDateParam = _policyCursorCommand.CreateParameter();
                processDateParam.ParameterName = "@ProcessDate";
                processDateParam.DbType = DbType.Date;
                processDateParam.Value = DateTime.Parse(_processDate!);
                _policyCursorCommand.Parameters.Add(processDateParam);

                _policyCursorReader = await _policyCursorCommand.ExecuteReaderAsync();

                return new PolicyDriverOutput { SqlCode = 0, PolicyData = null };
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error opening policy cursor.");
                return new PolicyDriverOutput { SqlCode = -3, PolicyData = null };
            }
        }

        /// <summary>
        /// Fetches the next row from the policy cursor.
        /// </summary>
        /// <returns>The output result with SQL code and serialized policy data.</returns>
        private async Task<PolicyDriverOutput> FetchPolicyCursorAsync()
        {
            if (_policyCursorReader == null)
            {
                _logger.LogWarning("Policy cursor is not open.");
                return new PolicyDriverOutput { SqlCode = -4, PolicyData = null };
            }

            try
            {
                if (await _policyCursorReader.ReadAsync())
                {
                    var policyRecord = MapPolicyRecord(_policyCursorReader);
                    var serializedPolicy = SerializePolicyRecord(policyRecord);
                    return new PolicyDriverOutput { SqlCode = 0, PolicyData = serializedPolicy };
                }
                else
                {
                    // SQLCODE 100: No more rows
                    return new PolicyDriverOutput { SqlCode = 100, PolicyData = null };
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error fetching from policy cursor.");
                return new PolicyDriverOutput { SqlCode = -5, PolicyData = null };
            }
        }

        /// <summary>
        /// Closes the policy cursor and releases resources.
        /// </summary>
        /// <returns>The output result with SQL code.</returns>
        private async Task<PolicyDriverOutput> ClosePolicyCursorAsync()
        {
            try
            {
                if (_policyCursorReader != null)
                {
                    await _policyCursorReader.DisposeAsync();
                    _policyCursorReader = null;
                }
                if (_policyCursorCommand != null)
                {
                    await _policyCursorCommand.DisposeAsync();
                    _policyCursorCommand = null;
                }
                return new PolicyDriverOutput { SqlCode = 0, PolicyData = null };
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error closing policy cursor.");
                return new PolicyDriverOutput { SqlCode = -6, PolicyData = null };
            }
        }

        /// <summary>
        /// Maps a data reader row to a <see cref="PolicyRecord"/>.
        /// </summary>
        /// <param name="reader">The data reader.</param>
        /// <returns>The mapped policy record.</returns>
        private static PolicyRecord MapPolicyRecord(DbDataReader reader)
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
        /// Serializes a <see cref="PolicyRecord"/> to a fixed-length string (COBOL PIC X(466) equivalent).
        /// </summary>
        /// <param name="policy">The policy record.</param>
        /// <returns>The serialized string.</returns>
        private static string SerializePolicyRecord(PolicyRecord policy)
        {
            // For demonstration, serialize as JSON and pad/truncate to 466 chars.
            var json = System.Text.Json.JsonSerializer.Serialize(policy);
            if (json.Length > 466)
                return json.Substring(0, 466);
            return json.PadRight(466);
        }

        /// <summary>
        /// Gets the SQL statement for the policy cursor.
        /// </summary>
        /// <returns>The SQL string.</returns>
        private static string GetPolicyCursorSql()
        {
            // Parameterized SQL for safety and maintainability
            return @"
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
  AND DATEDIFF(DAY, @ProcessDate, POLICY_EXPIRY_DATE) >= 30
  AND DATEDIFF(DAY, @ProcessDate, POLICY_EXPIRY_DATE) <= 35
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
        }

        /// <inheritdoc />
        public async ValueTask DisposeAsync()
        {
            if (_policyCursorReader != null)
                await _policyCursorReader.DisposeAsync();
            if (_policyCursorCommand != null)
                await _policyCursorCommand.DisposeAsync();
            if (_connection.State == ConnectionState.Open)
                await _connection.CloseAsync();
        }
    }

    /// <summary>
    /// Example of registering PolicyDataDriver with dependency injection.
    /// </summary>
    public static class ServiceCollectionExtensions
    {
        /// <summary>
        /// Adds the PolicyDataDriver to the service collection.
        /// </summary>
        /// <param name="services">The service collection.</param>
        /// <param name="connectionFactory">A factory for creating DbConnection instances.</param>
        /// <returns>The updated service collection.</returns>
        public static IServiceCollection AddPolicyDataDriver(
            this IServiceCollection services,
            Func<IServiceProvider, DbConnection> connectionFactory)
        {
            services.AddScoped<IPolicyDataDriver>(provider =>
            {
                var connection = connectionFactory(provider);
                var logger = provider.GetRequiredService<ILogger<PolicyDataDriver>>();
                return new PolicyDataDriver(connection, logger);
            });
            return services;
        }
    }
}