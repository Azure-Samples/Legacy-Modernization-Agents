using System;
using System.Globalization;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace Insurance.PolicyExpiryNotification
{
    /// <summary>
    /// Main entry point for the Policy Expiry Notification batch process.
    /// Processes insurance policies expiring within 30 days, generates notifications for customers and agents,
    /// and produces summary reports. Interacts with database and file services via injected dependencies.
    /// </summary>
    public class PolicyExpiryBatch
    {
        private readonly IDbDriver1 _dbDriver1;
        private readonly IDbDriver2 _dbDriver2;
        private readonly IFileDriver1 _fileDriver1;
        private readonly IFileDriver2 _fileDriver2;
        private readonly ILogger<PolicyExpiryBatch> _logger;

        // Working storage variables
        private PolicyRecord? _currentPolicy;
        private AgentRecord? _currentAgent;
        private CustomerNotifyRecord _customerNotifyRecord = new();
        private AgentNotifyRecord _agentNotifyRecord = new();

        private DateOnly _processDate;
        private string _currentState = string.Empty;
        private string _currentAgentCode = string.Empty;

        // Totals and counters
        private int _agentTotalPolicyCount = 0;
        private decimal _agentTotalPremium = 0m;
        private int _stateTotalPolicyCount = 0;
        private decimal _stateTotalPremium = 0m;
        private int _grandTotalPolicyCount = 0;
        private decimal _grandTotalPremium = 0m;

        // Report lines
        private readonly ReportLineBuilder _reportLineBuilder = new();

        /// <summary>
        /// Constructs the batch process with required dependencies.
        /// </summary>
        public PolicyExpiryBatch(
            IDbDriver1 dbDriver1,
            IDbDriver2 dbDriver2,
            IFileDriver1 fileDriver1,
            IFileDriver2 fileDriver2,
            ILogger<PolicyExpiryBatch> logger)
        {
            _dbDriver1 = dbDriver1;
            _dbDriver2 = dbDriver2;
            _fileDriver1 = fileDriver1;
            _fileDriver2 = fileDriver2;
            _logger = logger;
        }

        /// <summary>
        /// Executes the batch process asynchronously.
        /// </summary>
        public async Task RunAsync()
        {
            try
            {
                await InitializeAsync();
                await ProcessPoliciesAsync();
                await FinalizeAsync();
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Fatal error in PolicyExpiryBatch.");
                throw;
            }
        }

        /// <summary>
        /// Initializes files, cursors, and sets up the process date.
        /// </summary>
        private async Task InitializeAsync()
        {
            _processDate = DateOnly.FromDateTime(DateTime.Now);

            // Open policy cursor
            var openCursorResult = await _dbDriver1.OpenCursorAsync(_processDate);
            if (!openCursorResult.IsSuccess)
            {
                _logger.LogError("Error opening policy cursor: {SqlCode}", openCursorResult.ErrorCode);
                throw new ApplicationException($"Error opening policy cursor: {openCursorResult.ErrorCode}");
            }

            // Open agent file
            var openAgentFileResult = await _fileDriver1.OpenAsync();
            if (!openAgentFileResult.IsSuccess)
            {
                _logger.LogError("Error opening agent file: {StatusCode}", openAgentFileResult.ErrorCode);
                throw new ApplicationException($"Error opening agent file: {openAgentFileResult.ErrorCode}");
            }

            // Open notification files
            foreach (var fileName in new[] { "CUSTOMER-NOTIFY-FILE", "NOTIFY-REPORT-FILE", "AGENT-NOTIFY-FILE" })
            {
                var openNotifyFileResult = await _fileDriver2.OpenAsync(fileName);
                if (!openNotifyFileResult.IsSuccess)
                {
                    _logger.LogError("Error opening {FileName}: {StatusCode}", fileName, openNotifyFileResult.ErrorCode);
                    throw new ApplicationException($"Error opening {fileName}: {openNotifyFileResult.ErrorCode}");
                }
            }

            // Write report header
            await WriteReportHeaderAsync();
        }

        /// <summary>
        /// Main processing loop: fetches policies, processes notifications and summaries.
        /// </summary>
        private async Task ProcessPoliciesAsync()
        {
            bool noMorePolicy = false;

            while (!noMorePolicy)
            {
                var fetchResult = await _dbDriver1.FetchNextPolicyAsync();
                switch (fetchResult.Status)
                {
                    case DbFetchStatus.EndOfData:
                        noMorePolicy = true;
                        break;
                    case DbFetchStatus.Success:
                        _currentPolicy = fetchResult.Policy;
                        await GetAgentDetailAsync();
                        await WriteCustomerNotificationAsync();
                        await UpdateTrackingAsync();
                        await ProcessSummaryAsync();
                        break;
                    case DbFetchStatus.Error:
                        _logger.LogError("Error fetching policy record: {SqlCode}", fetchResult.ErrorCode);
                        throw new ApplicationException($"Error fetching policy record: {fetchResult.ErrorCode}");
                }
            }
        }

        /// <summary>
        /// Fetches agent details for the current policy.
        /// </summary>
        private async Task GetAgentDetailAsync()
        {
            if (_currentPolicy == null)
                throw new InvalidOperationException("Current policy is null.");

            var agentCode = _currentPolicy.AgentCode;
            var agentResult = await _fileDriver1.SearchAgentAsync(agentCode);

            if (agentResult.IsSuccess)
            {
                _currentAgent = agentResult.Agent;
                await WriteAgentNotificationAsync();
            }
            else
            {
                _logger.LogError("Error fetching agent record: {StatusCode}", agentResult.ErrorCode);
                throw new ApplicationException($"Error fetching agent record: {agentResult.ErrorCode}");
            }
        }

        /// <summary>
        /// Updates tracking for the processed policy.
        /// </summary>
        private async Task UpdateTrackingAsync()
        {
            if (_currentPolicy == null)
                throw new InvalidOperationException("Current policy is null.");

            var insertResult = await _dbDriver2.InsertTrackingAsync(_processDate, _currentPolicy.PolicyNumber);
            if (!insertResult.IsSuccess)
            {
                _logger.LogError("Error inserting into tracking: {SqlCode}", insertResult.ErrorCode);
                throw new ApplicationException($"Error inserting into tracking: {insertResult.ErrorCode}");
            }
        }

        /// <summary>
        /// Handles summary and break logic for state/agent, writes summary lines and headers.
        /// </summary>
        private async Task ProcessSummaryAsync()
        {
            if (_currentPolicy == null || _currentAgent == null)
                throw new InvalidOperationException("Current policy or agent is null.");

            var policyState = _currentPolicy.HolderState;
            var agentCode = _currentAgent.AgentCode;

            // State break
            if (!string.Equals(policyState, _currentState, StringComparison.OrdinalIgnoreCase))
            {
                if (_grandTotalPolicyCount != 0)
                {
                    await WriteAgentSummaryAsync();
                    await WriteBreakLineAsync();
                    await WriteStateSummaryAsync();
                }
                ResetAgentTotals();
                ResetStateTotals();
                _currentState = policyState;
                await WriteBreakLineAsync();
                await WriteStateHeaderAsync();
                await WriteBreakLineAsync();
                await WriteAgentHeaderAsync();
                await WriteBreakLineAsync();
                await WritePolicyHeaderAsync();
            }
            // Agent break
            else if (!string.Equals(agentCode, _currentAgentCode, StringComparison.OrdinalIgnoreCase))
            {
                await WriteAgentSummaryAsync();
                ResetAgentTotals();
                _currentAgentCode = agentCode;
                await WriteBreakLineAsync();
                await WriteAgentHeaderAsync();
                await WriteBreakLineAsync();
                await WritePolicyHeaderAsync();
            }

            await WritePolicyDetailLineAsync();

            _agentTotalPolicyCount++;
            _stateTotalPolicyCount++;
            _grandTotalPolicyCount++;

            _agentTotalPremium += _currentPolicy.PremiumAmount;
            _stateTotalPremium += _currentPolicy.PremiumAmount;
            _grandTotalPremium += _currentPolicy.PremiumAmount;
        }

        /// <summary>
        /// Resets agent totals.
        /// </summary>
        private void ResetAgentTotals()
        {
            _agentTotalPolicyCount = 0;
            _agentTotalPremium = 0m;
        }

        /// <summary>
        /// Resets state totals.
        /// </summary>
        private void ResetStateTotals()
        {
            _stateTotalPolicyCount = 0;
            _stateTotalPremium = 0m;
        }

        /// <summary>
        /// Populates and writes customer notification record.
        /// </summary>
        private async Task WriteCustomerNotificationAsync()
        {
            if (_currentPolicy == null || _currentAgent == null)
                throw new InvalidOperationException("Current policy or agent is null.");

            _customerNotifyRecord = CustomerNotifyRecord.FromPolicyAndAgent(_currentPolicy, _currentAgent, _processDate);

            var writeResult = await _fileDriver2.WriteAsync("CUSTOMER-NOTIFY-FILE", _customerNotifyRecord);
            if (!writeResult.IsSuccess)
            {
                _logger.LogError("Error writing to customer notify file: {StatusCode}", writeResult.ErrorCode);
            }
        }

        /// <summary>
        /// Populates and writes agent notification record if agent is corporate.
        /// </summary>
        private async Task WriteAgentNotificationAsync()
        {
            if (_currentPolicy == null || _currentAgent == null)
                throw new InvalidOperationException("Current policy or agent is null.");

            if (string.Equals(_currentAgent.AgentType, "CORPORATE", StringComparison.OrdinalIgnoreCase))
            {
                _agentNotifyRecord = AgentNotifyRecord.FromPolicyAndAgent(_currentPolicy, _currentAgent, _processDate);

                var writeResult = await _fileDriver2.WriteAsync("AGENT-NOTIFY-FILE", _agentNotifyRecord);
                if (!writeResult.IsSuccess)
                {
                    _logger.LogError("Error writing to agent notify file: {StatusCode}", writeResult.ErrorCode);
                }
            }
        }

        /// <summary>
        /// Writes notification report line to file.
        /// </summary>
        private async Task WriteNotificationReportAsync(string reportLine)
        {
            var writeResult = await _fileDriver2.WriteAsync("NOTIFY-REPORT-FILE", new NotifyReportRecord(reportLine));
            if (!writeResult.IsSuccess)
            {
                _logger.LogError("Error writing to notify report file: {StatusCode}", writeResult.ErrorCode);
            }
        }

        /// <summary>
        /// Writes agent summary line to report.
        /// </summary>
        private async Task WriteAgentSummaryAsync()
        {
            if (_currentAgent == null)
                throw new InvalidOperationException("Current agent is null.");

            var line = _reportLineBuilder.BuildAgentSummaryLine(_currentAgent.AgentCode, _agentTotalPolicyCount, _agentTotalPremium);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes state summary line to report.
        /// </summary>
        private async Task WriteStateSummaryAsync()
        {
            var line = _reportLineBuilder.BuildStateSummaryLine(_currentState, _stateTotalPolicyCount, _stateTotalPremium);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes grand summary line to report.
        /// </summary>
        private async Task WriteGrandSummaryAsync()
        {
            var line = _reportLineBuilder.BuildGrandSummaryLine(_grandTotalPolicyCount, _grandTotalPremium);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes state header line to report.
        /// </summary>
        private async Task WriteStateHeaderAsync()
        {
            var line = _reportLineBuilder.BuildStateHeaderLine(_currentState);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes agent header lines to report.
        /// </summary>
        private async Task WriteAgentHeaderAsync()
        {
            if (_currentAgent == null)
                throw new InvalidOperationException("Current agent is null.");

            foreach (var line in _reportLineBuilder.BuildAgentHeaderLines(_currentAgent))
            {
                await WriteNotificationReportAsync(line);
            }
        }

        /// <summary>
        /// Writes policy header lines to report.
        /// </summary>
        private async Task WritePolicyHeaderAsync()
        {
            foreach (var line in _reportLineBuilder.BuildPolicyHeaderLines())
            {
                await WriteNotificationReportAsync(line);
            }
        }

        /// <summary>
        /// Writes policy detail line to report.
        /// </summary>
        private async Task WritePolicyDetailLineAsync()
        {
            if (_currentPolicy == null)
                throw new InvalidOperationException("Current policy is null.");

            var line = _reportLineBuilder.BuildPolicyDetailLine(_currentPolicy);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes a blank/filler line to report.
        /// </summary>
        private async Task WriteBreakLineAsync()
        {
            var line = _reportLineBuilder.BuildFillerLine();
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes the main report header.
        /// </summary>
        private async Task WriteReportHeaderAsync()
        {
            await WriteBreakLineAsync();
            var headerLine = _reportLineBuilder.BuildMainHeaderLine(_processDate);
            await WriteNotificationReportAsync(headerLine);
            await WriteBreakLineAsync();
        }

        /// <summary>
        /// Finalizes the process by closing all files and cursors.
        /// </summary>
        private async Task FinalizeAsync()
        {
            // Close policy cursor
            var closeCursorResult = await _dbDriver1.CloseCursorAsync();
            if (!closeCursorResult.IsSuccess)
            {
                _logger.LogError("Error closing policy cursor: {SqlCode}", closeCursorResult.ErrorCode);
                throw new ApplicationException($"Error closing policy cursor: {closeCursorResult.ErrorCode}");
            }

            // Close agent file
            var closeAgentFileResult = await _fileDriver1.CloseAsync();
            if (!closeAgentFileResult.IsSuccess)
            {
                _logger.LogError("Error closing agent file: {StatusCode}", closeAgentFileResult.ErrorCode);
                throw new ApplicationException($"Error closing agent file: {closeAgentFileResult.ErrorCode}");
            }

            // Close notification files
            foreach (var fileName in new[] { "CUSTOMER-NOTIFY-FILE", "NOTIFY-REPORT-FILE", "AGENT-NOTIFY-FILE" })
            {
                var closeNotifyFileResult = await _fileDriver2.CloseAsync(fileName);
                if (!closeNotifyFileResult.IsSuccess)
                {
                    _logger.LogError("Error closing {FileName}: {StatusCode}", fileName, closeNotifyFileResult.ErrorCode);
                    throw new ApplicationException($"Error closing {fileName}: {closeNotifyFileResult.ErrorCode}");
                }
            }
        }
    }

    #region Data Models

    /// <summary>
    /// Represents a policy record.
    /// </summary>
    public record PolicyRecord
    {
        public string PolicyNumber { get; init; } = string.Empty;
        public string HolderFirstName { get; init; } = string.Empty;
        public string HolderMiddleName { get; init; } = string.Empty;
        public string HolderLastName { get; init; } = string.Empty;
        public string HolderAddress1 { get; init; } = string.Empty;
        public string HolderAddress2 { get; init; } = string.Empty;
        public string HolderCity { get; init; } = string.Empty;
        public string HolderState { get; init; } = string.Empty;
        public string HolderZipCode { get; init; } = string.Empty;
        public string StartDate { get; init; } = string.Empty;
        public string ExpiryDate { get; init; } = string.Empty;
        public decimal PremiumAmount { get; init; }
        public string BeneficiaryName { get; init; } = string.Empty;
        public string AgentCode { get; init; } = string.Empty;
    }

    /// <summary>
    /// Represents an agent record.
    /// </summary>
    public record AgentRecord
    {
        public string AgentCode { get; init; } = string.Empty;
        public string AgentName { get; init; } = string.Empty;
        public string AgentAddress1 { get; init; } = string.Empty;
        public string AgentAddress2 { get; init; } = string.Empty;
        public string AgentCity { get; init; } = string.Empty;
        public string AgentState { get; init; } = string.Empty;
        public string AgentZipCode { get; init; } = string.Empty;
        public string AgentEmail { get; init; } = string.Empty;
        public string AgentContactNo { get; init; } = string.Empty;
        public string AgentType { get; init; } = string.Empty;
    }

    /// <summary>
    /// Represents a customer notification record.
    /// </summary>
    public record CustomerNotifyRecord
    {
        public string PolicyNumber { get; init; } = string.Empty;
        public string FirstName { get; init; } = string.Empty;
        public string MiddleName { get; init; } = string.Empty;
        public string LastName { get; init; } = string.Empty;
        public string Address1 { get; init; } = string.Empty;
        public string Address2 { get; init; } = string.Empty;
        public string City { get; init; } = string.Empty;
        public string State { get; init; } = string.Empty;
        public string ZipCode { get; init; } = string.Empty;
        public string StartDate { get; init; } = string.Empty;
        public string ExpiryDate { get; init; } = string.Empty;
        public string NotifyDate { get; init; } = string.Empty;
        public string NotifyMessage { get; init; } = string.Empty;
        public string AgentCode { get; init; } = string.Empty;
        public string AgentName { get; init; } = string.Empty;
        public string Email { get; init; } = string.Empty;
        public string BeneficiaryName { get; init; } = string.Empty;
        public string StatutoryMessage { get; init; } = string.Empty;

        /// <summary>
        /// Factory method to create a customer notification record from policy and agent.
        /// </summary>
        public static CustomerNotifyRecord FromPolicyAndAgent(PolicyRecord policy, AgentRecord agent, DateOnly notifyDate)
        {
            return new CustomerNotifyRecord
            {
                PolicyNumber = policy.PolicyNumber,
                FirstName = policy.HolderFirstName,
                MiddleName = policy.HolderMiddleName,
                LastName = policy.HolderLastName,
                Address1 = policy.HolderAddress1,
                Address2 = policy.HolderAddress2,
                City = policy.HolderCity,
                State = policy.HolderState,
                ZipCode = policy.HolderZipCode,
                StartDate = policy.StartDate,
                ExpiryDate = policy.ExpiryDate,
                NotifyDate = notifyDate.ToString("yyyy/MM/dd"),
                NotifyMessage = "PLEASE NOTE YOUR POLICY IS EXPIRING SOON. GET IT RENEWED TO CONTINUE COVERAGE",
                AgentCode = policy.AgentCode,
                AgentName = agent.AgentName,
                Email = agent.AgentEmail,
                BeneficiaryName = policy.BeneficiaryName,
                StatutoryMessage = "IF YOU FAIL TO RENEW BY EXPIRY DATE YOUR INSURANCE COVERAGE WILL END"
            };
        }
    }

    /// <summary>
    /// Represents an agent notification record.
    /// </summary>
    public record AgentNotifyRecord
    {
        public string AgentCode { get; init; } = string.Empty;
        public string AgentName { get; init; } = string.Empty;
        public string Address1 { get; init; } = string.Empty;
        public string Address2 { get; init; } = string.Empty;
        public string City { get; init; } = string.Empty;
        public string State { get; init; } = string.Empty;
        public string ZipCode { get; init; } = string.Empty;
        public string Email { get; init; } = string.Empty;
        public string PolicyNumber { get; init; } = string.Empty;
        public string PolicyHolderFirstName { get; init; } = string.Empty;
        public string PolicyHolderMiddleName { get; init; } = string.Empty;
        public string PolicyHolderLastName { get; init; } = string.Empty;
        public string PolicyStartDate { get; init; } = string.Empty;
        public string PolicyExpiryDate { get; init; } = string.Empty;
        public string NotifyDate { get; init; } = string.Empty;
        public string NotifyMessage { get; init; } = string.Empty;

        /// <summary>
        /// Factory method to create an agent notification record from policy and agent.
        /// </summary>
        public static AgentNotifyRecord FromPolicyAndAgent(PolicyRecord policy, AgentRecord agent, DateOnly notifyDate)
        {
            return new AgentNotifyRecord
            {
                AgentCode = agent.AgentCode,
                AgentName = agent.AgentName,
                Address1 = agent.AgentAddress1,
                Address2 = agent.AgentAddress2,
                City = agent.AgentCity,
                State = agent.AgentState,
                ZipCode = agent.AgentZipCode,
                Email = agent.AgentEmail,
                PolicyNumber = policy.PolicyNumber,
                PolicyHolderFirstName = policy.HolderFirstName,
                PolicyHolderMiddleName = policy.HolderMiddleName,
                PolicyHolderLastName = policy.HolderLastName,
                PolicyStartDate = policy.StartDate,
                PolicyExpiryDate = policy.ExpiryDate,
                NotifyDate = notifyDate.ToString("yyyy/MM/dd"),
                NotifyMessage = "PLEASE NOTE CUSTOMER POLICY IS EXPIRING SOON"
            };
        }
    }

    /// <summary>
    /// Represents a notification report record (single line).
    /// </summary>
    public record NotifyReportRecord(string ReportLine);

    #endregion

    #region Driver Interfaces

    /// <summary>
    /// Interface for DB driver 1 (policy cursor operations).
    /// </summary>
    public interface IDbDriver1
    {
        Task<DbOperationResult> OpenCursorAsync(DateOnly processDate);
        Task<DbFetchResult> FetchNextPolicyAsync();
        Task<DbOperationResult> CloseCursorAsync();
    }

    /// <summary>
    /// Interface for DB driver 2 (tracking insert).
    /// </summary>
    public interface IDbDriver2
    {
        Task<DbOperationResult> InsertTrackingAsync(DateOnly processDate, string policyNumber);
    }

    /// <summary>
    /// Interface for file driver 1 (agent file operations).
    /// </summary>
    public interface IFileDriver1
    {
        Task<FileOperationResult> OpenAsync();
        Task<AgentSearchResult> SearchAgentAsync(string agentCode);
        Task<FileOperationResult> CloseAsync();
    }

    /// <summary>
    /// Interface for file driver 2 (notification/report file operations).
    /// </summary>
    public interface IFileDriver2
    {
        Task<FileOperationResult> OpenAsync(string fileName);
        Task<FileOperationResult> WriteAsync(string fileName, object record);
        Task<FileOperationResult> CloseAsync(string fileName);
    }

    #endregion

    #region Driver Result Models

    /// <summary>
    /// Represents the result of a DB operation.
    /// </summary>
    public record DbOperationResult(bool IsSuccess, int ErrorCode);

    /// <summary>
    /// Represents the result of a DB fetch operation.
    /// </summary>
    public record DbFetchResult(DbFetchStatus Status, PolicyRecord? Policy, int ErrorCode);

    /// <summary>
    /// Enum for DB fetch status.
    /// </summary>
    public enum DbFetchStatus
    {
        Success,
        EndOfData,
        Error
    }

    /// <summary>
    /// Represents the result of a file operation.
    /// </summary>
    public record FileOperationResult(bool IsSuccess, string ErrorCode);

    /// <summary>
    /// Represents the result of an agent search.
    /// </summary>
    public record AgentSearchResult(bool IsSuccess, AgentRecord? Agent, string ErrorCode);

    #endregion

    #region Report Line Builder

    /// <summary>
    /// Helper class to build report lines and headers.
    /// </summary>
    public class ReportLineBuilder
    {
        /// <summary>
        /// Builds the main header line for the report.
        /// </summary>
        public string BuildMainHeaderLine(DateOnly processDate)
        {
            return $"{new string(' ', 30)}30 DAYS POLICY EXPIRY REPORT AS OF {processDate:yyyy/MM/dd}{new string(' ', 57)}";
        }

        /// <summary>
        /// Builds the state header line.
        /// </summary>
        public string BuildStateHeaderLine(string stateCode)
        {
            return $"{new string(' ', 3)}FOR THE STATE OF {stateCode}{new string(' ', 92)}";
        }

        /// <summary>
        /// Builds agent header lines.
        /// </summary>
        public IEnumerable<string> BuildAgentHeaderLines(AgentRecord agent)
        {
            yield return $"{new string(' ', 3)}AGENT: {agent.AgentCode} - {agent.AgentName}{new string(' ', 65)}";
            yield return $"{new string(' ', 10)}{agent.AgentAddress1}{new string(' ', 73)}";
            yield return $"{new string(' ', 10)}{agent.AgentAddress2}{new string(' ', 73)}";
            yield return $"{new string(' ', 10)}{agent.AgentCity}{new string(' ', 2)}{agent.AgentState}{new string(' ', 2)}{agent.AgentZipCode}{new string(' ', 73)}";
            yield return $"{new string(' ', 10)}{agent.AgentContactNo}{new string(' ', 2)}{agent.AgentEmail}{new string(' ', 81)}";
        }

        /// <summary>
        /// Builds policy header lines.
        /// </summary>
        public IEnumerable<string> BuildPolicyHeaderLines()
        {
            yield return $"{new string(' ', 10)}POLICY NO {new string(' ', 2)}HOLDER NAME{new string(' ', 2)}START DATE{new string(' ', 2)}EXPIRY DATE{new string(' ', 2)}PREMIUM{new string(' ', 1)}";
            yield return $"{new string(' ', 10)}POLICY NO {new string(' ', 2)}-----------{new string(' ', 2)}----------{new string(' ', 2)}-----------{new string(' ', 2)}-------{new string(' ', 1)}";
        }

        /// <summary>
        /// Builds a policy detail line.
        /// </summary>
        public string BuildPolicyDetailLine(PolicyRecord policy)
        {
            var holderName = $"{policy.HolderFirstName} {policy.HolderMiddleName} {policy.HolderLastName}".Trim();
            return $"{new string(' ', 10)}{policy.PolicyNumber}{new string(' ', 2)}{holderName,-73}{new string(' ', 2)}{policy.StartDate}{new string(' ', 2)}{policy.ExpiryDate}{new string(' ', 3)}{policy.PremiumAmount,10:N2}{new string(' ', 1)}";
        }

        /// <summary>
        /// Builds agent summary line.
        /// </summary>
        public string BuildAgentSummaryLine(string agentCode, int policyCount, decimal premium)
        {
            return $"{new string(' ', 3)}AGENT: {agentCode}{new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premium:N2}{new string(' ', 1)}";
        }

        /// <summary>
        /// Builds state summary line.
        /// </summary>
        public string BuildStateSummaryLine(string stateCode, int policyCount, decimal premium)
        {
            return $"{new string(' ', 3)}STATE: {stateCode}{new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premium:N2}{new string(' ', 69)}";
        }

        /// <summary>
        /// Builds grand summary line.
        /// </summary>
        public string BuildGrandSummaryLine(int policyCount, decimal premium)
        {
            return $"{new string(' ', 3)}GRAND SUMMARY: {new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premium:N2}{new string(' ', 69)}";
        }

        /// <summary>
        /// Builds a filler line (blank line).
        /// </summary>
        public string BuildFillerLine()
        {
            return new string(' ', 133);
        }
    }

    #endregion
}