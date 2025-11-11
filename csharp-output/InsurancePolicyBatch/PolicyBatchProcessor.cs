using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace InsurancePolicyBatch
{
    /// <summary>
    /// Represents the main batch processor for insurance policy expiry notifications and reporting.
    /// </summary>
    public class PolicyBatchProcessor
    {
        private readonly IDbPolicyDriver _dbPolicyDriver;
        private readonly IDbTrackingDriver _dbTrackingDriver;
        private readonly IAgentFileDriver _agentFileDriver;
        private readonly INotificationFileDriver _notificationFileDriver;
        private readonly ILogger<PolicyBatchProcessor> _logger;

        // Working storage variables
        private DateTime _currentDate;
        private string _currentState = string.Empty;
        private string _currentAgent = string.Empty;

        private long _agentTotalPolicyCount = 0;
        private decimal _agentTotalPremium = 0m;
        private long _stateTotalPolicyCount = 0;
        private decimal _stateTotalPremium = 0m;
        private long _grandTotalPolicyCount = 0;
        private decimal _grandTotalPremium = 0m;

        /// <summary>
        /// Constructs the batch processor with required dependencies.
        /// </summary>
        public PolicyBatchProcessor(
            IDbPolicyDriver dbPolicyDriver,
            IDbTrackingDriver dbTrackingDriver,
            IAgentFileDriver agentFileDriver,
            INotificationFileDriver notificationFileDriver,
            ILogger<PolicyBatchProcessor> logger)
        {
            _dbPolicyDriver = dbPolicyDriver;
            _dbTrackingDriver = dbTrackingDriver;
            _agentFileDriver = agentFileDriver;
            _notificationFileDriver = notificationFileDriver;
            _logger = logger;
        }

        /// <summary>
        /// Entry point for batch processing.
        /// </summary>
        public async Task RunAsync()
        {
            try
            {
                await InitializeAsync();
                await ProcessAsync();
                await FinalizeAsync();
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Fatal error in batch processing.");
                throw;
            }
        }

        /// <summary>
        /// Initializes drivers, files, and writes report header.
        /// </summary>
        private async Task InitializeAsync()
        {
            _currentDate = DateTime.Now;

            // Open DB2 policy cursor
            var dbOpenResult = await _dbPolicyDriver.OpenCursorAsync(_currentDate);
            if (!dbOpenResult.IsSuccess)
            {
                _logger.LogError("Error opening policy cursor: {SqlCode}", dbOpenResult.SqlCode);
                throw new ApplicationException($"Error opening policy cursor: {dbOpenResult.SqlCode}");
            }

            // Open agent file
            var agentOpenResult = await _agentFileDriver.OpenAsync();
            if (!agentOpenResult.IsSuccess)
            {
                _logger.LogError("Error opening agent file: {StatusCode}", agentOpenResult.StatusCode);
                throw new ApplicationException($"Error opening agent file: {agentOpenResult.StatusCode}");
            }

            // Open notification files
            foreach (var fileType in new[] { NotificationFileType.CustomerNotify, NotificationFileType.NotifyReport, NotificationFileType.AgentNotify })
            {
                var notifyOpenResult = await _notificationFileDriver.OpenAsync(fileType);
                if (!notifyOpenResult.IsSuccess)
                {
                    _logger.LogError("Error opening notification file {FileType}: {StatusCode}", fileType, notifyOpenResult.StatusCode);
                    throw new ApplicationException($"Error opening notification file {fileType}: {notifyOpenResult.StatusCode}");
                }
            }

            await WriteReportHeaderAsync();
        }

        /// <summary>
        /// Main processing loop: fetches policies, processes notifications and summaries.
        /// </summary>
        private async Task ProcessAsync()
        {
            bool noMorePolicy = false;

            await _dbPolicyDriver.SetFetchModeAsync();

            while (!noMorePolicy)
            {
                var fetchResult = await _dbPolicyDriver.FetchNextAsync();
                switch (fetchResult.Status)
                {
                    case DbPolicyFetchStatus.EndOfData:
                        noMorePolicy = true;
                        break;
                    case DbPolicyFetchStatus.Success:
                        var policy = fetchResult.PolicyRecord;
                        var agent = await GetAgentDetailAsync(policy.AgentCode);

                        await WriteCustomerNotificationAsync(policy, agent);
                        await UpdateTrackingAsync(policy);

                        await ProcessSummaryAsync(policy, agent);

                        break;
                    case DbPolicyFetchStatus.Error:
                        _logger.LogError("Error fetching policy record: {SqlCode}", fetchResult.SqlCode);
                        throw new ApplicationException($"Error fetching policy record: {fetchResult.SqlCode}");
                }
            }
        }

        /// <summary>
        /// Finalizes processing: closes all files and DB2 cursor.
        /// </summary>
        private async Task FinalizeAsync()
        {
            var dbCloseResult = await _dbPolicyDriver.CloseCursorAsync();
            if (!dbCloseResult.IsSuccess)
            {
                _logger.LogError("Error closing policy cursor: {SqlCode}", dbCloseResult.SqlCode);
                throw new ApplicationException($"Error closing policy cursor: {dbCloseResult.SqlCode}");
            }

            var agentCloseResult = await _agentFileDriver.CloseAsync();
            if (!agentCloseResult.IsSuccess)
            {
                _logger.LogError("Error closing agent file: {StatusCode}", agentCloseResult.StatusCode);
                throw new ApplicationException($"Error closing agent file: {agentCloseResult.StatusCode}");
            }

            foreach (var fileType in new[] { NotificationFileType.CustomerNotify, NotificationFileType.NotifyReport, NotificationFileType.AgentNotify })
            {
                var notifyCloseResult = await _notificationFileDriver.CloseAsync(fileType);
                if (!notifyCloseResult.IsSuccess)
                {
                    _logger.LogError("Error closing notification file {FileType}: {StatusCode}", fileType, notifyCloseResult.StatusCode);
                    throw new ApplicationException($"Error closing notification file {fileType}: {notifyCloseResult.StatusCode}");
                }
            }
        }

        /// <summary>
        /// Retrieves agent details for a given agent code.
        /// </summary>
        private async Task<AgentRecord> GetAgentDetailAsync(string agentCode)
        {
            var searchResult = await _agentFileDriver.SearchAsync(agentCode);
            if (!searchResult.IsSuccess)
            {
                _logger.LogError("Error fetching agent record: {StatusCode}", searchResult.StatusCode);
                throw new ApplicationException($"Error fetching agent record: {searchResult.StatusCode}");
            }

            var agent = searchResult.AgentRecord;
            if (agent.Type == AgentType.Corporate)
            {
                await WriteAgentNotificationAsync(agent);
            }

            return agent;
        }

        /// <summary>
        /// Updates tracking for a processed policy.
        /// </summary>
        private async Task UpdateTrackingAsync(PolicyRecord policy)
        {
            var insertResult = await _dbTrackingDriver.InsertAsync(policy.Number, _currentDate);
            if (!insertResult.IsSuccess)
            {
                _logger.LogError("Error inserting into tracking: {SqlCode}", insertResult.SqlCode);
                throw new ApplicationException($"Error inserting into tracking: {insertResult.SqlCode}");
            }
        }

        /// <summary>
        /// Processes summary logic and writes report lines.
        /// </summary>
        private async Task ProcessSummaryAsync(PolicyRecord policy, AgentRecord agent)
        {
            // State break
            if (!string.Equals(policy.HolderState, _currentState, StringComparison.OrdinalIgnoreCase))
            {
                if (_grandTotalPolicyCount != 0)
                {
                    await WriteAgentSummaryAsync(agent);
                    await WriteBreakLineAsync();
                    await WriteStateSummaryAsync(policy.HolderState);
                }
                ResetAgentTotals();
                ResetStateTotals();
                _currentState = policy.HolderState;
                await WriteBreakLineAsync();
                await WriteStateHeaderAsync(_currentState);
                await WriteBreakLineAsync();
                await WriteAgentHeaderAsync(agent);
                await WriteBreakLineAsync();
                await WritePolicyHeaderAsync();
            }
            // Agent break
            else if (!string.Equals(agent.Code, _currentAgent, StringComparison.OrdinalIgnoreCase))
            {
                await WriteAgentSummaryAsync(agent);
                ResetAgentTotals();
                _currentAgent = agent.Code;
                await WriteBreakLineAsync();
                await WriteAgentHeaderAsync(agent);
                await WriteBreakLineAsync();
                await WritePolicyHeaderAsync();
            }

            await WritePolicyDetailLineAsync(policy, agent);

            _agentTotalPolicyCount++;
            _stateTotalPolicyCount++;
            _grandTotalPolicyCount++;

            _agentTotalPremium += policy.PremiumAmount;
            _stateTotalPremium += policy.PremiumAmount;
            _grandTotalPremium += policy.PremiumAmount;
        }

        /// <summary>
        /// Resets agent summary counters.
        /// </summary>
        private void ResetAgentTotals()
        {
            _agentTotalPolicyCount = 0;
            _agentTotalPremium = 0m;
        }

        /// <summary>
        /// Resets state summary counters.
        /// </summary>
        private void ResetStateTotals()
        {
            _stateTotalPolicyCount = 0;
            _stateTotalPremium = 0m;
        }

        /// <summary>
        /// Writes the main report header.
        /// </summary>
        private async Task WriteReportHeaderAsync()
        {
            await WriteBreakLineAsync();
            var header = ReportLineFormatter.FormatMainHeader(_currentDate);
            await WriteNotifyReportAsync(header);
            await WriteBreakLineAsync();
        }

        /// <summary>
        /// Writes a break (blank) line to the report.
        /// </summary>
        private async Task WriteBreakLineAsync()
        {
            await WriteNotifyReportAsync(ReportLineFormatter.FormatBlankLine());
        }

        /// <summary>
        /// Writes the state header line.
        /// </summary>
        private async Task WriteStateHeaderAsync(string stateCode)
        {
            var header = ReportLineFormatter.FormatStateHeader(stateCode);
            await WriteNotifyReportAsync(header);
        }

        /// <summary>
        /// Writes the agent header lines.
        /// </summary>
        private async Task WriteAgentHeaderAsync(AgentRecord agent)
        {
            foreach (var line in ReportLineFormatter.FormatAgentHeader(agent))
            {
                await WriteNotifyReportAsync(line);
            }
        }

        /// <summary>
        /// Writes the policy header lines.
        /// </summary>
        private async Task WritePolicyHeaderAsync()
        {
            foreach (var line in ReportLineFormatter.FormatPolicyHeader())
            {
                await WriteNotifyReportAsync(line);
            }
        }

        /// <summary>
        /// Writes a policy detail line.
        /// </summary>
        private async Task WritePolicyDetailLineAsync(PolicyRecord policy, AgentRecord agent)
        {
            var line = ReportLineFormatter.FormatPolicyDetailLine(policy, agent);
            await WriteNotifyReportAsync(line);
        }

        /// <summary>
        /// Writes the agent summary line.
        /// </summary>
        private async Task WriteAgentSummaryAsync(AgentRecord agent)
        {
            var line = ReportLineFormatter.FormatAgentSummary(agent.Code, _agentTotalPolicyCount, _agentTotalPremium);
            await WriteNotifyReportAsync(line);
        }

        /// <summary>
        /// Writes the state summary line.
        /// </summary>
        private async Task WriteStateSummaryAsync(string stateCode)
        {
            var line = ReportLineFormatter.FormatStateSummary(stateCode, _stateTotalPolicyCount, _stateTotalPremium);
            await WriteNotifyReportAsync(line);
        }

        /// <summary>
        /// Writes the grand summary line.
        /// </summary>
        private async Task WriteGrandSummaryAsync()
        {
            var line = ReportLineFormatter.FormatGrandSummary(_grandTotalPolicyCount, _grandTotalPremium);
            await WriteNotifyReportAsync(line);
        }

        /// <summary>
        /// Writes a line to the notification report file.
        /// </summary>
        private async Task WriteNotifyReportAsync(string reportLine)
        {
            var writeResult = await _notificationFileDriver.WriteAsync(NotificationFileType.NotifyReport, reportLine);
            if (!writeResult.IsSuccess)
            {
                _logger.LogError("Error writing to notify report file: {StatusCode}", writeResult.StatusCode);
                // Not throwing here, as original COBOL only displayed error
            }
        }

        /// <summary>
        /// Populates and writes customer notification record.
        /// </summary>
        private async Task WriteCustomerNotificationAsync(PolicyRecord policy, AgentRecord agent)
        {
            var customerNotify = CustomerNotificationRecord.CreateFromPolicy(policy, agent, _currentDate);
            var writeResult = await _notificationFileDriver.WriteAsync(NotificationFileType.CustomerNotify, customerNotify.ToFileLine());
            if (!writeResult.IsSuccess)
            {
                _logger.LogError("Error writing to customer notify file: {StatusCode}", writeResult.StatusCode);
                // Not throwing here, as original COBOL only displayed error
            }
        }

        /// <summary>
        /// Populates and writes agent notification record (for corporate agents).
        /// </summary>
        private async Task WriteAgentNotificationAsync(AgentRecord agent)
        {
            var agentNotify = AgentNotificationRecord.CreateFromAgent(agent, _currentDate);
            var writeResult = await _notificationFileDriver.WriteAsync(NotificationFileType.AgentNotify, agentNotify.ToFileLine());
            if (!writeResult.IsSuccess)
            {
                _logger.LogError("Error writing to agent notify file: {StatusCode}", writeResult.StatusCode);
                // Not throwing here, as original COBOL only displayed error
            }
        }
    }

    #region Interfaces for Drivers

    /// <summary>
    /// Interface for DB2 policy driver.
    /// </summary>
    public interface IDbPolicyDriver
    {
        Task<DbOperationResult> OpenCursorAsync(DateTime processDate);
        Task SetFetchModeAsync();
        Task<DbPolicyFetchResult> FetchNextAsync();
        Task<DbOperationResult> CloseCursorAsync();
    }

    /// <summary>
    /// Interface for DB2 tracking driver.
    /// </summary>
    public interface IDbTrackingDriver
    {
        Task<DbOperationResult> InsertAsync(string policyNumber, DateTime processDate);
    }

    /// <summary>
    /// Interface for agent file driver.
    /// </summary>
    public interface IAgentFileDriver
    {
        Task<FileOperationResult> OpenAsync();
        Task<FileSearchResult> SearchAsync(string agentCode);
        Task<FileOperationResult> CloseAsync();
    }

    /// <summary>
    /// Interface for notification file driver.
    /// </summary>
    public interface INotificationFileDriver
    {
        Task<FileOperationResult> OpenAsync(NotificationFileType fileType);
        Task<FileOperationResult> WriteAsync(NotificationFileType fileType, string recordLine);
        Task<FileOperationResult> CloseAsync(NotificationFileType fileType);
    }

    #endregion

    #region Data Models

    /// <summary>
    /// Represents a policy record.
    /// </summary>
    public record PolicyRecord(
        string Number,
        string AgentCode,
        string HolderFName,
        string HolderMName,
        string HolderLName,
        string HolderAddr1,
        string HolderAddr2,
        string HolderCity,
        string HolderState,
        string HolderZip,
        DateTime StartDate,
        DateTime ExpiryDate,
        decimal PremiumAmount,
        string BeneficiaryName
    );

    /// <summary>
    /// Represents an agent record.
    /// </summary>
    public record AgentRecord(
        string Code,
        string Name,
        string Address1,
        string Address2,
        string City,
        string State,
        string Zip,
        string Email,
        string ContactNo,
        AgentType Type
    );

    /// <summary>
    /// Enum for agent type.
    /// </summary>
    public enum AgentType
    {
        Individual,
        Corporate
    }

    /// <summary>
    /// Represents a customer notification record.
    /// </summary>
    public record CustomerNotificationRecord(
        string PolicyNumber,
        string FirstName,
        string MiddleName,
        string LastName,
        string Address1,
        string Address2,
        string City,
        string State,
        string Zip,
        DateTime StartDate,
        DateTime ExpiryDate,
        DateTime NotifyDate,
        string NotifyMessage,
        string AgentCode,
        string AgentName,
        string Email,
        string BeneficiaryName,
        string StatutoryMessage
    )
    {
        /// <summary>
        /// Creates a customer notification record from policy and agent.
        /// </summary>
        public static CustomerNotificationRecord CreateFromPolicy(PolicyRecord policy, AgentRecord agent, DateTime notifyDate)
        {
            return new CustomerNotificationRecord(
                PolicyNumber: policy.Number,
                FirstName: policy.HolderFName,
                MiddleName: policy.HolderMName,
                LastName: policy.HolderLName,
                Address1: policy.HolderAddr1,
                Address2: policy.HolderAddr2,
                City: policy.HolderCity,
                State: policy.HolderState,
                Zip: policy.HolderZip,
                StartDate: policy.StartDate,
                ExpiryDate: policy.ExpiryDate,
                NotifyDate: notifyDate,
                NotifyMessage: "PLEASE NOTE YOUR POLICY IS EXPIRING SOON. GET IT RENEWED TO CONTINUE COVERAGE",
                AgentCode: policy.AgentCode,
                AgentName: agent.Name,
                Email: agent.Email,
                BeneficiaryName: policy.BeneficiaryName,
                StatutoryMessage: "IF YOU FAIL TO RENEW BY EXPIRY DATE YOUR INSURANCE COVERAGE WILL END"
            );
        }

        /// <summary>
        /// Formats the record for file output.
        /// </summary>
        public string ToFileLine()
        {
            // Format as CSV or fixed-width as needed
            return string.Join(",",
                PolicyNumber, FirstName, MiddleName, LastName, Address1, Address2, City, State, Zip,
                StartDate.ToString("yyyy-MM-dd"), ExpiryDate.ToString("yyyy-MM-dd"), NotifyDate.ToString("yyyy-MM-dd"),
                NotifyMessage, AgentCode, AgentName, Email, BeneficiaryName, StatutoryMessage);
        }
    }

    /// <summary>
    /// Represents an agent notification record.
    /// </summary>
    public record AgentNotificationRecord(
        string AgentCode,
        string AgentName,
        string Address1,
        string Address2,
        string City,
        string State,
        string Zip,
        string Email,
        string PolicyNumber,
        string PolicyHolderFName,
        string PolicyHolderMName,
        string PolicyHolderLName,
        DateTime PolicyStartDate,
        DateTime PolicyExpiryDate,
        DateTime NotifyDate,
        string NotifyMessage
    )
    {
        /// <summary>
        /// Creates an agent notification record from agent and current date.
        /// </summary>
        public static AgentNotificationRecord CreateFromAgent(AgentRecord agent, DateTime notifyDate)
        {
            // Policy fields would be filled in real code; here, placeholders
            return new AgentNotificationRecord(
                AgentCode: agent.Code,
                AgentName: agent.Name,
                Address1: agent.Address1,
                Address2: agent.Address2,
                City: agent.City,
                State: agent.State,
                Zip: agent.Zip,
                Email: agent.Email,
                PolicyNumber: string.Empty,
                PolicyHolderFName: string.Empty,
                PolicyHolderMName: string.Empty,
                PolicyHolderLName: string.Empty,
                PolicyStartDate: notifyDate,
                PolicyExpiryDate: notifyDate,
                NotifyDate: notifyDate,
                NotifyMessage: "PLEASE NOTE CUSTOMER POLICY IS EXPIRING SOON"
            );
        }

        /// <summary>
        /// Formats the record for file output.
        /// </summary>
        public string ToFileLine()
        {
            return string.Join(",",
                AgentCode, AgentName, Address1, Address2, City, State, Zip, Email,
                PolicyNumber, PolicyHolderFName, PolicyHolderMName, PolicyHolderLName,
                PolicyStartDate.ToString("yyyy-MM-dd"), PolicyExpiryDate.ToString("yyyy-MM-dd"),
                NotifyDate.ToString("yyyy-MM-dd"), NotifyMessage);
        }
    }

    #endregion

    #region Driver Results and Enums

    /// <summary>
    /// Represents the result of a DB operation.
    /// </summary>
    public record DbOperationResult(bool IsSuccess, int SqlCode);

    /// <summary>
    /// Represents the result of a policy fetch.
    /// </summary>
    public record DbPolicyFetchResult(DbPolicyFetchStatus Status, PolicyRecord? PolicyRecord, int SqlCode);

    /// <summary>
    /// Enum for DB policy fetch status.
    /// </summary>
    public enum DbPolicyFetchStatus
    {
        Success,
        EndOfData,
        Error
    }

    /// <summary>
    /// Represents the result of a file operation.
    /// </summary>
    public record FileOperationResult(bool IsSuccess, string StatusCode);

    /// <summary>
    /// Represents the result of an agent file search.
    /// </summary>
    public record FileSearchResult(bool IsSuccess, AgentRecord AgentRecord, string StatusCode);

    /// <summary>
    /// Enum for notification file types.
    /// </summary>
    public enum NotificationFileType
    {
        CustomerNotify,
        AgentNotify,
        NotifyReport
    }

    #endregion

    #region Report Line Formatter

    /// <summary>
    /// Helper class for formatting report lines.
    /// </summary>
    public static class ReportLineFormatter
    {
        /// <summary>
        /// Formats the main report header.
        /// </summary>
        public static string FormatMainHeader(DateTime reportDate)
        {
            return $"{new string(' ', 30)}30 DAYS POLICY EXPIRY REPORT AS OF {reportDate:yyyy/MM/dd}{new string(' ', 57)}";
        }

        /// <summary>
        /// Formats a blank line.
        /// </summary>
        public static string FormatBlankLine() => new string(' ', 133);

        /// <summary>
        /// Formats the state header.
        /// </summary>
        public static string FormatStateHeader(string stateCode)
        {
            return $"{new string(' ', 3)}FOR THE STATE OF {stateCode}{new string(' ', 92)}";
        }

        /// <summary>
        /// Formats agent header lines.
        /// </summary>
        public static IEnumerable<string> FormatAgentHeader(AgentRecord agent)
        {
            yield return $"{new string(' ', 3)}AGENT: {agent.Code} - {agent.Name}{new string(' ', 65)}";
            yield return $"{new string(' ', 10)}{agent.Address1}{new string(' ', 73)}";
            yield return $"{new string(' ', 10)}{agent.Address2}{new string(' ', 73)}";
            yield return $"{new string(' ', 10)}{agent.City}{new string(' ', 2)}{agent.State}{new string(' ', 2)}{agent.Zip}{new string(' ', 73)}";
            yield return $"{new string(' ', 10)}{agent.ContactNo}{new string(' ', 2)}{agent.Email}{new string(' ', 81)}";
        }

        /// <summary>
        /// Formats policy header lines.
        /// </summary>
        public static IEnumerable<string> FormatPolicyHeader()
        {
            yield return $"{new string(' ', 10)}POLICY NO {new string(' ', 2)}HOLDER NAME{new string(' ', 2)}START DATE{new string(' ', 2)}EXPIRY DATE{new string(' ', 2)}PREMIUM{new string(' ', 1)}";
            yield return $"{new string(' ', 10)}POLICY NO {new string(' ', 2)}-----------{new string(' ', 2)}----------{new string(' ', 2)}-----------{new string(' ', 2)}-------{new string(' ', 1)}";
        }

        /// <summary>
        /// Formats a policy detail line.
        /// </summary>
        public static string FormatPolicyDetailLine(PolicyRecord policy, AgentRecord agent)
        {
            var holderName = $"{policy.HolderFName} {policy.HolderMName} {policy.HolderLName}".Trim();
            return $"{new string(' ', 10)}{policy.Number}{new string(' ', 2)}{holderName,-73}{new string(' ', 2)}{policy.StartDate:yyyy-MM-dd}{new string(' ', 2)}{policy.ExpiryDate:yyyy-MM-dd}{new string(' ', 3)}{policy.PremiumAmount,10:C}{new string(' ', 1)}";
        }

        /// <summary>
        /// Formats the agent summary line.
        /// </summary>
        public static string FormatAgentSummary(string agentCode, long policyCount, decimal premiumTotal)
        {
            return $"{new string(' ', 3)}AGENT: {agentCode}{new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premiumTotal:N2}{new string(' ', 1)}";
        }

        /// <summary>
        /// Formats the state summary line.
        /// </summary>
        public static string FormatStateSummary(string stateCode, long policyCount, decimal premiumTotal)
        {
            return $"{new string(' ', 3)}STATE: {stateCode}{new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premiumTotal:N2}{new string(' ', 69)}";
        }

        /// <summary>
        /// Formats the grand summary line.
        /// </summary>
        public static string FormatGrandSummary(long policyCount, decimal premiumTotal)
        {
            return $"{new string(' ', 3)}GRAND SUMMARY: {new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premiumTotal:N2}{new string(' ', 69)}";
        }
    }

    #endregion

    #region Program Entry Point

    /// <summary>
    /// Program entry point.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// Main method.
        /// </summary>
        public static async Task Main(string[] args)
        {
            // Setup DI container
            var serviceProvider = new ServiceCollection()
                .AddLogging(configure => configure.AddConsole())
                // .AddSingleton<IDbPolicyDriver, DbPolicyDriver>() // Implementations required
                // .AddSingleton<IDbTrackingDriver, DbTrackingDriver>()
                // .AddSingleton<IAgentFileDriver, AgentFileDriver>()
                // .AddSingleton<INotificationFileDriver, NotificationFileDriver>()
                .AddSingleton<PolicyBatchProcessor>()
                .BuildServiceProvider();

            var processor = serviceProvider.GetRequiredService<PolicyBatchProcessor>();
            await processor.RunAsync();
        }
    }

    #endregion
}