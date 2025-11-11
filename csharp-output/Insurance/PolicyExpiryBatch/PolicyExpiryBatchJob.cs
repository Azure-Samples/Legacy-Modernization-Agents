using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace Insurance.PolicyExpiryBatch
{
    /// <summary>
    /// Main batch program for processing insurance policies expiring within 30 days.
    /// Notifies customers and agents, and generates summary reports.
    /// </summary>
    public class PolicyExpiryBatchJob
    {
        private readonly IDbDriver1 dbDriver1;
        private readonly IDbDriver2 dbDriver2;
        private readonly IFileDriver1 fileDriver1;
        private readonly IFileDriver2 fileDriver2;
        private readonly ILogger<PolicyExpiryBatchJob> logger;

        // Working storage fields
        private DateTime currentDate;
        private string currentState = string.Empty;
        private string currentAgent = string.Empty;

        private long agentTotalPolicyCount = 0;
        private decimal agentTotalPremium = 0m;
        private long stateTotalPolicyCount = 0;
        private decimal stateTotalPremium = 0m;
        private long grandTotalPolicyCount = 0;
        private decimal grandTotalPremium = 0m;

        /// <summary>
        /// Constructs the batch job with injected dependencies.
        /// </summary>
        public PolicyExpiryBatchJob(
            IDbDriver1 dbDriver1,
            IDbDriver2 dbDriver2,
            IFileDriver1 fileDriver1,
            IFileDriver2 fileDriver2,
            ILogger<PolicyExpiryBatchJob> logger)
        {
            this.dbDriver1 = dbDriver1;
            this.dbDriver2 = dbDriver2;
            this.fileDriver1 = fileDriver1;
            this.fileDriver2 = fileDriver2;
            this.logger = logger;
        }

        /// <summary>
        /// Entry point for the batch job.
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
                logger.LogCritical(ex, "Fatal error in PolicyExpiryBatchJob.");
                throw;
            }
        }

        /// <summary>
        /// Initializes database and file connections, sets up date, writes report header.
        /// </summary>
        private async Task InitializeAsync()
        {
            currentDate = DateTime.Now;

            // Open policy cursor
            var openPolicyResult = await dbDriver1.OpenCursorAsync(currentDate);
            if (!openPolicyResult.IsSuccess)
            {
                logger.LogError("Error opening policy cursor: {SqlCode}", openPolicyResult.SqlCode);
                throw new ApplicationException($"Error opening policy cursor: {openPolicyResult.SqlCode}");
            }

            // Open agent file
            var openAgentFileResult = await fileDriver1.OpenAsync();
            if (!openAgentFileResult.IsSuccess)
            {
                logger.LogError("Error opening agent file: {StatusCode}", openAgentFileResult.StatusCode);
                throw new ApplicationException($"Error opening agent file: {openAgentFileResult.StatusCode}");
            }

            // Open notification files
            foreach (var fileName in new[] { "CUSTOMER-NOTIFY-FILE", "NOTIFY-REPORT-FILE", "AGENT-NOTIFY-FILE" })
            {
                var openNotifyFileResult = await fileDriver2.OpenAsync(fileName);
                if (!openNotifyFileResult.IsSuccess)
                {
                    logger.LogError("Error opening notify file {FileName}: {StatusCode}", fileName, openNotifyFileResult.StatusCode);
                    throw new ApplicationException($"Error opening notify file {fileName}: {openNotifyFileResult.StatusCode}");
                }
            }

            await WriteReportHeaderAsync();
        }

        /// <summary>
        /// Main processing loop: fetches policies, notifies customers/agents, updates tracking, accumulates summaries.
        /// </summary>
        private async Task ProcessAsync()
        {
            bool noMorePolicy = false;

            while (!noMorePolicy)
            {
                var fetchResult = await dbDriver1.FetchNextPolicyAsync();
                switch (fetchResult.Status)
                {
                    case DbFetchStatus.EndOfData:
                        noMorePolicy = true;
                        break;
                    case DbFetchStatus.Success:
                        var policy = fetchResult.Policy!;
                        var agent = await GetAgentDetailAsync(policy.AgentCode);

                        await WriteCustomerNotificationAsync(policy, agent);
                        await UpdateTrackingAsync(policy);

                        await ProcessSummaryAsync(policy, agent);

                        break;
                    case DbFetchStatus.Error:
                        logger.LogError("Error fetching policy record: {SqlCode}", fetchResult.SqlCode);
                        throw new ApplicationException($"Error fetching policy record: {fetchResult.SqlCode}");
                }
            }
        }

        /// <summary>
        /// Finalizes the batch job: closes all files and database connections.
        /// </summary>
        private async Task FinalizeAsync()
        {
            // Close policy cursor
            var closePolicyResult = await dbDriver1.CloseCursorAsync();
            if (!closePolicyResult.IsSuccess)
            {
                logger.LogError("Error closing policy cursor: {SqlCode}", closePolicyResult.SqlCode);
                throw new ApplicationException($"Error closing policy cursor: {closePolicyResult.SqlCode}");
            }

            // Close agent file
            var closeAgentFileResult = await fileDriver1.CloseAsync();
            if (!closeAgentFileResult.IsSuccess)
            {
                logger.LogError("Error closing agent file: {StatusCode}", closeAgentFileResult.StatusCode);
                throw new ApplicationException($"Error closing agent file: {closeAgentFileResult.StatusCode}");
            }

            // Close notification files
            foreach (var fileName in new[] { "CUSTOMER-NOTIFY-FILE", "NOTIFY-REPORT-FILE", "AGENT-NOTIFY-FILE" })
            {
                var closeNotifyFileResult = await fileDriver2.CloseAsync(fileName);
                if (!closeNotifyFileResult.IsSuccess)
                {
                    logger.LogError("Error closing notify file {FileName}: {StatusCode}", fileName, closeNotifyFileResult.StatusCode);
                    throw new ApplicationException($"Error closing notify file {fileName}: {closeNotifyFileResult.StatusCode}");
                }
            }
        }

        /// <summary>
        /// Writes the main report header.
        /// </summary>
        private async Task WriteReportHeaderAsync()
        {
            await WriteBreakLineAsync();

            var header = ReportLineFormatter.FormatMainHeader(currentDate);
            await WriteNotificationReportAsync(header);

            await WriteBreakLineAsync();
        }

        /// <summary>
        /// Fetches agent details for the given agent code.
        /// </summary>
        private async Task<AgentRecord> GetAgentDetailAsync(string agentCode)
        {
            var searchResult = await fileDriver1.SearchAgentAsync(agentCode);
            if (!searchResult.IsSuccess)
            {
                logger.LogError("Error fetching agent record: {StatusCode}", searchResult.StatusCode);
                throw new ApplicationException($"Error fetching agent record: {searchResult.StatusCode}");
            }

            var agent = searchResult.Agent!;
            if (agent.Type == AgentType.Corporate)
            {
                await WriteAgentNotificationAsync(agent);
            }

            return agent;
        }

        /// <summary>
        /// Writes customer notification record to file.
        /// </summary>
        private async Task WriteCustomerNotificationAsync(PolicyRecord policy, AgentRecord agent)
        {
            var customerNotify = NotificationRecordFactory.CreateCustomerNotification(policy, agent, currentDate);

            var writeResult = await fileDriver2.WriteAsync("CUSTOMER-NOTIFY-FILE", customerNotify);
            if (!writeResult.IsSuccess)
            {
                logger.LogError("Error writing to customer notify file: {StatusCode}", writeResult.StatusCode);
            }
        }

        /// <summary>
        /// Writes agent notification record to file if agent is corporate.
        /// </summary>
        private async Task WriteAgentNotificationAsync(AgentRecord agent)
        {
            var agentNotify = NotificationRecordFactory.CreateAgentNotification(agent, currentDate);

            var writeResult = await fileDriver2.WriteAsync("AGENT-NOTIFY-FILE", agentNotify);
            if (!writeResult.IsSuccess)
            {
                logger.LogError("Error writing to agent notify file: {StatusCode}", writeResult.StatusCode);
            }
        }

        /// <summary>
        /// Inserts tracking record for processed policy.
        /// </summary>
        private async Task UpdateTrackingAsync(PolicyRecord policy)
        {
            var insertResult = await dbDriver2.InsertTrackingAsync(policy.Number, currentDate);
            if (!insertResult.IsSuccess)
            {
                logger.LogError("Error inserting into tracking: {SqlCode}", insertResult.SqlCode);
                throw new ApplicationException($"Error inserting into tracking: {insertResult.SqlCode}");
            }
        }

        /// <summary>
        /// Handles summary and break logic for state/agent, writes report lines, accumulates totals.
        /// </summary>
        private async Task ProcessSummaryAsync(PolicyRecord policy, AgentRecord agent)
        {
            bool stateBreak = !string.Equals(policy.HolderState, currentState, StringComparison.OrdinalIgnoreCase);
            bool agentBreak = !string.Equals(agent.Code, currentAgent, StringComparison.OrdinalIgnoreCase);

            if (stateBreak)
            {
                if (grandTotalPolicyCount > 0)
                {
                    await WriteAgentSummaryAsync(agent);
                    await WriteBreakLineAsync();
                    await WriteStateSummaryAsync(policy.HolderState);
                }
                ResetAgentTotals();
                ResetStateTotals();

                currentState = policy.HolderState;
                await WriteBreakLineAsync();
                await WriteStateHeaderAsync(currentState);
                await WriteBreakLineAsync();
                await WriteAgentHeaderAsync(agent);
                await WriteBreakLineAsync();
                await WritePolicyHeaderAsync();
            }
            else if (agentBreak)
            {
                await WriteAgentSummaryAsync(agent);
                ResetAgentTotals();

                currentAgent = agent.Code;
                await WriteBreakLineAsync();
                await WriteAgentHeaderAsync(agent);
                await WriteBreakLineAsync();
                await WritePolicyHeaderAsync();
            }

            await WritePolicyDetailLineAsync(policy, agent);

            agentTotalPolicyCount++;
            stateTotalPolicyCount++;
            grandTotalPolicyCount++;

            agentTotalPremium += policy.PremiumAmount;
            stateTotalPremium += policy.PremiumAmount;
            grandTotalPremium += policy.PremiumAmount;
        }

        /// <summary>
        /// Resets agent summary counters.
        /// </summary>
        private void ResetAgentTotals()
        {
            agentTotalPolicyCount = 0;
            agentTotalPremium = 0m;
        }

        /// <summary>
        /// Resets state summary counters.
        /// </summary>
        private void ResetStateTotals()
        {
            stateTotalPolicyCount = 0;
            stateTotalPremium = 0m;
        }

        /// <summary>
        /// Writes agent summary line to report.
        /// </summary>
        private async Task WriteAgentSummaryAsync(AgentRecord agent)
        {
            var summaryLine = ReportLineFormatter.FormatAgentSummary(agent.Code, agentTotalPolicyCount, agentTotalPremium);
            await WriteNotificationReportAsync(summaryLine);
        }

        /// <summary>
        /// Writes state summary line to report.
        /// </summary>
        private async Task WriteStateSummaryAsync(string stateCode)
        {
            var summaryLine = ReportLineFormatter.FormatStateSummary(stateCode, stateTotalPolicyCount, stateTotalPremium);
            await WriteNotificationReportAsync(summaryLine);
        }

        /// <summary>
        /// Writes grand summary line to report.
        /// </summary>
        private async Task WriteGrandSummaryAsync()
        {
            var summaryLine = ReportLineFormatter.FormatGrandSummary(grandTotalPolicyCount, grandTotalPremium);
            await WriteNotificationReportAsync(summaryLine);
        }

        /// <summary>
        /// Writes state header to report.
        /// </summary>
        private async Task WriteStateHeaderAsync(string stateCode)
        {
            var headerLine = ReportLineFormatter.FormatStateHeader(stateCode);
            await WriteNotificationReportAsync(headerLine);
        }

        /// <summary>
        /// Writes agent header lines to report.
        /// </summary>
        private async Task WriteAgentHeaderAsync(AgentRecord agent)
        {
            foreach (var line in ReportLineFormatter.FormatAgentHeader(agent))
            {
                await WriteNotificationReportAsync(line);
            }
        }

        /// <summary>
        /// Writes policy header lines to report.
        /// </summary>
        private async Task WritePolicyHeaderAsync()
        {
            foreach (var line in ReportLineFormatter.FormatPolicyHeader())
            {
                await WriteNotificationReportAsync(line);
            }
        }

        /// <summary>
        /// Writes detailed policy line to report.
        /// </summary>
        private async Task WritePolicyDetailLineAsync(PolicyRecord policy, AgentRecord agent)
        {
            var line = ReportLineFormatter.FormatPolicyDetail(policy);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes blank line to report for formatting.
        /// </summary>
        private async Task WriteBreakLineAsync()
        {
            var line = ReportLineFormatter.FormatBreakLine();
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes a line to the notification report file.
        /// </summary>
        private async Task WriteNotificationReportAsync(string reportLine)
        {
            var writeResult = await fileDriver2.WriteAsync("NOTIFY-REPORT-FILE", reportLine);
            if (!writeResult.IsSuccess)
            {
                logger.LogError("Error writing to notify report file: {StatusCode}", writeResult.StatusCode);
            }
        }
    }

    #region Data Models

    /// <summary>
    /// Represents a policy record.
    /// </summary>
    public record PolicyRecord(
        string Number,
        string HolderFName,
        string HolderMName,
        string HolderLName,
        string HolderAddr1,
        string HolderAddr2,
        string HolderCity,
        string HolderState,
        string HolderZipCode,
        DateTime StartDate,
        DateTime ExpiryDate,
        decimal PremiumAmount,
        string AgentCode,
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
        string ZipCode,
        string Email,
        string ContactNo,
        AgentType Type
    );

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
        string ZipCode,
        DateTime StartDate,
        DateTime ExpiryDate,
        DateTime NotifyDate,
        string NotifyMessage,
        string AgentCode,
        string AgentName,
        string Email,
        string BeneficiaryName,
        string StatutoryMessage
    );

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
        string ZipCode,
        string Email,
        string PolicyNumber,
        string PolicyHolderFName,
        string PolicyHolderMName,
        string PolicyHolderLName,
        DateTime PolicyStartDate,
        DateTime PolicyExpiryDate,
        DateTime NotifyDate,
        string NotifyMessage
    );

    /// <summary>
    /// Agent type enumeration.
    /// </summary>
    public enum AgentType
    {
        Individual,
        Corporate
    }

    #endregion

    #region Driver Interfaces

    /// <summary>
    /// Interface for DB driver 1 (policy cursor).
    /// </summary>
    public interface IDbDriver1
    {
        Task<DbOperationResult> OpenCursorAsync(DateTime processDate);
        Task<DbFetchResult> FetchNextPolicyAsync();
        Task<DbOperationResult> CloseCursorAsync();
    }

    /// <summary>
    /// Interface for DB driver 2 (tracking insert).
    /// </summary>
    public interface IDbDriver2
    {
        Task<DbOperationResult> InsertTrackingAsync(string policyNumber, DateTime processDate);
    }

    /// <summary>
    /// Interface for file driver 1 (agent file).
    /// </summary>
    public interface IFileDriver1
    {
        Task<FileOperationResult> OpenAsync();
        Task<FileAgentSearchResult> SearchAgentAsync(string agentCode);
        Task<FileOperationResult> CloseAsync();
    }

    /// <summary>
    /// Interface for file driver 2 (notification files).
    /// </summary>
    public interface IFileDriver2
    {
        Task<FileOperationResult> OpenAsync(string fileName);
        Task<FileOperationResult> WriteAsync(string fileName, object recordOrLine);
        Task<FileOperationResult> CloseAsync(string fileName);
    }

    #endregion

    #region Driver Results

    /// <summary>
    /// Represents the result of a DB operation.
    /// </summary>
    public record DbOperationResult(bool IsSuccess, int SqlCode);

    /// <summary>
    /// Represents the result of fetching a policy record.
    /// </summary>
    public record DbFetchResult(DbFetchStatus Status, PolicyRecord? Policy, int SqlCode);

    /// <summary>
    /// Status of DB fetch operation.
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
    public record FileOperationResult(bool IsSuccess, string StatusCode);

    /// <summary>
    /// Represents the result of searching for an agent in the file.
    /// </summary>
    public record FileAgentSearchResult(bool IsSuccess, AgentRecord? Agent, string StatusCode);

    #endregion

    #region Notification Record Factory

    /// <summary>
    /// Factory for creating notification records.
    /// </summary>
    public static class NotificationRecordFactory
    {
        /// <summary>
        /// Creates a customer notification record from policy and agent.
        /// </summary>
        public static CustomerNotificationRecord CreateCustomerNotification(
            PolicyRecord policy,
            AgentRecord agent,
            DateTime notifyDate)
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
                ZipCode: policy.HolderZipCode,
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
        /// Creates an agent notification record.
        /// </summary>
        public static AgentNotificationRecord CreateAgentNotification(
            AgentRecord agent,
            DateTime notifyDate)
        {
            // For demonstration, some fields are left blank as policy context is not provided.
            return new AgentNotificationRecord(
                AgentCode: agent.Code,
                AgentName: agent.Name,
                Address1: agent.Address1,
                Address2: agent.Address2,
                City: agent.City,
                State: agent.State,
                ZipCode: agent.ZipCode,
                Email: agent.Email,
                PolicyNumber: string.Empty,
                PolicyHolderFName: string.Empty,
                PolicyHolderMName: string.Empty,
                PolicyHolderLName: string.Empty,
                PolicyStartDate: DateTime.MinValue,
                PolicyExpiryDate: DateTime.MinValue,
                NotifyDate: notifyDate,
                NotifyMessage: "PLEASE NOTE CUSTOMER POLICY IS EXPIRING SOON"
            );
        }
    }

    #endregion

    #region Report Line Formatter

    /// <summary>
    /// Formats report lines and headers.
    /// </summary>
    public static class ReportLineFormatter
    {
        /// <summary>
        /// Formats the main report header.
        /// </summary>
        public static string FormatMainHeader(DateTime reportDate)
        {
            return $"{new string(' ', 30)}30 DAYS POLICY EXPIRY REPORT AS OF {reportDate:MM/dd/yyyy}{new string(' ', 57)}";
        }

        /// <summary>
        /// Formats a break (blank) line.
        /// </summary>
        public static string FormatBreakLine() => new string(' ', 133);

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
            yield return $"{new string(' ', 10)}{agent.City}{new string(' ', 2)}{agent.State}{new string(' ', 2)}{agent.ZipCode}{new string(' ', 73)}";
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
        /// Formats a detailed policy line.
        /// </summary>
        public static string FormatPolicyDetail(PolicyRecord policy)
        {
            var holderName = $"{policy.HolderFName} {policy.HolderMName} {policy.HolderLName}".Trim();
            return $"{new string(' ', 10)}{policy.Number}{new string(' ', 2)}{holderName,-73}{new string(' ', 2)}{policy.StartDate:MM/dd/yyyy}{new string(' ', 2)}{policy.ExpiryDate:MM/dd/yyyy}{new string(' ', 3)}{policy.PremiumAmount,10:C}{new string(' ', 1)}";
        }

        /// <summary>
        /// Formats agent summary line.
        /// </summary>
        public static string FormatAgentSummary(string agentCode, long policyCount, decimal premium)
        {
            return $"{new string(' ', 3)}AGENT: {agentCode}{new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premium:N2}{new string(' ', 1)}";
        }

        /// <summary>
        /// Formats state summary line.
        /// </summary>
        public static string FormatStateSummary(string stateCode, long policyCount, decimal premium)
        {
            return $"{new string(' ', 3)}STATE: {stateCode}{new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premium:N2}{new string(' ', 69)}";
        }

        /// <summary>
        /// Formats grand summary line.
        /// </summary>
        public static string FormatGrandSummary(long policyCount, decimal premium)
        {
            return $"{new string(' ', 3)}GRAND SUMMARY: {new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premium:N2}{new string(' ', 69)}";
        }
    }

    #endregion
}