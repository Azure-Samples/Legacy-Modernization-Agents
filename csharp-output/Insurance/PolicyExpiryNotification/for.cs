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
    /// Main program class for processing insurance policy expiry notifications.
    /// Reads policy and agent data, generates notification records, updates tracking, and produces summary reports.
    /// </summary>
    public class PolicyExpiryProcessor
    {
        private readonly IDbDriver1 dbDriver1;
        private readonly IDbDriver2 dbDriver2;
        private readonly IFileDriver1 fileDriver1;
        private readonly IFileDriver2 fileDriver2;
        private readonly ILogger<PolicyExpiryProcessor> logger;

        // Working storage variables
        private PolicyRecord? currentPolicy;
        private AgentRecord? currentAgent;
        private CustomerNotifyRecord customerNotifyRecord = new();
        private AgentNotifyRecord agentNotifyRecord = new();

        private string currentState = string.Empty;
        private string currentAgentCode = string.Empty;

        private int agentTotalPolicyCount = 0;
        private decimal agentTotalPremium = 0m;
        private int stateTotalPolicyCount = 0;
        private decimal stateTotalPremium = 0m;
        private int grandTotalPolicyCount = 0;
        private decimal grandTotalPremium = 0m;

        private bool noMorePolicy = false;
        private bool policyFound = false;

        /// <summary>
        /// Constructs the processor with required dependencies.
        /// </summary>
        public PolicyExpiryProcessor(
            IDbDriver1 dbDriver1,
            IDbDriver2 dbDriver2,
            IFileDriver1 fileDriver1,
            IFileDriver2 fileDriver2,
            ILogger<PolicyExpiryProcessor> logger)
        {
            this.dbDriver1 = dbDriver1;
            this.dbDriver2 = dbDriver2;
            this.fileDriver1 = fileDriver1;
            this.fileDriver2 = fileDriver2;
            this.logger = logger;
        }

        /// <summary>
        /// Main entry point for processing.
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
                logger.LogError(ex, "Fatal error in PolicyExpiryProcessor.");
                throw;
            }
        }

        /// <summary>
        /// Initializes drivers, opens files, sets up date fields, writes report header.
        /// </summary>
        private async Task InitializeAsync()
        {
            var currentDate = DateTime.Now;
            var wsCurrentDate = FormatCobolDate(currentDate);

            // Open DB cursor
            var dbOpenResult = await dbDriver1.OpenCursorAsync(wsCurrentDate);
            if (dbOpenResult.SqlCode != 0)
            {
                logger.LogError("Error opening DB cursor: {SqlCode}", dbOpenResult.SqlCode);
                throw new ApplicationException($"Error opening DB cursor: {dbOpenResult.SqlCode}");
            }

            // Open agent file
            var agentFileOpenResult = await fileDriver1.OpenAsync();
            if (agentFileOpenResult.StatusCode != "00")
            {
                logger.LogError("Error opening agent file: {StatusCode}", agentFileOpenResult.StatusCode);
                throw new ApplicationException($"Error opening agent file: {agentFileOpenResult.StatusCode}");
            }

            // Open notification files
            foreach (var fileName in new[] { "CUSTOMER-NOTIFY-FILE", "NOTIFY-REPORT-FILE", "AGENT-NOTIFY-FILE" })
            {
                var notifyFileOpenResult = await fileDriver2.OpenAsync(fileName);
                if (notifyFileOpenResult.StatusCode != "00")
                {
                    logger.LogError("Error opening file {FileName}: {StatusCode}", fileName, notifyFileOpenResult.StatusCode);
                    throw new ApplicationException($"Error opening file {fileName}: {notifyFileOpenResult.StatusCode}");
                }
            }

            await WriteReportHeaderAsync(wsCurrentDate);
        }

        /// <summary>
        /// Main processing loop: fetches policies, processes notifications and summaries.
        /// </summary>
        private async Task ProcessAsync()
        {
            while (!noMorePolicy)
            {
                var fetchResult = await dbDriver1.FetchPolicyAsync();
                await CheckPolicyCallStatusAsync(fetchResult.SqlCode);

                if (policyFound && fetchResult.Policy != null)
                {
                    currentPolicy = fetchResult.Policy;
                    currentAgent = await GetAgentDetailAsync(currentPolicy.AgentCode);

                    await WriteCustomerNotificationAsync();
                    await UpdateTrackingAsync();
                    await ProcessSummaryAsync();
                }
            }
        }

        /// <summary>
        /// Checks SQLCODE after policy fetch, sets flags or throws on error.
        /// </summary>
        private Task CheckPolicyCallStatusAsync(int sqlCode)
        {
            if (sqlCode == 100)
            {
                noMorePolicy = true;
                policyFound = false;
            }
            else if (sqlCode == 0)
            {
                policyFound = true;
            }
            else
            {
                logger.LogError("Error fetching record: {SqlCode}", sqlCode);
                throw new ApplicationException($"Error fetching record: {sqlCode}");
            }
            return Task.CompletedTask;
        }

        /// <summary>
        /// Fetches agent record for current policy, writes agent notification if found.
        /// </summary>
        private async Task<AgentRecord?> GetAgentDetailAsync(string agentCode)
        {
            var searchResult = await fileDriver1.SearchAgentAsync(agentCode);
            if (searchResult.StatusCode == "00" && searchResult.Agent != null)
            {
                await WriteAgentNotificationAsync(searchResult.Agent);
                return searchResult.Agent;
            }
            else
            {
                logger.LogError("Error fetching agent record: {StatusCode}", searchResult.StatusCode);
                throw new ApplicationException($"Error fetching agent record: {searchResult.StatusCode}");
            }
        }

        /// <summary>
        /// Inserts tracking record for processed policy in DB2.
        /// </summary>
        private async Task UpdateTrackingAsync()
        {
            if (currentPolicy == null)
                throw new InvalidOperationException("Current policy is null.");

            var wsCurrentDate = FormatCobolDate(DateTime.Now);
            var insertResult = await dbDriver2.InsertTrackingAsync(wsCurrentDate, currentPolicy.PolicyNumber);
            if (insertResult.SqlCode != 0)
            {
                logger.LogError("Error inserting into tracking table: {SqlCode}", insertResult.SqlCode);
                throw new ApplicationException($"Error inserting into tracking table: {insertResult.SqlCode}");
            }
        }

        /// <summary>
        /// Handles summary logic and report breaks for state/agent changes, updates counters and totals.
        /// </summary>
        private async Task ProcessSummaryAsync()
        {
            if (currentPolicy == null || currentAgent == null)
                throw new InvalidOperationException("Current policy or agent is null.");

            bool stateChanged = !string.Equals(currentPolicy.HolderState, currentState, StringComparison.OrdinalIgnoreCase);
            bool agentChanged = !string.Equals(currentAgent.AgentCode, currentAgentCode, StringComparison.OrdinalIgnoreCase);

            if (stateChanged)
            {
                if (grandTotalPolicyCount != 0)
                {
                    await WriteAgentSummaryAsync();
                    await WriteBreakLineAsync();
                    await WriteStateSummaryAsync();
                }
                ResetAgentTotals();
                ResetStateTotals();
                currentState = currentPolicy.HolderState;
                await WriteBreakLineAsync();
                await WriteStateHeaderAsync();
                await WriteBreakLineAsync();
                currentAgentCode = currentAgent.AgentCode;
                await WriteAgentHeaderAsync();
                await WriteBreakLineAsync();
                await WritePolicyHeaderAsync();
            }
            else if (agentChanged)
            {
                await WriteAgentSummaryAsync();
                ResetAgentTotals();
                currentAgentCode = currentAgent.AgentCode;
                await WriteBreakLineAsync();
                await WriteAgentHeaderAsync();
                await WriteBreakLineAsync();
                await WritePolicyHeaderAsync();
            }

            await WritePolicyDetailLineAsync();

            agentTotalPolicyCount++;
            stateTotalPolicyCount++;
            grandTotalPolicyCount++;

            agentTotalPremium += currentPolicy.PremiumAmount;
            stateTotalPremium += currentPolicy.PremiumAmount;
            grandTotalPremium += currentPolicy.PremiumAmount;
        }

        /// <summary>
        /// Resets agent policy count and premium totals.
        /// </summary>
        private void ResetAgentTotals()
        {
            agentTotalPolicyCount = 0;
            agentTotalPremium = 0m;
        }

        /// <summary>
        /// Resets state policy count and premium totals.
        /// </summary>
        private void ResetStateTotals()
        {
            stateTotalPolicyCount = 0;
            stateTotalPremium = 0m;
        }

        /// <summary>
        /// Populates customer notification record and writes to file.
        /// </summary>
        private async Task WriteCustomerNotificationAsync()
        {
            if (currentPolicy == null || currentAgent == null)
                throw new InvalidOperationException("Current policy or agent is null.");

            PopulateCustomerDetail(currentPolicy, currentAgent);

            var writeResult = await fileDriver2.WriteCustomerNotifyAsync(customerNotifyRecord);
            if (writeResult.StatusCode != "00")
            {
                logger.LogError("Error writing to customer notify file: {StatusCode}", writeResult.StatusCode);
            }
        }

        /// <summary>
        /// Populates agent notification record and writes to file if agent type is 'CORPORATE'.
        /// </summary>
        private async Task WriteAgentNotificationAsync(AgentRecord agent)
        {
            if (currentPolicy == null)
                throw new InvalidOperationException("Current policy is null.");

            PopulateAgentDetail(agent, currentPolicy);

            if (string.Equals(agent.AgentType, "CORPORATE", StringComparison.OrdinalIgnoreCase))
            {
                var writeResult = await fileDriver2.WriteAgentNotifyAsync(agentNotifyRecord);
                if (writeResult.StatusCode != "00")
                {
                    logger.LogError("Error writing to agent notify file: {StatusCode}", writeResult.StatusCode);
                }
            }
        }

        /// <summary>
        /// Writes notification report record to file.
        /// </summary>
        private async Task WriteNotificationReportAsync(string reportLine)
        {
            var writeResult = await fileDriver2.WriteReportAsync(reportLine);
            if (writeResult.StatusCode != "00")
            {
                logger.LogError("Error writing to notify report file: {StatusCode}", writeResult.StatusCode);
            }
        }

        /// <summary>
        /// Moves agent summary line to report record and writes it.
        /// </summary>
        private async Task WriteAgentSummaryAsync()
        {
            var line = ReportFormatter.FormatAgentSummaryLine(currentAgentCode, agentTotalPolicyCount, agentTotalPremium);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Moves state summary line to report record and writes it.
        /// </summary>
        private async Task WriteStateSummaryAsync()
        {
            var line = ReportFormatter.FormatStateSummaryLine(currentState, stateTotalPolicyCount, stateTotalPremium);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Moves grand summary line to report record and writes it.
        /// </summary>
        private async Task WriteGrandSummaryAsync()
        {
            var line = ReportFormatter.FormatGrandSummaryLine(grandTotalPolicyCount, grandTotalPremium);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Moves state header to report record and writes it.
        /// </summary>
        private async Task WriteStateHeaderAsync()
        {
            var line = ReportFormatter.FormatStateHeader(currentState);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Moves agent header lines to report record and writes them.
        /// </summary>
        private async Task WriteAgentHeaderAsync()
        {
            if (currentAgent == null)
                throw new InvalidOperationException("Current agent is null.");

            var lines = ReportFormatter.FormatAgentHeaderLines(currentAgent);
            foreach (var line in lines)
            {
                await WriteNotificationReportAsync(line);
            }
        }

        /// <summary>
        /// Moves policy header lines to report record and writes them.
        /// </summary>
        private async Task WritePolicyHeaderAsync()
        {
            var lines = ReportFormatter.FormatPolicyHeaderLines();
            foreach (var line in lines)
            {
                await WriteNotificationReportAsync(line);
            }
        }

        /// <summary>
        /// Moves policy details to report line and writes it.
        /// </summary>
        private async Task WritePolicyDetailLineAsync()
        {
            if (currentPolicy == null)
                throw new InvalidOperationException("Current policy is null.");

            var line = ReportFormatter.FormatPolicyDetailLine(currentPolicy);
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes a blank/filler line to the report.
        /// </summary>
        private async Task WriteBreakLineAsync()
        {
            var line = ReportFormatter.FormatFillerLine();
            await WriteNotificationReportAsync(line);
        }

        /// <summary>
        /// Writes the main report header.
        /// </summary>
        private async Task WriteReportHeaderAsync(string wsCurrentDate)
        {
            await WriteBreakLineAsync();
            var headerLine = ReportFormatter.FormatMainHeader(wsCurrentDate);
            await WriteNotificationReportAsync(headerLine);
            await WriteBreakLineAsync();
        }

        /// <summary>
        /// Populates customer notification record fields from policy and agent data.
        /// </summary>
        private void PopulateCustomerDetail(PolicyRecord policy, AgentRecord agent)
        {
            customerNotifyRecord.PolicyNumber = policy.PolicyNumber;
            customerNotifyRecord.FirstName = policy.HolderFirstName;
            customerNotifyRecord.MiddleName = policy.HolderMiddleName;
            customerNotifyRecord.LastName = policy.HolderLastName;
            customerNotifyRecord.Address1 = policy.HolderAddress1;
            customerNotifyRecord.Address2 = policy.HolderAddress2;
            customerNotifyRecord.City = policy.HolderCity;
            customerNotifyRecord.State = policy.HolderState;
            customerNotifyRecord.ZipCode = policy.HolderZipCode;
            customerNotifyRecord.StartDate = policy.StartDate;
            customerNotifyRecord.ExpiryDate = policy.ExpiryDate;
            customerNotifyRecord.NotifyDate = FormatCobolDate(DateTime.Now);
            customerNotifyRecord.BeneficiaryName = policy.BeneficiaryName;
            customerNotifyRecord.NotifyMessage = "PLEASE NOTE YOUR POLICY IS EXPIRING SOON. GET IT RENEWED TO CONTINUE COVERAGE";
            customerNotifyRecord.AgentCode = policy.AgentCode;
            customerNotifyRecord.AgentName = agent.AgentName;
            customerNotifyRecord.Email = policy.HolderEmail;
            customerNotifyRecord.StatutoryMessage = "IF YOU FAIL TO RENEW BY EXPIRY DATE YOUR INSURANCE COVERAGE WILL END";
        }

        /// <summary>
        /// Populates agent notification record fields from agent and policy data.
        /// </summary>
        private void PopulateAgentDetail(AgentRecord agent, PolicyRecord policy)
        {
            agentNotifyRecord.AgentCode = agent.AgentCode;
            agentNotifyRecord.AgentName = agent.AgentName;
            agentNotifyRecord.Address1 = agent.Address1;
            agentNotifyRecord.Address2 = agent.Address2;
            agentNotifyRecord.City = agent.City;
            agentNotifyRecord.State = agent.State;
            agentNotifyRecord.ZipCode = agent.ZipCode;
            agentNotifyRecord.Email = agent.Email;
            agentNotifyRecord.PolicyNumber = policy.PolicyNumber;
            agentNotifyRecord.PolicyHolderFirstName = policy.HolderFirstName;
            agentNotifyRecord.PolicyHolderMiddleName = policy.HolderMiddleName;
            agentNotifyRecord.PolicyHolderLastName = policy.HolderLastName;
            agentNotifyRecord.PolicyStartDate = policy.StartDate;
            agentNotifyRecord.PolicyExpiryDate = policy.ExpiryDate;
            agentNotifyRecord.NotifyDate = FormatCobolDate(DateTime.Now);
            agentNotifyRecord.NotifyMessage = "PLEASE NOTE CUSTOMER POLICY IS EXPIRING SOON";
        }

        /// <summary>
        /// Finalizes processing: closes DB cursor and all files, checks for errors.
        /// </summary>
        private async Task FinalizeAsync()
        {
            // Close DB cursor
            var dbCloseResult = await dbDriver1.CloseCursorAsync();
            if (dbCloseResult.SqlCode != 0)
            {
                logger.LogError("Error closing DB cursor: {SqlCode}", dbCloseResult.SqlCode);
                throw new ApplicationException($"Error closing DB cursor: {dbCloseResult.SqlCode}");
            }

            // Close agent file
            var agentFileCloseResult = await fileDriver1.CloseAsync();
            if (agentFileCloseResult.StatusCode != "00")
            {
                logger.LogError("Error closing agent file: {StatusCode}", agentFileCloseResult.StatusCode);
                throw new ApplicationException($"Error closing agent file: {agentFileCloseResult.StatusCode}");
            }

            // Close notification files
            foreach (var fileName in new[] { "CUSTOMER-NOTIFY-FILE", "NOTIFY-REPORT-FILE", "AGENT-NOTIFY-FILE" })
            {
                var notifyFileCloseResult = await fileDriver2.CloseAsync(fileName);
                if (notifyFileCloseResult.StatusCode != "00")
                {
                    logger.LogError("Error closing file {FileName}: {StatusCode}", fileName, notifyFileCloseResult.StatusCode);
                    throw new ApplicationException($"Error closing file {fileName}: {notifyFileCloseResult.StatusCode}");
                }
            }
        }

        /// <summary>
        /// Formats a DateTime as COBOL-style date string (MM/DD/YYYY).
        /// </summary>
        private static string FormatCobolDate(DateTime date)
        {
            return date.ToString("MM/dd/yyyy", CultureInfo.InvariantCulture);
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
        public string HolderEmail { get; init; } = string.Empty;
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
        public string Address1 { get; init; } = string.Empty;
        public string Address2 { get; init; } = string.Empty;
        public string City { get; init; } = string.Empty;
        public string State { get; init; } = string.Empty;
        public string ZipCode { get; init; } = string.Empty;
        public string Email { get; init; } = string.Empty;
        public string ContactNumber { get; init; } = string.Empty;
        public string AgentType { get; init; } = string.Empty;
    }

    /// <summary>
    /// Represents a customer notification record.
    /// </summary>
    public class CustomerNotifyRecord
    {
        public string PolicyNumber { get; set; } = string.Empty;
        public string FirstName { get; set; } = string.Empty;
        public string MiddleName { get; set; } = string.Empty;
        public string LastName { get; set; } = string.Empty;
        public string Address1 { get; set; } = string.Empty;
        public string Address2 { get; set; } = string.Empty;
        public string City { get; set; } = string.Empty;
        public string State { get; set; } = string.Empty;
        public string ZipCode { get; set; } = string.Empty;
        public string StartDate { get; set; } = string.Empty;
        public string ExpiryDate { get; set; } = string.Empty;
        public string NotifyDate { get; set; } = string.Empty;
        public string NotifyMessage { get; set; } = string.Empty;
        public string AgentCode { get; set; } = string.Empty;
        public string AgentName { get; set; } = string.Empty;
        public string Email { get; set; } = string.Empty;
        public string BeneficiaryName { get; set; } = string.Empty;
        public string StatutoryMessage { get; set; } = string.Empty;
    }

    /// <summary>
    /// Represents an agent notification record.
    /// </summary>
    public class AgentNotifyRecord
    {
        public string AgentCode { get; set; } = string.Empty;
        public string AgentName { get; set; } = string.Empty;
        public string Address1 { get; set; } = string.Empty;
        public string Address2 { get; set; } = string.Empty;
        public string City { get; set; } = string.Empty;
        public string State { get; set; } = string.Empty;
        public string ZipCode { get; set; } = string.Empty;
        public string Email { get; set; } = string.Empty;
        public string PolicyNumber { get; set; } = string.Empty;
        public string PolicyHolderFirstName { get; set; } = string.Empty;
        public string PolicyHolderMiddleName { get; set; } = string.Empty;
        public string PolicyHolderLastName { get; set; } = string.Empty;
        public string PolicyStartDate { get; set; } = string.Empty;
        public string PolicyExpiryDate { get; set; } = string.Empty;
        public string NotifyDate { get; set; } = string.Empty;
        public string NotifyMessage { get; set; } = string.Empty;
    }

    #endregion

    #region Driver Interfaces

    /// <summary>
    /// Interface for DB driver 1 (policy cursor).
    /// </summary>
    public interface IDbDriver1
    {
        Task<DbDriverResult> OpenCursorAsync(string processDate);
        Task<DbDriverFetchResult> FetchPolicyAsync();
        Task<DbDriverResult> CloseCursorAsync();
    }

    /// <summary>
    /// Interface for DB driver 2 (tracking insert).
    /// </summary>
    public interface IDbDriver2
    {
        Task<DbDriverResult> InsertTrackingAsync(string processDate, string policyNumber);
    }

    /// <summary>
    /// Interface for file driver 1 (agent file).
    /// </summary>
    public interface IFileDriver1
    {
        Task<FileDriverResult> OpenAsync();
        Task<FileDriverAgentSearchResult> SearchAgentAsync(string agentCode);
        Task<FileDriverResult> CloseAsync();
    }

    /// <summary>
    /// Interface for file driver 2 (customer notify, agent notify, report).
    /// </summary>
    public interface IFileDriver2
    {
        Task<FileDriverResult> OpenAsync(string fileName);
        Task<FileDriverResult> WriteCustomerNotifyAsync(CustomerNotifyRecord record);
        Task<FileDriverResult> WriteAgentNotifyAsync(AgentNotifyRecord record);
        Task<FileDriverResult> WriteReportAsync(string reportLine);
        Task<FileDriverResult> CloseAsync(string fileName);
    }

    /// <summary>
    /// Result for DB driver operations.
    /// </summary>
    public record DbDriverResult(int SqlCode);

    /// <summary>
    /// Result for DB driver fetch operation.
    /// </summary>
    public record DbDriverFetchResult(int SqlCode, PolicyRecord? Policy);

    /// <summary>
    /// Result for file driver operations.
    /// </summary>
    public record FileDriverResult(string StatusCode);

    /// <summary>
    /// Result for file driver agent search.
    /// </summary>
    public record FileDriverAgentSearchResult(string StatusCode, AgentRecord? Agent);

    #endregion

    #region Report Formatting

    /// <summary>
    /// Helper class for formatting report lines.
    /// </summary>
    public static class ReportFormatter
    {
        /// <summary>
        /// Formats the main report header line.
        /// </summary>
        public static string FormatMainHeader(string reportDate)
        {
            return $"{new string(' ', 30)}30 DAYS POLICY EXPIRY REPORT AS OF {reportDate}{new string(' ', 57)}";
        }

        /// <summary>
        /// Formats the state header line.
        /// </summary>
        public static string FormatStateHeader(string stateCode)
        {
            return $"{new string(' ', 3)}FOR THE STATE OF {stateCode}{new string(' ', 92)}";
        }

        /// <summary>
        /// Formats agent header lines.
        /// </summary>
        public static IEnumerable<string> FormatAgentHeaderLines(AgentRecord agent)
        {
            yield return $"{new string(' ', 3)}AGENT: {agent.AgentCode} - {agent.AgentName}{new string(' ', 65)}";
            yield return $"{new string(' ', 10)}{agent.Address1}{new string(' ', Math.Max(0, 50 - agent.Address1.Length))}{new string(' ', 73)}";
            yield return $"{new string(' ', 10)}{agent.Address2}{new string(' ', Math.Max(0, 50 - agent.Address2.Length))}{new string(' ', 73)}";
            yield return $"{new string(' ', 10)}{agent.City}{new string(' ', Math.Max(0, 20 - agent.City.Length))}{new string(' ', 2)}{agent.State}{new string(' ', 2)}{agent.ZipCode}{new string(' ', 73)}";
            yield return $"{new string(' ', 10)}{agent.ContactNumber}{new string(' ', 2)}{agent.Email}{new string(' ', Math.Max(0, 30 - agent.Email.Length))}{new string(' ', 81)}";
        }

        /// <summary>
        /// Formats policy header lines.
        /// </summary>
        public static IEnumerable<string> FormatPolicyHeaderLines()
        {
            yield return $"{new string(' ', 10)}POLICY NO {new string(' ', 2)}HOLDER NAME{new string(' ', 2)}START DATE{new string(' ', 2)}EXPIRY DATE{new string(' ', 2)}PREMIUM{new string(' ', 1)}";
            yield return $"{new string(' ', 10)}POLICY NO {new string(' ', 2)}-----------{new string(' ', 2)}----------{new string(' ', 2)}-----------{new string(' ', 2)}-------{new string(' ', 1)}";
        }

        /// <summary>
        /// Formats a policy detail line.
        /// </summary>
        public static string FormatPolicyDetailLine(PolicyRecord policy)
        {
            var holderName = $"{policy.HolderFirstName} {policy.HolderMiddleName} {policy.HolderLastName}".Trim();
            return $"{new string(' ', 10)}{policy.PolicyNumber}{new string(' ', 2)}{holderName}{new string(' ', Math.Max(0, 73 - holderName.Length))}{new string(' ', 2)}{policy.StartDate}{new string(' ', 2)}{policy.ExpiryDate}{new string(' ', 3)}{policy.PremiumAmount.ToString("F2", CultureInfo.InvariantCulture)}{new string(' ', 1)}";
        }

        /// <summary>
        /// Formats agent summary line.
        /// </summary>
        public static string FormatAgentSummaryLine(string agentCode, int policyCount, decimal premiumTotal)
        {
            return $"{new string(' ', 3)}AGENT: {agentCode}{new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premiumTotal:N2}{new string(' ', 1)}";
        }

        /// <summary>
        /// Formats state summary line.
        /// </summary>
        public static string FormatStateSummaryLine(string stateCode, int policyCount, decimal premiumTotal)
        {
            return $"{new string(' ', 3)}STATE: {stateCode}{new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premiumTotal:N2}{new string(' ', 69)}";
        }

        /// <summary>
        /// Formats grand summary line.
        /// </summary>
        public static string FormatGrandSummaryLine(int policyCount, decimal premiumTotal)
        {
            return $"{new string(' ', 3)}GRAND SUMMARY: {new string(' ', 2)}POLICY COUNT: {policyCount:N0}{new string(' ', 2)}POLICY PREMIUM: {premiumTotal:N2}{new string(' ', 69)}";
        }

        /// <summary>
        /// Formats a filler line (blank line).
        /// </summary>
        public static string FormatFillerLine()
        {
            return new string(' ', 133);
        }
    }

    #endregion
}