using System;
using System.IO;
using System.Threading.Tasks;
using System.Linq;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace NotificationFileDriver
{
    /// <summary>
    /// Represents a notification record for an agent.
    /// </summary>
    public record AgentNotifyRecord(
        string AgentCode,
        string AgentName,
        string AgentAddress1,
        string AgentAddress2,
        string AgentCity,
        string AgentState,
        string AgentPolicyNumber,
        string AgentPolicyFName,
        string AgentPolicyMName,
        string AgentPolicyLName,
        string AgentPolicyStartDate,
        string AgentPolicyExpiryDate,
        string AgentNotifyDate,
        string AgentNotifyMessages
    );

    /// <summary>
    /// Represents a notification record for a customer.
    /// </summary>
    public record CustomerNotifyRecord(
        string PolicyNumber,
        string FirstName,
        string MiddleName,
        string LastName,
        string PolicyStartDate,
        string PolicyExpiryDate,
        string NotifyDate,
        string NotifyMessages,
        string AgentCode,
        string AgentName,
        string StatutoryMessage
    );

    /// <summary>
    /// Represents a report record.
    /// </summary>
    public record NotifyReportRecord(
        string ReportLine
    );

    /// <summary>
    /// Enum representing supported file types.
    /// </summary>
    public enum NotifyFileType
    {
        AgentNotify,
        CustomerNotify,
        NotifyReport,
        Unknown
    }

    /// <summary>
    /// Enum representing supported file operations.
    /// </summary>
    public enum FileOperationType
    {
        Open,
        Close,
        Write,
        Unknown
    }

    /// <summary>
    /// Provides file driver operations for notification files.
    /// </summary>
    public interface INotificationFileDriver
    {
        /// <summary>
        /// Executes a file operation asynchronously.
        /// </summary>
        /// <param name="fileName">Logical file name (e.g., "AGENT-NOTIFY-FILE").</param>
        /// <param name="operationType">Operation type ("OPEN", "CLOSE", "WRITE").</param>
        /// <param name="agentRecord">Agent record (if applicable).</param>
        /// <param name="customerRecord">Customer record (if applicable).</param>
        /// <param name="reportRecord">Report record (if applicable).</param>
        /// <returns>Operation status code ("00" for success, "99" for error, or file status code).</returns>
        Task<string> ExecuteAsync(
            string fileName,
            string operationType,
            AgentNotifyRecord? agentRecord = null,
            CustomerNotifyRecord? customerRecord = null,
            NotifyReportRecord? reportRecord = null
        );
    }

    /// <summary>
    /// Implements file driver operations for notification files.
    /// </summary>
    public class NotificationFileDriver : INotificationFileDriver
    {
        private readonly ILogger<NotificationFileDriver> _logger;

        // File paths (could be injected/configured)
        private const string AgentFilePhysicalName = "AGENTFLE";
        private const string CustomerFilePhysicalName = "CUSTFLE";
        private const string ReportFilePhysicalName = "RPTFLE";

        // File streams
        private FileStream? _agentFileStream;
        private FileStream? _customerFileStream;
        private FileStream? _reportFileStream;

        /// <summary>
        /// Initializes a new instance of <see cref="NotificationFileDriver"/>.
        /// </summary>
        /// <param name="logger">Logger instance.</param>
        public NotificationFileDriver(ILogger<NotificationFileDriver> logger)
        {
            _logger = logger;
        }

        /// <inheritdoc />
        public async Task<string> ExecuteAsync(
            string fileName,
            string operationType,
            AgentNotifyRecord? agentRecord = null,
            CustomerNotifyRecord? customerRecord = null,
            NotifyReportRecord? reportRecord = null)
        {
            var fileType = ParseFileType(fileName);
            var opType = ParseOperationType(operationType);

            try
            {
                switch (opType)
                {
                    case FileOperationType.Open:
                        await OpenFileAsync(fileType);
                        return "00";
                    case FileOperationType.Close:
                        await CloseFileAsync(fileType);
                        return "00";
                    case FileOperationType.Write:
                        await WriteRecordAsync(fileType, agentRecord, customerRecord, reportRecord);
                        return "00";
                    default:
                        _logger.LogError("Unsupported operation type: {OperationType}", operationType);
                        return "99";
                }
            }
            catch (IOException ioEx)
            {
                _logger.LogError(ioEx, "I/O error during {OperationType} on {FileName}", operationType, fileName);
                return "98"; // Custom code for I/O error
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error during {OperationType} on {FileName}", operationType, fileName);
                return "99";
            }
        }

        /// <summary>
        /// Opens the specified file for output.
        /// </summary>
        private async Task OpenFileAsync(NotifyFileType fileType)
        {
            switch (fileType)
            {
                case NotifyFileType.AgentNotify:
                    _agentFileStream = new FileStream(AgentFilePhysicalName, FileMode.Append, FileAccess.Write, FileShare.Read);
                    break;
                case NotifyFileType.CustomerNotify:
                    _customerFileStream = new FileStream(CustomerFilePhysicalName, FileMode.Append, FileAccess.Write, FileShare.Read);
                    break;
                case NotifyFileType.NotifyReport:
                    _reportFileStream = new FileStream(ReportFilePhysicalName, FileMode.Append, FileAccess.Write, FileShare.Read);
                    break;
                default:
                    throw new InvalidOperationException("Unknown file type for open operation.");
            }
            await Task.CompletedTask;
        }

        /// <summary>
        /// Closes the specified file.
        /// </summary>
        private async Task CloseFileAsync(NotifyFileType fileType)
        {
            switch (fileType)
            {
                case NotifyFileType.AgentNotify:
                    _agentFileStream?.Dispose();
                    _agentFileStream = null;
                    break;
                case NotifyFileType.CustomerNotify:
                    _customerFileStream?.Dispose();
                    _customerFileStream = null;
                    break;
                case NotifyFileType.NotifyReport:
                    _reportFileStream?.Dispose();
                    _reportFileStream = null;
                    break;
                default:
                    throw new InvalidOperationException("Unknown file type for close operation.");
            }
            await Task.CompletedTask;
        }

        /// <summary>
        /// Writes a record to the specified file.
        /// </summary>
        private async Task WriteRecordAsync(
            NotifyFileType fileType,
            AgentNotifyRecord? agentRecord,
            CustomerNotifyRecord? customerRecord,
            NotifyReportRecord? reportRecord)
        {
            switch (fileType)
            {
                case NotifyFileType.AgentNotify:
                    if (agentRecord is null)
                        throw new ArgumentNullException(nameof(agentRecord));
                    await WriteAgentRecordAsync(agentRecord);
                    break;
                case NotifyFileType.CustomerNotify:
                    if (customerRecord is null)
                        throw new ArgumentNullException(nameof(customerRecord));
                    await WriteCustomerRecordAsync(customerRecord);
                    break;
                case NotifyFileType.NotifyReport:
                    if (reportRecord is null)
                        throw new ArgumentNullException(nameof(reportRecord));
                    await WriteReportRecordAsync(reportRecord);
                    break;
                default:
                    throw new InvalidOperationException("Unknown file type for write operation.");
            }
        }

        /// <summary>
        /// Writes an agent notification record to the agent file.
        /// </summary>
        private async Task WriteAgentRecordAsync(AgentNotifyRecord record)
        {
            if (_agentFileStream is null)
                throw new InvalidOperationException("Agent file is not open.");

            var line = string.Join('|', new[]
            {
                record.AgentCode,
                record.AgentName,
                record.AgentAddress1,
                record.AgentAddress2,
                record.AgentCity,
                record.AgentState,
                record.AgentPolicyNumber,
                record.AgentPolicyFName,
                record.AgentPolicyMName,
                record.AgentPolicyLName,
                record.AgentPolicyStartDate,
                record.AgentPolicyExpiryDate,
                record.AgentNotifyDate,
                record.AgentNotifyMessages
            });

            var bytes = System.Text.Encoding.UTF8.GetBytes(line + Environment.NewLine);
            await _agentFileStream.WriteAsync(bytes, 0, bytes.Length);
            await _agentFileStream.FlushAsync();
        }

        /// <summary>
        /// Writes a customer notification record to the customer file.
        /// </summary>
        private async Task WriteCustomerRecordAsync(CustomerNotifyRecord record)
        {
            if (_customerFileStream is null)
                throw new InvalidOperationException("Customer file is not open.");

            var line = string.Join('|', new[]
            {
                record.PolicyNumber,
                record.FirstName,
                record.MiddleName,
                record.LastName,
                record.PolicyStartDate,
                record.PolicyExpiryDate,
                record.NotifyDate,
                record.NotifyMessages,
                record.AgentCode,
                record.AgentName,
                record.StatutoryMessage
            });

            var bytes = System.Text.Encoding.UTF8.GetBytes(line + Environment.NewLine);
            await _customerFileStream.WriteAsync(bytes, 0, bytes.Length);
            await _customerFileStream.FlushAsync();
        }

        /// <summary>
        /// Writes a report record to the report file.
        /// </summary>
        private async Task WriteReportRecordAsync(NotifyReportRecord record)
        {
            if (_reportFileStream is null)
                throw new InvalidOperationException("Report file is not open.");

            var line = record.ReportLine;
            var bytes = System.Text.Encoding.UTF8.GetBytes(line + Environment.NewLine);
            await _reportFileStream.WriteAsync(bytes, 0, bytes.Length);
            await _reportFileStream.FlushAsync();
        }

        /// <summary>
        /// Parses the logical file name to a <see cref="NotifyFileType"/>.
        /// </summary>
        private static NotifyFileType ParseFileType(string fileName) =>
            fileName switch
            {
                "AGENT-NOTIFY-FILE" => NotifyFileType.AgentNotify,
                "CUSTOMER-NOTIFY-FILE" => NotifyFileType.CustomerNotify,
                "NOTIFY-REPORT-FILE" => NotifyFileType.NotifyReport,
                _ => NotifyFileType.Unknown
            };

        /// <summary>
        /// Parses the operation type string to a <see cref="FileOperationType"/>.
        /// </summary>
        private static FileOperationType ParseOperationType(string operationType) =>
            operationType?.ToUpperInvariant() switch
            {
                "OPEN" => FileOperationType.Open,
                "CLOSE" => FileOperationType.Close,
                "WRITE" => FileOperationType.Write,
                _ => FileOperationType.Unknown
            };
    }

    /// <summary>
    /// Example of configuring and using the NotificationFileDriver with dependency injection.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// Entry point for demonstration.
        /// </summary>
        public static async Task Main(string[] args)
        {
            // Setup DI and logging
            var serviceCollection = new ServiceCollection();
            serviceCollection.AddLogging(configure => configure.AddConsole());
            serviceCollection.AddTransient<INotificationFileDriver, NotificationFileDriver>();
            var serviceProvider = serviceCollection.BuildServiceProvider();

            var driver = serviceProvider.GetRequiredService<INotificationFileDriver>();

            // Example usage
            var agentRecord = new AgentNotifyRecord(
                AgentCode: "A123456789",
                AgentName: "John Doe",
                AgentAddress1: "123 Main St",
                AgentAddress2: "Suite 100",
                AgentCity: "Copenhagen",
                AgentState: "DK",
                AgentPolicyNumber: "P987654321",
                AgentPolicyFName: "Jane",
                AgentPolicyMName: "M",
                AgentPolicyLName: "Smith",
                AgentPolicyStartDate: "2024-01-01",
                AgentPolicyExpiryDate: "2025-01-01",
                AgentNotifyDate: "2024-06-01",
                AgentNotifyMessages: "Policy renewal notification."
            );

            // Open file
            var openStatus = await driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            Console.WriteLine($"Open Status: {openStatus}");

            // Write record
            var writeStatus = await driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: agentRecord);
            Console.WriteLine($"Write Status: {writeStatus}");

            // Close file
            var closeStatus = await driver.ExecuteAsync("AGENT-NOTIFY-FILE", "CLOSE");
            Console.WriteLine($"Close Status: {closeStatus}");
        }
    }
}