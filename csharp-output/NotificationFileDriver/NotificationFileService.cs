using System;
using System.IO;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace NotificationFileDriver
{
    /// <summary>
    /// Represents the type of file operation to perform.
    /// </summary>
    public enum FileOperationType
    {
        Open,
        Close,
        Write
    }

    /// <summary>
    /// Represents the result of a file operation.
    /// </summary>
    public record FileOperationResult(string StatusCode, string? ErrorMessage = null);

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
        string CustPolicyNumber,
        string CustFName,
        string CustMName,
        string CustLName,
        string CustPolicyStartDate,
        string CustPolicyExpiryDate,
        string CustNotifyDate,
        string CustNotifyMessages,
        string CustAgentCode,
        string CustAgentName,
        string CustStatutoryMessage
    );

    /// <summary>
    /// Represents a report record.
    /// </summary>
    public record NotifyReportRecord(
        string ReportLine
    );

    /// <summary>
    /// Interface for file operations abstraction.
    /// </summary>
    public interface INotificationFileService
    {
        Task<FileOperationResult> OpenAsync(string fileName);
        Task<FileOperationResult> CloseAsync(string fileName);
        Task<FileOperationResult> WriteAgentAsync(string fileName, AgentNotifyRecord record);
        Task<FileOperationResult> WriteCustomerAsync(string fileName, CustomerNotifyRecord record);
        Task<FileOperationResult> WriteReportAsync(string fileName, NotifyReportRecord record);
    }

    /// <summary>
    /// Implements file operations for notification files.
    /// </summary>
    public class NotificationFileService : INotificationFileService
    {
        private readonly ILogger<NotificationFileService> _logger;
        private readonly object _fileLock = new();

        // File streams for each file type
        private FileStream? _agentFileStream;
        private FileStream? _customerFileStream;
        private FileStream? _reportFileStream;

        /// <summary>
        /// Initializes a new instance of <see cref="NotificationFileService"/>.
        /// </summary>
        public NotificationFileService(ILogger<NotificationFileService> logger)
        {
            _logger = logger;
        }

        /// <inheritdoc/>
        public async Task<FileOperationResult> OpenAsync(string fileName)
        {
            try
            {
                lock (_fileLock)
                {
                    switch (fileName)
                    {
                        case "AGENT-NOTIFY-FILE":
                            _agentFileStream = new FileStream("AGENTFLE", FileMode.Append, FileAccess.Write, FileShare.None);
                            break;
                        case "CUSTOMER-NOTIFY-FILE":
                            _customerFileStream = new FileStream("CUSTFLE", FileMode.Append, FileAccess.Write, FileShare.None);
                            break;
                        case "NOTIFY-REPORT-FILE":
                            _reportFileStream = new FileStream("RPTFLE", FileMode.Append, FileAccess.Write, FileShare.None);
                            break;
                        default:
                            return new FileOperationResult("99", $"Unknown file name: {fileName}");
                    }
                }
                return new FileOperationResult("00");
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error opening file {FileName}", fileName);
                return new FileOperationResult("99", ex.Message);
            }
        }

        /// <inheritdoc/>
        public async Task<FileOperationResult> CloseAsync(string fileName)
        {
            try
            {
                lock (_fileLock)
                {
                    switch (fileName)
                    {
                        case "AGENT-NOTIFY-FILE":
                            _agentFileStream?.Dispose();
                            _agentFileStream = null;
                            break;
                        case "CUSTOMER-NOTIFY-FILE":
                            _customerFileStream?.Dispose();
                            _customerFileStream = null;
                            break;
                        case "NOTIFY-REPORT-FILE":
                            _reportFileStream?.Dispose();
                            _reportFileStream = null;
                            break;
                        default:
                            return new FileOperationResult("99", $"Unknown file name: {fileName}");
                    }
                }
                return new FileOperationResult("00");
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error closing file {FileName}", fileName);
                return new FileOperationResult("99", ex.Message);
            }
        }

        /// <inheritdoc/>
        public async Task<FileOperationResult> WriteAgentAsync(string fileName, AgentNotifyRecord record)
        {
            try
            {
                lock (_fileLock)
                {
                    if (_agentFileStream == null)
                        return new FileOperationResult("98", "Agent file not open.");

                    var line = string.Concat(
                        record.AgentCode.PadRight(10),
                        record.AgentName.PadRight(45),
                        record.AgentAddress1.PadRight(50),
                        record.AgentAddress2.PadRight(50),
                        record.AgentCity.PadRight(20),
                        record.AgentState.PadRight(2),
                        record.AgentPolicyNumber.PadRight(10),
                        record.AgentPolicyFName.PadRight(35),
                        record.AgentPolicyMName.PadRight(1),
                        record.AgentPolicyLName.PadRight(35),
                        record.AgentPolicyStartDate.PadRight(10),
                        record.AgentPolicyExpiryDate.PadRight(10),
                        record.AgentNotifyDate.PadRight(10),
                        record.AgentNotifyMessages.PadRight(100)
                    );

                    var bytes = System.Text.Encoding.UTF8.GetBytes(line + Environment.NewLine);
                    _agentFileStream.Write(bytes, 0, bytes.Length);
                    _agentFileStream.Flush();
                }
                return new FileOperationResult("00");
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error writing agent record to file {FileName}", fileName);
                return new FileOperationResult("99", ex.Message);
            }
        }

        /// <inheritdoc/>
        public async Task<FileOperationResult> WriteCustomerAsync(string fileName, CustomerNotifyRecord record)
        {
            try
            {
                lock (_fileLock)
                {
                    if (_customerFileStream == null)
                        return new FileOperationResult("98", "Customer file not open.");

                    var line = string.Concat(
                        record.CustPolicyNumber.PadRight(10),
                        record.CustFName.PadRight(35),
                        record.CustMName.PadRight(1),
                        record.CustLName.PadRight(35),
                        record.CustPolicyStartDate.PadRight(10),
                        record.CustPolicyExpiryDate.PadRight(10),
                        record.CustNotifyDate.PadRight(10),
                        record.CustNotifyMessages.PadRight(100),
                        record.CustAgentCode.PadRight(10),
                        record.CustAgentName.PadRight(45),
                        record.CustStatutoryMessage.PadRight(100)
                    );

                    var bytes = System.Text.Encoding.UTF8.GetBytes(line + Environment.NewLine);
                    _customerFileStream.Write(bytes, 0, bytes.Length);
                    _customerFileStream.Flush();
                }
                return new FileOperationResult("00");
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error writing customer record to file {FileName}", fileName);
                return new FileOperationResult("99", ex.Message);
            }
        }

        /// <inheritdoc/>
        public async Task<FileOperationResult> WriteReportAsync(string fileName, NotifyReportRecord record)
        {
            try
            {
                lock (_fileLock)
                {
                    if (_reportFileStream == null)
                        return new FileOperationResult("98", "Report file not open.");

                    var line = record.ReportLine.PadRight(133);
                    var bytes = System.Text.Encoding.UTF8.GetBytes(line + Environment.NewLine);
                    _reportFileStream.Write(bytes, 0, bytes.Length);
                    _reportFileStream.Flush();
                }
                return new FileOperationResult("00");
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error writing report record to file {FileName}", fileName);
                return new FileOperationResult("99", ex.Message);
            }
        }
    }

    /// <summary>
    /// Main driver class for notification file operations.
    /// </summary>
    public class NotificationFileDriver
    {
        private readonly INotificationFileService _fileService;
        private readonly ILogger<NotificationFileDriver> _logger;

        /// <summary>
        /// Initializes a new instance of <see cref="NotificationFileDriver"/>.
        /// </summary>
        public NotificationFileDriver(INotificationFileService fileService, ILogger<NotificationFileDriver> logger)
        {
            _fileService = fileService;
            _logger = logger;
        }

        /// <summary>
        /// Executes the requested file operation.
        /// </summary>
        /// <param name="fileName">Logical file name (e.g., "AGENT-NOTIFY-FILE").</param>
        /// <param name="operationType">Operation type ("OPEN", "CLOSE", "WRITE").</param>
        /// <param name="agentRecord">Agent record (if applicable).</param>
        /// <param name="customerRecord">Customer record (if applicable).</param>
        /// <param name="reportRecord">Report record (if applicable).</param>
        /// <returns>File operation result.</returns>
        public async Task<FileOperationResult> ExecuteAsync(
            string fileName,
            string operationType,
            AgentNotifyRecord? agentRecord = null,
            CustomerNotifyRecord? customerRecord = null,
            NotifyReportRecord? reportRecord = null)
        {
            FileOperationType? opType = operationType.ToUpperInvariant() switch
            {
                "OPEN" => FileOperationType.Open,
                "CLOSE" => FileOperationType.Close,
                "WRITE" => FileOperationType.Write,
                _ => null
            };

            if (opType == null)
            {
                _logger.LogError("Invalid operation type: {OperationType}", operationType);
                return new FileOperationResult("99", $"Invalid operation type: {operationType}");
            }

            try
            {
                switch (opType)
                {
                    case FileOperationType.Open:
                        return await _fileService.OpenAsync(fileName);

                    case FileOperationType.Close:
                        return await _fileService.CloseAsync(fileName);

                    case FileOperationType.Write:
                        if (fileName == "AGENT-NOTIFY-FILE" && agentRecord != null)
                            return await _fileService.WriteAgentAsync(fileName, agentRecord);

                        if (fileName == "CUSTOMER-NOTIFY-FILE" && customerRecord != null)
                            return await _fileService.WriteCustomerAsync(fileName, customerRecord);

                        if (fileName == "NOTIFY-REPORT-FILE" && reportRecord != null)
                            return await _fileService.WriteReportAsync(fileName, reportRecord);

                        _logger.LogError("Write operation missing record for file: {FileName}", fileName);
                        return new FileOperationResult("99", $"Missing record for file: {fileName}");

                    default:
                        _logger.LogError("Unknown operation type: {OperationType}", operationType);
                        return new FileOperationResult("99", $"Unknown operation type: {operationType}");
                }
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error executing operation {OperationType} on file {FileName}", operationType, fileName);
                return new FileOperationResult("99", ex.Message);
            }
        }
    }

    /// <summary>
    /// Example of dependency injection setup and usage.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// Entry point for demonstration purposes.
        /// </summary>
        public static async Task Main(string[] args)
        {
            // Setup DI and logging
            var serviceCollection = new ServiceCollection();
            serviceCollection.AddLogging(configure => configure.AddConsole());
            serviceCollection.AddSingleton<INotificationFileService, NotificationFileService>();
            serviceCollection.AddSingleton<NotificationFileDriver>();

            var serviceProvider = serviceCollection.BuildServiceProvider();
            var driver = serviceProvider.GetRequiredService<NotificationFileDriver>();

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

            var resultOpen = await driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            Console.WriteLine($"Open Result: {resultOpen.StatusCode} {resultOpen.ErrorMessage}");

            var resultWrite = await driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: agentRecord);
            Console.WriteLine($"Write Result: {resultWrite.StatusCode} {resultWrite.ErrorMessage}");

            var resultClose = await driver.ExecuteAsync("AGENT-NOTIFY-FILE", "CLOSE");
            Console.WriteLine($"Close Result: {resultClose.StatusCode} {resultClose.ErrorMessage}");
        }
    }
}