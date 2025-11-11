using System;
using System.IO;
using System.Threading.Tasks;
using System.Text;
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
    /// Represents the supported notification file types.
    /// </summary>
    public enum NotificationFileType
    {
        AgentNotify,
        CustomerNotify,
        NotifyReport
    }

    /// <summary>
    /// Represents a record for agent notification.
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
    /// Represents a record for customer notification.
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
    /// Represents a record for notification report.
    /// </summary>
    public record NotifyReportRecord(
        string ReportLine
    );

    /// <summary>
    /// Encapsulates the parameters for a file operation request.
    /// </summary>
    public class FileOperationRequest
    {
        /// <summary>
        /// The logical file name (e.g., "AGENT-NOTIFY-FILE").
        /// </summary>
        public string FileName { get; set; } = string.Empty;

        /// <summary>
        /// The operation type (e.g., "OPEN", "CLOSE", "WRITE").
        /// </summary>
        public string OperationType { get; set; } = string.Empty;

        /// <summary>
        /// The agent notification record to write (if applicable).
        /// </summary>
        public AgentNotifyRecord? AgentNotifyRecord { get; set; }

        /// <summary>
        /// The customer notification record to write (if applicable).
        /// </summary>
        public CustomerNotifyRecord? CustomerNotifyRecord { get; set; }

        /// <summary>
        /// The report record to write (if applicable).
        /// </summary>
        public NotifyReportRecord? NotifyReportRecord { get; set; }
    }

    /// <summary>
    /// Encapsulates the result of a file operation.
    /// </summary>
    public class FileOperationResult
    {
        /// <summary>
        /// The operation status code ("00" for success, "99" for unknown operation, other codes for errors).
        /// </summary>
        public string StatusCode { get; set; } = "00";

        /// <summary>
        /// Optional error message if the operation failed.
        /// </summary>
        public string? ErrorMessage { get; set; }
    }

    /// <summary>
    /// Provides file driver operations for notification files.
    /// </summary>
    public interface INotificationFileDriver
    {
        /// <summary>
        /// Executes the requested file operation asynchronously.
        /// </summary>
        /// <param name="request">The file operation request.</param>
        /// <returns>The result of the operation.</returns>
        Task<FileOperationResult> ExecuteAsync(FileOperationRequest request);
    }

    /// <summary>
    /// Implements the notification file driver logic, converting COBOL file operations to modern C#.
    /// </summary>
    public class NotificationFileDriver : INotificationFileDriver
    {
        private readonly ILogger<NotificationFileDriver> _logger;

        // File paths mapping (could be injected/configured)
        private const string AgentNotifyFilePath = "AGENTFLE";
        private const string CustomerNotifyFilePath = "CUSTFLE";
        private const string NotifyReportFilePath = "RPTFLE";

        /// <summary>
        /// Initializes a new instance of the <see cref="NotificationFileDriver"/> class.
        /// </summary>
        /// <param name="logger">The logger instance.</param>
        public NotificationFileDriver(ILogger<NotificationFileDriver> logger)
        {
            _logger = logger;
        }

        /// <inheritdoc />
        public async Task<FileOperationResult> ExecuteAsync(FileOperationRequest request)
        {
            var result = new FileOperationResult();

            try
            {
                var fileType = GetFileType(request.FileName);
                var operationType = ParseOperationType(request.OperationType);

                switch (operationType)
                {
                    case FileOperationType.Open:
                        await OpenFileAsync(fileType, result);
                        break;
                    case FileOperationType.Close:
                        await CloseFileAsync(fileType, result);
                        break;
                    case FileOperationType.Write:
                        await WriteRecordAsync(fileType, request, result);
                        break;
                    default:
                        result.StatusCode = "99";
                        result.ErrorMessage = $"Unknown operation type: {request.OperationType}";
                        break;
                }
            }
            catch (Exception ex)
            {
                result.StatusCode = "99";
                result.ErrorMessage = ex.Message;
                _logger.LogError(ex, "Error during file operation: {OperationType} on file {FileName}", request.OperationType, request.FileName);
            }

            return result;
        }

        /// <summary>
        /// Determines the notification file type from the logical file name.
        /// </summary>
        /// <param name="fileName">The logical file name.</param>
        /// <returns>The notification file type.</returns>
        /// <exception cref="ArgumentException">Thrown if the file name is invalid.</exception>
        private static NotificationFileType GetFileType(string fileName) =>
            fileName switch
            {
                "AGENT-NOTIFY-FILE" => NotificationFileType.AgentNotify,
                "CUSTOMER-NOTIFY-FILE" => NotificationFileType.CustomerNotify,
                "NOTIFY-REPORT-FILE" => NotificationFileType.NotifyReport,
                _ => throw new ArgumentException($"Invalid file name: {fileName}")
            };

        /// <summary>
        /// Parses the operation type string to the corresponding enum value.
        /// </summary>
        /// <param name="operationType">The operation type string.</param>
        /// <returns>The parsed operation type.</returns>
        private static FileOperationType ParseOperationType(string operationType) =>
            operationType.ToUpperInvariant() switch
            {
                "OPEN" => FileOperationType.Open,
                "CLOSE" => FileOperationType.Close,
                "WRITE" => FileOperationType.Write,
                _ => FileOperationType.Write // Default to Write for backward compatibility
            };

        /// <summary>
        /// Opens the specified file for output.
        /// </summary>
        /// <param name="fileType">The notification file type.</param>
        /// <param name="result">The operation result to update.</param>
        private async Task OpenFileAsync(NotificationFileType fileType, FileOperationResult result)
        {
            try
            {
                var filePath = GetFilePath(fileType);

                // In .NET, opening a file for output is typically handled on write.
                // To mimic COBOL's OPEN OUTPUT, we can create or truncate the file.
                using var stream = new FileStream(filePath, FileMode.Create, FileAccess.Write, FileShare.None);
                await stream.FlushAsync();

                result.StatusCode = "00";
            }
            catch (Exception ex)
            {
                result.StatusCode = "99";
                result.ErrorMessage = $"Failed to open file: {ex.Message}";
                _logger.LogError(ex, "Failed to open file {FileType}", fileType);
            }
        }

        /// <summary>
        /// Closes the specified file.
        /// </summary>
        /// <param name="fileType">The notification file type.</param>
        /// <param name="result">The operation result to update.</param>
        private Task CloseFileAsync(NotificationFileType fileType, FileOperationResult result)
        {
            // In .NET, files are closed automatically when streams are disposed.
            // To mimic COBOL's CLOSE, we can check if the file exists.
            try
            {
                var filePath = GetFilePath(fileType);

                if (!File.Exists(filePath))
                {
                    result.StatusCode = "99";
                    result.ErrorMessage = $"File not open: {filePath}";
                }
                else
                {
                    // No action needed; file is closed after each operation.
                    result.StatusCode = "00";
                }
            }
            catch (Exception ex)
            {
                result.StatusCode = "99";
                result.ErrorMessage = $"Failed to close file: {ex.Message}";
                _logger.LogError(ex, "Failed to close file {FileType}", fileType);
            }

            return Task.CompletedTask;
        }

        /// <summary>
        /// Writes a record to the specified file.
        /// </summary>
        /// <param name="fileType">The notification file type.</param>
        /// <param name="request">The file operation request containing the record.</param>
        /// <param name="result">The operation result to update.</param>
        private async Task WriteRecordAsync(NotificationFileType fileType, FileOperationRequest request, FileOperationResult result)
        {
            try
            {
                var filePath = GetFilePath(fileType);

                switch (fileType)
                {
                    case NotificationFileType.AgentNotify:
                        if (request.AgentNotifyRecord is null)
                            throw new ArgumentNullException(nameof(request.AgentNotifyRecord), "AgentNotifyRecord is required for AgentNotify file.");

                        await WriteAgentNotifyRecordAsync(filePath, request.AgentNotifyRecord);
                        break;

                    case NotificationFileType.CustomerNotify:
                        if (request.CustomerNotifyRecord is null)
                            throw new ArgumentNullException(nameof(request.CustomerNotifyRecord), "CustomerNotifyRecord is required for CustomerNotify file.");

                        await WriteCustomerNotifyRecordAsync(filePath, request.CustomerNotifyRecord);
                        break;

                    case NotificationFileType.NotifyReport:
                        if (request.NotifyReportRecord is null)
                            throw new ArgumentNullException(nameof(request.NotifyReportRecord), "NotifyReportRecord is required for NotifyReport file.");

                        await WriteNotifyReportRecordAsync(filePath, request.NotifyReportRecord);
                        break;
                }

                result.StatusCode = "00";
            }
            catch (Exception ex)
            {
                result.StatusCode = "99";
                result.ErrorMessage = $"Failed to write record: {ex.Message}";
                _logger.LogError(ex, "Failed to write record to file {FileType}", fileType);
            }
        }

        /// <summary>
        /// Gets the physical file path for the specified notification file type.
        /// </summary>
        /// <param name="fileType">The notification file type.</param>
        /// <returns>The file path.</returns>
        private static string GetFilePath(NotificationFileType fileType) =>
            fileType switch
            {
                NotificationFileType.AgentNotify => AgentNotifyFilePath,
                NotificationFileType.CustomerNotify => CustomerNotifyFilePath,
                NotificationFileType.NotifyReport => NotifyReportFilePath,
                _ => throw new ArgumentOutOfRangeException(nameof(fileType), $"Unknown file type: {fileType}")
            };

        /// <summary>
        /// Writes an agent notification record to the specified file.
        /// </summary>
        /// <param name="filePath">The file path.</param>
        /// <param name="record">The agent notification record.</param>
        private static async Task WriteAgentNotifyRecordAsync(string filePath, AgentNotifyRecord record)
        {
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

            await AppendLineAsync(filePath, line);
        }

        /// <summary>
        /// Writes a customer notification record to the specified file.
        /// </summary>
        /// <param name="filePath">The file path.</param>
        /// <param name="record">The customer notification record.</param>
        private static async Task WriteCustomerNotifyRecordAsync(string filePath, CustomerNotifyRecord record)
        {
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

            await AppendLineAsync(filePath, line);
        }

        /// <summary>
        /// Writes a notification report record to the specified file.
        /// </summary>
        /// <param name="filePath">The file path.</param>
        /// <param name="record">The notification report record.</param>
        private static async Task WriteNotifyReportRecordAsync(string filePath, NotifyReportRecord record)
        {
            var line = record.ReportLine.PadRight(133);
            await AppendLineAsync(filePath, line);
        }

        /// <summary>
        /// Appends a line to the specified file asynchronously.
        /// </summary>
        /// <param name="filePath">The file path.</param>
        /// <param name="line">The line to append.</param>
        private static async Task AppendLineAsync(string filePath, string line)
        {
            await File.AppendAllTextAsync(filePath, line + Environment.NewLine, Encoding.UTF8);
        }
    }

    /// <summary>
    /// Example of setting up dependency injection and using the NotificationFileDriver.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// Entry point for demonstration purposes.
        /// </summary>
        public static async Task Main(string[] args)
        {
            // Setup DI
            var serviceCollection = new ServiceCollection();
            serviceCollection.AddLogging(configure => configure.AddConsole());
            serviceCollection.AddTransient<INotificationFileDriver, NotificationFileDriver>();
            var serviceProvider = serviceCollection.BuildServiceProvider();

            var driver = serviceProvider.GetRequiredService<INotificationFileDriver>();

            // Example usage: Write an agent notification record
            var request = new FileOperationRequest
            {
                FileName = "AGENT-NOTIFY-FILE",
                OperationType = "WRITE",
                AgentNotifyRecord = new AgentNotifyRecord(
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
                    AgentNotifyMessages: "Policy renewal notification"
                )
            };

            var result = await driver.ExecuteAsync(request);

            if (result.StatusCode != "00")
            {
                Console.WriteLine($"Error: {result.ErrorMessage}");
            }
            else
            {
                Console.WriteLine("Record written successfully.");
            }
        }
    }
}