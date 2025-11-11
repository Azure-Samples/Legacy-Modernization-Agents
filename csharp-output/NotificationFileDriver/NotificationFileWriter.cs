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
        Write,
        Unknown
    }

    /// <summary>
    /// Represents the result of a file operation.
    /// </summary>
    public record FileOperationResult(string Status);

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
    /// Interface for file writer abstraction.
    /// </summary>
    public interface INotificationFileWriter : IAsyncDisposable
    {
        Task OpenAsync();
        Task WriteAsync(object record);
        Task CloseAsync();
    }

    /// <summary>
    /// Implements file writing for notification files.
    /// </summary>
    public class NotificationFileWriter : INotificationFileWriter
    {
        private readonly string filePath;
        private readonly FileType fileType;
        private StreamWriter? writer;

        /// <summary>
        /// The type of file being written.
        /// </summary>
        public enum FileType
        {
            Agent,
            Customer,
            Report
        }

        /// <summary>
        /// Initializes a new instance of <see cref="NotificationFileWriter"/>.
        /// </summary>
        /// <param name="fileType">Type of file.</param>
        /// <param name="filePath">Path to the file.</param>
        public NotificationFileWriter(FileType fileType, string filePath)
        {
            this.fileType = fileType;
            this.filePath = filePath;
        }

        /// <inheritdoc />
        public async Task OpenAsync()
        {
            writer = new StreamWriter(filePath, append: true);
            await Task.CompletedTask;
        }

        /// <inheritdoc />
        public async Task WriteAsync(object record)
        {
            if (writer is null)
                throw new InvalidOperationException("File not opened.");

            string line = fileType switch
            {
                FileType.Agent => SerializeAgentRecord((AgentNotifyRecord)record),
                FileType.Customer => SerializeCustomerRecord((CustomerNotifyRecord)record),
                FileType.Report => SerializeReportRecord((NotifyReportRecord)record),
                _ => throw new ArgumentException("Unknown file type.")
            };

            await writer.WriteLineAsync(line);
        }

        /// <inheritdoc />
        public async Task CloseAsync()
        {
            if (writer != null)
            {
                await writer.FlushAsync();
                await writer.DisposeAsync();
                writer = null;
            }
        }

        /// <inheritdoc />
        public async ValueTask DisposeAsync()
        {
            await CloseAsync();
        }

        private static string SerializeAgentRecord(AgentNotifyRecord record)
        {
            // Fixed-width formatting, pad right to COBOL sizes
            return string.Concat(
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
        }

        private static string SerializeCustomerRecord(CustomerNotifyRecord record)
        {
            return string.Concat(
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
        }

        private static string SerializeReportRecord(NotifyReportRecord record)
        {
            return record.ReportLine.PadRight(133);
        }
    }

    /// <summary>
    /// Main file driver class for notification records.
    /// </summary>
    public class NotificationFileDriver
    {
        private readonly ILogger<NotificationFileDriver> logger;
        private readonly IServiceProvider serviceProvider;

        /// <summary>
        /// Initializes a new instance of <see cref="NotificationFileDriver"/>.
        /// </summary>
        /// <param name="logger">Logger instance.</param>
        /// <param name="serviceProvider">Service provider for DI.</param>
        public NotificationFileDriver(ILogger<NotificationFileDriver> logger, IServiceProvider serviceProvider)
        {
            this.logger = logger;
            this.serviceProvider = serviceProvider;
        }

        /// <summary>
        /// Executes the file operation based on input parameters.
        /// </summary>
        /// <param name="fileName">Logical file name (e.g., "AGENT-NOTIFY-FILE").</param>
        /// <param name="operationType">Operation type ("OPEN", "CLOSE", "WRITE").</param>
        /// <param name="agentRecord">Agent record (if applicable).</param>
        /// <param name="customerRecord">Customer record (if applicable).</param>
        /// <param name="reportRecord">Report record (if applicable).</param>
        /// <returns>Operation result status.</returns>
        /// <exception cref="ArgumentException">Thrown for invalid file or operation types.</exception>
        public async Task<FileOperationResult> ExecuteAsync(
            string fileName,
            string operationType,
            AgentNotifyRecord? agentRecord,
            CustomerNotifyRecord? customerRecord,
            NotifyReportRecord? reportRecord)
        {
            // Map file name to file type and path
            var (fileType, filePath) = fileName switch
            {
                "AGENT-NOTIFY-FILE" => (NotificationFileWriter.FileType.Agent, "AGENTFLE"),
                "CUSTOMER-NOTIFY-FILE" => (NotificationFileWriter.FileType.Customer, "CUSTFLE"),
                "NOTIFY-REPORT-FILE" => (NotificationFileWriter.FileType.Report, "RPTFLE"),
                _ => throw new ArgumentException($"Unknown file name: {fileName}")
            };

            var operation = operationType.ToUpperInvariant() switch
            {
                "OPEN" => FileOperationType.Open,
                "CLOSE" => FileOperationType.Close,
                "WRITE" => FileOperationType.Write,
                _ => FileOperationType.Unknown
            };

            if (operation == FileOperationType.Unknown)
            {
                logger.LogError("Unknown operation type: {OperationType}", operationType);
                return new FileOperationResult("99");
            }

            // Use DI to get file writer (could be extended for testing/mocking)
            var fileWriter = new NotificationFileWriter(fileType, filePath);

            try
            {
                switch (operation)
                {
                    case FileOperationType.Open:
                        await fileWriter.OpenAsync();
                        logger.LogInformation("Opened file {FilePath} for {FileType}", filePath, fileType);
                        return new FileOperationResult("00");

                    case FileOperationType.Close:
                        await fileWriter.CloseAsync();
                        logger.LogInformation("Closed file {FilePath} for {FileType}", filePath, fileType);
                        return new FileOperationResult("00");

                    case FileOperationType.Write:
                        await fileWriter.OpenAsync(); // Ensure file is open for writing
                        object? record = fileType switch
                        {
                            NotificationFileWriter.FileType.Agent => agentRecord,
                            NotificationFileWriter.FileType.Customer => customerRecord,
                            NotificationFileWriter.FileType.Report => reportRecord,
                            _ => null
                        };

                        if (record is null)
                        {
                            logger.LogError("No record provided for file type {FileType}", fileType);
                            return new FileOperationResult("99");
                        }

                        await fileWriter.WriteAsync(record);
                        await fileWriter.CloseAsync();
                        logger.LogInformation("Wrote record to file {FilePath} for {FileType}", filePath, fileType);
                        return new FileOperationResult("00");

                    default:
                        logger.LogError("Unhandled operation type: {OperationType}", operationType);
                        return new FileOperationResult("99");
                }
            }
            catch (IOException ioEx)
            {
                logger.LogError(ioEx, "IO error during {OperationType} on file {FilePath}", operationType, filePath);
                return new FileOperationResult("99");
            }
            catch (Exception ex)
            {
                logger.LogError(ex, "Error during {OperationType} on file {FilePath}", operationType, filePath);
                return new FileOperationResult("99");
            }
            finally
            {
                await fileWriter.DisposeAsync();
            }
        }
    }

    /// <summary>
    /// Example usage of NotificationFileDriver.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// Entry point for the Notification File Driver.
        /// </summary>
        public static async Task Main(string[] args)
        {
            // Setup DI and logging
            var serviceCollection = new ServiceCollection();
            serviceCollection.AddLogging(configure => configure.AddConsole());
            var serviceProvider = serviceCollection.BuildServiceProvider();

            var logger = serviceProvider.GetRequiredService<ILogger<NotificationFileDriver>>();
            var driver = new NotificationFileDriver(logger, serviceProvider);

            // Example: Write an agent record
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
                AgentNotifyMessages: "Policy renewal notification"
            );

            var result = await driver.ExecuteAsync(
                fileName: "AGENT-NOTIFY-FILE",
                operationType: "WRITE",
                agentRecord: agentRecord,
                customerRecord: null,
                reportRecord: null
            );

            Console.WriteLine($"Operation Status: {result.Status}");
        }
    }
}