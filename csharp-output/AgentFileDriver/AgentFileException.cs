using System;
using System.IO;
using System.Threading.Tasks;
using System.Linq;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace AgentFileDriver
{
    /// <summary>
    /// Represents the input parameters for agent file operations.
    /// </summary>
    public record AgentFileInput(
        string OperationType,
        string AgentCode
    );

    /// <summary>
    /// Represents the output result of agent file operations.
    /// </summary>
    public record AgentFileOutput(
        string StatusCode,
        AgentRecord? AgentRecord
    );

    /// <summary>
    /// Represents an agent record as stored in the agent file.
    /// </summary>
    public record AgentRecord(
        string AgentName,
        string AgentAddress1,
        string AgentAddress2,
        string AgentCity,
        string AgentState,
        string AgentZipCode,
        string AgentStatus,
        string AgentType,
        string AgentEmail,
        string AgentContactNo,
        string AgentStartDate,
        string AgentEndDate
    );

    /// <summary>
    /// Interface for agent file repository abstraction.
    /// </summary>
    public interface IAgentFileRepository : IAsyncDisposable
    {
        /// <summary>
        /// Opens the agent file for reading.
        /// </summary>
        Task OpenAsync();

        /// <summary>
        /// Closes the agent file.
        /// </summary>
        Task CloseAsync();

        /// <summary>
        /// Searches for an agent record by agent code.
        /// </summary>
        /// <param name="agentCode">The agent code to search for.</param>
        /// <returns>The agent record if found; otherwise, null.</returns>
        Task<AgentRecord?> SearchAsync(string agentCode);
    }

    /// <summary>
    /// Exception thrown when an agent file operation fails.
    /// </summary>
    public class AgentFileException : Exception
    {
        /// <summary>
        /// Gets the status code returned by the file operation.
        /// </summary>
        public string StatusCode { get; }

        public AgentFileException(string message, string statusCode)
            : base(message)
        {
            StatusCode = statusCode;
        }
    }

    /// <summary>
    /// Implements the agent file repository using a VSAM-like indexed file.
    /// This is a stub implementation for demonstration purposes.
    /// </summary>
    public class AgentFileRepository : IAgentFileRepository
    {
        private bool _isOpen;
        private readonly ILogger<AgentFileRepository> _logger;

        // Simulated agent records for demonstration.
        private static readonly AgentRecord[] _agentRecords = new[]
        {
            new AgentRecord(
                "John Doe", "123 Main St", "Suite 100", "Copenhagen", "DK", "1000", "A", "Broker",
                "john.doe@email.com", "1234567890", "2020-01-01", "2025-12-31"
            ),
            new AgentRecord(
                "Jane Smith", "456 Elm St", "Apt 2B", "Aarhus", "DK", "8000", "A", "Agent",
                "jane.smith@email.com", "0987654321", "2019-05-15", "2024-05-15"
            )
        };

        public AgentFileRepository(ILogger<AgentFileRepository> logger)
        {
            _logger = logger;
        }

        /// <inheritdoc />
        public async Task OpenAsync()
        {
            // Simulate file open delay.
            await Task.Delay(50);
            _isOpen = true;
            _logger.LogInformation("Agent file opened.");
        }

        /// <inheritdoc />
        public async Task CloseAsync()
        {
            await Task.Delay(50);
            _isOpen = false;
            _logger.LogInformation("Agent file closed.");
        }

        /// <inheritdoc />
        public async Task<AgentRecord?> SearchAsync(string agentCode)
        {
            if (!_isOpen)
                throw new AgentFileException("Agent file is not open.", "10");

            await Task.Delay(50); // Simulate IO delay.

            // For demonstration, use agentCode as a substring match on AgentName.
            var record = _agentRecords.FirstOrDefault(r => r.AgentName.Replace(" ", "").StartsWith(agentCode, StringComparison.OrdinalIgnoreCase));
            return record;
        }

        /// <inheritdoc />
        public ValueTask DisposeAsync()
        {
            // Dispose resources if needed.
            return ValueTask.CompletedTask;
        }
    }

    /// <summary>
    /// Main driver class for agent file operations, converted from COBOL FLDRIVR1.
    /// </summary>
    public class AgentFileDriver
    {
        private readonly IAgentFileRepository _repository;
        private readonly ILogger<AgentFileDriver> _logger;

        /// <summary>
        /// Initializes a new instance of <see cref="AgentFileDriver"/>.
        /// </summary>
        /// <param name="repository">The agent file repository.</param>
        /// <param name="logger">The logger instance.</param>
        public AgentFileDriver(IAgentFileRepository repository, ILogger<AgentFileDriver> logger)
        {
            _repository = repository;
            _logger = logger;
        }

        /// <summary>
        /// Executes the requested agent file operation asynchronously.
        /// </summary>
        /// <param name="input">The input parameters for the operation.</param>
        /// <returns>The output result containing status code and agent record (if applicable).</returns>
        /// <exception cref="AgentFileException">Thrown when a file operation fails.</exception>
        public async Task<AgentFileOutput> ExecuteAsync(AgentFileInput input)
        {
            string statusCode = "99";
            AgentRecord? agentRecord = null;

            try
            {
                switch (input.OperationType?.Trim().ToUpperInvariant())
                {
                    case "OPEN":
                        await _repository.OpenAsync();
                        statusCode = "00";
                        break;

                    case "CLOSE":
                        await _repository.CloseAsync();
                        statusCode = "00";
                        break;

                    case "SEARCH":
                        agentRecord = await SearchAgentAsync(input.AgentCode);
                        statusCode = agentRecord is not null ? "00" : "23";
                        break;

                    default:
                        statusCode = "99";
                        _logger.LogWarning("Unknown operation type: {OperationType}", input.OperationType);
                        break;
                }
            }
            catch (AgentFileException ex)
            {
                statusCode = ex.StatusCode;
                HandleError(input.OperationType, ex.Message, ex.StatusCode);
            }
            catch (Exception ex)
            {
                statusCode = "99";
                HandleError(input.OperationType, ex.Message, statusCode);
            }

            return new AgentFileOutput(statusCode, agentRecord);
        }

        /// <summary>
        /// Searches for an agent record by agent code.
        /// </summary>
        /// <param name="agentCode">The agent code to search for.</param>
        /// <returns>The agent record if found; otherwise, null.</returns>
        private async Task<AgentRecord?> SearchAgentAsync(string agentCode)
        {
            try
            {
                var record = await _repository.SearchAsync(agentCode);
                if (record is null)
                {
                    throw new AgentFileException($"Agent record not found for code: {agentCode}", "23");
                }
                return record;
            }
            catch (AgentFileException)
            {
                throw;
            }
            catch (Exception ex)
            {
                throw new AgentFileException($"Unexpected error during agent search: {ex.Message}", "99");
            }
        }

        /// <summary>
        /// Handles errors by logging and optionally performing additional actions.
        /// </summary>
        /// <param name="operationType">The operation type during which the error occurred.</param>
        /// <param name="errorMessage">The error message.</param>
        /// <param name="statusCode">The status code associated with the error.</param>
        private void HandleError(string? operationType, string errorMessage, string statusCode)
        {
            _logger.LogError("IN FLDRIVR1");
            _logger.LogError("ERROR: {OperationType} ON AGENTVSAM FILE STATUS CODE: {StatusCode}. Message: {ErrorMessage}",
                operationType, statusCode, errorMessage);
            // In COBOL, ABEND would terminate the program. In C#, we log and propagate the error.
        }
    }

    /// <summary>
    /// Example of setting up dependency injection and running the driver.
    /// </summary>
    public static class Program
    {
        /// <summary>
        /// Entry point for demonstration.
        /// </summary>
        public static async Task Main(string[] args)
        {
            // Setup DI and logging.
            var serviceCollection = new ServiceCollection();
            serviceCollection.AddLogging(configure => configure.AddConsole());
            serviceCollection.AddScoped<IAgentFileRepository, AgentFileRepository>();
            serviceCollection.AddScoped<AgentFileDriver>();

            var serviceProvider = serviceCollection.BuildServiceProvider();

            var driver = serviceProvider.GetRequiredService<AgentFileDriver>();

            // Example: Open file
            var openResult = await driver.ExecuteAsync(new AgentFileInput("OPEN", ""));
            Console.WriteLine($"OPEN Status: {openResult.StatusCode}");

            // Example: Search for agent
            var searchResult = await driver.ExecuteAsync(new AgentFileInput("SEARCH", "JohnDoe"));
            Console.WriteLine($"SEARCH Status: {searchResult.StatusCode}");
            if (searchResult.AgentRecord is not null)
            {
                Console.WriteLine($"Agent Name: {searchResult.AgentRecord.AgentName}");
            }

            // Example: Close file
            var closeResult = await driver.ExecuteAsync(new AgentFileInput("CLOSE", ""));
            Console.WriteLine($"CLOSE Status: {closeResult.StatusCode}");
        }
    }
}