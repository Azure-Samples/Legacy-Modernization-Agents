using System;
using System.IO;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.DependencyInjection;

namespace AgentFileDriver
{
    /// <summary>
    /// Represents a single agent record as defined in the COBOL CAGENT copybook.
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
        public string AgentStatus { get; init; } = string.Empty;
        public string AgentType { get; init; } = string.Empty;
        public string AgentEmail { get; init; } = string.Empty;
        public string AgentContactNo { get; init; } = string.Empty;
        public string AgentStartDate { get; init; } = string.Empty;
        public string AgentEndDate { get; init; } = string.Empty;
    }

    /// <summary>
    /// Represents the input area for the agent file driver operation.
    /// </summary>
    public class AgentFileDriverInput
    {
        public string OperationType { get; set; } = string.Empty; // "OPEN", "CLOSE", "SEARCH"
        public string AgentCode { get; set; } = string.Empty;
    }

    /// <summary>
    /// Represents the output area for the agent file driver operation.
    /// </summary>
    public class AgentFileDriverOutput
    {
        public string StatusCode { get; set; } = string.Empty; // "00" = OK, "23" = Not Found, "99" = Unknown Operation, etc.
        public AgentRecord? AgentRecord { get; set; }
    }

    /// <summary>
    /// Interface for agent file repository abstraction.
    /// </summary>
    public interface IAgentFileRepository
    {
        /// <summary>
        /// Opens the agent file for input operations.
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
    /// In-memory implementation of the agent file repository for demonstration purposes.
    /// Replace with actual file/DB access in production.
    /// </summary>
    public class InMemoryAgentFileRepository : IAgentFileRepository
    {
        private bool _isOpen;
        private readonly List<AgentRecord> _agentRecords;

        public InMemoryAgentFileRepository(IEnumerable<AgentRecord>? initialRecords = null)
        {
            _agentRecords = initialRecords?.ToList() ?? new List<AgentRecord>();
        }

        public Task OpenAsync()
        {
            _isOpen = true;
            return Task.CompletedTask;
        }

        public Task CloseAsync()
        {
            _isOpen = false;
            return Task.CompletedTask;
        }

        public Task<AgentRecord?> SearchAsync(string agentCode)
        {
            if (!_isOpen)
                throw new InvalidOperationException("Agent file is not open.");

            var record = _agentRecords.FirstOrDefault(r => r.AgentCode == agentCode);
            return Task.FromResult(record);
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
        /// Initializes a new instance of the <see cref="AgentFileDriver"/> class.
        /// </summary>
        /// <param name="repository">The agent file repository.</param>
        /// <param name="logger">The logger instance.</param>
        public AgentFileDriver(IAgentFileRepository repository, ILogger<AgentFileDriver> logger)
        {
            _repository = repository ?? throw new ArgumentNullException(nameof(repository));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Executes the requested operation on the agent file.
        /// </summary>
        /// <param name="input">The input area containing operation type and agent code.</param>
        /// <returns>The output area containing status code and agent record details.</returns>
        public async Task<AgentFileDriverOutput> ExecuteAsync(AgentFileDriverInput input)
        {
            if (input is null)
                throw new ArgumentNullException(nameof(input));

            var output = new AgentFileDriverOutput();

            try
            {
                switch (input.OperationType?.Trim().ToUpperInvariant())
                {
                    case "OPEN":
                        await OpenAgentFileAsync(output).ConfigureAwait(false);
                        break;
                    case "CLOSE":
                        await CloseAgentFileAsync(output).ConfigureAwait(false);
                        break;
                    case "SEARCH":
                        await SearchAgentFileAsync(input.AgentCode, output).ConfigureAwait(false);
                        break;
                    default:
                        output.StatusCode = "99"; // Unknown operation
                        _logger.LogWarning("Unknown operation type: {OperationType}", input.OperationType);
                        break;
                }
            }
            catch (AgentFileDriverException ex)
            {
                output.StatusCode = ex.StatusCode;
                _logger.LogError(ex, "AgentFileDriver error: {Message}", ex.Message);
            }
            catch (Exception ex)
            {
                output.StatusCode = "99";
                _logger.LogError(ex, "Unhandled exception in AgentFileDriver.");
            }

            return output;
        }

        /// <summary>
        /// Opens the agent file.
        /// </summary>
        /// <param name="output">The output area to update status code.</param>
        private async Task OpenAgentFileAsync(AgentFileDriverOutput output)
        {
            try
            {
                await _repository.OpenAsync().ConfigureAwait(false);
                output.StatusCode = "00";
            }
            catch (Exception ex)
            {
                output.StatusCode = "98"; // File open error
                throw new AgentFileDriverException("Error opening agent file.", output.StatusCode, ex);
            }
        }

        /// <summary>
        /// Closes the agent file.
        /// </summary>
        /// <param name="output">The output area to update status code.</param>
        private async Task CloseAgentFileAsync(AgentFileDriverOutput output)
        {
            try
            {
                await _repository.CloseAsync().ConfigureAwait(false);
                output.StatusCode = "00";
            }
            catch (Exception ex)
            {
                output.StatusCode = "97"; // File close error
                throw new AgentFileDriverException("Error closing agent file.", output.StatusCode, ex);
            }
        }

        /// <summary>
        /// Searches for an agent record by agent code and populates output.
        /// </summary>
        /// <param name="agentCode">The agent code to search for.</param>
        /// <param name="output">The output area to update status and agent record.</param>
        private async Task SearchAgentFileAsync(string agentCode, AgentFileDriverOutput output)
        {
            try
            {
                var record = await _repository.SearchAsync(agentCode).ConfigureAwait(false);
                if (record is null)
                {
                    output.StatusCode = "23"; // Not found
                    throw new AgentFileDriverException($"Agent code '{agentCode}' not found.", output.StatusCode);
                }
                output.StatusCode = "00";
                output.AgentRecord = record;
            }
            catch (AgentFileDriverException)
            {
                throw;
            }
            catch (Exception ex)
            {
                output.StatusCode = "96"; // File read/search error
                throw new AgentFileDriverException("Error searching agent file.", output.StatusCode, ex);
            }
        }
    }

    /// <summary>
    /// Custom exception for agent file driver errors.
    /// </summary>
    public class AgentFileDriverException : Exception
    {
        /// <summary>
        /// Gets the status code associated with the error.
        /// </summary>
        public string StatusCode { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="AgentFileDriverException"/> class.
        /// </summary>
        /// <param name="message">The error message.</param>
        /// <param name="statusCode">The status code.</param>
        /// <param name="innerException">The inner exception.</param>
        public AgentFileDriverException(string message, string statusCode, Exception? innerException = null)
            : base(message, innerException)
        {
            StatusCode = statusCode;
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
            // Setup DI
            var serviceCollection = new ServiceCollection();
            serviceCollection.AddLogging(configure => configure.AddConsole());
            serviceCollection.AddSingleton<IAgentFileRepository>(provider =>
                new InMemoryAgentFileRepository(new[]
                {
                    new AgentRecord
                    {
                        AgentCode = "A123456789",
                        AgentName = "John Doe",
                        AgentAddress1 = "123 Main St",
                        AgentAddress2 = "Suite 100",
                        AgentCity = "Copenhagen",
                        AgentState = "DK",
                        AgentZipCode = "2100",
                        AgentStatus = "A",
                        AgentType = "Broker",
                        AgentEmail = "john.doe@example.com",
                        AgentContactNo = "1234567890",
                        AgentStartDate = "2020-01-01",
                        AgentEndDate = "2025-12-31"
                    }
                })
            );
            serviceCollection.AddTransient<AgentFileDriver>();

            var serviceProvider = serviceCollection.BuildServiceProvider();
            var driver = serviceProvider.GetRequiredService<AgentFileDriver>();

            // Example usage
            var input = new AgentFileDriverInput
            {
                OperationType = "SEARCH",
                AgentCode = "A123456789"
            };

            var output = await driver.ExecuteAsync(input);

            Console.WriteLine($"Status: {output.StatusCode}");
            if (output.AgentRecord is not null)
            {
                Console.WriteLine($"Agent Name: {output.AgentRecord.AgentName}");
                Console.WriteLine($"Agent Email: {output.AgentRecord.AgentEmail}");
                // ... print other fields as needed
            }
        }
    }
}