using System;
using System.Threading.Tasks;
using System.Linq;
using Microsoft.Extensions.Logging;

namespace AgentFileDriver
{
    /// <summary>
    /// Represents the input parameters for agent file operations.
    /// </summary>
    public record AgentFileOperationInput(
        string OperationType, // "OPEN", "CLOSE", "SEARCH"
        string AgentCode      // Agent identifier for SEARCH
    );

    /// <summary>
    /// Represents the output/result of agent file operations.
    /// </summary>
    public record AgentFileOperationOutput(
        string StatusCode, // "00" = OK, "23" = Not Found, "99" = Invalid Operation, etc.
        AgentRecord? Agent // Populated for SEARCH if found
    );

    /// <summary>
    /// Represents an agent record as defined in the CAGENT copybook.
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
    /// Exception thrown when agent file operations fail.
    /// </summary>
    public class AgentFileException : Exception
    {
        public string StatusCode { get; }

        public AgentFileException(string message, string statusCode)
            : base(message)
        {
            StatusCode = statusCode;
        }
    }

    /// <summary>
    /// Implements the driver logic for agent file operations, converted from COBOL FLDRIVR1.
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
        /// Executes the requested agent file operation asynchronously.
        /// </summary>
        /// <param name="input">The operation input parameters.</param>
        /// <returns>The operation output/result.</returns>
        /// <exception cref="AgentFileException">Thrown on file errors.</exception>
        public async Task<AgentFileOperationOutput> ExecuteAsync(AgentFileOperationInput input)
        {
            if (input is null)
                throw new ArgumentNullException(nameof(input));

            string statusCode = "99"; // Default to invalid operation
            AgentRecord? agent = null;

            try
            {
                switch (input.OperationType?.Trim().ToUpperInvariant())
                {
                    case "OPEN":
                        await OpenAgentFileAsync();
                        statusCode = "00";
                        break;

                    case "CLOSE":
                        await CloseAgentFileAsync();
                        statusCode = "00";
                        break;

                    case "SEARCH":
                        (statusCode, agent) = await SearchAgentFileAsync(input.AgentCode);
                        break;

                    default:
                        statusCode = "99";
                        _logger.LogWarning("Invalid operation type: {OperationType}", input.OperationType);
                        break;
                }
            }
            catch (AgentFileException ex)
            {
                statusCode = ex.StatusCode;
                _logger.LogError(ex, "Agent file operation failed: {OperationType} (Status: {StatusCode})", input.OperationType, ex.StatusCode);
            }
            catch (Exception ex)
            {
                statusCode = "99";
                _logger.LogCritical(ex, "Unexpected error in AgentFileDriver: {OperationType}", input.OperationType);
            }

            return new AgentFileOperationOutput(statusCode, agent);
        }

        /// <summary>
        /// Opens the agent file for input operations.
        /// </summary>
        private async Task OpenAgentFileAsync()
        {
            try
            {
                await _repository.OpenAsync();
            }
            catch (Exception ex)
            {
                throw new AgentFileException("Failed to open agent file.", "98");
            }
        }

        /// <summary>
        /// Closes the agent file.
        /// </summary>
        private async Task CloseAgentFileAsync()
        {
            try
            {
                await _repository.CloseAsync();
            }
            catch (Exception ex)
            {
                throw new AgentFileException("Failed to close agent file.", "98");
            }
        }

        /// <summary>
        /// Searches for an agent record by agent code.
        /// </summary>
        /// <param name="agentCode">The agent code to search for.</param>
        /// <returns>
        /// Tuple of status code and agent record (if found).
        /// Status code: "00" = found, "23" = not found, "98" = file error.
        /// </returns>
        private async Task<(string statusCode, AgentRecord? agent)> SearchAgentFileAsync(string agentCode)
        {
            if (string.IsNullOrWhiteSpace(agentCode))
                return ("99", null);

            try
            {
                var agent = await _repository.SearchAsync(agentCode.Trim());
                if (agent is not null)
                {
                    return ("00", agent);
                }
                else
                {
                    return ("23", null); // Not found
                }
            }
            catch (Exception ex)
            {
                throw new AgentFileException("Failed to search agent file.", "98");
            }
        }
    }

    // Example stub implementation for IAgentFileRepository.
    // In production, this would access a database, VSAM file, or other persistent store.
    public class InMemoryAgentFileRepository : IAgentFileRepository
    {
        private bool _isOpen = false;
        private readonly AgentRecord[] _agents;

        public InMemoryAgentFileRepository(AgentRecord[] agents)
        {
            _agents = agents ?? Array.Empty<AgentRecord>();
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
                throw new InvalidOperationException("File not open.");

            // Simulate search by agent code (assume AgentContactNo is the code for demo)
            var agent = _agents.FirstOrDefault(a => a.AgentContactNo == agentCode);
            return Task.FromResult(agent);
        }

        public ValueTask DisposeAsync()
        {
            _isOpen = false;
            return ValueTask.CompletedTask;
        }
    }
}