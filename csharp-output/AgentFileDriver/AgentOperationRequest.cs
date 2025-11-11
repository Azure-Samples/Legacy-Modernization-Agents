using System;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;

namespace AgentFileDriver
{
    /// <summary>
    /// Represents an agent record as defined in the CAGENT copybook.
    /// </summary>
    public record AgentRecord(
        string AgentCode,
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
    /// Request DTO for agent file operations.
    /// </summary>
    public class AgentOperationRequest
    {
        /// <summary>
        /// The operation type: "OPEN", "CLOSE", "SEARCH".
        /// </summary>
        public string OperationType { get; set; } = string.Empty;

        /// <summary>
        /// The agent code to search for (used in SEARCH operation).
        /// </summary>
        public string? AgentCode { get; set; }
    }

    /// <summary>
    /// Response DTO for agent file operations.
    /// </summary>
    public class AgentOperationResponse
    {
        /// <summary>
        /// Status code of the operation ("00" = OK, "23" = Not Found, "99" = Invalid Operation, others = error).
        /// </summary>
        public string StatusCode { get; set; } = "00";

        /// <summary>
        /// The returned agent record, if found.
        /// </summary>
        public AgentRecord? AgentRecord { get; set; }
    }

    /// <summary>
    /// Interface for agent file repository abstraction.
    /// </summary>
    public interface IAgentRepository : IAsyncDisposable
    {
        /// <summary>
        /// Opens the agent file for input.
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
        Task<AgentRecord?> FindByCodeAsync(string agentCode);
    }

    /// <summary>
    /// Implements the business logic for agent file operations, corresponding to the COBOL FLDRIVR1 program.
    /// </summary>
    public class AgentFileService
    {
        private readonly IAgentRepository agentRepository;
        private readonly ILogger<AgentFileService> logger;

        /// <summary>
        /// Initializes a new instance of the <see cref="AgentFileService"/> class.
        /// </summary>
        /// <param name="agentRepository">The agent file repository.</param>
        /// <param name="logger">The logger instance.</param>
        public AgentFileService(IAgentRepository agentRepository, ILogger<AgentFileService> logger)
        {
            this.agentRepository = agentRepository ?? throw new ArgumentNullException(nameof(agentRepository));
            this.logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Executes the requested agent file operation.
        /// </summary>
        /// <param name="request">The operation request.</param>
        /// <returns>The operation response.</returns>
        public async Task<AgentOperationResponse> ExecuteAsync(AgentOperationRequest request)
        {
            if (request is null)
                throw new ArgumentNullException(nameof(request));

            var response = new AgentOperationResponse();

            try
            {
                switch (request.OperationType?.Trim().ToUpperInvariant())
                {
                    case "OPEN":
                        await OpenAgentFileAsync(response);
                        break;

                    case "CLOSE":
                        await CloseAgentFileAsync(response);
                        break;

                    case "SEARCH":
                        await SearchAgentFileAsync(request.AgentCode, response);
                        break;

                    default:
                        response.StatusCode = "99";
                        logger.LogWarning("Invalid operation type: {OperationType}", request.OperationType);
                        break;
                }
            }
            catch (AgentFileException ex)
            {
                response.StatusCode = ex.StatusCode;
                logger.LogError(ex, "Agent file error during {OperationType}", request.OperationType);
            }
            catch (Exception ex)
            {
                response.StatusCode = "99";
                logger.LogCritical(ex, "Unhandled exception in AgentFileService");
            }

            return response;
        }

        /// <summary>
        /// Opens the agent file for input.
        /// </summary>
        /// <param name="response">The operation response to update.</param>
        private async Task OpenAgentFileAsync(AgentOperationResponse response)
        {
            try
            {
                await agentRepository.OpenAsync();
                response.StatusCode = "00";
            }
            catch (Exception ex)
            {
                // Simulate COBOL FILE-STATUS-CODE mapping
                response.StatusCode = "98";
                throw new AgentFileException("Failed to open agent file.", response.StatusCode, ex);
            }
        }

        /// <summary>
        /// Closes the agent file.
        /// </summary>
        /// <param name="response">The operation response to update.</param>
        private async Task CloseAgentFileAsync(AgentOperationResponse response)
        {
            try
            {
                await agentRepository.CloseAsync();
                response.StatusCode = "00";
            }
            catch (Exception ex)
            {
                response.StatusCode = "97";
                throw new AgentFileException("Failed to close agent file.", response.StatusCode, ex);
            }
        }

        /// <summary>
        /// Searches for an agent record by agent code and populates the response.
        /// </summary>
        /// <param name="agentCode">The agent code to search for.</param>
        /// <param name="response">The operation response to update.</param>
        private async Task SearchAgentFileAsync(string? agentCode, AgentOperationResponse response)
        {
            if (string.IsNullOrWhiteSpace(agentCode))
            {
                response.StatusCode = "99";
                logger.LogWarning("Agent code is required for SEARCH operation.");
                return;
            }

            try
            {
                var agentRecord = await agentRepository.FindByCodeAsync(agentCode);

                if (agentRecord is null)
                {
                    response.StatusCode = "23"; // Not found
                    logger.LogInformation("Agent record not found for code: {AgentCode}", agentCode);
                }
                else
                {
                    response.StatusCode = "00";
                    response.AgentRecord = agentRecord;
                }
            }
            catch (Exception ex)
            {
                response.StatusCode = "96";
                throw new AgentFileException($"Error searching for agent code {agentCode}.", response.StatusCode, ex);
            }
        }
    }

    /// <summary>
    /// Custom exception for agent file errors, mapping COBOL error handling.
    /// </summary>
    public class AgentFileException : Exception
    {
        /// <summary>
        /// Gets the status code associated with the error.
        /// </summary>
        public string StatusCode { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="AgentFileException"/> class.
        /// </summary>
        /// <param name="message">The error message.</param>
        /// <param name="statusCode">The status code.</param>
        /// <param name="innerException">The inner exception.</param>
        public AgentFileException(string message, string statusCode, Exception? innerException = null)
            : base(message, innerException)
        {
            StatusCode = statusCode;
        }
    }

    // Example stub implementation for IAgentRepository (for testing/demo purposes)
    public class InMemoryAgentRepository : IAgentRepository
    {
        private bool isOpen = false;
        private readonly AgentRecord[] agents = new[]
        {
            new AgentRecord(
                "A123456789",
                "John Doe",
                "123 Main St",
                "Suite 100",
                "Copenhagen",
                "DK",
                "2100",
                "A",
                "Broker",
                "john.doe@example.com",
                "1234567890",
                "2020-01-01",
                "2025-12-31"
            )
            // Add more records as needed
        };

        public Task OpenAsync()
        {
            isOpen = true;
            return Task.CompletedTask;
        }

        public Task CloseAsync()
        {
            isOpen = false;
            return Task.CompletedTask;
        }

        public Task<AgentRecord?> FindByCodeAsync(string agentCode)
        {
            if (!isOpen)
                throw new InvalidOperationException("Agent file is not open.");

            var agent = Array.Find(agents, a => a.AgentCode.Equals(agentCode, StringComparison.OrdinalIgnoreCase));
            return Task.FromResult(agent);
        }

        public ValueTask DisposeAsync()
        {
            isOpen = false;
            return ValueTask.CompletedTask;
        }
    }
}