using System;

namespace AgentManagement.Models
{
    /// <summary>
    /// Represents an agent record containing personal and contact information.
    /// This class is a direct conversion from a COBOL record layout.
    /// </summary>
    public record AgentRecord
    {
        /// <summary>
        /// Gets the unique code identifying the agent.
        /// </summary>
        public string AgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets the full name of the agent.
        /// </summary>
        public string AgentName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the first line of the agent's address.
        /// </summary>
        public string AgentAddress1 { get; init; } = string.Empty;

        /// <summary>
        /// Gets the second line of the agent's address.
        /// </summary>
        public string AgentAddress2 { get; init; } = string.Empty;

        /// <summary>
        /// Gets the city where the agent resides.
        /// </summary>
        public string AgentCity { get; init; } = string.Empty;

        /// <summary>
        /// Gets the state code of the agent's address.
        /// </summary>
        public string AgentState { get; init; } = string.Empty;

        /// <summary>
        /// Gets the ZIP code of the agent's address.
        /// </summary>
        public string AgentZipCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets the date of birth of the agent (format: yyyy-MM-dd).
        /// </summary>
        public string AgentDateOfBirth { get; init; } = string.Empty;

        /// <summary>
        /// Gets the type of agent (e.g., sales, support).
        /// </summary>
        public string AgentType { get; init; } = string.Empty;

        /// <summary>
        /// Gets the status of the agent (e.g., active, inactive).
        /// </summary>
        public string AgentStatus { get; init; } = string.Empty;

        /// <summary>
        /// Gets the email address of the agent.
        /// </summary>
        public string AgentEmail { get; init; } = string.Empty;

        /// <summary>
        /// Gets the contact number of the agent.
        /// </summary>
        public string AgentContactNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets the start date of the agent's employment (format: yyyy-MM-dd).
        /// </summary>
        public string AgentStartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets the end date of the agent's employment (format: yyyy-MM-dd).
        /// </summary>
        public string AgentEndDate { get; init; } = string.Empty;
    }
}