using System;

namespace InsuranceAgents.Models
{
    /// <summary>
    /// Represents an insurance agent's record, including identification, contact information, status, and employment dates.
    /// </summary>
    public record AgentRecord
    {
        /// <summary>
        /// Gets or sets the unique code identifying the agent.
        /// </summary>
        public string AgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the full name of the agent.
        /// </summary>
        public string AgentName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the first line of the agent's address.
        /// </summary>
        public string AgentAddress1 { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the second line of the agent's address.
        /// </summary>
        public string AgentAddress2 { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the city where the agent resides.
        /// </summary>
        public string AgentCity { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the state code for the agent's address.
        /// </summary>
        public string AgentState { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the ZIP code for the agent's address.
        /// </summary>
        public string AgentZipCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent's date of birth (format: yyyy-MM-dd).
        /// </summary>
        public string AgentDateOfBirth { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the type of agent (e.g., 'Broker', 'Employee').
        /// </summary>
        public string AgentType { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the status of the agent (e.g., 'A' for active, 'I' for inactive).
        /// </summary>
        public string AgentStatus { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent's email address.
        /// </summary>
        public string AgentEmail { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent's contact number.
        /// </summary>
        public string AgentContactNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent's employment start date (format: yyyy-MM-dd).
        /// </summary>
        public string AgentStartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent's employment end date (format: yyyy-MM-dd).
        /// </summary>
        public string AgentEndDate { get; init; } = string.Empty;
    }
}