using System;

namespace AgentManagement.Models
{
    /// <summary>
    /// Represents an agent's information, including personal details, contact information, and employment status.
    /// </summary>
    public sealed record AgentRecord
    {
        /// <summary>
        /// Gets the unique agent code or identifier.
        /// </summary>
        public string AgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets the agent's full name.
        /// </summary>
        public string AgentName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the primary address line.
        /// </summary>
        public string AgentAddress1 { get; init; } = string.Empty;

        /// <summary>
        /// Gets the secondary address line.
        /// </summary>
        public string AgentAddress2 { get; init; } = string.Empty;

        /// <summary>
        /// Gets the city of the agent.
        /// </summary>
        public string AgentCity { get; init; } = string.Empty;

        /// <summary>
        /// Gets the state abbreviation.
        /// </summary>
        public string AgentState { get; init; } = string.Empty;

        /// <summary>
        /// Gets the ZIP code.
        /// </summary>
        public string AgentZipCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets the agent's date of birth (ISO 8601 format recommended).
        /// </summary>
        public string AgentDateOfBirth { get; init; } = string.Empty;

        /// <summary>
        /// Gets the agent type or classification.
        /// </summary>
        public string AgentType { get; init; } = string.Empty;

        /// <summary>
        /// Gets the agent status (e.g., active/inactive).
        /// </summary>
        public string AgentStatus { get; init; } = string.Empty;

        /// <summary>
        /// Gets the agent's email address.
        /// </summary>
        public string AgentEmail { get; init; } = string.Empty;

        /// <summary>
        /// Gets the agent's contact number.
        /// </summary>
        public string AgentContactNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets the employment start date (ISO 8601 format recommended).
        /// </summary>
        public string AgentStartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets the employment end date (ISO 8601 format recommended).
        /// </summary>
        public string AgentEndDate { get; init; } = string.Empty;
    }
}