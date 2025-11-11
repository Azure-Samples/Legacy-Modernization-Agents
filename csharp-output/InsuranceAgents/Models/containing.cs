#nullable enable

using System;
using System.ComponentModel.DataAnnotations;

namespace InsuranceAgents.Models
{
    /// <summary>
    /// Represents an insurance agent's record containing personal, contact, and employment information.
    /// </summary>
    public record AgentRecord
    {
        /// <summary>
        /// Gets the unique agent identifier code.
        /// </summary>
        [Required]
        [MaxLength(10)]
        public string AgentCode { get; init; }

        /// <summary>
        /// Gets the agent's full name.
        /// </summary>
        [Required]
        [MaxLength(45)]
        public string AgentName { get; init; }

        /// <summary>
        /// Gets the first line of the agent's address.
        /// </summary>
        [MaxLength(50)]
        public string? AgentAddress1 { get; init; }

        /// <summary>
        /// Gets the second line of the agent's address.
        /// </summary>
        [MaxLength(50)]
        public string? AgentAddress2 { get; init; }

        /// <summary>
        /// Gets the city where the agent resides.
        /// </summary>
        [MaxLength(20)]
        public string? AgentCity { get; init; }

        /// <summary>
        /// Gets the state abbreviation for the agent's address.
        /// </summary>
        [MaxLength(2)]
        public string? AgentState { get; init; }

        /// <summary>
        /// Gets the ZIP code for the agent's address.
        /// </summary>
        [MaxLength(10)]
        public string? AgentZipCode { get; init; }

        /// <summary>
        /// Gets the agent's date of birth (ISO 8601 format recommended).
        /// </summary>
        [MaxLength(10)]
        public string? AgentDateOfBirth { get; init; }

        /// <summary>
        /// Gets the type or classification of the agent.
        /// </summary>
        [MaxLength(10)]
        public string? AgentType { get; init; }

        /// <summary>
        /// Gets the status of the agent (e.g., active/inactive).
        /// </summary>
        [MaxLength(1)]
        public string? AgentStatus { get; init; }

        /// <summary>
        /// Gets the agent's email address.
        /// </summary>
        [MaxLength(30)]
        [EmailAddress]
        public string? AgentEmail { get; init; }

        /// <summary>
        /// Gets the agent's contact number.
        /// </summary>
        [MaxLength(10)]
        public string? AgentContactNumber { get; init; }

        /// <summary>
        /// Gets the employment start date for the agent (ISO 8601 format recommended).
        /// </summary>
        [MaxLength(10)]
        public string? AgentStartDate { get; init; }

        /// <summary>
        /// Gets the employment end date for the agent (ISO 8601 format recommended).
        /// </summary>
        [MaxLength(10)]
        public string? AgentEndDate { get; init; }
    }
}