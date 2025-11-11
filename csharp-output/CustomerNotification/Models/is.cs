using System;

namespace CustomerNotification.Models
{
    /// <summary>
    /// Represents a customer notification record containing policy, personal, agent, and statutory message details.
    /// </summary>
    /// <remarks>
    /// This class is converted from a COBOL working-storage record structure.
    /// All fields are strings to match the original PIC X(n) definitions.
    /// </remarks>
    public record CustomerNotifyRecord
    {
        /// <summary>
        /// Gets or sets the customer's policy number.
        /// </summary>
        public string? PolicyNumber { get; init; }

        /// <summary>
        /// Gets or sets the customer's first name.
        /// </summary>
        public string? FirstName { get; init; }

        /// <summary>
        /// Gets or sets the customer's middle initial.
        /// </summary>
        public string? MiddleName { get; init; }

        /// <summary>
        /// Gets or sets the customer's last name.
        /// </summary>
        public string? LastName { get; init; }

        /// <summary>
        /// Gets or sets the policy start date (format: yyyy-MM-dd or as provided).
        /// </summary>
        public string? StartDate { get; init; }

        /// <summary>
        /// Gets or sets the policy expiry date (format: yyyy-MM-dd or as provided).
        /// </summary>
        public string? ExpiryDate { get; init; }

        /// <summary>
        /// Gets or sets the notification date (format: yyyy-MM-dd or as provided).
        /// </summary>
        public string? NotifyDate { get; init; }

        /// <summary>
        /// Gets or sets the notification messages for the customer.
        /// </summary>
        public string? NotifyMessages { get; init; }

        /// <summary>
        /// Gets or sets the agent code associated with the customer.
        /// </summary>
        public string? AgentCode { get; init; }

        /// <summary>
        /// Gets or sets the agent name associated with the customer.
        /// </summary>
        public string? AgentName { get; init; }

        /// <summary>
        /// Gets or sets the statutory message for the customer.
        /// </summary>
        public string? StatutoryMessage { get; init; }
    }
}