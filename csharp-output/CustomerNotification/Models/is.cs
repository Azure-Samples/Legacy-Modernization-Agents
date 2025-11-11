using System;

namespace CustomerNotification.Models
{
    /// <summary>
    /// Represents a customer notification record containing policy, customer, agent, and message information.
    /// This class is a direct conversion from the COBOL <c>WS-CUSTOMER-NOTIFY-RECORD</c> structure.
    /// </summary>
    public record CustomerNotifyRecord
    {
        /// <summary>
        /// Gets or sets the policy number associated with the customer.
        /// </summary>
        public string PolicyNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the customer's first name.
        /// </summary>
        public string FirstName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the customer's middle name or initial.
        /// </summary>
        public string MiddleName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the customer's last name.
        /// </summary>
        public string LastName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy start date (format: yyyy-MM-dd or as provided).
        /// </summary>
        public string StartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy expiry date (format: yyyy-MM-dd or as provided).
        /// </summary>
        public string ExpiryDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the notification date (format: yyyy-MM-dd or as provided).
        /// </summary>
        public string NotifyDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the notification messages for the customer.
        /// </summary>
        public string NotifyMessages { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent code associated with the customer.
        /// </summary>
        public string AgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent name associated with the customer.
        /// </summary>
        public string AgentName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the statutory message for the customer notification.
        /// </summary>
        public string StatutoryMessage { get; init; } = string.Empty;
    }
}