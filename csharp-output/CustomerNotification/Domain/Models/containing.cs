using System;
using System.Threading.Tasks;

namespace CustomerNotification.Domain.Models
{
    /// <summary>
    /// Represents a customer notification record containing policy, personal, agent, and message details.
    /// </summary>
    public sealed record CustomerNotifyRecord
    {
        /// <summary>
        /// Gets the customer policy number.
        /// </summary>
        public string PolicyNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets the customer's first name.
        /// </summary>
        public string FirstName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the customer's middle initial or name.
        /// </summary>
        public string MiddleName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the customer's last name.
        /// </summary>
        public string LastName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the policy start date (ISO 8601 format recommended).
        /// </summary>
        public string StartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets the policy expiry date (ISO 8601 format recommended).
        /// </summary>
        public string ExpiryDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets the notification date (ISO 8601 format recommended).
        /// </summary>
        public string NotifyDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets the notification messages for the customer.
        /// </summary>
        public string NotifyMessages { get; init; } = string.Empty;

        /// <summary>
        /// Gets the agent code associated with the customer.
        /// </summary>
        public string AgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets the agent name associated with the customer.
        /// </summary>
        public string AgentName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the statutory message for the customer.
        /// </summary>
        public string StatutoryMessage { get; init; } = string.Empty;
    }
}