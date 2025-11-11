#nullable enable

using System;

namespace InsuranceAgentNotifications.Models
{
    /// <summary>
    /// Represents a notification record for an insurance agent and associated policy.
    /// </summary>
    public record AgentNotifyRecord
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
        /// Gets or sets the city where the agent is located.
        /// </summary>
        public string AgentCity { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the state code for the agent's location.
        /// </summary>
        public string AgentState { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy number associated with the notification.
        /// </summary>
        public string PolicyNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the first name of the policy holder.
        /// </summary>
        public string PolicyHolderFirstName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the middle initial of the policy holder.
        /// </summary>
        public string PolicyHolderMiddleInitial { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the last name of the policy holder.
        /// </summary>
        public string PolicyHolderLastName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the start date of the policy (as a string, format: yyyy-MM-dd).
        /// </summary>
        public string PolicyStartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the expiry date of the policy (as a string, format: yyyy-MM-dd).
        /// </summary>
        public string PolicyExpiryDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the date of notification (as a string, format: yyyy-MM-dd).
        /// </summary>
        public string NotifyDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the notification messages related to the agent and policy.
        /// </summary>
        public string NotifyMessages { get; init; } = string.Empty;

        /// <summary>
        /// Attempts to parse the policy start date as a <see cref="DateTime"/>.
        /// </summary>
        /// <returns>The parsed <see cref="DateTime"/> if successful; otherwise, <c>null</c>.</returns>
        public DateTime? GetPolicyStartDate()
        {
            if (DateTime.TryParse(PolicyStartDate, out var date))
                return date;
            return null;
        }

        /// <summary>
        /// Attempts to parse the policy expiry date as a <see cref="DateTime"/>.
        /// </summary>
        /// <returns>The parsed <see cref="DateTime"/> if successful; otherwise, <c>null</c>.</returns>
        public DateTime? GetPolicyExpiryDate()
        {
            if (DateTime.TryParse(PolicyExpiryDate, out var date))
                return date;
            return null;
        }

        /// <summary>
        /// Attempts to parse the notification date as a <see cref="DateTime"/>.
        /// </summary>
        /// <returns>The parsed <see cref="DateTime"/> if successful; otherwise, <c>null</c>.</returns>
        public DateTime? GetNotifyDate()
        {
            if (DateTime.TryParse(NotifyDate, out var date))
                return date;
            return null;
        }
    }
}