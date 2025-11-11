#nullable enable

using System;
using System.Threading.Tasks;

namespace InsuranceAgentNotifications.Models
{
    /// <summary>
    /// Represents a notification record for an insurance agent, including agent details,
    /// policy information, and notification messages.
    /// </summary>
    public record AgentNotifyRecord
    {
        /// <summary>
        /// Gets or sets the unique code identifying the agent.
        /// </summary>
        public string AgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent's full name.
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
        /// Gets or sets the state abbreviation for the agent's location.
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
        /// Gets or sets the start date of the policy (format: yyyy-MM-dd).
        /// </summary>
        public string PolicyStartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the expiry date of the policy (format: yyyy-MM-dd).
        /// </summary>
        public string PolicyExpiryDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the date the notification was sent (format: yyyy-MM-dd).
        /// </summary>
        public string NotifyDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the notification message(s) for the agent.
        /// </summary>
        public string NotifyMessages { get; init; } = string.Empty;
    }
}

namespace InsuranceAgentNotifications.Services
{
    /// <summary>
    /// Defines operations for handling agent notification records.
    /// </summary>
    public interface IAgentNotifyService
    {
        /// <summary>
        /// Asynchronously processes an agent notification record.
        /// </summary>
        /// <param name="record">The agent notification record to process.</param>
        /// <returns>A task representing the asynchronous operation.</returns>
        Task ProcessNotificationAsync(Models.AgentNotifyRecord record);
    }

    /// <summary>
    /// Provides functionality for processing agent notification records.
    /// </summary>
    public class AgentNotifyService : IAgentNotifyService
    {
        /// <summary>
        /// Asynchronously processes an agent notification record.
        /// </summary>
        /// <param name="record">The agent notification record to process.</param>
        /// <returns>A task representing the asynchronous operation.</returns>
        /// <exception cref="ArgumentNullException">Thrown when the record is null.</exception>
        public async Task ProcessNotificationAsync(Models.AgentNotifyRecord record)
        {
            if (record is null)
                throw new ArgumentNullException(nameof(record), "Agent notification record cannot be null.");

            try
            {
                // Simulate async processing (e.g., saving to database, sending notification)
                await Task.Delay(100); // Placeholder for actual async logic

                // Example: Log the notification (replace with real logging in production)
                Console.WriteLine($"Notification processed for agent: {record.AgentCode}, policy: {record.PolicyNumber}");
            }
            catch (Exception ex)
            {
                // Handle errors using standard .NET exception handling and logging
                // Replace with a proper logging framework in production
                Console.Error.WriteLine($"Error processing notification: {ex.Message}");
                throw;
            }
        }
    }
}