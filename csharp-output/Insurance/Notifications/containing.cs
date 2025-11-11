#nullable enable

using System;
using System.Threading.Tasks;

namespace Insurance.Notifications
{
    /// <summary>
    /// Represents an agent notification record containing details about the agent,
    /// associated policy, and notification messages.
    /// </summary>
    public sealed record AgentNotifyRecord
    {
        /// <summary>
        /// Gets the unique code identifying the agent.
        /// </summary>
        public string AgentCode { get; init; }

        /// <summary>
        /// Gets the full name of the agent.
        /// </summary>
        public string AgentName { get; init; }

        /// <summary>
        /// Gets the first line of the agent's address.
        /// </summary>
        public string AgentAddress1 { get; init; }

        /// <summary>
        /// Gets the second line of the agent's address.
        /// </summary>
        public string AgentAddress2 { get; init; }

        /// <summary>
        /// Gets the city where the agent is located.
        /// </summary>
        public string AgentCity { get; init; }

        /// <summary>
        /// Gets the state code for the agent's location.
        /// </summary>
        public string AgentState { get; init; }

        /// <summary>
        /// Gets the policy number associated with the notification.
        /// </summary>
        public string PolicyNumber { get; init; }

        /// <summary>
        /// Gets the first name of the policy holder.
        /// </summary>
        public string PolicyHolderFirstName { get; init; }

        /// <summary>
        /// Gets the middle initial of the policy holder.
        /// </summary>
        public string PolicyHolderMiddleInitial { get; init; }

        /// <summary>
        /// Gets the last name of the policy holder.
        /// </summary>
        public string PolicyHolderLastName { get; init; }

        /// <summary>
        /// Gets the start date of the policy (format: yyyy-MM-dd).
        /// </summary>
        public string PolicyStartDate { get; init; }

        /// <summary>
        /// Gets the expiry date of the policy (format: yyyy-MM-dd).
        /// </summary>
        public string PolicyExpiryDate { get; init; }

        /// <summary>
        /// Gets the date the notification was sent (format: yyyy-MM-dd).
        /// </summary>
        public string NotifyDate { get; init; }

        /// <summary>
        /// Gets the notification messages associated with the agent and policy.
        /// </summary>
        public string NotifyMessages { get; init; }

        /// <summary>
        /// Initializes a new instance of the <see cref="AgentNotifyRecord"/> record.
        /// </summary>
        /// <param name="agentCode">The agent code.</param>
        /// <param name="agentName">The agent name.</param>
        /// <param name="agentAddress1">The first line of the agent's address.</param>
        /// <param name="agentAddress2">The second line of the agent's address.</param>
        /// <param name="agentCity">The agent's city.</param>
        /// <param name="agentState">The agent's state.</param>
        /// <param name="policyNumber">The policy number.</param>
        /// <param name="policyHolderFirstName">The policy holder's first name.</param>
        /// <param name="policyHolderMiddleInitial">The policy holder's middle initial.</param>
        /// <param name="policyHolderLastName">The policy holder's last name.</param>
        /// <param name="policyStartDate">The policy start date.</param>
        /// <param name="policyExpiryDate">The policy expiry date.</param>
        /// <param name="notifyDate">The notification date.</param>
        /// <param name="notifyMessages">The notification messages.</param>
        public AgentNotifyRecord(
            string agentCode,
            string agentName,
            string agentAddress1,
            string agentAddress2,
            string agentCity,
            string agentState,
            string policyNumber,
            string policyHolderFirstName,
            string policyHolderMiddleInitial,
            string policyHolderLastName,
            string policyStartDate,
            string policyExpiryDate,
            string notifyDate,
            string notifyMessages)
        {
            AgentCode = agentCode;
            AgentName = agentName;
            AgentAddress1 = agentAddress1;
            AgentAddress2 = agentAddress2;
            AgentCity = agentCity;
            AgentState = agentState;
            PolicyNumber = policyNumber;
            PolicyHolderFirstName = policyHolderFirstName;
            PolicyHolderMiddleInitial = policyHolderMiddleInitial;
            PolicyHolderLastName = policyHolderLastName;
            PolicyStartDate = policyStartDate;
            PolicyExpiryDate = policyExpiryDate;
            NotifyDate = notifyDate;
            NotifyMessages = notifyMessages;
        }

        /// <summary>
        /// Asynchronously creates an <see cref="AgentNotifyRecord"/> from provided data.
        /// </summary>
        /// <param name="agentCode">The agent code.</param>
        /// <param name="agentName">The agent name.</param>
        /// <param name="agentAddress1">The first line of the agent's address.</param>
        /// <param name="agentAddress2">The second line of the agent's address.</param>
        /// <param name="agentCity">The agent's city.</param>
        /// <param name="agentState">The agent's state.</param>
        /// <param name="policyNumber">The policy number.</param>
        /// <param name="policyHolderFirstName">The policy holder's first name.</param>
        /// <param name="policyHolderMiddleInitial">The policy holder's middle initial.</param>
        /// <param name="policyHolderLastName">The policy holder's last name.</param>
        /// <param name="policyStartDate">The policy start date.</param>
        /// <param name="policyExpiryDate">The policy expiry date.</param>
        /// <param name="notifyDate">The notification date.</param>
        /// <param name="notifyMessages">The notification messages.</param>
        /// <returns>A task that represents the asynchronous creation of the record.</returns>
        public static async Task<AgentNotifyRecord> CreateAsync(
            string agentCode,
            string agentName,
            string agentAddress1,
            string agentAddress2,
            string agentCity,
            string agentState,
            string policyNumber,
            string policyHolderFirstName,
            string policyHolderMiddleInitial,
            string policyHolderLastName,
            string policyStartDate,
            string policyExpiryDate,
            string notifyDate,
            string notifyMessages)
        {
            // Simulate async operation (e.g., validation, external lookup)
            await Task.Yield();
            return new AgentNotifyRecord(
                agentCode,
                agentName,
                agentAddress1,
                agentAddress2,
                agentCity,
                agentState,
                policyNumber,
                policyHolderFirstName,
                policyHolderMiddleInitial,
                policyHolderLastName,
                policyStartDate,
                policyExpiryDate,
                notifyDate,
                notifyMessages);
        }
    }
}