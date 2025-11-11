using System;

namespace CustomerNotification.Models
{
    /// <summary>
    /// Represents a customer notification record containing policy, customer, agent, and statutory message details.
    /// </summary>
    /// <remarks>
    /// This record is mapped from the COBOL WS-CUSTOMER-NOTIFY-RECORD structure.
    /// All fields are strings, matching the original PIC X(n) definitions.
    /// </remarks>
    public record CustomerNotifyRecord
    {
        /// <summary>
        /// Gets the customer policy number.
        /// </summary>
        public string PolicyNumber { get; init; }

        /// <summary>
        /// Gets the customer's first name.
        /// </summary>
        public string FirstName { get; init; }

        /// <summary>
        /// Gets the customer's middle initial.
        /// </summary>
        public string MiddleName { get; init; }

        /// <summary>
        /// Gets the customer's last name.
        /// </summary>
        public string LastName { get; init; }

        /// <summary>
        /// Gets the policy start date (format: yyyy-MM-dd or as provided).
        /// </summary>
        public string StartDate { get; init; }

        /// <summary>
        /// Gets the policy expiry date (format: yyyy-MM-dd or as provided).
        /// </summary>
        public string ExpiryDate { get; init; }

        /// <summary>
        /// Gets the notification date (format: yyyy-MM-dd or as provided).
        /// </summary>
        public string NotifyDate { get; init; }

        /// <summary>
        /// Gets the notification messages.
        /// </summary>
        public string NotifyMessages { get; init; }

        /// <summary>
        /// Gets the agent code.
        /// </summary>
        public string AgentCode { get; init; }

        /// <summary>
        /// Gets the agent name.
        /// </summary>
        public string AgentName { get; init; }

        /// <summary>
        /// Gets the statutory message.
        /// </summary>
        public string StatutoryMessage { get; init; }

        /// <summary>
        /// Initializes a new instance of the <see cref="CustomerNotifyRecord"/> record.
        /// </summary>
        /// <param name="policyNumber">Customer policy number.</param>
        /// <param name="firstName">Customer first name.</param>
        /// <param name="middleName">Customer middle initial.</param>
        /// <param name="lastName">Customer last name.</param>
        /// <param name="startDate">Policy start date.</param>
        /// <param name="expiryDate">Policy expiry date.</param>
        /// <param name="notifyDate">Notification date.</param>
        /// <param name="notifyMessages">Notification messages.</param>
        /// <param name="agentCode">Agent code.</param>
        /// <param name="agentName">Agent name.</param>
        /// <param name="statutoryMessage">Statutory message.</param>
        public CustomerNotifyRecord(
            string policyNumber,
            string firstName,
            string middleName,
            string lastName,
            string startDate,
            string expiryDate,
            string notifyDate,
            string notifyMessages,
            string agentCode,
            string agentName,
            string statutoryMessage)
        {
            PolicyNumber = policyNumber;
            FirstName = firstName;
            MiddleName = middleName;
            LastName = lastName;
            StartDate = startDate;
            ExpiryDate = expiryDate;
            NotifyDate = notifyDate;
            NotifyMessages = notifyMessages;
            AgentCode = agentCode;
            AgentName = agentName;
            StatutoryMessage = statutoryMessage;
        }
    }
}