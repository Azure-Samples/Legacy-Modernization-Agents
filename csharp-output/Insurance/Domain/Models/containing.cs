using System;

namespace Insurance.Domain.Models
{
    /// <summary>
    /// Represents an insurance policy record containing all relevant details about the policy,
    /// policy holder, beneficiary, payment information, underwriting, and status.
    /// </summary>
    public record PolicyRecord
    {
        /// <summary>
        /// Gets or sets the unique policy number.
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
        /// Gets or sets the name of the beneficiary.
        /// </summary>
        public string PolicyBeneficiaryName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the relationship of the beneficiary to the policy holder.
        /// </summary>
        public string PolicyBeneficiaryRelation { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the first line of the policy holder's address.
        /// </summary>
        public string PolicyHolderAddress1 { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the second line of the policy holder's address.
        /// </summary>
        public string PolicyHolderAddress2 { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the city of the policy holder.
        /// </summary>
        public string PolicyHolderCity { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the state of the policy holder.
        /// </summary>
        public string PolicyHolderState { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the ZIP code of the policy holder.
        /// </summary>
        public string PolicyHolderZipCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the date of birth of the policy holder (format: yyyy-MM-dd).
        /// </summary>
        public string PolicyHolderDateOfBirth { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the gender of the policy holder.
        /// </summary>
        public string PolicyHolderGender { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the phone number of the policy holder.
        /// </summary>
        public string PolicyHolderPhone { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the email address of the policy holder.
        /// </summary>
        public string PolicyHolderEmail { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the payment frequency for the policy (e.g., Monthly, Quarterly).
        /// </summary>
        public string PolicyPaymentFrequency { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the payment method for the policy (e.g., Direct Debit, Credit Card).
        /// </summary>
        public string PolicyPaymentMethod { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the underwriter for the policy.
        /// </summary>
        public string PolicyUnderwriter { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the terms and conditions of the policy.
        /// </summary>
        public string PolicyTermsAndConditions { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets a flag indicating whether the policy has been claimed ('Y' or 'N').
        /// </summary>
        public string PolicyClaimed { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the discount code applied to the policy, if any.
        /// </summary>
        public string PolicyDiscountCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the premium amount for the policy.
        /// </summary>
        public decimal PolicyPremiumAmount { get; init; }

        /// <summary>
        /// Gets or sets the type of the policy.
        /// </summary>
        public string PolicyType { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the start date of the policy (format: yyyy-MM-dd).
        /// </summary>
        public string PolicyStartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the expiry date of the policy (format: yyyy-MM-dd).
        /// </summary>
        public string PolicyExpiryDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the status of the policy (e.g., Active, Expired).
        /// </summary>
        public string PolicyStatus { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent code associated with the policy.
        /// </summary>
        public string PolicyAgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets a flag indicating whether notifications are enabled for the policy ('Y' or 'N').
        /// </summary>
        public string PolicyNotifyFlag { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp when the policy was added (format: yyyy-MM-ddTHH:mm:ss.ffffff).
        /// </summary>
        public string PolicyAddTimestamp { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp when the policy was last updated (format: yyyy-MM-ddTHH:mm:ss.ffffff).
        /// </summary>
        public string PolicyUpdateTimestamp { get; init; } = string.Empty;
    }
}