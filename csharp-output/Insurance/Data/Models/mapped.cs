using System;

namespace Insurance.Data.Models
{
    /// <summary>
    /// Represents an insurance policy record mapped from the INSURNCE.TPOLICY DB2 table.
    /// </summary>
    public record Policy
    {
        /// <summary>
        /// Gets the unique policy number.
        /// </summary>
        public string PolicyNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets the first name of the policy holder.
        /// </summary>
        public string PolicyHolderFirstName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the middle initial of the policy holder.
        /// </summary>
        public string PolicyHolderMiddleName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the last name of the policy holder.
        /// </summary>
        public string PolicyHolderLastName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the name of the beneficiary.
        /// </summary>
        public string PolicyBeneficiaryName { get; init; } = string.Empty;

        /// <summary>
        /// Gets the relationship of the beneficiary to the policy holder.
        /// </summary>
        public string PolicyBeneficiaryRelation { get; init; } = string.Empty;

        /// <summary>
        /// Gets the first line of the policy holder's address.
        /// </summary>
        public string PolicyHolderAddress1 { get; init; } = string.Empty;

        /// <summary>
        /// Gets the second line of the policy holder's address.
        /// </summary>
        public string PolicyHolderAddress2 { get; init; } = string.Empty;

        /// <summary>
        /// Gets the city of the policy holder.
        /// </summary>
        public string PolicyHolderCity { get; init; } = string.Empty;

        /// <summary>
        /// Gets the state of the policy holder.
        /// </summary>
        public string PolicyHolderState { get; init; } = string.Empty;

        /// <summary>
        /// Gets the ZIP code of the policy holder.
        /// </summary>
        public string PolicyHolderZipCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets the date of birth of the policy holder (format: yyyy-MM-dd).
        /// </summary>
        public DateTime PolicyHolderDateOfBirth { get; init; }

        /// <summary>
        /// Gets the gender of the policy holder.
        /// </summary>
        public string PolicyHolderGender { get; init; } = string.Empty;

        /// <summary>
        /// Gets the phone number of the policy holder.
        /// </summary>
        public string PolicyHolderPhone { get; init; } = string.Empty;

        /// <summary>
        /// Gets the email address of the policy holder.
        /// </summary>
        public string PolicyHolderEmail { get; init; } = string.Empty;

        /// <summary>
        /// Gets the payment frequency for the policy.
        /// </summary>
        public string PolicyPaymentFrequency { get; init; } = string.Empty;

        /// <summary>
        /// Gets the payment method for the policy.
        /// </summary>
        public string PolicyPaymentMethod { get; init; } = string.Empty;

        /// <summary>
        /// Gets the underwriter for the policy.
        /// </summary>
        public string PolicyUnderwriter { get; init; } = string.Empty;

        /// <summary>
        /// Gets the terms and conditions of the policy.
        /// </summary>
        public string PolicyTermsAndConditions { get; init; } = string.Empty;

        /// <summary>
        /// Gets a value indicating whether the policy has been claimed ('Y' or 'N').
        /// </summary>
        public string PolicyClaimed { get; init; } = string.Empty;

        /// <summary>
        /// Gets the discount code applied to the policy.
        /// </summary>
        public string PolicyDiscountCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets the premium amount for the policy.
        /// </summary>
        public decimal PolicyPremiumAmount { get; init; }

        /// <summary>
        /// Gets the coverage amount for the policy.
        /// </summary>
        public decimal PolicyCoverageAmount { get; init; }

        /// <summary>
        /// Gets the type of the policy.
        /// </summary>
        public string PolicyType { get; init; } = string.Empty;

        /// <summary>
        /// Gets the start date of the policy.
        /// </summary>
        public DateTime PolicyStartDate { get; init; }

        /// <summary>
        /// Gets the expiry date of the policy.
        /// </summary>
        public DateTime PolicyExpiryDate { get; init; }

        /// <summary>
        /// Gets the status of the policy.
        /// </summary>
        public string PolicyStatus { get; init; } = string.Empty;

        /// <summary>
        /// Gets the agent code associated with the policy.
        /// </summary>
        public string PolicyAgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets the notification flag for the policy ('Y' or 'N').
        /// </summary>
        public string PolicyNotifyFlag { get; init; } = string.Empty;

        /// <summary>
        /// Gets the timestamp when the policy was added.
        /// </summary>
        public DateTime PolicyAddTimestamp { get; init; }

        /// <summary>
        /// Gets the timestamp when the policy was last updated.
        /// </summary>
        public DateTime PolicyUpdateTimestamp { get; init; }
    }
}