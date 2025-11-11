#nullable enable

using System;
using System.ComponentModel.DataAnnotations;

namespace Insurance.Data.Models
{
    /// <summary>
    /// Represents an insurance policy record mapped from the COBOL INSURNCE.TPOLICY DB2 table.
    /// </summary>
    public record Policy
    {
        /// <summary>
        /// Gets or sets the unique policy number.
        /// </summary>
        [Required, StringLength(10)]
        public string PolicyNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's first name.
        /// </summary>
        [Required, StringLength(35)]
        public string PolicyHolderFirstName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's middle initial.
        /// </summary>
        [Required, StringLength(1)]
        public string PolicyHolderMiddleName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's last name.
        /// </summary>
        [Required, StringLength(35)]
        public string PolicyHolderLastName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the beneficiary's name.
        /// </summary>
        [Required, StringLength(60)]
        public string PolicyBeneficiaryName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the beneficiary's relation to the policy holder.
        /// </summary>
        [Required, StringLength(15)]
        public string PolicyBeneficiaryRelation { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the first line of the policy holder's address.
        /// </summary>
        [Required, StringLength(100)]
        public string PolicyHolderAddress1 { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the second line of the policy holder's address.
        /// </summary>
        [Required, StringLength(100)]
        public string PolicyHolderAddress2 { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's city.
        /// </summary>
        [Required, StringLength(30)]
        public string PolicyHolderCity { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's state.
        /// </summary>
        [Required, StringLength(2)]
        public string PolicyHolderState { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's ZIP code.
        /// </summary>
        [Required, StringLength(10)]
        public string PolicyHolderZipCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's date of birth (format: yyyy-MM-dd).
        /// </summary>
        [Required, StringLength(10)]
        public string PolicyHolderDateOfBirth { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's gender.
        /// </summary>
        [Required, StringLength(8)]
        public string PolicyHolderGender { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's phone number.
        /// </summary>
        [Required, StringLength(10)]
        public string PolicyHolderPhone { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's email address.
        /// </summary>
        [Required, StringLength(30)]
        public string PolicyHolderEmail { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the payment frequency for the policy.
        /// </summary>
        [Required, StringLength(10)]
        public string PolicyPaymentFrequency { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the payment method for the policy.
        /// </summary>
        [Required, StringLength(8)]
        public string PolicyPaymentMethod { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the underwriter for the policy.
        /// </summary>
        [Required, StringLength(50)]
        public string PolicyUnderwriter { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the terms and conditions of the policy.
        /// </summary>
        [Required, StringLength(200)]
        public string PolicyTermsAndConditions { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the claimed flag for the policy ('Y' or 'N').
        /// </summary>
        [Required, StringLength(1)]
        public string PolicyClaimed { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the discount code for the policy.
        /// </summary>
        [Required, StringLength(10)]
        public string PolicyDiscountCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the premium amount for the policy.
        /// </summary>
        [Required]
        [Range(typeof(decimal), "0", "99999.99")]
        public decimal PolicyPremiumAmount { get; init; }

        /// <summary>
        /// Gets or sets the coverage amount for the policy.
        /// </summary>
        [Required]
        [Range(typeof(decimal), "0", "99999999.99")]
        public decimal PolicyCoverageAmount { get; init; }

        /// <summary>
        /// Gets or sets the type of the policy.
        /// </summary>
        [Required, StringLength(50)]
        public string PolicyType { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the start date of the policy.
        /// </summary>
        [Required]
        public DateTime PolicyStartDate { get; init; }

        /// <summary>
        /// Gets or sets the expiry date of the policy.
        /// </summary>
        [Required]
        public DateTime PolicyExpiryDate { get; init; }

        /// <summary>
        /// Gets or sets the status of the policy.
        /// </summary>
        [Required, StringLength(1)]
        public string PolicyStatus { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent code associated with the policy.
        /// </summary>
        [Required, StringLength(10)]
        public string PolicyAgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the notification flag for the policy ('Y' or 'N').
        /// </summary>
        [Required, StringLength(1)]
        public string PolicyNotifyFlag { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp when the policy was added.
        /// </summary>
        [Required]
        public DateTime PolicyAddTimestamp { get; init; }

        /// <summary>
        /// Gets or sets the timestamp when the policy was last updated.
        /// </summary>
        [Required]
        public DateTime PolicyUpdateTimestamp { get; init; }
    }
}