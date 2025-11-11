using System;

namespace Insurance.Data.Models
{
    /// <summary>
    /// Represents a policy record in the INSURNCE.TPOLICY table.
    /// </summary>
    public record Policy
    {
        /// <summary>
        /// Gets the unique policy number.
        /// </summary>
        public string PolicyNumber { get; init; }

        /// <summary>
        /// Gets the policy holder's first name.
        /// </summary>
        public string PolicyHolderFirstName { get; init; }

        /// <summary>
        /// Gets the policy holder's middle initial.
        /// </summary>
        public string PolicyHolderMiddleName { get; init; }

        /// <summary>
        /// Gets the policy holder's last name.
        /// </summary>
        public string PolicyHolderLastName { get; init; }

        /// <summary>
        /// Gets the beneficiary's name.
        /// </summary>
        public string PolicyBeneficiaryName { get; init; }

        /// <summary>
        /// Gets the beneficiary's relation to the policy holder.
        /// </summary>
        public string PolicyBeneficiaryRelation { get; init; }

        /// <summary>
        /// Gets the first line of the policy holder's address.
        /// </summary>
        public string PolicyHolderAddress1 { get; init; }

        /// <summary>
        /// Gets the second line of the policy holder's address.
        /// </summary>
        public string PolicyHolderAddress2 { get; init; }

        /// <summary>
        /// Gets the policy holder's city.
        /// </summary>
        public string PolicyHolderCity { get; init; }

        /// <summary>
        /// Gets the policy holder's state.
        /// </summary>
        public string PolicyHolderState { get; init; }

        /// <summary>
        /// Gets the policy holder's ZIP code.
        /// </summary>
        public string PolicyHolderZipCode { get; init; }

        /// <summary>
        /// Gets the policy holder's date of birth (format: yyyy-MM-dd).
        /// </summary>
        public string PolicyHolderDateOfBirth { get; init; }

        /// <summary>
        /// Gets the policy holder's gender.
        /// </summary>
        public string PolicyHolderGender { get; init; }

        /// <summary>
        /// Gets the policy holder's phone number.
        /// </summary>
        public string PolicyHolderPhone { get; init; }

        /// <summary>
        /// Gets the policy holder's email address.
        /// </summary>
        public string PolicyHolderEmail { get; init; }

        /// <summary>
        /// Gets the payment frequency for the policy.
        /// </summary>
        public string PolicyPaymentFrequency { get; init; }

        /// <summary>
        /// Gets the payment method for the policy.
        /// </summary>
        public string PolicyPaymentMethod { get; init; }

        /// <summary>
        /// Gets the underwriter for the policy.
        /// </summary>
        public string PolicyUnderwriter { get; init; }

        /// <summary>
        /// Gets the terms and conditions of the policy.
        /// </summary>
        public string PolicyTermsAndConditions { get; init; }

        /// <summary>
        /// Gets a value indicating whether the policy has been claimed ("Y"/"N").
        /// </summary>
        public string PolicyClaimed { get; init; }

        /// <summary>
        /// Gets the discount code applied to the policy.
        /// </summary>
        public string PolicyDiscountCode { get; init; }

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
        public string PolicyType { get; init; }

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
        public string PolicyStatus { get; init; }

        /// <summary>
        /// Gets the agent code associated with the policy.
        /// </summary>
        public string PolicyAgentCode { get; init; }

        /// <summary>
        /// Gets the notification flag for the policy ("Y"/"N").
        /// </summary>
        public string PolicyNotifyFlag { get; init; }

        /// <summary>
        /// Gets the timestamp when the policy was added.
        /// </summary>
        public DateTime PolicyAddTimestamp { get; init; }

        /// <summary>
        /// Gets the timestamp when the policy was last updated.
        /// </summary>
        public DateTime PolicyUpdateTimestamp { get; init; }
    }

    /// <summary>
    /// Defines the contract for a repository that manages Policy records.
    /// </summary>
    public interface IPolicyRepository
    {
        /// <summary>
        /// Asynchronously retrieves a policy by its policy number.
        /// </summary>
        /// <param name="policyNumber">The unique policy number.</param>
        /// <returns>The matching <see cref="Policy"/> if found; otherwise, <c>null</c>.</returns>
        Task<Policy?> GetPolicyByNumberAsync(string policyNumber);

        /// <summary>
        /// Asynchronously adds a new policy record.
        /// </summary>
        /// <param name="policy">The policy to add.</param>
        /// <returns>A task representing the asynchronous operation.</returns>
        Task AddPolicyAsync(Policy policy);

        /// <summary>
        /// Asynchronously updates an existing policy record.
        /// </summary>
        /// <param name="policy">The policy to update.</param>
        /// <returns>A task representing the asynchronous operation.</returns>
        Task UpdatePolicyAsync(Policy policy);

        /// <summary>
        /// Asynchronously deletes a policy by its policy number.
        /// </summary>
        /// <param name="policyNumber">The unique policy number.</param>
        /// <returns>A task representing the asynchronous operation.</returns>
        Task DeletePolicyAsync(string policyNumber);
    }
}