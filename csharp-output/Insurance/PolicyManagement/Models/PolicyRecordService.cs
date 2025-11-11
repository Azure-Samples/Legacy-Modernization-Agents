using System;
using System.ComponentModel.DataAnnotations;
using System.Threading.Tasks;

namespace Insurance.PolicyManagement.Models
{
    /// <summary>
    /// Represents an insurance policy record containing all relevant policy, holder, beneficiary, payment, and agent information.
    /// </summary>
    public record PolicyRecord
    {
        /// <summary>
        /// Gets or sets the unique policy number.
        /// </summary>
        [Required, StringLength(10)]
        public string PolicyNumber { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's first name.
        /// </summary>
        [StringLength(35)]
        public string PolicyHolderFirstName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's middle initial.
        /// </summary>
        [StringLength(1)]
        public string PolicyHolderMiddleName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's last name.
        /// </summary>
        [StringLength(35)]
        public string PolicyHolderLastName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the beneficiary's name.
        /// </summary>
        [StringLength(60)]
        public string PolicyBeneficiaryName { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the beneficiary's relation to the policy holder.
        /// </summary>
        [StringLength(15)]
        public string PolicyBeneficiaryRelation { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the first line of the policy holder's address.
        /// </summary>
        [StringLength(100)]
        public string PolicyHolderAddress1 { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the second line of the policy holder's address.
        /// </summary>
        [StringLength(100)]
        public string PolicyHolderAddress2 { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's city.
        /// </summary>
        [StringLength(30)]
        public string PolicyHolderCity { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's state.
        /// </summary>
        [StringLength(2)]
        public string PolicyHolderState { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's ZIP code.
        /// </summary>
        [StringLength(10)]
        public string PolicyHolderZipCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's date of birth (ISO 8601 format recommended).
        /// </summary>
        [StringLength(10)]
        public string PolicyHolderDateOfBirth { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's gender.
        /// </summary>
        [StringLength(8)]
        public string PolicyHolderGender { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's phone number.
        /// </summary>
        [StringLength(10)]
        public string PolicyHolderPhone { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy holder's email address.
        /// </summary>
        [StringLength(30)]
        public string PolicyHolderEmail { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the payment frequency (e.g., Monthly, Quarterly).
        /// </summary>
        [StringLength(10)]
        public string PolicyPaymentFrequency { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the payment method (e.g., Direct Debit, Credit Card).
        /// </summary>
        [StringLength(8)]
        public string PolicyPaymentMethod { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the underwriter's name.
        /// </summary>
        [StringLength(50)]
        public string PolicyUnderwriter { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the terms and conditions of the policy.
        /// </summary>
        [StringLength(200)]
        public string PolicyTermsAndConditions { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets a value indicating whether the policy has been claimed ('Y' or 'N').
        /// </summary>
        [StringLength(1)]
        public string PolicyClaimed { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the discount code applied to the policy.
        /// </summary>
        [StringLength(10)]
        public string PolicyDiscountCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the premium amount for the policy.
        /// </summary>
        [Range(0, 99999.99)]
        public decimal PolicyPremiumAmount { get; init; }

        /// <summary>
        /// Gets or sets the type of policy.
        /// </summary>
        [StringLength(50)]
        public string PolicyType { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy start date (ISO 8601 format recommended).
        /// </summary>
        [StringLength(10)]
        public string PolicyStartDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the policy expiry date (ISO 8601 format recommended).
        /// </summary>
        [StringLength(10)]
        public string PolicyExpiryDate { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the status of the policy (e.g., Active, Cancelled).
        /// </summary>
        [StringLength(1)]
        public string PolicyStatus { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the agent code associated with the policy.
        /// </summary>
        [StringLength(10)]
        public string PolicyAgentCode { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the notification flag ('Y' or 'N').
        /// </summary>
        [StringLength(1)]
        public string PolicyNotifyFlag { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp when the policy was added (ISO 8601 format recommended).
        /// </summary>
        [StringLength(26)]
        public string PolicyAddTimestamp { get; init; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp when the policy was last updated (ISO 8601 format recommended).
        /// </summary>
        [StringLength(26)]
        public string PolicyUpdateTimestamp { get; init; } = string.Empty;

        /// <summary>
        /// Validates the policy record and throws an exception if invalid.
        /// </summary>
        /// <exception cref="ValidationException">Thrown when validation fails.</exception>
        public void Validate()
        {
            var context = new ValidationContext(this);
            Validator.ValidateObject(this, context, validateAllProperties: true);
        }
    }
}

namespace Insurance.PolicyManagement.Repositories
{
    /// <summary>
    /// Defines a contract for policy record repository operations.
    /// </summary>
    public interface IPolicyRecordRepository
    {
        /// <summary>
        /// Asynchronously saves a policy record.
        /// </summary>
        /// <param name="policyRecord">The policy record to save.</param>
        /// <returns>A task representing the asynchronous operation.</returns>
        Task SaveAsync(PolicyRecord policyRecord);
    }
}

namespace Insurance.PolicyManagement.Services
{
    using Insurance.PolicyManagement.Models;
    using Insurance.PolicyManagement.Repositories;
    using Microsoft.Extensions.Logging;

    /// <summary>
    /// Provides business logic for managing policy records.
    /// </summary>
    public class PolicyRecordService
    {
        private readonly IPolicyRecordRepository _repository;
        private readonly ILogger<PolicyRecordService> _logger;

        /// <summary>
        /// Initializes a new instance of the <see cref="PolicyRecordService"/> class.
        /// </summary>
        /// <param name="repository">The policy record repository.</param>
        /// <param name="logger">The logger instance.</param>
        public PolicyRecordService(IPolicyRecordRepository repository, ILogger<PolicyRecordService> logger)
        {
            _repository = repository ?? throw new ArgumentNullException(nameof(repository));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Validates and saves a policy record asynchronously.
        /// </summary>
        /// <param name="policyRecord">The policy record to save.</param>
        /// <returns>A task representing the asynchronous operation.</returns>
        /// <exception cref="ValidationException">Thrown when the policy record is invalid.</exception>
        public async Task SavePolicyRecordAsync(PolicyRecord policyRecord)
        {
            try
            {
                policyRecord.Validate();
                await _repository.SaveAsync(policyRecord);
                _logger.LogInformation("Policy record {PolicyNumber} saved successfully.", policyRecord.PolicyNumber);
            }
            catch (ValidationException ex)
            {
                _logger.LogError(ex, "Validation failed for policy record {PolicyNumber}.", policyRecord.PolicyNumber);
                throw;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "An error occurred while saving policy record {PolicyNumber}.", policyRecord.PolicyNumber);
                throw new ApplicationException("Failed to save policy record.", ex);
            }
        }
    }
}