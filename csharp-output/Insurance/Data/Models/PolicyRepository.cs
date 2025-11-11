#nullable enable

using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Threading.Tasks;

namespace Insurance.Data.Models
{
    /// <summary>
    /// Represents an insurance policy record as defined in the INSURNCE.TPOLICY DB2 table.
    /// </summary>
    public record Policy
    {
        /// <summary>
        /// Gets the unique policy number.
        /// </summary>
        [Key]
        [Required]
        [StringLength(10)]
        public string PolicyNumber { get; init; }

        /// <summary>
        /// Gets the first name of the policy holder.
        /// </summary>
        [Required]
        [StringLength(35)]
        public string PolicyHolderFirstName { get; init; }

        /// <summary>
        /// Gets the middle initial of the policy holder.
        /// </summary>
        [Required]
        [StringLength(1)]
        public string PolicyHolderMiddleName { get; init; }

        /// <summary>
        /// Gets the last name of the policy holder.
        /// </summary>
        [Required]
        [StringLength(35)]
        public string PolicyHolderLastName { get; init; }

        /// <summary>
        /// Gets the name of the beneficiary.
        /// </summary>
        [Required]
        [StringLength(60)]
        public string PolicyBeneficiaryName { get; init; }

        /// <summary>
        /// Gets the relationship of the beneficiary to the policy holder.
        /// </summary>
        [Required]
        [StringLength(15)]
        public string PolicyBeneficiaryRelation { get; init; }

        /// <summary>
        /// Gets the first line of the policy holder's address.
        /// </summary>
        [Required]
        [StringLength(100)]
        public string PolicyHolderAddress1 { get; init; }

        /// <summary>
        /// Gets the second line of the policy holder's address.
        /// </summary>
        [Required]
        [StringLength(100)]
        public string PolicyHolderAddress2 { get; init; }

        /// <summary>
        /// Gets the city of the policy holder.
        /// </summary>
        [Required]
        [StringLength(30)]
        public string PolicyHolderCity { get; init; }

        /// <summary>
        /// Gets the state of the policy holder.
        /// </summary>
        [Required]
        [StringLength(2)]
        public string PolicyHolderState { get; init; }

        /// <summary>
        /// Gets the ZIP code of the policy holder.
        /// </summary>
        [Required]
        [StringLength(10)]
        public string PolicyHolderZipCode { get; init; }

        /// <summary>
        /// Gets the date of birth of the policy holder (ISO format: yyyy-MM-dd).
        /// </summary>
        [Required]
        [StringLength(10)]
        public string PolicyHolderDateOfBirth { get; init; }

        /// <summary>
        /// Gets the gender of the policy holder.
        /// </summary>
        [Required]
        [StringLength(8)]
        public string PolicyHolderGender { get; init; }

        /// <summary>
        /// Gets the phone number of the policy holder.
        /// </summary>
        [Required]
        [StringLength(10)]
        public string PolicyHolderPhone { get; init; }

        /// <summary>
        /// Gets the email address of the policy holder.
        /// </summary>
        [Required]
        [StringLength(30)]
        public string PolicyHolderEmail { get; init; }

        /// <summary>
        /// Gets the payment frequency for the policy.
        /// </summary>
        [Required]
        [StringLength(10)]
        public string PolicyPaymentFrequency { get; init; }

        /// <summary>
        /// Gets the payment method for the policy.
        /// </summary>
        [Required]
        [StringLength(8)]
        public string PolicyPaymentMethod { get; init; }

        /// <summary>
        /// Gets the underwriter for the policy.
        /// </summary>
        [Required]
        [StringLength(50)]
        public string PolicyUnderwriter { get; init; }

        /// <summary>
        /// Gets the terms and conditions of the policy.
        /// </summary>
        [Required]
        [StringLength(200)]
        public string PolicyTermsAndConditions { get; init; }

        /// <summary>
        /// Gets a value indicating whether the policy has been claimed ('Y' or 'N').
        /// </summary>
        [Required]
        [StringLength(1)]
        public string PolicyClaimed { get; init; }

        /// <summary>
        /// Gets the discount code applied to the policy.
        /// </summary>
        [Required]
        [StringLength(10)]
        public string PolicyDiscountCode { get; init; }

        /// <summary>
        /// Gets the premium amount for the policy.
        /// </summary>
        [Required]
        [Column(TypeName = "decimal(7,2)")]
        public decimal PolicyPremiumAmount { get; init; }

        /// <summary>
        /// Gets the coverage amount for the policy.
        /// </summary>
        [Required]
        [Column(TypeName = "decimal(10,2)")]
        public decimal PolicyCoverageAmount { get; init; }

        /// <summary>
        /// Gets the type of the policy.
        /// </summary>
        [Required]
        [StringLength(50)]
        public string PolicyType { get; init; }

        /// <summary>
        /// Gets the start date of the policy.
        /// </summary>
        [Required]
        public DateTime PolicyStartDate { get; init; }

        /// <summary>
        /// Gets the expiry date of the policy.
        /// </summary>
        [Required]
        public DateTime PolicyExpiryDate { get; init; }

        /// <summary>
        /// Gets the status of the policy ('A' for active, etc.).
        /// </summary>
        [Required]
        [StringLength(1)]
        public string PolicyStatus { get; init; }

        /// <summary>
        /// Gets the agent code associated with the policy.
        /// </summary>
        [Required]
        [StringLength(10)]
        public string PolicyAgentCode { get; init; }

        /// <summary>
        /// Gets the notification flag for the policy ('Y' or 'N').
        /// </summary>
        [Required]
        [StringLength(1)]
        public string PolicyNotifyFlag { get; init; }

        /// <summary>
        /// Gets the timestamp when the policy was added.
        /// </summary>
        [Required]
        public DateTimeOffset PolicyAddTimestamp { get; init; }

        /// <summary>
        /// Gets the timestamp when the policy was last updated.
        /// </summary>
        [Required]
        public DateTimeOffset PolicyUpdateTimestamp { get; init; }
    }
}

namespace Insurance.Data.Repositories
{
    using Insurance.Data.Models;
    using System.Collections.Generic;
    using System.Linq;
    using Microsoft.Extensions.Logging;

    /// <summary>
    /// Repository for accessing insurance policy records.
    /// </summary>
    public interface IPolicyRepository
    {
        /// <summary>
        /// Asynchronously retrieves a policy by its policy number.
        /// </summary>
        /// <param name="policyNumber">The policy number.</param>
        /// <returns>The matching <see cref="Policy"/> or <c>null</c> if not found.</returns>
        Task<Policy?> GetPolicyByNumberAsync(string policyNumber);

        /// <summary>
        /// Asynchronously retrieves all policies.
        /// </summary>
        /// <returns>A list of <see cref="Policy"/> records.</returns>
        Task<IReadOnlyList<Policy>> GetAllPoliciesAsync();

        /// <summary>
        /// Asynchronously adds a new policy.
        /// </summary>
        /// <param name="policy">The policy to add.</param>
        /// <returns>The added <see cref="Policy"/>.</returns>
        Task<Policy> AddPolicyAsync(Policy policy);
    }

    /// <summary>
    /// Implementation of <see cref="IPolicyRepository"/> using dependency injection and async/await.
    /// </summary>
    public class PolicyRepository : IPolicyRepository
    {
        private readonly InsuranceDbContext _dbContext;
        private readonly ILogger<PolicyRepository> _logger;

        /// <summary>
        /// Initializes a new instance of the <see cref="PolicyRepository"/> class.
        /// </summary>
        /// <param name="dbContext">The database context.</param>
        /// <param name="logger">The logger instance.</param>
        public PolicyRepository(InsuranceDbContext dbContext, ILogger<PolicyRepository> logger)
        {
            _dbContext = dbContext ?? throw new ArgumentNullException(nameof(dbContext));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <inheritdoc/>
        public async Task<Policy?> GetPolicyByNumberAsync(string policyNumber)
        {
            try
            {
                return await _dbContext.Policies
                    .Where(p => p.PolicyNumber == policyNumber)
                    .FirstOrDefaultAsync()
                    .ConfigureAwait(false);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error retrieving policy with number {PolicyNumber}", policyNumber);
                throw new RepositoryException($"Failed to retrieve policy {policyNumber}", ex);
            }
        }

        /// <inheritdoc/>
        public async Task<IReadOnlyList<Policy>> GetAllPoliciesAsync()
        {
            try
            {
                return await _dbContext.Policies
                    .OrderBy(p => p.PolicyNumber)
                    .ToListAsync()
                    .ConfigureAwait(false);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error retrieving all policies");
                throw new RepositoryException("Failed to retrieve policies", ex);
            }
        }

        /// <inheritdoc/>
        public async Task<Policy> AddPolicyAsync(Policy policy)
        {
            try
            {
                var entity = await _dbContext.Policies.AddAsync(policy).ConfigureAwait(false);
                await _dbContext.SaveChangesAsync().ConfigureAwait(false);
                return entity.Entity;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error adding policy {PolicyNumber}", policy.PolicyNumber);
                throw new RepositoryException($"Failed to add policy {policy.PolicyNumber}", ex);
            }
        }
    }

    /// <summary>
    /// Custom exception for repository errors.
    /// </summary>
    public class RepositoryException : Exception
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="RepositoryException"/> class.
        /// </summary>
        /// <param name="message">The error message.</param>
        /// <param name="innerException">The inner exception.</param>
        public RepositoryException(string message, Exception innerException)
            : base(message, innerException)
        {
        }
    }
}

namespace Insurance.Data
{
    using Insurance.Data.Models;
    using Microsoft.EntityFrameworkCore;

    /// <summary>
    /// Entity Framework Core database context for insurance policies.
    /// </summary>
    public class InsuranceDbContext : DbContext
    {
        /// <summary>
        /// Gets or sets the insurance policies.
        /// </summary>
        public DbSet<Policy> Policies { get; set; } = null!;

        /// <summary>
        /// Configures the model mappings for the insurance policy table.
        /// </summary>
        /// <param name="modelBuilder">The model builder.</param>
        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.Entity<Policy>(entity =>
            {
                entity.ToTable("TPOLICY", schema: "INSURNCE");
                entity.HasKey(e => e.PolicyNumber);
                entity.Property(e => e.PolicyAddTimestamp)
                    .HasDefaultValueSql("CURRENT_TIMESTAMP");
                entity.Property(e => e.PolicyUpdateTimestamp)
                    .HasDefaultValueSql("CURRENT_TIMESTAMP");
                // Additional mappings as needed
            });
        }
    }
}