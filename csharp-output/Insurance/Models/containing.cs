using System;
using System.Globalization;

namespace Insurance.Models
{
    /// <summary>
    /// Represents an insurance policy record containing all relevant policy, holder, beneficiary, payment, and audit information.
    /// </summary>
    public record PolicyRecord
    {
        /// <summary>
        /// Gets the unique policy number.
        /// </summary>
        public string? PolicyNumber { get; init; }

        /// <summary>
        /// Gets the first name of the policy holder.
        /// </summary>
        public string? PolicyHolderFirstName { get; init; }

        /// <summary>
        /// Gets the middle initial of the policy holder.
        /// </summary>
        public string? PolicyHolderMiddleName { get; init; }

        /// <summary>
        /// Gets the last name of the policy holder.
        /// </summary>
        public string? PolicyHolderLastName { get; init; }

        /// <summary>
        /// Gets the name of the beneficiary.
        /// </summary>
        public string? PolicyBeneficiaryName { get; init; }

        /// <summary>
        /// Gets the relationship of the beneficiary to the policy holder.
        /// </summary>
        public string? PolicyBeneficiaryRelation { get; init; }

        /// <summary>
        /// Gets the first address line of the policy holder.
        /// </summary>
        public string? PolicyHolderAddress1 { get; init; }

        /// <summary>
        /// Gets the second address line of the policy holder.
        /// </summary>
        public string? PolicyHolderAddress2 { get; init; }

        /// <summary>
        /// Gets the city of the policy holder.
        /// </summary>
        public string? PolicyHolderCity { get; init; }

        /// <summary>
        /// Gets the state code of the policy holder.
        /// </summary>
        public string? PolicyHolderState { get; init; }

        /// <summary>
        /// Gets the ZIP code of the policy holder.
        /// </summary>
        public string? PolicyHolderZipCode { get; init; }

        /// <summary>
        /// Gets the date of birth of the policy holder (as string, format: yyyy-MM-dd).
        /// </summary>
        public string? PolicyHolderDateOfBirthRaw { get; init; }

        /// <summary>
        /// Gets the date of birth of the policy holder as a nullable DateTime.
        /// </summary>
        public DateTime? PolicyHolderDateOfBirth =>
            DateTime.TryParseExact(PolicyHolderDateOfBirthRaw, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None, out var dt)
                ? dt
                : null;

        /// <summary>
        /// Gets the gender of the policy holder.
        /// </summary>
        public string? PolicyHolderGender { get; init; }

        /// <summary>
        /// Gets the phone number of the policy holder.
        /// </summary>
        public string? PolicyHolderPhone { get; init; }

        /// <summary>
        /// Gets the email address of the policy holder.
        /// </summary>
        public string? PolicyHolderEmail { get; init; }

        /// <summary>
        /// Gets the payment frequency for the policy.
        /// </summary>
        public string? PolicyPaymentFrequency { get; init; }

        /// <summary>
        /// Gets the payment method for the policy.
        /// </summary>
        public string? PolicyPaymentMethod { get; init; }

        /// <summary>
        /// Gets the underwriter for the policy.
        /// </summary>
        public string? PolicyUnderwriter { get; init; }

        /// <summary>
        /// Gets the terms and conditions of the policy.
        /// </summary>
        public string? PolicyTermsAndConditions { get; init; }

        /// <summary>
        /// Gets a value indicating whether the policy has been claimed ("Y" or "N").
        /// </summary>
        public string? PolicyClaimed { get; init; }

        /// <summary>
        /// Gets the discount code applied to the policy.
        /// </summary>
        public string? PolicyDiscountCode { get; init; }

        /// <summary>
        /// Gets the premium amount for the policy.
        /// </summary>
        public decimal PolicyPremiumAmount { get; init; }

        /// <summary>
        /// Gets the type of the policy.
        /// </summary>
        public string? PolicyType { get; init; }

        /// <summary>
        /// Gets the raw start date of the policy (format: yyyy-MM-dd).
        /// </summary>
        public string? PolicyStartDateRaw { get; init; }

        /// <summary>
        /// Gets the start date of the policy as a nullable DateTime.
        /// </summary>
        public DateTime? PolicyStartDate =>
            DateTime.TryParseExact(PolicyStartDateRaw, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None, out var dt)
                ? dt
                : null;

        /// <summary>
        /// Gets the raw expiry date of the policy (format: yyyy-MM-dd).
        /// </summary>
        public string? PolicyExpiryDateRaw { get; init; }

        /// <summary>
        /// Gets the expiry date of the policy as a nullable DateTime.
        /// </summary>
        public DateTime? PolicyExpiryDate =>
            DateTime.TryParseExact(PolicyExpiryDateRaw, "yyyy-MM-dd", CultureInfo.InvariantCulture, DateTimeStyles.None, out var dt)
                ? dt
                : null;

        /// <summary>
        /// Gets the status of the policy.
        /// </summary>
        public string? PolicyStatus { get; init; }

        /// <summary>
        /// Gets the agent code associated with the policy.
        /// </summary>
        public string? PolicyAgentCode { get; init; }

        /// <summary>
        /// Gets the notification flag for the policy ("Y" or "N").
        /// </summary>
        public string? PolicyNotifyFlag { get; init; }

        /// <summary>
        /// Gets the raw add timestamp (format: yyyy-MM-ddTHH:mm:ss.ffffff).
        /// </summary>
        public string? PolicyAddTimestampRaw { get; init; }

        /// <summary>
        /// Gets the add timestamp as a nullable DateTime.
        /// </summary>
        public DateTime? PolicyAddTimestamp =>
            DateTime.TryParseExact(PolicyAddTimestampRaw, "yyyy-MM-ddTHH:mm:ss.ffffff", CultureInfo.InvariantCulture, DateTimeStyles.None, out var dt)
                ? dt
                : null;

        /// <summary>
        /// Gets the raw update timestamp (format: yyyy-MM-ddTHH:mm:ss.ffffff).
        /// </summary>
        public string? PolicyUpdateTimestampRaw { get; init; }

        /// <summary>
        /// Gets the update timestamp as a nullable DateTime.
        /// </summary>
        public DateTime? PolicyUpdateTimestamp =>
            DateTime.TryParseExact(PolicyUpdateTimestampRaw, "yyyy-MM-ddTHH:mm:ss.ffffff", CultureInfo.InvariantCulture, DateTimeStyles.None, out var dt)
                ? dt
                : null;
    }
}