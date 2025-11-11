using System;
using FluentAssertions;
using Xunit;
using Insurance.Domain.Models;

namespace Insurance.Domain.Models.Tests
{
    public class containingTests : IDisposable
    {
        // Setup resources if needed
        public containingTests()
        {
            // No setup required for immutable record, but placeholder for future expansion
        }

        // Teardown resources if needed
        public void Dispose()
        {
            // No teardown required for immutable record, but placeholder for future expansion
        }

        [Fact]
        public void PolicyRecord_DefaultConstructor_ShouldInitializeAllPropertiesToDefaults()
        {
            // Arrange & Act
            var record = new PolicyRecord();

            // Assert
            record.PolicyNumber.Should().BeEmpty();
            record.PolicyHolderFirstName.Should().BeEmpty();
            record.PolicyHolderMiddleInitial.Should().BeEmpty();
            record.PolicyHolderLastName.Should().BeEmpty();
            record.PolicyBeneficiaryName.Should().BeEmpty();
            record.PolicyBeneficiaryRelation.Should().BeEmpty();
            record.PolicyHolderAddress1.Should().BeEmpty();
            record.PolicyHolderAddress2.Should().BeEmpty();
            record.PolicyHolderCity.Should().BeEmpty();
            record.PolicyHolderState.Should().BeEmpty();
            record.PolicyHolderZipCode.Should().BeEmpty();
            record.PolicyHolderDateOfBirth.Should().BeEmpty();
            record.PolicyHolderGender.Should().BeEmpty();
            record.PolicyHolderPhone.Should().BeEmpty();
            record.PolicyHolderEmail.Should().BeEmpty();
            record.PolicyPaymentFrequency.Should().BeEmpty();
            record.PolicyPaymentMethod.Should().BeEmpty();
            record.PolicyUnderwriter.Should().BeEmpty();
            record.PolicyTermsAndConditions.Should().BeEmpty();
            record.PolicyClaimed.Should().BeEmpty();
            record.PolicyDiscountCode.Should().BeEmpty();
            record.PolicyPremiumAmount.Should().Be(0m);
            record.PolicyType.Should().BeEmpty();
            record.PolicyStartDate.Should().BeEmpty();
            record.PolicyExpiryDate.Should().BeEmpty();
            record.PolicyStatus.Should().BeEmpty();
            record.PolicyAgentCode.Should().BeEmpty();
            record.PolicyNotifyFlag.Should().BeEmpty();
            record.PolicyAddTimestamp.Should().BeEmpty();
            record.PolicyUpdateTimestamp.Should().BeEmpty();
        }

        [Fact]
        public void PolicyRecord_ShouldSetAllPropertiesCorrectly()
        {
            // Arrange
            var expected = new PolicyRecord
            {
                PolicyNumber = "POL123456",
                PolicyHolderFirstName = "John",
                PolicyHolderMiddleInitial = "A",
                PolicyHolderLastName = "Doe",
                PolicyBeneficiaryName = "Jane Doe",
                PolicyBeneficiaryRelation = "Spouse",
                PolicyHolderAddress1 = "123 Main St",
                PolicyHolderAddress2 = "Apt 4B",
                PolicyHolderCity = "Metropolis",
                PolicyHolderState = "NY",
                PolicyHolderZipCode = "12345",
                PolicyHolderDateOfBirth = "1980-01-01",
                PolicyHolderGender = "M",
                PolicyHolderPhone = "555-1234",
                PolicyHolderEmail = "john.doe@example.com",
                PolicyPaymentFrequency = "Monthly",
                PolicyPaymentMethod = "Direct Debit",
                PolicyUnderwriter = "UnderwriterX",
                PolicyTermsAndConditions = "Standard Terms",
                PolicyClaimed = "N",
                PolicyDiscountCode = "DISC10",
                PolicyPremiumAmount = 1234.56m,
                PolicyType = "Life",
                PolicyStartDate = "2023-01-01",
                PolicyExpiryDate = "2024-01-01",
                PolicyStatus = "Active",
                PolicyAgentCode = "AGT001",
                PolicyNotifyFlag = "Y",
                PolicyAddTimestamp = "2023-01-01T12:00:00.000000",
                PolicyUpdateTimestamp = "2023-06-01T12:00:00.000000"
            };

            // Act
            var record = expected;

            // Assert
            record.Should().BeEquivalentTo(expected);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        public void PolicyRecord_ShouldAllowEmptyOrNullStrings_ForAllStringProperties(string testValue)
        {
            // Arrange
            var record = new PolicyRecord
            {
                PolicyNumber = testValue,
                PolicyHolderFirstName = testValue,
                PolicyHolderMiddleInitial = testValue,
                PolicyHolderLastName = testValue,
                PolicyBeneficiaryName = testValue,
                PolicyBeneficiaryRelation = testValue,
                PolicyHolderAddress1 = testValue,
                PolicyHolderAddress2 = testValue,
                PolicyHolderCity = testValue,
                PolicyHolderState = testValue,
                PolicyHolderZipCode = testValue,
                PolicyHolderDateOfBirth = testValue,
                PolicyHolderGender = testValue,
                PolicyHolderPhone = testValue,
                PolicyHolderEmail = testValue,
                PolicyPaymentFrequency = testValue,
                PolicyPaymentMethod = testValue,
                PolicyUnderwriter = testValue,
                PolicyTermsAndConditions = testValue,
                PolicyClaimed = testValue,
                PolicyDiscountCode = testValue,
                PolicyType = testValue,
                PolicyStartDate = testValue,
                PolicyExpiryDate = testValue,
                PolicyStatus = testValue,
                PolicyAgentCode = testValue,
                PolicyNotifyFlag = testValue,
                PolicyAddTimestamp = testValue,
                PolicyUpdateTimestamp = testValue
            };

            // Assert
            record.PolicyNumber.Should().Be(testValue);
            record.PolicyHolderFirstName.Should().Be(testValue);
            record.PolicyHolderMiddleInitial.Should().Be(testValue);
            record.PolicyHolderLastName.Should().Be(testValue);
            record.PolicyBeneficiaryName.Should().Be(testValue);
            record.PolicyBeneficiaryRelation.Should().Be(testValue);
            record.PolicyHolderAddress1.Should().Be(testValue);
            record.PolicyHolderAddress2.Should().Be(testValue);
            record.PolicyHolderCity.Should().Be(testValue);
            record.PolicyHolderState.Should().Be(testValue);
            record.PolicyHolderZipCode.Should().Be(testValue);
            record.PolicyHolderDateOfBirth.Should().Be(testValue);
            record.PolicyHolderGender.Should().Be(testValue);
            record.PolicyHolderPhone.Should().Be(testValue);
            record.PolicyHolderEmail.Should().Be(testValue);
            record.PolicyPaymentFrequency.Should().Be(testValue);
            record.PolicyPaymentMethod.Should().Be(testValue);
            record.PolicyUnderwriter.Should().Be(testValue);
            record.PolicyTermsAndConditions.Should().Be(testValue);
            record.PolicyClaimed.Should().Be(testValue);
            record.PolicyDiscountCode.Should().Be(testValue);
            record.PolicyType.Should().Be(testValue);
            record.PolicyStartDate.Should().Be(testValue);
            record.PolicyExpiryDate.Should().Be(testValue);
            record.PolicyStatus.Should().Be(testValue);
            record.PolicyAgentCode.Should().Be(testValue);
            record.PolicyNotifyFlag.Should().Be(testValue);
            record.PolicyAddTimestamp.Should().Be(testValue);
            record.PolicyUpdateTimestamp.Should().Be(testValue);
        }

        [Theory]
        [InlineData(-1)]
        [InlineData(0)]
        [InlineData(0.01)]
        [InlineData(9999999999.99)]
        public void PolicyRecord_ShouldAllowAnyDecimal_ForPolicyPremiumAmount(decimal premium)
        {
            // Arrange
            var record = new PolicyRecord
            {
                PolicyPremiumAmount = premium
            };

            // Assert
            record.PolicyPremiumAmount.Should().Be(premium);
        }

        [Theory]
        [InlineData("Y")]
        [InlineData("N")]
        [InlineData("y")]
        [InlineData("n")]
        [InlineData("")]
        [InlineData(null)]
        public void PolicyRecord_ShouldAllowValidAndInvalidValues_ForPolicyClaimed(string claimed)
        {
            // Arrange
            var record = new PolicyRecord
            {
                PolicyClaimed = claimed
            };

            // Assert
            record.PolicyClaimed.Should().Be(claimed);
        }

        [Theory]
        [InlineData("Y")]
        [InlineData("N")]
        [InlineData("y")]
        [InlineData("n")]
        [InlineData("")]
        [InlineData(null)]
        public void PolicyRecord_ShouldAllowValidAndInvalidValues_ForPolicyNotifyFlag(string notifyFlag)
        {
            // Arrange
            var record = new PolicyRecord
            {
                PolicyNotifyFlag = notifyFlag
            };

            // Assert
            record.PolicyNotifyFlag.Should().Be(notifyFlag);
        }

        [Theory]
        [InlineData("2023-01-01")]
        [InlineData("2023-12-31")]
        [InlineData("")]
        [InlineData(null)]
        [InlineData("not-a-date")]
        public void PolicyRecord_ShouldAllowAnyString_ForDateFields(string dateValue)
        {
            // Arrange
            var record = new PolicyRecord
            {
                PolicyHolderDateOfBirth = dateValue,
                PolicyStartDate = dateValue,
                PolicyExpiryDate = dateValue
            };

            // Assert
            record.PolicyHolderDateOfBirth.Should().Be(dateValue);
            record.PolicyStartDate.Should().Be(dateValue);
            record.PolicyExpiryDate.Should().Be(dateValue);
        }

        [Fact]
        public void PolicyRecord_Equality_ShouldReturnTrue_ForIdenticalRecords()
        {
            // Arrange
            var record1 = new PolicyRecord
            {
                PolicyNumber = "POL100",
                PolicyHolderFirstName = "Alice",
                PolicyPremiumAmount = 500.00m
            };
            var record2 = new PolicyRecord
            {
                PolicyNumber = "POL100",
                PolicyHolderFirstName = "Alice",
                PolicyPremiumAmount = 500.00m
            };

            // Act & Assert
            record1.Should().Be(record2);
            (record1 == record2).Should().BeTrue();
        }

        [Fact]
        public void PolicyRecord_Equality_ShouldReturnFalse_ForDifferentRecords()
        {
            // Arrange
            var record1 = new PolicyRecord
            {
                PolicyNumber = "POL100",
                PolicyHolderFirstName = "Alice",
                PolicyPremiumAmount = 500.00m
            };
            var record2 = new PolicyRecord
            {
                PolicyNumber = "POL101",
                PolicyHolderFirstName = "Bob",
                PolicyPremiumAmount = 600.00m
            };

            // Act & Assert
            record1.Should().NotBe(record2);
            (record1 != record2).Should().BeTrue();
        }

        [Fact]
        public void PolicyRecord_ShouldBeImmutable()
        {
            // Arrange
            var record = new PolicyRecord
            {
                PolicyNumber = "POL999"
            };

            // Act
            var modified = record with { PolicyNumber = "POL888" };

            // Assert
            record.PolicyNumber.Should().Be("POL999");
            modified.PolicyNumber.Should().Be("POL888");
        }

        [Fact]
        public void PolicyRecord_ShouldSupportWithExpression_ForPartialUpdates()
        {
            // Arrange
            var original = new PolicyRecord
            {
                PolicyNumber = "POL200",
                PolicyHolderFirstName = "Charlie",
                PolicyPremiumAmount = 1000.00m
            };

            // Act
            var updated = original with { PolicyPremiumAmount = 2000.00m };

            // Assert
            updated.PolicyNumber.Should().Be("POL200");
            updated.PolicyHolderFirstName.Should().Be("Charlie");
            updated.PolicyPremiumAmount.Should().Be(2000.00m);
            original.PolicyPremiumAmount.Should().Be(1000.00m);
        }

        [Fact]
        public void PolicyRecord_ToString_ShouldReturnNonEmptyString()
        {
            // Arrange
            var record = new PolicyRecord
            {
                PolicyNumber = "POL777",
                PolicyHolderFirstName = "Diana"
            };

            // Act
            var str = record.ToString();

            // Assert
            str.Should().NotBeNullOrWhiteSpace();
            str.Should().Contain("POL777");
            str.Should().Contain("Diana");
        }

        // Edge case: Very long string values
        [Fact]
        public void PolicyRecord_ShouldSupportVeryLongStrings()
        {
            // Arrange
            var longString = new string('A', 10000);
            var record = new PolicyRecord
            {
                PolicyHolderFirstName = longString,
                PolicyHolderLastName = longString,
                PolicyHolderAddress1 = longString
            };

            // Assert
            record.PolicyHolderFirstName.Should().Be(longString);
            record.PolicyHolderLastName.Should().Be(longString);
            record.PolicyHolderAddress1.Should().Be(longString);
        }

        // Edge case: Unicode and special characters
        [Fact]
        public void PolicyRecord_ShouldSupportUnicodeAndSpecialCharacters()
        {
            // Arrange
            var unicodeString = "测试✓🚀";
            var record = new PolicyRecord
            {
                PolicyHolderFirstName = unicodeString,
                PolicyHolderLastName = unicodeString,
                PolicyHolderAddress1 = unicodeString
            };

            // Assert
            record.PolicyHolderFirstName.Should().Be(unicodeString);
            record.PolicyHolderLastName.Should().Be(unicodeString);
            record.PolicyHolderAddress1.Should().Be(unicodeString);
        }

        // Edge case: Negative and large premium values
        [Theory]
        [InlineData(-9999999999.99)]
        [InlineData(0)]
        [InlineData(9999999999.99)]
        public void PolicyRecord_ShouldSupportExtremePremiumAmounts(decimal premium)
        {
            // Arrange
            var record = new PolicyRecord
            {
                PolicyPremiumAmount = premium
            };

            // Assert
            record.PolicyPremiumAmount.Should().Be(premium);
        }

        // Edge case: All properties set to null
        [Fact]
        public void PolicyRecord_ShouldAllowAllStringPropertiesToBeNull()
        {
            // Arrange
            var record = new PolicyRecord
            {
                PolicyNumber = null,
                PolicyHolderFirstName = null,
                PolicyHolderMiddleInitial = null,
                PolicyHolderLastName = null,
                PolicyBeneficiaryName = null,
                PolicyBeneficiaryRelation = null,
                PolicyHolderAddress1 = null,
                PolicyHolderAddress2 = null,
                PolicyHolderCity = null,
                PolicyHolderState = null,
                PolicyHolderZipCode = null,
                PolicyHolderDateOfBirth = null,
                PolicyHolderGender = null,
                PolicyHolderPhone = null,
                PolicyHolderEmail = null,
                PolicyPaymentFrequency = null,
                PolicyPaymentMethod = null,
                PolicyUnderwriter = null,
                PolicyTermsAndConditions = null,
                PolicyClaimed = null,
                PolicyDiscountCode = null,
                PolicyType = null,
                PolicyStartDate = null,
                PolicyExpiryDate = null,
                PolicyStatus = null,
                PolicyAgentCode = null,
                PolicyNotifyFlag = null,
                PolicyAddTimestamp = null,
                PolicyUpdateTimestamp = null
            };

            // Assert
            record.PolicyNumber.Should().BeNull();
            record.PolicyHolderFirstName.Should().BeNull();
            record.PolicyHolderMiddleInitial.Should().BeNull();
            record.PolicyHolderLastName.Should().BeNull();
            record.PolicyBeneficiaryName.Should().BeNull();
            record.PolicyBeneficiaryRelation.Should().BeNull();
            record.PolicyHolderAddress1.Should().BeNull();
            record.PolicyHolderAddress2.Should().BeNull();
            record.PolicyHolderCity.Should().BeNull();
            record.PolicyHolderState.Should().BeNull();
            record.PolicyHolderZipCode.Should().BeNull();
            record.PolicyHolderDateOfBirth.Should().BeNull();
            record.PolicyHolderGender.Should().BeNull();
            record.PolicyHolderPhone.Should().BeNull();
            record.PolicyHolderEmail.Should().BeNull();
            record.PolicyPaymentFrequency.Should().BeNull();
            record.PolicyPaymentMethod.Should().BeNull();
            record.PolicyUnderwriter.Should().BeNull();
            record.PolicyTermsAndConditions.Should().BeNull();
            record.PolicyClaimed.Should().BeNull();
            record.PolicyDiscountCode.Should().BeNull();
            record.PolicyType.Should().BeNull();
            record.PolicyStartDate.Should().BeNull();
            record.PolicyExpiryDate.Should().BeNull();
            record.PolicyStatus.Should().BeNull();
            record.PolicyAgentCode.Should().BeNull();
            record.PolicyNotifyFlag.Should().BeNull();
            record.PolicyAddTimestamp.Should().BeNull();
            record.PolicyUpdateTimestamp.Should().BeNull();
        }
    }
}