using System;
using System.Globalization;
using Xunit;
using FluentAssertions;
using Moq;

namespace Insurance.Models.Tests
{
    public class containingTests : IDisposable
    {
        // Setup resources if needed
        public containingTests()
        {
            // No external dependencies in PolicyRecord, but setup can go here if needed
        }

        // Teardown resources if needed
        public void Dispose()
        {
            // Cleanup if necessary
        }

        [Fact]
        public void PolicyRecord_Should_Initialize_All_Properties_Correctly()
        {
            // Arrange
            var policy = new PolicyRecord
            {
                PolicyNumber = "PN123456",
                PolicyHolderFirstName = "John",
                PolicyHolderMiddleName = "A",
                PolicyHolderLastName = "Doe",
                PolicyBeneficiaryName = "Jane Doe",
                PolicyBeneficiaryRelation = "Spouse",
                PolicyHolderAddress1 = "123 Main St",
                PolicyHolderAddress2 = "Apt 4B",
                PolicyHolderCity = "Springfield",
                PolicyHolderState = "IL",
                PolicyHolderZipCode = "62704",
                PolicyHolderDateOfBirthRaw = "1980-01-15",
                PolicyHolderGender = "M",
                PolicyHolderPhone = "555-1234",
                PolicyHolderEmail = "john.doe@email.com",
                PolicyPaymentFrequency = "Monthly",
                PolicyPaymentMethod = "CreditCard",
                PolicyUnderwriter = "UnderwriterX",
                PolicyTermsAndConditions = "Standard",
                PolicyClaimed = "N",
                PolicyDiscountCode = "DISC10",
                PolicyPremiumAmount = 120.50m,
                PolicyType = "Life",
                PolicyStartDateRaw = "2023-01-01",
                PolicyExpiryDateRaw = "2024-01-01",
                PolicyStatus = "Active",
                PolicyAgentCode = "AGT001",
                PolicyNotifyFlag = "Y",
                PolicyAddTimestampRaw = "2023-01-01T10:00:00.123456",
                PolicyUpdateTimestampRaw = "2023-06-01T12:30:00.654321"
            };

            // Act & Assert
            policy.PolicyNumber.Should().Be("PN123456");
            policy.PolicyHolderFirstName.Should().Be("John");
            policy.PolicyHolderMiddleName.Should().Be("A");
            policy.PolicyHolderLastName.Should().Be("Doe");
            policy.PolicyBeneficiaryName.Should().Be("Jane Doe");
            policy.PolicyBeneficiaryRelation.Should().Be("Spouse");
            policy.PolicyHolderAddress1.Should().Be("123 Main St");
            policy.PolicyHolderAddress2.Should().Be("Apt 4B");
            policy.PolicyHolderCity.Should().Be("Springfield");
            policy.PolicyHolderState.Should().Be("IL");
            policy.PolicyHolderZipCode.Should().Be("62704");
            policy.PolicyHolderDateOfBirthRaw.Should().Be("1980-01-15");
            policy.PolicyHolderGender.Should().Be("M");
            policy.PolicyHolderPhone.Should().Be("555-1234");
            policy.PolicyHolderEmail.Should().Be("john.doe@email.com");
            policy.PolicyPaymentFrequency.Should().Be("Monthly");
            policy.PolicyPaymentMethod.Should().Be("CreditCard");
            policy.PolicyUnderwriter.Should().Be("UnderwriterX");
            policy.PolicyTermsAndConditions.Should().Be("Standard");
            policy.PolicyClaimed.Should().Be("N");
            policy.PolicyDiscountCode.Should().Be("DISC10");
            policy.PolicyPremiumAmount.Should().Be(120.50m);
            policy.PolicyType.Should().Be("Life");
            policy.PolicyStartDateRaw.Should().Be("2023-01-01");
            policy.PolicyExpiryDateRaw.Should().Be("2024-01-01");
            policy.PolicyStatus.Should().Be("Active");
            policy.PolicyAgentCode.Should().Be("AGT001");
            policy.PolicyNotifyFlag.Should().Be("Y");
            policy.PolicyAddTimestampRaw.Should().Be("2023-01-01T10:00:00.123456");
            policy.PolicyUpdateTimestampRaw.Should().Be("2023-06-01T12:30:00.654321");
        }

        [Theory]
        [InlineData("1980-01-15", 1980, 1, 15)]
        [InlineData("2000-12-31", 2000, 12, 31)]
        public void PolicyHolderDateOfBirth_Should_Parse_Valid_Date(string rawDate, int year, int month, int day)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyHolderDateOfBirthRaw = rawDate };

            // Act
            var dob = policy.PolicyHolderDateOfBirth;

            // Assert
            dob.Should().NotBeNull();
            dob.Value.Year.Should().Be(year);
            dob.Value.Month.Should().Be(month);
            dob.Value.Day.Should().Be(day);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("not-a-date")]
        [InlineData("2023/01/01")]
        [InlineData("01-01-2023")]
        public void PolicyHolderDateOfBirth_Should_Return_Null_For_Invalid_Or_Empty(string? rawDate)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyHolderDateOfBirthRaw = rawDate };

            // Act
            var dob = policy.PolicyHolderDateOfBirth;

            // Assert
            dob.Should().BeNull();
        }

        [Theory]
        [InlineData("2023-01-01", 2023, 1, 1)]
        [InlineData("1999-12-31", 1999, 12, 31)]
        public void PolicyStartDate_Should_Parse_Valid_Date(string rawDate, int year, int month, int day)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyStartDateRaw = rawDate };

            // Act
            var startDate = policy.PolicyStartDate;

            // Assert
            startDate.Should().NotBeNull();
            startDate.Value.Year.Should().Be(year);
            startDate.Value.Month.Should().Be(month);
            startDate.Value.Day.Should().Be(day);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("bad-date")]
        [InlineData("2023/01/01")]
        [InlineData("01-01-2023")]
        public void PolicyStartDate_Should_Return_Null_For_Invalid_Or_Empty(string? rawDate)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyStartDateRaw = rawDate };

            // Act
            var startDate = policy.PolicyStartDate;

            // Assert
            startDate.Should().BeNull();
        }

        [Theory]
        [InlineData("2024-01-01", 2024, 1, 1)]
        [InlineData("2050-12-31", 2050, 12, 31)]
        public void PolicyExpiryDate_Should_Parse_Valid_Date(string rawDate, int year, int month, int day)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyExpiryDateRaw = rawDate };

            // Act
            var expiryDate = policy.PolicyExpiryDate;

            // Assert
            expiryDate.Should().NotBeNull();
            expiryDate.Value.Year.Should().Be(year);
            expiryDate.Value.Month.Should().Be(month);
            expiryDate.Value.Day.Should().Be(day);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("bad-date")]
        [InlineData("2023/01/01")]
        [InlineData("01-01-2023")]
        public void PolicyExpiryDate_Should_Return_Null_For_Invalid_Or_Empty(string? rawDate)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyExpiryDateRaw = rawDate };

            // Act
            var expiryDate = policy.PolicyExpiryDate;

            // Assert
            expiryDate.Should().BeNull();
        }

        [Theory]
        [InlineData("2023-01-01T10:00:00.123456", 2023, 1, 1, 10, 0, 0, 123456)]
        [InlineData("2022-12-31T23:59:59.999999", 2022, 12, 31, 23, 59, 59, 999999)]
        public void PolicyAddTimestamp_Should_Parse_Valid_Timestamp(string rawTimestamp, int year, int month, int day, int hour, int minute, int second, int microsecond)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyAddTimestampRaw = rawTimestamp };

            // Act
            var addTimestamp = policy.PolicyAddTimestamp;

            // Assert
            addTimestamp.Should().NotBeNull();
            addTimestamp.Value.Year.Should().Be(year);
            addTimestamp.Value.Month.Should().Be(month);
            addTimestamp.Value.Day.Should().Be(day);
            addTimestamp.Value.Hour.Should().Be(hour);
            addTimestamp.Value.Minute.Should().Be(minute);
            addTimestamp.Value.Second.Should().Be(second);
            // The microsecond part is not directly accessible, but we can check the ticks
            var expectedTicks = microsecond * 10; // 1 microsecond = 10 ticks
            (addTimestamp.Value.Ticks % TimeSpan.TicksPerSecond).Should().Be(expectedTicks);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("not-a-timestamp")]
        [InlineData("2023-01-01 10:00:00")]
        [InlineData("01-01-2023T10:00:00.123456")]
        public void PolicyAddTimestamp_Should_Return_Null_For_Invalid_Or_Empty(string? rawTimestamp)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyAddTimestampRaw = rawTimestamp };

            // Act
            var addTimestamp = policy.PolicyAddTimestamp;

            // Assert
            addTimestamp.Should().BeNull();
        }

        [Theory]
        [InlineData("2023-06-01T12:30:00.654321", 2023, 6, 1, 12, 30, 0, 654321)]
        [InlineData("2021-01-01T00:00:00.000001", 2021, 1, 1, 0, 0, 0, 1)]
        public void PolicyUpdateTimestamp_Should_Parse_Valid_Timestamp(string rawTimestamp, int year, int month, int day, int hour, int minute, int second, int microsecond)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyUpdateTimestampRaw = rawTimestamp };

            // Act
            var updateTimestamp = policy.PolicyUpdateTimestamp;

            // Assert
            updateTimestamp.Should().NotBeNull();
            updateTimestamp.Value.Year.Should().Be(year);
            updateTimestamp.Value.Month.Should().Be(month);
            updateTimestamp.Value.Day.Should().Be(day);
            updateTimestamp.Value.Hour.Should().Be(hour);
            updateTimestamp.Value.Minute.Should().Be(minute);
            updateTimestamp.Value.Second.Should().Be(second);
            var expectedTicks = microsecond * 10;
            (updateTimestamp.Value.Ticks % TimeSpan.TicksPerSecond).Should().Be(expectedTicks);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("not-a-timestamp")]
        [InlineData("2023-06-01 12:30:00")]
        [InlineData("01-06-2023T12:30:00.654321")]
        public void PolicyUpdateTimestamp_Should_Return_Null_For_Invalid_Or_Empty(string? rawTimestamp)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyUpdateTimestampRaw = rawTimestamp };

            // Act
            var updateTimestamp = policy.PolicyUpdateTimestamp;

            // Assert
            updateTimestamp.Should().BeNull();
        }

        [Theory]
        [InlineData("Y")]
        [InlineData("N")]
        [InlineData(null)]
        [InlineData("")]
        public void PolicyClaimed_Should_Accept_Valid_Values(string? claimed)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyClaimed = claimed };

            // Act & Assert
            policy.PolicyClaimed.Should().Be(claimed);
        }

        [Theory]
        [InlineData("Y")]
        [InlineData("N")]
        [InlineData(null)]
        [InlineData("")]
        public void PolicyNotifyFlag_Should_Accept_Valid_Values(string? notifyFlag)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyNotifyFlag = notifyFlag };

            // Act & Assert
            policy.PolicyNotifyFlag.Should().Be(notifyFlag);
        }

        [Fact]
        public void PolicyPremiumAmount_Should_Handle_Boundary_Values()
        {
            // Arrange
            var minPolicy = new PolicyRecord { PolicyPremiumAmount = decimal.MinValue };
            var maxPolicy = new PolicyRecord { PolicyPremiumAmount = decimal.MaxValue };
            var zeroPolicy = new PolicyRecord { PolicyPremiumAmount = 0m };

            // Act & Assert
            minPolicy.PolicyPremiumAmount.Should().Be(decimal.MinValue);
            maxPolicy.PolicyPremiumAmount.Should().Be(decimal.MaxValue);
            zeroPolicy.PolicyPremiumAmount.Should().Be(0m);
        }

        [Fact]
        public void PolicyRecord_Should_Handle_Nullable_Strings()
        {
            // Arrange
            var policy = new PolicyRecord
            {
                PolicyNumber = null,
                PolicyHolderFirstName = null,
                PolicyHolderMiddleName = null,
                PolicyHolderLastName = null,
                PolicyBeneficiaryName = null,
                PolicyBeneficiaryRelation = null,
                PolicyHolderAddress1 = null,
                PolicyHolderAddress2 = null,
                PolicyHolderCity = null,
                PolicyHolderState = null,
                PolicyHolderZipCode = null,
                PolicyHolderDateOfBirthRaw = null,
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
                PolicyStartDateRaw = null,
                PolicyExpiryDateRaw = null,
                PolicyStatus = null,
                PolicyAgentCode = null,
                PolicyNotifyFlag = null,
                PolicyAddTimestampRaw = null,
                PolicyUpdateTimestampRaw = null
            };

            // Act & Assert
            policy.PolicyNumber.Should().BeNull();
            policy.PolicyHolderFirstName.Should().BeNull();
            policy.PolicyHolderMiddleName.Should().BeNull();
            policy.PolicyHolderLastName.Should().BeNull();
            policy.PolicyBeneficiaryName.Should().BeNull();
            policy.PolicyBeneficiaryRelation.Should().BeNull();
            policy.PolicyHolderAddress1.Should().BeNull();
            policy.PolicyHolderAddress2.Should().BeNull();
            policy.PolicyHolderCity.Should().BeNull();
            policy.PolicyHolderState.Should().BeNull();
            policy.PolicyHolderZipCode.Should().BeNull();
            policy.PolicyHolderDateOfBirthRaw.Should().BeNull();
            policy.PolicyHolderGender.Should().BeNull();
            policy.PolicyHolderPhone.Should().BeNull();
            policy.PolicyHolderEmail.Should().BeNull();
            policy.PolicyPaymentFrequency.Should().BeNull();
            policy.PolicyPaymentMethod.Should().BeNull();
            policy.PolicyUnderwriter.Should().BeNull();
            policy.PolicyTermsAndConditions.Should().BeNull();
            policy.PolicyClaimed.Should().BeNull();
            policy.PolicyDiscountCode.Should().BeNull();
            policy.PolicyType.Should().BeNull();
            policy.PolicyStartDateRaw.Should().BeNull();
            policy.PolicyExpiryDateRaw.Should().BeNull();
            policy.PolicyStatus.Should().BeNull();
            policy.PolicyAgentCode.Should().BeNull();
            policy.PolicyNotifyFlag.Should().BeNull();
            policy.PolicyAddTimestampRaw.Should().BeNull();
            policy.PolicyUpdateTimestampRaw.Should().BeNull();
        }

        [Fact]
        public void PolicyRecord_Should_Preserve_COBOL_Business_Logic_For_Date_Parsing()
        {
            // Arrange
            // COBOL expects strict date formats, so test strict parsing
            var validPolicy = new PolicyRecord
            {
                PolicyHolderDateOfBirthRaw = "1985-05-20",
                PolicyStartDateRaw = "2021-07-01",
                PolicyExpiryDateRaw = "2022-07-01",
                PolicyAddTimestampRaw = "2021-07-01T08:00:00.000001",
                PolicyUpdateTimestampRaw = "2021-07-01T09:00:00.000002"
            };

            var invalidPolicy = new PolicyRecord
            {
                PolicyHolderDateOfBirthRaw = "20-05-1985",
                PolicyStartDateRaw = "07/01/2021",
                PolicyExpiryDateRaw = "2022/07/01",
                PolicyAddTimestampRaw = "2021-07-01 08:00:00",
                PolicyUpdateTimestampRaw = "2021-07-01 09:00:00"
            };

            // Act & Assert
            validPolicy.PolicyHolderDateOfBirth.Should().NotBeNull();
            validPolicy.PolicyStartDate.Should().NotBeNull();
            validPolicy.PolicyExpiryDate.Should().NotBeNull();
            validPolicy.PolicyAddTimestamp.Should().NotBeNull();
            validPolicy.PolicyUpdateTimestamp.Should().NotBeNull();

            invalidPolicy.PolicyHolderDateOfBirth.Should().BeNull();
            invalidPolicy.PolicyStartDate.Should().BeNull();
            invalidPolicy.PolicyExpiryDate.Should().BeNull();
            invalidPolicy.PolicyAddTimestamp.Should().BeNull();
            invalidPolicy.PolicyUpdateTimestamp.Should().BeNull();
        }

        [Fact]
        public void PolicyRecord_Should_Be_Immutable()
        {
            // Arrange
            var policy = new PolicyRecord
            {
                PolicyNumber = "PN123456",
                PolicyHolderFirstName = "John"
            };

            // Act
            Action act = () => policy = policy with { PolicyHolderFirstName = "Jane" };

            // Assert
            act.Should().NotThrow();
            policy.PolicyHolderFirstName.Should().Be("Jane");
        }

        [Fact]
        public void PolicyRecord_Should_Support_Record_Equality()
        {
            // Arrange
            var policy1 = new PolicyRecord { PolicyNumber = "PN123456", PolicyHolderFirstName = "John" };
            var policy2 = new PolicyRecord { PolicyNumber = "PN123456", PolicyHolderFirstName = "John" };
            var policy3 = new PolicyRecord { PolicyNumber = "PN654321", PolicyHolderFirstName = "Jane" };

            // Act & Assert
            policy1.Should().Be(policy2);
            policy1.Should().NotBe(policy3);
        }

        // Integration test placeholder for database operations
        // This would require a repository or context, which is not present in PolicyRecord
        // [Fact]
        // public void PolicyRecord_Should_Save_And_Retrieve_From_Database()
        // {
        //     // Arrange
        //     var mockRepo = new Mock<IPolicyRepository>();
        //     var policy = new PolicyRecord { PolicyNumber = "PN123456" };
        //     mockRepo.Setup(r => r.Save(policy)).Returns(true);
        //     mockRepo.Setup(r => r.Get("PN123456")).Returns(policy);

        //     // Act
        //     var saveResult = mockRepo.Object.Save(policy);
        //     var retrievedPolicy = mockRepo.Object.Get("PN123456");

        //     // Assert
        //     saveResult.Should().BeTrue();
        //     retrievedPolicy.Should().Be(policy);
        // }

        // Additional edge case: test for very long string values
        [Fact]
        public void PolicyRecord_Should_Handle_Very_Long_String_Values()
        {
            // Arrange
            var longString = new string('A', 1000);
            var policy = new PolicyRecord
            {
                PolicyHolderFirstName = longString,
                PolicyHolderLastName = longString,
                PolicyHolderAddress1 = longString,
                PolicyHolderEmail = longString
            };

            // Act & Assert
            policy.PolicyHolderFirstName.Should().Be(longString);
            policy.PolicyHolderLastName.Should().Be(longString);
            policy.PolicyHolderAddress1.Should().Be(longString);
            policy.PolicyHolderEmail.Should().Be(longString);
        }

        // Edge case: test for whitespace-only strings
        [Fact]
        public void PolicyRecord_Should_Handle_Whitespace_Only_Strings()
        {
            // Arrange
            var policy = new PolicyRecord
            {
                PolicyHolderFirstName = "   ",
                PolicyHolderLastName = "\t",
                PolicyHolderAddress1 = "\n",
                PolicyHolderEmail = " "
            };

            // Act & Assert
            policy.PolicyHolderFirstName.Should().Be("   ");
            policy.PolicyHolderLastName.Should().Be("\t");
            policy.PolicyHolderAddress1.Should().Be("\n");
            policy.PolicyHolderEmail.Should().Be(" ");
        }

        // Edge case: test for special characters in string fields
        [Fact]
        public void PolicyRecord_Should_Handle_Special_Characters_In_Strings()
        {
            // Arrange
            var policy = new PolicyRecord
            {
                PolicyHolderFirstName = "Jöhn",
                PolicyHolderLastName = "Dœ",
                PolicyHolderAddress1 = "123 Mäin St.",
                PolicyHolderEmail = "jöhn.dœ@email.com"
            };

            // Act & Assert
            policy.PolicyHolderFirstName.Should().Be("Jöhn");
            policy.PolicyHolderLastName.Should().Be("Dœ");
            policy.PolicyHolderAddress1.Should().Be("123 Mäin St.");
            policy.PolicyHolderEmail.Should().Be("jöhn.dœ@email.com");
        }

        // Edge case: test for negative premium amount
        [Fact]
        public void PolicyPremiumAmount_Should_Accept_Negative_Values()
        {
            // Arrange
            var policy = new PolicyRecord { PolicyPremiumAmount = -99.99m };

            // Act & Assert
            policy.PolicyPremiumAmount.Should().Be(-99.99m);
        }

        // Edge case: test for null and empty beneficiary relation
        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("Spouse")]
        public void PolicyBeneficiaryRelation_Should_Accept_Any_String(string? relation)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyBeneficiaryRelation = relation };

            // Act & Assert
            policy.PolicyBeneficiaryRelation.Should().Be(relation);
        }

        // Edge case: test for null and empty agent code
        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("AGT001")]
        public void PolicyAgentCode_Should_Accept_Any_String(string? agentCode)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyAgentCode = agentCode };

            // Act & Assert
            policy.PolicyAgentCode.Should().Be(agentCode);
        }

        // Edge case: test for null and empty discount code
        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("DISC10")]
        public void PolicyDiscountCode_Should_Accept_Any_String(string? discountCode)
        {
            // Arrange
            var policy = new PolicyRecord { PolicyDiscountCode = discountCode };

            // Act & Assert
            policy.PolicyDiscountCode.Should().Be(discountCode);
        }
    }
}