using System;
using Xunit;
using FluentAssertions;
using Moq;

namespace CustomerNotification.Models.Tests
{
    /// <summary>
    /// Comprehensive unit tests for CustomerNotifyRecord.
    /// </summary>
    public class isTests : IDisposable
    {
        // Setup resources if needed
        public isTests()
        {
            // No dependencies to mock for a record type, but setup can be added here if needed in future
        }

        // Teardown resources
        public void Dispose()
        {
            // Cleanup resources if any
        }

        [Fact]
        public void Constructor_Should_Initialize_All_Properties_To_Null_By_Default()
        {
            // Arrange & Act
            var record = new CustomerNotifyRecord();

            // Assert
            record.PolicyNumber.Should().BeNull();
            record.FirstName.Should().BeNull();
            record.MiddleName.Should().BeNull();
            record.LastName.Should().BeNull();
            record.StartDate.Should().BeNull();
            record.ExpiryDate.Should().BeNull();
            record.NotifyDate.Should().BeNull();
            record.NotifyMessages.Should().BeNull();
            record.AgentCode.Should().BeNull();
            record.AgentName.Should().BeNull();
            record.StatutoryMessage.Should().BeNull();
        }

        [Fact]
        public void Record_Should_Allow_Setting_All_Properties()
        {
            // Arrange
            var expected = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123456",
                FirstName = "John",
                MiddleName = "A",
                LastName = "Doe",
                StartDate = "2024-01-01",
                ExpiryDate = "2025-01-01",
                NotifyDate = "2024-06-01",
                NotifyMessages = "Renewal notice",
                AgentCode = "AGT001",
                AgentName = "Jane Agent",
                StatutoryMessage = "Legal info"
            };

            // Act
            var record = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123456",
                FirstName = "John",
                MiddleName = "A",
                LastName = "Doe",
                StartDate = "2024-01-01",
                ExpiryDate = "2025-01-01",
                NotifyDate = "2024-06-01",
                NotifyMessages = "Renewal notice",
                AgentCode = "AGT001",
                AgentName = "Jane Agent",
                StatutoryMessage = "Legal info"
            };

            // Assert
            record.Should().BeEquivalentTo(expected);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        [InlineData("PN999999")]
        public void PolicyNumber_Should_Accept_Null_Empty_Whitespace_And_Valid_Values(string value)
        {
            // Arrange & Act
            var record = new CustomerNotifyRecord { PolicyNumber = value };

            // Assert
            record.PolicyNumber.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("John")]
        [InlineData("J")]
        public void FirstName_Should_Accept_Null_Empty_And_Valid_Values(string value)
        {
            var record = new CustomerNotifyRecord { FirstName = value };
            record.FirstName.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("A")]
        [InlineData("AB")]
        public void MiddleName_Should_Accept_Null_Empty_And_Valid_Values(string value)
        {
            var record = new CustomerNotifyRecord { MiddleName = value };
            record.MiddleName.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("Doe")]
        public void LastName_Should_Accept_Null_Empty_And_Valid_Values(string value)
        {
            var record = new CustomerNotifyRecord { LastName = value };
            record.LastName.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("2024-01-01")]
        [InlineData("01/01/2024")]
        [InlineData("20240101")]
        public void StartDate_Should_Accept_Null_Empty_And_Various_Date_Formats(string value)
        {
            var record = new CustomerNotifyRecord { StartDate = value };
            record.StartDate.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("2025-01-01")]
        [InlineData("01/01/2025")]
        [InlineData("20250101")]
        public void ExpiryDate_Should_Accept_Null_Empty_And_Various_Date_Formats(string value)
        {
            var record = new CustomerNotifyRecord { ExpiryDate = value };
            record.ExpiryDate.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("2024-06-01")]
        [InlineData("06/01/2024")]
        [InlineData("20240601")]
        public void NotifyDate_Should_Accept_Null_Empty_And_Various_Date_Formats(string value)
        {
            var record = new CustomerNotifyRecord { NotifyDate = value };
            record.NotifyDate.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("Renewal notice")]
        [InlineData("Urgent: Contact agent")]
        public void NotifyMessages_Should_Accept_Null_Empty_And_Valid_Values(string value)
        {
            var record = new CustomerNotifyRecord { NotifyMessages = value };
            record.NotifyMessages.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("AGT001")]
        [InlineData("AGT999")]
        public void AgentCode_Should_Accept_Null_Empty_And_Valid_Values(string value)
        {
            var record = new CustomerNotifyRecord { AgentCode = value };
            record.AgentCode.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("Jane Agent")]
        [InlineData("Agent Smith")]
        public void AgentName_Should_Accept_Null_Empty_And_Valid_Values(string value)
        {
            var record = new CustomerNotifyRecord { AgentName = value };
            record.AgentName.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("Legal info")]
        [InlineData("Statutory notice")]
        public void StatutoryMessage_Should_Accept_Null_Empty_And_Valid_Values(string value)
        {
            var record = new CustomerNotifyRecord { StatutoryMessage = value };
            record.StatutoryMessage.Should().Be(value);
        }

        [Fact]
        public void Records_With_Same_Values_Should_Be_Equal()
        {
            // Arrange
            var record1 = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123",
                FirstName = "John",
                MiddleName = "A",
                LastName = "Doe",
                StartDate = "2024-01-01",
                ExpiryDate = "2025-01-01",
                NotifyDate = "2024-06-01",
                NotifyMessages = "Renewal",
                AgentCode = "AGT001",
                AgentName = "Jane Agent",
                StatutoryMessage = "Legal info"
            };

            var record2 = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123",
                FirstName = "John",
                MiddleName = "A",
                LastName = "Doe",
                StartDate = "2024-01-01",
                ExpiryDate = "2025-01-01",
                NotifyDate = "2024-06-01",
                NotifyMessages = "Renewal",
                AgentCode = "AGT001",
                AgentName = "Jane Agent",
                StatutoryMessage = "Legal info"
            };

            // Assert
            record1.Should().Be(record2);
            record1.GetHashCode().Should().Be(record2.GetHashCode());
        }

        [Fact]
        public void Records_With_Different_Values_Should_Not_Be_Equal()
        {
            // Arrange
            var record1 = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123"
            };

            var record2 = new CustomerNotifyRecord
            {
                PolicyNumber = "PN999"
            };

            // Assert
            record1.Should().NotBe(record2);
            record1.GetHashCode().Should().NotBe(record2.GetHashCode());
        }

        [Fact]
        public void Record_Should_Be_Immutable_After_Creation()
        {
            // Arrange
            var record = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123",
                FirstName = "John"
            };

            // Act
            var modified = record with { PolicyNumber = "PN999" };

            // Assert
            record.PolicyNumber.Should().Be("PN123");
            modified.PolicyNumber.Should().Be("PN999");
            modified.FirstName.Should().Be("John");
        }

        [Fact]
        public void Record_Should_Support_Deconstruction()
        {
            // Arrange
            var record = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123",
                FirstName = "John",
                MiddleName = "A",
                LastName = "Doe",
                StartDate = "2024-01-01",
                ExpiryDate = "2025-01-01",
                NotifyDate = "2024-06-01",
                NotifyMessages = "Renewal",
                AgentCode = "AGT001",
                AgentName = "Jane Agent",
                StatutoryMessage = "Legal info"
            };

            // Act
            var (policyNumber, firstName, middleName, lastName, startDate, expiryDate, notifyDate, notifyMessages, agentCode, agentName, statutoryMessage) = record;

            // Assert
            policyNumber.Should().Be("PN123");
            firstName.Should().Be("John");
            middleName.Should().Be("A");
            lastName.Should().Be("Doe");
            startDate.Should().Be("2024-01-01");
            expiryDate.Should().Be("2025-01-01");
            notifyDate.Should().Be("2024-06-01");
            notifyMessages.Should().Be("Renewal");
            agentCode.Should().Be("AGT001");
            agentName.Should().Be("Jane Agent");
            statutoryMessage.Should().Be("Legal info");
        }

        [Fact]
        public void Record_Should_Handle_All_Fields_Null()
        {
            // Arrange
            var record = new CustomerNotifyRecord
            {
                PolicyNumber = null,
                FirstName = null,
                MiddleName = null,
                LastName = null,
                StartDate = null,
                ExpiryDate = null,
                NotifyDate = null,
                NotifyMessages = null,
                AgentCode = null,
                AgentName = null,
                StatutoryMessage = null
            };

            // Assert
            record.PolicyNumber.Should().BeNull();
            record.FirstName.Should().BeNull();
            record.MiddleName.Should().BeNull();
            record.LastName.Should().BeNull();
            record.StartDate.Should().BeNull();
            record.ExpiryDate.Should().BeNull();
            record.NotifyDate.Should().BeNull();
            record.NotifyMessages.Should().BeNull();
            record.AgentCode.Should().BeNull();
            record.AgentName.Should().BeNull();
            record.StatutoryMessage.Should().BeNull();
        }

        [Fact]
        public void Record_Should_Support_With_Expression_For_Updating_Fields()
        {
            // Arrange
            var original = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123",
                FirstName = "John"
            };

            // Act
            var updated = original with { FirstName = "Jane" };

            // Assert
            updated.PolicyNumber.Should().Be("PN123");
            updated.FirstName.Should().Be("Jane");
            original.FirstName.Should().Be("John");
        }

        // Integration test placeholder: simulate database serialization/deserialization
        // In real scenarios, replace with actual DB context and repository
        [Fact]
        public void Record_Should_Serialize_And_Deserialize_Correctly()
        {
            // Arrange
            var record = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123",
                FirstName = "John",
                MiddleName = "A",
                LastName = "Doe",
                StartDate = "2024-01-01",
                ExpiryDate = "2025-01-01",
                NotifyDate = "2024-06-01",
                NotifyMessages = "Renewal",
                AgentCode = "AGT001",
                AgentName = "Jane Agent",
                StatutoryMessage = "Legal info"
            };

            // Act
            var json = System.Text.Json.JsonSerializer.Serialize(record);
            var deserialized = System.Text.Json.JsonSerializer.Deserialize<CustomerNotifyRecord>(json);

            // Assert
            deserialized.Should().BeEquivalentTo(record);
        }

        // Edge case: very long string values
        [Fact]
        public void Record_Should_Accept_Very_Long_String_Values()
        {
            // Arrange
            var longString = new string('X', 1000);

            var record = new CustomerNotifyRecord
            {
                PolicyNumber = longString,
                FirstName = longString,
                MiddleName = longString,
                LastName = longString,
                StartDate = longString,
                ExpiryDate = longString,
                NotifyDate = longString,
                NotifyMessages = longString,
                AgentCode = longString,
                AgentName = longString,
                StatutoryMessage = longString
            };

            // Assert
            record.PolicyNumber.Should().Be(longString);
            record.FirstName.Should().Be(longString);
            record.MiddleName.Should().Be(longString);
            record.LastName.Should().Be(longString);
            record.StartDate.Should().Be(longString);
            record.ExpiryDate.Should().Be(longString);
            record.NotifyDate.Should().Be(longString);
            record.NotifyMessages.Should().Be(longString);
            record.AgentCode.Should().Be(longString);
            record.AgentName.Should().Be(longString);
            record.StatutoryMessage.Should().Be(longString);
        }

        // Edge case: special characters in fields
        [Fact]
        public void Record_Should_Accept_Special_Characters_In_Fields()
        {
            // Arrange
            var specialString = "!@#$%^&*()_+-=[]{}|;':\",.<>/?`~";

            var record = new CustomerNotifyRecord
            {
                PolicyNumber = specialString,
                FirstName = specialString,
                MiddleName = specialString,
                LastName = specialString,
                StartDate = specialString,
                ExpiryDate = specialString,
                NotifyDate = specialString,
                NotifyMessages = specialString,
                AgentCode = specialString,
                AgentName = specialString,
                StatutoryMessage = specialString
            };

            // Assert
            record.PolicyNumber.Should().Be(specialString);
            record.FirstName.Should().Be(specialString);
            record.MiddleName.Should().Be(specialString);
            record.LastName.Should().Be(specialString);
            record.StartDate.Should().Be(specialString);
            record.ExpiryDate.Should().Be(specialString);
            record.NotifyDate.Should().Be(specialString);
            record.NotifyMessages.Should().Be(specialString);
            record.AgentCode.Should().Be(specialString);
            record.AgentName.Should().Be(specialString);
            record.StatutoryMessage.Should().Be(specialString);
        }

        // Edge case: Unicode characters
        [Fact]
        public void Record_Should_Accept_Unicode_Characters_In_Fields()
        {
            // Arrange
            var unicodeString = "客户通知😊漢字";

            var record = new CustomerNotifyRecord
            {
                PolicyNumber = unicodeString,
                FirstName = unicodeString,
                MiddleName = unicodeString,
                LastName = unicodeString,
                StartDate = unicodeString,
                ExpiryDate = unicodeString,
                NotifyDate = unicodeString,
                NotifyMessages = unicodeString,
                AgentCode = unicodeString,
                AgentName = unicodeString,
                StatutoryMessage = unicodeString
            };

            // Assert
            record.PolicyNumber.Should().Be(unicodeString);
            record.FirstName.Should().Be(unicodeString);
            record.MiddleName.Should().Be(unicodeString);
            record.LastName.Should().Be(unicodeString);
            record.StartDate.Should().Be(unicodeString);
            record.ExpiryDate.Should().Be(unicodeString);
            record.NotifyDate.Should().Be(unicodeString);
            record.NotifyMessages.Should().Be(unicodeString);
            record.AgentCode.Should().Be(unicodeString);
            record.AgentName.Should().Be(unicodeString);
            record.StatutoryMessage.Should().Be(unicodeString);
        }

        // Edge case: All fields empty string
        [Fact]
        public void Record_Should_Handle_All_Fields_Empty_String()
        {
            // Arrange
            var record = new CustomerNotifyRecord
            {
                PolicyNumber = "",
                FirstName = "",
                MiddleName = "",
                LastName = "",
                StartDate = "",
                ExpiryDate = "",
                NotifyDate = "",
                NotifyMessages = "",
                AgentCode = "",
                AgentName = "",
                StatutoryMessage = ""
            };

            // Assert
            record.PolicyNumber.Should().BeEmpty();
            record.FirstName.Should().BeEmpty();
            record.MiddleName.Should().BeEmpty();
            record.LastName.Should().BeEmpty();
            record.StartDate.Should().BeEmpty();
            record.ExpiryDate.Should().BeEmpty();
            record.NotifyDate.Should().BeEmpty();
            record.NotifyMessages.Should().BeEmpty();
            record.AgentCode.Should().BeEmpty();
            record.AgentName.Should().BeEmpty();
            record.StatutoryMessage.Should().BeEmpty();
        }

        // Complex scenario: Simulate usage in a notification service with Moq
        [Fact]
        public void Record_Should_Be_Usable_As_Parameter_In_Mocked_Service()
        {
            // Arrange
            var record = new CustomerNotifyRecord
            {
                PolicyNumber = "PN123",
                FirstName = "John",
                LastName = "Doe"
            };

            var mockService = new Mock<ICustomerNotificationService>();
            mockService.Setup(s => s.NotifyCustomer(record)).Returns(true);

            // Act
            var result = mockService.Object.NotifyCustomer(record);

            // Assert
            result.Should().BeTrue();
            mockService.Verify(s => s.NotifyCustomer(It.Is<CustomerNotifyRecord>(r =>
                r.PolicyNumber == "PN123" &&
                r.FirstName == "John" &&
                r.LastName == "Doe"
            )), Times.Once);
        }
    }

    // Mocked service interface for integration test
    public interface ICustomerNotificationService
    {
        bool NotifyCustomer(CustomerNotifyRecord record);
    }
}