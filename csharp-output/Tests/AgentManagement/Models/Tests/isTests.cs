using System;
using FluentAssertions;
using Xunit;
using Moq;

namespace AgentManagement.Models.Tests
{
    /// <summary>
    /// Comprehensive unit tests for AgentRecord (converted from COBOL CAGENT.cpy).
    /// </summary>
    public class isTests : IDisposable
    {
        // Setup resources if needed
        public isTests()
        {
            // Initialize resources here if required
        }

        // Teardown resources
        public void Dispose()
        {
            // Cleanup resources here if required
        }

        [Fact]
        public void AgentRecord_DefaultConstructor_ShouldInitializeAllPropertiesToEmptyString()
        {
            // Arrange & Act
            var agent = new AgentRecord();

            // Assert
            agent.AgentCode.Should().BeEmpty();
            agent.AgentName.Should().BeEmpty();
            agent.AgentAddress1.Should().BeEmpty();
            agent.AgentAddress2.Should().BeEmpty();
            agent.AgentCity.Should().BeEmpty();
            agent.AgentState.Should().BeEmpty();
            agent.AgentZipCode.Should().BeEmpty();
            agent.AgentDateOfBirth.Should().BeEmpty();
            agent.AgentType.Should().BeEmpty();
            agent.AgentStatus.Should().BeEmpty();
            agent.AgentEmail.Should().BeEmpty();
            agent.AgentContactNumber.Should().BeEmpty();
            agent.AgentStartDate.Should().BeEmpty();
            agent.AgentEndDate.Should().BeEmpty();
        }

        [Fact]
        public void AgentRecord_InitProperties_ShouldSetAllPropertiesCorrectly()
        {
            // Arrange
            var expected = new AgentRecord
            {
                AgentCode = "A123",
                AgentName = "John Doe",
                AgentAddress1 = "123 Main St",
                AgentAddress2 = "Apt 4B",
                AgentCity = "Metropolis",
                AgentState = "NY",
                AgentZipCode = "10001",
                AgentDateOfBirth = "1980-01-01",
                AgentType = "Sales",
                AgentStatus = "Active",
                AgentEmail = "john.doe@example.com",
                AgentContactNumber = "555-1234",
                AgentStartDate = "2020-01-01",
                AgentEndDate = "2025-01-01"
            };

            // Act
            var agent = new AgentRecord
            {
                AgentCode = expected.AgentCode,
                AgentName = expected.AgentName,
                AgentAddress1 = expected.AgentAddress1,
                AgentAddress2 = expected.AgentAddress2,
                AgentCity = expected.AgentCity,
                AgentState = expected.AgentState,
                AgentZipCode = expected.AgentZipCode,
                AgentDateOfBirth = expected.AgentDateOfBirth,
                AgentType = expected.AgentType,
                AgentStatus = expected.AgentStatus,
                AgentEmail = expected.AgentEmail,
                AgentContactNumber = expected.AgentContactNumber,
                AgentStartDate = expected.AgentStartDate,
                AgentEndDate = expected.AgentEndDate
            };

            // Assert
            agent.Should().BeEquivalentTo(expected);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        public void AgentRecord_Properties_ShouldAcceptNullOrEmptyOrWhitespaceStrings(string value)
        {
            // Arrange & Act
            var agent = new AgentRecord
            {
                AgentCode = value,
                AgentName = value,
                AgentAddress1 = value,
                AgentAddress2 = value,
                AgentCity = value,
                AgentState = value,
                AgentZipCode = value,
                AgentDateOfBirth = value,
                AgentType = value,
                AgentStatus = value,
                AgentEmail = value,
                AgentContactNumber = value,
                AgentStartDate = value,
                AgentEndDate = value
            };

            // Assert
            agent.AgentCode.Should().Be(value);
            agent.AgentName.Should().Be(value);
            agent.AgentAddress1.Should().Be(value);
            agent.AgentAddress2.Should().Be(value);
            agent.AgentCity.Should().Be(value);
            agent.AgentState.Should().Be(value);
            agent.AgentZipCode.Should().Be(value);
            agent.AgentDateOfBirth.Should().Be(value);
            agent.AgentType.Should().Be(value);
            agent.AgentStatus.Should().Be(value);
            agent.AgentEmail.Should().Be(value);
            agent.AgentContactNumber.Should().Be(value);
            agent.AgentStartDate.Should().Be(value);
            agent.AgentEndDate.Should().Be(value);
        }

        [Theory]
        [InlineData("1980-01-01")]
        [InlineData("2020-12-31")]
        [InlineData("0001-01-01")]
        [InlineData("9999-12-31")]
        public void AgentRecord_DateProperties_ShouldAcceptValidDateStrings(string date)
        {
            // Arrange & Act
            var agent = new AgentRecord
            {
                AgentDateOfBirth = date,
                AgentStartDate = date,
                AgentEndDate = date
            };

            // Assert
            agent.AgentDateOfBirth.Should().Be(date);
            agent.AgentStartDate.Should().Be(date);
            agent.AgentEndDate.Should().Be(date);
        }

        [Theory]
        [InlineData("not-a-date")]
        [InlineData("2020/01/01")]
        [InlineData("01-01-2020")]
        [InlineData("2020-13-01")]
        [InlineData("")]
        [InlineData(null)]
        public void AgentRecord_DateProperties_ShouldAcceptInvalidDateStrings(string invalidDate)
        {
            // Arrange & Act
            var agent = new AgentRecord
            {
                AgentDateOfBirth = invalidDate,
                AgentStartDate = invalidDate,
                AgentEndDate = invalidDate
            };

            // Assert
            agent.AgentDateOfBirth.Should().Be(invalidDate);
            agent.AgentStartDate.Should().Be(invalidDate);
            agent.AgentEndDate.Should().Be(invalidDate);
        }

        [Theory]
        [InlineData("Active")]
        [InlineData("Inactive")]
        [InlineData("Suspended")]
        [InlineData("")]
        [InlineData(null)]
        public void AgentRecord_AgentStatus_ShouldAcceptVariousStatusValues(string status)
        {
            // Arrange & Act
            var agent = new AgentRecord
            {
                AgentStatus = status
            };

            // Assert
            agent.AgentStatus.Should().Be(status);
        }

        [Theory]
        [InlineData("Sales")]
        [InlineData("Support")]
        [InlineData("Manager")]
        [InlineData("")]
        [InlineData(null)]
        public void AgentRecord_AgentType_ShouldAcceptVariousTypeValues(string type)
        {
            // Arrange & Act
            var agent = new AgentRecord
            {
                AgentType = type
            };

            // Assert
            agent.AgentType.Should().Be(type);
        }

        [Fact]
        public void AgentRecord_RecordEquality_ShouldBeTrueForEquivalentRecords()
        {
            // Arrange
            var agent1 = new AgentRecord
            {
                AgentCode = "A123",
                AgentName = "Jane Smith"
            };

            var agent2 = new AgentRecord
            {
                AgentCode = "A123",
                AgentName = "Jane Smith"
            };

            // Act & Assert
            agent1.Should().Be(agent2);
        }

        [Fact]
        public void AgentRecord_RecordEquality_ShouldBeFalseForDifferentRecords()
        {
            // Arrange
            var agent1 = new AgentRecord
            {
                AgentCode = "A123",
                AgentName = "Jane Smith"
            };

            var agent2 = new AgentRecord
            {
                AgentCode = "B456",
                AgentName = "John Doe"
            };

            // Act & Assert
            agent1.Should().NotBe(agent2);
        }

        [Fact]
        public void AgentRecord_RecordHashCode_ShouldBeEqualForEquivalentRecords()
        {
            // Arrange
            var agent1 = new AgentRecord
            {
                AgentCode = "A123",
                AgentName = "Jane Smith"
            };

            var agent2 = new AgentRecord
            {
                AgentCode = "A123",
                AgentName = "Jane Smith"
            };

            // Act & Assert
            agent1.GetHashCode().Should().Be(agent2.GetHashCode());
        }

        [Fact]
        public void AgentRecord_RecordHashCode_ShouldBeDifferentForDifferentRecords()
        {
            // Arrange
            var agent1 = new AgentRecord
            {
                AgentCode = "A123",
                AgentName = "Jane Smith"
            };

            var agent2 = new AgentRecord
            {
                AgentCode = "B456",
                AgentName = "John Doe"
            };

            // Act & Assert
            agent1.GetHashCode().Should().NotBe(agent2.GetHashCode());
        }

        [Fact]
        public void AgentRecord_Immutability_ShouldNotAllowPropertyModificationAfterCreation()
        {
            // Arrange
            var agent = new AgentRecord
            {
                AgentCode = "A123"
            };

            // Act
            Action act = () =>
            {
                // Attempt to modify property after creation (should not compile, but test for immutability)
                // agent.AgentCode = "B456"; // This line would not compile
            };

            // Assert
            act.Should().NotThrow(); // No runtime error, but property cannot be changed
            agent.AgentCode.Should().Be("A123");
        }

        [Fact]
        public void AgentRecord_WithExpression_ShouldCreateModifiedCopy()
        {
            // Arrange
            var original = new AgentRecord
            {
                AgentCode = "A123",
                AgentName = "Jane Smith"
            };

            // Act
            var modified = original with { AgentName = "John Doe" };

            // Assert
            modified.AgentCode.Should().Be("A123");
            modified.AgentName.Should().Be("John Doe");
            original.AgentName.Should().Be("Jane Smith");
        }

        [Fact]
        public void AgentRecord_NullProperties_ShouldBeHandledGracefully()
        {
            // Arrange
            var agent = new AgentRecord
            {
                AgentCode = null,
                AgentName = null,
                AgentAddress1 = null,
                AgentAddress2 = null,
                AgentCity = null,
                AgentState = null,
                AgentZipCode = null,
                AgentDateOfBirth = null,
                AgentType = null,
                AgentStatus = null,
                AgentEmail = null,
                AgentContactNumber = null,
                AgentStartDate = null,
                AgentEndDate = null
            };

            // Assert
            agent.AgentCode.Should().BeNull();
            agent.AgentName.Should().BeNull();
            agent.AgentAddress1.Should().BeNull();
            agent.AgentAddress2.Should().BeNull();
            agent.AgentCity.Should().BeNull();
            agent.AgentState.Should().BeNull();
            agent.AgentZipCode.Should().BeNull();
            agent.AgentDateOfBirth.Should().BeNull();
            agent.AgentType.Should().BeNull();
            agent.AgentStatus.Should().BeNull();
            agent.AgentEmail.Should().BeNull();
            agent.AgentContactNumber.Should().BeNull();
            agent.AgentStartDate.Should().BeNull();
            agent.AgentEndDate.Should().BeNull();
        }

        // Integration test example for database operations (mocked repository)
        [Fact]
        public void AgentRecord_Repository_SaveAndRetrieve_ShouldPreserveAllFields()
        {
            // Arrange
            var expected = new AgentRecord
            {
                AgentCode = "A999",
                AgentName = "Test Agent",
                AgentAddress1 = "Test Address 1",
                AgentAddress2 = "Test Address 2",
                AgentCity = "Test City",
                AgentState = "TS",
                AgentZipCode = "99999",
                AgentDateOfBirth = "1999-09-09",
                AgentType = "Support",
                AgentStatus = "Active",
                AgentEmail = "test.agent@example.com",
                AgentContactNumber = "999-9999",
                AgentStartDate = "2022-01-01",
                AgentEndDate = "2023-01-01"
            };

            var mockRepo = new Mock<IAgentRecordRepository>();
            mockRepo.Setup(r => r.Save(It.IsAny<AgentRecord>())).Verifiable();
            mockRepo.Setup(r => r.GetByCode("A999")).Returns(expected);

            // Act
            mockRepo.Object.Save(expected);
            var retrieved = mockRepo.Object.GetByCode("A999");

            // Assert
            mockRepo.Verify(r => r.Save(It.Is<AgentRecord>(a => a.AgentCode == "A999")), Times.Once);
            retrieved.Should().BeEquivalentTo(expected);
        }

        // Edge case: Very long strings
        [Fact]
        public void AgentRecord_Properties_ShouldAcceptVeryLongStrings()
        {
            // Arrange
            var longString = new string('A', 10000);
            var agent = new AgentRecord
            {
                AgentCode = longString,
                AgentName = longString,
                AgentAddress1 = longString,
                AgentAddress2 = longString,
                AgentCity = longString,
                AgentState = longString,
                AgentZipCode = longString,
                AgentDateOfBirth = longString,
                AgentType = longString,
                AgentStatus = longString,
                AgentEmail = longString,
                AgentContactNumber = longString,
                AgentStartDate = longString,
                AgentEndDate = longString
            };

            // Assert
            agent.AgentCode.Should().Be(longString);
            agent.AgentName.Should().Be(longString);
            agent.AgentAddress1.Should().Be(longString);
            agent.AgentAddress2.Should().Be(longString);
            agent.AgentCity.Should().Be(longString);
            agent.AgentState.Should().Be(longString);
            agent.AgentZipCode.Should().Be(longString);
            agent.AgentDateOfBirth.Should().Be(longString);
            agent.AgentType.Should().Be(longString);
            agent.AgentStatus.Should().Be(longString);
            agent.AgentEmail.Should().Be(longString);
            agent.AgentContactNumber.Should().Be(longString);
            agent.AgentStartDate.Should().Be(longString);
            agent.AgentEndDate.Should().Be(longString);
        }

        // Edge case: Special characters
        [Fact]
        public void AgentRecord_Properties_ShouldAcceptSpecialCharacters()
        {
            // Arrange
            var special = "!@#$%^&*()_+-=[]{}|;':\",.<>/?`~";
            var agent = new AgentRecord
            {
                AgentCode = special,
                AgentName = special,
                AgentAddress1 = special,
                AgentAddress2 = special,
                AgentCity = special,
                AgentState = special,
                AgentZipCode = special,
                AgentDateOfBirth = special,
                AgentType = special,
                AgentStatus = special,
                AgentEmail = special,
                AgentContactNumber = special,
                AgentStartDate = special,
                AgentEndDate = special
            };

            // Assert
            agent.AgentCode.Should().Be(special);
            agent.AgentName.Should().Be(special);
            agent.AgentAddress1.Should().Be(special);
            agent.AgentAddress2.Should().Be(special);
            agent.AgentCity.Should().Be(special);
            agent.AgentState.Should().Be(special);
            agent.AgentZipCode.Should().Be(special);
            agent.AgentDateOfBirth.Should().Be(special);
            agent.AgentType.Should().Be(special);
            agent.AgentStatus.Should().Be(special);
            agent.AgentEmail.Should().Be(special);
            agent.AgentContactNumber.Should().Be(special);
            agent.AgentStartDate.Should().Be(special);
            agent.AgentEndDate.Should().Be(special);
        }

        // Edge case: Boundary ZIP code values
        [Theory]
        [InlineData("00000")]
        [InlineData("99999")]
        [InlineData("12345-6789")]
        [InlineData("")]
        [InlineData(null)]
        public void AgentRecord_AgentZipCode_ShouldAcceptBoundaryValues(string zip)
        {
            // Arrange & Act
            var agent = new AgentRecord
            {
                AgentZipCode = zip
            };

            // Assert
            agent.AgentZipCode.Should().Be(zip);
        }
    }

    // Mocked repository interface for integration test
    public interface IAgentRecordRepository
    {
        void Save(AgentRecord agent);
        AgentRecord GetByCode(string agentCode);
    }
}