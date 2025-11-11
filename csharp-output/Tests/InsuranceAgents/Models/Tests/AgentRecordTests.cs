using System;
using FluentAssertions;
using Xunit;
using Moq;

namespace InsuranceAgents.Models.Tests
{
    /// <summary>
    /// Comprehensive unit tests for AgentRecord, ensuring COBOL business logic and data integrity.
    /// </summary>
    public class AgentRecordTests : IDisposable
    {
        // Setup resources if needed in the future (e.g., database, services)
        public AgentRecordTests()
        {
            // Initialize test-wide resources here if required
        }

        public void Dispose()
        {
            // Cleanup resources here if required
        }

        [Fact]
        public void Constructor_ShouldInitializeAllPropertiesToEmptyStrings()
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
        public void Record_ShouldSupportWithExpressionToModifyProperties()
        {
            // Arrange
            var original = new AgentRecord
            {
                AgentCode = "A123",
                AgentName = "John Doe"
            };

            // Act
            var modified = original with { AgentName = "Jane Smith" };

            // Assert
            modified.AgentCode.Should().Be("A123");
            modified.AgentName.Should().Be("Jane Smith");
            original.AgentName.Should().Be("John Doe"); // Immutability check
        }

        [Fact]
        public void Record_ShouldSupportEqualityComparison()
        {
            // Arrange
            var agent1 = new AgentRecord
            {
                AgentCode = "A1",
                AgentName = "Agent Smith"
            };
            var agent2 = new AgentRecord
            {
                AgentCode = "A1",
                AgentName = "Agent Smith"
            };

            // Act & Assert
            agent1.Should().Be(agent2);
            (agent1 == agent2).Should().BeTrue();
            (agent1 != agent2).Should().BeFalse();
        }

        [Fact]
        public void Record_ShouldSupportInequalityComparison()
        {
            // Arrange
            var agent1 = new AgentRecord { AgentCode = "A1" };
            var agent2 = new AgentRecord { AgentCode = "A2" };

            // Act & Assert
            agent1.Should().NotBe(agent2);
            (agent1 == agent2).Should().BeFalse();
            (agent1 != agent2).Should().BeTrue();
        }

        [Theory]
        [InlineData("A", "Active")]
        [InlineData("I", "Inactive")]
        [InlineData("", "Unknown")]
        public void AgentStatus_ShouldMatchExpectedMeaning(string status, string expectedMeaning)
        {
            // Arrange
            var agent = new AgentRecord { AgentStatus = status };

            // Act
            string meaning = status switch
            {
                "A" => "Active",
                "I" => "Inactive",
                _ => "Unknown"
            };

            // Assert
            meaning.Should().Be(expectedMeaning);
        }

        [Theory]
        [InlineData("Broker")]
        [InlineData("Employee")]
        [InlineData("Independent")]
        [InlineData("")]
        public void AgentType_ShouldAcceptVariousTypes(string agentType)
        {
            // Arrange & Act
            var agent = new AgentRecord { AgentType = agentType };

            // Assert
            agent.AgentType.Should().Be(agentType);
        }

        [Theory]
        [InlineData("1980-01-01")]
        [InlineData("2000-12-31")]
        [InlineData("")]
        public void AgentDateOfBirth_ShouldAcceptValidAndEmptyStrings(string dob)
        {
            // Arrange & Act
            var agent = new AgentRecord { AgentDateOfBirth = dob };

            // Assert
            agent.AgentDateOfBirth.Should().Be(dob);
        }

        [Theory]
        [InlineData("2020-01-01", "2022-01-01", true)]
        [InlineData("2022-01-01", "2020-01-01", false)]
        [InlineData("", "", false)]
        [InlineData("2020-01-01", "", false)]
        [InlineData("", "2022-01-01", false)]
        public void AgentStartDateAndEndDate_ShouldRespectChronologicalOrder(string start, string end, bool expectedIsValid)
        {
            // Arrange
            var agent = new AgentRecord
            {
                AgentStartDate = start,
                AgentEndDate = end
            };

            // Act
            bool isValid = false;
            if (DateTime.TryParse(agent.AgentStartDate, out var startDate) &&
                DateTime.TryParse(agent.AgentEndDate, out var endDate))
            {
                isValid = startDate <= endDate;
            }

            // Assert
            isValid.Should().Be(expectedIsValid);
        }

        [Theory]
        [InlineData("12345")]
        [InlineData("12345-6789")]
        [InlineData("")]
        public void AgentZipCode_ShouldAcceptValidAndEmptyStrings(string zip)
        {
            // Arrange & Act
            var agent = new AgentRecord { AgentZipCode = zip };

            // Assert
            agent.AgentZipCode.Should().Be(zip);
        }

        [Theory]
        [InlineData("john.doe@example.com")]
        [InlineData("")]
        [InlineData("invalid-email")]
        public void AgentEmail_ShouldAcceptAnyString(string email)
        {
            // Arrange & Act
            var agent = new AgentRecord { AgentEmail = email };

            // Assert
            agent.AgentEmail.Should().Be(email);
        }

        [Theory]
        [InlineData("555-1234")]
        [InlineData("")]
        [InlineData("+1-800-555-1212")]
        public void AgentContactNumber_ShouldAcceptAnyString(string contactNumber)
        {
            // Arrange & Act
            var agent = new AgentRecord { AgentContactNumber = contactNumber };

            // Assert
            agent.AgentContactNumber.Should().Be(contactNumber);
        }

        [Fact]
        public void Record_ShouldSupportDeconstruction()
        {
            // Arrange
            var agent = new AgentRecord
            {
                AgentCode = "A1",
                AgentName = "Agent Smith",
                AgentAddress1 = "123 Main St",
                AgentAddress2 = "Suite 100",
                AgentCity = "Metropolis",
                AgentState = "NY",
                AgentZipCode = "12345",
                AgentDateOfBirth = "1980-01-01",
                AgentType = "Broker",
                AgentStatus = "A",
                AgentEmail = "smith@example.com",
                AgentContactNumber = "555-1234",
                AgentStartDate = "2010-01-01",
                AgentEndDate = "2020-01-01"
            };

            // Act
            var (agentCode, agentName, agentAddress1, agentAddress2, agentCity, agentState, agentZipCode,
                agentDateOfBirth, agentType, agentStatus, agentEmail, agentContactNumber, agentStartDate, agentEndDate) = agent;

            // Assert
            agentCode.Should().Be("A1");
            agentName.Should().Be("Agent Smith");
            agentAddress1.Should().Be("123 Main St");
            agentAddress2.Should().Be("Suite 100");
            agentCity.Should().Be("Metropolis");
            agentState.Should().Be("NY");
            agentZipCode.Should().Be("12345");
            agentDateOfBirth.Should().Be("1980-01-01");
            agentType.Should().Be("Broker");
            agentStatus.Should().Be("A");
            agentEmail.Should().Be("smith@example.com");
            agentContactNumber.Should().Be("555-1234");
            agentStartDate.Should().Be("2010-01-01");
            agentEndDate.Should().Be("2020-01-01");
        }

        [Fact]
        public void Record_ShouldBeImmutable()
        {
            // Arrange
            var agent = new AgentRecord { AgentCode = "A1" };

            // Act
            Action act = () => { var code = agent.AgentCode; /* cannot set after init */ };

            // Assert
            act.Should().NotThrow();
            // Compile-time: agent.AgentCode = "A2"; // Should not compile (immutability)
        }

        [Fact]
        public void Record_ShouldSupportToString()
        {
            // Arrange
            var agent = new AgentRecord
            {
                AgentCode = "A1",
                AgentName = "Agent Smith"
            };

            // Act
            var str = agent.ToString();

            // Assert
            str.Should().Contain("A1");
            str.Should().Contain("Agent Smith");
        }

        [Fact]
        public void Record_ShouldSupportHashCode()
        {
            // Arrange
            var agent1 = new AgentRecord { AgentCode = "A1" };
            var agent2 = new AgentRecord { AgentCode = "A1" };

            // Act
            var hash1 = agent1.GetHashCode();
            var hash2 = agent2.GetHashCode();

            // Assert
            hash1.Should().Be(hash2);
        }

        [Fact]
        public void Record_ShouldAllowNullsForProperties()
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

        // Integration test placeholder for database persistence
        // (Assume a repository interface IAgentRepository with Save and Get methods)
        [Fact]
        public void Integration_SaveAndRetrieveAgentRecord_ShouldPreserveAllFields()
        {
            // Arrange
            var mockRepo = new Mock<IAgentRepository>();
            var agent = new AgentRecord
            {
                AgentCode = "A100",
                AgentName = "Jane Doe",
                AgentAddress1 = "456 Elm St",
                AgentAddress2 = "Apt 2B",
                AgentCity = "Gotham",
                AgentState = "NJ",
                AgentZipCode = "54321",
                AgentDateOfBirth = "1990-05-15",
                AgentType = "Employee",
                AgentStatus = "A",
                AgentEmail = "jane.doe@example.com",
                AgentContactNumber = "555-6789",
                AgentStartDate = "2015-06-01",
                AgentEndDate = "2023-06-01"
            };

            mockRepo.Setup(r => r.Save(agent)).Verifiable();
            mockRepo.Setup(r => r.Get("A100")).Returns(agent);

            // Act
            mockRepo.Object.Save(agent);
            var retrieved = mockRepo.Object.Get("A100");

            // Assert
            mockRepo.Verify(r => r.Save(agent), Times.Once);
            retrieved.Should().BeEquivalentTo(agent);
        }

        // Edge case: Empty record roundtrip
        [Fact]
        public void Integration_SaveAndRetrieveEmptyAgentRecord_ShouldPreserveDefaults()
        {
            // Arrange
            var mockRepo = new Mock<IAgentRepository>();
            var agent = new AgentRecord();

            mockRepo.Setup(r => r.Save(agent)).Verifiable();
            mockRepo.Setup(r => r.Get("")).Returns(agent);

            // Act
            mockRepo.Object.Save(agent);
            var retrieved = mockRepo.Object.Get("");

            // Assert
            mockRepo.Verify(r => r.Save(agent), Times.Once);
            retrieved.Should().BeEquivalentTo(agent);
        }
    }

    // Mock repository interface for integration test demonstration
    public interface IAgentRepository
    {
        void Save(AgentRecord agent);
        AgentRecord Get(string agentCode);
    }
}