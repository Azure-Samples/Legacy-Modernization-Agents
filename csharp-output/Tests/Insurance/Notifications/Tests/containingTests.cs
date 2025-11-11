using System;
using System.Threading.Tasks;
using FluentAssertions;
using Moq;
using Xunit;

namespace Insurance.Notifications.Tests
{
    public class containingTests : IDisposable
    {
        // Setup data for tests
        private readonly string _agentCode = "A123";
        private readonly string _agentName = "John Doe";
        private readonly string _agentAddress1 = "123 Main St";
        private readonly string _agentAddress2 = "Suite 200";
        private readonly string _agentCity = "Metropolis";
        private readonly string _agentState = "NY";
        private readonly string _policyNumber = "P456789";
        private readonly string _policyHolderFirstName = "Jane";
        private readonly string _policyHolderMiddleInitial = "Q";
        private readonly string _policyHolderLastName = "Public";
        private readonly string _policyStartDate = "2023-01-01";
        private readonly string _policyExpiryDate = "2024-01-01";
        private readonly string _notifyDate = "2023-12-01";
        private readonly string _notifyMessages = "Policy renewal notice";

        public containingTests()
        {
            // Setup code if needed
        }

        public void Dispose()
        {
            // Teardown code if needed
        }

        [Fact]
        public void Constructor_Should_Set_All_Properties_Correctly()
        {
            // Arrange & Act
            var record = new AgentNotifyRecord(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            // Assert
            record.AgentCode.Should().Be(_agentCode);
            record.AgentName.Should().Be(_agentName);
            record.AgentAddress1.Should().Be(_agentAddress1);
            record.AgentAddress2.Should().Be(_agentAddress2);
            record.AgentCity.Should().Be(_agentCity);
            record.AgentState.Should().Be(_agentState);
            record.PolicyNumber.Should().Be(_policyNumber);
            record.PolicyHolderFirstName.Should().Be(_policyHolderFirstName);
            record.PolicyHolderMiddleInitial.Should().Be(_policyHolderMiddleInitial);
            record.PolicyHolderLastName.Should().Be(_policyHolderLastName);
            record.PolicyStartDate.Should().Be(_policyStartDate);
            record.PolicyExpiryDate.Should().Be(_policyExpiryDate);
            record.NotifyDate.Should().Be(_notifyDate);
            record.NotifyMessages.Should().Be(_notifyMessages);
        }

        [Fact]
        public async Task CreateAsync_Should_Create_Record_With_Correct_Properties()
        {
            // Arrange & Act
            var record = await AgentNotifyRecord.CreateAsync(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            // Assert
            record.AgentCode.Should().Be(_agentCode);
            record.AgentName.Should().Be(_agentName);
            record.AgentAddress1.Should().Be(_agentAddress1);
            record.AgentAddress2.Should().Be(_agentAddress2);
            record.AgentCity.Should().Be(_agentCity);
            record.AgentState.Should().Be(_agentState);
            record.PolicyNumber.Should().Be(_policyNumber);
            record.PolicyHolderFirstName.Should().Be(_policyHolderFirstName);
            record.PolicyHolderMiddleInitial.Should().Be(_policyHolderMiddleInitial);
            record.PolicyHolderLastName.Should().Be(_policyHolderLastName);
            record.PolicyStartDate.Should().Be(_policyStartDate);
            record.PolicyExpiryDate.Should().Be(_policyExpiryDate);
            record.NotifyDate.Should().Be(_notifyDate);
            record.NotifyMessages.Should().Be(_notifyMessages);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        public void Constructor_Should_Allow_Null_Or_Empty_Strings_For_Optional_Fields(string value)
        {
            // Arrange & Act
            var record = new AgentNotifyRecord(
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value);

            // Assert
            record.AgentCode.Should().Be(value);
            record.AgentName.Should().Be(value);
            record.AgentAddress1.Should().Be(value);
            record.AgentAddress2.Should().Be(value);
            record.AgentCity.Should().Be(value);
            record.AgentState.Should().Be(value);
            record.PolicyNumber.Should().Be(value);
            record.PolicyHolderFirstName.Should().Be(value);
            record.PolicyHolderMiddleInitial.Should().Be(value);
            record.PolicyHolderLastName.Should().Be(value);
            record.PolicyStartDate.Should().Be(value);
            record.PolicyExpiryDate.Should().Be(value);
            record.NotifyDate.Should().Be(value);
            record.NotifyMessages.Should().Be(value);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        public async Task CreateAsync_Should_Allow_Null_Or_Empty_Strings_For_Optional_Fields(string value)
        {
            // Arrange & Act
            var record = await AgentNotifyRecord.CreateAsync(
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value,
                value);

            // Assert
            record.AgentCode.Should().Be(value);
            record.AgentName.Should().Be(value);
            record.AgentAddress1.Should().Be(value);
            record.AgentAddress2.Should().Be(value);
            record.AgentCity.Should().Be(value);
            record.AgentState.Should().Be(value);
            record.PolicyNumber.Should().Be(value);
            record.PolicyHolderFirstName.Should().Be(value);
            record.PolicyHolderMiddleInitial.Should().Be(value);
            record.PolicyHolderLastName.Should().Be(value);
            record.PolicyStartDate.Should().Be(value);
            record.PolicyExpiryDate.Should().Be(value);
            record.NotifyDate.Should().Be(value);
            record.NotifyMessages.Should().Be(value);
        }

        [Fact]
        public void Record_Should_Be_Immutable()
        {
            // Arrange
            var record = new AgentNotifyRecord(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            // Act & Assert
            Action act = () => record.AgentCode = "NEWCODE";
            act.Should().Throw<InvalidOperationException>();
        }

        [Fact]
        public void Records_With_Same_Values_Should_Be_Equal()
        {
            // Arrange
            var record1 = new AgentNotifyRecord(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            var record2 = new AgentNotifyRecord(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            // Assert
            record1.Should().Be(record2);
            (record1 == record2).Should().BeTrue();
        }

        [Fact]
        public void Records_With_Different_Values_Should_Not_Be_Equal()
        {
            // Arrange
            var record1 = new AgentNotifyRecord(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            var record2 = new AgentNotifyRecord(
                "DIFF",
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            // Assert
            record1.Should().NotBe(record2);
            (record1 != record2).Should().BeTrue();
        }

        [Fact]
        public void ToString_Should_Return_Expected_Format()
        {
            // Arrange
            var record = new AgentNotifyRecord(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            // Act
            var str = record.ToString();

            // Assert
            str.Should().Contain(nameof(AgentNotifyRecord));
            str.Should().Contain(_agentCode);
            str.Should().Contain(_agentName);
            str.Should().Contain(_policyNumber);
        }

        [Fact]
        public void GetHashCode_Should_Be_Consistent_For_Equal_Records()
        {
            // Arrange
            var record1 = new AgentNotifyRecord(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            var record2 = new AgentNotifyRecord(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            // Assert
            record1.GetHashCode().Should().Be(record2.GetHashCode());
        }

        [Fact]
        public void Should_Handle_Long_Strings_And_Boundary_Values()
        {
            // Arrange
            var longString = new string('X', 1000);
            var record = new AgentNotifyRecord(
                longString,
                longString,
                longString,
                longString,
                longString,
                longString,
                longString,
                longString,
                longString,
                longString,
                longString,
                longString,
                longString,
                longString);

            // Assert
            record.AgentCode.Should().Be(longString);
            record.AgentName.Should().Be(longString);
            record.AgentAddress1.Should().Be(longString);
            record.AgentAddress2.Should().Be(longString);
            record.AgentCity.Should().Be(longString);
            record.AgentState.Should().Be(longString);
            record.PolicyNumber.Should().Be(longString);
            record.PolicyHolderFirstName.Should().Be(longString);
            record.PolicyHolderMiddleInitial.Should().Be(longString);
            record.PolicyHolderLastName.Should().Be(longString);
            record.PolicyStartDate.Should().Be(longString);
            record.PolicyExpiryDate.Should().Be(longString);
            record.NotifyDate.Should().Be(longString);
            record.NotifyMessages.Should().Be(longString);
        }

        [Fact]
        public void Should_Handle_Invalid_Date_Formats_As_Strings()
        {
            // Arrange
            var invalidDate = "not-a-date";
            var record = new AgentNotifyRecord(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                invalidDate,
                invalidDate,
                invalidDate,
                _notifyMessages);

            // Assert
            record.PolicyStartDate.Should().Be(invalidDate);
            record.PolicyExpiryDate.Should().Be(invalidDate);
            record.NotifyDate.Should().Be(invalidDate);
        }

        // Example integration test for database operation (mocked, as no DB logic in class)
        [Fact]
        public async Task IntegrationTest_Should_Save_And_Retrieve_Record_From_Database_Mock()
        {
            // Arrange
            var dbMock = new Mock<IAgentNotifyRecordRepository>();
            var expectedRecord = await AgentNotifyRecord.CreateAsync(
                _agentCode,
                _agentName,
                _agentAddress1,
                _agentAddress2,
                _agentCity,
                _agentState,
                _policyNumber,
                _policyHolderFirstName,
                _policyHolderMiddleInitial,
                _policyHolderLastName,
                _policyStartDate,
                _policyExpiryDate,
                _notifyDate,
                _notifyMessages);

            dbMock.Setup(r => r.SaveAsync(It.IsAny<AgentNotifyRecord>())).Returns(Task.CompletedTask);
            dbMock.Setup(r => r.GetByPolicyNumberAsync(_policyNumber)).ReturnsAsync(expectedRecord);

            // Act
            await dbMock.Object.SaveAsync(expectedRecord);
            var retrieved = await dbMock.Object.GetByPolicyNumberAsync(_policyNumber);

            // Assert
            retrieved.Should().Be(expectedRecord);
            dbMock.Verify(r => r.SaveAsync(It.Is<AgentNotifyRecord>(x => x == expectedRecord)), Times.Once);
            dbMock.Verify(r => r.GetByPolicyNumberAsync(_policyNumber), Times.Once);
        }
    }

    // Mock repository interface for integration test
    public interface IAgentNotifyRecordRepository
    {
        Task SaveAsync(AgentNotifyRecord record);
        Task<AgentNotifyRecord> GetByPolicyNumberAsync(string policyNumber);
    }
}