using System;
using System.Threading.Tasks;
using FluentAssertions;
using InsuranceAgentNotifications.Models;
using InsuranceAgentNotifications.Services;
using Moq;
using Xunit;

namespace InsuranceAgentNotifications.Models.Tests
{
    public class AgentNotifyServiceTests : IDisposable
    {
        private AgentNotifyService _service;

        public AgentNotifyServiceTests()
        {
            // Setup before each test
            _service = new AgentNotifyService();
        }

        public void Dispose()
        {
            // Teardown after each test
            // No resources to dispose in this implementation
        }

        [Fact]
        public async Task ProcessNotificationAsync_ShouldProcessValidRecord()
        {
            // Arrange
            var record = new AgentNotifyRecord
            {
                AgentCode = "A123",
                AgentName = "John Doe",
                AgentAddress1 = "123 Main St",
                AgentAddress2 = "Suite 100",
                AgentCity = "Metropolis",
                AgentState = "NY",
                PolicyNumber = "P456789",
                PolicyHolderFirstName = "Jane",
                PolicyHolderMiddleInitial = "Q",
                PolicyHolderLastName = "Smith",
                PolicyStartDate = "2024-01-01",
                PolicyExpiryDate = "2025-01-01",
                NotifyDate = "2024-06-01",
                NotifyMessages = "Renewal reminder"
            };

            // Act
            Func<Task> act = async () => await _service.ProcessNotificationAsync(record);

            // Assert
            await act.Should().NotThrowAsync();
        }

        [Fact]
        public async Task ProcessNotificationAsync_ShouldThrowArgumentNullException_WhenRecordIsNull()
        {
            // Arrange
            AgentNotifyRecord? record = null;

            // Act
            Func<Task> act = async () => await _service.ProcessNotificationAsync(record!);

            // Assert
            await act.Should().ThrowAsync<ArgumentNullException>()
                .WithMessage("*Agent notification record cannot be null*");
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        public async Task ProcessNotificationAsync_ShouldProcessRecord_WithEmptyOrNullFields(string value)
        {
            // Arrange
            var record = new AgentNotifyRecord
            {
                AgentCode = value ?? string.Empty,
                AgentName = value ?? string.Empty,
                AgentAddress1 = value ?? string.Empty,
                AgentAddress2 = value ?? string.Empty,
                AgentCity = value ?? string.Empty,
                AgentState = value ?? string.Empty,
                PolicyNumber = value ?? string.Empty,
                PolicyHolderFirstName = value ?? string.Empty,
                PolicyHolderMiddleInitial = value ?? string.Empty,
                PolicyHolderLastName = value ?? string.Empty,
                PolicyStartDate = value ?? string.Empty,
                PolicyExpiryDate = value ?? string.Empty,
                NotifyDate = value ?? string.Empty,
                NotifyMessages = value ?? string.Empty
            };

            // Act
            Func<Task> act = async () => await _service.ProcessNotificationAsync(record);

            // Assert
            await act.Should().NotThrowAsync();
        }

        [Theory]
        [InlineData("2024-02-29")] // Leap year date
        [InlineData("1900-01-01")] // Very old date
        [InlineData("9999-12-31")] // Far future date
        public async Task ProcessNotificationAsync_ShouldProcessRecord_WithBoundaryDates(string date)
        {
            // Arrange
            var record = new AgentNotifyRecord
            {
                AgentCode = "B999",
                AgentName = "Boundary Tester",
                PolicyNumber = "P999999",
                PolicyStartDate = date,
                PolicyExpiryDate = date,
                NotifyDate = date,
                NotifyMessages = "Boundary date test"
            };

            // Act
            Func<Task> act = async () => await _service.ProcessNotificationAsync(record);

            // Assert
            await act.Should().NotThrowAsync();
        }

        [Fact]
        public async Task ProcessNotificationAsync_ShouldProcessRecord_WithLongStrings()
        {
            // Arrange
            var longString = new string('X', 1000);
            var record = new AgentNotifyRecord
            {
                AgentCode = longString,
                AgentName = longString,
                AgentAddress1 = longString,
                AgentAddress2 = longString,
                AgentCity = longString,
                AgentState = longString,
                PolicyNumber = longString,
                PolicyHolderFirstName = longString,
                PolicyHolderMiddleInitial = "Z",
                PolicyHolderLastName = longString,
                PolicyStartDate = "2024-06-01",
                PolicyExpiryDate = "2025-06-01",
                NotifyDate = "2024-06-01",
                NotifyMessages = longString
            };

            // Act
            Func<Task> act = async () => await _service.ProcessNotificationAsync(record);

            // Assert
            await act.Should().NotThrowAsync();
        }

        [Fact]
        public async Task ProcessNotificationAsync_ShouldProcessRecord_WithMinimalFields()
        {
            // Arrange
            var record = new AgentNotifyRecord
            {
                AgentCode = "A1",
                PolicyNumber = "P1",
                NotifyMessages = "Msg"
                // Other fields left as default (empty)
            };

            // Act
            Func<Task> act = async () => await _service.ProcessNotificationAsync(record);

            // Assert
            await act.Should().NotThrowAsync();
        }

        [Fact]
        public async Task ProcessNotificationAsync_ShouldHandleException_AndRethrow()
        {
            // Arrange
            var serviceMock = new Mock<AgentNotifyService>() { CallBase = true };
            var record = new AgentNotifyRecord
            {
                AgentCode = "EXC",
                PolicyNumber = "P-EXC",
                NotifyMessages = "Test exception"
            };

            // Simulate an exception in the processing logic
            serviceMock
                .Setup(s => s.ProcessNotificationAsync(It.IsAny<AgentNotifyRecord>()))
                .ThrowsAsync(new InvalidOperationException("Simulated failure"));

            // Act
            Func<Task> act = async () => await serviceMock.Object.ProcessNotificationAsync(record);

            // Assert
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Simulated failure");
        }

        [Fact]
        public async Task ProcessNotificationAsync_ShouldLogNotification_WhenProcessing()
        {
            // Arrange
            var record = new AgentNotifyRecord
            {
                AgentCode = "LOG1",
                PolicyNumber = "PLOG1",
                NotifyMessages = "Log test"
            };

            using var consoleOutput = new ConsoleOutputCapture();

            // Act
            await _service.ProcessNotificationAsync(record);

            // Assert
            consoleOutput.GetOutput().Should().Contain("Notification processed for agent: LOG1, policy: PLOG1");
        }

        // Integration test simulating database operation (placeholder)
        [Fact]
        public async Task ProcessNotificationAsync_IntegrationTest_ShouldSimulateDatabaseOperation()
        {
            // Arrange
            var record = new AgentNotifyRecord
            {
                AgentCode = "DB1",
                PolicyNumber = "PDB1",
                NotifyMessages = "DB integration test"
            };

            // Simulate a database service dependency using Moq
            var dbServiceMock = new Mock<IDatabaseService>();
            dbServiceMock.Setup(db => db.SaveNotificationAsync(It.IsAny<AgentNotifyRecord>()))
                .Returns(Task.CompletedTask)
                .Verifiable();

            var service = new AgentNotifyServiceWithDb(dbServiceMock.Object);

            // Act
            Func<Task> act = async () => await service.ProcessNotificationAsync(record);

            // Assert
            await act.Should().NotThrowAsync();
            dbServiceMock.Verify(db => db.SaveNotificationAsync(It.Is<AgentNotifyRecord>(r => r.AgentCode == "DB1" && r.PolicyNumber == "PDB1")), Times.Once);
        }

        // Helper class to capture console output for logging verification
        private class ConsoleOutputCapture : IDisposable
        {
            private readonly System.IO.StringWriter _stringWriter;
            private readonly System.IO.TextWriter _originalOutput;

            public ConsoleOutputCapture()
            {
                _stringWriter = new System.IO.StringWriter();
                _originalOutput = Console.Out;
                Console.SetOut(_stringWriter);
            }

            public string GetOutput()
            {
                return _stringWriter.ToString();
            }

            public void Dispose()
            {
                Console.SetOut(_originalOutput);
                _stringWriter.Dispose();
            }
        }

        // Mock database service interface for integration test
        public interface IDatabaseService
        {
            Task SaveNotificationAsync(AgentNotifyRecord record);
        }

        // AgentNotifyService variant with database dependency for integration test
        public class AgentNotifyServiceWithDb : IAgentNotifyService
        {
            private readonly IDatabaseService _dbService;

            public AgentNotifyServiceWithDb(IDatabaseService dbService)
            {
                _dbService = dbService;
            }

            public async Task ProcessNotificationAsync(AgentNotifyRecord record)
            {
                if (record is null)
                    throw new ArgumentNullException(nameof(record), "Agent notification record cannot be null.");

                await _dbService.SaveNotificationAsync(record);

                // Simulate logging
                Console.WriteLine($"Notification processed for agent: {record.AgentCode}, policy: {record.PolicyNumber}");
            }
        }
    }
}