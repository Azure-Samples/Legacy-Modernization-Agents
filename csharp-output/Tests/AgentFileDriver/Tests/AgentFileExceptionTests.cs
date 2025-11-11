using System;
using System.Threading.Tasks;
using AgentFileDriver;
using FluentAssertions;
using Moq;
using Xunit;
using Microsoft.Extensions.Logging;

namespace AgentFileDriver.Tests
{
    public class AgentFileExceptionTests : IDisposable
    {
        private readonly Mock<IAgentFileRepository> _repositoryMock;
        private readonly Mock<ILogger<AgentFileDriver.AgentFileDriver>> _loggerMock;
        private readonly AgentFileDriver.AgentFileDriver _driver;

        public AgentFileExceptionTests()
        {
            _repositoryMock = new Mock<IAgentFileRepository>(MockBehavior.Strict);
            _loggerMock = new Mock<ILogger<AgentFileDriver.AgentFileDriver>>(MockBehavior.Loose);
            _driver = new AgentFileDriver.AgentFileDriver(_repositoryMock.Object, _loggerMock.Object);
        }

        public void Dispose()
        {
            _repositoryMock.Reset();
            _loggerMock.Reset();
        }

        [Fact]
        public void AgentFileException_Should_Set_StatusCode_And_Message()
        {
            // Arrange
            var message = "Test error";
            var statusCode = "98";

            // Act
            var ex = new AgentFileException(message, statusCode);

            // Assert
            ex.Message.Should().Be(message);
            ex.StatusCode.Should().Be(statusCode);
        }

        [Fact]
        public async Task ExecuteAsync_Should_Throw_ArgumentNullException_When_Input_Is_Null()
        {
            // Arrange
            AgentFileOperationInput input = null;

            // Act
            Func<Task> act = async () => await _driver.ExecuteAsync(input);

            // Assert
            await act.Should().ThrowAsync<ArgumentNullException>();
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        [InlineData("INVALID")]
        [InlineData("openx")]
        public async Task ExecuteAsync_Should_Return_StatusCode_99_For_Invalid_OperationType(string operationType)
        {
            // Arrange
            var input = new AgentFileOperationInput(operationType, "A123");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("99");
            result.Agent.Should().BeNull();
            _loggerMock.Verify(
                l => l.Log(
                    LogLevel.Warning,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Invalid operation type")),
                    null,
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_Should_Return_StatusCode_00_For_Open_Operation()
        {
            // Arrange
            var input = new AgentFileOperationInput("OPEN", null);
            _repositoryMock.Setup(r => r.OpenAsync()).Returns(Task.CompletedTask);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.Agent.Should().BeNull();
            _repositoryMock.Verify(r => r.OpenAsync(), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_Should_Return_StatusCode_98_When_Open_Throws_Exception()
        {
            // Arrange
            var input = new AgentFileOperationInput("OPEN", null);
            _repositoryMock.Setup(r => r.OpenAsync()).ThrowsAsync(new Exception("IO error"));

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("98");
            result.Agent.Should().BeNull();
            _repositoryMock.Verify(r => r.OpenAsync(), Times.Once);
            _loggerMock.Verify(
                l => l.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Agent file operation failed")),
                    It.IsAny<AgentFileException>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_Should_Return_StatusCode_00_For_Close_Operation()
        {
            // Arrange
            var input = new AgentFileOperationInput("CLOSE", null);
            _repositoryMock.Setup(r => r.CloseAsync()).Returns(Task.CompletedTask);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.Agent.Should().BeNull();
            _repositoryMock.Verify(r => r.CloseAsync(), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_Should_Return_StatusCode_98_When_Close_Throws_Exception()
        {
            // Arrange
            var input = new AgentFileOperationInput("CLOSE", null);
            _repositoryMock.Setup(r => r.CloseAsync()).ThrowsAsync(new Exception("IO error"));

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("98");
            result.Agent.Should().BeNull();
            _repositoryMock.Verify(r => r.CloseAsync(), Times.Once);
            _loggerMock.Verify(
                l => l.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Agent file operation failed")),
                    It.IsAny<AgentFileException>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_Should_Return_StatusCode_99_For_Search_With_NullOrWhitespace_AgentCode()
        {
            // Arrange
            var input = new AgentFileOperationInput("SEARCH", null);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("99");
            result.Agent.Should().BeNull();
        }

        [Theory]
        [InlineData("")]
        [InlineData(" ")]
        public async Task ExecuteAsync_Should_Return_StatusCode_99_For_Search_With_EmptyOrWhitespace_AgentCode(string agentCode)
        {
            // Arrange
            var input = new AgentFileOperationInput("SEARCH", agentCode);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("99");
            result.Agent.Should().BeNull();
        }

        [Fact]
        public async Task ExecuteAsync_Should_Return_StatusCode_00_And_Agent_For_Search_Found()
        {
            // Arrange
            var agentCode = "A123";
            var expectedAgent = new AgentRecord(
                "John Doe", "123 Main St", "Suite 1", "Metropolis", "NY", "10001",
                "Active", "TypeA", "john@doe.com", agentCode, "2020-01-01", "2025-01-01"
            );
            var input = new AgentFileOperationInput("SEARCH", agentCode);

            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ReturnsAsync(expectedAgent);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.Agent.Should().BeEquivalentTo(expectedAgent);
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_Should_Return_StatusCode_23_For_Search_Not_Found()
        {
            // Arrange
            var agentCode = "A999";
            var input = new AgentFileOperationInput("SEARCH", agentCode);

            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ReturnsAsync((AgentRecord)null);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("23");
            result.Agent.Should().BeNull();
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_Should_Return_StatusCode_98_When_Search_Throws_Exception()
        {
            // Arrange
            var agentCode = "A123";
            var input = new AgentFileOperationInput("SEARCH", agentCode);

            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ThrowsAsync(new Exception("File error"));

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("98");
            result.Agent.Should().BeNull();
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
            _loggerMock.Verify(
                l => l.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Agent file operation failed")),
                    It.IsAny<AgentFileException>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Theory]
        [InlineData("open")]
        [InlineData("OPEN")]
        [InlineData(" Open ")]
        [InlineData("  open  ")]
        public async Task ExecuteAsync_Should_Handle_OperationType_Case_And_Whitespace_For_Open(string operationType)
        {
            // Arrange
            var input = new AgentFileOperationInput(operationType, null);
            _repositoryMock.Setup(r => r.OpenAsync()).Returns(Task.CompletedTask);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.Agent.Should().BeNull();
            _repositoryMock.Verify(r => r.OpenAsync(), Times.Once);
        }

        [Theory]
        [InlineData("close")]
        [InlineData("CLOSE")]
        [InlineData(" Close ")]
        [InlineData("  close  ")]
        public async Task ExecuteAsync_Should_Handle_OperationType_Case_And_Whitespace_For_Close(string operationType)
        {
            // Arrange
            var input = new AgentFileOperationInput(operationType, null);
            _repositoryMock.Setup(r => r.CloseAsync()).Returns(Task.CompletedTask);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.Agent.Should().BeNull();
            _repositoryMock.Verify(r => r.CloseAsync(), Times.Once);
        }

        [Theory]
        [InlineData("search")]
        [InlineData("SEARCH")]
        [InlineData(" Search ")]
        [InlineData("  search  ")]
        public async Task ExecuteAsync_Should_Handle_OperationType_Case_And_Whitespace_For_Search(string operationType)
        {
            // Arrange
            var agentCode = "A123";
            var expectedAgent = new AgentRecord(
                "Jane Doe", "456 Elm St", "Apt 2", "Gotham", "NJ", "07001",
                "Inactive", "TypeB", "jane@doe.com", agentCode, "2019-01-01", "2024-01-01"
            );
            var input = new AgentFileOperationInput(operationType, agentCode);

            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ReturnsAsync(expectedAgent);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.Agent.Should().BeEquivalentTo(expectedAgent);
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_Should_Return_StatusCode_99_When_Unexpected_Exception_Occurs()
        {
            // Arrange
            var input = new AgentFileOperationInput("OPEN", null);
            _repositoryMock.Setup(r => r.OpenAsync()).ThrowsAsync(new InvalidOperationException("Unexpected error"));

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("98"); // Should be mapped to AgentFileException, not "99"
            result.Agent.Should().BeNull();
        }

        [Fact]
        public async Task ExecuteAsync_Should_Log_Critical_When_Unexpected_Exception_Occurs()
        {
            // Arrange
            var input = new AgentFileOperationInput("OPEN", null);
            _repositoryMock.Setup(r => r.OpenAsync()).ThrowsAsync(new Exception("Critical error"));

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("98");
            _loggerMock.Verify(
                l => l.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.IsAny<It.IsAnyType>(),
                    It.IsAny<AgentFileException>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        // Integration test for InMemoryAgentFileRepository
        [Fact]
        public async Task Integration_Search_Should_Return_Agent_When_Found()
        {
            // Arrange
            var agentCode = "C789";
            var agent = new AgentRecord(
                "Alice Smith", "789 Oak St", "Floor 3", "Star City", "CA", "90001",
                "Active", "TypeC", "alice@smith.com", agentCode, "2021-01-01", "2026-01-01"
            );
            var repo = new InMemoryAgentFileRepository(new[] { agent });
            var logger = new Mock<ILogger<AgentFileDriver.AgentFileDriver>>();
            var driver = new AgentFileDriver.AgentFileDriver(repo, logger.Object);

            await repo.OpenAsync();

            var input = new AgentFileOperationInput("SEARCH", agentCode);

            // Act
            var result = await driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.Agent.Should().BeEquivalentTo(agent);

            await repo.DisposeAsync();
        }

        [Fact]
        public async Task Integration_Search_Should_Return_23_When_Not_Found()
        {
            // Arrange
            var repo = new InMemoryAgentFileRepository(Array.Empty<AgentRecord>());
            var logger = new Mock<ILogger<AgentFileDriver.AgentFileDriver>>();
            var driver = new AgentFileDriver.AgentFileDriver(repo, logger.Object);

            await repo.OpenAsync();

            var input = new AgentFileOperationInput("SEARCH", "UNKNOWN");

            // Act
            var result = await driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("23");
            result.Agent.Should().BeNull();

            await repo.DisposeAsync();
        }

        [Fact]
        public async Task Integration_Search_Should_Return_98_When_File_Not_Open()
        {
            // Arrange
            var agentCode = "C789";
            var agent = new AgentRecord(
                "Alice Smith", "789 Oak St", "Floor 3", "Star City", "CA", "90001",
                "Active", "TypeC", "alice@smith.com", agentCode, "2021-01-01", "2026-01-01"
            );
            var repo = new InMemoryAgentFileRepository(new[] { agent });
            var logger = new Mock<ILogger<AgentFileDriver.AgentFileDriver>>();
            var driver = new AgentFileDriver.AgentFileDriver(repo, logger.Object);

            // Do NOT open file

            var input = new AgentFileOperationInput("SEARCH", agentCode);

            // Act
            var result = await driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("98");
            result.Agent.Should().BeNull();

            await repo.DisposeAsync();
        }

        // Edge case: AgentCode with leading/trailing whitespace
        [Fact]
        public async Task ExecuteAsync_Should_Trim_AgentCode_Before_Search()
        {
            // Arrange
            var agentCode = "X123";
            var inputCode = "  X123  ";
            var expectedAgent = new AgentRecord(
                "Bob Brown", "321 Pine St", "", "Central City", "TX", "75001",
                "Active", "TypeD", "bob@brown.com", agentCode, "2022-01-01", "2027-01-01"
            );
            var input = new AgentFileOperationInput("SEARCH", inputCode);

            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ReturnsAsync(expectedAgent);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.Agent.Should().BeEquivalentTo(expectedAgent);
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
        }

        // Null checks for constructor
        [Fact]
        public void Constructor_Should_Throw_ArgumentNullException_When_Repository_Is_Null()
        {
            // Arrange
            IAgentFileRepository repo = null;
            var logger = new Mock<ILogger<AgentFileDriver.AgentFileDriver>>().Object;

            // Act
            Action act = () => new AgentFileDriver.AgentFileDriver(repo, logger);

            // Assert
            act.Should().Throw<ArgumentNullException>().WithParameterName("repository");
        }

        [Fact]
        public void Constructor_Should_Throw_ArgumentNullException_When_Logger_Is_Null()
        {
            // Arrange
            var repo = new Mock<IAgentFileRepository>().Object;
            ILogger<AgentFileDriver.AgentFileDriver> logger = null;

            // Act
            Action act = () => new AgentFileDriver.AgentFileDriver(repo, logger);

            // Assert
            act.Should().Throw<ArgumentNullException>().WithParameterName("logger");
        }

        // Edge case: AgentRecord with null/empty fields
        [Fact]
        public async Task ExecuteAsync_Should_Handle_AgentRecord_With_Null_Fields()
        {
            // Arrange
            var agentCode = "NUL123";
            var agent = new AgentRecord(
                null, null, null, null, null, null, null, null, null, agentCode, null, null
            );
            var input = new AgentFileOperationInput("SEARCH", agentCode);

            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ReturnsAsync(agent);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.Agent.Should().BeEquivalentTo(agent);
        }
    }
}