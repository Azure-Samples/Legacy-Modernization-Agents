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
            _repositoryMock.VerifyNoOtherCalls();
        }

        [Fact]
        public void AgentFileException_ShouldSetStatusCodeAndMessage()
        {
            // Arrange
            var message = "Test exception";
            var statusCode = "42";

            // Act
            var ex = new AgentFileException(message, statusCode);

            // Assert
            ex.Message.Should().Be(message);
            ex.StatusCode.Should().Be(statusCode);
        }

        [Fact]
        public async Task ExecuteAsync_OpenOperation_ShouldReturnStatusCode00()
        {
            // Arrange
            _repositoryMock.Setup(r => r.OpenAsync()).Returns(Task.CompletedTask);

            var input = new AgentFileInput("OPEN", "");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.OpenAsync(), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_CloseOperation_ShouldReturnStatusCode00()
        {
            // Arrange
            _repositoryMock.Setup(r => r.CloseAsync()).Returns(Task.CompletedTask);

            var input = new AgentFileInput("CLOSE", "");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.CloseAsync(), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_SearchOperation_Found_ShouldReturnStatusCode00AndAgentRecord()
        {
            // Arrange
            var agentCode = "JohnDoe";
            var expectedRecord = new AgentRecord(
                "John Doe", "123 Main St", "Suite 100", "Copenhagen", "DK", "1000", "A", "Broker",
                "john.doe@email.com", "1234567890", "2020-01-01", "2025-12-31"
            );
            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ReturnsAsync(expectedRecord);

            var input = new AgentFileInput("SEARCH", agentCode);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.AgentRecord.Should().BeEquivalentTo(expectedRecord);
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_SearchOperation_NotFound_ShouldReturnStatusCode23()
        {
            // Arrange
            var agentCode = "NonExistent";
            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ReturnsAsync((AgentRecord)null);

            var input = new AgentFileInput("SEARCH", agentCode);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("23");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_UnknownOperationType_ShouldReturnStatusCode99()
        {
            // Arrange
            var input = new AgentFileInput("INVALID_OP", "");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("99");
            result.AgentRecord.Should().BeNull();
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public async Task ExecuteAsync_OperationTypeNullOrWhitespace_ShouldReturnStatusCode99(string operationType)
        {
            // Arrange
            var input = new AgentFileInput(operationType, "");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("99");
            result.AgentRecord.Should().BeNull();
        }

        [Fact]
        public async Task ExecuteAsync_SearchOperation_RepositoryThrowsAgentFileException_ShouldReturnStatusCodeFromException()
        {
            // Arrange
            var agentCode = "ErrorAgent";
            var exception = new AgentFileException("File not open", "10");
            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ThrowsAsync(exception);

            var input = new AgentFileInput("SEARCH", agentCode);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("10");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_SearchOperation_RepositoryThrowsGeneralException_ShouldReturnStatusCode99()
        {
            // Arrange
            var agentCode = "ErrorAgent";
            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ThrowsAsync(new InvalidOperationException("Unexpected error"));

            var input = new AgentFileInput("SEARCH", agentCode);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("99");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_OpenOperation_RepositoryThrowsAgentFileException_ShouldReturnStatusCodeFromException()
        {
            // Arrange
            var exception = new AgentFileException("File open failed", "12");
            _repositoryMock.Setup(r => r.OpenAsync()).ThrowsAsync(exception);

            var input = new AgentFileInput("OPEN", "");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("12");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.OpenAsync(), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_CloseOperation_RepositoryThrowsGeneralException_ShouldReturnStatusCode99()
        {
            // Arrange
            _repositoryMock.Setup(r => r.CloseAsync()).ThrowsAsync(new IOException("Disk error"));

            var input = new AgentFileInput("CLOSE", "");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("99");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.CloseAsync(), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_SearchOperation_NullAgentCode_ShouldReturnStatusCode23()
        {
            // Arrange
            _repositoryMock.Setup(r => r.SearchAsync(null)).ReturnsAsync((AgentRecord)null);

            var input = new AgentFileInput("SEARCH", null);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("23");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.SearchAsync(null), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_SearchOperation_EmptyAgentCode_ShouldReturnStatusCode23()
        {
            // Arrange
            _repositoryMock.Setup(r => r.SearchAsync("")).ReturnsAsync((AgentRecord)null);

            var input = new AgentFileInput("SEARCH", "");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("23");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.SearchAsync(""), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_SearchOperation_WhitespaceAgentCode_ShouldReturnStatusCode23()
        {
            // Arrange
            _repositoryMock.Setup(r => r.SearchAsync("   ")).ReturnsAsync((AgentRecord)null);

            var input = new AgentFileInput("SEARCH", "   ");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("23");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.SearchAsync("   "), Times.Once);
        }

        // Integration test: Simulate repository with real data
        [Fact]
        public async Task ExecuteAsync_SearchOperation_Integration_ShouldReturnAgentRecord()
        {
            // Arrange
            var loggerRepoMock = new Mock<ILogger<AgentFileRepository>>(MockBehavior.Loose);
            var repo = new AgentFileRepository(loggerRepoMock.Object);
            await repo.OpenAsync();

            var loggerDriverMock = new Mock<ILogger<AgentFileDriver.AgentFileDriver>>(MockBehavior.Loose);
            var driver = new AgentFileDriver.AgentFileDriver(repo, loggerDriverMock.Object);

            var input = new AgentFileInput("SEARCH", "JaneSmith");

            // Act
            var result = await driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.AgentRecord.Should().NotBeNull();
            result.AgentRecord.AgentName.Should().Be("Jane Smith");

            await repo.CloseAsync();
        }

        // Integration test: Search for non-existent agent
        [Fact]
        public async Task ExecuteAsync_SearchOperation_Integration_NotFound_ShouldReturnStatusCode23()
        {
            // Arrange
            var loggerRepoMock = new Mock<ILogger<AgentFileRepository>>(MockBehavior.Loose);
            var repo = new AgentFileRepository(loggerRepoMock.Object);
            await repo.OpenAsync();

            var loggerDriverMock = new Mock<ILogger<AgentFileDriver.AgentFileDriver>>(MockBehavior.Loose);
            var driver = new AgentFileDriver.AgentFileDriver(repo, loggerDriverMock.Object);

            var input = new AgentFileInput("SEARCH", "DoesNotExist");

            // Act
            var result = await driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("23");
            result.AgentRecord.Should().BeNull();

            await repo.CloseAsync();
        }

        // Integration test: Open and Close operations
        [Fact]
        public async Task ExecuteAsync_OpenAndClose_Integration_ShouldReturnStatusCode00()
        {
            // Arrange
            var loggerRepoMock = new Mock<ILogger<AgentFileRepository>>(MockBehavior.Loose);
            var repo = new AgentFileRepository(loggerRepoMock.Object);

            var loggerDriverMock = new Mock<ILogger<AgentFileDriver.AgentFileDriver>>(MockBehavior.Loose);
            var driver = new AgentFileDriver.AgentFileDriver(repo, loggerDriverMock.Object);

            var openInput = new AgentFileInput("OPEN", "");
            var closeInput = new AgentFileInput("CLOSE", "");

            // Act
            var openResult = await driver.ExecuteAsync(openInput);
            var closeResult = await driver.ExecuteAsync(closeInput);

            // Assert
            openResult.StatusCode.Should().Be("00");
            openResult.AgentRecord.Should().BeNull();
            closeResult.StatusCode.Should().Be("00");
            closeResult.AgentRecord.Should().BeNull();
        }

        // Edge case: OperationType is mixed case and has spaces
        [Theory]
        [InlineData(" open ")]
        [InlineData("Open")]
        [InlineData("OPEN")]
        [InlineData("OpEn")]
        public async Task ExecuteAsync_OpenOperation_MixedCase_ShouldReturnStatusCode00(string operationType)
        {
            // Arrange
            _repositoryMock.Setup(r => r.OpenAsync()).Returns(Task.CompletedTask);

            var input = new AgentFileInput(operationType, "");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.OpenAsync(), Times.Once);
        }

        // Edge case: OperationType is mixed case and has spaces for CLOSE
        [Theory]
        [InlineData(" close ")]
        [InlineData("Close")]
        [InlineData("CLOSE")]
        [InlineData("ClOsE")]
        public async Task ExecuteAsync_CloseOperation_MixedCase_ShouldReturnStatusCode00(string operationType)
        {
            // Arrange
            _repositoryMock.Setup(r => r.CloseAsync()).Returns(Task.CompletedTask);

            var input = new AgentFileInput(operationType, "");

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.AgentRecord.Should().BeNull();
            _repositoryMock.Verify(r => r.CloseAsync(), Times.Once);
        }

        // Edge case: OperationType is mixed case and has spaces for SEARCH
        [Theory]
        [InlineData(" search ")]
        [InlineData("Search")]
        [InlineData("SEARCH")]
        [InlineData("SeArCh")]
        public async Task ExecuteAsync_SearchOperation_MixedCase_ShouldReturnStatusCode00(string operationType)
        {
            // Arrange
            var agentCode = "JohnDoe";
            var expectedRecord = new AgentRecord(
                "John Doe", "123 Main St", "Suite 100", "Copenhagen", "DK", "1000", "A", "Broker",
                "john.doe@email.com", "1234567890", "2020-01-01", "2025-12-31"
            );
            _repositoryMock.Setup(r => r.SearchAsync(agentCode)).ReturnsAsync(expectedRecord);

            var input = new AgentFileInput(operationType, agentCode);

            // Act
            var result = await _driver.ExecuteAsync(input);

            // Assert
            result.StatusCode.Should().Be("00");
            result.AgentRecord.Should().BeEquivalentTo(expectedRecord);
            _repositoryMock.Verify(r => r.SearchAsync(agentCode), Times.Once);
        }

        // Edge case: AgentFileException with empty message and status code
        [Fact]
        public void AgentFileException_EmptyMessageAndStatusCode_ShouldSetProperties()
        {
            // Arrange
            var ex = new AgentFileException("", "");

            // Assert
            ex.Message.Should().BeEmpty();
            ex.StatusCode.Should().BeEmpty();
        }

        // Edge case: AgentFileException with null message and status code
        [Fact]
        public void AgentFileException_NullMessageAndStatusCode_ShouldSetProperties()
        {
            // Arrange
            var ex = new AgentFileException(null, null);

            // Assert
            ex.Message.Should().BeNull();
            ex.StatusCode.Should().BeNull();
        }
    }
}