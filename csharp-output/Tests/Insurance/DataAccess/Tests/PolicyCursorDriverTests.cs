using System;
using System.Data;
using System.Data.Common;
using System.Threading;
using System.Threading.Tasks;
using FluentAssertions;
using Insurance.DataAccess;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace Insurance.DataAccess.Tests
{
    public class PolicyCursorDriverTests : IDisposable
    {
        private readonly Mock<DbConnection> _mockDbConnection;
        private readonly Mock<ILogger<PolicyCursorDriver>> _mockLogger;
        private readonly Mock<DbCommand> _mockDbCommand;
        private readonly Mock<DbDataReader> _mockDbDataReader;
        private readonly PolicyCursorDriver _driver;

        public PolicyCursorDriverTests()
        {
            _mockDbConnection = new Mock<DbConnection>();
            _mockLogger = new Mock<ILogger<PolicyCursorDriver>>();
            _mockDbCommand = new Mock<DbCommand>();
            _mockDbDataReader = new Mock<DbDataReader>();

            // Setup DbConnection to return our mock command
            _mockDbConnection.Setup(c => c.CreateCommand()).Returns(_mockDbCommand.Object);

            // Setup DbConnection State
            _mockDbConnection.SetupProperty(c => c.State, ConnectionState.Closed);

            _driver = new PolicyCursorDriver(_mockDbConnection.Object, _mockLogger.Object);
        }

        public void Dispose()
        {
            _driver.DisposeAsync().GetAwaiter().GetResult();
        }

        [Fact]
        public async Task ExecuteAsync_ShouldThrowArgumentNullException_WhenRequestIsNull()
        {
            Func<Task> act = async () => await _driver.ExecuteAsync(null);

            await act.Should().ThrowAsync<ArgumentNullException>();
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("INVALID")]
        public async Task ExecuteAsync_ShouldReturnMinusOneSqlCode_AndLogError_WhenOperationTypeIsInvalid(string operationType)
        {
            var request = new PolicyCursorRequest(operationType, "2024-06-01");

            var response = await _driver.ExecuteAsync(request);

            response.SqlCode.Should().Be(-1);
            response.PolicyData.Should().BeNull();
            _mockLogger.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Invalid operation type")),
                    null,
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldOpenPolicyCursor_WhenOperationTypeIsOpen_AndReturnZeroSqlCode()
        {
            var request = new PolicyCursorRequest("OPEN", "2024-06-01");

            // Setup OpenAsync to set State to Open
            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            // Setup command and parameter
            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);

            // Setup ExecuteReaderAsync
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            var response = await _driver.ExecuteAsync(request);

            response.SqlCode.Should().Be(0);
            response.PolicyData.Should().BeNull();
            _mockDbConnection.Verify(c => c.OpenAsync(It.IsAny<CancellationToken>()), Times.Once);
            _mockDbCommand.Verify(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldNotOpenConnection_IfAlreadyOpen()
        {
            var request = new PolicyCursorRequest("OPEN", "2024-06-01");
            _mockDbConnection.SetupGet(c => c.State).Returns(ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            var response = await _driver.ExecuteAsync(request);

            response.SqlCode.Should().Be(0);
            _mockDbConnection.Verify(c => c.OpenAsync(It.IsAny<CancellationToken>()), Times.Never);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldReturnDbExceptionErrorCode_WhenOpenPolicyCursorThrowsDbException()
        {
            var request = new PolicyCursorRequest("OPEN", "2024-06-01");

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .ThrowsAsync(CreateDbException(1234));

            var response = await _driver.ExecuteAsync(request);

            response.SqlCode.Should().Be(1234);
            _mockLogger.VerifyLogContains("Error opening policy cursor.", LogLevel.Error);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldReturnMinus99999_WhenOpenPolicyCursorThrowsUnexpectedException()
        {
            var request = new PolicyCursorRequest("OPEN", "2024-06-01");

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .ThrowsAsync(new InvalidOperationException("Unexpected"));

            var response = await _driver.ExecuteAsync(request);

            response.SqlCode.Should().Be(-99999);
            _mockLogger.VerifyLogContains("Unexpected error opening policy cursor.", LogLevel.Error);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldFetchPolicyRecord_WhenOperationTypeIsFetch_AndReturnPolicyData()
        {
            var requestOpen = new PolicyCursorRequest("OPEN", "2024-06-01");
            var requestFetch = new PolicyCursorRequest("FETCH", "2024-06-01");

            // Setup Open
            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            // Open the cursor
            await _driver.ExecuteAsync(requestOpen);

            // Setup ReadAsync for FETCH
            _mockDbDataReader.Setup(r => r.ReadAsync(It.IsAny<CancellationToken>())).ReturnsAsync(true);

            // Setup all Get* methods for a valid PolicyRecord
            SetupMockPolicyRecord(_mockDbDataReader);

            var response = await _driver.ExecuteAsync(requestFetch);

            response.SqlCode.Should().Be(0);
            response.PolicyData.Should().NotBeNull();
            response.PolicyData.PolicyNumber.Should().Be("PN123456");
            response.PolicyData.PolicyHolderFirstName.Should().Be("John");
            response.PolicyData.PolicyHolderLastName.Should().Be("Doe");
            response.PolicyData.PolicyPremiumAmount.Should().Be(1200.50m);
            response.PolicyData.PolicyCoverageAmount.Should().Be(50000.00m);
            response.PolicyData.PolicyClaimed.Should().BeTrue();
            response.PolicyData.PolicyNotifyFlag.Should().BeFalse();
        }

        [Fact]
        public async Task ExecuteAsync_ShouldReturn100SqlCode_WhenFetchReturnsNoData()
        {
            var requestOpen = new PolicyCursorRequest("OPEN", "2024-06-01");
            var requestFetch = new PolicyCursorRequest("FETCH", "2024-06-01");

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            await _driver.ExecuteAsync(requestOpen);

            _mockDbDataReader.Setup(r => r.ReadAsync(It.IsAny<CancellationToken>())).ReturnsAsync(false);

            var response = await _driver.ExecuteAsync(requestFetch);

            response.SqlCode.Should().Be(100);
            response.PolicyData.Should().BeNull();
        }

        [Fact]
        public async Task ExecuteAsync_ShouldReturnMinus2SqlCode_WhenFetchCalledWithoutOpen()
        {
            var requestFetch = new PolicyCursorRequest("FETCH", "2024-06-01");

            var response = await _driver.ExecuteAsync(requestFetch);

            response.SqlCode.Should().Be(-2);
            response.PolicyData.Should().BeNull();
            _mockLogger.VerifyLogContains("Policy cursor is not open.", LogLevel.Error);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldReturnDbExceptionErrorCode_WhenFetchThrowsDbException()
        {
            var requestOpen = new PolicyCursorRequest("OPEN", "2024-06-01");
            var requestFetch = new PolicyCursorRequest("FETCH", "2024-06-01");

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            await _driver.ExecuteAsync(requestOpen);

            _mockDbDataReader.Setup(r => r.ReadAsync(It.IsAny<CancellationToken>()))
                .ThrowsAsync(CreateDbException(5555));

            var response = await _driver.ExecuteAsync(requestFetch);

            response.SqlCode.Should().Be(5555);
            response.PolicyData.Should().BeNull();
            _mockLogger.VerifyLogContains("Error fetching from policy cursor.", LogLevel.Error);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldReturnMinus99999_WhenFetchThrowsUnexpectedException()
        {
            var requestOpen = new PolicyCursorRequest("OPEN", "2024-06-01");
            var requestFetch = new PolicyCursorRequest("FETCH", "2024-06-01");

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            await _driver.ExecuteAsync(requestOpen);

            _mockDbDataReader.Setup(r => r.ReadAsync(It.IsAny<CancellationToken>()))
                .ThrowsAsync(new InvalidOperationException("Unexpected"));

            var response = await _driver.ExecuteAsync(requestFetch);

            response.SqlCode.Should().Be(-99999);
            response.PolicyData.Should().BeNull();
            _mockLogger.VerifyLogContains("Unexpected error fetching from policy cursor.", LogLevel.Error);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldClosePolicyCursor_WhenOperationTypeIsClose_AndReturnZeroSqlCode()
        {
            var requestOpen = new PolicyCursorRequest("OPEN", "2024-06-01");
            var requestClose = new PolicyCursorRequest("CLOSE", "2024-06-01");

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            await _driver.ExecuteAsync(requestOpen);

            _mockDbDataReader.Setup(r => r.DisposeAsync()).Returns(ValueTask.CompletedTask);

            var response = await _driver.ExecuteAsync(requestClose);

            response.SqlCode.Should().Be(0);
            response.PolicyData.Should().BeNull();
            _mockDbDataReader.Verify(r => r.DisposeAsync(), Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldReturnDbExceptionErrorCode_WhenCloseThrowsDbException()
        {
            var requestOpen = new PolicyCursorRequest("OPEN", "2024-06-01");
            var requestClose = new PolicyCursorRequest("CLOSE", "2024-06-01");

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            await _driver.ExecuteAsync(requestOpen);

            _mockDbDataReader.Setup(r => r.DisposeAsync()).Throws(CreateDbException(8888));

            var response = await _driver.ExecuteAsync(requestClose);

            response.SqlCode.Should().Be(8888);
            _mockLogger.VerifyLogContains("Error closing policy cursor.", LogLevel.Error);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldReturnMinus99999_WhenCloseThrowsUnexpectedException()
        {
            var requestOpen = new PolicyCursorRequest("OPEN", "2024-06-01");
            var requestClose = new PolicyCursorRequest("CLOSE", "2024-06-01");

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            await _driver.ExecuteAsync(requestOpen);

            _mockDbDataReader.Setup(r => r.DisposeAsync()).Throws(new InvalidOperationException("Unexpected"));

            var response = await _driver.ExecuteAsync(requestClose);

            response.SqlCode.Should().Be(-99999);
            _mockLogger.VerifyLogContains("Unexpected error closing policy cursor.", LogLevel.Error);
        }

        [Fact]
        public async Task DisposeAsync_ShouldDisposePolicyCursorReader_AndCloseConnection()
        {
            _mockDbConnection.SetupGet(c => c.State).Returns(ConnectionState.Open);
            _mockDbConnection.Setup(c => c.CloseAsync()).Returns(Task.CompletedTask);

            _mockDbDataReader.Setup(r => r.DisposeAsync()).Returns(ValueTask.CompletedTask);

            // Simulate opening cursor
            var requestOpen = new PolicyCursorRequest("OPEN", "2024-06-01");
            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            await _driver.ExecuteAsync(requestOpen);

            await _driver.DisposeAsync();

            _mockDbDataReader.Verify(r => r.DisposeAsync(), Times.AtLeastOnce);
            _mockDbConnection.Verify(c => c.CloseAsync(), Times.AtLeastOnce);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldHandleBoundaryConditions_ForProcessDate()
        {
            // Test for process date exactly 30 days before expiry (boundary)
            var request = new PolicyCursorRequest("OPEN", DateTime.Today.AddDays(-30).ToString("yyyy-MM-dd"));

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            var response = await _driver.ExecuteAsync(request);

            response.SqlCode.Should().Be(0);
        }

        [Fact]
        public async Task ExecuteAsync_ShouldHandleNullsInPolicyRecordFields()
        {
            var requestOpen = new PolicyCursorRequest("OPEN", "2024-06-01");
            var requestFetch = new PolicyCursorRequest("FETCH", "2024-06-01");

            _mockDbConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockDbConnection.Object.State = ConnectionState.Open);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            var mockParameter = new Mock<DbParameter>();
            _mockDbCommand.Setup(c => c.CreateParameter()).Returns(mockParameter.Object);
            _mockDbCommand.SetupGet(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockDbCommand.Setup(c => c.ExecuteReaderAsync(CommandBehavior.SequentialAccess, It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockDbDataReader.Object);

            await _driver.ExecuteAsync(requestOpen);

            _mockDbDataReader.Setup(r => r.ReadAsync(It.IsAny<CancellationToken>())).ReturnsAsync(true);

            // Setup Get* methods with nulls where allowed
            _mockDbDataReader.Setup(r => r.GetString(It.IsAny<int>())).Returns((string)null);
            _mockDbDataReader.Setup(r => r.GetDateTime(It.IsAny<int>())).Returns(DateTime.MinValue);
            _mockDbDataReader.Setup(r => r.GetBoolean(It.IsAny<int>())).Returns(false);
            _mockDbDataReader.Setup(r => r.GetDecimal(It.IsAny<int>())).Returns(0m);

            var response = await _driver.ExecuteAsync(requestFetch);

            response.SqlCode.Should().Be(0);
            response.PolicyData.Should().NotBeNull();
            response.PolicyData.PolicyHolderFirstName.Should().BeNull();
            response.PolicyData.PolicyHolderDateOfBirth.Should().Be(DateTime.MinValue);
        }

        // Helper to setup mock DbDataReader for a valid PolicyRecord
        private void SetupMockPolicyRecord(Mock<DbDataReader> reader)
        {
            reader.Setup(r => r.GetString(0)).Returns("PN123456");
            reader.Setup(r => r.GetString(1)).Returns("John");
            reader.Setup(r => r.GetString(2)).Returns("M");
            reader.Setup(r => r.GetString(3)).Returns("Doe");
            reader.Setup(r => r.GetString(4)).Returns("Jane Doe");
            reader.Setup(r => r.GetString(5)).Returns("Spouse");
            reader.Setup(r => r.GetString(6)).Returns("123 Main St");
            reader.Setup(r => r.GetString(7)).Returns("Apt 4B");
            reader.Setup(r => r.GetString(8)).Returns("Los Angeles");
            reader.Setup(r => r.GetString(9)).Returns("CA");
            reader.Setup(r => r.GetString(10)).Returns("90001");
            reader.Setup(r => r.GetDateTime(11)).Returns(new DateTime(1980, 1, 1));
            reader.Setup(r => r.GetString(12)).Returns("M");
            reader.Setup(r => r.GetString(13)).Returns("555-1234");
            reader.Setup(r => r.GetString(14)).Returns("john.doe@email.com");
            reader.Setup(r => r.GetString(15)).Returns("Monthly");
            reader.Setup(r => r.GetString(16)).Returns("Credit Card");
            reader.Setup(r => r.GetString(17)).Returns("UnderwriterX");
            reader.Setup(r => r.GetString(18)).Returns("Standard Terms");
            reader.Setup(r => r.GetBoolean(19)).Returns(true);
            reader.Setup(r => r.GetString(20)).Returns("DISC10");
            reader.Setup(r => r.GetDecimal(21)).Returns(1200.50m);
            reader.Setup(r => r.GetDecimal(22)).Returns(50000.00m);
            reader.Setup(r => r.GetString(23)).Returns("HEALTH");
            reader.Setup(r => r.GetDateTime(24)).Returns(new DateTime(2024, 6, 1));
            reader.Setup(r => r.GetDateTime(25)).Returns(new DateTime(2025, 6, 1));
            reader.Setup(r => r.GetString(26)).Returns("A");
            reader.Setup(r => r.GetString(27)).Returns("AGT123");
            reader.Setup(r => r.GetBoolean(28)).Returns(false);
            reader.Setup(r => r.GetDateTime(29)).Returns(DateTime.Now.AddMonths(-1));
            reader.Setup(r => r.GetDateTime(30)).Returns(DateTime.Now);
        }

        // Helper to create a DbException with a specific error code
        private DbException CreateDbException(int errorCode)
        {
            var mockDbException = new Mock<DbException>();
            mockDbException.SetupGet(e => e.ErrorCode).Returns(errorCode);
            return mockDbException.Object;
        }
    }

    // Extension method for verifying logger calls
    public static class LoggerExtensions
    {
        public static void VerifyLogContains(this Mock<ILogger<PolicyCursorDriver>> logger, string message, LogLevel level)
        {
            logger.Verify(
                x => x.Log(
                    level,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains(message)),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.AtLeastOnce);
        }
    }
}