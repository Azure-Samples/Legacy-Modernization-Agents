using System;
using System.Data;
using System.Data.Common;
using System.Threading;
using System.Threading.Tasks;
using FluentAssertions;
using Insurance.Tracking;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace Insurance.Tracking.Tests
{
    public class TrackingRepositoryTests : IDisposable
    {
        private readonly Mock<DbConnection> _mockConnection;
        private readonly Mock<DbCommand> _mockCommand;
        private readonly Mock<DbDataReader> _mockReader;
        private readonly Mock<ILogger<TrackingRepository>> _mockLogger;
        private readonly TrackingRepository _repository;

        public TrackingRepositoryTests()
        {
            _mockConnection = new Mock<DbConnection>();
            _mockCommand = new Mock<DbCommand>();
            _mockReader = new Mock<DbDataReader>();
            _mockLogger = new Mock<ILogger<TrackingRepository>>();

            // Setup DbConnection to return our mock command
            _mockConnection.Setup(c => c.CreateCommand()).Returns(_mockCommand.Object);

            // Setup default connection state
            _mockConnection.SetupProperty(c => c.State, ConnectionState.Closed);

            _repository = new TrackingRepository(_mockConnection.Object, _mockLogger.Object);
        }

        public void Dispose()
        {
            // Cleanup if needed
        }

        #region GetTrackingRecordAsync Tests

        [Fact]
        public async Task GetTrackingRecordAsync_ShouldReturnTrackingRecord_WhenRecordExists()
        {
            // Arrange
            var policyNumber = "PN123";
            var notifyDate = "20240601";
            var status = "A";
            var addTimestamp = new DateTime(2024, 6, 1, 12, 0, 0);
            var updateTimestamp = new DateTime(2024, 6, 2, 13, 0, 0);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockReader.Object);

            _mockReader.SetupSequence(r => r.ReadAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(true)
                .ReturnsAsync(false);

            _mockReader.Setup(r => r.GetString(0)).Returns(policyNumber);
            _mockReader.Setup(r => r.GetString(1)).Returns(notifyDate);
            _mockReader.Setup(r => r.GetString(2)).Returns(status);
            _mockReader.Setup(r => r.GetDateTime(3)).Returns(addTimestamp);
            _mockReader.Setup(r => r.GetDateTime(4)).Returns(updateTimestamp);

            // Act
            var result = await _repository.GetTrackingRecordAsync(policyNumber);

            // Assert
            result.Should().NotBeNull();
            result.PolicyNumber.Should().Be(policyNumber);
            result.NotifyDate.Should().Be(notifyDate);
            result.Status.Should().Be(status);
            result.AddTimestamp.Should().Be(addTimestamp);
            result.UpdateTimestamp.Should().Be(updateTimestamp);
        }

        [Fact]
        public async Task GetTrackingRecordAsync_ShouldReturnNull_WhenRecordDoesNotExist()
        {
            // Arrange
            var policyNumber = "PN999";

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockReader.Object);

            _mockReader.Setup(r => r.ReadAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(false);

            // Act
            var result = await _repository.GetTrackingRecordAsync(policyNumber);

            // Assert
            result.Should().BeNull();
        }

        [Fact]
        public async Task GetTrackingRecordAsync_ShouldThrowDataException_OnDbException()
        {
            // Arrange
            var policyNumber = "PN123";
            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<CancellationToken>()))
                .ThrowsAsync(CreateDbException(-123));

            // Act
            Func<Task> act = async () => await _repository.GetTrackingRecordAsync(policyNumber);

            // Assert
            await act.Should().ThrowAsync<DataException>()
                .WithMessage("Error selecting TTRACKING record.*");
            _mockLogger.Verify(
                l => l.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error selecting TTRACKING")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        public async Task GetTrackingRecordAsync_ShouldHandleNullOrEmptyPolicyNumber(string policyNumber)
        {
            // Arrange
            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockReader.Object);

            _mockReader.Setup(r => r.ReadAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(false);

            // Act
            var result = await _repository.GetTrackingRecordAsync(policyNumber);

            // Assert
            result.Should().BeNull();
        }

        #endregion

        #region InsertTrackingRecordAsync Tests

        [Fact]
        public async Task InsertTrackingRecordAsync_ShouldReturnZero_WhenInsertSucceeds()
        {
            // Arrange
            var record = new TrackingRecord("PN123", "20240601", "A", DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(1);

            // Act
            var result = await _repository.InsertTrackingRecordAsync(record);

            // Assert
            result.Should().Be(0);
        }

        [Fact]
        public async Task InsertTrackingRecordAsync_ShouldReturnMinusOne_WhenNoRowsAffected()
        {
            // Arrange
            var record = new TrackingRecord("PN123", "20240601", "A", DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(0);

            // Act
            var result = await _repository.InsertTrackingRecordAsync(record);

            // Assert
            result.Should().Be(-1);
        }

        [Fact]
        public async Task InsertTrackingRecordAsync_ShouldReturnErrorCode_OnDbExceptionWithErrorCode()
        {
            // Arrange
            var record = new TrackingRecord("PN123", "20240601", "A", DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            var dbEx = CreateDbException(-222);
            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ThrowsAsync(dbEx);

            // Act
            var result = await _repository.InsertTrackingRecordAsync(record);

            // Assert
            result.Should().Be(-222);
            _mockLogger.Verify(
                l => l.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error inserting into TTRACKING")),
                    dbEx,
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task InsertTrackingRecordAsync_ShouldReturnMinusOne_OnDbExceptionWithZeroErrorCode()
        {
            // Arrange
            var record = new TrackingRecord("PN123", "20240601", "A", DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            var dbEx = CreateDbException(0);
            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ThrowsAsync(dbEx);

            // Act
            var result = await _repository.InsertTrackingRecordAsync(record);

            // Assert
            result.Should().Be(-1);
            _mockLogger.Verify(
                l => l.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error inserting into TTRACKING")),
                    dbEx,
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task InsertTrackingRecordAsync_ShouldHandleNullFields()
        {
            // Arrange
            var record = new TrackingRecord(null, null, null, DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(1);

            // Act
            var result = await _repository.InsertTrackingRecordAsync(record);

            // Assert
            result.Should().Be(0);
        }

        #endregion

        #region UpdateTrackingRecordAsync Tests

        [Fact]
        public async Task UpdateTrackingRecordAsync_ShouldReturnZero_WhenUpdateSucceeds()
        {
            // Arrange
            var record = new TrackingRecord("PN123", "20240601", "A", DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(1);

            // Act
            var result = await _repository.UpdateTrackingRecordAsync(record);

            // Assert
            result.Should().Be(0);
        }

        [Fact]
        public async Task UpdateTrackingRecordAsync_ShouldReturnMinusOne_WhenNoRowsAffected()
        {
            // Arrange
            var record = new TrackingRecord("PN123", "20240601", "A", DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(0);

            // Act
            var result = await _repository.UpdateTrackingRecordAsync(record);

            // Assert
            result.Should().Be(-1);
        }

        [Fact]
        public async Task UpdateTrackingRecordAsync_ShouldReturnErrorCode_OnDbExceptionWithErrorCode()
        {
            // Arrange
            var record = new TrackingRecord("PN123", "20240601", "A", DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            var dbEx = CreateDbException(-333);
            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ThrowsAsync(dbEx);

            // Act
            var result = await _repository.UpdateTrackingRecordAsync(record);

            // Assert
            result.Should().Be(-333);
            _mockLogger.Verify(
                l => l.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error updating TTRACKING")),
                    dbEx,
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task UpdateTrackingRecordAsync_ShouldReturnMinusOne_OnDbExceptionWithZeroErrorCode()
        {
            // Arrange
            var record = new TrackingRecord("PN123", "20240601", "A", DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            var dbEx = CreateDbException(0);
            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ThrowsAsync(dbEx);

            // Act
            var result = await _repository.UpdateTrackingRecordAsync(record);

            // Assert
            result.Should().Be(-1);
            _mockLogger.Verify(
                l => l.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error updating TTRACKING")),
                    dbEx,
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task UpdateTrackingRecordAsync_ShouldHandleNullFields()
        {
            // Arrange
            var record = new TrackingRecord(null, null, null, DateTime.UtcNow, DateTime.UtcNow);

            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);

            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.Setup(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(1);

            // Act
            var result = await _repository.UpdateTrackingRecordAsync(record);

            // Assert
            result.Should().Be(0);
        }

        #endregion

        #region Integration Tests (Simulated)

        // Simulate integration test for database operation using in-memory objects
        [Fact]
        public async Task Integration_InsertAndGetTrackingRecord_ShouldPreserveBusinessLogic()
        {
            // Arrange
            var policyNumber = "PN456";
            var notifyDate = "20240615";
            var status = "A";
            var addTimestamp = DateTime.UtcNow;
            var updateTimestamp = DateTime.UtcNow;
            var record = new TrackingRecord(policyNumber, notifyDate, status, addTimestamp, updateTimestamp);

            // Setup for Insert
            _mockCommand.SetupProperty(c => c.CommandText);
            _mockCommand.SetupProperty(c => c.CommandType);

            var mockParameterCollection = new Mock<DbParameterCollection>();
            _mockCommand.Setup(c => c.Parameters).Returns(mockParameterCollection.Object);
            _mockCommand.Setup(c => c.CreateParameter()).Returns(new Mock<DbParameter>().Object);

            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<CancellationToken>()))
                .Returns(Task.CompletedTask)
                .Callback(() => _mockConnection.Object.State = ConnectionState.Open);

            _mockCommand.SetupSequence(c => c.ExecuteNonQueryAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(1); // Insert succeeds

            // Act
            var insertResult = await _repository.InsertTrackingRecordAsync(record);

            // Setup for Get
            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(_mockReader.Object);

            _mockReader.SetupSequence(r => r.ReadAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(true)
                .ReturnsAsync(false);

            _mockReader.Setup(r => r.GetString(0)).Returns(policyNumber);
            _mockReader.Setup(r => r.GetString(1)).Returns(notifyDate);
            _mockReader.Setup(r => r.GetString(2)).Returns(status);
            _mockReader.Setup(r => r.GetDateTime(3)).Returns(addTimestamp);
            _mockReader.Setup(r => r.GetDateTime(4)).Returns(updateTimestamp);

            var getResult = await _repository.GetTrackingRecordAsync(policyNumber);

            // Assert
            insertResult.Should().Be(0);
            getResult.Should().NotBeNull();
            getResult.PolicyNumber.Should().Be(policyNumber);
            getResult.NotifyDate.Should().Be(notifyDate);
            getResult.Status.Should().Be(status);
        }

        #endregion

        #region Helper

        // Helper to create a DbException with a specific ErrorCode
        private DbException CreateDbException(int errorCode)
        {
            var mockDbException = new Mock<DbException>();
            mockDbException.SetupGet(e => e.ErrorCode).Returns(errorCode);
            return mockDbException.Object;
        }

        #endregion
    }
}