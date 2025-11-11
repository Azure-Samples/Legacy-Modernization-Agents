using System;
using System.Data;
using System.Data.Common;
using System.Threading.Tasks;
using FluentAssertions;
using Insurance.Tracking;
using Moq;
using Xunit;
using Microsoft.Extensions.Logging;

namespace Insurance.Tracking.Tests
{
    public class TrackingRecordTests : IDisposable
    {
        private readonly Mock<ITrackingRepository> _repositoryMock;
        private readonly Mock<ILogger<TrackingService>> _loggerMock;
        private readonly TrackingService _service;

        public TrackingRecordTests()
        {
            _repositoryMock = new Mock<ITrackingRepository>(MockBehavior.Strict);
            _loggerMock = new Mock<ILogger<TrackingService>>(MockBehavior.Loose);
            _service = new TrackingService(_repositoryMock.Object, _loggerMock.Object);
        }

        public void Dispose()
        {
            // Cleanup if needed
        }

        #region TrackingService.ProcessTrackingOperationAsync

        [Fact]
        public async Task ProcessTrackingOperationAsync_ShouldThrowArgumentNullException_WhenInputIsNull()
        {
            Func<Task> act = async () => await _service.ProcessTrackingOperationAsync(null);
            await act.Should().ThrowAsync<ArgumentNullException>();
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        [InlineData("DELETE")]
        [InlineData("INVALID")]
        public async Task ProcessTrackingOperationAsync_ShouldReturnMinusOne_WhenOperationTypeIsInvalid(string operationType)
        {
            var input = new TrackingOperationInput(operationType, "20240601", "PN123");
            var result = await _service.ProcessTrackingOperationAsync(input);
            result.SqlCode.Should().Be(-1);
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Invalid operation type")),
                    null,
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ProcessTrackingOperationAsync_ShouldInsert_WhenNoExistingRecord()
        {
            var input = new TrackingOperationInput("INSERT", "20240601", "PN456");
            _repositoryMock.Setup(r => r.GetTrackingRecordAsync("PN456"))
                .ReturnsAsync((TrackingRecord)null);
            _repositoryMock.Setup(r => r.InsertTrackingRecordAsync(It.Is<TrackingRecord>(tr =>
                tr.PolicyNumber == "PN456" &&
                tr.NotifyDate == "20240601" &&
                tr.Status == "A")))
                .ReturnsAsync(0);

            var result = await _service.ProcessTrackingOperationAsync(input);

            result.SqlCode.Should().Be(0);
            _repositoryMock.Verify(r => r.GetTrackingRecordAsync("PN456"), Times.Once);
            _repositoryMock.Verify(r => r.InsertTrackingRecordAsync(It.IsAny<TrackingRecord>()), Times.Once);
            _repositoryMock.Verify(r => r.UpdateTrackingRecordAsync(It.IsAny<TrackingRecord>()), Times.Never);
        }

        [Fact]
        public async Task ProcessTrackingOperationAsync_ShouldUpdate_WhenExistingRecordFound()
        {
            var input = new TrackingOperationInput("UPDATE", "20240602", "PN789");
            var existingRecord = new TrackingRecord
            {
                PolicyNumber = "PN789",
                NotifyDate = "20240601",
                Status = "A",
                AddTimestamp = DateTime.UtcNow.AddDays(-1),
                UpdateTimestamp = DateTime.UtcNow.AddDays(-1)
            };
            _repositoryMock.Setup(r => r.GetTrackingRecordAsync("PN789"))
                .ReturnsAsync(existingRecord);
            _repositoryMock.Setup(r => r.UpdateTrackingRecordAsync(It.Is<TrackingRecord>(tr =>
                tr.PolicyNumber == "PN789" &&
                tr.NotifyDate == "20240602" &&
                tr.Status == "A")))
                .ReturnsAsync(0);

            var result = await _service.ProcessTrackingOperationAsync(input);

            result.SqlCode.Should().Be(0);
            _repositoryMock.Verify(r => r.GetTrackingRecordAsync("PN789"), Times.Once);
            _repositoryMock.Verify(r => r.UpdateTrackingRecordAsync(It.IsAny<TrackingRecord>()), Times.Once);
            _repositoryMock.Verify(r => r.InsertTrackingRecordAsync(It.IsAny<TrackingRecord>()), Times.Never);
        }

        [Theory]
        [InlineData("INSERT")]
        [InlineData("UPDATE")]
        public async Task ProcessTrackingOperationAsync_ShouldReturnMinusOne_WhenRepositoryReturnsError(string operationType)
        {
            var input = new TrackingOperationInput(operationType, "20240603", "PN999");
            _repositoryMock.Setup(r => r.GetTrackingRecordAsync("PN999"))
                .ReturnsAsync(operationType == "INSERT" ? null : new TrackingRecord());
            if (operationType == "INSERT")
            {
                _repositoryMock.Setup(r => r.InsertTrackingRecordAsync(It.IsAny<TrackingRecord>()))
                    .ReturnsAsync(-1);
            }
            else
            {
                _repositoryMock.Setup(r => r.UpdateTrackingRecordAsync(It.IsAny<TrackingRecord>()))
                    .ReturnsAsync(-1);
            }

            var result = await _service.ProcessTrackingOperationAsync(input);

            result.SqlCode.Should().Be(-1);
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error")),
                    null,
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.AtLeastOnce);
        }

        [Fact]
        public async Task ProcessTrackingOperationAsync_ShouldReturnMinusOne_WhenExceptionThrown()
        {
            var input = new TrackingOperationInput("INSERT", "20240604", "PNERR");
            _repositoryMock.Setup(r => r.GetTrackingRecordAsync("PNERR"))
                .ThrowsAsync(new InvalidOperationException("DB error"));

            var result = await _service.ProcessTrackingOperationAsync(input);

            result.SqlCode.Should().Be(-1);
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.IsAny<It.IsAnyType>(),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Theory]
        [InlineData("insert")]
        [InlineData("INSERT")]
        [InlineData(" Insert ")]
        [InlineData("update")]
        [InlineData("UPDATE")]
        [InlineData(" Update ")]
        public async Task ProcessTrackingOperationAsync_ShouldBeCaseInsensitiveForOperationType(string operationType)
        {
            var input = new TrackingOperationInput(operationType, "20240605", "PNCASE");
            _repositoryMock.Setup(r => r.GetTrackingRecordAsync("PNCASE"))
                .ReturnsAsync((TrackingRecord)null);
            _repositoryMock.Setup(r => r.InsertTrackingRecordAsync(It.IsAny<TrackingRecord>()))
                .ReturnsAsync(0);

            var result = await _service.ProcessTrackingOperationAsync(input);

            result.SqlCode.Should().Be(0);
        }

        [Fact]
        public async Task ProcessTrackingOperationAsync_ShouldSetStatusToA_WhenInsertingOrUpdating()
        {
            var input = new TrackingOperationInput("INSERT", "20240606", "PNSTAT");
            TrackingRecord capturedRecord = null;
            _repositoryMock.Setup(r => r.GetTrackingRecordAsync("PNSTAT"))
                .ReturnsAsync((TrackingRecord)null);
            _repositoryMock.Setup(r => r.InsertTrackingRecordAsync(It.IsAny<TrackingRecord>()))
                .Callback<TrackingRecord>(tr => capturedRecord = tr)
                .ReturnsAsync(0);

            var result = await _service.ProcessTrackingOperationAsync(input);

            result.SqlCode.Should().Be(0);
            capturedRecord.Should().NotBeNull();
            capturedRecord.Status.Should().Be("A");
        }

        [Fact]
        public async Task ProcessTrackingOperationAsync_ShouldPassPolicyNumberAndProcessDateToRepository()
        {
            var input = new TrackingOperationInput("INSERT", "20240607", "PNDATA");
            TrackingRecord capturedRecord = null;
            _repositoryMock.Setup(r => r.GetTrackingRecordAsync("PNDATA"))
                .ReturnsAsync((TrackingRecord)null);
            _repositoryMock.Setup(r => r.InsertTrackingRecordAsync(It.IsAny<TrackingRecord>()))
                .Callback<TrackingRecord>(tr => capturedRecord = tr)
                .ReturnsAsync(0);

            var result = await _service.ProcessTrackingOperationAsync(input);

            capturedRecord.PolicyNumber.Should().Be("PNDATA");
            capturedRecord.NotifyDate.Should().Be("20240607");
        }

        #endregion

        #region TrackingRepository Integration Tests

        // Integration test for TrackingRepository using in-memory SQLite
        // This test requires System.Data.SQLite or Microsoft.Data.Sqlite
        // and is skipped if not available.

        [Fact(Skip = "Integration test requires SQLite. Enable and configure connection to run.")]
        public async Task TrackingRepository_ShouldInsertAndRetrieveRecord()
        {
            // Arrange: Setup in-memory SQLite connection
            var connection = new Microsoft.Data.Sqlite.SqliteConnection("DataSource=:memory:");
            await connection.OpenAsync();

            var createTableCmd = connection.CreateCommand();
            createTableCmd.CommandText = @"
                CREATE TABLE INSURNCE_TTRAKING (
                    TR_POLICY_NUMBER TEXT PRIMARY KEY,
                    TR_NOTIFY_DATE TEXT,
                    TR_STATUS TEXT,
                    TR_ADD_TIMESTAMP TEXT,
                    TR_UPDATE_TIMESTAMP TEXT
                );
            ";
            await createTableCmd.ExecuteNonQueryAsync();

            var repo = new TrackingRepository(connection);

            var record = new TrackingRecord
            {
                PolicyNumber = "PNINT",
                NotifyDate = "20240608",
                Status = "A"
            };

            // Act: Insert record
            var insertCode = await repo.InsertTrackingRecordAsync(record);

            // Assert: Insert should succeed
            insertCode.Should().Be(0);

            // Act: Retrieve record
            var retrieved = await repo.GetTrackingRecordAsync("PNINT");

            // Assert: Retrieved record should match inserted values
            retrieved.Should().NotBeNull();
            retrieved.PolicyNumber.Should().Be("PNINT");
            retrieved.NotifyDate.Should().Be("20240608");
            retrieved.Status.Should().Be("A");
        }

        [Fact(Skip = "Integration test requires SQLite. Enable and configure connection to run.")]
        public async Task TrackingRepository_ShouldUpdateRecord()
        {
            // Arrange: Setup in-memory SQLite connection
            var connection = new Microsoft.Data.Sqlite.SqliteConnection("DataSource=:memory:");
            await connection.OpenAsync();

            var createTableCmd = connection.CreateCommand();
            createTableCmd.CommandText = @"
                CREATE TABLE INSURNCE_TTRAKING (
                    TR_POLICY_NUMBER TEXT PRIMARY KEY,
                    TR_NOTIFY_DATE TEXT,
                    TR_STATUS TEXT,
                    TR_ADD_TIMESTAMP TEXT,
                    TR_UPDATE_TIMESTAMP TEXT
                );
            ";
            await createTableCmd.ExecuteNonQueryAsync();

            var repo = new TrackingRepository(connection);

            var record = new TrackingRecord
            {
                PolicyNumber = "PNUPD",
                NotifyDate = "20240609",
                Status = "A"
            };

            await repo.InsertTrackingRecordAsync(record);

            // Act: Update record
            record.NotifyDate = "20240610";
            record.Status = "B";
            var updateCode = await repo.UpdateTrackingRecordAsync(record);

            // Assert: Update should succeed
            updateCode.Should().Be(0);

            // Act: Retrieve updated record
            var retrieved = await repo.GetTrackingRecordAsync("PNUPD");

            // Assert: Retrieved record should reflect updated values
            retrieved.Should().NotBeNull();
            retrieved.NotifyDate.Should().Be("20240610");
            retrieved.Status.Should().Be("B");
        }

        #endregion

        #region TrackingRecord Data Structure Tests

        [Fact]
        public void TrackingRecord_ShouldInitializePropertiesWithDefaults()
        {
            var record = new TrackingRecord();
            record.PolicyNumber.Should().BeEmpty();
            record.NotifyDate.Should().BeEmpty();
            record.Status.Should().BeEmpty();
            record.AddTimestamp.Should().Be(default);
            record.UpdateTimestamp.Should().Be(default);
        }

        [Fact]
        public void TrackingRecord_ShouldSetPropertiesCorrectly()
        {
            var now = DateTime.UtcNow;
            var record = new TrackingRecord
            {
                PolicyNumber = "PNPROPS",
                NotifyDate = "20240611",
                Status = "A",
                AddTimestamp = now,
                UpdateTimestamp = now
            };

            record.PolicyNumber.Should().Be("PNPROPS");
            record.NotifyDate.Should().Be("20240611");
            record.Status.Should().Be("A");
            record.AddTimestamp.Should().Be(now);
            record.UpdateTimestamp.Should().Be(now);
        }

        #endregion

        #region TrackingOperationInput and TrackingOperationResult Records

        [Fact]
        public void TrackingOperationInput_ShouldSetPropertiesCorrectly()
        {
            var input = new TrackingOperationInput("INSERT", "20240612", "PNINPUT");
            input.OperationType.Should().Be("INSERT");
            input.ProcessDate.Should().Be("20240612");
            input.PolicyNumber.Should().Be("PNINPUT");
        }

        [Fact]
        public void TrackingOperationResult_ShouldSetSqlCodeCorrectly()
        {
            var result = new TrackingOperationResult(123);
            result.SqlCode.Should().Be(123);
        }

        #endregion
    }
}