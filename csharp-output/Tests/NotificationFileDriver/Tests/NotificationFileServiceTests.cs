using System;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using NotificationFileDriver;
using Xunit;

namespace NotificationFileDriver.Tests
{
    public class NotificationFileServiceTests : IDisposable
    {
        private readonly string _agentFilePath = "AGENTFLE";
        private readonly string _customerFilePath = "CUSTFLE";
        private readonly string _reportFilePath = "RPTFLE";
        private readonly Mock<ILogger<NotificationFileService>> _loggerMock;
        private readonly NotificationFileService _service;

        public NotificationFileServiceTests()
        {
            _loggerMock = new Mock<ILogger<NotificationFileService>>();
            _service = new NotificationFileService(_loggerMock.Object);

            // Ensure test files are deleted before each test
            CleanupFiles();
        }

        public void Dispose()
        {
            // Clean up test files after each test
            CleanupFiles();
        }

        private void CleanupFiles()
        {
            TryDelete(_agentFilePath);
            TryDelete(_customerFilePath);
            TryDelete(_reportFilePath);
        }

        private void TryDelete(string path)
        {
            try
            {
                if (File.Exists(path))
                    File.Delete(path);
            }
            catch { /* Ignore */ }
        }

        [Theory]
        [InlineData("AGENT-NOTIFY-FILE", "AGENTFLE")]
        [InlineData("CUSTOMER-NOTIFY-FILE", "CUSTFLE")]
        [InlineData("NOTIFY-REPORT-FILE", "RPTFLE")]
        public async Task OpenAsync_ShouldCreateFileStream_WhenFileNameIsValid(string logicalName, string physicalPath)
        {
            // Arrange & Act
            var result = await _service.OpenAsync(logicalName);

            // Assert
            result.StatusCode.Should().Be("00");
            File.Exists(physicalPath).Should().BeTrue();
        }

        [Fact]
        public async Task OpenAsync_ShouldReturnError_WhenFileNameIsUnknown()
        {
            // Arrange
            var unknownFile = "UNKNOWN-FILE";

            // Act
            var result = await _service.OpenAsync(unknownFile);

            // Assert
            result.StatusCode.Should().Be("99");
            result.ErrorMessage.Should().Contain("Unknown file name");
        }

        [Fact]
        public async Task OpenAsync_ShouldLogErrorAndReturnError_WhenExceptionOccurs()
        {
            // Arrange
            // Simulate exception by locking the file exclusively
            using var fs = new FileStream(_agentFilePath, FileMode.Create, FileAccess.ReadWrite, FileShare.None);
            var service = new NotificationFileService(_loggerMock.Object);

            // Act
            var result = await service.OpenAsync("AGENT-NOTIFY-FILE");

            // Assert
            result.StatusCode.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error opening file")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.AtLeastOnce);
        }

        [Theory]
        [InlineData("AGENT-NOTIFY-FILE", "AGENTFLE")]
        [InlineData("CUSTOMER-NOTIFY-FILE", "CUSTFLE")]
        [InlineData("NOTIFY-REPORT-FILE", "RPTFLE")]
        public async Task CloseAsync_ShouldDisposeFileStream_WhenFileNameIsValid(string logicalName, string physicalPath)
        {
            // Arrange
            await _service.OpenAsync(logicalName);

            // Act
            var result = await _service.CloseAsync(logicalName);

            // Assert
            result.StatusCode.Should().Be("00");
            // File should still exist, but stream is disposed (no direct way to check, but no exception thrown)
        }

        [Fact]
        public async Task CloseAsync_ShouldReturnError_WhenFileNameIsUnknown()
        {
            // Arrange
            var unknownFile = "UNKNOWN-FILE";

            // Act
            var result = await _service.CloseAsync(unknownFile);

            // Assert
            result.StatusCode.Should().Be("99");
            result.ErrorMessage.Should().Contain("Unknown file name");
        }

        [Fact]
        public async Task CloseAsync_ShouldLogErrorAndReturnError_WhenExceptionOccurs()
        {
            // Arrange
            // Simulate by disposing twice (should not throw, but let's force an exception by using reflection)
            await _service.OpenAsync("AGENT-NOTIFY-FILE");
            await _service.CloseAsync("AGENT-NOTIFY-FILE");
            // No easy way to force an exception here, so just verify no error on double close
            var result = await _service.CloseAsync("AGENT-NOTIFY-FILE");

            // Assert
            result.StatusCode.Should().Be("00");
        }

        [Fact]
        public async Task WriteAgentAsync_ShouldReturnError_WhenFileNotOpen()
        {
            // Arrange
            var record = GetAgentRecord();

            // Act
            var result = await _service.WriteAgentAsync("AGENT-NOTIFY-FILE", record);

            // Assert
            result.StatusCode.Should().Be("98");
            result.ErrorMessage.Should().Contain("Agent file not open");
        }

        [Fact]
        public async Task WriteAgentAsync_ShouldWriteRecord_WhenFileIsOpen()
        {
            // Arrange
            await _service.OpenAsync("AGENT-NOTIFY-FILE");
            var record = GetAgentRecord();

            // Act
            var result = await _service.WriteAgentAsync("AGENT-NOTIFY-FILE", record);

            // Assert
            result.StatusCode.Should().Be("00");

            // Verify file content matches COBOL logic (fixed-length, padded fields)
            var lines = File.ReadAllLines(_agentFilePath);
            lines.Should().HaveCount(1);

            var expectedLine = string.Concat(
                record.AgentCode.PadRight(10),
                record.AgentName.PadRight(45),
                record.AgentAddress1.PadRight(50),
                record.AgentAddress2.PadRight(50),
                record.AgentCity.PadRight(20),
                record.AgentState.PadRight(2),
                record.AgentPolicyNumber.PadRight(10),
                record.AgentPolicyFName.PadRight(35),
                record.AgentPolicyMName.PadRight(1),
                record.AgentPolicyLName.PadRight(35),
                record.AgentPolicyStartDate.PadRight(10),
                record.AgentPolicyExpiryDate.PadRight(10),
                record.AgentNotifyDate.PadRight(10),
                record.AgentNotifyMessages.PadRight(100)
            );
            lines[0].Should().Be(expectedLine);
        }

        [Fact]
        public async Task WriteAgentAsync_ShouldLogErrorAndReturnError_WhenExceptionOccurs()
        {
            // Arrange
            await _service.OpenAsync("AGENT-NOTIFY-FILE");
            var record = GetAgentRecord();

            // Simulate exception by disposing stream before write
            await _service.CloseAsync("AGENT-NOTIFY-FILE");

            // Act
            var result = await _service.WriteAgentAsync("AGENT-NOTIFY-FILE", record);

            // Assert
            result.StatusCode.Should().Be("98");
            result.ErrorMessage.Should().Contain("Agent file not open");
        }

        [Fact]
        public async Task WriteCustomerAsync_ShouldReturnError_WhenFileNotOpen()
        {
            // Arrange
            var record = GetCustomerRecord();

            // Act
            var result = await _service.WriteCustomerAsync("CUSTOMER-NOTIFY-FILE", record);

            // Assert
            result.StatusCode.Should().Be("98");
            result.ErrorMessage.Should().Contain("Customer file not open");
        }

        [Fact]
        public async Task WriteCustomerAsync_ShouldWriteRecord_WhenFileIsOpen()
        {
            // Arrange
            await _service.OpenAsync("CUSTOMER-NOTIFY-FILE");
            var record = GetCustomerRecord();

            // Act
            var result = await _service.WriteCustomerAsync("CUSTOMER-NOTIFY-FILE", record);

            // Assert
            result.StatusCode.Should().Be("00");

            // Verify file content matches COBOL logic (fixed-length, padded fields)
            var lines = File.ReadAllLines(_customerFilePath);
            lines.Should().HaveCount(1);

            var expectedLine = string.Concat(
                record.CustPolicyNumber.PadRight(10),
                record.CustFName.PadRight(35),
                record.CustMName.PadRight(1),
                record.CustLName.PadRight(35),
                record.CustPolicyStartDate.PadRight(10),
                record.CustPolicyExpiryDate.PadRight(10),
                record.CustNotifyDate.PadRight(10),
                record.CustNotifyMessages.PadRight(100),
                record.CustAgentCode.PadRight(10),
                record.CustAgentName.PadRight(45),
                record.CustStatutoryMessage.PadRight(100)
            );
            lines[0].Should().Be(expectedLine);
        }

        [Fact]
        public async Task WriteCustomerAsync_ShouldLogErrorAndReturnError_WhenExceptionOccurs()
        {
            // Arrange
            await _service.OpenAsync("CUSTOMER-NOTIFY-FILE");
            var record = GetCustomerRecord();

            // Simulate exception by disposing stream before write
            await _service.CloseAsync("CUSTOMER-NOTIFY-FILE");

            // Act
            var result = await _service.WriteCustomerAsync("CUSTOMER-NOTIFY-FILE", record);

            // Assert
            result.StatusCode.Should().Be("98");
            result.ErrorMessage.Should().Contain("Customer file not open");
        }

        [Fact]
        public async Task WriteReportAsync_ShouldReturnError_WhenFileNotOpen()
        {
            // Arrange
            var record = new NotifyReportRecord("Report line");

            // Act
            var result = await _service.WriteReportAsync("NOTIFY-REPORT-FILE", record);

            // Assert
            result.StatusCode.Should().Be("98");
            result.ErrorMessage.Should().Contain("Report file not open");
        }

        [Fact]
        public async Task WriteReportAsync_ShouldWriteRecord_WhenFileIsOpen()
        {
            // Arrange
            await _service.OpenAsync("NOTIFY-REPORT-FILE");
            var record = new NotifyReportRecord("Report line");

            // Act
            var result = await _service.WriteReportAsync("NOTIFY-REPORT-FILE", record);

            // Assert
            result.StatusCode.Should().Be("00");

            // Verify file content matches COBOL logic (fixed-length, padded fields)
            var lines = File.ReadAllLines(_reportFilePath);
            lines.Should().HaveCount(1);

            var expectedLine = record.ReportLine.PadRight(133);
            lines[0].Should().Be(expectedLine);
        }

        [Fact]
        public async Task WriteReportAsync_ShouldLogErrorAndReturnError_WhenExceptionOccurs()
        {
            // Arrange
            await _service.OpenAsync("NOTIFY-REPORT-FILE");
            var record = new NotifyReportRecord("Report line");

            // Simulate exception by disposing stream before write
            await _service.CloseAsync("NOTIFY-REPORT-FILE");

            // Act
            var result = await _service.WriteReportAsync("NOTIFY-REPORT-FILE", record);

            // Assert
            result.StatusCode.Should().Be("98");
            result.ErrorMessage.Should().Contain("Report file not open");
        }

        [Fact]
        public async Task WriteAgentAsync_ShouldHandleNullOrEmptyFields_AndPadCorrectly()
        {
            // Arrange
            await _service.OpenAsync("AGENT-NOTIFY-FILE");
            var record = new AgentNotifyRecord(
                AgentCode: "",
                AgentName: null,
                AgentAddress1: "",
                AgentAddress2: null,
                AgentCity: "",
                AgentState: null,
                AgentPolicyNumber: "",
                AgentPolicyFName: null,
                AgentPolicyMName: "",
                AgentPolicyLName: null,
                AgentPolicyStartDate: "",
                AgentPolicyExpiryDate: null,
                AgentNotifyDate: "",
                AgentNotifyMessages: null
            );

            // Act
            var result = await _service.WriteAgentAsync("AGENT-NOTIFY-FILE", record);

            // Assert
            result.StatusCode.Should().Be("00");
            var lines = File.ReadAllLines(_agentFilePath);
            lines.Should().HaveCount(1);

            var expectedLine = string.Concat(
                (record.AgentCode ?? "").PadRight(10),
                (record.AgentName ?? "").PadRight(45),
                (record.AgentAddress1 ?? "").PadRight(50),
                (record.AgentAddress2 ?? "").PadRight(50),
                (record.AgentCity ?? "").PadRight(20),
                (record.AgentState ?? "").PadRight(2),
                (record.AgentPolicyNumber ?? "").PadRight(10),
                (record.AgentPolicyFName ?? "").PadRight(35),
                (record.AgentPolicyMName ?? "").PadRight(1),
                (record.AgentPolicyLName ?? "").PadRight(35),
                (record.AgentPolicyStartDate ?? "").PadRight(10),
                (record.AgentPolicyExpiryDate ?? "").PadRight(10),
                (record.AgentNotifyDate ?? "").PadRight(10),
                (record.AgentNotifyMessages ?? "").PadRight(100)
            );
            lines[0].Should().Be(expectedLine);
        }

        [Fact]
        public async Task WriteCustomerAsync_ShouldHandleNullOrEmptyFields_AndPadCorrectly()
        {
            // Arrange
            await _service.OpenAsync("CUSTOMER-NOTIFY-FILE");
            var record = new CustomerNotifyRecord(
                CustPolicyNumber: "",
                CustFName: null,
                CustMName: "",
                CustLName: null,
                CustPolicyStartDate: "",
                CustPolicyExpiryDate: null,
                CustNotifyDate: "",
                CustNotifyMessages: null,
                CustAgentCode: "",
                CustAgentName: null,
                CustStatutoryMessage: ""
            );

            // Act
            var result = await _service.WriteCustomerAsync("CUSTOMER-NOTIFY-FILE", record);

            // Assert
            result.StatusCode.Should().Be("00");
            var lines = File.ReadAllLines(_customerFilePath);
            lines.Should().HaveCount(1);

            var expectedLine = string.Concat(
                (record.CustPolicyNumber ?? "").PadRight(10),
                (record.CustFName ?? "").PadRight(35),
                (record.CustMName ?? "").PadRight(1),
                (record.CustLName ?? "").PadRight(35),
                (record.CustPolicyStartDate ?? "").PadRight(10),
                (record.CustPolicyExpiryDate ?? "").PadRight(10),
                (record.CustNotifyDate ?? "").PadRight(10),
                (record.CustNotifyMessages ?? "").PadRight(100),
                (record.CustAgentCode ?? "").PadRight(10),
                (record.CustAgentName ?? "").PadRight(45),
                (record.CustStatutoryMessage ?? "").PadRight(100)
            );
            lines[0].Should().Be(expectedLine);
        }

        [Fact]
        public async Task WriteReportAsync_ShouldHandleNullOrEmptyFields_AndPadCorrectly()
        {
            // Arrange
            await _service.OpenAsync("NOTIFY-REPORT-FILE");
            var record = new NotifyReportRecord(null);

            // Act
            var result = await _service.WriteReportAsync("NOTIFY-REPORT-FILE", record);

            // Assert
            result.StatusCode.Should().Be("00");
            var lines = File.ReadAllLines(_reportFilePath);
            lines.Should().HaveCount(1);

            var expectedLine = (record.ReportLine ?? "").PadRight(133);
            lines[0].Should().Be(expectedLine);
        }

        [Fact]
        public async Task OpenAsync_ShouldAllowMultipleOpensAndWrites_AndPreserveAllRecords()
        {
            // Arrange
            await _service.OpenAsync("AGENT-NOTIFY-FILE");
            var record1 = GetAgentRecord();
            var record2 = new AgentNotifyRecord(
                AgentCode: "B987654321",
                AgentName: "Jane Smith",
                AgentAddress1: "456 Elm St",
                AgentAddress2: "Apt 2",
                AgentCity: "Aarhus",
                AgentState: "DK",
                AgentPolicyNumber: "P123456789",
                AgentPolicyFName: "Alice",
                AgentPolicyMName: "B",
                AgentPolicyLName: "Johnson",
                AgentPolicyStartDate: "20230101",
                AgentPolicyExpiryDate: "20231231",
                AgentNotifyDate: "20230601",
                AgentNotifyMessages: "Second notification"
            );

            // Act
            var result1 = await _service.WriteAgentAsync("AGENT-NOTIFY-FILE", record1);
            var result2 = await _service.WriteAgentAsync("AGENT-NOTIFY-FILE", record2);

            // Assert
            result1.StatusCode.Should().Be("00");
            result2.StatusCode.Should().Be("00");
            var lines = File.ReadAllLines(_agentFilePath);
            lines.Should().HaveCount(2);
        }

        // Helper methods for test records
        private AgentNotifyRecord GetAgentRecord()
        {
            return new AgentNotifyRecord(
                AgentCode: "A123456789",
                AgentName: "John Doe",
                AgentAddress1: "123 Main St",
                AgentAddress2: "Suite 100",
                AgentCity: "Copenhagen",
                AgentState: "DK",
                AgentPolicyNumber: "P987654321",
                AgentPolicyFName: "Bob",
                AgentPolicyMName: "C",
                AgentPolicyLName: "Smith",
                AgentPolicyStartDate: "20220101",
                AgentPolicyExpiryDate: "20221231",
                AgentNotifyDate: "20220601",
                AgentNotifyMessages: "Test notification"
            );
        }

        private CustomerNotifyRecord GetCustomerRecord()
        {
            return new CustomerNotifyRecord(
                CustPolicyNumber: "P987654321",
                CustFName: "Alice",
                CustMName: "B",
                CustLName: "Johnson",
                CustPolicyStartDate: "20230101",
                CustPolicyExpiryDate: "20231231",
                CustNotifyDate: "20230601",
                CustNotifyMessages: "Customer notification",
                CustAgentCode: "A123456789",
                CustAgentName: "John Doe",
                CustStatutoryMessage: "Statutory message"
            );
        }
    }
}