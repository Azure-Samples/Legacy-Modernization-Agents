using System;
using System.IO;
using System.Threading.Tasks;
using FluentAssertions;
using Moq;
using Xunit;
using Microsoft.Extensions.Logging;
using NotificationFileDriver;

namespace NotificationFileDriver.Tests
{
    public class NotificationFileDriverTests : IDisposable
    {
        private readonly string _agentFilePath = "AGENTFLE";
        private readonly string _customerFilePath = "CUSTFLE";
        private readonly string _reportFilePath = "RPTFLE";
        private readonly Mock<ILogger<NotificationFileDriver.NotificationFileDriver>> _loggerMock;
        private readonly NotificationFileDriver.NotificationFileDriver _driver;

        public NotificationFileDriverTests()
        {
            _loggerMock = new Mock<ILogger<NotificationFileDriver.NotificationFileDriver>>();
            _driver = new NotificationFileDriver.NotificationFileDriver(_loggerMock.Object);

            // Ensure clean test files
            DeleteTestFiles();
        }

        public void Dispose()
        {
            DeleteTestFiles();
        }

        private void DeleteTestFiles()
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
            catch { }
        }

        private AgentNotifyRecord GetAgentRecord() =>
            new AgentNotifyRecord(
                AgentCode: "A123456789",
                AgentName: "John Doe",
                AgentAddress1: "123 Main St",
                AgentAddress2: "Suite 100",
                AgentCity: "Copenhagen",
                AgentState: "DK",
                AgentPolicyNumber: "P987654321",
                AgentPolicyFName: "Jane",
                AgentPolicyMName: "M",
                AgentPolicyLName: "Smith",
                AgentPolicyStartDate: "2024-01-01",
                AgentPolicyExpiryDate: "2025-01-01",
                AgentNotifyDate: "2024-06-01",
                AgentNotifyMessages: "Policy renewal notification."
            );

        private CustomerNotifyRecord GetCustomerRecord() =>
            new CustomerNotifyRecord(
                PolicyNumber: "P987654321",
                FirstName: "Jane",
                MiddleName: "M",
                LastName: "Smith",
                PolicyStartDate: "2024-01-01",
                PolicyExpiryDate: "2025-01-01",
                NotifyDate: "2024-06-01",
                NotifyMessages: "Policy renewal notification.",
                AgentCode: "A123456789",
                AgentName: "John Doe",
                StatutoryMessage: "Statutory info"
            );

        private NotifyReportRecord GetReportRecord() =>
            new NotifyReportRecord(
                ReportLine: "Report line data"
            );

        [Fact]
        public async Task ExecuteAsync_OpenAgentFile_ShouldReturnSuccessCodeAndCreateFile()
        {
            // Arrange
            var fileName = "AGENT-NOTIFY-FILE";
            var operationType = "OPEN";

            // Act
            var result = await _driver.ExecuteAsync(fileName, operationType);

            // Assert
            result.Should().Be("00");
            File.Exists(_agentFilePath).Should().BeTrue();
        }

        [Fact]
        public async Task ExecuteAsync_OpenCustomerFile_ShouldReturnSuccessCodeAndCreateFile()
        {
            var result = await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "OPEN");
            result.Should().Be("00");
            File.Exists(_customerFilePath).Should().BeTrue();
        }

        [Fact]
        public async Task ExecuteAsync_OpenReportFile_ShouldReturnSuccessCodeAndCreateFile()
        {
            var result = await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "OPEN");
            result.Should().Be("00");
            File.Exists(_reportFilePath).Should().BeTrue();
        }

        [Theory]
        [InlineData("AGENT-NOTIFY-FILE", "CLOSE", "AGENTFLE")]
        [InlineData("CUSTOMER-NOTIFY-FILE", "CLOSE", "CUSTFLE")]
        [InlineData("NOTIFY-REPORT-FILE", "CLOSE", "RPTFLE")]
        public async Task ExecuteAsync_CloseFile_ShouldDisposeFileStreamAndReturnSuccess(string fileName, string operationType, string filePath)
        {
            // Arrange
            await _driver.ExecuteAsync(fileName, "OPEN");

            // Act
            var result = await _driver.ExecuteAsync(fileName, operationType);

            // Assert
            result.Should().Be("00");
            // File should still exist, but stream should be disposed (no direct way to check, but no exception on delete)
            Action act = () => File.Delete(filePath);
            act.Should().NotThrow();
        }

        [Fact]
        public async Task ExecuteAsync_WriteAgentRecord_ShouldWriteCorrectDataAndReturnSuccess()
        {
            // Arrange
            var agentRecord = GetAgentRecord();
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");

            // Act
            var result = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: agentRecord);

            // Assert
            result.Should().Be("00");
            var lines = File.ReadAllLines(_agentFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be(string.Join('|',
                agentRecord.AgentCode,
                agentRecord.AgentName,
                agentRecord.AgentAddress1,
                agentRecord.AgentAddress2,
                agentRecord.AgentCity,
                agentRecord.AgentState,
                agentRecord.AgentPolicyNumber,
                agentRecord.AgentPolicyFName,
                agentRecord.AgentPolicyMName,
                agentRecord.AgentPolicyLName,
                agentRecord.AgentPolicyStartDate,
                agentRecord.AgentPolicyExpiryDate,
                agentRecord.AgentNotifyDate,
                agentRecord.AgentNotifyMessages
            ));
        }

        [Fact]
        public async Task ExecuteAsync_WriteCustomerRecord_ShouldWriteCorrectDataAndReturnSuccess()
        {
            var customerRecord = GetCustomerRecord();
            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "OPEN");

            var result = await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "WRITE", customerRecord: customerRecord);

            result.Should().Be("00");
            var lines = File.ReadAllLines(_customerFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be(string.Join('|',
                customerRecord.PolicyNumber,
                customerRecord.FirstName,
                customerRecord.MiddleName,
                customerRecord.LastName,
                customerRecord.PolicyStartDate,
                customerRecord.PolicyExpiryDate,
                customerRecord.NotifyDate,
                customerRecord.NotifyMessages,
                customerRecord.AgentCode,
                customerRecord.AgentName,
                customerRecord.StatutoryMessage
            ));
        }

        [Fact]
        public async Task ExecuteAsync_WriteReportRecord_ShouldWriteCorrectDataAndReturnSuccess()
        {
            var reportRecord = GetReportRecord();
            await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "OPEN");

            var result = await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "WRITE", reportRecord: reportRecord);

            result.Should().Be("00");
            var lines = File.ReadAllLines(_reportFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be(reportRecord.ReportLine);
        }

        [Fact]
        public async Task ExecuteAsync_WriteAgentRecord_WithoutOpen_ShouldReturnErrorCodeAndLog()
        {
            var agentRecord = GetAgentRecord();

            var result = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: agentRecord);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error during WRITE")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_WriteCustomerRecord_WithoutOpen_ShouldReturnErrorCodeAndLog()
        {
            var customerRecord = GetCustomerRecord();

            var result = await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "WRITE", customerRecord: customerRecord);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error during WRITE")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_WriteReportRecord_WithoutOpen_ShouldReturnErrorCodeAndLog()
        {
            var reportRecord = GetReportRecord();

            var result = await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "WRITE", reportRecord: reportRecord);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error during WRITE")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_WriteAgentRecord_NullRecord_ShouldReturnErrorCodeAndLog()
        {
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");

            var result = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: null);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error during WRITE")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_WriteCustomerRecord_NullRecord_ShouldReturnErrorCodeAndLog()
        {
            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "OPEN");

            var result = await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "WRITE", customerRecord: null);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error during WRITE")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_WriteReportRecord_NullRecord_ShouldReturnErrorCodeAndLog()
        {
            await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "OPEN");

            var result = await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "WRITE", reportRecord: null);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error during WRITE")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Theory]
        [InlineData("UNKNOWN-FILE", "OPEN")]
        [InlineData("UNKNOWN-FILE", "WRITE")]
        [InlineData("UNKNOWN-FILE", "CLOSE")]
        public async Task ExecuteAsync_UnknownFileType_ShouldReturnErrorCodeAndLog(string fileName, string operationType)
        {
            var result = await _driver.ExecuteAsync(fileName, operationType);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Unsupported operation type")
                        || v.ToString().Contains("Unknown file type")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.AtLeastOnce);
        }

        [Theory]
        [InlineData("AGENT-NOTIFY-FILE", "INVALIDOP")]
        [InlineData("CUSTOMER-NOTIFY-FILE", "INVALIDOP")]
        [InlineData("NOTIFY-REPORT-FILE", "INVALIDOP")]
        public async Task ExecuteAsync_UnknownOperationType_ShouldReturnErrorCodeAndLog(string fileName, string operationType)
        {
            var result = await _driver.ExecuteAsync(fileName, operationType);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Unsupported operation type")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_OpenFile_WhenFileLocked_ShouldReturnIoErrorCodeAndLog()
        {
            // Arrange: Lock the agent file
            using (var fs = new FileStream(_agentFilePath, FileMode.Create, FileAccess.ReadWrite, FileShare.None))
            {
                // Act
                var result = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");

                // Assert
                result.Should().Be("98");
                _loggerMock.Verify(
                    x => x.Log(
                        LogLevel.Error,
                        It.IsAny<EventId>(),
                        It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("I/O error during OPEN")),
                        It.IsAny<IOException>(),
                        It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                    Times.Once);
            }
        }

        [Fact]
        public async Task ExecuteAsync_WriteAgentRecord_MultipleRecords_ShouldAppendAllRecords()
        {
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            var record1 = GetAgentRecord();
            var record2 = record1 with { AgentCode = "A987654321", AgentName = "Alice Doe" };

            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: record1);
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: record2);

            var lines = File.ReadAllLines(_agentFilePath);
            lines.Should().HaveCount(2);
            lines[0].Should().Be(string.Join('|',
                record1.AgentCode,
                record1.AgentName,
                record1.AgentAddress1,
                record1.AgentAddress2,
                record1.AgentCity,
                record1.AgentState,
                record1.AgentPolicyNumber,
                record1.AgentPolicyFName,
                record1.AgentPolicyMName,
                record1.AgentPolicyLName,
                record1.AgentPolicyStartDate,
                record1.AgentPolicyExpiryDate,
                record1.AgentNotifyDate,
                record1.AgentNotifyMessages
            ));
            lines[1].Should().Be(string.Join('|',
                record2.AgentCode,
                record2.AgentName,
                record2.AgentAddress1,
                record2.AgentAddress2,
                record2.AgentCity,
                record2.AgentState,
                record2.AgentPolicyNumber,
                record2.AgentPolicyFName,
                record2.AgentPolicyMName,
                record2.AgentPolicyLName,
                record2.AgentPolicyStartDate,
                record2.AgentPolicyExpiryDate,
                record2.AgentNotifyDate,
                record2.AgentNotifyMessages
            ));
        }

        [Fact]
        public async Task ExecuteAsync_NullOperationType_ShouldReturnErrorCodeAndLog()
        {
            var result = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", null);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Unsupported operation type")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        [Fact]
        public async Task ExecuteAsync_NullFileName_ShouldReturnErrorCodeAndLog()
        {
            var result = await _driver.ExecuteAsync(null, "OPEN");

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Unknown file type")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.AtLeastOnce);
        }

        [Fact]
        public async Task ExecuteAsync_WriteAgentRecord_EmptyFields_ShouldWritePipeDelimitedEmptyStrings()
        {
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            var emptyRecord = new AgentNotifyRecord(
                AgentCode: "",
                AgentName: "",
                AgentAddress1: "",
                AgentAddress2: "",
                AgentCity: "",
                AgentState: "",
                AgentPolicyNumber: "",
                AgentPolicyFName: "",
                AgentPolicyMName: "",
                AgentPolicyLName: "",
                AgentPolicyStartDate: "",
                AgentPolicyExpiryDate: "",
                AgentNotifyDate: "",
                AgentNotifyMessages: ""
            );

            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: emptyRecord);

            var lines = File.ReadAllLines(_agentFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be("||||||||||||||");
        }

        [Fact]
        public async Task ExecuteAsync_WriteCustomerRecord_EmptyFields_ShouldWritePipeDelimitedEmptyStrings()
        {
            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "OPEN");
            var emptyRecord = new CustomerNotifyRecord(
                PolicyNumber: "",
                FirstName: "",
                MiddleName: "",
                LastName: "",
                PolicyStartDate: "",
                PolicyExpiryDate: "",
                NotifyDate: "",
                NotifyMessages: "",
                AgentCode: "",
                AgentName: "",
                StatutoryMessage: ""
            );

            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "WRITE", customerRecord: emptyRecord);

            var lines = File.ReadAllLines(_customerFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be("||||||||||");
        }

        [Fact]
        public async Task ExecuteAsync_WriteReportRecord_EmptyLine_ShouldWriteEmptyLine()
        {
            await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "OPEN");
            var emptyRecord = new NotifyReportRecord("");

            await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "WRITE", reportRecord: emptyRecord);

            var lines = File.ReadAllLines(_reportFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().BeEmpty();
        }

        [Fact]
        public async Task ExecuteAsync_CloseFile_WithoutOpen_ShouldReturnSuccess()
        {
            var result = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "CLOSE");
            result.Should().Be("00");
        }

        [Fact]
        public async Task ExecuteAsync_OpenFile_Twice_ShouldNotThrowAndReturnSuccess()
        {
            var result1 = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            var result2 = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");

            result1.Should().Be("00");
            result2.Should().Be("00");
        }

        [Fact]
        public async Task ExecuteAsync_CloseFile_Twice_ShouldNotThrowAndReturnSuccess()
        {
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            var result1 = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "CLOSE");
            var result2 = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "CLOSE");

            result1.Should().Be("00");
            result2.Should().Be("00");
        }

        // Integration test: Write and read back agent record, simulating COBOL file output
        [Fact]
        public async Task Integration_AgentNotifyFile_WriteAndRead_ShouldPreserveBusinessLogic()
        {
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            var agentRecord = GetAgentRecord();

            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: agentRecord);
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "CLOSE");

            var lines = File.ReadAllLines(_agentFilePath);
            lines.Should().ContainSingle();
            var expected = string.Join('|',
                agentRecord.AgentCode,
                agentRecord.AgentName,
                agentRecord.AgentAddress1,
                agentRecord.AgentAddress2,
                agentRecord.AgentCity,
                agentRecord.AgentState,
                agentRecord.AgentPolicyNumber,
                agentRecord.AgentPolicyFName,
                agentRecord.AgentPolicyMName,
                agentRecord.AgentPolicyLName,
                agentRecord.AgentPolicyStartDate,
                agentRecord.AgentPolicyExpiryDate,
                agentRecord.AgentNotifyDate,
                agentRecord.AgentNotifyMessages
            );
            lines[0].Should().Be(expected);
        }

        // Integration test: Write and read back customer record, simulating COBOL file output
        [Fact]
        public async Task Integration_CustomerNotifyFile_WriteAndRead_ShouldPreserveBusinessLogic()
        {
            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "OPEN");
            var customerRecord = GetCustomerRecord();

            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "WRITE", customerRecord: customerRecord);
            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "CLOSE");

            var lines = File.ReadAllLines(_customerFilePath);
            lines.Should().ContainSingle();
            var expected = string.Join('|',
                customerRecord.PolicyNumber,
                customerRecord.FirstName,
                customerRecord.MiddleName,
                customerRecord.LastName,
                customerRecord.PolicyStartDate,
                customerRecord.PolicyExpiryDate,
                customerRecord.NotifyDate,
                customerRecord.NotifyMessages,
                customerRecord.AgentCode,
                customerRecord.AgentName,
                customerRecord.StatutoryMessage
            );
            lines[0].Should().Be(expected);
        }

        // Integration test: Write and read back report record, simulating COBOL file output
        [Fact]
        public async Task Integration_NotifyReportFile_WriteAndRead_ShouldPreserveBusinessLogic()
        {
            await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "OPEN");
            var reportRecord = GetReportRecord();

            await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "WRITE", reportRecord: reportRecord);
            await _driver.ExecuteAsync("NOTIFY-REPORT-FILE", "CLOSE");

            var lines = File.ReadAllLines(_reportFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be(reportRecord.ReportLine);
        }

        // Edge case: Write record with very long strings
        [Fact]
        public async Task ExecuteAsync_WriteAgentRecord_LongStrings_ShouldWriteFullData()
        {
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            var longStr = new string('A', 1000);
            var agentRecord = new AgentNotifyRecord(
                AgentCode: longStr,
                AgentName: longStr,
                AgentAddress1: longStr,
                AgentAddress2: longStr,
                AgentCity: longStr,
                AgentState: longStr,
                AgentPolicyNumber: longStr,
                AgentPolicyFName: longStr,
                AgentPolicyMName: longStr,
                AgentPolicyLName: longStr,
                AgentPolicyStartDate: longStr,
                AgentPolicyExpiryDate: longStr,
                AgentNotifyDate: longStr,
                AgentNotifyMessages: longStr
            );

            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: agentRecord);

            var lines = File.ReadAllLines(_agentFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be(string.Join('|',
                agentRecord.AgentCode,
                agentRecord.AgentName,
                agentRecord.AgentAddress1,
                agentRecord.AgentAddress2,
                agentRecord.AgentCity,
                agentRecord.AgentState,
                agentRecord.AgentPolicyNumber,
                agentRecord.AgentPolicyFName,
                agentRecord.AgentPolicyMName,
                agentRecord.AgentPolicyLName,
                agentRecord.AgentPolicyStartDate,
                agentRecord.AgentPolicyExpiryDate,
                agentRecord.AgentNotifyDate,
                agentRecord.AgentNotifyMessages
            ));
        }

        // Edge case: Write record with special characters
        [Fact]
        public async Task ExecuteAsync_WriteCustomerRecord_SpecialCharacters_ShouldWriteCorrectly()
        {
            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "OPEN");
            var specialRecord = new CustomerNotifyRecord(
                PolicyNumber: "P|9876\n54321",
                FirstName: "Jane\r\n",
                MiddleName: "M\t",
                LastName: "Smith|Doe",
                PolicyStartDate: "2024-01-01",
                PolicyExpiryDate: "2025-01-01",
                NotifyDate: "2024-06-01",
                NotifyMessages: "Policy renewal notification.\nSecond line.",
                AgentCode: "A123456789",
                AgentName: "John Doe",
                StatutoryMessage: "Statutory info"
            );

            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "WRITE", customerRecord: specialRecord);

            var lines = File.ReadAllLines(_customerFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be(string.Join('|',
                specialRecord.PolicyNumber,
                specialRecord.FirstName,
                specialRecord.MiddleName,
                specialRecord.LastName,
                specialRecord.PolicyStartDate,
                specialRecord.PolicyExpiryDate,
                specialRecord.NotifyDate,
                specialRecord.NotifyMessages,
                specialRecord.AgentCode,
                specialRecord.AgentName,
                specialRecord.StatutoryMessage
            ));
        }

        // Edge case: Write record with nulls in fields (should throw at construction, but test for completeness)
        [Fact]
        public async Task ExecuteAsync_WriteAgentRecord_NullFields_ShouldThrowArgumentNullExceptionOnRecord()
        {
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            // AgentNotifyRecord does not allow nulls, but test for completeness
            Action act = () => new AgentNotifyRecord(
                AgentCode: null,
                AgentName: null,
                AgentAddress1: null,
                AgentAddress2: null,
                AgentCity: null,
                AgentState: null,
                AgentPolicyNumber: null,
                AgentPolicyFName: null,
                AgentPolicyMName: null,
                AgentPolicyLName: null,
                AgentPolicyStartDate: null,
                AgentPolicyExpiryDate: null,
                AgentNotifyDate: null,
                AgentNotifyMessages: null
            );
            act.Should().Throw<ArgumentNullException>();
        }

        // Edge case: Write record with whitespace only fields
        [Fact]
        public async Task ExecuteAsync_WriteCustomerRecord_WhitespaceFields_ShouldWritePipeDelimitedWhitespaceStrings()
        {
            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "OPEN");
            var whitespaceRecord = new CustomerNotifyRecord(
                PolicyNumber: " ",
                FirstName: " ",
                MiddleName: " ",
                LastName: " ",
                PolicyStartDate: " ",
                PolicyExpiryDate: " ",
                NotifyDate: " ",
                NotifyMessages: " ",
                AgentCode: " ",
                AgentName: " ",
                StatutoryMessage: " "
            );

            await _driver.ExecuteAsync("CUSTOMER-NOTIFY-FILE", "WRITE", customerRecord: whitespaceRecord);

            var lines = File.ReadAllLines(_customerFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be(" | | | | | | | | | | ");
        }

        // Edge case: Write record after file closed should return error
        [Fact]
        public async Task ExecuteAsync_WriteAgentRecord_AfterClose_ShouldReturnErrorCodeAndLog()
        {
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "CLOSE");
            var agentRecord = GetAgentRecord();

            var result = await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: agentRecord);

            result.Should().Be("99");
            _loggerMock.Verify(
                x => x.Log(
                    LogLevel.Error,
                    It.IsAny<EventId>(),
                    It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error during WRITE")),
                    It.IsAny<Exception>(),
                    It.IsAny<Func<It.IsAnyType, Exception, string>>()),
                Times.Once);
        }

        // Edge case: Write record with boundary values (min/max string length)
        [Fact]
        public async Task ExecuteAsync_WriteAgentRecord_BoundaryStringLengths_ShouldWriteCorrectly()
        {
            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "OPEN");
            var minStr = "A";
            var maxStr = new string('B', 1024);
            var agentRecord = new AgentNotifyRecord(
                AgentCode: minStr,
                AgentName: maxStr,
                AgentAddress1: minStr,
                AgentAddress2: maxStr,
                AgentCity: minStr,
                AgentState: maxStr,
                AgentPolicyNumber: minStr,
                AgentPolicyFName: maxStr,
                AgentPolicyMName: minStr,
                AgentPolicyLName: maxStr,
                AgentPolicyStartDate: minStr,
                AgentPolicyExpiryDate: maxStr,
                AgentNotifyDate: minStr,
                AgentNotifyMessages: maxStr
            );

            await _driver.ExecuteAsync("AGENT-NOTIFY-FILE", "WRITE", agentRecord: agentRecord);

            var lines = File.ReadAllLines(_agentFilePath);
            lines.Should().ContainSingle();
            lines[0].Should().Be(string.Join('|',
                agentRecord.AgentCode,
                agentRecord.AgentName,
                agentRecord.AgentAddress1,
                agentRecord.AgentAddress2,
                agentRecord.AgentCity,
                agentRecord.AgentState,
                agentRecord.AgentPolicyNumber,
                agentRecord.AgentPolicyFName,
                agentRecord.AgentPolicyMName,
                agentRecord.AgentPolicyLName,
                agentRecord.AgentPolicyStartDate,
                agentRecord.AgentPolicyExpiryDate,
                agentRecord.AgentNotifyDate,
                agentRecord.AgentNotifyMessages
            ));
        }
    }
}