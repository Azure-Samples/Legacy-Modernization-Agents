using System;
using System.Data;
using System.Data.Common;
using System.Threading.Tasks;
using FluentAssertions;
using Insurance.DataDriver;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace Insurance.DataDriver.Tests
{
    public class PolicyDataDriverTests : IDisposable
    {
        private readonly Mock<DbConnection> _mockConnection;
        private readonly Mock<ILogger<PolicyDataDriver>> _mockLogger;
        private readonly Mock<DbCommand> _mockCommand;
        private readonly Mock<DbDataReader> _mockReader;
        private readonly PolicyDataDriver _driver;

        public PolicyDataDriverTests()
        {
            _mockConnection = new Mock<DbConnection>();
            _mockLogger = new Mock<ILogger<PolicyDataDriver>>();
            _mockCommand = new Mock<DbCommand>();
            _mockReader = new Mock<DbDataReader>();

            // Setup default behaviors
            _mockConnection.Setup(c => c.State).Returns(ConnectionState.Open);
            _mockConnection.Setup(c => c.CreateCommand()).Returns(_mockCommand.Object);

            _driver = new PolicyDataDriver(_mockConnection.Object, _mockLogger.Object);
        }

        public void Dispose()
        {
            // Cleanup if needed
        }

        [Fact]
        public async Task ProcessAsync_ShouldThrowArgumentNullException_WhenInputIsNull()
        {
            Func<Task> act = async () => await _driver.ProcessAsync(null);
            await act.Should().ThrowAsync<ArgumentNullException>();
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("INVALID")]
        public async Task ProcessAsync_ShouldReturnMinusOneSqlCode_ForInvalidOperationType(string operationType)
        {
            var input = new PolicyDriverInput { OperationType = operationType, ProcessDate = "2024-01-01" };
            var result = await _driver.ProcessAsync(input);

            result.SqlCode.Should().Be(-1);
            result.PolicyData.Should().BeNull();
            _mockLogger.Verify(l => l.Log(
                LogLevel.Warning,
                It.IsAny<EventId>(),
                It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Invalid operation type")),
                null,
                It.IsAny<Func<It.IsAnyType, Exception, string>>()), Times.Once);
        }

        [Fact]
        public async Task ProcessAsync_ShouldReturnMinusTwoSqlCode_WhenExceptionIsThrown()
        {
            var input = new PolicyDriverInput { OperationType = "OPEN", ProcessDate = "invalid-date" };

            // Setup command to throw on parameter creation (simulate bad date parsing)
            _mockCommand.Setup(c => c.CreateParameter()).Throws(new FormatException("Bad date"));

            var result = await _driver.ProcessAsync(input);

            result.SqlCode.Should().Be(-2);
            result.PolicyData.Should().BeNull();
            _mockLogger.Verify(l => l.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Exception during OPEN operation.")),
                It.IsAny<FormatException>(),
                It.IsAny<Func<It.IsAnyType, Exception, string>>()), Times.Once);
        }

        [Fact]
        public async Task ProcessAsync_Open_ShouldOpenConnectionAndReturnZeroSqlCode()
        {
            var input = new PolicyDriverInput { OperationType = "OPEN", ProcessDate = "2024-01-01" };

            var mockParam = new Mock<DbParameter>();
            _mockCommand.Setup(c => c.CreateParameter()).Returns(mockParam.Object);
            _mockCommand.Setup(c => c.Parameters.Add(It.IsAny<DbParameter>())).Verifiable();
            _mockCommand.SetupSet(c => c.CommandText = It.IsAny<string>()).Verifiable();
            _mockCommand.SetupSet(c => c.CommandType = CommandType.Text).Verifiable();
            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<System.Threading.CancellationToken>())).ReturnsAsync(_mockReader.Object);

            var result = await _driver.ProcessAsync(input);

            result.SqlCode.Should().Be(0);
            result.PolicyData.Should().BeNull();
            _mockCommand.Verify(c => c.CreateParameter(), Times.Once);
            _mockCommand.Verify(c => c.Parameters.Add(It.IsAny<DbParameter>()), Times.Once);
            _mockCommand.VerifySet(c => c.CommandText = It.IsAny<string>(), Times.Once);
            _mockCommand.VerifySet(c => c.CommandType = CommandType.Text, Times.Once);
            _mockCommand.Verify(c => c.ExecuteReaderAsync(It.IsAny<System.Threading.CancellationToken>()), Times.Once);
        }

        [Fact]
        public async Task ProcessAsync_Open_ShouldOpenConnectionIfNotOpen()
        {
            var input = new PolicyDriverInput { OperationType = "OPEN", ProcessDate = "2024-01-01" };

            _mockConnection.Setup(c => c.State).Returns(ConnectionState.Closed);
            _mockConnection.Setup(c => c.OpenAsync(It.IsAny<System.Threading.CancellationToken>())).Returns(Task.CompletedTask);

            var mockParam = new Mock<DbParameter>();
            _mockCommand.Setup(c => c.CreateParameter()).Returns(mockParam.Object);
            _mockCommand.Setup(c => c.Parameters.Add(It.IsAny<DbParameter>())).Verifiable();
            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<System.Threading.CancellationToken>())).ReturnsAsync(_mockReader.Object);

            var result = await _driver.ProcessAsync(input);

            result.SqlCode.Should().Be(0);
            _mockConnection.Verify(c => c.OpenAsync(It.IsAny<System.Threading.CancellationToken>()), Times.Once);
        }

        [Fact]
        public async Task ProcessAsync_Open_ShouldReturnMinusThreeSqlCode_OnException()
        {
            var input = new PolicyDriverInput { OperationType = "OPEN", ProcessDate = "2024-01-01" };

            _mockCommand.Setup(c => c.CreateParameter()).Throws(new Exception("DB error"));

            var result = await _driver.ProcessAsync(input);

            result.SqlCode.Should().Be(-3);
            result.PolicyData.Should().BeNull();
            _mockLogger.Verify(l => l.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error opening policy cursor.")),
                It.IsAny<Exception>(),
                It.IsAny<Func<It.IsAnyType, Exception, string>>()), Times.Once);
        }

        [Fact]
        public async Task ProcessAsync_Fetch_ShouldReturnMinusFourSqlCode_IfCursorNotOpen()
        {
            var input = new PolicyDriverInput { OperationType = "FETCH", ProcessDate = "2024-01-01" };
            // _policyCursorReader is null by default

            var result = await _driver.ProcessAsync(input);

            result.SqlCode.Should().Be(-4);
            result.PolicyData.Should().BeNull();
            _mockLogger.Verify(l => l.Log(
                LogLevel.Warning,
                It.IsAny<EventId>(),
                It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Policy cursor is not open.")),
                null,
                It.IsAny<Func<It.IsAnyType, Exception, string>>()), Times.Once);
        }

        [Fact]
        public async Task ProcessAsync_Fetch_ShouldReturnZeroSqlCode_AndSerializedPolicy_WhenRowIsAvailable()
        {
            // First, open the cursor
            var openInput = new PolicyDriverInput { OperationType = "OPEN", ProcessDate = "2024-01-01" };
            var mockParam = new Mock<DbParameter>();
            _mockCommand.Setup(c => c.CreateParameter()).Returns(mockParam.Object);
            _mockCommand.Setup(c => c.Parameters.Add(It.IsAny<DbParameter>())).Verifiable();
            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<System.Threading.CancellationToken>())).ReturnsAsync(_mockReader.Object);

            await _driver.ProcessAsync(openInput);

            // Setup reader to return a row
            _mockReader.Setup(r => r.ReadAsync(It.IsAny<System.Threading.CancellationToken>())).ReturnsAsync(true);

            // Setup reader fields for MapPolicyRecord
            _mockReader.Setup(r => r["POLICY_NUMBER"]).Returns("PN123");
            _mockReader.Setup(r => r["POLICY_HOLDER_FNAME"]).Returns("John");
            _mockReader.Setup(r => r["POLICY_HOLDER_MNAME"]).Returns("A");
            _mockReader.Setup(r => r["POLICY_HOLDER_LNAME"]).Returns("Doe");
            _mockReader.Setup(r => r["POLICY_BENEF_NAME"]).Returns("Jane Doe");
            _mockReader.Setup(r => r["POLICY_BENEF_RELATION"]).Returns("Spouse");
            _mockReader.Setup(r => r["POLICY_HOLDER_ADDR_1"]).Returns("123 Main St");
            _mockReader.Setup(r => r["POLICY_HOLDER_ADDR_2"]).Returns("Apt 4");
            _mockReader.Setup(r => r["POLICY_HOLDER_CITY"]).Returns("Springfield");
            _mockReader.Setup(r => r["POLICY_HOLDER_STATE"]).Returns("IL");
            _mockReader.Setup(r => r["POLICY_HOLDER_ZIP_CD"]).Returns("62704");
            _mockReader.Setup(r => r["POLICY_HOLDER_DOB"]).Returns(new DateTime(1980, 1, 1));
            _mockReader.Setup(r => r["POLICY_HOLDER_GENDER"]).Returns("M");
            _mockReader.Setup(r => r["POLICY_HOLDER_PHONE"]).Returns("555-1234");
            _mockReader.Setup(r => r["POLICY_HOLDER_EMAIL"]).Returns("john.doe@email.com");
            _mockReader.Setup(r => r["POLICY_PAYMENT_FREQUENCY"]).Returns("Monthly");
            _mockReader.Setup(r => r["POLICY_PAYMENT_METHOD"]).Returns("CreditCard");
            _mockReader.Setup(r => r["POLICY_UNDERWRITER"]).Returns("Acme");
            _mockReader.Setup(r => r["POLICY_TERMS_CONDITIONS"]).Returns("Standard");
            _mockReader.Setup(r => r["POLICY_CLAIMED"]).Returns("N");
            _mockReader.Setup(r => r["POLICY_DISCOUNT_CODE"]).Returns("DISC10");
            _mockReader.Setup(r => r["POLICY_PREMIUM_AMOUNT"]).Returns(123.45m);
            _mockReader.Setup(r => r["POLICY_COVERAGE_AMOUNT"]).Returns(10000.00m);
            _mockReader.Setup(r => r["POLICY_TYPE"]).Returns("Life");
            _mockReader.Setup(r => r["POLICY_START_DATE"]).Returns(new DateTime(2024, 1, 1));
            _mockReader.Setup(r => r["POLICY_EXPIRY_DATE"]).Returns(new DateTime(2025, 1, 1));
            _mockReader.Setup(r => r["POLICY_STATUS"]).Returns("Active");
            _mockReader.Setup(r => r["POLICY_AGENT_CODE"]).Returns("AGT001");
            _mockReader.Setup(r => r["POLICY_NOTIFY_FLAG"]).Returns("Y");
            _mockReader.Setup(r => r["POLICY_ADD_TIMESTAMP"]).Returns(new DateTime(2024, 1, 1, 12, 0, 0));
            _mockReader.Setup(r => r["POLICY_UPDATE_TIMESTAMP"]).Returns(new DateTime(2024, 6, 1, 12, 0, 0));

            var fetchInput = new PolicyDriverInput { OperationType = "FETCH", ProcessDate = "2024-01-01" };
            var result = await _driver.ProcessAsync(fetchInput);

            result.SqlCode.Should().Be(0);
            result.PolicyData.Should().NotBeNull();
            result.PolicyData!.Length.Should().Be(466); // COBOL PIC X(466) equivalent
        }

        [Fact]
        public async Task ProcessAsync_Fetch_ShouldReturn100SqlCode_WhenNoMoreRows()
        {
            // Open the cursor first
            var openInput = new PolicyDriverInput { OperationType = "OPEN", ProcessDate = "2024-01-01" };
            var mockParam = new Mock<DbParameter>();
            _mockCommand.Setup(c => c.CreateParameter()).Returns(mockParam.Object);
            _mockCommand.Setup(c => c.Parameters.Add(It.IsAny<DbParameter>())).Verifiable();
            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<System.Threading.CancellationToken>())).ReturnsAsync(_mockReader.Object);

            await _driver.ProcessAsync(openInput);

            // Setup reader to return no rows
            _mockReader.Setup(r => r.ReadAsync(It.IsAny<System.Threading.CancellationToken>())).ReturnsAsync(false);

            var fetchInput = new PolicyDriverInput { OperationType = "FETCH", ProcessDate = "2024-01-01" };
            var result = await _driver.ProcessAsync(fetchInput);

            result.SqlCode.Should().Be(100);
            result.PolicyData.Should().BeNull();
        }

        [Fact]
        public async Task ProcessAsync_Fetch_ShouldReturnMinusFiveSqlCode_OnException()
        {
            // Open the cursor first
            var openInput = new PolicyDriverInput { OperationType = "OPEN", ProcessDate = "2024-01-01" };
            var mockParam = new Mock<DbParameter>();
            _mockCommand.Setup(c => c.CreateParameter()).Returns(mockParam.Object);
            _mockCommand.Setup(c => c.Parameters.Add(It.IsAny<DbParameter>())).Verifiable();
            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<System.Threading.CancellationToken>())).ReturnsAsync(_mockReader.Object);

            await _driver.ProcessAsync(openInput);

            // Setup reader to throw on ReadAsync
            _mockReader.Setup(r => r.ReadAsync(It.IsAny<System.Threading.CancellationToken>())).ThrowsAsync(new Exception("Read error"));

            var fetchInput = new PolicyDriverInput { OperationType = "FETCH", ProcessDate = "2024-01-01" };
            var result = await _driver.ProcessAsync(fetchInput);

            result.SqlCode.Should().Be(-5);
            result.PolicyData.Should().BeNull();
            _mockLogger.Verify(l => l.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error fetching from policy cursor.")),
                It.IsAny<Exception>(),
                It.IsAny<Func<It.IsAnyType, Exception, string>>()), Times.Once);
        }

        [Fact]
        public async Task ProcessAsync_Close_ShouldDisposeCursorAndReturnZeroSqlCode()
        {
            // Open the cursor first
            var openInput = new PolicyDriverInput { OperationType = "OPEN", ProcessDate = "2024-01-01" };
            var mockParam = new Mock<DbParameter>();
            _mockCommand.Setup(c => c.CreateParameter()).Returns(mockParam.Object);
            _mockCommand.Setup(c => c.Parameters.Add(It.IsAny<DbParameter>())).Verifiable();
            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<System.Threading.CancellationToken>())).ReturnsAsync(_mockReader.Object);

            await _driver.ProcessAsync(openInput);

            // Setup reader and command to dispose
            _mockReader.Setup(r => r.DisposeAsync()).Returns(ValueTask.CompletedTask).Verifiable();
            _mockCommand.Setup(c => c.DisposeAsync()).Returns(ValueTask.CompletedTask).Verifiable();

            var closeInput = new PolicyDriverInput { OperationType = "CLOSE", ProcessDate = "2024-01-01" };
            var result = await _driver.ProcessAsync(closeInput);

            result.SqlCode.Should().Be(0);
            result.PolicyData.Should().BeNull();
            _mockReader.Verify(r => r.DisposeAsync(), Times.Once);
            _mockCommand.Verify(c => c.DisposeAsync(), Times.Once);
        }

        [Fact]
        public async Task ProcessAsync_Close_ShouldReturnZeroSqlCode_IfCursorAlreadyClosed()
        {
            var closeInput = new PolicyDriverInput { OperationType = "CLOSE", ProcessDate = "2024-01-01" };
            var result = await _driver.ProcessAsync(closeInput);

            result.SqlCode.Should().Be(0);
            result.PolicyData.Should().BeNull();
        }

        [Fact]
        public async Task ProcessAsync_Close_ShouldReturnMinusSixSqlCode_OnException()
        {
            // Open the cursor first
            var openInput = new PolicyDriverInput { OperationType = "OPEN", ProcessDate = "2024-01-01" };
            var mockParam = new Mock<DbParameter>();
            _mockCommand.Setup(c => c.CreateParameter()).Returns(mockParam.Object);
            _mockCommand.Setup(c => c.Parameters.Add(It.IsAny<DbParameter>())).Verifiable();
            _mockCommand.Setup(c => c.ExecuteReaderAsync(It.IsAny<System.Threading.CancellationToken>())).ReturnsAsync(_mockReader.Object);

            await _driver.ProcessAsync(openInput);

            // Setup reader to throw on DisposeAsync
            _mockReader.Setup(r => r.DisposeAsync()).Throws(new Exception("Dispose error"));

            var closeInput = new PolicyDriverInput { OperationType = "CLOSE", ProcessDate = "2024-01-01" };
            var result = await _driver.ProcessAsync(closeInput);

            result.SqlCode.Should().Be(-6);
            result.PolicyData.Should().BeNull();
            _mockLogger.Verify(l => l.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.Is<It.IsAnyType>((v, t) => v.ToString().Contains("Error closing policy cursor.")),
                It.IsAny<Exception>(),
                It.IsAny<Func<It.IsAnyType, Exception, string>>()), Times.Once);
        }

        [Fact]
        public void MapPolicyRecord_ShouldMapAllFieldsCorrectly()
        {
            var mockReader = new Mock<DbDataReader>();

            // Setup all fields with sample values and nulls for edge cases
            mockReader.Setup(r => r["POLICY_NUMBER"]).Returns("PN999");
            mockReader.Setup(r => r["POLICY_HOLDER_FNAME"]).Returns(DBNull.Value);
            mockReader.Setup(r => r["POLICY_HOLDER_MNAME"]).Returns("B");
            mockReader.Setup(r => r["POLICY_HOLDER_LNAME"]).Returns("Smith");
            mockReader.Setup(r => r["POLICY_BENEF_NAME"]).Returns("Alice Smith");
            mockReader.Setup(r => r["POLICY_BENEF_RELATION"]).Returns("Child");
            mockReader.Setup(r => r["POLICY_HOLDER_ADDR_1"]).Returns("456 Elm St");
            mockReader.Setup(r => r["POLICY_HOLDER_ADDR_2"]).Returns(DBNull.Value);
            mockReader.Setup(r => r["POLICY_HOLDER_CITY"]).Returns("Metropolis");
            mockReader.Setup(r => r["POLICY_HOLDER_STATE"]).Returns("NY");
            mockReader.Setup(r => r["POLICY_HOLDER_ZIP_CD"]).Returns("10001");
            mockReader.Setup(r => r["POLICY_HOLDER_DOB"]).Returns(DBNull.Value);
            mockReader.Setup(r => r["POLICY_HOLDER_GENDER"]).Returns("F");
            mockReader.Setup(r => r["POLICY_HOLDER_PHONE"]).Returns("555-6789");
            mockReader.Setup(r => r["POLICY_HOLDER_EMAIL"]).Returns("alice.smith@email.com");
            mockReader.Setup(r => r["POLICY_PAYMENT_FREQUENCY"]).Returns("Annual");
            mockReader.Setup(r => r["POLICY_PAYMENT_METHOD"]).Returns("Check");
            mockReader.Setup(r => r["POLICY_UNDERWRITER"]).Returns("BestIns");
            mockReader.Setup(r => r["POLICY_TERMS_CONDITIONS"]).Returns("Premium");
            mockReader.Setup(r => r["POLICY_CLAIMED"]).Returns("Y");
            mockReader.Setup(r => r["POLICY_DISCOUNT_CODE"]).Returns(DBNull.Value);
            mockReader.Setup(r => r["POLICY_PREMIUM_AMOUNT"]).Returns(DBNull.Value);
            mockReader.Setup(r => r["POLICY_COVERAGE_AMOUNT"]).Returns(50000.00m);
            mockReader.Setup(r => r["POLICY_TYPE"]).Returns("Health");
            mockReader.Setup(r => r["POLICY_START_DATE"]).Returns(new DateTime(2023, 1, 1));
            mockReader.Setup(r => r["POLICY_EXPIRY_DATE"]).Returns(new DateTime(2024, 1, 1));
            mockReader.Setup(r => r["POLICY_STATUS"]).Returns("Expired");
            mockReader.Setup(r => r["POLICY_AGENT_CODE"]).Returns("AGT002");
            mockReader.Setup(r => r["POLICY_NOTIFY_FLAG"]).Returns("N");
            mockReader.Setup(r => r["POLICY_ADD_TIMESTAMP"]).Returns(new DateTime(2023, 1, 1, 8, 0, 0));
            mockReader.Setup(r => r["POLICY_UPDATE_TIMESTAMP"]).Returns(DBNull.Value);

            var record = typeof(PolicyDataDriver)
                .GetMethod("MapPolicyRecord", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static)!
                .Invoke(null, new object[] { mockReader.Object }) as PolicyRecord;

            record.Should().NotBeNull();
            record!.PolicyNumber.Should().Be("PN999");
            record.PolicyHolderFirstName.Should().BeEmpty(); // Null mapped to empty
            record.PolicyHolderMiddleName.Should().Be("B");
            record.PolicyHolderLastName.Should().Be("Smith");
            record.PolicyBeneficiaryName.Should().Be("Alice Smith");
            record.PolicyBeneficiaryRelation.Should().Be("Child");
            record.PolicyHolderAddress1.Should().Be("456 Elm St");
            record.PolicyHolderAddress2.Should().BeEmpty(); // Null mapped to empty
            record.PolicyHolderCity.Should().Be("Metropolis");
            record.PolicyHolderState.Should().Be("NY");
            record.PolicyHolderZipCode.Should().Be("10001");
            record.PolicyHolderDateOfBirth.Should().BeNull();
            record.PolicyHolderGender.Should().Be("F");
            record.PolicyHolderPhone.Should().Be("555-6789");
            record.PolicyHolderEmail.Should().Be("alice.smith@email.com");
            record.PolicyPaymentFrequency.Should().Be("Annual");
            record.PolicyPaymentMethod.Should().Be("Check");
            record.PolicyUnderwriter.Should().Be("BestIns");
            record.PolicyTermsConditions.Should().Be("Premium");
            record.PolicyClaimed.Should().Be("Y");
            record.PolicyDiscountCode.Should().BeEmpty(); // Null mapped to empty
            record.PolicyPremiumAmount.Should().BeNull();
            record.PolicyCoverageAmount.Should().Be(50000.00m);
            record.PolicyType.Should().Be("Health");
            record.PolicyStartDate.Should().Be(new DateTime(2023, 1, 1));
            record.PolicyExpiryDate.Should().Be(new DateTime(2024, 1, 1));
            record.PolicyStatus.Should().Be("Expired");
            record.PolicyAgentCode.Should().Be("AGT002");
            record.PolicyNotifyFlag.Should().Be("N");
            record.PolicyAddTimestamp.Should().Be(new DateTime(2023, 1, 1, 8, 0, 0));
            record.PolicyUpdateTimestamp.Should().BeNull();
        }

        [Fact]
        public void SerializePolicyRecord_ShouldPadOrTruncateTo466Characters()
        {
            var policy = new PolicyRecord
            {
                PolicyNumber = new string('A', 100),
                PolicyHolderFirstName = new string('B', 100),
                PolicyHolderMiddleName = new string('C', 100),
                PolicyHolderLastName = new string('D', 100),
                PolicyBeneficiaryName = new string('E', 100),
                PolicyBeneficiaryRelation = new string('F', 100),
                PolicyHolderAddress1 = new string('G', 100),
                PolicyHolderAddress2 = new string('H', 100),
                PolicyHolderCity = new string('I', 100),
                PolicyHolderState = new string('J', 100),
                PolicyHolderZipCode = new string('K', 100),
                PolicyHolderDateOfBirth = DateTime.Now,
                PolicyHolderGender = "M",
                PolicyHolderPhone = "555-0000",
                PolicyHolderEmail = "test@email.com",
                PolicyPaymentFrequency = "Monthly",
                PolicyPaymentMethod = "CreditCard",
                PolicyUnderwriter = "Underwriter",
                PolicyTermsConditions = "Terms",
                PolicyClaimed = "N",
                PolicyDiscountCode = "DISC",
                PolicyPremiumAmount = 99999.99m,
                PolicyCoverageAmount = 999999.99m,
                PolicyType = "Type",
                PolicyStartDate = DateTime.Now,
                PolicyExpiryDate = DateTime.Now.AddYears(1),
                PolicyStatus = "Active",
                PolicyAgentCode = "AGT",
                PolicyNotifyFlag = "Y",
                PolicyAddTimestamp = DateTime.Now,
                PolicyUpdateTimestamp = DateTime.Now
            };

            var serializeMethod = typeof(PolicyDataDriver)
                .GetMethod("SerializePolicyRecord", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static)!;

            var serialized = serializeMethod.Invoke(null, new object[] { policy }) as string;

            serialized.Should().NotBeNull();
            serialized!.Length.Should().Be(466);

            // If JSON is longer than 466, it should be truncated
            var policy2 = new PolicyRecord
            {
                PolicyNumber = new string('Z', 1000)
            };
            var serialized2 = serializeMethod.Invoke(null, new object[] { policy2 }) as string;
            serialized2!.Length.Should().Be(466);
        }

        [Fact]
        public void GetPolicyCursorSql_ShouldReturnNonEmptySqlString()
        {
            var sqlMethod = typeof(PolicyDataDriver)
                .GetMethod("GetPolicyCursorSql", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Static)!;
            var sql = sqlMethod.Invoke(null, null) as string;

            sql.Should().NotBeNullOrWhiteSpace();
            sql!.Should().Contain("SELECT");
            sql.Should().Contain("FROM");
        }
    }
}