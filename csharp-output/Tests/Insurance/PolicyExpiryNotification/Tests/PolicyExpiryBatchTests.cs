using System;
using System.Threading.Tasks;
using System.Collections.Generic;
using FluentAssertions;
using Moq;
using Xunit;
using Microsoft.Extensions.Logging;

namespace Insurance.PolicyExpiryNotification.Tests
{
    public class PolicyExpiryBatchTests : IDisposable
    {
        private readonly Mock<IDbDriver1> _dbDriver1Mock;
        private readonly Mock<IDbDriver2> _dbDriver2Mock;
        private readonly Mock<IFileDriver1> _fileDriver1Mock;
        private readonly Mock<IFileDriver2> _fileDriver2Mock;
        private readonly Mock<ILogger<PolicyExpiryBatch>> _loggerMock;
        private readonly PolicyExpiryBatch _batch;

        public PolicyExpiryBatchTests()
        {
            _dbDriver1Mock = new Mock<IDbDriver1>(MockBehavior.Strict);
            _dbDriver2Mock = new Mock<IDbDriver2>(MockBehavior.Strict);
            _fileDriver1Mock = new Mock<IFileDriver1>(MockBehavior.Strict);
            _fileDriver2Mock = new Mock<IFileDriver2>(MockBehavior.Strict);
            _loggerMock = new Mock<ILogger<PolicyExpiryBatch>>(MockBehavior.Loose);

            _batch = new PolicyExpiryBatch(
                _dbDriver1Mock.Object,
                _dbDriver2Mock.Object,
                _fileDriver1Mock.Object,
                _fileDriver2Mock.Object,
                _loggerMock.Object
            );
        }

        public void Dispose()
        {
            // Cleanup if needed
        }

        [Fact]
        public async Task RunAsync_Should_Initialize_Process_And_Finalize_When_All_Success()
        {
            // Arrange
            SetupInitializeAsyncSuccess();
            SetupProcessPoliciesAsyncSuccess();
            SetupFinalizeAsyncSuccess();

            // Act
            Func<Task> act = async () => await _batch.RunAsync();

            // Assert
            await act.Should().NotThrowAsync();
            _dbDriver1Mock.VerifyAll();
            _dbDriver2Mock.VerifyAll();
            _fileDriver1Mock.VerifyAll();
            _fileDriver2Mock.VerifyAll();
        }

        [Fact]
        public async Task RunAsync_Should_LogError_And_Throw_When_InitializeAsync_Fails()
        {
            // Arrange
            _dbDriver1Mock.Setup(d => d.OpenCursorAsync(It.IsAny<DateOnly>()))
                .ReturnsAsync(new DbOpenResult { IsSuccess = false, ErrorCode = -999 });

            // Act
            Func<Task> act = async () => await _batch.RunAsync();

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error opening policy cursor: -999");
            _loggerMock.Verify(
                x => x.LogError("Error opening policy cursor: {SqlCode}", -999),
                Times.Once
            );
        }

        [Fact]
        public async Task RunAsync_Should_LogError_And_Throw_When_ProcessPoliciesAsync_Fetch_Error()
        {
            // Arrange
            SetupInitializeAsyncSuccess();

            _dbDriver1Mock.SetupSequence(d => d.FetchNextPolicyAsync())
                .ReturnsAsync(new DbFetchResult { Status = DbFetchStatus.Error, ErrorCode = -123 });

            // Act
            Func<Task> act = async () => await _batch.RunAsync();

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error fetching policy record: -123");
            _loggerMock.Verify(
                x => x.LogError("Error fetching policy record: {SqlCode}", -123),
                Times.Once
            );
        }

        [Fact]
        public async Task RunAsync_Should_Process_No_Policies_When_EndOfData_Immediately()
        {
            // Arrange
            SetupInitializeAsyncSuccess();

            _dbDriver1Mock.SetupSequence(d => d.FetchNextPolicyAsync())
                .ReturnsAsync(new DbFetchResult { Status = DbFetchStatus.EndOfData });

            SetupFinalizeAsyncSuccess();

            // Act
            Func<Task> act = async () => await _batch.RunAsync();

            // Assert
            await act.Should().NotThrowAsync();
        }

        [Fact]
        public async Task RunAsync_Should_Throw_When_Agent_File_Open_Fails()
        {
            // Arrange
            _dbDriver1Mock.Setup(d => d.OpenCursorAsync(It.IsAny<DateOnly>()))
                .ReturnsAsync(new DbOpenResult { IsSuccess = true });
            _fileDriver1Mock.Setup(d => d.OpenAsync())
                .ReturnsAsync(new FileOpenResult { IsSuccess = false, ErrorCode = 42 });

            // Act
            Func<Task> act = async () => await _batch.RunAsync();

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error opening agent file: 42");
            _loggerMock.Verify(
                x => x.LogError("Error opening agent file: {StatusCode}", 42),
                Times.Once
            );
        }

        [Theory]
        [InlineData("CUSTOMER-NOTIFY-FILE")]
        [InlineData("NOTIFY-REPORT-FILE")]
        [InlineData("AGENT-NOTIFY-FILE")]
        public async Task RunAsync_Should_Throw_When_Notification_File_Open_Fails(string fileName)
        {
            // Arrange
            _dbDriver1Mock.Setup(d => d.OpenCursorAsync(It.IsAny<DateOnly>()))
                .ReturnsAsync(new DbOpenResult { IsSuccess = true });
            _fileDriver1Mock.Setup(d => d.OpenAsync())
                .ReturnsAsync(new FileOpenResult { IsSuccess = true });

            _fileDriver2Mock.Setup(d => d.OpenAsync(It.Is<string>(f => f == fileName)))
                .ReturnsAsync(new FileOpenResult { IsSuccess = false, ErrorCode = 77 });

            foreach (var otherFile in new[] { "CUSTOMER-NOTIFY-FILE", "NOTIFY-REPORT-FILE", "AGENT-NOTIFY-FILE" })
            {
                if (otherFile != fileName)
                {
                    _fileDriver2Mock.Setup(d => d.OpenAsync(otherFile))
                        .ReturnsAsync(new FileOpenResult { IsSuccess = true });
                }
            }

            // Act
            Func<Task> act = async () => await _batch.RunAsync();

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage($"Error opening {fileName}: 77");
            _loggerMock.Verify(
                x => x.LogError("Error opening {FileName}: {StatusCode}", fileName, 77),
                Times.Once
            );
        }

        [Fact]
        public async Task GetAgentDetailAsync_Should_Throw_When_CurrentPolicy_Is_Null()
        {
            // Arrange
            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("GetAgentDetailAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            Func<Task> act = async () => await (Task)method.Invoke(_batch, null);

            // Assert
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Current policy is null.");
        }

        [Fact]
        public async Task GetAgentDetailAsync_Should_Throw_When_Agent_Search_Fails()
        {
            // Arrange
            SetPrivateField("_currentPolicy", new PolicyRecord { AgentCode = "AG001" });

            _fileDriver1Mock.Setup(d => d.SearchAgentAsync("AG001"))
                .ReturnsAsync(new AgentSearchResult { IsSuccess = false, ErrorCode = 55 });

            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("GetAgentDetailAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            Func<Task> act = async () => await (Task)method.Invoke(_batch, null);

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error fetching agent record: 55");
            _loggerMock.Verify(
                x => x.LogError("Error fetching agent record: {StatusCode}", 55),
                Times.Once
            );
        }

        [Fact]
        public async Task GetAgentDetailAsync_Should_WriteAgentNotification_When_AgentType_Corporate()
        {
            // Arrange
            var policy = new PolicyRecord { AgentCode = "AG002" };
            var agent = new AgentRecord { AgentCode = "AG002", AgentType = "CORPORATE" };

            SetPrivateField("_currentPolicy", policy);

            _fileDriver1Mock.Setup(d => d.SearchAgentAsync("AG002"))
                .ReturnsAsync(new AgentSearchResult { IsSuccess = true, Agent = agent });

            _fileDriver2Mock.Setup(d => d.WriteAsync("AGENT-NOTIFY-FILE", It.IsAny<AgentNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { IsSuccess = true });

            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("GetAgentDetailAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            Func<Task> act = async () => await (Task)method.Invoke(_batch, null);

            // Assert
            await act.Should().NotThrowAsync();
            _fileDriver2Mock.Verify(
                x => x.WriteAsync("AGENT-NOTIFY-FILE", It.IsAny<AgentNotifyRecord>()),
                Times.Once
            );
        }

        [Fact]
        public async Task WriteCustomerNotificationAsync_Should_Throw_When_CurrentPolicy_Or_Agent_Is_Null()
        {
            // Arrange
            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("WriteCustomerNotificationAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            Func<Task> act = async () => await (Task)method.Invoke(_batch, null);

            // Assert
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Current policy or agent is null.");
        }

        [Fact]
        public async Task WriteCustomerNotificationAsync_Should_LogError_When_Write_Fails()
        {
            // Arrange
            var policy = new PolicyRecord { AgentCode = "AG003", PolicyNumber = "P123", PremiumAmount = 100m, HolderState = "TX" };
            var agent = new AgentRecord { AgentCode = "AG003", AgentType = "INDIVIDUAL" };
            SetPrivateField("_currentPolicy", policy);
            SetPrivateField("_currentAgent", agent);

            _fileDriver2Mock.Setup(d => d.WriteAsync("CUSTOMER-NOTIFY-FILE", It.IsAny<CustomerNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { IsSuccess = false, ErrorCode = 88 });

            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("WriteCustomerNotificationAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            await ((Task)method.Invoke(_batch, null));

            // Assert
            _loggerMock.Verify(
                x => x.LogError("Error writing to customer notify file: {StatusCode}", 88),
                Times.Once
            );
        }

        [Fact]
        public async Task UpdateTrackingAsync_Should_Throw_When_CurrentPolicy_Is_Null()
        {
            // Arrange
            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("UpdateTrackingAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            Func<Task> act = async () => await (Task)method.Invoke(_batch, null);

            // Assert
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Current policy is null.");
        }

        [Fact]
        public async Task UpdateTrackingAsync_Should_Throw_When_InsertTracking_Fails()
        {
            // Arrange
            var policy = new PolicyRecord { PolicyNumber = "P456" };
            SetPrivateField("_currentPolicy", policy);

            SetPrivateField("_processDate", DateOnly.FromDateTime(DateTime.Now));

            _dbDriver2Mock.Setup(d => d.InsertTrackingAsync(It.IsAny<DateOnly>(), "P456"))
                .ReturnsAsync(new DbInsertResult { IsSuccess = false, ErrorCode = 66 });

            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("UpdateTrackingAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            Func<Task> act = async () => await (Task)method.Invoke(_batch, null);

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error inserting into tracking: 66");
            _loggerMock.Verify(
                x => x.LogError("Error inserting into tracking: {SqlCode}", 66),
                Times.Once
            );
        }

        [Fact]
        public async Task ProcessSummaryAsync_Should_Throw_When_CurrentPolicy_Or_Agent_Is_Null()
        {
            // Arrange
            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("ProcessSummaryAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            Func<Task> act = async () => await (Task)method.Invoke(_batch, null);

            // Assert
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Current policy or agent is null.");
        }

        [Fact]
        public async Task WriteAgentNotificationAsync_Should_Not_Write_When_AgentType_IsNot_Corporate()
        {
            // Arrange
            var policy = new PolicyRecord { AgentCode = "AG004" };
            var agent = new AgentRecord { AgentCode = "AG004", AgentType = "INDIVIDUAL" };
            SetPrivateField("_currentPolicy", policy);
            SetPrivateField("_currentAgent", agent);

            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("WriteAgentNotificationAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            await ((Task)method.Invoke(_batch, null));

            // Assert
            _fileDriver2Mock.Verify(
                x => x.WriteAsync("AGENT-NOTIFY-FILE", It.IsAny<AgentNotifyRecord>()),
                Times.Never
            );
        }

        [Fact]
        public async Task WriteAgentNotificationAsync_Should_LogError_When_Write_Fails_For_Corporate()
        {
            // Arrange
            var policy = new PolicyRecord { AgentCode = "AG005" };
            var agent = new AgentRecord { AgentCode = "AG005", AgentType = "CORPORATE" };
            SetPrivateField("_currentPolicy", policy);
            SetPrivateField("_currentAgent", agent);

            _fileDriver2Mock.Setup(d => d.WriteAsync("AGENT-NOTIFY-FILE", It.IsAny<AgentNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { IsSuccess = false, ErrorCode = 99 });

            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("WriteAgentNotificationAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            await ((Task)method.Invoke(_batch, null));

            // Assert
            _loggerMock.Verify(
                x => x.LogError("Error writing to agent notify file: {StatusCode}", 99),
                Times.Once
            );
        }

        [Fact]
        public async Task WriteNotificationReportAsync_Should_LogError_When_Write_Fails()
        {
            // Arrange
            _fileDriver2Mock.Setup(d => d.WriteAsync("NOTIFY-REPORT-FILE", It.IsAny<NotifyReportRecord>()))
                .ReturnsAsync(new FileWriteResult { IsSuccess = false, ErrorCode = 101 });

            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("WriteNotificationReportAsync", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            await ((Task)method.Invoke(_batch, new object[] { "REPORT LINE" }));

            // Assert
            _loggerMock.Verify(
                x => x.LogError("Error writing to notify report file: {StatusCode}", 101),
                Times.Once
            );
        }

        [Fact]
        public void ResetAgentTotals_Should_Reset_Agent_Counters()
        {
            // Arrange
            SetPrivateField("_agentTotalPolicyCount", 5);
            SetPrivateField("_agentTotalPremium", 500m);

            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("ResetAgentTotals", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            method.Invoke(_batch, null);

            // Assert
            GetPrivateField<int>("_agentTotalPolicyCount").Should().Be(0);
            GetPrivateField<decimal>("_agentTotalPremium").Should().Be(0m);
        }

        [Fact]
        public void ResetStateTotals_Should_Reset_State_Counters()
        {
            // Arrange
            SetPrivateField("_stateTotalPolicyCount", 7);
            SetPrivateField("_stateTotalPremium", 700m);

            var batchType = typeof(PolicyExpiryBatch);
            var method = batchType.GetMethod("ResetStateTotals", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);

            // Act
            method.Invoke(_batch, null);

            // Assert
            GetPrivateField<int>("_stateTotalPolicyCount").Should().Be(0);
            GetPrivateField<decimal>("_stateTotalPremium").Should().Be(0m);
        }

        // Integration Test: Simulate full batch run with multiple policies, state/agent breaks, and verify summary logic
        [Fact]
        public async Task RunAsync_Integration_Should_Process_Multiple_Policies_And_Summaries()
        {
            // Arrange
            SetupInitializeAsyncSuccess();

            var policies = new List<PolicyRecord>
            {
                new PolicyRecord { PolicyNumber = "P1", AgentCode = "A1", PremiumAmount = 100m, HolderState = "CA" },
                new PolicyRecord { PolicyNumber = "P2", AgentCode = "A1", PremiumAmount = 200m, HolderState = "CA" },
                new PolicyRecord { PolicyNumber = "P3", AgentCode = "A2", PremiumAmount = 300m, HolderState = "CA" },
                new PolicyRecord { PolicyNumber = "P4", AgentCode = "A2", PremiumAmount = 400m, HolderState = "TX" }
            };

            var agents = new Dictionary<string, AgentRecord>
            {
                { "A1", new AgentRecord { AgentCode = "A1", AgentType = "INDIVIDUAL" } },
                { "A2", new AgentRecord { AgentCode = "A2", AgentType = "CORPORATE" } }
            };

            var fetchSequence = _dbDriver1Mock.SetupSequence(d => d.FetchNextPolicyAsync());
            foreach (var p in policies)
                fetchSequence = fetchSequence.ReturnsAsync(new DbFetchResult { Status = DbFetchStatus.Success, Policy = p });
            fetchSequence = fetchSequence.ReturnsAsync(new DbFetchResult { Status = DbFetchStatus.EndOfData });

            foreach (var agentCode in agents.Keys)
            {
                _fileDriver1Mock.Setup(d => d.SearchAgentAsync(agentCode))
                    .ReturnsAsync(new AgentSearchResult { IsSuccess = true, Agent = agents[agentCode] });
            }

            _fileDriver2Mock.Setup(d => d.WriteAsync("CUSTOMER-NOTIFY-FILE", It.IsAny<CustomerNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { IsSuccess = true });
            _fileDriver2Mock.Setup(d => d.WriteAsync("AGENT-NOTIFY-FILE", It.IsAny<AgentNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { IsSuccess = true });
            _fileDriver2Mock.Setup(d => d.WriteAsync("NOTIFY-REPORT-FILE", It.IsAny<NotifyReportRecord>()))
                .ReturnsAsync(new FileWriteResult { IsSuccess = true });

            _dbDriver2Mock.Setup(d => d.InsertTrackingAsync(It.IsAny<DateOnly>(), It.IsAny<string>()))
                .ReturnsAsync(new DbInsertResult { IsSuccess = true });

            SetupFinalizeAsyncSuccess();

            // Act
            Func<Task> act = async () => await _batch.RunAsync();

            // Assert
            await act.Should().NotThrowAsync();

            // Verify agent notification was written only for corporate agent
            _fileDriver2Mock.Verify(
                x => x.WriteAsync("AGENT-NOTIFY-FILE", It.IsAny<AgentNotifyRecord>()),
                Times.Exactly(2)
            );

            // Verify customer notifications written for all policies
            _fileDriver2Mock.Verify(
                x => x.WriteAsync("CUSTOMER-NOTIFY-FILE", It.IsAny<CustomerNotifyRecord>()),
                Times.Exactly(4)
            );

            // Verify tracking inserted for all policies
            _dbDriver2Mock.Verify(
                x => x.InsertTrackingAsync(It.IsAny<DateOnly>(), It.IsAny<string>()),
                Times.Exactly(4)
            );
        }

        // Helper methods for setting up mocks and private fields

        private void SetupInitializeAsyncSuccess()
        {
            _dbDriver1Mock.Setup(d => d.OpenCursorAsync(It.IsAny<DateOnly>()))
                .ReturnsAsync(new DbOpenResult { IsSuccess = true });
            _fileDriver1Mock.Setup(d => d.OpenAsync())
                .ReturnsAsync(new FileOpenResult { IsSuccess = true });
            _fileDriver2Mock.Setup(d => d.OpenAsync("CUSTOMER-NOTIFY-FILE"))
                .ReturnsAsync(new FileOpenResult { IsSuccess = true });
            _fileDriver2Mock.Setup(d => d.OpenAsync("NOTIFY-REPORT-FILE"))
                .ReturnsAsync(new FileOpenResult { IsSuccess = true });
            _fileDriver2Mock.Setup(d => d.OpenAsync("AGENT-NOTIFY-FILE"))
                .ReturnsAsync(new FileOpenResult { IsSuccess = true });

            _fileDriver2Mock.Setup(d => d.WriteAsync("NOTIFY-REPORT-FILE", It.IsAny<NotifyReportRecord>()))
                .ReturnsAsync(new FileWriteResult { IsSuccess = true });
        }

        private void SetupProcessPoliciesAsyncSuccess()
        {
            _dbDriver1Mock.SetupSequence(d => d.FetchNextPolicyAsync())
                .ReturnsAsync(new DbFetchResult { Status = DbFetchStatus.EndOfData });
        }

        private void SetupFinalizeAsyncSuccess()
        {
            // FinalizeAsync is not shown in code, but assume it writes grand summary
            _fileDriver2Mock.Setup(d => d.WriteAsync("NOTIFY-REPORT-FILE", It.IsAny<NotifyReportRecord>()))
                .ReturnsAsync(new FileWriteResult { IsSuccess = true });
        }

        private void SetPrivateField<T>(string fieldName, T value)
        {
            var field = typeof(PolicyExpiryBatch).GetField(fieldName, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            field.SetValue(_batch, value);
        }

        private T GetPrivateField<T>(string fieldName)
        {
            var field = typeof(PolicyExpiryBatch).GetField(fieldName, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            return (T)field.GetValue(_batch);
        }
    }

    // Dummy interfaces and classes for compilation (would be real in actual codebase)
    public interface IDbDriver1
    {
        Task<DbOpenResult> OpenCursorAsync(DateOnly processDate);
        Task<DbFetchResult> FetchNextPolicyAsync();
    }

    public interface IDbDriver2
    {
        Task<DbInsertResult> InsertTrackingAsync(DateOnly processDate, string policyNumber);
    }

    public interface IFileDriver1
    {
        Task<FileOpenResult> OpenAsync();
        Task<AgentSearchResult> SearchAgentAsync(string agentCode);
    }

    public interface IFileDriver2
    {
        Task<FileOpenResult> OpenAsync(string fileName);
        Task<FileWriteResult> WriteAsync(string fileName, object record);
    }

    public class DbOpenResult { public bool IsSuccess; public int ErrorCode; }
    public class DbFetchResult { public DbFetchStatus Status; public PolicyRecord Policy; public int ErrorCode; }
    public enum DbFetchStatus { Success, EndOfData, Error }
    public class DbInsertResult { public bool IsSuccess; public int ErrorCode; }
    public class FileOpenResult { public bool IsSuccess; public int ErrorCode; }
    public class FileWriteResult { public bool IsSuccess; public int ErrorCode; }
    public class AgentSearchResult { public bool IsSuccess; public AgentRecord Agent; public int ErrorCode; }

    public class PolicyRecord
    {
        public string PolicyNumber { get; set; }
        public string AgentCode { get; set; }
        public decimal PremiumAmount { get; set; }
        public string HolderState { get; set; }
    }

    public class AgentRecord
    {
        public string AgentCode { get; set; }
        public string AgentType { get; set; }
    }

    public class CustomerNotifyRecord
    {
        public static CustomerNotifyRecord FromPolicyAndAgent(PolicyRecord policy, AgentRecord agent, DateOnly processDate) => new CustomerNotifyRecord();
    }

    public class AgentNotifyRecord
    {
        public static AgentNotifyRecord FromPolicyAndAgent(PolicyRecord policy, AgentRecord agent, DateOnly processDate) => new AgentNotifyRecord();
    }

    public class NotifyReportRecord
    {
        public NotifyReportRecord(string line) { }
    }

    public class ReportLineBuilder
    {
        public string BuildAgentSummaryLine(string agentCode, int count, decimal premium) => $"AgentSummary:{agentCode},{count},{premium}";
        public string BuildStateSummaryLine(string state, int count, decimal premium) => $"StateSummary:{state},{count},{premium}";
        public string BuildGrandSummaryLine(int count, decimal premium) => $"GrandSummary:{count},{premium}";
        public string BuildStateHeaderLine(string state) => $"StateHeader:{state}";
        public IEnumerable<string> BuildAgentHeaderLines(AgentRecord agent) => new[] { $"AgentHeader:{agent.AgentCode}" };
        public IEnumerable<string> BuildPolicyHeaderLines() => new[] { "PolicyHeader" };
        public string BuildPolicyDetailLine(PolicyRecord policy) => $"PolicyDetail:{policy.PolicyNumber}";
        public string BuildFillerLine() => "";
    }
}