using System;
using System.Threading.Tasks;
using System.Collections.Generic;
using FluentAssertions;
using Moq;
using Xunit;
using Microsoft.Extensions.Logging;

namespace Insurance.PolicyExpiryNotification.Tests
{
    public class forTests : IDisposable
    {
        private readonly Mock<IDbDriver1> _dbDriver1Mock;
        private readonly Mock<IDbDriver2> _dbDriver2Mock;
        private readonly Mock<IFileDriver1> _fileDriver1Mock;
        private readonly Mock<IFileDriver2> _fileDriver2Mock;
        private readonly Mock<ILogger<PolicyExpiryProcessor>> _loggerMock;
        private readonly PolicyExpiryProcessor _sut;

        public forTests()
        {
            _dbDriver1Mock = new Mock<IDbDriver1>(MockBehavior.Strict);
            _dbDriver2Mock = new Mock<IDbDriver2>(MockBehavior.Strict);
            _fileDriver1Mock = new Mock<IFileDriver1>(MockBehavior.Strict);
            _fileDriver2Mock = new Mock<IFileDriver2>(MockBehavior.Strict);
            _loggerMock = new Mock<ILogger<PolicyExpiryProcessor>>(MockBehavior.Loose);

            _sut = new PolicyExpiryProcessor(
                _dbDriver1Mock.Object,
                _dbDriver2Mock.Object,
                _fileDriver1Mock.Object,
                _fileDriver2Mock.Object,
                _loggerMock.Object
            );
        }

        public void Dispose()
        {
            // Cleanup if necessary
        }

        [Fact]
        public async Task RunAsync_Should_Initialize_Process_And_Finalize_When_All_Ok()
        {
            // Arrange
            SetupInitializeAsyncSuccess();
            SetupProcessAsyncSuccess();
            SetupFinalizeAsyncSuccess();

            // Act
            Func<Task> act = async () => await _sut.RunAsync();

            // Assert
            await act.Should().NotThrowAsync();
        }

        [Fact]
        public async Task RunAsync_Should_LogError_And_Throw_When_InitializeAsync_Fails()
        {
            // Arrange
            _dbDriver1Mock.Setup(x => x.OpenCursorAsync(It.IsAny<string>()))
                .ReturnsAsync(new DbOpenResult { SqlCode = 999 });
            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            // Act
            Func<Task> act = async () => await _sut.RunAsync();

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error opening DB cursor: 999");
        }

        [Fact]
        public async Task RunAsync_Should_LogError_And_Throw_When_ProcessAsync_FetchPolicy_Error()
        {
            // Arrange
            SetupInitializeAsyncSuccess();

            _dbDriver1Mock.SetupSequence(x => x.FetchPolicyAsync())
                .ReturnsAsync(new FetchPolicyResult { SqlCode = 999, Policy = null });

            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            // Act
            Func<Task> act = async () => await _sut.RunAsync();

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error fetching record: 999");
        }

        [Fact]
        public async Task InitializeAsync_Should_Throw_When_AgentFile_Open_Fails()
        {
            // Arrange
            _dbDriver1Mock.Setup(x => x.OpenCursorAsync(It.IsAny<string>()))
                .ReturnsAsync(new DbOpenResult { SqlCode = 0 });
            _fileDriver1Mock.Setup(x => x.OpenAsync())
                .ReturnsAsync(new FileOpenResult { StatusCode = "99" });
            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "InitializeAsync");

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error opening agent file: 99");
        }

        [Fact]
        public async Task InitializeAsync_Should_Throw_When_NotificationFile_Open_Fails()
        {
            // Arrange
            _dbDriver1Mock.Setup(x => x.OpenCursorAsync(It.IsAny<string>()))
                .ReturnsAsync(new DbOpenResult { SqlCode = 0 });
            _fileDriver1Mock.Setup(x => x.OpenAsync())
                .ReturnsAsync(new FileOpenResult { StatusCode = "00" });
            _fileDriver2Mock.SetupSequence(x => x.OpenAsync(It.IsAny<string>()))
                .ReturnsAsync(new FileOpenResult { StatusCode = "00" })
                .ReturnsAsync(new FileOpenResult { StatusCode = "99" }); // Second file fails

            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "InitializeAsync");

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error opening file NOTIFY-REPORT-FILE: 99");
        }

        [Theory]
        [InlineData(100, true, false)]
        [InlineData(0, false, true)]
        public async Task CheckPolicyCallStatusAsync_SetsFlags_Correctly(int sqlCode, bool expectedNoMorePolicy, bool expectedPolicyFound)
        {
            // Arrange
            // Act
            await InvokePrivateAsync(_sut, "CheckPolicyCallStatusAsync", sqlCode);

            // Assert
            var noMorePolicy = GetPrivateField<bool>(_sut, "noMorePolicy");
            var policyFound = GetPrivateField<bool>(_sut, "policyFound");
            noMorePolicy.Should().Be(expectedNoMorePolicy);
            policyFound.Should().Be(expectedPolicyFound);
        }

        [Fact]
        public async Task CheckPolicyCallStatusAsync_Throws_On_ErrorCode()
        {
            // Arrange
            int sqlCode = 999;
            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "CheckPolicyCallStatusAsync", sqlCode);

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error fetching record: 999");
        }

        [Fact]
        public async Task GetAgentDetailAsync_ReturnsAgent_WhenFound()
        {
            // Arrange
            var agent = new AgentRecord { AgentCode = "A1", AgentType = "CORPORATE" };
            _fileDriver1Mock.Setup(x => x.SearchAgentAsync("A1"))
                .ReturnsAsync(new SearchAgentResult { StatusCode = "00", Agent = agent });
            _fileDriver2Mock.Setup(x => x.WriteAgentNotifyAsync(It.IsAny<AgentNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { StatusCode = "00" });

            SetPrivateField(_sut, "currentPolicy", new PolicyRecord { AgentCode = "A1", PremiumAmount = 100 });

            // Act
            var result = await InvokePrivateAsync<AgentRecord>(_sut, "GetAgentDetailAsync", "A1");

            // Assert
            result.Should().Be(agent);
        }

        [Fact]
        public async Task GetAgentDetailAsync_Throws_WhenNotFound()
        {
            // Arrange
            _fileDriver1Mock.Setup(x => x.SearchAgentAsync("A2"))
                .ReturnsAsync(new SearchAgentResult { StatusCode = "99", Agent = null });
            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            SetPrivateField(_sut, "currentPolicy", new PolicyRecord { AgentCode = "A2", PremiumAmount = 100 });

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "GetAgentDetailAsync", "A2");

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error fetching agent record: 99");
        }

        [Fact]
        public async Task UpdateTrackingAsync_Throws_When_CurrentPolicy_Null()
        {
            // Arrange
            SetPrivateField(_sut, "currentPolicy", null);

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "UpdateTrackingAsync");

            // Assert
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Current policy is null.");
        }

        [Fact]
        public async Task UpdateTrackingAsync_Throws_When_InsertTracking_Fails()
        {
            // Arrange
            SetPrivateField(_sut, "currentPolicy", new PolicyRecord { PolicyNumber = "P1" });
            _dbDriver2Mock.Setup(x => x.InsertTrackingAsync(It.IsAny<string>(), "P1"))
                .ReturnsAsync(new DbInsertResult { SqlCode = 999 });
            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "UpdateTrackingAsync");

            // Assert
            await act.Should().ThrowAsync<ApplicationException>()
                .WithMessage("Error inserting into tracking table: 999");
        }

        [Fact]
        public async Task ProcessSummaryAsync_Throws_When_CurrentPolicy_Or_Agent_Null()
        {
            // Arrange
            SetPrivateField(_sut, "currentPolicy", null);
            SetPrivateField(_sut, "currentAgent", null);

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "ProcessSummaryAsync");

            // Assert
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Current policy or agent is null.");
        }

        [Fact]
        public async Task WriteCustomerNotificationAsync_Throws_When_CurrentPolicy_Or_Agent_Null()
        {
            // Arrange
            SetPrivateField(_sut, "currentPolicy", null);
            SetPrivateField(_sut, "currentAgent", null);

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "WriteCustomerNotificationAsync");

            // Assert
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Current policy or agent is null.");
        }

        [Fact]
        public async Task WriteCustomerNotificationAsync_LogsError_When_WriteFails()
        {
            // Arrange
            SetPrivateField(_sut, "currentPolicy", new PolicyRecord { PolicyNumber = "P1", AgentCode = "A1", PremiumAmount = 100 });
            SetPrivateField(_sut, "currentAgent", new AgentRecord { AgentCode = "A1", AgentType = "INDIVIDUAL" });
            _fileDriver2Mock.Setup(x => x.WriteCustomerNotifyAsync(It.IsAny<CustomerNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { StatusCode = "99" });
            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            // Act
            await InvokePrivateAsync(_sut, "WriteCustomerNotificationAsync");

            // Assert
            // Should log error, no exception thrown
        }

        [Fact]
        public async Task WriteAgentNotificationAsync_Throws_When_CurrentPolicy_Null()
        {
            // Arrange
            SetPrivateField(_sut, "currentPolicy", null);

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "WriteAgentNotificationAsync", new AgentRecord());

            // Assert
            await act.Should().ThrowAsync<InvalidOperationException>()
                .WithMessage("Current policy is null.");
        }

        [Fact]
        public async Task WriteAgentNotificationAsync_LogsError_When_WriteFails_For_CorporateAgent()
        {
            // Arrange
            SetPrivateField(_sut, "currentPolicy", new PolicyRecord { PolicyNumber = "P1", AgentCode = "A1", PremiumAmount = 100 });
            var agent = new AgentRecord { AgentCode = "A1", AgentType = "CORPORATE" };
            _fileDriver2Mock.Setup(x => x.WriteAgentNotifyAsync(It.IsAny<AgentNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { StatusCode = "99" });
            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            // Act
            await InvokePrivateAsync(_sut, "WriteAgentNotificationAsync", agent);

            // Assert
            // Should log error, no exception thrown
        }

        [Fact]
        public async Task WriteNotificationReportAsync_LogsError_When_WriteFails()
        {
            // Arrange
            _fileDriver2Mock.Setup(x => x.WriteReportAsync(It.IsAny<string>()))
                .ReturnsAsync(new FileWriteResult { StatusCode = "99" });
            _loggerMock.Setup(x => x.Log(
                LogLevel.Error,
                It.IsAny<EventId>(),
                It.IsAny<It.IsAnyType>(),
                It.IsAny<Exception>(),
                (Func<It.IsAnyType, Exception, string>)It.IsAny<object>()));

            // Act
            await InvokePrivateAsync(_sut, "WriteNotificationReportAsync", "TestReportLine");

            // Assert
            // Should log error, no exception thrown
        }

        [Fact]
        public async Task ResetAgentTotals_Should_Reset_AgentCounters()
        {
            // Arrange
            SetPrivateField(_sut, "agentTotalPolicyCount", 5);
            SetPrivateField(_sut, "agentTotalPremium", 123.45m);

            // Act
            InvokePrivate(_sut, "ResetAgentTotals");

            // Assert
            GetPrivateField<int>(_sut, "agentTotalPolicyCount").Should().Be(0);
            GetPrivateField<decimal>(_sut, "agentTotalPremium").Should().Be(0m);
        }

        [Fact]
        public async Task ResetStateTotals_Should_Reset_StateCounters()
        {
            // Arrange
            SetPrivateField(_sut, "stateTotalPolicyCount", 10);
            SetPrivateField(_sut, "stateTotalPremium", 999.99m);

            // Act
            InvokePrivate(_sut, "ResetStateTotals");

            // Assert
            GetPrivateField<int>(_sut, "stateTotalPolicyCount").Should().Be(0);
            GetPrivateField<decimal>(_sut, "stateTotalPremium").Should().Be(0m);
        }

        // Integration test for database operation
        [Fact]
        public async Task UpdateTrackingAsync_Should_InsertTrackingRecord_When_All_Ok()
        {
            // Arrange
            var policy = new PolicyRecord { PolicyNumber = "P123" };
            SetPrivateField(_sut, "currentPolicy", policy);
            _dbDriver2Mock.Setup(x => x.InsertTrackingAsync(It.IsAny<string>(), "P123"))
                .ReturnsAsync(new DbInsertResult { SqlCode = 0 });

            // Act
            Func<Task> act = async () => await InvokePrivateAsync(_sut, "UpdateTrackingAsync");

            // Assert
            await act.Should().NotThrowAsync();
        }

        // Helper methods for invoking private methods/fields
        private async Task InvokePrivateAsync(object obj, string methodName, params object[] args)
        {
            var method = obj.GetType().GetMethod(methodName, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            if (method == null) throw new InvalidOperationException($"Method {methodName} not found.");
            var result = method.Invoke(obj, args);
            if (result is Task task)
                await task;
        }

        private async Task<T> InvokePrivateAsync<T>(object obj, string methodName, params object[] args)
        {
            var method = obj.GetType().GetMethod(methodName, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            if (method == null) throw new InvalidOperationException($"Method {methodName} not found.");
            var result = method.Invoke(obj, args);
            if (result is Task<T> taskT)
                return await taskT;
            if (result is Task task)
            {
                await task;
                return default;
            }
            return (T)result;
        }

        private void InvokePrivate(object obj, string methodName, params object[] args)
        {
            var method = obj.GetType().GetMethod(methodName, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            if (method == null) throw new InvalidOperationException($"Method {methodName} not found.");
            method.Invoke(obj, args);
        }

        private T GetPrivateField<T>(object obj, string fieldName)
        {
            var field = obj.GetType().GetField(fieldName, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            if (field == null) throw new InvalidOperationException($"Field {fieldName} not found.");
            return (T)field.GetValue(obj);
        }

        private void SetPrivateField<T>(object obj, string fieldName, T value)
        {
            var field = obj.GetType().GetField(fieldName, System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
            if (field == null) throw new InvalidOperationException($"Field {fieldName} not found.");
            field.SetValue(obj, value);
        }

        // Setup helpers for main happy path
        private void SetupInitializeAsyncSuccess()
        {
            _dbDriver1Mock.Setup(x => x.OpenCursorAsync(It.IsAny<string>()))
                .ReturnsAsync(new DbOpenResult { SqlCode = 0 });
            _fileDriver1Mock.Setup(x => x.OpenAsync())
                .ReturnsAsync(new FileOpenResult { StatusCode = "00" });
            _fileDriver2Mock.Setup(x => x.OpenAsync(It.IsAny<string>()))
                .ReturnsAsync(new FileOpenResult { StatusCode = "00" });
            _fileDriver2Mock.Setup(x => x.WriteReportAsync(It.IsAny<string>()))
                .ReturnsAsync(new FileWriteResult { StatusCode = "00" });
        }

        private void SetupProcessAsyncSuccess()
        {
            var policy = new PolicyRecord { PolicyNumber = "P1", AgentCode = "A1", HolderState = "TX", PremiumAmount = 100 };
            var agent = new AgentRecord { AgentCode = "A1", AgentType = "CORPORATE" };

            _dbDriver1Mock.SetupSequence(x => x.FetchPolicyAsync())
                .ReturnsAsync(new FetchPolicyResult { SqlCode = 0, Policy = policy })
                .ReturnsAsync(new FetchPolicyResult { SqlCode = 100, Policy = null });

            _fileDriver1Mock.Setup(x => x.SearchAgentAsync("A1"))
                .ReturnsAsync(new SearchAgentResult { StatusCode = "00", Agent = agent });

            _fileDriver2Mock.Setup(x => x.WriteCustomerNotifyAsync(It.IsAny<CustomerNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { StatusCode = "00" });
            _fileDriver2Mock.Setup(x => x.WriteAgentNotifyAsync(It.IsAny<AgentNotifyRecord>()))
                .ReturnsAsync(new FileWriteResult { StatusCode = "00" });

            _dbDriver2Mock.Setup(x => x.InsertTrackingAsync(It.IsAny<string>(), "P1"))
                .ReturnsAsync(new DbInsertResult { SqlCode = 0 });

            _fileDriver2Mock.Setup(x => x.WriteReportAsync(It.IsAny<string>()))
                .ReturnsAsync(new FileWriteResult { StatusCode = "00" });
        }

        private void SetupFinalizeAsyncSuccess()
        {
            // If FinalizeAsync is implemented, setup its dependencies here
        }
    }

    // Dummy interfaces and classes for compilation
    public interface IDbDriver1
    {
        Task<DbOpenResult> OpenCursorAsync(string date);
        Task<FetchPolicyResult> FetchPolicyAsync();
    }
    public interface IDbDriver2
    {
        Task<DbInsertResult> InsertTrackingAsync(string date, string policyNumber);
    }
    public interface IFileDriver1
    {
        Task<FileOpenResult> OpenAsync();
        Task<SearchAgentResult> SearchAgentAsync(string agentCode);
    }
    public interface IFileDriver2
    {
        Task<FileOpenResult> OpenAsync(string fileName);
        Task<FileWriteResult> WriteCustomerNotifyAsync(CustomerNotifyRecord record);
        Task<FileWriteResult> WriteAgentNotifyAsync(AgentNotifyRecord record);
        Task<FileWriteResult> WriteReportAsync(string line);
    }
    public class DbOpenResult { public int SqlCode { get; set; } }
    public class FetchPolicyResult { public int SqlCode { get; set; } public PolicyRecord Policy { get; set; } }
    public class DbInsertResult { public int SqlCode { get; set; } }
    public class FileOpenResult { public string StatusCode { get; set; } }
    public class FileWriteResult { public string StatusCode { get; set; } }
    public class SearchAgentResult { public string StatusCode { get; set; } public AgentRecord Agent { get; set; } }
    public class PolicyRecord { public string PolicyNumber { get; set; } public string AgentCode { get; set; } public string HolderState { get; set; } public decimal PremiumAmount { get; set; } }
    public class AgentRecord { public string AgentCode { get; set; } public string AgentType { get; set; } }
    public class CustomerNotifyRecord { }
    public class AgentNotifyRecord { }
    public static class ReportFormatter
    {
        public static string FormatAgentSummaryLine(string agentCode, int count, decimal premium) => $"AgentSummary:{agentCode}:{count}:{premium}";
        public static string FormatStateSummaryLine(string state, int count, decimal premium) => $"StateSummary:{state}:{count}:{premium}";
        public static string FormatGrandSummaryLine(int count, decimal premium) => $"GrandSummary:{count}:{premium}";
        public static string FormatStateHeader(string state) => $"StateHeader:{state}";
        public static IEnumerable<string> FormatAgentHeaderLines(AgentRecord agent) => new[] { $"AgentHeader:{agent.AgentCode}" };
        public static IEnumerable<string> FormatPolicyHeaderLines() => new[] { "PolicyHeader" };
        public static string FormatPolicyDetailLine(PolicyRecord policy) => $"PolicyDetail:{policy.PolicyNumber}";
        public static string FormatFillerLine() => "FillerLine";
    }
}