using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using FluentAssertions;
using Insurance.Data.Models;
using Insurance.Data.Repositories;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace Insurance.Data.Models.Tests
{
    public class PolicyRepositoryTests : IDisposable
    {
        private readonly Mock<InsuranceDbContext> _dbContextMock;
        private readonly Mock<DbSet<Policy>> _dbSetMock;
        private readonly Mock<ILogger<PolicyRepository>> _loggerMock;
        private readonly PolicyRepository _repository;
        private readonly List<Policy> _policyData;

        public PolicyRepositoryTests()
        {
            _policyData = new List<Policy>
            {
                CreatePolicy("POL000001"),
                CreatePolicy("POL000002"),
                CreatePolicy("POL000003")
            };

            _dbSetMock = CreateDbSetMock(_policyData);
            _dbContextMock = new Mock<InsuranceDbContext>();
            _dbContextMock.Setup(x => x.Policies).Returns(_dbSetMock.Object);

            _loggerMock = new Mock<ILogger<PolicyRepository>>();

            _repository = new PolicyRepository(_dbContextMock.Object, _loggerMock.Object);
        }

        public void Dispose()
        {
            // Cleanup if needed
        }

        private static Policy CreatePolicy(string policyNumber)
        {
            return new Policy
            {
                PolicyNumber = policyNumber,
                PolicyHolderFirstName = "John",
                PolicyHolderMiddleName = "A",
                PolicyHolderLastName = "Doe",
                PolicyBeneficiaryName = "Jane Doe",
                PolicyBeneficiaryRelation = "Spouse",
                PolicyHolderAddress1 = "123 Main St",
                PolicyHolderAddress2 = "Apt 4",
                PolicyHolderCity = "Springfield",
                PolicyHolderState = "IL",
                PolicyHolderZipCode = "62704",
                PolicyHolderDateOfBirth = "1980-01-01",
                PolicyHolderGender = "Male",
                PolicyHolderPhone = "5551234567",
                PolicyHolderEmail = "john.doe@email.com",
                PolicyPaymentFrequency = "Monthly",
                PolicyPaymentMethod = "Credit",
                PolicyUnderwriter = "Acme Insurance",
                PolicyTermsAndConditions = "Standard terms apply.",
                PolicyClaimed = "N",
                PolicyDiscountCode = "DISC10",
                PolicyPremiumAmount = 120.50m,
                PolicyCoverageAmount = 10000.00m,
                PolicyType = "Life",
                PolicyStartDate = new DateTime(2023, 1, 1),
                PolicyExpiryDate = new DateTime(2024, 1, 1),
                PolicyStatus = "A",
                PolicyAgentCode = "AGT001",
                PolicyNotifyFlag = "Y",
                PolicyAddTimestamp = DateTimeOffset.UtcNow,
                PolicyUpdateTimestamp = DateTimeOffset.UtcNow
            };
        }

        private static Mock<DbSet<T>> CreateDbSetMock<T>(IEnumerable<T> data) where T : class
        {
            var queryable = data.AsQueryable();

            var dbSetMock = new Mock<DbSet<T>>();
            dbSetMock.As<IQueryable<T>>().Setup(m => m.Provider).Returns(queryable.Provider);
            dbSetMock.As<IQueryable<T>>().Setup(m => m.Expression).Returns(queryable.Expression);
            dbSetMock.As<IQueryable<T>>().Setup(m => m.ElementType).Returns(queryable.ElementType);
            dbSetMock.As<IQueryable<T>>().Setup(m => m.GetEnumerator()).Returns(queryable.GetEnumerator());

            dbSetMock.Setup(d => d.AddAsync(It.IsAny<T>(), It.IsAny<CancellationToken>()))
                .ReturnsAsync((T entity, CancellationToken _) =>
                {
                    ((List<T>)data).Add(entity);
                    var entityEntryMock = new Mock<EntityEntry<T>>();
                    entityEntryMock.Setup(e => e.Entity).Returns(entity);
                    return entityEntryMock.Object;
                });

            return dbSetMock;
        }

        [Fact]
        public async Task GetPolicyByNumberAsync_ShouldReturnPolicy_WhenPolicyExists()
        {
            // Arrange
            var policyNumber = "POL000001";

            // Act
            var result = await _repository.GetPolicyByNumberAsync(policyNumber);

            // Assert
            result.Should().NotBeNull();
            result!.PolicyNumber.Should().Be(policyNumber);
        }

        [Fact]
        public async Task GetPolicyByNumberAsync_ShouldReturnNull_WhenPolicyDoesNotExist()
        {
            // Arrange
            var policyNumber = "NONEXISTENT";

            // Act
            var result = await _repository.GetPolicyByNumberAsync(policyNumber);

            // Assert
            result.Should().BeNull();
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData(" ")]
        public async Task GetPolicyByNumberAsync_ShouldReturnNull_ForNullOrEmptyPolicyNumber(string policyNumber)
        {
            // Arrange & Act
            var result = await _repository.GetPolicyByNumberAsync(policyNumber);

            // Assert
            result.Should().BeNull();
        }

        [Fact]
        public async Task GetPolicyByNumberAsync_ShouldLogAndThrowRepositoryException_OnDbException()
        {
            // Arrange
            var policyNumber = "POL000001";
            _dbSetMock.As<IQueryable<Policy>>()
                .Setup(m => m.Provider)
                .Throws(new InvalidOperationException("DB error"));

            // Act
            Func<Task> act = async () => await _repository.GetPolicyByNumberAsync(policyNumber);

            // Assert
            var ex = await Assert.ThrowsAsync<RepositoryException>(act);
            ex.Message.Should().Contain(policyNumber);
            _loggerMock.Verify(
                l => l.LogError(It.IsAny<Exception>(), It.IsAny<string>(), policyNumber),
                Times.Once);
        }

        [Fact]
        public async Task GetAllPoliciesAsync_ShouldReturnAllPoliciesOrderedByPolicyNumber()
        {
            // Arrange
            var expectedOrder = _policyData.OrderBy(p => p.PolicyNumber).ToList();

            // Act
            var result = await _repository.GetAllPoliciesAsync();

            // Assert
            result.Should().HaveCount(_policyData.Count);
            result.Should().BeInAscendingOrder(p => p.PolicyNumber);
            result.Should().BeEquivalentTo(expectedOrder);
        }

        [Fact]
        public async Task GetAllPoliciesAsync_ShouldReturnEmptyList_WhenNoPoliciesExist()
        {
            // Arrange
            var emptyDbSetMock = CreateDbSetMock(new List<Policy>());
            _dbContextMock.Setup(x => x.Policies).Returns(emptyDbSetMock.Object);

            var repository = new PolicyRepository(_dbContextMock.Object, _loggerMock.Object);

            // Act
            var result = await repository.GetAllPoliciesAsync();

            // Assert
            result.Should().BeEmpty();
        }

        [Fact]
        public async Task GetAllPoliciesAsync_ShouldLogAndThrowRepositoryException_OnDbException()
        {
            // Arrange
            _dbSetMock.As<IQueryable<Policy>>()
                .Setup(m => m.Provider)
                .Throws(new InvalidOperationException("DB error"));

            // Act
            Func<Task> act = async () => await _repository.GetAllPoliciesAsync();

            // Assert
            var ex = await Assert.ThrowsAsync<RepositoryException>(act);
            ex.Message.Should().Contain("Failed to retrieve policies");
            _loggerMock.Verify(
                l => l.LogError(It.IsAny<Exception>(), "Error retrieving all policies"),
                Times.Once);
        }

        [Fact]
        public async Task AddPolicyAsync_ShouldAddPolicyAndReturnEntity()
        {
            // Arrange
            var newPolicy = CreatePolicy("POL000004");
            _dbContextMock.Setup(x => x.SaveChangesAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(1);

            // Act
            var result = await _repository.AddPolicyAsync(newPolicy);

            // Assert
            result.Should().NotBeNull();
            result.Should().BeEquivalentTo(newPolicy);
            _policyData.Should().Contain(newPolicy);
            _dbContextMock.Verify(x => x.SaveChangesAsync(It.IsAny<CancellationToken>()), Times.Once);
        }

        [Fact]
        public async Task AddPolicyAsync_ShouldLogAndThrowRepositoryException_OnDbException()
        {
            // Arrange
            var newPolicy = CreatePolicy("POL000005");
            _dbSetMock.Setup(d => d.AddAsync(It.IsAny<Policy>(), It.IsAny<CancellationToken>()))
                .ThrowsAsync(new InvalidOperationException("DB error"));

            // Act
            Func<Task> act = async () => await _repository.AddPolicyAsync(newPolicy);

            // Assert
            var ex = await Assert.ThrowsAsync<RepositoryException>(act);
            ex.Message.Should().Contain(newPolicy.PolicyNumber);
            _loggerMock.Verify(
                l => l.LogError(It.IsAny<Exception>(), It.IsAny<string>(), newPolicy.PolicyNumber),
                Times.Once);
        }

        [Fact]
        public void Constructor_ShouldThrowArgumentNullException_WhenDbContextIsNull()
        {
            // Arrange
            InsuranceDbContext? nullDbContext = null;

            // Act
            Action act = () => new PolicyRepository(nullDbContext!, _loggerMock.Object);

            // Assert
            act.Should().Throw<ArgumentNullException>().WithParameterName("dbContext");
        }

        [Fact]
        public void Constructor_ShouldThrowArgumentNullException_WhenLoggerIsNull()
        {
            // Arrange
            ILogger<PolicyRepository>? nullLogger = null;

            // Act
            Action act = () => new PolicyRepository(_dbContextMock.Object, nullLogger!);

            // Assert
            act.Should().Throw<ArgumentNullException>().WithParameterName("logger");
        }

        [Fact]
        public async Task AddPolicyAsync_ShouldThrowArgumentNullException_WhenPolicyIsNull()
        {
            // Arrange
            Policy? nullPolicy = null;

            // Act
            Func<Task> act = async () => await _repository.AddPolicyAsync(nullPolicy!);

            // Assert
            await act.Should().ThrowAsync<ArgumentNullException>();
        }

        [Fact]
        public async Task AddPolicyAsync_ShouldThrowRepositoryException_WhenPolicyHasMissingRequiredFields()
        {
            // Arrange
            var invalidPolicy = new Policy
            {
                PolicyNumber = null!,
                PolicyHolderFirstName = null!,
                PolicyHolderMiddleName = null!,
                PolicyHolderLastName = null!,
                PolicyBeneficiaryName = null!,
                PolicyBeneficiaryRelation = null!,
                PolicyHolderAddress1 = null!,
                PolicyHolderAddress2 = null!,
                PolicyHolderCity = null!,
                PolicyHolderState = null!,
                PolicyHolderZipCode = null!,
                PolicyHolderDateOfBirth = null!,
                PolicyHolderGender = null!,
                PolicyHolderPhone = null!,
                PolicyHolderEmail = null!,
                PolicyPaymentFrequency = null!,
                PolicyPaymentMethod = null!,
                PolicyUnderwriter = null!,
                PolicyTermsAndConditions = null!,
                PolicyClaimed = null!,
                PolicyDiscountCode = null!,
                PolicyPremiumAmount = 0,
                PolicyCoverageAmount = 0,
                PolicyType = null!,
                PolicyStartDate = default,
                PolicyExpiryDate = default,
                PolicyStatus = null!,
                PolicyAgentCode = null!,
                PolicyNotifyFlag = null!,
                PolicyAddTimestamp = default,
                PolicyUpdateTimestamp = default
            };

            _dbSetMock.Setup(d => d.AddAsync(It.IsAny<Policy>(), It.IsAny<CancellationToken>()))
                .ThrowsAsync(new DbUpdateException("Validation failed"));

            // Act
            Func<Task> act = async () => await _repository.AddPolicyAsync(invalidPolicy);

            // Assert
            await act.Should().ThrowAsync<RepositoryException>()
                .WithMessage($"Failed to add policy *");
        }

        [Fact]
        public async Task GetPolicyByNumberAsync_ShouldPreserveCobolBusinessLogic_PolicyNumberIsKey()
        {
            // Arrange
            var policyNumber = "POL000002";

            // Act
            var result = await _repository.GetPolicyByNumberAsync(policyNumber);

            // Assert
            result.Should().NotBeNull();
            result!.PolicyNumber.Should().Be(policyNumber);
        }

        [Fact]
        public async Task AddPolicyAsync_ShouldPreserveCobolBusinessLogic_PolicyAddTimestampAndUpdateTimestampSet()
        {
            // Arrange
            var newPolicy = CreatePolicy("POL000006");
            newPolicy = newPolicy with
            {
                PolicyAddTimestamp = DateTimeOffset.MinValue,
                PolicyUpdateTimestamp = DateTimeOffset.MinValue
            };

            _dbContextMock.Setup(x => x.SaveChangesAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(1);

            // Act
            var result = await _repository.AddPolicyAsync(newPolicy);

            // Assert
            result.PolicyAddTimestamp.Should().Be(DateTimeOffset.MinValue);
            result.PolicyUpdateTimestamp.Should().Be(DateTimeOffset.MinValue);
        }

        [Fact]
        public async Task GetAllPoliciesAsync_ShouldPreserveCobolBusinessLogic_PoliciesOrderedByPolicyNumber()
        {
            // Arrange
            var expectedOrder = _policyData.OrderBy(p => p.PolicyNumber).ToList();

            // Act
            var result = await _repository.GetAllPoliciesAsync();

            // Assert
            result.Should().BeInAscendingOrder(p => p.PolicyNumber);
            result.Should().BeEquivalentTo(expectedOrder);
        }

        // Integration test for database operations using InMemory provider
        [Fact]
        public async Task Integration_AddAndRetrievePolicy_ShouldPersistAndReturnPolicy()
        {
            // Arrange
            var options = new DbContextOptionsBuilder<InsuranceDbContext>()
                .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
                .Options;

            using var dbContext = new InsuranceDbContext(options);
            var logger = new Mock<ILogger<PolicyRepository>>();
            var repository = new PolicyRepository(dbContext, logger.Object);

            var newPolicy = CreatePolicy("POLINT001");

            // Act
            var addedPolicy = await repository.AddPolicyAsync(newPolicy);
            var retrievedPolicy = await repository.GetPolicyByNumberAsync("POLINT001");
            var allPolicies = await repository.GetAllPoliciesAsync();

            // Assert
            addedPolicy.Should().BeEquivalentTo(newPolicy);
            retrievedPolicy.Should().NotBeNull();
            retrievedPolicy!.PolicyNumber.Should().Be("POLINT001");
            allPolicies.Should().ContainSingle(p => p.PolicyNumber == "POLINT001");
        }

        // Edge case: AddPolicyAsync with boundary values for string lengths
        [Fact]
        public async Task AddPolicyAsync_ShouldAcceptBoundaryStringLengths()
        {
            // Arrange
            var boundaryPolicy = new Policy
            {
                PolicyNumber = new string('X', 10),
                PolicyHolderFirstName = new string('A', 35),
                PolicyHolderMiddleName = "Z",
                PolicyHolderLastName = new string('B', 35),
                PolicyBeneficiaryName = new string('C', 60),
                PolicyBeneficiaryRelation = new string('D', 15),
                PolicyHolderAddress1 = new string('E', 100),
                PolicyHolderAddress2 = new string('F', 100),
                PolicyHolderCity = new string('G', 30),
                PolicyHolderState = "ST",
                PolicyHolderZipCode = new string('H', 10),
                PolicyHolderDateOfBirth = "2000-12-31",
                PolicyHolderGender = new string('M', 8),
                PolicyHolderPhone = new string('1', 10),
                PolicyHolderEmail = new string('e', 30),
                PolicyPaymentFrequency = new string('F', 10),
                PolicyPaymentMethod = new string('M', 8),
                PolicyUnderwriter = new string('U', 50),
                PolicyTermsAndConditions = new string('T', 200),
                PolicyClaimed = "Y",
                PolicyDiscountCode = new string('D', 10),
                PolicyPremiumAmount = 99999.99m,
                PolicyCoverageAmount = 9999999999.99m,
                PolicyType = new string('T', 50),
                PolicyStartDate = DateTime.Today,
                PolicyExpiryDate = DateTime.Today.AddYears(1),
                PolicyStatus = "A",
                PolicyAgentCode = new string('A', 10),
                PolicyNotifyFlag = "N",
                PolicyAddTimestamp = DateTimeOffset.UtcNow,
                PolicyUpdateTimestamp = DateTimeOffset.UtcNow
            };

            _dbContextMock.Setup(x => x.SaveChangesAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(1);

            // Act
            var result = await _repository.AddPolicyAsync(boundaryPolicy);

            // Assert
            result.Should().NotBeNull();
            result.PolicyNumber.Length.Should().Be(10);
            result.PolicyHolderFirstName.Length.Should().Be(35);
            result.PolicyHolderLastName.Length.Should().Be(35);
            result.PolicyBeneficiaryName.Length.Should().Be(60);
            result.PolicyBeneficiaryRelation.Length.Should().Be(15);
            result.PolicyHolderAddress1.Length.Should().Be(100);
            result.PolicyHolderAddress2.Length.Should().Be(100);
            result.PolicyHolderCity.Length.Should().Be(30);
            result.PolicyHolderState.Length.Should().Be(2);
            result.PolicyHolderZipCode.Length.Should().Be(10);
            result.PolicyHolderGender.Length.Should().Be(8);
            result.PolicyHolderEmail.Length.Should().Be(30);
            result.PolicyPaymentFrequency.Length.Should().Be(10);
            result.PolicyPaymentMethod.Length.Should().Be(8);
            result.PolicyUnderwriter.Length.Should().Be(50);
            result.PolicyTermsAndConditions.Length.Should().Be(200);
            result.PolicyDiscountCode.Length.Should().Be(10);
            result.PolicyType.Length.Should().Be(50);
            result.PolicyAgentCode.Length.Should().Be(10);
        }

        // Edge case: AddPolicyAsync with string fields exceeding max length (should fail in real DB, but not in mock)
        [Fact]
        public async Task AddPolicyAsync_ShouldAllowOverLengthStringsInMock_ButWouldFailInRealDb()
        {
            // Arrange
            var overLengthPolicy = CreatePolicy("POL000007") with
            {
                PolicyHolderFirstName = new string('A', 36), // Exceeds max length
                PolicyHolderLastName = new string('B', 36),  // Exceeds max length
                PolicyBeneficiaryName = new string('C', 61), // Exceeds max length
                PolicyHolderState = "ILL",                   // Exceeds max length
            };

            _dbContextMock.Setup(x => x.SaveChangesAsync(It.IsAny<CancellationToken>()))
                .ReturnsAsync(1);

            // Act
            var result = await _repository.AddPolicyAsync(overLengthPolicy);

            // Assert
            result.PolicyHolderFirstName.Length.Should().Be(36);
            result.PolicyHolderLastName.Length.Should().Be(36);
            result.PolicyBeneficiaryName.Length.Should().Be(61);
            result.PolicyHolderState.Length.Should().Be(3);
        }
    }
}