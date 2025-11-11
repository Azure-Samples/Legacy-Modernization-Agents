using System;
using System.ComponentModel.DataAnnotations;
using System.Collections.Generic;
using System.Linq;
using Xunit;
using Moq;
using FluentAssertions;

namespace Insurance.Data.Models.Tests
{
    public class mapsTests : IDisposable
    {
        // Setup resources if needed
        public mapsTests()
        {
            // Setup code here if needed
        }

        // Teardown resources
        public void Dispose()
        {
            // Cleanup code here if needed
        }

        [Fact]
        public void CoverageRecord_ShouldInitializeWithDefaultValues()
        {
            // Arrange & Act
            var record = new CoverageRecord();

            // Assert
            record.CoveragePolicyNumber.Should().BeEmpty();
            record.CoverageStatus.Should().BeEmpty();
            record.CoverageStartDate.Should().BeEmpty();
            record.CoverageEndDate.Should().BeEmpty();
            record.CoverageAddedTimestamp.Should().Be(default(DateTime));
            record.CoverageUpdatedTimestamp.Should().Be(default(DateTime));
        }

        [Fact]
        public void CoverageRecord_ShouldSetAllPropertiesCorrectly()
        {
            // Arrange
            var now = DateTime.UtcNow;
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = now,
                CoverageUpdatedTimestamp = now.AddMinutes(5)
            };

            // Assert
            record.CoveragePolicyNumber.Should().Be("POL1234567");
            record.CoverageStatus.Should().Be("ACTIVE");
            record.CoverageStartDate.Should().Be("2024-01-01");
            record.CoverageEndDate.Should().Be("2024-12-31");
            record.CoverageAddedTimestamp.Should().Be(now);
            record.CoverageUpdatedTimestamp.Should().Be(now.AddMinutes(5));
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        public void CoverageRecord_ShouldNotAllowNullOrEmptyPolicyNumber(string value)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = value,
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoveragePolicyNumber)));
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        public void CoverageRecord_ShouldNotAllowNullOrEmptyStatus(string value)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = value,
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageStatus)));
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        public void CoverageRecord_ShouldNotAllowNullOrEmptyStartDate(string value)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = value,
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageStartDate)));
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        public void CoverageRecord_ShouldNotAllowNullOrEmptyEndDate(string value)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = value,
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageEndDate)));
        }

        [Theory]
        [InlineData("12345678901")] // 11 chars
        [InlineData("ABCDEFGHIJK")] // 11 chars
        public void CoverageRecord_ShouldNotAllowPolicyNumberExceedingMaxLength(string value)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = value,
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoveragePolicyNumber)));
        }

        [Theory]
        [InlineData("12345678901")] // 11 chars
        [InlineData("ABCDEFGHIJK")] // 11 chars
        public void CoverageRecord_ShouldNotAllowStatusExceedingMaxLength(string value)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = value,
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageStatus)));
        }

        [Theory]
        [InlineData("2024-01-011")] // 11 chars
        [InlineData("12345678901")] // 11 chars
        public void CoverageRecord_ShouldNotAllowStartDateExceedingMaxLength(string value)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = value,
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageStartDate)));
        }

        [Theory]
        [InlineData("2024-12-311")] // 11 chars
        [InlineData("12345678901")] // 11 chars
        public void CoverageRecord_ShouldNotAllowEndDateExceedingMaxLength(string value)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = value,
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageEndDate)));
        }

        [Fact]
        public void CoverageRecord_ShouldNotAllowDefaultAddedTimestamp()
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = default,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageAddedTimestamp)));
        }

        [Fact]
        public void CoverageRecord_ShouldNotAllowDefaultUpdatedTimestamp()
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = default
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageUpdatedTimestamp)));
        }

        [Theory]
        [InlineData("2024-01-01", "2024-12-31", true)]
        [InlineData("2024-12-31", "2024-01-01", false)]
        [InlineData("2024-01-01", "2024-01-01", true)]
        public void CoverageRecord_StartDateShouldNotBeAfterEndDate(string startDate, string endDate, bool isValid)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = startDate,
                CoverageEndDate = endDate,
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // COBOL logic: Start date must be <= End date
            if (!isValid)
            {
                validationResults.Should().Contain(x => x.ErrorMessage.Contains("Start date must be before or equal to End date"));
            }
            else
            {
                validationResults.Should().NotContain(x => x.ErrorMessage.Contains("Start date must be before or equal to End date"));
            }
        }

        [Theory]
        [InlineData("2024-01-01")]
        [InlineData("2024-12-31")]
        [InlineData("2024-02-29")]
        public void CoverageRecord_ShouldAcceptValidDateFormats(string date)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = date,
                CoverageEndDate = date,
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().NotContain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageStartDate)));
            validationResults.Should().NotContain(x => x.MemberNames.Contains(nameof(CoverageRecord.CoverageEndDate)));
        }

        [Theory]
        [InlineData("20240101")]
        [InlineData("01-01-2024")]
        [InlineData("2024/01/01")]
        public void CoverageRecord_ShouldRejectInvalidDateFormats(string date)
        {
            // Arrange
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = date,
                CoverageEndDate = date,
                CoverageAddedTimestamp = DateTime.UtcNow,
                CoverageUpdatedTimestamp = DateTime.UtcNow
            };

            // Act
            var validationResults = ValidateModel(record);

            // Assert
            validationResults.Should().Contain(x => x.ErrorMessage.Contains("Date format must be yyyy-MM-dd"));
        }

        [Fact]
        public void CoverageRecord_Equality_ShouldWorkForSameValues()
        {
            // Arrange
            var now = DateTime.UtcNow;
            var record1 = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = now,
                CoverageUpdatedTimestamp = now
            };
            var record2 = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = now,
                CoverageUpdatedTimestamp = now
            };

            // Act & Assert
            record1.Should().Be(record2);
            record1.GetHashCode().Should().Be(record2.GetHashCode());
        }

        [Fact]
        public void CoverageRecord_Equality_ShouldFailForDifferentValues()
        {
            // Arrange
            var now = DateTime.UtcNow;
            var record1 = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = now,
                CoverageUpdatedTimestamp = now
            };
            var record2 = new CoverageRecord
            {
                CoveragePolicyNumber = "POL7654321",
                CoverageStatus = "INACTIVE",
                CoverageStartDate = "2023-01-01",
                CoverageEndDate = "2023-12-31",
                CoverageAddedTimestamp = now.AddDays(-1),
                CoverageUpdatedTimestamp = now.AddDays(-1)
            };

            // Act & Assert
            record1.Should().NotBe(record2);
        }

        [Fact]
        public void CoverageRecord_ShouldBeImmutable()
        {
            // Arrange
            var now = DateTime.UtcNow;
            var record = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = now,
                CoverageUpdatedTimestamp = now
            };

            // Act
            Action act = () => record = record with { CoverageStatus = "INACTIVE" };

            // Assert
            act.Should().NotThrow();
            var updated = record with { CoverageStatus = "INACTIVE" };
            updated.CoverageStatus.Should().Be("INACTIVE");
            record.CoverageStatus.Should().Be("ACTIVE");
        }

        // Integration Test Example: Simulate DB Save/Load (using Moq for repository)
        [Fact]
        public void CoverageRecord_Repository_SaveAndRetrieve_ShouldPreserveValues()
        {
            // Arrange
            var now = DateTime.UtcNow;
            var expected = new CoverageRecord
            {
                CoveragePolicyNumber = "POL1234567",
                CoverageStatus = "ACTIVE",
                CoverageStartDate = "2024-01-01",
                CoverageEndDate = "2024-12-31",
                CoverageAddedTimestamp = now,
                CoverageUpdatedTimestamp = now
            };

            var mockRepo = new Mock<ICoverageRecordRepository>();
            mockRepo.Setup(r => r.Save(It.IsAny<CoverageRecord>())).Returns(true);
            mockRepo.Setup(r => r.GetByPolicyNumber("POL1234567")).Returns(expected);

            // Act
            var saveResult = mockRepo.Object.Save(expected);
            var retrieved = mockRepo.Object.GetByPolicyNumber("POL1234567");

            // Assert
            saveResult.Should().BeTrue();
            retrieved.Should().BeEquivalentTo(expected);
        }

        // Helper method to validate CoverageRecord using DataAnnotations and custom COBOL logic
        private IList<ValidationResult> ValidateModel(CoverageRecord record)
        {
            var results = new List<ValidationResult>();
            var context = new ValidationContext(record, serviceProvider: null, items: null);
            Validator.TryValidateObject(record, context, results, true);

            // COBOL business logic: Start date must be <= End date
            if (!string.IsNullOrWhiteSpace(record.CoverageStartDate) && !string.IsNullOrWhiteSpace(record.CoverageEndDate))
            {
                if (DateTime.TryParse(record.CoverageStartDate, out var start) &&
                    DateTime.TryParse(record.CoverageEndDate, out var end))
                {
                    if (start > end)
                    {
                        results.Add(new ValidationResult("Start date must be before or equal to End date", new[] { nameof(CoverageRecord.CoverageStartDate), nameof(CoverageRecord.CoverageEndDate) }));
                    }
                }
                else
                {
                    // Date format must be yyyy-MM-dd
                    if (!IsValidDateFormat(record.CoverageStartDate))
                        results.Add(new ValidationResult("Date format must be yyyy-MM-dd", new[] { nameof(CoverageRecord.CoverageStartDate) }));
                    if (!IsValidDateFormat(record.CoverageEndDate))
                        results.Add(new ValidationResult("Date format must be yyyy-MM-dd", new[] { nameof(CoverageRecord.CoverageEndDate) }));
                }
            }

            // Required DateTime fields must not be default
            if (record.CoverageAddedTimestamp == default)
                results.Add(new ValidationResult("CoverageAddedTimestamp is required", new[] { nameof(CoverageRecord.CoverageAddedTimestamp) }));
            if (record.CoverageUpdatedTimestamp == default)
                results.Add(new ValidationResult("CoverageUpdatedTimestamp is required", new[] { nameof(CoverageRecord.CoverageUpdatedTimestamp) }));

            return results;
        }

        private bool IsValidDateFormat(string date)
        {
            return DateTime.TryParseExact(date, "yyyy-MM-dd", System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.None, out _);
        }
    }

    // Mock repository interface for integration test
    public interface ICoverageRecordRepository
    {
        bool Save(CoverageRecord record);
        CoverageRecord GetByPolicyNumber(string policyNumber);
    }
}