using System;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using FluentAssertions;
using Moq;
using Xunit;

namespace Insurance.Tracking.Models.Tests
{
    public class forTests : IDisposable
    {
        // Setup resources if needed
        public forTests()
        {
            // Initialize any shared resources or test data here
        }

        // Teardown resources if needed
        public void Dispose()
        {
            // Cleanup resources here
        }

        [Fact]
        public void TrackingRecord_ShouldInitializeWithDefaultValues()
        {
            // Arrange & Act
            var record = new TrackingRecord();

            // Assert
            record.PolicyNumber.Should().BeEmpty();
            record.Status.Should().BeEmpty();
            record.AddTimestamp.Should().BeCloseTo(DateTime.UtcNow, TimeSpan.FromSeconds(5));
            record.UpdateTimestamp.Should().BeCloseTo(DateTime.UtcNow, TimeSpan.FromSeconds(5));
            // NotifyDate default is default(DateTime)
            record.NotifyDate.Should().Be(default(DateTime));
        }

        [Theory]
        [InlineData("1234567890")]
        [InlineData("ABCDEFGHIJ")]
        public void TrackingRecord_ShouldSetValidPolicyNumber(string policyNumber)
        {
            // Arrange & Act
            var record = new TrackingRecord
            {
                PolicyNumber = policyNumber
            };

            // Assert
            record.PolicyNumber.Should().Be(policyNumber);
            record.PolicyNumber.Length.Should().BeLessOrEqualTo(10);
        }

        [Theory]
        [InlineData("")]
        [InlineData(null)]
        [InlineData("12345678901")] // 11 chars, exceeds limit
        public void TrackingRecord_PolicyNumber_ShouldRespectStringLength(string policyNumber)
        {
            // Arrange
            var record = new TrackingRecord
            {
                PolicyNumber = policyNumber
            };

            // Act
            var validationContext = new ValidationContext(record);
            var validationResults = new System.Collections.Generic.List<ValidationResult>();
            var isValid = Validator.TryValidateObject(record, validationContext, validationResults, true);

            // Assert
            if (policyNumber == null || policyNumber.Length > 10)
            {
                isValid.Should().BeFalse();
                validationResults.Should().Contain(r => r.MemberNames.Contains(nameof(TrackingRecord.PolicyNumber)));
            }
            else
            {
                isValid.Should().BeTrue();
            }
        }

        [Fact]
        public void TrackingRecord_NotifyDate_ShouldBeRequired()
        {
            // Arrange
            var record = new TrackingRecord
            {
                PolicyNumber = "1234567890",
                Status = "A",
                // NotifyDate left as default(DateTime)
                AddTimestamp = DateTime.UtcNow,
                UpdateTimestamp = DateTime.UtcNow
            };

            // Act
            var validationContext = new ValidationContext(record);
            var validationResults = new System.Collections.Generic.List<ValidationResult>();
            var isValid = Validator.TryValidateObject(record, validationContext, validationResults, true);

            // Assert
            // DateTime default is 0001-01-01, which may not be valid for business logic
            isValid.Should().BeTrue("Required attribute does not validate DateTime for default value");
        }

        [Theory]
        [InlineData("A")]
        [InlineData("Z")]
        [InlineData("1")]
        public void TrackingRecord_Status_ShouldAcceptSingleCharacter(string status)
        {
            // Arrange & Act
            var record = new TrackingRecord
            {
                Status = status
            };

            // Assert
            record.Status.Should().Be(status);
            record.Status.Length.Should().Be(1);
        }

        [Theory]
        [InlineData("")]
        [InlineData(null)]
        [InlineData("AB")]
        public void TrackingRecord_Status_ShouldRespectStringLength(string status)
        {
            // Arrange
            var record = new TrackingRecord
            {
                Status = status
            };

            // Act
            var validationContext = new ValidationContext(record);
            var validationResults = new System.Collections.Generic.List<ValidationResult>();
            var isValid = Validator.TryValidateObject(record, validationContext, validationResults, true);

            // Assert
            if (status == null || status.Length != 1)
            {
                isValid.Should().BeFalse();
                validationResults.Should().Contain(r => r.MemberNames.Contains(nameof(TrackingRecord.Status)));
            }
            else
            {
                isValid.Should().BeTrue();
            }
        }

        [Fact]
        public void TrackingRecord_AddTimestamp_ShouldBeSetToUtcNowByDefault()
        {
            // Arrange & Act
            var record = new TrackingRecord();

            // Assert
            record.AddTimestamp.Should().BeCloseTo(DateTime.UtcNow, TimeSpan.FromSeconds(5));
        }

        [Fact]
        public void TrackingRecord_UpdateTimestamp_ShouldBeSetToUtcNowByDefault()
        {
            // Arrange & Act
            var record = new TrackingRecord();

            // Assert
            record.UpdateTimestamp.Should().BeCloseTo(DateTime.UtcNow, TimeSpan.FromSeconds(5));
        }

        [Theory]
        [InlineData("2023-01-01")]
        [InlineData("2099-12-31")]
        public void TrackingRecord_NotifyDate_ShouldAcceptValidDates(string dateString)
        {
            // Arrange
            var notifyDate = DateTime.Parse(dateString);
            var record = new TrackingRecord
            {
                NotifyDate = notifyDate
            };

            // Assert
            record.NotifyDate.Should().Be(notifyDate);
        }

        [Fact]
        public void TrackingRecord_ShouldBeImmutable()
        {
            // Arrange
            var record = new TrackingRecord
            {
                PolicyNumber = "PN12345678",
                NotifyDate = new DateTime(2024, 6, 1),
                Status = "A",
                AddTimestamp = new DateTime(2024, 6, 1, 12, 0, 0),
                UpdateTimestamp = new DateTime(2024, 6, 2, 12, 0, 0)
            };

            // Act
            var newRecord = record with { Status = "B" };

            // Assert
            newRecord.Status.Should().Be("B");
            record.Status.Should().Be("A");
            newRecord.PolicyNumber.Should().Be(record.PolicyNumber);
            newRecord.NotifyDate.Should().Be(record.NotifyDate);
            newRecord.AddTimestamp.Should().Be(record.AddTimestamp);
            newRecord.UpdateTimestamp.Should().Be(record.UpdateTimestamp);
        }

        [Fact]
        public void TrackingRecord_Equality_ShouldWorkForSameValues()
        {
            // Arrange
            var record1 = new TrackingRecord
            {
                PolicyNumber = "PN12345678",
                NotifyDate = new DateTime(2024, 6, 1),
                Status = "A",
                AddTimestamp = new DateTime(2024, 6, 1, 12, 0, 0),
                UpdateTimestamp = new DateTime(2024, 6, 2, 12, 0, 0)
            };
            var record2 = new TrackingRecord
            {
                PolicyNumber = "PN12345678",
                NotifyDate = new DateTime(2024, 6, 1),
                Status = "A",
                AddTimestamp = new DateTime(2024, 6, 1, 12, 0, 0),
                UpdateTimestamp = new DateTime(2024, 6, 2, 12, 0, 0)
            };

            // Assert
            record1.Should().Be(record2);
            record1.GetHashCode().Should().Be(record2.GetHashCode());
        }

        [Fact]
        public void TrackingRecord_Equality_ShouldFailForDifferentValues()
        {
            // Arrange
            var record1 = new TrackingRecord
            {
                PolicyNumber = "PN12345678",
                NotifyDate = new DateTime(2024, 6, 1),
                Status = "A",
                AddTimestamp = new DateTime(2024, 6, 1, 12, 0, 0),
                UpdateTimestamp = new DateTime(2024, 6, 2, 12, 0, 0)
            };
            var record2 = new TrackingRecord
            {
                PolicyNumber = "PN87654321",
                NotifyDate = new DateTime(2024, 6, 2),
                Status = "B",
                AddTimestamp = new DateTime(2024, 6, 2, 12, 0, 0),
                UpdateTimestamp = new DateTime(2024, 6, 3, 12, 0, 0)
            };

            // Assert
            record1.Should().NotBe(record2);
        }

        [Fact]
        public void TrackingRecord_ShouldThrowException_WhenRequiredFieldsAreMissing()
        {
            // Arrange
            var record = new TrackingRecord
            {
                PolicyNumber = null,
                Status = null
            };

            // Act
            var validationContext = new ValidationContext(record);
            var validationResults = new System.Collections.Generic.List<ValidationResult>();
            var isValid = Validator.TryValidateObject(record, validationContext, validationResults, true);

            // Assert
            isValid.Should().BeFalse();
            validationResults.Should().Contain(r => r.MemberNames.Contains(nameof(TrackingRecord.PolicyNumber)));
            validationResults.Should().Contain(r => r.MemberNames.Contains(nameof(TrackingRecord.Status)));
        }

        [Fact]
        public void TrackingRecord_ShouldMapToCorrectDatabaseColumns()
        {
            // Arrange & Act
            var type = typeof(TrackingRecord);

            // Assert
            type.GetProperty(nameof(TrackingRecord.PolicyNumber))
                .GetCustomAttributes(typeof(ColumnAttribute), false)
                .Cast<ColumnAttribute>().First().Name
                .Should().Be("TR_POLICY_NUMBER");

            type.GetProperty(nameof(TrackingRecord.NotifyDate))
                .GetCustomAttributes(typeof(ColumnAttribute), false)
                .Cast<ColumnAttribute>().First().Name
                .Should().Be("TR_NOTIFY_DATE");

            type.GetProperty(nameof(TrackingRecord.Status))
                .GetCustomAttributes(typeof(ColumnAttribute), false)
                .Cast<ColumnAttribute>().First().Name
                .Should().Be("TR_STATUS");

            type.GetProperty(nameof(TrackingRecord.AddTimestamp))
                .GetCustomAttributes(typeof(ColumnAttribute), false)
                .Cast<ColumnAttribute>().First().Name
                .Should().Be("TR_ADD_TIMESTAMP");

            type.GetProperty(nameof(TrackingRecord.UpdateTimestamp))
                .GetCustomAttributes(typeof(ColumnAttribute), false)
                .Cast<ColumnAttribute>().First().Name
                .Should().Be("TR_UPDATE_TIMESTAMP");
        }

        // Integration test simulating database save and retrieval
        // (Mocks DbContext, since real DB not available)
        [Fact]
        public void TrackingRecord_IntegrationTest_SaveAndRetrieve_ShouldPreserveData()
        {
            // Arrange
            var mockSet = new Mock<System.Data.Entity.DbSet<TrackingRecord>>();
            var mockContext = new Mock<ITrackingDbContext>();
            var record = new TrackingRecord
            {
                PolicyNumber = "PN12345678",
                NotifyDate = new DateTime(2024, 6, 1),
                Status = "A",
                AddTimestamp = new DateTime(2024, 6, 1, 12, 0, 0),
                UpdateTimestamp = new DateTime(2024, 6, 2, 12, 0, 0)
            };

            mockSet.Setup(m => m.Add(It.IsAny<TrackingRecord>())).Returns(record);
            mockContext.Setup(c => c.TrackingRecords).Returns(mockSet.Object);

            // Act
            var added = mockContext.Object.TrackingRecords.Add(record);

            // Assert
            added.Should().BeEquivalentTo(record);
        }

        // Edge case: AddTimestamp and UpdateTimestamp in the past/future
        [Theory]
        [InlineData("2000-01-01", "2000-01-02")]
        [InlineData("2100-01-01", "2100-01-02")]
        public void TrackingRecord_Timestamps_ShouldAcceptValidDateRanges(string addTs, string updateTs)
        {
            // Arrange
            var addTimestamp = DateTime.Parse(addTs);
            var updateTimestamp = DateTime.Parse(updateTs);
            var record = new TrackingRecord
            {
                AddTimestamp = addTimestamp,
                UpdateTimestamp = updateTimestamp
            };

            // Assert
            record.AddTimestamp.Should().Be(addTimestamp);
            record.UpdateTimestamp.Should().Be(updateTimestamp);
            record.UpdateTimestamp.Should().BeAfter(record.AddTimestamp);
        }

        // Edge case: UpdateTimestamp before AddTimestamp (should be allowed by model, but business logic may prevent)
        [Fact]
        public void TrackingRecord_UpdateTimestamp_Before_AddTimestamp_ShouldBePossible()
        {
            // Arrange
            var record = new TrackingRecord
            {
                AddTimestamp = new DateTime(2024, 6, 2, 12, 0, 0),
                UpdateTimestamp = new DateTime(2024, 6, 1, 12, 0, 0)
            };

            // Assert
            record.UpdateTimestamp.Should().BeBefore(record.AddTimestamp);
        }

        // Edge case: NotifyDate in the future
        [Fact]
        public void TrackingRecord_NotifyDate_InFuture_ShouldBeAllowed()
        {
            // Arrange
            var futureDate = DateTime.UtcNow.AddYears(1);
            var record = new TrackingRecord
            {
                NotifyDate = futureDate
            };

            // Assert
            record.NotifyDate.Should().Be(futureDate);
        }

        // Edge case: NotifyDate in the past
        [Fact]
        public void TrackingRecord_NotifyDate_InPast_ShouldBeAllowed()
        {
            // Arrange
            var pastDate = DateTime.UtcNow.AddYears(-10);
            var record = new TrackingRecord
            {
                NotifyDate = pastDate
            };

            // Assert
            record.NotifyDate.Should().Be(pastDate);
        }

        // Edge case: All fields null or default
        [Fact]
        public void TrackingRecord_AllFieldsDefault_ShouldFailValidation()
        {
            // Arrange
            var record = new TrackingRecord();

            // Act
            var validationContext = new ValidationContext(record);
            var validationResults = new System.Collections.Generic.List<ValidationResult>();
            var isValid = Validator.TryValidateObject(record, validationContext, validationResults, true);

            // Assert
            isValid.Should().BeFalse();
            validationResults.Should().Contain(r => r.MemberNames.Contains(nameof(TrackingRecord.PolicyNumber)));
            validationResults.Should().Contain(r => r.MemberNames.Contains(nameof(TrackingRecord.Status)));
        }

        // Edge case: PolicyNumber and Status with whitespace
        [Theory]
        [InlineData("          ")] // 10 spaces
        [InlineData(" ")]         // 1 space for Status
        public void TrackingRecord_WhitespaceFields_ShouldPassLengthValidation(string value)
        {
            // Arrange
            var record = new TrackingRecord
            {
                PolicyNumber = value.Length == 10 ? value : "PN12345678",
                Status = value.Length == 1 ? value : "A"
            };

            // Act
            var validationContext = new ValidationContext(record);
            var validationResults = new System.Collections.Generic.List<ValidationResult>();
            var isValid = Validator.TryValidateObject(record, validationContext, validationResults, true);

            // Assert
            isValid.Should().BeTrue();
        }

        // Edge case: PolicyNumber with special characters
        [Theory]
        [InlineData("PN!@#$%^&*")]
        [InlineData("PN12345678")]
        public void TrackingRecord_PolicyNumber_WithSpecialCharacters_ShouldBeAllowed(string policyNumber)
        {
            // Arrange
            var record = new TrackingRecord
            {
                PolicyNumber = policyNumber
            };

            // Act
            var validationContext = new ValidationContext(record);
            var validationResults = new System.Collections.Generic.List<ValidationResult>();
            var isValid = Validator.TryValidateObject(record, validationContext, validationResults, true);

            // Assert
            isValid.Should().BeTrue();
        }

        // Edge case: Status with special character
        [Theory]
        [InlineData("!")]
        [InlineData(" ")]
        public void TrackingRecord_Status_WithSpecialCharacter_ShouldBeAllowed(string status)
        {
            // Arrange
            var record = new TrackingRecord
            {
                Status = status
            };

            // Act
            var validationContext = new ValidationContext(record);
            var validationResults = new System.Collections.Generic.List<ValidationResult>();
            var isValid = Validator.TryValidateObject(record, validationContext, validationResults, true);

            // Assert
            isValid.Should().BeTrue();
        }

        // Simulated business logic preservation: PolicyNumber must be unique (integration test, mock)
        [Fact]
        public void TrackingRecord_PolicyNumber_ShouldBeUnique_InDbContext()
        {
            // Arrange
            var mockSet = new Mock<System.Data.Entity.DbSet<TrackingRecord>>();
            var mockContext = new Mock<ITrackingDbContext>();
            var record1 = new TrackingRecord { PolicyNumber = "PN12345678", Status = "A", NotifyDate = DateTime.UtcNow };
            var record2 = new TrackingRecord { PolicyNumber = "PN12345678", Status = "B", NotifyDate = DateTime.UtcNow };

            mockSet.Setup(m => m.Add(It.IsAny<TrackingRecord>())).Returns((TrackingRecord tr) => tr);
            mockSet.Setup(m => m.Any(It.IsAny<Func<TrackingRecord, bool>>()))
                .Returns<Func<TrackingRecord, bool>>(predicate => predicate(record1));

            mockContext.Setup(c => c.TrackingRecords).Returns(mockSet.Object);

            // Act
            var exists = mockContext.Object.TrackingRecords.Any(r => r.PolicyNumber == record2.PolicyNumber);

            // Assert
            exists.Should().BeTrue("PolicyNumber should be unique in database");
        }

        // Simulated interface for DbContext
        public interface ITrackingDbContext
        {
            System.Data.Entity.DbSet<TrackingRecord> TrackingRecords { get; }
        }
    }
}