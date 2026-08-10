using CobolToQuarkusMigration.Agents;
using CobolToQuarkusMigration.Models;
using FluentAssertions;
using Xunit;

namespace CobolToQuarkusMigration.Tests.Agents;

public class BusinessLogicExtractorAgentTests
{
    [Fact]
    public void ParseBusinessLogicResponse_RequiredMarkdown_PopulatesAllSections()
    {
        var response = """
            ## Business Purpose

            Allows an operator to locate a customer by identifier and view the customer record.

            ## Use Cases
            ### Use Case 1: Find customer
            **Trigger:** An operator enters a customer identifier.
            **Description:** The system retrieves the matching customer record.
            **Benefit:** The operator can review current customer information.
            **Key Steps:**
            1. Accept the customer identifier.
            2. Retrieve the matching record.

            ## Business Rules
            ### Rule 1: Customer must exist
            **Condition:** No record matches the supplied identifier.
            **Action:** Inform the operator that the customer is invalid.
            **Source:** CUSTOMER-INQUIRY.cbl:38-43 — keyed customer read with found/not-found branches
            """;

        var result = BusinessLogicExtractorAgent.ParseBusinessLogicResponse(
            new CobolFile
            {
                FileName = "CUSTOMER-INQUIRY.cbl",
                FilePath = "source/CUSTOMER-INQUIRY.cbl"
            },
            response);

        result.BusinessPurpose.Should().Contain("locate a customer");
        result.UserStories.Should().ContainSingle();
        result.UserStories[0].Title.Should().Be("Find customer");
        result.UserStories[0].Benefit.Should().Contain("review current customer information");
        result.UserStories[0].AcceptanceCriteria.Should().HaveCount(2);
        result.Features.Should().BeEmpty();
        result.BusinessRules.Should().ContainSingle();
        result.BusinessRules[0].Description.Should().Be("Customer must exist");
        result.BusinessRules[0].Condition.Should().Contain("No record matches");
        result.BusinessRules[0].Action.Should().Contain("Inform the operator");
        result.BusinessRules[0].SourceLocation.Should().Be(
            "CUSTOMER-INQUIRY.cbl:38-43 — keyed customer read with found/not-found branches");
    }

    [Fact]
    public void ParseBusinessLogicResponse_BlankLineAfterPurposeHeading_DoesNotDiscardPurpose()
    {
        var response = """
            ## 1. Business Purpose

            Defines the shared customer record used by inquiry and display operations.

            ## Business Rules
            - Customer identifiers contain exactly eight numeric digits.
            """;

        var result = BusinessLogicExtractorAgent.ParseBusinessLogicResponse(
            new CobolFile
            {
                FileName = "CUSTOMER-DATA.cpy",
                FilePath = "source/CUSTOMER-DATA.cpy",
                IsCopybook = true
            },
            response);

        result.BusinessPurpose.Should().Be(
            "Defines the shared customer record used by inquiry and display operations.");
        result.BusinessRules.Should().ContainSingle();
        result.BusinessRules[0].Description.Should().Contain("eight numeric digits");
    }

    [Fact]
    public void EnsureUsableBusinessLogic_EmptyResult_AddsVisibleDiagnostic()
    {
        var result = new BusinessLogic
        {
            FileName = "CUSTOMER-INQUIRY.cbl"
        };
        string? warning = null;

        BusinessLogicExtractorAgent.EnsureUsableBusinessLogic(result, message => warning = message);

        result.BusinessPurpose.Should().Be(
            "Business logic extraction did not match the required output format.");
        warning.Should().Be(result.BusinessPurpose);
    }

    [Fact]
    public async Task ExtractFileSafelyAsync_ExtractionThrows_ReturnsFileLocalDiagnostic()
    {
        var file = new CobolFile
        {
            FileName = "CUSTOMER-DATA.cpy",
            FilePath = "source/CUSTOMER-DATA.cpy",
            IsCopybook = true
        };
        Exception? loggedException = null;

        var result = await BusinessLogicExtractorAgent.ExtractFileSafelyAsync(
            file,
            () => Task.FromException<BusinessLogic>(new InvalidOperationException("unexpected parser failure")),
            exception => loggedException = exception);

        result.FileName.Should().Be(file.FileName);
        result.FilePath.Should().Be(file.FilePath);
        result.IsCopybook.Should().BeTrue();
        result.BusinessPurpose.Should().Be("Business logic extraction failed: unexpected parser failure");
        loggedException.Should().BeOfType<InvalidOperationException>();
    }

    [Fact]
    public void ParseBusinessLogicResponse_RuleTitleContainsSectionKeyword_PreservesRule()
    {
        var response = """
            ## Business Purpose
            Validates customer records.

            ## Business Rules
            ### Rule 1: Validations must complete
            **Condition:** A customer record is submitted.
            **Action:** Complete every validation before accepting the record.
            **Source:** VALIDATE-CUSTOMER
            """;

        var result = BusinessLogicExtractorAgent.ParseBusinessLogicResponse(
            new CobolFile { FileName = "CUSTOMER.cbl" },
            response);

        result.BusinessRules.Should().ContainSingle();
        result.BusinessRules[0].Description.Should().Be("Validations must complete");
    }

    [Fact]
    public void ParseBusinessLogicResponse_UseCaseTitleContainsOperation_DoesNotCreateFeature()
    {
        var response = """
            ## Business Purpose
            Supports customer lookup.

            ## Use Cases
            ### Use Case 1: Perform customer lookup operation
            **Trigger:** An operator enters an identifier.
            **Description:** Retrieve the matching customer.
            **Key Steps:**
            1. Read the identifier.
            """;

        var result = BusinessLogicExtractorAgent.ParseBusinessLogicResponse(
            new CobolFile { FileName = "CUSTOMER.cbl" },
            response);

        result.UserStories.Should().ContainSingle();
        result.Features.Should().BeEmpty();
    }

    [Fact]
    public void ParseBusinessLogicResponse_NumberedStepStartsWithNumber_PreservesStepContent()
    {
        var response = """
            ## Business Purpose
            Handles customer lookup failures.

            ## Use Cases
            ### Use Case 1: Reject missing customer
            **Key Steps:**
            1. 401 responses are returned for unauthorized requests.
            """;

        var result = BusinessLogicExtractorAgent.ParseBusinessLogicResponse(
            new CobolFile { FileName = "CUSTOMER.cbl" },
            response);

        result.UserStories[0].AcceptanceCriteria.Should()
            .ContainSingle("401 responses are returned for unauthorized requests.");
    }

    [Fact]
    public void CreateMissingAnalysisResult_PreservesFileIdentityAndDiagnostic()
    {
        var file = new CobolFile
        {
            FileName = "CUSTOMER-DATA.cpy",
            FilePath = "source/CUSTOMER-DATA.cpy",
            IsCopybook = true
        };

        var result = BusinessLogicExtractorAgent.CreateMissingAnalysisResult(file);

        result.FileName.Should().Be(file.FileName);
        result.FilePath.Should().Be(file.FilePath);
        result.IsCopybook.Should().BeTrue();
        result.BusinessPurpose.Should().Contain("technical analysis was unavailable");
    }

    [Fact]
    public void AddSourceLineNumbers_UsesOneBasedStableReferences()
    {
        var numbered = BusinessLogicExtractorAgent.AddSourceLineNumbers(
            "READ CUSTOMER-FILE\r\n    INVALID KEY\r\n        DISPLAY ERR-INVALID-CUST");

        numbered.Should().Be(
            "     1 | READ CUSTOMER-FILE\n" +
            "     2 |     INVALID KEY\n" +
            "     3 |         DISPLAY ERR-INVALID-CUST");
    }
}
