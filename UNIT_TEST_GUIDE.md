# Unit Test Generation Guide

## Overview

The COBOL Migration Tool includes an **AI-powered Unit Test Generation** capability that automatically creates comprehensive test suites for converted Java and C# code. The UnitTestAgent uses Azure OpenAI to analyze the original COBOL business logic and generate tests that verify functional equivalence.

## Features

### 🧪 Test Framework Support
- **Java**: JUnit 5 with Mockito for mocking
- **C#**: xUnit with Moq for mocking

### 🎯 Test Coverage
- Public method testing
- Edge case scenarios
- Null checks and boundary conditions
- Database operation testing
- COBOL business logic preservation
- Error handling verification

### 📊 Code Quality
- Arrange-Act-Assert pattern
- Meaningful test names
- Comprehensive code coverage (target >80%)
- Well-documented test scenarios
- Integration test support

## How It Works

### Automatic Test Generation
Unit tests are **automatically generated** during the migration process:

1. **COBOL Analysis** → Agent analyzes business logic
2. **Code Conversion** → Java/C# code is generated
3. **Test Generation** → Unit tests are created based on:
   - Converted code structure
   - COBOL business logic analysis
   - Data structures and flows
   - Expected behaviors

### AI-Powered Intelligence
The UnitTestAgent:
- Understands COBOL business logic from the analysis
- Maps COBOL constructs to modern test patterns
- Generates tests that verify functional equivalence
- Creates mocks for dependencies
- Includes edge cases specific to the COBOL logic

## Usage

### Running Migration with Test Generation

#### Java Tests
```bash
./doctor.sh run --target java
```

**Generated test location:** `./java-output/src/test/java/`

#### C# Tests
```bash
./doctor.sh run --target csharp
```

**Generated test location:** `./csharp-output/Tests/`

#### Both Languages
```bash
./doctor.sh run --target both
```

### Manual Test Generation Command

Check what tests have been generated:
```bash
./doctor.sh generate-tests
```

With specific target:
```bash
./doctor.sh generate-tests --target java
./doctor.sh generate-tests --target csharp
```

**Note:** Tests are automatically generated during migration. The `generate-tests` command provides information about test locations.

## Test Output Structure

### Java Tests Structure
```
java-output/
├── src/
│   ├── main/
│   │   └── java/
│   │       └── com/example/cobol/
│   │           ├── DbDriver1.java
│   │           └── MainProgram.java
│   └── test/
│       └── java/
│           └── com/example/cobol/
│               ├── DbDriver1Test.java
│               └── MainProgramTest.java
```

### C# Tests Structure
```
csharp-output/
├── AgentFileDriver/
│   └── AgentFileDriver.cs
├── Insurance/
│   └── InsuranceProgram.cs
└── Tests/
    ├── AgentFileDriverTests.cs
    └── InsuranceProgramTests.cs
```

## Test Characteristics

### Java (JUnit 5) Tests

#### Example Test Structure
```java
package com.example.cobol;

import org.junit.jupiter.api.*;
import org.mockito.*;
import static org.junit.jupiter.api.Assertions.*;

class DbDriver1Test {
    
    @Mock
    private DatabaseConnection mockConnection;
    
    @InjectMocks
    private DbDriver1 dbDriver;
    
    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
    }
    
    @Test
    @DisplayName("Should process policy data correctly")
    void testProcessPolicyData() {
        // Arrange
        PolicyData input = new PolicyData("12345");
        
        // Act
        Result result = dbDriver.processPolicy(input);
        
        // Assert
        assertNotNull(result);
        assertEquals("SUCCESS", result.getStatus());
    }
    
    @Test
    @DisplayName("Should handle null input gracefully")
    void testNullInputHandling() {
        // Act & Assert
        assertThrows(IllegalArgumentException.class, 
            () -> dbDriver.processPolicy(null));
    }
}
```

#### Key Features
- `@Test` annotations for test methods
- `@BeforeEach` / `@AfterEach` for setup/teardown
- `@Mock` for dependency mocking
- `@DisplayName` for readable test names
- Mockito for behavior verification
- AssertJ or JUnit assertions

### C# (xUnit) Tests

#### Example Test Structure
```csharp
using Xunit;
using Moq;
using FluentAssertions;

namespace Insurance.Tests
{
    public class InsuranceProgramTests : IDisposable
    {
        private readonly InsuranceProgram _program;
        private readonly Mock<IDatabase> _mockDatabase;
        
        public InsuranceProgramTests()
        {
            _mockDatabase = new Mock<IDatabase>();
            _program = new InsuranceProgram(_mockDatabase.Object);
        }
        
        [Fact]
        public void ProcessClaim_WithValidData_ShouldReturnSuccess()
        {
            // Arrange
            var claim = new Claim { Id = "12345" };
            
            // Act
            var result = _program.ProcessClaim(claim);
            
            // Assert
            result.Should().NotBeNull();
            result.Status.Should().Be("SUCCESS");
        }
        
        [Theory]
        [InlineData(null)]
        [InlineData("")]
        public void ProcessClaim_WithInvalidId_ShouldThrowException(string id)
        {
            // Arrange
            var claim = new Claim { Id = id };
            
            // Act & Assert
            Assert.Throws<ArgumentException>(() => 
                _program.ProcessClaim(claim));
        }
        
        public void Dispose()
        {
            // Cleanup
            _mockDatabase.Reset();
        }
    }
}
```

#### Key Features
- `[Fact]` for simple tests
- `[Theory]` with `[InlineData]` for parameterized tests
- `IDisposable` for cleanup
- Moq for mocking
- FluentAssertions for readable assertions

## Test Coverage Areas

### 1. Business Logic Verification
Tests ensure COBOL business rules are preserved:
- Data validation rules
- Calculation logic
- Conditional processing
- Loop iterations
- File processing logic

### 2. Data Structure Testing
Tests verify data handling:
- Record structure mapping
- Field conversions
- Data type handling
- Copybook data structures

### 3. Error Handling
Tests check error scenarios:
- Invalid input handling
- Database errors
- File I/O errors
- Boundary conditions

### 4. Integration Points
Tests cover external dependencies:
- Database operations (mocked)
- File system operations
- External service calls

## Running the Tests

### Java Tests (Maven)
```bash
cd java-output
mvn test
```

With coverage:
```bash
mvn test jacoco:report
```

### C# Tests (dotnet)
```bash
cd csharp-output
dotnet test
```

With coverage:
```bash
dotnet test /p:CollectCoverage=true /p:CoverageReportsFormat=lcov
```

## Configuration

### Test Generation Settings

The test generation can be configured in `Config/appsettings.json`:

```json
{
  "AISettings": {
    "UnitTestModelId": "gpt-4.1"
  },
  "ApplicationSettings": {
    "TestOutputFolder": "test-output"
  }
}
```

### Environment Variables

Set the model for test generation:
```bash
export AZURE_OPENAI_UNIT_TEST_MODEL="gpt-4.1"
```

## Best Practices

### 1. Review Generated Tests
- **Always review** AI-generated tests
- Verify test logic matches COBOL behavior
- Add domain-specific test cases
- Enhance edge case coverage

### 2. Customize Tests
- Add business-specific scenarios
- Include integration tests
- Add performance tests if needed
- Document complex test scenarios

### 3. Maintain Tests
- Update tests when code changes
- Keep tests aligned with COBOL source
- Refactor tests for clarity
- Remove obsolete tests

### 4. Test Organization
- Group related tests
- Use descriptive test names
- Follow naming conventions
- Keep tests focused and simple

## Troubleshooting

### No Tests Generated

**Problem:** Tests are missing after migration

**Solutions:**
1. Check that UnitTestAgent is initialized:
   ```bash
   grep -r "UnitTestAgent" Logs/
   ```

2. Verify test directories exist:
   ```bash
   ls -la java-output/src/test/java/
   ls -la csharp-output/Tests/
   ```

3. Check migration logs for errors:
   ```bash
   cat Logs/FULL_CHAT_LOG_*.md | grep -i "test"
   ```

### Test Compilation Errors

**Problem:** Generated tests don't compile

**Solutions:**
1. Check for missing dependencies
2. Verify package/namespace declarations
3. Review import/using statements
4. Check for API compatibility

### Low Test Coverage

**Problem:** Generated tests don't cover all code

**Solutions:**
1. Manually add tests for uncovered areas
2. Review COBOL analysis for missing logic
3. Add integration tests
4. Use coverage tools to identify gaps

## API Call Tracking

Test generation is tracked in the API statistics:

```
📊 API Call Statistics
======================

Agent Breakdown:
┌──────────────────┬───────┬──────────┬──────────┬─────────┐
│ Agent            │ Calls │ Duration │ Tokens   │ Cost    │
├──────────────────┼───────┼──────────┼──────────┼─────────┤
│ UnitTestAgent    │     5 │  12.3s   │  45,234  │ $0.089  │
└──────────────────┴───────┴──────────┴──────────┴─────────┘
```

## Examples

### Example 1: Database Driver Test

**COBOL Code:**
```cobol
PROCEDURE DIVISION.
    EXEC SQL
        SELECT POLICY_NBR, COVERAGE_AMT
        INTO :WS-POLICY, :WS-AMOUNT
        FROM POLICIES
        WHERE POLICY_NBR = :WS-POLICY-IN
    END-EXEC.
```

**Generated Java Test:**
```java
@Test
void testPolicyLookup_WithValidPolicyNumber() {
    // Arrange
    when(mockConnection.executeQuery(anyString()))
        .thenReturn(createMockResultSet("12345", 50000));
    
    // Act
    PolicyResult result = driver.lookupPolicy("12345");
    
    // Assert
    assertEquals("12345", result.getPolicyNumber());
    assertEquals(50000.0, result.getCoverageAmount());
}
```

### Example 2: C# Calculation Test

**COBOL Code:**
```cobol
COMPUTE WS-PREMIUM = WS-BASE-PREMIUM * WS-RATE-FACTOR.
```

**Generated C# Test:**
```csharp
[Theory]
[InlineData(1000, 1.5, 1500)]
[InlineData(2000, 2.0, 4000)]
[InlineData(0, 1.0, 0)]
public void CalculatePremium_WithVariousInputs_ShouldReturnCorrectAmount(
    decimal basePremium, 
    decimal rateFactor, 
    decimal expected)
{
    // Act
    var result = _program.CalculatePremium(basePremium, rateFactor);
    
    // Assert
    result.Should().Be(expected);
}
```

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Test Converted Code

on: [push, pull_request]

jobs:
  test-java:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Set up JDK
        uses: actions/setup-java@v2
        with:
          java-version: '17'
      - name: Run Java Tests
        run: |
          cd java-output
          mvn test
      - name: Upload Coverage
        uses: codecov/codecov-action@v2

  test-csharp:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup .NET
        uses: actions/setup-dotnet@v1
        with:
          dotnet-version: '8.0'
      - name: Run C# Tests
        run: |
          cd csharp-output
          dotnet test --collect:"XPlat Code Coverage"
```

## Summary

The Unit Test Generation feature:
- ✅ **Automatically creates tests** during migration
- ✅ **Preserves COBOL business logic** in test assertions
- ✅ **Supports Java and C#** test frameworks
- ✅ **Generates comprehensive coverage** (>80% target)
- ✅ **Includes edge cases** and error handling
- ✅ **Uses modern testing patterns** (AAA, mocking)
- ✅ **Tracked in API statistics** for cost monitoring

**Remember:** AI-generated tests are a starting point. Always review and enhance them with domain-specific knowledge and additional scenarios!

---

**For more information:**
- [Validation Guide](VALIDATION_GUIDE.md) - Code validation features
- [README](README.md) - Main migration guide
- [Semantic Kernel Architecture](SEMANTIC_KERNEL_ARCHITECTURE.md) - Technical details
