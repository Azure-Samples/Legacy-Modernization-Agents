# Reverse Engineering Feature

## Overview

The reverse engineering feature extracts business logic and identifies modernization opportunities from COBOL applications **without requiring full code migration**. This is perfect for:

- Creating documentation for RFPs and contractor briefings
- Understanding business logic before committing to migration
- Identifying selective modernization opportunities
- Generating user stories for agile planning

## Quick Start

### Standalone Reverse Engineering

Extract business logic from COBOL code:

```bash
dotnet run reverse-engineer --cobol-source ./cobol-source
```

With custom output folder:

```bash
dotnet run reverse-engineer --cobol-source ./cobol-source --output ./my-analysis
```

### Output Files

The tool generates three markdown files in `reverse-engineering-output/`:

1. **business-logic.md** - Business-focused documentation
   - User stories (for interactive/transactional logic)
   - Feature descriptions (for batch/calculation processes)
   - Business rules
   - Data entities

2. **technical-details.md** - Technical analysis
   - Utility code patterns (date/time, string, math operations)
   - Classification (standard vs. business-specific)
   - Modernization recommendations
   - Modern Java equivalents with example code

3. **summary.md** - Overview and statistics
   - Total files analyzed
   - User stories, features, and business rules count
   - Modernization opportunities summary
   - Next steps recommendations

## Key Features

### Business Logic Extraction

**User Stories** - Generated for interactive/transactional logic:
```markdown
#### US-1: Process Customer Order
**As a** Sales Representative
**I want to** process customer oil orders
**So that** customers receive their products

**Acceptance Criteria:**
- Given a valid customer ID
- When order is submitted
- Then inventory is updated and invoice is generated
```

**Features** - Generated for batch/calculation processes:
```markdown
#### F-1: Monthly Sales Report Generation
**Description:** Processes oil sales data and generates summary reports

**Business Rules:**
- Calculate total sales by customer
- Apply pricing based on unit size
- Format output for printing

**Inputs:**
- BaseOilsSalesFile (sequential file, sorted by CustomerId)

**Outputs:**
- Formatted sales report
```

### Utility Code Analysis

Identifies three types of utility patterns:

1. **Date/Time Operations**
   - Date formatting and parsing
   - Date arithmetic
   - Calendar calculations

2. **String Manipulation**
   - Concatenation, substring, trimming
   - Case conversion, padding
   - Pattern matching

3. **Mathematical Operations**
   - Basic arithmetic
   - Rounding and formatting
   - Business calculations

### Classification System

Each utility pattern is classified as:

- **Standard Utility**: Generic pattern that modern libraries handle
  - Example: Date formatting → `java.time.DateTimeFormatter`
  - Recommendation: Use standard library
  - Confidence: HIGH, Effort: LOW

- **Business-Specific**: Contains custom business rules
  - Example: Custom customer ID validation with check digit
  - Recommendation: Migrate as-is or extract to utility class
  - Reasoning: Contains company-specific validation rules

### Detection Criteria

The tool uses multiple criteria for accurate classification:

1. **Pattern Matching**: Recognizes common utility function signatures
2. **Business Constants**: Detects business-specific magic numbers/values
3. **Domain Data**: Identifies operations on business domain structures

## Use Cases

### 1. Documentation-Only Workflow
```
Run reverse engineering → Review documentation → 
Use for RFP/contractor briefing → No migration needed
```

### 2. Assessment Before Migration
```
Run reverse engineering → Review complexity and scope →
Decide which parts to migrate → Plan migration strategy
```

### 3. Selective Modernization
```
Run reverse engineering → Identify high-value, low-effort changes →
Prioritize modernization → Implement improvements incrementally
```

### 4. Integrated with Full Migration
```
Run reverse engineering → Review output → Confirm continuation →
Proceed with full Java/Quarkus migration
```

## Example Output

### Business Logic Extract
```markdown
## Listing12-1.cbl

### Business Purpose
Generates summary sales reports for Aromamora base oils by customer.

### Features

#### F-1: Customer Sales Summarization
**Description:** Reads sales file and produces customer-level summaries

**Business Rules:**
- Group sales by customer ID
- Calculate total value using oil pricing table
- Format currency with $ prefix

**Processing Steps:**
1. Read sales records sequentially
2. Accumulate sales per customer
3. Print customer summary line
4. Continue until end of file
```

### Technical Analysis
```markdown
## Listing12-1.cbl

### Mathematical Operations

**Pattern: Currency Calculation**
- **Location:** PrintCustomerLines paragraph
- **Classification:** Business-Specific
- **Reasoning:** Uses domain-specific pricing table with 14 oil types

**Modernization Opportunity:**
- **Current:** COBOL COMPUTE with table lookup
- **Modern Equivalent:** `BigDecimal` with pricing service
- **Confidence:** MEDIUM
- **Effort:** MEDIUM
- **Reasoning:** Pricing logic is business-specific but can benefit from modern money handling

**Example:**
```java
BigDecimal calculateSaleValue(String oilId, int units, int unitSize) {
    BigDecimal unitCost = pricingService.getUnitCost(oilId, unitSize);
    return unitCost.multiply(BigDecimal.valueOf(units));
}
```

## Architecture

### Components

```
Agents/
  BusinessLogicExtractorAgent.cs   # Extracts user stories and features
  UtilityCodeAnalyzerAgent.cs      # Analyzes utility patterns

Models/
  BusinessLogic.cs                 # User stories, features, rules
  UtilityCodeAnalysis.cs           # Patterns and recommendations

Processes/
  ReverseEngineeringProcess.cs    # Orchestrates the workflow
```

### Process Flow

```
1. File Discovery
   └─ Scan COBOL source folder

2. Technical Analysis
   └─ Use CobolAnalyzerAgent to analyze structure

3. Business Logic Extraction
   └─ Extract user stories, features, rules
   └─ Focus on WHAT, not HOW

4. Utility Code Analysis
   └─ Identify utility patterns
   └─ Classify as standard vs. business-specific
   └─ Generate modernization recommendations

5. Generate Documentation
   └─ business-logic.md
   └─ technical-details.md
   └─ summary.md
```

## Configuration

Uses the same configuration as the main migration tool:

```bash
# Set Azure OpenAI credentials
export AZURE_OPENAI_ENDPOINT="https://your-endpoint.openai.azure.com/"
export AZURE_OPENAI_API_KEY="your-api-key"
export AZURE_OPENAI_DEPLOYMENT_NAME="gpt-4"
```

Or use `Config/ai-config.local.env`.

## Tips

1. **Start Small**: Run on a few representative files first
2. **Review Thoroughly**: AI-generated analysis should be validated
3. **Iterate**: Refine based on initial results
4. **Combine with Migration**: Use as first step in full migration workflow

## Next Steps

After reverse engineering:

1. **Review Documentation**: Understand the business logic
2. **Identify Priorities**: Focus on high-value areas
3. **Plan Strategy**: Decide on migration approach
4. **Execute**: Run full migration if desired:
   ```bash
   dotnet run --cobol-source ./cobol-source --java-output ./java-output
   ```

## Support

For issues or questions, refer to:
- Main README.md for general setup
- CHANGELOG.md for feature details
- GitHub Issues for bug reports
