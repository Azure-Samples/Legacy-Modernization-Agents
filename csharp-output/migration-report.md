# COBOL to CSharp Migration Report
Generated: 2025-11-11 05:46:38 UTC
Total Migration Time: 00:18:17.6403650

## 📊 Migration Overview
- **Source Files**: 12 COBOL files
- **Generated C# Files**: 12
- **Dependencies Found**: 11
- **Copybooks Analyzed**: 7
- **Average Dependencies per Program**: 2.2

## 🗂️ C# File Mapping
| COBOL File | C# File | Type |
|------------|---------|------|
| DBDRIVR1.cbl | PolicyDriver | Program |
| DBDRIVR2.cbl | LinkArea | Program |
| FLDRIVR1.cbl | AgentFileDriverInput | Program |
| FLDRIVR2.cbl | FileOperationRequest | Program |
| MAINPGM.cbl | PolicyBatchProcessor | Program |
| AGNTNTFY.cpy | for | Copybook |
| CAGENT.cpy | containing | Copybook |
| CPOLICY.cpy | containing | Copybook |
| CUSTNTFY.cpy | containing | Copybook |
| DCOVERAG.cpy | maps | Copybook |
| DPOLICY.cpy | mapped | Copybook |
| DTRAKING.cpy | is | Copybook |

## 🔗 Dependency Analysis
### Most Used Copybooks
- **SQLCA.cpy**: Used by 2 programs
- **DTRAKING.cpy**: Used by 2 programs
- **CAGENT.cpy**: Used by 2 programs
- **DPOLICY.cpy**: Used by 1 programs
- **DCOVERAG.cpy**: Used by 1 programs
- **CPOLICY.cpy**: Used by 1 programs
- **CUSTNTFY.cpy**: Used by 1 programs
- **AGNTNTFY.cpy**: Used by 1 programs

## 📈 Migration Metrics
- **Files per Minute**: 0.7
- **Average File Size**: 5175 characters
- **Total Lines of Code**: 1,372

## 🚀 Next Steps
1. Review generated files for accuracy
2. Run unit tests (if UnitTestAgent is configured)
3. Check dependency diagram for architecture insights
4. Validate business logic in converted code
6. Configure appsettings.json and dependency injection for C#

## 📁 Generated Files
- `dependency-map.json` - Complete dependency analysis
- `dependency-diagram.md` - Mermaid dependency visualization
- `migration-conversation-log.md` - AI agent conversation log
- Individual C# files in respective namespaces
