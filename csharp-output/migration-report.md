# COBOL to CSharp Migration Report
Generated: 2025-11-11 07:44:11 UTC
Total Migration Time: 00:22:33.1808927

## 📊 Migration Overview
- **Source Files**: 12 COBOL files
- **Generated C# Files**: 12
- **Dependencies Found**: 11
- **Copybooks Analyzed**: 7
- **Average Dependencies per Program**: 2.2

## 🗂️ C# File Mapping
| COBOL File | C# File | Type |
|------------|---------|------|
| DBDRIVR1.cbl | PolicyCursorDriver | Program |
| DBDRIVR2.cbl | TrackingRepository | Program |
| FLDRIVR1.cbl | AgentFileException | Program |
| FLDRIVR2.cbl | NotificationFileService | Program |
| MAINPGM.cbl | for | Program |
| AGNTNTFY.cpy | containing | Copybook |
| CAGENT.cpy | is | Copybook |
| CPOLICY.cpy | containing | Copybook |
| CUSTNTFY.cpy | is | Copybook |
| DCOVERAG.cpy | maps | Copybook |
| DPOLICY.cpy | PolicyRepository | Copybook |
| DTRAKING.cpy | for | Copybook |

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
- **Files per Minute**: 0.5
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
