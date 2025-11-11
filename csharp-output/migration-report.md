# COBOL to CSharp Migration Report
Generated: 2025-11-11 06:42:23 UTC
Total Migration Time: 00:33:50.7995930

## 📊 Migration Overview
- **Source Files**: 12 COBOL files
- **Generated C# Files**: 12
- **Dependencies Found**: 11
- **Copybooks Analyzed**: 7
- **Average Dependencies per Program**: 2.2

## 🗂️ C# File Mapping
| COBOL File | C# File | Type |
|------------|---------|------|
| DBDRIVR1.cbl | PolicyDataDriver | Program |
| DBDRIVR2.cbl | TrackingRecord | Program |
| FLDRIVR1.cbl | AgentFileException | Program |
| FLDRIVR2.cbl | NotificationFileDriver | Program |
| MAINPGM.cbl | PolicyExpiryBatch | Program |
| AGNTNTFY.cpy | AgentNotifyService | Copybook |
| CAGENT.cpy | AgentRecord | Copybook |
| CPOLICY.cpy | containing | Copybook |
| CUSTNTFY.cpy | is | Copybook |
| DCOVERAG.cpy | maps | Copybook |
| DPOLICY.cpy | mapped | Copybook |
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
- **Files per Minute**: 0.4
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
