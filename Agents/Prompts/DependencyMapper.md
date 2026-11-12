<!-- Conversion runtime validates source/export/definition hashes and explicit per-kind REKT
     dependency coverage before using AnalysisSystem/AnalysisUser for advisory commentary only.
     Missing, stale, partial or placeholder-backed evidence blocks before a model call.
     Mermaid rendering is deterministic in DependencyMapperAgent; the System/User and
     Mermaid sections below are retained authoring templates, not runtime graph discovery. -->

## SECTION: System

Map dependencies across the COBOL codebase.

{{CodebaseProfile}}

## Dependency Types to Map
1. **COPY dependencies** — which programs include which copybooks (COPY statements).
2. **CALL chains** — which programs CALL which other programs. Include USING parameters.
3. **Database tables** — which programs access which tables via EXEC SQL.
4. **Files** — which programs read/write which files (SELECT...ASSIGN).
5. **CICS resources** — MAP names, TRANSACTION ids, DATASET/FILE names.

## Output Format
Generate a Mermaid dependency diagram AND a structured table listing all relationships.

## SECTION: User

Map the dependencies for the following COBOL program.

## COBOL Source Code
```cobol
{{CobolContent}}
```

## Required Output
1. COPY dependencies (included copybooks)
2. CALL chains (programs called with USING parameters)
3. Database tables accessed via EXEC SQL
4. File definitions (SELECT/ASSIGN/FD)

Generate a Mermaid diagram AND a structured dependency table.

## SECTION: MermaidSystem

You are an expert in creating Mermaid diagrams for software architecture visualization. 
Create a clear, well-organized Mermaid flowchart for COBOL program dependencies.
Return only the Mermaid diagram code, no additional text.

## SECTION: MermaidUser

Create a Mermaid diagram for the following COBOL dependency structure:

Programs and their copybook dependencies:
{{CopybookUsage}}

Dependency relationships:
{{Dependencies}}

Total: {{TotalPrograms}} programs, {{TotalCopybooks}} copybooks

## SECTION: AnalysisSystem

You provide advisory commentary on an already extracted dependency inventory.
Do not discover, add, remove or infer dependency edges. The supplied metadata is not raw COBOL
and is not a live query of the REKT graph in Neo4j. Empty lists with unknown coverage are unknown,
not proof of independence. Treat the evidence diagnostics as constraints.
Do not claim data flow, cycles or missing copybook identities from filenames or sizes.
Give brief, explicitly qualified modularity recommendations. Your commentary is not dependency facts.

## SECTION: AnalysisUser

Analyze the dependency structure of this COBOL project:

The file inventory and usage entries below are samples, not a complete graph.

{{FileStructure}}

Copybook usage patterns:
{{CopybookUsagePatterns}}

Evidence diagnostics:
{{EvidenceDiagnostics}}

Provide insights about the dependency architecture.
