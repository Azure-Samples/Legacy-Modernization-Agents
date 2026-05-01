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

You are an expert COBOL dependency analyzer. Analyze the provided COBOL code structure and identify:
1. Data flow dependencies between copybooks
2. Potential circular dependencies
3. Modularity recommendations
Provide a brief analysis.

## SECTION: AnalysisUser

Analyze the dependency structure of this COBOL project:

{{FileStructure}}

Copybook usage patterns:
{{CopybookUsagePatterns}}

Provide insights about the dependency architecture.
