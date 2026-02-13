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
