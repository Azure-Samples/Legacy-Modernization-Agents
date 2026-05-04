**Last updated**: 2026-04-20

# Cobol-REKT: Deep Code Intelligence for COBOL Modernization

1. **Static analysis without execution** — Cobol-REKT parses COBOL source code into ASTs, control-flow graphs, and data-structure maps without needing a mainframe or runtime environment.

2. **Full dependency mapping** — Automatically discovers CALL chains, COPY/INCLUDE relationships, and cross-program data flows across the entire codebase, revealing hidden coupling that manual analysis misses.

3. **Control-flow graph (CFG) extraction** — Builds precise CFGs for every paragraph and section, making GO TO spaghetti, PERFORM THRU ranges, and fall-through logic visible and navigable.

4. **Graph-powered impact analysis** — Ingests all parse artifacts into Neo4j, enabling Cypher queries like "which programs are affected if I change this copybook?" — answers in seconds, not days.

5. **Migration risk scoring** — The structural complexity data (cyclomatic complexity, nesting depth, dead code) feeds directly into the migration agents, so the hardest files get the most thorough conversion strategy.

6. **Dead code detection** — Identifies unreachable paragraphs, unused copybooks, and orphaned data definitions, reducing the scope of what actually needs to be modernized.

7. **Service boundary discovery** — Analyzes CALL graphs and data coupling to suggest natural microservice boundaries, turning a monolithic COBOL application into a candidate architecture for Java/Quarkus services.

8. **Interactive visual exploration** — Results are browsable in the web portal with Sigma.js graph views, Mermaid diagrams, and an AST explorer — giving architects and developers a shared understanding of the legacy system.

9. **Preprocessor-aware parsing** — Handles IMS/DLI calls, CICS EXEC statements, and dialect-specific extensions through a preprocessing step, so real-world enterprise COBOL parses correctly out of the box.

10. **Feeds AI-driven migration** — All structural intelligence (ASTs, CFGs, dependencies) is consumed by the migration agents as grounding context, producing higher-quality Java output that preserves the original business logic and control flow.
