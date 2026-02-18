# Spec-Driven Code Generation: Implementation Plan

## Pipeline

```
Analyze → Plan → Clarify → Generate
           ↑         ↑         ↑
      Constitution  User   Constitution
                  Answers   + Plan
```

**Analyze** already exists. The three new pieces are below.

---

## 1. Constitution

User-provided coding standards (YAML) injected into the system prompt. Loaded via a `constitutionFile` field in `GenerationProfiles.json`.

```yaml
architecture:
  decomposition: by-business-domain
  shared-copybooks: shared-library
  state-management: explicit-state-objects

naming:
  packages: com.acme.migration.{domain}
  methods: camelCase, verb-first

patterns:
  exceptions: unchecked
  null-handling: Optional for return types
  di: constructor-injection only
  date-time: java.time only

logging:
  framework: SLF4J

testing:
  framework: JUnit 5
  style: given-when-then

documentation:
  javadoc: all public classes and methods
  cobol-traceability: "// Migrated from: {original-paragraph}"
```

**What to build:**
- Constitution loader in `PromptLoader` / `ProfileManager`
- `constitutionFile` field in `GenerationProfiles.json` profiles
- Inject constitution content into system prompt alongside existing tool rules

**LLM cost:** Zero — prompt content only.

---

## 2. Plan

New `ConversionPlannerAgent` that takes `CobolAnalysis` + Constitution and outputs a `ConversionPlan` (target classes, methods, package layout, COBOL-to-Java mapping).

```csharp
public interface IConversionPlannerAgent
{
    Task<ConversionPlan> CreatePlanAsync(
        CobolFile cobolFile, CobolAnalysis analysis, Constitution constitution);
}
```

The plan is passed to the converter so each chunk has forward knowledge of the full structure.

**What to build:**
- `ConversionPlan` model (target classes, methods, shared types, package structure)
- `ConversionPlannerAgent` (new agent + prompt)
- Overloads on `ConvertAsync` / `ConvertChunkAsync` accepting a plan
- Persist plans as JSON for review and re-use

**LLM cost:** +1 call per file.

---

## 3. Clarify

Plan generation surfaces ambiguities the constitution doesn't cover. In the portal, these become chat questions. In CLI batch mode, the constitution's policy defaults auto-resolve them.

**What to build:**
- `ClarificationRequest` / `ClarificationResponse` models
- Portal: chat-based Q&A flow before generation
- CLI: auto-resolve from constitution defaults; two-phase batch if needed

**LLM cost:** +0–1 calls (ambiguities come from plan; resolution may need one refinement call).

---

## Order

1. **Constitution** — low effort, zero extra LLM cost, immediate quality improvement
2. **Plan** — medium effort, solves chunk forward-reference problem
3. **Clarify** — portal-only initially, depends on Plan
