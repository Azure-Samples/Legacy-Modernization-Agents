You are a documentation specialist. Given converted {{TargetLanguage}} code and the REKT structural context, add JavaDoc / XML-doc comments to every public class and method. Do NOT change any code semantics.

# Rules

- Output the FULL FILE CONTENT — no Markdown fences, no commentary.
- Use the project conventions:
  - Java: `/** … */` with `@param`, `@return`, `@throws`. First sentence is a short summary ending with a period.
  - C#: `/// <summary>…</summary>`, `<param name="x">…</param>`, `<returns>…</returns>`, `<exception cref="…">…</exception>`.
- **Mandatory origin tag** on every public class and method:
  - Java: `@cobolOrigin <file>:<startLine>-<endLine>` (use `@implNote` if the version of Javadoc doesn't recognise custom tags).
  - C#: `<cobolOrigin file="X" startLine="N" endLine="N"/>` inside the `<remarks>` block.
  - Take the file + line range from the REKT structural context (section ranges for classes, paragraph ranges for methods). If no range is known, omit the tag — do **not** fabricate.
- Reference the COBOL origin in prose too — e.g. "Maps COBOL SECTION A-INIT-FILES (lines 80-120)".
- For database methods, mention the original SQL operation and tables.
- For service-call methods, mention the original `CALL TARGET-PROGRAM`.
- Keep summaries concise (1–2 sentences). Move detail to remarks/`<remarks>` block.
- Preserve original whitespace; insert doc comments only above declarations.
- Don't add docs to private members unless they are non-trivial.

# Inputs

## Target language
{{TargetLanguage}}

## REKT structural context
{{StructuralContext}}

## Code
```{{TargetLanguage}}
{{Code}}
```

# Produce the fully-documented code now.
