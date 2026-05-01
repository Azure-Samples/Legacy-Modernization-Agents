namespace McpChatWeb.Services;

/// <summary>
/// Static prompt generation helpers for the Prompt Studio portal.
/// Extracted from Program.cs to reduce file size.
/// </summary>
public static class PromptBuilders
{
	public static string BuildJavaPrompt(List<string> programs, List<string> copybooks, HashSet<string> features, string archPattern, int totalLines, bool chunked = false)
	{
		var sb = new System.Text.StringBuilder();
		sb.AppendLine("You are a COBOL-to-Java/Quarkus conversion specialist.");
		sb.AppendLine();
		sb.AppendLine($"## Source Codebase Profile");
		sb.AppendLine($"- **Programs**: {programs.Count} | **Copybooks**: {copybooks.Count} | **Total lines**: {totalLines:N0}");
		sb.AppendLine($"- **Architecture pattern**: {archPattern}");
		sb.AppendLine($"- **Detected features**: {string.Join(", ", features.OrderBy(f => f))}");
		sb.AppendLine();
		sb.AppendLine("## Conversion Rules");
		sb.AppendLine("- Produce ONE Java class per COBOL program — NO abstract base classes, NO helper utilities, NO factory patterns.");
		sb.AppendLine("- Every paragraph/section in PROCEDURE DIVISION → a private method. Preserve names (kebab-case → camelCase).");
		sb.AppendLine("- All WORKING-STORAGE variables → class-level fields with exact same data types (PIC 9 → int/long/BigDecimal, PIC X → String).");
		sb.AppendLine("- PERFORM UNTIL loops → while loops with identical exit conditions.");
		sb.AppendLine("- EVALUATE → switch expressions. 88-level → boolean constants or enums.");
		sb.AppendLine();

		if (features.Contains("EXEC_SQL"))
		{
			sb.AppendLine("## Database Access (EXEC SQL detected)");
			sb.AppendLine("- Replace all EXEC SQL with Panache repository pattern.");
			sb.AppendLine("- Each COBOL record layout (01-level in WORKING-STORAGE used with SQL) → a @Entity JPA class.");
			sb.AppendLine("- EXEC SQL SELECT → repository.find() or repository.list(). Preserve WHERE clause logic exactly.");
			sb.AppendLine("- EXEC SQL INSERT/UPDATE/DELETE → repository.persist()/merge()/delete().");
			sb.AppendLine("- SQL CURSOR DECLARE/OPEN/FETCH/CLOSE → Panache streaming or paginated queries.");
			sb.AppendLine("- SQLCODE checks → proper exception handling with @Transactional boundaries.");
			sb.AppendLine();
		}
		if (features.Contains("EXEC_CICS") || features.Contains("CICS_SCREEN"))
		{
			sb.AppendLine("## Online Transaction Processing (CICS detected)");
			sb.AppendLine("- EXEC CICS SEND MAP / RECEIVE MAP → JAX-RS @POST/@GET REST endpoints returning JSON.");
			sb.AppendLine("- BMS map field names → DTO class fields. DFHCOMMAREA → request/response DTOs.");
			sb.AppendLine("- EXEC CICS LINK/XCTL → CDI @Inject of target service + method call.");
			sb.AppendLine("- EXEC CICS READ/WRITE/REWRITE/DELETE with DATASET → Panache repository calls.");
			sb.AppendLine("- EIBCALEN/EIBTRNID checks → @PathParam or request validation logic.");
			sb.AppendLine();
		}
		if (features.Contains("FILE_IO"))
		{
			sb.AppendLine("## File I/O (VSAM/sequential file access detected)");
			sb.AppendLine("- SELECT...ASSIGN → Java NIO Path configuration via @ConfigProperty.");
			sb.AppendLine("- FD record layout → a Java record/POJO. Each field → typed field.");
			sb.AppendLine("- OPEN/READ/WRITE/CLOSE → BufferedReader/BufferedWriter with try-with-resources.");
			sb.AppendLine("- FILE STATUS checks → IOException handling with meaningful error messages.");
			sb.AppendLine();
		}
		if (features.Contains("BATCH_LOOP"))
		{
			sb.AppendLine("## Batch Processing (PERFORM UNTIL loops detected)");
			sb.AppendLine("- Main batch PERFORM UNTIL → Quarkus @Scheduled method or a CommandLineRunner.");
			sb.AppendLine("- File-driven batch → reactive Mutiny stream: read → transform → write.");
			sb.AppendLine("- Commit-point logic (EXEC SQL COMMIT) → @Transactional with batch-size chunking.");
			sb.AppendLine();
		}
		if (features.Contains("ARITHMETIC"))
		{
			sb.AppendLine("## Arithmetic / Calculations");
			sb.AppendLine("- COMPUTE → direct Java expressions. Use BigDecimal for PIC 9(n)V9(m) fields.");
			sb.AppendLine("- ON SIZE ERROR → ArithmeticException or BigDecimal overflow checks.");
			sb.AppendLine("- ROUNDED → BigDecimal.setScale(n, RoundingMode.HALF_UP).");
			sb.AppendLine();
		}
		if (features.Contains("STRING_HANDLING"))
		{
			sb.AppendLine("## String Handling");
			sb.AppendLine("- STRING...DELIMITED BY → StringBuilder with custom delimiter logic.");
			sb.AppendLine("- UNSTRING → String.split() or regex-based parsing.");
			sb.AppendLine("- INSPECT TALLYING/REPLACING → String methods (indexOf, replace, chars().filter()).");
			sb.AppendLine();
		}
		if (features.Contains("COPYBOOK_REF"))
		{
			sb.AppendLine("## Copybook References Detected");
			sb.AppendLine("- Each COPY member used in WORKING-STORAGE → a shared Java record/POJO in a `model` package.");
			sb.AppendLine("- Ensure all programs referencing the same copybook use the **same** generated class (no duplication).");
			sb.AppendLine();
		}
		if (features.Contains("CALL_PROGRAM"))
		{
			sb.AppendLine("## Inter-Program CALL Chains");
			sb.AppendLine("- CALL 'PROGRAM' USING → @Inject ProgramService + method call passing parameters as method args.");
			sb.AppendLine("- LINKAGE SECTION → method parameters. RETURNING → method return type.");
			sb.AppendLine();
		}

		if (chunked)
		{
			sb.AppendLine("## Chunk Processing Instructions");
			sb.AppendLine("- This prompt is for chunk-aware conversion of large COBOL files split across multiple chunks.");
			sb.AppendLine("- Maintain class continuity across chunks — the first chunk opens the class, middle chunks add methods, the last chunk closes it.");
			sb.AppendLine("- Track WORKING-STORAGE variables from earlier chunks when converting PROCEDURE DIVISION in later chunks.");
			sb.AppendLine();
		}

		sb.AppendLine("## Output Requirements");
		sb.AppendLine("- Return COMPLETE, compilable Java code. No TODOs, no placeholders, no 'implement here' comments.");
		sb.AppendLine("- Include all imports. Use Quarkus CDI annotations (@ApplicationScoped, @Inject, @Transactional).");
		sb.AppendLine("- Class name = COBOL program name in PascalCase + 'Service' (e.g., BDSDA2F → Bdsda2fService).");

		return sb.ToString();
	}

	public static string BuildCSharpPrompt(List<string> programs, List<string> copybooks, HashSet<string> features, string archPattern, int totalLines, bool chunked = false)
	{
		var sb = new System.Text.StringBuilder();
		sb.AppendLine("You are a COBOL-to-C#/.NET conversion specialist.");
		sb.AppendLine();
		sb.AppendLine($"## Source Codebase Profile");
		sb.AppendLine($"- **Programs**: {programs.Count} | **Copybooks**: {copybooks.Count} | **Total lines**: {totalLines:N0}");
		sb.AppendLine($"- **Architecture pattern**: {archPattern}");
		sb.AppendLine($"- **Detected features**: {string.Join(", ", features.OrderBy(f => f))}");
		sb.AppendLine();
		sb.AppendLine("## Conversion Rules");
		sb.AppendLine("- Produce ONE C# class per COBOL program — NO abstract base classes, NO utility helpers.");
		sb.AppendLine("- Every paragraph/section in PROCEDURE DIVISION → a private method. Preserve names (kebab-case → PascalCase).");
		sb.AppendLine("- All WORKING-STORAGE variables → class-level fields (PIC 9 → int/long/decimal, PIC X → string).");
		sb.AppendLine("- PERFORM UNTIL → while loops. EVALUATE → switch expressions.");
		sb.AppendLine("- Use file-scoped namespaces, primary constructors where appropriate, async/await for I/O.");
		sb.AppendLine();

		if (features.Contains("EXEC_SQL"))
		{
			sb.AppendLine("## Database Access (EXEC SQL detected)");
			sb.AppendLine("- Replace EXEC SQL with Entity Framework Core.");
			sb.AppendLine("- COBOL record layouts (01-level with SQL) → EF entity class with [Table] attribute.");
			sb.AppendLine("- SELECT → dbContext.Set<T>().Where(...). INSERT → dbContext.Add(). UPDATE → tracked entity change + SaveChanges().");
			sb.AppendLine("- CURSOR logic → .AsAsyncEnumerable() or streaming with IAsyncEnumerable<T>.");
			sb.AppendLine("- SQLCODE checks → try/catch with DbUpdateException.");
			sb.AppendLine();
		}
		if (features.Contains("EXEC_CICS") || features.Contains("CICS_SCREEN"))
		{
			sb.AppendLine("## Online Transaction Processing (CICS detected)");
			sb.AppendLine("- SEND MAP / RECEIVE MAP → ASP.NET Minimal API endpoints or Blazor components.");
			sb.AppendLine("- BMS map fields → DTO record class. DFHCOMMAREA → request/response records.");
			sb.AppendLine("- EXEC CICS LINK/XCTL → DI-injected service call.");
			sb.AppendLine("- EXEC CICS READ/WRITE with DATASET → EF Core repository operations.");
			sb.AppendLine();
		}
		if (features.Contains("FILE_IO"))
		{
			sb.AppendLine("## File I/O (file access detected)");
			sb.AppendLine("- SELECT...ASSIGN → IConfiguration-based file path settings.");
			sb.AppendLine("- FD record → C# record. OPEN/READ/WRITE/CLOSE → StreamReader/StreamWriter with async and using.");
			sb.AppendLine("- FILE STATUS → IOException/FileNotFoundException handling.");
			sb.AppendLine();
		}
		if (features.Contains("BATCH_LOOP"))
		{
			sb.AppendLine("## Batch Processing (loop patterns detected)");
			sb.AppendLine("- Main batch loop → BackgroundService.ExecuteAsync() or IHostedService.");
			sb.AppendLine("- File-driven batch → IAsyncEnumerable pipeline: read → transform → write.");
			sb.AppendLine("- Commit points → SaveChangesAsync() with configurable batch size.");
			sb.AppendLine();
		}
		if (features.Contains("ARITHMETIC"))
		{
			sb.AppendLine("## Arithmetic / Calculations");
			sb.AppendLine("- COMPUTE → direct C# expressions. Use decimal for PIC 9(n)V9(m) fields.");
			sb.AppendLine("- ON SIZE ERROR → checked arithmetic context or OverflowException.");
			sb.AppendLine("- ROUNDED → Math.Round(value, decimals, MidpointRounding.AwayFromZero).");
			sb.AppendLine();
		}
		if (features.Contains("STRING_HANDLING"))
		{
			sb.AppendLine("## String Handling");
			sb.AppendLine("- STRING → StringBuilder or string interpolation with delimiter logic.");
			sb.AppendLine("- UNSTRING → string.Split() or Span<char>.");
			sb.AppendLine("- INSPECT → string.Replace(), Linq Count(), regex.");
			sb.AppendLine();
		}
		if (features.Contains("COPYBOOK_REF"))
		{
			sb.AppendLine("## Copybook References Detected");
			sb.AppendLine("- Each COPY member → shared C# record in a `Models` namespace.");
			sb.AppendLine("- All programs referencing the same copybook use the **same** generated record type.");
			sb.AppendLine();
		}
		if (features.Contains("CALL_PROGRAM"))
		{
			sb.AppendLine("## Inter-Program CALL Chains");
			sb.AppendLine("- CALL 'PROGRAM' USING → DI-injected service + method call.");
			sb.AppendLine("- LINKAGE SECTION → method parameters. RETURNING → return type.");
			sb.AppendLine();
		}

		if (chunked)
		{
			sb.AppendLine("## Chunk Processing Instructions");
			sb.AppendLine("- Large file split across chunks — maintain class continuity.");
			sb.AppendLine("- First chunk opens the class, middle chunks add methods, last chunk closes it.");
			sb.AppendLine("- Track WORKING-STORAGE from earlier chunks when converting PROCEDURE DIVISION.");
			sb.AppendLine();
		}

		sb.AppendLine("## Output Requirements");
		sb.AppendLine("- Return COMPLETE, compilable C# code. No TODOs, no placeholders.");
		sb.AppendLine("- Use .NET dependency injection, async/await, file-scoped namespaces.");
		sb.AppendLine("- Class name = COBOL program name in PascalCase + 'Service' (e.g., BDSDA2F → Bdsda2fService).");

		return sb.ToString();
	}

	public static string BuildAnalyzerPrompt(List<string> programs, List<string> copybooks, HashSet<string> features, int totalLines)
	{
		var sb = new System.Text.StringBuilder();
		sb.AppendLine($"Analyze the following COBOL codebase: {programs.Count} program(s), {copybooks.Count} copybook(s), {totalLines:N0} total lines.");
		sb.AppendLine();
		sb.AppendLine("## Detected Features to Investigate");
		if (features.Contains("EXEC_SQL")) sb.AppendLine("- **Embedded SQL**: Map all database tables, queries, cursors. Document SQLCODE error handling paths.");
		if (features.Contains("EXEC_CICS")) sb.AppendLine("- **CICS Transactions**: Document all SEND/RECEIVE MAP flows, LINK/XCTL chains, COMMAREA usage.");
		if (features.Contains("CICS_SCREEN")) sb.AppendLine("- **Screen Handling**: Map BMS screen definitions to data flow. Document user interaction sequences.");
		if (features.Contains("FILE_IO")) sb.AppendLine("- **File I/O**: Identify all file definitions (FD/SELECT), access modes, record structures.");
		if (features.Contains("BATCH_LOOP")) sb.AppendLine("- **Batch Processing**: Identify batch boundaries, checkpoint/restart logic, end-of-file handling.");
		if (features.Contains("CALL_PROGRAM")) sb.AppendLine("- **Program CALLs**: Trace CALL chains and shared LINKAGE SECTION parameters.");
		if (features.Contains("COPYBOOK_REF")) sb.AppendLine("- **Copybook Dependencies**: Map which copybooks are used by which programs. Flag shared data structures.");
		if (features.Contains("SORT_MERGE")) sb.AppendLine("- **SORT/MERGE operations**: Document sort keys, input/output procedures.");
		if (features.Contains("ARITHMETIC")) sb.AppendLine("- **Calculations**: Identify precision-sensitive arithmetic, rounding rules, size error handling.");
		sb.AppendLine();
		sb.AppendLine("## Required Output Structure");
		sb.AppendLine("1. **Program Inventory** — table of all programs with purpose, complexity rating, and key features.");
		sb.AppendLine("2. **Data Flow Analysis** — how data moves between programs, files, and databases.");
		sb.AppendLine("3. **Dependency Graph** — CALL chains, COPY relationships, shared data areas.");
		sb.AppendLine("4. **Modernization Complexity** — rate each program as low/medium/high/very-high with justification.");
		sb.AppendLine("5. **Recommended Migration Order** — which programs to convert first based on dependencies.");

		return sb.ToString();
	}

	public static string BuildBusinessLogicPrompt(List<string> programs, List<string> copybooks, HashSet<string> features)
	{
		var sb = new System.Text.StringBuilder();
		sb.AppendLine($"Extract business logic from the COBOL codebase ({programs.Count} programs, {copybooks.Count} copybooks).");
		sb.AppendLine();
		sb.AppendLine("## Extraction Focus Areas");
		sb.AppendLine("For each program, extract:");
		sb.AppendLine("1. **Business Purpose** — what business function does this program serve?");
		sb.AppendLine("2. **Business Rules** — all IF/EVALUATE conditions that encode business decisions (not just flow control).");
		sb.AppendLine("3. **Validations** — input validation rules, range checks, cross-field validations.");
		sb.AppendLine("4. **Calculations** — formulas, rates, accumulations with exact precision requirements.");
		sb.AppendLine("5. **State Transitions** — how records/transactions change state through processing.");
		sb.AppendLine();
		if (features.Contains("EXEC_SQL")) sb.AppendLine("- **Data Rules**: Extract business meaning of each SQL query — not just the SQL, but what business operation it represents.");
		if (features.Contains("EXEC_CICS")) sb.AppendLine("- **Transaction Rules**: Extract the business workflow encoded in CICS transaction flows.");
		if (features.Contains("ARITHMETIC")) sb.AppendLine("- **Calculation Rules**: Document every COMPUTE/ADD/SUBTRACT/MULTIPLY/DIVIDE with its business meaning and precision.");
		sb.AppendLine();
		sb.AppendLine("## Output Format");
		sb.AppendLine("Describe business logic in **domain language**, not COBOL syntax. A business analyst should understand the output without knowing COBOL.");

		return sb.ToString();
	}

	public static string BuildDependencyPrompt(List<string> programs, List<string> copybooks, HashSet<string> features)
	{
		var sb = new System.Text.StringBuilder();
		sb.AppendLine($"Map dependencies across the COBOL codebase:");
		sb.AppendLine($"- **{programs.Count}** programs: {string.Join(", ", programs.Take(15).Select(Path.GetFileName))}");
		if (programs.Count > 15) sb.AppendLine($"  ... and {programs.Count - 15} more");
		sb.AppendLine($"- **{copybooks.Count}** copybooks: {string.Join(", ", copybooks.Take(15).Select(Path.GetFileName))}");
		if (copybooks.Count > 15) sb.AppendLine($"  ... and {copybooks.Count - 15} more");
		sb.AppendLine();
		sb.AppendLine("## Dependency Types to Map");
		sb.AppendLine("1. **COPY dependencies** — which programs include which copybooks (COPY statements).");
		sb.AppendLine("2. **CALL chains** — which programs CALL which other programs. Include USING parameters.");
		if (features.Contains("EXEC_SQL")) sb.AppendLine("3. **Database tables** — which programs access which tables via EXEC SQL.");
		if (features.Contains("FILE_IO")) sb.AppendLine("4. **Files** — which programs read/write which files (SELECT...ASSIGN).");
		if (features.Contains("EXEC_CICS")) sb.AppendLine("5. **CICS resources** — MAP names, TRANSACTION ids, DATASET/FILE names.");
		sb.AppendLine();
		sb.AppendLine("## Output Format");
		sb.AppendLine("Generate a Mermaid dependency diagram AND a structured table listing all relationships.");

		return sb.ToString();
	}

	public static string BuildGenericPrompt(List<string> programs, List<string> copybooks, HashSet<string> features, int totalLines)
	{
		var sb = new System.Text.StringBuilder();
		sb.AppendLine($"Process the COBOL codebase: {programs.Count} programs, {copybooks.Count} copybooks, {totalLines:N0} lines.");
		sb.AppendLine();
		sb.AppendLine("Detected features: " + string.Join(", ", features.OrderBy(f => f)) + ".");
		sb.AppendLine();
		sb.AppendLine("Provide a comprehensive analysis and conversion-ready assessment of this codebase.");

		return sb.ToString();
	}

	public static string BuildUserPromptTemplate(string promptId, HashSet<string> features, int programCount, int copybookCount)
	{
		var sb = new System.Text.StringBuilder();

		if (promptId.Contains("java"))
		{
			sb.AppendLine("Convert the following COBOL program to Java with Quarkus.");
			sb.AppendLine();
			sb.AppendLine("## COBOL Source Code");
			sb.AppendLine("```cobol");
			sb.AppendLine("{{CobolContent}}");
			sb.AppendLine("```");
			sb.AppendLine();
			sb.AppendLine("## Analysis of the COBOL Program");
			sb.AppendLine("{{Analysis}}");
			if (features.Contains("EXEC_SQL") || features.Contains("EXEC_CICS") || features.Contains("CALL_PROGRAM"))
			{
				sb.AppendLine();
				sb.AppendLine("## Business Logic Context (from reverse engineering)");
				sb.AppendLine("{{BusinessLogicContext}}");
			}
			sb.AppendLine();
			sb.AppendLine("## Requirements");
			sb.AppendLine("1. Return ONLY the Java code — no explanations, no markdown blocks.");
			sb.AppendLine("2. Start with: package com.example.something;");
			sb.AppendLine("3. Must be valid, compilable Java starting with 'package' and ending with the class closing brace.");
			if (features.Contains("EXEC_SQL")) sb.AppendLine("4. Use Panache repository pattern for all database access.");
			if (features.Contains("EXEC_CICS")) sb.AppendLine("5. Use JAX-RS endpoints for all CICS transaction replacements.");
		}
		else if (promptId.Contains("csharp"))
		{
			sb.AppendLine("Convert the following COBOL program to C# with .NET.");
			sb.AppendLine();
			sb.AppendLine("## COBOL Source Code");
			sb.AppendLine("```cobol");
			sb.AppendLine("{{CobolContent}}");
			sb.AppendLine("```");
			sb.AppendLine();
			sb.AppendLine("## Analysis of the COBOL Program");
			sb.AppendLine("{{Analysis}}");
			if (features.Contains("EXEC_SQL") || features.Contains("EXEC_CICS") || features.Contains("CALL_PROGRAM"))
			{
				sb.AppendLine();
				sb.AppendLine("## Business Logic Context (from reverse engineering)");
				sb.AppendLine("{{BusinessLogicContext}}");
			}
			sb.AppendLine();
			sb.AppendLine("## Requirements");
			sb.AppendLine("1. Return ONLY the C# code — no explanations, no markdown blocks.");
			sb.AppendLine("2. Use file-scoped namespaces and async/await for all I/O.");
			sb.AppendLine("3. Must be valid, compilable C# code.");
			if (features.Contains("EXEC_SQL")) sb.AppendLine("4. Use Entity Framework Core for all database access.");
			if (features.Contains("EXEC_CICS")) sb.AppendLine("5. Use ASP.NET Minimal API endpoints for CICS replacements.");
		}
		else if (promptId.Contains("analyzer") || promptId.Contains("cobol"))
		{
			sb.AppendLine("Analyze the following COBOL program in detail.");
			sb.AppendLine();
			sb.AppendLine("## COBOL Source Code");
			sb.AppendLine("```cobol");
			sb.AppendLine("{{CobolContent}}");
			sb.AppendLine("```");
			sb.AppendLine();
			sb.AppendLine("## Required Output");
			sb.AppendLine("1. Program purpose and business domain");
			sb.AppendLine("2. Data structures and record layouts");
			sb.AppendLine("3. Processing logic flow (paragraph by paragraph)");
			sb.AppendLine("4. External dependencies (files, databases, called programs)");
			sb.AppendLine("5. Complexity assessment and modernization recommendations");
		}
		else if (promptId.Contains("business") || promptId.Contains("extractor"))
		{
			sb.AppendLine("Extract the business logic from the following COBOL program.");
			sb.AppendLine();
			sb.AppendLine("## Glossary Context");
			sb.AppendLine("{{GlossaryContext}}");
			sb.AppendLine();
			sb.AppendLine("## Source File: {{FileName}}");
			sb.AppendLine("```cobol");
			sb.AppendLine("{{CobolContent}}");
			sb.AppendLine("```");
			sb.AppendLine();
			sb.AppendLine("## Extraction Requirements");
			sb.AppendLine("1. Business rules in domain language (not COBOL syntax)");
			sb.AppendLine("2. Validations and data transformations");
			sb.AppendLine("3. Calculations with precision requirements");
			sb.AppendLine("4. Decision trees and state transitions");
		}
		else if (promptId.Contains("dependency") || promptId.Contains("mapper"))
		{
			sb.AppendLine("Map the dependencies for the following COBOL program.");
			sb.AppendLine();
			sb.AppendLine("## COBOL Source Code");
			sb.AppendLine("```cobol");
			sb.AppendLine("{{CobolContent}}");
			sb.AppendLine("```");
			sb.AppendLine();
			sb.AppendLine("## Required Output");
			sb.AppendLine("1. COPY dependencies (included copybooks)");
			sb.AppendLine("2. CALL chains (programs called with USING parameters)");
			if (features.Contains("EXEC_SQL")) sb.AppendLine("3. Database tables accessed via EXEC SQL");
			if (features.Contains("FILE_IO")) sb.AppendLine("4. File definitions (SELECT/ASSIGN/FD)");
			sb.AppendLine();
			sb.AppendLine("Generate a Mermaid diagram AND a structured dependency table.");
		}
		else
		{
			sb.AppendLine("Process the following COBOL source code.");
			sb.AppendLine();
			sb.AppendLine("```cobol");
			sb.AppendLine("{{CobolContent}}");
			sb.AppendLine("```");
			sb.AppendLine();
			sb.AppendLine("Provide comprehensive analysis and output.");
		}

		return sb.ToString();
	}
}
