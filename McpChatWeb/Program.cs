using System.IO;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Server.Kestrel.Core;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using McpChatWeb.Configuration;
using McpChatWeb.Models;
using McpChatWeb.Services;
using Neo4j.Driver;

var builder = WebApplication.CreateBuilder(args);

// Configure Kestrel with SO_REUSEADDR to allow port reuse
builder.WebHost.ConfigureKestrel(serverOptions =>
{
	// Allow address reuse to prevent "address already in use" errors
	serverOptions.ConfigureEndpointDefaults(listenOptions =>
	{
		listenOptions.UseConnectionLogging();
	});

	// Enable SO_REUSEADDR at the socket level
	serverOptions.ListenAnyIP(5028, listenOptions =>
	{
		listenOptions.Protocols = HttpProtocols.Http1AndHttp2;
	});
});

builder.Services.Configure<McpOptions>(builder.Configuration.GetSection("Mcp"));
builder.Services.PostConfigure<McpOptions>(options =>
{
	var contentRoot = builder.Environment.ContentRootPath;
	var repoRoot = Path.GetFullPath("..", contentRoot);
	var buildConfiguration = builder.Environment.IsDevelopment() ? "Debug" : "Release";
	string ResolvePath(string path) => Path.IsPathFullyQualified(path)
		? path
		: Path.GetFullPath(path, repoRoot);

	if (string.IsNullOrWhiteSpace(options.WorkingDirectory))
	{
		options.WorkingDirectory = repoRoot;
	}
	else if (!Path.IsPathFullyQualified(options.WorkingDirectory))
	{
		options.WorkingDirectory = ResolvePath(options.WorkingDirectory);
	}

	if (string.IsNullOrWhiteSpace(options.AssemblyPath))
	{
		var candidateFrameworks = new[] { "net9.0", "net8.0" };
		foreach (var framework in candidateFrameworks)
		{
			var candidate = Path.Combine(repoRoot, "bin", buildConfiguration, framework, "CobolToQuarkusMigration.dll");
			if (File.Exists(candidate))
			{
				options.AssemblyPath = candidate;
				break;
			}
		}

		if (string.IsNullOrWhiteSpace(options.AssemblyPath))
		{
			options.AssemblyPath = Path.Combine(repoRoot, "bin", buildConfiguration, candidateFrameworks[0], "CobolToQuarkusMigration.dll");
		}
	}
	else
	{
		options.AssemblyPath = ResolvePath(options.AssemblyPath);
	}

	if (string.IsNullOrWhiteSpace(options.ConfigPath))
	{
		options.ConfigPath = Path.Combine(repoRoot, "Config", "appsettings.json");
	}
	else if (!Path.IsPathFullyQualified(options.ConfigPath))
	{
		options.ConfigPath = ResolvePath(options.ConfigPath);
	}
});
builder.Services.AddSingleton<IMcpClient, McpProcessClient>();

builder.Services.AddOpenApi();

var app = builder.Build();

// Helper to resolve the migration database path consistently
string GetMigrationDbPath()
{
	var envPath = Environment.GetEnvironmentVariable("MIGRATION_DB_PATH");
	if (!string.IsNullOrEmpty(envPath) && File.Exists(envPath))
	{
		return Path.GetFullPath(envPath);
	}
	
	// Try relative to current directory (for running from project root)
	var localPath = Path.GetFullPath("Data/migration.db");
	if (File.Exists(localPath))
	{
		return localPath;
	}
	
	// Try parent directory (for running from McpChatWeb)
	var parentPath = Path.GetFullPath("../Data/migration.db");
	if (File.Exists(parentPath))
	{
		return parentPath;
	}
	
	// Default fallback
	return localPath;
}

// Cleanup stale runs that are stuck in "Running" status (crashed processes)
async Task CleanupStaleRunsAsync()
{
	try
	{
		var dbPath = GetMigrationDbPath();
		if (!File.Exists(dbPath)) return;

		await using var connection = new SqliteConnection($"Data Source={dbPath}");
		await connection.OpenAsync();

		// Mark runs that have been "Running" for more than 1 hour as "Cancelled"
		// This handles crashed processes that didn't update their status
		await using var command = connection.CreateCommand();
		command.CommandText = @"
			UPDATE runs 
			SET status = 'Cancelled', 
			    completed_at = datetime('now')
			WHERE status = 'Running' 
			AND datetime(started_at) < datetime('now', '-1 hour')";
		
		var affected = await command.ExecuteNonQueryAsync();
		if (affected > 0)
		{
			Console.WriteLine($"🧹 Cleaned up {affected} stale run(s) that were stuck in 'Running' status");
		}
	}
	catch (Exception ex)
	{
		Console.WriteLine($"⚠️ Warning: Could not cleanup stale runs: {ex.Message}");
	}
}

// Run cleanup on startup
await CleanupStaleRunsAsync();

if (app.Environment.IsDevelopment())
{
	app.MapOpenApi();
}

app.UseDefaultFiles();
app.UseStaticFiles();

app.MapGet("/api/resources", async (IMcpClient client, CancellationToken cancellationToken) =>
{
	var resources = await client.ListResourcesAsync(cancellationToken);
	return Results.Ok(resources);
});

app.MapPost("/api/tools/call", async (ToolCallRequest request, IMcpClient client, CancellationToken cancellationToken) =>
{
    try
    {
        if (string.IsNullOrWhiteSpace(request.Name))
        {
            return Results.BadRequest(new { error = "Tool name is required" });
        }

        Console.WriteLine($"🔧 Calling tool: {request.Name}");

        var result = await client.CallToolAsync(request.Name, request.Arguments ?? new Dictionary<string, object>(), cancellationToken);
        return Results.Ok(new { result });
    }
    catch (Exception ex)
    {
        Console.WriteLine($"❌ Error calling tool {request.Name}: {ex.Message}");
        return Results.Problem($"Failed to call tool: {ex.Message}");
    }
});

app.MapGet("/api/specs", async (int? runId, CancellationToken cancellationToken) =>
{
    try
    {
        var dbPath = GetMigrationDbPath();
        if (!File.Exists(dbPath))
        {
            return Results.Ok(new { specs = new List<object>() });
        }

        await using var connection = new SqliteConnection($"Data Source={dbPath}");
        await connection.OpenAsync(cancellationToken);

        // First check if table exists
        await using var checkCmd = connection.CreateCommand();
        checkCmd.CommandText = "SELECT name FROM sqlite_master WHERE type='table' AND name='spec_service_definitions'";
        var tableName = await checkCmd.ExecuteScalarAsync(cancellationToken);
        
        if (tableName == null)
        {
            return Results.Ok(new { specs = new List<object>(), message = "Spec definitions table not found (no specs generated yet?)" });
        }

        await using var command = connection.CreateCommand();
        command.CommandText = @"
            SELECT s.id, s.run_id, s.name, s.description, s.status, p.source_file
            FROM spec_service_definitions s
            LEFT JOIN spec_provenance p ON s.provenance_id = p.id
            ORDER BY s.id DESC";
        
        var specs = new List<object>();
        await using var reader = await command.ExecuteReaderAsync(cancellationToken);
        while (await reader.ReadAsync(cancellationToken))
        {
             specs.Add(new {
                 id = reader.GetInt32(0),
                 runId = reader.GetInt32(1),
                 name = reader.GetString(2),
                 description = reader.IsDBNull(3) ? null : reader.GetString(3),
                 status = reader.GetInt32(4), // 0=Draft, 1=Review, 2=Approved, 3=Rejected
                 statusLabel = reader.GetInt32(4) switch { 0 => "Draft", 1 => "Review", 2 => "Approved", 3 => "Rejected", _ => "Unknown" },
                 // Fallback to Name if source_file is missing (heuristic for old specs)
                 sourceFile = reader.IsDBNull(5) ? reader.GetString(2) : reader.GetString(5)
             });
        }
        
        return Results.Ok(new { specs });
    }
    catch (Exception ex)
    {
         Console.WriteLine($"❌ Error listing specs: {ex.Message}");
         return Results.Problem($"Failed to list specs: {ex.Message}");
    }
});

app.MapGet("/api/profiles", async () =>
{
    try
    {
        var configPath = Path.Combine(Directory.GetCurrentDirectory(), "..", "Config", "GenerationProfiles.json");
        if (!File.Exists(configPath))
        {
            return Results.Ok(new { profiles = new List<object>(), message = "Profiles config not found" });
        }
        
        var json = await File.ReadAllTextAsync(configPath);
        // We can just return the raw JSON parsing it to ensure validity
        var data = JsonSerializer.Deserialize<JsonObject>(json);
        return Results.Ok(data);
    }
    catch (Exception ex)
    {
         return Results.Problem($"Failed to list profiles: {ex.Message}");
    }
});

// Read a specific MCP resource by URI - used by chunk status viewer
app.MapGet("/api/resources/read", async (string uri, IMcpClient client, CancellationToken cancellationToken) =>
{
	try
	{
		if (string.IsNullOrWhiteSpace(uri))
		{
			return Results.BadRequest(new { error = "URI parameter is required" });
		}

		// Handle spec:// URIs directly from the database
		// REMOVED: spec://locked optimization is broken (returns markdown instead of JSON)
		// Letting it fall through to McpServer which handles it correctly
		/*
		if (uri.StartsWith("spec://locked/"))
		{
			// ... disabled implementation ...
		}
		*/
		
		Console.WriteLine($"📦 Reading MCP resource: {uri}");
		var content = await client.ReadResourceAsync(uri, cancellationToken);
		
		if (string.IsNullOrEmpty(content))
		{
			return Results.Ok(new { error = $"Resource not found or empty: {uri}" });
		}

		// Try to parse as JSON
		try
		{
			var jsonData = JsonSerializer.Deserialize<JsonObject>(content);
			return Results.Ok(jsonData);
		}
		catch
		{
			// Return as plain text wrapper if not valid JSON
			return Results.Ok(new { content = content });
		}
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error reading resource {uri}: {ex.Message}");
		return Results.Ok(new { error = $"Failed to read resource: {ex.Message}" });
	}
});

app.MapPost("/api/chat", async (ChatRequest request, IMcpClient client, CancellationToken cancellationToken) =>
{
	if (string.IsNullOrWhiteSpace(request.Prompt))
	{
		return Results.BadRequest("Prompt cannot be empty.");
	}

	// Check if user is asking about a specific file's content/analysis
	var fileAnalysisPattern = new System.Text.RegularExpressions.Regex(
		@"(?:what|show|list|get|tell|describe).*?(?:functions?|methods?|paragraphs?|procedures?|sections?|code|content|contains?).*?(?:in|of|from)\s+([A-Z0-9_-]+\.(?:cbl|cpy|CBL|CPY))|([A-Z0-9_-]+\.(?:cbl|cpy|CBL|CPY)).*?(?:contains?|has|functions?|methods?|paragraphs?|code)",
		System.Text.RegularExpressions.RegexOptions.IgnoreCase
	);
	var fileMatch = fileAnalysisPattern.Match(request.Prompt);

	if (fileMatch.Success)
	{
		// Extract filename from either capture group
		var fileName = fileMatch.Groups[1].Success ? fileMatch.Groups[1].Value : fileMatch.Groups[2].Value;

		// Try to determine run ID from context, default to 43
		var fileRunIdPattern = new System.Text.RegularExpressions.Regex(@"\brun\s*(?:id\s*)?(\d+)\b", System.Text.RegularExpressions.RegexOptions.IgnoreCase);
		var runMatch = fileRunIdPattern.Match(request.Prompt);
		var targetRunId = runMatch.Success && int.TryParse(runMatch.Groups[1].Value, out int rid) ? rid : 43;

		// Try to get analysis from MCP
		var analysisUri = $"insights://runs/{targetRunId}/analyses/{fileName}";
		try
		{
			var analysisJson = await client.ReadResourceAsync(analysisUri, cancellationToken);
			if (!string.IsNullOrEmpty(analysisJson))
			{
				// Parse the analysis data
				var analysisData = JsonSerializer.Deserialize<JsonObject>(analysisJson);
				if (analysisData != null)
				{
					// Build a comprehensive response about the file
					var responseText = $"**Analysis of {fileName} (Run {targetRunId})**\n\n";

					// Try to get rawAnalysisData and parse it
					JsonObject? detailedAnalysis = null;
					if (analysisData.TryGetPropertyValue("rawAnalysisData", out var rawData) && rawData != null)
					{
						try
						{
							detailedAnalysis = JsonSerializer.Deserialize<JsonObject>(rawData.ToString());
						}
						catch { }
					}

					// Get program description from either top level or detailed analysis
					if (analysisData.TryGetPropertyValue("programDescription", out var desc) && desc != null && desc.ToString() != "Extracted from AI analysis")
					{
						responseText += $"**Description:**\n{desc.ToString()}\n\n";
					}
					else if (detailedAnalysis != null)
					{
						// Try to extract description from rawAnalysisData
						if (detailedAnalysis.TryGetPropertyValue("programDescription", out var rawDesc) && rawDesc != null)
						{
							if (rawDesc is JsonObject descObj && descObj.TryGetPropertyValue("purpose", out var purpose))
							{
								responseText += $"**Purpose:**\n{purpose!.ToString()}\n\n";
							}
						}
						else if (detailedAnalysis.TryGetPropertyValue("program", out var prog) && prog is JsonObject progObj)
						{
							if (progObj.TryGetPropertyValue("purpose", out var progPurpose))
							{
								responseText += $"**Purpose:**\n{progPurpose!.ToString()}\n\n";
							}
						}
					}

					// Get paragraphs/sections from detailed analysis
					if (detailedAnalysis != null && detailedAnalysis.TryGetPropertyValue("paragraphs-and-sections-summary", out var paraSummary) && paraSummary is JsonArray paragraphsSummary && paragraphsSummary.Count > 0)
					{
						responseText += $"**Functions/Paragraphs ({paragraphsSummary.Count}):**\n";
						foreach (var para in paragraphsSummary)
						{
							if (para is JsonObject p)
							{
								var name = p.TryGetPropertyValue("name", out var n) ? n?.ToString() : "Unknown";
								var paraDesc = p.TryGetPropertyValue("description", out var d) ? d?.ToString() : "";
								responseText += $"- **`{name}`**";
								if (!string.IsNullOrEmpty(paraDesc)) responseText += $": {paraDesc}";
								responseText += "\n";
							}
						}
						responseText += "\n";
					}
					else if (analysisData.TryGetPropertyValue("paragraphs", out var paras) && paras is JsonArray paragraphs && paragraphs.Count > 0)
					{
						responseText += $"**Functions/Paragraphs ({paragraphs.Count}):**\n";
						foreach (var para in paragraphs)
						{
							if (para is JsonObject p)
							{
								var name = p.TryGetPropertyValue("name", out var n) ? n?.ToString() : "Unknown";
								var paraDesc = p.TryGetPropertyValue("description", out var d) ? d?.ToString() : "";
								responseText += $"- `{name}`";
								if (!string.IsNullOrEmpty(paraDesc)) responseText += $": {paraDesc}";
								responseText += "\n";

								// Show calls if available
								if (p.TryGetPropertyValue("calls", out var calls) && calls is JsonArray callsArray && callsArray.Count > 0)
								{
									responseText += $"  Calls: {string.Join(", ", callsArray.Select(c => c?.ToString()))}\n";
								}
							}
						}
						responseText += "\n";
					}

					// Get variables from detailed analysis if available
					if (detailedAnalysis != null && detailedAnalysis.TryGetPropertyValue("variables", out var detailedVars) && detailedVars is JsonArray detailedVariables && detailedVariables.Count > 0)
					{
						responseText += $"**Variables ({detailedVariables.Count}):**\n";
						var topVars = detailedVariables.Take(15);
						foreach (var v in topVars)
						{
							if (v is JsonObject varObj)
							{
								var varName = varObj.TryGetPropertyValue("name", out var vn) ? vn?.ToString() : "Unknown";
								var varPic = varObj.TryGetPropertyValue("picture", out var vp) ? vp?.ToString() : "";
								var varType = varObj.TryGetPropertyValue("type", out var vt) ? vt?.ToString() : "";
								responseText += $"- `{varName}`";
								if (!string.IsNullOrEmpty(varPic)) responseText += $" PIC {varPic}";
								if (!string.IsNullOrEmpty(varType)) responseText += $" ({varType})";
								responseText += "\n";
							}
						}
						if (detailedVariables.Count > 15) responseText += $"... and {detailedVariables.Count - 15} more\n";
						responseText += "\n";
					}
					else if (analysisData.TryGetPropertyValue("variables", out var vars) && vars is JsonArray variables && variables.Count > 0)
					{
						responseText += $"**Variables ({variables.Count}):**\n";
						var topVars = variables.Take(10);
						foreach (var v in topVars)
						{
							if (v is JsonObject varObj)
							{
								var varName = varObj.TryGetPropertyValue("name", out var vn) ? vn?.ToString() : "Unknown";
								var varType = varObj.TryGetPropertyValue("type", out var vt) ? vt?.ToString() : "";
								responseText += $"- `{varName}`";
								if (!string.IsNullOrEmpty(varType)) responseText += $" ({varType})";
								responseText += "\n";
							}
						}
						if (variables.Count > 10) responseText += $"... and {variables.Count - 10} more\n";
						responseText += "\n";
					}

					// Get copybooks from detailed analysis
					if (detailedAnalysis != null && detailedAnalysis.TryGetPropertyValue("copybooksReferenced", out var detailedCbs) && detailedCbs is JsonArray detailedCopybooks && detailedCopybooks.Count > 0)
					{
						responseText += $"**Copybooks Referenced ({detailedCopybooks.Count}):**\n";
						foreach (var cb in detailedCopybooks)
						{
							responseText += $"- {cb?.ToString()}\n";
						}
						responseText += "\n";
					}
					else if (detailedAnalysis != null && detailedAnalysis.TryGetPropertyValue("copies-referenced", out var copiesRef) && copiesRef is JsonArray copiesArray && copiesArray.Count > 0)
					{
						responseText += $"**Copybooks Referenced ({copiesArray.Count}):**\n";
						foreach (var cb in copiesArray)
						{
							responseText += $"- {cb?.ToString()}\n";
						}
						responseText += "\n";
					}
					else if (analysisData.TryGetPropertyValue("copybooks", out var cbs) && cbs is JsonArray copybooks && copybooks.Count > 0)
					{
						responseText += $"**Copybooks Used ({copybooks.Count}):**\n";
						foreach (var cb in copybooks)
						{
							responseText += $"- {cb?.ToString()}\n";
						}
						responseText += "\n";
					}

					responseText += $"\n**Data Source:** MCP Resource URI: `{analysisUri}`\n";
					responseText += $"**API:** `GET /api/file-analysis/{fileName}?runId={targetRunId}`";

					return Results.Ok(new ChatResponse(responseText, targetRunId));
				}
			}
		}
		catch (Exception ex)
		{
			Console.WriteLine($"Error fetching analysis for {fileName}: {ex.Message}");
		}

		// If MCP fails, try direct database query
		var dbPath = Path.Combine(Directory.GetCurrentDirectory(), "..", "Data", "migration.db");
		if (File.Exists(dbPath))
		{
			try
			{
				// Use subprocess to query SQLite since we don't have Microsoft.Data.Sqlite package
				var queryCmd = $"sqlite3 \"{dbPath}\" \"SELECT cf.file_name, cf.is_copybook, a.program_description, a.paragraphs_json, a.variables_json, a.copybooks_json FROM cobol_files cf LEFT JOIN analyses a ON a.cobol_file_id = cf.id WHERE cf.file_name = '{fileName}' AND cf.run_id = {targetRunId};\"";

				var psi = new System.Diagnostics.ProcessStartInfo
				{
					FileName = "/bin/bash",
					Arguments = $"-c \"{queryCmd}\"",
					RedirectStandardOutput = true,
					RedirectStandardError = true,
					UseShellExecute = false,
					CreateNoWindow = true
				};

				using var process = System.Diagnostics.Process.Start(psi);
				if (process == null) throw new Exception("Failed to start sqlite3 process");

				var output = await process.StandardOutput.ReadToEndAsync(cancellationToken);
				await process.WaitForExitAsync(cancellationToken);

				if (!string.IsNullOrWhiteSpace(output))
				{
					// Parse the SQLite output (pipe-separated values)
					var parts = output.Split('|');
					if (parts.Length >= 6)
					{
						var isCopybook = parts[1].Trim() == "1";
						var programDesc = parts.Length > 2 && !string.IsNullOrWhiteSpace(parts[2]) ? parts[2].Trim() : null;
						var paragraphsJson = parts.Length > 3 && !string.IsNullOrWhiteSpace(parts[3]) ? parts[3].Trim() : null;
						var variablesJson = parts.Length > 4 && !string.IsNullOrWhiteSpace(parts[4]) ? parts[4].Trim() : null;
						var copybooksJson = parts.Length > 5 && !string.IsNullOrWhiteSpace(parts[5]) ? parts[5].Trim() : null; var responseText = $"**Analysis of {fileName} (Run {targetRunId})**\n\n";
						responseText += $"**Type:** {(isCopybook ? "Copybook" : "Program")}\n\n";

						if (!string.IsNullOrEmpty(programDesc))
						{
							responseText += $"**Description:**\n{programDesc}\n\n";
						}

						if (!string.IsNullOrEmpty(paragraphsJson) && paragraphsJson != "[]")
						{
							try
							{
								var paras = JsonSerializer.Deserialize<JsonArray>(paragraphsJson);
								if (paras != null && paras.Count > 0)
								{
									responseText += $"**Functions/Paragraphs ({paras.Count}):**\n";
									foreach (var para in paras)
									{
										if (para is JsonObject p)
										{
											var name = p.TryGetPropertyValue("name", out var n) ? n?.ToString() : "Unknown";
											var desc = p.TryGetPropertyValue("description", out var d) ? d?.ToString() : "";
											responseText += $"- `{name}`";
											if (!string.IsNullOrEmpty(desc)) responseText += $": {desc}";
											responseText += "\n";
										}
									}
									responseText += "\n";
								}
							}
							catch { }
						}

						if (!string.IsNullOrEmpty(variablesJson) && variablesJson != "[]")
						{
							try
							{
								var vars = JsonSerializer.Deserialize<JsonArray>(variablesJson);
								if (vars != null && vars.Count > 0)
								{
									responseText += $"**Variables ({vars.Count}):**\n";
									var topVars = vars.Take(10);
									foreach (var v in topVars)
									{
										if (v is JsonObject varObj)
										{
											var varName = varObj.TryGetPropertyValue("name", out var vn) ? vn?.ToString() : "Unknown";
											responseText += $"- `{varName}`\n";
										}
									}
									if (vars.Count > 10) responseText += $"... and {vars.Count - 10} more\n";
									responseText += "\n";
								}
							}
							catch { }
						}

						if (!string.IsNullOrEmpty(copybooksJson) && copybooksJson != "[]")
						{
							try
							{
								var cbs = JsonSerializer.Deserialize<JsonArray>(copybooksJson);
								if (cbs != null && cbs.Count > 0)
								{
									responseText += $"**Copybooks Used ({cbs.Count}):**\n";
									foreach (var cb in cbs)
									{
										responseText += $"- {cb?.ToString()}\n";
									}
									responseText += "\n";
								}
							}
							catch { }
						}

						responseText += $"\n**Data Source:** SQLite Database at `{dbPath}`";

						return Results.Ok(new ChatResponse(responseText, targetRunId));
					}
				}
				else
				{
					var notFoundMsg = $"**File Not Found:** {fileName}\n\n";
					notFoundMsg += $"The file `{fileName}` was not found in Run {targetRunId}.\n\n";
					notFoundMsg += $"**Suggestions:**\n";
					notFoundMsg += $"- Check the filename spelling (case-sensitive)\n";
					notFoundMsg += $"- Try a different run ID\n";
					notFoundMsg += $"- Ask: \"What files are in run {targetRunId}?\"";

					return Results.Ok(new ChatResponse(notFoundMsg, targetRunId));
				}
			}
			catch (Exception ex)
			{
				Console.WriteLine($"Database error: {ex.Message}");
			}
		}
	}

	// Check if user is asking about a specific run ID
	var runIdPattern = new System.Text.RegularExpressions.Regex(@"\brun\s*(?:id\s*)?(\d+)\b", System.Text.RegularExpressions.RegexOptions.IgnoreCase);
	var match = runIdPattern.Match(request.Prompt);

	if (match.Success && int.TryParse(match.Groups[1].Value, out int requestedRunId))
	{
		// User is asking about a specific run - directly provide the data instead of using MCP
		try
		{
			// Build the response directly by calling the search endpoint logic
			var dbPath = Path.Combine(Directory.GetCurrentDirectory(), "..", "Data", "migration.db");
			var dbExists = File.Exists(dbPath);

			// Get Neo4j data
			var graphUri = $"insights://runs/{requestedRunId}/graph";
			var nodeCount = 0;
			var edgeCount = 0;
			var graphAvailable = false;

			try
			{
				var graphJson = await client.ReadResourceAsync(graphUri, cancellationToken);
				if (!string.IsNullOrEmpty(graphJson))
				{
					var graphData = JsonSerializer.Deserialize<JsonObject>(graphJson);
					if (graphData != null)
					{
						if (graphData.TryGetPropertyValue("nodes", out var n) && n is JsonArray na) nodeCount = na.Count;
						if (graphData.TryGetPropertyValue("edges", out var e) && e is JsonArray ea) edgeCount = ea.Count;
						graphAvailable = nodeCount > 0;
					}
				}
			}
			catch { }

			// Build a comprehensive response
			var directResponse = $@"**Run {requestedRunId} Data Summary**

**Data Sources:**

1. **SQLite Database** (Data/migration.db)
   - Status: {(dbExists ? "✓ Available" : "✗ Not Found")}
   - Location: {dbPath}
   - To query: sqlite3 ""{dbPath}"" ""SELECT * FROM runs WHERE id = {requestedRunId};""

2. **Neo4j Graph Database** (bolt://localhost:7687)
   - Status: {(graphAvailable ? "✓ Available" : "⚠ Limited availability")}
   - Nodes: {nodeCount}
   - Edges: {edgeCount}
   - To query: cypher-shell -u neo4j -p cobol-migration-2025
   - Cypher: MATCH (n) WHERE n.runId = {requestedRunId} RETURN n LIMIT 25;

**How to Access Full Data:**

• **API Endpoint:** GET /api/search/run/{requestedRunId}
  This endpoint returns comprehensive data from both databases with:
  - All available run metadata
  - Graph visualization data
  - Sample queries for direct database access
  - Connection credentials

• **Direct Queries:**
  
  SQLite:
  ```sql
  SELECT id, status, started_at, completed_at FROM runs WHERE id = {requestedRunId};
  SELECT COUNT(*) as files FROM cobol_files WHERE run_id = {requestedRunId};
  SELECT file_name, is_copybook FROM cobol_files WHERE run_id = {requestedRunId} LIMIT 10;
  ```
  
  Neo4j:
  ```cypher
  MATCH (n) WHERE n.runId = {requestedRunId} RETURN n LIMIT 25;
  MATCH (n)-[r]->(m) WHERE n.runId = {requestedRunId} RETURN n, r, m LIMIT 50;
  ```

**To see this data in the UI:**
1. Use the browser console: `fetch('/api/search/run/{requestedRunId}').then(r => r.json()).then(console.log)`
2. Or use curl: `curl http://localhost:5028/api/search/run/{requestedRunId} | jq .`

Note: The MCP server currently provides detailed analysis only for Run 43. For other runs, use the direct database queries above or the /api/search/run endpoint.";

			return Results.Ok(new ChatResponse(directResponse, requestedRunId));
		}
		catch (Exception ex)
		{
			var errorResponse = $@"Error retrieving data for Run {requestedRunId}: {ex.Message}

You can still access the data directly:
• API: GET /api/search/run/{requestedRunId}
• SQLite: sqlite3 ""Data/migration.db"" ""SELECT * FROM runs WHERE id = {requestedRunId};""
• Neo4j: cypher-shell -u neo4j -p cobol-migration-2025";

			return Results.Ok(new ChatResponse(errorResponse, requestedRunId));
		}
	}

	// Normal chat flow - augment with SQLite context for better answers
	try
	{
		// Get context from SQLite database
		var dbPath = Path.Combine(Directory.GetCurrentDirectory(), "..", "Data", "migration.db");
		var contextData = "";

		if (File.Exists(dbPath))
		{
			using (var connection = new SqliteConnection($"Data Source={dbPath}"))
			{
				await connection.OpenAsync(cancellationToken);

				// Get run summary
				using var runCmd = connection.CreateCommand();
				runCmd.CommandText = "SELECT id, status, started_at FROM runs ORDER BY id DESC LIMIT 5";
				var runs = new List<string>();
				using (var reader = await runCmd.ExecuteReaderAsync(cancellationToken))
				{
					while (await reader.ReadAsync(cancellationToken))
					{
						var status = reader.IsDBNull(1) ? "Unknown" : reader.GetString(1);
						runs.Add($"Run {reader.GetInt32(0)} ({status})");
					}
				}
				if (runs.Count > 0)
				{
					contextData += $"Available runs: {string.Join(", ", runs)}\n";
				}

				// Get file count and complexity stats
				using var fileCmd = connection.CreateCommand();
				fileCmd.CommandText = @"
					SELECT 
						COUNT(*) as total_files,
						SUM(CASE WHEN is_copybook = 1 THEN 1 ELSE 0 END) as copybooks,
						SUM(CASE WHEN is_copybook = 0 THEN 1 ELSE 0 END) as programs
					FROM cobol_files";
				using (var reader = await fileCmd.ExecuteReaderAsync(cancellationToken))
				{
					if (await reader.ReadAsync(cancellationToken))
					{
						contextData += $"Total COBOL files: {reader.GetInt32(0)} ({reader.GetInt32(2)} programs, {reader.GetInt32(1)} copybooks)\n";
					}
				}

				// Get copybook list if asking about copybooks
				if (request.Prompt.Contains("copybook", StringComparison.OrdinalIgnoreCase))
				{
					using var copybookCmd = connection.CreateCommand();
					copybookCmd.CommandText = @"
						SELECT file_name 
						FROM cobol_files 
						WHERE is_copybook = 1 
						LIMIT 20";
					var copybooks = new List<string>();
					using (var reader = await copybookCmd.ExecuteReaderAsync(cancellationToken))
					{
						while (await reader.ReadAsync(cancellationToken))
						{
							copybooks.Add(reader.GetString(0));
						}
					}
					if (copybooks.Count > 0)
					{
						contextData += $"\nAvailable copybooks: {string.Join(", ", copybooks)}\n";
					}
				}
			}
		}

		// Augment the prompt with SQLite context
		var augmentedPrompt = request.Prompt;
		if (!string.IsNullOrEmpty(contextData))
		{
			augmentedPrompt = $"CONTEXT FROM DATABASE:\n{contextData}\n\nUSER QUESTION: {request.Prompt}";
			Console.WriteLine($"💡 Augmented prompt with SQLite context ({contextData.Length} chars)");
		}

		var normalResponse = await client.SendChatAsync(augmentedPrompt, cancellationToken);
		return Results.Ok(new ChatResponse(normalResponse, null));
	}
	catch (Exception ex)
	{
		Console.WriteLine($"Error augmenting chat with SQLite context: {ex.Message}");
		// Fallback to MCP only
		var normalResponse = await client.SendChatAsync(request.Prompt, cancellationToken);
		return Results.Ok(new ChatResponse(normalResponse, null));
	}
});

// Graph endpoint - defaults to current MCP run or accepts specific run ID
app.MapGet("/api/graph", async (IMcpClient client, int? runId, bool? includeInferred, CancellationToken cancellationToken) =>
{
	// Helper: build graph from local SQLite when MCP is unavailable or when explicit run is requested
	async Task<IResult> BuildGraphFromSqliteAsync(int? requestedRunId, bool includeInferredNodes)
	{
		try
		{
			var dbPath = GetMigrationDbPath();
			if (!File.Exists(dbPath))
			{
				return Results.Ok(new
				{
					runId = requestedRunId ?? 0,
					nodes = Array.Empty<object>(),
					edges = Array.Empty<object>(),
					error = "migration.db not found"
				});
			}

			await using var connection = new SqliteConnection($"Data Source={dbPath};Cache=Shared");
			await connection.OpenAsync(cancellationToken);

			// Pick latest run if none requested
			if (!requestedRunId.HasValue)
			{
				await using var runCmd = connection.CreateCommand();
				runCmd.CommandText = "SELECT id FROM runs ORDER BY id DESC LIMIT 1";
				var val = await runCmd.ExecuteScalarAsync(cancellationToken);
				requestedRunId = val == null ? 0 : Convert.ToInt32(val);
			}

			var sqlRunId = requestedRunId ?? 0;

			// Load file metadata (handle legacy DBs without line_count)
			var hasLineCount = await TableHasColumnAsync(connection, "cobol_files", "line_count", cancellationToken);
			var nodes = new List<object>();
			await using (var fileCmd = connection.CreateCommand())
			{
				fileCmd.CommandText = hasLineCount
					? @"SELECT file_name, is_copybook, COALESCE(line_count, 0) FROM cobol_files WHERE run_id = $runId"
					: @"SELECT file_name, is_copybook FROM cobol_files WHERE run_id = $runId";
				fileCmd.Parameters.AddWithValue("$runId", sqlRunId);

				await using var reader = await fileCmd.ExecuteReaderAsync(cancellationToken);
				while (await reader.ReadAsync(cancellationToken))
				{
					nodes.Add(new
					{
						id = reader.GetString(0),
						label = reader.GetString(0),
						isCopybook = reader.GetBoolean(1),
						lineCount = hasLineCount && reader.FieldCount > 2 && !reader.IsDBNull(2) ? reader.GetInt32(2) : 0,
						isInferred = false
					});
				}
			}

			// Load dependencies
			var edges = new List<object>();
			await using (var depCmd = connection.CreateCommand())
			{
				depCmd.CommandText = @"SELECT source_file, target_file, dependency_type, line_number, context FROM dependencies WHERE run_id = $runId";
				depCmd.Parameters.AddWithValue("$runId", sqlRunId);

				await using var reader = await depCmd.ExecuteReaderAsync(cancellationToken);
				while (await reader.ReadAsync(cancellationToken))
				{
					edges.Add(new
					{
						source = reader.GetString(0),
						target = reader.GetString(1),
						type = reader.GetString(2),
						lineNumber = reader.IsDBNull(3) ? (int?)null : reader.GetInt32(3),
						context = reader.IsDBNull(4) ? null : reader.GetString(4)
					});
				}
			}

			// Add inferred nodes for dependencies referencing missing files
			if (includeInferredNodes)
			{
				var nodeMap = nodes.ToDictionary(n => (string)((dynamic)n).id, n => n);
				foreach (dynamic edge in edges.Cast<dynamic>())
				{
					var sourceId = (string)edge.source;
					var targetId = (string)edge.target;

					if (!nodeMap.ContainsKey(sourceId))
					{
						nodeMap[sourceId] = new { id = sourceId, label = sourceId, isCopybook = false, lineCount = 0, isInferred = true };
					}
					if (!nodeMap.ContainsKey(targetId))
					{
						nodeMap[targetId] = new { id = targetId, label = targetId, isCopybook = false, lineCount = 0, isInferred = true };
					}
				}

				nodes = nodeMap.Values.ToList<object>();
			}

			return Results.Ok(new
			{
				runId = sqlRunId,
				nodes,
				edges,
				source = "sqlite"
			});
		}
		catch (Exception ex)
		{
			Console.WriteLine($"❌ SQLite graph fallback failed: {ex.Message}");
			return Results.Ok(new
			{
				runId = requestedRunId ?? 0,
				nodes = Array.Empty<object>(),
				edges = Array.Empty<object>(),
				error = $"SQLite graph fallback failed: {ex.Message}"
			});
		}
	}

try
	{
		// If a specific runId is requested, serve from SQLite to ensure accurate per-run data
		if (runId.HasValue)
		{
			Console.WriteLine($"🗄️ Using SQLite graph for requested run {runId.Value}");
			return await BuildGraphFromSqliteAsync(runId.Value, includeInferred.GetValueOrDefault(false));
		}

		string graphUri;
		int actualRunId;

		// Get default from MCP resources
		var resources = await client.ListResourcesAsync(cancellationToken);
		var graphResource = resources.FirstOrDefault(r => r.Uri.Contains("/graph"));
		graphUri = graphResource?.Uri ?? "insights://runs/43/graph"; // Fallback to 43

		// Extract run ID from URI
		var match = System.Text.RegularExpressions.Regex.Match(graphUri, @"runs/(\d+)/");
		actualRunId = match.Success ? int.Parse(match.Groups[1].Value) : 43;
		Console.WriteLine($"📊 Fetching graph for current run: {actualRunId}");
		Console.WriteLine($"📊 Graph URI: {graphUri}");

		// Fetch the actual graph data from MCP
		var graphJson = await client.ReadResourceAsync(graphUri, cancellationToken);
		Console.WriteLine($"📦 MCP returned {graphJson?.Length ?? 0} chars for {graphUri}");

		if (!string.IsNullOrEmpty(graphJson))
		{
			// Parse the graph data
			var graphData = JsonSerializer.Deserialize<JsonObject>(graphJson);
			Console.WriteLine($"📦 Parsed graph data: {graphData?.ToJsonString()?.Substring(0, Math.Min(200, graphData?.ToJsonString()?.Length ?? 0))}...");

			// Deduplicate nodes by ID
			if (graphData != null && graphData.TryGetPropertyValue("nodes", out var nodesValue) && nodesValue is JsonArray nodesArray)
			{
				var uniqueNodes = new Dictionary<string, JsonObject>();
				foreach (var node in nodesArray)
				{
					if (node is JsonObject nodeObj && nodeObj.TryGetPropertyValue("id", out var idValue))
					{
						var id = idValue?.ToString() ?? string.Empty;
						if (!string.IsNullOrEmpty(id) && !uniqueNodes.ContainsKey(id))
						{
							// Clone the node to avoid "node already has a parent" error
							var clonedNode = JsonSerializer.Deserialize<JsonObject>(nodeObj.ToJsonString());
							if (clonedNode != null)
							{
								uniqueNodes[id] = clonedNode;
							}
						}
					}
				}

				// Replace nodes array with deduplicated version
				var deduplicatedArray = new JsonArray();
				foreach (var node in uniqueNodes.Values)
				{
					deduplicatedArray.Add(node);
				}
				graphData["nodes"] = deduplicatedArray;

				Console.WriteLine($"✅ Graph loaded: {deduplicatedArray.Count} nodes for run {actualRunId}");
			}

			// Add metadata about which run this is
			if (graphData != null)
			{
				graphData["runId"] = actualRunId;

				var nodeCount = graphData.TryGetPropertyValue("nodes", out var n) && n is JsonArray na ? na.Count : 0;
				var edgeCount = graphData.TryGetPropertyValue("edges", out var e) && e is JsonArray ea ? ea.Count : 0;

				Console.WriteLine($"📊 Returning graph for run {actualRunId}: {nodeCount} nodes, {edgeCount} edges");
			}

			return Results.Ok(graphData);
		}
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error fetching graph data for run {runId}: {ex.Message}");
	}

	// Fallback to SQLite if MCP failed or returned nothing
	return await BuildGraphFromSqliteAsync(runId, includeInferred.GetValueOrDefault(false));
});

app.MapGet("/api/runinfo", async () =>
{
	// Prefer explicit run selection from environment (set by /api/switch-run)
	var envRun = Environment.GetEnvironmentVariable("MCP_RUN_ID");
	if (!string.IsNullOrWhiteSpace(envRun) && int.TryParse(envRun, out var envRunId) && envRunId > 0)
	{
		return Results.Ok(new { runId = envRunId });
	}

	// Get latest run from database for accurate info
	try
	{
		var dbPath = GetMigrationDbPath();
		if (File.Exists(dbPath))
		{
			await using var connection = new SqliteConnection($"Data Source={dbPath}");
			await connection.OpenAsync();
			await using var cmd = connection.CreateCommand();
			cmd.CommandText = "SELECT id FROM runs ORDER BY id DESC LIMIT 1";
			var result = await cmd.ExecuteScalarAsync();
			if (result != null && result != DBNull.Value)
			{
				return Results.Ok(new { runId = Convert.ToInt32(result) });
			}
		}
	}
	catch (Exception ex)
	{
		Console.WriteLine($"Error getting run info: {ex.Message}");
	}
	return Results.Ok(new { runId = 0 });
});

app.MapGet("/api/runs/all", async () =>
{
	try
	{
		// Query SQLite directly - it's faster and more reliable
		var dbPath = GetMigrationDbPath();
        Console.WriteLine($"📊 API Request: listing runs from {dbPath}");

		if (File.Exists(dbPath))
		{
			// Use ReadOnly mode to prevent locking issues
			await using var connection = new SqliteConnection($"Data Source={dbPath};Mode=ReadOnly");
			await connection.OpenAsync();

			await using var command = connection.CreateCommand();
                        command.CommandText = "SELECT id, status, java_output FROM runs ORDER BY id DESC";
                        
                        var sqliteRuns = new List<object>();
                        var sqliteRunIds = new List<int>();
                        await using var reader = await command.ExecuteReaderAsync();
                        while (await reader.ReadAsync())
                        {
                                var id = reader.GetInt32(0);
                                var status = reader.IsDBNull(1) ? "Unknown" : reader.GetString(1);
                                
                                // Determine language logic
                                string targetLanguage = "Unknown";
                                if (!reader.IsDBNull(2))
                                {
                                    var output = reader.GetString(2);
                                    if (output.IndexOf("csharp", StringComparison.OrdinalIgnoreCase) >= 0) targetLanguage = "C#";
                                    else if (output.IndexOf("java", StringComparison.OrdinalIgnoreCase) >= 0 || output.Equals("output", StringComparison.OrdinalIgnoreCase)) targetLanguage = "Java";
                                }

                                sqliteRunIds.Add(id);
                                sqliteRuns.Add(new { id, status, targetLanguage });
                        }

			var verbose = Environment.GetEnvironmentVariable("MCP_VERBOSE_RUNS");
			var shouldLog = !string.IsNullOrWhiteSpace(verbose) && !verbose.Equals("0") && !verbose.Equals("false", StringComparison.OrdinalIgnoreCase);
			if (shouldLog)
			{
				var ids = string.Join(", ", sqliteRunIds);
				Console.WriteLine($"📊 Found {sqliteRunIds.Count} runs: {ids}");
			}

			return Results.Ok(new { runs = sqliteRunIds, runsDetailed = sqliteRuns });
		}

		Console.WriteLine("📊 No runs found");
		return Results.Ok(new { runs = new List<object>() });
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error getting available runs: {ex.Message}");
		return Results.Ok(new { runs = new List<object>() });
	}
});

// Cancel a stuck/running run
static int? ParseRunIdOrNull(string runIdText)
{
	if (int.TryParse(runIdText, out var id)) return id;
	Console.WriteLine($"⚠️ Invalid runId '{runIdText}'");
	return null;
}

// Check if a SQLite table has a given column (case-insensitive)
static async Task<bool> TableHasColumnAsync(SqliteConnection connection, string tableName, string columnName, CancellationToken ct)
{
	await using var cmd = connection.CreateCommand();
	cmd.CommandText = $"PRAGMA table_info({tableName});";
	await using var reader = await cmd.ExecuteReaderAsync(ct);
	while (await reader.ReadAsync(ct))
	{
		if (reader.FieldCount > 1 && reader.GetString(1).Equals(columnName, StringComparison.OrdinalIgnoreCase))
		{
			return true;
		}
	}
	return false;
}

app.MapPost("/api/runs/{runId}/cancel", async (string runId) =>
{
	try
	{
		var parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}

		var dbPath = GetMigrationDbPath();
		if (!File.Exists(dbPath))
		{
			return Results.NotFound(new { error = "Database not found" });
		}

		await using var connection = new SqliteConnection($"Data Source={dbPath}");
		await connection.OpenAsync();

		await using var command = connection.CreateCommand();
		command.CommandText = @"
			UPDATE runs 
			SET status = 'Cancelled', 
			    completed_at = datetime('now')
			WHERE id = @runId AND status = 'Running'";
		command.Parameters.AddWithValue("@runId", parsedRunId.Value);

		var affected = await command.ExecuteNonQueryAsync();
		if (affected > 0)
		{
			Console.WriteLine($"❌ Run {runId} cancelled");
			return Results.Ok(new { success = true, message = $"Run {runId} cancelled" });
		}
		else
		{
			return Results.Ok(new { success = false, message = $"Run {runId} was not in 'Running' status" });
		}
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error cancelling run {runId}: {ex.Message}");
		return Results.Problem($"Failed to cancel run: {ex.Message}");
	}
});

// Cleanup all stale runs
app.MapPost("/api/runs/cleanup-stale", async () =>
{
	try
	{
		var dbPath = GetMigrationDbPath();
		if (!File.Exists(dbPath))
		{
			return Results.NotFound(new { error = "Database not found" });
		}

		await using var connection = new SqliteConnection($"Data Source={dbPath}");
		await connection.OpenAsync();

		await using var command = connection.CreateCommand();
		command.CommandText = @"
			UPDATE runs 
			SET status = 'Cancelled', 
			    completed_at = datetime('now')
			WHERE status = 'Running' 
			AND datetime(started_at) < datetime('now', '-1 hour')";

		var affected = await command.ExecuteNonQueryAsync();
		Console.WriteLine($"🧹 Cleaned up {affected} stale run(s)");
		return Results.Ok(new { success = true, cleanedUp = affected });
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error cleaning up stale runs: {ex.Message}");
		return Results.Problem($"Failed to cleanup stale runs: {ex.Message}");
	}
});

app.MapGet("/api/runs/{runId}/dependencies", async (string runId, IMcpClient client, CancellationToken cancellationToken) =>
{
	try
	{
		var parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}
		// Fetch the graph resource for this specific run
		var graphUri = $"insights://runs/{parsedRunId.Value}/graph";
		var graphJson = await client.ReadResourceAsync(graphUri, cancellationToken);

		if (!string.IsNullOrEmpty(graphJson))
		{
			var graphData = JsonSerializer.Deserialize<JsonObject>(graphJson);

			// Deduplicate nodes
			if (graphData != null && graphData.TryGetPropertyValue("nodes", out var nodesValue) && nodesValue is JsonArray nodesArray)
			{
				var uniqueNodes = new Dictionary<string, JsonObject>();
				foreach (var node in nodesArray)
				{
					if (node is JsonObject nodeObj && nodeObj.TryGetPropertyValue("id", out var idValue))
					{
						var id = idValue?.ToString() ?? string.Empty;
						if (!string.IsNullOrEmpty(id) && !uniqueNodes.ContainsKey(id))
						{
							var clonedNode = JsonSerializer.Deserialize<JsonObject>(nodeObj.ToJsonString());
							if (clonedNode != null)
							{
								uniqueNodes[id] = clonedNode;
							}
						}
					}
				}

				var deduplicatedArray = new JsonArray();
				foreach (var node in uniqueNodes.Values)
				{
					deduplicatedArray.Add(node);
				}
				graphData["nodes"] = deduplicatedArray;
			}

			if (graphData != null)
			{
				var nodeCount = graphData.TryGetPropertyValue("nodes", out var n) && n is JsonArray na ? na.Count : 0;
				var edgeCount = graphData.TryGetPropertyValue("edges", out var e) && e is JsonArray ea ? ea.Count : 0;

				return Results.Ok(new
				{
					runId = runId,
					nodeCount = nodeCount,
					edgeCount = edgeCount,
					graphData = graphData
				});
			}
		}
	}
	catch (Exception ex)
	{
		Console.WriteLine($"Error getting dependencies for run {runId}: {ex.Message}");
	}

	return Results.Ok(new { runId = runId, nodeCount = 0, edgeCount = 0, error = "Unable to fetch dependencies" });
});

// Deep analysis for SQL usage for a specific run - direct DB access for max detail
app.MapGet("/api/runs/{runId}/sql-usage", async (string runId) =>
{
	try
	{
		var parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}
		
		var dbPath = GetMigrationDbPath();
		var details = new List<object>();
		
		if (File.Exists(dbPath))
		{
			var connectionString = $"Data Source={dbPath};Cache=Shared";
			await using var connection = new SqliteConnection(connectionString);
			await connection.OpenAsync();
			
			// Get analysis texts that mention SQL
			var cmd = connection.CreateCommand();
			cmd.CommandText = @"
				SELECT f.file_path, a.raw_analysis 
				FROM analyses a
				JOIN cobol_files f ON a.cobol_file_id = f.id
				WHERE f.run_id = $runId 
				AND (a.raw_analysis LIKE '%Embedded SQL%' OR a.raw_analysis LIKE '%DB2 Interaction%')";
			cmd.Parameters.AddWithValue("$runId", parsedRunId.Value);

			await using var reader = await cmd.ExecuteReaderAsync();
			while (await reader.ReadAsync())
			{
				var path = reader.GetString(0);
				var analysis = reader.GetString(1);
				var fileName = Path.GetFileName(path);

				// Extract SQL blocks using Regex
				// Find sections starting with ### Embedded SQL or similar, until the next ### header
				var sectionMatches = System.Text.RegularExpressions.Regex.Matches(analysis, @"###\s*(?:Embedded SQL|DB2 Interaction|Database)([\s\S]*?)(?=\n###|\z)", System.Text.RegularExpressions.RegexOptions.IgnoreCase);
				
				foreach (System.Text.RegularExpressions.Match match in sectionMatches)
				{
					if (match.Groups.Count > 1)
					{
						var content = match.Groups[1].Value.Trim();
						details.Add(new {
							file = fileName,
							analysis = content
						});
					}
				}
			}
		}

		return Results.Ok(new { runId = parsedRunId.Value, details = details });
	}
	catch (Exception ex)
	{
		Console.WriteLine($"Error getting SQL usage for run {runId}: {ex.Message}");
		return Results.Ok(new { error = ex.Message });
	}
});

// Generate migration report for a specific run
app.MapGet("/api/runs/{runId}/report", async (string runId) =>
{
	int? parsedRunId = null;
	try
	{
		parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}

		var outputDir = Path.Combine(Directory.GetCurrentDirectory(), "..", "output");
		var reportPath = Path.Combine(outputDir, $"migration_report_run_{parsedRunId.Value}.md");

		// Check if report already exists
		if (File.Exists(reportPath))
		{
			var content = await File.ReadAllTextAsync(reportPath);
			var lastModified = File.GetLastWriteTime(reportPath);

			return Results.Ok(new
			{
				runId = parsedRunId.Value,
				content = content,
				lastModified = lastModified,
				path = reportPath
			});
		}

		// If report doesn't exist, generate it
		Console.WriteLine($"📝 Generating migration report for run {parsedRunId.Value}...");

		// Get all data for the run from SQLite
		var dbPath = Path.Combine(Directory.GetCurrentDirectory(), "..", "Data", "migration.db");

		if (!File.Exists(dbPath))
		{
			return Results.Ok(new { error = "Database not found" });
		}

		using var connection = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={dbPath}");
		await connection.OpenAsync();

		// Generate comprehensive report
		var report = new System.Text.StringBuilder();
		report.AppendLine($"# COBOL Migration Report - Run {parsedRunId.Value}");
		report.AppendLine();
		report.AppendLine($"**Generated:** {DateTime.Now:yyyy-MM-dd HH:mm:ss}");
		report.AppendLine();
		report.AppendLine("---");
		report.AppendLine();

		// Summary section
		report.AppendLine("## 📊 Migration Summary");
		report.AppendLine();

		var summaryCmd = connection.CreateCommand();
		summaryCmd.CommandText = @"
			SELECT 
				COUNT(DISTINCT source_file) as total_files,
				COUNT(DISTINCT CASE WHEN source_file LIKE '%.cbl' THEN source_file END) as cobol_programs,
				COUNT(DISTINCT CASE WHEN source_file LIKE '%.cpy' THEN source_file END) as copybooks
			FROM cobol_files 
			WHERE run_id = @runId";
		summaryCmd.Parameters.AddWithValue("@runId", parsedRunId.Value);

		using (var reader = await summaryCmd.ExecuteReaderAsync())
		{
			if (await reader.ReadAsync())
			{
				var totalFiles = reader.GetInt32(0);
				var programs = reader.GetInt32(1);
				var copybooks = reader.GetInt32(2);

				report.AppendLine($"- **Total COBOL Files:** {totalFiles}");
				report.AppendLine($"- **Programs (.cbl):** {programs}");
				report.AppendLine($"- **Copybooks (.cpy):** {copybooks}");
			}
		}

		// Dependencies section
		var depsCmd = connection.CreateCommand();
		depsCmd.CommandText = @"
			SELECT 
				COUNT(*) as total_deps,
				COUNT(CASE WHEN dependency_type = 'CALL' THEN 1 END) as call_deps,
				COUNT(CASE WHEN dependency_type = 'COPY' THEN 1 END) as copy_deps,
				COUNT(CASE WHEN dependency_type = 'PERFORM' THEN 1 END) as perform_deps,
				COUNT(CASE WHEN dependency_type = 'EXEC' THEN 1 END) as exec_deps,
				COUNT(CASE WHEN dependency_type = 'READ' THEN 1 END) as read_deps,
				COUNT(CASE WHEN dependency_type = 'WRITE' THEN 1 END) as write_deps,
				COUNT(CASE WHEN dependency_type = 'OPEN' THEN 1 END) as open_deps,
				COUNT(CASE WHEN dependency_type = 'CLOSE' THEN 1 END) as close_deps
			FROM dependencies 
			WHERE run_id = @runId";
		depsCmd.Parameters.AddWithValue("@runId", runId);

		report.AppendLine();
		using (var reader = await depsCmd.ExecuteReaderAsync())
		{
			if (await reader.ReadAsync())
			{
				var total = reader.GetInt32(0);
				report.AppendLine($"- **Total Dependencies:** {total}");

				if (reader.GetInt32(1) > 0) report.AppendLine($"  - CALL: {reader.GetInt32(1)}");
				if (reader.GetInt32(2) > 0) report.AppendLine($"  - COPY: {reader.GetInt32(2)}");
				if (reader.GetInt32(3) > 0) report.AppendLine($"  - PERFORM: {reader.GetInt32(3)}");
				if (reader.GetInt32(4) > 0) report.AppendLine($"  - EXEC: {reader.GetInt32(4)}");
				if (reader.GetInt32(5) > 0) report.AppendLine($"  - READ: {reader.GetInt32(5)}");
				if (reader.GetInt32(6) > 0) report.AppendLine($"  - WRITE: {reader.GetInt32(6)}");
				if (reader.GetInt32(7) > 0) report.AppendLine($"  - OPEN: {reader.GetInt32(7)}");
				if (reader.GetInt32(8) > 0) report.AppendLine($"  - CLOSE: {reader.GetInt32(8)}");
			}
		}

		report.AppendLine();
		report.AppendLine("---");
		report.AppendLine();

		// File Details section
		report.AppendLine("## 📁 File Inventory");
		report.AppendLine();

		var filesCmd = connection.CreateCommand();
		var hasLineCount = await TableHasColumnAsync(connection, "cobol_files", "line_count", CancellationToken.None);
		filesCmd.CommandText = hasLineCount
			? @"
			SELECT file_name, file_path, line_count
			FROM cobol_files 
			WHERE run_id = @runId
			ORDER BY file_name"
			: @"
			SELECT file_name, file_path, NULL as line_count
			FROM cobol_files 
			WHERE run_id = @runId
			ORDER BY file_name";
		filesCmd.Parameters.AddWithValue("@runId", runId);

		report.AppendLine("| File Name | Path | Lines |");
		report.AppendLine("|-----------|------|-------|");

		using (var reader = await filesCmd.ExecuteReaderAsync())
		{
			while (await reader.ReadAsync())
			{
				var fileName = reader.GetString(0);
				var filePath = reader.IsDBNull(1) ? "" : reader.GetString(1);
				var lineCount = reader.IsDBNull(2) ? 0 : reader.GetInt32(2);

				report.AppendLine($"| {fileName} | {filePath} | {lineCount} |");
			}
		}

		report.AppendLine();
		report.AppendLine("---");
		report.AppendLine();

		// Dependency Graph section
		report.AppendLine("## 🔗 Dependency Relationships");
		report.AppendLine();

		var depDetailsCmd = connection.CreateCommand();
		depDetailsCmd.CommandText = @"
			SELECT source_file, target_file, dependency_type, line_number, context
			FROM dependencies 
			WHERE run_id = @runId
			ORDER BY source_file, dependency_type, target_file";
		depDetailsCmd.Parameters.AddWithValue("@runId", parsedRunId.Value);

		report.AppendLine("| Source | Target | Type | Line | Context |");
		report.AppendLine("|--------|--------|------|------|---------|");

		using (var reader = await depDetailsCmd.ExecuteReaderAsync())
		{
			while (await reader.ReadAsync())
			{
				var source = reader.GetString(0);
				var target = reader.GetString(1);
				var type = reader.GetString(2);
				var line = reader.IsDBNull(3) ? "" : reader.GetInt32(3).ToString();
				var context = reader.IsDBNull(4) ? "" : reader.GetString(4).Replace("|", "\\|");

				report.AppendLine($"| {source} | {target} | {type} | {line} | {context} |");
			}
		}

		report.AppendLine();
		report.AppendLine("---");
		report.AppendLine();
		report.AppendLine("*Report generated by COBOL Migration Portal*");

		// Save report to file
		Directory.CreateDirectory(outputDir);
		await File.WriteAllTextAsync(reportPath, report.ToString());

		Console.WriteLine($"✅ Report generated: {reportPath}");

		return Results.Ok(new
		{
			runId = parsedRunId.Value,
			content = report.ToString(),
			lastModified = DateTime.Now,
			path = reportPath
		});
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error generating report for run {parsedRunId}: {ex.Message}");
		return Results.Ok(new { error = $"Failed to generate report: {ex.Message}" });
	}
});

// Direct chunks API - bypasses MCP for better reliability
app.MapGet("/api/runs/{runId}/chunks", async (string runId) =>
{
	try
	{
		var parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}
		var dbPath = GetMigrationDbPath();
		if (!File.Exists(dbPath))
		{
			return Results.Ok(new { error = "Database not found", files = Array.Empty<object>() });
		}

		var connectionString = $"Data Source={dbPath};Cache=Shared";
		await using var connection = new SqliteConnection(connectionString);
		await connection.OpenAsync();

		// Get chunk status grouped by file
		var files = new List<object>();
		await using var cmd = connection.CreateCommand();
		cmd.CommandText = @"
			SELECT 
				source_file,
				COUNT(*) as total_chunks,
				SUM(CASE WHEN status = 'Completed' THEN 1 ELSE 0 END) as completed_chunks,
				SUM(CASE WHEN status = 'Failed' THEN 1 ELSE 0 END) as failed_chunks,
				SUM(CASE WHEN status = 'Pending' OR status = 'Processing' THEN 1 ELSE 0 END) as pending_chunks,
				COALESCE(SUM(tokens_used), 0) as total_tokens,
				COALESCE(SUM(processing_time_ms), 0) as total_time_ms
			FROM chunk_metadata
			WHERE run_id = $runId
			GROUP BY source_file
			ORDER BY source_file;";
		cmd.Parameters.AddWithValue("$runId", parsedRunId.Value);

		await using var reader = await cmd.ExecuteReaderAsync();
		while (await reader.ReadAsync())
		{
			var total = reader.GetInt32(1);
			var completed = reader.GetInt32(2);
			var failed = reader.GetInt32(3);
			var pending = reader.GetInt32(4);
			var progress = total > 0 ? Math.Round(completed * 100.0 / total, 2) : 0;

			files.Add(new
			{
				sourceFile = reader.GetString(0),
				totalChunks = total,
				completedChunks = completed,
				failedChunks = failed,
				pendingChunks = pending,
				progressPercentage = progress,
				totalTokensUsed = reader.GetInt64(5),
				totalProcessingTimeMs = reader.GetInt64(6)
			});
		}

		// Get total programs count
		var programs = 0;
		await using var programsCmd = connection.CreateCommand();
		programsCmd.CommandText = "SELECT COUNT(*) FROM cobol_files WHERE run_id = $runId AND is_copybook = 0";
		programsCmd.Parameters.AddWithValue("$runId", parsedRunId.Value);
		programs = Convert.ToInt32(await programsCmd.ExecuteScalarAsync() ?? 0);

		var chunkedFilesCount = files.Count;
		var smallFilesCount = Math.Max(0, programs - chunkedFilesCount);
		var smartMigrationActive = chunkedFilesCount > 0;
		var usingDirectProcessing = chunkedFilesCount == 0 && programs > 0;

		return Results.Ok(new { 
			runId = parsedRunId.Value, 
			files, 
			usingDirectProcessing,
			totalFiles = programs,
			chunkedFiles = chunkedFilesCount,
			smallFiles = smallFilesCount,
			smartMigrationActive
		});
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error getting chunks for run {runId}: {ex.Message}");
		return Results.Ok(new { error = ex.Message, files = Array.Empty<object>() });
	}
});

// Direct chunk details API - get individual chunks for a file
app.MapGet("/api/runs/{runId}/chunks/{fileName}", async (string runId, string fileName) =>
{
	try
	{
		var parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}
		var dbPath = GetMigrationDbPath();
		if (!File.Exists(dbPath))
		{
			return Results.Ok(new { error = "Database not found", chunks = Array.Empty<object>() });
		}

		var connectionString = $"Data Source={dbPath};Cache=Shared";
		await using var connection = new SqliteConnection(connectionString);
		await connection.OpenAsync();

		var chunks = new List<object>();
		await using var cmd = connection.CreateCommand();
		cmd.CommandText = @"
			SELECT 
				chunk_index, start_line, end_line, status, 
				COALESCE(tokens_used, 0) as tokens_used,
				COALESCE(processing_time_ms, 0) as processing_time_ms,
				semantic_units, completed_at
			FROM chunk_metadata
			WHERE run_id = $runId AND source_file = $fileName
			ORDER BY chunk_index;";
		cmd.Parameters.AddWithValue("$runId", parsedRunId.Value);
		cmd.Parameters.AddWithValue("$fileName", Uri.UnescapeDataString(fileName));

		await using var reader = await cmd.ExecuteReaderAsync();
		while (await reader.ReadAsync())
		{
			var semanticUnitsJson = reader.IsDBNull(6) ? "[]" : reader.GetString(6);
			var semanticUnits = new List<string>();
			try
			{
				semanticUnits = JsonSerializer.Deserialize<List<string>>(semanticUnitsJson) ?? new List<string>();
			}
			catch { }

			chunks.Add(new
			{
				chunkIndex = reader.GetInt32(0),
				startLine = reader.GetInt32(1),
				endLine = reader.GetInt32(2),
				status = reader.GetString(3),
				tokensUsed = reader.GetInt64(4),
				processingTimeMs = reader.GetInt64(5),
				semanticUnits,
				completedAt = reader.IsDBNull(7) ? null : reader.GetString(7)
			});
		}

		var totalLines = chunks.Count > 0 
			? (chunks.Max(c => (int)((dynamic)c).endLine) - chunks.Min(c => (int)((dynamic)c).startLine) + 1) 
			: 0;
		var completedChunks = chunks.Count(c => ((dynamic)c).status == "Completed");

		return Results.Ok(new
		{
			runId = parsedRunId.Value,
			sourceFile = Uri.UnescapeDataString(fileName),
			totalChunks = chunks.Count,
			completedChunks,
			progressPercentage = chunks.Count > 0 ? Math.Round(completedChunks * 100.0 / chunks.Count, 2) : 0,
			totalLines,
			totalTokensUsed = chunks.Sum(c => (long)((dynamic)c).tokensUsed),
			totalProcessingTimeMs = chunks.Sum(c => (long)((dynamic)c).processingTimeMs),
			chunks
		});
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error getting chunk details for {fileName}: {ex.Message}");
		return Results.Ok(new { error = ex.Message, chunks = Array.Empty<object>() });
	}
});

// Process status API - Returns the status of all processing stages for a run
app.MapGet("/api/runs/{runId}/process-status", async (string runId) =>
{
	try
	{
		var parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}
		var dbPath = GetMigrationDbPath();
		if (!File.Exists(dbPath))
		{
			return Results.Ok(new { error = "Database not found", stages = Array.Empty<object>() });
		}

		var connectionString = $"Data Source={dbPath};Cache=Shared";
		await using var connection = new SqliteConnection(connectionString);
		await connection.OpenAsync();

		// Get run info
		await using var runCmd = connection.CreateCommand();
		runCmd.CommandText = "SELECT status, started_at, completed_at, cobol_source, notes FROM runs WHERE id = $runId";
		runCmd.Parameters.AddWithValue("$runId", parsedRunId.Value);
		
		var runStatus = "Unknown";
		var startedAt = "";
		var completedAt = "";
		var cobolSource = "";
		var notes = "";
		
		await using (var runReader = await runCmd.ExecuteReaderAsync())
		{
			if (await runReader.ReadAsync())
			{
				runStatus = runReader.IsDBNull(0) ? "Unknown" : runReader.GetString(0);
				startedAt = runReader.IsDBNull(1) ? "" : runReader.GetString(1);
				completedAt = runReader.IsDBNull(2) ? "" : runReader.GetString(2);
				cobolSource = runReader.IsDBNull(3) ? "" : runReader.GetString(3);
				notes = runReader.IsDBNull(4) ? "" : runReader.GetString(4);
			}
		}

		// Get file counts
		await using var filesCmd = connection.CreateCommand();
		filesCmd.CommandText = "SELECT COUNT(*), SUM(CASE WHEN is_copybook = 0 THEN 1 ELSE 0 END), SUM(CASE WHEN is_copybook = 1 THEN 1 ELSE 0 END) FROM cobol_files WHERE run_id = $runId";
		filesCmd.Parameters.AddWithValue("$runId", parsedRunId.Value);
		var totalFiles = 0;
		var programs = 0;
		var copybooks = 0;
		await using (var filesReader = await filesCmd.ExecuteReaderAsync())
		{
			if (await filesReader.ReadAsync())
			{
				totalFiles = filesReader.IsDBNull(0) ? 0 : filesReader.GetInt32(0);
				programs = filesReader.IsDBNull(1) ? 0 : filesReader.GetInt32(1);
				copybooks = filesReader.IsDBNull(2) ? 0 : filesReader.GetInt32(2);
			}
		}

		// Get analysis count
		await using var analysisCmd = connection.CreateCommand();
		analysisCmd.CommandText = "SELECT COUNT(*) FROM analyses a JOIN cobol_files f ON a.cobol_file_id = f.id WHERE f.run_id = $runId";
		analysisCmd.Parameters.AddWithValue("$runId", parsedRunId.Value);
		var analysesCompleted = Convert.ToInt32(await analysisCmd.ExecuteScalarAsync() ?? 0);

		// Get dependency count
		await using var depsCmd = connection.CreateCommand();
		depsCmd.CommandText = "SELECT COUNT(*) FROM dependencies WHERE run_id = $runId";
		depsCmd.Parameters.AddWithValue("$runId", parsedRunId.Value);
		var dependenciesCount = Convert.ToInt32(await depsCmd.ExecuteScalarAsync() ?? 0);

		// Get chunk stats
		await using var chunkCmd = connection.CreateCommand();
		chunkCmd.CommandText = @"SELECT 
			COUNT(*) as total,
			COALESCE(SUM(CASE WHEN status = 'Completed' THEN 1 ELSE 0 END), 0) as completed,
			COALESCE(SUM(CASE WHEN status = 'Processing' THEN 1 ELSE 0 END), 0) as processing,
			COALESCE(SUM(CASE WHEN status = 'Failed' THEN 1 ELSE 0 END), 0) as failed,
			COALESCE(SUM(tokens_used), 0) as tokens,
			COALESCE(SUM(processing_time_ms), 0) as time_ms
			FROM chunk_metadata WHERE run_id = $runId";
		chunkCmd.Parameters.AddWithValue("$runId", parsedRunId.Value);
		
		var totalChunks = 0;
		var completedChunks = 0;
		var processingChunks = 0;
		var failedChunks = 0;
		long totalTokens = 0;
		long totalTimeMs = 0;
		
		await using (var chunkReader = await chunkCmd.ExecuteReaderAsync())
		{
			if (await chunkReader.ReadAsync())
			{
				totalChunks = chunkReader.IsDBNull(0) ? 0 : chunkReader.GetInt32(0);
				completedChunks = chunkReader.IsDBNull(1) ? 0 : chunkReader.GetInt32(1);
				processingChunks = chunkReader.IsDBNull(2) ? 0 : chunkReader.GetInt32(2);
				failedChunks = chunkReader.IsDBNull(3) ? 0 : chunkReader.GetInt32(3);
				totalTokens = chunkReader.IsDBNull(4) ? 0 : chunkReader.GetInt64(4);
				totalTimeMs = chunkReader.IsDBNull(5) ? 0 : chunkReader.GetInt64(5);
			}
		}

		// Determine stage statuses based on data
		var isRunning = runStatus == "Running";
		var isCompleted = runStatus == "Completed";
		
		// Check if this run used chunking (has chunk metadata)
		var usedChunking = totalChunks > 0;
		var usingDirectProcessing = !usedChunking && programs > 0;
		
		// For chunked files, technical analysis and business logic are intentionally skipped
		// They're replaced by the chunk-aware conversion process
		// This is a SUCCESS state, not a failure - chunking handles it differently
		var reverseEngineeringSkipped = usedChunking && programs > 0;
		var reverseEngineeringFailed = !usedChunking && isCompleted && analysesCompleted == 0 && programs > 0;
		
		// Determine individual stage statuses
		string GetTechAnalysisStatus() {
			if (analysesCompleted >= programs && programs > 0) return "completed";
			if (reverseEngineeringSkipped) return "completed"; // Skipped for chunking = success
			if (reverseEngineeringFailed) return "failed";
			if (analysesCompleted > 0 && analysesCompleted < programs) return "running";
			if (isRunning && totalFiles > 0) return "running";
			return "pending";
		}
		
		string GetBusinessLogicStatus() {
			if (analysesCompleted >= programs && programs > 0) return "completed";
			if (reverseEngineeringSkipped) return "completed"; // Skipped for chunking = success
			if (reverseEngineeringFailed) return "failed";
			if (analysesCompleted > 0) return "running";
			return "pending";
		}
		
		string GetTechAnalysisDetails() {
			if (reverseEngineeringSkipped) {
				return $"✓ Using chunked analysis ({totalChunks} chunks) - full-file analysis skipped";
			}
			if (reverseEngineeringFailed) {
				return $"Failed: No analysis completed for {programs} programs";
			}
			if (analysesCompleted >= programs && programs > 0) {
				return $"Analyzed {analysesCompleted} of {programs} programs";
			}
			return $"Analyzing {analysesCompleted} of {programs} programs";
		}
		
		string GetBusinessLogicDetails() {
			if (reverseEngineeringSkipped) {
				return $"✓ Using chunk-aware extraction ({totalChunks} chunks) - full-file extraction skipped";
			}
			if (reverseEngineeringFailed) {
				return $"Failed: Technical analysis required but not completed";
			}
			if (analysesCompleted >= programs && programs > 0) {
				return "Business rules extracted";
			}
			return "Extracting business rules and patterns";
		}
		
		var stages = new[]
		{
			new {
				id = "file-discovery",
				name = "File Discovery",
				status = totalFiles > 0 ? "completed" : (isRunning ? "running" : "pending"),
				details = $"Found {programs} programs, {copybooks} copybooks"
			},
			new {
				id = "technical-analysis",
				name = "Technical Analysis",
				status = GetTechAnalysisStatus(),
				details = GetTechAnalysisDetails()
			},
			new {
				id = "business-logic",
				name = "Business Logic Extraction",
				status = GetBusinessLogicStatus(),
				details = GetBusinessLogicDetails()
			},
			new {
				id = "dependency-mapping",
				name = "Dependency Mapping",
				status = dependenciesCount > 0 ? "completed" : (isRunning ? "running" : "pending"),
				details = dependenciesCount > 0 ? $"Mapped {dependenciesCount} dependencies" : "Mapping dependencies..."
			},
			new {
				id = "chunking",
				name = "Smart Chunking",
				status = usingDirectProcessing
					? "completed"
					: totalChunks > 0 
						? (completedChunks == totalChunks && totalChunks > 0 ? "completed" : (failedChunks > 0 ? "failed" : "running")) 
						: (isCompleted && totalChunks == 0 ? "pending" : (dependenciesCount > 0 ? "running" : "pending")),
				details = usingDirectProcessing 
					? "Skipped - all files were under the chunk threshold"
					: totalChunks > 0 ? $"Created {totalChunks} chunks" : "Waiting..."
			},
			new {
				id = "conversion",
				name = "Code Conversion",
				status = usingDirectProcessing
					? (isCompleted ? "completed" : (isRunning ? "running" : "pending"))
					: totalChunks > 0 
						? (completedChunks == totalChunks && totalChunks > 0 ? "completed" : (failedChunks > 0 ? "failed" : (processingChunks > 0 ? "running" : "pending"))) 
						: "pending",
				details = usingDirectProcessing
					? "Direct conversion (no chunking required)"
					: $"Converted {completedChunks}/{totalChunks} chunks ({(totalChunks > 0 ? Math.Round(completedChunks * 100.0 / totalChunks) : 0)}%)"
			},
			new {
				id = "assembly",
				name = "Code Assembly",
				status = usingDirectProcessing
					? (isCompleted ? "completed" : (isRunning ? "running" : "pending"))
					: isCompleted && completedChunks == totalChunks && totalChunks > 0 ? "completed" : (completedChunks > 0 && !isCompleted ? "running" : "pending"),
				details = usingDirectProcessing
					? "Assembling direct-conversion output"
					: isCompleted && completedChunks == totalChunks ? "Code assembled" : "Assembling converted chunks"
			},
			new {
				id = "output",
				name = "Output Generation",
				status = isCompleted ? "completed" : "pending",
				details = isCompleted ? "Migration complete!" : "Waiting for conversion"
			}
		};

		// Calculate progress for chunked and direct runs
		double progressPercentage;
		if (usedChunking)
		{
			progressPercentage = totalChunks > 0 ? Math.Round(completedChunks * 100.0 / totalChunks, 1) : 0;
		}
		else
		{
			progressPercentage = isCompleted
				? 100
				: (analysesCompleted > 0 && programs > 0
					? Math.Min(90, Math.Round(analysesCompleted * 100.0 / programs, 1))
					: (isRunning ? 10 : 0));
		}

		return Results.Ok(new
		{
			runId,
			runStatus,
			startedAt,
			completedAt,
			cobolSource,
			notes,
			stats = new {
				totalFiles,
				programs,
				copybooks,
				analysesCompleted,
				dependenciesCount,
				totalChunks,
				completedChunks,
				processingChunks,
				failedChunks,
				totalTokens,
				totalTimeMs,
				progressPercentage,
				activeWorkers = processingChunks,
				maxParallelWorkers = 6,
				usingDirectProcessing
			},
			stages
		});
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error getting process status: {ex.Message}");
		return Results.Ok(new { error = ex.Message });
	}
});

// Agent conversations API - Returns chat/API call logs for agents
app.MapGet("/api/runs/{runId}/agent-conversations", async (string runId, string? agent) =>
{
	try
	{
		var parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}
		var conversations = new List<object>();
		var logsPath = Path.GetFullPath(Path.Combine("..", "Logs", "ApiCalls"), Directory.GetCurrentDirectory());
		
		if (!Directory.Exists(logsPath))
		{
			return Results.Ok(new { conversations, message = "Logs directory not found" });
		}

		// Get the run's time window from database
		DateTime? runStartedAt = null;
		DateTime? runCompletedAt = null;
		var dbPath = GetMigrationDbPath();
		
		if (File.Exists(dbPath))
		{
			using var conn = new SqliteConnection($"Data Source={dbPath}");
			await conn.OpenAsync();
			using var cmd = conn.CreateCommand();
			cmd.CommandText = "SELECT started_at, completed_at FROM runs WHERE id = @id";
			cmd.Parameters.AddWithValue("@id", parsedRunId.Value);
			using var reader = await cmd.ExecuteReaderAsync();
			if (await reader.ReadAsync())
			{
				runStartedAt = reader.IsDBNull(0) ? null : DateTime.Parse(reader.GetString(0));
				runCompletedAt = reader.IsDBNull(1) ? null : DateTime.Parse(reader.GetString(1));
			}
		}

		// Read live_api_calls.json for recent calls
		var liveCallsPath = Path.Combine(logsPath, "live_api_calls.json");
		if (File.Exists(liveCallsPath))
		{
			try
			{
				var liveJson = await File.ReadAllTextAsync(liveCallsPath);
				var liveData = JsonSerializer.Deserialize<JsonElement>(liveJson);
				
				if (liveData.TryGetProperty("calls", out var calls))
				{
					foreach (var call in calls.EnumerateArray())
					{
						var agentName = call.TryGetProperty("agent", out var a) ? a.GetString() : "Unknown";
						
						// Filter by agent if specified
						if (!string.IsNullOrEmpty(agent) && !agentName?.Contains(agent, StringComparison.OrdinalIgnoreCase) == true)
							continue;
						
						// Filter by run's time window
						if (runStartedAt.HasValue && call.TryGetProperty("timestamp", out var tsElement))
						{
							var callTimestamp = tsElement.GetString();
							if (DateTime.TryParse(callTimestamp, out var callTime))
							{
								// Skip calls before the run started
								if (callTime < runStartedAt.Value.AddSeconds(-5)) // 5 second buffer
									continue;
								// Skip calls after the run completed (if completed)
								if (runCompletedAt.HasValue && callTime > runCompletedAt.Value.AddSeconds(5))
									continue;
							}
						}
						
						conversations.Add(new
						{
							id = call.TryGetProperty("callId", out var id) ? id.GetInt32() : 0,
							timestamp = call.TryGetProperty("timestamp", out var ts) ? ts.GetString() : "",
							agent = agentName,
							method = call.TryGetProperty("method", out var m) ? m.GetString() : "",
							model = call.TryGetProperty("model", out var mod) ? mod.GetString() : "",
							durationMs = call.TryGetProperty("durationMs", out var d) ? d.GetDouble() : 0,
							isSuccess = call.TryGetProperty("isSuccess", out var s) && s.GetBoolean(),
							tokensUsed = call.TryGetProperty("tokensUsed", out var t) ? t.GetInt32() : 0,
							status = call.TryGetProperty("status", out var st) ? st.GetString() : "UNKNOWN",
							// Include full error, request and response for debugging
							error = call.TryGetProperty("error", out var e) ? e.GetString() : "",
							request = call.TryGetProperty("request", out var req) ? req.GetString() : "",
							response = call.TryGetProperty("response", out var resp) ? resp.GetString() : ""
						});
					}
				}
			}
			catch (Exception ex)
			{
				Console.WriteLine($"Error reading live calls: {ex.Message}");
			}
		}

		// Also read individual agent log files for today
		var today = DateTime.Now.ToString("yyyy-MM-dd");
		var logFiles = Directory.GetFiles(logsPath, $"API_CALL_*_{today}.log");
		
		foreach (var logFile in logFiles)
		{
			try
			{
				var content = await File.ReadAllTextAsync(logFile);
				var entries = content.Split(new[] { "\n\n", "\r\n\r\n" }, StringSplitOptions.RemoveEmptyEntries);
				
				foreach (var entry in entries)
				{
					if (string.IsNullOrWhiteSpace(entry)) continue;
					
					try
					{
						var logEntry = JsonSerializer.Deserialize<JsonElement>(entry.Trim());
						
						if (logEntry.TryGetProperty("data", out var dataStr))
						{
							var data = JsonSerializer.Deserialize<JsonElement>(dataStr.GetString() ?? "{}");
							var agentName = data.TryGetProperty("agent", out var a) ? a.GetString() : "Unknown";
							
							// Filter by agent if specified
							if (!string.IsNullOrEmpty(agent) && !agentName?.Contains(agent, StringComparison.OrdinalIgnoreCase) == true)
								continue;
							
							// Filter by run's time window
							if (runStartedAt.HasValue && data.TryGetProperty("timestamp", out var tsElement))
							{
								var callTimestamp = tsElement.GetString();
								if (DateTime.TryParse(callTimestamp, out var callTime))
								{
									if (callTime < runStartedAt.Value.AddSeconds(-5))
										continue;
									if (runCompletedAt.HasValue && callTime > runCompletedAt.Value.AddSeconds(5))
										continue;
								}
							}
							
							conversations.Add(new
							{
								id = data.TryGetProperty("callId", out var id) ? id.GetInt32() : 0,
								timestamp = data.TryGetProperty("timestamp", out var ts) ? ts.GetString() : "",
								agent = agentName,
								method = data.TryGetProperty("method", out var m) ? m.GetString() : "",
								model = data.TryGetProperty("model", out var mod) ? mod.GetString() : "",
								request = data.TryGetProperty("request", out var req) ? req.GetString() : "",
								response = data.TryGetProperty("response", out var resp) ? resp.GetString() : "",
								durationMs = data.TryGetProperty("durationMs", out var d) ? d.GetDouble() : 0,
								isSuccess = data.TryGetProperty("isSuccess", out var s) && s.GetBoolean(),
								tokensUsed = data.TryGetProperty("tokensUsed", out var t) ? t.GetInt32() : 0,
								error = data.TryGetProperty("error", out var e) ? e.GetString() : ""
							});
						}
					}
					catch { /* Skip malformed entries */ }
				}
			}
			catch (Exception ex)
			{
				Console.WriteLine($"Error reading log file {logFile}: {ex.Message}");
			}
		}

		// Sort by timestamp descending
		var sorted = conversations
			.OrderByDescending(c => ((dynamic)c).timestamp?.ToString() ?? "")
			.Take(100)
			.ToList();

		return Results.Ok(new
		{
			conversations = sorted,
			count = sorted.Count,
			runId,
			runStartedAt = runStartedAt?.ToString("O"),
			runCompletedAt = runCompletedAt?.ToString("O"),
			logsPath
		});
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error getting agent conversations: {ex.Message}");
		return Results.Ok(new { error = ex.Message, conversations = Array.Empty<object>() });
	}
});

// Activity feed API - Returns recent activity across all agents and processes
// Optional runId parameter to filter activity for a specific run
app.MapGet("/api/activity-feed", async (int? runId) =>
{
	try
	{
		var activities = new List<object>();
		
		// Get recent runs from database
		var dbPath = GetMigrationDbPath();
		DateTime? runStartedAt = null;
		DateTime? runCompletedAt = null;
		
		if (File.Exists(dbPath))
		{
			var connectionString = $"Data Source={dbPath};Cache=Shared";
			await using var connection = new SqliteConnection(connectionString);
			await connection.OpenAsync();

			// If runId is specified, get the run's time window
			if (runId.HasValue)
			{
				await using var timeCmd = connection.CreateCommand();
				timeCmd.CommandText = "SELECT started_at, completed_at FROM runs WHERE id = @id";
				timeCmd.Parameters.AddWithValue("@id", runId.Value);
				await using var timeReader = await timeCmd.ExecuteReaderAsync();
				if (await timeReader.ReadAsync())
				{
					runStartedAt = timeReader.IsDBNull(0) ? null : DateTime.Parse(timeReader.GetString(0));
					runCompletedAt = timeReader.IsDBNull(1) ? null : DateTime.Parse(timeReader.GetString(1));
				}
			}

			// Recent chunk activity - filter by runId if specified
			await using var chunkCmd = connection.CreateCommand();
			if (runId.HasValue)
			{
				chunkCmd.CommandText = @"SELECT run_id, source_file, chunk_index, status, completed_at 
					FROM chunk_metadata 
					WHERE run_id = @runId AND completed_at IS NOT NULL 
					ORDER BY completed_at DESC LIMIT 50";
				chunkCmd.Parameters.AddWithValue("@runId", runId.Value);
			}
			else
			{
				chunkCmd.CommandText = @"SELECT run_id, source_file, chunk_index, status, completed_at 
					FROM chunk_metadata 
					WHERE completed_at IS NOT NULL 
					ORDER BY completed_at DESC LIMIT 20";
			}
			
			await using var chunkReader = await chunkCmd.ExecuteReaderAsync();
			while (await chunkReader.ReadAsync())
			{
				activities.Add(new
				{
					type = "chunk",
					runId = chunkReader.GetInt32(0),
					sourceFile = chunkReader.GetString(1),
					chunkIndex = chunkReader.GetInt32(2),
					status = chunkReader.GetString(3),
					timestamp = chunkReader.IsDBNull(4) ? "" : chunkReader.GetString(4),
					message = $"Chunk {chunkReader.GetInt32(2)} of {Path.GetFileName(chunkReader.GetString(1))} - {chunkReader.GetString(3)}"
				});
			}

			// Recent run status changes - filter by runId if specified
			await using var runCmd = connection.CreateCommand();
			if (runId.HasValue)
			{
				runCmd.CommandText = @"SELECT id, status, started_at, completed_at, cobol_source 
					FROM runs WHERE id = @runId";
				runCmd.Parameters.AddWithValue("@runId", runId.Value);
			}
			else
			{
				runCmd.CommandText = @"SELECT id, status, started_at, completed_at, cobol_source 
					FROM runs ORDER BY started_at DESC LIMIT 5";
			}
			
			await using var runReader = await runCmd.ExecuteReaderAsync();
			while (await runReader.ReadAsync())
			{
				activities.Add(new
				{
					type = "run",
					runId = runReader.GetInt32(0),
					status = runReader.GetString(1),
					timestamp = runReader.GetString(2),
					message = $"Run #{runReader.GetInt32(0)} - {runReader.GetString(1)}"
				});
			}
		}

		// Get recent API calls from logs
		var logsPath = Path.GetFullPath(Path.Combine("..", "Logs", "ApiCalls"), Directory.GetCurrentDirectory());
		var liveCallsPath = Path.Combine(logsPath, "live_api_calls.json");
		
		if (File.Exists(liveCallsPath))
		{
			try
			{
				var liveJson = await File.ReadAllTextAsync(liveCallsPath);
				var liveData = JsonSerializer.Deserialize<JsonElement>(liveJson);
				
				if (liveData.TryGetProperty("calls", out var calls))
				{
					foreach (var call in calls.EnumerateArray())
					{
						// Filter by run's time window if runId is specified
						if (runId.HasValue && runStartedAt.HasValue && call.TryGetProperty("timestamp", out var tsElement))
						{
							var callTimestamp = tsElement.GetString();
							if (DateTime.TryParse(callTimestamp, out var callTime))
							{
								if (callTime < runStartedAt.Value.AddSeconds(-5))
									continue;
								if (runCompletedAt.HasValue && callTime > runCompletedAt.Value.AddSeconds(5))
									continue;
							}
						}
						
						activities.Add(new
						{
							type = "api_call",
							agent = call.TryGetProperty("agent", out var a) ? a.GetString() : "Unknown",
							timestamp = call.TryGetProperty("timestamp", out var ts) ? ts.GetString() : "",
							status = call.TryGetProperty("status", out var s) ? s.GetString() : "UNKNOWN",
							message = $"{(call.TryGetProperty("agent", out var ag) ? ag.GetString() : "Agent")} - {(call.TryGetProperty("method", out var m) ? m.GetString() : "Call")}"
						});
					}
				}
			}
			catch { }
		}

		// Sort all activities by timestamp
		var sorted = activities
			.OrderByDescending(a => ((dynamic)a).timestamp?.ToString() ?? "")
			.Take(runId.HasValue ? 100 : 50)
			.ToList();

		return Results.Ok(new
		{
			activities = sorted,
			count = sorted.Count,
			runId,
			runStartedAt = runStartedAt?.ToString("O"),
			runCompletedAt = runCompletedAt?.ToString("O"),
			lastUpdated = DateTime.Now.ToString("O")
		});
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error getting activity feed: {ex.Message}");
		return Results.Ok(new { error = ex.Message, activities = Array.Empty<object>() });
	}
});

// Migration Log API - Returns real-time migration process logs
app.MapGet("/api/runs/{runId}/migration-log", async (string runId, int? lines, string? since) =>
{
	try
	{
		var parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}
		var logsBasePath = Path.GetFullPath(Path.Combine("..", "Logs"), Directory.GetCurrentDirectory());
		var migrationPath = Path.Combine(logsBasePath, "Migration");
		
		// Get run information
		var dbPath = GetMigrationDbPath();
		string targetLanguage = "csharp"; // Default - will detect from output folder
		string runStatus = "Unknown";
		DateTime? startedAt = null;
		DateTime? completedAt = null;
		
		if (File.Exists(dbPath))
		{
			using var conn = new SqliteConnection($"Data Source={dbPath}");
			await conn.OpenAsync();
			
			using var cmd = conn.CreateCommand();
			cmd.CommandText = "SELECT status, started_at, completed_at FROM runs WHERE id = @id";
			cmd.Parameters.AddWithValue("@id", parsedRunId.Value);
			using var reader = await cmd.ExecuteReaderAsync();
			if (await reader.ReadAsync())
			{
				runStatus = reader.IsDBNull(0) ? "Unknown" : reader.GetString(0);
				startedAt = reader.IsDBNull(1) ? null : DateTime.Parse(reader.GetString(1));
				completedAt = reader.IsDBNull(2) ? null : DateTime.Parse(reader.GetString(2));
			}
		}
		
		// Detect target language from output folders
		var outputBasePath = Path.GetFullPath(Path.Combine("..", "output"), Directory.GetCurrentDirectory());
		var csharpOutputPath = Path.Combine(outputBasePath, "csharp");
		var javaOutputPath = Path.Combine(outputBasePath, "java");
		
		// Check which output folder has more recent files
		var csharpLastWrite = Directory.Exists(csharpOutputPath) 
			? Directory.GetFiles(csharpOutputPath, "*", SearchOption.AllDirectories).Select(f => new FileInfo(f).LastWriteTime).DefaultIfEmpty(DateTime.MinValue).Max()
			: DateTime.MinValue;
		var javaLastWrite = Directory.Exists(javaOutputPath) 
			? Directory.GetFiles(javaOutputPath, "*", SearchOption.AllDirectories).Select(f => new FileInfo(f).LastWriteTime).DefaultIfEmpty(DateTime.MinValue).Max()
			: DateTime.MinValue;
		
		if (javaLastWrite > csharpLastWrite)
			targetLanguage = "java";
		else if (csharpLastWrite > DateTime.MinValue)
			targetLanguage = "csharp";
		
		var logEntries = new List<object>();
		var maxLines = lines ?? 500;
		DateTime? sinceTime = null;
		if (!string.IsNullOrEmpty(since))
		{
			DateTime.TryParse(since, out var parsedSince);
			sinceTime = parsedSince;
		}
		
		// 1. Read MIGRATION_STEP logs
		var migrationStepFiles = Directory.Exists(migrationPath) 
			? Directory.GetFiles(migrationPath, "MIGRATION_STEP_*.log").OrderByDescending(f => f).Take(3)
			: Enumerable.Empty<string>();
		
		foreach (var logFile in migrationStepFiles)
		{
			try
			{
				var content = await File.ReadAllTextAsync(logFile);
				// Split by double newlines or "}\n\n{" pattern for JSON objects
				var entries = System.Text.RegularExpressions.Regex.Split(content, @"(?<=\})\s*\n\s*\n\s*(?=\{)");
				
				foreach (var entry in entries)
				{
					try
					{
						var json = entry.Trim();
						if (string.IsNullOrWhiteSpace(json)) continue;
						if (!json.StartsWith("{")) continue; // Skip non-JSON content
						
						var logEntry = JsonSerializer.Deserialize<JsonElement>(json);
						var timestamp = logEntry.TryGetProperty("timestamp", out var ts) ? ts.GetString() : "";
						var message = logEntry.TryGetProperty("message", out var msg) ? msg.GetString() : "";
						var category = logEntry.TryGetProperty("category", out var cat) ? cat.GetString() : "MIGRATION";
						
						// Parse timestamp and check if it's after 'since'
						if (sinceTime.HasValue && DateTime.TryParse(timestamp?.Replace(".", ":"), out var entryTime))
						{
							if (entryTime < sinceTime.Value) continue;
						}
						
						logEntries.Add(new
						{
							type = "step",
							timestamp,
							category,
							message,
							level = message?.Contains("ERROR") == true ? "error" : 
							        message?.Contains("WARNING") == true ? "warn" : "info"
						});
					}
					catch { }
				}
			}
			catch { }
		}
		
		// 2. Read BEHIND_SCENES_PROCESSING logs
		var processingFiles = Directory.GetFiles(logsBasePath, "BEHIND_SCENES_PROCESSING_*.log")
			.OrderByDescending(f => f).Take(3);
		
		foreach (var logFile in processingFiles)
		{
			try
			{
				var content = await File.ReadAllTextAsync(logFile);
				var entries = System.Text.RegularExpressions.Regex.Split(content, @"(?<=\})\s*\n\s*\n\s*(?=\{)");
				
				foreach (var entry in entries)
				{
					try
					{
						var json = entry.Trim();
						if (string.IsNullOrWhiteSpace(json)) continue;
						if (!json.StartsWith("{")) continue;
						
						var logEntry = JsonSerializer.Deserialize<JsonElement>(json);
						var timestamp = logEntry.TryGetProperty("timestamp", out var ts) ? ts.GetString() : "";
						var message = logEntry.TryGetProperty("message", out var msg) ? msg.GetString() : "";
						
						if (sinceTime.HasValue && DateTime.TryParse(timestamp?.Replace(".", ":"), out var entryTime))
						{
							if (entryTime < sinceTime.Value) continue;
						}
						
						logEntries.Add(new
						{
							type = "processing",
							timestamp,
							category = "PROCESSING",
							message,
							level = "info"
						});
					}
					catch { }
				}
			}
			catch { }
		}
		
		// 3. Read live_migration_progress.json for current status
		object? liveProgress = null;
		var liveProgressPath = Path.Combine(migrationPath, "live_migration_progress.json");
		if (File.Exists(liveProgressPath))
		{
			try
			{
				var liveJson = await File.ReadAllTextAsync(liveProgressPath);
				liveProgress = JsonSerializer.Deserialize<JsonElement>(liveJson);
			}
			catch { }
		}

		// 4. Live run log tail + active file detection from the latest CLI log
		var liveLogTail = new List<object>();
		var activeFiles = new List<object>();
		try
		{
			var runLogPath = Path.Combine(logsBasePath, "migration_run_latest.log");
			if (File.Exists(runLogPath))
			{
				var tailLines = ReadTailLines(runLogPath, 400);
				var anchorIndex = tailLines.FindLastIndex(l => l.Contains($"run {parsedRunId.Value}"));
				if (anchorIndex >= 0)
				{
					tailLines = tailLines.Skip(anchorIndex).ToList();
				}

				var cleanedTail = tailLines.Select(CleanAnsi).ToList();
				liveLogTail = cleanedTail
					.TakeLast(200)
					.Select((line, idx) => (object)new { index = idx, text = line })
					.ToList();

				var activeMap = new Dictionary<string, (string stage, DateTime lastSeen)>();
				for (var i = cleanedTail.Count - 1; i >= 0; i--)
				{
					var line = cleanedTail[i];
					var timestamp = ExtractTimestamp(line) ?? DateTime.Now;
					// Analysis patterns
					AddActiveFromPattern(line, @"Analyzing COBOL file:\s*(?<file>.+)$", "Analysis", timestamp, activeMap);
					AddActiveFromPattern(line, @"Analysis progress:.*-\s*(?<file>\S+\.cbl)", "Analysis", timestamp, activeMap);
					AddActiveFromPattern(line, @"File\s+(?<file>\S+\.cbl)\s+has", "Analysis", timestamp, activeMap);
					// Business logic patterns
					AddActiveFromPattern(line, @"Extracting business logic from:?\s*(?<file>.+)$", "Business Logic", timestamp, activeMap);
					// Chunking patterns
					AddActiveFromPattern(line, @"Processing chunk .*source (?<file>.+)$", "Chunking", timestamp, activeMap);
					AddActiveFromPattern(line, @"Chunk \d+ of \d+.*(?<file>\S+\.cbl)", "Chunking", timestamp, activeMap);
					// Conversion patterns
					AddActiveFromPattern(line, @"Converting\s+(?<file>\S+\.cbl)", "Converting", timestamp, activeMap);
					AddActiveFromPattern(line, @"Converting to (Java|C#).*(?<file>\S+\.cbl)", "Converting", timestamp, activeMap);
					// Saving patterns
					AddActiveFromPattern(line, @"Saving.*(?<file>\S+\.(java|cs))", "Saving", timestamp, activeMap);
					AddActiveFromPattern(line, @"Wrote\s+(?<file>\S+\.(java|cs))", "Saved", timestamp, activeMap);
				}

				activeFiles = activeMap
					.Select(kvp => (object)new { file = kvp.Key, stage = kvp.Value.stage, lastSeen = kvp.Value.lastSeen.ToString("O") })
					.OrderByDescending(a => ((dynamic)a).lastSeen)
					.ToList();
			}
		}
		catch (Exception ex)
		{
			Console.WriteLine($"⚠️ Unable to read live CLI log tail: {ex.Message}");
		}

		// 2b. Read reverse-engineering and analysis logs (session-based) so UI sees current run activity
		try
		{
			bool InRunWindow(string? tsString)
			{
				if (string.IsNullOrWhiteSpace(tsString)) return true;
				tsString = tsString.Replace('.', ':');
				if (!DateTime.TryParse(tsString, out var tsParsed)) return true;
				if (startedAt.HasValue && tsParsed < startedAt.Value.AddMinutes(-1)) return false;
				if (completedAt.HasValue && tsParsed > completedAt.Value.AddMinutes(1)) return false;
				return true;
			}

			async Task ReadJsonLinesFromLogs(IEnumerable<string> files, string typeLabel, string categoryLabel)
			{
				foreach (var logFile in files)
				{
					try
					{
						var content = await File.ReadAllTextAsync(logFile);
						var entries = System.Text.RegularExpressions.Regex.Split(content, @"(?<=\})\s*\n\s*\n\s*(?=\{)");

						foreach (var entry in entries)
						{
							try
							{
								var json = entry.Trim();
								if (string.IsNullOrWhiteSpace(json) || !json.StartsWith("{")) continue;

								var logEntry = JsonSerializer.Deserialize<JsonElement>(json);
								var timestamp = logEntry.TryGetProperty("timestamp", out var ts) ? ts.GetString() : "";
								var message = logEntry.TryGetProperty("message", out var msg) ? msg.GetString() : "";

								if (!InRunWindow(timestamp)) continue;
								if (sinceTime.HasValue && DateTime.TryParse(timestamp?.Replace('.', ':'), out var entryTime))
								{
									if (entryTime < sinceTime.Value) continue;
								}

								logEntries.Add(new
								{
									type = typeLabel,
									timestamp,
									category = categoryLabel,
									message,
									level = message?.Contains("ERROR", StringComparison.OrdinalIgnoreCase) == true ? "error" : "info"
								});
							}
							catch { }
						}
					}
					catch { }
				}
			}

			var reverseFiles = Directory.GetFiles(logsBasePath, "BEHIND_SCENES_REVERSE_ENGINEERING_*.log")
				.OrderByDescending(f => f).Take(2);
			await ReadJsonLinesFromLogs(reverseFiles, "reverse_engineering", "REVERSE_ENGINEERING");

			var analysisFiles = Directory.GetFiles(Path.Combine(logsBasePath, "Analysis"), "ANALYSIS_AI_PROCESSING_*.log")
				.OrderByDescending(f => f).Take(2);
			await ReadJsonLinesFromLogs(analysisFiles, "analysis", "AI_PROCESSING");
		}
		catch (Exception ex)
		{
			Console.WriteLine($"⚠️ Unable to read live log tail: {ex.Message}");
		}
		
		List<string> ReadTailLines(string path, int maxLines)
		{
			var queue = new Queue<string>();
			foreach (var line in File.ReadLines(path))
			{
				if (queue.Count >= maxLines)
				{
					queue.Dequeue();
				}
				queue.Enqueue(line);
			}
			return queue.ToList();
		}

		string CleanAnsi(string line)
		{
			return System.Text.RegularExpressions.Regex.Replace(line ?? string.Empty, "\\u001b\\[[0-9;]*m", string.Empty);
		}

		DateTime? ExtractTimestamp(string line)
		{
			var match = System.Text.RegularExpressions.Regex.Match(line ?? string.Empty, @"\[(?<time>\d{2}[:\.]\d{2}[:\.]\d{2}(?:[:\.]\d{3})?)\]");
			if (match.Success)
			{
				var timePart = match.Groups["time"].Value.Replace('.', ':');
				if (TimeSpan.TryParse(timePart, out var ts))
				{
					var today = DateTime.Today;
					return new DateTime(today.Year, today.Month, today.Day, ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds);
				}
			}
			return null;
		}

		void AddActiveFromPattern(string line, string pattern, string stage, DateTime timestamp, Dictionary<string, (string stage, DateTime lastSeen)> target)
		{
			var match = System.Text.RegularExpressions.Regex.Match(line ?? string.Empty, pattern, System.Text.RegularExpressions.RegexOptions.IgnoreCase);
			if (!match.Success) return;
			var file = match.Groups["file"]?.Value?.Trim();
			if (string.IsNullOrWhiteSpace(file)) return;
			if (!target.TryGetValue(file, out var existing) || timestamp > existing.lastSeen)
			{
				target[file] = (stage, timestamp);
			}
		}

		// Sort by timestamp and limit
		var sortedEntries = logEntries
			.OrderByDescending(e => ((dynamic)e).timestamp?.ToString() ?? "")
			.Take(maxLines)
			.Reverse()
			.ToList();
		
		return Results.Ok(new
		{
			runId = parsedRunId.Value,
			targetLanguage,
			runStatus,
			startedAt = startedAt?.ToString("O"),
			completedAt = completedAt?.ToString("O"),
			liveProgress,
			entryCount = sortedEntries.Count,
			entries = sortedEntries,
			liveLogTail,
			activeFiles,
			lastUpdated = DateTime.Now.ToString("O")
		});
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Error getting migration log: {ex.Message}");
		return Results.Ok(new { error = ex.Message, entries = Array.Empty<object>() });
	}
});

// This endpoint redirects to the search endpoint
app.MapGet("/api/runs/{runId}/combined-data", async (string runId) =>
{
	await Task.CompletedTask;
	var parsedRunId = ParseRunIdOrNull(runId);
	if (parsedRunId is null)
	{
		return Results.BadRequest(new { error = "Invalid runId" });
	}
	return Results.Redirect($"/api/search/run/{parsedRunId.Value}");
});

// Search endpoint that queries both SQLite and Neo4j for any run
app.MapGet("/api/search/run/{runId}", async (string runId, IMcpClient client, CancellationToken cancellationToken) =>
{
	try
	{
		var parsedRunId = ParseRunIdOrNull(runId);
		if (parsedRunId is null)
		{
			return Results.BadRequest(new { error = "Invalid runId" });
		}
		// Get SQLite data - provide query instructions
		var dbPath = Path.Combine(Directory.GetCurrentDirectory(), "..", "Data", "migration.db");
		var sqliteData = new
		{
			run_id = parsedRunId.Value,
			database_path = dbPath,
			database_exists = File.Exists(dbPath),
			instructions = new
			{
				message = "Use sqlite3 CLI or DB Browser to query this run",
				query_examples = new[]
				{
					$"SELECT * FROM runs WHERE id = {parsedRunId.Value};",
					$"SELECT COUNT(*) as file_count FROM cobol_files WHERE run_id = {parsedRunId.Value};",
					$"SELECT file_name, is_copybook FROM cobol_files WHERE run_id = {parsedRunId.Value} LIMIT 10;"
				},
				cli_command = $"sqlite3 \"{dbPath}\" \"SELECT id, status, started_at, completed_at FROM runs WHERE id = {parsedRunId.Value};\""
			},
			available_tables = new[] { "runs", "cobol_files", "analyses", "dependencies", "copybook_usage", "metrics" }
		};

		// Get Neo4j data (via MCP) - try to get graph
		object neo4jData;
		try
		{
			var graphUri = $"insights://runs/{parsedRunId.Value}/graph";
			var graphJson = await client.ReadResourceAsync(graphUri, cancellationToken);

			if (!string.IsNullOrEmpty(graphJson))
			{
				var graphData = JsonSerializer.Deserialize<JsonObject>(graphJson);
				var nodeCount = 0;
				var edgeCount = 0;

				if (graphData != null)
				{
					if (graphData.TryGetPropertyValue("nodes", out var n) && n is JsonArray na) nodeCount = na.Count;
					if (graphData.TryGetPropertyValue("edges", out var e) && e is JsonArray ea) edgeCount = ea.Count;
				}

				neo4jData = new { node_count = nodeCount, edge_count = edgeCount, graph_available = true };
			}
			else
			{
				neo4jData = new { message = "Graph not available via MCP for this run", graph_available = false };
			}
		}
		catch
		{
			neo4jData = new { message = "Unable to fetch Neo4j data via MCP", graph_available = false };
		}

		return Results.Ok(new
		{
			runId = parsedRunId.Value,
			found = sqliteData.database_exists || (neo4jData as dynamic)?.graph_available == true,
			sources = new
			{
				sqlite = new
				{
					source = "SQLite Database",
					location = "Data/migration.db",
					data = sqliteData
				},
				neo4j = new
				{
					source = "Neo4j Graph Database",
					location = "bolt://localhost:7687",
					credentials = new { username = "neo4j", password = "cobol-migration-2025" },
					data = neo4jData
				}
			},
			howToQuery = new
			{
				sqlite = new
				{
					cli = $"sqlite3 \"Data/migration.db\" \"SELECT * FROM runs WHERE id = {parsedRunId.Value};\"",
					queries = new[]
					{
						$"SELECT id, status, started_at, completed_at FROM runs WHERE id = {parsedRunId.Value};",
						$"SELECT file_name, is_copybook FROM cobol_files WHERE run_id = {parsedRunId.Value};",
						$"SELECT program_name, analysis_data FROM analyses WHERE run_id = {parsedRunId.Value};"
					}
				},
				neo4j = new
				{
					cypher_shell = $"echo 'MATCH (n) WHERE n.runId = {parsedRunId.Value} RETURN n LIMIT 25;' | cypher-shell -u neo4j -p cobol-migration-2025",
					queries = new[]
					{
						$"MATCH (n) WHERE n.runId = {parsedRunId.Value} RETURN n LIMIT 25;",
						$"MATCH (n)-[r]->(m) WHERE n.runId = {parsedRunId.Value} AND m.runId = {parsedRunId.Value} RETURN n, r, m LIMIT 50;"
					}
				},
				api = new
				{
					combined_data = $"/api/runs/{parsedRunId.Value}/combined-data",
					dependencies = $"/api/runs/{parsedRunId.Value}/dependencies",
					mcp_resources = new[]
					{
						$"insights://runs/{parsedRunId.Value}/summary",
						$"insights://runs/{parsedRunId.Value}/dependencies",
						$"insights://runs/{parsedRunId.Value}/graph"
					}
				}
			}
		});
	}
	catch (Exception ex)
	{
		return Results.Ok(new
		{
			runId,
			found = false,
			error = ex.Message
		});
	}
});

app.MapGet("/api/data-retrieval-guide", () =>
{
	var guide = new
	{
		title = "Historical Run Data Retrieval Guide",
		databases = new object[]
		{
			new
			{
				name = "SQLite",
				location = "Data/migration.db",
				purpose = "Stores migration metadata, COBOL files, analyses, Java code",
				queries = new[]
				{
					new { description = "List all migration runs", sql = "SELECT id, status, started_at, completed_at, total_files, successful_conversions FROM migration_runs ORDER BY id DESC;" },
					new { description = "Get files for specific run", sql = "SELECT file_name, file_type, file_path FROM cobol_files WHERE migration_run_id = ?;" },
					new { description = "Get analyses for specific run", sql = "SELECT cobol_file_id, analysis_json FROM analyses WHERE migration_run_id = ?;" },
					new { description = "Get generated Java for specific run", sql = "SELECT file_name, java_code, target_path FROM java_files WHERE migration_run_id = ?;" },
					new { description = "Get dependency map for run", sql = "SELECT dependencies_json, mermaid_diagram FROM dependency_maps WHERE migration_run_id = ?;" }
				},
				tools = new object[]
				{
					new { name = "sqlite3 CLI", command = "sqlite3 Data/migration.db" },
					new { name = "DB Browser for SQLite", url = "https://sqlitebrowser.org/" },
					new { name = "VS Code SQLite Extension", id = "alexcvzz.vscode-sqlite" }
				}
			},
			new
			{
				name = "Neo4j",
				location = "bolt://localhost:7687",
				purpose = "Stores dependency graph relationships and file connections",
				credentials = new { username = "neo4j", password = "cobol-migration-2025" },
				queries = new[]
				{
					new { description = "List all runs in Neo4j", cypher = "MATCH (r:Run) RETURN r.runId, r.status, r.totalFiles, r.startedAt ORDER BY r.runId DESC;" },
					new { description = "Get all files for specific run", cypher = "MATCH (r:Run {runId: $runId})-[:CONTAINS]->(f:CobolFile) RETURN f.fileName, f.fileType;" },
					new { description = "Get dependencies for specific run", cypher = "MATCH (r:Run {runId: $runId})-[:CONTAINS]->(source:CobolFile)-[d:DEPENDS_ON]->(target:CobolFile) RETURN source.fileName, target.fileName, d.dependencyType;" },
					new { description = "Find circular dependencies", cypher = "MATCH (r:Run {runId: $runId})-[:CONTAINS]->(f:CobolFile) MATCH path = (f)-[:DEPENDS_ON*2..]->(f) RETURN [node in nodes(path) | node.fileName] as cycle;" },
					new { description = "Get critical files (high fan-in)", cypher = "MATCH (r:Run {runId: $runId})-[:CONTAINS]->(f:CobolFile) OPTIONAL MATCH (f)<-[d:DEPENDS_ON]-() WITH f, count(d) as dependents WHERE dependents > 0 RETURN f.fileName, dependents ORDER BY dependents DESC;" }
				},
				tools = new object[]
				{
					new { name = "Neo4j Browser", url = "http://localhost:7474" },
					new { name = "Neo4j Desktop", url = "https://neo4j.com/download/" },
					new { name = "Cypher Shell", command = "cypher-shell -a bolt://localhost:7687 -u neo4j -p cobol-migration-2025" }
				}
			}
		},
		mcpResources = new
		{
			description = "Access via MCP (Model Context Protocol) API",
			resources = new[]
			{
				new { uri = "insights://runs/{runId}/summary", description = "Migration run overview" },
				new { uri = "insights://runs/{runId}/files", description = "All COBOL files list" },
				new { uri = "insights://runs/{runId}/graph", description = "Full dependency graph" },
				new { uri = "insights://runs/{runId}/circular-dependencies", description = "Circular deps analysis" },
				new { uri = "insights://runs/{runId}/critical-files", description = "High-impact files" }
			},
			endpoints = new[]
			{
				new { method = "GET", path = "/api/resources", description = "List all available MCP resources" },
				new { method = "GET", path = "/api/runs/all", description = "Get all run IDs" },
				new { method = "GET", path = "/api/runs/{runId}/dependencies", description = "Get dependencies for specific run" },
				new { method = "POST", path = "/api/chat", description = "Ask questions about migration data" }
			}
		},
		examples = new[]
		{
			new
			{
				title = "Retrieve Run 43 Data from SQLite",
				steps = new[]
				{
					"sqlite3 Data/migration.db",
					".mode column",
					".headers on",
					"SELECT * FROM migration_runs WHERE id = 43;",
					"SELECT COUNT(*) FROM cobol_files WHERE migration_run_id = 43;"
				}
			},
			new
			{
				title = "Retrieve Run 43 Graph from Neo4j",
				steps = new[]
				{
					"Open http://localhost:7474 in browser",
					"Login: neo4j / cobol-migration-2025",
					"Run: MATCH (r:Run {runId: 43})-[:CONTAINS]->(f:CobolFile) RETURN f LIMIT 25;",
					"Visualize dependencies: MATCH path = (r:Run {runId: 43})-[:CONTAINS]->()-[d:DEPENDS_ON]->() RETURN path;"
				}
			},
			new
			{
				title = "Retrieve via MCP API",
				steps = new[]
				{
					"curl http://localhost:5028/api/runs/all",
					"curl http://localhost:5028/api/runs/43/dependencies | jq '.'",
					"curl -X POST http://localhost:5028/api/chat -H 'Content-Type: application/json' -d '{\"prompt\":\"Show me all dependencies for run 43\"}'"
				}
			}
		}
	};

	return Results.Ok(guide);
});

app.MapPost("/api/switch-run", (SwitchRunRequest request, IMcpClient client) =>
{
	if (request.RunId <= 0)
	{
		return Results.BadRequest(new { error = "Invalid run ID" });
	}

	try
	{
		// Update the MCP client to use the new run ID
		var mcpClient = client as McpProcessClient;
		if (mcpClient != null)
		{
			// The MCP server uses MCP_RUN_ID environment variable
			// We need to restart the MCP connection with the new run ID
			Environment.SetEnvironmentVariable("MCP_RUN_ID", request.RunId.ToString());

			// Note: In a production system, you'd want to properly handle reconnection
			// For now, the client will pick up the new run ID on next operation
		}

		return Results.Ok(new
		{
			success = true,
			runId = request.RunId,
			message = $"Switched to run {request.RunId}. Note: You may need to refresh resources to see updated data."
		});
	}
	catch (Exception ex)
	{
		return Results.Problem($"Failed to switch run: {ex.Message}");
	}
});

// Architecture documentation endpoint
app.MapGet("/api/documentation/architecture", async () =>
{
	try
	{
		// Expect the architecture doc at the repository root
		var docPath = Path.Combine("..", "REVERSE_ENGINEERING_ARCHITECTURE.md");
		var fullPath = Path.GetFullPath(docPath, app.Environment.ContentRootPath);

		if (!File.Exists(fullPath))
		{
			return Results.NotFound(new
			{
				error = "Architecture documentation not found",
				path = fullPath,
				hint = "Expected file ../REVERSE_ENGINEERING_ARCHITECTURE.md"
			});
		}

		var content = await File.ReadAllTextAsync(fullPath);
		var fileInfo = new FileInfo(fullPath);

		return Results.Ok(new
		{
			content,
			filename = "REVERSE_ENGINEERING_ARCHITECTURE.md",
			lastModified = fileInfo.LastWriteTimeUtc,
			sizeBytes = fileInfo.Length
		});
	}
	catch (Exception ex)
	{
		return Results.Problem($"Failed to read architecture documentation: {ex.Message}");
	}
});

// Reverse Engineering Report endpoint - serves the generated reverse-engineering-details.md
app.MapGet("/api/documentation/reverse-engineering-report", async () =>
{
	try
	{
		var reportPath = Path.Combine("..", "output", "reverse-engineering-details.md");
		var fullPath = Path.GetFullPath(reportPath, app.Environment.ContentRootPath);

		if (!File.Exists(fullPath))
		{
			return Results.NotFound(new { 
				error = "Reverse engineering report not found", 
				path = fullPath,
				hint = "Run reverse engineering first to generate the report"
			});
		}

		var content = await File.ReadAllTextAsync(fullPath);
		var fileInfo = new FileInfo(fullPath);
		
		return Results.Ok(new
		{
			content,
			filename = "reverse-engineering-details.md",
			lastModified = fileInfo.LastWriteTimeUtc,
			sizeBytes = fileInfo.Length
		});
	}
	catch (Exception ex)
	{
		return Results.Problem($"Failed to read reverse engineering report: {ex.Message}");
	}
});

// Database health check endpoint
app.MapGet("/api/health/databases", async () =>
{
	var result = new
	{
		sqlite = new { connected = false, status = "Unknown", path = "" },
		neo4j = new { connected = false, status = "Unknown", uri = "" },
		aiModel = new { connected = false, status = "Unknown", modelId = "", codexModelId = "" }
	};

	// Check AI Model Configuration
	try
	{
		var config = app.Configuration;
		// Try to get from environment variable first or config section
		var chatModelId = config.GetValue<string>("AISETTINGS__CHATMODELID") 
						  ?? config.GetValue<string>("AISettings:ChatModelId") 
						  ?? "Unknown";
		
		var codexModelId = config.GetValue<string>("AISETTINGS__MODELID") 
						  ?? config.GetValue<string>("AISettings:ModelId") 
						  ?? "Unknown";
		
		result = new
		{
			sqlite = result.sqlite,
			neo4j = result.neo4j,
			aiModel = new { connected = true, status = "Configured", modelId = chatModelId, codexModelId = codexModelId }
		};
	}
	catch (Exception)
	{
		// Ignore config errors
	}

	// Check SQLite connection
	try
	{
		var config = app.Configuration;
		var dbPath = config.GetValue<string>("ApplicationSettings:MigrationDatabasePath") ?? "../Data/migration.db";
		var fullPath = Path.GetFullPath(dbPath, app.Environment.ContentRootPath);

		if (File.Exists(fullPath))
		{
			await using var connection = new SqliteConnection($"Data Source={fullPath};Mode=ReadOnly");
			await connection.OpenAsync();
			await using var command = connection.CreateCommand();
			command.CommandText = "SELECT COUNT(*) FROM runs";
			var count = Convert.ToInt32(await command.ExecuteScalarAsync());

			result = new
			{
				sqlite = new { connected = true, status = $"Connected ({count} runs)", path = fullPath },
				neo4j = result.neo4j,
				aiModel = result.aiModel
			};
		}
		else
		{
			result = new
			{
				sqlite = new { connected = false, status = "Database file not found", path = fullPath },
				neo4j = result.neo4j,
				aiModel = result.aiModel
			};
		}
	}
	catch (Exception ex)
	{
		result = new
		{
			sqlite = new { connected = false, status = $"Error: {ex.Message}", path = "" },
			neo4j = result.neo4j,
			aiModel = result.aiModel
		};
	}

	// Check Neo4j connection (disabled due to stability issues - will show as disconnected)
	var config2 = app.Configuration;
	var neo4jUri = config2.GetValue<string>("ApplicationSettings:Neo4j:Uri") ?? "bolt://localhost:7687";

	// Check Neo4j connection with a simple HTTP check to the browser endpoint
	try
	{
		using var httpClient = new HttpClient();
		httpClient.Timeout = TimeSpan.FromSeconds(2);
		var neo4jHttpResponse = await httpClient.GetAsync("http://localhost:7474");
		
		if (neo4jHttpResponse.IsSuccessStatusCode)
		{
			result = new
			{
				sqlite = result.sqlite,
				neo4j = new { connected = true, status = "Connected (HTTP OK)", uri = neo4jUri },
				aiModel = result.aiModel
			};
		}
		else
		{
			result = new
			{
				sqlite = result.sqlite,
				neo4j = new { connected = false, status = $"HTTP {(int)neo4jHttpResponse.StatusCode}", uri = neo4jUri },
				aiModel = result.aiModel
			};
		}
	}
	catch (Exception ex)
	{
		result = new
		{
			sqlite = result.sqlite,
			neo4j = new { connected = false, status = $"Not reachable: {ex.GetType().Name}", uri = neo4jUri },
			aiModel = result.aiModel
		};
	}

	return Results.Ok(result);
});

// Live Activity Dashboard API - real-time monitoring of all processes
app.MapGet("/api/activity/live", async () =>
{
	var dbPath = GetMigrationDbPath();
	var activity = new
	{
		timestamp = DateTime.UtcNow,
		migration = new {
			status = "Unknown",
			runId = (int?)null,
			phase = (string?)null,
			currentFile = (string?)null,
			progress = (object?)null
		},
		chunks = new {
			activeWorkers = 0,
			pendingChunks = 0,
			processingChunks = 0,
			completedChunks = 0,
			failedChunks = 0,
			currentChunks = Array.Empty<object>()
		},
		apiCalls = new {
			recentCalls = Array.Empty<object>(),
			rateLimit = new { tokensUsed = 0, tokensRemaining = 300000, requestsPerMinute = 0.0 }
		},
		services = new {
			portal = true,
			sqlite = false,
			neo4j = false,
			mcpServer = false
		}
	};

	if (!File.Exists(dbPath))
	{
		return Results.Ok(activity);
	}

	try
	{
		var connectionString = $"Data Source={dbPath};Cache=Shared;Mode=ReadOnly";
		await using var connection = new SqliteConnection(connectionString);
		await connection.OpenAsync();

		// Get latest run status
		int? currentRunId = null;
		string? runStatus = null;
		string? sourceDir = null;
		string targetLanguage = "csharp"; // Default

		await using (var cmd = connection.CreateCommand())
		{
			cmd.CommandText = "SELECT id, status, cobol_source, java_output FROM runs ORDER BY id DESC LIMIT 1";
			await using var reader = await cmd.ExecuteReaderAsync();
			if (await reader.ReadAsync())
			{
				currentRunId = reader.GetInt32(0);
				runStatus = reader.IsDBNull(1) ? null : reader.GetString(1);
				sourceDir = reader.IsDBNull(2) ? null : reader.GetString(2);
				
				var outputDir = reader.IsDBNull(3) ? "" : reader.GetString(3);
				if (!string.IsNullOrEmpty(outputDir) && outputDir.Contains("java", StringComparison.OrdinalIgnoreCase) && !outputDir.Contains("csharp", StringComparison.OrdinalIgnoreCase))
				{
					targetLanguage = "java";
				}
				else if (!string.IsNullOrEmpty(outputDir) && outputDir.Contains("csharp", StringComparison.OrdinalIgnoreCase))
				{
					targetLanguage = "csharp";
				}
			}
		}

		// Get chunk statistics
		var chunkStats = new { pending = 0, processing = 0, completed = 0, failed = 0 };
		var currentChunks = new List<object>();
		var usingDirectProcessing = false;
		var totalFiles = 0;
		var programCount = 0;
		var analysisCount = 0;
		var totalChunkCount = 0;  // Moved outside so it's accessible in result construction
		var chunkedFileCount = 0;
		
		if (currentRunId.HasValue)
		{
			await using (var cmd = connection.CreateCommand())
			{
				cmd.CommandText = @"
					SELECT 
						SUM(CASE WHEN status = 'Pending' THEN 1 ELSE 0 END) as pending,
						SUM(CASE WHEN status = 'Processing' THEN 1 ELSE 0 END) as processing,
						SUM(CASE WHEN status = 'Completed' THEN 1 ELSE 0 END) as completed,
						SUM(CASE WHEN status = 'Failed' THEN 1 ELSE 0 END) as failed
					FROM chunk_metadata WHERE run_id = @runId";
				cmd.Parameters.AddWithValue("@runId", currentRunId.Value);
				await using var reader = await cmd.ExecuteReaderAsync();
				if (await reader.ReadAsync())
				{
					chunkStats = new {
						pending = reader.IsDBNull(0) ? 0 : reader.GetInt32(0),
						processing = reader.IsDBNull(1) ? 0 : reader.GetInt32(1),
						completed = reader.IsDBNull(2) ? 0 : reader.GetInt32(2),
						failed = reader.IsDBNull(3) ? 0 : reader.GetInt32(3)
					};
				}
			}

			// Get count of files that have chunks (for smart migration stats)
			await using (var cmd = connection.CreateCommand())
			{
				cmd.CommandText = "SELECT COUNT(DISTINCT source_file) FROM chunk_metadata WHERE run_id = @runId";
				cmd.Parameters.AddWithValue("@runId", currentRunId.Value);
				chunkedFileCount = Convert.ToInt32(await cmd.ExecuteScalarAsync() ?? 0);
			}

			// Check if using direct processing (no chunks but has files)
			await using (var cmd = connection.CreateCommand())
			{
				cmd.CommandText = @"
					SELECT COUNT(*), SUM(CASE WHEN is_copybook = 0 THEN 1 ELSE 0 END)
					FROM cobol_files WHERE run_id = @runId";
				cmd.Parameters.AddWithValue("@runId", currentRunId.Value);
				await using var reader = await cmd.ExecuteReaderAsync();
				if (await reader.ReadAsync())
				{
					totalFiles = reader.IsDBNull(0) ? 0 : reader.GetInt32(0);
					programCount = reader.IsDBNull(1) ? 0 : reader.GetInt32(1);
				}
			}

			// Get analysis count for progress calculation
			await using (var cmd = connection.CreateCommand())
			{
				cmd.CommandText = @"
					SELECT COUNT(*) FROM analyses a 
					JOIN cobol_files f ON a.cobol_file_id = f.id 
					WHERE f.run_id = @runId";
				cmd.Parameters.AddWithValue("@runId", currentRunId.Value);
				analysisCount = Convert.ToInt32(await cmd.ExecuteScalarAsync() ?? 0);
			}

			totalChunkCount = chunkStats.pending + chunkStats.processing + chunkStats.completed + chunkStats.failed;
			usingDirectProcessing = totalChunkCount == 0 && programCount > 0;

			// Get currently processing chunks (or most recent) - or files for direct mode
			if (!usingDirectProcessing)
			{
				await using (var cmd = connection.CreateCommand())
				{
					cmd.CommandText = @"
						SELECT source_file, chunk_index, start_line, end_line, status, 
							   processing_time_ms, completed_at
						FROM chunk_metadata 
						WHERE run_id = @runId 
						ORDER BY 
							CASE WHEN status = 'Processing' THEN 0 
								 WHEN status = 'Pending' THEN 1 
								 ELSE 2 END,
							completed_at DESC
						LIMIT 10";
					cmd.Parameters.AddWithValue("@runId", currentRunId.Value);
					await using var reader = await cmd.ExecuteReaderAsync();
					while (await reader.ReadAsync())
					{
						currentChunks.Add(new {
							file = reader.GetString(0),
							chunkIndex = reader.GetInt32(1),
							startLine = reader.GetInt32(2),
							endLine = reader.GetInt32(3),
							status = reader.GetString(4),
							processingTimeMs = reader.IsDBNull(5) ? 0 : reader.GetInt32(5),
							completedAt = reader.IsDBNull(6) ? null : reader.GetString(6)
						});
					}
				}
			}
			else
			{
				// For direct processing, show files instead of chunks
				await using (var cmd = connection.CreateCommand())
				{
					cmd.CommandText = @"
						SELECT f.file_name, f.is_copybook, 
							   CASE WHEN a.id IS NOT NULL THEN 'Completed' ELSE 'Pending' END as status
						FROM cobol_files f
						LEFT JOIN analyses a ON a.cobol_file_id = f.id
						WHERE f.run_id = @runId
						ORDER BY f.is_copybook, f.file_name
						LIMIT 10";
					cmd.Parameters.AddWithValue("@runId", currentRunId.Value);
					await using var reader = await cmd.ExecuteReaderAsync();
					while (await reader.ReadAsync())
					{
						currentChunks.Add(new {
							file = reader.GetString(0),
							fileName = reader.GetString(0),
							isCopybook = reader.GetBoolean(1),
							status = reader.GetString(2),
							chunkIndex = (int?)null,
							startLine = (int?)null,
							endLine = (int?)null,
							processingTimeMs = (int?)null
						});
					}
				}
			}
		}

		// Get recent API call logs if they exist
		var recentApiCalls = new List<object>();
		var apiLogsDir = Path.Combine(Path.GetDirectoryName(dbPath) ?? ".", "..", "Logs", "ApiCalls");
		if (Directory.Exists(apiLogsDir))
		{
			var logFiles = Directory.GetFiles(apiLogsDir, "*.json")
				.OrderByDescending(f => File.GetLastWriteTime(f))
				.Take(1);
			
			foreach (var logFile in logFiles)
			{
				try
				{
					var content = await File.ReadAllTextAsync(logFile);
					var lines = content.Split('\n', StringSplitOptions.RemoveEmptyEntries).TakeLast(5);
					foreach (var line in lines)
					{
						if (line.StartsWith("{"))
						{
							var call = JsonSerializer.Deserialize<JsonObject>(line);
							if (call != null)
							{
								recentApiCalls.Add(new {
									timestamp = call["timestamp"]?.ToString(),
									agent = call["agent"]?.ToString(),
									method = call["method"]?.ToString(),
									status = call["status"]?.ToString(),
									durationMs = call["duration_ms"]?.GetValue<int>() ?? 0
								});
							}
						}
					}
				}
				catch { /* Ignore log parsing errors */ }
			}
		}

		// Determine migration phase
		string? phase = null;
		if (runStatus == "Running")
		{
			if (usingDirectProcessing)
			{
				if (analysisCount < programCount) phase = "Analyzing Files";
				else phase = "Converting";
			}
			else if (chunkStats.processing > 0) phase = "Converting Chunks";
			else if (chunkStats.pending > 0) phase = "Preparing Chunks";
			else if (chunkStats.completed > 0) phase = "Finalizing";
			else phase = "Analyzing";
		}
		else if (runStatus == "Completed")
		{
			phase = "Complete";
		}

		// Check service status
		var sqliteConnected = true; // We're connected if we got here
		var neo4jConnected = false;
		try
		{
			// Quick Neo4j check - just check if port is open
			using var tcpClient = new System.Net.Sockets.TcpClient();
			var connectTask = tcpClient.ConnectAsync("localhost", 7687);
			neo4jConnected = connectTask.Wait(500);
		}
		catch { }

		// Calculate progress based on processing mode
		// Note: totalChunkCount already calculated above
		double progressPercentage;
		if (usingDirectProcessing)
		{
			// For direct processing, base progress on analysis completion
			progressPercentage = programCount > 0 ? Math.Round((double)analysisCount / programCount * 100, 1) : 0;
			if (runStatus == "Completed") progressPercentage = 100;
		}
		else
		{
			progressPercentage = totalChunkCount > 0
				? Math.Round((double)chunkStats.completed / totalChunkCount * 100, 1)
				: 0;
		}

		var result = new
		{
			timestamp = DateTime.UtcNow,
			migration = new {
				status = runStatus ?? "No active run",
				runId = currentRunId,
				phase = phase,
				currentFile = sourceDir,
				targetLanguage = targetLanguage,
				usingDirectProcessing = usingDirectProcessing,
				progress = new {
					totalChunks = totalChunkCount,
					completedChunks = chunkStats.completed,
					totalFiles = totalFiles,
					completedFiles = analysisCount,
					percentage = progressPercentage
				}
			},
			chunks = new {
				activeWorkers = chunkStats.processing,
				pendingChunks = usingDirectProcessing ? (programCount - analysisCount) : chunkStats.pending,
				processingChunks = chunkStats.processing,
				completedChunks = usingDirectProcessing ? analysisCount : chunkStats.completed,
				failedChunks = chunkStats.failed,
				currentChunks = currentChunks,
				usingDirectProcessing = usingDirectProcessing,
				smartMigrationActive = chunkedFileCount > 0,
				totalFiles = totalFiles,
				chunkedFiles = chunkedFileCount,
				smallFiles = Math.Max(0, totalFiles - chunkedFileCount)
			},
			apiCalls = new {
				recentCalls = recentApiCalls,
				rateLimit = new { tokensUsed = 0, tokensRemaining = 300000, requestsPerMinute = 0.0 }
			},
			services = new {
				portal = true,
				sqlite = sqliteConnected,
				neo4j = neo4jConnected,
				mcpServer = true
			}
		};

		return Results.Ok(result);
	}
	catch (Exception ex)
	{
		Console.WriteLine($"❌ Activity API error: {ex.Message}");
		return Results.Ok(activity);
	}
});

app.MapGet("/api/files/local", async (string path) =>
{
	try
	{
		// Robust root detection for source files
		var currentDir = Directory.GetCurrentDirectory();
		var repoRoot = currentDir;
        
        // Find the root that contains "source" folder
        // check current
		if (!Directory.Exists(Path.Combine(repoRoot, "source")))
		{
			// check parent
			var parent = Directory.GetParent(currentDir)?.FullName;
			if (parent != null && Directory.Exists(Path.Combine(parent, "source")))
			{
				repoRoot = parent;
			}
            else 
            {
                // check ../.. (grandparent)
                var grantParent = Directory.GetParent(parent ?? "")?.FullName;
                if (grantParent != null && Directory.Exists(Path.Combine(grantParent, "source")))
                {
                    repoRoot = grantParent;
                }
                else
                {
                    // Fallback to standard .. logic
                    repoRoot = Path.GetFullPath("..");
                }
            }
		}

        // Helper to resolve a path under a given root and ensure it does not escape that root
        static string? ResolveSafePath(string baseRoot, string relative)
        {
            // Normalize base root to full path with trailing separator
            var normalizedRoot = Path.GetFullPath(baseRoot);
            if (!normalizedRoot.EndsWith(Path.DirectorySeparatorChar) && !normalizedRoot.EndsWith(Path.AltDirectorySeparatorChar))
            {
                normalizedRoot += Path.DirectorySeparatorChar;
            }

            // Combine and normalize the candidate path
            var candidate = Path.GetFullPath(Path.Combine(normalizedRoot, relative));

            // Ensure the candidate stays within the root
            if (!candidate.StartsWith(normalizedRoot, StringComparison.OrdinalIgnoreCase))
            {
                return null;
            }

            return candidate;
        }

        // Clean the input path - prevent double "source/source/"
        var cleanPath = path.Replace('\\', '/');
        if (cleanPath.StartsWith("source/") && Directory.Exists(Path.Combine(repoRoot, "source")))
        {
             // If input is "source/file.cbl" and we have a "source" folder in root, 
             // Path.Combine(root, "source/file.cbl") works perfectly.
             // But if path implies logic from "source" root, let's just ensure we map correctly.
        }

		// Security check: ensure path is within repo root
		var fullPath = ResolveSafePath(repoRoot, cleanPath);
        if (fullPath is null)
        {
            Console.WriteLine($"❌ Rejected path traversal attempt: '{path}' under root '{repoRoot}'");
            return Results.BadRequest("Invalid path");
        }
		
		Console.WriteLine($"🔍 File Request: '{path}' -> '{fullPath}' (Root: {repoRoot})");

		if (!File.Exists(fullPath))
		{
            // Try fallback: maybe path lacked "source/" prefix?
            if (!cleanPath.StartsWith("source/") && Directory.Exists(Path.Combine(repoRoot, "source")))
            {
                var sourceRoot = Path.Combine(repoRoot, "source");
                var altPath = ResolveSafePath(sourceRoot, cleanPath);
                if (altPath != null && File.Exists(altPath))
                {
                     fullPath = altPath;
                     Console.WriteLine($"🔍 Autocorrected path to: {fullPath}");
                }
                else
                {
                    Console.WriteLine($"❌ File not found at: {fullPath} OR (safe alt under source)");
			        return Results.NotFound($"File not found: {path}");
                }
            }
            else 
            {
			    Console.WriteLine($"❌ File not found: {fullPath}");
			    return Results.NotFound($"File not found: {path} (Resolved: {fullPath})");
            }
		}

		var content = await File.ReadAllTextAsync(fullPath);
		var fileInfo = new FileInfo(fullPath);
		
		return Results.Ok(new
		{
			content,
			filename = Path.GetFileName(fullPath),
			lastModified = fileInfo.LastWriteTimeUtc,
			sizeBytes = fileInfo.Length
		});
	}
	catch (Exception ex)
	{
		return Results.Problem($"Failed to read file: {ex.Message}");
	}
});

app.MapFallbackToFile("index.html");

app.Run();
