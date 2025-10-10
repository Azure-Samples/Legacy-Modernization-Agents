using System.IO;
using System.Text.Json;
using System.Text.Json.Nodes;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Server.Kestrel.Core;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using McpChatWeb.Configuration;
using McpChatWeb.Models;
using McpChatWeb.Services;

var builder = WebApplication.CreateBuilder(args);

// Configure Kestrel with SO_REUSEADDR to allow port reuse
builder.WebHost.ConfigureKestrel(serverOptions =>
{
	// Allow address reuse to prevent "address already in use" errors
	serverOptions.ConfigureEndpointDefaults(listenOptions =>
	{
		listenOptions.UseConnectionLogging();
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

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();

var app = builder.Build();

if (app.Environment.IsDevelopment())
{
	app.UseSwagger();
	app.UseSwaggerUI();
}

app.UseDefaultFiles();
app.UseStaticFiles();

app.MapGet("/api/resources", async (IMcpClient client, CancellationToken cancellationToken) =>
{
	var resources = await client.ListResourcesAsync(cancellationToken);
	return Results.Ok(resources);
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
								responseText += $"**Purpose:**\n{purpose.ToString()}\n\n";
							}
						}
						else if (detailedAnalysis.TryGetPropertyValue("program", out var prog) && prog is JsonObject progObj)
						{
							if (progObj.TryGetPropertyValue("purpose", out var progPurpose))
							{
								responseText += $"**Purpose:**\n{progPurpose.ToString()}\n\n";
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
					var copybooksJson = parts.Length > 5 && !string.IsNullOrWhiteSpace(parts[5]) ? parts[5].Trim() : null;					var responseText = $"**Analysis of {fileName} (Run {targetRunId})**\n\n";
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
   - To query: cypher-shell -u neo4j -p YOUR_NEO4J_PASSWORD
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
2. Or use curl: `curl http://localhost:5250/api/search/run/{requestedRunId} | jq .`

Note: The MCP server currently provides detailed analysis only for Run 43. For other runs, use the direct database queries above or the /api/search/run endpoint.";

			return Results.Ok(new ChatResponse(directResponse, requestedRunId));
		}
		catch (Exception ex)
		{
			var errorResponse = $@"Error retrieving data for Run {requestedRunId}: {ex.Message}

You can still access the data directly:
• API: GET /api/search/run/{requestedRunId}
• SQLite: sqlite3 ""Data/migration.db"" ""SELECT * FROM runs WHERE id = {requestedRunId};""
• Neo4j: cypher-shell -u neo4j -p YOUR_NEO4J_PASSWORD";
			
			return Results.Ok(new ChatResponse(errorResponse, requestedRunId));
		}
	}
	
	// Normal chat flow
	var normalResponse = await client.SendChatAsync(request.Prompt, cancellationToken);
	return Results.Ok(new ChatResponse(normalResponse, null));
});

// Graph endpoint - defaults to current MCP run or accepts specific run ID
app.MapGet("/api/graph", async (IMcpClient client, int? runId, CancellationToken cancellationToken) =>
{
	try
	{
		string graphUri;
		if (runId.HasValue)
		{
			// Use specific run ID
			graphUri = $"insights://runs/{runId.Value}/graph";
		}
		else
		{
			// Get default from MCP resources
			var resources = await client.ListResourcesAsync(cancellationToken);
			var graphResource = resources.FirstOrDefault(r => r.Uri.Contains("/graph"));
			graphUri = graphResource?.Uri ?? "insights://runs/43/graph"; // Fallback to 43
		}
		
		// Fetch the actual graph data from MCP
		var graphJson = await client.ReadResourceAsync(graphUri, cancellationToken);
		
		if (!string.IsNullOrEmpty(graphJson))
		{
			// Parse the graph data
			var graphData = JsonSerializer.Deserialize<JsonObject>(graphJson);
			
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
			}
			
			// Add metadata about which run this is
			if (graphData != null)
			{
				graphData["runId"] = runId ?? 43;
			}
			
			return Results.Ok(graphData);
		}
	}
	catch (Exception ex)
	{
		Console.WriteLine($"Error fetching graph data for run {runId}: {ex.Message}");
	}

	// Return empty graph if fetch fails
	var emptyGraphData = new
	{
		runId = runId ?? 43,
		nodes = Array.Empty<object>(),
		edges = Array.Empty<object>(),
		error = $"Unable to fetch graph data from MCP server for run {runId}"
	};
	
	return Results.Ok(emptyGraphData);
});

app.MapGet("/api/runinfo", async (IMcpClient client, CancellationToken cancellationToken) =>
{
	try
	{
		var resources = await client.ListResourcesAsync(cancellationToken);
		// Extract run ID from first resource URI (e.g., insights://runs/43/summary)
		var firstResource = resources.FirstOrDefault();
		if (firstResource != null)
		{
			var match = System.Text.RegularExpressions.Regex.Match(firstResource.Uri, @"runs/(\d+)/");
			if (match.Success)
			{
				return Results.Ok(new { runId = int.Parse(match.Groups[1].Value) });
			}
		}
	}
	catch (Exception ex)
	{
		Console.WriteLine($"Error getting run info: {ex.Message}");
	}
	return Results.Ok(new { runId = 0 });
});

app.MapGet("/api/runs/all", async (IMcpClient client, CancellationToken cancellationToken) =>
{
	try
	{
		// This would ideally query the database directly, but for now we'll return info from MCP
		var resources = await client.ListResourcesAsync(cancellationToken);
		
		// Extract unique run IDs from resources
		var runIds = new HashSet<int>();
		foreach (var resource in resources)
		{
			var match = System.Text.RegularExpressions.Regex.Match(resource.Uri, @"runs/(\d+)/");
			if (match.Success)
			{
				runIds.Add(int.Parse(match.Groups[1].Value));
			}
		}
		
		return Results.Ok(new { runs = runIds.OrderDescending().ToList() });
	}
	catch (Exception ex)
	{
		Console.WriteLine($"Error getting all runs: {ex.Message}");
		return Results.Ok(new { runs = new List<int>() });
	}
});

app.MapGet("/api/runs/{runId}/dependencies", async (int runId, IMcpClient client, CancellationToken cancellationToken) =>
{
	try
	{
		// Fetch the graph resource for this specific run
		var graphUri = $"insights://runs/{runId}/graph";
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
				
				return Results.Ok(new { 
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

// This endpoint redirects to the search endpoint
app.MapGet("/api/runs/{runId}/combined-data", async (int runId) =>
{
	await Task.CompletedTask;
	return Results.Redirect($"/api/search/run/{runId}");
});

// Search endpoint that queries both SQLite and Neo4j for any run
app.MapGet("/api/search/run/{runId}", async (int runId, IMcpClient client, CancellationToken cancellationToken) =>
{
	try
	{
		// Get SQLite data - provide query instructions
		var dbPath = Path.Combine(Directory.GetCurrentDirectory(), "..", "Data", "migration.db");
		var sqliteData = new
		{
			run_id = runId,
			database_path = dbPath,
			database_exists = File.Exists(dbPath),
			instructions = new
			{
				message = "Use sqlite3 CLI or DB Browser to query this run",
				query_examples = new[]
				{
					$"SELECT * FROM runs WHERE id = {runId};",
					$"SELECT COUNT(*) as file_count FROM cobol_files WHERE run_id = {runId};",
					$"SELECT file_name, is_copybook FROM cobol_files WHERE run_id = {runId} LIMIT 10;"
				},
				cli_command = $"sqlite3 \"{dbPath}\" \"SELECT id, status, started_at, completed_at FROM runs WHERE id = {runId};\""
			},
			available_tables = new[] { "runs", "cobol_files", "analyses", "dependencies", "copybook_usage", "metrics" }
		};
		
		// Get Neo4j data (via MCP) - try to get graph
		object neo4jData;
		try
		{
			var graphUri = $"insights://runs/{runId}/graph";
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
			runId,
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
				credentials = new { username = "neo4j", password = "YOUR_NEO4J_PASSWORD" },
				data = neo4jData
				}
			},
			howToQuery = new
			{
				sqlite = new
				{
					cli = $"sqlite3 \"Data/migration.db\" \"SELECT * FROM runs WHERE id = {runId};\"",
					queries = new[]
					{
						$"SELECT id, status, started_at, completed_at FROM runs WHERE id = {runId};",
						$"SELECT file_name, is_copybook FROM cobol_files WHERE run_id = {runId};",
						$"SELECT program_name, analysis_data FROM analyses WHERE run_id = {runId};"
					}
				},
				neo4j = new
				{
					cypher_shell = $"echo 'MATCH (n) WHERE n.runId = {runId} RETURN n LIMIT 25;' | cypher-shell -u neo4j -p YOUR_NEO4J_PASSWORD",
					queries = new[]
					{
						$"MATCH (n) WHERE n.runId = {runId} RETURN n LIMIT 25;",
						$"MATCH (n)-[r]->(m) WHERE n.runId = {runId} AND m.runId = {runId} RETURN n, r, m LIMIT 50;"
					}
				},
				api = new
				{
					combined_data = $"/api/runs/{runId}/combined-data",
					dependencies = $"/api/runs/{runId}/dependencies",
					mcp_resources = new[]
					{
						$"insights://runs/{runId}/summary",
						$"insights://runs/{runId}/dependencies",
						$"insights://runs/{runId}/graph"
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

app.MapGet("/api/data-retrieval-guide", async () =>
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
			credentials = new { username = "neo4j", password = "YOUR_NEO4J_PASSWORD" },
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
					new { name = "Cypher Shell", command = "cypher-shell -a bolt://localhost:7687 -u neo4j -p YOUR_NEO4J_PASSWORD" }
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
				"Login: neo4j / YOUR_NEO4J_PASSWORD",
				"Run: MATCH (r:Run {runId: 43})-[:CONTAINS]->(f:CobolFile) RETURN f LIMIT 25;",
					"Visualize dependencies: MATCH path = (r:Run {runId: 43})-[:CONTAINS]->()-[d:DEPENDS_ON]->() RETURN path;"
				}
			},
			new 
			{ 
				title = "Retrieve via MCP API",
				steps = new[]
				{
					"curl http://localhost:5250/api/runs/all",
					"curl http://localhost:5250/api/runs/43/dependencies | jq '.'",
					"curl -X POST http://localhost:5250/api/chat -H 'Content-Type: application/json' -d '{\"prompt\":\"Show me all dependencies for run 43\"}'"
				}
			}
		}
	};
	
	await Task.CompletedTask;
	return Results.Ok(guide);
});

app.MapFallbackToFile("index.html");

app.Run();
