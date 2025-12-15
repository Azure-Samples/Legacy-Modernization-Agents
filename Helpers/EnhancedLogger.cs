using Microsoft.Extensions.Logging;
using System.Text;
using System.Text.Json;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Enhanced logger with ASCII progress bars, API call tracking, color-coded console output, and comprehensive file logging, because its cool .
/// </summary>
public class EnhancedLogger
{
    private readonly ILogger _logger;
    private readonly object _consoleLock = new object();
    private readonly object _fileLock = new object();
    private readonly List<ConversationEntry> _agentConversations = new List<ConversationEntry>();
    private readonly List<ApiCallEntry> _apiCalls = new List<ApiCallEntry>();
    private int _apiCallCounter = 0;
    
    // File logging paths
    private readonly string _baseLogPath;
    private readonly string _apiCallLogPath;
    private readonly string _migrationLogPath;
    private readonly string _analysisLogPath;
    private readonly string _performanceLogPath;
    private readonly string _sessionId;

    // ANSI Color Codes for console output
    public static class Colors
    {
        public const string Reset = "\u001b[0m";
        public const string Bold = "\u001b[1m";
        
        // Standard Colors
        public const string Black = "\u001b[30m";
        public const string Red = "\u001b[31m";
        public const string Green = "\u001b[32m";
        public const string Yellow = "\u001b[33m";
        public const string Blue = "\u001b[34m";
        public const string Magenta = "\u001b[35m";
        public const string Cyan = "\u001b[36m";
        public const string White = "\u001b[37m";
        
        // Bright Colors
        public const string BrightBlack = "\u001b[90m";
        public const string BrightRed = "\u001b[91m";
        public const string BrightGreen = "\u001b[92m";
        public const string BrightYellow = "\u001b[93m";
        public const string BrightBlue = "\u001b[94m";
        public const string BrightMagenta = "\u001b[95m";
        public const string BrightCyan = "\u001b[96m";
        public const string BrightWhite = "\u001b[97m";
        
        // Background Colors
        public const string BgRed = "\u001b[41m";
        public const string BgGreen = "\u001b[42m";
        public const string BgYellow = "\u001b[43m";
        public const string BgBlue = "\u001b[44m";
    }

    /// <summary>
    /// Represents an API call entry for tracking purposes.
    /// </summary>
    public class ApiCallEntry
    {
        public int CallId { get; set; }
        public DateTime Timestamp { get; set; }
        public string Agent { get; set; } = string.Empty;
        public string Method { get; set; } = string.Empty;
        public string Endpoint { get; set; } = string.Empty;
        public string Model { get; set; } = string.Empty;
        public string Request { get; set; } = string.Empty;
        public string Response { get; set; } = string.Empty;
        public TimeSpan Duration { get; set; }
        public bool IsSuccess { get; set; }
        public string Error { get; set; } = string.Empty;
        public int TokensUsed { get; set; }
        public decimal Cost { get; set; }
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="EnhancedLogger"/> class with default logging paths.
    /// </summary>
    /// <param name="logger">The underlying logger instance.</param>
    public EnhancedLogger(ILogger logger) : this(logger, "Logs", DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss"))
    {
    }

    /// <summary>
    /// Initializes a new instance of the <see cref="EnhancedLogger"/> class.
    /// </summary>
    /// <param name="logger">The underlying logger instance.</param>
    /// <param name="baseLogPath">The base path for log files.</param>
    /// <param name="sessionId">The session ID for logging.</param>
    public EnhancedLogger(ILogger logger, string baseLogPath, string sessionId)
    {
        _logger = logger;
        _sessionId = sessionId;
        _baseLogPath = Path.Combine(Directory.GetCurrentDirectory(), baseLogPath);

        _apiCallLogPath = Path.Combine(_baseLogPath, "ApiCalls");
        _migrationLogPath = Path.Combine(_baseLogPath, "Migration");
        _analysisLogPath = Path.Combine(_baseLogPath, "Analysis");
        _performanceLogPath = Path.Combine(_baseLogPath, "Performance");

        // Ensure all log directories exist
        Directory.CreateDirectory(_apiCallLogPath);
        Directory.CreateDirectory(_migrationLogPath);
        Directory.CreateDirectory(_analysisLogPath);
        Directory.CreateDirectory(_performanceLogPath);

        // Create session start log
        LogToFile("SESSION_START", $"Enhanced logging session started: {sessionId}", _baseLogPath);
    }

    /// <summary>
    /// Displays an ASCII progress bar with current progress.
    /// </summary>
    /// <param name="current">Current progress value.</param>
    /// <param name="total">Total expected value.</param>
    /// <param name="message">Progress message.</param>
    /// <param name="width">Width of the progress bar (default: 50).</param>
    public void ShowProgressBar(int current, int total, string message, int width = 50)
    {
        lock (_consoleLock)
        {
            var percentage = total > 0 ? (double)current / total : 0;
            var filled = (int)(percentage * width);
            var empty = width - filled;

            var progressBar = new StringBuilder();
            progressBar.Append($"{Colors.BrightBlue}[{Colors.Reset}");
            progressBar.Append($"{Colors.BrightGreen}{new string('█', filled)}{Colors.Reset}");
            progressBar.Append($"{Colors.BrightBlack}{new string('░', empty)}{Colors.Reset}");
            progressBar.Append($"{Colors.BrightBlue}]{Colors.Reset}");
            
            var percentText = $"{Colors.BrightYellow}{percentage:P1}{Colors.Reset}".PadLeft(15);
            var countText = $"{Colors.BrightCyan}({current}/{total}){Colors.Reset}".PadLeft(20);
            
            Console.Write($"\r{Colors.BrightWhite}{message}{Colors.Reset}: {progressBar} {percentText} {countText}");
            
            if (current >= total)
            {
                Console.WriteLine(); // New line when complete
            }
        }
    }

    /// <summary>
    /// Displays a fancy header for process sections with enhanced color coding.
    /// </summary>
    /// <param name="title">The section title.</param>
    /// <param name="subtitle">Optional subtitle.</param>
    public void ShowSectionHeader(string title, string? subtitle = null)
    {
        lock (_consoleLock)
        {
            var width = 80;
            var border = new string('═', width);
            
            Console.WriteLine();
            Console.WriteLine($"{Colors.BrightCyan}{Colors.Bold}╔{border}╗{Colors.Reset}");
            Console.WriteLine($"{Colors.BrightCyan}║{Colors.Reset}{Colors.BrightWhite}{Colors.Bold}{title.PadLeft((width + title.Length) / 2).PadRight(width)}{Colors.Reset}{Colors.BrightCyan}║{Colors.Reset}");
            
            if (!string.IsNullOrEmpty(subtitle))
            {
                Console.WriteLine($"{Colors.BrightCyan}║{Colors.Reset}{Colors.BrightYellow}{subtitle.PadLeft((width + subtitle.Length) / 2).PadRight(width)}{Colors.Reset}{Colors.BrightCyan}║{Colors.Reset}");
            }
            
            Console.WriteLine($"{Colors.BrightCyan}╚{border}╝{Colors.Reset}");
            Console.WriteLine();
        }
        
        _logger.LogInformation("=== {Title} {Subtitle} ===", title, subtitle ?? "");
    }

    /// <summary>
    /// Shows a step indicator with enhanced color formatting.
    /// </summary>
    /// <param name="stepNumber">Current step number.</param>
    /// <param name="totalSteps">Total number of steps.</param>
    /// <param name="stepName">Name of the current step.</param>
    /// <param name="description">Optional step description.</param>
    public void ShowStep(int stepNumber, int totalSteps, string stepName, string? description = null)
    {
        lock (_consoleLock)
        {
            Console.Write($"{Colors.BrightGreen}Step {stepNumber}/{totalSteps}:{Colors.Reset} ");
            Console.Write($"{Colors.BrightYellow}{Colors.Bold}{stepName}{Colors.Reset}");
            
            if (!string.IsNullOrEmpty(description))
            {
                Console.Write($"{Colors.BrightBlue} - {description}{Colors.Reset}");
            }
            
            Console.WriteLine();
        }
        
        _logger.LogInformation("Step {StepNumber}/{TotalSteps}: {StepName} - {Description}", 
            stepNumber, totalSteps, stepName, description ?? "");
    }

    /// <summary>
    /// Logs an agent conversation entry for debugging and analysis.
    /// </summary>
    /// <param name="agentName">Name of the agent.</param>
    /// <param name="action">Action being performed.</param>
    /// <param name="input">Input data (truncated for logging).</param>
    /// <param name="output">Output data (truncated for logging).</param>
    /// <param name="duration">Time taken for the operation.</param>
    public void LogAgentConversation(string agentName, string action, string input, string output, TimeSpan duration)
    {
        var conversation = new ConversationEntry
        {
            Timestamp = DateTime.UtcNow,
            AgentName = agentName,
            Action = action,
            Input = TruncateText(input, 500),
            Output = TruncateText(output, 500),
            Duration = duration
        };

        lock (_agentConversations)
        {
            _agentConversations.Add(conversation);
        }

        _logger.LogDebug("Agent: {AgentName} | Action: {Action} | Duration: {Duration}ms | Input: {InputLength} chars | Output: {OutputLength} chars",
            agentName, action, duration.TotalMilliseconds, input.Length, output.Length);
    }

    /// <summary>
    /// Shows a summary of agent conversations and performance metrics.
    /// </summary>
    public void ShowConversationSummary()
    {
        lock (_consoleLock)
        {
            Console.WriteLine();
            Console.ForegroundColor = ConsoleColor.Magenta;
            Console.WriteLine("╔════════════════════════════════════════════════════════════════════════════════╗");
            Console.WriteLine("║                            AGENT CONVERSATION SUMMARY                         ║");
            Console.WriteLine("╚════════════════════════════════════════════════════════════════════════════════╝");
            Console.ResetColor();

            if (!_agentConversations.Any())
            {
                Console.WriteLine("No agent conversations recorded.");
                return;
            }

            var groupedConversations = _agentConversations
                .GroupBy(c => c.AgentName)
                .OrderBy(g => g.Key);

            foreach (var agentGroup in groupedConversations)
            {
                var totalDuration = agentGroup.Sum(c => c.Duration.TotalMilliseconds);
                var avgDuration = agentGroup.Average(c => c.Duration.TotalMilliseconds);
                var actionCounts = agentGroup.GroupBy(c => c.Action).ToDictionary(g => g.Key, g => g.Count());

                Console.WriteLine($"\n{Colors.BrightCyan}🤖 Agent: {agentGroup.Key}{Colors.Reset}");
                Console.WriteLine($"   {Colors.Green}Total Calls:{Colors.Reset} {agentGroup.Count()}");
                Console.WriteLine($"   {Colors.Yellow}Total Time:{Colors.Reset} {totalDuration:F1}ms");
                Console.WriteLine($"   {Colors.Cyan}Average Time:{Colors.Reset} {avgDuration:F1}ms");
                Console.WriteLine($"   {Colors.Magenta}Actions:{Colors.Reset} {string.Join(", ", actionCounts.Select(kv => $"{kv.Key}({kv.Value})"))}");
            }

            var totalCalls = _agentConversations.Count;
            var totalTime = _agentConversations.Sum(c => c.Duration.TotalMilliseconds);
            
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine($"\n📊 Overall Stats:");
            Console.ResetColor();
            Console.WriteLine($"   Total Agent Calls: {totalCalls}");
            Console.WriteLine($"   Total Processing Time: {totalTime:F1}ms ({totalTime / 1000:F1}s)");
            Console.WriteLine($"   Average Call Duration: {(totalCalls > 0 ? totalTime / totalCalls : 0):F1}ms");
            Console.WriteLine();
        }
    }

    /// <summary>
    /// Shows a warning message with special formatting.
    /// </summary>
    /// <param name="message">Warning message.</param>
    public void ShowWarning(string message)
    {
        lock (_consoleLock)
        {
            Console.ForegroundColor = ConsoleColor.Yellow;
            Console.WriteLine($"⚠️  Warning: {message}");
            Console.ResetColor();
        }
        
        _logger.LogWarning(message);
    }

    /// <summary>
    /// Shows an error message with special formatting.
    /// </summary>
    /// <param name="message">Error message.</param>
    /// <param name="exception">Optional exception details.</param>
    public void ShowError(string message, Exception? exception = null)
    {
        lock (_consoleLock)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.WriteLine($"❌ Error: {message}");
            if (exception != null)
            {
                Console.WriteLine($"   Details: {exception.Message}");
            }
            Console.ResetColor();
        }
        
        if (exception != null)
        {
            _logger.LogError(exception, message);
        }
        else
        {
            _logger.LogError(message);
        }
    }

    /// <summary>
    /// Shows a success message with special formatting.
    /// </summary>
    /// <param name="message">Success message.</param>
    public void ShowSuccess(string message)
    {
        lock (_consoleLock)
        {
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine($"✅ {message}");
            Console.ResetColor();
        }
        
        _logger.LogInformation(message);
    }

    /// <summary>
    /// Exports the conversation log to a file for analysis.
    /// </summary>
    /// <param name="filePath">Path to save the conversation log.</param>
    public async Task ExportConversationLogAsync(string filePath)
    {
        var logContent = new StringBuilder();
        logContent.AppendLine("# Agent Conversation Log");
        logContent.AppendLine($"Generated: {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss} UTC");
        logContent.AppendLine();

        lock (_agentConversations)
        {
            foreach (var conversation in _agentConversations.OrderBy(c => c.Timestamp))
            {
                logContent.AppendLine($"## {conversation.Timestamp:HH:mm:ss.fff} - {conversation.AgentName}");
                logContent.AppendLine($"**Action:** {conversation.Action}");
                logContent.AppendLine($"**Duration:** {conversation.Duration.TotalMilliseconds:F1}ms");
                logContent.AppendLine($"**Input:** ```{conversation.Input}```");
                logContent.AppendLine($"**Output:** ```{conversation.Output}```");
                logContent.AppendLine();
            }
        }

        await File.WriteAllTextAsync(filePath, logContent.ToString());
        _logger.LogInformation("Conversation log exported to: {FilePath}", filePath);
    }

    /// <summary>
    /// Truncates text to a specified length for logging.
    /// </summary>
    /// <param name="text">Text to truncate.</param>
    /// <param name="maxLength">Maximum length.</param>
    /// <returns>Truncated text.</returns>
    private static string TruncateText(string text, int maxLength)
    {
        if (string.IsNullOrEmpty(text) || text.Length <= maxLength)
            return text;

        return text.Substring(0, maxLength - 3) + "...";
    }

    /// <summary>
    /// Logs the start of an API call and returns a tracking ID.
    /// </summary>
    /// <param name="agent">The AI agent making the call.</param>
    /// <param name="method">HTTP method or API method.</param>
    /// <param name="endpoint">API endpoint or service.</param>
    /// <param name="model">AI model being used.</param>
    /// <param name="request">Request details (truncated for logging).</param>
    /// <returns>API call tracking ID.</returns>
    public int LogApiCallStart(string agent, string method, string endpoint, string model, string request = "")
    {
        lock (_consoleLock)
        {
            var callId = ++_apiCallCounter;
            var truncatedRequest = request.Length > 100 ? request.Substring(0, 100) + "..." : request;
            
            var apiCall = new ApiCallEntry
            {
                CallId = callId,
                Timestamp = DateTime.UtcNow,
                Agent = agent,
                Method = method,
                Endpoint = endpoint,
                Model = model,
                Request = truncatedRequest
            };
            
            _apiCalls.Add(apiCall);
            
            // Color-coded console output
            Console.WriteLine($"{Colors.BrightCyan}🌐 API CALL #{callId:D3}{Colors.Reset} " +
                            $"{Colors.Yellow}[{agent}]{Colors.Reset} " +
                            $"{Colors.Blue}{method}{Colors.Reset} → " +
                            $"{Colors.Magenta}{model}{Colors.Reset} " +
                            $"{Colors.Green}@ {DateTime.Now:HH:mm:ss.fff}{Colors.Reset}");
            
            if (!string.IsNullOrEmpty(truncatedRequest))
            {
                Console.WriteLine($"   {Colors.BrightBlue}📤 Request:{Colors.Reset} {truncatedRequest}");
            }
            
            _logger.LogInformation("API Call #{CallId} started: {Agent} -> {Method} {Endpoint} using {Model}", 
                callId, agent, method, endpoint, model);
            
            return callId;
        }
    }

    /// <summary>
    /// Logs the completion of an API call.
    /// </summary>
    /// <param name="callId">API call tracking ID.</param>
    /// <param name="response">Response details (truncated for logging).</param>
    /// <param name="tokensUsed">Number of tokens consumed.</param>
    /// <param name="cost">Estimated cost of the call.</param>
    public void LogApiCallEnd(int callId, string response = "", int tokensUsed = 0, decimal cost = 0)
    {
        lock (_consoleLock)
        {
            var apiCall = _apiCalls.FirstOrDefault(c => c.CallId == callId);
            if (apiCall != null)
            {
                apiCall.Duration = DateTime.UtcNow - apiCall.Timestamp;
                apiCall.Response = response.Length > 200 ? response.Substring(0, 200) + "..." : response;
                apiCall.IsSuccess = true;
                apiCall.TokensUsed = tokensUsed;
                apiCall.Cost = cost;
                
                // Log to file
                LogApiCallToFile(apiCall);
                // Color-coded success output
                Console.WriteLine($"{Colors.BrightGreen}✅ API CALL #{callId:D3} COMPLETED{Colors.Reset} " +
                                $"{Colors.Cyan}⏱️ {apiCall.Duration.TotalMilliseconds:F0}ms{Colors.Reset} " +
                                $"{Colors.Yellow}🔤 {tokensUsed} tokens{Colors.Reset} " +
                                $"{Colors.Green}💰 ${cost:F4}{Colors.Reset}");
                
                if (!string.IsNullOrEmpty(apiCall.Response))
                {
                    Console.WriteLine($"   {Colors.BrightGreen}📥 Response:{Colors.Reset} {apiCall.Response}");
                }
            }
            
            _logger.LogInformation("API Call #{CallId} completed in {Duration}ms with {Tokens} tokens", 
                callId, apiCall?.Duration.TotalMilliseconds ?? 0, tokensUsed);
        }
    }

    /// <summary>
    /// Logs an API call error.
    /// </summary>
    /// <param name="callId">API call tracking ID.</param>
    /// <param name="error">Error message.</param>
    public void LogApiCallError(int callId, string error)
    {
        lock (_consoleLock)
        {
            var apiCall = _apiCalls.FirstOrDefault(c => c.CallId == callId);
            if (apiCall != null)
            {
                apiCall.Duration = DateTime.UtcNow - apiCall.Timestamp;
                apiCall.IsSuccess = false;
                apiCall.Error = error;
                
                // Log to file
                LogApiCallToFile(apiCall);
                
                // Color-coded error output
                Console.WriteLine($"{Colors.BrightRed}❌ API CALL #{callId:D3} FAILED{Colors.Reset} " +
                                $"{Colors.Cyan}⏱️ {apiCall.Duration.TotalMilliseconds:F0}ms{Colors.Reset}");
                Console.WriteLine($"   {Colors.BrightRed}💥 Error:{Colors.Reset} {error}");
            }
            
            _logger.LogError("API Call #{CallId} failed: {Error}", callId, error);
        }
    }

    /// <summary>
    /// Shows API call statistics with color coding.
    /// </summary>
    public void ShowApiCallStatistics()
    {
        lock (_consoleLock)
        {
            var totalCalls = _apiCalls.Count;
            var successfulCalls = _apiCalls.Count(c => c.IsSuccess);
            var failedCalls = _apiCalls.Count(c => !c.IsSuccess);
            var totalTokens = _apiCalls.Sum(c => c.TokensUsed);
            var totalCost = _apiCalls.Sum(c => c.Cost);
            var avgDuration = _apiCalls.Any() ? _apiCalls.Average(c => c.Duration.TotalMilliseconds) : 0;

            Console.WriteLine();
            Console.WriteLine($"{Colors.BrightCyan}╔═══════════════════════════════════════════════════════════════════════════════╗{Colors.Reset}");
            Console.WriteLine($"{Colors.BrightCyan}║{Colors.Reset}                           {Colors.Bold}API CALL STATISTICS{Colors.Reset}                            {Colors.BrightCyan}║{Colors.Reset}");
            Console.WriteLine($"{Colors.BrightCyan}╚═══════════════════════════════════════════════════════════════════════════════╝{Colors.Reset}");
            Console.WriteLine($"{Colors.Green}📊 Total API Calls:{Colors.Reset} {totalCalls}");
            Console.WriteLine($"{Colors.BrightGreen}✅ Successful:{Colors.Reset} {successfulCalls} " +
                            $"{Colors.BrightRed}❌ Failed:{Colors.Reset} {failedCalls}");
            Console.WriteLine($"{Colors.Yellow}🔤 Total Tokens:{Colors.Reset} {totalTokens:N0}");
            Console.WriteLine($"{Colors.Green}💰 Total Cost:{Colors.Reset} ${totalCost:F4}");
            Console.WriteLine($"{Colors.Cyan}⏱️ Average Duration:{Colors.Reset} {avgDuration:F0}ms");
            Console.WriteLine();
        }
    }

    /// <summary>
    /// Exports API call log to a file.
    /// </summary>
    /// <param name="filePath">Path to export the API call log.</param>
    public async Task ExportApiCallLogAsync(string filePath)
    {
        var json = System.Text.Json.JsonSerializer.Serialize(_apiCalls, new System.Text.Json.JsonSerializerOptions
        {
            WriteIndented = true
        });
        
        await File.WriteAllTextAsync(filePath, json);
        Console.WriteLine($"{Colors.BrightGreen}📁 API call log exported:{Colors.Reset} {filePath}");
    }

    /// <summary>
    /// Logs detailed behind-the-scenes information with color coding.
    /// </summary>
    /// <param name="category">Category of the operation (e.g., "FILE_IO", "AI_PROCESSING", "VALIDATION").</param>
    /// <param name="operation">Specific operation being performed.</param>
    /// <param name="details">Detailed information about the operation.</param>
    /// <param name="data">Optional data payload (will be truncated for logging).</param>
    public void LogBehindTheScenes(string category, string operation, string details, object? data = null)
    {
        lock (_consoleLock)
        {
            var timestamp = DateTime.Now.ToString("HH:mm:ss.fff");
            var categoryColor = GetCategoryColor(category);
            var truncatedData = data?.ToString()?.Length > 150 ? 
                data.ToString()!.Substring(0, 150) + "..." : 
                data?.ToString() ?? "";

            Console.WriteLine($"{Colors.BrightBlack}[{timestamp}]{Colors.Reset} " +
                            $"{categoryColor}[{category}]{Colors.Reset} " +
                            $"{Colors.BrightWhite}{operation}{Colors.Reset} " +
                            $"{Colors.BrightBlue}→{Colors.Reset} {details}");
            
            if (!string.IsNullOrEmpty(truncatedData))
            {
                Console.WriteLine($"   {Colors.BrightBlack}💾 Data:{Colors.Reset} {truncatedData}");
            }
        }
        
        // Log to appropriate file based on category
        switch (category.ToUpper())
        {
            case "MIGRATION":
            case "STEP_START":
            case "STEP_END":
                LogMigrationStepToFile(operation, "IN_PROGRESS", details, data);
                break;
            case "AI_PROCESSING":
            case "COBOL_ANALYSIS":
            case "JAVA_CONVERSION":
            case "DEPENDENCY_ANALYSIS":
                LogAnalysisToFile(operation, category, details, TimeSpan.Zero);
                break;
            case "PERFORMANCE":
                LogPerformanceToFile(operation, TimeSpan.Zero, 0, 0);
                break;
            default:
                LogToFile($"BEHIND_SCENES_{category}", $"{operation}: {details}", _baseLogPath, data);
                break;
        }
        
        _logger.LogDebug("[{Category}] {Operation}: {Details}", category, operation, details);
    }

    /// <summary>
    /// Gets the appropriate color for a logging category.
    /// </summary>
    /// <param name="category">The logging category.</param>
    /// <returns>ANSI color code for the category.</returns>
    private string GetCategoryColor(string category)
    {
        return category.ToUpper() switch
        {
            "FILE_IO" => Colors.BrightMagenta,
            "AI_PROCESSING" => Colors.BrightCyan,
            "VALIDATION" => Colors.BrightGreen,
            "ERROR" => Colors.BrightRed,
            "WARNING" => Colors.BrightYellow,
            "NETWORK" => Colors.BrightBlue,
            "DATABASE" => Colors.Magenta,
            "SECURITY" => Colors.Red,
            "PERFORMANCE" => Colors.Yellow,
            _ => Colors.BrightWhite
        };
    }

    /// <summary>
    /// Logs performance metrics with color coding.
    /// </summary>
    /// <param name="operation">The operation being measured.</param>
    /// <param name="duration">Duration of the operation.</param>
    /// <param name="itemsProcessed">Number of items processed (optional).</param>
    /// <param name="memoryUsed">Memory usage in MB (optional).</param>
    public void LogPerformanceMetrics(string operation, TimeSpan duration, int itemsProcessed = 0, double memoryUsed = 0)
    {
        lock (_consoleLock)
        {
            var durationColor = duration.TotalSeconds < 1 ? Colors.BrightGreen :
                              duration.TotalSeconds < 5 ? Colors.Yellow :
                              Colors.BrightRed;

            Console.WriteLine($"{Colors.BrightCyan}⚡ PERFORMANCE:{Colors.Reset} " +
                            $"{Colors.BrightWhite}{operation}{Colors.Reset} " +
                            $"{durationColor}⏱️ {duration.TotalMilliseconds:F0}ms{Colors.Reset}");

            if (itemsProcessed > 0)
            {
                var throughput = itemsProcessed / Math.Max(duration.TotalSeconds, 0.001);
                Console.WriteLine($"   {Colors.BrightGreen}📊 Processed:{Colors.Reset} {itemsProcessed} items " +
                                $"{Colors.BrightYellow}📈 Throughput:{Colors.Reset} {throughput:F1} items/sec");
            }

            if (memoryUsed > 0)
            {
                var memoryColor = memoryUsed < 100 ? Colors.BrightGreen :
                                memoryUsed < 500 ? Colors.Yellow :
                                Colors.BrightRed;
                Console.WriteLine($"   {memoryColor}🧠 Memory:{Colors.Reset} {memoryUsed:F1} MB");
            }
        }
        
        // Log to performance file
        LogPerformanceToFile(operation, duration, itemsProcessed, memoryUsed);
        
        _logger.LogInformation("Performance: {Operation} completed in {Duration}ms, processed {Items} items, used {Memory}MB",
            operation, duration.TotalMilliseconds, itemsProcessed, memoryUsed);
    }

    /// <summary>
    /// Shows comprehensive API call statistics with detailed analytics.
    /// </summary>
    public void ShowApiStatistics()
    {
        lock (_consoleLock)
        {
            Console.WriteLine();
            Console.WriteLine($"{Colors.BrightMagenta}{Colors.Bold}╔════════════════════════════════════════════════════════════════════════════════╗{Colors.Reset}");
            Console.WriteLine($"{Colors.BrightMagenta}{Colors.Bold}║                               API CALL STATISTICS                              ║{Colors.Reset}");
            Console.WriteLine($"{Colors.BrightMagenta}{Colors.Bold}╚════════════════════════════════════════════════════════════════════════════════╝{Colors.Reset}");

            if (!_apiCalls.Any())
            {
                Console.WriteLine($"{Colors.Yellow}No API calls recorded.{Colors.Reset}");
                return;
            }

            var totalCalls = _apiCalls.Count;
            var successfulCalls = _apiCalls.Count(c => c.IsSuccess);
            var failedCalls = totalCalls - successfulCalls;
            var totalDuration = _apiCalls.Sum(c => c.Duration.TotalMilliseconds);
            var avgDuration = totalCalls > 0 ? totalDuration / totalCalls : 0;
            var totalTokens = _apiCalls.Sum(c => c.TokensUsed);
            var totalCost = _apiCalls.Sum(c => c.Cost);

            // Overall statistics
            Console.WriteLine($"\n{Colors.BrightCyan}📊 Overall Statistics:{Colors.Reset}");
            Console.WriteLine($"   {Colors.Green}Total API Calls:{Colors.Reset} {totalCalls}");
            Console.WriteLine($"   {Colors.BrightGreen}Successful Calls:{Colors.Reset} {successfulCalls} ({(totalCalls > 0 ? (double)successfulCalls / totalCalls * 100 : 0):F1}%)");
            Console.WriteLine($"   {Colors.BrightRed}Failed Calls:{Colors.Reset} {failedCalls} ({(totalCalls > 0 ? (double)failedCalls / totalCalls * 100 : 0):F1}%)");
            Console.WriteLine($"   {Colors.Yellow}Total Duration:{Colors.Reset} {totalDuration:F1}ms ({totalDuration / 1000:F1}s)");
            Console.WriteLine($"   {Colors.Cyan}Average Duration:{Colors.Reset} {avgDuration:F1}ms");
            Console.WriteLine($"   {Colors.BrightBlue}Total Tokens:{Colors.Reset} {totalTokens:N0}");
            Console.WriteLine($"   {Colors.BrightMagenta}Estimated Cost:{Colors.Reset} ${totalCost:F4}");

            // Agent breakdown
            var agentStats = _apiCalls
                .GroupBy(c => c.Agent)
                .OrderBy(g => g.Key);

            Console.WriteLine($"\n{Colors.BrightCyan}🤖 Agent Breakdown:{Colors.Reset}");
            foreach (var agentGroup in agentStats)
            {
                var agentCalls = agentGroup.Count();
                var agentSuccess = agentGroup.Count(c => c.IsSuccess);
                var agentDuration = agentGroup.Sum(c => c.Duration.TotalMilliseconds);
                var agentTokens = agentGroup.Sum(c => c.TokensUsed);
                var agentCost = agentGroup.Sum(c => c.Cost);

                Console.WriteLine($"\n   {Colors.BrightWhite}{agentGroup.Key}:{Colors.Reset}");
                Console.WriteLine($"     {Colors.Green}Calls:{Colors.Reset} {agentCalls} ({agentSuccess}/{agentCalls} successful)");
                Console.WriteLine($"     {Colors.Yellow}Duration:{Colors.Reset} {agentDuration:F1}ms");
                Console.WriteLine($"     {Colors.BrightBlue}Tokens:{Colors.Reset} {agentTokens:N0}");
                Console.WriteLine($"     {Colors.BrightMagenta}Cost:{Colors.Reset} ${agentCost:F4}");
            }

            // Model usage breakdown
            var modelStats = _apiCalls
                .GroupBy(c => c.Model)
                .OrderBy(g => g.Key);

            Console.WriteLine($"\n{Colors.BrightCyan}🧠 Model Usage:{Colors.Reset}");
            foreach (var modelGroup in modelStats)
            {
                var modelCalls = modelGroup.Count();
                var modelTokens = modelGroup.Sum(c => c.TokensUsed);
                var modelCost = modelGroup.Sum(c => c.Cost);

                Console.WriteLine($"   {Colors.BrightWhite}{modelGroup.Key}:{Colors.Reset} {modelCalls} calls, {modelTokens:N0} tokens, ${modelCost:F4}");
            }

            // Performance insights
            if (totalCalls > 0)
            {
                var fastestCall = _apiCalls.Min(c => c.Duration.TotalMilliseconds);
                var slowestCall = _apiCalls.Max(c => c.Duration.TotalMilliseconds);

                Console.WriteLine($"\n{Colors.BrightCyan}⚡ Performance Insights:{Colors.Reset}");
                Console.WriteLine($"   {Colors.Green}Fastest Call:{Colors.Reset} {fastestCall:F1}ms");
                Console.WriteLine($"   {Colors.Yellow}Slowest Call:{Colors.Reset} {slowestCall:F1}ms");
                Console.WriteLine($"   {Colors.Cyan}Throughput:{Colors.Reset} {(totalCalls / (totalDuration / 1000)):F2} calls/second");
            }

            Console.WriteLine();
        }
    }

    /// <summary>
    /// Shows recent API calls with detailed information.
    /// </summary>
    /// <param name="count">Number of recent calls to show (default: 10).</param>
    public void ShowRecentApiCalls(int count = 10)
    {
        lock (_consoleLock)
        {
            Console.WriteLine($"\n{Colors.BrightCyan}📞 Recent API Calls (Last {count}):{Colors.Reset}");
            
            var recentCalls = _apiCalls
                .OrderByDescending(c => c.Timestamp)
                .Take(count);

            foreach (var call in recentCalls)
            {
                var statusIcon = call.IsSuccess ? $"{Colors.Green}✅" : $"{Colors.Red}❌";
                var durationColor = call.Duration.TotalMilliseconds < 1000 ? Colors.Green : 
                                  call.Duration.TotalMilliseconds < 5000 ? Colors.Yellow : Colors.Red;
                
                Console.WriteLine($"   {statusIcon} [{call.CallId:D3}] {Colors.BrightWhite}{call.Agent}{Colors.Reset}");
                Console.WriteLine($"       {Colors.Cyan}Model:{Colors.Reset} {call.Model}");
                Console.WriteLine($"       {durationColor}Duration:{Colors.Reset} {call.Duration.TotalMilliseconds:F1}ms");
                Console.WriteLine($"       {Colors.BrightBlue}Tokens:{Colors.Reset} {call.TokensUsed}");
                
                if (!call.IsSuccess && !string.IsNullOrEmpty(call.Error))
                {
                    Console.WriteLine($"       {Colors.Red}Error:{Colors.Reset} {call.Error}");
                }
                Console.WriteLine();
            }
        }
    }

    /// <summary>
    /// Shows cost analysis and token usage patterns.
    /// </summary>
    public void ShowCostAnalysis()
    {
        lock (_consoleLock)
        {
            Console.WriteLine($"\n{Colors.BrightMagenta}💰 Cost Analysis:{Colors.Reset}");

            if (!_apiCalls.Any())
            {
                Console.WriteLine($"{Colors.Yellow}No API calls to analyze.{Colors.Reset}");
                return;
            }

            var totalCost = _apiCalls.Sum(c => c.Cost);
            var totalTokens = _apiCalls.Sum(c => c.TokensUsed);
            var avgCostPerCall = _apiCalls.Count > 0 ? totalCost / _apiCalls.Count : 0;
            var avgTokensPerCall = _apiCalls.Count > 0 ? (double)totalTokens / _apiCalls.Count : 0;

            Console.WriteLine($"   {Colors.BrightMagenta}Total Cost:{Colors.Reset} ${totalCost:F4}");
            Console.WriteLine($"   {Colors.BrightBlue}Total Tokens:{Colors.Reset} {totalTokens:N0}");
            Console.WriteLine($"   {Colors.Cyan}Average Cost per Call:{Colors.Reset} ${avgCostPerCall:F4}");
            Console.WriteLine($"   {Colors.Yellow}Average Tokens per Call:{Colors.Reset} {avgTokensPerCall:F1}");

            if (totalTokens > 0)
            {
                Console.WriteLine($"   {Colors.Green}Cost per 1K Tokens:{Colors.Reset} ${(totalCost / totalTokens * 1000):F4}");
            }

            // Cost by agent
            var agentCosts = _apiCalls
                .GroupBy(c => c.Agent)
                .Select(g => new { Agent = g.Key, Cost = g.Sum(c => c.Cost), Tokens = g.Sum(c => c.TokensUsed) })
                .OrderByDescending(a => a.Cost);

            Console.WriteLine($"\n   {Colors.BrightCyan}Cost by Agent:{Colors.Reset}");
            foreach (var agent in agentCosts)
            {
                Console.WriteLine($"     {Colors.BrightWhite}{agent.Agent}:{Colors.Reset} ${agent.Cost:F4} ({agent.Tokens:N0} tokens)");
            }
        }
    }

    // ============================================================================
    // FILE LOGGING METHODS
    // ============================================================================

    /// <summary>
    /// Logs a message to a specific file.
    /// </summary>
    /// <param name="category">Log category.</param>
    /// <param name="message">Message to log.</param>
    /// <param name="logPath">Path to the log directory.</param>
    /// <param name="data">Optional data object to serialize.</param>
    private void LogToFile(string category, string message, string logPath, object? data = null)
    {
        try
        {
            lock (_fileLock)
            {
                var timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff");
                var fileName = $"{category}_{DateTime.Now:yyyy-MM-dd}.log";
                var fullPath = Path.Combine(logPath, fileName);

                // Safely serialize data, avoiding circular references
                string? serializedData = null;
                if (data != null)
                {
                    try
                    {
                        // Convert complex objects to string representation to avoid circular references
                        if (data is Exception ex)
                        {
                            serializedData = ex.ToString();
                        }
                        else if (data.GetType().FullName?.Contains("Microsoft.SemanticKernel") == true)
                        {
                            serializedData = data.ToString();
                        }
                        else
                        {
                            serializedData = JsonSerializer.Serialize(data, new JsonSerializerOptions 
                            { 
                                WriteIndented = true,
                                PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                                ReferenceHandler = System.Text.Json.Serialization.ReferenceHandler.IgnoreCycles,
                                MaxDepth = 32
                            });
                        }
                    }
                    catch
                    {
                        serializedData = data.ToString();
                    }
                }

                var logEntry = new
                {
                    Timestamp = timestamp,
                    SessionId = _sessionId,
                    Category = category,
                    Message = message,
                    Data = serializedData
                };

                var jsonLog = JsonSerializer.Serialize(logEntry, new JsonSerializerOptions 
                { 
                    WriteIndented = true,
                    PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                    ReferenceHandler = System.Text.Json.Serialization.ReferenceHandler.IgnoreCycles,
                    MaxDepth = 32
                });

                File.AppendAllText(fullPath, jsonLog + Environment.NewLine + Environment.NewLine);
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to write to log file: {LogPath}", logPath);
        }
    }

    /// <summary>
    /// Logs API call details to dedicated API call log files.
    /// </summary>
    /// <param name="apiCall">API call entry to log.</param>
    public void LogApiCallToFile(ApiCallEntry apiCall)
    {
        try
        {
            var callData = new
            {
                apiCall.CallId,
                apiCall.Timestamp,
                apiCall.Agent,
                apiCall.Method,
                apiCall.Endpoint,
                apiCall.Model,
                Request = apiCall.Request.Length > 1000 ? apiCall.Request.Substring(0, 1000) + "..." : apiCall.Request,
                Response = apiCall.Response.Length > 2000 ? apiCall.Response.Substring(0, 2000) + "..." : apiCall.Response,
                DurationMs = apiCall.Duration.TotalMilliseconds,
                apiCall.IsSuccess,
                apiCall.Error,
                apiCall.TokensUsed,
                apiCall.Cost
            };

            LogToFile($"API_CALL_{apiCall.Agent}", 
                     $"API Call #{apiCall.CallId} - {apiCall.Method} {apiCall.Endpoint}", 
                     _apiCallLogPath, 
                     callData);

            // Also create a live tracking file that's constantly updated
            var liveTrackingPath = Path.Combine(_apiCallLogPath, "live_api_calls.json");
            lock (_fileLock)
            {
                var allCalls = _apiCalls.Select(c => new
                {
                    c.CallId,
                    c.Timestamp,
                    c.Agent,
                    c.Method,
                    c.Model,
                    DurationMs = c.Duration.TotalMilliseconds,
                    c.IsSuccess,
                    c.TokensUsed,
                    c.Cost,
                    Status = c.IsSuccess ? "SUCCESS" : "ERROR"
                }).ToList();

                var liveData = new
                {
                    SessionId = _sessionId,
                    LastUpdated = DateTime.Now,
                    TotalCalls = allCalls.Count,
                    SuccessfulCalls = allCalls.Count(c => c.IsSuccess),
                    FailedCalls = allCalls.Count(c => !c.IsSuccess),
                    TotalTokens = allCalls.Sum(c => c.TokensUsed),
                    TotalCost = allCalls.Sum(c => c.Cost),
                    Calls = allCalls
                };

                File.WriteAllText(liveTrackingPath, JsonSerializer.Serialize(liveData, new JsonSerializerOptions 
                { 
                    WriteIndented = true,
                    PropertyNamingPolicy = JsonNamingPolicy.CamelCase
                }));
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to log API call to file");
        }
    }

    /// <summary>
    /// Logs migration process steps to dedicated migration log files.
    /// </summary>
    /// <param name="step">Migration step.</param>
    /// <param name="status">Step status.</param>
    /// <param name="details">Additional details.</param>
    /// <param name="data">Optional data object.</param>
    public void LogMigrationStepToFile(string step, string status, string details, object? data = null)
    {
        var stepData = new
        {
            Step = step,
            Status = status,
            Details = details,
            Data = data
        };

        LogToFile("MIGRATION_STEP", 
                 $"Step: {step} | Status: {status} | {details}", 
                 _migrationLogPath, 
                 stepData);

        // Update live migration progress
        var liveProgressPath = Path.Combine(_migrationLogPath, "live_migration_progress.json");
        try
        {
            lock (_fileLock)
            {
                var progressData = new
                {
                    SessionId = _sessionId,
                    LastUpdated = DateTime.Now,
                    CurrentStep = step,
                    Status = status,
                    Details = details
                };

                File.WriteAllText(liveProgressPath, JsonSerializer.Serialize(progressData, new JsonSerializerOptions 
                { 
                    WriteIndented = true,
                    PropertyNamingPolicy = JsonNamingPolicy.CamelCase
                }));
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to update live migration progress");
        }
    }

    /// <summary>
    /// Logs analysis results to dedicated analysis log files.
    /// </summary>
    /// <param name="fileName">File being analyzed.</param>
    /// <param name="analysisType">Type of analysis.</param>
    /// <param name="result">Analysis result.</param>
    /// <param name="duration">Analysis duration.</param>
    public void LogAnalysisToFile(string fileName, string analysisType, string result, TimeSpan duration)
    {
        var analysisData = new
        {
            FileName = fileName,
            AnalysisType = analysisType,
            Result = result.Length > 5000 ? result.Substring(0, 5000) + "..." : result,
            DurationMs = duration.TotalMilliseconds,
            ResultLength = result.Length
        };

        LogToFile($"ANALYSIS_{analysisType}", 
                 $"Analyzed {fileName} in {duration.TotalMilliseconds:F2}ms", 
                 _analysisLogPath, 
                 analysisData);
    }

    /// <summary>
    /// Logs performance metrics to dedicated performance log files.
    /// </summary>
    /// <param name="operation">Operation name.</param>
    /// <param name="duration">Operation duration.</param>
    /// <param name="itemsProcessed">Number of items processed.</param>
    /// <param name="memoryUsed">Memory usage in MB.</param>
    /// <param name="additionalMetrics">Additional performance metrics.</param>
    public void LogPerformanceToFile(string operation, TimeSpan duration, int itemsProcessed = 0, double memoryUsed = 0, Dictionary<string, object>? additionalMetrics = null)
    {
        var performanceData = new
        {
            Operation = operation,
            DurationMs = duration.TotalMilliseconds,
            ItemsProcessed = itemsProcessed,
            MemoryUsedMB = memoryUsed,
            Throughput = itemsProcessed > 0 ? itemsProcessed / duration.TotalSeconds : 0,
            AdditionalMetrics = additionalMetrics ?? new Dictionary<string, object>()
        };

        LogToFile("PERFORMANCE", 
                 $"Operation: {operation} | Duration: {duration.TotalMilliseconds:F2}ms | Items: {itemsProcessed}", 
                 _performanceLogPath, 
                 performanceData);

        // Update live performance dashboard
        var livePerfPath = Path.Combine(_performanceLogPath, "live_performance_dashboard.json");
        try
        {
            lock (_fileLock)
            {
                var dashboard = new
                {
                    SessionId = _sessionId,
                    LastUpdated = DateTime.Now,
                    CurrentOperation = operation,
                    Duration = duration.TotalMilliseconds,
                    ItemsProcessed = itemsProcessed,
                    Throughput = itemsProcessed > 0 ? itemsProcessed / duration.TotalSeconds : 0
                };

                File.WriteAllText(livePerfPath, JsonSerializer.Serialize(dashboard, new JsonSerializerOptions 
                { 
                    WriteIndented = true,
                    PropertyNamingPolicy = JsonNamingPolicy.CamelCase
                }));
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to update live performance dashboard");
        }
    }

    /// <summary>
    /// Creates a summary report of the entire migration session.
    /// </summary>
    public void CreateSessionSummary()
    {
        try
        {
            var summaryPath = Path.Combine(_baseLogPath, $"session_summary_{_sessionId}.json");
            
            var summary = new
            {
                SessionId = _sessionId,
                StartTime = _apiCalls.FirstOrDefault()?.Timestamp ?? DateTime.Now,
                EndTime = DateTime.Now,
                TotalDuration = _apiCalls.Any() ? 
                    DateTime.Now - (_apiCalls.FirstOrDefault()?.Timestamp ?? DateTime.Now) : 
                    TimeSpan.Zero,
                ApiCallSummary = new
                {
                    TotalCalls = _apiCalls.Count,
                    SuccessfulCalls = _apiCalls.Count(c => c.IsSuccess),
                    FailedCalls = _apiCalls.Count(c => !c.IsSuccess),
                    TotalTokens = _apiCalls.Sum(c => c.TokensUsed),
                    TotalCost = _apiCalls.Sum(c => c.Cost),
                    AverageDuration = _apiCalls.Any() ? _apiCalls.Average(c => c.Duration.TotalMilliseconds) : 0,
                    CallsByAgent = _apiCalls.GroupBy(c => c.Agent)
                        .ToDictionary(g => g.Key, g => new
                        {
                            Count = g.Count(),
                            TotalTokens = g.Sum(c => c.TokensUsed),
                            TotalCost = g.Sum(c => c.Cost),
                            AvgDuration = g.Average(c => c.Duration.TotalMilliseconds)
                        })
                }
            };

            File.WriteAllText(summaryPath, JsonSerializer.Serialize(summary, new JsonSerializerOptions 
            { 
                WriteIndented = true,
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase
            }));

            LogToFile("SESSION_END", $"Session summary created: {summaryPath}", _baseLogPath);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Failed to create session summary");
        }
    }
}

/// <summary>
/// Represents a conversation entry between the system and an AI agent.
/// </summary>
public class ConversationEntry
{
    /// <summary>
    /// Gets or sets the timestamp of the conversation.
    /// </summary>
    public DateTime Timestamp { get; set; }

    /// <summary>
    /// Gets or sets the name of the agent.
    /// </summary>
    public string AgentName { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the action being performed.
    /// </summary>
    public string Action { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the input to the agent.
    /// </summary>
    public string Input { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the output from the agent.
    /// </summary>
    public string Output { get; set; } = string.Empty;

    /// <summary>
    /// Gets or sets the duration of the operation.
    /// </summary>
    public TimeSpan Duration { get; set; }
}
