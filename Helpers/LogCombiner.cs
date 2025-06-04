using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Globalization;

namespace CobolToQuarkusMigration.Helpers
{
    /// <summary>
    /// Combines all log files into a readable conversation format where agents appear to communicate their findings
    /// </summary>
    public class LogCombiner
    {
        private readonly string _logDirectory;
        private readonly string _outputDirectory;
        private readonly EnhancedLogger _logger;
        
        public LogCombiner(string logDirectory, EnhancedLogger logger)
        {
            _logDirectory = logDirectory;
            _outputDirectory = Path.Combine(logDirectory, "ConversationOutput");
            _logger = logger;
            
            // Ensure output directory exists
            Directory.CreateDirectory(_outputDirectory);
        }

        /// <summary>
        /// Combines all logs into a narrative conversation format
        /// </summary>
        public async Task<string> CreateConversationNarrativeAsync(string? sessionId = null)
        {
            _logger.LogBehindTheScenes("Starting log combination process...", "LOG_COMBINER", "CONVERSATION_GENERATION", new { SessionId = sessionId });
            
            var logEntries = await CollectAllLogEntriesAsync(sessionId);
            var sortedEntries = logEntries.OrderBy(e => e.Timestamp).ToList();
            
            var conversation = new StringBuilder();
            conversation.AppendLine("# 🤖 COBOL to Java Migration: Agent Conversation Log");
            conversation.AppendLine($"## Session: {sessionId ?? "Latest"}");
            conversation.AppendLine($"## Generated: {DateTime.Now:yyyy-MM-dd HH:mm:ss}");
            conversation.AppendLine();
            conversation.AppendLine("---");
            conversation.AppendLine();
            
            string currentAgent = "";
            
            foreach (var entry in sortedEntries)
            {
                switch (entry.Category.ToUpper())
                {
                    case "SESSION_START":
                        conversation.AppendLine("🎬 **MIGRATION SESSION STARTED**");
                        conversation.AppendLine($"*{entry.Timestamp:HH:mm:ss}* - System initializing enhanced logging...");
                        conversation.AppendLine();
                        break;
                        
                    case var c when c.Contains("ANALYZER"):
                        if (currentAgent != "CobolAnalyzer")
                        {
                            currentAgent = "CobolAnalyzer";
                            conversation.AppendLine("---");
                            conversation.AppendLine();
                            conversation.AppendLine("## 🔍 **COBOL ANALYZER AGENT** speaking:");
                            conversation.AppendLine();
                        }
                        AppendAgentMessage(conversation, entry, "📊");
                        break;
                        
                    case var c when c.Contains("CONVERTER") || c.Contains("JAVA"):
                        if (currentAgent != "JavaConverter")
                        {
                            currentAgent = "JavaConverter";
                            conversation.AppendLine("---");
                            conversation.AppendLine();
                            conversation.AppendLine("## ☕ **JAVA CONVERTER AGENT** speaking:");
                            conversation.AppendLine();
                        }
                        AppendAgentMessage(conversation, entry, "⚡");
                        break;
                        
                    case var c when c.Contains("DEPENDENCY"):
                        if (currentAgent != "DependencyMapper")
                        {
                            currentAgent = "DependencyMapper";
                            conversation.AppendLine("---");
                            conversation.AppendLine();
                            conversation.AppendLine("## 🗺️ **DEPENDENCY MAPPER AGENT** speaking:");
                            conversation.AppendLine();
                        }
                        AppendAgentMessage(conversation, entry, "🔗");
                        break;
                        
                    case var c when c.Contains("MIGRATION"):
                        if (currentAgent != "MigrationOrchestrator")
                        {
                            currentAgent = "MigrationOrchestrator";
                            conversation.AppendLine("---");
                            conversation.AppendLine();
                            conversation.AppendLine("## 🎯 **MIGRATION ORCHESTRATOR** speaking:");
                            conversation.AppendLine();
                        }
                        AppendAgentMessage(conversation, entry, "🚀");
                        break;
                        
                    case var c when c.Contains("API_CALL"):
                        AppendApiCallNarrative(conversation, entry);
                        break;
                        
                    case var c when c.Contains("PERFORMANCE"):
                        AppendPerformanceInsight(conversation, entry);
                        break;
                        
                    case var c when c.Contains("ERROR"):
                        conversation.AppendLine($"❌ **ERROR at {entry.Timestamp:HH:mm:ss}**: {entry.Message}");
                        conversation.AppendLine();
                        break;
                }
            }
            
            // Add session summary
            conversation.AppendLine("---");
            conversation.AppendLine();
            conversation.AppendLine("## 📈 **SESSION SUMMARY**");
            conversation.AppendLine();
            AppendSessionSummary(conversation, sortedEntries);
            
            var outputPath = Path.Combine(_outputDirectory, $"migration_conversation_{sessionId ?? DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss")}.md");
            await File.WriteAllTextAsync(outputPath, conversation.ToString());
            
            _logger.LogBehindTheScenes($"Conversation narrative created: {outputPath}", "LOG_COMBINER", "CONVERSATION_COMPLETE", new { OutputPath = outputPath });
            
            return outputPath;
        }

        /// <summary>
        /// Collects all log entries from various log files
        /// </summary>
        private async Task<List<LogEntry>> CollectAllLogEntriesAsync(string? sessionId)
        {
            var entries = new List<LogEntry>();
            
            // Collect from session start log
            await CollectFromFileAsync(entries, Path.Combine(_logDirectory, "SESSION_START*.log"));
            
            // Collect from behind-the-scenes logs
            await CollectFromFileAsync(entries, Path.Combine(_logDirectory, "BEHIND_SCENES_*.log"));
            
            // Collect from API call logs
            var apiCallDir = Path.Combine(_logDirectory, "ApiCalls");
            if (Directory.Exists(apiCallDir))
            {
                await CollectFromDirectoryAsync(entries, apiCallDir, "*.log");
            }
            
            // Collect from migration logs
            var migrationDir = Path.Combine(_logDirectory, "Migration");
            if (Directory.Exists(migrationDir))
            {
                await CollectFromDirectoryAsync(entries, migrationDir, "*.log");
            }
            
            // Collect from analysis logs
            var analysisDir = Path.Combine(_logDirectory, "Analysis");
            if (Directory.Exists(analysisDir))
            {
                await CollectFromDirectoryAsync(entries, analysisDir, "*.log");
            }
            
            // Collect from performance logs
            var performanceDir = Path.Combine(_logDirectory, "Performance");
            if (Directory.Exists(performanceDir))
            {
                await CollectFromDirectoryAsync(entries, performanceDir, "*.log");
            }
            
            return entries;
        }

        /// <summary>
        /// Collects log entries from a specific file pattern
        /// </summary>
        private async Task CollectFromFileAsync(List<LogEntry> entries, string filePattern)
        {
            var directory = Path.GetDirectoryName(filePattern);
            var pattern = Path.GetFileName(filePattern);
            
            if (!Directory.Exists(directory)) return;
            
            var files = Directory.GetFiles(directory, pattern);
            
            foreach (var file in files)
            {
                try
                {
                    var content = await File.ReadAllTextAsync(file);
                    var lines = content.Split('\n', StringSplitOptions.RemoveEmptyEntries);
                    
                    foreach (var line in lines)
                    {                    if (TryParseLogEntry(line, out var entry) && entry != null)
                    {
                        entries.Add(entry);
                    }
                    }
                }
                catch (Exception ex)
                {
                    _logger.LogBehindTheScenes($"Error reading file {file}: {ex.Message}", "LOG_COMBINER", "FILE_READ_ERROR", new { FilePath = file, Error = ex.Message });
                }
            }
        }

        /// <summary>
        /// Collects log entries from all files in a directory
        /// </summary>
        private async Task CollectFromDirectoryAsync(List<LogEntry> entries, string directory, string pattern)
        {
            if (!Directory.Exists(directory)) return;
            
            var files = Directory.GetFiles(directory, pattern);
            
            foreach (var file in files)
            {
                await CollectFromFileAsync(entries, file);
            }
        }

        /// <summary>
        /// Tries to parse a JSON log entry
        /// </summary>
        private bool TryParseLogEntry(string jsonLine, out LogEntry? entry)
        {
            entry = null;
            try
            {
                using var doc = JsonDocument.Parse(jsonLine);
                var root = doc.RootElement;
                
                entry = new LogEntry
                {
                    Timestamp = DateTime.Parse(root.GetProperty("timestamp").GetString() ?? ""),
                    Category = root.GetProperty("category").GetString() ?? "",
                    Message = root.GetProperty("message").GetString() ?? "",
                    Data = root.TryGetProperty("data", out var dataElement) && !dataElement.ValueKind.Equals(JsonValueKind.Null) 
                        ? dataElement.GetRawText() 
                        : null
                };
                
                return true;
            }
            catch
            {
                return false;
            }
        }

        /// <summary>
        /// Appends an agent message in conversational format
        /// </summary>
        private void AppendAgentMessage(StringBuilder conversation, LogEntry entry, string emoji)
        {
            conversation.AppendLine($"{emoji} *{entry.Timestamp:HH:mm:ss}*: \"{entry.Message}\"");
            
            if (!string.IsNullOrEmpty(entry.Data))
            {
                try
                {
                    using var doc = JsonDocument.Parse(entry.Data);
                    var root = doc.RootElement;
                    
                    // Extract meaningful information from the data
                    if (root.TryGetProperty("fileName", out var fileName))
                    {
                        conversation.AppendLine($"   📁 Working on file: `{fileName.GetString()}`");
                    }
                    
                    if (root.TryGetProperty("linesOfCode", out var loc))
                    {
                        conversation.AppendLine($"   📏 Lines of code: {loc.GetInt32()}");
                    }
                    
                    if (root.TryGetProperty("stepName", out var stepName))
                    {
                        conversation.AppendLine($"   🎯 Current step: {stepName.GetString()}");
                    }
                    
                    if (root.TryGetProperty("status", out var status))
                    {
                        var statusEmoji = status.GetString()?.ToLower() switch
                        {
                            "completed" => "✅",
                            "in_progress" => "⏳",
                            "error" => "❌",
                            _ => "ℹ️"
                        };
                        conversation.AppendLine($"   {statusEmoji} Status: {status.GetString()}");
                    }
                }
                catch
                {
                    // If data parsing fails, just show the raw message
                }
            }
            
            conversation.AppendLine();
        }

        /// <summary>
        /// Appends API call information in narrative format
        /// </summary>
        private void AppendApiCallNarrative(StringBuilder conversation, LogEntry entry)
        {
            if (string.IsNullOrEmpty(entry.Data)) return;
            
            try
            {
                using var doc = JsonDocument.Parse(entry.Data);
                var root = doc.RootElement;
                
                if (root.TryGetProperty("agent", out var agent) && 
                    root.TryGetProperty("durationMs", out var duration) &&
                    root.TryGetProperty("tokensUsed", out var tokens))
                {
                    var agentName = agent.GetString();
                    var durationSec = Math.Round(duration.GetDouble() / 1000, 1);
                    var tokenCount = tokens.GetInt32();
                    
                    conversation.AppendLine($"   🧠 *Thinking...*: Called AI service ({durationSec}s, {tokenCount} tokens)");
                    
                    if (root.TryGetProperty("request", out var request))
                    {
                        var requestText = request.GetString() ?? "";
                        if (requestText.Length > 100)
                        {
                            requestText = requestText.Substring(0, 97) + "...";
                        }
                        conversation.AppendLine($"   💭 Question: \"{requestText}\"");
                    }
                    
                    if (root.TryGetProperty("response", out var response))
                    {
                        var responseText = response.GetString() ?? "";
                        if (responseText.Length > 200)
                        {
                            responseText = responseText.Substring(0, 197) + "...";
                        }
                        conversation.AppendLine($"   💡 AI Response: \"{responseText}\"");
                    }
                    
                    conversation.AppendLine();
                }
            }
            catch
            {
                // If parsing fails, skip this entry
            }
        }

        /// <summary>
        /// Appends performance insight in narrative format
        /// </summary>
        private void AppendPerformanceInsight(StringBuilder conversation, LogEntry entry)
        {
            conversation.AppendLine($"📊 *Performance Insight at {entry.Timestamp:HH:mm:ss}*: {entry.Message}");
            conversation.AppendLine();
        }

        /// <summary>
        /// Appends session summary at the end
        /// </summary>
        private void AppendSessionSummary(StringBuilder conversation, List<LogEntry> entries)
        {
            var sessionStart = entries.FirstOrDefault(e => e.Category.Contains("SESSION_START"))?.Timestamp;
            var sessionEnd = entries.LastOrDefault()?.Timestamp;
            
            if (sessionStart.HasValue && sessionEnd.HasValue)
            {
                var duration = sessionEnd.Value - sessionStart.Value;
                conversation.AppendLine($"⏱️ **Session Duration**: {duration.TotalMinutes:F1} minutes");
            }
            
            var apiCalls = entries.Count(e => e.Category.Contains("API_CALL"));
            conversation.AppendLine($"🔧 **Total AI Calls**: {apiCalls}");
            
            var analyzedFiles = entries.Count(e => e.Category.Contains("ANALYZER"));
            conversation.AppendLine($"📁 **Files Analyzed**: {analyzedFiles}");
            
            var migrationSteps = entries.Count(e => e.Category.Contains("MIGRATION"));
            conversation.AppendLine($"🎯 **Migration Steps**: {migrationSteps}");
            
            var errors = entries.Count(e => e.Category.Contains("ERROR"));
            if (errors > 0)
            {
                conversation.AppendLine($"❌ **Errors Encountered**: {errors}");
            }
            else
            {
                conversation.AppendLine("✅ **Session Completed Successfully!**");
            }
            
            conversation.AppendLine();
            conversation.AppendLine("*End of conversation log*");
        }

        /// <summary>
        /// Creates a live conversation feed that updates in real-time
        /// </summary>
        public async Task<string> CreateLiveConversationFeedAsync()
        {
            var outputPath = Path.Combine(_outputDirectory, "live_conversation.md");
            
            // Create initial conversation
            await CreateConversationNarrativeAsync();
            
            // Set up file watcher for live updates
            var watcher = new FileSystemWatcher(_logDirectory, "*.log")
            {
                IncludeSubdirectories = true,
                NotifyFilter = NotifyFilters.LastWrite | NotifyFilters.CreationTime
            };
            
            watcher.Changed += async (sender, e) =>
            {
                await Task.Delay(1000); // Debounce
                await CreateConversationNarrativeAsync();
            };
            
            watcher.EnableRaisingEvents = true;
            
            return outputPath;
        }
    }

    /// <summary>
    /// Represents a parsed log entry
    /// </summary>
    public class LogEntry
    {
        public DateTime Timestamp { get; set; }
        public string Category { get; set; } = string.Empty;
        public string Message { get; set; } = string.Empty;
        public string? Data { get; set; }
    }
}
