using Microsoft.Extensions.Logging;
using System.Text;
using System.Text.Json;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// Logger for capturing full-length conversations between agents and Azure OpenAI
/// </summary>
public class ChatLogger
{
    private readonly ILogger<ChatLogger> _logger;
    private readonly string _logDirectory;
    private readonly List<ChatMessage> _messages;
    private readonly object _lockObject = new object();
    private readonly string _sessionId;

    public ChatLogger(ILogger<ChatLogger> logger, string logDirectory = "Logs")
    {
        _logger = logger;
        _logDirectory = logDirectory;
        _messages = new List<ChatMessage>();
        _sessionId = DateTime.Now.ToString("yyyy-MM-dd_HH-mm-ss");
        
        // Ensure log directory exists
        Directory.CreateDirectory(_logDirectory);
    }

    /// <summary>
    /// Logs a message sent to Azure OpenAI
    /// </summary>
    public void LogUserMessage(string agentName, string fileName, string prompt, string systemMessage = "")
    {
        lock (_lockObject)
        {
            var message = new ChatMessage
            {
                Timestamp = DateTime.UtcNow,
                AgentName = agentName,
                FileName = fileName,
                MessageType = "USER_TO_AI",
                SystemMessage = systemMessage,
                Content = prompt,
                TokenCount = EstimateTokens(prompt + systemMessage)
            };
            
            _messages.Add(message);
            _logger.LogInformation("Chat: {Agent} → Azure OpenAI for {File} ({Tokens} tokens)", 
                agentName, fileName, message.TokenCount);
        }
    }

    /// <summary>
    /// Logs a response received from Azure OpenAI
    /// </summary>
    public void LogAIResponse(string agentName, string fileName, string response, int actualTokens = 0)
    {
        lock (_lockObject)
        {
            var message = new ChatMessage
            {
                Timestamp = DateTime.UtcNow,
                AgentName = agentName,
                FileName = fileName,
                MessageType = "AI_TO_USER",
                Content = response,
                TokenCount = actualTokens > 0 ? actualTokens : EstimateTokens(response)
            };
            
            _messages.Add(message);
            _logger.LogInformation("Chat: Azure OpenAI → {Agent} for {File} ({Tokens} tokens)", 
                agentName, fileName, message.TokenCount);
        }
    }

    /// <summary>
    /// Saves the complete chat log as a readable conversation
    /// </summary>
    public async Task SaveChatLogAsync()
    {
        var fileName = $"FULL_CHAT_LOG_{_sessionId}.md";
        var filePath = Path.Combine(_logDirectory, fileName);
        
        var chatContent = GenerateReadableChatLog();
        await File.WriteAllTextAsync(filePath, chatContent);
        
        _logger.LogInformation("Complete chat log saved to: {FilePath}", filePath);
    }

    /// <summary>
    /// Saves the chat log in JSON format for programmatic access
    /// </summary>
    public async Task SaveChatLogJsonAsync()
    {
        var fileName = $"FULL_CHAT_LOG_{_sessionId}.json";
        var filePath = Path.Combine(_logDirectory, fileName);
        
        var options = new JsonSerializerOptions
        {
            WriteIndented = true,
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };
        
        var jsonContent = JsonSerializer.Serialize(_messages, options);
        await File.WriteAllTextAsync(filePath, jsonContent);
        
        _logger.LogInformation("Chat log JSON saved to: {FilePath}", filePath);
    }

    /// <summary>
    /// Generates a human-readable chat conversation format
    /// </summary>
    private string GenerateReadableChatLog()
    {
        var sb = new StringBuilder();
        
        // Header
        sb.AppendLine("# 🤖 Complete Azure OpenAI Chat Log");
        sb.AppendLine($"**Session ID:** {_sessionId}");
        sb.AppendLine($"**Generated:** {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss} UTC");
        sb.AppendLine($"**Total Messages:** {_messages.Count}");
        sb.AppendLine($"**Total Tokens:** {_messages.Sum(m => m.TokenCount):N0}");
        sb.AppendLine();
        sb.AppendLine("---");
        sb.AppendLine();

        // Group messages by file for better readability
        var messagesByFile = _messages.GroupBy(m => new { m.AgentName, m.FileName })
                                     .OrderBy(g => g.Key.FileName);

        foreach (var fileGroup in messagesByFile)
        {
            sb.AppendLine($"## 📁 File: {fileGroup.Key.FileName}");
            sb.AppendLine($"**Agent:** {fileGroup.Key.AgentName}");
            sb.AppendLine();

            var fileMessages = fileGroup.OrderBy(m => m.Timestamp).ToList();
            
            for (int i = 0; i < fileMessages.Count; i++)
            {
                var message = fileMessages[i];
                var messageNumber = i + 1;
                
                if (message.MessageType == "USER_TO_AI")
                {
                    sb.AppendLine($"### 👤 Human → AI (Message {messageNumber})");
                    sb.AppendLine($"**Time:** {message.Timestamp:HH:mm:ss}");
                    sb.AppendLine($"**Tokens:** {message.TokenCount:N0}");
                    sb.AppendLine();
                    
                    if (!string.IsNullOrEmpty(message.SystemMessage))
                    {
                        sb.AppendLine("**System Message:**");
                        sb.AppendLine("```");
                        sb.AppendLine(message.SystemMessage);
                        sb.AppendLine("```");
                        sb.AppendLine();
                    }
                    
                    sb.AppendLine("**User Prompt:**");
                    sb.AppendLine("```");
                    sb.AppendLine(message.Content);
                    sb.AppendLine("```");
                }
                else
                {
                    sb.AppendLine($"### 🤖 AI → Human (Response {messageNumber})");
                    sb.AppendLine($"**Time:** {message.Timestamp:HH:mm:ss}");
                    sb.AppendLine($"**Tokens:** {message.TokenCount:N0}");
                    sb.AppendLine();
                    sb.AppendLine("**AI Response:**");
                    sb.AppendLine("```");
                    sb.AppendLine(message.Content);
                    sb.AppendLine("```");
                }
                
                sb.AppendLine();
                sb.AppendLine("---");
                sb.AppendLine();
            }
        }

        // Summary
        sb.AppendLine("## 📊 Session Summary");
        sb.AppendLine();
        
        var agentStats = _messages.GroupBy(m => m.AgentName)
                                 .Select(g => new
                                 {
                                     Agent = g.Key,
                                     Messages = g.Count(),
                                     Tokens = g.Sum(m => m.TokenCount),
                                     Files = g.Select(m => m.FileName).Distinct().Count()
                                 })
                                 .OrderBy(a => a.Agent);

        sb.AppendLine("| Agent | Messages | Files Processed | Total Tokens |");
        sb.AppendLine("|-------|----------|-----------------|--------------|");
        
        foreach (var stat in agentStats)
        {
            sb.AppendLine($"| {stat.Agent} | {stat.Messages} | {stat.Files} | {stat.Tokens:N0} |");
        }
        
        sb.AppendLine();
        sb.AppendLine($"**Total Session Tokens:** {_messages.Sum(m => m.TokenCount):N0}");
        sb.AppendLine($"**Average Tokens per Message:** {(_messages.Count > 0 ? _messages.Average(m => m.TokenCount) : 0):F0}");
        sb.AppendLine($"**Session Duration:** {(_messages.Count > 0 ? _messages.Max(m => m.Timestamp) - _messages.Min(m => m.Timestamp) : TimeSpan.Zero):hh\\:mm\\:ss}");

        return sb.ToString();
    }

    /// <summary>
    /// Estimates token count for a text (rough calculation)
    /// </summary>
    private int EstimateTokens(string text)
    {
        if (string.IsNullOrEmpty(text)) return 0;
        
        // Rough estimate: 1 token ≈ 4 characters for English text
        // This is a simplified calculation - actual tokenization is more complex
        return text.Length / 4;
    }

    /// <summary>
    /// Gets real-time statistics
    /// </summary>
    public ChatStatistics GetStatistics()
    {
        lock (_lockObject)
        {
            return new ChatStatistics
            {
                TotalMessages = _messages.Count,
                TotalTokens = _messages.Sum(m => m.TokenCount),
                SessionDuration = _messages.Count > 0 ? 
                    _messages.Max(m => m.Timestamp) - _messages.Min(m => m.Timestamp) : 
                    TimeSpan.Zero,
                AgentBreakdown = _messages.GroupBy(m => m.AgentName)
                                         .ToDictionary(g => g.Key, g => g.Count())
            };
        }
    }
}

/// <summary>
/// Represents a single chat message in the conversation
/// </summary>
public class ChatMessage
{
    public DateTime Timestamp { get; set; }
    public string AgentName { get; set; } = string.Empty;
    public string FileName { get; set; } = string.Empty;
    public string MessageType { get; set; } = string.Empty; // "USER_TO_AI" or "AI_TO_USER"
    public string SystemMessage { get; set; } = string.Empty;
    public string Content { get; set; } = string.Empty;
    public int TokenCount { get; set; }
}

/// <summary>
/// Real-time chat statistics
/// </summary>
public class ChatStatistics
{
    public int TotalMessages { get; set; }
    public int TotalTokens { get; set; }
    public TimeSpan SessionDuration { get; set; }
    public Dictionary<string, int> AgentBreakdown { get; set; } = new();
}
