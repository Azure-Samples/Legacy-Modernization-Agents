using System.Text;

namespace CobolToQuarkusMigration.Helpers;

/// <summary>
/// A TextWriter that captures console output and writes it to a live log file
/// that can be monitored by the portal's Live Run Log panel.
/// </summary>
public class LiveLogWriter : TextWriter
{
    private readonly TextWriter _originalOut;
    private readonly StreamWriter _logFile;
    private readonly object _lock = new();
    private static LiveLogWriter? _instance;
    
    public override Encoding Encoding => Encoding.UTF8;
    
    /// <summary>
    /// Initializes the live log writer and redirects console output.
    /// </summary>
    /// <param name="logFilePath">Path to the live log file (e.g., Logs/migration_run_latest.log)</param>
    public LiveLogWriter(string logFilePath)
    {
        _originalOut = Console.Out;
        
        // Ensure the directory exists
        var directory = Path.GetDirectoryName(logFilePath);
        if (!string.IsNullOrEmpty(directory))
        {
            Directory.CreateDirectory(directory);
        }
        
        // Create or truncate the log file
        _logFile = new StreamWriter(logFilePath, append: false, Encoding.UTF8)
        {
            AutoFlush = true
        };
        
        // Write header
        var header = $"[{DateTime.Now:yyyy-MM-dd HH:mm:ss}] === Live Migration Log Started ===";
        _logFile.WriteLine(header);
        _originalOut.WriteLine(header);
    }
    
    /// <summary>
    /// Starts capturing console output to the live log file.
    /// </summary>
    /// <param name="logsDirectory">Base logs directory (Logs folder)</param>
    /// <returns>The LiveLogWriter instance</returns>
    public static LiveLogWriter Start(string logsDirectory)
    {
        var logFilePath = Path.Combine(logsDirectory, "migration_run_latest.log");
        _instance = new LiveLogWriter(logFilePath);
        Console.SetOut(_instance);
        return _instance;
    }
    
    /// <summary>
    /// Stops capturing and restores the original console output.
    /// </summary>
    public static void Stop()
    {
        if (_instance != null)
        {
            Console.SetOut(_instance._originalOut);
            _instance.Dispose();
            _instance = null;
        }
    }
    
    public override void Write(char value)
    {
        lock (_lock)
        {
            _originalOut.Write(value);
            _logFile.Write(value);
        }
    }
    
    public override void Write(string? value)
    {
        if (value == null) return;
        lock (_lock)
        {
            _originalOut.Write(value);
            _logFile.Write(value);
        }
    }
    
    public override void WriteLine(string? value)
    {
        lock (_lock)
        {
            var timestampedLine = $"[{DateTime.Now:HH:mm:ss.fff}] {value}";
            _originalOut.WriteLine(value); // Original output without timestamp (already has ANSI colors etc.)
            _logFile.WriteLine(timestampedLine); // Log file gets timestamp for portal parsing
        }
    }
    
    public override void WriteLine()
    {
        lock (_lock)
        {
            _originalOut.WriteLine();
            _logFile.WriteLine();
        }
    }
    
    protected override void Dispose(bool disposing)
    {
        if (disposing)
        {
            lock (_lock)
            {
                try
                {
                    _logFile.WriteLine($"[{DateTime.Now:yyyy-MM-dd HH:mm:ss}] === Live Migration Log Ended ===");
                    _logFile.Flush();
                    _logFile.Dispose();
                }
                catch { /* Ignore disposal errors */ }
            }
        }
        base.Dispose(disposing);
    }
}
