namespace McpChatWeb.Configuration;

public class McpOptions
{
    public string DotnetExecutable { get; set; } = "dotnet";

    /// <summary>
    /// Path to the compiled CobolToQuarkusMigration DLL. Assumes a prior dotnet build.
    /// </summary>
    public string AssemblyPath { get; set; } = string.Empty;

    public string ConfigPath { get; set; } = string.Empty;

    public int? RunId { get; set; }

    public string WorkingDirectory { get; set; } = string.Empty;
}
