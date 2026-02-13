# Microsoft Agent Framework Migration Plan

**From:** Semantic Kernel v1.22.0  
**To:** Microsoft Agent Framework (Preview)  
**Target Language:** .NET  
**Status:** 📋 Planned

**Reference:** [Official SK → AF Migration Guide](https://learn.microsoft.com/en-us/agent-framework/migration-guide/from-semantic-kernel/?pivots=programming-language-csharp)  
**Samples:** [SK Migration Samples (GitHub)](https://github.com/microsoft/semantic-kernel/tree/main/dotnet/samples/AgentFrameworkMigration)

---

## Executive Summary

This document outlines the migration plan from Semantic Kernel to Microsoft Agent Framework for the Legacy Modernization Agents project. The Agent Framework provides a more flexible, multi-agent oriented architecture with better orchestration patterns.

---

## Why Migrate?

### Current Limitations with Semantic Kernel
- Monolithic Kernel pattern requires manual orchestration
- Every agent depends on a `Kernel` instance (even if empty)
- Complex plugin registration with `[KernelFunction]` attributes
- Thread type must be known and created manually by caller
- Complex options setup with `KernelArguments` wrapping

### Agent Framework Benefits (from Official Guide)
- **Simplified API:** Reduced complexity and boilerplate code
- **Better Performance:** Optimized object creation and memory usage
- **Unified Interface:** Consistent patterns across different AI providers
- **Enhanced Developer Experience:** More intuitive and discoverable APIs
- **Agent Type Consolidation:** Single `ChatClientAgent` works with all services
- **Simplified Tool Registration:** No decorators/plugins required
- **Thread Abstraction:** Agent creates threads via `agent.GetNewThread()`

---

## Key API Changes (from Official Guide)

### 1. Namespace Updates
```csharp
// OLD: Semantic Kernel
using Microsoft.SemanticKernel;
using Microsoft.SemanticKernel.Agents;

// NEW: Agent Framework
using Microsoft.Extensions.AI;
using Microsoft.Agents.AI;
```

### 2. Agent Creation
```csharp
// OLD: Semantic Kernel - requires Kernel instance
Kernel kernel = Kernel.CreateBuilder()
    .AddAzureOpenAIChatCompletion(deployment, endpoint, apiKey)
    .Build();
ChatCompletionAgent agent = new() { Instructions = instructions, Kernel = kernel };

// NEW: Agent Framework - simplified
AIAgent agent = chatClient.CreateAIAgent(instructions: instructions);
// Or for Azure AI Foundry:
AIAgent agent = await persistentAgentsClient.CreateAIAgentAsync(instructions: instructions);
```

### 3. Thread Creation
```csharp
// OLD: Semantic Kernel - caller must know thread type
AgentThread thread = new OpenAIAssistantAgentThread(this.AssistantClient);

// NEW: Agent Framework - agent creates thread
AgentThread thread = agent.GetNewThread();
```

### 4. Tool/Function Registration
```csharp
// OLD: Semantic Kernel - requires [KernelFunction], Plugin, Kernel
[KernelFunction]
public static string GetWeather(string location) => ...;

KernelFunction function = KernelFunctionFactory.CreateFromMethod(GetWeather);
KernelPlugin plugin = KernelPluginFactory.CreateFromFunctions("Plugin", [function]);
kernel.Plugins.Add(plugin);
ChatCompletionAgent agent = new() { Kernel = kernel, ... };

// NEW: Agent Framework - direct registration
[Description("Get weather for location")]  // Optional
public static string GetWeather(string location) => ...;

AIAgent agent = chatClient.CreateAIAgent(tools: [AIFunctionFactory.Create(GetWeather)]);
```

### 5. Invocation Pattern
```csharp
// OLD: Semantic Kernel - IAsyncEnumerable for non-streaming
await foreach (var result in agent.InvokeAsync(userInput, thread, options))
{
    Console.WriteLine(result.Message);
}

// NEW: Agent Framework - single response object
AgentRunResponse response = await agent.RunAsync(userInput, thread);
Console.WriteLine(response.Text);
// response.Messages contains all messages (tool calls, function results, etc.)
```

### 6. Streaming Invocation
```csharp
// OLD: Semantic Kernel
await foreach (StreamingChatMessageContent update in agent.InvokeStreamingAsync(input, thread))
{
    Console.Write(update);
}

// NEW: Agent Framework
await foreach (AgentRunResponseUpdate update in agent.RunStreamingAsync(input, thread))
{
    Console.Write(update);  // ToString() friendly
}
```

### 7. Options Configuration
```csharp
// OLD: Semantic Kernel - complex wrapping
OpenAIPromptExecutionSettings settings = new() { MaxTokens = 1000 };
AgentInvokeOptions options = new() { KernelArguments = new(settings) };

// NEW: Agent Framework - simplified
ChatClientAgentRunOptions options = new(new() { MaxOutputTokens = 1000 });
```

### 8. Agent Type Consolidation
| Semantic Kernel | Agent Framework |
|----------------|-----------------|
| `ChatCompletionAgent` | `ChatClientAgent` |
| `OpenAIAssistantAgent` | `ChatClientAgent` |
| `AzureAIAgent` | `ChatClientAgent` |

Agent Framework uses a single `ChatClientAgent` type for all `IChatClient` implementations.

### 9. Incremental Migration with `.AsAIAgent()`
The SK repo samples show a **bridge pattern** for incremental migration:

```csharp
// STEP 1: Keep existing SK agent code
ChatCompletionAgent skAgent = new()
{
    Kernel = builder.Build(),
    Name = "Joker",
    Instructions = "You are good at telling jokes.",
};

// STEP 2: Convert to AF agent using bridge method
#pragma warning disable SKEXP0110
var afAgent = skAgent.AsAIAgent();
#pragma warning restore SKEXP0110

// STEP 3: Use AF patterns with the converted agent
var thread = afAgent.GetNewThread();
var result = await afAgent.RunAsync(userInput, thread, agentOptions);
```

This allows **gradual migration** without rewriting everything at once.

### 10. OpenAPI Plugin Conversion
For OpenAPI tools (if used):

```csharp
// Load OpenAPI spec and convert to AF tools
KernelPlugin plugin = await OpenApiKernelPluginFactory.CreateFromOpenApiAsync("github", "OpenAPISpec.json");

// Convert SK plugin functions to AF tools (requires dummy Kernel)
Kernel kernel = new();
List<AITool> tools = plugin.Select(x => x.WithKernel(kernel)).Cast<AITool>().ToList();

// Use tools in AF agent
AIAgent agent = chatClient.CreateAIAgent(instructions: "...", tools: tools);
```

---

## Current Architecture Analysis

### Files Using Semantic Kernel

| File | SK Features Used | Migration Complexity |
|------|------------------|---------------------|
| `Program.cs` | Kernel.CreateBuilder, AddAzureOpenAIChatCompletion | Medium |
| `Mcp/McpServer.cs` | Kernel, IChatCompletionService | Medium |
| `Agents/CobolAnalyzerAgent.cs` | IKernelBuilder, InvokePromptAsync, OpenAIPromptExecutionSettings | High |
| `Agents/JavaConverterAgent.cs` | IKernelBuilder, InvokePromptAsync, OpenAIPromptExecutionSettings | High |
| `Agents/CSharpConverterAgent.cs` | IKernelBuilder, InvokePromptAsync | High |
| `Agents/DependencyMapperAgent.cs` | IKernelBuilder, InvokePromptAsync | Medium |
| `Agents/BusinessLogicExtractorAgent.cs` | IKernelBuilder, InvokePromptAsync | Medium |
| `Agents/ChunkAwareJavaConverter.cs` | IKernelBuilder, InvokePromptAsync | High |
| `Agents/ChunkAwareCSharpConverter.cs` | IKernelBuilder, InvokePromptAsync | High |
| `Helpers/EnhancedLogger.cs` | Type checking for SK objects | Low |

### Agent Interfaces to Migrate

| Interface | Methods | Notes |
|-----------|---------|-------|
| `ICobolAnalyzerAgent` | AnalyzeCobolFileAsync, AnalyzeCobolFilesAsync | Core analysis agent |
| `ICodeConverterAgent` | ConvertToCodeAsync | Base converter interface |
| `IJavaConverterAgent` | ConvertToJavaAsync, ConvertToJavaQuarkusAsync | Java-specific conversion |
| `IDependencyMapperAgent` | MapDependenciesAsync | Dependency analysis |
| `IChunkAwareConverter` | ProcessChunkAsync | Chunked processing |
| `IUnitTestAgent` | GenerateTestsAsync | Test generation |

---

## Migration Phases

### Phase 1: Package Setup (Estimated: 1 day)
**Status:** 📋 Not Started

#### Tasks
- [ ] Add Agent Framework NuGet packages
  ```bash
  dotnet add package Microsoft.Agents.AI.AzureAI --prerelease
  dotnet add package Microsoft.Agents.AI.OpenAI --prerelease
  dotnet add package Microsoft.Agents.AI.Workflows --prerelease
  ```
- [ ] Keep Semantic Kernel temporarily for parallel operation
- [ ] Update .NET SDK if required
- [ ] Configure Azure credentials for Agent Framework

#### Acceptance Criteria
- All packages installed without conflicts
- Project builds successfully
- Existing functionality unaffected

---

### Phase 2: Chat Client Abstraction (Estimated: 2-3 days)
**Status:** 📋 Not Started

#### Goal
Create an abstraction layer that can work with both SK and AF chat clients during transition.

#### Migration Strategy Options

**Option A: Bridge Pattern (Recommended for Lower Risk)**
Use SK's built-in `.AsAIAgent()` method to convert existing agents:

```csharp
// Keep existing SK agent creation
var skAgent = new ChatCompletionAgent()
{
    Kernel = builder.Build(),
    Name = "CobolAnalyzer",
    Instructions = systemPrompt,
};

// Convert to AF agent using bridge
#pragma warning disable SKEXP0110
var afAgent = skAgent.AsAIAgent();
#pragma warning restore SKEXP0110

// Use new AF patterns
var thread = afAgent.GetNewThread();
var result = await afAgent.RunAsync(userPrompt, thread);
```

**Option B: Full Rewrite (Cleaner but Higher Effort)**
Create pure AF agents from scratch (shown in later sections).

#### Tasks
- [ ] Evaluate bridge pattern (`.AsAIAgent()`) vs full rewrite
- [ ] Create `IChatClientAdapter` interface if using Option B
- [ ] Implement adapter for selected approach
- [ ] Add configuration switch for client selection

#### Code Pattern: Before (Semantic Kernel)
```csharp
// Current pattern in CobolAnalyzerAgent
var kernelBuilder = Kernel.CreateBuilder();
kernelBuilder.AddAzureOpenAIChatCompletion(deployment, endpoint, apiKey);
var kernel = kernelBuilder.Build();

var executionSettings = new OpenAIPromptExecutionSettings
{
    ExtensionData = new Dictionary<string, object>
    {
        ["max_completion_tokens"] = maxOutputTokens
    }
};

var result = await kernel.InvokePromptAsync(fullPrompt, new KernelArguments(executionSettings));
return result.GetValue<string>() ?? string.Empty;
```

#### Code Pattern: After (Agent Framework)
```csharp
// New pattern using Agent Framework
using Microsoft.Extensions.AI;
using Microsoft.Agents.AI;
using Azure.Identity;

// Create chat client once
var chatClient = new AzureOpenAIChatClient(
    new DefaultAzureCredential(),
    new Uri(endpoint),
    deployment
);

// Create agent with instructions
var agent = chatClient.CreateAIAgent(
    name: "CobolAnalyzer",
    instructions: systemPrompt
);

// Run with options
var options = new ChatClientAgentRunOptions(new ChatOptions 
{ 
    MaxOutputTokens = maxOutputTokens 
});

var response = await agent.RunAsync(userPrompt, options: options);
return response.Text;

// For streaming (if needed):
await foreach (var update in agent.RunStreamingAsync(userPrompt))
{
    if (!string.IsNullOrEmpty(update.Text))
        yield return update.Text;
}
```

#### Acceptance Criteria
- Abstraction layer working with both backends
- Feature flag to switch between SK and AF
- No breaking changes to existing agents

---

### Phase 3: Agent Migration - CobolAnalyzerAgent (Estimated: 2 days)
**Status:** 📋 Not Started

#### Key Changes Required
Based on the official migration guide:

| Aspect | Current (SK) | Target (AF) |
|--------|--------------|-------------|
| Namespace | `Microsoft.SemanticKernel` | `Microsoft.Extensions.AI`, `Microsoft.Agents.AI` |
| Agent Creation | `Kernel.CreateBuilder()` → `Build()` | `chatClient.CreateAIAgent()` |
| Invocation | `kernel.InvokePromptAsync()` | `agent.RunAsync()` |
| Return Type | `FunctionResult.GetValue<string>()` | `AgentRunResponse.Text` |
| Settings | `OpenAIPromptExecutionSettings` | `ChatClientAgentRunOptions` |

#### Tasks
- [ ] Update using statements to AF namespaces
- [ ] Replace `IKernelBuilder` with `IChatClient` dependency
- [ ] Refactor `AnalyzeCobolFileAsync` to use `agent.RunAsync()`
- [ ] Update `AnalyzeCobolFilesAsync` for AF pattern
- [ ] Preserve rate limiting integration (adapt to AF)
- [ ] Preserve logging integration
- [ ] Update unit tests for new patterns
- [ ] Parallel testing with SK implementation

#### Acceptance Criteria
- Same analysis quality as SK version
- Rate limiting works correctly
- Logging integration preserved
- All tests pass

---

### Phase 4: Agent Migration - Converter Agents (Estimated: 3-4 days)
**Status:** 📋 Not Started

#### Agents to Migrate
1. `JavaConverterAgent`
2. `CSharpConverterAgent`
3. `ChunkAwareJavaConverter`
4. `ChunkAwareCSharpConverter`

#### Special Consideration: Thread Management for Chunks
The chunk-aware converters need to maintain context across multiple chunks. Agent Framework provides explicit thread management:

```csharp
// Create agent and thread once per file conversion
var agent = chatClient.CreateAIAgent(
    name: "JavaConverter",
    instructions: conversionInstructions
);

// Thread maintains conversation history across chunks
AgentThread thread = agent.GetNewThread();

foreach (var chunk in chunks)
{
    // Each chunk call adds to the conversation history
    var response = await agent.RunAsync(
        $"Convert chunk {chunk.Index}: {chunk.Content}",
        thread  // Same thread preserves context
    );
    
    // response.Messages contains full history if needed
}
```

#### Tasks
- [ ] Migrate JavaConverterAgent
- [ ] Migrate CSharpConverterAgent
- [ ] Migrate chunk-aware variants with thread persistence
- [ ] Ensure cross-chunk context preserved via AgentThread
- [ ] Update signature registry integration
- [ ] Update unit tests

#### Thread Cleanup Note
Per the official guide: Agent Framework doesn't have thread deletion in `AgentThread` since not all providers support it. If using hosted threads, track and delete via provider SDK:
```csharp
// Only if using OpenAI Assistants (deprecated)
await assistantClient.DeleteThreadAsync(thread.ConversationId);
```

---

### Phase 5: Agent Migration - Support Agents (Estimated: 2 days)
**Status:** 📋 Not Started

#### Agents to Migrate
1. `DependencyMapperAgent`
2. `BusinessLogicExtractorAgent`
3. `UnitTestAgent` (if exists)

#### Tasks
- [ ] Migrate each agent to AF pattern
- [ ] Update interfaces if needed
- [ ] Update unit tests

---

### Phase 6: MCP Server Migration (Estimated: 2-3 days)
**Status:** 📋 Not Started

#### Current Implementation
`McpServer.cs` uses Semantic Kernel for chat completion in the MCP protocol handler.

#### Target Implementation
Agent Framework with MCP tool integration:

```csharp
using ModelContextProtocol.Client;

var mcps = new List<McpClient>
{
    await McpClient.CreateAsync(new HttpClientTransport(
        new() { Name = "Legacy Migration", Endpoint = new Uri("...") }
    ))
};

var tools = new List<AITool>();
foreach (var mcp in mcps)
{
    var mcpTools = await mcp.ListToolsAsync();
    tools.AddRange(mcpTools);
}

var agent = await client.GetAIAgentAsync(
    agentId: agentId,
    new ChatOptions { Tools = tools }
);
```

#### Tasks
- [ ] Migrate McpServer to AF
- [ ] Integrate MCP tool discovery
- [ ] Update McpChatWeb integration
- [ ] Update portal tests

---

### Phase 7: Multi-Agent Orchestration (Estimated: 3-4 days)
**Status:** 📋 Not Started

#### Goal
Implement proper multi-agent orchestration for the migration pipeline.

#### Official Sample Categories
From [AgentOrchestrations samples](https://github.com/microsoft/semantic-kernel/tree/main/dotnet/samples/AgentFrameworkMigration/AgentOrchestrations):

| Pattern | SK Implementation | AF Implementation |
|---------|-------------------|-------------------|
| Concurrent | `ConcurrentOrchestration` | `ConcurrentBuilder` |
| Sequential | `SequentialOrchestration` | `SequentialBuilder` |
| Handoff | `HandoffOrchestration` | `HandoffBuilder` |

#### Example: Concurrent Translation Agents
```csharp
// Agent Framework concurrent workflow
using Microsoft.Agents.AI.Workflows;

var azureClient = new AzureOpenAIClient(new Uri(endpoint), new AzureCliCredential());

AIAgent GetTranslationAgent(string targetLanguage) =>
    azureClient.GetChatClient(deploymentName).CreateAIAgent(
        name: $"Translator_{targetLanguage}",
        instructions: $"Translate input to {targetLanguage}"
    );

var workflow = new ConcurrentBuilder()
    .Participants([
        GetTranslationAgent("French"),
        GetTranslationAgent("Spanish"),
        GetTranslationAgent("German")
    ])
    .Build();

await foreach (var event in workflow.RunStreamAsync(inputText))
{
    if (event is WorkflowOutputEvent output)
    {
        Console.WriteLine($"Translation: {output}");
    }
}
```

#### Example: Sequential Pipeline (Analysis → Conversion → Validation)
```csharp
var workflow = new SequentialBuilder()
    .Participants([
        analyzerAgent,     // Step 1: Analyze COBOL
        extractorAgent,    // Step 2: Extract business logic
        converterAgent,    // Step 3: Convert to Java/C#
        validatorAgent     // Step 4: Validate output
    ])
    .Build();

await foreach (var event in workflow.RunStreamAsync(cobolFile))
{
    // Each step output feeds to next step
}
```

#### Orchestration Patterns for This Project
1. **Concurrent:** Parallel chunk processing (already implemented manually)
2. **Sequential:** Analysis → Extraction → Conversion → Validation pipeline
3. **Handoff:** Future: Interactive code review with human-in-loop
```

#### Tasks
- [ ] Design orchestration pipeline
- [ ] Implement sequential workflow
- [ ] Integrate with chunking orchestrator
- [ ] Add progress reporting
- [ ] Update logging

---

### Phase 8: Cleanup & Semantic Kernel Removal (Estimated: 1-2 days)
**Status:** 📋 Not Started

#### Tasks
- [ ] Remove Semantic Kernel package references
- [ ] Remove SK-specific code
- [ ] Update doctor.sh to check for AF packages
- [ ] Update documentation
- [ ] Final testing pass

#### Package Removal
```xml
<!-- Remove from .csproj -->
<PackageReference Include="Microsoft.SemanticKernel" Version="1.22.0" />
```

---

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| AF Preview Instability | Medium | High | Maintain SK fallback during transition |
| Breaking API Changes | Medium | Medium | Pin package versions, test frequently |
| Performance Regression | Low | High | Benchmark before/after each phase |
| Token Budget Differences | Low | Medium | Validate chunking behavior |
| Rate Limit Handling | Medium | Medium | Adapt RateLimiter to AF patterns |

---

## Testing Strategy

### Unit Tests
- Each migrated agent needs updated tests
- Compare output quality between SK and AF
- Mock AF dependencies for isolation

### Integration Tests
- End-to-end migration tests
- Chunking behavior verification
- MCP protocol tests

### Performance Tests
- Token usage comparison
- Response time benchmarks
- Memory usage monitoring

---

## Rollback Plan

1. **Keep SK packages** until Phase 8 is complete
2. **Feature flag** to switch between SK and AF
3. **Branch strategy:** Each phase in separate branch
4. **Checkpoint:** Working state after each phase

---

## Timeline Estimate

| Phase | Duration | Dependencies |
|-------|----------|--------------|
| Phase 1: Package Setup | 1 day | None |
| Phase 2: Chat Abstraction | 2-3 days | Phase 1 |
| Phase 3: CobolAnalyzer | 2 days | Phase 2 |
| Phase 4: Converters | 3-4 days | Phase 3 |
| Phase 5: Support Agents | 2 days | Phase 3 |
| Phase 6: MCP Server | 2-3 days | Phase 4, 5 |
| Phase 7: Orchestration | 3-4 days | Phase 6 |
| Phase 8: Cleanup | 1-2 days | Phase 7 |

**Total Estimated Duration:** 16-21 working days

---

## References

### Official Documentation
- [Official SK → AF Migration Guide (.NET)](https://learn.microsoft.com/en-us/agent-framework/migration-guide/from-semantic-kernel/?pivots=programming-language-csharp)
- [Microsoft.Extensions.AI API](https://learn.microsoft.com/dotnet/api/microsoft.extensions.ai)
- [Microsoft Foundry Documentation](https://learn.microsoft.com/azure/ai-foundry/)

### Migration Samples (GitHub)
- [SK AgentFrameworkMigration Samples](https://github.com/microsoft/semantic-kernel/tree/main/dotnet/samples/AgentFrameworkMigration) - **Primary reference**
  - [AzureOpenAI](https://github.com/microsoft/semantic-kernel/tree/main/dotnet/samples/AgentFrameworkMigration/AzureOpenAI) - Direct Azure OpenAI API
  - [AzureAIFoundry](https://github.com/microsoft/semantic-kernel/tree/main/dotnet/samples/AgentFrameworkMigration/AzureAIFoundry) - Azure AI Foundry agents
  - [AgentOrchestrations](https://github.com/microsoft/semantic-kernel/tree/main/dotnet/samples/AgentFrameworkMigration/AgentOrchestrations) - Concurrent, Sequential, Handoff
- [Agent Framework GitHub](https://github.com/microsoft/agent-framework)
- [SK → AF Migration Samples (Python)](https://github.com/microsoft/agent-framework/tree/main/python/samples/semantic-kernel-migration)

### NuGet Packages
- [Microsoft.Agents.AI.* Packages](https://www.nuget.org/packages?q=Microsoft.Agents.AI)

---

## Migration Checklist (Per Agent)

Use this checklist when migrating each agent:

- [ ] **Namespaces:** Replace `Microsoft.SemanticKernel.*` with `Microsoft.Extensions.AI` and `Microsoft.Agents.AI`
- [ ] **Constructor:** Replace `IKernelBuilder` parameter with `IChatClient` or create client inline
- [ ] **Agent Creation:** Replace `Kernel.Build()` pattern with `chatClient.CreateAIAgent()`
- [ ] **Thread Management:** Use `agent.GetNewThread()` instead of manual thread type creation
- [ ] **Invocation:** Replace `kernel.InvokePromptAsync()` with `agent.RunAsync()`
- [ ] **Return Handling:** Replace `result.GetValue<string>()` with `response.Text`
- [ ] **Settings:** Replace `OpenAIPromptExecutionSettings` with `ChatClientAgentRunOptions`
- [ ] **Streaming:** Replace `InvokeStreamingAsync` with `RunStreamingAsync`
- [ ] **Tools/Functions:** Remove `[KernelFunction]` attributes, use `AIFunctionFactory.Create()` or `[Description]`
- [ ] **Tests:** Update mocks and assertions for new types

---

## Appendix: Package Reference

### Required Packages (.NET)

> **Important:** The `--prerelease` flag is required while Agent Framework is in preview.

```bash
# Core AI packages
dotnet add package Microsoft.Agents.AI.AzureAI --prerelease
dotnet add package Microsoft.Agents.AI.OpenAI --prerelease

# For multi-agent workflows
dotnet add package Microsoft.Agents.AI.Workflows --prerelease

# For Azure authentication
dotnet add package Azure.Identity
```

Or using version wildcard:
```bash
dotnet add package Microsoft.Agents.AI.AzureAI --version *-*
dotnet add package Microsoft.Agents.AI.OpenAI --version *-*
dotnet add package Microsoft.Agents.AI.Workflows --version *-*
```

### For Hosted Agents (Azure AI Foundry)
```bash
dotnet add package Azure.AI.Agents.Persistent --prerelease
```

### Optional Packages
```bash
# For MCP integration
dotnet add package ModelContextProtocol --prerelease
```

### Packages to Remove (After Migration)
```xml
<!-- Remove from .csproj after Phase 8 -->
<PackageReference Include="Microsoft.SemanticKernel" Version="1.22.0" />
```

---

*Document Version: 1.2*  
*Created: 2025-12-11*  
*Last Updated: 2025-12-11*  
*Based on: Official Microsoft SK → AF Migration Guide + SK GitHub Samples*
