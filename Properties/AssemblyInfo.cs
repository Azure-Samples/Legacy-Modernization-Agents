using System.Runtime.CompilerServices;

[assembly: InternalsVisibleTo("CobolToQuarkusMigration.Tests")]

// The portal reuses the CLI's source-identity helpers so artifacts resolve by identical
// rules; duplicating that logic in the web project would let the two drift apart.
[assembly: InternalsVisibleTo("McpChatWeb")]
