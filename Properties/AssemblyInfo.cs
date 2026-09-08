using System.Runtime.CompilerServices;

[assembly: InternalsVisibleTo("CobolToQuarkusMigration.Tests")]

// The portal reuses the source-identity helpers (SourcePathHelper,
// ProgramFactsArtifactLocator) so that REKT artifacts are resolved with exactly
// the same rules the CLI uses. Duplicating that logic in the web project would
// let the two drift apart.
[assembly: InternalsVisibleTo("McpChatWeb")]
