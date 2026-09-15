**Last updated**: 2026-09-15

# Building behind an NPM registry block

`GitHub.Copilot.SDK` 1.x downloads the Copilot CLI binary from an NPM registry **at build time**. On a machine where the public registry is unreachable the build fails before any code is compiled:

```
error MSB3923: Failed to download file
"https://registry.npmjs.org/@github/copilot-darwin-arm64/-/copilot-darwin-arm64-1.0.57.tgz"
```

## The setting is not read from .npmrc

This is the part that costs time. The SDK resolves the registry from its own MSBuild property, `CopilotNpmRegistryUrl`, which defaults to the public registry. It never consults `npm config` or any `.npmrc`. A machine whose npm client is correctly pointed at an internal mirror will still fail this build, because nothing in the build path looks at npm's configuration.

## Option 1 — build against an internal registry mirror

Set the property to a mirror that proxies the public registry. As an environment variable, so nothing internal is committed to the repository:

```bash
export CopilotNpmRegistryUrl=https://<your-mirror>/npm
dotnet build
```

Or per invocation:

```bash
dotnet build -p:CopilotNpmRegistryUrl=https://<your-mirror>/npm
```

Microsoft-managed devices: the approved mirror is the one `npm config get registry` already reports. Use that value.

## Option 2 — build against an already-installed CLI

Where no registry is reachable at all, point the build at a Copilot CLI that is already on the machine. The download step is skipped entirely:

```bash
dotnet build -p:CopilotCliBinaryPath=/path/to/copilot
```

This is the option for air-gapped environments, and it is also faster, since nothing is fetched or extracted.

## Verifying

A successful build leaves the binary under `obj/`:

```
obj/Debug/net10.0/copilot-cli/<version>/<platform>/copilot
```

The expected CLI version is compiled into the assembly as metadata, so a mismatch between the CLI the SDK was built against and the one installed can be reported directly rather than surfacing as a protocol error during the handshake.
