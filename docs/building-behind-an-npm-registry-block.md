**Last updated**: 2026-10-07

# Building behind an NPM registry block

`GitHub.Copilot.SDK` 1.x downloads the Copilot CLI binary from an NPM registry **at build time**. On a machine where the public registry is unreachable the build fails before any code is compiled:

```
error MSB3923: Failed to download file
"https://registry.npmjs.org/@github/copilot-darwin-arm64/-/copilot-darwin-arm64-1.0.57.tgz"
```

The same failure shows up during `./doctor.sh setup` as *Could not fetch user-specific models, falling back to CLI model list*, because the model list is fetched by building and running the CLI.

## doctor.sh handles it

Before its first build, `doctor.sh` checks whether the registry is reachable. If it is not, it picks the first of these that works and says which it used:

1. The registry `npm config get registry` reports, when that is a different, reachable mirror (sets `CopilotNpmRegistryUrl`).
2. The native binary of an installed Copilot CLI (sets `CopilotCliBinaryPath`).

If neither is available it prints the options below. Nothing is checked once a build has downloaded the CLI, or when either property is already set. Plain `dotnet build` and `dotnet run` commands need one of the options below.

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

The path must be the **native binary**, not the `copilot` command on your `PATH`. npm and Homebrew installs put a Node loader script there, and the SDK copies the file into the build output, where the script cannot run. The native binary sits next to it:

```bash
# macOS on Apple silicon; use darwin-x64, linux-x64, linux-arm64 or win32-x64 elsewhere
export CopilotCliBinaryPath="$(npm root -g)/@github/copilot/node_modules/@github/copilot-darwin-arm64/copilot"
```

An environment variable works for every `dotnet` command, because MSBuild reads environment variables as properties. The installed CLI can be newer than the version the SDK pins; the SDK talks to it the same way.

## Verifying

A successful download leaves the binary under `obj/`:

```
obj/Debug/net10.0/copilot-cli/<version>/<platform>/copilot
```

Either way, the build copies it to `bin/Debug/net10.0/runtimes/<rid>/native/`, which is where it runs from.

The expected CLI version is compiled into the assembly as metadata, so a mismatch between the CLI the SDK was built against and the one installed can be reported directly rather than surfacing as a protocol error during the handshake.
