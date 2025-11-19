# Shell Script Fixes - November 19, 2025

## Problem Summary

Multiple shell scripts in the repository were failing to execute with the error:
```bash
bash: ./script.sh: cannot execute: required file not found
```

Despite the files existing and being readable, they could not be executed due to Windows-style line endings (CRLF) instead of Unix-style line endings (LF).

Additionally, Docker commands were failing with permission errors when attempting to access the Docker daemon socket.

## Root Causes

1. **Windows Line Endings (CRLF)**: Shell scripts had Windows-style line endings (`\r\n`) instead of Unix-style line endings (`\n`)
   - This caused the bash interpreter to fail with "required file not found" errors
   - The shebang line `#!/bin/bash` was being interpreted incorrectly due to the trailing `\r` character

2. **Docker Socket Permissions**: The Docker daemon socket had incorrect group ownership
   - Socket owned by `root:root` instead of `root:docker`
   - User was already in the `docker` group but couldn't access the socket

## Affected Files

### Shell Scripts Fixed
- `doctor.sh` - Main configuration and diagnostic tool
- `helper-scripts/demo.sh` - Demo mode portal launcher
- `helper-scripts/cleanup-databases.sh`
- `helper-scripts/open-portal.sh`
- `helper-scripts/status.sh`
- `helper-scripts/test_mcp_communication.sh`
- `helper-scripts/verify-data-persistence.sh`
- `helper-scripts/verify-port-standardization.sh`
- `Config/load-config.sh` - Configuration loader

### Configuration Files Fixed
- `Config/ai-config.env` - Base configuration template
- `Config/ai-config.local.env` - Local configuration (user-specific)
- `Config/ai-config.env.example` - Example configuration
- `Config/ai-config.local.env.template` - Template for local config

## Solutions Applied

### 1. Fixed Line Endings
Converted all shell scripts from Windows (CRLF) to Unix (LF) format:

```bash
# Find and fix all shell scripts recursively
find /workspaces/Legacy-Modernization-Agents -type f \( -name "*.sh" -o -name "load-config.sh" \) -exec sed -i 's/\r$//' {} \;
```

**What this does:**
- `find` - Searches for files
- `-type f` - Only files (not directories)
- `-name "*.sh"` - Match all .sh files
- `-name "load-config.sh"` - Also match the config loader

Also fixed Windows line endings in environment configuration files:

```bash
# Fix line endings in all .env configuration files
find /workspaces/Legacy-Modernization-Agents/Config -type f -name "*.env*" -exec sed -i 's/\r$//' {} \;
```

**Why this matters for .env files:**
- Environment variables with CRLF endings include `\r` (carriage return) in their values
- Example: `AZURE_OPENAI_SERVICE_TYPE="AzureOpenAI"` becomes `"AzureOpenAI"\r"` 
- This causes parsing errors like: `Unsupported AI service type: AzureOpenAI"`
- The trailing quote and carriage return are treated as part of the value
- `-exec sed -i 's/\r$//' {} \;` - Remove carriage return characters from end of lines

### 2. Made Scripts Executable
Added execute permissions to all shell scripts:

```bash
# Make all shell scripts executable
find /workspaces/Legacy-Modernization-Agents -type f \( -name "*.sh" \) -exec chmod +x {} \;
```

**What this does:**
- Adds execute permission (`+x`) to all `.sh` files
- Allows scripts to be run directly with `./script.sh`

### 3. Fixed Docker Permissions
Changed Docker socket group ownership:

```bash
# Change Docker socket group to docker
sudo chgrp docker /var/run/docker.sock
```

**What this does:**
- Changes the group owner of `/var/run/docker.sock` from `root` to `docker`
- Allows users in the `docker` group to access the Docker daemon
- Enables running Docker commands without `sudo`

## Verification

### Before Fixes

**Shell Scripts:**
```bash
vscode ➜ /workspaces/Legacy-Modernization-Agents (main) $ ./doctor.sh
bash: ./doctor.sh: cannot execute: required file not found

vscode ➜ /workspaces/Legacy-Modernization-Agents (main) $ ./helper-scripts/demo.sh
bash: ./helper-scripts/demo.sh: cannot execute: required file not found
```

**Docker:**
```bash
vscode ➜ /workspaces/Legacy-Modernization-Agents (main) $ docker ps
permission denied while trying to connect to the Docker daemon socket...
```

**Configuration Files:**
```bash
vscode ➜ /workspaces/Legacy-Modernization-Agents (main) $ ./doctor.sh run
fail: Program[0]
      Unsupported AI service type: AzureOpenAI"
```

### After Fixes

**Shell Scripts:**
```bash
vscode ➜ /workspaces/Legacy-Modernization-Agents (main) $ ./doctor.sh
🏥 Configuration Doctor - COBOL Migration Tool
==============================================
✅ Template configuration found: Config/ai-config.env
✅ Local configuration found: Config/ai-config.local.env
...
🎉 Configuration validation successful!
```

**Docker:**
```bash
vscode ➜ /workspaces/Legacy-Modernization-Agents (main) $ docker ps
CONTAINER ID   IMAGE     COMMAND   CREATED   STATUS   PORTS   NAMES
...
```

**Configuration Files:**
```bash
vscode ➜ /workspaces/Legacy-Modernization-Agents (main) $ ./doctor.sh run
info: Program[0]
      Loading settings from Config/appsettings.json
info: Program[0]
      Target Language: CSharp (from environment: CSharp)
✅ Configuration loaded successfully!

vscode ➜ /workspaces/Legacy-Modernization-Agents (main) $ ./helper-scripts/demo.sh
╔══════════════════════════════════════════════════════════════╗
║   COBOL Migration Portal - Demo Mode                        ║
║   (View existing data - No new analysis)                    ║
╚══════════════════════════════════════════════════════════════╝
🔍 Checking prerequisites...
✅ All prerequisites met
📊 Step 1: Starting Neo4j graph database...

vscode ➜ /workspaces/Legacy-Modernization-Agents (main) $ docker ps
CONTAINER ID   IMAGE     COMMAND   CREATED   STATUS   PORTS   NAMES
...
```

## Impact

✅ **All shell scripts now execute properly**
- `doctor.sh` - Configuration validation and setup
- `demo.sh` - Portal demonstration mode
- All helper scripts in `helper-scripts/` directory

✅ **Docker commands work without permission errors**
- Can run `docker ps`, `docker-compose up`, etc.
- Neo4j container can be started and managed
- Portal dependencies can be orchestrated

✅ **Configuration loading works correctly**
- `Config/load-config.sh` properly loads environment variables
- AI configuration is validated successfully

## Prevention

To prevent this issue in the future:

1. **Configure Git to handle line endings automatically:**
   ```bash
   git config --global core.autocrlf input
   ```

2. **Add `.gitattributes` file to repository:**
   ```
   # Force LF line endings for shell scripts
   *.sh text eol=lf
   ```

3. **Use Unix-based editors or configure Windows editors:**
   - VS Code: Set `"files.eol": "\n"` in settings
   - Notepad++: Edit → EOL Conversion → Unix (LF)

## Related Documentation

- [QUICK_START.md](QUICK_START.md) - Getting started guide
- [CONFIGURATION_GUIDE.md](CONFIGURATION_GUIDE.md) - Configuration setup
- [doctor.sh](doctor.sh) - Configuration diagnostic tool
- [helper-scripts/demo.sh](helper-scripts/demo.sh) - Demo mode launcher

## Commands Reference

### Useful Commands After Fixes

```bash
# Validate configuration
./doctor.sh

# Run demo mode (existing data)
./helper-scripts/demo.sh

# Check system status
./helper-scripts/status.sh

# Open portal
./helper-scripts/open-portal.sh

# Run migration
dotnet run
# or
./doctor.sh run

# Check Docker status
docker ps
docker-compose ps
```

## Technical Notes

### Why Line Endings Matter

Unix-based systems (Linux, macOS) use Line Feed (`\n`, ASCII 10) as line terminator.
Windows uses Carriage Return + Line Feed (`\r\n`, ASCII 13 + 10) as line terminator.

When a shell script with CRLF endings is executed on Linux:
- The shebang `#!/bin/bash\r` is interpreted with the trailing `\r`
- Bash looks for an interpreter at `/bin/bash\r` (with the carriage return as part of the path)
- This path doesn't exist, causing "required file not found" error
- Even though the script file itself exists and is readable

### Docker Socket Permissions

The Docker daemon listens on a Unix socket at `/var/run/docker.sock`.
By default, only `root` user can access this socket.
To allow non-root users:
- Add users to the `docker` group
- Ensure the socket has group ownership set to `docker`
- Group members get read/write access to the socket

## Conclusion

All shell scripts in the repository are now functional with proper Unix line endings and execute permissions. Docker integration works correctly with proper socket permissions. The development environment is fully operational for COBOL migration tasks.
