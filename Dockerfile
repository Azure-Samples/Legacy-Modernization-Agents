# Build stage
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
WORKDIR /src

# Copy solution and project files
COPY Legacy-Modernization-Agents.sln ./
COPY CobolToQuarkusMigration.csproj ./
COPY McpChatWeb/McpChatWeb.csproj McpChatWeb/
COPY CobolToQuarkusMigration.Tests/CobolToQuarkusMigration.Tests.csproj CobolToQuarkusMigration.Tests/
COPY McpChatWeb.Tests/McpChatWeb.Tests.csproj McpChatWeb.Tests/

# Restore dependencies
RUN dotnet restore Legacy-Modernization-Agents.sln

# Copy all source code
COPY . .

# Publish CLI tool
RUN dotnet publish CobolToQuarkusMigration.csproj -c Release -o /app/cli

# Publish Web Portal
RUN dotnet publish McpChatWeb/McpChatWeb.csproj -c Release -o /app/web

# Runtime stage
FROM mcr.microsoft.com/dotnet/aspnet:9.0
WORKDIR /app

# Install sqlite3 for debugging/direct DB access if needed by the app
RUN apt-get update && apt-get install -y sqlite3 && rm -rf /var/lib/apt/lists/*

# Copy artifacts
COPY --from=build /app/cli /app/cli
COPY --from=build /app/web /app/web
# Note: We don't copy Config/Data/Logs here because we mount them for persistence & live updates

# Setup environment variables
ENV ASPNETCORE_URLS=http://+:5028
ENV Mcp__DotnetExecutable=dotnet
ENV Mcp__AssemblyPath=/app/cli/CobolToQuarkusMigration.dll

# Expose port
EXPOSE 5028

# Set entrypoint
WORKDIR /app/web
ENTRYPOINT ["dotnet", "McpChatWeb.dll"]
