#!/bin/bash

# Test MCP Server Communication
cd "/Users/gustav/funpark/cobol/persist and mcp/Legacy-Modernization-Agents"

echo "Starting MCP server..."
dotnet run -- mcp --run-id 43 --config Config/appsettings.json > /tmp/mcp_server.log 2>&1 &
MCP_PID=$!
echo "MCP PID: $MCP_PID"

sleep 3

echo "Sending initialize request..."
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}' | nc localhost 8080

sleep 2

echo "Killing MCP server..."
kill $MCP_PID 2>/dev/null

echo "Server log:"
cat /tmp/mcp_server.log
