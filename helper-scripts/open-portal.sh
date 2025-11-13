#!/bin/bash

# Quick script to open the portal in browser
# Works in both local and dev container environments

echo "🌐 Opening COBOL Migration Portal..."
echo ""

# Check if portal is running
if ! lsof -ti:5028 >/dev/null 2>&1; then
    echo "❌ Portal is not running on port 5028"
    echo ""
    echo "To start the portal, run:"
    echo "   ./demo.sh"
    echo ""
    exit 1
fi

# Check if portal responds
HTTP_CODE=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:5028/ 2>/dev/null || echo "000")
if [ "$HTTP_CODE" != "200" ]; then
    echo "⚠️  Portal is running but not responding (HTTP $HTTP_CODE)"
    echo ""
    echo "Check logs: tail -f /tmp/cobol-portal.log"
    exit 1
fi

echo "✅ Portal is running and ready"
echo ""

# Try different methods to open the browser
OPENED=false

# Method 1: VS Code command (works in dev containers)
if [ -n "$VSCODE_GIT_IPC_HANDLE" ]; then
    echo "🚀 Opening in VS Code Simple Browser..."
    code --open-url http://localhost:5028 2>/dev/null && OPENED=true || true
fi

# Method 2: System default browser (works on Linux/Mac)
if [ "$OPENED" = false ] && command -v xdg-open >/dev/null 2>&1; then
    echo "🚀 Opening in system browser..."
    xdg-open http://localhost:5028 2>/dev/null && OPENED=true || true
fi

# Method 3: macOS
if [ "$OPENED" = false ] && command -v open >/dev/null 2>&1; then
    echo "🚀 Opening in system browser..."
    open http://localhost:5028 2>/dev/null && OPENED=true || true
fi

# Method 4: Environment variable
if [ "$OPENED" = false ] && [ -n "$BROWSER" ]; then
    echo "🚀 Opening with \$BROWSER..."
    $BROWSER http://localhost:5028 2>/dev/null && OPENED=true || true
fi

echo ""
if [ "$OPENED" = true ]; then
    echo "✅ Browser opened successfully"
else
    echo "💡 Couldn't auto-open browser. Manual options:"
    echo ""
    echo "   1. In VS Code:"
    echo "      - Go to the 'PORTS' tab (bottom panel)"
    echo "      - Find port 5028"
    echo "      - Click the globe icon (🌐) to open"
    echo ""
    echo "   2. Or paste this URL in your browser:"
    echo "      http://localhost:5028"
fi

echo ""
echo "📊 Portal Status:"
echo "   URL:     http://localhost:5028"
echo "   Status:  Running (HTTP 200)"
echo "   PID:     $(lsof -ti:5028)"
echo ""
