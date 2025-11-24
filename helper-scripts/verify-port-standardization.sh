#!/bin/bash

# Port Standardization Verification Script
# Verifies that all port references have been updated to 5028

set -e

echo "🔍 Verifying Port Standardization to 5028"
echo "=========================================="
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track results
PASS=0
FAIL=0

# Exclude these directories and files from search
EXCLUDE_DIRS="--exclude-dir={bin,obj,node_modules,.git,Logs,Data,neo4j_data}"
EXCLUDE_FILES="--exclude=*.log --exclude=*.db --exclude=*.so --exclude=verify-port-standardization.sh"

echo "1. Checking for old port 5250 references..."
echo "   (Excluding documentation about the migration itself and runtime logs)"
if grep -r "5250" $EXCLUDE_DIRS $EXCLUDE_FILES . \
   | grep -v "PORT_STANDARDIZATION_SUMMARY.md" \
   | grep -v "CHANGELOG.md" \
   | grep -v "updated to port 5028" \
   | grep -v "from 5250" \
   | grep -v "5250 vs 5028" \
   | grep -v "tokenCount.*5250" \
   | grep -v "tokensUsed.*5250" \
   | grep -q .; then
    echo -e "${RED}❌ FAIL: Found old port 5250 references in source files:${NC}"
    grep -r "5250" $EXCLUDE_DIRS $EXCLUDE_FILES . \
       | grep -v "PORT_STANDARDIZATION_SUMMARY.md" \
       | grep -v "CHANGELOG.md" \
       | grep -v "updated to port 5028" \
       | grep -v "from 5250" \
       | grep -v "5250 vs 5028" \
       | grep -v "tokenCount.*5250" \
       | grep -v "tokensUsed.*5250"
    ((FAIL++))
else
    echo -e "${GREEN}✅ PASS: No old port 5250 references found in source files${NC}"
    ((PASS++))
fi
echo ""

echo "2. Verifying demo.sh uses port 5028..."
if grep -q "lsof -ti:5028" demo.sh; then
    echo -e "${GREEN}✅ PASS: demo.sh portal_running() checks port 5028${NC}"
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: demo.sh does not check port 5028${NC}"
    ((FAIL++))
fi
echo ""

echo "3. Verifying doctor.sh enforces port 5028..."
if grep -q "DEFAULT_MCP_PORT=5028" doctor.sh && \
   grep -q "export MCP_WEB_PORT=5028" doctor.sh && \
   grep -q "ASPNETCORE_URLS=\"http://localhost:5028\"" doctor.sh; then
    echo -e "${GREEN}✅ PASS: doctor.sh enforces port 5028${NC}"
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: doctor.sh does not properly enforce port 5028${NC}"
    ((FAIL++))
fi
echo ""

echo "4. Verifying devcontainer.json uses port 5028..."
if grep -q '"5028"' .devcontainer/devcontainer.json && \
   grep -q 'Migration Portal' .devcontainer/devcontainer.json; then
    echo -e "${GREEN}✅ PASS: devcontainer.json configured for port 5028${NC}"
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: devcontainer.json not properly configured${NC}"
    ((FAIL++))
fi
echo ""

echo "5. Checking documentation files..."
DOCS_TO_CHECK=("README.md" "DEMO.md" "QUERY_GUIDE.md" "McpChatWeb/wwwroot/index.html")
DOC_PASS=0
DOC_FAIL=0

for doc in "${DOCS_TO_CHECK[@]}"; do
    if [ -f "$doc" ]; then
        if grep -q "5028" "$doc"; then
            DOC_PASS=$((DOC_PASS + 1))
        else
            echo -e "${YELLOW}⚠️  WARNING: $doc does not reference port 5028${NC}"
            DOC_FAIL=$((DOC_FAIL + 1))
        fi
    fi
done

if [ $DOC_FAIL -eq 0 ]; then
    echo -e "${GREEN}✅ PASS: All documentation files reference port 5028${NC}"
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: Some documentation files missing port 5028 references${NC}"
    ((FAIL++))
fi
echo ""

echo "6. Verifying auto-start configuration..."
if grep -q "Data/migration.db" .devcontainer/devcontainer.json && \
   grep -q "dotnet run --urls http://localhost:5028" .devcontainer/devcontainer.json && \
   grep -q "Auto-starting portal" .devcontainer/devcontainer.json; then
    echo -e "${GREEN}✅ PASS: Auto-start configured correctly${NC}"
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: Auto-start not configured correctly${NC}"
    ((FAIL++))
fi
echo ""

echo "7. Checking for port enforcement in scripts..."
if grep -q "lsof -ti:5028" doctor.sh && \
   grep -q "pkill -f \"dotnet.*McpChatWeb\"" doctor.sh; then
    echo -e "${GREEN}✅ PASS: Port conflict resolution implemented${NC}"
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: Port conflict resolution not implemented${NC}"
    ((FAIL++))
fi
echo ""

echo "8. Verifying new documentation exists..."
NEW_DOCS=("DEVCONTAINER_AUTO_START.md" "PORT_STANDARDIZATION_SUMMARY.md")
NEW_DOC_PASS=0
NEW_DOC_FAIL=0

for doc in "${NEW_DOCS[@]}"; do
    if [ -f "$doc" ]; then
        echo -e "   ${GREEN}✓${NC} Found: $doc"
        NEW_DOC_PASS=$((NEW_DOC_PASS + 1))
    else
        echo -e "   ${RED}✗${NC} Missing: $doc"
        NEW_DOC_FAIL=$((NEW_DOC_FAIL + 1))
    fi
done

if [ $NEW_DOC_FAIL -eq 0 ]; then
    echo -e "${GREEN}✅ PASS: All new documentation files exist${NC}"
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: Some documentation files missing${NC}"
    ((FAIL++))
fi
echo ""

echo "9. Checking CHANGELOG.md updates..."
if grep -q "DevContainer Auto-Start" CHANGELOG.md && \
   grep -q "Locked Port Configuration" CHANGELOG.md && \
   grep -q "Enhanced Doctor.sh Auto-Fixing" CHANGELOG.md; then
    echo -e "${GREEN}✅ PASS: CHANGELOG.md properly updated${NC}"
    ((PASS++))
else
    echo -e "${RED}❌ FAIL: CHANGELOG.md missing required entries${NC}"
    ((FAIL++))
fi
echo ""

# Summary
echo "=========================================="
echo "Verification Summary"
echo "=========================================="
echo ""
echo -e "${GREEN}Passed: $PASS${NC}"
echo -e "${RED}Failed: $FAIL${NC}"
echo ""

if [ $FAIL -eq 0 ]; then
    echo -e "${GREEN}✅ ALL CHECKS PASSED!${NC}"
    echo ""
    echo "Port standardization to 5028 is complete."
    echo "Auto-start and port locking are properly configured."
    echo ""
    echo "Next steps:"
    echo "  1. Test in devcontainer: Open in VS Code → Reopen in Container"
    echo "  2. Run migration: ./doctor.sh run"
    echo "  3. Restart container to test auto-start"
    echo "  4. Verify portal opens at http://localhost:5028"
    exit 0
else
    echo -e "${RED}❌ SOME CHECKS FAILED!${NC}"
    echo ""
    echo "Please review the failures above and fix before proceeding."
    exit 1
fi
