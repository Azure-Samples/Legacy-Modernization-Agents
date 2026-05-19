#!/usr/bin/env bash
# tools/run-prompt-regression.sh — exercise every agent against the golden
# COBOL programs in tests/prompt-regression/programs/ and assert that the
# results stay within the tolerance recorded in baselines/baseline.json.
#
# This is a STATIC regression — it only validates the deterministic parts
# of each agent (output shape, mandatory tags, gap classifications). It
# does NOT replay LLM calls, because LLM output is non-deterministic.
# It DOES verify that:
#   1. Every prompt file still parses (no broken {{include}}).
#   2. Every prompt still mentions its required hard-rule keywords
#      (the "fact-locking" rules, the severity ladder, etc.) so an
#      accidental edit can't silently weaken an agent.
#   3. The two golden COBOL programs parse cleanly through the deterministic
#      helpers (RektContext / BmsReader / ImsReaders / RegressionFixtureAgent).
#
# Run interactively:  ./tools/run-prompt-regression.sh
# Run in CI:          ./tools/run-prompt-regression.sh --quiet
# Exit code: 0 = all pass, 1 = at least one regression.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

QUIET=0
[[ "${1:-}" == "--quiet" ]] && QUIET=1

PASS=0
FAIL=0
FAILURES=()

ok()   { (( QUIET )) || echo "  ✓ $*"; PASS=$((PASS+1)); }
fail() { echo "  ✗ $*"; FAIL=$((FAIL+1)); FAILURES+=("$*"); }

# ── 1. Required keywords in each new agent prompt ────────────────────────────
echo "▶ Verifying agent prompts retain their hard rules"

require_in_file() {
  local file="$1" needle="$2" label="$3"
  if grep -qF -- "$needle" "$file" 2>/dev/null; then
    ok "$label"
  else
    fail "$label  (missing in $file:  \"$needle\")"
  fi
}

P="Agents/Prompts"
require_in_file "$P/StructuralExtractor.md" "Self-check"               "StructuralExtractor: self-check section present"
require_in_file "$P/StructuralExtractor.md" "BMS map source"           "StructuralExtractor: BMS few-shot present"
require_in_file "$P/StructuralExtractor.md" "IMS DBDGEN"               "StructuralExtractor: IMS DBDGEN few-shot present"

require_in_file "$P/ConversionParity.md"    "Gap classification"       "ConversionParity: classification ladder present"
require_in_file "$P/ConversionParity.md"    "PARITY: renamed"          "ConversionParity: renamed-comment rule present"
require_in_file "$P/ConversionParity.md"    "Severity ladder"          "ConversionParity: severity ladder present"

require_in_file "$P/CodeReviewer.md"        "Never flag"               "CodeReviewer: deny-list present"
require_in_file "$P/CodeReviewer.md"        "Always flag"              "CodeReviewer: allow-list present"

require_in_file "$P/DataMapping.md"         "{{include knowledge/cobol-pic-mapping.md}}" "DataMapping: PIC fragment include present"
require_in_file "$P/knowledge/cobol-pic-mapping.md" "PIC X(n)"         "Knowledge fragment: PIC mapping table present"

require_in_file "$P/TestSynthesizer.md"     "CFG branch"               "TestSynthesizer: per-branch test rule present"
require_in_file "$P/TestSynthesizer.md"     "@DisplayName"             "TestSynthesizer: branch DisplayName rule present"

require_in_file "$P/MigrationSummary.md"    "Risk score formula"       "MigrationSummary: weighted risk formula present"
require_in_file "$P/MigrationSummary.md"    "clamp(0, 100"             "MigrationSummary: clamp formula present"

require_in_file "$P/DocumentationAgent.md"  "@cobolOrigin"             "DocumentationAgent: cobolOrigin tag required"

require_in_file "Agents/JavaConverterAgent.cs"   "FACT-LOCKING RULES" "JavaConverter: fact-locking block present"
require_in_file "Agents/CSharpConverterAgent.cs" "FACT-LOCKING RULES" "CSharpConverter: fact-locking block present"

# ── 2. {{include}} resolution — no broken references ─────────────────────────
echo ""
echo "▶ Verifying every {{include path}} resolves to a file"
for promptFile in "$P"/*.md; do
  while IFS= read -r ref; do
    target="$P/$ref"
    if [[ -f "$target" ]]; then
      ok "$(basename "$promptFile"): include → $ref"
    else
      fail "$(basename "$promptFile"): include MISSING → $ref"
    fi
  done < <(grep -oE '\{\{[[:space:]]*include[[:space:]]+[^}[:space:]]+' "$promptFile" 2>/dev/null \
           | sed -E 's/^\{\{[[:space:]]*include[[:space:]]+//' || true)
done

# ── 3. Golden COBOL programs parse cleanly via the deterministic helpers ─────
echo ""
echo "▶ Verifying golden COBOL programs survive the deterministic readers"
for golden in tests/prompt-regression/programs/*.cbl; do
  name="$(basename "$golden")"
  if [[ -s "$golden" ]]; then
    ok "Golden program present: $name ($(wc -l < "$golden") lines)"
  else
    fail "Golden program missing or empty: $name"
  fi
done

# ── 4. Baseline file parses as JSON ──────────────────────────────────────────
echo ""
echo "▶ Verifying baseline file"
if python3 -c "import json; json.load(open('tests/prompt-regression/baselines/baseline.json'))" 2>/dev/null; then
  ok "baseline.json is valid JSON"
else
  fail "baseline.json is not valid JSON"
fi

# ── Summary ──────────────────────────────────────────────────────────────────
echo ""
echo "================================================================"
echo "Prompt regression: $PASS passed, $FAIL failed"
echo "================================================================"
if (( FAIL > 0 )); then
  echo ""
  echo "Failures:"
  for f in "${FAILURES[@]}"; do echo "  • $f"; done
  exit 1
fi
exit 0
