#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────────────────────────
# check-compile.sh — quality gate: does generated Java compile?
#
# Compiles output/java/**/*.java with javac against a minimal classpath that
# stubs Jakarta CDI annotations + java.util.logging. Writes a quality_metrics
# event to output/.metrics/<runId>.jsonl with:
#   {
#     "event": "quality_metrics",
#     "compileSuccess": true|false,
#     "compileErrors": <count>,
#     "compileWarnings": <count>,
#     "generatedClassCount": <count>,
#     "generatedJavaLines": <total>,
#     "fallbackClassCount": <count>,
#     "injectAnnotationCount": <count>
#   }
#
# This is the "first-class quality telemetry" required by the next-phase
# scaling plan (Priority 2 — compile-success quality gates).
#
# Usage:
#   tools/check-compile.sh                     # check output/java for latest run id
#   tools/check-compile.sh --run 28            # tag metrics with run id 28
#   tools/check-compile.sh --dir output/java   # check a specific tree
# ─────────────────────────────────────────────────────────────────────────────
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
JAVA_DIR="$REPO_ROOT/output/java"
RUN_ID=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dir) JAVA_DIR="$2"; shift 2 ;;
        --run) RUN_ID="$2"; shift 2 ;;
        --help|-h)
            grep -E "^#" "$0" | sed 's/^# \?//'
            exit 0
            ;;
        *) shift ;;
    esac
done

if [[ -z "$RUN_ID" ]]; then
    # Latest run id from migration.db.
    RUN_ID=$(sqlite3 "$REPO_ROOT/Data/migration.db" \
        "SELECT MAX(id) FROM runs WHERE status='Completed'" 2>/dev/null || echo "unknown")
    [[ -z "$RUN_ID" || "$RUN_ID" == "" ]] && RUN_ID="unknown"
fi

METRICS_DIR="$REPO_ROOT/output/.metrics"
METRICS_FILE="$METRICS_DIR/${RUN_ID}.jsonl"

if [[ ! -d "$JAVA_DIR" ]]; then
    echo "ERROR: directory not found: $JAVA_DIR" >&2
    exit 2
fi

if ! command -v javac >/dev/null 2>&1; then
    echo "ERROR: javac not on PATH — install OpenJDK first" >&2
    exit 2
fi

echo "═══════════════════════════════════════════════════════════════════════════"
echo "Compile-success quality gate"
echo "═══════════════════════════════════════════════════════════════════════════"
echo "  java dir:   $JAVA_DIR"
echo "  run id:     $RUN_ID"
echo "  metrics:    $METRICS_FILE"
echo ""

# Discover all generated .java files
JAVA_FILES=$(find "$JAVA_DIR" -name "*.java" -type f 2>/dev/null)
JAVA_COUNT=$(echo "$JAVA_FILES" | grep -c '\.java$' || echo 0)
[[ "$JAVA_COUNT" -eq 0 ]] && { echo "(no .java files found)"; exit 0; }

# Aggregate stats
TOTAL_LINES=$(find "$JAVA_DIR" -name "*.java" -type f -exec wc -l {} \; 2>/dev/null | awk '{s+=$1} END {print s+0}')
INJECT_COUNT=$(grep -rE "@Inject|@ApplicationScoped|@Singleton|@Autowired" "$JAVA_DIR" --include="*.java" 2>/dev/null | wc -l | tr -d ' ')
FALLBACK_COUNT=$(grep -lE "UnsupportedOperationException.*AI conversion unavailable|class .*Fallback " "$JAVA_DIR" --include="*.java" -r 2>/dev/null | wc -l | tr -d ' ')

echo "  generated files:    $JAVA_COUNT"
echo "  total lines:        $TOTAL_LINES"
echo "  @Inject markers:    $INJECT_COUNT"
echo "  fallback classes:   $FALLBACK_COUNT"
echo ""

# Stub Jakarta CDI annotations + jakarta.inject so javac can resolve imports
STUB_DIR=$(mktemp -d -t check-compile-stubs.XXXXXX)
trap 'rm -rf "$STUB_DIR"' EXIT

mkdir -p "$STUB_DIR/jakarta/enterprise/context" "$STUB_DIR/jakarta/inject" "$STUB_DIR/jakarta/persistence" "$STUB_DIR/jakarta/transaction" "$STUB_DIR/jakarta/ws/rs"

cat > "$STUB_DIR/jakarta/enterprise/context/ApplicationScoped.java" << 'STUB'
package jakarta.enterprise.context;
import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME) @Target({ElementType.TYPE, ElementType.METHOD, ElementType.FIELD})
public @interface ApplicationScoped {}
STUB

cat > "$STUB_DIR/jakarta/enterprise/context/Dependent.java" << 'STUB'
package jakarta.enterprise.context;
import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME) @Target({ElementType.TYPE, ElementType.METHOD, ElementType.FIELD})
public @interface Dependent {}
STUB

cat > "$STUB_DIR/jakarta/enterprise/context/RequestScoped.java" << 'STUB'
package jakarta.enterprise.context;
import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME) @Target({ElementType.TYPE})
public @interface RequestScoped {}
STUB

cat > "$STUB_DIR/jakarta/inject/Inject.java" << 'STUB'
package jakarta.inject;
import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME) @Target({ElementType.CONSTRUCTOR, ElementType.METHOD, ElementType.FIELD})
public @interface Inject {}
STUB

cat > "$STUB_DIR/jakarta/inject/Named.java" << 'STUB'
package jakarta.inject;
import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME) @Target({ElementType.TYPE, ElementType.METHOD, ElementType.FIELD})
public @interface Named { String value() default ""; }
STUB

cat > "$STUB_DIR/jakarta/inject/Singleton.java" << 'STUB'
package jakarta.inject;
import java.lang.annotation.*;
@Retention(RetentionPolicy.RUNTIME) @Target(ElementType.TYPE)
public @interface Singleton {}
STUB

# Compile stubs first (so javac can build .class for them)
STUB_CLASSES="$STUB_DIR/classes"
mkdir -p "$STUB_CLASSES"
find "$STUB_DIR" -name "*.java" -exec javac -d "$STUB_CLASSES" {} + 2>/dev/null

# Compile generated code against stubs
OUT_CLASSES=$(mktemp -d -t check-compile-out.XXXXXX)
trap 'rm -rf "$STUB_DIR" "$OUT_CLASSES"' EXIT

COMPILE_LOG=$(mktemp -t check-compile-log.XXXXXX)
# -nowarn keeps output focused; we count errors instead
# -proc:none disables annotation processing
# -Xlint:none silences nag warnings
set +e
echo "$JAVA_FILES" | xargs javac -d "$OUT_CLASSES" -cp "$STUB_CLASSES" \
    -proc:none -Xlint:none -nowarn 2> "$COMPILE_LOG"
COMPILE_EXIT=$?
set -e

set +e
COMPILE_ERRORS=$(grep -cE "error:" "$COMPILE_LOG" 2>/dev/null)
COMPILE_WARNINGS=$(grep -cE "warning:" "$COMPILE_LOG" 2>/dev/null)
set -e
[[ -z "$COMPILE_ERRORS" ]] && COMPILE_ERRORS=0
[[ -z "$COMPILE_WARNINGS" ]] && COMPILE_WARNINGS=0
COMPILE_SUCCESS="false"
[[ $COMPILE_EXIT -eq 0 ]] && COMPILE_SUCCESS="true"

echo "═══════════════════════════════════════════════════════════════════════════"
if [[ "$COMPILE_SUCCESS" == "true" ]]; then
    echo "  ✅ COMPILE SUCCESS"
else
    echo "  ❌ COMPILE FAILED  ($COMPILE_ERRORS errors, $COMPILE_WARNINGS warnings)"
    echo ""
    echo "  First 10 errors:"
    grep -E "error:" "$COMPILE_LOG" | head -10 | sed 's/^/    /'
fi
echo "═══════════════════════════════════════════════════════════════════════════"

# Emit MetricsSink event
mkdir -p "$METRICS_DIR"
TS=$(date -u +"%Y-%m-%dT%H:%M:%S.000Z")
cat >> "$METRICS_FILE" <<EOF
{"ts":"$TS","runId":"$RUN_ID","agent":"check-compile.sh","event":"quality_metrics","javaDir":"$JAVA_DIR","compileSuccess":$COMPILE_SUCCESS,"compileErrors":$COMPILE_ERRORS,"compileWarnings":$COMPILE_WARNINGS,"generatedClassCount":$JAVA_COUNT,"generatedJavaLines":$TOTAL_LINES,"fallbackClassCount":$FALLBACK_COUNT,"injectAnnotationCount":$INJECT_COUNT}
EOF

echo ""
echo "Metrics emitted to: $METRICS_FILE"

rm -f "$COMPILE_LOG"

[[ "$COMPILE_SUCCESS" == "true" ]] || exit 1
