#!/bin/bash
# Normalizes COBOL sources for rekt compatibility.

set -euo pipefail

SOURCE_DIR="${1:?Usage: preprocess-for-rekt.sh <source-dir>}"
SOURCE_DIR="$(cd -- "$SOURCE_DIR" && pwd)"
PREPROC_DIR="${SOURCE_DIR}/.preprocessed"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Each estate (a top-level folder of source/) is preprocessed into its own .preprocessed/, so
# one estate's copybooks never shadow or rewrite another's. Loose files at the source/ root
# form their own estate and set REKT_SOURCE_TOP_LEVEL_ONLY=true to leave the folders alone.
FIND_DEPTH=()
[[ "${REKT_SOURCE_TOP_LEVEL_ONLY:-false}" == "true" ]] && FIND_DEPTH=(-maxdepth 1)

mkdir -p "$PREPROC_DIR"

# Preprocessing, evidence, platform contracts and BMS maps run in the migration CLI. doctor.sh exports the assembly it
# already built; run standalone, the script builds it quietly once.
LMA_CLI_ASSEMBLY="${_LMA_CLI_ASSEMBLY:-${MIGRATION_CLI_ASSEMBLY:-}}"
if [[ -z "$LMA_CLI_ASSEMBLY" ]]; then
    REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
    if ! (cd "$REPO_ROOT" && "${DOTNET_CMD:-dotnet}" build CobolToQuarkusMigration.csproj -v q -nologo >/dev/null 2>&1); then
        echo "  ⚠️  Build failed. Run: dotnet build CobolToQuarkusMigration.csproj" >&2
        exit 1
    fi
    LMA_CLI_ASSEMBLY="$(cd "$REPO_ROOT" && "${DOTNET_CMD:-dotnet}" build CobolToQuarkusMigration.csproj -nologo -getProperty:TargetPath 2>/dev/null | tr -d '\r')"
fi
if [[ ! -f "$LMA_CLI_ASSEMBLY" ]]; then
    echo "  ⚠️  Migration CLI assembly not found: ${LMA_CLI_ASSEMBLY:-unresolved}" >&2
    exit 1
fi
# shellcheck source=lib/lma-cli.sh
source "$SCRIPT_DIR/lib/lma-cli.sh"
export LMA_HELPER_LOG="${LMA_HELPER_LOG:-$(cd "$SCRIPT_DIR/.." && pwd)/Logs/doctor-helpers.log}"
lma_cli() {
    lma_cli_run "$LMA_CLI_ASSEMBLY" "$@"
}

# Old shadows cannot override changed originals, including files no longer needing a rewrite.
# Preserve them for diagnosis, and generate this run's outputs only from current inputs.
lma_cli rekt-evidence reset-preprocessed --output-root "$PREPROC_DIR"

# Preprocess copybooks before programs.
cpy_count=0
# Recursive find so nested layouts (source/lib/cpy/, etc.) are picked up.
# -print0 / read -d '' avoids issues with paths containing whitespace.
while IFS= read -r -d '' cpy; do
    if ! lma_cli rekt-evidence validate-definition --source "$cpy"; then
        echo "  Ignoring placeholder or unverified definition; genuine source required: $cpy"
        continue
    fi
    # doctor.sh lists generated copybooks that a real one of the same name replaces.
    if [[ -n "${REKT_SHADOWED_COPYBOOKS_FILE:-}" && -f "$REKT_SHADOWED_COPYBOOKS_FILE" ]] \
        && grep -qxF "$cpy" "$REKT_SHADOWED_COPYBOOKS_FILE"; then
        continue
    fi
    if lma_cli rekt-preprocess copybook --source "$cpy" --output-root "$PREPROC_DIR" 2>/dev/null; then
        lma_cli rekt-evidence provenance \
            --source "$cpy" --target "$PREPROC_DIR/$(basename "$cpy")"
        cpy_count=$((cpy_count + 1))
    fi
done < <(find "$SOURCE_DIR" ${FIND_DEPTH[@]+"${FIND_DEPTH[@]}"} \
    \( -name "*.cpy" -o -name "*.CPY" \) \
    -type f \
    ! -path "*/.rekt-staging/*" \
    ! -path "*/.preprocessed/*" \
    ! -path "*/.convert-*/*" \
    ! -path "*/.convert-*/*" \
    -print0)

# Messages show paths relative to the caller's folder; absolute paths bury the useful part.
display_path() {
    local from="/tools/../" to="/"
    local path="${1//"$from"/$to}"
    printf '%s' "${path#"$PWD"/}"
}

# Optional estate-specific rewrites (git-ignored). Validated once so a malformed file
# is reported instead of silently skipping every program below.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REKT_PREPROCESS_RULES="${REKT_PREPROCESS_RULES:-$SCRIPT_DIR/../Config/rekt-preprocess.local.json}"
if [[ -f "$REKT_PREPROCESS_RULES" ]]; then
    if lma_cli rekt-preprocess check-rules --rules "$REKT_PREPROCESS_RULES" 2>/dev/null; then
        echo "  Using local preprocess rules: $(display_path "$REKT_PREPROCESS_RULES")"
        export REKT_PREPROCESS_RULES
    else
        echo "  ⚠️  Ignoring $REKT_PREPROCESS_RULES: not valid JSON with a programRewrites list"
        unset REKT_PREPROCESS_RULES
    fi
else
    unset REKT_PREPROCESS_RULES
fi

# Preprocess COBOL programs.
cbl_count=0
while IFS= read -r -d '' cbl; do
    if lma_cli rekt-preprocess program --source "$cbl" --source-root "$SOURCE_DIR" --output-root "$PREPROC_DIR" \
        ${REKT_PREPROCESS_RULES:+--rules "$REKT_PREPROCESS_RULES"} 2>/dev/null; then
        lma_cli rekt-evidence provenance \
            --source "$cbl" --target "$PREPROC_DIR/${cbl#"$SOURCE_DIR/"}"
        cbl_count=$((cbl_count + 1))
    fi
done < <(find "$SOURCE_DIR" ${FIND_DEPTH[@]+"${FIND_DEPTH[@]}"} \
    \( -name "*.cbl" -o -name "*.CBL" -o -name "*.cob" -o -name "*.COB" \) \
    -type f \
    ! -path "*/.rekt-staging/*" \
    ! -path "*/.preprocessed/*" \
    ! -path "*/.convert-*/*" \
    -print0)

total=$((cbl_count + cpy_count))
if [[ $total -gt 0 ]]; then
    echo "  Preprocessed $cbl_count program(s) and $cpy_count copybook(s) → .preprocessed/"
else
    echo "  No files needed preprocessing"
fi

# Create eight-character aliases for long copybook names. They live beside the preprocessed
# output rather than in the source tree, which would otherwise grow an extra copybook per long
# name on every run. They are real content, not stubs, so they are kept apart from those.
ALIAS_DIR="$PREPROC_DIR/aliases"
while IFS= read -r -d '' cpy; do
    fname=$(basename "$cpy")
    if ! lma_cli rekt-evidence validate-definition --source "$cpy"; then
        continue
    fi
    base="${fname%.*}"
    ext="${fname##*.}"
    if [[ ${#base} -gt 8 ]]; then
        short="${base:0:8}"
        target="$ALIAS_DIR/${short}.${ext}"
        if [[ ! -e "$target" && ! -e "$SOURCE_DIR/${short}.${ext}" ]]; then
            mkdir -p "$ALIAS_DIR"
            cp "$cpy" "$target"
            lma_cli rekt-evidence provenance --source "$cpy" --target "$target"
            echo "  Created 8-char alias: ${short}.${ext} → $fname"
        fi
    fi
done < <(find "$SOURCE_DIR" ${FIND_DEPTH[@]+"${FIND_DEPTH[@]}"} \
    \( -name "*.cpy" -o -name "*.CPY" \) \
    -type f \
    ! -path "*/.rekt-staging/*" \
    ! -path "*/.preprocessed/*" \
    ! -path "*/.convert-*/*" \
    -print0)

# Copy bundled system copybooks only when the user has not supplied them.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SYS_CPY_DIR="$SCRIPT_DIR/system-copybooks"
sys_count=0
if [[ -d "$SYS_CPY_DIR" ]]; then
    while IFS= read -r -d '' sys_cpy; do
        sys_name=$(basename "$sys_cpy")
        sys_base=$(echo "$sys_name" | sed 's/\.[^.]*$//' | tr '[:lower:]' '[:upper:]')
        # If user has a real copybook of the same name in source/, prefer theirs
        user_match=""
        while IFS= read -r -d '' candidate; do
            if lma_cli rekt-evidence validate-definition --source "$candidate"; then
                user_match="$candidate"
                break
            fi
        done < <(find "$SOURCE_DIR" ${FIND_DEPTH[@]+"${FIND_DEPTH[@]}"} \
            \( -iname "${sys_base}.cpy" -o -iname "${sys_base}.CPY" -o -iname "${sys_base}.cpb" \) \
            -type f \
            ! -path "*/.rekt-staging/*" \
            ! -path "*/.preprocessed/*" \
            ! -path "*/.convert-*/*" \
            -print0)
        if [[ -n "$user_match" ]]; then continue; fi
        # Don't clobber an already-preprocessed version
        if [[ -f "$PREPROC_DIR/$sys_name" ]]; then continue; fi
        cp "$sys_cpy" "$PREPROC_DIR/$sys_name"
        lma_cli rekt-evidence provenance \
            --source "$sys_cpy" --target "$PREPROC_DIR/$sys_name"
        sys_count=$((sys_count + 1))
    done < <(find "$SYS_CPY_DIR" -maxdepth 1 -type f \( -name "*.cpy" -o -name "*.CPY" \) -print0)
fi
if [[ "$sys_count" -gt 0 ]]; then
    echo "  Shipped $sys_count bundled system copybook(s) (e.g. SQLCA) → $(display_path "$PREPROC_DIR")/"
fi

# Bind this estate to a snapshot of the configured platform contract. Later edits to the shared
# profile then affect an estate only when that estate is preprocessed and parsed again.
if [[ -n "${REKT_PLATFORM_CONTRACTS:-}" ]]; then
    cp "$REKT_PLATFORM_CONTRACTS" "$PREPROC_DIR/platform-contracts.json"
    REKT_PLATFORM_CONTRACTS="$PREPROC_DIR/platform-contracts.json" \
        lma_cli rekt-platform-contracts "$SOURCE_DIR" "$PREPROC_DIR"
fi

# Generate COBOL symbolic maps from BMS mapsets (z/OS builds produce these; repos ship only
# the .bms). Runs before stubbing so a COPY of a mapset gets its real fields, not an empty stub.
# Set REKT_NO_BMS_MAPS=true to disable.
if [[ "${REKT_NO_BMS_MAPS:-false}" != "true" ]]; then
    if ! lma_cli rekt-bms-map "$SOURCE_DIR" "$PREPROC_DIR"; then
        echo "  ⚠️  BMS symbolic map generation failed; affected COPY targets remain unresolved"
    fi
fi
