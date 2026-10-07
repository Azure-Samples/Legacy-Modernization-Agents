#!/usr/bin/env bash
# Blocks commits that would publish a customer's estate: mainframe source files, or text
# that names the estate's programs, copybooks and jobs.
#
# Usage:
#   tools/check-no-estate-data.sh                 check what is staged (pre-commit)
#   tools/check-no-estate-data.sh --all           check every tracked file
#   tools/check-no-estate-data.sh --install-hook  run the staged check before every commit
#
set -euo pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

MODE="staged"
case "${1:-}" in
    --all) MODE="all" ;;
    --install-hook)
        hook="$(git rev-parse --git-path hooks)/pre-commit"
        if [[ -e "$hook" ]] && ! grep -q "check-no-estate-data" "$hook"; then
            echo "A different pre-commit hook already exists at $hook; add this line to it:"
            echo "  tools/check-no-estate-data.sh"
            exit 1
        fi
        printf '#!/usr/bin/env bash\nexec "$(git rev-parse --show-toplevel)/tools/check-no-estate-data.sh"\n' > "$hook"
        chmod +x "$hook"
        echo "Installed $hook"
        exit 0 ;;
    "") ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
esac

# The three bundled samples and the shipped system copybooks are public by design.
ALLOWED_SOURCE='^(source/CUSTOMER-(DATA\.cpy|DISPLAY\.cbl|INQUIRY\.cbl)|tools/system-copybooks/[^/]+\.cpy)$'
MAINFRAME_EXT='\.(cbl|cpy|cob|cobol|jcl|prc|bms|csd|ims|dbd|psb|mfs)$'

if [[ "$MODE" == "staged" ]]; then
    files="$(git diff --cached --name-only --diff-filter=ACMR)"
else
    files="$(git ls-files)"
fi
[[ -z "$files" ]] && exit 0

failed=0

source_files="$(printf '%s\n' "$files" | grep -iE "$MAINFRAME_EXT" | grep -vE "$ALLOWED_SOURCE" || true)"
if [[ -n "$source_files" ]]; then
    echo "✖ Mainframe source files would be committed:"
    printf '    %s\n' $source_files
    failed=1
fi

terms="$(mktemp)"
trap 'rm -f "$terms"' EXIT
if [[ -d source ]]; then
    find source -type f ! -name '.*' ! -path '*/.preprocessed/*' ! -path '*/.rekt-staging/*' \
        | sed -E 's#.*/##; s/\.[^.]+$//' | grep -E '[0-9]' | awk 'length($0) >= 4' >> "$terms" || true
fi
if [[ -f Config/sensitive-terms.local.txt ]]; then
    sed -E 's/#.*//; s/^[[:space:]]+//; s/[[:space:]]+$//' Config/sensitive-terms.local.txt \
        | grep -v '^$' >> "$terms" || true
fi
sort -u -o "$terms" "$terms"

if [[ -s "$terms" ]]; then
    grep_scope=()
    [[ "$MODE" == "staged" ]] && grep_scope=(--cached)
    # Paths are listed explicitly so ignored-but-tracked files are still checked.
    hits="$(printf '%s\n' "$files" | tr '\n' '\0' \
        | xargs -0 git grep ${grep_scope[@]+"${grep_scope[@]}"} -I -n -i -F -f "$terms" -- 2>/dev/null \
        | grep -vE "^($ALLOWED_SOURCE):" || true)"
    if [[ -n "$hits" ]]; then
        echo "✖ Estate names found in files that would be committed:"
        printf '%s\n' "$hits" | cut -c1-200 | sed 's/^/    /' | head -50
        count="$(printf '%s\n' "$hits" | wc -l | tr -d ' ')"
        [[ "$count" -gt 50 ]] && echo "    … and $((count - 50)) more"
        failed=1
    fi
fi

if [[ "$failed" -ne 0 ]]; then
    echo
    echo "Mask the names (keep the length if the text is fixed-format COBOL), or move"
    echo "estate-specific rules to a git-ignored Config/*.local.* file."
    exit 1
fi
if [[ "$MODE" == all ]]; then echo "✓ No estate data in tracked files"; else echo "✓ No estate data in staged files"; fi
