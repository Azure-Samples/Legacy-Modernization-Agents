#!/bin/bash
# ═══════════════════════════════════════════════════════════════════
# Portability lint for the project's shell scripts.
#
# The scripts have to run unchanged on:
#   - macOS  /bin/bash 3.2 (still the system shell)
#   - Git Bash / MSYS2 bash 5.x on Windows
#   - Linux  bash 5.x
#
# That two-sided constraint is easy to break, and the failures are not
# caught by the usual tooling: `bash -n` and shellcheck both accept the
# constructs below, which then fail at *runtime* on one platform only.
# This script encodes the specific cases we have actually been bitten by.
#
# Usage: tools/check-bash-compat.sh [file ...]     (defaults to all *.sh)
# Exits non-zero if any problem is found.
# ═══════════════════════════════════════════════════════════════════

set -uo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 1

if [[ $# -gt 0 ]]; then
    files=("$@")
else
    # git ls-files keeps generated/vendored scripts out of scope, but fall back
    # to find when git is unavailable or this is not a working checkout.
    files=()
    while IFS= read -r line; do
        [[ -n "$line" ]] && files+=("$line")
    done < <(git ls-files '*.sh' 2>/dev/null)
    if [[ ${#files[@]} -eq 0 ]]; then
        while IFS= read -r line; do
            [[ -n "$line" ]] && files+=("$line")
        done < <(find . -name '*.sh' -type f -not -path './.git/*' 2>/dev/null)
    fi
    # Never report success because discovery failed — that would turn this
    # guard into a silent no-op in exactly the environments it protects.
    if [[ ${#files[@]} -eq 0 ]]; then
        echo "✗ No shell scripts found to check (file discovery failed)." >&2
        exit 1
    fi
fi

problems=0

report() {
    problems=$((problems + 1))
    printf '%s:%s: %s\n' "$1" "$2" "$3"
    printf '    %s\n' "$4"
}

for f in "${files[@]}"; do
    [[ -f "$f" ]] || continue

    # ── 1. Here-document inside $( ) followed by || or && ─────────────
    # Parses on bash 3.2, but bash 5.x re-parses the substitution body and
    # fails with a syntax error on the trailing operator.
    # The guard and the fallback must live outside the substitution.
    # Patterns use [x] brackets so this file does not match its own rules.
    while IFS=: read -r ln text; do
        [[ -z "$ln" ]] && continue
        report "$f" "$ln" "here-doc inside command substitution with a trailing or/and operator — breaks on bash 5.x (Git Bash)" \
            "move the guard and fallback outside the substitution"
    done < <(awk '/\$\(.*<[<]/ { p = index($0, "<<"); if (substr($0, p) ~ /[|][|]|[&][&]/) printf "%d:%s\n", NR, $0 }' "$f")

    # ── 2. bash 4+ only syntax — breaks macOS /bin/bash 3.2 ───────────
    while IFS=: read -r ln text; do
        [[ -z "$ln" ]] && continue
        report "$f" "$ln" "bash 4+ only construct — breaks macOS /bin/bash 3.2" \
            "avoid associative arrays, case-conversion expansions, map[f]ile/read[a]rray, globstar"
    done < <(grep -nE 'declare -[A]|local -[A]|\$\{[A-Za-z_][A-Za-z0-9_]*(\^\^|,,)|\b(map[f]ile|read[a]rray)\b|shopt -s (globstar|lastpipe)' "$f" | cut -d: -f1,2)

    # ── 3. CRLF line endings ──────────────────────────────────────────
    # A CR before the here-doc terminator stops it matching, and `\r` in a
    # shebang makes the interpreter unresolvable. .gitattributes pins LF;
    # this catches a file that slipped in with CRLF anyway.
    if grep -qU $'\r' "$f" 2>/dev/null; then
        report "$f" "1" "CRLF line endings — breaks here-doc terminators and shebangs" \
            "store shell scripts with LF only (see .gitattributes)"
    fi
done

if [[ $problems -gt 0 ]]; then
    echo ""
    echo "✗ $problems portability problem(s) found."
    exit 1
fi

echo "✓ No shell portability problems found (checked ${#files[@]} file(s))."
