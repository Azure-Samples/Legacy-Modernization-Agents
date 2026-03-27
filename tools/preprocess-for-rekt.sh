#!/bin/bash
# ═══════════════════════════════════════════════════════════════════
# COBOL Preprocessor for rekt compatibility
# Handles:
#   - EXEC DLI → comment-out entire block (IMS/DL/I)
#   - EXEC SQL INCLUDE name → COPY name (DB2 includes)
#   - EXEC SQL GET DIAGNOSTICS → comment-out block
#   - Pseudo-text replacement tokens :TOKEN: → XTOKEN in copybooks
#   - Quoted COPY names: COPY 'NAME' → COPY NAME
#   - Embedded line numbers in columns 1-6
# ═══════════════════════════════════════════════════════════════════

set -euo pipefail

SOURCE_DIR="${1:?Usage: preprocess-for-rekt.sh <source-dir>}"
PREPROC_DIR="${SOURCE_DIR}/.preprocessed"

mkdir -p "$PREPROC_DIR"

# Detect python
PYTHON=""
if command -v python3 >/dev/null 2>&1; then
    PYTHON="python3"
elif command -v python >/dev/null 2>&1; then
    PYTHON="python"
fi

if [[ -z "$PYTHON" ]]; then
    echo "  ⚠️  Python not found — preprocessor needs python3"
    exit 1
fi

# ─── Phase 1: Preprocess copybooks (.cpy) ───────────────────────────
# Resolve pseudo-text tokens :TOKEN: → XTOKEN and add FILLER to anonymous entries
cpy_count=0
for cpy in "$SOURCE_DIR"/*.cpy "$SOURCE_DIR"/*.CPY; do
    [[ -e "$cpy" ]] || continue
    fname=$(basename "$cpy")

    "$PYTHON" -c "
import re, sys

with open('$cpy', 'r', encoding='latin-1') as f:
    content = f.read()

original = content

# Strip trailing sequence numbers embedded in the content area
# Some files have cols 73-80 seq numbers that bleed into cols 8-72
# (e.g., '001600 01 DB2FEJL REDEFINES SQLCA.           00000160')
# Also handles seq numbers concatenated after a period: '.00000210'
def strip_trailing_seq(text):
    out = []
    for line in text.split('\n'):
        raw = line.rstrip()
        if len(raw) >= 14 and raw[:6].strip().isdigit():
            # Skip comment lines
            if len(raw) > 6 and raw[6] == '*':
                out.append(line)
                continue
            # Pattern 1: trailing seq separated by 2+ spaces
            m = re.match(r'^(.+?)\s{2,}(\d{8})$', raw)
            if m:
                out.append(m.group(1))
                continue
            # Pattern 2: trailing seq concatenated after period
            m = re.match(r'^(.+\.)(\d{8})$', raw)
            if m:
                out.append(m.group(1))
                continue
        out.append(line)
    return '\n'.join(out)

content = strip_trailing_seq(content)

# Replace :TOKEN: with XTOKEN (valid COBOL identifier)
content = re.sub(r\":\'([A-Z][A-Z0-9_-]+)\':\", lambda m: \"'X\" + m.group(1).replace('-','') + \"'\", content)
content = re.sub(r':([A-Z][A-Z0-9_-]+):', lambda m: 'X' + m.group(1).replace('-',''), content)

def insert_filler(m):
    prefix = m.group(1)   # seq + leading whitespace
    level = m.group(2)    # level number
    gap = m.group(3)      # whitespace between level and PIC/REDEFINES
    kw = m.group(4)       # PIC or REDEFINES
    # Shrink gap to make room for 'FILLER ' (7 chars) — keep at least 1 space
    new_gap_len = max(1, len(gap) - 7)
    return prefix + level + ' ' * new_gap_len + 'FILLER ' + kw

# Add FILLER to anonymous data entries: '  NN  PIC' → '  NN FILLER PIC'
content = re.sub(
    r'^(\s*\d{0,6}\s+)(\d{2})(\s{8,})(PIC\b)',
    insert_filler,
    content,
    flags=re.MULTILINE
)

# Add FILLER to anonymous REDEFINES: '  NN  REDEFINES' → '  NN FILLER REDEFINES'
content = re.sub(
    r'^(\s*\d{0,6}\s+)(\d{2})(\s{8,})(REDEFINES\b)',
    insert_filler,
    content,
    flags=re.MULTILINE
)

# Replace standalone COMP-2/COMP-1 (no PIC) with PIC-based equivalent
# COMP-2 = 8-byte double float, COMP-1 = 4-byte single float
content = re.sub(r'\bCOMP-2\b', 'PIC X(8)', content)
content = re.sub(r'\bCOMP-1\b', 'PIC X(4)', content)

# Enforce column 72 limit: shrink interior whitespace on overlong lines
def enforce_col72(text):
    out_lines = []
    for line in text.split('\n'):
        raw = line.rstrip()
        if len(raw) > 72 and not raw.lstrip().startswith('*'):
            # Compress runs of multiple spaces (keeping at least 1)
            import re as _re
            compressed = _re.sub(r'(\S)(  +)(\S)', lambda m: m.group(1) + ' ' * max(1, len(m.group(2)) - (len(raw) - 72)) + m.group(3), raw)
            if len(compressed) > 72:
                # More aggressive: shrink ALL multi-space runs
                while len(compressed) > 72:
                    compressed = _re.sub(r'(  +)', lambda m: ' ' * max(1, len(m.group(1)) - 1), compressed, count=1)
                    if '  ' not in compressed:
                        break
            out_lines.append(compressed)
        else:
            out_lines.append(line)
    return '\n'.join(out_lines)

content = enforce_col72(content)

# Rename consecutive FILLER REDEFINES of the same field to unique names.
# smojol parser chokes on >10 consecutive FILLER REDEFINES of one field.
counter = [0]
def unique_redefines(m):
    counter[0] += 1
    prefix = m.group(1)  # leading whitespace + seq number
    level = m.group(2)   # level number (05)
    mid = m.group(3)     # whitespace before REDEFINES keyword
    rest = m.group(4)    # 'REDEFINES PARM-DATO' etc.
    unique_name = f'FIL-R{counter[0]:03d}'
    # Shrink mid whitespace to compensate for longer name
    filler_len = len('FILLER')
    name_len = len(unique_name)
    adjust = name_len - filler_len
    new_mid = mid[:-(adjust)] if adjust > 0 and len(mid) > adjust else mid
    return prefix + level + ' ' + unique_name + new_mid + rest

content = re.sub(
    r'^(\s*\d{0,6}\s+)(\d{2})\s+FILLER(\s+)(REDEFINES\s+\S+)',
    unique_redefines,
    content,
    flags=re.MULTILINE
)

if content != original:
    with open('$PREPROC_DIR/$fname', 'w', encoding='latin-1') as f:
        f.write(content)
    sys.exit(0)
else:
    sys.exit(1)
" 2>/dev/null && cpy_count=$((cpy_count + 1))
done

# ─── Phase 2: Preprocess ALL programs (.cbl) ────────────────────────
cbl_count=0
for cbl in "$SOURCE_DIR"/*.cbl "$SOURCE_DIR"/*.CBL; do
    [[ -e "$cbl" ]] || continue
    fname=$(basename "$cbl")

    "$PYTHON" -c "
import re, sys

with open('$cbl', 'r', encoding='latin-1') as f:
    content = f.read()

original = content

# 0. Strip trailing sequence numbers embedded in content area
def strip_trailing_seq(text):
    out = []
    for line in text.split('\n'):
        raw = line.rstrip()
        if len(raw) >= 14 and raw[:6].strip().isdigit():
            if len(raw) > 6 and raw[6] == '*':
                out.append(line)
                continue
            m = re.match(r'^(.+?)\s{2,}(\d{8})$', raw)
            if m:
                out.append(m.group(1))
                continue
            m = re.match(r'^(.+\.)(\d{8})$', raw)
            if m:
                out.append(m.group(1))
                continue
        out.append(line)
    return '\n'.join(out)

content = strip_trailing_seq(content)

# 1. Comment out EXEC DLI ... END-EXEC blocks (IMS/DL/I calls)
content = re.sub(
    r'([ ]{6,})EXEC\s+DLI\b.*?END-EXEC\.?',
    lambda m: re.sub(r'^(.{6})', r'\g<1>*IMS>', m.group(0), flags=re.MULTILINE),
    content,
    flags=re.DOTALL | re.IGNORECASE
)

# 2. EXEC SQL INCLUDE name END-EXEC → COPY name.
content = re.sub(
    r'EXEC\s+SQL\s+INCLUDE\s+(\w+)\s*END-EXEC\.?',
    r'COPY \1.',
    content,
    flags=re.IGNORECASE
)

# 3. Comment out EXEC SQL GET DIAGNOSTICS ... END-EXEC blocks
content = re.sub(
    r'([ ]{6,})EXEC\s+SQL\s+GET\s+DIAGNOSTICS\b.*?END-EXEC\.?',
    lambda m: re.sub(r'^(.{6})', r'\g<1>*DB2>', m.group(0), flags=re.MULTILINE),
    content,
    flags=re.DOTALL | re.IGNORECASE
)

# 4. Strip quoted COPY names: COPY 'NAME' → COPY NAME
content = re.sub(r\"COPY\s+'([A-Z0-9]+)'\", r'COPY \1', content)

# 4b. Truncate COPY names >8 chars to 8 chars (smojol limit)
import os
def truncate_copy(m):
    prefix = m.group(1)
    name = m.group(2)
    suffix = m.group(3)
    if len(name) > 8:
        short = name[:8]
        # Only truncate if an 8-char alias copybook exists
        alias = os.path.join('$SOURCE_DIR', short + '.cpy')
        if os.path.exists(alias):
            return prefix + short + suffix
    return m.group(0)

content = re.sub(
    r'(COPY\s+)([A-Z][A-Z0-9_-]{8,})([\s.])',
    truncate_copy,
    content,
    flags=re.IGNORECASE
)

# 5. Resolve pseudo-text tokens :TOKEN: → XTOKEN
content = re.sub(r\":\'([A-Z][A-Z0-9_-]+)\':\", lambda m: \"'X\" + m.group(1).replace('-','') + \"'\", content)
content = re.sub(r':([A-Z][A-Z0-9_-]+):', lambda m: 'X' + m.group(1).replace('-',''), content)

# 5b. Replace MOVE CORR/CORRESPONDING with comment (smojol NPE bug)
# Place '*' in column 7 for proper COBOL comment
def comment_move_corr(m):
    full = m.group(0)
    # Ensure column 7 has '*'
    if len(full) >= 7:
        return full[:6] + '*' + full[7:]
    return full

content = re.sub(
    r'^.{0,6}[ ]+MOVE\s+CORR(?:ESPONDING)?\s.*$',
    comment_move_corr,
    content,
    flags=re.MULTILINE | re.IGNORECASE
)

# 5c. Simplify reference modification with arithmetic expressions
# (TALLY + 1:WS-LGT) → (1:WS-LGT) - smojol can't parse arithmetic in ref-mod
content = re.sub(
    r'\(([A-Z][A-Z0-9_-]*\s*[+\-]\s*\d+):',
    r'(1:',
    content,
    flags=re.IGNORECASE
)

def insert_filler(m):
    prefix = m.group(1)
    level = m.group(2)
    gap = m.group(3)
    kw = m.group(4)
    new_gap_len = max(1, len(gap) - 7)
    return prefix + level + ' ' * new_gap_len + 'FILLER ' + kw

# 6. Add FILLER to anonymous data entries (shrink whitespace to stay within col 72)
content = re.sub(
    r'^(\s*\d{0,6}\s+)(\d{2})(\s{8,})(PIC\b)',
    insert_filler,
    content,
    flags=re.MULTILINE
)

# 7. Add FILLER to anonymous REDEFINES
content = re.sub(
    r'^(\s*\d{0,6}\s+)(\d{2})(\s{8,})(REDEFINES\b)',
    insert_filler,
    content,
    flags=re.MULTILINE
)

# 8. Replace standalone COMP-2/COMP-1 (no PIC) with byte-equivalent PIC
content = re.sub(r'\bCOMP-2\b', 'PIC X(8)', content)
content = re.sub(r'\bCOMP-1\b', 'PIC X(4)', content)

# 9. Enforce column 72 limit
def enforce_col72(text):
    out_lines = []
    for line in text.split('\n'):
        raw = line.rstrip()
        if len(raw) > 72 and not raw.lstrip().startswith('*'):
            import re as _re
            compressed = _re.sub(r'(\S)(  +)(\S)', lambda m: m.group(1) + ' ' * max(1, len(m.group(2)) - (len(raw) - 72)) + m.group(3), raw)
            if len(compressed) > 72:
                while len(compressed) > 72:
                    compressed = _re.sub(r'(  +)', lambda m: ' ' * max(1, len(m.group(1)) - 1), compressed, count=1)
                    if '  ' not in compressed:
                        break
            out_lines.append(compressed)
        else:
            out_lines.append(line)
    return '\n'.join(out_lines)

content = enforce_col72(content)

# 10. Rename consecutive FILLER REDEFINES to unique names (smojol limit)
counter = [0]
def unique_redefines(m):
    counter[0] += 1
    prefix = m.group(1)
    level = m.group(2)
    mid = m.group(3)
    rest = m.group(4)
    unique_name = f'FIL-R{counter[0]:03d}'
    filler_len = len('FILLER')
    name_len = len(unique_name)
    adjust = name_len - filler_len
    new_mid = mid[:-(adjust)] if adjust > 0 and len(mid) > adjust else mid
    return prefix + level + ' ' + unique_name + new_mid + rest

content = re.sub(
    r'^(\s*\d{0,6}\s+)(\d{2})\s+FILLER(\s+)(REDEFINES\s+\S+)',
    unique_redefines,
    content,
    flags=re.MULTILINE
)

if content != original:
    with open('$PREPROC_DIR/$fname', 'w', encoding='latin-1') as f:
        f.write(content)
    sys.exit(0)
else:
    sys.exit(1)
" 2>/dev/null && cbl_count=$((cbl_count + 1))
done

total=$((cbl_count + cpy_count))
if [[ $total -gt 0 ]]; then
    echo "  Preprocessed $cbl_count program(s) and $cpy_count copybook(s) → .preprocessed/"
else
    echo "  No files needed preprocessing"
fi

# ─── Phase 3: Handle >8-char copybook names ─────────────────────────
# smojol warns about copybook names >8 characters. Create 8-char .cpy
# aliases in the source dir so the parser can resolve them.
for cpy in "$SOURCE_DIR"/*.cpy "$SOURCE_DIR"/*.CPY; do
    [[ -e "$cpy" ]] || continue
    fname=$(basename "$cpy")
    base="${fname%.*}"
    ext="${fname##*.}"
    if [[ ${#base} -gt 8 ]]; then
        short="${base:0:8}"
        target="$SOURCE_DIR/${short}.${ext}"
        if [[ ! -e "$target" ]]; then
            cp "$cpy" "$target"
            echo "  Created 8-char alias: ${short}.${ext} → $fname"
        fi
    fi
done
