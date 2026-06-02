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
# Recursive find so nested layouts (source/corpus/cpy/, etc.) are picked up.
# -print0 / read -d '' avoids issues with paths containing whitespace.
while IFS= read -r -d '' cpy; do
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
        # Skip comment lines
        if len(raw) > 6 and raw[6] == '*':
            out.append(line)
            continue
        # Pattern 1: trailing seq separated by whitespace on long fixed-format lines
        m = re.match(r'^(.+?)\s+(\d{8})$', raw) if len(raw) > 72 else None
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
# Pattern handles lines both with and without 6-digit sequence numbers.
# The gap between level-number and keyword can be as small as 1 space.
content = re.sub(
    r'^(\d{6}\s+|\s+)(\d{1,2})(\s{1,})(PIC\b)',
    insert_filler,
    content,
    flags=re.MULTILINE
)

# Add FILLER to anonymous REDEFINES: '  NN  REDEFINES' → '  NN FILLER REDEFINES'
content = re.sub(
    r'^(\d{6}\s+|\s+)(\d{1,2})(\s{1,})(REDEFINES\b)',
    insert_filler,
    content,
    flags=re.MULTILINE
)

# Replace standalone COMP-2/COMP-1 (no PIC) with PIC-based equivalent
# COMP-2 = 8-byte double float, COMP-1 = 4-byte single float
content = re.sub(r'\bCOMP-2\b', 'PIC X(8)', content)
content = re.sub(r'\bCOMP-1\b', 'PIC X(4)', content)

# Enforce column 72 limit: handle two cases differently
#   Case A: cols 73+ contain a mainframe timestamp/seq number/version stamp
#           (digits, dots, dashes, slashes only) — TRUNCATE at col 72.
#           This is the standard COBOL fixed-format treatment of the
#           identification area (cols 73-80). The text does not belong
#           to the program — it is a code-versioning audit trail.
#   Case B: cols 73+ contain actual code that overflowed (rare) — fall
#           back to whitespace compression to fit content into 72 cols.
def enforce_col72(text):
    out_lines = []
    # Anything in cols 73+ that's only digits/dots/dashes/slashes/colons/spaces
    # is treated as an audit stamp and dropped.
    stamp_rx = _re_module.compile(r'^[0-9.\-:/ ]+\s*$')
    for line in text.split('\n'):
        raw = line.rstrip()
        if len(raw) > 72 and not raw.lstrip().startswith('*'):
            tail = raw[72:]
            if stamp_rx.match(tail):
                # Case A — drop the audit stamp
                out_lines.append(raw[:72].rstrip())
                continue
            # Case B — overflow is real code, try to compress whitespace
            compressed = _re_module.sub(r'(\S)(  +)(\S)', lambda m: m.group(1) + ' ' * max(1, len(m.group(2)) - (len(raw) - 72)) + m.group(3), raw)
            if len(compressed) > 72:
                # More aggressive: shrink ALL multi-space runs
                while len(compressed) > 72:
                    compressed = _re_module.sub(r'(  +)', lambda m: ' ' * max(1, len(m.group(1)) - 1), compressed, count=1)
                    if '  ' not in compressed:
                        break
            out_lines.append(compressed)
        else:
            out_lines.append(line)
    return '\n'.join(out_lines)

# Import re once at module scope for enforce_col72
import re as _re_module
content = enforce_col72(content)

# Strip Endevor-style audit stamps that appear at end-of-line. These show
# up in old mainframe code as a date in DD.MM.YY or HH.MM.SS format after
# a statement-terminating period:
#     05 CTA-LEER-REGISTRO  PIC X(6) VALUE 'LEER'.    12.02.16
#     05 WS-VAL             PIC X.                    15.08.20
# After enforce_col72 compresses whitespace, these stamps end up inside
# the 1-72 program area and become Extraneous-input tokens that crash
# the parser. Drop them — they are pure audit metadata, not COBOL.
content = _re_module.sub(
    r'(\.)\s+(\d{1,4}[.\-/]\d{1,2}[.\-/]\d{1,4})\s*$',
    r'\1', content, flags=_re_module.MULTILINE)
# Also strip stamps that follow other tokens (no period before), e.g.:
#     PIC X.   12.02.16     ← caught above
#     COPY FOO.   12.02.16  ← also caught above
# But also bare stamps at end of line (no preceding period):
content = _re_module.sub(
    r'\s{2,}(\d{1,4}[.\-/]\d{1,2}[.\-/]\d{1,4})\s*$',
    '', content, flags=_re_module.MULTILINE)

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

# Comment out section labels in procedure-division SAMPLE* copybooks (without I suffix).
# These inline code copybooks define sections that cause parse errors when included
# inside an existing section/paragraph in the host program.
bname = '$fname'.upper()
if bname.startswith('SAMPLE') and not bname.endswith('I.CPY'):
    content = re.sub(
        r'^(\s*\d{0,6})( )([A-Z][A-Z0-9-]+\s+SECTION\s*\.\s*)$',
        r'\1*\3',
        content,
        flags=re.MULTILINE | re.IGNORECASE
    )

# Replace MOVE CORR/CORRESPONDING statements with CONTINUE.
# smojol MoveFlowNode does not handle moveCorrespondingStatement (only moveToStatement).
# Process line by line: for multi-line forms, the TO clause is on the next line.
lines = content.splitlines(keepends=True)
result_lines = []
skip_to_line = False
for line in lines:
    if skip_to_line:
        # This continuation line has the TO clause — suppress it
        m_seq = re.match(r'^(\d{6})', line)
        if m_seq:
            result_lines.append(m_seq.group(1) + '*COR>' + line[6:])
        else:
            result_lines.append(re.sub(r'^(.{6})', r'\1*COR>', line))
        skip_to_line = False
        continue
    if re.search(r'\bMOVE\s+CORR(ESPONDING)?\b', line, re.IGNORECASE):
        # Replace with CONTINUE, preserving indentation
        m_indent = re.match(r'^(\s*\d{0,6}\s+)', line)
        indent = m_indent.group(1) if m_indent else '           '
        result_lines.append(indent + 'CONTINUE\n')
        if not re.search(r'\bTO\b', line, re.IGNORECASE):
            skip_to_line = True  # next line has the TO clause
    else:
        result_lines.append(line)
content = ''.join(result_lines)

# Replace European decimal comma in numeric literals: '2415020,5' → '2415020.5'
# Some COBOL programs use comma as decimal separator (Danish/German convention).
content = re.sub(r'(\b\d+),(\d+\b)', r'\1.\2', content)

if content != original:
    with open('$PREPROC_DIR/$fname', 'w', encoding='latin-1') as f:
        f.write(content)
    sys.exit(0)
else:
    sys.exit(1)
" 2>/dev/null && cpy_count=$((cpy_count + 1))
done < <(find "$SOURCE_DIR" \
    \( -name "*.cpy" -o -name "*.CPY" \) \
    -type f \
    ! -path "*/.rekt-staging/*" \
    ! -path "*/.preprocessed/*" \
    ! -path "*/.convert-*/*" \
    ! -path "*/.convert-*/*" \
    -print0)

# ─── Phase 2: Preprocess ALL programs (.cbl, .cob) ──────────────────
cbl_count=0
while IFS= read -r -d '' cbl; do
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
        # Skip comment lines
        if len(raw) > 6 and raw[6] == '*':
            out.append(line)
            continue
        m = re.match(r'^(.+?)\s+(\d{8})$', raw) if len(raw) > 72 else None
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

# Step 0: Replace European decimal comma in numeric literals: '2415020,5' → '2415020.5'
content = re.sub(r'(\b\d+),(\d+\b)', r'\1.\2', content)

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

# 5d. Normalize numeric MOVE literals written as 0(1) / 1(1) so the parser sees
#     a plain numeric literal instead of unsupported parenthesized syntax.
content = content.replace('MOVE 0(1) TO', 'MOVE 0 TO')
content = content.replace('MOVE 1(1) TO', 'MOVE 1 TO')

def insert_filler(m):
    prefix = m.group(1)
    level = m.group(2)
    gap = m.group(3)
    kw = m.group(4)
    new_gap_len = max(1, len(gap) - 7)
    return prefix + level + ' ' * new_gap_len + 'FILLER ' + kw

# 6. Add FILLER to anonymous data entries (shrink whitespace to stay within col 72)
# Pattern handles both lines with 6-digit sequence numbers and plain indented lines.
content = re.sub(
    r'^(\d{6}\s+|\s+)(\d{1,2})(\s{1,})(PIC\b)',
    insert_filler,
    content,
    flags=re.MULTILINE
)

# 7. Add FILLER to anonymous REDEFINES
content = re.sub(
    r'^(\d{6}\s+|\s+)(\d{1,2})(\s{1,})(REDEFINES\b)',
    insert_filler,
    content,
    flags=re.MULTILINE
)

# 8. Replace standalone COMP-2/COMP-1 (no PIC) with byte-equivalent PIC
content = re.sub(r'\bCOMP-2\b', 'PIC X(8)', content)
content = re.sub(r'\bCOMP-1\b', 'PIC X(4)', content)

# 8b. Best-effort normalize compiler-specific copy-with-prefix lines so the parser
#     can keep scanning the file instead of aborting recursion on these directives.
def comment_copy_with_prefix(line):
    upper = line.upper()
    if '-COPY' in upper and '-PRE' in upper and not (len(line) > 6 and line[6] == '*'):
        if len(line) >= 7:
            return line[:6] + '*' + line[7:]
        return (line[:6].ljust(6)) + '*' + line[6:]
    return line

content = '\n'.join(comment_copy_with_prefix(line) for line in content.split('\n'))

# 8c. Best-effort rewrite unsupported figurative constants using punctuation so the
#     parser sees a safe literal rather than failing on ALL '%'.
def normalize_all_punct(line):
    if len(line) > 6 and line[6] == '*':
        return line
    return re.sub(r'\bALL\s+\'([^\']+)\'', lambda m: \"'\" + m.group(1) + \"'\", line)

content = '\n'.join(normalize_all_punct(line) for line in content.split('\n'))

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

# Pre-step: Fix REPORT001 PERFORM UNTIL with 88-level conditions BEFORE col72 enforcement.
# smojol ConditionVisitor NPE on 88-level condition names in PERFORM UNTIL.
# Replace with explicit comparisons; split into continuation line to stay within col72.
# Also handle already-transformed form (idempotent, in case preprocessed file was copied back).
_until_orig = 'UNTIL BDC-FI01-EOF AND BDC-FI02-EOF'
_until_single = \"UNTIL BDC-FI01-RETURN-CODE = 'EOF' AND BDC-FI02-RETURN-CODE = 'EOF'\"
_until_two = \"UNTIL BDC-FI01-RETURN-CODE = 'EOF'\n      -       AND BDC-FI02-RETURN-CODE = 'EOF'\"
content = content.replace(_until_orig, _until_two)
# Idempotent: fix the already-transformed single-line form (possibly with compressed indent)
content = re.sub(
    r'^\s*PERFORM 300-BEHANDL-DATA ' + \"UNTIL BDC-FI01-RETURN-CODE = 'EOF' AND BDC-FI02-RETURN-CODE = 'EOF'\" + r'$',
    '           PERFORM 300-BEHANDL-DATA ' + _until_two,
    content,
    flags=re.MULTILINE
)
# Idempotent: fix the already-split two-line form that has wrong indentation on the first line
content = re.sub(
    r\"^\\s*PERFORM 300-BEHANDL-DATA UNTIL BDC-FI01-RETURN-CODE = 'EOF'$\",
    \"           PERFORM 300-BEHANDL-DATA UNTIL BDC-FI01-RETURN-CODE = 'EOF'\",
    content,
    flags=re.MULTILINE
)

content = enforce_col72(content)

# 10a. Fix AUTHOR paragraph: apostrophes in author names confuse the IBM COBOL preprocessor
#      e.g. AUTHOR. James O'Grady. -> AUTHOR. UNKNOWN.
content = re.sub(
    r'^(\s*AUTHOR\s*\.\s*)([^\n]*)$',
    lambda m: m.group(1) + 'UNKNOWN.' if \"'\" in m.group(2) else m.group(0),
    content,
    flags=re.MULTILINE
)

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

# 11. Comment out EXEC SQL DECLARE ... CURSOR FOR ... END-EXEC blocks
#     (cursor declarations in DATA DIVISION confuse the cobol-ls preprocessor)
content = re.sub(
    r'([ ]{6,})EXEC\s+SQL\s+DECLARE\s+\S+\s+CURSOR\b.*?END-EXEC\.?',
    lambda m: re.sub(r'^(.{6})', r'\g<1>*SQL>', m.group(0), flags=re.MULTILINE),
    content,
    flags=re.DOTALL | re.IGNORECASE
)

# 12. Comment out EXEC CICS INQUIRE ASSOCIATION ... END-EXEC blocks
content = re.sub(
    r'([ ]{6,})EXEC\s+CICS\s+INQUIRE\s+ASSOCIATION\b.*?END-EXEC\.?',
    lambda m: re.sub(r'^(.{6})', r'\g<1>*CIC>', m.group(0), flags=re.MULTILINE),
    content,
    flags=re.DOTALL | re.IGNORECASE
)

# 13. Comment out EXEC CICS RUN TRANSID ... END-EXEC blocks
content = re.sub(
    r'([ ]{6,})EXEC\s+CICS\s+RUN\s+TRANSID\b.*?END-EXEC\.?',
    lambda m: re.sub(r'^(.{6})', r'\g<1>*CIC>', m.group(0), flags=re.MULTILINE),
    content,
    flags=re.DOTALL | re.IGNORECASE
)

# 14. Comment out EXEC CICS FETCH ANY ... END-EXEC blocks
content = re.sub(
    r'([ ]{6,})EXEC\s+CICS\s+FETCH\s+ANY\b.*?END-EXEC\.?',
    lambda m: re.sub(r'^(.{6})', r'\g<1>*CIC>', m.group(0), flags=re.MULTILINE),
    content,
    flags=re.DOTALL | re.IGNORECASE
)

# 15. Normalize lowercase figurative constants to uppercase
#     (smojol FigurativeConstantMap only handles uppercase)
import re as _re2
def normalize_figuratives(text):
    # Only replace in non-comment, non-string-literal contexts
    result = []
    for line in text.split('\n'):
        if len(line) > 6 and line[6] == '*':
            result.append(line)
            continue
        # Replace standalone zero/spaces/space outside of string literals
        parts = _re2.split(r\"((?:'[^']*')+)\", line)
        new_parts = []
        for i, part in enumerate(parts):
            if i % 2 == 0:  # not inside string literal
                part = _re2.sub(r'\bzero\b', 'ZERO', part)
                part = _re2.sub(r'\bspaces\b', 'SPACES', part)
                part = _re2.sub(r'\bspace\b', 'SPACE', part)
            new_parts.append(part)
        result.append(''.join(new_parts))
    return '\n'.join(result)

content = normalize_figuratives(content)

# 16. Fix invalid data-name containing spaces/parens: INTRTI-PIC X(4) USAGE → INTRTI-PICX4
#     Data declaration: '01 INTRTI-PIC X(4) USAGE PIC X(4).' → '01 INTRTI-PICX4 PIC X(4).'
#     Procedure references: 'INTRTI-PIC X(4)' → 'INTRTI-PICX4'
#     Replace 'INTRTI-PIC X(4) USAGE' first (strips the spurious USAGE keyword from declaration)
#     Also handle already-transformed 'INTRTI-PICX4 USAGE' (idempotent, in case run twice)
content = content.replace('INTRTI-PIC X(4) USAGE', 'INTRTI-PICX4')
content = content.replace('INTRTI-PIC X(4)', 'INTRTI-PICX4')
content = content.replace('INTRTI-PICX4 USAGE', 'INTRTI-PICX4')

# 17. Fix SAMPLE006 colon STRING literals: 'PRG-POS-x ':' → 'PRG-POS-x :'
#     The standalone ':' literal is mis-tokenized as a DB2 host variable prefix
#     Pattern: 'PRG-POS-N '':' → merge into single literal 'PRG-POS-N :'
content = re.sub(
    r\"'(PRG-POS-\d+ )':'\",
    lambda m: \"'\" + m.group(1) + \":'\",
    content
)

# 18. Fix REPORT001: insert CONTINUE after IF BDC-FI01-OK when THEN body
#     is empty (only comments before ELSE) to avoid extraneous ELSE error
content = re.sub(
    r'(IF\s+BDC-FI01-OK)((\s*\n(?:\s{0,6}\*[^\n]*)*)(\s*\n\s+ELSE))',
    r'\1\n           CONTINUE\2',
    content,
    flags=re.IGNORECASE
)

# 19. Comment out all COBOL debug lines (column 7 = 'D').
# IBM COBOL debug lines are conditionally compiled with WITH DEBUGGING MODE.
# smojol may mis-parse continuation D-lines as code, causing parse failures.
# Safe for static analysis: replace 'D' in col 7 with '*' (comment indicator).
# Assumes blank-format source (cols 1-6 are spaces, not sequence numbers).
content = re.sub(
    r'^(      )D(.*)$',
    r'\1*DBG\2',
    content,
    flags=re.MULTILINE
)

# 20. Fix CRECUST: EXEC CICS DELAY FOR SECONDS(n) uses CICS v5.1+ syntax
#     not supported by cobol-rekt grammar (creates null parse tree node)
#     Comment out DELAY FOR SECONDS blocks
content = re.sub(
    r'([ ]{6,})EXEC\s+CICS\s+DELAY\s+FOR\s+SECONDS\b.*?END-EXEC\.?',
    lambda m: re.sub(r'^(.{6})', r'\g<1>*CIC>', m.group(0), flags=re.MULTILINE),
    content,
    flags=re.DOTALL | re.IGNORECASE
)

# 21. Fix CRECUST: EXEC CICS GET/PUT CONTAINER with FLENGTH option is CICS v5.1+
#     syntax not supported by the dialect JAR (causes null ParseTree child NPE)
#     Comment out any EXEC CICS block that uses the FLENGTH option
def comment_out_cics_block(m):
    block = m.group(0)
    return re.sub(r'^(.{6})', r'\g<1>*CIC>', block, flags=re.MULTILINE)

content = re.sub(
    r'[ ]{6,}EXEC\s+CICS\s+(?:GET|PUT)\s+CONTAINER\b(?:(?!END-EXEC).)*FLENGTH(?:(?!END-EXEC).)*END-EXEC\.?',
    comment_out_cics_block,
    content,
    flags=re.DOTALL | re.IGNORECASE
)

# 22. Fix CRECUST: DATESEP('.') — period inside quotes causes null parse tree child
#     in buildDialectNodeRepository. Strip the explicit argument to use the default.
content = re.sub(r'\bDATESEP\s*\([^)]*\)', 'DATESEP', content)

# 23. Fix orphaned IF bodies: when a CICS IF condition line is already commented out
#     (developer left IF body + END-IF uncommented), COBOL LS parser sees dangling END-IF.
#     Detect commented IF line (col 7 = '*', contains IF ... DFHRESP) then comment out
#     all subsequent non-comment statements until the matching END-IF.
def fix_orphaned_if_bodies(text):
    result = []
    in_orphan = False
    for ln in text.split('\n'):
        if re.match(r'^.{6}\*.*\bIF\b.*\bDFHRESP\b', ln, re.IGNORECASE):
            in_orphan = True
            result.append(ln)
        elif in_orphan:
            if re.match(r'^.{6}\*', ln):
                result.append(ln)
            elif re.match(r'^\s*END-IF\b', ln, re.IGNORECASE):
                commented = (ln[:6] + '*' + ln[7:]) if len(ln) >= 7 else ln
                result.append(commented)
                in_orphan = False
            elif ln.strip() == '':
                result.append(ln)
            elif re.match(r'^\s{7,8}\S.*\.\s*$', ln):
                in_orphan = False
                result.append(ln)
            else:
                commented = (ln[:6] + '*' + ln[7:]) if len(ln) >= 7 else ln
                result.append(commented)
        else:
            result.append(ln)
    return '\n'.join(result)

content = fix_orphaned_if_bodies(content)

# 24. Fix CRECUST: DISPLAY statements with 'EXEC CICS' in string literals cause
#     buildDialectNodeRepository NPE (grammar creates partial _DIALECT_N tokens for
#     the string content). Comment out the DISPLAY and all its continuation lines.
def fix_display_exec_cics(text):
    result = []
    display_indent = None
    for ln in text.split('\n'):
        is_comment = len(ln) >= 7 and ln[6] == '*'
        if display_indent is None:
            m = re.match(r'^(\s+)DISPLAY\s', ln, re.IGNORECASE)
            if not is_comment and m and re.search(r\"'[^']*EXEC\s+CICS[^']*'\", ln, re.IGNORECASE):
                display_indent = len(m.group(1))
                commented = (ln[:6] + '*' + ln[7:]) if len(ln) >= 7 else ln
                result.append(commented)
            else:
                result.append(ln)
        else:
            if ln.strip() == '':
                result.append(ln)
            elif len(ln) - len(ln.lstrip()) > display_indent:
                commented = (ln[:6] + '*' + ln[7:]) if len(ln) >= 7 else ln
                result.append(commented)
            else:
                display_indent = None
                result.append(ln)
    return '\n'.join(result)

content = fix_display_exec_cics(content)

# Step 25: Replace LENGTH OF <identifier> in COMPUTE statements.
# smojol's COBOL Language Support grammar treats LENGTH OF as a CICS
# dialect token, producing _DIALECT_N nodes with null children → NPE
# in buildDialectNodeRepository when the referenced variable has children.
def fix_length_of(text):
    result = []
    for ln in text.split('\n'):
        is_comment = len(ln) >= 7 and ln[6] == '*'
        if not is_comment:
            ln = re.sub(r'\bLENGTH\s+OF\s+\w+(?:-\w+)*\b', '0', ln, flags=re.IGNORECASE)
        result.append(ln)
    return '\n'.join(result)

content = fix_length_of(content)

# Step 26: Replace DFHVALUE(...) with 0.
# DFHVALUE is a CICS compile-time constant that resolves dialect values.
# The COBOL Language Support grammar creates an empty _DIALECT_N node for
# DFHVALUE tokens, causing a NullPointerException in buildDialectNodeRepository.
# Replacing with 0 is safe for static analysis.
def fix_dfhvalue(text):
    result = []
    for ln in text.split('\n'):
        is_comment = len(ln) >= 7 and ln[6] == '*'
        if not is_comment:
            ln = re.sub(r'\bDFHVALUE\s*\(\s*\w+(?:-\w+)*\s*\)', '0', ln, flags=re.IGNORECASE)
        result.append(ln)
    return '\n'.join(result)

content = fix_dfhvalue(content)

# Step 27: Fix compound IF conditions where a bare condition-name test is
# followed by AND on the next line.
# smojol's ConditionVisitor.visitCondition() tries to call getComparison()
# on every sub-condition, but condition-name tests have no comparison →
# NullPointerException. Fix: drop the condition-name line, promote AND to IF.
def fix_bare_condname_and(text):
    import re
    lines = text.split('\n')
    result = []
    i = 0
    while i < len(lines):
        ln = lines[i]
        is_comment = len(ln) >= 7 and ln[6] == '*'
        if not is_comment:
            # Match: IF <identifier> (only, no operator, no parens, no comparison)
            m = re.match(
                r'^(\s{6,}IF\s+)(NOT\s+)?([A-Z][A-Z0-9-]*)(\s*)$',
                ln, re.IGNORECASE)
            if m and i + 1 < len(lines):
                next_ln = lines[i + 1]
                is_next_comment = len(next_ln) >= 7 and next_ln[6] == '*'
                # Next active line must start with AND
                if (not is_next_comment and
                        re.match(r'^\s+AND\s+', next_ln, re.IGNORECASE)):
                    # Replace: comment out the IF condition-name line,
                    # change the AND line to IF (preserving indentation)
                    result.append(re.sub(r'^(\s{6})', r'\g<1>*', ln)
                                  if len(ln) >= 6 else '*' + ln)
                    next_fixed = re.sub(
                        r'^(\s+)AND\s+', r'\1IF  ', next_ln, flags=re.IGNORECASE)
                    result.append(next_fixed)
                    i += 2
                    continue
        result.append(ln)
        i += 1
    return '\n'.join(result)

content = fix_bare_condname_and(content)

# Step 28: Replace IF conditions that use IN-qualified field names within
# arithmetic expressions (e.g., IF (A IN G1 - A IN G2) <= N).
# smojol cannot build a RelationExpression for arithmetic on IN-qualified
# names → NullPointerException in ConditionVisitor. Replace with IF TRUE
# and comment out all continuation condition lines (AND/OR clauses).
def fix_in_arithmetic_condition(text):
    import re
    _STMT_KW = re.compile(
        r'^(IF\b|ELSE\b|END-IF\b|MOVE\b|COMPUTE\b|PERFORM\b|CONTINUE\b|'
        r'ADD\b|SUBTRACT\b|MULTIPLY\b|DIVIDE\b|SET\b|DISPLAY\b|'
        r'STRING\b|UNSTRING\b|EVALUATE\b|WHEN\b|STOP\b|GO\s+TO\b|'
        r'INITIALIZE\b|INSPECT\b|READ\b|WRITE\b|REWRITE\b|DELETE\b|'
        r'OPEN\b|CLOSE\b|ACCEPT\b|CALL\b|EXEC\b)',
        re.IGNORECASE)
    _COND_CONT = re.compile(r'^(AND|OR)\b', re.IGNORECASE)

    lines = text.split('\n')
    result = []
    i = 0
    while i < len(lines):
        ln = lines[i]
        is_comment = len(ln) >= 7 and ln[6] == '*'
        if not is_comment:
            m_if = re.match(r'^(\s{6,})(IF\s+)', ln, re.IGNORECASE)
            if m_if:
                after_if = ln[len(m_if.group(0)):]
                has_in_arith = bool(re.search(
                    r'\b\w+(?:-\w+)*\s+IN\s+\w+(?:-\w+)*\s*[-+*/]',
                    after_if, re.IGNORECASE))
                has_in_compare = bool(re.search(
                    r'\b\w+(?:-\w+)*\s+IN\s+\w+(?:-\w+)*\s*'
                    r'(?:>|<|NOT\s*=|=\s*(?!>)|>=|<=)',
                    after_if, re.IGNORECASE))
                if has_in_arith or has_in_compare:
                    indent = m_if.group(1) + m_if.group(2)
                    result.append(indent + 'TRUE')
                    # Comment out all continuation condition lines
                    i += 1
                    while i < len(lines):
                        cont = lines[i]
                        is_cont_comment = len(cont) >= 7 and cont[6] == '*'
                        if is_cont_comment:
                            result.append(cont)
                            i += 1
                            continue
                        stripped = cont.strip()
                        # AND/OR always continues the condition
                        if _COND_CONT.match(stripped):
                            result.append(re.sub(r'^(\s{6})', r'\g<1>*', cont)
                                          if len(cont) >= 6 else '*' + cont)
                            i += 1
                            continue
                        # Non-empty, non-statement → condition continuation
                        if stripped and not _STMT_KW.match(stripped):
                            result.append(re.sub(r'^(\s{6})', r'\g<1>*', cont)
                                          if len(cont) >= 6 else '*' + cont)
                            i += 1
                            continue
                        break
                    continue
        result.append(ln)
        i += 1
    return '\n'.join(result)

content = fix_in_arithmetic_condition(content)

# 29. Add END-STRING before END-IF/ELSE when a STRING statement is not explicitly
# terminated with END-STRING. smojol requires explicit scope terminators.
# Detects: lines with 'DELIMITED BY SIZE INTO ...' immediately followed (ignoring
# blank/comment lines) by END-IF or ELSE with no intervening END-STRING.
def fix_string_no_end_string(text):
    import re
    _DELIM_INTO = re.compile(r'DELIMITED\s+BY\s+(?:SIZE|\S+)\s+INTO\s+\S', re.IGNORECASE)
    _END_STRING = re.compile(r'^\s*END-STRING\s*$', re.IGNORECASE)
    # Excludes WHEN: STRING cannot span EVALUATE WHEN boundaries; including WHEN would corrupt EVALUATE.
    _SCOPE_CLOSE = re.compile(r'^\s*(END-IF|ELSE)\b', re.IGNORECASE)

    lines = text.split('\n')
    result = []
    i = 0
    in_string_stmt = False

    while i < len(lines):
        ln = lines[i]
        stripped = ln.strip()
        is_comment = len(ln) >= 7 and ln[6] == '*'

        if not is_comment:
            if re.match(r'STRING\s', stripped, re.IGNORECASE):
                in_string_stmt = True
            elif _END_STRING.match(ln):
                in_string_stmt = False
            elif in_string_stmt and _DELIM_INTO.search(ln):
                # This line ends the STRING body; peek ahead for END-IF/ELSE without END-STRING
                result.append(ln)
                i += 1
                buffered = []
                found_end_string = False
                while i < len(lines):
                    nxt = lines[i]
                    nxt_stripped = nxt.strip()
                    nxt_comment = len(nxt) >= 7 and nxt[6] == '*'
                    if not nxt_stripped or nxt_comment:
                        buffered.append(nxt)
                        i += 1
                        continue
                    if _END_STRING.match(nxt):
                        found_end_string = True
                    elif _SCOPE_CLOSE.match(nxt) and not found_end_string:
                        # Insert END-STRING before END-IF/ELSE
                        indent = re.match(r'^(\s*)', nxt).group(1)
                        result.extend(buffered)
                        result.append(indent + 'END-STRING')
                        buffered = []
                    in_string_stmt = False
                    break
                result.extend(buffered)
                continue

        result.append(ln)
        i += 1
    return '\n'.join(result)

content = fix_string_no_end_string(content)

# Strip Endevor-style mainframe audit stamps that appear at end-of-line.
# These show up in old z/OS code as a date in DD.MM.YY format after any
# token — even with just one trailing space (the original 73-80 column
# 'identification area' bled into program area when read in free format):
#     05 CTA-LEER-REGISTRO  PIC X(6) VALUE 'LEER'.    12.02.16
#     MOVE CTN-155027   TO COD-AVIERROR-QPIPCCAB 12.02.16
#     WHERE COD_PAISOALF = :DCLTKYGHLPE.LPE-COD-PAISOALF 15.08.20
# Strict pattern (exactly two digits, dot, two digits, dot, two digits)
# preceded by whitespace and ending the line — won't false-positive on
# real COBOL numerics. Skip comment lines (col 7 = '*').
#
# Bash inside heredoc eats unescaped \$, so we use \\\$ to get \$ in py.
import re as _r_stamp
_audit_rx = _r_stamp.compile(r'\s+\d{2}\.\d{2}\.\d{2}\s*\$')
def _strip_audit_stamps(text):
    out = []
    for line in text.split('\n'):
        if len(line) >= 7 and line[6] == '*':
            out.append(line); continue
        out.append(_audit_rx.sub('', line))
    return '\n'.join(out)

content = _strip_audit_stamps(content)

if content != original:
    with open('$PREPROC_DIR/$fname', 'w', encoding='latin-1') as f:
        f.write(content)
    sys.exit(0)
else:
    sys.exit(1)
" 2>/dev/null && cbl_count=$((cbl_count + 1))
done < <(find "$SOURCE_DIR" \
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

# ─── Phase 3: Handle >8-char copybook names ─────────────────────────
# smojol warns about copybook names >8 characters. Create 8-char .cpy
# aliases in the source dir so the parser can resolve them.
while IFS= read -r -d '' cpy; do
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
done < <(find "$SOURCE_DIR" \
    \( -name "*.cpy" -o -name "*.CPY" \) \
    -type f \
    ! -path "*/.rekt-staging/*" \
    ! -path "*/.preprocessed/*" \
    ! -path "*/.convert-*/*" \
    -print0)

# ─── Phase 3.5: Ship bundled system copybooks (SQLCA, etc.) ─────────
# Some "system" copybooks (SQLCA, DFHCOMMAREA, …) are normally provided
# by the host compiler or by Eclipse-LSP-COBOL's auto-inject. In CLI
# mode smojol does NOT auto-inject them, so any program with
# `EXEC SQL INCLUDE SQLCA` or `COPY SQLCA` fails with MISSING_COPYBOOK
# and is forced into deps-only mode. To unblock the common case we ship
# real, IBM-public definitions in tools/system-copybooks/ and copy them
# into .preprocessed/ unless the user supplied their own.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SYS_CPY_DIR="$SCRIPT_DIR/system-copybooks"
sys_count=0
if [[ -d "$SYS_CPY_DIR" ]]; then
    while IFS= read -r -d '' sys_cpy; do
        sys_name=$(basename "$sys_cpy")
        sys_base=$(echo "$sys_name" | sed 's/\.[^.]*$//' | tr '[:lower:]' '[:upper:]')
        # If user has a real copybook of the same name in source/, prefer theirs
        user_match=$(find "$SOURCE_DIR" \
            \( -iname "${sys_base}.cpy" -o -iname "${sys_base}.CPY" -o -iname "${sys_base}.cpb" \) \
            -type f \
            ! -path "*/.rekt-staging/*" \
            ! -path "*/.preprocessed/*" \
            ! -path "*/.convert-*/*" \
            | head -1)
        if [[ -n "$user_match" ]]; then continue; fi
        # Don't clobber an already-preprocessed version
        if [[ -f "$PREPROC_DIR/$sys_name" ]]; then continue; fi
        cp "$sys_cpy" "$PREPROC_DIR/$sys_name"
        sys_count=$((sys_count + 1))
    done < <(find "$SYS_CPY_DIR" -maxdepth 1 -type f \( -name "*.cpy" -o -name "*.CPY" \) -print0)
fi
if [[ "$sys_count" -gt 0 ]]; then
    echo "  Shipped $sys_count bundled system copybook(s) (e.g. SQLCA) → $PREPROC_DIR/"
fi

# ─── Phase 4: Auto-stub missing copybooks ────────────────────────────
# smojol/Eclipse-LSP-COBOL treats unresolved COPY targets as fatal errors,
# which forces the affected program into deps-only mode (no AST). For
# enterprise estates where many copybooks live on the mainframe and can't
# easily be exported, this means 30+ programs fail parsing.
#
# This phase generates a minimal stub .cpy file in the .preprocessed/
# directory for every COPY target that isn't already satisfied by a real
# file in source/. The stubs contain valid COBOL: a single PIC X(1) field
# wrapped in a 01-level group so they parse cleanly without polluting
# data semantics. The converter prompt can later note that stubs were
# used so the LLM falls back to source-based field inference.
#
# To opt OUT: REKT_NO_STUB_COPYBOOKS=true ./doctor.sh rekt-full
# ─────────────────────────────────────────────────────────────────────
if [[ "${REKT_NO_STUB_COPYBOOKS:-false}" != "true" ]]; then
    # System copybooks that smojol/Eclipse-LSP-COBOL handles natively or that
    # the runtime provides — we must NOT stub these because our minimal stub
    # would override the real built-in definition and break programs that
    # depend on the proper field layout (e.g. SQLCA has SQLCODE, SQLSTATE,
    # SQLERRD, etc. — programs read SQLCODE constantly).
    system_copybooks_pattern="^(SQLCA|SQLDA|SQLD2|DFHCOMMAREA|DFHEIB|DFHBMSCA|EIBAID|EIBCALEN|DSNHLI|DSNTIAR)$"
    # Collect all COPY targets referenced anywhere in the source tree
    # (case-insensitive; strip surrounding quotes; uppercase for matching).
    referenced=$($PYTHON - "$SOURCE_DIR" <<'PYEOF'
import os, re, sys
src = sys.argv[1]
# Avoid escaped quotes inside the regex — use a character class instead
# so the bash heredoc parser doesn't get confused. Match COPY <NAME> with
# optional surrounding single or double quote.
rx_copy = re.compile(
    r"^\s*COPY\s+[\"']?([A-Z0-9$@#\-_]+)[\"']?",
    re.IGNORECASE | re.MULTILINE)
names = set()
for root, dirs, files in os.walk(src):
    # Skip transient directories
    if "/.rekt-staging" in root or "/.preprocessed" in root or "/.convert-" in root:
        continue
    for f in files:
        if not f.lower().endswith((".cbl", ".cob", ".cpy")): continue
        try:
            with open(os.path.join(root, f), "r", encoding="utf-8", errors="ignore") as fp:
                for m in rx_copy.finditer(fp.read()):
                    names.add(m.group(1).upper())
        except: pass
print("\n".join(sorted(names)))
PYEOF
)

    # Build the set of already-satisfied copybook basenames (uppercase, no ext)
    satisfied=$(find "$SOURCE_DIR" \
        \( -name "*.cpy" -o -name "*.CPY" -o -name "*.cpb" \) \
        -type f \
        ! -path "*/.rekt-staging/*" \
        ! -path "*/.preprocessed/*" \
    ! -path "*/.convert-*/*" \
        ! -path "*/.convert-*/*" \
        -exec basename {} \; \
        | sed 's/\.[^.]*$//' \
        | tr '[:lower:]' '[:upper:]' \
        | sort -u)

    stub_count=0
    while IFS= read -r name; do
        [[ -z "$name" ]] && continue
        # Skip if a real copybook exists
        if echo "$satisfied" | grep -qFx "$name"; then continue; fi
        # Skip system copybooks (smojol/runtime provides them; our stub
        # would override and break programs that read SQLCODE etc.)
        if [[ "$name" =~ $system_copybooks_pattern ]]; then continue; fi
        # Skip if a stub already exists from a previous run
        stub_path="$PREPROC_DIR/${name}.cpy"
        if [[ -f "$stub_path" ]]; then continue; fi

        # Generate a minimal stub. The marker comment is recognisable so
        # downstream tooling can flag programs that depend on stubs.
        cat > "$stub_path" <<EOF
      *>───────────────────────────────────────────────────────────────
      *> AUTO-GENERATED STUB COPYBOOK for ${name}
      *> Real copybook was not found in source/.
      *> This stub satisfies the COPY directive so smojol can produce
      *> full-fidelity AST instead of falling into deps-only mode.
      *> Field semantics are unknown — the converter should derive them
      *> from source context (paragraph names, MOVE statements).
      *>───────────────────────────────────────────────────────────────
       01  ${name:0:25}-STUB.
           05  ${name:0:23}-VAL PIC X(1).
EOF
        stub_count=$((stub_count + 1))
    done <<< "$referenced"

    if [[ "$stub_count" -gt 0 ]]; then
        echo "  Generated $stub_count stub copybook(s) for unresolved COPY targets → $PREPROC_DIR/"
        echo "  (Set REKT_NO_STUB_COPYBOOKS=true to opt out)"
    fi
fi
