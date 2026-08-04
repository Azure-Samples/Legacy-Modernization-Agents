#!/bin/bash
# Normalizes COBOL sources for rekt compatibility.

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

# Preprocess copybooks before programs.
cpy_count=0
# Recursive find so nested layouts (source/lib/cpy/, etc.) are picked up.
# -print0 / read -d '' avoids issues with paths containing whitespace.
while IFS= read -r -d '' cpy; do
    fname=$(basename "$cpy")

    "$PYTHON" -c "
import re, sys

with open('$cpy', 'r', encoding='latin-1') as f:
    content = f.read()

original = content

# Strip sequence numbers that leaked from columns 73-80 into source text.
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

# Add FILLER to anonymous data entries, with or without sequence numbers.
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

# Truncate identification-area metadata; compress whitespace only for actual overflow code.
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

# Remove Endevor date/time audit stamps that the parser mistakes for COBOL tokens.
content = _re_module.sub(
    r'(\.)\s+(\d{1,4}[.\-/]\d{1,2}[.\-/]\d{1,4})\s*$',
    r'\1', content, flags=_re_module.MULTILINE)
# Also remove audit stamps without a preceding period.
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

# Comment out section labels that become invalid when SAMPLE copybooks are included inline.
bname = '$fname'.upper()
if bname.startswith('SAMPLE') and not bname.endswith('I.CPY'):
    content = re.sub(
        r'^(\s*\d{0,6})( )([A-Z][A-Z0-9-]+\s+SECTION\s*\.\s*)$',
        r'\1*\3',
        content,
        flags=re.MULTILINE | re.IGNORECASE
    )

# Replace unsupported MOVE CORR/CORRESPONDING statements, including multiline forms.
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

# Preprocess COBOL programs.
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

# Replace European decimal commas in numeric literals.
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

# Rewrite REPORT001's unsupported 88-level PERFORM condition before column enforcement.
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

# Normalize the malformed INTRTI-PIC X(4) data name and its references.
content = content.replace('INTRTI-PIC X(4) USAGE', 'INTRTI-PICX4')
content = content.replace('INTRTI-PIC X(4)', 'INTRTI-PICX4')
content = content.replace('INTRTI-PICX4 USAGE', 'INTRTI-PICX4')

# Merge SAMPLE006's standalone colon literal to avoid DB2 host-variable tokenization.
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

# Treat fixed-format debug lines as comments for static analysis.
content = re.sub(
    r'^(      )D(.*)$',
    r'\1*DBG\2',
    content,
    flags=re.MULTILINE
)

# Comment out unsupported CICS DELAY FOR SECONDS blocks.
content = re.sub(
    r'([ ]{6,})EXEC\s+CICS\s+DELAY\s+FOR\s+SECONDS\b.*?END-EXEC\.?',
    lambda m: re.sub(r'^(.{6})', r'\g<1>*CIC>', m.group(0), flags=re.MULTILINE),
    content,
    flags=re.DOTALL | re.IGNORECASE
)

# Comment out CICS container blocks whose FLENGTH option breaks the dialect parser.
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

# Comment out orphaned CICS IF bodies through their matching END-IF.
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

# Comment out DISPLAY strings that trigger false EXEC CICS dialect nodes.
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

# Replace LENGTH OF expressions that produce invalid dialect nodes.
def fix_length_of(text):
    result = []
    for ln in text.split('\n'):
        is_comment = len(ln) >= 7 and ln[6] == '*'
        if not is_comment:
            ln = re.sub(r'\bLENGTH\s+OF\s+\w+(?:-\w+)*\b', '0', ln, flags=re.IGNORECASE)
        result.append(ln)
    return '\n'.join(result)

content = fix_length_of(content)

# Replace DFHVALUE compile-time constants with 0 for static analysis.
def fix_dfhvalue(text):
    result = []
    for ln in text.split('\n'):
        is_comment = len(ln) >= 7 and ln[6] == '*'
        if not is_comment:
            ln = re.sub(r'\bDFHVALUE\s*\(\s*\w+(?:-\w+)*\s*\)', '0', ln, flags=re.IGNORECASE)
        result.append(ln)
    return '\n'.join(result)

content = fix_dfhvalue(content)

# Drop unsupported bare condition-name clauses and promote the following AND to IF.
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

# Replace unsupported arithmetic on IN-qualified names and its continuation clauses.
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

# Add END-STRING before END-IF or ELSE when explicit scope termination is missing.
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

# Remove strict DD.MM.YY audit stamps while preserving comment lines and COBOL numerics.
# Dollar signs are escaped because this Python block is embedded in a Bash heredoc.
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

# Create eight-character aliases for long copybook names.
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

# Copy bundled system copybooks only when the user has not supplied them.
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

# Generate minimal stubs for unresolved COPY targets; set REKT_NO_STUB_COPYBOOKS=true to disable.
if [[ "${REKT_NO_STUB_COPYBOOKS:-false}" != "true" ]]; then
    # Never stub system copybooks because doing so would hide their required field layouts.
    system_copybooks_pattern="^(SQLCA|SQLDA|SQLD2|DFHCOMMAREA|DFHEIB|DFHBMSCA|EIBAID|EIBCALEN|DSNHLI|DSNTIAR)$"
    # Collect all COPY targets referenced anywhere in the source tree
    # (case-insensitive; strip surrounding quotes; uppercase for matching).
    referenced=$($PYTHON - "$SOURCE_DIR" <<'PYEOF'
import os, re, sys
src = sys.argv[1]
# Use a character class for optional COPY-name quotes to keep the heredoc parser stable.
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
