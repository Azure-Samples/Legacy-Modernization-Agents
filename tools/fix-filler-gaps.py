#!/usr/bin/env python3
"""Fix FILLER insertions where gap wasn't shrunk, causing col 72 overflow."""
import re
import glob
import sys

source_dir = sys.argv[1] if len(sys.argv) > 1 else "source"

def fix_filler(m):
    prefix = m.group(1)
    level = m.group(2)
    gap = m.group(3)
    kw = m.group(4)
    # Shrink gap to compensate for 'FILLER ' (7 chars)
    new_gap_len = max(1, len(gap) - 7)
    return prefix + level + " " * new_gap_len + "FILLER " + kw

fixed = 0
for pattern in [f"{source_dir}/*.cpy", f"{source_dir}/*.cbl"]:
    for path in glob.glob(pattern):
        with open(path, "r", encoding="latin-1") as f:
            content = f.read()

        original = content

        # Fix lines where FILLER was already inserted but gap wasn't shrunk
        content = re.sub(
            r"^(\s*\d{0,6}\s+)(\d{2})(\s+)FILLER (PIC\b)",
            fix_filler,
            content,
            flags=re.MULTILINE,
        )
        content = re.sub(
            r"^(\s*\d{0,6}\s+)(\d{2})(\s+)FILLER (REDEFINES\b)",
            fix_filler,
            content,
            flags=re.MULTILINE,
        )

        if content != original:
            with open(path, "w", encoding="latin-1") as f:
                f.write(content)
            print(f"  Fixed: {path}")
            fixed += 1

print(f"  Fixed {fixed} file(s)")
