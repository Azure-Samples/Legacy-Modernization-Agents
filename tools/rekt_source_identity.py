#!/usr/bin/env python3
"""Decide whether source/ can be staged for Cobol-REKT without losing file identity.

REKT resolves COPY targets by basename from one flat directory, so two copybooks with the
same name can only be staged as one. That is safe when they hold the same text, or when
nothing names them in a COPY or INCLUDE: the parse never reads them. It is unsafe only
when they differ and something uses the name, because the parse would then read whichever
copy was staged last.

Usage: rekt_source_identity.py <source-root> <preprocessed-dir> [<shadowed-copybooks-file>]

The optional file lists absolute paths of generated stand-in copybooks that a real copybook
of the same name replaces; they are left out of the check because they are never staged.

Prints one tab-separated line per finding: kind, lower-cased basename, then paths relative
to the source root. Exits 1 when any finding blocks staging.

Blocking kinds:
  copybook-collision              copies differ and a COPY or INCLUDE names them
  copybook-collision-users        follows copybook-collision; files that name it
  preprocessed-program-collision  duplicate program basename with an ambiguous legacy flat rewrite

Informational kinds:
  program-duplicate               same program basename in more than one directory
  copybook-duplicate-identical    copies hold the same text, so one is staged
  copybook-duplicate-unused       copies differ but nothing names them
"""
import os
import re
import sys

PROGRAM_EXTS = {".cbl", ".cob"}
COPYBOOK_EXTS = {".cpy"}
SKIP_DIRS = {".preprocessed", ".rekt-staging"}
# The preprocessor gives long copybook names an alias of this length, and rewrites a long
# COPY name to it, so a reference this long can stand for any longer name it prefixes.
ALIAS_LENGTH = 8

_NAME = r"['\"]?([A-Za-z0-9#@$_-]+)"
REFERENCE_PATTERNS = [
    re.compile(r"(?<![A-Za-z0-9-])COPY\s+" + _NAME, re.IGNORECASE),
    re.compile(r"\bEXEC\s+SQL\s+INCLUDE\s+" + _NAME, re.IGNORECASE),
    re.compile(r"\+\+INCLUDE\s+" + _NAME, re.IGNORECASE),
    re.compile(r"^\s*-INC\s+" + _NAME, re.IGNORECASE),
]


def read_text(path):
    with open(path, "rb") as handle:
        return handle.read().decode("latin-1")


def code_lines(text):
    """Yield source lines with comments removed, in fixed or free format."""
    for line in text.splitlines():
        if len(line) > 6 and line[6] in "*/":
            continue
        if line.lstrip().startswith("*>"):
            continue
        cut = line.find("*>")
        yield line if cut < 0 else line[:cut]


def referenced_names(text):
    names = set()
    for line in dependency_guard_text(text).splitlines():
        for pattern in REFERENCE_PATTERNS:
            for match in pattern.finditer(line):
                names.add(match.group(1).upper())
    return names


_IDENTIFICATION = re.compile(r"\s*(?:IDENTIFICATION|ID)\s+DIVISION\b", re.IGNORECASE)
_LATER_DIVISION = re.compile(r"\s*(?:ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b", re.IGNORECASE)
_COMMENT_ENTRY = re.compile(r"\s*(?:AUTHOR|INSTALLATION|DATE-WRITTEN|DATE-COMPILED|SECURITY|REMARKS)\s*\.",
                            re.IGNORECASE)
_PARAGRAPH = re.compile(r"\s*(?:PROGRAM-ID|AUTHOR|INSTALLATION|DATE-WRITTEN|DATE-COMPILED|SECURITY|REMARKS)\b",
                        re.IGNORECASE)


def program_text_lines(text):
    """Blank sequence areas and IDENTIFICATION comment-entries, keeping line and column positions."""
    free = re.search(r">>\s*SOURCE\s+(?:FORMAT\s+)?(?:IS\s+)?FREE\b", text, re.IGNORECASE) is not None
    identification = documentary = False
    for raw in text.splitlines():
        fixed = (not free and len(raw) > 6 and raw[6] in " -*/Dd"
                 and all(c == " " or c.isascii() and c.isdigit() for c in raw[:6]))
        line = "      " + raw[6:72] if fixed else raw
        area = line[7:] if fixed else line
        if _IDENTIFICATION.match(area):
            identification = True
        elif _LATER_DIVISION.match(area):
            identification = documentary = False
        elif documentary and (_PARAGRAPH.match(area) or (fixed and area[:4].strip())):
            documentary = False
        # Comment-entries are free text, so an apostrophe there must not open a literal.
        if identification and (entry := _COMMENT_ENTRY.match(area)):
            documentary = True
            yield line[:len(line) - len(area) + entry.end()]
        else:
            yield "" if documentary else line


def dependency_guard_text(text):
    """Mask literals/comments for contradiction checks, retaining quoted dependency operands."""
    result, quote, operand = [], None, False
    for line in program_text_lines(text):
        if (len(line) > 6 and line[6] in "*/") or line.lstrip().startswith("*>"):
            continue
        row, index = [], 0
        continuation_quote = None
        if quote and len(line) > 6 and line[6] == "-":
            start = 7
            while start < len(line) and line[start].isspace():
                start += 1
            if start < len(line) and line[start] == quote:
                continuation_quote = start
        while index < len(line):
            char = line[index]
            if index == continuation_quote:
                row.append(" ")
            elif quote:
                row.append(char if operand else " ")
                if char == quote:
                    if index + 1 < len(line) and line[index + 1] == quote:
                        row.append(char if operand else " ")
                        index += 1
                    else:
                        quote, operand = None, False
            elif line[index:index + 2] == "*>":
                break
            elif char in "'\"":
                prefix = "\n".join(result[-1:]) + "".join(row)
                operand = re.search(r"\b(?:COPY|CALL|INCLUDE)\s*$", prefix, re.IGNORECASE) is not None
                quote = char
                row.append(char if operand else " ")
            else:
                row.append(char)
            index += 1
        result.append("".join(row))
    return "\n".join(result)


def normalised(text):
    return " ".join(text.split())


def stem(basename):
    return os.path.splitext(basename)[0].upper()


def is_used(copybook_stem, references):
    if copybook_stem in references:
        return True
    if len(copybook_stem) > ALIAS_LENGTH:
        return copybook_stem[:ALIAS_LENGTH] in references
    return False


def scan(source_root, shadowed=frozenset()):
    programs, copybooks, references = {}, {}, {}
    for root, dirs, files in os.walk(source_root):
        if os.environ.get("REKT_SOURCE_TOP_LEVEL_ONLY") == "true":
            dirs[:] = []
        else:
            dirs[:] = [d for d in dirs if d not in SKIP_DIRS and not d.startswith(".convert-")]
        for name in files:
            path = os.path.join(root, name)
            rel = os.path.relpath(path, source_root).replace(os.sep, "/")
            ext = os.path.splitext(name)[1].lower()
            if ext in PROGRAM_EXTS:
                programs.setdefault(name.lower(), []).append(rel)
            elif ext in COPYBOOK_EXTS:
                if path in shadowed:
                    continue
                copybooks.setdefault(name.lower(), []).append(rel)
            else:
                continue
            for referenced in referenced_names(read_text(path)):
                references.setdefault(referenced, set()).add(rel)
    return programs, copybooks, references


def users_of(copybook_stem, references):
    users = set(references.get(copybook_stem, ()))
    if len(copybook_stem) > ALIAS_LENGTH:
        users |= references.get(copybook_stem[:ALIAS_LENGTH], set())
    return users


def check(source_root, preproc_dir, shadowed=frozenset()):
    programs, copybooks, references = scan(source_root, shadowed)

    preprocessed = {}
    if os.path.isdir(preproc_dir):
        for name in os.listdir(preproc_dir):
            if os.path.isfile(os.path.join(preproc_dir, name)):
                preprocessed[name.lower()] = os.path.join(preproc_dir, name)

    findings, errors = [], []
    for basename, paths in sorted(copybooks.items()):
        if len(paths) < 2:
            continue
        paths = sorted(paths)
        contents = {normalised(read_text(os.path.join(source_root, p))) for p in paths}
        copybook_stem = stem(basename)
        if len(contents) == 1:
            findings.append(("copybook-duplicate-identical", basename, paths))
        elif not is_used(copybook_stem, references):
            findings.append(("copybook-duplicate-unused", basename, paths))
        else:
            errors.append(("copybook-collision", basename, paths))
            errors.append(("copybook-collision-users", basename,
                           sorted(users_of(copybook_stem, references) - set(paths))))

    for basename, paths in sorted(programs.items()):
        if len(paths) > 1:
            paths = sorted(paths)
            findings.append(("program-duplicate", basename, paths))
            if basename in preprocessed:
                from rekt_dependency_evidence import current_transform
                root_program = next((path for path in paths if "/" not in path), None)
                if root_program is None or current_transform(
                        preprocessed[basename], os.path.join(source_root, root_program)) is None:
                    errors.append(("preprocessed-program-collision", basename, paths))

    return findings, errors


def read_shadowed(path):
    if not path or not os.path.isfile(path):
        return frozenset()
    with open(path, encoding="utf-8") as handle:
        return frozenset(line.rstrip("\n") for line in handle if line.strip())


def main(argv):
    if len(argv) not in (3, 4):
        print(__doc__.strip().splitlines()[0], file=sys.stderr)
        print("usage: rekt_source_identity.py <source-root> <preprocessed-dir> [<shadowed-copybooks-file>]", file=sys.stderr)
        return 2
    findings, errors = check(argv[1], argv[2], read_shadowed(argv[3] if len(argv) == 4 else None))
    for kind, basename, paths in findings + errors:
        print("\t".join([kind, basename] + list(paths)))
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
