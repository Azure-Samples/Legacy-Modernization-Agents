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
  preprocessed-program-collision  duplicate program basename that the preprocessor rewrote

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
    for line in code_lines(text):
        for pattern in REFERENCE_PATTERNS:
            for match in pattern.finditer(line):
                names.add(match.group(1).upper())
    return names


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

    preprocessed = set()
    if os.path.isdir(preproc_dir):
        for name in os.listdir(preproc_dir):
            if os.path.isfile(os.path.join(preproc_dir, name)):
                preprocessed.add(name.lower())

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
