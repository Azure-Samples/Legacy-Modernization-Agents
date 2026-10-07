#!/usr/bin/env python3
"""Generate COBOL symbolic map copybooks from CICS BMS mapset sources.

On z/OS the BMS assembler step writes a symbolic map copybook (``<map>I`` / ``<map>O``) for
each mapset, and programs ``COPY`` it. Repositories usually ship only the ``*.bms`` source, so
without this step the copybook is missing, gets an empty stub, and every program that moves
data to or from a screen field fails to parse with full fidelity.

Usage: bms_symbolic_map.py <source-dir> <output-dir>

The copybook is named after the BMS source file, as z/OS builds name the member, not after
the mapset label (two sources may declare the same label). A real copybook of that name in
the source tree always wins, and only files this tool or the stub generator wrote are
replaced.
"""

from __future__ import annotations

import os
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path

GENERATED_MARKER = "SYMBOLIC MAP GENERATED FROM BMS"
STUB_MARKER = "AUTO-GENERATED STUB COPYBOOK"
COPYBOOK_EXTENSIONS = (".cpy", ".cpb", ".copy")
SKIPPED_DIRS = (".rekt-staging", ".preprocessed")

# Extended attribute bytes, in the order CICS lays them out, and their name suffixes.
ATTRIBUTE_SUFFIXES = (
    ("COLOR", "C"),
    ("PS", "P"),
    ("HILIGHT", "H"),
    ("VALIDN", "V"),
    ("OUTLINE", "U"),
    ("SOSI", "M"),
    ("TRANSP", "T"),
)
EXTATT_YES = ("COLOR", "HILIGHT", "PS", "VALIDN")
TIOA_PREFIX_BYTES = 12


@dataclass
class BmsField:
    name: str
    length: int
    occurs: int = 1
    picin: str | None = None
    picout: str | None = None


@dataclass
class BmsMap:
    name: str
    attributes: tuple[str, ...]
    tioapfx: bool
    fields: list[BmsField] = field(default_factory=list)


@dataclass
class BmsMapset:
    name: str
    mode: str
    lang: str
    maps: list[BmsMap] = field(default_factory=list)


@dataclass
class Statement:
    label: str | None
    macro: str
    operands: dict[str, object]


def _ends_inside_quote(text: str) -> bool:
    return text.count("'") % 2 == 1


def logical_lines(text: str) -> list[str]:
    """Join assembler continuation lines (non-blank column 72, resume in column 16)."""
    result: list[str] = []
    current: str | None = None
    for raw in text.splitlines():
        line = raw.rstrip("\r\n").expandtabs()
        if current is None:
            if not line.strip() or line.startswith("*") or line.startswith(".*"):
                continue
        continued = len(line) > 71 and line[71] != " "
        body = line[:71]
        if current is None:
            current = body
        else:
            part = body[15:] if len(body) > 15 else ""
            current += part if _ends_inside_quote(current) else part.lstrip()
        if continued:
            if not _ends_inside_quote(current):
                current = current.rstrip()
            continue
        result.append(current.rstrip())
        current = None
    if current is not None:
        result.append(current.rstrip())
    return result


def _split_top_level(text: str, separator: str) -> list[str]:
    parts, depth, quoted, start = [], 0, False, 0
    for i, ch in enumerate(text):
        if ch == "'":
            quoted = not quoted
        elif not quoted:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
            elif ch == separator and depth == 0:
                parts.append(text[start:i])
                start = i + 1
    parts.append(text[start:])
    return parts


def _operand_field(text: str) -> str:
    """The operand field ends at the first blank outside quotes and parentheses."""
    depth, quoted = 0, False
    for i, ch in enumerate(text):
        if ch == "'":
            quoted = not quoted
        elif not quoted:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
            elif ch == " " and depth == 0:
                return text[:i]
    return text


def _value(raw: str) -> object:
    raw = raw.strip()
    if raw.startswith("'") and raw.endswith("'") and len(raw) >= 2:
        return raw[1:-1].replace("''", "'")
    if raw.startswith("(") and raw.endswith(")"):
        return tuple(v.strip().upper() for v in _split_top_level(raw[1:-1], ",") if v.strip())
    return raw.upper()


def parse_statement(line: str) -> Statement | None:
    label = None
    rest = line
    if not line.startswith(" "):
        label, _, rest = line.partition(" ")
        label = label.upper()
    tokens = rest.strip().split(None, 1)
    if not tokens:
        return None
    macro = tokens[0].upper()
    operands: dict[str, object] = {}
    if len(tokens) > 1:
        for operand in _split_top_level(_operand_field(tokens[1]), ","):
            key, eq, value = operand.partition("=")
            if eq and key.strip():
                operands[key.strip().upper()] = _value(value)
    return Statement(label, macro, operands)


def _attributes(operands: dict[str, object]) -> tuple[str, ...] | None:
    dsatts = operands.get("DSATTS")
    if dsatts is not None:
        names = dsatts if isinstance(dsatts, tuple) else (str(dsatts),)
    elif operands.get("EXTATT") == "YES":
        names = EXTATT_YES
    else:
        names = None
    if names is not None:
        return tuple(n for n, _ in ATTRIBUTE_SUFFIXES if n in names)
    extatt = operands.get("EXTATT")
    if extatt in ("NO", "MAPONLY"):
        return ()
    return None


def _flag(operands: dict[str, object], key: str) -> bool | None:
    value = operands.get(key)
    return None if value is None else value == "YES"


def parse_bms(text: str) -> list[BmsMapset]:
    mapsets: list[BmsMapset] = []
    mapset: BmsMapset | None = None
    mapset_attributes: tuple[str, ...] = ()
    mapset_tioapfx = False
    current: BmsMap | None = None

    for line in logical_lines(text):
        stmt = parse_statement(line)
        if stmt is None:
            continue
        if stmt.macro == "DFHMSD":
            if stmt.operands.get("TYPE") == "FINAL":
                mapset, current = None, None
                continue
            mapset = BmsMapset(
                name=stmt.label or "",
                mode=str(stmt.operands.get("MODE", "OUT")),
                lang=str(stmt.operands.get("LANG", "ASM")),
            )
            mapsets.append(mapset)
            mapset_attributes = _attributes(stmt.operands) or ()
            mapset_tioapfx = bool(_flag(stmt.operands, "TIOAPFX"))
            current = None
        elif stmt.macro == "DFHMDI" and mapset is not None and stmt.label:
            attributes = _attributes(stmt.operands)
            tioapfx = _flag(stmt.operands, "TIOAPFX")
            current = BmsMap(
                name=stmt.label,
                attributes=mapset_attributes if attributes is None else attributes,
                tioapfx=mapset_tioapfx if tioapfx is None else tioapfx,
            )
            mapset.maps.append(current)
        elif stmt.macro == "DFHMDF" and current is not None and stmt.label:
            try:
                length = int(str(stmt.operands.get("LENGTH", "1")))
            except ValueError:
                length = 1
            try:
                occurs = int(str(stmt.operands.get("OCCURS", "1")))
            except ValueError:
                occurs = 1
            picin = stmt.operands.get("PICIN")
            picout = stmt.operands.get("PICOUT")
            current.fields.append(BmsField(
                name=stmt.label,
                length=max(length, 1),
                occurs=max(occurs, 1),
                picin=str(picin) if picin else None,
                picout=str(picout) if picout else None,
            ))
    return mapsets


def _input_entries(fld: BmsField, attributes: tuple[str, ...], level: int, indent: str) -> list[str]:
    sub = level + 1
    lines = [
        f"{indent}{level:02d}  {fld.name}L    COMP  PIC  S9(4).",
        f"{indent}{level:02d}  {fld.name}F    PICTURE X.",
        f"{indent}{level:02d}  FILLER REDEFINES {fld.name}F.",
        f"{indent}  {sub:02d}  {fld.name}A    PICTURE X.",
    ]
    if attributes:
        lines.append(f"{indent}{level:02d}  FILLER   PICTURE X({len(attributes)}).")
    lines.append(f"{indent}{level:02d}  {fld.name}I  PIC {fld.picin or f'X({fld.length})'}.")
    return lines


def _output_entries(fld: BmsField, attributes: tuple[str, ...], level: int, indent: str) -> list[str]:
    lines = [f"{indent}{level:02d}  FILLER PICTURE X(3)."]
    suffixes = dict(ATTRIBUTE_SUFFIXES)
    for name in attributes:
        lines.append(f"{indent}{level:02d}  {fld.name}{suffixes[name]}    PICTURE X.")
    lines.append(f"{indent}{level:02d}  {fld.name}O  PIC {fld.picout or f'X({fld.length})'}.")
    return lines


def _structure(bms_map: BmsMap, side: str, header: str, counter: list[int]) -> list[str]:
    indent = " " * 11
    lines = [header]
    if bms_map.tioapfx:
        lines.append(f"{indent}02  FILLER PIC X({TIOA_PREFIX_BYTES}).")
    if not bms_map.fields:
        lines.append(f"{indent}02  FILLER PIC X.")
    build = _input_entries if side == "I" else _output_entries
    for fld in bms_map.fields:
        if fld.occurs > 1:
            counter[0] += 1
            lines.append(f"{indent}02  DFHMS{counter[0]} OCCURS {fld.occurs} TIMES.")
            lines.extend(build(fld, bms_map.attributes, 3, indent + "  "))
        else:
            lines.extend(build(fld, bms_map.attributes, 2, indent))
    return lines


def render_copybook(mapsets: list[BmsMapset], source_name: str) -> str:
    lines = [
        f"      *> {GENERATED_MARKER} {source_name}",
        "      *> Written by tools/bms_symbolic_map.py; edit the .bms instead.",
    ]
    counter = [0]
    for mapset in mapsets:
        mode = mapset.mode.upper()
        for bms_map in mapset.maps:
            has_in = mode in ("IN", "INOUT")
            has_out = mode in ("OUT", "INOUT")
            if has_in:
                lines.extend(_structure(bms_map, "I", f"       01  {bms_map.name}I.", counter))
            if has_out:
                header = (f"       01  {bms_map.name}O REDEFINES {bms_map.name}I."
                          if has_in else f"       01  {bms_map.name}O.")
                lines.extend(_structure(bms_map, "O", header, counter))
    return "\n".join(lines) + "\n"


def _real_copybooks(source_dir: Path) -> set[str]:
    names: set[str] = set()
    for root, dirs, files in os.walk(source_dir):
        dirs[:] = [d for d in dirs if d not in SKIPPED_DIRS and not d.startswith(".convert-")]
        for f in files:
            stem, ext = os.path.splitext(f)
            if ext.lower() in COPYBOOK_EXTENSIONS:
                names.add(stem.upper())
    return names


def _bms_sources(source_dir: Path) -> list[Path]:
    found: list[Path] = []
    for root, dirs, files in os.walk(source_dir):
        dirs[:] = [d for d in dirs if d not in SKIPPED_DIRS and not d.startswith(".convert-")]
        found.extend(Path(root) / f for f in files if f.lower().endswith(".bms"))
    return sorted(found)


def _replaceable(path: Path) -> bool:
    if not path.exists():
        return True
    head = path.read_text(encoding="utf-8", errors="ignore")[:2000]
    return GENERATED_MARKER in head or STUB_MARKER in head


def generate(source_dir: Path, output_dir: Path) -> tuple[list[str], list[str]]:
    """Write one copybook per BMS source. Returns (generated names, warnings)."""
    real = _real_copybooks(source_dir)
    generated: list[str] = []
    warnings: list[str] = []
    claimed: dict[str, Path] = {}
    for bms in _bms_sources(source_dir):
        name = bms.stem.upper()
        if name in real:
            continue
        if name in claimed:
            warnings.append(f"{bms} and {claimed[name]} both produce {name}.cpy; kept the first")
            continue
        try:
            mapsets = parse_bms(bms.read_text(encoding="utf-8", errors="ignore"))
        except Exception as exc:  # one bad mapset must not stop the rest
            warnings.append(f"{bms}: could not parse ({exc})")
            continue
        mapsets = [m for m in mapsets if m.lang.upper() == "COBOL"]
        if not any(m.maps for m in mapsets):
            continue
        target = output_dir / f"{name}.cpy"
        if not _replaceable(target):
            continue
        output_dir.mkdir(parents=True, exist_ok=True)
        target.write_text(render_copybook(mapsets, bms.name), encoding="utf-8")
        claimed[name] = bms
        generated.append(name)
    return generated, warnings


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(__doc__.strip().splitlines()[2], file=sys.stderr)
        return 2
    generated, warnings = generate(Path(argv[1]), Path(argv[2]))
    for warning in warnings:
        print(f"  ⚠️  BMS: {warning}", file=sys.stderr)
    if generated:
        print(f"  Generated {len(generated)} symbolic map copybook(s) from BMS: {', '.join(generated)}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
