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
replaced. A supplied symbolic map is never replaced: when its COBOL tokens equal the BMS
assembly of the same-named source, its preprocessed copy gets the same hash-bound layout proof
as a generated map, grounded in that BMS source.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import sys
from dataclasses import asdict, dataclass, field
from pathlib import Path
from rekt_dependency_evidence import (current_transform, definition_problem, digest, preserve, provenance,
                                      read_json, write_json)

USAGE = "Usage: bms_symbolic_map.py <source-dir> <output-dir>"
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
LAYOUT_RECIPE = "bms-symbolic-map/layout-v1"


@dataclass
class BmsField:
    name: str
    length: int
    occurs: int = 1
    picin: str | None = None
    picout: str | None = None
    position: tuple[int, int] | None = None
    initial: str | None = None
    attributes: tuple[str, ...] = ()


@dataclass
class BmsMap:
    name: str
    attributes: tuple[str, ...]
    tioapfx: bool
    fields: list[BmsField] = field(default_factory=list)
    size: tuple[int, int] | None = None
    screen_fields: list[BmsField] = field(default_factory=list)


@dataclass
class BmsMapset:
    name: str
    mode: str
    lang: str
    maps: list[BmsMap] = field(default_factory=list)
    storage_auto: bool = False


@dataclass
class Statement:
    label: str | None
    macro: str
    operands: dict[str, object]


def _ends_inside_quote(text: str) -> bool:
    return text.count("'") % 2 == 1


def logical_lines(text: str, strict: bool = False) -> list[str]:
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
        if strict and _ends_inside_quote(current):
            raise ValueError("Unterminated quoted BMS operand")
        result.append(current.rstrip())
        current = None
    if current is not None:
        if strict:
            raise ValueError("Unterminated BMS continuation")
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


def parse_statement(line: str, strict: bool = False) -> Statement | None:
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
            if strict and (not eq or not key.strip() or not value.strip()
                           or key.strip().upper() in operands):
                raise ValueError(f"Malformed or duplicate BMS operand: {operand}")
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


def _integer(operands: dict[str, object], key: str, default: int = 1, minimum: int = 1) -> int:
    value = int(str(operands.get(key, default)))
    if value < minimum:
        raise ValueError(f"{key} must be at least {minimum}, got {value}")
    return value


def _coordinates(operands: dict[str, object], key: str) -> tuple[int, int] | None:
    value = operands.get(key)
    if value is None:
        return None
    if not isinstance(value, tuple) or len(value) != 2:
        raise ValueError(f"{key} requires two coordinates, got {value}")
    coordinates = tuple(int(str(part)) for part in value)
    if any(part < 1 for part in coordinates):
        raise ValueError(f"{key} coordinates must be positive, got {value}")
    return coordinates


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
                storage_auto=stmt.operands.get("STORAGE") == "AUTO",
            )
            mapsets.append(mapset)
            mapset_attributes = _attributes(stmt.operands) or ()
            prefix = _flag(stmt.operands, "TIOAPFX")
            mapset_tioapfx = mapset.storage_auto if prefix is None else prefix
            current = None
        elif stmt.macro == "DFHMDI" and mapset is not None and stmt.label:
            attributes = _attributes(stmt.operands)
            tioapfx = _flag(stmt.operands, "TIOAPFX")
            current = BmsMap(
                name=stmt.label,
                attributes=mapset_attributes if attributes is None else attributes,
                tioapfx=mapset_tioapfx if tioapfx is None else tioapfx,
                size=_coordinates(stmt.operands, "SIZE"),
            )
            mapset.maps.append(current)
        elif stmt.macro == "DFHMDF" and current is not None:
            if "LENGTH" not in stmt.operands:
                raise ValueError("DFHMDF requires an explicit supported LENGTH; no default size is invented")
            length = _integer(stmt.operands, "LENGTH", minimum=0)
            occurs = _integer(stmt.operands, "OCCURS")
            if length == 0 and stmt.label:
                raise ValueError(f"Named zero-length symbolic fields are not supported: {stmt.label}")
            picin = stmt.operands.get("PICIN")
            picout = stmt.operands.get("PICOUT")
            attributes = stmt.operands.get("ATTRB", ())
            screen_field = BmsField(
                name=stmt.label or "",
                length=length,
                occurs=occurs,
                picin=str(picin) if picin else None,
                picout=str(picout) if picout else None,
                position=_coordinates(stmt.operands, "POS"),
                initial=str(stmt.operands["INITIAL"]) if "INITIAL" in stmt.operands else None,
                attributes=attributes if isinstance(attributes, tuple) else (str(attributes),),
            )
            current.screen_fields.append(screen_field)
            if stmt.label:
                current.fields.append(screen_field)
    return mapsets


def screen_definition(source: Path, source_dir: Path) -> dict:
    data = source.read_bytes()
    return {
        "path": source.relative_to(source_dir).as_posix(),
        "sha256": hashlib.sha256(data).hexdigest(),
        "resourceName": source.stem.upper(),
        "mapsets": [asdict(mapset) for mapset in parse_bms(data.decode("utf-8"))
                    if mapset.lang.upper() in ("COBOL", "COBOL2")],
    }


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
    if not bms_map.fields and not bms_map.tioapfx:
        raise ValueError(f"{bms_map.name}: empty symbolic storage cannot be represented without inventing a byte")
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
        base = None
        for bms_map in mapset.maps:
            has_in = mode in ("IN", "INOUT")
            has_out = mode in ("OUT", "INOUT")
            if has_in:
                header = f"       01  {bms_map.name}I"
                if base and not mapset.storage_auto:
                    header += f" REDEFINES {base}"
                lines.extend(_structure(bms_map, "I", header + ".", counter))
                base = base or f"{bms_map.name}I"
            if has_out:
                header = (f"       01  {bms_map.name}O REDEFINES {bms_map.name}I."
                          if has_in else f"       01  {bms_map.name}O"
                          + (f" REDEFINES {base}" if base and not mapset.storage_auto else "") + ".")
                lines.extend(_structure(bms_map, "O", header, counter))
                base = base or f"{bms_map.name}O"
    return "\n".join(lines) + "\n"


def _picture_bytes(picture: str, length: int) -> None:
    parts = re.findall(r"([AX9Z0B,./+*SVP-])(?:\((\d+)\))?", picture.upper())
    if not parts or "".join(symbol + (f"({count})" if count else "") for symbol, count in parts) != picture.upper():
        raise ValueError(f"Unsupported symbolic-map picture: {picture}")
    size = sum((int(count) if count else 1) for symbol, count in parts if symbol not in "SVP")
    if size != length:
        raise ValueError(f"Picture {picture} uses {size} bytes, but LENGTH is {length}")


def complete_layout_evidence(source: Path, copybook: Path) -> dict:
    """Prove the supported symbolic storage recipe against exact original and generated bytes."""
    data = source.read_bytes()
    text = data.decode("utf-8")
    allowed = {
        "DFHMSD": {"TYPE", "MODE", "LANG", "STORAGE", "CTRL", "EXTATT", "TERM", "TIOAPFX",
                   "DSATTS", "MAPATTS", "COLOR", "HILIGHT", "OUTLINE", "PS", "SOSI", "TRANSP", "VALIDN"},
        "DFHMDI": {"SIZE", "COLUMN", "LINE", "CTRL", "TIOAPFX", "EXTATT", "DSATTS", "MAPATTS",
                   "COLOR", "HILIGHT", "OUTLINE", "PS", "SOSI", "TRANSP", "VALIDN"},
        "DFHMDF": {"POS", "LENGTH", "OCCURS", "PICIN", "PICOUT", "INITIAL", "ATTRB",
                   "JUSTIFY", "COLOR", "HILIGHT", "OUTLINE", "PS", "SOSI", "TRANSP", "VALIDN"},
        "END": set(),
    }
    active = False
    current_map = False
    for line in logical_lines(text, strict=True):
        # HLASM listing-control instructions only format the assembly listing.
        if line.split(None, 1) and line.split(None, 1)[0].upper() in ("TITLE", "EJECT", "SPACE"):
            continue
        statement = parse_statement(line, strict=True)
        if statement is None:
            continue
        if statement.macro not in allowed or set(statement.operands) - allowed[statement.macro]:
            raise ValueError(f"Unsupported symbolic layout statement or operands: {line.strip()}")
        values = statement.operands
        if "EXTATT" in values and values["EXTATT"] not in ("YES", "NO", "MAPONLY"):
            raise ValueError(f"Unsupported EXTATT: {values['EXTATT']}")
        for key in ("DSATTS", "MAPATTS"):
            names = values.get(key, ())
            names = names if isinstance(names, tuple) else (names,)
            if any(name not in dict(ATTRIBUTE_SUFFIXES) for name in names):
                raise ValueError(f"Unsupported {key}: {names}")
        if "TIOAPFX" in values and values["TIOAPFX"] not in ("YES", "NO"):
            raise ValueError(f"Unsupported TIOAPFX: {values['TIOAPFX']}")
        if "PS" in values and values["PS"] != "BASE":
            raise ValueError("Programmed-symbol character sets need separate encoding/layout support")
        if statement.macro == "DFHMSD":
            if values.get("TYPE") == "FINAL":
                if not active:
                    raise ValueError("DFHMSD FINAL without an active mapset")
                active, current_map = False, False
            else:
                if active or not statement.label:
                    raise ValueError("Nested or unnamed mapset")
                # &&SYSPARM is the escaped form used where the source passes through another substitution.
                if values.get("TYPE") not in ("&SYSPARM", "&&SYSPARM", "MAP", "DSECT"):
                    raise ValueError("Unsupported mapset assembly type")
                if values.get("LANG") not in ("COBOL", "COBOL2"):
                    raise ValueError("Only COBOL symbolic layouts are supported")
                if values.get("MODE", "OUT") not in ("IN", "OUT", "INOUT"):
                    raise ValueError("Unsupported symbolic map mode")
                if values.get("STORAGE") not in (None, "AUTO"):
                    raise ValueError("Unsupported symbolic map storage")
                if values.get("TERM", "3270") not in ("3270", "3270-1", "3270-2"):
                    raise ValueError("Only 3270 symbolic layouts are supported")
                active, current_map = True, False
        elif statement.macro == "DFHMDI":
            if not active or not statement.label:
                raise ValueError("DFHMDI without an active mapset or map name")
            current_map = True
        elif statement.macro == "DFHMDF":
            if not active or not current_map:
                raise ValueError("DFHMDF without an active map")
            attrs = values.get("ATTRB", ())
            attrs = attrs if isinstance(attrs, tuple) else (attrs,)
            if "DET" in attrs:
                raise ValueError("Detectable-field symbolic input layout needs separate support")
        elif statement.macro == "END" and active:
            raise ValueError("END before DFHMSD FINAL")
    if active:
        raise ValueError("Missing DFHMSD FINAL")
    mapsets = parse_bms(text)
    if not mapsets or not all(mapset.maps for mapset in mapsets):
        raise ValueError("No complete mapset")
    maps = []
    for mapset in mapsets:
        names = set()
        for screen in mapset.maps:
            if screen.name in names:
                raise ValueError(f"Duplicate symbolic map name: {screen.name}")
            names.add(screen.name)
            offset = TIOA_PREFIX_BYTES if screen.tioapfx else 0
            fields = []
            seen = set()
            for entry in screen.fields:
                if entry.name in seen or not re.fullmatch(r"[A-Z][A-Z0-9_-]{0,28}", entry.name):
                    raise ValueError(f"Duplicate or unsupported symbolic field name: {entry.name}")
                seen.add(entry.name)
                if not 1 <= entry.length <= 256:
                    raise ValueError(f"Unsupported symbolic field length: {entry.length}")
                for picture in (entry.picin, entry.picout):
                    if picture:
                        _picture_bytes(picture, entry.length)
                stride = 3 + len(screen.attributes) + entry.length
                fields.append({"name": entry.name, "offset": offset, "dataOffset": offset + 3 + len(screen.attributes),
                               "length": entry.length, "occurs": entry.occurs, "stride": stride})
                offset += stride * entry.occurs
            maps.append({"name": screen.name, "mapset": mapset.name, "mode": mapset.mode, "byteLength": offset,
                         "extendedAttributes": list(screen.attributes), "fields": fields})
    rendered = render_copybook(mapsets, source.name).encode("utf-8")
    output = copybook.read_bytes()
    if output == rendered:
        match = "exact-render"
    elif symbolic_tokens(output.decode("latin-1")) == symbolic_tokens(rendered.decode("utf-8")):
        match = "token-equivalent"
    else:
        raise ValueError("Symbolic copybook differs from its original BMS recipe")
    return {"schemaVersion": 1, "producer": LAYOUT_RECIPE, "status": "complete",
            "sourceSha256": hashlib.sha256(data).hexdigest(),
            "outputSha256": hashlib.sha256(output).hexdigest(), "copybookMatch": match, "maps": maps}


def symbolic_tokens(text: str) -> list[str]:
    """Fixed-format COBOL tokens, ignoring comments, sequence/identification areas and spacing.
    FIL-Rnnn is the preprocessor's layout-neutral rename of FILLER REDEFINES."""
    tokens: list[str] = []
    for line in text.splitlines():
        if len(line) > 6 and line[6] in "*/":
            continue
        tokens.extend("FILLER" if re.fullmatch(r"FIL-R\d+", token) else token for token in line[7:72].split())
    return tokens


def _bind_supplied(bms: Path, name: str, output_dir: Path, warnings: list[str]) -> None:
    """Prove a supplied symbolic map's preprocessed copy against its same-named BMS source."""
    if not output_dir.is_dir():
        return
    copies = [path for path in output_dir.iterdir() if path.is_file() and path.stem.upper() == name
              and path.suffix.lower() in COPYBOOK_EXTENSIONS]
    copies = [path for path in copies if (original := current_transform(path)) is not None
              and original.suffix.lower() in COPYBOOK_EXTENSIONS]
    if len(copies) != 1:
        return
    copy = copies[0]
    try:
        proof = complete_layout_evidence(bms, copy)
    except (OSError, UnicodeError, ValueError) as exc:
        warnings.append(f"{bms}: supplied {copy.name} is not proven equivalent; layout unbound ({exc})")
        return
    record = read_json(str(copy) + ".preprocess.json")
    record.update({"bmsSourcePath": str(bms.resolve()), "bmsSourceSha256": digest(bms)})
    write_json(str(copy) + ".preprocess.json", record)
    write_json(str(copy) + ".bms-layout.json", proof)


def _top_level_only() -> bool:
    return os.environ.get("REKT_SOURCE_TOP_LEVEL_ONLY") == "true"


def _real_copybooks(source_dir: Path) -> set[str]:
    names: set[str] = set()
    for root, dirs, files in os.walk(source_dir):
        dirs[:] = [] if _top_level_only() else [d for d in dirs if d not in SKIPPED_DIRS and not d.startswith(".convert-")]
        for f in files:
            stem, ext = os.path.splitext(f)
            if ext.lower() in COPYBOOK_EXTENSIONS:
                if not definition_problem(Path(root) / f):
                    names.add(stem.upper())
    return names


def _bms_sources(source_dir: Path) -> list[Path]:
    found: list[Path] = []
    for root, dirs, files in os.walk(source_dir):
        dirs[:] = [] if _top_level_only() else [d for d in dirs if d not in SKIPPED_DIRS and not d.startswith(".convert-")]
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
    screens: list[dict] = []
    for bms in _bms_sources(source_dir):
        name = bms.stem.upper()
        try:
            screen = screen_definition(bms, source_dir)
        except (OSError, UnicodeError, ValueError) as exc:
            warnings.append(f"{bms}: could not parse screen definition ({exc})")
            continue
        if any(mapset["maps"] for mapset in screen["mapsets"]):
            screens.append(screen)
        if name in real:
            if name in claimed:
                warnings.append(f"{bms} and {claimed[name]} both describe supplied {name}; layout unbound")
                continue
            _bind_supplied(bms, name, output_dir, warnings)
            claimed[name] = bms
            continue
        if name in claimed:
            warnings.append(f"{bms} and {claimed[name]} both produce {name}.cpy; kept the first")
            continue
        try:
            mapsets = parse_bms(bms.read_text(encoding="utf-8", errors="ignore"))
        except (OSError, UnicodeError, ValueError) as exc:
            warnings.append(f"{bms}: could not parse ({exc})")
            continue
        mapsets = [m for m in mapsets if m.lang.upper() in ("COBOL", "COBOL2")]
        if not any(m.maps for m in mapsets):
            continue
        target = output_dir / f"{name}.cpy"
        if not _replaceable(target):
            continue
        output_dir.mkdir(parents=True, exist_ok=True)
        proof_path = Path(str(target) + ".bms-layout.json")
        preserve([target, Path(str(target) + ".preprocess.json"), proof_path], output_dir)
        try:
            target.write_text(render_copybook(mapsets, bms.name), encoding="utf-8")
        except (OSError, ValueError) as exc:
            warnings.append(f"{bms}: could not generate symbolic storage ({exc})")
            continue
        provenance(bms, target, producer="bms-symbolic-map")
        try:
            proof = complete_layout_evidence(bms, target)
        except (OSError, UnicodeError, ValueError) as exc:
            warnings.append(f"{bms}: symbolic storage remains partial ({exc})")
            proof = {"schemaVersion": 1, "producer": LAYOUT_RECIPE, "status": "partial",
                     "diagnostics": [str(exc)]}
        write_json(proof_path, proof)
        claimed[name] = bms
        generated.append(name)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "bms-screen-definitions.json").write_text(json.dumps({
        "schemaVersion": 1, "producer": "bms-symbolic-map/screen-v1",
        "sourceRoot": os.path.relpath(source_dir.resolve(), output_dir.resolve()),
        "definitions": screens,
    }, indent=2) + "\n", encoding="utf-8")
    return generated, warnings


def main(argv: list[str]) -> int:
    if len(argv) != 3:
        print(USAGE, file=sys.stderr)
        return 2
    generated, warnings = generate(Path(argv[1]), Path(argv[2]))
    for warning in warnings:
        print(f"  ⚠️  BMS: {warning}", file=sys.stderr)
    if generated:
        print(f"  Generated {len(generated)} symbolic map copybook(s) from BMS: {', '.join(generated)}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
