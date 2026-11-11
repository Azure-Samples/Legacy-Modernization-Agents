#!/usr/bin/env python3
"""Explicit analysis-only projections of documented platform event symbols."""
import argparse
import hashlib
import json
import os
import re
from pathlib import Path

from rekt_dependency_evidence import definition_problem, original_files, provenance


MARKER = "ANALYSIS-ONLY PLATFORM EVENT CONTRACT; NOT A VENDOR COPYBOOK"
DOCUMENTATION = "https://www.ibm.com/docs/en/cics-ts/6.x"
SYMBOLS = {
    "DFHENTER": "ENTER",
    "DFHCLEAR": "CLEAR",
    **{f"DFHPA{i}": f"PA{i}" for i in range(1, 4)},
    **{f"DFHPF{i}": f"PF{i}" for i in range(1, 25)},
}
BMS_DOCUMENTATION = [
    "https://www.ibm.com/docs/en/cics-ts/6.x?topic=characteristics-attribute-value-definitions-dfhbmsca",
    "https://www.ibm.com/docs/en/cics-ts/6.x?topic=output-setting-display-characteristics",
    "https://www.ibm.com/docs/en/cics-tx/11.1.0?topic=constants-field-attributes-printer-control-characters",
]
BMS_SYMBOLS = {
    "DFHBMASK": "A:AUTOSKIP",
    "DFHBMPRF": "A:PROTECTED_MDT_SET",
    "DFHBMFSE": "A:MDT_SET",
    "DFHNEUTR": "C:NEUTRAL",
    "DFHGREEN": "C:GREEN",
    "DFHUNDLN": "H:UNDERSCORE",
    "DFHBMUNP": "A:UNPROTECTED",
    "DFHBMPRO": "A:PROTECTED",
    "DFHBMBRY": "A:BRIGHT",
    "DFHBMDAR": "A:DARK",
    "DFHBMASF": "A:AUTOSKIP_MDT_SET",
    "DFHBMASB": "A:AUTOSKIP_BRIGHT",
    "DFHDFCOL": "C:DEFAULT",
    "DFHBLUE": "C:BLUE",
    "DFHRED": "C:RED",
    "DFHPINK": "C:PINK",
    "DFHTURQ": "C:TURQUOISE",
    "DFHYELLO": "C:YELLOW",
    "DFHBLINK": "H:BLINK",
    "DFHREVRS": "H:REVERSE",
}
BMS_CONTRACT = {
    "recipe": "symbolic-bms-attributes-v1",
    "selector": "bms-output-field",
    "symbols": BMS_SYMBOLS,
    "documentation": BMS_DOCUMENTATION,
    "targetSemantics": "preserve-field-attribute-assignments",
}


def validate(document):
    if (not isinstance(document, dict)
            or set(document) != {"schemaVersion", "profileKind", "profileName", "copybooks"}
            or document["schemaVersion"] != 1
            or document["profileKind"] != "platform-replacement"
            or not isinstance(document["profileName"], str) or not document["profileName"].strip()
            or not isinstance(document["copybooks"], dict)
            or not document["copybooks"]
            or not set(document["copybooks"]) <= {"DFHAID", "DFHBMSCA"}):
        raise ValueError("Unsupported platform replacement contract schema")
    if "DFHBMSCA" in document["copybooks"] and document["copybooks"]["DFHBMSCA"] != BMS_CONTRACT:
        raise ValueError("Unsupported DFHBMSCA attribute contract")
    if "DFHAID" not in document["copybooks"]:
        return None
    contract = document["copybooks"]["DFHAID"]
    if (not isinstance(contract, dict)
            or set(contract) != {"recipe", "selector", "symbols", "documentation", "targetSemantics"}
            or contract["recipe"] != "symbolic-terminal-events-v1"
            or contract["selector"] != "EIBAID"
            or contract["symbols"] != SYMBOLS
            or contract["documentation"] != [DOCUMENTATION]
            or contract["targetSemantics"] != "named-events-preserve-source-branches"):
        raise ValueError("Unsupported DFHAID event contract; no source bytes or business actions may be inferred")
    return contract


def projection(contract):
    name = "DFHBMSCA" if contract == BMS_CONTRACT else "DFHAID"
    return (
        f"      * {MARKER}\n"
        "      * No VALUE clauses: these names are symbolic, not blank data.\n"
        f"       01 {name}.\n"
        + "".join(f"          02 {name} PIC X.\n" for name in sorted(contract["symbols"]))
    ).encode("ascii")


def bms_bindings(usage):
    """Single unqualified targets keep fieldBinding; qualified or multi-target MOVEs list them."""
    if ("fieldBinding" in usage) == ("fieldBindings" in usage):
        raise ValueError("BMS usage must carry exactly one binding shape")
    if "fieldBinding" in usage:
        return [usage["fieldBinding"]], [usage["definitionSyntax"]]
    bindings, declarations = usage["fieldBindings"], usage["definitionSyntaxes"]
    if not isinstance(bindings, list) or not bindings or len(bindings) != len(declarations):
        raise ValueError("BMS usage bindings and declarations differ")
    return bindings, declarations


_OVERLAY_LEVEL = re.compile(r"0?[1-9]|[1-4][0-9]")
_OVERLAY_NAME = re.compile(r"[A-Z][A-Z0-9-]*")
_OVERLAY_DISPLAY = re.compile(r"PIC(?:TURE)?(?:IS)?X(?:\(0*([1-9][0-9]{0,3})\))?\.")
_OVERLAY_HALFWORD = re.compile(
    r"(?:PIC(?:TURE)?(?:IS)?S9\(0*4\)(?:COMP|COMPUTATIONAL|BINARY|COMP-4|COMPUTATIONAL-4)"
    r"|(?:COMP|COMPUTATIONAL|BINARY|COMP-4|COMPUTATIONAL-4)PIC(?:TURE)?(?:IS)?S9\(0*4\))\.")
_OVERLAY_OCCURS = re.compile(r"OCCURS0*([1-9][0-9]{0,3})(?:TIMES)?\.")


def overlay_token(token):
    token = token.upper()
    return "FILLER" if re.fullmatch(r"FIL-R[0-9]+", token) else token


def overlay_layout(record, members, target):
    """(redefined, base, stride, count) of an OCCURS element in a 01 FILLER REDEFINES <map>I|O overlay, else None.

    Only DISPLAY X fields, binary halfwords, plain groups, group REDEFINES of an earlier named sibling and one
    group OCCURS are accepted (mirrors BmsOverlayLayout.java and DependencyEvidenceGate.BmsOverlayLayout)."""
    # FIL-Rnnn is the preprocessor's layout-neutral rename of FILLER REDEFINES.
    head = [overlay_token(token) for token in record]
    if (len(head) != 5 or head[0] not in ("1", "01") or head[1] != "FILLER" or head[2] != "REDEFINES"
            or not _OVERLAY_NAME.fullmatch(head[3]) or head[4] != "." or not 0 <= target < len(members)):
        return None
    root = {"level": 1, "name": "FILLER", "redefines": None, "occurs": 1, "size": -1, "parent": None, "children": []}
    entries, current = [], root
    for member in members:
        tokens = [overlay_token(token) for token in member]
        if (len(tokens) < 3 or not _OVERLAY_LEVEL.fullmatch(tokens[0]) or not _OVERLAY_NAME.fullmatch(tokens[1])
                or tokens[1] in ("REDEFINES", "OCCURS")):
            return None
        entry = {"level": int(tokens[0]), "name": tokens[1], "redefines": None, "occurs": 1, "size": -1,
                 "parent": None, "children": []}
        rest, clause = tokens[2:], "".join(tokens[2:])
        if rest == ["."]:
            pass
        elif len(rest) == 3 and rest[0] == "REDEFINES" and _OVERLAY_NAME.fullmatch(rest[1]) and rest[2] == ".":
            entry["redefines"] = rest[1]
        elif rest[0] == "OCCURS" and _OVERLAY_OCCURS.fullmatch(clause):
            entry["occurs"] = int(_OVERLAY_OCCURS.fullmatch(clause)[1])
        elif _OVERLAY_DISPLAY.fullmatch(clause):
            entry["size"] = int(_OVERLAY_DISPLAY.fullmatch(clause)[1] or 1)
        elif _OVERLAY_HALFWORD.fullmatch(clause):
            entry["size"] = 2
        else:
            return None
        if entry["level"] <= 1:
            return None
        while current is not root and current["level"] >= entry["level"]:
            current = current["parent"]
        if current["size"] >= 0:
            return None
        entry["parent"] = current
        current["children"].append(entry)
        entries.append(entry)
        if entry["size"] < 0:
            current = entry

    def size(entry):
        if not entry["children"]:
            return entry["size"]
        widths = [size(child) for child in entry["children"]]
        if any(width <= 0 for width in widths):
            return -1
        return sum(width for width, child in zip(widths, entry["children"]) if child["redefines"] is None) * entry["occurs"]

    def place(group, start):
        cursor = start
        for index, child in enumerate(group["children"]):
            if child["redefines"] is not None:
                bases = [sibling for sibling in group["children"][:index]
                         if sibling["redefines"] is None and sibling["name"] == child["redefines"]]
                if child["occurs"] != 1 or not bases or bases[-1]["occurs"] != 1 or size(child) > size(bases[-1]):
                    return False
                child["offset"] = bases[-1]["offset"]
            else:
                child["offset"] = cursor
                cursor += size(child)
            if child["children"] and not place(child, child["offset"]):
                return False
        return True

    if size(root) < 0 or not place(root, 0):
        return None
    field = entries[target]
    if field["size"] != 1 or field["name"] == "FILLER" or sum(entry["name"] == field["name"] for entry in entries) != 1:
        return None
    tables, ancestor = [], field["parent"]
    while ancestor is not None:
        if ancestor["occurs"] > 1:
            tables.append(ancestor)
        ancestor = ancestor["parent"]
    if len(tables) != 1:
        return None
    return head[3], field["offset"], size(tables[0]) // tables[0]["occurs"], tables[0]["occurs"]


def attribute_suffix(delta, extended):
    """Symbolic map field: length halfword at +0, attribute byte at +2, extended attributes from +3 in DSATTS order."""
    if delta == 2:
        return "A"
    position = delta - 3
    if not 0 <= position < len(extended):
        return None
    return {"COLOR": "C", "HILIGHT": "H"}.get(extended[position])


OVERLAY_KEYS = {"name", "subscript", "map", "mapset", "suffix", "layoutPath", "layoutSha256", "outputPath",
                "outputSha256", "recordSyntax", "memberSyntaxes", "definitionSyntax", "occurrences"}


def bms_overlay_usage_bound(usage, tokens, value):
    from rekt_dependency_evidence import native_syntax_nodes
    bindings = usage["overlayBindings"]
    if ("fieldBinding" in usage or "fieldBindings" in usage or not isinstance(bindings, list)
            or (len(tokens) - 3) % 4 or len(bindings) != (len(tokens) - 3) // 4 or not bindings):
        return False
    def words(syntax):
        if not isinstance(syntax, dict) or syntax.get("rule") != "DataDescriptionEntryFormat1Context":
            return None
        nodes = native_syntax_nodes(syntax)
        return [node["token"].strip() for node in nodes if "token" in node and node["token"].strip()] if nodes else None
    for position, binding in zip(range(3, len(tokens), 4), bindings):
        name, subscript = tokens[position], tokens[position + 2]
        if (not isinstance(binding, dict) or set(binding) != OVERLAY_KEYS or tokens[position + 1] != "("
                or tokens[position + 3] != ")" or not _OVERLAY_NAME.fullmatch(name)
                or not re.fullmatch(r"[A-Z][A-Z0-9-]*|[1-9][0-9]{0,3}", subscript)
                or binding["name"] != name or binding["subscript"] != subscript
                or not isinstance(binding["memberSyntaxes"], list) or not isinstance(binding["occurrences"], list)
                or not all(isinstance(binding.get(key), str) and binding[key] for key in
                           ("map", "mapset", "layoutPath", "layoutSha256", "outputPath", "outputSha256"))):
            return False
        record, members = words(binding["recordSyntax"]), [words(item) for item in binding["memberSyntaxes"]]
        targets = [index for index, item in enumerate(binding["memberSyntaxes"]) if item == binding["definitionSyntax"]]
        if record is None or any(item is None for item in members) or len(targets) != 1 or members[targets[0]][1].upper() != name:
            return False
        located = overlay_layout(record, members, targets[0])
        if located is None or located[0] not in (binding["map"] + "I", binding["map"] + "O"):
            return False
        _, base, stride, count = located
        if [item.get("offset") if isinstance(item, dict) else None for item in binding["occurrences"]] != [
                base + occurrence * stride for occurrence in range(count)]:
            return False
        if not all(set(item) == {"field", "offset"} and isinstance(item["field"], str) for item in binding["occurrences"]):
            return False
        suffix = binding["suffix"]
        if not (suffix in ("A", "C", "H") and (value in BMS_SYMBOLS and BMS_SYMBOLS[value].startswith(suffix + ":")
                                               or value in ("'A'", '"A"') and suffix == "A"
                                               or value in ("HIGH-VALUE", "HIGH-VALUES") and suffix in ("C", "H"))):
            return False
    return True


def bms_usage_bound(usage):
    from rekt_dependency_evidence import native_syntax_nodes, cics_compact
    nodes = native_syntax_nodes(usage["syntax"])
    tokens = [node["token"] if node["token"].startswith(("'", '"')) else node["token"].upper()
              for node in nodes if "token" in node]
    if "overlayBindings" in usage:
        kind = (lambda value: "named-attribute" if value in BMS_SYMBOLS else
                "source-character-literal" if value in ("'A'", '"A"') else "figurative-constant")
        try:
            return (usage.get("grammarRule") == "MoveStatementContext"
                    and usage["syntax"].get("rule") == "MoveStatementContext"
                    and usage.get("coordinateSpace") == "expanded-parser-input"
                    and isinstance(usage.get("line"), int) and usage["line"] >= 1
                    and isinstance(usage.get("sourceText"), str) and usage["sourceText"].strip() != ""
                    and len(tokens) >= 7 and tokens[0] == "MOVE" and tokens[2] == "TO"
                    and cics_compact(usage["sourceText"]) == cics_compact(" ".join(tokens))
                    and usage.get("valueKind") == kind(tokens[1])
                    and bms_overlay_usage_bound(usage, tokens, tokens[1]))
        except (KeyError, TypeError, IndexError, AttributeError):
            return False
    try:
        bindings, declarations = bms_bindings(usage)
    except (ValueError, KeyError, TypeError):
        return False
    if (usage.get("grammarRule") != "MoveStatementContext"
            or usage["syntax"].get("rule") != "MoveStatementContext"
            or usage.get("coordinateSpace") != "expanded-parser-input"
            or not isinstance(usage.get("line"), int) or usage["line"] < 1
            or not isinstance(usage.get("sourceText"), str) or not usage["sourceText"].strip()
            or len(tokens) < 4 or tokens[0] != "MOVE" or tokens[2] != "TO"
            or any(declaration.get("rule") != "DataDescriptionEntryFormat1Context" for declaration in declarations)):
        return False
    targets, index = [], 3
    while index < len(tokens):
        name, group, index = tokens[index], None, index + 1
        if index < len(tokens) and tokens[index] in ("OF", "IN"):
            if index + 1 >= len(tokens):
                return False
            group, index = tokens[index + 1], index + 2
        targets.append((name, group))
    if len(targets) != len(bindings) or ("fieldBinding" in usage) != (targets == [(targets[0][0], None)]):
        return False
    if cics_compact(usage["sourceText"]) != cics_compact(" ".join(tokens)):
        return False
    value = tokens[1]
    kind = ("named-attribute" if value in BMS_SYMBOLS else
            "source-character-literal" if value in ("'A'", '"A"') else "figurative-constant")
    if usage.get("valueKind") != kind:
        return False
    for (name, group), field in zip(targets, bindings):
        suffix = field["suffix"]
        if (name != field["name"] or field["name"] != field["field"] + suffix or suffix not in ("A", "C", "H")
                or group is not None and group not in (field["map"] + "O", field["map"] + "I")
                or not all(isinstance(field.get(key), str) and field[key] for key in
                           ("map", "mapset", "layoutPath", "layoutSha256", "outputPath", "outputSha256"))):
            return False
        if not (value in BMS_SYMBOLS and BMS_SYMBOLS[value].startswith(suffix + ":")
                or value in ("'A'", '"A"') and suffix == "A"
                or value in ("HIGH-VALUE", "HIGH-VALUES") and suffix in ("C", "H")):
            return False
    return True


def bms_layouts_bound(binding, definitions):
    def bound_fields(usage):
        if "overlayBindings" not in usage:
            return bms_bindings(usage)[0]
        return [dict(overlay, name=item["field"] + overlay["suffix"], field=item["field"], offset=item["offset"])
                for overlay in usage["overlayBindings"] for item in overlay["occurrences"]]
    for field in (item for usage in binding["usages"] for item in bound_fields(usage)):
        layouts = {item["path"] for item in definitions if item["kind"] in ("BMS_LAYOUT", "CICS_MAP_LAYOUT")
                   and item["sha256"] == field["layoutSha256"]}
        outputs = {item["path"] for item in definitions if item["kind"] in ("COPY", "COPY_INPUT")
                   and item["sha256"] == field["outputSha256"]}
        if len(layouts) != 1 or len(outputs) != 1:
            return False
        layout, output = Path(next(iter(layouts))), Path(next(iter(outputs)))
        if (str(layout) != str(output) + ".bms-layout.json"
                or hashlib.sha256(layout.read_bytes()).hexdigest() != field["layoutSha256"]
                or hashlib.sha256(output.read_bytes()).hexdigest() != field["outputSha256"]
                or field["layoutPath"] != field["outputPath"] + ".bms-layout.json"):
            return False
        proof = json.loads(layout.read_text())
        maps = [screen for screen in proof["maps"] if screen["name"] == field["map"]
                and screen["mapset"] == field["mapset"] and screen["mode"] in ("OUT", "INOUT")]
        if (proof["producer"] != "bms-symbolic-map/layout-v1" or proof["status"] != "complete"
                or proof["outputSha256"] != field["outputSha256"] or len(maps) != 1):
            return False
        fields = [item for item in maps[0]["fields"] if item["name"] == field["field"] and item["occurs"] == 1]
        if (len(fields) != 1 or field["suffix"] != "A"
                and {"C": "COLOR", "H": "HILIGHT"}[field["suffix"]] not in maps[0]["extendedAttributes"]):
            return False
        if "offset" in field and attribute_suffix(field["offset"] - fields[0]["offset"],
                                                  maps[0]["extendedAttributes"]) != field["suffix"]:
            return False
    return True


COPY_STATEMENT = re.compile(
    r"(?<![A-Za-z0-9-])COPY\s+(['\"]?)([A-Za-z0-9#@$_-]+)\1(?:\s+REPLACING\b(.*?))?\s*\.", re.IGNORECASE | re.DOTALL)
COPY_OPERAND = r"(?:==(.*?)==|([A-Za-z0-9-]+))"
COPY_PAIR = re.compile(r"\s*" + COPY_OPERAND + r"\s+BY\s+" + COPY_OPERAND, re.IGNORECASE | re.DOTALL)
DELIMITED_TAG = re.compile(r"\([A-Za-z0-9-]+\)|:[A-Za-z0-9-]+:")


def copy_replacements(clause):
    pairs, index = [], 0
    while clause[index:].strip():
        match = COPY_PAIR.match(clause, index)
        if match is None:
            return None
        source = (match.group(1) if match.group(1) is not None else match.group(2)).strip()
        target = (match.group(3) if match.group(3) is not None else match.group(4)).strip()
        tag = DELIMITED_TAG.fullmatch(source) is not None
        if not (tag or re.fullmatch(r"[A-Za-z0-9-]+", source)) or not re.fullmatch(r"[A-Za-z0-9-]*", target):
            return None
        pairs.append((source, target, tag))
        index = match.end()
    if not pairs or any(other.upper() in target.upper() for _, target, _ in pairs for other, _, _ in pairs):
        return None
    return pairs


def replace_copy_text(text, pairs):
    lines = []
    for raw in text.splitlines():
        fixed = len(raw) > 6 and all(character == " " or character.isdigit() for character in raw[:6])
        if fixed and raw[6] in "*/":
            lines.append(raw)
            continue
        code = raw[7:72] if fixed else raw
        replaced = code
        for source, target, tag in pairs:
            pattern = re.escape(source) if tag else r"(?<![A-Za-z0-9_-])" + re.escape(source) + r"(?![A-Za-z0-9_-])"
            replaced = re.sub(pattern, lambda _: target, replaced, flags=re.IGNORECASE)
        if replaced == code:
            lines.append(raw)
            continue
        if fixed and raw[6] != " " or "'" in replaced or '"' in replaced:
            return None
        lines.append("       " + replaced)
    return "\n".join(lines)


def copy_expanded_text(text, definitions, active=()):
    """Splice hash-bound PROCEDURE DIVISION COPY bodies in parser-input order; unsupported forms fail closed (None)."""
    from rekt_dependency_evidence import dependency_guard_text, digest
    if len(active) > 16:
        return None
    # Fixed-format sequence areas (columns 1-6 and 73+) are not program text.
    text = "\n".join("      " + line[6:72] if len(line) > 6 and all(c == " " or c.isdigit() for c in line[:6]) else line
                     for line in text.splitlines())
    guarded, output, cursor = dependency_guard_text(text), [], 0
    # Symbol uses are procedural; DATA DIVISION copybooks contribute declarations, not uses.
    division = re.search(r"(?<![A-Za-z0-9-])PROCEDURE\s+DIVISION\b", guarded, re.IGNORECASE)
    start = 0 if active else division.end() if division else len(guarded)
    for match in COPY_STATEMENT.finditer(guarded, start):
        name = match.group(2).upper()
        bound = [item for item in definitions if item["kind"] == "COPY" and Path(item["name"]).stem.upper() == name]
        if not bound:
            continue
        if len(bound) != 1 or name in active or not Path(bound[0]["path"]).is_file() \
                or digest(bound[0]["path"]) != bound[0]["sha256"]:
            return None
        body = Path(bound[0]["path"]).read_text(encoding="latin-1")
        if match.group(3) is not None:
            pairs = copy_replacements(match.group(3))
            body = replace_copy_text(body, pairs) if pairs else None
            if body is None:
                return None
        expanded = copy_expanded_text(body, definitions, (*active, name))
        if expanded is None:
            return None
        output += [guarded[cursor:match.start()], "\n", expanded, "\n"]
        cursor = match.end()
    return "".join(output) + guarded[cursor:]


def bms_source_uses_bound(binding, source, definitions):
    import re
    from rekt_dependency_evidence import dependency_guard_text
    fields = set()
    for path in {item["path"] for item in definitions if item["kind"] in ("BMS_LAYOUT", "CICS_MAP_LAYOUT")}:
        for screen in json.loads(Path(path).read_text())["maps"]:
            for field in screen["fields"]:
                fields.update(field["name"] + suffix for suffix in ("A", "C", "H"))
    def relevant(text):
        return [token for token in re.findall(r"\b[A-Z][A-Z0-9_-]*\b", dependency_guard_text(text).upper())
                if token in BMS_SYMBOLS or token in fields]
    expanded = copy_expanded_text(source, definitions)
    return expanded is not None and relevant(expanded) == relevant("\n".join(usage["sourceText"] for usage in binding["usages"]))


def generate(source_root, output_root, configured):
    if not configured:
        return
    configured = Path(configured)
    if not configured.is_absolute():
        raise ValueError("REKT_PLATFORM_CONTRACTS must name an absolute contract path")
    document = json.loads(configured.read_text(encoding="utf-8"))
    validate(document)
    for name, contract in document["copybooks"].items():
        candidates = [
            path for path in original_files(Path(source_root))
            if (os.environ.get("REKT_SOURCE_TOP_LEVEL_ONLY") != "true" or path.parent == Path(source_root))
            and path.suffix.lower() in (".cpy", ".cpb", ".copy") and path.stem.upper() == name
        ]
        if any(definition_problem(path) is None for path in candidates):
            print(f"  Using supplied {name}; no platform projection generated.")
            continue
        output = Path(output_root) / f"{name}.cpy"
        if output.exists():
            raise ValueError(f"Refusing to overwrite an existing analysis input: {output}")
        output.write_bytes(projection(contract))
        provenance(configured, output, producer="platform-event-contract/v1")
        print(f"  {name}: explicit symbolic platform replacement; vendor values/layout NOT supplied.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source_root", type=Path)
    parser.add_argument("output_root", type=Path)
    args = parser.parse_args()
    generate(args.source_root, args.output_root, os.environ.get("REKT_PLATFORM_CONTRACTS"))
