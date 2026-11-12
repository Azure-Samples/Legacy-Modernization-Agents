#!/usr/bin/env python3
"""Bind REKT's actual dependency export to its original inputs, without filling gaps."""
import argparse
import hashlib
import json
import os
import re
import shutil
import sys
import uuid
from collections import Counter
from datetime import datetime, timezone
from pathlib import Path

from rekt_source_identity import dependency_guard_text, referenced_names

KINDS = ("COPY", "CALL", "EXEC_CICS", "EXEC_SQL", "FILE_IO")
ABSENCE_PROOFS = {
    "EXEC_CICS": "antlr-tree:no-dialect-contexts",
    "EXEC_SQL": "antlr-tree:no-dialect-contexts",
    "FILE_IO": "antlr-tree:no-file-io-contexts",
}


def native_syntax_nodes(syntax):
    if not isinstance(syntax, dict):
        return []
    if set(syntax) == {"token"}:
        return [syntax] if isinstance(syntax["token"], str) else []
    if (set(syntax) != {"rule", "children"} or not isinstance(syntax["rule"], str)
            or not isinstance(syntax["children"], list)):
        return []
    nodes = [syntax]
    for child in syntax["children"]:
        descendants = native_syntax_nodes(child)
        if not descendants:
            return []
        nodes.extend(descendants)
    return nodes


def missing_native_operations(source, document):
    lines = []
    identification = documentary = False
    for raw in source.read_text(encoding="latin-1").splitlines():
        fixed = (len(raw) > 7 and all(character == " " or character.isascii() and character.isdigit()
                                     for character in raw[:6]) and raw[6] in " -*/Dd")
        if fixed and raw[6] in "*/":
            continue
        line = raw[7:72] if fixed else raw
        if re.match(r"\s*(?:IDENTIFICATION|ID)\s+DIVISION\b", line, re.I):
            identification = True
        if re.match(r"\s*(?:ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b", line, re.I):
            identification = documentary = False
        if identification and re.match(
                r"\s*(?:AUTHOR|INSTALLATION|DATE-WRITTEN|DATE-COMPILED|SECURITY|REMARKS)\s*\.", line, re.I):
            documentary = True
        if not documentary:
            lines.append(("      " + raw[6].upper() if fixed else "       ") + line)
    masked = dependency_guard_text("\n".join(lines))
    masked = re.sub(r"'(?:[^']|'')*'|\"(?:[^\"]|\"\")*\"", " ", masked)
    ordinary = re.sub(r"(?m)^      D.*$", "", masked)
    if not re.search(r"\bWITH\s+DEBUGGING\s+MODE\b", re.sub(r"(?m)^      -", "       ", ordinary), re.I):
        masked = ordinary
    masked = re.sub(r"(?m)^      [-D]", "       ", masked)
    original = Counter((f"EXEC_{kind.upper()}", operation.upper())
                       for kind, operation in re.findall(r"\bEXEC\s+(CICS|SQL)\s+([A-Z][A-Z0-9-]*)",
                                                        masked, re.I)
                       if not (kind.upper() == "SQL" and operation.upper() == "INCLUDE"))
    original.update(("EXEC_CICS", operation.upper())
                    for operation in re.findall(r"\b(DFHRESP|DFHVALUE)\s*\(", masked, re.I))
    native = Counter((statement.get("kind"), str(statement.get("operation", "")).upper())
                     for statement in document.get("dependencyDialectStatements", [])
                     if isinstance(statement, dict))
    return [f"original-operation-not-exported:{kind}:{operation}:{count}"
            for (kind, operation), count in (original - native).items()]


def native_cics_statement_bound(statement):
    syntax = statement.get("syntax")
    if (not isinstance(syntax, dict) or statement.get("bindingStatus") != "complete"
            or not isinstance(statement.get("sourceText"), str)):
        return False
    rule = statement.get("grammarRule")
    if syntax.get("rule") != rule:
        return False
    nodes = native_syntax_nodes(syntax)
    tokens = [node["token"] for node in nodes if isinstance(node.get("token"), str)]
    if rule == "Cics_returnContext":
        if [token.upper() for token in tokens] == ["RETURN"]:
            return (statement.get("operation") == "RETURN" and len(syntax.get("children", [])) == 1
                    and statement.get("sourceText", "").strip().upper() == "RETURN"
                    and not statement.get("resourceBindings") and not statement.get("bindingDiagnostics"))
    if rule in ("CicsDfhRespLiteralContext", "CicsDfhValueLiteralContext"):
        operation = "DFHRESP" if rule == "CicsDfhRespLiteralContext" else "DFHVALUE"
        symbols = (("NORMAL", "INVREQ", "MAPFAIL", "NOTFINISHED", "NOTFND", "ENDFILE", "DUPREC", "DUPKEY") if operation == "DFHRESP"
                   else ("NORMAL", "ABEND", "SECERROR", "UCTRAN", "TRANIDONLY", "NOUCTRAN"))
        return (statement.get("operation") == operation
                and len(tokens) == 4 and [tokens[0].upper(), tokens[1], tokens[3]] == [operation, "(", ")"]
                and tokens[2].upper() in symbols
                and re.fullmatch(operation + r"\s*\(\s*" + re.escape(tokens[2]) + r"\s*\)",
                                 statement["sourceText"].strip(), re.IGNORECASE) is not None)
    if rule in CICS_LOCAL_OPERATIONS or (rule in CICS_FILE_OPERATIONS
                                         and statement.get("bindingBasis") == "parsed-cics-file-operation"):
        return native_cics_local_bound(statement, syntax, nodes, tokens)
    if rule != "Cics_writeContext" or statement.get("operation") != "WRITE":
        return False
    files = [node for node in nodes if node.get("rule") == "Cics_file_nameContext"]
    bindings = statement.get("resourceBindings")
    if (len(files) != 1 or not any(node.get("rule") == "Cics_write_fileContext" for node in nodes)
            or "SYSID" in [token.upper() for token in tokens]
            or not isinstance(bindings, list) or len(bindings) != 1 or not isinstance(bindings[0], dict)):
        return False
    file_tokens = [node["token"] for node in native_syntax_nodes(files[0])
                   if isinstance(node.get("token"), str)]
    if (len(file_tokens) != 4 or file_tokens[0].upper() not in ("FILE", "DATASET")
            or file_tokens[1] != "(" or file_tokens[3] != ")"):
        return False
    operand = file_tokens[2]
    if len(operand) < 2 or operand[0] not in "'\"" or operand[-1] != operand[0]:
        return False
    binding = bindings[0]
    attributes = binding.get("attributes")
    return (binding.get("kind") == "CICS_FILE" and binding.get("name") == operand[1:-1]
            and isinstance(binding.get("sourcePath"), str) and Path(binding["sourcePath"]).is_absolute()
            and isinstance(binding.get("sourceSha256"), str)
            and re.fullmatch(r"[0-9a-f]{64}", binding["sourceSha256"]) is not None
            and isinstance(attributes, dict) and isinstance(attributes.get("dsname"), str)
            and bool(attributes["dsname"].strip()) and type(attributes.get("recordsize")) is int
            and type(attributes.get("keylength")) is int
            and 0 <= attributes["keylength"] <= attributes["recordsize"] > 0)


CICS_LOCAL_OPERATIONS = {
    "Cics_assignContext": "ASSIGN", "Cics_abendContext": "ABEND", "Cics_syncpointContext": "SYNCPOINT",
    "Cics_asktimeContext": "ASKTIME", "Cics_formattimeContext": "FORMATTIME",
    "Cics_handleContext": "HANDLE", "Cics_linkContext": "LINK",
    "Cics_enqContext": "ENQ", "Cics_deqContext": "DEQ",
    "Cics_delayContext": "DELAY", "Cics_getContext": "GET", "Cics_putContext": "PUT",
    "Cics_fetchContext": "FETCH", "Cics_runContext": "RUN", "Cics_xctlContext": "XCTL",
    "Cics_sendContext": "SEND", "Cics_receiveContext": "RECEIVE", "Cics_returnContext": "RETURN",
    "Cics_inquireContext": "INQUIRE", "Cics_set_terminalContext": "SET", "Cics_bifContext": "BIF",
    "Cics_writeqContext": "WRITEQ",
}

CICS_FILE_OPERATIONS = {
    "Cics_readContext": ("READ",), "Cics_readnextContext": ("READNEXT", "READPREV"),
    "Cics_writeContext": ("WRITE",), "Cics_rewriteContext": ("REWRITE",), "Cics_deleteContext": ("DELETE",),
    "Cics_startbrContext": ("STARTBR",), "Cics_endbrContext": ("ENDBR",), "Cics_unlockContext": ("UNLOCK",),
}
# Keyed VSAM access only; mirrors the native parser's unsupported file options.
CICS_FILE_UNSUPPORTED = {"SYSID", "RBA", "RRN", "RBN", "XRBA", "SET", "TOKEN", "MASSINSERT", "GENERIC", "REQID",
                         "DEBKEY", "DEBREC", "UNCOMMITTED", "CONSISTENT", "REPEATABLE", "REPETABLE"}
CICS_FILE_SERVICES = {"READNEXT": "browse", "READPREV": "browse", "STARTBR": "browse", "ENDBR": "browse",
                      "WRITE": "add", "REWRITE": "update", "UNLOCK": "update", "DELETE": "delete"}


def cics_file_options(syntax):
    parents = {"Cics_write_fileContext", "Cics_delete_fileContext", "Cics_delete_ridfldContext", "Cics_intoContext"}
    options = []
    def visit(node, parent_rule):
        for child in node.get("children") or []:
            if "token" in child and (node is syntax or parent_rule in parents):
                options.append(child["token"].upper())
            elif "rule" in child:
                visit(child, child["rule"])
    visit(syntax, syntax.get("rule"))
    return options


def native_cics_tdqueue_bound(syntax, binding):
    """WRITEQ TD QUEUE(name) FROM(area) [LENGTH(n)] to a local fixed-record extrapartition output queue."""
    children = syntax.get("children", [])
    queues = cics_operands(syntax, "QUEUE")
    if (len(children) != 2 or children[1].get("rule") != "Cics_writeq_tdContext" or len(queues) != 1
            or len(cics_operands(syntax, "FROM")) != 1 or len(cics_operands(syntax, "LENGTH")) > 1
            or cics_operands(syntax, "SYSID")):
        return False
    name = cics_constant_operand(queues[0], binding.get("targetResolution"), 1, 4)
    attributes = binding.get("attributes")
    return (name is not None and binding.get("name") == name
            and set(binding) == {"kind", "name", "sourcePath", "sourceSha256", "attributes", "targetResolution"}
            and isinstance(binding.get("sourcePath"), str) and Path(binding["sourcePath"]).is_absolute()
            and re.fullmatch(r"[0-9a-f]{64}", str(binding.get("sourceSha256"))) is not None
            and isinstance(attributes, dict)
            and set(attributes) == {"type", "typefile", "ddname", "recordsize", "recordformat"}
            and (attributes["type"], attributes["typefile"], attributes["recordformat"]) == ("EXTRA", "OUTPUT", "FIXED")
            and isinstance(attributes["ddname"], str)
            and re.fullmatch(r"[A-Z@$#][A-Z0-9@$#]{0,7}", attributes["ddname"]) is not None
            and type(attributes["recordsize"]) is int and attributes["recordsize"] > 0)


def native_cics_file_bound(syntax, operation, binding):
    names = [node for node in native_syntax_nodes(syntax) if node.get("rule") == "Cics_file_nameContext"]
    options = cics_file_options(syntax)
    if (len(names) != 1 or len(names[0].get("children") or []) != 2
            or names[0]["children"][1].get("rule") != "Cics_nameContext"
            or set(options) & CICS_FILE_UNSUPPORTED
            or syntax["rule"] == "Cics_deleteContext" and len(syntax.get("children") or []) != 2):
        return False
    name = cics_constant_operand(names[0]["children"][1], binding.get("targetResolution"), 1, 8)
    attributes = binding.get("attributes")
    if (name is None or binding.get("kind") != "CICS_FILE" or binding.get("name") != name
            or not isinstance(binding.get("sourcePath"), str) or not Path(binding["sourcePath"]).is_absolute()
            or re.fullmatch(r"[0-9a-f]{64}", str(binding.get("sourceSha256"))) is None
            or not isinstance(attributes, dict) or not isinstance(attributes.get("dsname"), str)
            or not attributes["dsname"].strip() or type(attributes.get("recordsize")) is not int
            or type(attributes.get("keylength")) is not int
            or not 0 <= attributes["keylength"] <= attributes["recordsize"] > 0):
        return False
    if "services" not in attributes:
        return set(attributes) == {"dsname", "recordsize", "keylength"} and "datasetDefinitions" not in binding
    services, sources = attributes["services"], binding.get("datasetDefinitions")
    service = ("update" if "UPDATE" in options else "read") if operation == "READ" else CICS_FILE_SERVICES[operation]
    path = attributes.get("accessPath")
    return (set(attributes) == {"dsname", "organization", "accessPath", "recordsize", "keylength", "keyOffset",
                                "uniqueKeys", "services", "lengthProvenance"}
            and attributes["organization"] == "KSDS" and attributes["lengthProvenance"] == "idcams-define"
            and isinstance(services, dict) and set(services) == {"add", "browse", "delete", "read", "update"}
            and all(value in ("YES", "NO") for value in services.values()) and services[service] == "YES"
            and type(attributes["keyOffset"]) is int and type(attributes["uniqueKeys"]) is bool
            and attributes["keyOffset"] + attributes["keylength"] <= attributes["recordsize"]
            and path in ("base-cluster", "alternate-index-path")
            and isinstance(sources, list) and sources and all(
                isinstance(source, dict) and set(source) == {"type", "name", "sourcePath", "sourceSha256", "command"}
                and source["type"] in ("CLUSTER", "ALTERNATEINDEX", "PATH")
                and isinstance(source["sourcePath"], str) and Path(source["sourcePath"]).is_absolute()
                and re.fullmatch(r"[0-9a-f]{64}", str(source["sourceSha256"])) is not None
                and isinstance(source["command"], str)
                and re.match(r"DEFINE\s+(?:" + ("AIX|" if source["type"] == "ALTERNATEINDEX" else "") + source["type"]
                             + r")\s*\(\s*NAME\s*\(" + re.escape(source["name"]) + r"\)",
                             source["command"], re.IGNORECASE) is not None
                for source in sources)
            and {source["type"] for source in sources} == (
                {"CLUSTER"} if path == "base-cluster" else {"CLUSTER", "ALTERNATEINDEX", "PATH"})
            and any(source["type"] == ("CLUSTER" if path == "base-cluster" else "PATH")
                    and source["name"] == attributes["dsname"] for source in sources))


CICS_TERMINAL_OPERATIONS = {
    "Cics_sendContext", "Cics_receiveContext", "Cics_returnContext",
    "Cics_inquireContext", "Cics_set_terminalContext", "Cics_bifContext",
}


def cics_operands(syntax, option):
    return [children[index]["children"][0]
            if option == "MAP" and children[index].get("rule") == "Cics_receive_mapContext" else children[index]
            for node in native_syntax_nodes(syntax)
            for children in [node.get("children", [])] for index in range(1, len(children))
            if children[index - 1].get("token", "").upper() == option
            and "rule" in children[index]]


def cics_literal(operand, minimum, maximum):
    value = "".join(node["token"] for node in native_syntax_nodes(operand) if "token" in node)
    match = re.fullmatch(r"""\((['"])([A-Za-z0-9@$#_-]+)\1\)""", value)
    return match[2] if match and minimum <= len(match[2]) <= maximum else None


CONSTANT_OPERAND_KINDS = ("immutable-working-storage-literal", "immutable-local-storage-literal")
CONSTANT_OPERAND_PROOF = "single-native-program:read-only-constant-references"


def cics_constant_operand(operand, resolution, minimum, maximum):
    """Value of a CICS name operand: a literal, or a VALUE proved read-only by the native parser."""
    literal = cics_literal(operand, minimum, maximum)
    if literal is not None:
        return literal if resolution in (None, {"kind": "literal"}) else None
    text = "".join(node["token"] for node in native_syntax_nodes(operand) if "token" in node)
    name = re.fullmatch(r"\(([A-Za-z][A-Za-z0-9-]*)\)", text)
    if (not name or not isinstance(resolution, dict) or set(resolution) != {
            "kind", "operand", "definitionSyntax", "ancestorSyntax", "usageProof"}
            or resolution["kind"] not in CONSTANT_OPERAND_KINDS or resolution["operand"] != name[1]
            or resolution["usageProof"] != CONSTANT_OPERAND_PROOF):
        return None
    definition, ancestors = resolution["definitionSyntax"], resolution["ancestorSyntax"]
    if (not isinstance(definition, dict) or not isinstance(ancestors, list)
            or definition.get("rule") not in ("DataDescriptionEntryFormat1Context", "DataDescriptionEntryFormat1Level77Context")):
        return None
    declaration = re.fullmatch(r"(0?[1-9]|[1-4][0-9]|77)" + re.escape(name[1])
                               + r"""PIC(?:TURE)?(?:IS)?X\(0*([1-9][0-9]?)\)VALUE(?:IS)?(['"])([A-Za-z0-9@$#_-]+)( *)\3\.""",
                               "".join(node["token"] for node in native_syntax_nodes(definition) if "token" in node),
                               re.IGNORECASE)
    if (not declaration or not minimum <= len(declaration[4]) <= maximum
            or len(declaration[4]) + len(declaration[5]) > int(declaration[2])):
        return None
    levels = []
    for ancestor in ancestors:
        group = isinstance(ancestor, dict) and ancestor.get("rule") == "DataDescriptionEntryFormat1Context" and re.fullmatch(
            r"(0?[1-9]|[1-4][0-9])[A-Za-z][A-Za-z0-9-]*\.",
            "".join(node["token"] for node in native_syntax_nodes(ancestor) if "token" in node))
        if not group:
            return None
        levels.append(int(group[1]))
    level = int(declaration[1])
    if level in (1, 77):
        return declaration[4] if not levels else None
    chain = levels + [level]
    return declaration[4] if levels and levels[0] == 1 and all(a < b for a, b in zip(chain, chain[1:])) else None



TABLE_OPERAND_PROOF = "single-native-program:read-only-table-references"
_LEVEL, _NAME = r"(0?[1-9]|[1-4][0-9])", r"([A-Za-z][A-Za-z0-9-]*)"


def cics_table_operand(operand, resolution, minimum, maximum):
    """Per-occurrence values of NAME(SUBSCRIPT) over a REDEFINES of literal VALUE items; None marks storage outside them."""
    def text(syntax):
        return "".join(node["token"] for node in native_syntax_nodes(syntax) if "token" in node)
    def entry(syntax):
        return isinstance(syntax, dict) and syntax.get("rule") == "DataDescriptionEntryFormat1Context" and native_syntax_nodes(syntax)
    keys = {"kind", "operand", "subscript", "definitionSyntax", "ancestorSyntax", "redefiningSyntax", "occursSyntax",
            "memberSyntax", "redefinedSyntax", "valueSyntax", "occurrenceValues", "usageProof"}
    operand_text = re.fullmatch(r"\(" + _NAME + r"\(" + _NAME + r"\)\)", text(operand))
    if (not operand_text or not isinstance(resolution, dict) or set(resolution) != keys
            or resolution["kind"] != "immutable-redefined-table-values" or resolution["usageProof"] != TABLE_OPERAND_PROOF
            or resolution["operand"] != operand_text[1] or resolution["subscript"] != operand_text[2]
            or not all(isinstance(resolution[key], list) for key in ("ancestorSyntax", "memberSyntax", "valueSyntax"))
            or not all(entry(item) for key in ("ancestorSyntax", "memberSyntax", "valueSyntax") for item in resolution[key])
            or not all(entry(resolution[key]) for key in ("definitionSyntax", "redefiningSyntax", "occursSyntax", "redefinedSyntax"))
            or not resolution["ancestorSyntax"] or not resolution["valueSyntax"]):
        return None
    levels = []
    for ancestor in resolution["ancestorSyntax"]:
        group = re.fullmatch(_LEVEL + _NAME + r"\.", text(ancestor))
        if not group or int(group[1]) <= (levels[-1] if levels else 0) or not levels and int(group[1]) != 1:
            return None
        levels.append(int(group[1]))
    redefining = re.fullmatch(_LEVEL + _NAME + "REDEFINES" + _NAME + r"\.", text(resolution["redefiningSyntax"]), re.IGNORECASE)
    occurs = re.fullmatch(_LEVEL + _NAME + r"OCCURS0*([1-9][0-9]{0,3})(?:TIMES)?\.", text(resolution["occursSyntax"]), re.IGNORECASE)
    redefined = re.fullmatch(_LEVEL + _NAME + r"\.", text(resolution["redefinedSyntax"]))
    if (not redefining or not occurs or not redefined or int(redefining[1]) <= levels[-1]
            or int(occurs[1]) <= int(redefining[1]) or redefined[1].lstrip("0") != redefining[1].lstrip("0")
            or redefined[2].upper() != redefining[3].upper()):
        return None
    member_shape = re.compile(_LEVEL + _NAME + r"PIC(?:TURE)?(?:IS)?([X9])\(0*([1-9][0-9]{0,3})\)\.", re.IGNORECASE)
    size, offset, width = 0, None, 0
    for member in resolution["memberSyntax"]:
        shape = member_shape.fullmatch(text(member))
        if not shape or int(shape[1]) <= int(occurs[1]):
            return None
        if member == resolution["definitionSyntax"]:
            if shape[2].upper() != operand_text[1].upper() or shape[3].upper() != "X" or offset is not None:
                return None
            offset, width = size, int(shape[4])
        size += int(shape[4])
    if offset is None:
        return None
    area = ""
    for item in resolution["valueSyntax"]:
        body = text(item)
        alphanumeric = re.fullmatch(_LEVEL + r"(?:[A-Za-z][A-Za-z0-9-]*)?PIC(?:TURE)?(?:IS)?X\(0*([1-9][0-9]{0,3})\)"
                                    r"""VALUE(?:IS)?(['"])([^'"]*)\3\.""", body, re.IGNORECASE)
        numeric = re.fullmatch(_LEVEL + r"(?:[A-Za-z][A-Za-z0-9-]*)?PIC(?:TURE)?(?:IS)?9\(0*([1-9][0-9]?)\)VALUE(?:IS)?([0-9]+)\.",
                               body, re.IGNORECASE)
        if alphanumeric and len(alphanumeric[4]) <= int(alphanumeric[2]) and int(alphanumeric[1]) > int(redefined[1]):
            area += alphanumeric[4].ljust(int(alphanumeric[2]))
        elif numeric and len(numeric[3]) <= int(numeric[2]) and int(numeric[1]) > int(redefined[1]):
            area += numeric[3].rjust(int(numeric[2]), "0")
        else:
            return None
    values = []
    for index in range(int(occurs[3])):
        start = index * size + offset
        if start >= len(area):
            values.append(None)
            continue
        value = area[start:start + width].rstrip(" ")
        if start + width > len(area) or not re.fullmatch(r"[A-Za-z0-9@$#]+", value) or not minimum <= len(value) <= maximum:
            return None
        values.append(value)
    return values if values == resolution["occurrenceValues"] and any(values) else None


REACHING_OPERAND_PROOF = "single-native-program:structured-reaching-definitions"
STRAIGHT_LINE_OPERAND_PROOF = "single-native-program:paragraph-straight-line-assignment"


def cics_reaching_operand(operand, resolution, minimum, maximum):
    """Values the native parser proved can reach an XCTL operand from structured MOVE/INITIALIZE statements."""
    def text(syntax):
        return "".join(node["token"] for node in native_syntax_nodes(syntax) if "token" in node)
    keys = {"kind", "operand", "definitionSyntax", "ancestorSyntax", "assignments", "initialReaches",
            "reachingValues", "usageProof"}
    name = re.fullmatch(r"\(" + _NAME + r"\)", text(operand))
    if (not name or not isinstance(resolution, dict) or set(resolution) != keys
            or resolution["kind"] != "structured-reaching-assignments"
            or resolution["usageProof"] not in (REACHING_OPERAND_PROOF, STRAIGHT_LINE_OPERAND_PROOF)
            or resolution["usageProof"] == STRAIGHT_LINE_OPERAND_PROOF and (
                resolution["initialReaches"] is not False or not isinstance(resolution["assignments"], list)
                or len(resolution["assignments"]) != 1)
            or resolution["operand"] != name[1] or not isinstance(resolution["ancestorSyntax"], list)
            or not isinstance(resolution["assignments"], list) or not isinstance(resolution["initialReaches"], bool)
            or not isinstance(resolution["reachingValues"], list)
            or not isinstance(resolution["definitionSyntax"], dict)
            or resolution["definitionSyntax"].get("rule") not in (
                "DataDescriptionEntryFormat1Context", "DataDescriptionEntryFormat1Level77Context")):
        return None
    definition = re.fullmatch(r"(0?[1-9]|[1-4][0-9]|77)" + re.escape(name[1])
                              + r"""PIC(?:TURE)?(?:IS)?X\(0*([1-9][0-9]{0,3})\)(?:VALUE(?:IS)?(['"])([^'"]*)\3)?\.""",
                              text(resolution["definitionSyntax"]), re.IGNORECASE)
    if not definition:
        return None
    groups, levels = [name[1].upper()], []
    for ancestor in resolution["ancestorSyntax"]:
        group = (isinstance(ancestor, dict) and ancestor.get("rule") == "DataDescriptionEntryFormat1Context"
                 and re.fullmatch(_LEVEL + _NAME + r"\.", text(ancestor)))
        if not group or int(group[1]) <= (levels[-1] if levels else 0) or not levels and int(group[1]) != 1:
            return None
        levels.append(int(group[1]))
        groups.append(group[2].upper())
    if (int(definition[1]) in (1, 77)) != (not levels) or levels and int(definition[1]) <= levels[-1]:
        return None
    def program(literal):
        value = literal.rstrip(" ")
        if not value:
            return None
        if (not re.fullmatch(r"[A-Za-z0-9@$#]+", value) or not minimum <= len(value) <= maximum
                or len(value) > int(definition[2])):
            raise ValueError(value)
        return value
    qualifier = "(?:(?:OF|IN)(?:" + "|".join(re.escape(group) for group in groups) + "))*"
    target = "(?:" + "|".join(re.escape(group) for group in groups) + ")" + qualifier
    values = []
    try:
        for assignment in resolution["assignments"]:
            if (not isinstance(assignment, dict) or not set(assignment) <= {"syntax", "sourceResolution"}
                    or not isinstance(assignment.get("syntax"), dict)):
                return None
            rule, body = assignment["syntax"].get("rule"), text(assignment["syntax"])
            own = re.escape(name[1]) + qualifier
            literal = re.fullmatch(r"""MOVE(['"])([^'"]*)\1TO""" + own, body, re.IGNORECASE)
            figurative = re.fullmatch(r"MOVE(?:SPACES?|LOW-VALUES?)TO" + target, body, re.IGNORECASE)
            initialize = re.fullmatch(r"INITIALIZE" + target, body, re.IGNORECASE)
            constant = re.fullmatch(r"MOVE" + _NAME + "TO" + own, body, re.IGNORECASE)
            if rule == "MoveStatementContext" and literal and "sourceResolution" not in assignment:
                values.append(program(literal[2]))
            elif (rule == "MoveStatementContext" and figurative or rule == "InitializeStatementContext" and initialize) \
                    and "sourceResolution" not in assignment:
                values.append(None)
            elif rule == "MoveStatementContext" and constant and "sourceResolution" in assignment:
                value = cics_constant_operand({"rule": "Cics_nameContext", "children": [
                    {"token": "("}, {"token": constant[1]}, {"token": ")"}]},
                    assignment["sourceResolution"], minimum, maximum)
                if value is None or len(value) > int(definition[2]):
                    return None
                values.append(value)
            else:
                return None
        if resolution["initialReaches"]:
            values.append(program(definition[4]) if definition[4] is not None else None)
    except ValueError:
        return None
    reaching = list(dict.fromkeys(values))
    return reaching if reaching == resolution["reachingValues"] and any(reaching) else None


SLOT_LOCAL_PROOF = "single-native-program:flow-insensitive-slot-writes"
SLOT_CLOSURE_PROOF = "estate-closed-world:flow-insensitive-commarea-slot-closure"
SLOT_KINDS = {"flow-insensitive-slot-writes": SLOT_LOCAL_PROOF, "closed-world-commarea-slot": SLOT_CLOSURE_PROOF}


def cics_slot_operand(operand, resolution, minimum, maximum):
    """Universe of a PIC X slot from every write the native parser listed; closure proofs are rechecked separately."""
    def text(syntax):
        return "".join(node["token"] for node in native_syntax_nodes(syntax) if "token" in node)
    keys = {"kind", "operand", "record", "definitionSyntax", "ancestorSyntax", "slots", "universe", "usageProof"}
    name = re.fullmatch(r"\(" + _NAME + r"\)", text(operand))
    if (not name or not isinstance(resolution, dict) or resolution.get("kind") not in SLOT_KINDS
            or set(resolution) != keys | ({"closureSha256", "programSourceSha256"}
                                          if resolution["kind"] == "closed-world-commarea-slot" else set())
            or resolution["usageProof"] != SLOT_KINDS[resolution["kind"]]
            or resolution["operand"] != name[1].upper() or not isinstance(resolution["slots"], dict)
            or not isinstance(resolution["universe"], list) or not isinstance(resolution["ancestorSyntax"], list)
            or not resolution["ancestorSyntax"] or name[1].upper() not in resolution["slots"]):
        return None
    groups, levels = [], []
    for ancestor in resolution["ancestorSyntax"]:
        group = (isinstance(ancestor, dict) and ancestor.get("rule") == "DataDescriptionEntryFormat1Context"
                 and re.fullmatch(_LEVEL + _NAME + r"\.", text(ancestor)))
        if not group or int(group[1]) <= (levels[-1] if levels else 0) or not levels and int(group[1]) != 1:
            return None
        levels.append(int(group[1]))
        groups.append(group[2].upper())
    if groups[0] != str(resolution["record"]).upper():
        return None
    def program(value):
        if value is None:
            return None
        if not isinstance(value, str) or not re.fullmatch(r"[A-Za-z0-9@$#]+", value) or not minimum <= len(value) <= maximum:
            raise ValueError(value)
        return value
    slot_pattern = re.compile(r"(0?[2-9]|[1-4][0-9])([A-Z0-9-]+)PIC(?:TURE)?(?:IS)?(?:X\(0*([1-8])\)|(X{1,8}))"
                              r"""(?:VALUE(?:IS)?(?:(['"])([^'"]*)\5|(SPACES?|LOW-VALUES?)))?\.""", re.IGNORECASE)
    def initialized(syntax, slot_name):
        tokens = [node["token"].upper() for node in native_syntax_nodes(syntax) if "token" in node]
        if (not tokens or tokens[0] != "INITIALIZE"
                or {"REPLACING", "VALUE", "DEFAULT", "FILLER", "(", ":"} & set(tokens)):
            return False
        operands, index = [], 1
        while index < len(tokens):
            if tokens[index] == ",":
                index += 1
                continue
            path = [tokens[index]]
            index += 1
            while index + 1 < len(tokens) and tokens[index] in ("OF", "IN"):
                path.append(tokens[index + 1])
                index += 2
            operands.append(path)
        return any(path[0] in groups + [slot_name] and all(item in groups for item in path[1:])
                   for path in operands)
    record = re.escape(groups[0])
    entry_body = r"MOVEDFHCOMMAREA\(1:(?:EIBCALEN|LENGTHOF" + record + r")\)TO" + record
    values, sources, entry_seen = {}, {}, {}
    try:
        for slot_name, slot in resolution["slots"].items():
            # Gson omits null map values, so an absent initialValue means the slot starts as SPACES.
            if (not isinstance(slot, dict) or not {"definitionSyntax", "entry", "writes"} <= set(slot)
                    or not set(slot) <= {"definitionSyntax", "initialValue", "entry", "writes"}
                    or not isinstance(slot["writes"], list) or not isinstance(slot["entry"], bool)
                    or not isinstance(slot["definitionSyntax"], dict)
                    or slot["definitionSyntax"].get("rule") != "DataDescriptionEntryFormat1Context"):
                return None
            shape = slot_pattern.fullmatch(text(slot["definitionSyntax"]))
            if not shape or shape[2].upper() != slot_name:
                return None
            initial = (shape[6] or "").rstrip(" ") or None
            if initial != slot.get("initialValue"):
                return None
            own = re.escape(slot_name) + "(?:(?:OF|IN)(?:" + "|".join(re.escape(group) for group in groups) + "))*"
            holders = "(?:" + own + "|" + "|".join(re.escape(group) for group in groups) + ")"
            found, copies, entered = [program(initial)], [], False
            for write in slot["writes"]:
                if not isinstance(write, dict) or not isinstance(write.get("syntax"), dict):
                    return None
                rule, body, effect = write["syntax"].get("rule"), text(write["syntax"]), write.get("effect")
                if not set(write) <= {"effect", "value", "source", "line", "syntax", "sourceResolution"}:
                    return None
                literal = re.fullmatch(r"""MOVE(['"])([^'"]*)\1TO""" + own, body, re.IGNORECASE)
                constant = re.fullmatch(r"MOVE" + _NAME + "TO" + own, body, re.IGNORECASE)
                if effect == "entry" and rule == "MoveStatementContext" and re.fullmatch(entry_body, body, re.IGNORECASE):
                    entered = True
                elif effect == "null" and "sourceResolution" not in write and (
                        rule == "MoveStatementContext"
                        and re.fullmatch(r"MOVE(?:SPACES?|LOW-VALUES?)TO" + holders, body, re.IGNORECASE)
                        or rule == "InitializeStatementContext" and initialized(write["syntax"], slot_name)):
                    found.append(None)
                elif effect == "value" and literal and "sourceResolution" not in write:
                    if (literal[2].rstrip(" ") or None) != write.get("value"):
                        return None
                    found.append(program(write.get("value")))
                elif effect == "value" and constant and "sourceResolution" in write:
                    value = cics_constant_operand({"rule": "Cics_nameContext", "children": [
                        {"token": "("}, {"token": constant[1]}, {"token": ")"}]},
                        write["sourceResolution"], minimum, maximum)
                    if value is None or value != write.get("value"):
                        return None
                    found.append(value)
                elif (effect == "copy" and constant and "sourceResolution" not in write
                      and constant[1].upper() == write.get("source") and write["source"] in resolution["slots"]):
                    copies.append(write["source"])
                else:
                    return None
            if entered != slot["entry"]:
                return None
            values[slot_name], sources[slot_name], entry_seen[slot_name] = found, copies, entered
    except ValueError:
        return None
    universe, pending, seen, depends_on_entry = [], [name[1].upper()], set(), False
    while pending:
        current = pending.pop()
        if current in seen:
            continue
        seen.add(current)
        depends_on_entry = depends_on_entry or entry_seen[current]
        universe.extend(values[current])
        pending.extend(sources[current])
    if seen != set(resolution["slots"]):
        return None
    if resolution["kind"] == "flow-insensitive-slot-writes":
        if depends_on_entry or set(universe) != set(resolution["universe"]):
            return None
    elif not depends_on_entry or not set(universe) <= set(resolution["universe"]):
        return None
    if len(set(resolution["universe"])) != len(resolution["universe"]):
        return None
    try:
        [program(value) for value in resolution["universe"]]
    except ValueError:
        return None
    return resolution["universe"] if any(resolution["universe"]) else None


def cics_program_targets(operand, resolution, minimum, maximum):
    """Ordered distinct program names a PROGRAM operand can denote, or None."""
    value = cics_constant_operand(operand, resolution, minimum, maximum)
    if value is not None:
        return [value]
    values = (cics_table_operand(operand, resolution, minimum, maximum)
              or cics_reaching_operand(operand, resolution, minimum, maximum)
              or cics_slot_operand(operand, resolution, minimum, maximum))
    return list(dict.fromkeys(value for value in values if value)) if values else None


def cics_program_targets_bound(syntax, programs, effect):
    operands = cics_operands(syntax, "PROGRAM")
    if len(operands) != 1 or not programs or any(binding.get("targetResolution") != programs[0].get("targetResolution")
                                                 for binding in programs):
        return False
    proof = programs[0].get("targetResolution")
    targets = (cics_program_targets(operands[0], proof, 1, 8) if isinstance(proof, dict)
               and (effect == "transfer-control" or proof.get("kind") not in (
                   "structured-reaching-assignments", *SLOT_KINDS)) else None)
    return (targets is not None and [binding.get("name") for binding in programs] == targets
            and all(binding.get("controlEffect") == effect
                    and cics_program_attributes_valid(binding.get("attributes"))
                    and all(isinstance(binding.get(key), str) and Path(binding[key]).is_absolute()
                            for key in ("sourcePath", "programSourcePath"))
                    and all(isinstance(binding.get(key), str) and re.fullmatch(r"[0-9a-f]{64}", binding[key])
                            for key in ("sourceSha256", "programSourceSha256"))
                    for binding in programs))

PLAIN_SEND_TAIL = {"Cics_send_from_waitContext", "Cics_send_defaultmaxContext", "Cics_send_lu23Context",
                   "Cics_send_from_ctlcharContext", "Cics_send_3560_3270Context", "Cics_send_eraseContext"}


def cics_plain_send(syntax):
    """SEND FROM(area) LENGTH|FLENGTH(n) [WAIT] [ERASE] [RESP/NOHANDLE]: a raw write to the principal facility."""
    def tail(node):
        if node.get("rule") == "Cics_handle_responseContext":
            return True
        if "token" in node:
            return node["token"].upper() in ("WAIT", "ERASE")
        return (node.get("rule") in PLAIN_SEND_TAIL
                and (node["rule"] != "Cics_send_eraseContext" or len(node.get("children", [])) == 1)
                and all(tail(child) for child in node.get("children", [])))
    children = syntax.get("children", [])
    group = children[1] if len(children) == 2 else {}
    parts = group.get("children", []) if group.get("rule") == "Cics_send_groupContext" else []
    return len(parts) == 2 and parts[0].get("rule") == "Cics_send_fromContext" and tail(parts[1])


def cics_terminal_variant(syntax):
    nodes = native_syntax_nodes(syntax)
    rules = {node.get("rule") for node in nodes}
    tokens = [node["token"].upper() for node in nodes if "token" in node]
    rule = syntax["rule"]
    supported = {
        "Cics_sendContext": bool(rules & {"Cics_send_control_minContext", "Cics_send_map_minContext",
                                         "Cics_send_text_stdContext"}) or cics_plain_send(syntax),
        "Cics_receiveContext": ("Cics_receive_mapContext" in rules
                                and "Cics_receive_mappingdevContext" not in rules
                                and not cics_operands(syntax, "INPARTN")),
        "Cics_returnContext": ("Cics_return_inputmsgContext" not in rules
                               and not {"CHANNEL", "ENDACTIVITY"} & set(tokens)),
        "Cics_inquireContext": ("Cics_inquire_programContext" in rules and len(cics_operands(syntax, "PROGRAM")) == 1
                                or "Cics_inquire_terminalContext" in rules
                                or "Cics_inquire_associationContext" in rules
                                and len(cics_operands(syntax, "ASSOCIATION")) == 1),
        "Cics_set_terminalContext": True,
        "Cics_bifContext": "Cics_bif_deeditContext" in rules and len(cics_operands(syntax, "FIELD")) == 1,
    }
    return supported.get(rule, False) and all(
        len(cics_operands(syntax, option)) <= 1 for option in
        ("TRANSID", "MAP", "MAPSET", "TERMINAL", "UCTRANST", "COMMAREA", "LENGTH",
         "FROM", "INTO", "FIELD", "RESP", "RESP2", "PROGRAM",
         "ASSOCIATION", "ODAPPLID", "ODUSERID", "ODFACILNAME", "ODNETWORKID", "ODFACILTYPE"))


def cics_compact(text):
    return "".join(re.findall(r"""'(?:''|[^'])*'|"(?:""|[^"])*"|\S""", text)).upper()


def native_cics_map_bound(syntax, binding):
    maps, mapsets = cics_operands(syntax, "MAP"), cics_operands(syntax, "MAPSET")
    if len(maps) != 1 or len(mapsets) > 1:
        return False
    resolution = binding.get("targetResolution")
    if resolution is not None and (not isinstance(resolution, dict)
                                   or set(resolution) != ({"map", "mapset"} if mapsets else {"map"})):
        return False
    roles = resolution or {}
    def screen(operand, proof):
        value = cics_constant_operand(operand, proof, 1, 7)
        if value is None and isinstance(proof, dict) and proof.get("kind") == "structured-reaching-assignments":
            # One SEND names one screen: only a reaching proof with a single possible name binds it.
            reaching = cics_reaching_operand(operand, proof, 1, 7) if syntax["rule"] == "Cics_sendContext" else None
            value = reaching[0] if reaching is not None and len(reaching) == 1 else None
        return value
    name = screen(maps[0], roles.get("map"))
    mapset = screen(mapsets[0], roles.get("mapset")) if mapsets else name
    definition = binding.get("definition", {})
    return (name is not None and mapset is not None and binding.get("kind") == "CICS_MAP"
            and binding.get("name") == name and binding.get("mapset") == mapset
            and isinstance(definition, dict) and definition.get("name") == name
            and definition.get("mapset") == mapset
            and definition.get("mode") in (("IN", "INOUT") if syntax["rule"] == "Cics_receiveContext"
                                           else ("OUT", "INOUT"))
            and all(isinstance(binding.get(key), str) and Path(binding[key]).is_absolute()
                    for key in ("sourcePath", "layoutPath", "outputPath"))
            and all(re.fullmatch(r"[0-9a-f]{64}", str(binding.get(key))) is not None
                    for key in ("sourceSha256", "layoutSha256", "outputSha256")))


def native_cics_continuation_bound(syntax, transactions):
    operands = cics_operands(syntax, "TRANSID")
    if not operands:
        return not transactions
    if len(operands) != 1 or len(transactions) != 1:
        return False
    transaction = transactions[0]
    name = (cics_constant_operand(operands[0], transaction["targetResolution"], 4, 4)
            if isinstance(transaction.get("targetResolution"), dict) else None)
    attributes = transaction.get("attributes", {})
    program = transaction.get("programBinding", {})
    return (name is not None and transaction.get("name") == name
            and transaction.get("controlEffect") == "transaction-continuation"
            and isinstance(attributes, dict) and set(attributes) == {"program", "dynamic", "routable", "status"}
            and attributes.get("dynamic") == "NO" and attributes.get("routable") == "NO"
            and attributes.get("status") == "ENABLED"
            and isinstance(program, dict) and program.get("kind") == "CICS_PROGRAM"
            and program.get("name") == attributes.get("program")
            and cics_program_attributes_valid(program.get("attributes"))
            and all(isinstance(resource.get(key), str) and Path(resource[key]).is_absolute()
                    for resource in (transaction, program) for key in ("sourcePath",))
            and all(re.fullmatch(r"[0-9a-f]{64}", str(resource.get("sourceSha256"))) is not None
                    for resource in (transaction, program))
            and isinstance(program.get("programSourcePath"), str) and Path(program["programSourcePath"]).is_absolute()
            and re.fullmatch(r"[0-9a-f]{64}", str(program.get("programSourceSha256"))) is not None)


def native_cics_data_bound(binding, reference):
    tokens = [node["token"].upper() for node in native_syntax_nodes(reference) if "token" in node]
    # LENGTH OF is a special register over the named item; LENGTH is reserved, never a data name.
    if len(tokens) > 2 and tokens[:2] == ["LENGTH", "OF"]:
        tokens = tokens[2:]
    if len(tokens) == 4 and tokens[1] == "(" and tokens[3] == ")":
        return native_cics_subscripted_bound(binding, tokens)
    if (not tokens or len(tokens) % 2 == 0
            or any(not re.fullmatch(r"[A-Z][A-Z0-9_-]*", token) if index % 2 == 0
                   else token not in ("OF", "IN") for index, token in enumerate(tokens))):
        return False
    names = tokens[::2]
    definition = binding["definitionSyntax"]
    def entry_name(syntax):
        entries = [node for node in native_syntax_nodes(syntax) if node.get("rule") == "EntryNameContext"]
        return ("".join(node["token"] for node in native_syntax_nodes(entries[0]) if "token" in node).upper()
                if len(entries) == 1 else "FILLER" if not entries else None)
    if entry_name(definition) != names[0]:
        return False
    if len(names) == 1:
        return True
    path = binding.get("qualificationPath")
    if (not isinstance(path, list) or len(path) < 2 or path[-1] != definition
            or any(not isinstance(item, dict) or item.get("rule") != "DataDescriptionEntryFormat1Context"
                   or not native_syntax_nodes(item) for item in path)):
        return False
    levels = [next((node["token"] for node in native_syntax_nodes(item) if "token" in node), "")
              for item in path]
    if (any(not level.isdigit() or not 1 <= int(level) <= 49 for level in levels)
            or any(int(a) >= int(b) for a, b in zip(levels, levels[1:]))
            or any(node.get("rule") == "DataPictureClauseContext"
                   for item in path[:-1] for node in native_syntax_nodes(item))):
        return False
    ancestors = iter(entry_name(item) for item in reversed(path[:-1]))
    return all(any(ancestor == qualifier for ancestor in ancestors) for qualifier in names[1:])


def native_cics_subscripted_bound(binding, tokens):
    """NAME(SUBSCRIPT): NAME sits under exactly one OCCURS entry; an identifier subscript is a declared item."""
    def entry_name(syntax):
        entries = [node for node in native_syntax_nodes(syntax) if node.get("rule") == "EntryNameContext"]
        return ("".join(node["token"] for node in native_syntax_nodes(entries[0]) if "token" in node).upper()
                if len(entries) == 1 else None)
    definition, path = binding["definitionSyntax"], binding.get("qualificationPath")
    if (not re.fullmatch(r"[A-Z][A-Z0-9_-]*", tokens[0])
            or not re.fullmatch(r"[A-Z][A-Z0-9_-]*|[1-9][0-9]{0,3}", tokens[2])
            or entry_name(definition) != tokens[0] or not isinstance(path, list) or not path or path[-1] != definition
            or any(not isinstance(item, dict) or item.get("rule") != "DataDescriptionEntryFormat1Context"
                   for item in path)
            or sum(any(node.get("rule") == "DataOccursClauseContext" for node in native_syntax_nodes(item))
                   for item in path) != 1):
        return False
    levels = [next((node["token"] for node in native_syntax_nodes(item) if "token" in node), "") for item in path]
    if (any(not level.isdigit() or not 1 <= int(level) <= 49 for level in levels)
            or int(levels[0]) != 1 or any(int(a) >= int(b) for a, b in zip(levels, levels[1:]))):
        return False
    if tokens[2].isdigit():
        return "subscriptSyntax" not in binding
    subscript = binding.get("subscriptSyntax")
    return (isinstance(subscript, dict) and subscript.get("rule") in (
                "DataDescriptionEntryFormat1Context", "DataDescriptionEntryFormat1Level77Context")
            and entry_name(subscript) == tokens[2])


def native_cics_local_bound(statement, syntax, nodes, tokens):
    rule = statement["grammarRule"]
    control = rule in ("Cics_handleContext", "Cics_linkContext", "Cics_xctlContext")
    file_operation = rule in CICS_FILE_OPERATIONS
    lock = rule in ("Cics_enqContext", "Cics_deqContext")
    asynchronous = rule == "Cics_runContext"
    queue = rule == "Cics_writeqContext"
    terminal = rule in CICS_TERMINAL_OPERATIONS
    inquiry = rule == "Cics_inquireContext" and any(node.get("rule") == "Cics_inquire_programContext" for node in nodes)
    operations = CICS_FILE_OPERATIONS[rule] if file_operation else (CICS_LOCAL_OPERATIONS[rule],)
    if (statement.get("operation") not in operations or not tokens
            or tokens[0].upper() != statement.get("operation")
            or statement.get("bindingBasis") != ("parsed-cics-terminal-operation" if terminal else
                                                "parsed-cics-file-operation" if file_operation else
                                                "parsed-cics-async-transaction" if asynchronous else
                                                "parsed-cics-queue-operation" if queue else
                                                "parsed-cics-control-binding" if control else
                                                "parsed-cics-lock-operation" if lock else "parsed-current-task-operation")
            or statement.get("bindingDiagnostics")):
        return False
    if terminal and not cics_terminal_variant(syntax):
        return False
    if terminal and cics_compact(statement["sourceText"]) != cics_compact("".join(tokens)):
        return False
    if lock and sum(token.upper() == "RESOURCE" for token in tokens) != 1:
        return False
    if rule in ("Cics_getContext", "Cics_putContext") and not any(
            node.get("rule") == ("Cics_get_channelContext" if rule == "Cics_getContext" else "Cics_put_channelContext")
            for node in nodes):
        return False
    children = syntax.get("children", [])
    condition = children[1] if rule == "Cics_handleContext" and len(children) == 2 else {}
    if condition.get("rule") == "Cics_handle_conditionContext":
        # HANDLE CONDITION name[(label)]...: labels are checked below as CICS_LABEL bindings.
        if (len(condition.get("children", [])) < 2 or any(
                child.get("rule") == "Cics_handle_responseContext" for child in condition["children"][1:])):
            return False
    elif rule == "Cics_handleContext":
        labels, cancels = len(cics_operands(syntax, "LABEL")), sum(token.upper() == "CANCEL" for token in tokens)
        if (not any(node.get("rule") == "Cics_handle_abendContext" for node in nodes)
                or {"PROGRAM", "RESET"} & {token.upper() for token in tokens}
                or (labels, cancels) not in ((1, 0), (0, 1))):
            return False
    operand = next((children[index] for node in nodes if node.get("rule") == "Cics_link_programContext"
                    for children in [node.get("children", [])] for index in range(1, len(children))
                    if children[index - 1].get("token", "").upper() == "PROGRAM"
                    and children[index].get("rule") == "Cics_nameContext"), None)
    program_nodes = {id(node) for node in native_syntax_nodes(operand)} if operand else set()
    terminal_nodes = {id(node) for operand in cics_operands(syntax, "TERMINAL")
                      for node in native_syntax_nodes(operand)}
    association_nodes = {id(node) for operand in cics_operands(syntax, "ASSOCIATION")
                         for node in native_syntax_nodes(operand)}
    references, reference_nodes = [], []
    for node in nodes:
        label = node.get("rule") == "ParagraphNameUsageContext"
        data = (node.get("rule") == "VariableNameUsageContext" and node.get("children")
                and node["children"][0].get("rule") == "GeneralIdentifierContext")
        if not (label or data) or id(node) in program_nodes:
            continue
        name = "".join(child["token"] for child in native_syntax_nodes(node) if "token" in child).upper()
        context = (rule in ("Cics_inquireContext", "Cics_set_terminalContext")
                   and name == "EIBTRMID" and id(node) in terminal_nodes
                   or rule == "Cics_inquireContext" and name == "EIBTASKN" and id(node) in association_nodes)
        references.append({"kind": "CICS_CONTEXT" if context else "CICS_LABEL" if label else "CICS_DATA",
                           "name": name, "grammarRule": node["rule"]})
        reference_nodes.append(node)
    bindings = statement.get("resourceBindings")
    if (statement.get("resourceReferences") != references or not isinstance(bindings, list)
            or any(not isinstance(binding, dict) for binding in bindings)):
        return False
    local = [binding for binding in bindings if binding.get("kind") in ("CICS_DATA", "CICS_LABEL", "CICS_CONTEXT")]
    if [(binding.get("kind"), binding.get("name")) for binding in local] != [
            (reference["kind"], reference["name"]) for reference in references]:
        return False
    for binding, reference in zip(local, reference_nodes):
        if binding["kind"] == "CICS_CONTEXT":
            role = {"EIBTRMID": "terminal-id", "EIBTASKN": "task-number"}.get(binding.get("name"))
            if (role is None or binding.get("definitionScope") != "cics-execution-interface"
                    or binding.get("role") != role):
                return False
            continue
        definition = binding.get("definitionSyntax")
        if (binding.get("definitionScope") != "expanded-parser-input"
                or not isinstance(definition, dict) or not native_syntax_nodes(definition)):
            return False
        if binding["kind"] == "CICS_DATA":
            if definition.get("rule") not in ("DataDescriptionEntryFormat1Context", "DataDescriptionEntryFormat1Level77Context"):
                return False
            if not native_cics_data_bound(binding, reference):
                return False
        elif (definition.get("rule") not in ("ParagraphDefinitionNameContext", "SectionNameContext")
              or "".join(node["token"] for node in native_syntax_nodes(definition) if "token" in node).upper() != binding["name"]):
            return False
    programs = [binding for binding in bindings if binding.get("kind") == "CICS_PROGRAM"]
    transactions = [binding for binding in bindings if binding.get("kind") == "CICS_TRANSACTION"]
    maps = [binding for binding in bindings if binding.get("kind") == "CICS_MAP"]
    files = [binding for binding in bindings if binding.get("kind") == "CICS_FILE"]
    queues = [binding for binding in bindings if binding.get("kind") == "CICS_TDQUEUE"]
    if queue:
        return (len(local) + len(queues) == len(bindings) and len(queues) == 1
                and native_cics_tdqueue_bound(syntax, queues[0]))
    if (len(local) + len(programs) + len(transactions) + len(maps) + len(files) != len(bindings)
            or len(programs) != (1 if rule == "Cics_linkContext" and not any("controlEffect" in binding for binding in programs)
                                 else len(programs) if inquiry or rule in ("Cics_xctlContext", "Cics_linkContext") else 0)
            or (rule in ("Cics_xctlContext", "Cics_linkContext") or inquiry) and not programs
            or len(files) != (1 if file_operation else 0)):
        return False
    if file_operation:
        return not transactions and not maps and native_cics_file_bound(syntax, statement["operation"], files[0])
    if rule == "Cics_xctlContext":
        return (not transactions and not maps
                and not {"CHANNEL", "INPUTMSG", "INPUTMSGLEN"} & {
                    child["token"].upper() for child in syntax.get("children", []) if "token" in child}
                and cics_program_targets_bound(syntax, programs, "transfer-control"))
    if inquiry:
        return not transactions and not maps and cics_program_targets_bound(syntax, programs, "program-definition-inquiry")
    if len(maps) != len(cics_operands(syntax, "MAP")) or any(
            not native_cics_map_bound(syntax, binding) for binding in maps):
        return False
    if rule == "Cics_returnContext":
        return native_cics_continuation_bound(syntax, transactions)
    if asynchronous:
        return native_cics_transactions_bound(nodes, tokens, transactions)
    if transactions:
        return False
    if not programs:
        return True
    if any(token.upper() in ("SYSID", "CHANNEL", "INPUTEVENT", "ACTIVITY") for token in tokens):
        return False
    if any("controlEffect" in binding for binding in programs):
        return cics_program_targets_bound(syntax, programs, "call")
    binding = programs[0]
    resolution = binding.get("targetResolution", {})
    if (operand is None or len(programs) != 1
            or not cics_program_attributes_valid(binding.get("attributes"))
            or not all(isinstance(binding.get(key), str) and Path(binding[key]).is_absolute()
                       for key in ("sourcePath", "programSourcePath"))
            or not all(isinstance(binding.get(key), str) and re.fullmatch(r"[0-9a-f]{64}", binding[key])
                       for key in ("sourceSha256", "programSourceSha256")) or not isinstance(resolution, dict)):
        return False
    value = "".join(node["token"] for node in native_syntax_nodes(operand) if "token" in node)[1:-1]
    if resolution.get("kind") == "literal":
        return (len(value) <= 10 and bool(re.fullmatch(r"""(['"])[A-Za-z0-9@$#_-]{1,8} *\1""", value))
                and binding.get("name") == value[1:-1].rstrip(" "))
    definition = resolution.get("definitionSyntax")
    if (resolution.get("kind") not in ("immutable-working-storage-literal", "immutable-local-storage-literal")
            or resolution.get("operand") != value
            or resolution.get("usageProof") != "single-native-program:all-symbol-references-are-cics-program-operands"
            or not isinstance(definition, dict) or definition.get("rule") != "DataDescriptionEntryFormat1Context"):
        return False
    declaration = "".join(node["token"] for node in native_syntax_nodes(definition) if "token" in node)
    match = re.fullmatch(r"01" + re.escape(value) + r"""PIC(?:TURE)?(?:IS)?X\(0*8\)VALUE(?:IS)?(['"])([A-Z0-9@$#_-]{1,8})( *)\1\.""",
                         declaration, re.IGNORECASE)
    return bool(match and len(match[2]) + len(match[3]) <= 8 and match[2] == binding.get("name"))


CICS_PROGRAM_ATTRIBUTES = (
    {"language": "COBOL", "dynamic": "NO"},
    # Omitted DYNAMIC is IBM CICS DYNAMIC(NO); the exporter records that provenance.
    {"language": "COBOL", "dynamic": "NO", "dynamicProvenance": "cics-default"},
    # IBM CICS runs COBOL under Language Environment with LANGUAGE(COBOL) or a blank LANGUAGE.
    {"language": "COBOL", "dynamic": "NO", "languageProvenance": "cics-blank-language-environment"},
)


def cics_program_attributes_valid(attributes):
    return attributes in CICS_PROGRAM_ATTRIBUTES


def native_cics_transactions_bound(nodes, tokens, transactions):
    variants = [node for node in nodes if node.get("rule") == "Cics_run_transidContext"]
    if len(variants) != 1 or sum(token.upper() == "CHILD" for token in tokens) != 1 or not transactions:
        return False
    operand = next((child for child in variants[0].get("children", []) if child.get("rule") == "Cics_nameContext"), None)
    value = "".join(node["token"] for node in native_syntax_nodes(operand) if "token" in node)[1:-1]
    resolution = transactions[0].get("targetResolution", {})
    if resolution.get("kind") == "literal":
        if not re.fullmatch(r"""(['"])[A-Za-z0-9@$#_-]{4}\1""", value):
            return False
        names = [value[1:-1]]
    elif resolution.get("kind") == "bounded-varying-string":
        if (resolution.get("operand") != value
                or resolution.get("usageProof") != "single-native-program:unconditional-string-with-read-only-target-and-counter-uses"):
            return False
        texts = {}
        for key, rule in (("definitionSyntax", "DataDescriptionEntryFormat1Context"),
                          ("counterDefinitionSyntax", "DataDescriptionEntryFormat1Context"),
                          ("loopHeaderSyntax", "PerformTypeContext"), ("stringSyntax", "StringStatementContext")):
            syntax = resolution.get(key)
            if not isinstance(syntax, dict) or syntax.get("rule") != rule or not native_syntax_nodes(syntax):
                return False
            texts[key] = "".join(node["token"] for node in native_syntax_nodes(syntax) if "token" in node)
        counter = resolution.get("counter")
        if not isinstance(counter, str) or not re.fullmatch(r"[A-Z][A-Z0-9_-]*", counter):
            return False
        bounds = re.fullmatch(r"(?:WITH)?(?:TESTBEFORE)?VARYING" + re.escape(counter)
                              + r"FROM([1-8])BY1UNTIL" + re.escape(counter) + r">([1-8])(?:WITHTESTBEFORE)?",
                              texts["loopHeaderSyntax"])
        construction = re.fullmatch(r"""STRING(['"])([A-Za-z0-9@$#_-]{3})\1DELIMITED(?:BY)?SIZE,?"""
                                    + re.escape(counter) + r"DELIMITED(?:BY)?SIZEINTO"
                                    + re.escape(value) + r"(?:END-STRING)?", texts["stringSyntax"])
        if (not bounds or not construction or int(bounds[1]) > int(bounds[2])
                or not re.fullmatch("01" + re.escape(value) + r"PIC(?:TURE)?(?:IS)?X\(0*4\)(?:VALUE(?:IS)?SPACES?)?\.",
                                    texts["definitionSyntax"])
                or not re.fullmatch("01" + re.escape(counter) + r"PIC(?:TURE)?(?:IS)?9(?:VALUE(?:IS)?0)?\.",
                                    texts["counterDefinitionSyntax"])):
            return False
        names = [construction[2] + str(number) for number in range(int(bounds[1]), int(bounds[2]) + 1)]
        if resolution.get("transactionNames") != names:
            return False
    else:
        return False
    if [binding.get("name") for binding in transactions] != names:
        return False
    for binding in transactions:
        attributes = binding.get("attributes", {})
        program = binding.get("programBinding", {})
        if (binding.get("targetResolution") != resolution
                or not isinstance(attributes, dict) or set(attributes) != {"program", "dynamic", "routable", "status"}
                or attributes.get("dynamic") != "NO" or attributes.get("routable") != "NO" or attributes.get("status") != "ENABLED"
                or not isinstance(program, dict) or program.get("kind") != "CICS_PROGRAM"
                or program.get("name") != attributes.get("program")
                or not cics_program_attributes_valid(program.get("attributes"))):
            return False
        for resource in (binding, program):
            if (not isinstance(resource.get("sourcePath"), str) or not Path(resource["sourcePath"]).is_absolute()
                    or re.fullmatch(r"[0-9a-f]{64}", str(resource.get("sourceSha256"))) is None):
                return False
        if (not isinstance(program.get("programSourcePath"), str) or not Path(program["programSourcePath"]).is_absolute()
                or re.fullmatch(r"[0-9a-f]{64}", str(program.get("programSourceSha256"))) is None):
            return False
    return True


def native_cics_resources(statement):
    resources = []
    for binding in statement.get("resourceBindings", []):
        resources.append(binding)
        if binding.get("kind") == "CICS_TRANSACTION":
            resources.append(binding["programBinding"])
    return resources


def cics_map_evidence_current(binding, definitions, source_root):
    from bms_symbolic_map import complete_layout_evidence
    selected = []
    for kind, key in (("CICS_MAP", "sourceSha256"), ("CICS_MAP_LAYOUT", "layoutSha256"),
                      ("COPY_INPUT", "outputSha256")):
        matches = [item for item in definitions if item["kind"] == kind and item["name"] == binding.get("name")
                   and item["sha256"] == binding.get(key)]
        if len(matches) != 1 or not Path(matches[0]["path"]).resolve().is_relative_to(Path(source_root).resolve()):
            return False
        selected.append(Path(matches[0]["path"]))
    source, layout, output = selected
    proof = complete_layout_evidence(source, output)
    return (read_json(layout) == proof and digest(layout) == binding["layoutSha256"]
            and proof["sourceSha256"] == binding["sourceSha256"]
            and proof["outputSha256"] == binding["outputSha256"]
            and [screen for screen in proof["maps"] if screen["name"] == binding["name"]
                 and screen["mapset"] == binding["mapset"]] == [binding["definition"]])


SQL_REFERENCE_RULES = {
    "Dbs_table_nameContext": "SQL_RELATION", "Dbs_view_nameContext": "SQL_RELATION",
    "Dbs_cursor_nameContext": "SQL_CURSOR", "Dbs_host_var_identifierContext": "SQL_HOST_VARIABLE",
    "Dbs_host_indicator_identifierContext": "SQL_HOST_VARIABLE",
    "Dbs_host_variable_arrayContext": "SQL_HOST_VARIABLE", "Dbs_sequence_nameContext": "SQL_SEQUENCE",
    "Dbs_statement_nameContext": "SQL_STATEMENT", "Dbs_distinct_type_nameContext": "SQL_TYPE",
    "Dbs_function_nameContext": "SQL_FUNCTION",
}

LANGUAGE_ENVIRONMENT = "IBM z/OS Language Environment"
LE_SERVICES = "https://www.ibm.com/docs/en/zos/3.1.0?topic=services-"
# name -> (platform, required documentation reference, declared parameter count)
RUNTIME_DOCUMENTATION = {
    "CEEGMT": (LANGUAGE_ENVIRONMENT, LE_SERVICES + "ceegmtget-current-greenwich-mean-time", 3),
    "CEEDATM": (LANGUAGE_ENVIRONMENT, LE_SERVICES + "ceedatmconvert-seconds-character-timestamp", 4),
    "CEEDAYS": (LANGUAGE_ENVIRONMENT, LE_SERVICES + "ceedaysconvert-date-lilian-format", 4),
    "CEELOCT": (LANGUAGE_ENVIRONMENT, LE_SERVICES + "ceeloctget-current-local-date-time", 4),
    "CEE3ABD": (LANGUAGE_ENVIRONMENT, LE_SERVICES + "cee3abdterminate-enclave-abend", 2),
    # DB PCB form: function, PCB, I/O area and the optional repeating SSA.
    "CBLTDLI": ("IBM IMS",
                "https://www.ibm.com/docs/en/SSEPH2_15.4.0/com.ibm.ims154.doc.apg/ims_imsdbcobolapp.htm", 4),
    # CICS form of DSNTIAR: eib, commarea, sqlca, message, lrecl.
    "DSNTIAC": ("IBM Db2 for z/OS", "https://www.ibm.com/docs/en/db2-for-zos/13.0.0?topic=statements-handling-sql-error-codes-in-cobol-applications", 5),
}

# Documented z/OS system macros an estate assembler routine may invoke.
ASSEMBLER_SERVICES = {
    "STIMER": "https://www.ibm.com/docs/en/SSLTBW_3.1.0/com.ibm.zos.v3r1.ieaa900/stimer.htm",
}
# Machine instructions and HLASM directives whose meaning needs no further source.
HLASM_OPERATIONS = frozenset({
    "START", "CSECT", "DSECT", "USING", "DROP", "EQU", "DS", "DC", "LTORG", "END", "COPY", "MACRO", "MEND",
    "STM", "LM", "L", "ST", "LA", "LR", "SR", "BR", "B", "BE", "BNE", "MVI", "MVC", "CLI", "CLC",
})


def assembler_statements(text):
    for line in text.splitlines():
        line = line[:71].rstrip()
        if not line.strip() or line.startswith(("*", ".*")):
            continue
        fields = line.split()
        yield fields if line[0] != " " else [""] + fields


def assembler_routine_problem(name, definition, source_root=None, estate=None):
    """Return why an estate assembler routine is not a closed, hash-bound leaf, or None."""
    try:
        includes, services = definition["includes"], definition["systemServices"]
        parameters = definition["parameters"]
        if (definition.get("kind") != "estate-assembler-routine"
                or definition.get("language") != "IBM High Level Assembler" or definition.get("entry") != name
                or not isinstance(includes, list) or not isinstance(services, list)
                or not isinstance(parameters, list) or not parameters
                or not all(isinstance(item, dict) and item.get("passing") == "BY REFERENCE"
                           and item.get("direction") in ("in", "out", "inout") and item.get("name")
                           and item.get("representation") for item in parameters)
                or not isinstance(definition.get("semantics"), dict) or not definition["semantics"]
                or not all(isinstance(item, dict) and item.get("kind") in ("macro", "copy")
                           and isinstance(item.get("name"), str) for item in includes)
                or len({item["name"] for item in includes}) != len(includes)
                or not all(isinstance(item, dict) and item.get("name") in ASSEMBLER_SERVICES
                           and item.get("documentation") == [ASSEMBLER_SERVICES[item["name"]]] for item in services)):
            return "assembler routine contract is malformed"
        files = [(definition["source"], definition["sourceSha256"])] + [
            (item["source"], item["sourceSha256"]) for item in includes]
        for relative, sha in files:
            if (not isinstance(relative, str) or Path(relative).is_absolute() or ".." in Path(relative).parts
                    or not isinstance(sha, str) or re.fullmatch(r"[0-9a-f]{64}", sha) is None
                    or estate is not None and Path(relative).parts[0] != estate):
                return f"assembler source {relative} is not an estate-relative hash-bound path"
        if source_root is None:
            return None
        texts = []
        for relative, sha in files:
            path = (Path(source_root) / relative).resolve()
            if not path.is_relative_to(Path(source_root).resolve()) or digest(path) != sha:
                return f"assembler source {relative} differs from its contract hash"
            texts.append(path.read_text(encoding="latin-1"))
        statements = list(assembler_statements(texts[0]))
        if not any(fields[0] == name and len(fields) > 1 and fields[1] in ("START", "CSECT") for fields in statements):
            return f"assembler source does not define entry {name}"
        used, copies = set(), set()
        for fields in statements:
            used.add(fields[1])
        for item, text in zip(includes, texts[1:]):
            body = list(assembler_statements(text))
            if item["kind"] == "macro":
                if len(body) < 3 or body[0][1:] != ["MACRO"] or body[1][1] != item["name"] or body[-1][1] != "MEND":
                    return f"macro {item['name']} does not define its prototype"
                body = body[2:-1]
            used.update(fields[1] for fields in body)
            statements += body
        copies = {fields[2] for fields in statements if fields[1] == "COPY" and len(fields) > 2}
        if copies != {item["name"] for item in includes if item["kind"] == "copy"}:
            return "assembler COPY members differ from the declared includes"
        declared = {item["name"] for item in includes if item["kind"] == "macro"} | {item["name"] for item in services}
        if used - HLASM_OPERATIONS != declared:
            return f"assembler operations outside the closed contract: {sorted(used - HLASM_OPERATIONS ^ declared)}"
        return None
    except (KeyError, TypeError, OSError, IndexError):
        return "assembler routine contract is malformed"


def native_profile_reference(binding):
    return (isinstance(binding, dict) and binding.get("profileKind") == "target-port"
            and isinstance(binding.get("profileName"), str) and bool(binding["profileName"])
            and isinstance(binding.get("sourcePath"), str) and Path(binding["sourcePath"]).is_absolute()
            and isinstance(binding.get("sourceSha256"), str)
            and re.fullmatch(r"[0-9a-f]{64}", binding["sourceSha256"]) is not None)


def read_bound_profile(binding, path):
    profile = read_json(path)
    if (not isinstance(profile, dict) or not native_profile_reference(binding) or digest(path) != binding["sourceSha256"]
            or profile.get("schemaVersion") != 1 or profile.get("profileKind") != binding["profileKind"]
            or profile.get("profileName") != binding["profileName"]):
        raise ValueError("configured profile identity/hash mismatch")
    return profile


def native_platform_contract_bound(node, profile):
    from platform_copybook_contracts import SYMBOLS, projection, validate, bms_usage_bound
    try:
        validate(profile)
        binding = node["runtimeContract"]
        name = binding["name"]
        contract = profile["copybooks"][name]
        if (not isinstance(binding, dict) or node.get("dependencyKind") != "PLATFORM_COPY"
                or node.get("runtimeContractBindingComplete") is not True
                or node.get("dependencyAnalysisComplete") is not False
                or node.get("sourcePath") is not None or node.get("sourceSha256") is not None
                or node.get("dependencies") != []
                or node.get("callTarget", {}).get("name") != name
                or node.get("callTarget", {}).get("programReferenceType") != "STATIC"
                or binding.get("profileKind") != "platform-replacement"
                or binding.get("profileName") != profile["profileName"]
                or binding.get("definition") != contract
                or binding.get("bindingStatus") != "complete"
                or binding.get("resolutionKind") != "symbolic-platform-replacement"
                or binding.get("runtimeMapping") != "implementation-required"
                or binding.get("sourceValues") != "not-supplied"
                or binding.get("outputSha256") != hashlib.sha256(projection(contract)).hexdigest()
                or not isinstance(binding.get("usages"), list)):
            return False
        if name == "DFHBMSCA":
            return all(bms_usage_bound(usage) for usage in binding["usages"])
        allowed = set(SYMBOLS) | {"EIBAID", "=", "NOT", "AND", "OR", "IS", "EQUAL", "TO", "(", ")"}
        def syntax_tokens(syntax, parent=None):
            if not isinstance(syntax, dict):
                raise ValueError("invalid native platform syntax")
            if set(syntax) == {"token"} and isinstance(syntax["token"], str):
                return [syntax["token"].upper()]
            if set(syntax) != {"rule", "children"} or not isinstance(syntax["children"], list):
                raise ValueError("invalid native platform grammar")
            tokens = [token for child in syntax["children"] for token in syntax_tokens(child, syntax["rule"])]
            if syntax["rule"] == "SimpleConditionContext":
                if len(tokens) == 1:
                    if parent != "AdditionalConditionContext" or tokens[0] not in SYMBOLS:
                        raise ValueError("unsupported event predicate")
                elif len(tokens) < 3 or tokens[0] != "EIBAID" or tokens[-1] not in SYMBOLS:
                    raise ValueError("event equality must compare EIBAID to a named event")
            return tokens
        def flat_tokens(syntax):
            if set(syntax) == {"token"} and isinstance(syntax["token"], str):
                return [syntax["token"].upper()]
            if set(syntax) != {"rule", "children"} or not isinstance(syntax["children"], list):
                raise ValueError("invalid native platform grammar")
            return [token for child in syntax["children"] for token in flat_tokens(child)]
        previous = None
        for position, usage in enumerate(binding["usages"]):
            role = usage.get("conditionRole")
            # EVALUATE EIBAID: one subject usage followed by one usage per WHEN <named event>.
            if role in ("evaluate-subject", "evaluate-when"):
                rule = "EvaluateSelectContext" if role == "evaluate-subject" else "EvaluateWhenContext"
                tokens = flat_tokens(usage["syntax"])
                following = (binding["usages"][position + 1].get("conditionRole")
                             if position + 1 < len(binding["usages"]) else None)
                if (usage.get("grammarRule") != rule or usage["syntax"].get("rule") != rule
                        or usage.get("coordinateSpace") != "expanded-parser-input"
                        or not isinstance(usage.get("line"), int) or usage["line"] < 1
                        or not isinstance(usage.get("sourceText"), str) or not usage["sourceText"].strip()
                        or role == "evaluate-subject" and (tokens != ["EIBAID"] or following != "evaluate-when")
                        or role == "evaluate-when" and (len(tokens) != 2 or tokens[0] != "WHEN"
                                                        or tokens[1] not in SYMBOLS
                                                        or previous not in ("evaluate-subject", "evaluate-when"))):
                    return False
                previous = role
                continue
            previous = role
            tokens = syntax_tokens(usage["syntax"])
            # An EIBAID equality ANDed with application relations is recorded as its own conjunct.
            rule = ("SimpleConditionContext" if usage.get("conditionRole") == "application-and-conjunct"
                    else "ConditionContext")
            if (usage.get("grammarRule") != rule
                    or usage["syntax"].get("rule") != rule
                    or rule == "SimpleConditionContext"
                    and (tokens[0] != "EIBAID" or sum(token in SYMBOLS for token in tokens) != 1)
                    or usage.get("coordinateSpace") != "expanded-parser-input"
                    or not isinstance(usage.get("line"), int) or usage["line"] < 1
                    or not isinstance(usage.get("sourceText"), str) or not usage["sourceText"].strip()
                    or not set(tokens) <= allowed or "EIBAID" not in tokens
                    or not ({"=", "EQUAL"} & set(tokens)) or not (set(SYMBOLS) & set(tokens))):
                return False
        return True
    except (ValueError, KeyError, TypeError):
        return False


CALL_OPERAND_FOLLOWERS = frozenset({"USING", "RETURNING", "ON", "NOT", "EXCEPTION", "OVERFLOW", "END-CALL", "."})


def dynamic_call_resolution_bound(resolution, name):
    """CALL identifier proved by the native parser to hold one never-modified program-name constant."""
    if not isinstance(resolution, dict) or set(resolution) != {"operand", "targetResolution", "callSyntax"}:
        return False
    operand, syntax = resolution["operand"], resolution["callSyntax"]
    if (not isinstance(operand, str) or not re.fullmatch(r"[A-Z][A-Z0-9-]*", operand)
            or not isinstance(syntax, dict) or syntax.get("rule") != "CallStatementContext"):
        return False
    tokens = [node["token"].upper() for node in native_syntax_nodes(syntax) if "token" in node]
    if (len(tokens) < 2 or tokens[:2] != ["CALL", operand]
            or len(tokens) > 2 and tokens[2] not in CALL_OPERAND_FOLLOWERS):
        return False
    synthetic = {"rule": "Cics_nameContext", "children": [{"token": "("}, {"token": operand}, {"token": ")"}]}
    value = cics_constant_operand(synthetic, resolution["targetResolution"], 1, 8)
    return isinstance(value, str) and value.upper() == name


def native_runtime_contract_bound(node, profile=None):
    target = node.get("callTarget", {})
    binding = node.get("runtimeContract")
    if (not isinstance(target, dict) or node.get("dependencyKind") != "EXTERNAL_RUNTIME"
            or node.get("runtimeContractBindingComplete") is not True
            or node.get("dependencyAnalysisComplete") is not False
            or node.get("sourcePath") is not None or node.get("sourceSha256") is not None
            or target.get("programReferenceType") != "STATIC"
            or node.get("dependencies") or node.get("dependencyDiagnostics")
            or not native_profile_reference(binding)
            or binding.get("name") != target.get("name")
            or binding.get("resolutionKind") != "documented-platform-contract"
            or binding.get("runtimeMapping") != "unverified"
            or "targetResolution" in binding
            and not dynamic_call_resolution_bound(binding["targetResolution"], binding["name"])):
        return False
    expected = RUNTIME_DOCUMENTATION.get(binding["name"])
    definition = binding.get("definition")
    if not isinstance(definition, dict):
        return False
    if definition.get("kind") == "estate-assembler-routine":
        return (expected is None and assembler_routine_problem(binding["name"], definition) is None
                and (profile is None or profile.get("runtimeContracts", {}).get(binding["name"]) == definition))
    if not expected:
        return False
    platform, reference, count = expected
    references = definition.get("documentation")
    parameters = definition.get("parameters")
    return (definition.get("kind") == "documented-platform-service"
            and definition.get("platform") == platform
            and isinstance(references, list) and reference in references
            and isinstance(parameters, list) and len(parameters) == count
            and all(isinstance(parameter, dict) and parameter.get("passing") == "BY REFERENCE"
                    and parameter.get("direction") in ("in", "out", "inout") and parameter.get("name")
                    and parameter.get("representation") for parameter in parameters)
            and isinstance(definition.get("semantics"), dict)
            and (profile is None or profile.get("runtimeContracts", {}).get(binding["name"]) == definition))


# Unqualified Db2 13 for z/OS built-in scalar functions missing from the grammar's dbs_inbuild_functions rule
# (https://www.ibm.com/docs/en/db2-for-zos/13?topic=functions-timestamp-format); SYSIBM leads the default SQL path.
DB2_ZOS_BUILTINS_OUTSIDE_GRAMMAR = frozenset({"TIMESTAMP_FORMAT"})
# Fully qualified Db2 for z/OS catalog tables whose contents carry no estate data
# (https://www.ibm.com/docs/en/db2-for-zos/13.0.0?topic=tables-sysdummy1: one row, IBMREQD CHAR(1)).
DB2_ZOS_PLATFORM_RELATIONS = frozenset({"SYSIBM.SYSDUMMY1"})


def native_sql_references(syntax, parent_rule=None):
    rule = syntax.get("rule")
    kind = SQL_REFERENCE_RULES.get(rule)
    nodes = native_syntax_nodes(syntax)
    if kind == "SQL_FUNCTION" and (any(node.get("rule") == "Dbs_inbuild_functionsContext" for node in nodes)
                                   or "".join(node["token"] for node in nodes if "token" in node).upper()
                                   in DB2_ZOS_BUILTINS_OUTSIDE_GRAMMAR):
        kind = None
    if (kind == "SQL_RELATION" and parent_rule != "Dbs_declare_tableContext"
            and "".join(node["token"] for node in nodes if "token" in node).upper() in DB2_ZOS_PLATFORM_RELATIONS):
        kind = None
    if kind:
        name = "".join(node["token"] for node in nodes if "token" in node).removeprefix(":")
        if '"' not in name:
            name = name.upper()
        declaration = (parent_rule == "Dbs_declare_tableContext"
                       or kind == "SQL_CURSOR" and parent_rule == "Dbs_declare_cursorContext")
        return [{"kind": kind, "name": name, "role": "declaration" if declaration else "reference",
                 "grammarRule": rule}]
    return [reference for child in syntax.get("children", []) if "rule" in child
            for reference in native_sql_references(child, rule)]


def native_sql_statement_bound(statement):
    syntax = statement.get("semanticSyntax")
    if (statement.get("kind") != "EXEC_SQL" or statement.get("grammarRule") != "SqlCodeContext"
            or statement.get("bindingStatus") != "complete" or statement.get("semanticStatus") != "complete"
            or not isinstance(syntax, dict) or syntax.get("rule") != "StartSqlRuleContext"
            or statement.get("bindingDiagnostics")):
        return False
    nodes = native_syntax_nodes(syntax)
    if not nodes:
        return False
    tokens = [node["token"] for node in nodes if "token" in node]
    operation = statement.get("operation")
    if (operation not in ("DECLARE", "SELECT", "INSERT", "UPDATE", "DELETE",
                          "OPEN", "FETCH", "CLOSE", "COMMIT", "ROLLBACK")
            or not tokens or tokens[0].upper() != operation):
        return False
    references = native_sql_references(syntax)
    if statement.get("resourceReferences") != references:
        return False
    if operation == "DECLARE" and not any(reference["role"] == "declaration" for reference in references):
        return False
    bindings = statement.get("resourceBindings")
    if not isinstance(bindings, list) or any(not isinstance(binding, dict) for binding in bindings):
        return False
    required = {(reference["kind"], reference["name"]) for reference in references if reference["role"] == "reference"}
    if {(binding.get("kind"), binding.get("name")) for binding in bindings} != required:
        return False
    for binding in bindings:
        definition = binding.get("definitionSyntax")
        scope = binding.get("definitionScope")
        if (scope not in ("expanded-parser-input", "explicit-sql-catalog")
                or not isinstance(definition, dict) or not native_syntax_nodes(definition)):
            return False
        if scope == "explicit-sql-catalog" and (
                binding.get("kind") != "SQL_RELATION"
                or not isinstance(binding.get("sourcePath"), str) or not Path(binding["sourcePath"]).is_absolute()
                or re.fullmatch(r"[0-9a-f]{64}", str(binding.get("sourceSha256"))) is None):
            return False
        if binding["kind"] == "SQL_HOST_VARIABLE":
            if definition.get("rule") not in (
                    "DataDescriptionEntryFormat1Context", "DataDescriptionEntryFormat1Level77Context"):
                return False
            names = ["".join(node["token"] for node in native_syntax_nodes(entry) if "token" in node).upper()
                     for entry in native_syntax_nodes(definition) if entry.get("rule") == "EntryNameContext"]
            if names != [binding["name"]]:
                return False
        elif binding["kind"] in ("SQL_RELATION", "SQL_CURSOR"):
            if definition.get("rule") != {"SQL_RELATION": "Dbs_declare_tableContext",
                                          "SQL_CURSOR": "Dbs_declare_cursorContext"}[binding["kind"]]:
                return False
            declaration_name = binding["name"]
            if "qualification" in binding:
                qualification = binding["qualification"]
                if (binding["kind"] != "SQL_RELATION" or not native_profile_reference(qualification)
                        or re.fullmatch(r"[A-Z][A-Z0-9_]*", binding["name"]) is None
                        or not isinstance(qualification.get("schema"), str)
                        or re.fullmatch(r"[A-Z][A-Z0-9_]*", qualification["schema"]) is None):
                    return False
                declaration_name = qualification["schema"] + "." + binding["name"]
            if not any(reference["kind"] == binding["kind"] and reference["name"] == declaration_name
                       and reference["role"] == "declaration" for reference in native_sql_references(definition)):
                return False
        else:
            return False
    return True


NATIVE_FILE_RULES = {
    "OPEN": "OpenStatementContext", "CLOSE": "CloseStatementContext", "READ": "ReadStatementContext",
    "WRITE": "WriteStatementContext", "REWRITE": "RewriteStatementContext",
    "DELETE": "DeleteStatementContext", "START": "StartStatementContext",
}


def native_file_statement_bound(statement):
    """A native file statement must bind every operand to a parsed SELECT entry and FD."""
    rule = NATIVE_FILE_RULES.get(statement.get("operation"))
    syntax = statement.get("syntax")
    operands = statement.get("fileOperands")
    bindings = statement.get("resourceBindings")
    if (rule is None or statement.get("grammarRule") != rule or statement.get("bindingStatus") != "complete"
            or not isinstance(statement.get("sourceText"), str) or not statement["sourceText"].strip()
            or not isinstance(syntax, dict) or syntax.get("rule") != rule or not native_syntax_nodes(syntax)
            or not isinstance(operands, list) or not operands or not all(isinstance(item, str) for item in operands)
            or not isinstance(bindings, list) or not bindings):
        return False
    for binding in bindings:
        if (not isinstance(binding, dict) or binding.get("kind") != "COBOL_FILE"
                or binding.get("definitionScope") != "expanded-parser-input"
                or not isinstance(binding.get("name"), str) or not isinstance(binding.get("assignment"), str)
                or not binding["assignment"] or not isinstance(binding.get("records"), list)):
            return False
        definition = binding.get("definitionSyntax")
        description = binding.get("descriptionSyntax")
        if (not isinstance(definition, dict) or definition.get("rule") != "FileControlEntryContext"
                or not isinstance(description, dict)
                or description.get("rule") != "FileDescriptionEntryClausesContext"):
            return False
        declared = [node for node in native_syntax_nodes(definition) if node.get("rule") == "FileNameContext"]
        described = [node for node in native_syntax_nodes(description) if node.get("rule") == "CobolWordContext"]
        if (not declared or not described
                or "".join(node["token"] for node in native_syntax_nodes(declared[0]) if "token" in node).upper()
                != binding["name"]
                or "".join(node["token"] for node in native_syntax_nodes(described[0]) if "token" in node).upper()
                != binding["name"]):
            return False
    names = {binding["name"] for binding in bindings}
    records = {record.upper() for binding in bindings for record in binding["records"] if isinstance(record, str)}
    return all(operand.upper() in names or operand.upper() in records for operand in operands)


def native_domain_evidence(document, kind):
    absence = document.get("dependencyAbsenceEvidence", {})
    presence = document.get("dependencyPresenceEvidence", {})
    statements = document.get("dependencyDialectStatements", [])
    if (not isinstance(absence, dict) or not isinstance(presence, dict)
            or not isinstance(statements, list)
            or any(not isinstance(statement, dict)
                   or statement.get("kind") not in ("EXEC_CICS", "EXEC_SQL", "FILE_IO") for statement in statements)):
        return False
    matching = [statement for statement in statements if statement.get("kind") == kind]
    native_files = [statement for statement in statements if statement["kind"] == "FILE_IO"]
    if native_files and not all(native_file_statement_bound(statement) for statement in native_files):
        return False
    if kind not in ABSENCE_PROOFS:
        return True
    bound_files = [statement for statement in statements
                   if statement["kind"] == "EXEC_CICS"
                   and isinstance(statement.get("resourceBindings", []), list)
                   and any(isinstance(binding, dict) and binding.get("kind") == "CICS_FILE"
                           for binding in statement.get("resourceBindings", []))]
    if kind == "FILE_IO" and presence.get(kind) == "antlr-tree:bound-file-control-resources":
        return (kind not in absence and bool(native_files) and not bound_files
                and (not any(statement["kind"] == "EXEC_SQL" for statement in statements)
                     or native_domain_evidence(document, "EXEC_SQL"))
                and all(native_cics_statement_bound(statement) for statement in statements
                        if statement["kind"] == "EXEC_CICS"))
    if kind == "FILE_IO" and presence.get(kind) == "antlr-tree:bound-cics-file-resources":
        return (kind not in absence and bool(bound_files) and not native_files
                and (not any(statement["kind"] == "EXEC_SQL" for statement in statements)
                     or native_domain_evidence(document, "EXEC_SQL"))
                and all(native_cics_statement_bound(statement) if statement["kind"] == "EXEC_CICS"
                        else native_sql_statement_bound(statement) for statement in statements))
    if kind == "FILE_IO" and presence.get(kind) == "antlr-tree:bound-sql-resources":
        return (kind not in absence and not bound_files and not native_files
                and native_domain_evidence(document, "EXEC_SQL")
                and all(native_cics_statement_bound(statement) for statement in statements
                        if statement["kind"] == "EXEC_CICS"))
    if absence.get(kind) == ABSENCE_PROOFS[kind]:
        if kind == "FILE_IO" and any(node.get("rule") == "Cics_file_nameContext"
                                     for statement in statements for node in native_syntax_nodes(statement.get("syntax"))):
            return False
        if kind == "FILE_IO" and any(statement["kind"] == "EXEC_SQL" for statement in statements):
            return False
        return not matching and (kind == "FILE_IO" or not statements)
    per_domain = {"EXEC_SQL": "antlr-tree:no-sql-contexts", "EXEC_CICS": "antlr-tree:no-cics-contexts"}
    if kind in per_domain and absence.get(kind) == per_domain[kind]:
        return bool(statements) and not matching
    if kind == "EXEC_SQL" and kind not in absence and matching and presence.get(kind) == "antlr-tree:bound-sql-resources":
        declarations = [node for statement in matching for node in native_syntax_nodes(statement.get("semanticSyntax"))
                        if node.get("rule") in ("Dbs_declare_tableContext", "Dbs_declare_cursorContext")]
        return (all(native_sql_statement_bound(statement) for statement in matching)
                and all(binding["definitionSyntax"] in declarations
                        or binding.get("definitionScope") == "explicit-sql-catalog"
                        and binding["kind"] == "SQL_RELATION" for statement in matching
                        for binding in statement.get("resourceBindings", [])
                        if binding["kind"] in ("SQL_RELATION", "SQL_CURSOR")))
    if kind != "EXEC_CICS" or kind in absence or not matching:
        return False
    if presence.get(kind) == "antlr-tree:bound-cics-resources":
        return all(native_cics_statement_bound(statement) for statement in matching)
    if presence.get(kind) != "antlr-tree:resource-free-cics-return":
        return False
    for statement in matching:
        syntax = statement.get("syntax")
        if (statement.get("operation") != "RETURN"
                or statement.get("grammarRule") != "Cics_returnContext"
                or statement.get("bindingStatus") != "complete"
                or not isinstance(statement.get("sourceText"), str)
                or statement["sourceText"].strip().upper() != "RETURN"
                or not isinstance(syntax, dict) or syntax.get("rule") != "Cics_returnContext"):
            return False
        children = syntax.get("children")
        if (not isinstance(children, list) or len(children) != 1
                or not isinstance(children[0], dict) or set(children[0]) != {"token"}
                or not isinstance(children[0]["token"], str) or children[0]["token"].upper() != "RETURN"):
            return False
    return True


PRODUCER = "cobol-rekt/dependency"
SKIP_DIRS = {".preprocessed", ".rekt-staging", ".git", ".parser-input", ".dependency-input", ".previous", ".work"}
STUB = re.compile(
    r"AUTO-GENERATED STUB COPYBOOK|\b(?:DFHAID|DFHBMSCA|SQLCA)-STUB\b"
    r"|^\s*(?:\d{6}\s*)?(?:01|77)\s+[\w-]+-STUB\b",
    re.IGNORECASE | re.MULTILINE,
)


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


def write_json(path, value):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    # Refuse to overwrite evidence from a previous attempt without preserving it.
    if path.exists():
        preserve([path], path.parent)
    path.write_text(json.dumps(value, indent=2) + "\n", encoding="utf-8")


def preserve(paths, root):
    paths = [Path(path) for path in paths if Path(path).exists()]
    if not paths:
        return
    root = Path(root)
    archive = root / ".previous" / (
        datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S") + "-" + uuid.uuid4().hex
    )
    for path in paths:
        target = archive / path.relative_to(root)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.move(str(path), str(target))


def original_files(root):
    for directory, dirs, files in os.walk(root):
        dirs[:] = sorted(d for d in dirs if d not in SKIP_DIRS and not d.startswith(".convert-"))
        for name in sorted(files):
            yield Path(directory) / name


def contaminated_report(report):
    return any(STUB.search(path.read_text(encoding="latin-1"))
               for path in original_files(report)
               if path.suffix.lower() == ".json" and path.name != "dependency-evidence.json"
               and ".previous" not in path.parts)


def definition_problem(path):
    path = Path(path)
    if STUB.search(path.read_text(encoding="latin-1")):
        return "placeholder-definition"
    generated_dirs = {
        name.strip().lower()
        for name in os.environ.get("REKT_GENERATED_COPYBOOK_DIRS", "copy-generated").split(",")
        if name.strip()
    }
    if path.suffix.lower() in (".cpy", ".cpb", ".copy") and any(part.lower() in generated_dirs for part in path.parts):
        # A generated-folder name establishes neither authenticity nor that a file is a stub.
        try:
            record = read_json(str(path) + ".preprocess.json")
            original = Path(record["originalSourcePath"])
            if (record["producer"] == "bms-symbolic-map" and original.suffix.lower() == ".bms"
                    and record["originalSourceSha256"] == digest(original)
                    and record["outputSha256"] == digest(path)):
                return None
        except (OSError, ValueError, KeyError, TypeError):
            pass
        return "unverified-definition-provenance"
    return None


def provenance(source, output, producer="rekt-preprocess"):
    write_json(str(output) + ".preprocess.json", {
        "producer": producer,
        "originalSourcePath": str(Path(source).resolve()),
        "originalSourceSha256": digest(source),
        "outputSha256": digest(output),
    })


def current_transform(output, source=None):
    try:
        record = read_json(str(output) + ".preprocess.json")
        original = Path(record["originalSourcePath"])
        if source is not None and original.resolve() != Path(source).resolve():
            return None
        if record["producer"] == "platform-event-contract/v1":
            from platform_copybook_contracts import projection, validate
            profile = read_json(original)
            validate(profile)
            if Path(output).read_bytes() != projection(profile["copybooks"][Path(output).stem.upper()]):
                return None
        elif record["producer"] not in ("rekt-preprocess", "bms-symbolic-map"):
            return None
        if digest(original) != record["originalSourceSha256"] or digest(output) != record["outputSha256"]:
            return None
        if definition_problem(original) or STUB.search(Path(output).read_text(encoding="latin-1")):
            return None
        return original
    except (OSError, ValueError, KeyError, TypeError):
        return None


def bms_grounding(output):
    """Return the BMS source a symbolic-map copybook was generated from or proven equivalent to."""
    original = current_transform(output)
    if original is None:
        return None
    if original.suffix.lower() == ".bms":
        return original
    try:
        record = read_json(str(output) + ".preprocess.json")
        if "bmsSourcePath" not in record:
            return None
        bms = Path(record["bmsSourcePath"])
        if bms.suffix.lower() != ".bms" or digest(bms) != record["bmsSourceSha256"]:
            return None
        return bms
    except (OSError, ValueError, KeyError, TypeError):
        return None


def stage(source, target, transformed=None, derived=False):
    source, target = Path(source), Path(target)
    if derived:
        original = current_transform(source)
        if original is None:
            raise ValueError(f"unverified preprocessed definition: {source}")
        selected = source
    else:
        original = source
        if source.suffix.lower() in (".cpy", ".cpb") and definition_problem(source):
            raise ValueError(f"{definition_problem(source)}: {source}")
        selected = Path(transformed) if transformed and current_transform(transformed, source) else source
        if source.suffix.lower() in (".cpy", ".cpb"):
            grounding = current_transform(source)
            if grounding is not None and grounding.suffix.lower() == ".bms":
                original, selected = grounding, source
    target.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(selected, target)
    if selected != original and Path(str(selected) + ".preprocess.json").is_file():
        shutil.copyfile(str(selected) + ".preprocess.json", str(target) + ".preprocess.json")
    bms = original if original.suffix.lower() == ".bms" else (
        bms_grounding(selected) if selected != source else None)
    if bms is not None:
        from bms_symbolic_map import complete_layout_evidence
        for suffix in (".bms-layout.json", ".bms-source"):
            Path(str(target) + suffix).unlink(missing_ok=True)
        try:
            proof = complete_layout_evidence(bms, selected)
        except (OSError, UnicodeError, ValueError) as error:
            print(f"BMS layout remains partial: {bms}: {error}", file=sys.stderr)
        else:
            if not Path(str(selected) + ".bms-layout.json").is_file() or read_json(str(selected) + ".bms-layout.json") != proof:
                write_json(str(selected) + ".bms-layout.json", proof)
            shutil.copyfile(str(selected) + ".bms-layout.json", str(target) + ".bms-layout.json")
            shutil.copyfile(bms, str(target) + ".bms-source")
    write_json(str(target) + ".rekt-source.json", {
        "originalSourcePath": str(original.resolve()),
        "originalSourceSha256": digest(original),
        "stagedSha256": digest(target),
        "transformedPath": str(selected.resolve()) if selected != original else None,
        "transformedSha256": digest(selected) if selected != original else None,
    })


def bms_layout_definition(source, copybook, name, definitions, errors):
    from bms_symbolic_map import complete_layout_evidence
    try:
        path = Path(str(copybook) + ".bms-layout.json")
        if read_json(path) != complete_layout_evidence(source, copybook):
            raise ValueError("symbolic layout manifest differs from the original BMS recipe")
        definition = {"kind": "BMS_LAYOUT", "name": Path(name).stem.upper(),
                      "path": str(path.resolve()), "sha256": digest(path)}
        if definition not in definitions:
            definitions.append(definition)
    except (OSError, UnicodeError, ValueError, KeyError, TypeError) as error:
        errors.append(f"partial-derived-bms-layout:{name}")
        errors.append(f"bms-layout-unverified:{name}:{error}")


def dependency_search_view(staging_root, relative_path):
    staging_root = Path(staging_root).resolve()
    selected = (staging_root / relative_path).resolve()
    if not selected.is_relative_to(staging_root) or Path(relative_path).is_absolute():
        raise ValueError("source-relative identity required")
    target = staging_root / ".dependency-input" / relative_path
    candidates = {}
    for path in original_files(staging_root):
        if path.suffix.lower() in (".cbl", ".cob"):
            candidates.setdefault(path.stem.upper(), []).append(path)
    if selected not in candidates.get(selected.stem.upper(), []):
        raise ValueError("selected program is not a staged program")
    preserve([target], target.parent)
    target.mkdir(parents=True)
    ambiguous = {name: [path.relative_to(staging_root).as_posix() for path in paths]
                 for name, paths in candidates.items() if len(paths) != 1}
    # The selected entry is explicit even if another estate has the same basename.
    inputs = [selected] + [paths[0] for name, paths in sorted(candidates.items())
                           if len(paths) == 1 and name != selected.stem.upper()]
    for source in inputs:
        record = read_json(str(source) + ".rekt-source.json")
        if record["stagedSha256"] != digest(source):
            raise ValueError(f"stale staged program: {source}")
        shutil.copyfile(source, target / source.name)
        shutil.copyfile(str(source) + ".rekt-source.json", str(target / source.name) + ".rekt-source.json")
    write_json(target / ".rekt-search.json", {
        "relativePath": selected.relative_to(staging_root).as_posix(),
        "ambiguousPrograms": ambiguous,
        "programs": {path.name: path.relative_to(staging_root).as_posix() for path in inputs},
    })
    return target


def exported_nodes(export, basename):
    if not isinstance(export, dict) or not isinstance(export.get("dependencies"), list):
        raise ValueError("invalid-export-schema")
    call_target = export.get("callTarget")
    if not isinstance(call_target, dict) or call_target.get("name", "").lower() != basename.lower():
        raise ValueError("export-source-identity-mismatch")
    def walk(nodes):
        for node in nodes:
            if not isinstance(node, dict):
                raise ValueError("invalid-export-dependency")
            target = node.get("callTarget", node)
            if not isinstance(target, dict) or not isinstance(target.get("name"), str) or not target["name"]:
                raise ValueError("invalid-export-target")
            yield node
            if "dependencies" in node:
                if not isinstance(node["dependencies"], list):
                    raise ValueError("invalid-export-dependency")
                yield from walk(node["dependencies"])

    return list(walk(export["dependencies"]))


def exported_names(export, basename):
    copies, calls = set(), set()
    exported_nodes(export, basename)
    for node in export["dependencies"]:
        target = node.get("callTarget", node)
        name = target["name"]
        if node.get("dependencyKind") in ("COPY", "PLATFORM_COPY") or Path(name).suffix.lower() in (".cpy", ".cpb"):
            copies.add(Path(name).stem.upper())
        elif target.get("programReferenceType") != "DYNAMIC" and node.get("dependencyKind", "CALL") in ("CALL", "EXTERNAL_RUNTIME", "CICS_LINK", "CICS_RUN"):
            calls.add(name.upper())
    return copies, calls


def native_bindings(document, source_root, relative_path, staging_root, parser_source_root, errors,
                    dependency_profile=None, sql_declarations=None):
    definitions = []
    if document.get("dependencyExportSchemaVersion") != 1:
        return False, definitions
    source = source_root / relative_path
    parsed_source = (Path(staging_root) / ".parser-input" / relative_path / source.name
                     if staging_root else source)
    search_view = Path(staging_root) / ".dependency-input" / relative_path if staging_root else None
    parser_source_root = Path(parser_source_root or ("/source" if staging_root else source_root))

    def host_path(value, *, profile=False):
        path = Path(value)
        if not path.is_absolute():
            raise ValueError("native input path must be absolute")
        if path.is_relative_to(parser_source_root):
            path = source_root / path.relative_to(parser_source_root)
        elif not profile or dependency_profile is None:
            raise ValueError("native input outside declared parser source root")
        if profile and dependency_profile is not None:
            configured = Path(dependency_profile)
            if not configured.is_absolute():
                raise ValueError("configured dependency profile path must be absolute")
            if configured.is_relative_to(parser_source_root):
                configured = source_root / configured.relative_to(parser_source_root)
            if path.resolve() != configured.resolve():
                raise ValueError("native profile does not match explicitly configured dependency profile")
        return path

    try:
        native_source = host_path(document["sourcePath"]).resolve()
        expected_source = search_view / source.name if search_view is not None and search_view.is_dir() else parsed_source
        if (native_source != expected_source.resolve()
                or document["sourceSha256"] != digest(parsed_source)
                or digest(native_source) != digest(parsed_source)
                or (staging_root and digest(native_source) != digest(Path(staging_root) / relative_path))):
            raise ValueError("native parser source identity/hash mismatch")
        search_record = read_json(search_view / ".rekt-search.json") if search_view is not None and search_view.is_dir() else {}
        ambiguous = search_record.get("ambiguousPrograms", {})
    except (OSError, ValueError, TypeError, KeyError) as error:
        errors.append(f"native-export-source-unverified:{error}")
        return False, definitions
    for node in [document] + exported_nodes(document, source.name):
        diagnostics = node.get("dependencyDiagnostics", [])
        if not isinstance(diagnostics, list) or any(not isinstance(item, str) for item in diagnostics):
            errors.append("invalid-native-dependency-diagnostics")
        else:
            errors.extend(f"native:{item}" for item in diagnostics)
        derived_inputs = node.get("dependencyDerivedInputs", [])
        if not isinstance(derived_inputs, list):
            errors.append("invalid-native-derived-inputs")
            derived_inputs = []
        for binding in derived_inputs:
            try:
                if not isinstance(binding, dict) or binding.get("kind") != "BMS_LAYOUT":
                    raise ValueError("unsupported native derived input")
                for key, hash_key in (("sourcePath", "sourceSha256"), ("layoutPath", "layoutSha256"),
                                      ("outputPath", "outputSha256")):
                    path = host_path(binding[key]).resolve()
                    if not path.is_relative_to(source_root) or digest(path) != binding[hash_key]:
                        raise ValueError("native derived input path/hash mismatch")
                if (binding["layoutPath"] != binding["outputPath"] + ".bms-layout.json"
                        or binding["sourcePath"] != binding["outputPath"] + ".bms-source"
                        or not any(child.get("dependencyKind") == "COPY"
                                   and child.get("sourcePath") == binding["outputPath"]
                                   and child.get("sourceSha256") == binding["outputSha256"]
                                   for child in node.get("dependencies", []))):
                    raise ValueError("native BMS layout is not bound to a parsed COPY")
            except (OSError, ValueError, KeyError, TypeError) as error:
                errors.append(f"native-bms-layout-unverified:{error}")
        for statement in node.get("dependencyDialectStatements", []):
            if not isinstance(statement, dict):
                errors.append("invalid-native-dialect-statement")
                continue
            bindings = statement.get("resourceBindings", [])
            if not isinstance(bindings, list) or any(not isinstance(binding, dict) for binding in bindings):
                errors.append("invalid-native-resource-bindings")
                continue
            for binding in bindings:
                if binding.get("definitionScope") == "explicit-sql-catalog":
                    try:
                        path = host_path(binding["sourcePath"]).resolve()
                        configured = {host_path(value).resolve() for value in (sql_declarations or "").split(os.pathsep)
                                      if value}
                        if (not path.is_relative_to(source_root) or path not in configured
                                or digest(path) != binding["sourceSha256"] or definition_problem(path)
                                or not native_sql_statement_bound(statement)):
                            raise ValueError("SQL declaration does not match an explicit native catalog input")
                        definition = {"kind": "SQL_DECLARATION", "name": binding["name"],
                                      "path": str(path), "sha256": digest(path)}
                        if definition not in definitions:
                            definitions.append(definition)
                    except (OSError, ValueError, KeyError, TypeError) as error:
                        errors.append(f"native-sql-catalog-unverified:{error}")
                if "qualification" not in binding:
                    continue
                qualification = binding["qualification"]
                try:
                    path = host_path(qualification["sourcePath"], profile=True).resolve()
                    profile = read_bound_profile(qualification, path)
                    if (not native_sql_statement_bound(statement)
                            or profile.get("sqlDefaultSchema") != qualification["schema"]):
                        raise ValueError("configured SQL qualification is not supported by the native declaration")
                    definition = {"kind": "ANALYSIS_PROFILE", "name": profile["profileName"],
                                  "path": str(path), "sha256": digest(path)}
                    if definition not in definitions:
                        definitions.append(definition)
                except (OSError, ValueError, KeyError, TypeError) as error:
                    errors.append(f"native-sql-profile-unverified:{error}")
            if not isinstance(statement, dict) or not native_cics_statement_bound(statement):
                continue
            for binding in native_cics_resources(statement):
                if binding.get("kind") == "CICS_MAP":
                    try:
                        output = host_path(binding["outputPath"]).resolve()
                        layout = host_path(binding["layoutPath"]).resolve()
                        snapshot = host_path(binding["sourcePath"]).resolve()
                        for path, key in ((output, "outputSha256"), (layout, "layoutSha256"),
                                          (snapshot, "sourceSha256")):
                            if not path.is_relative_to(source_root) or digest(path) != binding[key]:
                                raise ValueError("native map resource path/hash mismatch")
                        record = read_json(str(output) + ".rekt-source.json")
                        original, transformed = Path(record["originalSourcePath"]), Path(record["transformedPath"])
                        if (not original.resolve().is_relative_to(source_root)
                                or not transformed.resolve().is_relative_to(source_root)
                                or record["originalSourceSha256"] != digest(original)
                                or record["transformedSha256"] != digest(transformed)
                                or record["stagedSha256"] != digest(output)
                                or current_transform(transformed, original) is None):
                            raise ValueError("native map resource lacks current source provenance")
                        bms = bms_grounding(transformed)
                        if bms is None:
                            raise ValueError("native map resource lacks a grounded BMS source")
                        additions = [
                            {"kind": "CICS_MAP", "name": binding["name"], "path": str(bms.resolve()),
                             "sha256": digest(bms)},
                            {"kind": "CICS_MAP_LAYOUT", "name": binding["name"],
                             "path": str(Path(str(transformed) + ".bms-layout.json").resolve()),
                             "sha256": binding["layoutSha256"]},
                            {"kind": "COPY_INPUT", "name": binding["name"], "path": str(transformed.resolve()),
                             "sha256": digest(transformed)},
                        ]
                        if not cics_map_evidence_current(binding, additions, source_root):
                            raise ValueError("native map differs from its source-backed BMS recipe")
                        for definition in additions:
                            if definition not in definitions:
                                definitions.append(definition)
                    except (OSError, ValueError, KeyError, TypeError) as error:
                        errors.append(f"native-cics-map-unverified:{binding.get('name')}:{error}")
                    continue
                if binding.get("kind") not in ("CICS_FILE", "CICS_PROGRAM", "CICS_TRANSACTION", "CICS_TDQUEUE"):
                    continue
                try:
                    path = host_path(binding["sourcePath"]).resolve()
                    if not path.is_relative_to(source_root) or digest(path) != binding["sourceSha256"]:
                        raise ValueError("native CICS resource definition identity/hash mismatch")
                    if binding["kind"] == "CICS_PROGRAM":
                        program_path = host_path(binding["programSourcePath"]).resolve()
                        continuation = statement.get("grammarRule") in ("Cics_returnContext", "Cics_xctlContext", "Cics_inquireContext")
                        if (not program_path.is_relative_to(source_root)
                                or digest(program_path) != binding["programSourceSha256"]
                                or not continuation and not any(child.get("dependencyKind") in ("CALL", "CICS_LINK", "CICS_RUN")
                                           and child.get("callTarget", {}).get("name") == binding["name"]
                                           and child.get("sourcePath") == binding["programSourcePath"]
                                           and child.get("sourceSha256") == binding["programSourceSha256"]
                                           for child in node.get("dependencies", []))):
                            raise ValueError("native CICS callee does not match its parsed source")
                        if continuation:
                            original = current = program_path
                            if staging_root:
                                record = read_json(str(program_path) + ".rekt-source.json")
                                original = Path(record["originalSourcePath"]).resolve()
                                current = Path(record.get("transformedPath") or original).resolve()
                                relative = Path(search_record["programs"][program_path.name])
                                canonical = (Path(staging_root) / relative).resolve()
                                if (relative.is_absolute() or not canonical.is_relative_to(Path(staging_root).resolve())
                                        or program_path.parent != search_view.resolve()
                                        or record != read_json(str(canonical) + ".rekt-source.json")
                                        or original != (source_root / relative).resolve()
                                        or record["stagedSha256"] != digest(program_path)
                                        or record["originalSourceSha256"] != digest(original)
                                        or current != original and current_transform(current, original) is None):
                                    raise ValueError("continuation source provenance invalid")
                            if (not current.is_relative_to(source_root) or not original.is_relative_to(source_root)
                                    or digest(current) != binding["programSourceSha256"]
                                    or definition_problem(current) or definition_problem(original)):
                                raise ValueError("continuation does not match genuine supplied program source")
                            for kind, selected in (("CICS_CONTINUATION", current), ("CICS_CONTINUATION_SOURCE", original)):
                                definition = {"kind": kind, "name": binding["name"], "path": str(selected),
                                              "sha256": digest(selected)}
                                if definition not in definitions:
                                    definitions.append(definition)
                    definition = {"kind": binding["kind"], "name": binding["name"],
                                  "path": str(path), "sha256": digest(path)}
                    if definition not in definitions:
                        definitions.append(definition)
                    for dataset in binding.get("datasetDefinitions", []) if binding["kind"] == "CICS_FILE" else []:
                        dataset_path = host_path(dataset["sourcePath"]).resolve()
                        if (not dataset_path.is_relative_to(source_root)
                                or digest(dataset_path) != dataset["sourceSha256"]):
                            raise ValueError("native VSAM dataset definition identity/hash mismatch")
                        definition = {"kind": "CICS_DATASET", "name": dataset["name"],
                                      "path": str(dataset_path), "sha256": dataset["sourceSha256"]}
                        if definition not in definitions:
                            definitions.append(definition)
                except (OSError, ValueError, KeyError, TypeError) as error:
                    errors.append(f"native-cics-resource-definition-unverified:{binding.get('name')}:{error}")
        if node is document:
            continue
        target = node.get("callTarget", node)
        kind = node.get("dependencyKind")
        if kind in ("CICS_LINK", "CICS_RUN"):
            kind = "CALL"
        name = target["name"]
        if kind == "PLATFORM_COPY":
            try:
                from platform_copybook_contracts import projection, validate
                binding = node["runtimeContract"]
                configured = os.environ.get("REKT_PLATFORM_CONTRACTS")
                if not configured or not Path(configured).is_absolute():
                    raise ValueError("no explicitly configured platform replacement profile")
                profile_path = Path(configured).resolve()
                native_profile = Path(binding["sourcePath"])
                if native_profile.is_relative_to(parser_source_root):
                    native_profile = source_root / native_profile.relative_to(parser_source_root)
                profile = read_json(profile_path)
                if native_profile.resolve() != profile_path:
                    raise ValueError("platform replacement profile path mismatch")
                if binding["sourceSha256"] != digest(profile_path):
                    raise ValueError("platform replacement profile SHA256 mismatch")
                if not native_platform_contract_bound(node, profile):
                    if any("UNRESOLVED_CICS_AID_SEMANTIC:" in issue
                           for issue in document.get("dependencyDiagnostics", [])):
                        raise ValueError("UNRESOLVED_CICS_AID_SEMANTIC: unsupported standalone DFHAID comparison")
                    raise ValueError("platform replacement projection or symbol usage is unsupported")
                staged = host_path(binding["outputPath"]).resolve()
                record = read_json(str(staged) + ".rekt-source.json")
                transformed = Path(record["transformedPath"]).resolve()
                if (staging_root is None or staged != (Path(staging_root) / f"{name}.cpy").resolve()
                        or transformed != (profile_path.parent / f"{name}.cpy").resolve()
                        or Path(record["originalSourcePath"]).resolve() != profile_path
                        or record["originalSourceSha256"] != digest(profile_path)
                        or record["stagedSha256"] != digest(staged)
                        or record["transformedSha256"] != digest(transformed)
                        or current_transform(transformed, profile_path) is None
                        or staged.read_bytes() != projection(profile["copybooks"][name])
                        or digest(transformed) != binding["outputSha256"]):
                    raise ValueError("platform projection does not match the explicit symbolic recipe")
                for definition in [
                    {"kind": "PLATFORM_COPY", "name": name, "path": str(profile_path), "sha256": digest(profile_path)},
                    {"kind": "PLATFORM_INPUT", "name": name, "path": str(transformed), "sha256": digest(transformed)},
                ]:
                    if definition not in definitions:
                        definitions.append(definition)
            except (OSError, ValueError, KeyError, TypeError) as error:
                errors.append(f"native-platform-contract-unverified:{name}:{error}")
            continue
        if kind == "EXTERNAL_RUNTIME":
            try:
                binding = node["runtimeContract"]
                path = host_path(binding["sourcePath"], profile=True).resolve()
                profile = read_bound_profile(binding, path)
                if not native_runtime_contract_bound(node, profile):
                    raise ValueError("invalid documented runtime-contract leaf")
                contract = binding["definition"]
                additions = [{"kind": "RUNTIME_CONTRACT", "name": name, "path": str(path), "sha256": digest(path)}]
                if contract.get("kind") == "estate-assembler-routine":
                    problem = assembler_routine_problem(name, contract, source_root, Path(relative_path).parts[0])
                    if problem:
                        raise ValueError(problem)
                    additions += [{"kind": "ASSEMBLER_SOURCE", "name": item.get("name", name),
                                   "path": str((source_root / item["source"]).resolve()),
                                   "sha256": item["sourceSha256"]}
                                  for item in [contract] + contract["includes"]]
                for definition in additions:
                    if definition not in definitions:
                        definitions.append(definition)
            except (OSError, ValueError, KeyError, TypeError) as error:
                errors.append(f"native-runtime-contract-unverified:{name}:{error}")
            continue
        target_stem = Path(name).stem if Path(name).suffix.lower() in (".cbl", ".cob") else name
        if kind == "CALL" and target_stem.upper() in ambiguous:
            errors.append(f"ambiguous-call-target:{name}")
        if document.get("dependencyAnalysisComplete") is True and kind != "COPY":
            child_coverage = node.get("dependencyCoverage", {})
            if (node.get("dependencyAnalysisComplete") is not True
                    or not isinstance(child_coverage, dict)
                    or any(child_coverage.get(item) != "complete" for item in KINDS)
                    or any(not native_domain_evidence(node, item) for item in ABSENCE_PROOFS)):
                errors.append(f"native-incomplete-callee:{name}")
        if kind not in ("COPY", "CALL"):
            continue
        if target.get("programReferenceType") != "STATIC":
            errors.append(f"dynamic-call-target-unresolved:{name}")
            continue
        try:
            path = host_path(node["sourcePath"]).resolve()
            if digest(path) != node["sourceSha256"]:
                raise ValueError("native resolved input hash mismatch")
            if STUB.search(path.read_text(encoding="latin-1")):
                raise ValueError("placeholder-definition")
            if staging_root:
                record = read_json(str(path) + ".rekt-source.json")
                original = Path(record["originalSourcePath"])
                if kind == "CALL" and search_view is not None and path.is_relative_to(search_view):
                    staged_relative = Path(search_record["programs"][path.name])
                    canonical = (Path(staging_root) / staged_relative).resolve()
                    if (staged_relative.is_absolute() or not canonical.is_relative_to(Path(staging_root).resolve())
                            or path.parent != search_view.resolve()
                            or record != read_json(str(canonical) + ".rekt-source.json")
                            or digest(canonical) != digest(path)
                            or original.resolve() != (source_root / staged_relative).resolve()):
                        raise ValueError("callee search view provenance invalid")
                if definition_problem(original):
                    raise ValueError(definition_problem(original))
                if (record["originalSourceSha256"] != digest(original)
                        or record["stagedSha256"] != digest(path)):
                    raise ValueError("staged definition provenance invalid")
                transformed = record.get("transformedPath")
                if transformed:
                    transformed = Path(transformed)
                    if (current_transform(transformed, original) is None
                            or record["transformedSha256"] != digest(transformed)
                            or digest(transformed) != digest(path)):
                        raise ValueError("derived definition provenance invalid")
                    if kind == "CALL":
                        definitions.append({"kind": "CALL", "name": name,
                                            "path": str(transformed.resolve()), "sha256": digest(transformed)})
                    elif original.suffix.lower() == ".bms":
                        definitions.append({"kind": "BMS", "name": Path(name).stem.upper(),
                                            "path": str(original.resolve()), "sha256": digest(original)})
                        original = transformed
                    else:
                        definitions.append({"kind": "COPY_INPUT", "name": Path(name).stem.upper(),
                                            "path": str(transformed.resolve()), "sha256": digest(transformed)})
                path = original.resolve()
            if definition_problem(path):
                raise ValueError(definition_problem(path))
            grounding = current_transform(path) if kind == "COPY" else None
            layout_output = path
            if (kind == "COPY" and staging_root and transformed and original.suffix.lower() != ".bms"
                    and (grounding is None or grounding.suffix.lower() != ".bms")):
                grounding, layout_output = bms_grounding(transformed), transformed.resolve()
            if grounding is not None and grounding.suffix.lower() == ".bms":
                if not any(item["kind"] == "BMS" and item["path"] == str(grounding.resolve())
                           for item in definitions):
                    definitions.append({"kind": "BMS", "name": Path(name).stem.upper(),
                                        "path": str(grounding.resolve()), "sha256": digest(grounding)})
                bms_layout_definition(grounding, layout_output, name, definitions, errors)
                proof = Path(str(layout_output) + ".bms-layout.json")
                if not any(binding.get("kind") == "BMS_LAYOUT"
                           and binding.get("outputPath") == node["sourcePath"]
                           and binding.get("outputSha256") == node["sourceSha256"]
                           and binding.get("layoutSha256") == (digest(proof) if proof.is_file() else None)
                           for owner in [document] + exported_nodes(document, source.name)
                           for binding in owner.get("dependencyDerivedInputs", []) if isinstance(binding, dict)):
                    errors.append(f"native-bms-layout-binding-missing:{name}")
            definitions.append({"kind": kind, "name": Path(name).stem.upper() if kind == "COPY" else name,
                                "path": str(path), "sha256": digest(path)})
        except (OSError, ValueError, KeyError, TypeError) as error:
            errors.append(f"native-resolved-definition-unverified:{name}:{error}")
    for owner in [document] + exported_nodes(document, source.name):
        if owner.get("dependencyKind") == "PLATFORM_COPY" and owner.get("callTarget", {}).get("name") == "DFHBMSCA":
            from platform_copybook_contracts import bms_layouts_bound, bms_source_uses_bound
            try:
                if not bms_layouts_bound(owner["runtimeContract"], definitions):
                    raise ValueError("attribute assignments lack hash-bound BMS field layouts")
                parent = next(program for program in [document] + exported_nodes(document, source.name)
                              if any(child is owner for child in program.get("dependencies", [])))
                original = source
                if parent is not document:
                    selected = host_path(parent["sourcePath"])
                    original = (Path(read_json(str(selected) + ".rekt-source.json")["originalSourcePath"])
                                if staging_root else current_transform(selected) or selected)
                if not bms_source_uses_bound(owner["runtimeContract"], original.read_text(encoding="latin-1"), definitions):
                    raise ValueError("BMS attribute uses omit or differ from the original source")
            except (OSError, ValueError, KeyError, TypeError) as error:
                errors.append(f"native-platform-contract-unverified:DFHBMSCA:{error}")
    return True, definitions


# COBOL Language Support reports, at INFO severity, that a COPY text-name exceeds the
# 8-character z/OS PDS member limit. It is not a resolution failure.
COPY_NAME_LENGTH_INFO = re.compile(r"Copybook declaration has more than 8 characters for: ([A-Z0-9][A-Z0-9_-]*)",
                                   re.IGNORECASE)
COPY_RESOLUTION_PROBLEMS = ("unresolved-copybook", "unverified-copybook", "ambiguous-copybook",
                            "unverified-staged-copybook", "native:unresolved-copybook")


def drop_bound_copy_name_length_notes(result, errors, host_notes):
    """Keep the name-length note as a blocker unless that exact COPY was resolved and hash-bound."""
    bound = {item["name"].upper() for item in result["definitions"]
             if item.get("kind") == "COPY" and item.get("sha256")}
    problems = {item.split(":")[-1].upper() for item in errors
                if item.rsplit(":", 1)[0] in COPY_RESOLUTION_PROBLEMS}
    problems |= {name.upper() for item in errors
                 for name in re.findall(r"missing copybook:([A-Z0-9][A-Z0-9_-]*)", item, re.IGNORECASE)}

    def benign(diagnostic, prefix):
        if not diagnostic.startswith(prefix):
            return False
        match = COPY_NAME_LENGTH_INFO.fullmatch(diagnostic[len(prefix):])
        return bool(match) and match.group(1).upper() in bound - problems

    errors[:] = [item for item in errors if not benign(item, "native:parser-diagnostic::")]
    errors.extend(item for item in host_notes if not benign(item, "parser-diagnostic:None:"))


def build_evidence(source_root, relative_path, output_root, export_exit_code, staging_root=None,
                   parser_source_root=None, dependency_profile=None, sql_declarations=None):
    source_root, output_root = Path(source_root).resolve(), Path(output_root).resolve()
    source = (source_root / relative_path).resolve()
    if not source.is_relative_to(source_root) or Path(relative_path).is_absolute():
        raise ValueError("source-relative identity required")
    relative_path = source.relative_to(source_root).as_posix()
    export_path = output_root / (relative_path + "-deps.json")
    report = output_root / (relative_path + ".report")
    result = {
        "schemaVersion": 1,
        "producer": PRODUCER,
        "relativePath": relative_path,
        "originalSourcePath": str(source),
        "originalSourceSha256": digest(source),
        "exportPath": str(export_path),
        "exportSha256": digest(export_path) if export_path.is_file() else None,
        "exportExitCode": export_exit_code,
        "coverage": dict.fromkeys(KINDS, "unavailable"),
        "definitions": [],
        "unresolvedDiagnostics": [],
        "placeholderContaminated": False,
        "provenance": "parser-dependency-export; source scans are contradiction guards only",
    }
    errors = result["unresolvedDiagnostics"]
    if staging_root is not None:
        try:
            staged_source = Path(staging_root) / relative_path
            record = read_json(str(staged_source) + ".rekt-source.json")
            if (Path(record["originalSourcePath"]).resolve() != source
                    or record["originalSourceSha256"] != digest(source)
                    or record["stagedSha256"] != digest(staged_source)
                    or STUB.search(staged_source.read_text(encoding="latin-1"))):
                raise ValueError("stale-or-placeholder-source")
        except (OSError, ValueError, KeyError, TypeError):
            errors.append("unverified-staged-source")
    if STUB.search(source.read_text(encoding="latin-1")):
        result["placeholderContaminated"] = True
        errors.append("placeholder-source")
    if contaminated_report(report):
        result["placeholderContaminated"] = True
        errors.append("placeholder-parser-artifact")
    log_path = output_root / (relative_path + ".parse.log")
    if log_path.is_file():
        result["parserLogPath"] = str(log_path)
        result["parserLogSha256"] = digest(log_path)
    copies, calls = set(), set()
    export_valid = False
    export_document = None
    if export_exit_code != 0:
        errors.append(f"dependency-export-failed:{export_exit_code}")
    if export_path.is_file() or export_exit_code == 0:
        try:
            export_document = read_json(export_path)
            copies, calls = exported_names(export_document, source.name)
            export_valid = True
            result["coverage"]["COPY"] = "partial"
            result["coverage"]["CALL"] = "partial"
        except (OSError, ValueError, TypeError) as exc:
            errors.append(f"dependency-export-invalid:{exc}")

    diagnostic_path = report / "parse-diagnostics.json"
    name_length_notes = []
    if diagnostic_path.is_file():
        try:
            diagnostics = read_json(diagnostic_path)
            if not isinstance(diagnostics, list):
                raise ValueError("expected diagnostics array")
            for item in diagnostics:
                if not isinstance(item, dict):
                    raise ValueError("invalid diagnostic")
                diagnostic = f"parser-diagnostic:{item.get('code')}:{item.get('message')}"
                if item.get("severity") == "INFO" and item.get("code") is None \
                        and COPY_NAME_LENGTH_INFO.fullmatch(str(item.get("message"))):
                    name_length_notes.append(diagnostic)
                else:
                    errors.append(diagnostic)
        except (OSError, ValueError) as exc:
            errors.append(f"invalid-parser-diagnostics:{exc}")

    text = source.read_text(encoding="latin-1")
    expected = referenced_names(text)
    for missing in sorted(expected - copies):
        errors.append(f"copy-not-exported:{missing}")
    if expected and not copies:
        errors.append("empty-COPY-export-contradiction")
    guarded = dependency_guard_text(text)
    for target in re.findall(r"(?<![\w-])CALL\s+['\"]([^'\"]+)['\"]", guarded, re.IGNORECASE):
        if target.upper() not in calls:
            errors.append(f"call-not-exported:{target}")
    # A dynamic operand passes only when an exported runtime leaf carries its constant proof; that proof is
    # re-verified by native_runtime_contract_bound before the leaf is trusted.
    resolved = {node["runtimeContract"]["targetResolution"]["operand"].upper()
                for node in (export_document or {}).get("dependencies", []) if isinstance(node, dict)
                and node.get("dependencyKind") == "EXTERNAL_RUNTIME" and isinstance(node.get("runtimeContract"), dict)
                and isinstance(node["runtimeContract"].get("targetResolution"), dict)
                and isinstance(node["runtimeContract"]["targetResolution"].get("operand"), str)}
    if any(operand.upper() not in resolved
           for operand in re.findall(r"(?<![\w-])CALL\s+(?!['\"])([A-Z0-9_-]+)", guarded, re.IGNORECASE)):
        errors.append("dynamic-CALL-target-unresolved")

    by_name = {}
    for path in original_files(source_root):
        if path.suffix.lower() in (".cpy", ".cpb"):
            by_name.setdefault(path.stem.upper(), []).append(path)
    # Include original references for missing/placeholder diagnostics, never as exported deps.
    native = False
    if export_valid and export_document.get("dependencyExportSchemaVersion") == 1:
        native, result["definitions"] = native_bindings(
            export_document, source_root, relative_path, staging_root, parser_source_root, errors,
            dependency_profile, sql_declarations)
        closure_definitions, closure_problem = commarea_closure_bindings(export_document, output_root, relative_path)
        result["definitions"].extend(closure_definitions)
        if closure_problem:
            errors.append(closure_problem)
        if any(item.endswith(":placeholder-definition") for item in errors):
            result["placeholderContaminated"] = True
    pending, visited = list(sorted(copies | expected)), set()
    while pending:
        name = pending.pop()
        if name in visited:
            continue
        visited.add(name)
        if any(item["kind"] == "PLATFORM_COPY" and item["name"].upper() == name for item in result["definitions"]):
            continue
        candidates = by_name.get(name, [])
        genuine = [path for path in candidates if not definition_problem(path)]
        if not genuine:
            if staging_root is not None:
                # Grounded BMS outputs and bundled definitions carry an input/output
                # hash chain; directory names or unmarked historical files do not.
                try:
                    staged = next(path for path in Path(staging_root).iterdir()
                                  if path.stem.upper() == name and path.suffix.lower() in (".cpy", ".cpb"))
                    record = read_json(str(staged) + ".rekt-source.json")
                    transformed = Path(record["transformedPath"])
                    original = current_transform(transformed)
                    if (original is None or original.resolve() != Path(record["originalSourcePath"]).resolve()
                            or record["originalSourceSha256"] != digest(original)
                            or record["stagedSha256"] != digest(staged)
                            or record["transformedSha256"] != digest(transformed)):
                        raise ValueError("invalid-derived-definition")
                    if name in copies and not native:
                        errors.append(f"definition-resolution-not-exported:{name}")
                        if original.suffix.lower() == ".bms":
                            bms_layout_definition(original, transformed, name, result["definitions"], errors)
                    continue
                except (OSError, ValueError, KeyError, TypeError, StopIteration):
                    pass
            if any(definition_problem(path) == "placeholder-definition" for path in candidates):
                result["placeholderContaminated"] = True
            errors.append(f"unresolved-copybook:{name}" if not candidates else f"unverified-copybook:{name}")
            continue
        if len({digest(path) for path in genuine}) != 1:
            errors.append(f"ambiguous-copybook:{name}")
            continue
        path = sorted(genuine)[0]
        staged_verified = staging_root is None
        if staging_root is not None:
            staged = Path(staging_root) / path.name
            try:
                staged_record = read_json(str(staged) + ".rekt-source.json")
                if (Path(staged_record["originalSourcePath"]).resolve() != path.resolve()
                        or staged_record["originalSourceSha256"] != digest(path)
                        or staged_record["stagedSha256"] != digest(staged)
                        or STUB.search(staged.read_text(encoding="latin-1"))):
                    raise ValueError("stale-or-placeholder-staging")
                staged_verified = True
            except (OSError, ValueError, KeyError, TypeError):
                errors.append(f"unverified-staged-copybook:{name}")
        if name in copies and staged_verified and not native:
            errors.append(f"definition-resolution-not-exported:{name}")
        nested = referenced_names(path.read_text(encoding="latin-1"))
        for missing in sorted(nested - copies):
            errors.append(f"nested-copy-not-exported:{missing}")
        pending.extend(nested - visited)

    drop_bound_copy_name_length_notes(result, errors, name_length_notes)

    # The legacy tree has no per-kind completeness guarantee. Even an empty, exit-0
    # export is not proof that all dependency kinds were visited by the parser.
    if native:
        errors.extend(missing_native_operations(source, export_document))
        declared = export_document.get("dependencyCoverage", {})
        if not isinstance(declared, dict):
            errors.append("invalid-native-coverage")
        else:
            for kind in KINDS:
                status = declared.get(kind)
                if status not in ("complete", "partial", "unavailable"):
                    errors.append(f"native-coverage-not-declared:{kind}")
                    continue
                if status == "complete" and kind in ABSENCE_PROOFS:
                    if not native_domain_evidence(export_document, kind):
                        errors.append(f"native-absence-proof-missing:{kind}")
                        status = "partial"
                    elif export_document.get("dependencyAnalysisComplete") is not True:
                        errors.append(f"native-analysis-incomplete:{kind}")
                        status = "partial"
                result["coverage"][kind] = status
            if errors:
                for kind in KINDS:
                    if result["coverage"][kind] == "complete":
                        result["coverage"][kind] = "partial"
    elif export_valid:
        errors.append("exporter-coverage-not-declared:COPY,CALL")
    return result


COMMAREA_CLOSURE_NAME = ".commarea-closure.json"


def commarea_lexically_inert(source):
    """True when a fixed-format program that the parser could not export provably sends no COMMAREA."""
    try:
        lines = Path(source).read_text(encoding="latin-1").splitlines()
    except OSError:
        return False
    procedure, words = False, []
    for line in lines:
        if len(line) >= 7 and line[6] in "*/":
            continue
        area = line[7:72].upper()
        if re.search(r"(?<![A-Z0-9-])REPLACE(?![A-Z0-9-])", area):
            return False
        if re.search(r"(?<![A-Z0-9-])PROCEDURE\s+DIVISION(?![A-Z0-9-])", area):
            procedure = True
        if procedure:
            words.extend(re.findall(r"[A-Z0-9-]+", area))
    return procedure and "COMMAREA" not in words and "COPY" not in words


def commarea_closure(source_root, output_root, estate):
    """Closed-world value sets of COMMAREA program slots across every program of one estate."""
    source_root, output_root = Path(source_root).resolve(), Path(output_root).resolve()
    estate_root = source_root / estate
    errors, participants, programs, inventories = [], [], {}, {}
    sources = sorted(path for path in estate_root.rglob("*") if path.is_file()
                     and path.suffix.lower() in (".cbl", ".cob")
                     and not any(part.startswith(".") for part in path.relative_to(estate_root).parts))
    for source in sources:
        relative = source.relative_to(source_root).as_posix()
        stem = source.stem.upper()
        inventory, parsed_sha = None, None
        try:
            export = read_json(output_root / (relative + "-deps.json"))
            evidence = read_json(output_root / (relative + ".report") / "dependency-evidence.json")
            if evidence.get("originalSourceSha256") == digest(source) and isinstance(export.get("commareaInventory"), dict):
                inventory, parsed_sha = export["commareaInventory"], export.get("sourceSha256")
        except (OSError, ValueError):
            pass
        if inventory is None:
            if commarea_lexically_inert(source):
                participants.append({"relativePath": relative, "sourceSha256": digest(source), "mode": "lexical"})
                inventories[stem] = {"records": [], "carriers": [], "sends": []}
            else:
                errors.append("commarea-closure-participant-unavailable:" + relative)
            continue
        if inventory.get("schema") != 1 or not isinstance(parsed_sha, str) or stem in inventories:
            errors.append("commarea-closure-participant-invalid:" + relative)
            continue
        participants.append({"relativePath": relative, "sourceSha256": digest(source), "mode": "inventory",
                             "inventorySha256": hashlib.sha256(json.dumps(inventory, sort_keys=True).encode()).hexdigest()})
        inventories[stem] = inventory
        programs[parsed_sha] = {"relativePath": relative, "program": stem}
    bad, sent, values, layouts = object(), [], {}, set()

    def linked_to_inert(send):
        # A LINK whose every proved target loads no COMMAREA record cannot feed any program slot.
        targets = send.get("targets")
        return (send["command"] == "LINK" and send.get("bindingStatus") == "complete"
                and isinstance(targets, list) and targets and all(
                    target in inventories and not any(record["loaded"] for record in inventories[target]["records"])
                    for target in targets))
    for stem, inventory in inventories.items():
        records = {record["name"]: record for record in inventory["records"]}
        carriers = {carrier["name"]: carrier for carrier in inventory["carriers"]}
        for carrier in carriers.values():
            if carrier["issues"] or carrier["record"] not in records:
                errors.append(f"commarea-closure-carrier-unsupported:{stem}:{carrier['name']}")
        for send in inventory["sends"]:
            if send["kind"] == "record":
                if not linked_to_inert(send):
                    sent.append((stem, send["record"]))
            elif send["kind"] == "carrier" and send["carrier"] in carriers:
                sent.append((stem, carriers[send["carrier"]]["record"]))
            elif send["kind"] == "foreign":
                targets = send.get("targets") or []
                if (not targets or any(target not in inventories or any(
                        record["loaded"] for record in inventories[target]["records"]) for target in targets)):
                    errors.append(f"commarea-closure-foreign-send:{stem}:{send['operand']}:{send['line']}")
            else:
                errors.append(f"commarea-closure-send-unsupported:{stem}:{send['operand']}")
        for record in records.values():
            if record["loaded"] or (stem, record["name"]) in sent:
                layouts.add(tuple(record["layout"]))
            for slot, item in record["slots"].items():
                ok = not record["issues"] and not item["unsupported"] and all(
                    value is None or isinstance(value, str) and re.fullmatch(r"[A-Za-z0-9@$#]{1,8}", value)
                    for value in item["values"])
                values[(stem, record["name"], slot)] = set(item["values"]) if ok else bad
    # Programs may extend the shared record after its copybook; only the common leading entries align.
    prefix = min(layouts, key=len) if layouts else ()
    if any(layout[:len(prefix)] != prefix for layout in layouts):
        errors.append("commarea-closure-incompatible-layouts:" + str(len(layouts)))
    shared = {match[1] for entry in prefix for match in [re.match(r"[0-9]+([A-Z0-9-]+)PIC", entry)] if match}
    changed = True
    while changed and not errors:
        changed = False
        universe = {}
        for stem, record in sent:
            for (owner, name, slot), current in values.items():
                if owner == stem and name == record and slot in shared:
                    known = universe.get(slot, set())
                    universe[slot] = bad if current is bad or known is bad else known | current
        for stem, inventory in inventories.items():
            for record in inventory["records"]:
                for slot, item in record["slots"].items():
                    key = (stem, record["name"], slot)
                    if values[key] is bad:
                        continue
                    merged = set(values[key])
                    contributions = [values[(stem, record["name"], copy)] for copy in item["copies"]]
                    if item["entry"]:
                        contributions.append(universe.get(slot, set()) if slot in shared else bad)
                    if any(contribution is bad for contribution in contributions):
                        values[key], changed = bad, True
                        continue
                    for contribution in contributions:
                        merged |= contribution
                    if merged != values[key]:
                        values[key], changed = merged, True
    result = {}
    if not errors:
        for parsed_sha, program in sorted(programs.items(), key=lambda item: item[1]["relativePath"]):
            slots, duplicate = {}, set()
            for record in inventories[program["program"]]["records"]:
                for slot in record["slots"]:
                    current = values[(program["program"], record["name"], slot)]
                    if slot in slots or slot in duplicate:
                        duplicate.add(slot)
                        slots.pop(slot, None)
                    elif current is not bad:
                        slots[slot] = sorted(current, key=lambda value: (value is not None, value or ""))
            result[parsed_sha] = {"relativePath": program["relativePath"], "slots": slots}
    return {"schema": 1, "estate": estate, "errors": sorted(set(errors)), "participants": participants,
            "programs": result}


def commarea_closure_bindings(document, output_root, relative_path):
    """Closure files used by this export's slot proofs, or a diagnostic when any proof disagrees with the estate."""
    estate = Path(relative_path).parts[0]
    closure_path = Path(output_root).resolve() / estate / COMMAREA_CLOSURE_NAME
    used = [binding.get("targetResolution") for statement in document.get("dependencyDialectStatements", [])
            for binding in statement.get("resourceBindings", [])
            if binding.get("kind") == "CICS_PROGRAM" and isinstance(binding.get("targetResolution"), dict)
            and binding["targetResolution"].get("kind") == "closed-world-commarea-slot"]
    if not used:
        return [], None
    try:
        closure, closure_sha = read_json(closure_path), digest(closure_path)
    except (OSError, ValueError):
        return [], "commarea-closure-missing"
    program = closure.get("programs", {}).get(document.get("sourceSha256"))
    if closure.get("errors") or not program or program.get("relativePath") != relative_path:
        return [], "commarea-closure-program-missing"
    for proof in used:
        if (proof.get("closureSha256") != closure_sha or proof.get("programSourceSha256") != document.get("sourceSha256")
                or proof.get("universe") != program["slots"].get(proof.get("operand"))):
            return [], "commarea-closure-proof-mismatch:" + str(proof.get("operand"))
    return [{"kind": "COMMAREA_CLOSURE", "name": estate, "path": str(closure_path), "sha256": closure_sha}], None


def evidence_current(source_root, relative_path, output_root):
    try:
        source = (Path(source_root) / relative_path).resolve()
        evidence = read_json(Path(output_root) / (relative_path + ".report") / "dependency-evidence.json")
        if (evidence["schemaVersion"] != 1 or evidence["producer"] != PRODUCER
                or evidence["relativePath"] != relative_path
                or Path(evidence["originalSourcePath"]).resolve() != source
                or evidence["originalSourceSha256"] != digest(source)
                or evidence["exportExitCode"] != 0
                or Path(evidence["exportPath"]).resolve() != (Path(output_root) / (relative_path + "-deps.json")).resolve()
                or evidence["exportSha256"] != digest(evidence["exportPath"])
                or evidence["placeholderContaminated"] or evidence["unresolvedDiagnostics"]
                or any(evidence["coverage"].get(kind) != "complete" for kind in KINDS)):
            return False
        if contaminated_report(Path(output_root) / (relative_path + ".report")):
            return False
        for definition in evidence["definitions"]:
            if definition["kind"] == "COMMAREA_CLOSURE":
                # Every participant must still match: recompute from current sources and exports.
                current = commarea_closure(source_root, output_root, definition["name"])
                if (definition["sha256"] != digest(definition["path"])
                        or read_json(definition["path"]) != json.loads(json.dumps(current))):
                    return False
                continue
            if definition["sha256"] != digest(definition["path"]) or definition_problem(definition["path"]):
                return False
        # Recheck original references; old "complete" empty COPY exports are invalid.
        document = read_json(evidence["exportPath"])
        if (document.get("dependencyExportSchemaVersion") != 1
                or commarea_closure_bindings(document, output_root, relative_path)[1]
                or document.get("dependencyCoverage") != evidence["coverage"]
                or missing_native_operations(source, document)):
            return False
        nodes = [document] + exported_nodes(document, source.name)
        for definition in evidence["definitions"]:
            if definition["kind"] != "BMS":
                continue
            copies = list({item["path"]: item for item in evidence["definitions"] if item["kind"] == "COPY_INPUT"
                           and item["name"] == definition["name"]}.values()) or list(
                {item["path"]: item for item in evidence["definitions"] if item["kind"] == "COPY"
                 and item["name"] == definition["name"]}.values())
            layouts = list({item["path"]: item for item in evidence["definitions"] if item["kind"] == "BMS_LAYOUT"
                            and item["name"] == definition["name"]}.values())
            if len(copies) != 1 or len(layouts) != 1:
                return False
            failures = []
            bms_layout_definition(Path(definition["path"]), Path(copies[0]["path"]),
                                  definition["name"], list(evidence["definitions"]), failures)
            if failures or not any(binding.get("kind") == "BMS_LAYOUT"
                                   and binding.get("sourceSha256") == definition["sha256"]
                                   and binding.get("outputSha256") == copies[0]["sha256"]
                                   and binding.get("layoutSha256") == layouts[0]["sha256"]
                                   for owner in nodes for binding in owner.get("dependencyDerivedInputs", [])):
                return False
        for node in nodes:
            if node is not document and node.get("dependencyKind") == "COPY":
                continue
            if node.get("dependencyKind") == "PLATFORM_COPY":
                binding = node.get("runtimeContract", {})
                profiles = [item for item in evidence["definitions"] if item["kind"] == "PLATFORM_COPY"
                            and item["name"] == binding.get("name") and item["sha256"] == binding.get("sourceSha256")]
                inputs = [item for item in evidence["definitions"] if item["kind"] == "PLATFORM_INPUT"
                          and item["name"] == binding.get("name") and item["sha256"] == binding.get("outputSha256")]
                if (len(profiles) != 1 or len(inputs) != 1
                        or not native_platform_contract_bound(node, read_json(profiles[0]["path"]))):
                    return False
                if binding.get("name") == "DFHBMSCA":
                    from platform_copybook_contracts import bms_layouts_bound, bms_source_uses_bound
                    if not bms_layouts_bound(binding, evidence["definitions"]):
                        return False
                    parent = next(owner for owner in nodes if any(child is node for child in owner.get("dependencies", [])))
                    original = source
                    if parent is not document:
                        originals = [item["path"] for item in evidence["definitions"] if item["kind"] == "CALL"
                                     and item["name"] == parent["callTarget"]["name"]
                                     and ".preprocessed" not in Path(item["path"]).parts]
                        if len(set(originals)) != 1:
                            return False
                        original = Path(originals[0])
                    if not bms_source_uses_bound(binding, original.read_text(encoding="latin-1"), evidence["definitions"]):
                        return False
                continue
            if node.get("dependencyKind") == "EXTERNAL_RUNTIME":
                binding = node.get("runtimeContract", {})
                definitions = [definition for definition in evidence["definitions"]
                               if definition["kind"] == "RUNTIME_CONTRACT"
                               and definition["name"] == binding.get("name")
                               and definition["sha256"] == binding.get("sourceSha256")]
                if len(definitions) != 1 or not native_runtime_contract_bound(
                        node, read_bound_profile(binding, definitions[0]["path"])):
                    return False
                contract = binding["definition"]
                if contract.get("kind") == "estate-assembler-routine" and not all(
                        any(item["kind"] == "ASSEMBLER_SOURCE" and item["sha256"] == source["sourceSha256"]
                            and Path(item["path"]).as_posix().endswith("/" + source["source"])
                            and digest(item["path"]) == item["sha256"] for item in evidence["definitions"])
                        for source in [contract] + contract["includes"]):
                    return False
                continue
            coverage = node.get("dependencyCoverage", {})
            if (node.get("dependencyAnalysisComplete") is not True or node.get("dependencyDiagnostics")
                    or not isinstance(coverage, dict) or any(coverage.get(kind) != "complete" for kind in KINDS)
                    or any(not native_domain_evidence(node, kind) for kind in ABSENCE_PROOFS)):
                return False
            for statement in node.get("dependencyDialectStatements", []):
                for binding in native_cics_resources(statement):
                    if binding.get("kind") == "CICS_MAP":
                        if not cics_map_evidence_current(binding, evidence["definitions"], source_root):
                            return False
                        continue
                    if binding.get("definitionScope") == "explicit-sql-catalog":
                        declarations = [item for item in evidence["definitions"] if item["kind"] == "SQL_DECLARATION"
                                        and item["name"] == binding.get("name")
                                        and item["sha256"] == binding.get("sourceSha256")]
                        if len(declarations) != 1 or not Path(declarations[0]["path"]).resolve().is_relative_to(
                                Path(source_root).resolve()):
                            return False
                    if "qualification" in binding:
                        qualification = binding["qualification"]
                        definitions = [definition for definition in evidence["definitions"]
                                       if definition["kind"] == "ANALYSIS_PROFILE"
                                       and definition["name"] == qualification.get("profileName")
                                       and definition["sha256"] == qualification.get("sourceSha256")]
                        if (len(definitions) != 1
                                or read_bound_profile(qualification, definitions[0]["path"])
                                    .get("sqlDefaultSchema") != qualification.get("schema")):
                            return False
                    if binding.get("kind") not in ("CICS_FILE", "CICS_PROGRAM", "CICS_TRANSACTION", "CICS_TDQUEUE"):
                        continue
                    if not any(definition["kind"] == binding["kind"] and definition["name"] == binding.get("name")
                               and definition["sha256"] == binding.get("sourceSha256")
                               for definition in evidence["definitions"]):
                        return False
                    if binding["kind"] == "CICS_FILE" and not all(
                            any(definition["kind"] == "CICS_DATASET" and definition["name"] == source.get("name")
                                and definition["sha256"] == source.get("sourceSha256")
                                for definition in evidence["definitions"])
                            for source in binding.get("datasetDefinitions", [])):
                        return False
                    transfer = statement.get("grammarRule") in ("Cics_returnContext", "Cics_xctlContext", "Cics_inquireContext")
                    if binding["kind"] == "CICS_PROGRAM" and (
                            not any(definition["kind"] == ("CICS_CONTINUATION" if transfer else "CALL")
                                    and definition["name"] == binding.get("name")
                                    and definition["sha256"] == binding.get("programSourceSha256")
                                    for definition in evidence["definitions"])
                            or not transfer
                            and not any(child.get("dependencyKind") in ("CALL", "CICS_LINK", "CICS_RUN")
                                       and child.get("callTarget", {}).get("name") == binding.get("name")
                                       and child.get("sourcePath") == binding.get("programSourcePath")
                                       and child.get("sourceSha256") == binding.get("programSourceSha256")
                                       for child in node.get("dependencies", []))):
                        return False
        copies, _ = exported_names(document, source.name)
        if referenced_names(source.read_text(encoding="latin-1")) - copies:
            return False
        defined = {item["name"].upper() for item in evidence["definitions"] if item["kind"] in ("COPY", "PLATFORM_COPY")}
        return copies <= defined
    except (OSError, ValueError, KeyError, TypeError):
        return False


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("write", "validate", "quarantine", "reset-preprocessed", "provenance", "stage", "validate-definition", "dependency-search-view",
                                               "commarea-closure"))
    parser.add_argument("--source-root", type=Path)
    parser.add_argument("--relative-path")
    parser.add_argument("--output-root", type=Path)
    parser.add_argument("--staging-root", type=Path)
    parser.add_argument("--parser-source-root", type=Path)
    parser.add_argument("--dependency-profile", type=Path, default=os.environ.get("REKT_DEPENDENCY_PROFILE") or None)
    parser.add_argument("--sql-declarations", default=os.environ.get("REKT_SQL_DECLARATIONS") or None)
    parser.add_argument("--export-exit-code", type=int, default=1)
    parser.add_argument("--source", type=Path)
    parser.add_argument("--target", type=Path)
    parser.add_argument("--transformed", type=Path)
    parser.add_argument("--derived", action="store_true")
    parser.add_argument("--estate")
    args = parser.parse_args()
    if args.action == "commarea-closure":
        closure = commarea_closure(args.source_root, args.output_root, args.estate)
        path = Path(args.output_root) / args.estate / COMMAREA_CLOSURE_NAME
        path.parent.mkdir(parents=True, exist_ok=True)
        temporary = path.with_name(path.name + ".tmp")
        temporary.write_text(json.dumps(closure, indent=2, sort_keys=True) + "\n", encoding="utf-8")
        temporary.replace(path)
        for error in closure["errors"]:
            print(f"REKT COMMAREA closure: {error}", file=sys.stderr)
        return 0
    if args.action == "dependency-search-view":
        dependency_search_view(args.staging_root, args.relative_path)
    elif args.action == "validate-definition":
        return 1 if definition_problem(args.source) else 0
    elif args.action == "provenance":
        provenance(args.source, args.target)
    elif args.action == "stage":
        stage(args.source, args.target, args.transformed, args.derived)
    elif args.action == "reset-preprocessed":
        root = args.output_root
        root.mkdir(parents=True, exist_ok=True)
        preserve([path for path in root.iterdir() if path.name != ".previous"], root)
    elif args.action == "quarantine":
        preserve([args.output_root / (args.relative_path + suffix)
                  for suffix in (".report", "-deps.json", ".facts.json", ".parse.log")], args.output_root)
    elif args.action == "validate":
        return 0 if evidence_current(args.source_root, args.relative_path, args.output_root) else 1
    else:
        evidence = build_evidence(args.source_root, args.relative_path, args.output_root,
                                  args.export_exit_code, args.staging_root, args.parser_source_root,
                                  args.dependency_profile, args.sql_declarations)
        write_json(args.output_root / (args.relative_path + ".report") / "dependency-evidence.json", evidence)
        if any(item.startswith(("dependency-export-failed:", "dependency-export-invalid:"))
               for item in evidence["unresolvedDiagnostics"]):
            return 1
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError) as error:
        print(f"REKT evidence: {error}", file=sys.stderr)
        sys.exit(1)
