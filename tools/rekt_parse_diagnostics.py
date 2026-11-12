#!/usr/bin/env python3
"""Read persisted parser diagnostics without treating missing definitions as resolved."""
import json
import sys
from pathlib import Path


def missing_copybook_warnings(diagnostics):
    return sorted({
        f"missing-copybook:{diagnostic['message']}".replace("|", "/")
        for diagnostic in diagnostics
        if diagnostic.get("code") == "missing copybook"
    })


def has_structure(report, directories=("flow_ast", "cfg")):
    for directory in directories:
        artifacts = list((Path(report) / directory).glob("*.json"))
        if not artifacts:
            return False
        for artifact in artifacts:
            with artifact.open(encoding="utf-8") as handle:
                value = json.load(handle)
            if not isinstance(value, (dict, list)) or not value:
                return False
    return True


if __name__ == "__main__":
    if sys.argv[1] == "--structure":
        sys.exit(0 if has_structure(sys.argv[2]) else 1)
    else:
        with open(sys.argv[1], encoding="utf-8") as handle:
            print("|".join(missing_copybook_warnings(json.load(handle))))
