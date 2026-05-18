#!/usr/bin/env python3
"""
resolve-programs.py — Resolve a CLI selector to a concrete list of program file
names from output/rekt/ + target-architecture.json + source/. Mirrors the
ProgramSelectorService logic in McpChatWeb so the CLI can drive focused
conversions without a running portal.

Usage:
    resolve-programs.py [--program NAME ...]
                        [--transaction TRANID ...]
                        [--wave N ...]
                        [--target COMPONENT ...]
                        [--keyword TEXT ...]
                        [--include-callees]
                        [--include-callers]
                        [--source-folder source]
                        [--repo-root .]

Combine logic:
  - Same flag repeated  → OR within that flag
  - Different flags     → AND between flags
  - --include-callees/-callers expand the result through the CALL graph

Output: one file name per line on stdout. Summary + reasons on stderr.
Exit code 0 if at least one match, 2 if no selectors, 1 on error.
"""
from __future__ import annotations
import argparse, json, os, re, sys
from pathlib import Path


def enumerate_program_files(source_dir: Path) -> list[str]:
    if not source_dir.is_dir():
        return []
    out: list[str] = []
    for p in sorted(source_dir.iterdir(), key=lambda x: x.name.lower()):
        if p.is_file() and p.suffix.lower() in (".cbl", ".cpy"):
            out.append(p.name)
    return out


def load_target_plans(rekt_dir: Path) -> dict[str, dict]:
    path = rekt_dir / "target-architecture.json"
    if not path.is_file():
        return {}
    try:
        doc = json.loads(path.read_text())
    except Exception:
        return {}
    out: dict[str, dict] = {}
    for entry in doc.get("programMappings", []) or []:
        prog = entry.get("program") or ""
        if not prog:
            continue
        rec = entry.get("recommendation") or {}
        out[prog.lower()] = rec
        stem = Path(prog).stem.lower()
        out[stem] = rec
    return out


def load_call_targets(rekt_dir: Path, program: str) -> list[str]:
    """Read REKT deps + flow-ast for CALL targets of a single program."""
    stem = Path(program).stem
    targets: list[str] = []
    deps = rekt_dir / f"{stem}-deps.json"
    if deps.is_file():
        try:
            d = json.loads(deps.read_text())
            for dep in d.get("dependencies", []) or []:
                name = dep.get("name") or ""
                if name and not name.lower().endswith(".cpy"):
                    targets.append(name)
        except Exception:
            pass
    # flow-ast may surface additional dynamic call targets — best-effort walk
    ast = rekt_dir / f"flow-ast-{stem}.json"
    if ast.is_file():
        try:
            text = ast.read_text()
            for m in re.finditer(r'"nodeType"\s*:\s*"(?:CALL|CallStatement)"[^{}]*?"name"\s*:\s*"([^"]+)"', text):
                t = m.group(1).strip("'\"")
                if t and t not in targets:
                    targets.append(t)
        except Exception:
            pass
    return targets


def match_file(all_files: list[str], target: str) -> str | None:
    clean = target.strip().strip("'\"")
    stem = Path(clean).stem.lower()
    for f in all_files:
        if f.lower() == clean.lower() or Path(f).stem.lower() == stem:
            return f
    return None


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--program",     action="append", default=[])
    ap.add_argument("--transaction", action="append", default=[])
    ap.add_argument("--wave",        action="append", default=[], type=int)
    ap.add_argument("--target",      action="append", default=[])
    ap.add_argument("--keyword",     action="append", default=[])
    ap.add_argument("--include-callees", action="store_true")
    ap.add_argument("--include-callers", action="store_true")
    ap.add_argument("--source-folder", default="source")
    ap.add_argument("--repo-root", default=".")
    args = ap.parse_args()

    repo_root = Path(args.repo_root).resolve()
    source_dir = repo_root / args.source_folder
    rekt_dir   = repo_root / "output" / "rekt"
    all_files  = enumerate_program_files(source_dir)
    if not all_files:
        print(f"⚠️  No COBOL files in {source_dir}", file=sys.stderr)
        return 1

    hits: list[set[str]] = []
    reasons: dict[str, list[str]] = {}

    def note(f: str, why: str) -> None:
        reasons.setdefault(f, []).append(why)

    # --program
    if args.program:
        bucket: set[str] = set()
        for p in args.program:
            stem = Path(p).stem.lower()
            for f in all_files:
                if f.lower() == p.lower() or Path(f).stem.lower() == stem:
                    bucket.add(f); note(f, f"--program {p}")
        hits.append(bucket)

    # --transaction (CICS TRANSID / LINK PROGRAM)
    if args.transaction:
        bucket = set()
        for tran in args.transaction:
            rx_tran = re.compile(
                rf"\bEXEC\s+CICS\b[^.]*?\bTRANSID\s*\(\s*['\"]?{re.escape(tran)}['\"]?\s*\)",
                re.IGNORECASE | re.DOTALL)
            rx_link = re.compile(
                rf"\bEXEC\s+CICS\s+LINK\b[^.]*?\bPROGRAM\s*\(\s*['\"]?{re.escape(tran)}['\"]?\s*\)",
                re.IGNORECASE | re.DOTALL)
            for f in all_files:
                try:
                    src = (source_dir / f).read_text(encoding="latin-1")
                except Exception:
                    continue
                if rx_tran.search(src) or rx_link.search(src):
                    bucket.add(f); note(f, f"transaction '{tran}'")
        hits.append(bucket)

    # --wave / --target
    plans = load_target_plans(rekt_dir)
    if args.wave:
        bucket = set()
        for w in args.wave:
            for prog, rec in plans.items():
                if rec.get("wave") != w:
                    continue
                m = match_file(all_files, prog)
                if m:
                    bucket.add(m); note(m, f"wave {w}")
        hits.append(bucket)

    if args.target:
        bucket = set()
        for tgt in args.target:
            for prog, rec in plans.items():
                if (rec.get("targetComponent") or "").lower() != tgt.lower():
                    continue
                m = match_file(all_files, prog)
                if m:
                    bucket.add(m); note(m, f"target {tgt}")
        hits.append(bucket)

    # --keyword
    if args.keyword:
        bucket = set()
        for k in args.keyword:
            rx = re.compile(rf"\b{re.escape(k)}\b", re.IGNORECASE)
            for f in all_files:
                try:
                    src = (source_dir / f).read_text(encoding="latin-1")
                except Exception:
                    continue
                if rx.search(src):
                    bucket.add(f); note(f, f"keyword '{k}'")
        hits.append(bucket)

    if not hits:
        print("⚠️  No selectors supplied. Pass --program/--wave/--target/--keyword/--transaction.", file=sys.stderr)
        return 2

    # AND intersect
    resolved: set[str] = hits[0].copy()
    for h in hits[1:]:
        resolved &= h

    # Closure expansion
    if args.include_callees and resolved:
        queue, seen, added = list(resolved), set(resolved), set()
        while queue:
            f = queue.pop(0)
            for t in load_call_targets(rekt_dir, f):
                m = match_file(all_files, t)
                if m and m not in seen:
                    seen.add(m); added.add(m); queue.append(m)
                    note(m, f"transitive callee of {f}")
        resolved |= added

    if args.include_callers and resolved:
        # Inverse — scan every file's CallTargets and add any whose targets include a seed.
        queue, seen, added = list(resolved), set(resolved), set()
        while queue:
            target = queue.pop(0)
            target_stem = Path(target).stem.lower()
            for f in all_files:
                if f in seen:
                    continue
                ts = load_call_targets(rekt_dir, f)
                ts_norm = {Path(t).stem.lower() for t in ts} | {t.lower() for t in ts}
                if target.lower() in ts_norm or target_stem in ts_norm:
                    seen.add(f); added.add(f); queue.append(f)
                    note(f, f"transitive caller of {target}")
        resolved |= added

    # Output
    for f in sorted(resolved, key=str.lower):
        print(f)
    print(f"\n✓ {len(resolved)} program(s) selected", file=sys.stderr)
    for f in sorted(resolved, key=str.lower):
        print(f"  {f}  ({'; '.join(reasons.get(f, ['matched']))})", file=sys.stderr)
    return 0 if resolved else 1


if __name__ == "__main__":
    sys.exit(main())
