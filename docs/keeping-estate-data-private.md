# Keeping estate data private

**Last updated**: 2026-10-06

The repository is public. The code you convert, and everything derived from it, is not.
Three mechanisms keep the two apart.

## 1. Git ignores estate material wherever it lands

| Ignored | Why |
|---|---|
| `source/*` (except the three `CUSTOMER-*` samples) | The estate itself |
| `*.cbl *.cpy *.cob *.jcl *.prc *.bms *.csd *.ims *.dbd *.psb *.mfs`, in either case, anywhere | A copy of the estate outside `source/` (a slice, a scratch folder) |
| `cobol-source/`, `slices/`, `**/.preprocessed/`, `**/.rekt-staging/` | Common staging folders |
| `output/*`, `java-output/`, `reverse-engineering-output/` | Converted code, reports, REKT facts and graphs, all derived from the estate |
| `Logs/*`, `*.log` | Prompts and model replies quote the source |
| `Data/`, `*.db` | Run history and stored facts |
| `Config/` (except the tracked templates) | Endpoints, keys and the local files below |

## 2. A pre-commit guard

```bash
tools/check-no-estate-data.sh --install-hook   # once per clone
tools/check-no-estate-data.sh --all            # audit every tracked file
```

The hook refuses a commit that adds a mainframe source file, or text containing an estate
name. Names come from two places that are never committed:

- file names under `source/` that contain a digit (program, copybook and job names);
- `Config/sensitive-terms.local.txt`, one term per line, for anything else, such as company
  or application names.

When it reports a hit, mask the name. In fixed-format COBOL inside tests, keep the length
so the columns stay valid.

## 3. Estate-specific preprocessing rules stay local

`tools/preprocess-for-rekt.sh` contains only rewrites that apply to any estate. A rewrite
that only makes sense for one estate's programs goes in
`Config/rekt-preprocess.local.json`, or the file named by `REKT_PREPROCESS_RULES`:

```json
{
  "programRewrites": [
    {
      "description": "Expand two 88-level names the parser rejects in PERFORM UNTIL",
      "stage": "early",
      "literal": true,
      "find": "UNTIL FILE-A-EOF AND FILE-B-EOF",
      "replace": "UNTIL FILE-A-STATUS = 'EOF' AND FILE-B-STATUS = 'EOF'"
    },
    {
      "stage": "late",
      "find": "'(POS-\\d+ )':'",
      "replace": "'\\1:'",
      "flags": ["MULTILINE"]
    }
  ]
}
```

- `stage`: `early` runs before column-72 enforcement, `late` near the end of the program pass.
- `literal: true` is a plain text replacement; otherwise `find` is a Python regular expression
  and `flags` names `re` flags.
- The file is validated once per run. A malformed file is reported and ignored, so
  preprocessing still runs.
