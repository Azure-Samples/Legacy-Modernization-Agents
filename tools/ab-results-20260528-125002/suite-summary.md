# Suite results

- target: `java`
- programs run: 1 (success: 1, fail: 0)
- mode: projection-only
- output dir: `/Users/gustav/funpark/cobol/v.latest/Legacy-Modernization-Agents/tools/ab-results-20260528-125002`

## Automated metrics

| program | baseline ms | projection ms | baseline in | projection in | baseline total | projection total | input Δ | total Δ | baseline cache | projection cache | status |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---|---|---|
| BDSM043 | 0 | 334000 | 0 | 0 | 0 | 0 | n/a | n/a | - | - | ok |

## Per-program raw logs

- `BDSM043` : [`baseline.log`](./BDSM043/baseline.log) · [`projection.log`](./BDSM043/projection.log)

## Next steps

1. Inspect each pair of logs for compile diagnostics (`grep -E 'error|warning' projection.log`).
2. Diff the generated source files between the two legs.
3. Score each program against the rubric in `docs/p1-ab-validation-protocol.md`.
4. Paste the table above into the protocol doc under §Automated metrics.
