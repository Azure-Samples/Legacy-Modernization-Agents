# Bank-of-Z source corrections

Bank-of-Z is a demonstration estate. Its generated copybooks under
`source/bank-of-z/copy-generated/` are placeholders, not IBM SDK copybooks.
They are not used as evidence for the corrections below.

## PF12 termination: `EIBAID = DFHAID OR DFHPF12`

Programs: BNK1CAC, BNK1CCA, BNK1CRA, BNK1DCS, BNK1TFN and BNK1UAC. The line is
identical in upstream `IBM/Bank-of-Z` (`src/base/cics/cobol`), so it is not a
local transcription error.

**Evidence**

- `EIBAID` is a one-byte field (`PIC X`).
- In the IBM CICS `DFHAID` copybook, `DFHAID` is the `01` group containing
  the AID constants. It starts with `DFHNULL` (space), followed by `DFHENTER`
  (quote), `DFHCLEAR` (underscore), the PA keys and the PF keys. It is not an
  individual key.
- The abbreviated condition expands to
  `EIBAID = DFHAID OR EIBAID = DFHPF12`.
- COBOL compares a one-byte item with a group alphanumerically. The shorter
  operand is padded with spaces. Byte 2 of the padded `EIBAID` is a space.
  Byte 2 of the group is the `DFHENTER` quote. So `EIBAID = DFHAID` is false
  for every `EIBAID` value.
- In every occurrence, the branch calls `SEND-TERMINATION-MSG` and ends with
  `EXEC CICS RETURN`. The other AID keys have their own `WHEN` branches.

**Correction**

The condition is now `EIBAID = DFHPF12`. Runtime behaviour on CICS does not
change: PF12 was already the only key that could reach this branch. The
comment "If the aid or Pf12 is pressed" was updated to match.

The parser still reports `UNRESOLVED_CICS_AID_SEMANTIC` for any remaining
standalone `DFHAID` comparison. Regression coverage is in
`tools/tests/test_bank_of_z_source_corrections.py`.

## Mixed ENTER conditions in BNK1DCS

`IF EIBAID = DFHENTER AND COMM-UPD NOT = 'Y'` and
`IF EIBAID = DFHENTER AND COMM-UPD = 'Y'` are valid source and were not
changed. Native patch `0030` binds the `EIBAID = DFHENTER` conjunct while
leaving the application relation in the original source.

## CICS PROGRAM `DYNAMIC` default (parser, not source)

`source/bank-of-z/cics/bank-of-z-definitions.yaml` omits `dynamic` for `UPDCUST`, `GETCOMPY` and `GETSCODE`. IBM CICS defines the PROGRAM resource attribute `DYNAMIC({NO|YES})` with default `NO`, so the YAML is intentionally left unchanged and the REKT exporter (patch `0031`) applies that default, recording `dynamicProvenance: cics-default` on the binding. Explicit `YES`, invalid values and missing PROGRAM definitions remain blocking.
