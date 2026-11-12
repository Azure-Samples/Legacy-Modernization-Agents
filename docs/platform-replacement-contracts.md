# Explicit platform replacement contracts

**Last updated**: 2026-10-27

An application can replace a platform service without redistributing its SDK
copybooks. That choice must be explicit, preserve the application's uses, and
remain distinct from recovering the original platform implementation.

## Terminal input events

`tools/cobol-rekt/contracts/cics-terminal-events-v1.json` declares a bounded
DFHAID modernization recipe: EIBAID comparisons against DFHENTER, DFHCLEAR,
DFHPA1–3 and DFHPF1–24 become comparisons against named target input events.
It supplies no IBM byte values, binary layout or banking-action mapping.
The IBM CICS documentation index is a general platform reference, not evidence
that a particular application's behavior or target adapter has been verified.

With native patch 0027 installed, opt in using an absolute path accessible at
the same location on the host and inside the parser container:

```bash
export REKT_PLATFORM_CONTRACTS="$PWD/tools/cobol-rekt/contracts/cics-terminal-events-v1.json"
./doctor.sh rekt-full
```

The setting can alternatively be placed in `Config/ai-config.local.env` using
its absolute path. Existing licensed, supplied DFHAID definitions take
precedence. No contract is selected by default.

Preprocessing creates a marked, analysis-only declaration projection without
VALUE clauses. Its profile, bytes, staging provenance and native syntax are
bound by hashes. The export records `PLATFORM_COPY`, not a resolved vendor
`COPY` or callable COBOL program. The strict gate independently checks the
profile, projection, predicates and original source uses. Business COPY
definitions and all existing coverage checks remain mandatory.

Supported predicates include equality, negation, and abbreviated alternatives
such as `EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3`. Every supported native predicate
is retained as a target implementation requirement. Aggregate `DFHAID` uses,
byte/layout access, writes, arithmetic, symbol-to-symbol comparisons and
unsupported mixed conditions remain explicit blockers. In particular,
`EIBAID = DFHAID` is **not** interpreted as ENTER, null or another key.

The conversion context excludes the projection's group from ordinary data
storage and requires named-event implementations that preserve the original
branches. A key must not be assigned directly to an invented business action.
Compile the generated project and test its event routing separately; accepted
dependency evidence is not runtime or behavioral-equivalence verification.

## BMS output attributes

With native patch 0029, `tools/cobol-rekt/contracts/cics-terminal-and-bms-v1.json`
adds an independently checked DFHBMSCA recipe. Select that profile explicitly
instead of the event-only profile to enable both recipes. Supplied genuine
copybooks still take precedence over projections; historical generated files
without accepted provenance do not.

The bounded attribute recipe covers DFHBMASK (autoskip), DFHBMPRF (protected
and MDT set), DFHBMFSE (MDT set), DFHNEUTR (neutral colour), DFHGREEN (green)
and DFHUNDLN (underscore). It supplies symbolic meanings, not IBM byte values.
Every supported scalar MOVE must bind a unique declared output subfield to
its actual map, mapset, named field and hash-bound BMS layout. Attribute,
colour and highlighting symbols must target the appropriate A/C/H subfield.
Ambiguous fields, incompatible suffixes, arithmetic, writes to constants and
unsupported assignments remain blockers.

Character literal `'A'` assignments to A subfields and HIGH-VALUE(S)
assignments to C/H subfields are retained as distinct original source
assignments, not converted to guessed bytes or interpreted as named flags.
The target adapter must implement their original CICS/character-encoding
semantics and behavior-test the edit/protect transitions. Accepted dependency
bindings do not certify a working screen or equivalent target behavior.

## Unresolved application AID semantics

`UNRESOLVED_CICS_AID_SEMANTIC` identifies a condition involving standalone
DFHAID rather than a supported named key. It is not a profile hash failure.
Preserve the condition and keep the conversion gate closed unless application
or genuine SDK evidence establishes its semantics. In particular, do not
silently remove the term from `EIBAID = DFHAID OR DFHPF12`, or map it to ENTER,
CLEAR or PF12. For Bank-of-Z, the IBM `DFHAID` group layout shows that the
term can never be true. The documented correction is recorded in
[bank-of-z-source-corrections.md](bank-of-z-source-corrections.md).

A named-key equality joined with `AND` to complete application relations,
such as `EIBAID = DFHENTER AND COMM-UPD = 'Y'`, is supported. The event
equality is recorded as an `application-and-conjunct` usage. The application
relation stays in the original source and is not interpreted. OR mixing,
abbreviated objects, negated event operands and bare application operands
remain blockers.

Neither recipe replaces missing business source or approves general CICS/Db2
conversion.
