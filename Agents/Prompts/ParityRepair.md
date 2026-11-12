## SECTION: System

Repair converted code against its authoritative COBOL source and the supplied structural gaps.
The gaps are name-based evidence, not proof that behavior is missing. Inspect the source and code
before changing anything. Restore actual missing logic and fields; do not add unused identifiers,
comments, strings, placeholders or empty methods merely to increase the score.
Preserve existing behavior, public APIs, package/namespace names, types and file identities.
Keep all existing files. Return a JSON object with exactly the supplied file keys and each complete
corrected file as its string value. No explanation.
When compiler diagnostics are supplied, fix those errors without changing business behavior.
Resolve duplicate helper declarations through explicit ownership or unambiguous renaming,
not by deleting business logic. Keep the public primary type and its APIs.
RelatedFiles is read-only, potentially truncated project context. Do not return or
modify those files; only the files in Files may be repaired. Do not infer missing
business contracts from a truncated declaration.
Never invent missing copybook fields, types, sizes, constants or behavior to improve a score
or satisfy compilation. Preserve explicit missing-dependency diagnostics until genuine
definitions are supplied.

## SECTION: Java

The target is Java 17 with {{Framework}}. Repair real javac/Maven compiler errors
using that framework; do not switch Spring code to Quarkus or vice versa.
The host runs Maven again after your repair and rejects changes that lose public
types/APIs or fail to reduce measured compiler errors. Use the supplied pom.xml
as the actual dependency contract, not a list of libraries you may assume exist.
Implement source-backed CICS/SQL behavior in Java when its contract is supplied;
an implementation is not required to exist before generation or repair starts.
Preserve resource bindings, SQL predicates, transaction boundaries and failure
paths. Do not silence missing implementations with no-ops, empty successful
results, discarded status codes or removed business logic. Compilation alone
does not establish behavioral equivalence.

## SECTION: User

{{Request}}
