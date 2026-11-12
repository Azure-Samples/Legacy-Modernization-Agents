# Ordered chunk output assembly

**Last updated**: 2026-10-22

Chunk-aware Java and C# converters use ordered continuation fragments, not independently complete compilation units. The first fragment declares the package/namespace, imports/usings and primary type, leaving that type open. Middle fragments add members or continue a method. The last fragment closes the outstanding scopes. A single chunk returns a complete unit.

Assembly sorts arrivals by planned index and preserves every fragment byte, inserting only a newline between fragments. It does not rename types, synthesize wrappers, inject imports, extract bodies, or merge duplicate definitions. Legitimate nested and additional types remain in the original unit; Java permits at most one public top-level type.

Acceptance requires exactly one successful, nonempty response for every planned index, consistent conversion results, a complete supported type, balanced structural delimiters and no repeated type/member signatures. The primary type must remain open across fragment boundaries. Complete-unit chunks, conflicting headers, repeated members (including exact identical overlaps), missing/failed chunks and unknown structural forms are explicitly rejected, never silently deduplicated. Source overlap supplied for conversion context must not be re-emitted.

This bounded lexical guard is not a language parser or a compile gate. It supports block-bodied type declarations, generic methods and overloads, comments, escaped/character/verbatim strings and triple-quoted strings. Preprocessor directives, interpolated C# strings, longer raw-string delimiters, bodyless positional records, braced expressions in parameter/attribute lists and multiple field declarators are rejected rather than guessed. Other compiler/semantic errors remain the responsibility of the language compile gate.

Converters accept plain source or one whole-response fenced code block for compatibility. Multiple blocks or prose surrounding a fenced block fail with the complete raw response retained; selecting only the first block would lose output. The single-chunk prompt explicitly requests a complete unit, rather than accidentally applying the first-fragment instruction to leave its class open.

Rejected files produce no compilable assembly output. All received raw chunks, including nonsentinel failed responses, are retained unchanged as `.rejected.txt` files under `assembly-diagnostics/source-N/`. `assembly-errors.txt` records authoritative source identity, expected count, received indices and rejection reasons. Duplicate arrivals have distinct diagnostic names. Diagnostics are excluded from generated-file lists and parity/compilation inputs.

The file success flag is set only after assembly acceptance. The migration report reflects assembly rejection, and a rejected file causes the migration to persist `Failed` and throw to its caller after diagnostics/report generation. A successful build of other files cannot turn missing or conflicting source coverage into a completed migration.
