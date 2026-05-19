# COBOL PIC → target type mapping

Use this table for **every** field unless the structural context or an
EXEC SQL DECLARE says otherwise. Add a `notes[]` entry to your output if
you deviate.

| COBOL PIC                          | Java                           | C#                       | Notes |
|------------------------------------|--------------------------------|--------------------------|-------|
| `PIC X(n)`                         | `String` (length=n)            | `string` (n)             | Trim trailing spaces on read. |
| `PIC A(n)`                         | `String`                       | `string`                 | Alphabetic — same as X. |
| `PIC 9(n)`             (n ≤ 9)     | `int`                          | `int`                    | Unsigned semantics — validate ≥ 0 on write. |
| `PIC 9(n)`             (9 < n ≤ 18)| `long`                         | `long`                   | |
| `PIC 9(n)`             (n > 18)    | `BigInteger`                   | `System.Numerics.BigInteger` | Rare — usually a copy error. |
| `PIC S9(n)`            (n ≤ 9)     | `int`                          | `int`                    | Signed. |
| `PIC S9(n)`            (9 < n ≤ 18)| `long`                         | `long`                   | |
| `PIC 9(p)V9(s)`                    | `BigDecimal` (precision=p+s, scale=s) | `decimal`         | Decimal places explicit. |
| `PIC S9(p)V9(s)`                   | `BigDecimal` (precision=p+s, scale=s) | `decimal`         | Signed decimal. |
| `PIC S9(p)V9(s) COMP-3`            | `BigDecimal` (p+s, s)          | `decimal`                | Packed decimal — DO NOT use `double`. |
| `PIC S9(p) COMP-3`                 | `BigDecimal` (p, 0)            | `decimal`                | Packed decimal integer. |
| `PIC S9(p) COMP` / `COMP-4`        | `int` (p≤9) / `long` (p>9)     | `int` / `long`           | Binary. |
| `PIC S9(p) COMP-1`                 | `float`                        | `float`                  | Single-precision floating. |
| `PIC S9(p) COMP-2`                 | `double`                       | `double`                 | Double-precision floating. |
| `PIC S9(p) COMP-5`                 | `int` / `long`                 | `int` / `long`           | Native binary — same as COMP-4. |
| 88-level condition name            | `boolean` / `bool` getter      | property                 | Map as `is<Name>()` returning the comparison. |
| Group item (no PIC)                | nested DTO                     | nested record            | Preserve hierarchy. |
| `REDEFINES`                        | single field + discriminator   | single field + discriminator | Document choice in `notes[]`. |
| `OCCURS n`                         | `List<T>` (cap n) or child table | `List<T>` / table     | Pick one and explain in `notes[]`. |
| `OCCURS n DEPENDING ON x`          | `List<T>`                      | `List<T>`                | Variable length. |
| Date `PIC 9(8)`                    | `java.time.LocalDate`          | `DateOnly`               | Parse `yyyyMMdd`. |
| Time `PIC 9(6)`                    | `java.time.LocalTime`          | `TimeOnly`               | Parse `HHmmss`. |
| Timestamp `PIC X(26)`              | `java.time.LocalDateTime`      | `DateTime`               | DB2 timestamp format. |

## Annotations

- **Java JPA**: `@Column(name = "EXACT-COBOL-NAME", length = n, precision = p, scale = s, nullable = ...)`.
- **C# EF Core**: `[Column("EXACT-COBOL-NAME", TypeName = "decimal(p,s)")]`; nullability via `?`.
- **Tables**: `@Table(name = "EXACT-COBOL-TABLE")` / `[Table("EXACT-COBOL-TABLE")]`.

## Hard rules

- Never silently downcast `BigDecimal` / `decimal` to `double`/`float` — precision loss is a defect.
- Never collapse `S9V9` to `int` — money fields require decimal precision.
- Always preserve the original COBOL field length / scale in the annotation; downstream consumers may rely on it.
- For `REDEFINES`, prefer one class with a `kind` discriminator over two parallel classes.
