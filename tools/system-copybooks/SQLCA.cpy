      *>───────────────────────────────────────────────────────────────
      *> SQLCA — SQL Communication Area
      *> Standard IBM Db2 layout (public, documented in the Db2 Application
      *> Programming and SQL Guide). Ships with this repository because
      *> Eclipse-LSP-COBOL does NOT auto-inject SQLCA in CLI mode, which
      *> causes smojol to abort 30+ programs as MISSING_COPYBOOK errors.
      *> Bundled so the parser can resolve `EXEC SQL INCLUDE SQLCA` and
      *> any `COPY SQLCA` reference to a real definition.
      *> Source: https://www.ibm.com/docs/en/db2-for-zos/12?topic=programs-sqlca
      *>───────────────────────────────────────────────────────────────
       01  SQLCA.
           05  SQLCAID         PIC  X(8).
           05  SQLCABC         PIC  S9(9) COMP-5.
           05  SQLCODE         PIC  S9(9) COMP-5.
           05  SQLERRM.
               49  SQLERRML    PIC  S9(4) COMP-5.
               49  SQLERRMC    PIC  X(70).
           05  SQLERRP         PIC  X(8).
           05  SQLERRD         OCCURS 6 TIMES
                               PIC  S9(9) COMP-5.
           05  SQLWARN.
               10  SQLWARN0    PIC  X(1).
               10  SQLWARN1    PIC  X(1).
               10  SQLWARN2    PIC  X(1).
               10  SQLWARN3    PIC  X(1).
               10  SQLWARN4    PIC  X(1).
               10  SQLWARN5    PIC  X(1).
               10  SQLWARN6    PIC  X(1).
               10  SQLWARN7    PIC  X(1).
               10  SQLWARN8    PIC  X(1).
               10  SQLWARN9    PIC  X(1).
               10  SQLWARNA    PIC  X(1).
           05  SQLSTATE        PIC  X(5).
