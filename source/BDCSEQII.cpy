000100******************************************************************
000200*********            *                                           *
000300*        ****        *   MODULNAVN      BDCSEQII                 *
000400*            ****    *                                           *
000500* N O T E        *****                                           *
000600*            ****    *                                           *
000700*        ****        *   OPRETTET       2014-05-13               *
000800*********            *   SIDST ÆNDRET   2014-05-13               *
000900******************************************************************
001000
001100  01  BDC-SEQ-:XXXX:.
001200     03  BDC-:XXXX:-DDNAME.
001300      04 BDC-GSAM-DDNAME         PIC X(04) VALUE 'GSAM'.
001400      04 BDC-FILE-DDNAME         PIC X(04) VALUE :'XXXX':.
001500     03  BDC-:XXXX:-REQUEST-TYPE PIC X(06) VALUE 'OPEN  '.
001600     03  BDC-:XXXX:-FILE-TYPE    PIC X(01) VALUE 'I'.
001700     03  BDC-:XXXX:-RETURN-CODE  PIC X(03) VALUE LOW-VALUES.
001800       88  BDC-:XXXX:-OK                   VALUE '   '.
001900       88  BDC-:XXXX:-EOF                  VALUE 'EOF'.
002000     03  FILLER                  PIC X(18).
002100     03  BDC-:XXXX:-BLEN         PIC S9(4) COMP-5.
002200     03  BDC-:XXXX:-CURR-REC-LEN PIC S9(4) COMP-5.
002300     03  BDC-:XXXX:-MAX-REC-LEN  PIC S9(4) COMP-5.
002400     03  BDC-:XXXX:-BLOCKSIZE    PIC S9(4) COMP-5.
002500     03  BDC-:XXXX:-RECFORMAT    PIC X(3).
002600     03  BDC-:XXXX:-ERROR        PIC X.
002700     03  FILLER                  PIC X(32).
002800
002900
