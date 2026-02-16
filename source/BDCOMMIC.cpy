000100*SPC>*************************************************************
000200*BSK>*************************************************************
000300*********            *                                           *
000400*        ****        *   MODULNAVN      BDCOMMIT                 *
000500*            ****    *                                           *
000600* N O T E        *****                                           *
000700*            ****    *                                           *
000800*        ****        *   GÆLDENDE FRA   20.03.17                 *
000900*********            *   SIDST ÆNDRET   01.10.95                 *
001000******************************************************************
001100
001200 01  BDCOMMIT-AREA.
001300
001400   02  BDC-ROUTINE-DATA.
001500
001600     03  BDC-RECORD-KEY.
001700       04  BDC-PGM-NAME        PIC X(08).
001800       04  BDC-JOB-NAME        PIC X(08).
001900       04  FILLER              PIC X(04).
002000
002100     03  BDC-RECORD-DATA.
002200       04  BDC-FREQUENCY       PIC S9(8) VALUE ZERO COMP.
002300       04  BDC-INTERNAL        PIC X(56).
002400
002500     03  BDC-REQUEST-TYPE      PIC X(01) VALUE 'S'.
002600       88 BDC-REQUEST-START              VALUE 'S'.
002700       88 BDC-REQUEST-TERMINATE          VALUE 'E'.
002800       88 BDC-REQUEST-FORCE              VALUE 'F'.
002900       88 BDC-REQUEST-RESET              VALUE 'R'.
003000       88 BDC-REQUEST-WOULD              VALUE 'W'.
003100     03  BDC-RETURN-CODE       PIC X(01).
003200       88 BDC-PROGRAM-IS-RESTARTING      VALUE 'R'.
003300       88 BDC-RETURN-OK                  VALUE ' '.
003400     03  BDC-TAKEN-FLAG        PIC X(01).
003500       88 BDC-COMMIT-NOT-TAKEN           VALUE 'N'.
003600       88 BDC-COMMIT-WAS-TAKEN           VALUE 'Y'.
003700       88 BDC-COMMIT-WOULD-BE-TAKEN      VALUE 'W'.
003800     03  BDC-COMMIT-TYPE       PIC X(01) VALUE 'S'.
003900       88 BDC-COMMIT-BASIC               VALUE 'B'.
004000       88 BDC-COMMIT-SYMBOLIC            VALUE 'S'.
004100     03  FILLER                PIC X(17).
004200     03  BDC-PARALLEL          PIC X(1)  VALUE 'N'.
004300     03  FILLER                PIC X(24).
004400
004500     03  BDC-ROLLBACK          PIC X(01).
004600     03  FILLER                PIC X(01).
004700     03  FILLER                PIC X(08).
004800     03  BDC-FRA-KEY           PIC X(200).
004900     03  BDC-TIL-KEY           PIC X(200).
005000
005100   02  BDCOMMIT-AREA-END       PIC X(20) VALUE
005200                               '** CKPT AREA END **'.
