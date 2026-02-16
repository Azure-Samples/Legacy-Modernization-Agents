      *#PREVIEW# PROGRAM REVIEW UNDTAGET
       ID DIVISION.
       PROGRAM-ID.         BDSMFJL.
       AUTHOR.             CALLE HYLDAHL, CAP GEMINI A/S.
       DATE-WRITTEN.       25.09.1996.
      *SPC>*************************************************************
      *SPC>          EVENTUEL SPECIALPROGRAMMERING OMFATTER:           *
      *SPC>                                                            *
      *SPC> BANKNR.: XXX, XXX, XXX, XXX, XXX, XXX, XXX, XXX, XXX, XXX  *
      *SPC>                                                            *
      *SPC> REGNR.: XXXX, XXXX, XXXX, XXXX, XXXX, XXXX, XXXX, XXXX     *
      *SPC>                                                            *
      *SPC> OVENSTÅENDE FORMAT MÅ IKKE ÆNDRES. BANKNR ANGIVES ALTID,   *
      *SPC> OG MED FORANSTILLEDE NUL. DER KAN REPEAT'ES EFTER BEHOV.   *
      *SPC>*************************************************************
      *BSK>*************************************************************
      *BSK>            GENEREL BESKRIVELSE AF PROGRAMMET               *
      *BSK>                                                            *
      *BSK> FUNKTION :                                                 *
      *BSK> INPUT    :                                                 *
      *BSK> PROCES   :                                                 *
      *BSK> OUTPUT   :                                                 *
      *BSK>*************************************************************
      *BSK>                                                            *
      *BSK> RETTELSER:                                                 *
      *BSK>                                                            *
      *BSK>  25.09.1996 - CNH   PROGRAMMET OPRETTET.                   *
      *BSK>  17.11.2006 - JCH   MULTIROW FEJLHÅNDTERING (DB28V)        *
      *BSK>                                                            *
      *BSK>*************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      ******************************************************************
      *                           WORKFELTER.                          *
      ******************************************************************
       01  FILLER             PIC  X(16)  VALUE 'START WORK******'.
      
       01          LE-DUMP-FELTER.
           03      LE-DUMP-BESTILLER      PIC X(8).
           03      SW-LE-DUMP             PIC 9(1) VALUE 0.
            88     DONT-LE-DUMP                    VALUE 0.
            88     DO-LE-DUMP                      VALUE 1.
      
       01          WS-FELTER.
           03      WORK-PAK COMP-3.
             05    WS-IDX                 PIC S9(3)  VALUE 0.
      
           03      WORK-UPK.
             05     WS-TIMESTMP           PIC X(26).
             05     WS-DISP-LINIE.
               07   WS-DISP-FEJL-ID.
                 09 WS-DISP-TIMESTMP      PIC X(26).
                 09 WS-DISP-LOEBENR       PIC 9(04).
               07   WS-DISP-INDH          PIC X(50).
      
             05     WS-DISP-LIN1.
               07                         PIC X(05) VALUE ':PRG:'.
               07   WS-DISP-I-PROGNAVN    PIC X(20).
               07                         PIC X(06) VALUE ':FEJL:'.
               07   WS-DISP-FJL-FUNKTION  PIC X(16).
               07                         PIC X(01) VALUE ':'.
      
             05     WS-DISP-LIN2.
               07                         PIC X(06) VALUE ':FUNK:'.
               07   WS-DISP-I-FUNKTION    PIC X(25).
               07                         PIC X(03) VALUE ':K:'.
               07   WS-DISP-I-STATU-KD-X  PIC X(15).
               07                         PIC X(01) VALUE ':'.
      
             05     WS-DISP-LIN3.
               07                         PIC X(07) VALUE ':IDENT:'.
               07   WS-DISP-SCR-KEY-REGNR PIC 9(04).
               07                         PIC X(01) VALUE ':'.
               07   WS-DISP-OPID          PIC X(08).
               07                         PIC X(01) VALUE ':'.
               07   WS-DISP-TERM          PIC X(08).
               07                         PIC X(01) VALUE ':'.
               07   WS-DISP-SYS-ID        PIC X(03).
               07                         PIC X(01) VALUE ':'.
               07   WS-DISP-SCR-ID        PIC X(11).
               07                         PIC X(01) VALUE ':'.
      
             05     WS-DISP-LIN4.
               07                         PIC X(09) VALUE ':PRG-POS:'.
               07   WS-DISP-I-PRG-POS     PIC X(40).
               07                         PIC X(01) VALUE ':'.
      
             05     WS-DISP-SQL.
               07                         PIC X(01) VALUE ':'.
               07   WS-DISP-SQL-TEXT      PIC X(14).
               07                         PIC X(01) VALUE ':'.
               07   WS-DISP-SQL-INDH      PIC X(34).
      
             05     WS-DISP-SQL-ERR.
               07                         PIC X(10).
               07   WS-DISP-SQL-ERR-1     PIC X(50).
               07   WS-DISP-SQL-ERR-2     PIC X(36).
               07   WS-DISP-SQL-ERR-3     PIC X(36).
      
             05    WS-TAB-DISP-DATA       PIC X(3000).
             05    REDEFINES WS-TAB-DISP-DATA .
               07  WS-TAB-DISP            PIC X(50) OCCURS 60.
             05    WS-STATU-KD            PIC 9(3).
      
       01 DIAGNOSTICS-FELTER.
            05  WS-TAELLER           PIC S9(9) USAGE COMP.
            05  WS-DB2-RETURNED-SQLCODE PIC +ZZZ.
            05  WS-ANTAL-FEJL        PIC S9(9) USAGE COMP-3 VALUE ZERO.
      
            05  WS-TAELLER-DISP      PIC X(9).
            05  WS-DB2-ROW-NUM       PIC X(9).
            05  WS-POSITION          PIC 9(9).
            05  WS-LGT               PIC 9(5).
      
            05  WS-SQLCODE-DISPLAY   PIC +ZZZ VALUE ZERO.
      
      ******************************************************************
      ** INCLUDE AREALER
      ******************************************************************
      
       01 BDSDATO-PARM.
           COPY BDSDATOI.
      
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.
      
           COPY DB2FEJLI.
      
           COPY SQLIFMDI.
           COPY BDSFDMPI.
       01 DB2-DIAGNOSTICS.
           COPY DB2DIAGI.
      
      ******************************************************************
       01  FILLER             PIC  X(16)  VALUE 'SLUT WORK*******'.
      ******************************************************************
       LINKAGE SECTION.
       01  LS-PARM.
           COPY BDSMFJLI.
      ******************************************************************
      * P R O C E D U R E   D I V I S I O N                            *
      ******************************************************************
       PROCEDURE DIVISION USING LS-PARM.
      
       000-START SECTION.
      
           PERFORM 010-INIT
      
           PERFORM 020-VALIDERING
      
           PERFORM 030-BEHANDLING
      
           PERFORM 040-AFSLUT
           .
      ******************************************************************
      * INITIERING AF WORK FELTER.                                     *
      ******************************************************************
       010-INIT SECTION.
      
           INITIALIZE WS-FELTER
      
           MOVE BDSMFJL-STATU-KD
             TO WS-STATU-KD
      
           SET  BDSMFJL-STATU-OK TO TRUE
           MOVE SPACES
             TO BDSMFJL-MEDD-TEKST1
                BDSMFJL-MEDD-TEKST2
           .
      ******************************************************************
      *                                                                *
      ******************************************************************
       020-VALIDERING SECTION.
      
           EVALUATE TRUE
              WHEN BDSMFJL-FUNK-CALL
              WHEN BDSMFJL-FUNK-INPUT
              WHEN BDSMFJL-FUNK-DATA
              WHEN BDSMFJL-FUNK-SQL
              WHEN BDSMFJL-FUNK-SUPRA
              WHEN BDSMFJL-FUNK-MANTIS
              WHEN BDSMFJL-FUNK-ANDET
              WHEN BDSMFJL-FUNK-LE-DUMP
                   CONTINUE
              WHEN OTHER
                   SET  BDSMFJL-STATU-SYSTEMFEJL TO TRUE
                   PERFORM 700-FIND-TIMESTAMP
                   MOVE ZEROES
                     TO WS-DISP-LOEBENR
                   MOVE 'FEJL I KALD TIL BDSMFJL !!!!!:'
                     TO WS-TAB-DISP-DATA
                   MOVE BDSMFJL-I-DATA-ANDET
                     TO WS-TAB-DISP-DATA(30:)
                   PERFORM 500-TABEL-BEH
           END-EVALUATE
           .
      ******************************************************************
      *                                                                *
      ******************************************************************
       030-BEHANDLING SECTION.
      
           IF BDSMFJL-FUNK-LE-DUMP
             MOVE BDSMFJL-I-PROGNAVN TO LE-DUMP-BESTILLER
             SET DO-LE-DUMP TO TRUE
           ELSE
             PERFORM 050-GENEREL
      
             EVALUATE TRUE
                WHEN BDSMFJL-FUNK-CALL
                     PERFORM 100-FUNK-CALL
                WHEN BDSMFJL-FUNK-INPUT
                     PERFORM 150-FUNK-INPUT
                WHEN BDSMFJL-FUNK-DATA
                     PERFORM 200-FUNK-DATA
                WHEN BDSMFJL-FUNK-SQL
                     PERFORM 250-FUNK-SQL
                WHEN BDSMFJL-FUNK-SUPRA
                     PERFORM 300-FUNK-SUPRA
                WHEN BDSMFJL-FUNK-MANTIS
                     PERFORM 350-FUNK-MANTIS
                WHEN BDSMFJL-FUNK-ANDET
                     PERFORM 400-FUNK-ANDET
             END-EVALUATE
           END-IF
           .
      ******************************************************************
      *                                         AFSLUT PROGRAMMET      *
      ******************************************************************
       040-AFSLUT SECTION.
      
           MOVE WS-DISP-FEJL-ID
             TO BDSMFJL-FEJL-ID
      
           GOBACK
      
           .
      ******************************************************************
      *    GENEREL HÅNDTERING I ALLE TILFÆLDE                          *
      ******************************************************************
       050-GENEREL SECTION.
      
           IF DO-LE-DUMP
             SET DONT-LE-DUMP TO TRUE
             MOVE SPACES TO BDSFDMP-TITLE
             STRING 'FORCERET LE-DUMP BESTILT AF ' LE-DUMP-BESTILLER
               DELIMITED BY SIZE INTO BDSFDMP-TITLE
             CALL 'CEE3DMP' USING BDSFDMP-TITLE
                                  BDSFDMP-OPTIONS
                                  BDSFDMP-FC
           END-IF
      
           IF BDSMFJL-FEJL-ID NOT = SPACES
              MOVE BDSMFJL-FEJL-ID
                TO WS-DISP-FEJL-ID
           ELSE
              PERFORM 700-FIND-TIMESTAMP
              MOVE ZEROES
                TO WS-DISP-LOEBENR
              IF WS-STATU-KD = 999
      *         FØRSTE KALD TIL BDSMFJL MED SYSTEMFEJL, VI SKAL BEHANDLE
      *         FEJLEN
                PERFORM 800-KALD-BDSMQFJ
              END-IF
           END-IF
      
           MOVE ALL '*'
             TO WS-DISP-INDH
           PERFORM 600-DISPLAY-LINIE
      
           MOVE BDSMFJL-I-PROGNAVN
             TO WS-DISP-I-PROGNAVN
           MOVE BDSMFJL-FUNKTION
             TO WS-DISP-FJL-FUNKTION
           MOVE WS-DISP-LIN1
             TO WS-DISP-INDH
           PERFORM 600-DISPLAY-LINIE
      
           MOVE BDSMFJL-I-FUNKTION
             TO WS-DISP-I-FUNKTION
           MOVE BDSMFJL-I-STATU-KD-X
             TO WS-DISP-I-STATU-KD-X
           MOVE WS-DISP-LIN2
             TO WS-DISP-INDH
           PERFORM 600-DISPLAY-LINIE
      
           IF BDSMFJL-SCR-KEY-REGNR NUMERIC
              MOVE BDSMFJL-SCR-KEY-REGNR
                TO WS-DISP-SCR-KEY-REGNR
           ELSE
              MOVE ZEROES
                TO WS-DISP-SCR-KEY-REGNR
           END-IF
           MOVE BDSMFJL-OPID
             TO WS-DISP-OPID
           MOVE BDSMFJL-TERM
             TO WS-DISP-TERM
           MOVE BDSMFJL-SYS-ID
             TO WS-DISP-SYS-ID
           MOVE BDSMFJL-SCR-ID
             TO WS-DISP-SCR-ID
           MOVE WS-DISP-LIN3
             TO WS-DISP-INDH
           PERFORM 600-DISPLAY-LINIE
      
           MOVE SPACES TO WS-DISP-INDH
           STRING 'PRG-POS:' BDSMFJL-I-PRG-POS ':'
             DELIMITED BY SIZE INTO WS-DISP-INDH
           PERFORM 600-DISPLAY-LINIE
      
           IF BDSMFJL-I-PRG-POS-1 NOT = SPACES
             MOVE SPACES TO WS-DISP-INDH
             STRING 'PRG-POS-2 ':'
               DELIMITED BY SIZE INTO WS-DISP-INDH
             PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-PRG-POS-2 NOT = SPACES
             MOVE SPACES TO WS-DISP-INDH
             STRING 'PRG-POS-3 ':'
               DELIMITED BY SIZE INTO WS-DISP-INDH
             PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-PRG-POS-3 NOT = SPACES
             MOVE SPACES TO WS-DISP-INDH
             STRING 'PRG-POS-4 ':'
               DELIMITED BY SIZE INTO WS-DISP-INDH
             PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-BESKRIV NOT = SPACES
              MOVE BDSMFJL-I-BESKRIV
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           .
      ******************************************************************
      *  BEHANDEL FUNKTIONSKODEN = 'FEJL-CALL'                         *
      ******************************************************************
       100-FUNK-CALL SECTION.
      
           MOVE 'PG:'
             TO WS-DISP-INDH
           MOVE BDSMFJL-I-CALL-PRG
             TO WS-DISP-INDH(4:)
           MOVE ':'
             TO WS-DISP-INDH(25:1)
           MOVE BDSMFJL-I-CALL-FUNKTION
             TO WS-DISP-INDH(26:)
           PERFORM 600-DISPLAY-LINIE
      
           MOVE 'KD-X:'
             TO WS-DISP-INDH
           MOVE BDSMFJL-I-CALL-KD-X
             TO WS-DISP-INDH(6:)
           MOVE ':STATUS:'
             TO WS-DISP-INDH(22:8)
           MOVE BDSMFJL-I-CALL-STAT
             TO WS-DISP-INDH(30:)
           PERFORM 600-DISPLAY-LINIE
      
           IF BDSMFJL-I-CALL-KD NUMERIC
              MOVE 'KODE:'
                TO WS-DISP-INDH
              MOVE BDSMFJL-I-CALL-KD
                TO WS-DISP-INDH(6:)
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-CALL-MEDD1 NOT = SPACES AND
              BDSMFJL-I-CALL-MEDD1 NOT = LOW-VALUES
              MOVE 'MEDD1:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-CALL-MEDD1
                TO WS-TAB-DISP-DATA(7:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-CALL-MEDD2 NOT = SPACES AND
              BDSMFJL-I-CALL-MEDD2 NOT = LOW-VALUES
              MOVE 'MEDD2:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-CALL-MEDD2
                TO WS-TAB-DISP-DATA(7:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-CALL-MEDD3 NOT = SPACES AND
              BDSMFJL-I-CALL-MEDD3 NOT = LOW-VALUES
              MOVE 'MEDD3:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-CALL-MEDD3
                TO WS-TAB-DISP-DATA(7:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-CALL-MEDD4 NOT = SPACES AND
              BDSMFJL-I-CALL-MEDD4 NOT = LOW-VALUES
              MOVE 'MEDD4:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-CALL-MEDD4
                TO WS-TAB-DISP-DATA(7:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-CALL-VAR1 NOT = SPACES AND
              BDSMFJL-I-CALL-VAR1 NOT = LOW-VALUES
              MOVE 'VAR1:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-CALL-VAR1
                TO WS-TAB-DISP-DATA(6:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-CALL-VAR2 NOT = SPACES AND
              BDSMFJL-I-CALL-VAR2 NOT = LOW-VALUES
              MOVE 'VAR2:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-CALL-VAR2
                TO WS-TAB-DISP-DATA(6:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-CALL-VAR3 NOT = SPACES AND
              BDSMFJL-I-CALL-VAR3 NOT = LOW-VALUES
              MOVE 'VAR3:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-CALL-VAR3
                TO WS-TAB-DISP-DATA(6:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-CALL-VAR4 NOT = SPACES AND
              BDSMFJL-I-CALL-VAR4 NOT = LOW-VALUES
              MOVE 'VAR4:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-CALL-VAR4
                TO WS-TAB-DISP-DATA(6:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           .
      ******************************************************************
      *  BEHANDEL FUNKTIONSKODEN = 'FEJL-INPUT'                        *
      ******************************************************************
       150-FUNK-INPUT SECTION.
      
           MOVE BDSMFJL-I-DATA-INPUT
             TO WS-TAB-DISP-DATA
           PERFORM 500-TABEL-BEH
      
           .
      ******************************************************************
      *  BEHANDEL FUNKTIONSKODEN = 'FEJL-DATA'                         *
      ******************************************************************
       200-FUNK-DATA SECTION.
      
           IF BDSMFJL-I-DATA-NGL1 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL1 NOT = LOW-VALUES
              MOVE 'DATA NØGLE # 1'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL1
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-NGL2 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL2 NOT = LOW-VALUES
              MOVE 'DATA NØGLE # 2'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL2
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-NGL3 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL3 NOT = LOW-VALUES
              MOVE 'DATA NØGLE # 3'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL3
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-NGL4 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL4 NOT = LOW-VALUES
              MOVE 'DATA NØGLE # 4'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL4
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-NGL5 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL5 NOT = LOW-VALUES
              MOVE 'DATA NØGLE # 5'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL5
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-NGL6 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL6 NOT = LOW-VALUES
              MOVE 'DATA NØGLE # 6'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL6
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-NGL7 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL7 NOT = LOW-VALUES
              MOVE 'DATA NØGLE # 7'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL7
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-NGL8 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL8 NOT = LOW-VALUES
              MOVE 'DATA NØGLE # 8'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL8
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-NGL9 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL9 NOT = LOW-VALUES
              MOVE 'DATA NØGLE # 9'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL9
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-NGL10 NOT = SPACES AND
              BDSMFJL-I-DATA-NGL10 NOT = LOW-VALUES
              MOVE 'DATA NØGLE #10'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-NGL10
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL1 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL1 NOT = LOW-VALUES
              MOVE 'DATA TABEL # 1'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL1
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL2 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL2 NOT = LOW-VALUES
              MOVE 'DATA TABEL # 2'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL2
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL3 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL3 NOT = LOW-VALUES
              MOVE 'DATA TABEL # 3'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL3
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL4 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL4 NOT = LOW-VALUES
              MOVE 'DATA TABEL # 4'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL4
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL5 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL5 NOT = LOW-VALUES
              MOVE 'DATA TABEL # 5'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL5
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL6 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL6 NOT = LOW-VALUES
              MOVE 'DATA TABEL # 6'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL6
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL7 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL7 NOT = LOW-VALUES
              MOVE 'DATA TABEL # 7'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL7
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL8 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL8 NOT = LOW-VALUES
              MOVE 'DATA TABEL # 8'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL8
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL9 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL9 NOT = LOW-VALUES
              MOVE 'DATA TABEL # 9'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL9
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-TBL10 NOT = SPACES AND
              BDSMFJL-I-DATA-TBL10 NOT = LOW-VALUES
              MOVE 'DATA TABEL #10'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-DATA-TBL1
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-DATA-SQLCA NOT = SPACES
      
              MOVE BDSMFJL-I-DATA-SQLCA
                TO SQLCA
      
              CALL 'SQLIFMD' USING SQLCA SQLIFMDI
      
              MOVE '- SQLIFID'
                TO WS-DISP-SQL-TEXT
              MOVE SQLIFID
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
      
              MOVE '- SQLIFBC '
                TO WS-DISP-SQL-TEXT
              MOVE SQLIFBC
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
      
              MOVE '- KATEGORI'
                TO WS-DISP-SQL-TEXT
              MOVE KATEGORI
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
      
              MOVE '- KAT-TEXT'
                TO WS-DISP-SQL-TEXT
              MOVE KATEGORI-TEXT
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
      
              PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 8
                 MOVE ERROR-MSG-TEXT-R (WS-IDX)
                   TO WS-DISP-SQL-ERR
                 IF WS-DISP-SQL-ERR-1 NOT = SPACES AND
                    WS-DISP-SQL-ERR-1 NOT = LOW-VALUES
                    MOVE WS-DISP-SQL-ERR-1
                      TO WS-DISP-INDH
                    PERFORM 600-DISPLAY-LINIE
                 END-IF
                 IF WS-DISP-SQL-ERR-2 NOT = SPACES AND
                    WS-DISP-SQL-ERR-2 NOT = LOW-VALUES
                    MOVE ' - #2'
                      TO WS-DISP-INDH
                    MOVE WS-DISP-SQL-ERR-2
                      TO WS-DISP-INDH(14:36)
                    PERFORM 600-DISPLAY-LINIE
                 END-IF
                 IF WS-DISP-SQL-ERR-3 NOT = SPACES AND
                    WS-DISP-SQL-ERR-3 NOT = LOW-VALUES
                    MOVE ' - #3'
                      TO WS-DISP-INDH
                    MOVE WS-DISP-SQL-ERR-3
                      TO WS-DISP-INDH(14:36)
                    PERFORM 600-DISPLAY-LINIE
                 END-IF
              END-PERFORM
      
              PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
                 MOVE SQLWARN-TEXT (WS-IDX)
                   TO WS-DISP-SQL-ERR
                 IF WS-DISP-SQL-ERR-1 NOT = SPACES AND
                    WS-DISP-SQL-ERR-1 NOT = LOW-VALUES
                    MOVE WS-DISP-SQL-ERR-1
                      TO WS-DISP-INDH
                    PERFORM 600-DISPLAY-LINIE
                 END-IF
                 IF WS-DISP-SQL-ERR-2 NOT = SPACES AND
                    WS-DISP-SQL-ERR-2 NOT = LOW-VALUES
                    MOVE ' - #2'
                      TO WS-DISP-INDH
                    MOVE WS-DISP-SQL-ERR-2
                      TO WS-DISP-INDH(14:36)
                    PERFORM 600-DISPLAY-LINIE
                 END-IF
                 IF WS-DISP-SQL-ERR-3 NOT = SPACES AND
                    WS-DISP-SQL-ERR-3 NOT = LOW-VALUES
                    MOVE ' - #3'
                      TO WS-DISP-INDH
                    MOVE WS-DISP-SQL-ERR-3
                      TO WS-DISP-INDH(14:36)
                    PERFORM 600-DISPLAY-LINIE
                 END-IF
              END-PERFORM
           END-IF
      
           IF BDSMFJL-I-DATA-VAR1 NOT = SPACES AND
              BDSMFJL-I-DATA-VAR1 NOT = LOW-VALUES
              MOVE 'VAR1:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-DATA-VAR1
                TO WS-TAB-DISP-DATA(6:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-DATA-VAR2 NOT = SPACES AND
              BDSMFJL-I-DATA-VAR2 NOT = LOW-VALUES
              MOVE 'VAR2:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-DATA-VAR2
                TO WS-TAB-DISP-DATA(6:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-DATA-VAR3 NOT = SPACES AND
              BDSMFJL-I-DATA-VAR3 NOT = LOW-VALUES
              MOVE 'VAR3:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-DATA-VAR3
                TO WS-TAB-DISP-DATA(6:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           IF BDSMFJL-I-DATA-VAR4 NOT = SPACES AND
              BDSMFJL-I-DATA-VAR4 NOT = LOW-VALUES
              MOVE 'VAR4:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-DATA-VAR4
                TO WS-TAB-DISP-DATA(6:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           .
      ******************************************************************
      *  BEHANDEL FUNKTIONSKODEN = 'FEJL-SQL'                          *
      ******************************************************************
       250-FUNK-SQL SECTION.
      
           IF BDSMFJL-I-SQL-NGL1 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL1 NOT = LOW-VALUES
              MOVE 'SQL NØGLE # 1'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL1
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-NGL2 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL2 NOT = LOW-VALUES
              MOVE 'SQL NØGLE # 2'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL2
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-NGL3 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL3 NOT = LOW-VALUES
              MOVE 'SQL NØGLE # 3'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL3
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-NGL4 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL4 NOT = LOW-VALUES
              MOVE 'SQL NØGLE # 4'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL4
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-NGL5 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL5 NOT = LOW-VALUES
              MOVE 'SQL NØGLE # 5'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL5
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-NGL6 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL6 NOT = LOW-VALUES
              MOVE 'SQL NØGLE # 6'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL6
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-NGL7 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL7 NOT = LOW-VALUES
              MOVE 'SQL NØGLE # 7'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL7
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-NGL8 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL8 NOT = LOW-VALUES
              MOVE 'SQL NØGLE # 8'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL8
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-NGL9 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL9 NOT = LOW-VALUES
              MOVE 'SQL NØGLE # 9'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL9
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-NGL10 NOT = SPACES AND
              BDSMFJL-I-SQL-NGL10 NOT = LOW-VALUES
              MOVE 'SQL NØGLE #10'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-NGL10
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL1 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL1 NOT = LOW-VALUES
              MOVE 'SQL TABEL # 1'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL1
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL2 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL2 NOT = LOW-VALUES
              MOVE 'SQL TABEL # 2'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL2
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL3 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL3 NOT = LOW-VALUES
              MOVE 'SQL TABEL # 3'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL3
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL4 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL4 NOT = LOW-VALUES
              MOVE 'SQL TABEL # 4'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL4
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL5 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL5 NOT = LOW-VALUES
              MOVE 'SQL TABEL # 5'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL5
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL6 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL6 NOT = LOW-VALUES
              MOVE 'SQL TABEL # 6'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL6
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL7 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL7 NOT = LOW-VALUES
              MOVE 'SQL TABEL # 7'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL7
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL8 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL8 NOT = LOW-VALUES
              MOVE 'SQL TABEL # 8'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL8
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL9 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL9 NOT = LOW-VALUES
              MOVE 'SQL TABEL # 9'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL9
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-TBL10 NOT = SPACES AND
              BDSMFJL-I-SQL-TBL10 NOT = LOW-VALUES
              MOVE 'SQL TABEL #10'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SQL-TBL10
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SQL-SQLCA NOT = SPACES
              MOVE BDSMFJL-I-SQL-SQLCA
                TO SQLCA
      
      * NY KODE START
      * DISSE SQLCODER SKAL DER BRUGES GET DIAGNOSTICS PÅ
              EVALUATE SQLCODE
                  WHEN -393
                  WHEN -353
                  WHEN -254
                  WHEN -253
                  WHEN -227
                  WHEN +252
                  WHEN +335
                  WHEN +354
                       PERFORM 255-STYR-GET-DIAGNOSTICS
                  WHEN OTHER
                       PERFORM 260-KALD-SQLIFMD
              END-EVALUATE
           END-IF
      
           .
      ******************************************************************
       255-STYR-GET-DIAGNOSTICS SECTION.
      ******************************************************************
      *   WS-ANTAL-FEJL FELTET S K A L VÆRE DEFINERET SOM ET COMP FELT
      
      *    INSERT EN LINIE I FEJLLOGGEN OMKRING OVERORDNEDE SQLFEJL
           MOVE ':- OVERORDNET SQL FEJL:' TO WS-DISP-INDH
           MOVE SQLCODE                   TO WS-DISP-INDH ( 24: )
           PERFORM 600-DISPLAY-LINIE
           PERFORM 260-KALD-SQLIFMD
      
      *    HENT ANTAL FEJL
           EXEC SQL GET DIAGNOSTICS
               :WS-ANTAL-FEJL = NUMBER
           END-EXEC
      
      *    HENT ALLE DETAIL FEJL FOR RÆKKERNE
           PERFORM VARYING WS-TAELLER FROM WS-ANTAL-FEJL
                           BY -1 UNTIL WS-TAELLER <= 1
               PERFORM 265-HENT-CONDITION-DIAG
      
      *        LAVER EN HEADER TIL HVER SQLCODE
               MOVE SPACE TO WS-DISP-INDH
               MOVE ':- HERUNDER SQL FEJL ' TO WS-DISP-INDH
               MOVE 22 TO WS-POSITION
      
               MOVE DB2-ROW-NUMBER TO WS-DB2-ROW-NUM
               MOVE WS-TAELLER     TO WS-TAELLER-DISP
      
      *        PAKKER FEJLNUMMER SAMMEN MED FEJLTEKSTEN
               MOVE ZERO TO TALLY
               INSPECT WS-TAELLER-DISP TALLYING TALLY FOR LEADING ZEROES
               MOVE 9 TO WS-LGT
               SUBTRACT TALLY FROM WS-LGT
               MOVE WS-TAELLER-DISP (TALLY + 1:WS-LGT)
                 TO WS-DISP-INDH (WS-POSITION:)
               ADD WS-LGT TO WS-POSITION
               ADD 1      TO WS-POSITION
      
      *        PAKKER RÆKKENUMMER SAMMEN MED FEJLTEKSTEN
      *        HVIS DET ER EN RÆKKERELATERET FEJL
               IF WS-DB2-ROW-NUM > ZEROES
                 MOVE 'TIL RÆKKE ' TO WS-DISP-INDH(WS-POSITION:)
                 ADD 10 TO WS-POSITION
                 MOVE ZERO TO TALLY
                 INSPECT WS-DB2-ROW-NUM  TALLYING TALLY FOR LEADING ZERO
                 MOVE 9 TO WS-LGT
                 SUBTRACT TALLY FROM WS-LGT
                 MOVE WS-DB2-ROW-NUM (TALLY + 1:WS-LGT)
                   TO WS-DISP-INDH (WS-POSITION:)
                 ADD WS-LGT TO WS-POSITION
               END-IF
      
               PERFORM 600-DISPLAY-LINIE
      *        HEADER SLUT
               PERFORM 260-KALD-SQLIFMD
           END-PERFORM
           .
      ******************************************************************
       260-KALD-SQLIFMD SECTION.
      ******************************************************************
      
            CALL 'SQLIFMD' USING SQLCA SQLIFMDI
      
            MOVE '- SQLIFID'
              TO WS-DISP-SQL-TEXT
            MOVE SQLIFID
              TO WS-DISP-SQL-INDH
            MOVE WS-DISP-SQL
              TO WS-DISP-INDH
            PERFORM 600-DISPLAY-LINIE
      
            MOVE '- SQLIFBC '
              TO WS-DISP-SQL-TEXT
            MOVE SQLIFBC
              TO WS-DISP-SQL-INDH
            MOVE WS-DISP-SQL
              TO WS-DISP-INDH
            PERFORM 600-DISPLAY-LINIE
      
            MOVE '- KATEGORI'
              TO WS-DISP-SQL-TEXT
            MOVE KATEGORI
              TO WS-DISP-SQL-INDH
            MOVE WS-DISP-SQL
              TO WS-DISP-INDH
            PERFORM 600-DISPLAY-LINIE
      
            MOVE '- KAT-TEXT'
              TO WS-DISP-SQL-TEXT
            MOVE KATEGORI-TEXT
              TO WS-DISP-SQL-INDH
            MOVE WS-DISP-SQL
              TO WS-DISP-INDH
            PERFORM 600-DISPLAY-LINIE
      
            PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 8
               MOVE ERROR-MSG-TEXT-R (WS-IDX)
                 TO WS-DISP-SQL-ERR
               IF WS-DISP-SQL-ERR-1 NOT = SPACES AND
                  WS-DISP-SQL-ERR-1 NOT = LOW-VALUES
                  MOVE WS-DISP-SQL-ERR-1
                    TO WS-DISP-INDH
                  PERFORM 600-DISPLAY-LINIE
               END-IF
               IF WS-DISP-SQL-ERR-2 NOT = SPACES AND
                  WS-DISP-SQL-ERR-2 NOT = LOW-VALUES
                  MOVE ' - #2'
                    TO WS-DISP-INDH
                  MOVE WS-DISP-SQL-ERR-2
                    TO WS-DISP-INDH(14:36)
                  PERFORM 600-DISPLAY-LINIE
               END-IF
               IF WS-DISP-SQL-ERR-3 NOT = SPACES AND
                  WS-DISP-SQL-ERR-3 NOT = LOW-VALUES
                  MOVE ' - #3'
                    TO WS-DISP-INDH
                  MOVE WS-DISP-SQL-ERR-3
                    TO WS-DISP-INDH(14:36)
                  PERFORM 600-DISPLAY-LINIE
               END-IF
            END-PERFORM
      
            PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
               MOVE SQLWARN-TEXT (WS-IDX)
                 TO WS-DISP-SQL-ERR
               IF WS-DISP-SQL-ERR-1 NOT = SPACES AND
                  WS-DISP-SQL-ERR-1 NOT = LOW-VALUES
                  MOVE WS-DISP-SQL-ERR-1
                    TO WS-DISP-INDH
                  PERFORM 600-DISPLAY-LINIE
               END-IF
               IF WS-DISP-SQL-ERR-2 NOT = SPACES AND
                  WS-DISP-SQL-ERR-2 NOT = LOW-VALUES
                  MOVE ' - #2'
                    TO WS-DISP-INDH
                  MOVE WS-DISP-SQL-ERR-2
                    TO WS-DISP-INDH(14:36)
                  PERFORM 600-DISPLAY-LINIE
               END-IF
               IF WS-DISP-SQL-ERR-3 NOT = SPACES AND
                  WS-DISP-SQL-ERR-3 NOT = LOW-VALUES
                  MOVE ' - #3'
                    TO WS-DISP-INDH
                  MOVE WS-DISP-SQL-ERR-3
                    TO WS-DISP-INDH(14:36)
                  PERFORM 600-DISPLAY-LINIE
               END-IF
            END-PERFORM
           .
      ******************************************************************
       265-HENT-CONDITION-DIAG SECTION.
      ******************************************************************
      
           EXEC SQL GET DIAGNOSTICS CONDITION :WS-TAELLER
             :CONDITION-NUMBER           = CONDITION_NUMBER
           , :DB2-RETURNED-SQLCODE       = DB2_RETURNED_SQLCODE
           , :DB2-ROW-NUMBER             = DB2_ROW_NUMBER
           , :MESSAGE-TEXT               = MESSAGE_TEXT
           , :RETURNED-SQLSTATE          = RETURNED_SQLSTATE
           END-EXEC
      
      
           COMPUTE WS-DB2-RETURNED-SQLCODE = DB2-RETURNED-SQLCODE
      
      *    SKAL FLYTTE FELTERNE, DA DIAG HAR RESAT SQLCA
           MOVE DB2-RETURNED-SQLCODE TO SQLCODE
           MOVE RETURNED-SQLSTATE    TO SQLSTATE
           .
      ******************************************************************
      *  BEHANDEL FUNKTIONSKODEN = 'FEJL-SUPRA'                        *
      ******************************************************************
       300-FUNK-SUPRA SECTION.
      
           IF BDSMFJL-I-SUPRA-NGL1 NOT = SPACES AND
              BDSMFJL-I-SUPRA-NGL1 NOT = LOW-VALUES
              MOVE 'SUPRA NGL # 1'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SUPRA-NGL1
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-NGL2 NOT = SPACES AND
              BDSMFJL-I-SUPRA-NGL2 NOT = LOW-VALUES
              MOVE 'SUPRA NGL # 2'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SUPRA-NGL2
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-NGL3 NOT = SPACES AND
              BDSMFJL-I-SUPRA-NGL3 NOT = LOW-VALUES
              MOVE 'SUPRA NGL # 3'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SUPRA-NGL3
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-NGL4 NOT = SPACES AND
              BDSMFJL-I-SUPRA-NGL4 NOT = LOW-VALUES
              MOVE 'SUPRA NGL # 4'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SUPRA-NGL4
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-NGL5 NOT = SPACES AND
              BDSMFJL-I-SUPRA-NGL5 NOT = LOW-VALUES
              MOVE 'SUPRA NGL # 5'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SUPRA-NGL5
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-NGL6 NOT = SPACES AND
              BDSMFJL-I-SUPRA-NGL6 NOT = LOW-VALUES
              MOVE 'SUPRA NGL # 6'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SUPRA-NGL6
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-NGL7 NOT = SPACES AND
              BDSMFJL-I-SUPRA-NGL7 NOT = LOW-VALUES
              MOVE 'SUPRA NGL # 7'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SUPRA-NGL7
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-NGL8 NOT = SPACES AND
              BDSMFJL-I-SUPRA-NGL8 NOT = LOW-VALUES
              MOVE 'SUPRA NGL # 8'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SUPRA-NGL8
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-NGL9 NOT = SPACES AND
              BDSMFJL-I-SUPRA-NGL9 NOT = LOW-VALUES
              MOVE 'SUPRA NGL # 9'
                TO WS-DISP-SQL-TEXT
              MOVE BDSMFJL-I-SUPRA-NGL9
                TO WS-DISP-SQL-INDH
              MOVE WS-DISP-SQL
                TO WS-DISP-INDH
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           MOVE 'VIEW...: '
             TO WS-DISP-INDH
           MOVE BDSMFJL-I-SUPRA-VIEW
             TO WS-DISP-INDH(9:)
           PERFORM 600-DISPLAY-LINIE
      
           MOVE 'TIS-OBJ: '
             TO WS-DISP-INDH
           MOVE BDSMFJL-I-OBJECT-NAME
             TO WS-DISP-INDH(9:)
           PERFORM 600-DISPLAY-LINIE
      
           MOVE 'TIS-OPR: '
             TO WS-DISP-INDH
           MOVE BDSMFJL-I-OPERATION
             TO WS-DISP-INDH(9:)
           MOVE ':'
             TO WS-DISP-INDH(15:1)
           MOVE BDSMFJL-I-FSI
             TO WS-DISP-INDH(16:1)
           MOVE ':'
             TO WS-DISP-INDH(17:1)
           MOVE BDSMFJL-I-VSI
             TO WS-DISP-INDH(18:1)
           MOVE ':'
             TO WS-DISP-INDH(19:1)
           MOVE BDSMFJL-I-PASSWORD
             TO WS-DISP-INDH(20:8)
           MOVE ':'
             TO WS-DISP-INDH(29:1)
           MOVE BDSMFJL-I-OPTIONS
             TO WS-DISP-INDH(30:4)
           MOVE ':'
             TO WS-DISP-INDH(35:1)
           MOVE BDSMFJL-I-CONTEXT
             TO WS-DISP-INDH(36:4)
           MOVE ':'
             TO WS-DISP-INDH(40:1)
           MOVE BDSMFJL-I-LVCONTEXT
             TO WS-DISP-INDH(41:4)
           MOVE ':'
             TO WS-DISP-INDH(45:1)
           PERFORM 600-DISPLAY-LINIE
              PERFORM 600-DISPLAY-LINIE
      
           IF BDSMFJL-I-SUPRA-ASI1 NOT = SPACES AND
              BDSMFJL-I-SUPRA-ASI1 NOT = LOW-VALUES
              MOVE 'SUPRA ASI1:'
                TO WS-DISP-INDH
              MOVE BDSMFJL-I-SUPRA-ASI1
                TO WS-DISP-INDH(12:)
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-ASI2 NOT = SPACES AND
              BDSMFJL-I-SUPRA-ASI2 NOT = LOW-VALUES
              MOVE 'SUPRA ASI2:'
                TO WS-DISP-INDH
              MOVE BDSMFJL-I-SUPRA-ASI2
                TO WS-DISP-INDH(12:)
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-ASI3 NOT = SPACES AND
              BDSMFJL-I-SUPRA-ASI3 NOT = LOW-VALUES
              MOVE 'SUPRA ASI3:'
                TO WS-DISP-INDH
              MOVE BDSMFJL-I-SUPRA-ASI3
                TO WS-DISP-INDH(12:)
              PERFORM 600-DISPLAY-LINIE
           END-IF
      
           IF BDSMFJL-I-SUPRA-VAR NOT = SPACES AND
              BDSMFJL-I-SUPRA-VAR NOT = LOW-VALUES
              MOVE 'SUPRA VAR:'
                TO WS-TAB-DISP-DATA
              MOVE BDSMFJL-I-DATA-SUPRA
                TO WS-TAB-DISP-DATA(11:)
              PERFORM 500-TABEL-BEH
           END-IF
      
           .
      ******************************************************************
      *  BEHANDEL FUNKTIONSKODEN = 'FEJL-MANTIS'                       *
      ******************************************************************
       350-FUNK-MANTIS SECTION.
      
           MOVE BDSMFJL-I-DATA-MANTIS
             TO WS-TAB-DISP-DATA
           PERFORM 500-TABEL-BEH
      
           .
      ******************************************************************
      *  BEHANDEL FUNKTIONSKODEN = 'FEJL-ANDET'                        *
      ******************************************************************
       400-FUNK-ANDET SECTION.
      
           MOVE BDSMFJL-I-DATA-ANDET
             TO WS-TAB-DISP-DATA
           PERFORM 500-TABEL-BEH
      
           .
      ******************************************************************
      *  DISPLAY FEJL-LINIE                                            *
      ******************************************************************
       500-TABEL-BEH SECTION.
      
           PERFORM VARYING WS-IDX FROM 1 BY 1
             UNTIL WS-IDX > 60
              IF WS-TAB-DISP(WS-IDX) NOT = SPACES AND
                 WS-TAB-DISP(WS-IDX) NOT = LOW-VALUES
                 MOVE WS-TAB-DISP(WS-IDX)
                   TO WS-DISP-INDH
                 PERFORM 600-DISPLAY-LINIE
              END-IF
           END-PERFORM
      
           .
      ******************************************************************
      *  DISPLAY FEJL-LINIE                                            *
      ******************************************************************
       600-DISPLAY-LINIE SECTION.
      
           IF WS-DISP-LOEBENR NUMERIC
              ADD  1
                TO WS-DISP-LOEBENR
           ELSE
              MOVE 0
                TO WS-DISP-LOEBENR
           END-IF
      
            DISPLAY WS-DISP-LINIE
           .
      *****************************************************************
      ******
      *****************************************************************
       700-FIND-TIMESTAMP SECTION.
      
           SET FORMAT-DB2-TIMESTAMP IN BDSDATO-PARM TO TRUE
           SET BDSDATO-KLOKKEN      IN BDSDATO-PARM TO TRUE
      
           CALL 'BDSDA21'  USING BDSDATO-PARM
           END-CALL
      
           IF BDSDATO-OK
              MOVE DATO-DB2-TIMESTAMP            IN BDSDATO-PARM
                TO WS-TIMESTMP
              MOVE '#BDF#'
                TO WS-DISP-TIMESTMP
              MOVE WS-TIMESTMP(1:4)
                TO WS-DISP-TIMESTMP(6:)
              MOVE WS-TIMESTMP(6:2)
                TO WS-DISP-TIMESTMP(10:)
              MOVE WS-TIMESTMP(9:2)
                TO WS-DISP-TIMESTMP(12:)
              MOVE WS-TIMESTMP(12:2)
                TO WS-DISP-TIMESTMP(14:)
              MOVE WS-TIMESTMP(15:2)
                TO WS-DISP-TIMESTMP(16:)
              MOVE WS-TIMESTMP(18:9)
                TO WS-DISP-TIMESTMP(18:)
           ELSE
              MOVE '#BDF#19000101010101.000001'
                TO WS-DISP-TIMESTMP
              MOVE ZEROES
                TO WS-DISP-LOEBENR
              PERFORM 600-DISPLAY-LINIE
           END-IF
           .
      *-----------------------------------------------------------------
       800-KALD-BDSMQFJ SECTION.
      *-----------------------------------------------------------------
           CALL 'BDSMQFJ' USING LS-PARM WS-TIMESTMP
           END-CALL
           .
      