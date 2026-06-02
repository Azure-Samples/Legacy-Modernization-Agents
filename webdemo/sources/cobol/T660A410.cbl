       ID DIVISION.
       PROGRAM-ID.           T660A410  INITIAL.
       AUTHOR.               LARS VERMDAHL.
           DATE-WRITTEN.     FEB 1997.
           REMARKS.

      ******************************************************************
      *                                                                *
      *    O B S E R V E R A !                                         *
      *                                                                *
      *    DET FINNS EN GAMMAL VERSION AV DENNA BUSINESS ACTION.       *
      *                                                                *
      *    GAMMALT NAMN: T6601410                                      *
      *    ÄNDRING:      SE NEDAN                                      *
      *    DATUM:        2003-06-03   B.UTBULT                         *
      *                                                                *
      *    O B S E R V E R A !                                         *
      *                                                                *
      ******************************************************************
      *                 PROGRAM = T660A410
      ******************************************************************
      *
      *                 BUSINESS-ACTION
      *                 05  HÄMTA VISSA ITEM-NUMMERSERIER
      *    INDATA OCH UTDATA I SAMMA COPYTEXT (T660A410)
      *    RETURKOD OCH IDFEL I COPYTEXT TALAR OM HUR DET GÅTT
      ******************************************************************
      *  Ä N D R I N G S L O G G
      *
      * DATUM  SIGN ÄNDRING
      * ------ ---- ---------------------------------------------------
      * 030603 BU   - TAGIT BORT KDANSVKK, IDAVD, KDARTKAT, KDARTTYP2
      *             - LAGT TILL TIAAAAMMDD-ALLOK
      * 030909 LV   - GÖR EJ SELECT COUNT OM ANROP KOMMER FRÅN ANNAN BA
      *
      * 040517 LV   - LAGT TILL KDBATCH
      *
      * 050420 LV   - TAGIT BORT DISPLAY
      * 120521 MA   - INITIERING AV 'PSEUDO WAIT FOR INPUT'
      *               NY SEKTION AA-INITERA-VARIABLER
      ******************************************************************
           EJECT
       ENVIRONMENT DIVISION.
      *DATAMANAGER SECTION.
      *    SUBPGM
       CONFIGURATION SECTION.

       SOURCE-COMPUTER.   IBM-370.
      *SOURCE-COMPUTER.   IBM-370 WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       DATA DIVISION.

       FILE SECTION.
           EJECT
       WORKING-STORAGE SECTION.

       77  PROGRAM-NAMN                PIC X(8)    VALUE 'T660A410'.
       77  W-SECTION                   PIC X(20)   VALUE SPACE.

       01  GENERELLA-KONSTANTER.
           03  JA                      PIC X(1)    VALUE 'Y'.
           03  NEJ                     PIC X(1)    VALUE 'N'.

       01  DIVERSE-VARIABLER.
           03  W-AREA                  PIC X(500).
           03  W-ANTAL-RADER           PIC 9(6).
           03  W-ANTAL-RADER-Z         PIC ZZZZZ9.
           03  MAX-LA410-RAD-IX        PIC S9(5)   COMP-3 VALUE +1000.
           03  ACK-FNUTT               PIC S9(4)   COMP.

       01  SUBPROGRAM.
           03  T6601010                PIC X(8)    VALUE 'T6601010'.
           03  T6601112                PIC X(8)    VALUE 'T6601112'.
           03  T660A411                PIC X(8)    VALUE 'T660A411'.
           EJECT
      *    -COPY   T6601010   -PRE W1010-.
           EJECT
      *    -COPY   T6601112   -PRE W1112-.
           EJECT
      *    -COPY   T660A411   -PRE WA411-.
           EJECT

       LINKAGE SECTION.

      *    -COPY   T660A410   -PRE LA410-.
       01  LA410-AREA          PIC X(500).
           EJECT
       PROCEDURE DIVISION USING LA410-T660A410 LA410-AREA.

       STYR SECTION.
           MOVE 'STYR                         ' TO W-SECTION
      D    DISPLAY '*** T660A410 START ***'

           PERFORM A-INIT
           PERFORM B-KOLLA-BEH

           IF W1010-FLBEHOERIG = JA
             PERFORM C-KOLLA-LA410
             IF  LA410-KDRETUR = ZERO
             AND LA410-IDFEL   = ZERO
               IF LA410-KDBEHAND = '05'
                 PERFORM D-HAMTA-VISSA-INT
               END-IF

               PERFORM F-HAMTA-MEDD
               PERFORM G-ANTAL-RADER
             ELSE
               PERFORM F-HAMTA-MEDD
             END-IF
           ELSE
             PERFORM F-HAMTA-MEDD
           END-IF

           PERFORM Z-FINIT
      D    DISPLAY '*** T660A410 SLUT  ***'
           MOVE ZERO TO RETURN-CODE
           GOBACK
           CONTINUE.
           EJECT
       A-INIT SECTION.
      ******************************************************************
      *
      ******************************************************************
           MOVE 'A-                      ' TO W-SECTION
      D    DISPLAY W-SECTION
           PERFORM AA-INITIERA-VARIABLER

           MOVE '000'                      TO LA410-KDRETUR
           MOVE '0000'                     TO LA410-IDFEL
           CONTINUE.
           EJECT

       AA-INITIERA-VARIABLER SECTION.
      ******************************************************************
      *
      ******************************************************************
           MOVE 'AA-INITIERA-VARIABLER   '  TO W-SECTION
      D    DISPLAY W-SECTION

           MOVE SPACE                       TO W-AREA
           MOVE ZERO                        TO W-ANTAL-RADER
                                               W-ANTAL-RADER-Z
                                               ACK-FNUTT
           .

       B-KOLLA-BEH SECTION.
      ******************************************************************
      *
      ******************************************************************
           MOVE 'B-                      ' TO W-SECTION
      D    DISPLAY W-SECTION

           MOVE SPACE                      TO W1010-T6601010
           MOVE LA410-IDUSER               TO W1010-IDUSER
           MOVE LA410-PASSWORD             TO W1010-PASSWORD
           MOVE LA410-IDBA                 TO W1010-IDBA

           CALL T6601010 USING W1010-T6601010
                               W-AREA

           MOVE W1010-IDFEL                TO LA410-IDFEL
           MOVE W1010-KDRETUR              TO LA410-KDRETUR
           CONTINUE.
           EJECT
       C-KOLLA-LA410 SECTION.
      ******************************************************************
      *
      ******************************************************************
           MOVE 'C-                      ' TO W-SECTION
      D    DISPLAY W-SECTION

           IF LA410-KDBEHAND = '05'
             PERFORM CA-KOLLA-05
           ELSE
             MOVE '1006'                   TO LA410-IDFEL
           END-IF
           CONTINUE.
           EJECT
       CA-KOLLA-05 SECTION.
      ******************************************************************
      *
      ******************************************************************
           MOVE 'CA-                     ' TO W-SECTION
      D    DISPLAY W-SECTION

           INSPECT LA410-SORTORD REPLACING ALL SPACE BY ZERO
           IF LA410-SORTORD = ZERO
             MOVE '01'                     TO LA410-KDSRTORD-IDITEM-FROM
           END-IF

           IF LA410-IDINDAPX-IN = SPACE OR '%%%'
             MOVE 'VO '                    TO LA410-IDINDAPX-IN
           END-IF

           MOVE ZERO                       TO ACK-FNUTT
           INSPECT  LA410-DATA
           TALLYING ACK-FNUTT
           FOR ALL  QUOTE
           IF ACK-FNUTT > ZERO
             MOVE '1104'                   TO LA410-IDFEL
           END-IF
           CONTINUE.
           EJECT
       D-HAMTA-VISSA-INT SECTION.
      ******************************************************************
      *
      ******************************************************************
           MOVE 'D-                      ' TO W-SECTION
      D    DISPLAY W-SECTION

           MOVE SPACE                      TO WA411-T660A411
           IF LA410-IDBA-START > SPACE
             MOVE 'KOLA    '               TO WA411-IDUSER
           END-IF
           MOVE LA410-FRAGA                TO WA411-FRAGA
           MOVE LA410-KDBEHAND             TO WA411-KDBEHAND
           MOVE LA410-KDBATCH              TO WA411-KDBATCH

           CALL T660A411 USING WA411-T660A411
                               W-AREA

           MOVE WA411-ANTAL                TO LA410-ANTAL
           MOVE WA411-IDFEL                TO LA410-IDFEL
           MOVE WA411-KDRETUR              TO LA410-KDRETUR
           MOVE WA411-SVAR                 TO LA410-SVAR
           MOVE WA411-IDSERV-SQLFEL        TO LA410-IDSERV-SQLFEL
           MOVE WA411-IDSECT-SQLFEL        TO LA410-IDSECT-SQLFEL
           CONTINUE.
           EJECT
       F-HAMTA-MEDD SECTION.
      ******************************************************************
      *
      ******************************************************************
           MOVE 'F-                      ' TO W-SECTION
      D    DISPLAY W-SECTION

           MOVE SPACE                      TO W1112-T6601112
           MOVE LA410-IDFEL                TO W1112-IDFEL-IN
           MOVE W1010-KDSPRAAK             TO W1112-KDSPRAAK

           CALL T6601112 USING W1112-T6601112
                               W-AREA

           IF W1112-KDRETUR = '000'
             MOVE W1112-BEFEL-UT           TO LA410-BEFEL
             MOVE W1112-KDURSP-FEL-UT      TO LA410-KDURSP-FEL
             MOVE W1112-KDALLVAR-FEL-UT    TO LA410-KDALLVAR-FEL
           END-IF
           CONTINUE.
           EJECT
       G-ANTAL-RADER SECTION.
      ******************************************************************
      *
      ******************************************************************
           MOVE 'G-                      ' TO W-SECTION
      D    DISPLAY W-SECTION

           MOVE LA410-KVTRAFF              TO W-ANTAL-RADER-Z
           INSPECT LA410-BEFEL REPLACING  ALL 'XXXXXX'
           BY W-ANTAL-RADER-Z
           MOVE LA410-KVRADANTAL           TO W-ANTAL-RADER-Z
           INSPECT LA410-BEFEL REPLACING  ALL 'YYYYYY'
           BY W-ANTAL-RADER-Z
           CONTINUE.
           EJECT
       Z-FINIT SECTION.
      ******************************************************************
      *
      ******************************************************************
           MOVE 'Z-                      ' TO W-SECTION
      D    DISPLAY W-SECTION

           CONTINUE.
           EJECT
