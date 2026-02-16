       ID DIVISION.
       PROGRAM-ID.         BDSM043.
       AUTHOR.             KIM ANDERSEN, CAP GEMINI.
       DATE-WRITTEN.       17.11.1998.
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
      *BSK> MODUL TIL DISPLAY AF HVOR MANGE LOGISKE ENHEDER DER BLIVER *
      *BSK> BEHANDLET PR. MINUT I ET BATCH PROGRAM                     *
      *BSK>                                                            *
      *BSK> INPUT     : DISPLAY-TEKST  DEN TEKST DER SKAL DISPLAY'ES   *
      *BSK>                            FØRST                           *
      *BSK> OUTPUT    : INGEN                                          *
      *BSK>                                                            *
      *BSK>*************************************************************
      *BSK>                                                            *
      *BSK> RETTELSER:                                                 *
      *BSK>                                                            *
      *BSK>  03.02.2000 - HJA DER OPSÆTTES DEFAULT STOPUR= JA          *
      *BSK>  04.09.2013 - RUC FØRSTE PERIODE BEREGNES KORREKT          *
      *BSK>               OG PERFORMANCE FORBEDRINGER.                 *
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
      
       01  INDIVIDUELLE-WORK-FELTER.
           03  WS-PROGRAMNAVN            PIC X(8) VALUE 'BDSM043'.
           03 FILLER                     PIC 9(1) VALUE ZERO.
              88 SW-FOERSTE-GANG                  VALUE ZERO.
              88 SW-EJ-FOERSTE-GANG               VALUE 1.
           03 WS-ANT-BEH-MIN             PIC 9(9) COMP-5 VALUE 0.
           03 WS-ANT-BEH-1-MIN           PIC 9(9) COMP-5 VALUE 0.
           03 WS-ANT-BEH-GNS-1-MIN       PIC 9(9) COMP-5 VALUE 0.
           03 WS-ANT-BEH-TOTAL           PIC 9(9) COMP-5 VALUE 0.
           03 WS-ANT-BEH-MIN-Z           PIC ZZZ.ZZZ.ZZ9.
           03 WS-ANT-BEH-TOTAL-Z         PIC ZZZ.ZZZ.ZZ9.
           03 WS-ANT-PER                 PIC 9(9) COMP-5 VALUE 0.
           03 WS-ANT-PER-GNS-100         PIC 9(9) COMP-5 VALUE 3.
           03 WS-ANT-PER-GNS-TEST        PIC 9(9) COMP-5 VALUE 0.
           03 WS-ANT-PER-GNS             PIC 9(9) COMP-5.
           03 WS-ANT-PER-GNS-Z           PIC ZZZ.ZZZ.ZZ9.
           03 WS-SPEED-1                 PIC X(75) VALUE
           ' SPEED               ANTAL         TOTAL     GNS     '.
           03 WS-SPEED-2                 PIC X(75) VALUE
           ' SPEED  KLOKKEN    PR. MINUT       ANTAL   PR. MINUT NGL'.
           03 WS-SPEED-3                 PIC X(75) VALUE
           ' SPEED  --------  ----------  ----------  ---------- ----'.
      
           03 WS-TIME.
              05 WS-TID-TT               PIC 9(2).
              05 WS-TID-MM               PIC 9(2).
              05 WS-TID-SS               PIC 9(2).
              05 WS-TID-UU               PIC 9(2).
      
           03 WS-TIME-GEM.
              05 WS-TID-AKTUEL-MM        PIC 9(2) COMP-5.
              05 WS-TID-GEM-MM           PIC 9(2) COMP-5 VALUE 0.
              05 WS-TID-GEM-SS           PIC 9(2) COMP-5 VALUE 0.
      /
       01 BDSMFJL-PARM.
           COPY BDSMFJLI.
      /
       LINKAGE SECTION.
       01  BDSM043-PARM.
           COPY BDSM043I.
      *
      ******************************************************************
      * P R O C E D U R E   D I V I S I O N                            *
      ******************************************************************
       PROCEDURE DIVISION USING BDSM043-PARM.
      
           REPLACE ==:BDSIXXX:== BY ==BDSM043==.
      
      * INCLUDER RUTINER TIL PROGRAM-STYRING ***************************
           COPY BDSISTYR.
      *
      ******************************************************************
       003-BEHANDLING SECTION.
      ******************************************************************
           PERFORM 200-OPTAEL
           .
      ******************************************************************
       100-IDV-INIT SECTION.
      ******************************************************************
           IF SW-FOERSTE-GANG
             SET SW-EJ-FOERSTE-GANG TO TRUE
      
             DISPLAY ' '
             DISPLAY WS-SPEED-1
             DISPLAY WS-SPEED-2
             DISPLAY WS-SPEED-3
           END-IF
           .
      ******************************************************************
       200-OPTAEL SECTION.
      ******************************************************************
      *    VI SAMPLER KUN TID HVER GANG VI HAR BEHANDLET 1/100 AF
      *    GENNEMSNIT.
           IF WS-ANT-BEH-MIN >= WS-ANT-PER-GNS-TEST
              ACCEPT WS-TIME FROM TIME
              MOVE WS-TID-MM TO WS-TID-AKTUEL-MM
      
              IF WS-TID-AKTUEL-MM NOT =
                 WS-TID-GEM-MM
                  MOVE WS-ANT-BEH-MIN TO WS-ANT-BEH-MIN-Z
      *           ER DER IKKE BEHANDLET NOGLE, DISPLAYER VI FØRSTE NUL
      *           SPEED.
                  IF WS-ANT-BEH-TOTAL > 0
                  OR WS-ANT-BEH-MIN   > 0
      *              NÅR DEN NYE TID ER HØJERE REGNER VI PÅ EEN MÅDE,
      *              ELLERS SKAL VI TAGE HØJDE FOR AT VI HAR SKIFTET
      *              TIME.
                     IF WS-TID-AKTUEL-MM > WS-TID-GEM-MM
                        COMPUTE WS-ANT-PER
                              = WS-ANT-PER
                              + (WS-TID-AKTUEL-MM
                              - WS-TID-GEM-MM)
                        END-COMPUTE
                     ELSE
                        COMPUTE WS-ANT-PER
                              = WS-ANT-PER
                              + (60 - WS-TID-GEM-MM)
                              + WS-TID-AKTUEL-MM
                        END-COMPUTE
                     END-IF
      *              FØRSTE PERIODE BEREGNES SPECIELT, SÅ GENNEMSNITTET
      *              KOMMER TIL AT MATCHE OVER ET HELT MINUT.
                     IF WS-ANT-PER = 1
                        COMPUTE WS-ANT-BEH-GNS-1-MIN
                              = WS-ANT-BEH-MIN
                              / (60 - WS-TID-GEM-SS)
                              * 60
                        END-COMPUTE
                        DIVIDE WS-ANT-BEH-GNS-1-MIN BY 100
                        GIVING WS-ANT-PER-GNS-100
                               WS-ANT-PER-GNS-TEST
                        MOVE WS-ANT-BEH-GNS-1-MIN TO WS-ANT-PER-GNS-Z
                        MOVE WS-ANT-BEH-MIN       TO WS-ANT-BEH-TOTAL-Z
                                                     WS-ANT-BEH-1-MIN
                     ELSE
                        ADD WS-ANT-BEH-MIN TO WS-ANT-BEH-TOTAL
                        COMPUTE WS-ANT-PER-GNS
                             = (WS-ANT-BEH-TOTAL + WS-ANT-BEH-GNS-1-MIN)
                             / WS-ANT-PER
                        END-COMPUTE
                        DIVIDE WS-ANT-PER-GNS BY 100
                        GIVING WS-ANT-PER-GNS-100
                               WS-ANT-PER-GNS-TEST
                        MOVE WS-ANT-PER-GNS   TO WS-ANT-PER-GNS-Z
                        ADD WS-ANT-BEH-TOTAL TO WS-ANT-BEH-1-MIN
                        GIVING WS-ANT-BEH-TOTAL-Z
                     END-IF
                  ELSE
                     MOVE 0                TO WS-ANT-PER-GNS-Z
                                              WS-ANT-BEH-TOTAL-Z
                     MOVE WS-TID-SS        TO WS-TID-GEM-SS
                     MOVE 100              TO WS-ANT-PER-GNS-TEST
                  END-IF
                  MOVE WS-TID-AKTUEL-MM TO WS-TID-GEM-MM
                  DISPLAY
                      ' SPEED  '
                      WS-TID-TT ':' WS-TID-MM ':' WS-TID-SS ' '
                      WS-ANT-BEH-MIN-Z  ' '
                      WS-ANT-BEH-TOTAL-Z ' '
                      WS-ANT-PER-GNS-Z '  '
                      DISPLAY-TEKST IN BDSM043-PARM
                  MOVE 0 TO WS-ANT-BEH-MIN
              ELSE
                 IF WS-ANT-BEH-TOTAL = 0
                    ADD WS-ANT-PER-GNS-100 TO WS-ANT-PER-GNS-TEST
                    IF WS-ANT-PER-GNS-100 * 13 < WS-ANT-PER-GNS-TEST
                       DIVIDE WS-ANT-BEH-MIN BY 10
                       GIVING WS-ANT-PER-GNS-100
                    END-IF
                 ELSE
                    ADD WS-ANT-PER-GNS-100 TO WS-ANT-PER-GNS-TEST
                 END-IF
              END-IF
           END-IF
      
           ADD 1 TO WS-ANT-BEH-MIN
           .
      / INCLUDER RUTINER TIL BEHANDLING AF DIV. FEJL********************
           COPY BDSIREST REPLACING ==SQLCA== BY ==SPACES==.
      