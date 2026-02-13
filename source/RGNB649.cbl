      *#PREVIEW# PROGRAM REVIEW FORETAGET.
       ID DIVISION.
       PROGRAM-ID.         RGNB649.
       AUTHOR.             JØRGEN STUBTORFT (JSU).
       DATE-WRITTEN.       11.06.2021
      *SPC>-------------------------------------------------------------
      *SPC>          EVENTUEL SPECIALPROGRAMMERING OMFATTER:
      *SPC>
      *SPC> BANKNR.: XXX, XXX, XXX, XXX, XXX, XXX, XXX, XXX, XXX, XXX
      *SPC>
      *SPC> REGNR.: XXXX, XXXX, XXXX, XXXX, XXXX, XXXX, XXXX, XXXX
      *SPC>
      *SPC> OVENSTÅENDE FORMAT MÅ IKKE ÆNDRES. BANKNR ANGIVES ALTID,
      *SPC> OG MED FORANSTILLEDE NUL. DER KAN REPEAT'ES EFTER BEHOV.
      *BSK>-------------------------------------------------------------
      *BSK> MODULETS FORMÅL/OPGAVE (KORT BESKREVET)  SKELET: BDSBBE3
      *BSK>-------------------------------------------------------------
      *BSK>
      *BSK> MODULET FINDER OPLYSNINGER OM RENTESATSER OG KNÆK
      *BSK> TJEKKER PÅ, AT DER ER TALE OM KRD-RENTESATSER OG
      *BSK> DE IKKE ER FIKTIVE
      *BSK>
      *BSK>-------------------------------------------------------------
      *BSK>
      *BSK> PROGRAMMET HAR 2 GSAMFILER SOM INPUT
      *BSK> DET FORVENTES AT DATA ER SORTERET PÅ FI01-NØGLE OG AT DER
      *BSK> ER 1:1 MELLEM FI01 OG FI02
      *BSK>
      *BSK> I DETTE PROGRAM HAR VI VALGT AT UDSKRIVE DE RECORD'S SOM
      *BSK> IKKE ER ENS, MEN KAN OGSÅ VÆLGE AT SPRINGE OVER ELLER DUMPE
      *BSK>
      *BSK> DER DANNES FO01 FOR ALLE FI01-NOGLE = FI02-NOGLE
      *BSK>
      *BSK> DER DANNES FO02 FOR ALLE FI01-NOGLE UDEN FI02-NOGLE
      *BSK>
      *BSK> DER DANNES FO03 FOR ALLE FI02-NOGLE UDEN FI01-NOGLE
      *BSK>
      *BSK>
      *BSK> GENERELT BATCH-MODUL SOM INDEHOLDER COMMIT/RESTART
      *BSK> SAMT LÆS AF DIVERSE PARAMETRE.
      *BSK>
      *BSK> INPUT:    PARAMETER
      *BSK>             KDATO  : KØRSELSDATO
      *BSK>
      *BSK>           FI01     : GSAMFIL SORTERET I FI01-NOGLE ORDEN
      *BSK>           FI02     : GSAMFIL SORTERET I FI01-NOGLE ORDEN
      *BSK>
      *BSK> OUTPUT:   FO01     : GSAMFIL ENS
      *BSK>           FO02     : GSAMFI01 MANGLER I FI02 (FEJL ?)
      *BSK>           FO03     : GSAMFI02 MANGLER I FI01 (FEJL ?)
      *BSK>
      *BSK> MODULER:  BDSPARM  : FINDER PARAMETER OPLYSNINGER
      *BSK>           BDCOMMIT : RESTART OG COMMIT STYRING
      *BSK>           BDSDATO  : STANDARD DATORUTINE
      *BSK>           BDSPUST  : FINDER MILJØ PGM AFVIKLES I
      *BSK>           BDSM043  : SPEEDOMETER
      *BSK>
      *BSK> TABELLER: XXXTB999 : POSTERINGER
      *BSK>
      *CHG>----------+------+-------------------------------------------
      *CHG>   DATO   ! INIT ! FORMÅL MED ÆNDRING
      *CHG>----------+------+-------------------------------------------
      *CHG>11.06.2021+ JSU  + PROGRAMMERING PÅBEGYNDT
      *CHG>----------+------+-------------------------------------------
      *CHG>----------+------+-------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
               REPLACE ==:BDSIXXX:== BY ==RGNB649==.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      
       01  WS-HARD-CODE.
            03 WS-PROGRAMNAVN       PIC  X(08)  VALUE 'RGNB649'.
      
       01  WS-PARM-FELTER.
            03 WS-PARM-KOER-DATO    PIC  X(10).
            03 WS-PARM-KOER-FUNK    PIC  X(10).
      
       01  WS-FELTER.
            03 WS-KOER-DATO-PRIMO   PIC  X(10).
            03 WS-KOER-DATO-ULTIMO  PIC  X(10).
            03 WS-KOER-AAR          PIC  X(04).
            03 WS-KOER-MD           PIC  X(02).
      
       01  WS-SPEEDO-LINIE.
            03 BANKNR               PIC  9(03).
            03 FILLER               PIC  X(01)  VALUE SPACE.
            03 REGNR                PIC  9(04).
            03 FILLER               PIC  X(01)  VALUE SPACE.
            03 KTONR                PIC  9(10).
            03 FILLER               PIC  X(31)  VALUE SPACE.
      
      *-----------------------------------------------------------------
      * COMMITAREALER
      *-----------------------------------------------------------------
           COPY BDCOMMIC.
      
      *-----------------------------------------------------------------
      * DATA SOM GEMMES VED COMMIT
      *-----------------------------------------------------------------
       01  CHKP-AREA-1.
            03 ANTAL-CHKP-TAKEN     PIC  9(09)  VALUE 0 COMP-5.
            03 ANTAL-LAES-FI01      PIC  9(09)  VALUE 0 COMP-5.
            03 ANTAL-LAES-FI02      PIC  9(09)  VALUE 0 COMP-5.
            03 ANTAL-SKRIV-FO01     PIC  9(09)  VALUE 0 COMP-5.
            03 ANTAL-SKRIV-FO02     PIC  9(09)  VALUE 0 COMP-5.
            03 ANTAL-SKRIV-FO03     PIC  9(09)  VALUE 0 COMP-5.
            03 FI01-NOGLE.
              10 BANKNR             PIC  9(03)  VALUE 0.
              10 REGNR              PIC  9(04)  VALUE 0.
              10 KTONR              PIC  9(10)  VALUE 0.
              10 DETAIL-RTENR       PIC  9(5)   VALUE 0.
            03 FI02-NOGLE.
              10 BANKNR             PIC  9(03)  VALUE 0.
              10 REGNR              PIC  9(04)  VALUE 0.
              10 KTONR              PIC  9(10)  VALUE 0.
              10 DETAIL-RTENR       PIC  9(5)   VALUE 0.
       01  CHKP-AREA-1-END          PIC  X(01).
      
      *-----------------------------------------------------------------
      * FIL DEFINITIONER
      *-----------------------------------------------------------------
       01  FI01-REC.
           COPY RENI035.
      
       01  FI02-REC.
           COPY RENI033.
      
       01  FO01-REC.
           COPY RGNI656.
      
       01  FO02-REC.
           COPY RENI035.
      
       01  FO03-REC.
           COPY RENI033.
      
      *-----------------------------------------------------------------
      * COPYBOOKS
      *-----------------------------------------------------------------
       01  BDSDATO-PARM.
           COPY BDSDATOI.
      
           COPY BDSPARMC.
      
           COPY BDCSEQII REPLACING ==:XXXX:== BY ==FI01==
                                 ==:'XXXX':== BY =='FI01'==.
           COPY BDCSEQII REPLACING ==:XXXX:== BY ==FI02==
                                 ==:'XXXX':== BY =='FI02'==.
           COPY BDCSEQOI REPLACING ==:XXXX:== BY ==FO01==
                                 ==:'XXXX':== BY =='FO01'==.
           COPY BDCSEQOI REPLACING ==:XXXX:== BY ==FO02==
                                 ==:'XXXX':== BY =='FO02'==.
           COPY BDCSEQOI REPLACING ==:XXXX:== BY ==FO03==
                                 ==:'XXXX':== BY =='FO03'==.
      
       01  BDSM043-PARM.
           COPY BDSM043I.
      
       01  BDSIWKS2-PARM.
           COPY BDSIWKS2.
      
      *01  BDSIWKSD-PARM.
      *    COPY BDSIWKSD.
      
       01  BDSMFJL-PARM.
           COPY BDSMFJLI.
      
       01  RGNB649-PARM.
           COPY BDSISTDI.
            05 FUNKTION           PIC  X(16) VALUE SPACES.
      *----------------------------------------------------------------
      * SQL DEFINITIONER
      *----------------------------------------------------------------
           EXEC SQL INCLUDE SQLCA END-EXEC.
      *    COPY DB2FEJLI.
      
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       000-START-PGM SECTION.
      *-----------------------------------------------------------------
      
           INITIALIZE FI01-REC
                      FI02-REC
      
           PERFORM INC-COMMIT-INIT
      
           PERFORM 001-INIT
           PERFORM 003-BEHANDLING
           PERFORM 002-AFSLUT
           .
           COPY BDSIINI1.
      
      *-----------------------------------------------------------------
       003-BEHANDLING SECTION.
      *-----------------------------------------------------------------
      *BDS> HVIS DER INITIERES FELTER SOM DIREKT ER RELATERET TIL DEN
      *BDS> FORRETNINGSMÆSSIGE LOGIK GØRES DET HER
      
           PERFORM 300-BEHANDL-DATA UNTIL BDC-FI01-EOF AND BDC-FI02-EOF
      
      *BDS> HVIS DER SKAL SKE YDERLIGERE BEHANDLING, SÅ SOM OPDATERING
      *BDS> AF SUMMER O.LIGN. INDSÆTTES KODEN HER.
           .
      *-----------------------------------------------------------------
       100-IDV-INIT SECTION.
      *-----------------------------------------------------------------
      
           PERFORM 101-INITIER-FELTER
           PERFORM 200-OPEN-FILER
      
           PERFORM 121-CHECK-PGMSTART
      
           INITIALIZE BDSM043-PARM
           SET FUNK-OPTAEL IN BDSM043-PARM TO TRUE
           PERFORM 131-KALD-SPEEDOMETER
           .
      *-----------------------------------------------------------------
       101-INITIER-FELTER SECTION.
      *-----------------------------------------------------------------
      
           PERFORM 102-HENT-KOER-DATO
           PERFORM 103-HENT-PARAMETRE
      
           PERFORM 104-BEREGN-PRIMO-ULTIMO
           .
      *-----------------------------------------------------------------
       102-HENT-KOER-DATO SECTION.
      *-----------------------------------------------------------------
      *BDS> DET KAN I NOGLE TILFÆLDE VÆRE NYTTIGT AT VIDE HVILKET MILJØ
      *BDS> PROGRAMMET KØRER I.
      
           DISPLAY ' '
           CALL 'BDSPUST' USING WS-DER-KOERES-I-MILJO
           DISPLAY 'DER KØRES PÅ MILJØ: ' WS-DER-KOERES-I-MILJO
      
      *BDS> KØRSELSDATOEN FINDES SOM PARAMETER I JCL'ET
           MOVE '----------'     TO DATO-YYYY-MM-DD
           SET FORMAT-YYYY-MM-DD TO TRUE
           MOVE 1 TO BDSMFJL-I-PRG-POS
           PERFORM INC-KALD-BDSDATO-KDATO-MFH
      
           MOVE DATO-YYYY-MM-DD TO WS-PARM-KOER-DATO
           DISPLAY 'KØRSELSDATO.......: ' WS-PARM-KOER-DATO
           DISPLAY ' '
           .
      *-----------------------------------------------------------------
       103-HENT-PARAMETRE SECTION.
      *-----------------------------------------------------------------
      *BDS> HENTER YDERLIGERE KØRSELSPARAMETER
      
           INITIALIZE BDSPARMC
           MOVE 'KOERFUNK' TO BDSPNAME
           SET BDSP-FUNC-GET TO TRUE
      
           MOVE 2   TO BDSMFJL-I-PRG-POS
           PERFORM INC-KALD-BDSPARM
      
           IF NOT BDSP-RET-OK
             MOVE 3        TO BDSMFJL-I-PRG-POS
             PERFORM INC-FEJLMELD-BDSPARM
           END-IF
      
           MOVE BDSPDATA TO WS-PARM-KOER-FUNK
           .
      *-----------------------------------------------------------------
       104-BEREGN-PRIMO-ULTIMO SECTION.
      *-----------------------------------------------------------------
      
           MOVE WS-PARM-KOER-DATO  (1:8)
             TO WS-KOER-DATO-PRIMO (1:8)
           MOVE '01'
             TO WS-KOER-DATO-PRIMO (9:2)
      
           DISPLAY 'KØR DATO PRIMO.: ' WS-KOER-DATO-PRIMO
      
           MOVE WS-PARM-KOER-DATO  (1:4)
             TO WS-KOER-AAR
      
           MOVE WS-PARM-KOER-DATO  (6:2)
             TO WS-KOER-MD
      
           MOVE WS-PARM-KOER-DATO  (1:8)
             TO WS-KOER-DATO-ULTIMO (1:8)
      
           EVALUATE WS-KOER-MD
            WHEN 1
            WHEN 3
            WHEN 5
            WHEN 7
            WHEN 8
            WHEN 10
            WHEN 12
              MOVE '31' TO WS-KOER-DATO-ULTIMO (9:2)
            WHEN 4
            WHEN 6
            WHEN 9
            WHEN 11
              MOVE '30' TO WS-KOER-DATO-ULTIMO (9:2)
            WHEN 2
              EVALUATE WS-KOER-AAR
               WHEN '2024'
               WHEN '2028'
               WHEN '2032'
               WHEN '2036'
               WHEN '2040'
               WHEN '2044'
               WHEN '2048'
               WHEN '2052'
               WHEN '2056'
                 MOVE '29' TO WS-KOER-DATO-ULTIMO (9:2)
               WHEN OTHER
                 MOVE '28' TO WS-KOER-DATO-ULTIMO (9:2)
              END-EVALUATE
            WHEN OTHER
              MOVE '99' TO WS-KOER-DATO-ULTIMO (9:2)
           END-EVALUATE
      
           DISPLAY 'KØR DATO ULTIMO: ' WS-KOER-DATO-ULTIMO
           .
      *-----------------------------------------------------------------
       110-IDV-AFSLUT SECTION.
      *-----------------------------------------------------------------
      
           IF STATU-SYSTEMFEJL IN :BDSIXXX:-PARM
             PERFORM INC-ROLL-BACK
           END-IF
      
           DISPLAY ' '
           DISPLAY 'PROGRAM STATUS'
           DISPLAY 'FORETAGET.:' ANTAL-CHKP-TAKEN ' CHECKPOINTS'
           DISPLAY 'LÆST FI01.:' ANTAL-LAES-FI01
           DISPLAY 'LÆST FI02.:' ANTAL-LAES-FI02
           DISPLAY 'SKREVET...:' ANTAL-SKRIV-FO01 ' ENS '
           DISPLAY 'SKREVET...:' ANTAL-SKRIV-FO02 ' FI01 UDEN FI02'
           DISPLAY 'SKREVET...:' ANTAL-SKRIV-FO03 ' FI02 UDEN FI01'
      
           PERFORM 201-CLOSE-FILER
           PERFORM INC-COMMIT-SLUT
           .
      *-----------------------------------------------------------------
       115-IDV-DISPLAY-V-SYSTEMFEJL SECTION.
      *-----------------------------------------------------------------
      *BDS> RET DETTE TIL DE AKTUELLE NØGLER VED EN EVT. SYSTEMFEJL.
      *BDS> OG TILFØJ DE VARIABLER DER YDERLIGERE ØNSKES VIST.
           DISPLAY 'NØGLER VED SYSTEMFEJL :'
           IF BDC-FI01-OK
             DISPLAY 'BANKNR        ' BANKNR        IN FI01-REC
             DISPLAY 'REGNR         ' REGNR         IN FI01-REC
             DISPLAY 'KTONR         ' KTONR         IN FI01-REC
             DISPLAY 'DETAIL-RTENR  ' DETAIL-RTENR  IN FI01-REC
             DISPLAY 'DETAIL-RTE-ENHEDNR '
                                      DETAIL-RTE-ENHEDNR IN FI01-REC
           END-IF
           IF BDC-FI02-OK
             DISPLAY 'BANKNR        ' BANKNR        IN FI02-REC
             DISPLAY 'REGNR         ' REGNR         IN FI02-REC
             DISPLAY 'KTONR         ' KTONR         IN FI02-REC
             DISPLAY 'DETAIL-RTENR  ' DETAIL-RTENR  IN FI02-REC
             DISPLAY 'DETAIL-RTE-ENHEDNR '
                                      DETAIL-RTE-ENHEDNR IN FI02-REC
           END-IF
           .
      *-----------------------------------------------------------------
       120-VI-HAR-RESTARTET SECTION.
      *-----------------------------------------------------------------
      *BDS> HVIS DER SKAL GØRES NOGET SPECIELT NÅR PROGRAMMET RESTARTER
      *BDS> SKRIVES KODEN HER. VÆR OPMÆRKSOM PÅ AT DENNE SECTION KALDES
      *BDS> I FORBINDELSE MED KALD TIL COMMIT-INIT, DVS. FØR INITIERING!
      
           CONTINUE
           .
      *-----------------------------------------------------------------
       121-CHECK-PGMSTART SECTION.
      *-----------------------------------------------------------------
      *BDS> I DENNE SECTION UDFØRES KUN STYRINGSMÆSSIG KODE. ALLE
      *BDS> VARIABLE ER PÅ DETTE TIDSPUNKT INITIERET!
      
           IF VI-HAR-RESTARTET
             DISPLAY '***   RESTART AF RGNB649             ********'
             DISPLAY ' '
             DISPLAY 'NØGLER VED RESTART :'
             IF BDC-FI01-OK
               DISPLAY 'FI01 - BANKNR    ' BANKNR        IN FI01-REC
               DISPLAY 'FI01 - REGNR     ' REGNR         IN FI01-REC
               DISPLAY 'FI01 - KTONR     ' KTONR         IN FI01-REC
               DISPLAY 'FI01 - DETAIL-RTENR       '
                                            DETAIL-RTENR IN FI01-REC
               DISPLAY 'FI01 - DETAIL-RTE-ENHEDNR '
                                      DETAIL-RTE-ENHEDNR IN FI01-REC
             ELSE
               DISPLAY 'FI01 - EOF'
             END-IF
             DISPLAY ' '
             IF BDC-FI02-OK
               DISPLAY 'FI02 - BANKNR    ' BANKNR        IN FI02-REC
               DISPLAY 'FI02 - REGNR     ' REGNR         IN FI02-REC
               DISPLAY 'FI02 - KTONR     ' KTONR         IN FI02-REC
               DISPLAY 'FI02 - DETAIL-RTENR       '
                                            DETAIL-RTENR IN FI02-REC
               DISPLAY 'FI02 - DETAIL-RTE-ENHEDNR '
                                      DETAIL-RTE-ENHEDNR IN FI02-REC
             ELSE
               DISPLAY 'FI02 - EOF'
             END-IF
           END-IF
           .
      *-----------------------------------------------------------------
       130-COMMIT SECTION.
      *-----------------------------------------------------------------
      *BDS> VED COMMIT KALDES SPEEDOMETER OGSÅ.
      
           PERFORM INC-COMMIT
      
           PERFORM 131-KALD-SPEEDOMETER
           .
      *-----------------------------------------------------------------
       131-KALD-SPEEDOMETER SECTION.
      *-----------------------------------------------------------------
      *BDS> KALD TIL SPEEDOMETER.
      
           IF BDC-FI01-OK
             MOVE CORR FI01-NOGLE TO WS-SPEEDO-LINIE
           ELSE
             INITIALIZE WS-SPEEDO-LINIE
           END-IF
           MOVE WS-SPEEDO-LINIE TO DISPLAY-TEKST IN BDSM043-PARM
           MOVE 4   TO BDSMFJL-I-PRG-POS
           PERFORM INC-KALD-BDSM043
           .
      *-----------------------------------------------------------------
       200-OPEN-FILER SECTION.
      *-----------------------------------------------------------------
      *BDS> NÅR INPUT FILER ÅBNES LÆSES DER ALTID, DET GÆLDER BÅDE FOR
      *BDS> NORMAL START OG RESTART.
      
           PERFORM INC-FI01-COMMIT-INIT
           PERFORM 700-LAES-FI01
      
           PERFORM INC-FI02-COMMIT-INIT
           PERFORM 701-LAES-FI02
      
           PERFORM INC-FO01-COMMIT-INIT
           PERFORM INC-FO02-COMMIT-INIT
           PERFORM INC-FO03-COMMIT-INIT
           .
      *-----------------------------------------------------------------
       201-CLOSE-FILER SECTION.
      *-----------------------------------------------------------------
      
           PERFORM INC-FI01-COMMIT-SLUT
           PERFORM INC-FI02-COMMIT-SLUT
           PERFORM INC-FO01-COMMIT-SLUT
           PERFORM INC-FO02-COMMIT-SLUT
           PERFORM INC-FO03-COMMIT-SLUT
           .
      *-----------------------------------------------------------------
       300-BEHANDL-DATA SECTION.
      *-----------------------------------------------------------------
      *BDS> BEHANDLING AF DATA OMFATTER KUN DEN FORRETNINGSMÆSSIGE LOGIK
      
           EVALUATE TRUE
             WHEN BDC-FI01-EOF
               PERFORM 311-FI02-UDEN-FI01
               PERFORM 701-LAES-FI02
      
             WHEN BDC-FI02-EOF
               PERFORM 310-FI01-UDEN-FI02
               PERFORM 700-LAES-FI01
      
             WHEN FI01-NOGLE = FI02-NOGLE
               PERFORM 301-FI01-MED-FI02
      *BDS> COMMIT SKAL/KAN KUN FORETAGES NÅR DE TO FILER ER ENS!!!
               PERFORM 130-COMMIT
               PERFORM 700-LAES-FI01
      *   ***  PERFORM 701-LAES-FI02
      *JSU: DA DER ER 1:M FI01:FI02 LÆSES KUN FI01 HER. KAN GIVE EN
      *     MASSE MARKERINGER PÅ FO02 OG FO03. MEN DET SKAL KUN BRUGES
      *     TIL KONTROL OG MÅ SES BORT FRA
      
             WHEN FI01-NOGLE > FI02-NOGLE
               PERFORM 311-FI02-UDEN-FI01
               PERFORM 701-LAES-FI02
      
             WHEN FI01-NOGLE < FI02-NOGLE
               PERFORM 310-FI01-UDEN-FI02
               PERFORM 700-LAES-FI01
      
             WHEN OTHER
               SET BDSMFJL-FUNK-DATA TO TRUE
               MOVE 5 TO BDSMFJL-I-PRG-POS
               MOVE 'FEJL I LOGIKKEN: ' TO BDSMFJL-I-BESKRIV
               PERFORM 999-FEJLMELD
           END-EVALUATE
           .
      *-----------------------------------------------------------------
       301-FI01-MED-FI02 SECTION.
      *-----------------------------------------------------------------
      *
      *  DER SKAL VÆRE TALE OM EN KREDIT RENTETYPE OG DEN MÅ IKKE VÆRE
      *  FIKTIV. DEN SKAL VÆRE AKTIV I DEN MÅNED OPGØRELSEN VEDRØRER.
      *
      
           INITIALIZE FO01-REC
      
           IF RTE-TYP-KRD IN FI02-REC
           AND FIKTIV-RENTE-NEJ IN FI02-REC
           AND AKTIV-DATO IN FI01-REC  <= WS-KOER-DATO-ULTIMO
           AND FRKOBL-DATO IN FI01-REC >= WS-KOER-DATO-PRIMO
              PERFORM 305-OPBYG-FO01
              PERFORM 710-SKRIV-FO01
           ELSE
              MOVE FI01-REC
                TO FO02-REC
              PERFORM 711-SKRIV-FO02
      
              MOVE FI02-REC
                TO FO03-REC
              PERFORM 712-SKRIV-FO03
           END-IF
           .
      *-----------------------------------------------------------------
       305-OPBYG-FO01 SECTION.
      *-----------------------------------------------------------------
           INITIALIZE FO01-REC
      
           MOVE BANKNR              IN FI01-REC
             TO BANKNR              IN FO01-REC
           MOVE REGNR               IN FI01-REC
             TO REGNR               IN FO01-REC
           MOVE KTONR               IN FI01-REC
             TO KTONR               IN FO01-REC
           MOVE DETAIL-RTENR        IN FI01-REC
             TO DETAIL-RTENR        IN FO01-REC
           MOVE INTVALNR            IN FI01-REC
             TO INTVALNR            IN FO01-REC
           MOVE DETAIL-RTE-ENHEDNR  IN FI01-REC
             TO DETAIL-RTE-ENHEDNR  IN FO01-REC
           MOVE RTE-TYP             IN FI01-REC
             TO RTE-TYP             IN FO01-REC
           MOVE AKTIV-DATO          IN FI01-REC
             TO AKTIV-DATO          IN FO01-REC
           MOVE FRKOBL-DATO         IN FI01-REC
             TO FRKOBL-DATO         IN FO01-REC
           MOVE UDG-DATO            IN FI01-REC
             TO UDG-DATO            IN FO01-REC
           MOVE NAVN                IN FI01-REC
             TO NAVN                IN FO01-REC
           MOVE MINI-NAVN           IN FI01-REC
             TO MINI-NAVN           IN FO01-REC
           MOVE PROD-RTE-ENHEDNR    IN FI01-REC
             TO PROD-RTE-ENHEDNR    IN FO01-REC
           MOVE PROD-RTENR          IN FI01-REC
             TO PROD-RTENR          IN FO01-REC
           MOVE PRIS-TLKOBL-DATO    IN FI01-REC
             TO PRIS-TLKOBL-DATO    IN FO01-REC
           MOVE RTE-PRIS-TYP        IN FI01-REC
             TO RTE-PRIS-TYP        IN FO01-REC
           MOVE RTE-PRISNR          IN FI01-REC
             TO RTE-PRISNR          IN FO01-REC
           MOVE INTVAL-TIL-BEL      IN FI01-REC
             TO INTVAL-TIL-BEL      IN FO01-REC
           MOVE MIN-SATS            IN FI01-REC
             TO MIN-SATS            IN FO01-REC
           MOVE KNAEK-MAX-KD        IN FI01-REC
             TO KNAEK-MAX-KD        IN FO01-REC
           MOVE FAST-PR-KD          IN FI01-REC
             TO FAST-PR-KD          IN FO01-REC
           MOVE GRUND-PRISNR        IN FI01-REC
             TO GRUND-PRISNR        IN FO01-REC
           MOVE GRUND-SATS          IN FI01-REC
             TO GRUND-SATS          IN FO01-REC
           MOVE AFVIG-GRUND-SATS    IN FI01-REC
             TO AFVIG-GRUND-SATS    IN FO01-REC
           MOVE RTE-PRIS-SATS       IN FI01-REC
             TO RTE-PRIS-SATS       IN FO01-REC
           MOVE AFVIG-RTE-SATS      IN FI01-REC
             TO AFVIG-RTE-SATS      IN FO01-REC
           MOVE DETAIL-SATS         IN FI01-REC
             TO DETAIL-SATS         IN FO01-REC
           MOVE BER-RTE-KD          IN FI01-REC
             TO BER-RTE-KD          IN FO01-REC
           MOVE MAX-SATS            IN FI01-REC
             TO MAX-SATS            IN FO01-REC
           MOVE PRIS-MIN-SATS       IN FI01-REC
             TO PRIS-MIN-SATS       IN FO01-REC
           MOVE PRIS-MIN-SATS-KD    IN FI01-REC
             TO PRIS-MIN-SATS-KD    IN FO01-REC
           MOVE RTE-LOFT-SATS       IN FI01-REC
             TO RTE-LOFT-SATS       IN FO01-REC
           MOVE RTE-LOFT-KD         IN FI01-REC
             TO RTE-LOFT-KD         IN FO01-REC
           MOVE RTE-LOFT-UDLOB-DATO IN FI01-REC
             TO RTE-LOFT-UDLOB-DATO IN FO01-REC
           MOVE UND-RTE-TYP-KD      IN FI01-REC
             TO UND-RTE-TYP-KD      IN FO01-REC
      
           IF DETAIL-GRLAG-TYP-28   IN FI02-REC
              SET NETTING-SORT-JA   IN FO01-REC TO TRUE
           ELSE
              SET NETTING-SORT-NEJ  IN FO01-REC TO TRUE
           END-IF
           .
      *-----------------------------------------------------------------
       310-FI01-UDEN-FI02 SECTION.
      *-----------------------------------------------------------------
      *
      *  HER BEHANDLES DE RECORD SOM KUN ER I FI01 (EVT DUMP)
      *
           INITIALIZE FO02-REC
      
           MOVE FI01-REC
             TO FO02-REC
      
           PERFORM 711-SKRIV-FO02
           .
      *-----------------------------------------------------------------
       311-FI02-UDEN-FI01 SECTION.
      *-----------------------------------------------------------------
      *
      *  HER BEHANDLES DE RECORD SOM KUN ER I FI02 (EVT DUMP)
      *
      
           INITIALIZE FO03-REC
      
           MOVE FI02-REC
             TO FO03-REC
      
           PERFORM 712-SKRIV-FO03
           .
      *-----------------------------------------------------------------
       700-LAES-FI01 SECTION.
      *-----------------------------------------------------------------
      
           PERFORM INC-FI01-LAES
           IF BDC-FI01-OK
             MOVE BANKNR IN FI01-REC  TO BANKNR IN FI01-NOGLE
             MOVE REGNR  IN FI01-REC  TO REGNR  IN FI01-NOGLE
             MOVE KTONR  IN FI01-REC  TO KTONR  IN FI01-NOGLE
             MOVE DETAIL-RTENR IN FI01-REC
               TO DETAIL-RTENR IN FI01-NOGLE IN CHKP-AREA-1
             ADD 1 TO ANTAL-LAES-FI01
           END-IF
           .
      *-----------------------------------------------------------------
       701-LAES-FI02 SECTION.
      *-----------------------------------------------------------------
      
           PERFORM INC-FI02-LAES
           IF BDC-FI02-OK
             MOVE BANKNR IN FI02-REC  TO BANKNR IN FI02-NOGLE
             MOVE REGNR  IN FI02-REC  TO REGNR  IN FI02-NOGLE
             MOVE KTONR  IN FI02-REC  TO KTONR  IN FI02-NOGLE
             MOVE DETAIL-RTENR IN FI02-REC
               TO DETAIL-RTENR IN FI02-NOGLE IN CHKP-AREA-1
             ADD 1 TO ANTAL-LAES-FI02
           END-IF
           .
      *----------------------------------------------------------------
       710-SKRIV-FO01 SECTION.
      *----------------------------------------------------------------
      
           PERFORM INC-FO01-SKRIV
      
           IF BDC-FO01-OK
             ADD 1 TO ANTAL-SKRIV-FO01
           END-IF
           .
      *----------------------------------------------------------------
       711-SKRIV-FO02 SECTION.
      *----------------------------------------------------------------
      
           PERFORM INC-FO02-SKRIV
      
           IF BDC-FO02-OK
             ADD 1 TO ANTAL-SKRIV-FO02
           END-IF
           .
      *----------------------------------------------------------------
       712-SKRIV-FO03 SECTION.
      *----------------------------------------------------------------
      
           PERFORM INC-FO03-SKRIV
      
           IF BDC-FO03-OK
             ADD 1 TO ANTAL-SKRIV-FO03
           END-IF
           .
      *-----------------------------------------------------------------
      * INCLUDER RUTINER TIL STANDARD DATO FEJLBEHANDLING
           COPY BDSDA2FK.
           COPY BDSDA23K.
      * INCLUDER RUTINER TIL STANDARD PARAMETER FEJLBEHANDLING
           COPY BDSPARMX.
      * INCLUDER RUTINER TIL STANDARD FEJLBEHANDLING
           COPY BDSIREST REPLACING ==002-AFSLUT==
                                BY ==005-AFSLUT-SYSTEMFEJL==.
      * INCLUDER RUTINER TIL BEHANDLING AF BDCOMMIT GENERELT
           COPY BDCOMMIK.
      * INCLUDER RUTINER TIL SPEEDOMETER
           COPY BDSM043K.
           COPY BDCSEQIK REPLACING ==:XXXX:== BY ==FI01==.
           COPY BDCSEQIK REPLACING ==:XXXX:== BY ==FI02==.
           COPY BDCSEQOK REPLACING ==:XXXX:== BY ==FO01==.
           COPY BDCSEQOK REPLACING ==:XXXX:== BY ==FO02==.
           COPY BDCSEQOK REPLACING ==:XXXX:== BY ==FO03==.
      