       ID DIVISION.
       PROGRAM-ID.         BDSDA2F.
       AUTHOR.             Rune Christensen.
       DATE-WRITTEN.       08.11.2013.
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
      *BSK> MODULETS FORMÅL/OPGAVE (KORT BESKREVET)  SKELET: BDSMXX1
      *BSK>-------------------------------------------------------------
      *BSK>
      *BSK> FUNKTIONSMODUL Danner pæne fejlbeskeder til bdsdato fejl.
      *BSK>
      *BSK> FUNKTION
      *BSK>
      *BSK> DAN-FEJL       Danner fejlbesked
      *BSK>
      *BSK>-------------------------------------------------------------
      *OFF> NEJ
      *BDS>-------------------------------------------------------------
      *BDS>
      *BDS> ANDRE TILRETTELSER I 003-BEHANDLING
      *BDS> OG I WORKING-STORAGE
      *BDS>      AUTHOR
      *BDS>      DATE-WRITTEN
      *BDS>
      *CHG>----------+------+-------------------------------------------
      *CHG>   DATO   ! INIT ! FORMÅL MED ÆNDRING
      *CHG>----------+------+-------------------------------------------
      *CHG>----------+------+-------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       DATA DIVISION.
       FILE SECTION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      
       01  FASTE-WORK-FELTER.
           COPY BDSIWKSD.
      
       01  INDIVIDUELLE-WORK-FELTER.
           03  WS-PROGRAMNAVN            PIC X(8) VALUE 'BDSDA2F'.
           03  WS-IX                     PIC S9(4) COMP.
           1  WS-BDSMFJL-I-PRG-POS      PIC  X(05) VALUE SPACES.
           2  WS-BDSMFJL-I-PRG-POS-2    PIC  X(05) VALUE SPACES.
           3  WS-BDSMFJL-I-PRG-POS-3    PIC  X(05) VALUE SPACES.
           4  WS-BDSMFJL-I-PRG-POS-4    PIC  X(05) VALUE SPACES.
      * Felter til lækker fejlhåndtering
           03  FILLER.
        05 BDSDFDT1-NUM                               PIC X(2).
        05 BDSDFDT1-9 REDEFINES BDSDFDT1-NUM          PIC 9(2).
        05 BDSDFDT2-NUM                               PIC X(2).
        05 BDSDFDT2-9 REDEFINES BDSDFDT2-NUM          PIC 9(2).
        05 BDSDFKL1-NUM                               PIC X(2).
        05 BDSDFKL1-9 REDEFINES BDSDFKL1-NUM          PIC 9(2).
        05 BDSDFKL2-NUM                               PIC X(2).
        05 BDSDFKL2-9 REDEFINES BDSDFKL2-NUM          PIC 9(2).
        05 ANTAL-DAGE-9                               PIC ZZZ.ZZZ.ZZ9-.
        05 BEREGNINGS-METODE-9                        PIC ZZZ.ZZZ.ZZ9-.
        05  DATO-FORMAT-00      PIC X(20) VALUE 'INTET-FORMAT-SAT    '.
        05 DATO-FORMAT-LISTE.
         07 DATO-FORMAT-01      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-02      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-03      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-04      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-05      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-06      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-07      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-08      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-09      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-10      PIC X(20) VALUE 'DD-MM-YY-19XX       '.
         07 DATO-FORMAT-11      PIC X(20) VALUE 'DD-MM-YY            '.
         07 DATO-FORMAT-12      PIC X(20) VALUE 'DD-MM-YYYY          '.
         07 DATO-FORMAT-13      PIC X(20) VALUE 'YY-MM-DD-19XX       '.
         07 DATO-FORMAT-14      PIC X(20) VALUE 'YY-MM-DD            '.
         07 DATO-FORMAT-15      PIC X(20) VALUE 'YYYY-MM-DD          '.
         07 DATO-FORMAT-16      PIC X(20) VALUE 'DB2-TIMESTAMP       '.
         07 DATO-FORMAT-17      PIC X(20) VALUE 'ALFA-TIMESTAMP      '.
         07 DATO-FORMAT-18      PIC X(20) VALUE 'ALFA2-TIMESTAMP     '.
         07 DATO-FORMAT-19      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-20      PIC X(20) VALUE 'VBS-D-DD-MNN-YYYY   '.
         07 DATO-FORMAT-21      PIC X(20) VALUE 'VBL-D-DD-MNN-YYYY   '.
         07 DATO-FORMAT-22      PIC X(20) VALUE 'VAS-D-DD-MNN-YYYY   '.
         07 DATO-FORMAT-23      PIC X(20) VALUE 'HBS-D-DD-MNN-YYYY   '.
         07 DATO-FORMAT-24      PIC X(20) VALUE 'HAS-D-DD-MNN-YYYY   '.
         07 DATO-FORMAT-25      PIC X(20) VALUE 'VBS-DNN-D-DD-MN-YYYY'.
         07 DATO-FORMAT-26      PIC X(20) VALUE 'VBL-DNN-D-DD-MN-YYYY'.
         07 DATO-FORMAT-27      PIC X(20) VALUE 'VAS-DNN-D-DD-MN-YYYY'.
         07 DATO-FORMAT-28      PIC X(20) VALUE 'HBS-DNN-D-DD-MN-YYYY'.
         07 DATO-FORMAT-29      PIC X(20) VALUE 'HAS-DNN-D-DD-MN-YYYY'.
         07 DATO-FORMAT-30      PIC X(20) VALUE 'DDDDD               '.
         07 DATO-FORMAT-31      PIC X(20) VALUE 'YYDDD-19XX          '.
         07 DATO-FORMAT-32      PIC X(20) VALUE 'YYDDD               '.
         07 DATO-FORMAT-33      PIC X(20) VALUE 'DDDDDDD             '.
         07 DATO-FORMAT-34      PIC X(20) VALUE 'YYYDDD              '.
         07 DATO-FORMAT-35      PIC X(20) VALUE 'YYYYDDD             '.
         07 DATO-FORMAT-36      PIC X(20) VALUE 'DDMMYY-NNNN-CPRNR   '.
         07 DATO-FORMAT-37      PIC X(20) VALUE 'YYMMDD-19XX         '.
         07 DATO-FORMAT-38      PIC X(20) VALUE 'YYMMDD              '.
         07 DATO-FORMAT-39      PIC X(20) VALUE 'YYYYMMDD            '.
         07 DATO-FORMAT-40      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-41      PIC X(20) VALUE 'DDMMYY-19XX         '.
         07 DATO-FORMAT-42      PIC X(20) VALUE 'DDMMYY              '.
         07 DATO-FORMAT-43      PIC X(20) VALUE 'DDMMYYYY            '.
         07 DATO-FORMAT-44      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-45      PIC X(20) VALUE 'YYUUD-19XX          '.
         07 DATO-FORMAT-46      PIC X(20) VALUE 'YYUUD               '.
         07 DATO-FORMAT-47      PIC X(20) VALUE 'YYYYUUD             '.
         07 DATO-FORMAT-48      PIC X(20) VALUE 'DUUYYYY             '.
         07 DATO-FORMAT-49      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-50      PIC X(20) VALUE 'P-DDDDD             '.
         07 DATO-FORMAT-51      PIC X(20) VALUE 'P-YYDDD-19XX        '.
         07 DATO-FORMAT-52      PIC X(20) VALUE 'P-YYDDD             '.
         07 DATO-FORMAT-53      PIC X(20) VALUE 'P-DDDDDDD           '.
         07 DATO-FORMAT-54      PIC X(20) VALUE 'P-YYYDDD            '.
         07 DATO-FORMAT-55      PIC X(20) VALUE 'P-YYYYDDD           '.
         07 DATO-FORMAT-56      PIC X(20) VALUE 'P-DDMMYY-NNNN-CPRNR '.
         07 DATO-FORMAT-57      PIC X(20) VALUE 'P-YYMMDD-19XX       '.
         07 DATO-FORMAT-58      PIC X(20) VALUE 'P-YYMMDD            '.
         07 DATO-FORMAT-59      PIC X(20) VALUE 'P-YYYYMMDD          '.
         07 DATO-FORMAT-60      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-61      PIC X(20) VALUE 'P-DDMMYY-19XX       '.
         07 DATO-FORMAT-62      PIC X(20) VALUE 'P-DDMMYY            '.
         07 DATO-FORMAT-63      PIC X(20) VALUE 'P-DDMMYYYY          '.
         07 DATO-FORMAT-64      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-65      PIC X(20) VALUE 'P-YYUUD-19XX        '.
         07 DATO-FORMAT-66      PIC X(20) VALUE 'P-YYUUD             '.
         07 DATO-FORMAT-67      PIC X(20) VALUE 'P-YYYYUUD           '.
         07 DATO-FORMAT-68      PIC X(20) VALUE 'P-DUUYYYY           '.
         07 DATO-FORMAT-69      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-70      PIC X(20) VALUE 'STCK-TIMESTAMP      '.
         07 DATO-FORMAT-71      PIC X(20) VALUE 'F-YYDDD-19XX        '.
         07 DATO-FORMAT-72      PIC X(20) VALUE 'F-YYDDD             '.
         07 DATO-FORMAT-73      PIC X(20) VALUE 'F-DDDDDDD           '.
         07 DATO-FORMAT-74      PIC X(20) VALUE 'F-YYYDDD            '.
         07 DATO-FORMAT-75      PIC X(20) VALUE 'F-YYYYDDD           '.
         07 DATO-FORMAT-76      PIC X(20) VALUE 'F-DDMMYY-NNNN-CPRNR '.
         07 DATO-FORMAT-77      PIC X(20) VALUE 'F-YYMMDD-19XX       '.
         07 DATO-FORMAT-78      PIC X(20) VALUE 'F-YYMMDD            '.
         07 DATO-FORMAT-79      PIC X(20) VALUE 'F-YYYYMMDD          '.
         07 DATO-FORMAT-80      PIC X(20) VALUE 'F-DDDDD             '.
         07 DATO-FORMAT-81      PIC X(20) VALUE 'F-DDMMYY-19XX       '.
         07 DATO-FORMAT-82      PIC X(20) VALUE 'F-DDMMYY            '.
         07 DATO-FORMAT-83      PIC X(20) VALUE 'F-DDMMYYYY          '.
         07 DATO-FORMAT-84      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-85      PIC X(20) VALUE 'F-YYUUD-19XX        '.
         07 DATO-FORMAT-86      PIC X(20) VALUE 'F-YYUUD             '.
         07 DATO-FORMAT-87      PIC X(20) VALUE 'F-YYYYUUD           '.
         07 DATO-FORMAT-88      PIC X(20) VALUE 'F-DUUYYYY           '.
         07 DATO-FORMAT-89      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-90      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-91      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-92      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-93      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-94      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-95      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-96      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-97      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-98      PIC X(20) VALUE 'UKENDT              '.
         07 DATO-FORMAT-99      PIC X(20) VALUE 'UKENDT              '.
        05 DATO-FORMAT-OCCURS REDEFINES DATO-FORMAT-LISTE
                       PIC X(20) OCCURS 99 TIMES.
        05  DATO-FORMAT-T-00    PIC 9(2) VALUE 16.
        05 DATO-FORMAT-T-LEN.
         07 DATO-FORMAT-T-01    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-02    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-03    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-04    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-05    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-06    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-07    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-08    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-09    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-10    PIC 9(2) VALUE 13.
         07 DATO-FORMAT-T-11    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-12    PIC 9(2) VALUE 10.
         07 DATO-FORMAT-T-13    PIC 9(2) VALUE 13.
         07 DATO-FORMAT-T-14    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-15    PIC 9(2) VALUE 10.
         07 DATO-FORMAT-T-16    PIC 9(2) VALUE 13.
         07 DATO-FORMAT-T-17    PIC 9(2) VALUE 14.
         07 DATO-FORMAT-T-18    PIC 9(2) VALUE 15.
         07 DATO-FORMAT-T-19    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-20    PIC 9(2) VALUE 17.
         07 DATO-FORMAT-T-21    PIC 9(2) VALUE 17.
         07 DATO-FORMAT-T-22    PIC 9(2) VALUE 17.
         07 DATO-FORMAT-T-23    PIC 9(2) VALUE 17.
         07 DATO-FORMAT-T-24    PIC 9(2) VALUE 17.
         07 DATO-FORMAT-T-25    PIC 9(2) VALUE 20.
         07 DATO-FORMAT-T-26    PIC 9(2) VALUE 20.
         07 DATO-FORMAT-T-27    PIC 9(2) VALUE 20.
         07 DATO-FORMAT-T-28    PIC 9(2) VALUE 20.
         07 DATO-FORMAT-T-29    PIC 9(2) VALUE 20.
         07 DATO-FORMAT-T-30    PIC 9(2) VALUE 5.
         07 DATO-FORMAT-T-31    PIC 9(2) VALUE 10.
         07 DATO-FORMAT-T-32    PIC 9(2) VALUE 5.
         07 DATO-FORMAT-T-33    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-34    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-35    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-36    PIC 9(2) VALUE 17.
         07 DATO-FORMAT-T-37    PIC 9(2) VALUE 11.
         07 DATO-FORMAT-T-38    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-39    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-40    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-41    PIC 9(2) VALUE 11.
         07 DATO-FORMAT-T-42    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-43    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-44    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-45    PIC 9(2) VALUE 10.
         07 DATO-FORMAT-T-46    PIC 9(2) VALUE 5.
         07 DATO-FORMAT-T-47    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-48    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-49    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-50    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-51    PIC 9(2) VALUE 12.
         07 DATO-FORMAT-T-52    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-53    PIC 9(2) VALUE 9.
         07 DATO-FORMAT-T-54    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-55    PIC 9(2) VALUE 9.
         07 DATO-FORMAT-T-56    PIC 9(2) VALUE 19.
         07 DATO-FORMAT-T-57    PIC 9(2) VALUE 13.
         07 DATO-FORMAT-T-58    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-59    PIC 9(2) VALUE 10.
         07 DATO-FORMAT-T-60    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-61    PIC 9(2) VALUE 13.
         07 DATO-FORMAT-T-62    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-63    PIC 9(2) VALUE 10.
         07 DATO-FORMAT-T-64    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-65    PIC 9(2) VALUE 12.
         07 DATO-FORMAT-T-66    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-67    PIC 9(2) VALUE 9.
         07 DATO-FORMAT-T-68    PIC 9(2) VALUE 9.
         07 DATO-FORMAT-T-69    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-70    PIC 9(2) VALUE 14.
         07 DATO-FORMAT-T-71    PIC 9(2) VALUE 12.
         07 DATO-FORMAT-T-72    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-73    PIC 9(2) VALUE 9.
         07 DATO-FORMAT-T-74    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-75    PIC 9(2) VALUE 9.
         07 DATO-FORMAT-T-76    PIC 9(2) VALUE 19.
         07 DATO-FORMAT-T-77    PIC 9(2) VALUE 13.
         07 DATO-FORMAT-T-78    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-79    PIC 9(2) VALUE 10.
         07 DATO-FORMAT-T-80    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-81    PIC 9(2) VALUE 13.
         07 DATO-FORMAT-T-82    PIC 9(2) VALUE 8.
         07 DATO-FORMAT-T-83    PIC 9(2) VALUE 10.
         07 DATO-FORMAT-T-84    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-85    PIC 9(2) VALUE 12.
         07 DATO-FORMAT-T-86    PIC 9(2) VALUE 7.
         07 DATO-FORMAT-T-87    PIC 9(2) VALUE 9.
         07 DATO-FORMAT-T-88    PIC 9(2) VALUE 9.
         07 DATO-FORMAT-T-89    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-90    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-91    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-92    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-93    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-94    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-95    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-96    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-97    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-98    PIC 9(2) VALUE 6.
         07 DATO-FORMAT-T-99    PIC 9(2) VALUE 6.
        05 DATO-FORMAT-O-T-LEN REDEFINES DATO-FORMAT-T-LEN
                       PIC 9(2) OCCURS 99 TIMES.
        05  TID-FORMAT-00       PIC X(15) VALUE 'INTET-FORMAT   '.
        05 TID-FORMAT-LISTE.
         07 TID-FORMAT-01       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-02       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-03       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-04       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-05       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-06       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-07       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-08       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-09       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-10       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-11       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-12       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-13       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-14       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-15       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-16       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-17       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-18       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-19       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-20       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-21       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-22       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-23       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-24       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-25       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-26       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-27       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-28       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-29       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-30       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-31       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-32       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-33       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-34       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-35       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-36       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-37       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-38       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-39       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-40       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-41       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-42       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-43       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-44       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-45       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-46       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-47       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-48       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-49       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-50       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-51       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-52       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-53       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-54       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-55       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-56       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-57       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-58       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-59       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-60       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-61       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-62       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-63       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-64       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-65       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-66       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-67       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-68       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-69       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-70       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-71       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-72       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-73       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-74       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-75       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-76       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-77       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-78       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-79       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-80       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-81       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-82       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-83       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-84       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-85       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-86       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-87       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-88       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-89       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-90       PIC X(15) VALUE 'HH-MM          '.
         07 TID-FORMAT-91       PIC X(15) VALUE 'HH-MM-SS       '.
         07 TID-FORMAT-92       PIC X(15) VALUE 'HH-MM-SS-T     '.
         07 TID-FORMAT-93       PIC X(15) VALUE 'HH-MM-SS-UUUUUU'.
         07 TID-FORMAT-94       PIC X(15) VALUE 'UKENDT         '.
         07 TID-FORMAT-95       PIC X(15) VALUE 'HHMM           '.
         07 TID-FORMAT-96       PIC X(15) VALUE 'HHMMSST        '.
         07 TID-FORMAT-97       PIC X(15) VALUE 'SSSSSHH        '.
         07 TID-FORMAT-98       PIC X(15) VALUE 'SSSSS          '.
         07 TID-FORMAT-99       PIC X(15) VALUE 'SSSSSUUUUUU    '.
        05 TID-FORMAT-OCCURS REDEFINES TID-FORMAT-LISTE
                       PIC X(15) OCCURS 99 TIMES.
        05  TID-FORMAT-T-00     PIC 9(2) VALUE 12.
        05 TID-FORMAT-T-LEN.
         07 TID-FORMAT-T-01     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-02     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-03     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-04     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-05     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-06     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-07     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-08     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-09     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-10     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-11     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-12     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-13     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-14     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-15     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-16     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-17     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-18     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-19     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-20     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-21     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-22     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-23     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-24     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-25     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-26     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-27     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-28     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-29     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-30     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-31     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-32     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-33     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-34     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-35     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-36     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-37     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-38     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-39     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-40     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-41     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-42     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-43     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-44     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-45     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-46     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-47     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-48     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-49     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-50     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-51     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-52     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-53     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-54     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-55     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-56     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-57     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-58     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-59     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-60     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-61     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-62     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-63     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-64     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-65     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-66     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-67     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-68     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-69     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-70     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-71     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-72     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-73     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-74     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-75     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-76     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-77     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-78     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-79     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-80     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-81     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-82     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-83     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-84     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-85     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-86     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-87     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-88     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-89     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-90     PIC 9(2) VALUE 5.
         07 TID-FORMAT-T-91     PIC 9(2) VALUE 8.
         07 TID-FORMAT-T-92     PIC 9(2) VALUE 10.
         07 TID-FORMAT-T-93     PIC 9(2) VALUE 15.
         07 TID-FORMAT-T-94     PIC 9(2) VALUE 6.
         07 TID-FORMAT-T-95     PIC 9(2) VALUE 4.
         07 TID-FORMAT-T-96     PIC 9(2) VALUE 7.
         07 TID-FORMAT-T-97     PIC 9(2) VALUE 7.
         07 TID-FORMAT-T-98     PIC 9(2) VALUE 5.
         07 TID-FORMAT-T-99     PIC 9(2) VALUE 11.
        05 TID-FORMAT-O-T-LEN REDEFINES TID-FORMAT-T-LEN
                       PIC 9(2) OCCURS 99 TIMES.
      
      *-----------------------------------------------------------------
      * INCLUDE AREALER
      *-----------------------------------------------------------------
      
           EXEC SQL INCLUDE SQLCA END-EXEC.
           COPY DB2FEJLI.
      
           REPLACE ==:BDSIXXX:== BY ==BDSDA2F==.
      
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------
      
       01  BDSMFJL-PARM.
           COPY BDSMFJLI.
      
       01  BDSDATO-PARM.
           COPY BDSDATOI.
      
      *-----------------------------------------------------------------
       PROCEDURE DIVISION USING BDSDATO-PARM BDSMFJL-PARM.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------
       000-START SECTION.
      *-----------------------------------------------------------------
      
           PERFORM 001-INIT
      
           PERFORM 003-BEHANDLING
      
           PERFORM 002-AFSLUT
           .
      *-----------------------------------------------------------------
       001-INIT SECTION.
      *-----------------------------------------------------------------
      
           MOVE BDSMFJL-I-PRG-POS   TO WS-BDSMFJL-I-PRG-POS
           MOVE BDSMFJL-I-PRG-POS-1 TO WS-BDSMFJL-I-PRG-POS-2
           MOVE BDSMFJL-I-PRG-POS-1 TO WS-BDSMFJL-I-PRG-POS-3
           MOVE BDSMFJL-I-PRG-POS-1 TO WS-BDSMFJL-I-PRG-POS-4
      
           INITIALIZE BDSMFJL-PARM
      
           MOVE WS-BDSMFJL-I-PRG-POS   TO BDSMFJL-I-PRG-POS
           MOVE WS-BDSMFJL-I-PRG-POS-1 TO BDSMFJL-I-PRG-POS-2
           MOVE WS-BDSMFJL-I-PRG-POS-1 TO BDSMFJL-I-PRG-POS-3
           MOVE WS-BDSMFJL-I-PRG-POS-1 TO BDSMFJL-I-PRG-POS-4
           .
      *-----------------------------------------------------------------
       002-AFSLUT SECTION.
      *-----------------------------------------------------------------
      
           GOBACK
           .
      *-----------------------------------------------------------------
       003-BEHANDLING SECTION.
      *-----------------------------------------------------------------
      
           PERFORM 030-FEJLMELD-PARAMETRE
           PERFORM 120-FEJLMELD-FEJLBESKEDER
           EVALUATE TRUE
           WHEN BDSDATO-KLOKKEN IN BDSDATO-PARM
              PERFORM 040-FEJLMELD-FORMAT
              MOVE BDSDFKL1     IN BDSDATO-PARM
                TO BDSDFKL1-NUM
              IF BDSDFKL1-9 IS NUMERIC
                STRING 'TID-FORMAT: ' DELIMITED BY SPACE
                        TID-FORMAT-OCCURS
                       (BDSDFKL1-9)
                 DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR3
              ELSE
                STRING 'TID-FORMAT: ' DELIMITED BY SPACE
                        BDSDFKL1-NUM
                       ' ER IKKE NUMERISK'
                 DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR3
              END-IF
           WHEN BDSDATO-DATO IN BDSDATO-PARM
           WHEN BDSDATO-KDATO IN BDSDATO-PARM
              PERFORM 040-FEJLMELD-FORMAT
           WHEN BDSDATO-KONTROL IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
           WHEN BDSDATO-SKONTROL IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
              PERFORM 090-LANDEKODE
           WHEN BDSDATO-FORMAT IN BDSDATO-PARM
           WHEN BDSDATO-KONTROLF IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-UFB IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-UFA IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-USB IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-USA IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-MFB IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-MFA IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-MSB IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-MSA IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-YFB IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-YFA IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-YSB IN BDSDATO-PARM
           WHEN BDSDATO-BEREGN-YSA IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
              PERFORM 060-FEJLMELD-FORMAT2
           WHEN BDSDATO-TIDER IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
              MOVE BDSDFDT2     IN BDSDATO-PARM
                TO BDSDFDT2-NUM
              MOVE BDSDFKL1     IN BDSDATO-PARM
                TO BDSDFKL1-NUM
              MOVE BDSDFKL2     IN BDSDATO-PARM
                TO BDSDFKL2-NUM
              STRING 'TID:' DELIMITED BY SIZE
                      TID-FORMAT-OCCURS
                     (BDSDFKL1-9) DELIMITED BY SPACE
                     ' ' DELIMITED BY SIZE
                      PARM-TID IN BDSDATO-PARM
                     (1:TID-FORMAT-O-T-LEN(BDSDFKL1-9))
               DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR3
              END-STRING
              STRING 'OUTPUT DATO: ' DELIMITED BY SIZE
                      DATO-FORMAT-OCCURS
                     (BDSDFDT2-9) DELIMITED BY SPACE
                    ' TID: ' DELIMITED BY SIZE
                      TID-FORMAT-OCCURS
                     (BDSDFKL2-9)
               DELIMITED BY SPACE INTO BDSMFJL-I-CALL-VAR4
              END-STRING
           WHEN BDSDATO-ADATO IN BDSDATO-PARM
           WHEN BDSDATO-BDATOP IN BDSDATO-PARM
           WHEN BDSDATO-BDATOM IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
              PERFORM 060-FEJLMELD-FORMAT2
              PERFORM 080-FEJLMELD-ANTAL-DAGE
           WHEN BDSDATO-SDATOP IN BDSDATO-PARM
           WHEN BDSDATO-SDATOM IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
              PERFORM 060-FEJLMELD-FORMAT2
              PERFORM 100-FEJLMELD-ANTAL-LAND
           WHEN BDSDATO-ADAGE IN BDSDATO-PARM
           WHEN BDSDATO-ALDER IN BDSDATO-PARM
           WHEN BDSDATO-BDAGE IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
              PERFORM 070-FEJLMELD-FORMAT2-DATO2
           WHEN BDSDATO-SDAGE IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
              PERFORM 070-FEJLMELD-FORMAT2-DATO2
              PERFORM 090-LANDEKODE
           WHEN BDSDATO-RENTEDT IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
              PERFORM 060-FEJLMELD-FORMAT2
              MOVE ANTAL-DAGE   IN BDSDATO-PARM
                TO ANTAL-DAGE-9
              MOVE BEREGNINGS-METODE IN BDSDATO-PARM
                TO BEREGNINGS-METODE-9
              STRING 'ANTAL-DAGE: '
                      ANTAL-DAGE-9
                    ' BEREGNINGS-METODE: '
                      BEREGNINGS-METODE-9
                DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR4
              END-STRING
           WHEN BDSDATO-RENTEDG IN BDSDATO-PARM
              PERFORM 050-FEJLMELD-FORMAT-DATO
              PERFORM 070-FEJLMELD-FORMAT2-DATO2
              MOVE BEREGNINGS-METODE IN BDSDATO-PARM
                TO BEREGNINGS-METODE-9
              STRING 'BEREGNINGS-METODE: '
                      BEREGNINGS-METODE-9
                DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR4
              END-STRING
           END-EVALUATE
           .
      *-----------------------------------------------------------------
       030-FEJLMELD-PARAMETRE SECTION.
      *-----------------------------------------------------------------
           STRING 'PARAMETRE: '
                   BDSDATOC IN BDSDATO-PARM
             DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR1
           END-STRING
           .
      *-----------------------------------------------------------------
       040-FEJLMELD-FORMAT SECTION.
      *-----------------------------------------------------------------
           MOVE BDSDFDT1     IN BDSDATO-PARM
             TO BDSDFDT1-NUM
           IF BDSDFDT1-9 IS NUMERIC
             STRING 'INPUT: DATO-FORMAT: '        DELIMITED BY SIZE
                     DATO-FORMAT-OCCURS
                     (BDSDFDT1-9)
              DELIMITED BY SPACE INTO BDSMFJL-I-CALL-VAR2
             END-STRING
           ELSE
             STRING 'INPUT: DATO-FORMAT: '        DELIMITED BY SIZE
                     BDSDFDT1-NUM
                    ' ER IKKE NUMERISK'
              DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR2
             END-STRING
           END-IF
           .
      *-----------------------------------------------------------------
       050-FEJLMELD-FORMAT-DATO SECTION.
      *-----------------------------------------------------------------
           MOVE BDSDFDT1     IN BDSDATO-PARM
             TO BDSDFDT1-NUM
           STRING 'INPUT: DATO-FORMAT: '        DELIMITED BY SIZE
                   DATO-FORMAT-OCCURS
                   (BDSDFDT1-9) DELIMITED BY SPACE
                 ' DATO: '
                   PARM-DATO IN BDSDATO-PARM
                  (1:DATO-FORMAT-O-T-LEN(BDSDFDT1-9))
             DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR2
           END-STRING
           .
      *-----------------------------------------------------------------
       060-FEJLMELD-FORMAT2 SECTION.
      *-----------------------------------------------------------------
           MOVE BDSDFDT2     IN BDSDATO-PARM
             TO BDSDFDT2-NUM
           IF BDSDFDT2-9 IS NUMERIC
             STRING 'OUTPUT: DATO-FORMAT: '        DELIMITED BY SIZE
                     DATO-FORMAT-OCCURS
                     (BDSDFDT2-9)
              DELIMITED BY SPACE INTO BDSMFJL-I-CALL-VAR3
             END-STRING
           ELSE
             STRING 'OUTPUT: DATO-FORMAT: '        DELIMITED BY SIZE
                     BDSDFDT2-NUM
                    ' ER IKKE NUMERISK'
              DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR3
             END-STRING
           END-IF
           .
      *-----------------------------------------------------------------
       070-FEJLMELD-FORMAT2-DATO2 SECTION.
      *-----------------------------------------------------------------
           MOVE BDSDFDT2     IN BDSDATO-PARM
             TO BDSDFDT2-NUM
           STRING 'DATO2: FORMAT: '        DELIMITED BY SIZE
                   DATO-FORMAT-OCCURS
                   (BDSDFDT2-9) DELIMITED BY SPACE
                 ' DATO ' DELIMITED BY SIZE
                   PARM-DATO2 IN BDSDATO-PARM
                   (1:DATO-FORMAT-O-T-LEN(BDSDFDT2-9))
             DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR3
           END-STRING
           .
      *-----------------------------------------------------------------
       080-FEJLMELD-ANTAL-DAGE SECTION.
      *-----------------------------------------------------------------
           MOVE ANTAL-DAGE   IN BDSDATO-PARM
             TO ANTAL-DAGE-9
           STRING 'ANTAL-DAGE: '
                   ANTAL-DAGE-9
             DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR4
           END-STRING
           .
      *-----------------------------------------------------------------
       090-LANDEKODE SECTION.
      *-----------------------------------------------------------------
           STRING 'LANDEKODE: '
                   BDSDSKAL(6:2)
             DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR4
           END-STRING
           .
      *-----------------------------------------------------------------
       100-FEJLMELD-ANTAL-LAND SECTION.
      *-----------------------------------------------------------------
           MOVE ANTAL-DAGE   IN BDSDATO-PARM
             TO ANTAL-DAGE-9
           STRING 'LANDEKODE: '
                   BDSDSKAL(6:2)
                 ' ANTAL-DAGE: '
                   ANTAL-DAGE-9
             DELIMITED BY SIZE INTO BDSMFJL-I-CALL-VAR4
           END-STRING
           .
      *-----------------------------------------------------------------
       120-FEJLMELD-FEJLBESKEDER SECTION.
      *-----------------------------------------------------------------
      
      *    Udfylder fejlbeskeder i de første call-medd
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 4
                OR BDSDATO-MEDD(WS-IX) NOT = SPACES
           END-PERFORM
           IF WS-IX <= 4
             MOVE BDSDATO-MEDD(WS-IX) TO BDSMFJL-I-CALL-MEDD1
             ADD 1 TO WS-IX
             PERFORM VARYING WS-IX FROM WS-IX BY 1 UNTIL WS-IX > 4
                  OR BDSDATO-MEDD(WS-IX) NOT = SPACES
             END-PERFORM
             IF WS-IX <= 4
               MOVE BDSDATO-MEDD(WS-IX) TO BDSMFJL-I-CALL-MEDD2
               ADD 1 TO WS-IX
               PERFORM VARYING WS-IX FROM WS-IX BY 1 UNTIL WS-IX > 4
                    OR BDSDATO-MEDD(WS-IX) NOT = SPACES
               END-PERFORM
               IF WS-IX <= 4
                 MOVE BDSDATO-MEDD(WS-IX) TO BDSMFJL-I-CALL-MEDD3
                 IF WS-IX = 3
                 AND BDSDATO-MEDD(4) NOT = SPACES
                   MOVE BDSDATO-MEDD(4) TO BDSMFJL-I-CALL-MEDD4
                 END-IF
               END-IF
             END-IF
           END-IF
           .
      