      ******************************************************************18.11.19
      *                      DESCRIPCION DEL PROGRAMA                  *18.11.19
      *                      ------------------------                  *18.11.19
      *        AUTOR       : MNEMO                                     *18.11.19
      *        FECHA       : 31/01/2012                                *18.11.19
      *        ENTORNO     : KGPE                                      *18.11.19
      *        LENGUAJE    : ENTERPRISE COBOL                          *18.11.19
      *        COMENTARIOS : PERMITE CONSULTA DE LIMITES Y DISPONIBLES *18.11.19
      *                      QUE TIENE UN CLIENTE                      *18.11.19
      ******************************************************************18.11.19
      *    L-0001    USUARIO:MNEMO  FECHA: 20-03-2012                  *        
      *    DESCRIPCION: Modificaciones y mejoras del programa          *        
      ******************************************************************        
      *   L-0002    USUARIO:MNEMO  FECHA: 19-04-2012                   *        
      *   DESCRIPCION: AÑADIR EL CAMPO WS-ATRIBOPR-AUX Y SU TRATAMIENTO*        
      ******************************************************************        
      *   L-0003    USUARIO:E043682  FECHA: 17-08-2015                 *        
      *   DESCRIPCION: SI EL CANAL NO VIENE INFORMADO SE INFORMA EL DE *        
      *                LA CABECERA                                              
      ******************************************************************        
      *   L-0004    USUARIO:XE78969  FECHA: 26-10-2018                 *        
      *   DESCRIPCION: MEJORA DE PROGRAMA. CAMBIO CTN-08 POR CTN-12 EN *        
      *                CONSULTA LIMITES                                *        
      ******************************************************************        
      *                      IDENTIFICATION DIVISION                   *12.04.19
      ******************************************************************18.11.19
       IDENTIFICATION DIVISION.                                         18.11.19
      *                                                                 18.11.19
       PROGRAM-ID.    KYGHG003.                                         18.11.19
       AUTHOR.        MNEMO.                                            18.11.19
       DATE-WRITTEN.  31-01-2012.                                       18.11.19
      *                                                                 18.11.19
      ******************************************************************18.11.19
      *                      ENVIRONMENT DIVISION                      *18.11.19
      ******************************************************************18.11.19
       ENVIRONMENT DIVISION.                                            18.11.19
      *                                                                 18.11.19
       CONFIGURATION SECTION.                                           18.11.19
      *-----------------------------------------------------------------18.11.19
       SOURCE-COMPUTER.      IBM-3090.                                  18.11.19
       OBJECT-COMPUTER.      IBM-3090.                                  18.11.19
       SPECIAL-NAMES.        DECIMAL-POINT   IS COMMA.                  18.11.19
      *                                                                 18.11.19
      ******************************************************************18.11.19
      *                      DATA DIVISION                             *18.11.19
      ******************************************************************18.11.19
       DATA DIVISION.                                                   18.11.19
      *                                                                 18.11.19
      ******************************************************************18.11.19
      *                      WORKING-STORAGE SECTION                   *18.11.19
      ******************************************************************18.11.19
       WORKING-STORAGE  SECTION.                                        18.11.19
      *INC AUDIT. NO BORRAR DESDE AQUI HASTA FIN INCLUDE                        
       77 AUDIT-TRAIL PIC X(60) VALUE                                           
           '[** AUDIT ** KYGHG003-XE78969-181119-113903-             ]'.        
      *FIN INC AUDIT.                                                           
      *                                                                 18.11.19
      ******************************************************************18.11.19
      *                      CONSTANTES                                *18.11.19
      ******************************************************************18.11.19
       01  CTA-CONSTANTES.                                              18.11.19
           05  CTA-PROGRAMA                PIC X(08)   VALUE 'KYGHG003'.20.10.28
           05  CTA-KYGHR003                PIC X(08)   VALUE 'KYGHR003'.20.10.28
           05  CTA-PARR-120000             PIC X(08)   VALUE '120000- '.18.11.19
           05  CTA-SEP                     PIC X(01)   VALUE '#'.       20.10.28
           05  CTA-I                       PIC X(01)   VALUE 'I'.       18.11.19
           05  CTA-E                       PIC X(01)   VALUE 'E'.       20.10.28
           05  CTA-V                       PIC X(01)   VALUE 'V'.       18.11.19
           05  CTA-COD-PAISOALF-E          PIC X(14)   VALUE            18.11.19
               'COD-PAISOALF-E'.                                        18.11.19
           05  CTA-COD-ENTALFA-E           PIC X(14)   VALUE            18.11.19
               'COD-ENTALFA-E'.                                         18.11.19
           05  CTA-COD-PERSCTPN-E          PIC X(14)   VALUE            18.11.19
               'COD-PERSCTPN-E'.                                        18.11.19
           05  CTA-COD-OPERACN-E           PIC X(13)   VALUE            18.11.19
               'COD-OPERACN-E'.                                         18.11.19
      *L-0001-INI                                                       18.11.19
           05  CTA-COD-CANAL-DV-E          PIC X(14)   VALUE            18.11.19
               'COD-CANAL-DV-E'.                                        20.10.28
           05  CTN-04                      PIC 9(02)   VALUE 04.        18.11.19
           05  CTN-12                      PIC 9(02)   VALUE 12.        18.11.19
           05  CTN-08                      PIC 9(02)   VALUE 08.        20.10.28
           05  CTN-ERR-22003               PIC 9(05)   VALUE 22003.     20.10.28
      *                                                                 18.11.19
      ******************************************************************18.11.19
      *                      VARIABLES                                 *18.11.19
      ******************************************************************18.11.19
       01  VARIABLES.                                                   18.11.19
           05  WS-COD-AVIERROR             PIC 9(08).                   18.11.19
           05  WS-SQLCA-ERR-QPEJCAQA       PIC X(150).                  20.10.28
      ******************************************************************18.11.19
      *                      AREA DE COPYS                             *18.11.19
      ******************************************************************18.11.19
      *                                                                 18.11.19
       COPY QPIPCCAB.                                                   20.10.28
       COPY KYGHCA03.                                                   20.10.28
                                                                        20.10.28
      ******************************************************************18.11.19
      *                      LINKAGE SECTION                           *18.11.19
      ******************************************************************18.11.19
       LINKAGE SECTION.                                                 18.11.19
      *                                                                 18.11.19
       COPY QPEJCAQA.                                                   18.11.19
      *                                                                 18.11.19
       COPY KYGHC003.                                                   18.11.19
      *                                                                 18.11.19
      ******************************************************************18.11.19
      *                      PROCEDURE DIVISION                        *18.11.19
      ******************************************************************18.11.19
       PROCEDURE DIVISION USING R-QPEJCAQA                              18.11.19
                                KYGHT003-01-01-01-E                     18.11.19
                                KYGHT003-01-01-01-S.                    18.11.19
      *                                                                 18.11.19
           PERFORM 100000-INICIO                                        18.11.19
      *                                                                 18.11.19
           PERFORM 200000-PROCESO                                       18.11.19
      *                                                                 18.11.19
           PERFORM 300000-FIN.                                          18.11.19
      *                                                                 18.11.19
      ******************************************************************18.11.19
      * INICIO                                                         *18.11.19
      * SE INICIALIZAN LAS VARIABLES DE TRABAJO Y SE VALIDA QUE LOS    *18.11.19
      * CAMPOS DE ENTRADA ESTEN INFORMADOS.                            *18.11.19
      ******************************************************************18.11.19
       100000-INICIO.                                                   18.11.19
      *                                                                 18.11.19
           INITIALIZE RETORNO-QPIPCCAB                                  18.11.19
                      RETORNOS-APLICACION-QPEJCAQA                      18.11.19
                      VARIABLES                                         20.10.28
      *L-0002-INI                                                       18.11.19
           MOVE LOW-VALUES                 TO KYGHT003-01-01-01-S       18.11.19
      *L-0002-FIN                                                       20.10.29
      *                                                                 18.11.19
           PERFORM 110000-INFORMAR-CONTEXTO                             18.11.19
           PERFORM 120000-VALIDACION-DATOS.                             18.11.19
      *                                                                 18.11.19
      ******************************************************************18.11.19
      * INFORMAR CONTEXTO                                              *18.11.19
      * SE INFORMAN LOS DATOS DEL CONTEXTO.                            *18.11.19
      ******************************************************************18.11.19
       110000-INFORMAR-CONTEXTO.                                        18.11.19
      *                                                                 18.11.19
           MOVE COD-PAIS-QPEJCAQA          TO COD-PAIS-QPIPCCAB         18.11.19
           MOVE COD-BANCO-QPEJCAQA         TO COD-BANCO-QPIPCCAB        18.11.19
           MOVE COD-OFICINA-QPEJCAQA       TO COD-OFICINA-QPIPCCAB      18.11.19
           MOVE COD-PUESTO-QPEJCAQA        TO COD-PUESTO-QPIPCCAB       18.11.19
           MOVE COD-CANAL-QPEJCAQA         TO COD-CANAL-QPIPCCAB        18.11.19
           MOVE COD-MEDIO-QPEJCAQA         TO COD-MEDIO-QPIPCCAB        18.11.19
           MOVE COD-APLCANAL-QPEJCAQA      TO COD-APLCANAL-QPIPCCAB     18.11.19
           MOVE COD-IDIOMA-QPEJCAQA        TO COD-IDIOMA-QPIPCCAB       18.11.19
           MOVE COD-USUARIO-QPEJCAQA       TO COD-USUARIO-QPIPCCAB      18.11.19
           MOVE FEC-PROCESO-QPEJCAQA       TO FEC-PROCESO-QPIPCCAB      18.11.19
           MOVE COD-BANCO-OPER-QPEJCAQA    TO COD-BANCO-OPER-QPIPCCAB   18.11.19
           MOVE COD-OFICINA-OPER-QPEJCAQA  TO COD-OFICINA-OPER-QPIPCCAB 18.11.19
           MOVE CTA-PROGRAMA               TO COD-PROGRAMA-QPIPCCAB.    18.11.19
      *                                                                 18.11.19
      ******************************************************************18.11.19
      * VALIDACION DATOS                                               *18.11.19
      * SE REALIZA LA VALIDACION DE DATOS DE ENTRADA.                  *18.11.19
      ******************************************************************18.11.19
       120000-VALIDACION-DATOS.                                         18.11.19
      *                                                                 18.11.19
           IF  C003-COD-PAISOALF-E EQUAL SPACES OR                      18.11.19
               C003-COD-PAISOALF-E EQUAL LOW-VALUES OR                  18.11.19
               C003-COD-PAISOALF-E EQUAL HIGH-VALUES                    18.11.19
      *                                                                 18.11.19
               STRING CTA-COD-PAISOALF-E                                18.11.19
                      C003-COD-PAISOALF-E                               18.11.19
                      CTA-SEP                                           18.11.19
                      KYGHT003-01-01-01-E                               18.11.19
                  DELIMITED BY SIZE                                     18.11.19
                  INTO WS-SQLCA-ERR-QPEJCAQA                            18.11.19
                                                                        18.11.19
               END-STRING                                               18.11.19
      *                                                                 18.11.19
               MOVE CTN-ERR-22003          TO WS-COD-AVIERROR           18.11.19
      *                                                                 18.11.19
               PERFORM 900000-INFORMA-ERR-VALIDACION                    18.11.19
           END-IF                                                       18.11.19
      *                                                                 18.11.19
           IF  C003-COD-ENTALFA-E EQUAL SPACES OR                       18.11.19
               C003-COD-ENTALFA-E EQUAL LOW-VALUES OR                   18.11.19
               C003-COD-ENTALFA-E EQUAL HIGH-VALUES                     18.11.19
      *                                                                 18.11.19
               STRING CTA-COD-ENTALFA-E                                 18.11.19
                      C003-COD-ENTALFA-E                                18.11.19
                      CTA-SEP                                           18.11.19
                      KYGHT003-01-01-01-E                               18.11.19
                  DELIMITED BY SIZE                                     18.11.19
                  INTO WS-SQLCA-ERR-QPEJCAQA                            18.11.19
                                                                        18.11.19
               END-STRING                                               18.11.19
      *                                                                 18.11.19
               MOVE CTN-ERR-22003          TO WS-COD-AVIERROR           18.11.19
      *                                                                 18.11.19
               PERFORM 900000-INFORMA-ERR-VALIDACION                    18.11.19
           END-IF                                                       18.11.19
                                                                        18.11.19
           IF  C003-COD-PERSCTPN-E EQUAL SPACES OR                      18.11.19
               C003-COD-PERSCTPN-E EQUAL LOW-VALUES OR                  18.11.19
               C003-COD-PERSCTPN-E EQUAL HIGH-VALUES                    18.11.19
      *                                                                 18.11.19
               STRING CTA-COD-PERSCTPN-E                                18.11.19
                      C003-COD-PERSCTPN-E                               18.11.19
                      CTA-SEP                                           18.11.19
                      KYGHT003-01-01-01-E                               18.11.19
                  DELIMITED BY SIZE                                     18.11.19
                  INTO WS-SQLCA-ERR-QPEJCAQA                            18.11.19
                                                                        18.11.19
               END-STRING                                               18.11.19
      *                                                                 18.11.19
               MOVE CTN-ERR-22003          TO WS-COD-AVIERROR           18.11.19
      *                                                                 18.11.19
               PERFORM 900000-INFORMA-ERR-VALIDACION                    18.11.19
           END-IF                                                       18.11.19
                                                                        18.11.19
           IF  C003-COD-OPERACN-E EQUAL SPACES OR                       18.11.19
               C003-COD-OPERACN-E EQUAL LOW-VALUES OR                   18.11.19
               C003-COD-OPERACN-E EQUAL HIGH-VALUES                     18.11.19
      *                                                                 18.11.19
               STRING CTA-COD-OPERACN-E                                 18.11.19
                      C003-COD-OPERACN-E                                18.11.19
                      CTA-SEP                                           18.11.19
                      KYGHT003-01-01-01-E                               18.11.19
                  DELIMITED BY SIZE                                     18.11.19
                  INTO WS-SQLCA-ERR-QPEJCAQA                            18.11.19
                                                                        18.11.19
               END-STRING                                               18.11.19
      *                                                                 18.11.19
               MOVE CTN-ERR-22003          TO WS-COD-AVIERROR           18.11.19
      *                                                                 18.11.19
               PERFORM 900000-INFORMA-ERR-VALIDACION                    18.11.19
           END-IF                                                       18.11.19
      *                                                                 18.11.19
      *L-0001-INI                                                       18.11.19
           IF   C003-COD-CANAL-DV-E EQUAL SPACES OR                     18.11.19
                C003-COD-CANAL-DV-E EQUAL LOW-VALUES OR                 18.11.19
                C003-COD-CANAL-DV-E EQUAL HIGH-VALUES                   18.11.19
      *L-0002-INI                                                       18.11.19
             OR C003-COD-CANAL-DV-E(1:1) EQUAL SPACES OR                18.11.19
                C003-COD-CANAL-DV-E(1:1) EQUAL LOW-VALUES OR            18.11.19
                C003-COD-CANAL-DV-E(1:1) EQUAL HIGH-VALUES OR           18.11.19
                C003-COD-CANAL-DV-E(2:1) EQUAL SPACES OR                18.11.19
                C003-COD-CANAL-DV-E(2:1) EQUAL LOW-VALUES OR            18.11.19
                C003-COD-CANAL-DV-E(2:1) EQUAL HIGH-VALUES              18.11.19
      *L-0002-FIN                                                       18.11.19
      *L-0003-INI                                                       18.11.19
      *         STRING CTA-COD-CANAL-DV-E                               18.11.19
      *                C003-COD-CANAL-DV-E                              18.11.19
      *                CTA-SEP                                          18.11.19
      *                KYGHT003-01-01-01-E                              18.11.19
      *            DELIMITED BY SIZE                                    18.11.19
      *            INTO WS-SQLCA-ERR-QPEJCAQA                           18.11.19
      *         END-STRING                                              18.11.19
      *                                                                 18.11.19
      *         MOVE CTN-ERR-22003          TO WS-COD-AVIERROR          18.11.19
      *                                                                 18.11.19
      *         PERFORM 900000-INFORMA-ERR-VALIDACION                   18.11.19
                MOVE COD-CANAL-QPEJCAQA  TO C003-COD-CANAL-DV-E         18.11.19
      *L-0003-FIN                                                       18.11.19
           END-IF.                                                      18.11.19
      *                                                                 18.11.19
                                                                        18.11.19
      *L-0001-FIN                                                       18.11.19
      ******************************************************************18.11.19
      * PROCESO PRINCIPAL                                              *18.11.19
      * SE REALIZA EL TRATAMIENTO PRINCIPAL DEL PROGRAMA      .        *18.11.19
      ******************************************************************18.11.19
       200000-PROCESO.                                                  18.11.19
      *                                                                 18.11.19
           PERFORM 210000-RUTINA-KYGHR003.                              20.10.28
      *                                                                 18.11.19
      ******************************************************************18.11.19
      * 210000-RUTINA-KYGHR003                                         *20.10.28
      * SE REALIZA LA LLAMADA A LA RUTINA KYGHR003                     *20.10.28
      ******************************************************************18.11.19
       210000-RUTINA-KYGHR003.                                          20.10.28
      *                                                                 18.11.19
           INITIALIZE RETORNO-QPIPCCAB                                  18.11.19
                      CA03-KYGHCA03                                     20.10.28
      *                                                                 18.11.19
           MOVE C003-COD-PAISOALF-E         TO CA03-COD-PAISOALF-E      20.10.28
           MOVE C003-COD-ENTALFA-E          TO CA03-COD-ENTALFA-E       20.10.28
           MOVE C003-COD-PERSCTPN-E         TO CA03-COD-PERSCTPN-E      20.10.28
           MOVE C003-COD-OPERACN-E          TO CA03-COD-OPERACN-E       20.10.28
           MOVE C003-COD-CANAL-DV-E         TO CA03-COD-CANAL-DV-E      20.10.28
      *                                                                 20.10.28
           CALL CTA-KYGHR003 USING R-QPIPCCAB CA03-KYGHCA03             20.10.29
      *                                                                 18.11.19
           IF  XTI-AVIERROR-QPIPCCAB EQUAL SPACES OR                    18.11.19
               XTI-AVIERROR-QPIPCCAB EQUAL ZERO                         18.11.19
      *                                                                 20.10.28
               MOVE CA03-DES-OPERACN-S      TO C003-DES-OPERACN-S       20.10.28
               MOVE TB-CA03-TABLA-LIMITES-S TO TB-C003-TABLA-LIMITES-S  20.10.28
      *                                                                 18.11.19
           ELSE                                                         20.10.28
                                                                        20.10.28
               PERFORM 999999-FIN-ERROR                                 20.10.28
                                                                        20.10.28
           END-IF.                                                      20.10.28
      *                                                                 18.11.19
      ******************************************************************18.11.19
      * 300000-FIN                                                     *18.11.19
      * SE DEVUELVE EL CONTROL AL PROGRAMA LLAMANTE.                   *18.11.19
      ******************************************************************18.11.19
       300000-FIN.                                                      18.11.19
                                                                        18.11.19
           PERFORM 310000-INFORMA-CONTEXTO                              18.11.19
      *                                                                 18.11.19
           GOBACK.                                                      18.11.19
      *                                                                 18.11.19
      ******************************************************************18.11.19
      * 310000-INFORMA-CONTEXTO                                        *18.11.19
      * SE INFORMA EL AREA DE CONTEXTO.                                *18.11.19
      ******************************************************************18.11.19
       310000-INFORMA-CONTEXTO.                                         18.11.19
      *                                                                 18.11.19
           MOVE COD-PAIS-QPIPCCAB          TO COD-PAIS-QPEJCAQA         18.11.19
           MOVE COD-BANCO-QPIPCCAB         TO COD-BANCO-QPEJCAQA        18.11.19
           MOVE COD-OFICINA-QPIPCCAB       TO COD-OFICINA-QPEJCAQA      18.11.19
           MOVE COD-PUESTO-QPIPCCAB        TO COD-PUESTO-QPEJCAQA       18.11.19
           MOVE COD-CANAL-QPIPCCAB         TO COD-CANAL-QPEJCAQA        18.11.19
           MOVE COD-MEDIO-QPIPCCAB         TO COD-MEDIO-QPEJCAQA        18.11.19
           MOVE COD-APLCANAL-QPIPCCAB      TO COD-APLCANAL-QPEJCAQA     18.11.19
           MOVE COD-IDIOMA-QPIPCCAB        TO COD-IDIOMA-QPEJCAQA       18.11.19
           MOVE COD-USUARIO-QPIPCCAB       TO COD-USUARIO-QPEJCAQA      18.11.19
           MOVE FEC-PROCESO-QPIPCCAB       TO FEC-PROCESO-QPEJCAQA      18.11.19
           MOVE COD-BANCO-OPER-QPIPCCAB    TO COD-BANCO-OPER-QPEJCAQA   18.11.19
           MOVE COD-OFICINA-OPER-QPIPCCAB  TO COD-OFICINA-OPER-QPEJCAQA.18.11.19
                                                                        20.10.28
      *                                                                 18.11.19
      ******************************************************************18.11.19
      * 900000-INFORMA-ERR-VALIDACION                                  *18.11.19
      * SE INFORMA EL ERROR EN VALIDACION.                             *18.11.19
      ******************************************************************18.11.19
       900000-INFORMA-ERR-VALIDACION.                                   18.11.19
      *                                                                 18.11.19
           MOVE CTA-V                      TO XTI-AVIERROR-QPIPCCAB     18.11.19
           MOVE WS-COD-AVIERROR            TO COD-AVIERROR-QPIPCCAB     18.11.19
           MOVE CTA-PROGRAMA               TO COD-MODULO-ERR-QPIPCCAB   18.11.19
           MOVE CTA-PARR-120000            TO COD-PARRAFO-ERR-QPIPCCAB  18.11.19
           MOVE SPACES                     TO COD-TABLA-ERR-QPIPCCAB    18.11.19
           MOVE SPACES                     TO COD-ACCESO-ERR-QPIPCCAB   18.11.19
           MOVE WS-SQLCA-ERR-QPEJCAQA      TO DES-SQLCA-ERR-QPIPCCAB    18.11.19
           MOVE ZEROS                      TO QNU-SQLCODE-ERR-QPIPCCAB  18.11.19
                                                                        18.11.19
           PERFORM 999999-FIN-ERROR.                                    18.11.19
      ******************************************************************18.11.19
      * 999999-FIN-ERROR                                               *18.11.19
      * SE INFORMA ERRORES                                             *18.11.19
      ******************************************************************18.11.19
       999999-FIN-ERROR.                                                18.11.19
      *                                                                 18.11.19
           IF  XTI-AVIERROR-QPIPCCAB EQUAL CTA-I                        18.11.19
      *                                                                 18.11.19
               MOVE CTN-04                 TO COD-RETORNO-QPEJCAQA      18.11.19
      *                                                                 18.11.19
           ELSE                                                         18.11.19
      *                                                                 18.11.19
               IF  XTI-AVIERROR-QPIPCCAB EQUAL CTA-E                    18.11.19
      *                                                                 18.11.19
                   MOVE CTN-12             TO COD-RETORNO-QPEJCAQA      18.11.19
      *                                                                 18.11.19
               ELSE                                                     18.11.19
      *                                                                 18.11.19
                   MOVE CTN-08             TO COD-RETORNO-QPEJCAQA      18.11.19
      *                                                                 18.11.19
               END-IF                                                   18.11.19
      *                                                                 18.11.19
           END-IF                                                       18.11.19
      *                                                                 18.11.19
           MOVE COD-AVIERROR-QPIPCCAB      TO COD-AVIERROR-QPEJCAQA     18.11.19
           MOVE COD-MODULO-ERR-QPIPCCAB    TO COD-MODULO-ERR-QPEJCAQA   18.11.19
           MOVE COD-PARRAFO-ERR-QPIPCCAB   TO COD-PARRAFO-ERR-QPEJCAQA  18.11.19
           MOVE COD-TABLA-ERR-QPIPCCAB     TO COD-TABLA-ERR-QPEJCAQA    18.11.19
           MOVE COD-ACCESO-ERR-QPIPCCAB    TO COD-ACCESO-ERR-QPEJCAQA   18.11.19
           MOVE QNU-SQLCODE-ERR-QPIPCCAB   TO QNU-SQLCODE-ERR-QPEJCAQA  18.11.19
           MOVE DES-SQLCA-ERR-QPIPCCAB     TO DES-SQLCA-ERR-QPEJCAQA    18.11.19
      *                                                                 18.11.19
           PERFORM 300000-FIN.                                                  