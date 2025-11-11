       IDENTIFICATION DIVISION.  
       PROGRAM-ID. DBDRIVR1.  
       ENVIRONMENT DIVISION.  
       CONFIGURATION SECTION.  
       DATA DIVISION.  
       WORKING-STORAGE SECTION.  
  
       EXEC SQL  
           INCLUDE SQLCA  
       END-EXEC.  
         
       EXEC SQL  
           INCLUDE DPOLICY  
       END-EXEC.  

       EXEC SQL  
           INCLUDE DCOVERAG  
       END-EXEC.  

       EXEC SQL  
           INCLUDE DTRAKING  
       END-EXEC.  
  
       01  WS-PROCESS-DATE             PIC X(10).  
       01  WS-OPERATION-TYPE           PIC X(10).  
       01  WS-SQLCODE                  PIC S9(9) COMP.  
  
       EXEC SQL  
           DECLARE POLICY-CURSOR CURSOR FOR  
               SELECT POLICY_NUMBER,  
                      POLICY_HOLDER_FNAME,  
                      POLICY_HOLDER_MNAME,  
                      POLICY_HOLDER_LNAME,
                      POLICY_BENEF_NAME,
                      POLICY_BENEF_RELATION,
                      POLICY_HOLDER_ADDR_1,  
                      POLICY_HOLDER_ADDR_2,  
                      POLICY_HOLDER_CITY,  
                      POLICY_HOLDER_STATE, 
                      POLICY_HOLDER_ZIP_CD,
                      POLICY_HOLDER_DOB,
                      POLICY_HOLDER_GENDER,
                      POLICY_HOLDER_PHONE,
                      POLICY_HOLDER_EMAIL,
                      POLICY_PAYMENT_FREQUENCY,
                      POLICY_PAYMENT_METHOD,
                      POLICY_UNDERWRITER,
                      POLICY_TERMS_CONDITIONS,
                      POLICY_CLAIMED,
                      POLICY_DISCOUNT_CODE,
                      POLICY_PREMIUM_AMOUNT,
                      POLICY_COVERAGE_AMOUNT,
                      POLICY_TYPE,  
                      POLICY_START_DATE,  
                      POLICY_EXPIRY_DATE,  
                      POLICY_STATUS,
                      POLICY_AGENT_CODE,
                      POLICY_NOTIFY_FLAG,
                      POLICY_ADD_TIMESTAMP,  
                      POLICY_UPDATE_TIMESTAMP 
                 FROM INSURNCE.TPOLICY , INSURNCE.TCOVERAG
                 WHERE POLICY_STATUS = 'A'  
                   AND POLICY_HOLDER_STATE IN (
                                'CA', 'MN', 'NY')  
                   AND POLICY_EXPIRY_DATE - :WS-PROCESS-DATE >= 30  
                   AND POLICY_EXPIRY_DATE - :WS-PROCESS-DATE <= 35 
                   AND POLICY_TYPE     = 'HEALTH'
                   AND POLICY_NUMBER = COVERAGE_POL_NUM
                   AND COVERAGE_STATUS = 'ACTIVE'
                   AND COVERAGE_TYPE = 'CHARGEABLE'
                   AND POLICY_NUMBER NOT IN ( 
                            SELECT TR_POLICY_NUMBER 
                            FROM INSURNCE.TTRAKING
                            WHERE TR_POLICY_NUMBER = POLICY_NUMBER
                              AND TR_STATUS = 'A'
                            )
                   ORDER BY  POLICY_HOLDER_STATE, POLICY_AGENT_CODE  
       END-EXEC.  
  
       LINKAGE SECTION.  
       01  LNK-AREA.  
           05  LNK-INPUT-AREA.  
               10  LNK-OPERATION-TYPE           PIC X(10).  
               10  LNK-PROCESS-DATE             PIC X(10).  
           05 LNK-OUTPUT-AREA.  
               10  LNK-SQLCODE                  PIC S9(9) COMP.  
               10  LNK-DCLPOLICY                PIC X(466).  
  
       PROCEDURE DIVISION USING LNK-AREA.  
       MAIN-PARA.  
           MOVE LNK-PROCESS-DATE   TO WS-PROCESS-DATE  
           MOVE LNK-OPERATION-TYPE TO WS-OPERATION-TYPE  
           EVALUATE WS-OPERATION-TYPE  
               WHEN 'OPEN'  
                   PERFORM OPEN-POLICY-CURSOR  
               WHEN 'FETCH'  
                   PERFORM FETCH-POLICY-CURSOR  
               WHEN 'CLOSE'  
                   PERFORM CLOSE-POLICY-CURSOR  
               WHEN OTHER  
                   DISPLAY 'INVALID OPERATION TYPE'  
                   MOVE -1 TO WS-SQLCODE  
           END-EVALUATE.  
           MOVE SQLCODE TO WS-SQLCODE. 
           MOVE WS-SQLCODE TO LNK-SQLCODE. 
           GOBACK.   
  
       OPEN-POLICY-CURSOR.  
           EXEC SQL  
               OPEN POLICY-CURSOR  
           END-EXEC  
           IF SQLCODE NOT EQUAL TO 0  
               DISPLAY 'ERROR OPENING POLICY-CURSOR SQLCODE: ' SQLCODE  
               MOVE SQLCODE TO WS-SQLCODE  
      *        CALL 'ABEND'  
           END-IF.  
  
       FETCH-POLICY-CURSOR.  
           EXEC SQL  
               FETCH POLICY-CURSOR INTO :DCLPOLICY  
           END-EXEC  
           IF SQLCODE EQUAL TO 0
               MOVE DCLPOLICY   TO LNK-DCLPOLICY
           ELSE
           IF SQLCODE NOT EQUAL TO 100  
               DISPLAY 'ERROR FETCHING FROM POLICY-CURSOR SQLCODE: ' 
                                                              SQLCODE  
               MOVE SQLCODE TO WS-SQLCODE  
      *        CALL 'ABEND'  
           END-IF.  
  
  
       CLOSE-POLICY-CURSOR.  
           EXEC SQL  
               CLOSE POLICY-CURSOR  
           END-EXEC  
           IF SQLCODE NOT EQUAL TO 0  
               DISPLAY 'ERROR CLOSING POLICY-CURSOR SQLCODE: ' SQLCODE  
               MOVE SQLCODE TO WS-SQLCODE  
      *        CALL 'ABEND'  
           END-IF.  
  
       END PROGRAM DBDRIVR1.  