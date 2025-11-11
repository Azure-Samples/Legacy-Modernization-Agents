       IDENTIFICATION DIVISION.  
       PROGRAM-ID. DBDRIVR2.  
       ENVIRONMENT DIVISION.  
       CONFIGURATION SECTION.  
       DATA DIVISION.  
       WORKING-STORAGE SECTION.  
  
       EXEC SQL  
           INCLUDE SQLCA  
       END-EXEC.  
         
       EXEC SQL  
           INCLUDE DTRAKING  
       END-EXEC.  
  
       01  WS-PROCESS-DATE                PIC X(10).  
       01  WS-OPERATION-TYPE              PIC X(10).  
       01  WS-SQLCODE                     PIC S9(9) COMP.  
       01  WS-NOT-PRESENT-IN-TRACKING-SW  PIC X(3) VALUE 'NO'.
           88 NOT-PRESENT-IN-TRACKING              VALUE 'YES'. 
  

       LINKAGE SECTION.  
       01  LNK-AREA.  
           05  LNK-INPUT-AREA.  
               10  LNK-OPERATION-TYPE           PIC X(10).  
               10  LNK-PROCESS-DATE             PIC X(10). 
               10  LNK-POLICY-NUMBER            PIC X(10). 
           05 LNK-OUTPUT-AREA.  
               10  LNK-SQLCODE                  PIC S9(9) COMP.  

  
       PROCEDURE DIVISION USING LNK-AREA.  
       1000-MAIN-PARA.  
           MOVE LNK-PROCESS-DATE   TO WS-PROCESS-DATE  
           MOVE LNK-OPERATION-TYPE TO WS-OPERATION-TYPE  
           EVALUATE WS-OPERATION-TYPE  
               WHEN 'INSERT'  
                   PERFORM 2000-INSERT-UPDATE-TRACKING  
               WHEN 'UPDATE'  
                   PERFORM 2000-INSERT-UPDATE-TRACKING 
               WHEN OTHER  
                   DISPLAY 'INVALID OPERATION TYPE'  
                   MOVE -1 TO WS-SQLCODE  
           END-EVALUATE.  
           MOVE SQLCODE TO WS-SQLCODE. 
           MOVE WS-SQLCODE TO LNK-SQLCODE. 
           GOBACK.   
  
       2000-INSERT-UPDATE-TRACKING.  
           PERFORM 2100-SELECT-TRACKING
           PERFORM 2200-POPULATE-TRACKING
           IF NOT-PRESENT-IN-TRACKING
               EXEC SQL  
                   INSERT INTO INSURNCE.TTRAKING (
                    TR_POLICY_NUMBER,
                    TR_NOTIFY_DATE,
                    TR_STATUS,
                    TR_ADD_TIMESTAMP,
                    TR_UPDATE_TIMESTAMP
                    ) VALUES (
                    :TR-POLICY-NUMBER,
                    :TR-NOTIFY-DATE,
                    :TR-STATUS,
                    CURRENT TIMESTAMP,
                    CURRENT TIMESTAMP
                    ) 
               END-EXEC  
               IF SQLCODE NOT EQUAL TO 0  
                   DISPLAY 'ERROR INSERTING IN TTRACKING SQLCODE: ' 
                                                               SQLCODE  
                   MOVE SQLCODE TO WS-SQLCODE  
      *            CALL 'ABEND'  
               END-IF
           ELSE
              EXEC SQL  
                  UPDATE INSURNCE.TTRAKING
                  SET   TR_NOTIFY_DATE = :TR-NOTIFY-DATE,
                        TR_STATUS      = :TR-STATUS,
                        TR_UPDATE_TIMESTAMP =  CURRENT TIMESTAMP
                   WHERE  TR_POLICY_NUMBER = :TR-POLICY-NUMBER 
              END-EXEC  
              IF SQLCODE NOT EQUAL TO 0  
                  DISPLAY 'ERROR UPDATING TTRACKING SQLCODE: ' SQLCODE  
                  MOVE SQLCODE TO WS-SQLCODE  
      *           CALL 'ABEND'  
              END-IF
           END-IF.

       2100-SELECT-TRACKING.
           MOVE LNK-POLICY-NUMBER    TO TR-POLICY-NUMBER
           EXEC SQL 
               SELECT * 
               INTO :DCLTRAKI
               FROM INSURNCE.TTRAKING 
               WHERE TR_POLICY_NUMBER = :TR-POLICY-NUMBER 
           END-EXEC  
           IF SQLCODE NOT EQUAL TO 0 AND  SQLCODE NOT EQUAL TO 100
               DISPLAY 'ERROR SELECTING TTRACKING SQLCODE: ' SQLCODE  
               MOVE SQLCODE TO WS-SQLCODE  
      *        CALL 'ABEND'  
           ELSE 
              IF SQLCODE EQUAL TO 100
                  SET NOT-PRESENT-IN-TRACKING TO TRUE
              ELSE
                  SET NOT-PRESENT-IN-TRACKING TO FALSE
              END-IF
           END-IF.
           

       2200-POPULATE-TRACKING.
           MOVE LNK-POLICY-NUMBER    TO TR-POLICY-NUMBER.
           MOVE WS-PROCESS-DATE  TO TR-NOTIFY-DATE.
           MOVE 'A'              TO TR-STATUS.

       END PROGRAM DBDRIVR2.  