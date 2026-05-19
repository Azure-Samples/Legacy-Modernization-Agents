      IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTLKUP.
      *>**********************************************************
      *> Golden program for the prompt regression suite.
      *> CICS pseudo-conversational lookup. One DFHCOMMAREA,
      *> one EXEC CICS RETURN TRANSID, one CALL to a child program.
      *>**********************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MSG             PIC X(60).
       01 WS-LOOKUP-RC       PIC S9(4) COMP.

       LINKAGE SECTION.
       01 DFHCOMMAREA.
          05 CA-CUST-ID      PIC X(10).
          05 CA-CUST-NAME    PIC X(40).
          05 CA-STATUS       PIC X(1).

       PROCEDURE DIVISION.

       0000-MAIN SECTION.
       0000-MAIN-START.
           IF CA-CUST-ID = SPACES
               MOVE 'Customer ID required' TO WS-MSG
               PERFORM 9000-SEND-ERROR
           ELSE
               CALL 'CUSTREAD' USING CA-CUST-ID CA-CUST-NAME WS-LOOKUP-RC
               IF WS-LOOKUP-RC = 0
                   MOVE 'O' TO CA-STATUS
               ELSE
                   MOVE 'N' TO CA-STATUS
               END-IF
           END-IF
           EXEC CICS RETURN
               TRANSID('CL01')
               COMMAREA(DFHCOMMAREA)
           END-EXEC.

       9000-SEND-ERROR SECTION.
       9000-START.
           EXEC CICS SEND TEXT FROM(WS-MSG) LENGTH(60)
                ERASE
           END-EXEC.
       9000-END.
           EXIT.
