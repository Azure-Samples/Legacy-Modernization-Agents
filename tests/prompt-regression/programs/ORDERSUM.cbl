      IDENTIFICATION DIVISION.
       PROGRAM-ID. ORDERSUM.
      *>**********************************************************
      *> Golden program for the prompt regression suite.
      *> Simple SQL batch: read orders, sum totals per customer,
      *> write a summary row. Two paragraphs, one EVALUATE,
      *> one EXEC SQL SELECT, one EXEC SQL INSERT.
      *>**********************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CUST-ID         PIC X(10).
       01 WS-TOTAL           PIC S9(9)V99 COMP-3.
       01 WS-STATUS          PIC X(1).
          88 STATUS-OK       VALUE 'O'.
          88 STATUS-BAD      VALUE 'B'.
       01 WS-SQLCODE         PIC S9(9) COMP.

       LINKAGE SECTION.
       01 LK-IN-CUST-ID      PIC X(10).
       01 LK-OUT-TOTAL       PIC S9(9)V99 COMP-3.
       01 LK-OUT-STATUS      PIC X(1).

       PROCEDURE DIVISION USING LK-IN-CUST-ID
                                LK-OUT-TOTAL
                                LK-OUT-STATUS.

       0000-MAIN SECTION.
       0000-MAIN-START.
           MOVE LK-IN-CUST-ID TO WS-CUST-ID
           PERFORM 1000-COMPUTE-TOTAL
           PERFORM 2000-WRITE-SUMMARY
           MOVE WS-TOTAL  TO LK-OUT-TOTAL
           MOVE WS-STATUS TO LK-OUT-STATUS
           GOBACK.

       1000-COMPUTE-TOTAL SECTION.
       1000-START.
           EXEC SQL
               SELECT SUM(AMOUNT)
                 INTO :WS-TOTAL
                 FROM ORDERS
                WHERE CUSTOMER_ID = :WS-CUST-ID
           END-EXEC
           EVALUATE TRUE
               WHEN WS-SQLCODE = 0
                   SET STATUS-OK TO TRUE
               WHEN WS-SQLCODE = 100
                   MOVE 0 TO WS-TOTAL
                   SET STATUS-OK TO TRUE
               WHEN OTHER
                   SET STATUS-BAD TO TRUE
           END-EVALUATE.
       1000-END.
           EXIT.

       2000-WRITE-SUMMARY SECTION.
       2000-START.
           IF STATUS-OK
               EXEC SQL
                   INSERT INTO ORDER_SUMMARY
                          (CUSTOMER_ID, TOTAL_AMOUNT)
                   VALUES (:WS-CUST-ID, :WS-TOTAL)
               END-EXEC
           END-IF.
       2000-END.
           EXIT.
