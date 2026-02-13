       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRESSPROG.
       AUTHOR. GENERATED-BY-CHATGPT.
       INSTALLATION. CHUNKING-STRESS.
       DATE-WRITTEN. 2026-02-12.
       SECURITY. NONE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DUMMY-IN  ASSIGN TO 'DUMMYIN'.
           SELECT DUMMY-OUT ASSIGN TO 'DUMMYOUT'.
       DATA DIVISION.
       FILE SECTION.
       FD  DUMMY-IN.
       01  DUMMY-IN-REC                  PIC X(120).
       FD  DUMMY-OUT.
       01  DUMMY-OUT-REC                 PIC X(200).
       WORKING-STORAGE SECTION.
       77  WS-RUN-ID                     PIC 9(09) VALUE 0.
       77  WS-STATUS                     PIC X(02) VALUE '00'.
       77  WS-LOOP-LIMIT                 PIC 9(05) VALUE 2500.
       77  WS-ERR-COUNT                  PIC 9(07) VALUE 0.
       01  WS-FLAGS.
           05  WS-EOF                    PIC X VALUE 'N'.
           05  WS-MODE                   PIC X(08) VALUE 'STRESS'.
       01  WS-ACCUMULATORS.
           05  WS-SUM-A                  PIC S9(11)V99 COMP-3 VALUE +0.
           05  WS-SUM-B                  PIC S9(11)V99 COMP-3 VALUE +0.
           05  WS-HASH                   PIC 9(09) COMP VALUE 1.
       01  WS-TEXT.
           05  WS-MSG                    PIC X(80) VALUE SPACES.
       01  WS-TABLE.
           05  WS-TBL                    OCCURS 100 TIMES INDEXED BY WS-IX.
               10  WS-TBL-KEY            PIC X(12).
               10  WS-TBL-VAL            PIC 9(09) COMP.
       COPY 'STRESSCOPY.cpy'.
       01  WS-PROG-BUFFER.
           05  WS-BUF-01                 PIC X(64) VALUE ALL '0'.
           05  WS-BUF-02                 PIC X(64) VALUE ALL '1'.
           05  WS-BUF-03                 PIC X(64) VALUE ALL '2'.
       01  WS-CALL-AREA.
           05  WS-CALL-CODE              PIC X(08) VALUE 'INIT'.
           05  WS-CALL-DATA              PIC X(72) VALUE SPACES.
       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-PARM-LEN               PIC 9(04) COMP.
           05  LK-PARM-TEXT              PIC X(256).
       PROCEDURE DIVISION USING LK-PARM.
       MAIN-ENTRY.
           PERFORM INIT-0001
           PERFORM LOAD-TABLE-0010
           PERFORM PROCESS-LOOP-0100
           PERFORM FINALIZE-9990
           GOBACK.
       INIT-0001.
           ADD 1 TO WS-RUN-ID
           MOVE '00' TO WS-STATUS
           MOVE 'N'  TO WS-EOF
           MOVE 'BOOT' TO WS-CALL-CODE
           STRING 'RUN=' WS-RUN-ID DELIMITED BY SIZE
                  ' MODE=' WS-MODE  DELIMITED BY SIZE
             INTO WS-MSG
           END-STRING
           PERFORM HASH-SEED-0002.
       HASH-SEED-0002.
           COMPUTE WS-HASH = FUNCTION MOD(WS-RUN-ID * 1103515245 + 12345, 2147483647).
       LOAD-TABLE-0010.
           SET WS-IX TO 1
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 100
               MOVE WS-IX TO WS-TBL-VAL(WS-IX)
               MOVE 'KEY-' TO WS-TBL-KEY(WS-IX)(1:4)
               MOVE WS-IX  TO WS-TBL-KEY(WS-IX)(5:3)
           END-PERFORM.
       PROCESS-LOOP-0100.
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > WS-LOOP-LIMIT
               PERFORM DISPATCH-0200
           END-PERFORM.
       DISPATCH-0200.
           ADD 1400 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 2600, 2147483647)
           MOVE WS-HASH TO SCB-A-000201
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0001' TO WS-CALL-CODE
           CALL 'EXTM0001' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0201.
           ADD 1407 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 2613, 2147483647)
           MOVE WS-HASH TO SCB-A-000202
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0002' TO WS-CALL-CODE
           CALL 'EXTM0002' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0202.
           ADD 1414 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 2626, 2147483647)
           MOVE WS-HASH TO SCB-A-000203
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0003' TO WS-CALL-CODE
           CALL 'EXTM0003' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0203.
           ADD 1421 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 2639, 2147483647)
           MOVE WS-HASH TO SCB-A-000204
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0004' TO WS-CALL-CODE
           CALL 'EXTM0004' USING WS-CALL-AREA
           PERFORM VALIDATE-0203.
       VALIDATE-0203.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0204.
           ADD 1428 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 2652, 2147483647)
           MOVE WS-HASH TO SCB-A-000205
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0005' TO WS-CALL-CODE
           CALL 'EXTM0005' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0205.
           ADD 1435 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 2665, 2147483647)
           MOVE WS-HASH TO SCB-A-000206
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0006' TO WS-CALL-CODE
           CALL 'EXTM0006' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0206.
           ADD 1442 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 2678, 2147483647)
           MOVE WS-HASH TO SCB-A-000207
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0007' TO WS-CALL-CODE
           CALL 'EXTM0007' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0207.
           ADD 1449 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 2691, 2147483647)
           MOVE WS-HASH TO SCB-A-000208
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0008' TO WS-CALL-CODE
           CALL 'EXTM0008' USING WS-CALL-AREA
           PERFORM AUX-0207.
       AUX-0207.
           MOVE 'AUX-0207' TO WS-CALL-DATA(1:12)
           ADD 13 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0208.
           ADD 1456 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 2704, 2147483647)
           MOVE WS-HASH TO SCB-A-000209
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0009' TO WS-CALL-CODE
           CALL 'EXTM0009' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0209.
           ADD 1463 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 2717, 2147483647)
           MOVE WS-HASH TO SCB-A-000210
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0010' TO WS-CALL-CODE
           CALL 'EXTM0010' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0210.
           ADD 1470 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 2730, 2147483647)
           MOVE WS-HASH TO SCB-A-000211
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0011' TO WS-CALL-CODE
           CALL 'EXTM0011' USING WS-CALL-AREA
           PERFORM VALIDATE-0210.
       VALIDATE-0210.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0211.
           ADD 1477 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 2743, 2147483647)
           MOVE WS-HASH TO SCB-A-000212
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0012' TO WS-CALL-CODE
           CALL 'EXTM0012' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0212.
           ADD 1484 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 2756, 2147483647)
           MOVE WS-HASH TO SCB-A-000213
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0013' TO WS-CALL-CODE
           CALL 'EXTM0013' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0213.
           ADD 1491 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 2769, 2147483647)
           MOVE WS-HASH TO SCB-A-000214
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0014' TO WS-CALL-CODE
           CALL 'EXTM0014' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0214.
           ADD 1498 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 2782, 2147483647)
           MOVE WS-HASH TO SCB-A-000215
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0015' TO WS-CALL-CODE
           CALL 'EXTM0015' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0215.
           ADD 1505 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 2795, 2147483647)
           MOVE WS-HASH TO SCB-A-000216
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0016' TO WS-CALL-CODE
           CALL 'EXTM0016' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0216.
           ADD 1512 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 2808, 2147483647)
           MOVE WS-HASH TO SCB-A-000217
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0017' TO WS-CALL-CODE
           CALL 'EXTM0017' USING WS-CALL-AREA
           PERFORM AUX-0216.
       AUX-0216.
           MOVE 'AUX-0216' TO WS-CALL-DATA(1:12)
           ADD 22 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0217.
           ADD 1519 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 2821, 2147483647)
           MOVE WS-HASH TO SCB-A-000218
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0018' TO WS-CALL-CODE
           CALL 'EXTM0018' USING WS-CALL-AREA
           PERFORM VALIDATE-0217.
       VALIDATE-0217.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0218.
           ADD 1526 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 2834, 2147483647)
           MOVE WS-HASH TO SCB-A-000219
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0019' TO WS-CALL-CODE
           CALL 'EXTM0019' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0219.
           ADD 1533 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 2847, 2147483647)
           MOVE WS-HASH TO SCB-A-000220
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0020' TO WS-CALL-CODE
           CALL 'EXTM0020' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0220.
           ADD 1540 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 2860, 2147483647)
           MOVE WS-HASH TO SCB-A-000221
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0021' TO WS-CALL-CODE
           CALL 'EXTM0021' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0221.
           ADD 1547 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 2873, 2147483647)
           MOVE WS-HASH TO SCB-A-000222
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0022' TO WS-CALL-CODE
           CALL 'EXTM0022' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0222.
           ADD 1554 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 2886, 2147483647)
           MOVE WS-HASH TO SCB-A-000223
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0023' TO WS-CALL-CODE
           CALL 'EXTM0023' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0223.
           ADD 1561 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 2899, 2147483647)
           MOVE WS-HASH TO SCB-A-000224
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0024' TO WS-CALL-CODE
           CALL 'EXTM0024' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0224.
           ADD 1568 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 2912, 2147483647)
           MOVE WS-HASH TO SCB-A-000225
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0025' TO WS-CALL-CODE
           CALL 'EXTM0025' USING WS-CALL-AREA
           PERFORM VALIDATE-0224.
       VALIDATE-0224.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0225.
           ADD 1575 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 2925, 2147483647)
           MOVE WS-HASH TO SCB-A-000226
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0026' TO WS-CALL-CODE
           CALL 'EXTM0026' USING WS-CALL-AREA
           PERFORM AUX-0225.
       AUX-0225.
           MOVE 'AUX-0225' TO WS-CALL-DATA(1:12)
           ADD 31 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0226.
           ADD 1582 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 2938, 2147483647)
           MOVE WS-HASH TO SCB-A-000227
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0027' TO WS-CALL-CODE
           CALL 'EXTM0027' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0227.
           ADD 1589 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 2951, 2147483647)
           MOVE WS-HASH TO SCB-A-000228
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0028' TO WS-CALL-CODE
           CALL 'EXTM0028' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0228.
           ADD 1596 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 2964, 2147483647)
           MOVE WS-HASH TO SCB-A-000229
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0029' TO WS-CALL-CODE
           CALL 'EXTM0029' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0229.
           ADD 1603 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 2977, 2147483647)
           MOVE WS-HASH TO SCB-A-000230
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0030' TO WS-CALL-CODE
           CALL 'EXTM0030' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0230.
           ADD 1610 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 2990, 2147483647)
           MOVE WS-HASH TO SCB-A-000231
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0031' TO WS-CALL-CODE
           CALL 'EXTM0031' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0231.
           ADD 1617 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 3003, 2147483647)
           MOVE WS-HASH TO SCB-A-000232
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0032' TO WS-CALL-CODE
           CALL 'EXTM0032' USING WS-CALL-AREA
           PERFORM VALIDATE-0231.
       VALIDATE-0231.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0232.
           ADD 1624 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 3016, 2147483647)
           MOVE WS-HASH TO SCB-A-000233
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0033' TO WS-CALL-CODE
           CALL 'EXTM0033' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0233.
           ADD 1631 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 3029, 2147483647)
           MOVE WS-HASH TO SCB-A-000234
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0034' TO WS-CALL-CODE
           CALL 'EXTM0034' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0234.
           ADD 1638 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 3042, 2147483647)
           MOVE WS-HASH TO SCB-A-000235
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0035' TO WS-CALL-CODE
           CALL 'EXTM0035' USING WS-CALL-AREA
           PERFORM AUX-0234.
       AUX-0234.
           MOVE 'AUX-0234' TO WS-CALL-DATA(1:12)
           ADD 40 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0235.
           ADD 1645 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 3055, 2147483647)
           MOVE WS-HASH TO SCB-A-000236
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0036' TO WS-CALL-CODE
           CALL 'EXTM0036' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0236.
           ADD 1652 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 3068, 2147483647)
           MOVE WS-HASH TO SCB-A-000237
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0037' TO WS-CALL-CODE
           CALL 'EXTM0037' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0237.
           ADD 1659 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 3081, 2147483647)
           MOVE WS-HASH TO SCB-A-000238
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0038' TO WS-CALL-CODE
           CALL 'EXTM0038' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0238.
           ADD 1666 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 3094, 2147483647)
           MOVE WS-HASH TO SCB-A-000239
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0039' TO WS-CALL-CODE
           CALL 'EXTM0039' USING WS-CALL-AREA
           PERFORM VALIDATE-0238.
       VALIDATE-0238.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0239.
           ADD 1673 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 3107, 2147483647)
           MOVE WS-HASH TO SCB-A-000240
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0040' TO WS-CALL-CODE
           CALL 'EXTM0040' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0240.
           ADD 1680 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 3120, 2147483647)
           MOVE WS-HASH TO SCB-A-000241
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0041' TO WS-CALL-CODE
           CALL 'EXTM0041' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0241.
           ADD 1687 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 3133, 2147483647)
           MOVE WS-HASH TO SCB-A-000242
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0042' TO WS-CALL-CODE
           CALL 'EXTM0042' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0242.
           ADD 1694 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 3146, 2147483647)
           MOVE WS-HASH TO SCB-A-000243
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0043' TO WS-CALL-CODE
           CALL 'EXTM0043' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0243.
           ADD 1701 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 3159, 2147483647)
           MOVE WS-HASH TO SCB-A-000244
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0044' TO WS-CALL-CODE
           CALL 'EXTM0044' USING WS-CALL-AREA
           PERFORM AUX-0243.
       AUX-0243.
           MOVE 'AUX-0243' TO WS-CALL-DATA(1:12)
           ADD 49 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0244.
           ADD 1708 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 3172, 2147483647)
           MOVE WS-HASH TO SCB-A-000245
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0045' TO WS-CALL-CODE
           CALL 'EXTM0045' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0245.
           ADD 1715 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 3185, 2147483647)
           MOVE WS-HASH TO SCB-A-000246
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0046' TO WS-CALL-CODE
           CALL 'EXTM0046' USING WS-CALL-AREA
           PERFORM VALIDATE-0245.
       VALIDATE-0245.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0246.
           ADD 1722 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 3198, 2147483647)
           MOVE WS-HASH TO SCB-A-000247
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0047' TO WS-CALL-CODE
           CALL 'EXTM0047' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0247.
           ADD 1729 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 3211, 2147483647)
           MOVE WS-HASH TO SCB-A-000248
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0048' TO WS-CALL-CODE
           CALL 'EXTM0048' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0248.
           ADD 1736 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 3224, 2147483647)
           MOVE WS-HASH TO SCB-A-000249
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0049' TO WS-CALL-CODE
           CALL 'EXTM0049' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0249.
           ADD 1743 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 3237, 2147483647)
           MOVE WS-HASH TO SCB-A-000250
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0050' TO WS-CALL-CODE
           CALL 'EXTM0050' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0250.
           ADD 1750 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 3250, 2147483647)
           MOVE WS-HASH TO SCB-A-000251
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0051' TO WS-CALL-CODE
           CALL 'EXTM0051' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0251.
           ADD 1757 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 3263, 2147483647)
           MOVE WS-HASH TO SCB-A-000252
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0052' TO WS-CALL-CODE
           CALL 'EXTM0052' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0252.
           ADD 1764 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 3276, 2147483647)
           MOVE WS-HASH TO SCB-A-000253
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0053' TO WS-CALL-CODE
           CALL 'EXTM0053' USING WS-CALL-AREA
           PERFORM AUX-0252.
       AUX-0252.
           MOVE 'AUX-0252' TO WS-CALL-DATA(1:12)
           ADD 58 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0253.
           ADD 1771 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 3289, 2147483647)
           MOVE WS-HASH TO SCB-A-000254
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0054' TO WS-CALL-CODE
           CALL 'EXTM0054' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0254.
           ADD 1778 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 3302, 2147483647)
           MOVE WS-HASH TO SCB-A-000255
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0055' TO WS-CALL-CODE
           CALL 'EXTM0055' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0255.
           ADD 1785 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 3315, 2147483647)
           MOVE WS-HASH TO SCB-A-000256
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0056' TO WS-CALL-CODE
           CALL 'EXTM0056' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0256.
           ADD 1792 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 3328, 2147483647)
           MOVE WS-HASH TO SCB-A-000257
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0057' TO WS-CALL-CODE
           CALL 'EXTM0057' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0257.
           ADD 1799 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 3341, 2147483647)
           MOVE WS-HASH TO SCB-A-000258
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0058' TO WS-CALL-CODE
           CALL 'EXTM0058' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0258.
           ADD 1806 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 3354, 2147483647)
           MOVE WS-HASH TO SCB-A-000259
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0059' TO WS-CALL-CODE
           CALL 'EXTM0059' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0259.
           ADD 1813 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 3367, 2147483647)
           MOVE WS-HASH TO SCB-A-000260
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0060' TO WS-CALL-CODE
           CALL 'EXTM0060' USING WS-CALL-AREA
           PERFORM VALIDATE-0259.
       VALIDATE-0259.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0260.
           ADD 1820 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 3380, 2147483647)
           MOVE WS-HASH TO SCB-A-000261
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0061' TO WS-CALL-CODE
           CALL 'EXTM0061' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0261.
           ADD 1827 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 3393, 2147483647)
           MOVE WS-HASH TO SCB-A-000262
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0062' TO WS-CALL-CODE
           CALL 'EXTM0062' USING WS-CALL-AREA
           PERFORM AUX-0261.
       AUX-0261.
           MOVE 'AUX-0261' TO WS-CALL-DATA(1:12)
           ADD 67 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0262.
           ADD 1834 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 3406, 2147483647)
           MOVE WS-HASH TO SCB-A-000263
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0063' TO WS-CALL-CODE
           CALL 'EXTM0063' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0263.
           ADD 1841 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 3419, 2147483647)
           MOVE WS-HASH TO SCB-A-000264
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0064' TO WS-CALL-CODE
           CALL 'EXTM0064' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0264.
           ADD 1848 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 3432, 2147483647)
           MOVE WS-HASH TO SCB-A-000265
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0065' TO WS-CALL-CODE
           CALL 'EXTM0065' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0265.
           ADD 1855 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 3445, 2147483647)
           MOVE WS-HASH TO SCB-A-000266
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0066' TO WS-CALL-CODE
           CALL 'EXTM0066' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0266.
           ADD 1862 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 3458, 2147483647)
           MOVE WS-HASH TO SCB-A-000267
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0067' TO WS-CALL-CODE
           CALL 'EXTM0067' USING WS-CALL-AREA
           PERFORM VALIDATE-0266.
       VALIDATE-0266.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0267.
           ADD 1869 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 3471, 2147483647)
           MOVE WS-HASH TO SCB-A-000268
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0068' TO WS-CALL-CODE
           CALL 'EXTM0068' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0268.
           ADD 1876 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 3484, 2147483647)
           MOVE WS-HASH TO SCB-A-000269
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0069' TO WS-CALL-CODE
           CALL 'EXTM0069' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0269.
           ADD 1883 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 3497, 2147483647)
           MOVE WS-HASH TO SCB-A-000270
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0070' TO WS-CALL-CODE
           CALL 'EXTM0070' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0270.
           ADD 1890 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 3510, 2147483647)
           MOVE WS-HASH TO SCB-A-000271
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0071' TO WS-CALL-CODE
           CALL 'EXTM0071' USING WS-CALL-AREA
           PERFORM AUX-0270.
       AUX-0270.
           MOVE 'AUX-0270' TO WS-CALL-DATA(1:12)
           ADD 76 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0271.
           ADD 1897 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 3523, 2147483647)
           MOVE WS-HASH TO SCB-A-000272
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0072' TO WS-CALL-CODE
           CALL 'EXTM0072' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0272.
           ADD 1904 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 3536, 2147483647)
           MOVE WS-HASH TO SCB-A-000273
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0073' TO WS-CALL-CODE
           CALL 'EXTM0073' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0273.
           ADD 1911 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 3549, 2147483647)
           MOVE WS-HASH TO SCB-A-000274
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0074' TO WS-CALL-CODE
           CALL 'EXTM0074' USING WS-CALL-AREA
           PERFORM VALIDATE-0273.
       VALIDATE-0273.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0274.
           ADD 1918 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 3562, 2147483647)
           MOVE WS-HASH TO SCB-A-000275
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0075' TO WS-CALL-CODE
           CALL 'EXTM0075' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0275.
           ADD 1925 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 3575, 2147483647)
           MOVE WS-HASH TO SCB-A-000276
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0076' TO WS-CALL-CODE
           CALL 'EXTM0076' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0276.
           ADD 1932 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 3588, 2147483647)
           MOVE WS-HASH TO SCB-A-000277
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0077' TO WS-CALL-CODE
           CALL 'EXTM0077' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0277.
           ADD 1939 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 3601, 2147483647)
           MOVE WS-HASH TO SCB-A-000278
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0078' TO WS-CALL-CODE
           CALL 'EXTM0078' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0278.
           ADD 1946 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 3614, 2147483647)
           MOVE WS-HASH TO SCB-A-000279
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0079' TO WS-CALL-CODE
           CALL 'EXTM0079' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0279.
           ADD 1953 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 3627, 2147483647)
           MOVE WS-HASH TO SCB-A-000280
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0080' TO WS-CALL-CODE
           CALL 'EXTM0080' USING WS-CALL-AREA
           PERFORM AUX-0279.
       AUX-0279.
           MOVE 'AUX-0279' TO WS-CALL-DATA(1:12)
           ADD 85 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0280.
           ADD 1960 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 3640, 2147483647)
           MOVE WS-HASH TO SCB-A-000281
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0081' TO WS-CALL-CODE
           CALL 'EXTM0081' USING WS-CALL-AREA
           PERFORM VALIDATE-0280.
       VALIDATE-0280.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0281.
           ADD 1967 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 3653, 2147483647)
           MOVE WS-HASH TO SCB-A-000282
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0082' TO WS-CALL-CODE
           CALL 'EXTM0082' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0282.
           ADD 1974 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 3666, 2147483647)
           MOVE WS-HASH TO SCB-A-000283
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0083' TO WS-CALL-CODE
           CALL 'EXTM0083' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0283.
           ADD 1981 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 3679, 2147483647)
           MOVE WS-HASH TO SCB-A-000284
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0084' TO WS-CALL-CODE
           CALL 'EXTM0084' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0284.
           ADD 1988 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 3692, 2147483647)
           MOVE WS-HASH TO SCB-A-000285
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0085' TO WS-CALL-CODE
           CALL 'EXTM0085' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0285.
           ADD 1995 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 3705, 2147483647)
           MOVE WS-HASH TO SCB-A-000286
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0086' TO WS-CALL-CODE
           CALL 'EXTM0086' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0286.
           ADD 2002 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 3718, 2147483647)
           MOVE WS-HASH TO SCB-A-000287
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0087' TO WS-CALL-CODE
           CALL 'EXTM0087' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0287.
           ADD 2009 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 3731, 2147483647)
           MOVE WS-HASH TO SCB-A-000288
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0088' TO WS-CALL-CODE
           CALL 'EXTM0088' USING WS-CALL-AREA
           PERFORM VALIDATE-0287.
       VALIDATE-0287.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0288.
           ADD 2016 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 3744, 2147483647)
           MOVE WS-HASH TO SCB-A-000289
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0089' TO WS-CALL-CODE
           CALL 'EXTM0089' USING WS-CALL-AREA
           PERFORM AUX-0288.
       AUX-0288.
           MOVE 'AUX-0288' TO WS-CALL-DATA(1:12)
           ADD 94 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0289.
           ADD 2023 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 3757, 2147483647)
           MOVE WS-HASH TO SCB-A-000290
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0090' TO WS-CALL-CODE
           CALL 'EXTM0090' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0290.
           ADD 2030 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 3770, 2147483647)
           MOVE WS-HASH TO SCB-A-000291
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0091' TO WS-CALL-CODE
           CALL 'EXTM0091' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0291.
           ADD 2037 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 3783, 2147483647)
           MOVE WS-HASH TO SCB-A-000292
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0092' TO WS-CALL-CODE
           CALL 'EXTM0092' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0292.
           ADD 2044 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 3796, 2147483647)
           MOVE WS-HASH TO SCB-A-000293
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0093' TO WS-CALL-CODE
           CALL 'EXTM0093' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0293.
           ADD 2051 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 3809, 2147483647)
           MOVE WS-HASH TO SCB-A-000294
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0094' TO WS-CALL-CODE
           CALL 'EXTM0094' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0294.
           ADD 2058 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 3822, 2147483647)
           MOVE WS-HASH TO SCB-A-000295
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0095' TO WS-CALL-CODE
           CALL 'EXTM0095' USING WS-CALL-AREA
           PERFORM VALIDATE-0294.
       VALIDATE-0294.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0295.
           ADD 2065 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 3835, 2147483647)
           MOVE WS-HASH TO SCB-A-000296
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0096' TO WS-CALL-CODE
           CALL 'EXTM0096' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0296.
           ADD 2072 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 3848, 2147483647)
           MOVE WS-HASH TO SCB-A-000297
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0097' TO WS-CALL-CODE
           CALL 'EXTM0097' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0297.
           ADD 2079 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 3861, 2147483647)
           MOVE WS-HASH TO SCB-A-000298
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0098' TO WS-CALL-CODE
           CALL 'EXTM0098' USING WS-CALL-AREA
           PERFORM AUX-0297.
       AUX-0297.
           MOVE 'AUX-0297' TO WS-CALL-DATA(1:12)
           ADD 6 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0298.
           ADD 2086 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 3874, 2147483647)
           MOVE WS-HASH TO SCB-A-000299
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0099' TO WS-CALL-CODE
           CALL 'EXTM0099' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0299.
           ADD 2093 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 3887, 2147483647)
           MOVE WS-HASH TO SCB-A-000300
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0100' TO WS-CALL-CODE
           CALL 'EXTM0100' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0300.
           ADD 2100 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 3900, 2147483647)
           MOVE WS-HASH TO SCB-A-000301
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0101' TO WS-CALL-CODE
           CALL 'EXTM0101' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0301.
           ADD 2107 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 3913, 2147483647)
           MOVE WS-HASH TO SCB-A-000302
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0102' TO WS-CALL-CODE
           CALL 'EXTM0102' USING WS-CALL-AREA
           PERFORM VALIDATE-0301.
       VALIDATE-0301.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0302.
           ADD 2114 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 3926, 2147483647)
           MOVE WS-HASH TO SCB-A-000303
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0103' TO WS-CALL-CODE
           CALL 'EXTM0103' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0303.
           ADD 2121 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 3939, 2147483647)
           MOVE WS-HASH TO SCB-A-000304
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0104' TO WS-CALL-CODE
           CALL 'EXTM0104' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0304.
           ADD 2128 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 3952, 2147483647)
           MOVE WS-HASH TO SCB-A-000305
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0105' TO WS-CALL-CODE
           CALL 'EXTM0105' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0305.
           ADD 2135 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 3965, 2147483647)
           MOVE WS-HASH TO SCB-A-000306
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0106' TO WS-CALL-CODE
           CALL 'EXTM0106' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0306.
           ADD 2142 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 3978, 2147483647)
           MOVE WS-HASH TO SCB-A-000307
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0107' TO WS-CALL-CODE
           CALL 'EXTM0107' USING WS-CALL-AREA
           PERFORM AUX-0306.
       AUX-0306.
           MOVE 'AUX-0306' TO WS-CALL-DATA(1:12)
           ADD 15 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0307.
           ADD 2149 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 3991, 2147483647)
           MOVE WS-HASH TO SCB-A-000308
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0108' TO WS-CALL-CODE
           CALL 'EXTM0108' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0308.
           ADD 2156 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 4004, 2147483647)
           MOVE WS-HASH TO SCB-A-000309
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0109' TO WS-CALL-CODE
           CALL 'EXTM0109' USING WS-CALL-AREA
           PERFORM VALIDATE-0308.
       VALIDATE-0308.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0309.
           ADD 2163 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 4017, 2147483647)
           MOVE WS-HASH TO SCB-A-000310
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0110' TO WS-CALL-CODE
           CALL 'EXTM0110' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0310.
           ADD 2170 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 4030, 2147483647)
           MOVE WS-HASH TO SCB-A-000311
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0111' TO WS-CALL-CODE
           CALL 'EXTM0111' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0311.
           ADD 2177 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 4043, 2147483647)
           MOVE WS-HASH TO SCB-A-000312
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0112' TO WS-CALL-CODE
           CALL 'EXTM0112' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0312.
           ADD 2184 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 4056, 2147483647)
           MOVE WS-HASH TO SCB-A-000313
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0113' TO WS-CALL-CODE
           CALL 'EXTM0113' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0313.
           ADD 2191 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 4069, 2147483647)
           MOVE WS-HASH TO SCB-A-000314
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0114' TO WS-CALL-CODE
           CALL 'EXTM0114' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0314.
           ADD 2198 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 4082, 2147483647)
           MOVE WS-HASH TO SCB-A-000315
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0115' TO WS-CALL-CODE
           CALL 'EXTM0115' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0315.
           ADD 2205 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 4095, 2147483647)
           MOVE WS-HASH TO SCB-A-000316
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0116' TO WS-CALL-CODE
           CALL 'EXTM0116' USING WS-CALL-AREA
           PERFORM AUX-0315.
       AUX-0315.
           MOVE 'AUX-0315' TO WS-CALL-DATA(1:12)
           ADD 24 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0316.
           ADD 2212 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 4108, 2147483647)
           MOVE WS-HASH TO SCB-A-000317
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0117' TO WS-CALL-CODE
           CALL 'EXTM0117' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0317.
           ADD 2219 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 4121, 2147483647)
           MOVE WS-HASH TO SCB-A-000318
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0118' TO WS-CALL-CODE
           CALL 'EXTM0118' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0318.
           ADD 2226 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 4134, 2147483647)
           MOVE WS-HASH TO SCB-A-000319
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0119' TO WS-CALL-CODE
           CALL 'EXTM0119' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0319.
           ADD 2233 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 4147, 2147483647)
           MOVE WS-HASH TO SCB-A-000320
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0120' TO WS-CALL-CODE
           CALL 'EXTM0120' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0320.
           ADD 2240 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 4160, 2147483647)
           MOVE WS-HASH TO SCB-A-000321
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0121' TO WS-CALL-CODE
           CALL 'EXTM0121' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0321.
           ADD 2247 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 4173, 2147483647)
           MOVE WS-HASH TO SCB-A-000322
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0122' TO WS-CALL-CODE
           CALL 'EXTM0122' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0322.
           ADD 2254 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 4186, 2147483647)
           MOVE WS-HASH TO SCB-A-000323
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0123' TO WS-CALL-CODE
           CALL 'EXTM0123' USING WS-CALL-AREA
           PERFORM VALIDATE-0322.
       VALIDATE-0322.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0323.
           ADD 2261 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 4199, 2147483647)
           MOVE WS-HASH TO SCB-A-000324
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0124' TO WS-CALL-CODE
           CALL 'EXTM0124' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0324.
           ADD 2268 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 4212, 2147483647)
           MOVE WS-HASH TO SCB-A-000325
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0125' TO WS-CALL-CODE
           CALL 'EXTM0125' USING WS-CALL-AREA
           PERFORM AUX-0324.
       AUX-0324.
           MOVE 'AUX-0324' TO WS-CALL-DATA(1:12)
           ADD 33 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0325.
           ADD 2275 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 4225, 2147483647)
           MOVE WS-HASH TO SCB-A-000326
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0126' TO WS-CALL-CODE
           CALL 'EXTM0126' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0326.
           ADD 2282 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 4238, 2147483647)
           MOVE WS-HASH TO SCB-A-000327
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0127' TO WS-CALL-CODE
           CALL 'EXTM0127' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0327.
           ADD 2289 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 4251, 2147483647)
           MOVE WS-HASH TO SCB-A-000328
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0128' TO WS-CALL-CODE
           CALL 'EXTM0128' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0328.
           ADD 2296 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 4264, 2147483647)
           MOVE WS-HASH TO SCB-A-000329
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0129' TO WS-CALL-CODE
           CALL 'EXTM0129' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0329.
           ADD 2303 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 4277, 2147483647)
           MOVE WS-HASH TO SCB-A-000330
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0130' TO WS-CALL-CODE
           CALL 'EXTM0130' USING WS-CALL-AREA
           PERFORM VALIDATE-0329.
       VALIDATE-0329.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0330.
           ADD 2310 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 4290, 2147483647)
           MOVE WS-HASH TO SCB-A-000331
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0131' TO WS-CALL-CODE
           CALL 'EXTM0131' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0331.
           ADD 2317 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 4303, 2147483647)
           MOVE WS-HASH TO SCB-A-000332
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0132' TO WS-CALL-CODE
           CALL 'EXTM0132' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0332.
           ADD 2324 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 4316, 2147483647)
           MOVE WS-HASH TO SCB-A-000333
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0133' TO WS-CALL-CODE
           CALL 'EXTM0133' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0333.
           ADD 2331 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 4329, 2147483647)
           MOVE WS-HASH TO SCB-A-000334
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0134' TO WS-CALL-CODE
           CALL 'EXTM0134' USING WS-CALL-AREA
           PERFORM AUX-0333.
       AUX-0333.
           MOVE 'AUX-0333' TO WS-CALL-DATA(1:12)
           ADD 42 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0334.
           ADD 2338 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 4342, 2147483647)
           MOVE WS-HASH TO SCB-A-000335
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0135' TO WS-CALL-CODE
           CALL 'EXTM0135' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0335.
           ADD 2345 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 4355, 2147483647)
           MOVE WS-HASH TO SCB-A-000336
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0136' TO WS-CALL-CODE
           CALL 'EXTM0136' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0336.
           ADD 2352 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 4368, 2147483647)
           MOVE WS-HASH TO SCB-A-000337
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0137' TO WS-CALL-CODE
           CALL 'EXTM0137' USING WS-CALL-AREA
           PERFORM VALIDATE-0336.
       VALIDATE-0336.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0337.
           ADD 2359 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 4381, 2147483647)
           MOVE WS-HASH TO SCB-A-000338
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0138' TO WS-CALL-CODE
           CALL 'EXTM0138' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0338.
           ADD 2366 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 4394, 2147483647)
           MOVE WS-HASH TO SCB-A-000339
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0139' TO WS-CALL-CODE
           CALL 'EXTM0139' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0339.
           ADD 2373 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 4407, 2147483647)
           MOVE WS-HASH TO SCB-A-000340
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0140' TO WS-CALL-CODE
           CALL 'EXTM0140' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0340.
           ADD 2380 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 4420, 2147483647)
           MOVE WS-HASH TO SCB-A-000341
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0141' TO WS-CALL-CODE
           CALL 'EXTM0141' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0341.
           ADD 2387 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 4433, 2147483647)
           MOVE WS-HASH TO SCB-A-000342
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0142' TO WS-CALL-CODE
           CALL 'EXTM0142' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0342.
           ADD 2394 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 4446, 2147483647)
           MOVE WS-HASH TO SCB-A-000343
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0143' TO WS-CALL-CODE
           CALL 'EXTM0143' USING WS-CALL-AREA
           PERFORM AUX-0342.
       AUX-0342.
           MOVE 'AUX-0342' TO WS-CALL-DATA(1:12)
           ADD 51 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0343.
           ADD 2401 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 4459, 2147483647)
           MOVE WS-HASH TO SCB-A-000344
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0144' TO WS-CALL-CODE
           CALL 'EXTM0144' USING WS-CALL-AREA
           PERFORM VALIDATE-0343.
       VALIDATE-0343.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0344.
           ADD 2408 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 4472, 2147483647)
           MOVE WS-HASH TO SCB-A-000345
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0145' TO WS-CALL-CODE
           CALL 'EXTM0145' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0345.
           ADD 2415 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 4485, 2147483647)
           MOVE WS-HASH TO SCB-A-000346
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0146' TO WS-CALL-CODE
           CALL 'EXTM0146' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0346.
           ADD 2422 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 4498, 2147483647)
           MOVE WS-HASH TO SCB-A-000347
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0147' TO WS-CALL-CODE
           CALL 'EXTM0147' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0347.
           ADD 2429 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 4511, 2147483647)
           MOVE WS-HASH TO SCB-A-000348
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0148' TO WS-CALL-CODE
           CALL 'EXTM0148' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0348.
           ADD 2436 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 4524, 2147483647)
           MOVE WS-HASH TO SCB-A-000349
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0149' TO WS-CALL-CODE
           CALL 'EXTM0149' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0349.
           ADD 2443 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 4537, 2147483647)
           MOVE WS-HASH TO SCB-A-000350
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0150' TO WS-CALL-CODE
           CALL 'EXTM0150' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0350.
           ADD 2450 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 4550, 2147483647)
           MOVE WS-HASH TO SCB-A-000351
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0151' TO WS-CALL-CODE
           CALL 'EXTM0151' USING WS-CALL-AREA
           PERFORM VALIDATE-0350.
       VALIDATE-0350.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0351.
           ADD 2457 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 4563, 2147483647)
           MOVE WS-HASH TO SCB-A-000352
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0152' TO WS-CALL-CODE
           CALL 'EXTM0152' USING WS-CALL-AREA
           PERFORM AUX-0351.
       AUX-0351.
           MOVE 'AUX-0351' TO WS-CALL-DATA(1:12)
           ADD 60 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0352.
           ADD 2464 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 4576, 2147483647)
           MOVE WS-HASH TO SCB-A-000353
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0153' TO WS-CALL-CODE
           CALL 'EXTM0153' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0353.
           ADD 2471 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 4589, 2147483647)
           MOVE WS-HASH TO SCB-A-000354
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0154' TO WS-CALL-CODE
           CALL 'EXTM0154' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0354.
           ADD 2478 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 4602, 2147483647)
           MOVE WS-HASH TO SCB-A-000355
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0155' TO WS-CALL-CODE
           CALL 'EXTM0155' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0355.
           ADD 2485 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 4615, 2147483647)
           MOVE WS-HASH TO SCB-A-000356
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0156' TO WS-CALL-CODE
           CALL 'EXTM0156' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0356.
           ADD 2492 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 4628, 2147483647)
           MOVE WS-HASH TO SCB-A-000357
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0157' TO WS-CALL-CODE
           CALL 'EXTM0157' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0357.
           ADD 2499 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 4641, 2147483647)
           MOVE WS-HASH TO SCB-A-000358
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0158' TO WS-CALL-CODE
           CALL 'EXTM0158' USING WS-CALL-AREA
           PERFORM VALIDATE-0357.
       VALIDATE-0357.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0358.
           ADD 2506 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 4654, 2147483647)
           MOVE WS-HASH TO SCB-A-000359
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0159' TO WS-CALL-CODE
           CALL 'EXTM0159' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0359.
           ADD 2513 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 4667, 2147483647)
           MOVE WS-HASH TO SCB-A-000360
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0160' TO WS-CALL-CODE
           CALL 'EXTM0160' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0360.
           ADD 2520 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 4680, 2147483647)
           MOVE WS-HASH TO SCB-A-000361
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0161' TO WS-CALL-CODE
           CALL 'EXTM0161' USING WS-CALL-AREA
           PERFORM AUX-0360.
       AUX-0360.
           MOVE 'AUX-0360' TO WS-CALL-DATA(1:12)
           ADD 69 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0361.
           ADD 2527 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 4693, 2147483647)
           MOVE WS-HASH TO SCB-A-000362
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0162' TO WS-CALL-CODE
           CALL 'EXTM0162' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0362.
           ADD 2534 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 4706, 2147483647)
           MOVE WS-HASH TO SCB-A-000363
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0163' TO WS-CALL-CODE
           CALL 'EXTM0163' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0363.
           ADD 2541 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 4719, 2147483647)
           MOVE WS-HASH TO SCB-A-000364
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0164' TO WS-CALL-CODE
           CALL 'EXTM0164' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0364.
           ADD 2548 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 4732, 2147483647)
           MOVE WS-HASH TO SCB-A-000365
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0165' TO WS-CALL-CODE
           CALL 'EXTM0165' USING WS-CALL-AREA
           PERFORM VALIDATE-0364.
       VALIDATE-0364.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0365.
           ADD 2555 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 4745, 2147483647)
           MOVE WS-HASH TO SCB-A-000366
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0166' TO WS-CALL-CODE
           CALL 'EXTM0166' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0366.
           ADD 2562 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 4758, 2147483647)
           MOVE WS-HASH TO SCB-A-000367
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0167' TO WS-CALL-CODE
           CALL 'EXTM0167' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0367.
           ADD 2569 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 4771, 2147483647)
           MOVE WS-HASH TO SCB-A-000368
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0168' TO WS-CALL-CODE
           CALL 'EXTM0168' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0368.
           ADD 2576 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 4784, 2147483647)
           MOVE WS-HASH TO SCB-A-000369
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0169' TO WS-CALL-CODE
           CALL 'EXTM0169' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0369.
           ADD 2583 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 4797, 2147483647)
           MOVE WS-HASH TO SCB-A-000370
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0170' TO WS-CALL-CODE
           CALL 'EXTM0170' USING WS-CALL-AREA
           PERFORM AUX-0369.
       AUX-0369.
           MOVE 'AUX-0369' TO WS-CALL-DATA(1:12)
           ADD 78 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0370.
           ADD 2590 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 4810, 2147483647)
           MOVE WS-HASH TO SCB-A-000371
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0171' TO WS-CALL-CODE
           CALL 'EXTM0171' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0371.
           ADD 2597 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 4823, 2147483647)
           MOVE WS-HASH TO SCB-A-000372
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0172' TO WS-CALL-CODE
           CALL 'EXTM0172' USING WS-CALL-AREA
           PERFORM VALIDATE-0371.
       VALIDATE-0371.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0372.
           ADD 2604 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 4836, 2147483647)
           MOVE WS-HASH TO SCB-A-000373
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0173' TO WS-CALL-CODE
           CALL 'EXTM0173' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0373.
           ADD 2611 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 4849, 2147483647)
           MOVE WS-HASH TO SCB-A-000374
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0174' TO WS-CALL-CODE
           CALL 'EXTM0174' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0374.
           ADD 2618 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 4862, 2147483647)
           MOVE WS-HASH TO SCB-A-000375
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0175' TO WS-CALL-CODE
           CALL 'EXTM0175' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0375.
           ADD 2625 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 4875, 2147483647)
           MOVE WS-HASH TO SCB-A-000376
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0176' TO WS-CALL-CODE
           CALL 'EXTM0176' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0376.
           ADD 2632 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 4888, 2147483647)
           MOVE WS-HASH TO SCB-A-000377
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0177' TO WS-CALL-CODE
           CALL 'EXTM0177' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0377.
           ADD 2639 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 4901, 2147483647)
           MOVE WS-HASH TO SCB-A-000378
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0178' TO WS-CALL-CODE
           CALL 'EXTM0178' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0378.
           ADD 2646 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 4914, 2147483647)
           MOVE WS-HASH TO SCB-A-000379
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0179' TO WS-CALL-CODE
           CALL 'EXTM0179' USING WS-CALL-AREA
           PERFORM AUX-0378.
       AUX-0378.
           MOVE 'AUX-0378' TO WS-CALL-DATA(1:12)
           ADD 87 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0379.
           ADD 2653 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 4927, 2147483647)
           MOVE WS-HASH TO SCB-A-000380
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0180' TO WS-CALL-CODE
           CALL 'EXTM0180' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0380.
           ADD 2660 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 4940, 2147483647)
           MOVE WS-HASH TO SCB-A-000381
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0181' TO WS-CALL-CODE
           CALL 'EXTM0181' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0381.
           ADD 2667 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 4953, 2147483647)
           MOVE WS-HASH TO SCB-A-000382
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0182' TO WS-CALL-CODE
           CALL 'EXTM0182' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0382.
           ADD 2674 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 4966, 2147483647)
           MOVE WS-HASH TO SCB-A-000383
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0183' TO WS-CALL-CODE
           CALL 'EXTM0183' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0383.
           ADD 2681 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 4979, 2147483647)
           MOVE WS-HASH TO SCB-A-000384
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0184' TO WS-CALL-CODE
           CALL 'EXTM0184' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0384.
           ADD 2688 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 4992, 2147483647)
           MOVE WS-HASH TO SCB-A-000385
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0185' TO WS-CALL-CODE
           CALL 'EXTM0185' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0385.
           ADD 2695 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 5005, 2147483647)
           MOVE WS-HASH TO SCB-A-000386
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0186' TO WS-CALL-CODE
           CALL 'EXTM0186' USING WS-CALL-AREA
           PERFORM VALIDATE-0385.
       VALIDATE-0385.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0386.
           ADD 2702 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 5018, 2147483647)
           MOVE WS-HASH TO SCB-A-000387
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0187' TO WS-CALL-CODE
           CALL 'EXTM0187' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0387.
           ADD 2709 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 5031, 2147483647)
           MOVE WS-HASH TO SCB-A-000388
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0188' TO WS-CALL-CODE
           CALL 'EXTM0188' USING WS-CALL-AREA
           PERFORM AUX-0387.
       AUX-0387.
           MOVE 'AUX-0387' TO WS-CALL-DATA(1:12)
           ADD 96 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0388.
           ADD 2716 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 5044, 2147483647)
           MOVE WS-HASH TO SCB-A-000389
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0189' TO WS-CALL-CODE
           CALL 'EXTM0189' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0389.
           ADD 2723 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 5057, 2147483647)
           MOVE WS-HASH TO SCB-A-000390
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0190' TO WS-CALL-CODE
           CALL 'EXTM0190' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0390.
           ADD 2730 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 5070, 2147483647)
           MOVE WS-HASH TO SCB-A-000391
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0191' TO WS-CALL-CODE
           CALL 'EXTM0191' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0391.
           ADD 2737 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 5083, 2147483647)
           MOVE WS-HASH TO SCB-A-000392
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0192' TO WS-CALL-CODE
           CALL 'EXTM0192' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0392.
           ADD 2744 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 5096, 2147483647)
           MOVE WS-HASH TO SCB-A-000393
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0193' TO WS-CALL-CODE
           CALL 'EXTM0193' USING WS-CALL-AREA
           PERFORM VALIDATE-0392.
       VALIDATE-0392.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0393.
           ADD 2751 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 5109, 2147483647)
           MOVE WS-HASH TO SCB-A-000394
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0194' TO WS-CALL-CODE
           CALL 'EXTM0194' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0394.
           ADD 2758 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 5122, 2147483647)
           MOVE WS-HASH TO SCB-A-000395
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0195' TO WS-CALL-CODE
           CALL 'EXTM0195' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0395.
           ADD 2765 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 5135, 2147483647)
           MOVE WS-HASH TO SCB-A-000396
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0196' TO WS-CALL-CODE
           CALL 'EXTM0196' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0396.
           ADD 2772 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 5148, 2147483647)
           MOVE WS-HASH TO SCB-A-000397
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0197' TO WS-CALL-CODE
           CALL 'EXTM0197' USING WS-CALL-AREA
           PERFORM AUX-0396.
       AUX-0396.
           MOVE 'AUX-0396' TO WS-CALL-DATA(1:12)
           ADD 8 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0397.
           ADD 2779 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 5161, 2147483647)
           MOVE WS-HASH TO SCB-A-000398
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0198' TO WS-CALL-CODE
           CALL 'EXTM0198' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0398.
           ADD 2786 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 5174, 2147483647)
           MOVE WS-HASH TO SCB-A-000399
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0199' TO WS-CALL-CODE
           CALL 'EXTM0199' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0399.
           ADD 2793 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 5187, 2147483647)
           MOVE WS-HASH TO SCB-A-000400
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0200' TO WS-CALL-CODE
           CALL 'EXTM0200' USING WS-CALL-AREA
           PERFORM VALIDATE-0399.
       VALIDATE-0399.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0400.
           ADD 2800 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 5200, 2147483647)
           MOVE WS-HASH TO SCB-A-000401
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0201' TO WS-CALL-CODE
           CALL 'EXTM0201' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0401.
           ADD 2807 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 5213, 2147483647)
           MOVE WS-HASH TO SCB-A-000402
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0202' TO WS-CALL-CODE
           CALL 'EXTM0202' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0402.
           ADD 2814 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 5226, 2147483647)
           MOVE WS-HASH TO SCB-A-000403
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0203' TO WS-CALL-CODE
           CALL 'EXTM0203' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0403.
           ADD 2821 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 5239, 2147483647)
           MOVE WS-HASH TO SCB-A-000404
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0204' TO WS-CALL-CODE
           CALL 'EXTM0204' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0404.
           ADD 2828 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 5252, 2147483647)
           MOVE WS-HASH TO SCB-A-000405
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0205' TO WS-CALL-CODE
           CALL 'EXTM0205' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0405.
           ADD 2835 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 5265, 2147483647)
           MOVE WS-HASH TO SCB-A-000406
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0206' TO WS-CALL-CODE
           CALL 'EXTM0206' USING WS-CALL-AREA
           PERFORM AUX-0405.
       AUX-0405.
           MOVE 'AUX-0405' TO WS-CALL-DATA(1:12)
           ADD 17 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0406.
           ADD 2842 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 5278, 2147483647)
           MOVE WS-HASH TO SCB-A-000407
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0207' TO WS-CALL-CODE
           CALL 'EXTM0207' USING WS-CALL-AREA
           PERFORM VALIDATE-0406.
       VALIDATE-0406.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0407.
           ADD 2849 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 5291, 2147483647)
           MOVE WS-HASH TO SCB-A-000408
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0208' TO WS-CALL-CODE
           CALL 'EXTM0208' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0408.
           ADD 2856 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 5304, 2147483647)
           MOVE WS-HASH TO SCB-A-000409
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0209' TO WS-CALL-CODE
           CALL 'EXTM0209' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0409.
           ADD 2863 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 5317, 2147483647)
           MOVE WS-HASH TO SCB-A-000410
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0210' TO WS-CALL-CODE
           CALL 'EXTM0210' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0410.
           ADD 2870 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 5330, 2147483647)
           MOVE WS-HASH TO SCB-A-000411
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0211' TO WS-CALL-CODE
           CALL 'EXTM0211' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0411.
           ADD 2877 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 5343, 2147483647)
           MOVE WS-HASH TO SCB-A-000412
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0212' TO WS-CALL-CODE
           CALL 'EXTM0212' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0412.
           ADD 2884 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 5356, 2147483647)
           MOVE WS-HASH TO SCB-A-000413
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0213' TO WS-CALL-CODE
           CALL 'EXTM0213' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0413.
           ADD 2891 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 5369, 2147483647)
           MOVE WS-HASH TO SCB-A-000414
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0214' TO WS-CALL-CODE
           CALL 'EXTM0214' USING WS-CALL-AREA
           PERFORM VALIDATE-0413.
       VALIDATE-0413.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0414.
           ADD 2898 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 5382, 2147483647)
           MOVE WS-HASH TO SCB-A-000415
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0215' TO WS-CALL-CODE
           CALL 'EXTM0215' USING WS-CALL-AREA
           PERFORM AUX-0414.
       AUX-0414.
           MOVE 'AUX-0414' TO WS-CALL-DATA(1:12)
           ADD 26 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0415.
           ADD 2905 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 5395, 2147483647)
           MOVE WS-HASH TO SCB-A-000416
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0216' TO WS-CALL-CODE
           CALL 'EXTM0216' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0416.
           ADD 2912 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 5408, 2147483647)
           MOVE WS-HASH TO SCB-A-000417
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0217' TO WS-CALL-CODE
           CALL 'EXTM0217' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0417.
           ADD 2919 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 5421, 2147483647)
           MOVE WS-HASH TO SCB-A-000418
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0218' TO WS-CALL-CODE
           CALL 'EXTM0218' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0418.
           ADD 2926 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 5434, 2147483647)
           MOVE WS-HASH TO SCB-A-000419
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0219' TO WS-CALL-CODE
           CALL 'EXTM0219' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0419.
           ADD 2933 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 5447, 2147483647)
           MOVE WS-HASH TO SCB-A-000420
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0220' TO WS-CALL-CODE
           CALL 'EXTM0220' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0420.
           ADD 2940 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 5460, 2147483647)
           MOVE WS-HASH TO SCB-A-000421
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0221' TO WS-CALL-CODE
           CALL 'EXTM0221' USING WS-CALL-AREA
           PERFORM VALIDATE-0420.
       VALIDATE-0420.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0421.
           ADD 2947 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 5473, 2147483647)
           MOVE WS-HASH TO SCB-A-000422
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0222' TO WS-CALL-CODE
           CALL 'EXTM0222' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0422.
           ADD 2954 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 5486, 2147483647)
           MOVE WS-HASH TO SCB-A-000423
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0223' TO WS-CALL-CODE
           CALL 'EXTM0223' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0423.
           ADD 2961 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 5499, 2147483647)
           MOVE WS-HASH TO SCB-A-000424
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0224' TO WS-CALL-CODE
           CALL 'EXTM0224' USING WS-CALL-AREA
           PERFORM AUX-0423.
       AUX-0423.
           MOVE 'AUX-0423' TO WS-CALL-DATA(1:12)
           ADD 35 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0424.
           ADD 2968 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 5512, 2147483647)
           MOVE WS-HASH TO SCB-A-000425
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0225' TO WS-CALL-CODE
           CALL 'EXTM0225' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0425.
           ADD 2975 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 5525, 2147483647)
           MOVE WS-HASH TO SCB-A-000426
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0226' TO WS-CALL-CODE
           CALL 'EXTM0226' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0426.
           ADD 2982 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 5538, 2147483647)
           MOVE WS-HASH TO SCB-A-000427
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0227' TO WS-CALL-CODE
           CALL 'EXTM0227' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0427.
           ADD 2989 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 5551, 2147483647)
           MOVE WS-HASH TO SCB-A-000428
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0228' TO WS-CALL-CODE
           CALL 'EXTM0228' USING WS-CALL-AREA
           PERFORM VALIDATE-0427.
       VALIDATE-0427.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0428.
           ADD 2996 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 5564, 2147483647)
           MOVE WS-HASH TO SCB-A-000429
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0229' TO WS-CALL-CODE
           CALL 'EXTM0229' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0429.
           ADD 3003 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 5577, 2147483647)
           MOVE WS-HASH TO SCB-A-000430
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0230' TO WS-CALL-CODE
           CALL 'EXTM0230' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0430.
           ADD 3010 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 5590, 2147483647)
           MOVE WS-HASH TO SCB-A-000431
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0231' TO WS-CALL-CODE
           CALL 'EXTM0231' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0431.
           ADD 3017 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 5603, 2147483647)
           MOVE WS-HASH TO SCB-A-000432
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0232' TO WS-CALL-CODE
           CALL 'EXTM0232' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0432.
           ADD 3024 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 5616, 2147483647)
           MOVE WS-HASH TO SCB-A-000433
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0233' TO WS-CALL-CODE
           CALL 'EXTM0233' USING WS-CALL-AREA
           PERFORM AUX-0432.
       AUX-0432.
           MOVE 'AUX-0432' TO WS-CALL-DATA(1:12)
           ADD 44 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0433.
           ADD 3031 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 5629, 2147483647)
           MOVE WS-HASH TO SCB-A-000434
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0234' TO WS-CALL-CODE
           CALL 'EXTM0234' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0434.
           ADD 3038 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 5642, 2147483647)
           MOVE WS-HASH TO SCB-A-000435
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0235' TO WS-CALL-CODE
           CALL 'EXTM0235' USING WS-CALL-AREA
           PERFORM VALIDATE-0434.
       VALIDATE-0434.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0435.
           ADD 3045 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 5655, 2147483647)
           MOVE WS-HASH TO SCB-A-000436
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0236' TO WS-CALL-CODE
           CALL 'EXTM0236' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0436.
           ADD 3052 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 5668, 2147483647)
           MOVE WS-HASH TO SCB-A-000437
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0237' TO WS-CALL-CODE
           CALL 'EXTM0237' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0437.
           ADD 3059 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 5681, 2147483647)
           MOVE WS-HASH TO SCB-A-000438
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0238' TO WS-CALL-CODE
           CALL 'EXTM0238' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0438.
           ADD 3066 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 5694, 2147483647)
           MOVE WS-HASH TO SCB-A-000439
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0239' TO WS-CALL-CODE
           CALL 'EXTM0239' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0439.
           ADD 3073 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 5707, 2147483647)
           MOVE WS-HASH TO SCB-A-000440
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0240' TO WS-CALL-CODE
           CALL 'EXTM0240' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0440.
           ADD 3080 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 5720, 2147483647)
           MOVE WS-HASH TO SCB-A-000441
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0241' TO WS-CALL-CODE
           CALL 'EXTM0241' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0441.
           ADD 3087 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 5733, 2147483647)
           MOVE WS-HASH TO SCB-A-000442
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0242' TO WS-CALL-CODE
           CALL 'EXTM0242' USING WS-CALL-AREA
           PERFORM AUX-0441.
       AUX-0441.
           MOVE 'AUX-0441' TO WS-CALL-DATA(1:12)
           ADD 53 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0442.
           ADD 3094 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 5746, 2147483647)
           MOVE WS-HASH TO SCB-A-000443
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0243' TO WS-CALL-CODE
           CALL 'EXTM0243' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0443.
           ADD 3101 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 5759, 2147483647)
           MOVE WS-HASH TO SCB-A-000444
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0244' TO WS-CALL-CODE
           CALL 'EXTM0244' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0444.
           ADD 3108 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 5772, 2147483647)
           MOVE WS-HASH TO SCB-A-000445
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0245' TO WS-CALL-CODE
           CALL 'EXTM0245' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0445.
           ADD 3115 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 5785, 2147483647)
           MOVE WS-HASH TO SCB-A-000446
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0246' TO WS-CALL-CODE
           CALL 'EXTM0246' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0446.
           ADD 3122 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 5798, 2147483647)
           MOVE WS-HASH TO SCB-A-000447
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0247' TO WS-CALL-CODE
           CALL 'EXTM0247' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0447.
           ADD 3129 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 5811, 2147483647)
           MOVE WS-HASH TO SCB-A-000448
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0248' TO WS-CALL-CODE
           CALL 'EXTM0248' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0448.
           ADD 3136 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 5824, 2147483647)
           MOVE WS-HASH TO SCB-A-000449
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0249' TO WS-CALL-CODE
           CALL 'EXTM0249' USING WS-CALL-AREA
           PERFORM VALIDATE-0448.
       VALIDATE-0448.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0449.
           ADD 3143 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 5837, 2147483647)
           MOVE WS-HASH TO SCB-A-000450
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0250' TO WS-CALL-CODE
           CALL 'EXTM0250' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0450.
           ADD 3150 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 5850, 2147483647)
           MOVE WS-HASH TO SCB-A-000451
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0251' TO WS-CALL-CODE
           CALL 'EXTM0251' USING WS-CALL-AREA
           PERFORM AUX-0450.
       AUX-0450.
           MOVE 'AUX-0450' TO WS-CALL-DATA(1:12)
           ADD 62 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0451.
           ADD 3157 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 5863, 2147483647)
           MOVE WS-HASH TO SCB-A-000452
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0252' TO WS-CALL-CODE
           CALL 'EXTM0252' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0452.
           ADD 3164 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 5876, 2147483647)
           MOVE WS-HASH TO SCB-A-000453
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0253' TO WS-CALL-CODE
           CALL 'EXTM0253' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0453.
           ADD 3171 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 5889, 2147483647)
           MOVE WS-HASH TO SCB-A-000454
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0254' TO WS-CALL-CODE
           CALL 'EXTM0254' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0454.
           ADD 3178 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 5902, 2147483647)
           MOVE WS-HASH TO SCB-A-000455
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0255' TO WS-CALL-CODE
           CALL 'EXTM0255' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0455.
           ADD 3185 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 5915, 2147483647)
           MOVE WS-HASH TO SCB-A-000456
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0256' TO WS-CALL-CODE
           CALL 'EXTM0256' USING WS-CALL-AREA
           PERFORM VALIDATE-0455.
       VALIDATE-0455.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0456.
           ADD 3192 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 5928, 2147483647)
           MOVE WS-HASH TO SCB-A-000457
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0257' TO WS-CALL-CODE
           CALL 'EXTM0257' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0457.
           ADD 3199 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 5941, 2147483647)
           MOVE WS-HASH TO SCB-A-000458
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0258' TO WS-CALL-CODE
           CALL 'EXTM0258' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0458.
           ADD 3206 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 5954, 2147483647)
           MOVE WS-HASH TO SCB-A-000459
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0259' TO WS-CALL-CODE
           CALL 'EXTM0259' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0459.
           ADD 3213 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 5967, 2147483647)
           MOVE WS-HASH TO SCB-A-000460
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0260' TO WS-CALL-CODE
           CALL 'EXTM0260' USING WS-CALL-AREA
           PERFORM AUX-0459.
       AUX-0459.
           MOVE 'AUX-0459' TO WS-CALL-DATA(1:12)
           ADD 71 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0460.
           ADD 3220 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 5980, 2147483647)
           MOVE WS-HASH TO SCB-A-000461
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0261' TO WS-CALL-CODE
           CALL 'EXTM0261' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0461.
           ADD 3227 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 5993, 2147483647)
           MOVE WS-HASH TO SCB-A-000462
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0262' TO WS-CALL-CODE
           CALL 'EXTM0262' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0462.
           ADD 3234 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 6006, 2147483647)
           MOVE WS-HASH TO SCB-A-000463
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0263' TO WS-CALL-CODE
           CALL 'EXTM0263' USING WS-CALL-AREA
           PERFORM VALIDATE-0462.
       VALIDATE-0462.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0463.
           ADD 3241 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 6019, 2147483647)
           MOVE WS-HASH TO SCB-A-000464
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0264' TO WS-CALL-CODE
           CALL 'EXTM0264' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0464.
           ADD 3248 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 6032, 2147483647)
           MOVE WS-HASH TO SCB-A-000465
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0265' TO WS-CALL-CODE
           CALL 'EXTM0265' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0465.
           ADD 3255 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 6045, 2147483647)
           MOVE WS-HASH TO SCB-A-000466
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0266' TO WS-CALL-CODE
           CALL 'EXTM0266' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0466.
           ADD 3262 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 6058, 2147483647)
           MOVE WS-HASH TO SCB-A-000467
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0267' TO WS-CALL-CODE
           CALL 'EXTM0267' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0467.
           ADD 3269 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 6071, 2147483647)
           MOVE WS-HASH TO SCB-A-000468
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0268' TO WS-CALL-CODE
           CALL 'EXTM0268' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0468.
           ADD 3276 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 6084, 2147483647)
           MOVE WS-HASH TO SCB-A-000469
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0269' TO WS-CALL-CODE
           CALL 'EXTM0269' USING WS-CALL-AREA
           PERFORM AUX-0468.
       AUX-0468.
           MOVE 'AUX-0468' TO WS-CALL-DATA(1:12)
           ADD 80 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0469.
           ADD 3283 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 6097, 2147483647)
           MOVE WS-HASH TO SCB-A-000470
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0270' TO WS-CALL-CODE
           CALL 'EXTM0270' USING WS-CALL-AREA
           PERFORM VALIDATE-0469.
       VALIDATE-0469.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0470.
           ADD 3290 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 6110, 2147483647)
           MOVE WS-HASH TO SCB-A-000471
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0271' TO WS-CALL-CODE
           CALL 'EXTM0271' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0471.
           ADD 3297 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 6123, 2147483647)
           MOVE WS-HASH TO SCB-A-000472
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0272' TO WS-CALL-CODE
           CALL 'EXTM0272' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0472.
           ADD 3304 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 6136, 2147483647)
           MOVE WS-HASH TO SCB-A-000473
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0273' TO WS-CALL-CODE
           CALL 'EXTM0273' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0473.
           ADD 3311 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 6149, 2147483647)
           MOVE WS-HASH TO SCB-A-000474
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0274' TO WS-CALL-CODE
           CALL 'EXTM0274' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0474.
           ADD 3318 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 6162, 2147483647)
           MOVE WS-HASH TO SCB-A-000475
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0275' TO WS-CALL-CODE
           CALL 'EXTM0275' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0475.
           ADD 3325 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 6175, 2147483647)
           MOVE WS-HASH TO SCB-A-000476
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0276' TO WS-CALL-CODE
           CALL 'EXTM0276' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0476.
           ADD 3332 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 6188, 2147483647)
           MOVE WS-HASH TO SCB-A-000477
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0277' TO WS-CALL-CODE
           CALL 'EXTM0277' USING WS-CALL-AREA
           PERFORM VALIDATE-0476.
       VALIDATE-0476.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0477.
           ADD 3339 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 6201, 2147483647)
           MOVE WS-HASH TO SCB-A-000478
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0278' TO WS-CALL-CODE
           CALL 'EXTM0278' USING WS-CALL-AREA
           PERFORM AUX-0477.
       AUX-0477.
           MOVE 'AUX-0477' TO WS-CALL-DATA(1:12)
           ADD 89 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0478.
           ADD 3346 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 6214, 2147483647)
           MOVE WS-HASH TO SCB-A-000479
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0279' TO WS-CALL-CODE
           CALL 'EXTM0279' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0479.
           ADD 3353 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 6227, 2147483647)
           MOVE WS-HASH TO SCB-A-000480
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0280' TO WS-CALL-CODE
           CALL 'EXTM0280' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0480.
           ADD 3360 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 6240, 2147483647)
           MOVE WS-HASH TO SCB-A-000481
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0281' TO WS-CALL-CODE
           CALL 'EXTM0281' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0481.
           ADD 3367 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 6253, 2147483647)
           MOVE WS-HASH TO SCB-A-000482
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0282' TO WS-CALL-CODE
           CALL 'EXTM0282' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0482.
           ADD 3374 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 6266, 2147483647)
           MOVE WS-HASH TO SCB-A-000483
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0283' TO WS-CALL-CODE
           CALL 'EXTM0283' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0483.
           ADD 3381 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 6279, 2147483647)
           MOVE WS-HASH TO SCB-A-000484
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0284' TO WS-CALL-CODE
           CALL 'EXTM0284' USING WS-CALL-AREA
           PERFORM VALIDATE-0483.
       VALIDATE-0483.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0484.
           ADD 3388 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 6292, 2147483647)
           MOVE WS-HASH TO SCB-A-000485
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0285' TO WS-CALL-CODE
           CALL 'EXTM0285' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0485.
           ADD 3395 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 6305, 2147483647)
           MOVE WS-HASH TO SCB-A-000486
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0286' TO WS-CALL-CODE
           CALL 'EXTM0286' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0486.
           ADD 3402 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 6318, 2147483647)
           MOVE WS-HASH TO SCB-A-000487
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0287' TO WS-CALL-CODE
           CALL 'EXTM0287' USING WS-CALL-AREA
           PERFORM AUX-0486.
       AUX-0486.
           MOVE 'AUX-0486' TO WS-CALL-DATA(1:12)
           ADD 1 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0487.
           ADD 3409 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 6331, 2147483647)
           MOVE WS-HASH TO SCB-A-000488
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0288' TO WS-CALL-CODE
           CALL 'EXTM0288' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0488.
           ADD 3416 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 6344, 2147483647)
           MOVE WS-HASH TO SCB-A-000489
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0289' TO WS-CALL-CODE
           CALL 'EXTM0289' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0489.
           ADD 3423 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 6357, 2147483647)
           MOVE WS-HASH TO SCB-A-000490
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0290' TO WS-CALL-CODE
           CALL 'EXTM0290' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0490.
           ADD 3430 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 6370, 2147483647)
           MOVE WS-HASH TO SCB-A-000491
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0291' TO WS-CALL-CODE
           CALL 'EXTM0291' USING WS-CALL-AREA
           PERFORM VALIDATE-0490.
       VALIDATE-0490.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0491.
           ADD 3437 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 6383, 2147483647)
           MOVE WS-HASH TO SCB-A-000492
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0292' TO WS-CALL-CODE
           CALL 'EXTM0292' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0492.
           ADD 3444 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 6396, 2147483647)
           MOVE WS-HASH TO SCB-A-000493
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0293' TO WS-CALL-CODE
           CALL 'EXTM0293' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0493.
           ADD 3451 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 6409, 2147483647)
           MOVE WS-HASH TO SCB-A-000494
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0294' TO WS-CALL-CODE
           CALL 'EXTM0294' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0494.
           ADD 3458 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 6422, 2147483647)
           MOVE WS-HASH TO SCB-A-000495
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0295' TO WS-CALL-CODE
           CALL 'EXTM0295' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0495.
           ADD 3465 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 6435, 2147483647)
           MOVE WS-HASH TO SCB-A-000496
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0296' TO WS-CALL-CODE
           CALL 'EXTM0296' USING WS-CALL-AREA
           PERFORM AUX-0495.
       AUX-0495.
           MOVE 'AUX-0495' TO WS-CALL-DATA(1:12)
           ADD 10 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0496.
           ADD 3472 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 6448, 2147483647)
           MOVE WS-HASH TO SCB-A-000497
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0297' TO WS-CALL-CODE
           CALL 'EXTM0297' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0497.
           ADD 3479 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 6461, 2147483647)
           MOVE WS-HASH TO SCB-A-000498
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0298' TO WS-CALL-CODE
           CALL 'EXTM0298' USING WS-CALL-AREA
           PERFORM VALIDATE-0497.
       VALIDATE-0497.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0498.
           ADD 3486 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 6474, 2147483647)
           MOVE WS-HASH TO SCB-A-000499
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0299' TO WS-CALL-CODE
           CALL 'EXTM0299' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0499.
           ADD 3493 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 6487, 2147483647)
           MOVE WS-HASH TO SCB-A-000500
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0300' TO WS-CALL-CODE
           CALL 'EXTM0300' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0500.
           ADD 3500 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 6500, 2147483647)
           MOVE WS-HASH TO SCB-A-000001
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0301' TO WS-CALL-CODE
           CALL 'EXTM0301' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0501.
           ADD 3507 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 6513, 2147483647)
           MOVE WS-HASH TO SCB-A-000002
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0302' TO WS-CALL-CODE
           CALL 'EXTM0302' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0502.
           ADD 3514 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 6526, 2147483647)
           MOVE WS-HASH TO SCB-A-000003
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0303' TO WS-CALL-CODE
           CALL 'EXTM0303' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0503.
           ADD 3521 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 6539, 2147483647)
           MOVE WS-HASH TO SCB-A-000004
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0304' TO WS-CALL-CODE
           CALL 'EXTM0304' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0504.
           ADD 3528 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 6552, 2147483647)
           MOVE WS-HASH TO SCB-A-000005
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0305' TO WS-CALL-CODE
           CALL 'EXTM0305' USING WS-CALL-AREA
           PERFORM AUX-0504.
       AUX-0504.
           MOVE 'AUX-0504' TO WS-CALL-DATA(1:12)
           ADD 19 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0505.
           ADD 3535 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 6565, 2147483647)
           MOVE WS-HASH TO SCB-A-000006
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0306' TO WS-CALL-CODE
           CALL 'EXTM0306' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0506.
           ADD 3542 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 6578, 2147483647)
           MOVE WS-HASH TO SCB-A-000007
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0307' TO WS-CALL-CODE
           CALL 'EXTM0307' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0507.
           ADD 3549 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 6591, 2147483647)
           MOVE WS-HASH TO SCB-A-000008
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0308' TO WS-CALL-CODE
           CALL 'EXTM0308' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0508.
           ADD 3556 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 6604, 2147483647)
           MOVE WS-HASH TO SCB-A-000009
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0309' TO WS-CALL-CODE
           CALL 'EXTM0309' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0509.
           ADD 3563 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 6617, 2147483647)
           MOVE WS-HASH TO SCB-A-000010
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0310' TO WS-CALL-CODE
           CALL 'EXTM0310' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0510.
           ADD 3570 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 6630, 2147483647)
           MOVE WS-HASH TO SCB-A-000011
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0311' TO WS-CALL-CODE
           CALL 'EXTM0311' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0511.
           ADD 3577 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 6643, 2147483647)
           MOVE WS-HASH TO SCB-A-000012
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0312' TO WS-CALL-CODE
           CALL 'EXTM0312' USING WS-CALL-AREA
           PERFORM VALIDATE-0511.
       VALIDATE-0511.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0512.
           ADD 3584 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 6656, 2147483647)
           MOVE WS-HASH TO SCB-A-000013
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0313' TO WS-CALL-CODE
           CALL 'EXTM0313' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0513.
           ADD 3591 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 6669, 2147483647)
           MOVE WS-HASH TO SCB-A-000014
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0314' TO WS-CALL-CODE
           CALL 'EXTM0314' USING WS-CALL-AREA
           PERFORM AUX-0513.
       AUX-0513.
           MOVE 'AUX-0513' TO WS-CALL-DATA(1:12)
           ADD 28 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0514.
           ADD 3598 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 6682, 2147483647)
           MOVE WS-HASH TO SCB-A-000015
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0315' TO WS-CALL-CODE
           CALL 'EXTM0315' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0515.
           ADD 3605 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 6695, 2147483647)
           MOVE WS-HASH TO SCB-A-000016
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0316' TO WS-CALL-CODE
           CALL 'EXTM0316' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0516.
           ADD 3612 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 6708, 2147483647)
           MOVE WS-HASH TO SCB-A-000017
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0317' TO WS-CALL-CODE
           CALL 'EXTM0317' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0517.
           ADD 3619 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 6721, 2147483647)
           MOVE WS-HASH TO SCB-A-000018
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0318' TO WS-CALL-CODE
           CALL 'EXTM0318' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0518.
           ADD 3626 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 6734, 2147483647)
           MOVE WS-HASH TO SCB-A-000019
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0319' TO WS-CALL-CODE
           CALL 'EXTM0319' USING WS-CALL-AREA
           PERFORM VALIDATE-0518.
       VALIDATE-0518.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0519.
           ADD 3633 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 6747, 2147483647)
           MOVE WS-HASH TO SCB-A-000020
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0320' TO WS-CALL-CODE
           CALL 'EXTM0320' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0520.
           ADD 3640 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 6760, 2147483647)
           MOVE WS-HASH TO SCB-A-000021
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0321' TO WS-CALL-CODE
           CALL 'EXTM0321' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0521.
           ADD 3647 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 6773, 2147483647)
           MOVE WS-HASH TO SCB-A-000022
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0322' TO WS-CALL-CODE
           CALL 'EXTM0322' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0522.
           ADD 3654 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 6786, 2147483647)
           MOVE WS-HASH TO SCB-A-000023
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0323' TO WS-CALL-CODE
           CALL 'EXTM0323' USING WS-CALL-AREA
           PERFORM AUX-0522.
       AUX-0522.
           MOVE 'AUX-0522' TO WS-CALL-DATA(1:12)
           ADD 37 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0523.
           ADD 3661 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 6799, 2147483647)
           MOVE WS-HASH TO SCB-A-000024
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0324' TO WS-CALL-CODE
           CALL 'EXTM0324' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0524.
           ADD 3668 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 6812, 2147483647)
           MOVE WS-HASH TO SCB-A-000025
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0325' TO WS-CALL-CODE
           CALL 'EXTM0325' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0525.
           ADD 3675 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 6825, 2147483647)
           MOVE WS-HASH TO SCB-A-000026
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0326' TO WS-CALL-CODE
           CALL 'EXTM0326' USING WS-CALL-AREA
           PERFORM VALIDATE-0525.
       VALIDATE-0525.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0526.
           ADD 3682 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 6838, 2147483647)
           MOVE WS-HASH TO SCB-A-000027
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0327' TO WS-CALL-CODE
           CALL 'EXTM0327' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0527.
           ADD 3689 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 6851, 2147483647)
           MOVE WS-HASH TO SCB-A-000028
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0328' TO WS-CALL-CODE
           CALL 'EXTM0328' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0528.
           ADD 3696 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 6864, 2147483647)
           MOVE WS-HASH TO SCB-A-000029
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0329' TO WS-CALL-CODE
           CALL 'EXTM0329' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0529.
           ADD 3703 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 6877, 2147483647)
           MOVE WS-HASH TO SCB-A-000030
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0330' TO WS-CALL-CODE
           CALL 'EXTM0330' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0530.
           ADD 3710 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 6890, 2147483647)
           MOVE WS-HASH TO SCB-A-000031
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0331' TO WS-CALL-CODE
           CALL 'EXTM0331' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0531.
           ADD 3717 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 6903, 2147483647)
           MOVE WS-HASH TO SCB-A-000032
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0332' TO WS-CALL-CODE
           CALL 'EXTM0332' USING WS-CALL-AREA
           PERFORM AUX-0531.
       AUX-0531.
           MOVE 'AUX-0531' TO WS-CALL-DATA(1:12)
           ADD 46 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0532.
           ADD 3724 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 6916, 2147483647)
           MOVE WS-HASH TO SCB-A-000033
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0333' TO WS-CALL-CODE
           CALL 'EXTM0333' USING WS-CALL-AREA
           PERFORM VALIDATE-0532.
       VALIDATE-0532.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0533.
           ADD 3731 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 6929, 2147483647)
           MOVE WS-HASH TO SCB-A-000034
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0334' TO WS-CALL-CODE
           CALL 'EXTM0334' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0534.
           ADD 3738 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 6942, 2147483647)
           MOVE WS-HASH TO SCB-A-000035
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0335' TO WS-CALL-CODE
           CALL 'EXTM0335' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0535.
           ADD 3745 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 6955, 2147483647)
           MOVE WS-HASH TO SCB-A-000036
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0336' TO WS-CALL-CODE
           CALL 'EXTM0336' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0536.
           ADD 3752 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 6968, 2147483647)
           MOVE WS-HASH TO SCB-A-000037
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0337' TO WS-CALL-CODE
           CALL 'EXTM0337' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0537.
           ADD 3759 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 6981, 2147483647)
           MOVE WS-HASH TO SCB-A-000038
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0338' TO WS-CALL-CODE
           CALL 'EXTM0338' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0538.
           ADD 3766 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 6994, 2147483647)
           MOVE WS-HASH TO SCB-A-000039
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0339' TO WS-CALL-CODE
           CALL 'EXTM0339' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0539.
           ADD 3773 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 7007, 2147483647)
           MOVE WS-HASH TO SCB-A-000040
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0340' TO WS-CALL-CODE
           CALL 'EXTM0340' USING WS-CALL-AREA
           PERFORM VALIDATE-0539.
       VALIDATE-0539.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0540.
           ADD 3780 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 7020, 2147483647)
           MOVE WS-HASH TO SCB-A-000041
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0341' TO WS-CALL-CODE
           CALL 'EXTM0341' USING WS-CALL-AREA
           PERFORM AUX-0540.
       AUX-0540.
           MOVE 'AUX-0540' TO WS-CALL-DATA(1:12)
           ADD 55 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0541.
           ADD 3787 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 7033, 2147483647)
           MOVE WS-HASH TO SCB-A-000042
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0342' TO WS-CALL-CODE
           CALL 'EXTM0342' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0542.
           ADD 3794 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 7046, 2147483647)
           MOVE WS-HASH TO SCB-A-000043
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0343' TO WS-CALL-CODE
           CALL 'EXTM0343' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0543.
           ADD 3801 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 7059, 2147483647)
           MOVE WS-HASH TO SCB-A-000044
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0344' TO WS-CALL-CODE
           CALL 'EXTM0344' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0544.
           ADD 3808 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 7072, 2147483647)
           MOVE WS-HASH TO SCB-A-000045
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0345' TO WS-CALL-CODE
           CALL 'EXTM0345' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0545.
           ADD 3815 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 7085, 2147483647)
           MOVE WS-HASH TO SCB-A-000046
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0346' TO WS-CALL-CODE
           CALL 'EXTM0346' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0546.
           ADD 3822 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 7098, 2147483647)
           MOVE WS-HASH TO SCB-A-000047
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0347' TO WS-CALL-CODE
           CALL 'EXTM0347' USING WS-CALL-AREA
           PERFORM VALIDATE-0546.
       VALIDATE-0546.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0547.
           ADD 3829 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 7111, 2147483647)
           MOVE WS-HASH TO SCB-A-000048
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0348' TO WS-CALL-CODE
           CALL 'EXTM0348' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0548.
           ADD 3836 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 7124, 2147483647)
           MOVE WS-HASH TO SCB-A-000049
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0349' TO WS-CALL-CODE
           CALL 'EXTM0349' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0549.
           ADD 3843 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 7137, 2147483647)
           MOVE WS-HASH TO SCB-A-000050
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0350' TO WS-CALL-CODE
           CALL 'EXTM0350' USING WS-CALL-AREA
           PERFORM AUX-0549.
       AUX-0549.
           MOVE 'AUX-0549' TO WS-CALL-DATA(1:12)
           ADD 64 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0550.
           ADD 3850 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 7150, 2147483647)
           MOVE WS-HASH TO SCB-A-000051
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0351' TO WS-CALL-CODE
           CALL 'EXTM0351' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0551.
           ADD 3857 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 7163, 2147483647)
           MOVE WS-HASH TO SCB-A-000052
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0352' TO WS-CALL-CODE
           CALL 'EXTM0352' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0552.
           ADD 3864 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 7176, 2147483647)
           MOVE WS-HASH TO SCB-A-000053
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0353' TO WS-CALL-CODE
           CALL 'EXTM0353' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0553.
           ADD 3871 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 7189, 2147483647)
           MOVE WS-HASH TO SCB-A-000054
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0354' TO WS-CALL-CODE
           CALL 'EXTM0354' USING WS-CALL-AREA
           PERFORM VALIDATE-0553.
       VALIDATE-0553.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0554.
           ADD 3878 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 7202, 2147483647)
           MOVE WS-HASH TO SCB-A-000055
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0355' TO WS-CALL-CODE
           CALL 'EXTM0355' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0555.
           ADD 3885 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 7215, 2147483647)
           MOVE WS-HASH TO SCB-A-000056
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0356' TO WS-CALL-CODE
           CALL 'EXTM0356' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0556.
           ADD 3892 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 7228, 2147483647)
           MOVE WS-HASH TO SCB-A-000057
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0357' TO WS-CALL-CODE
           CALL 'EXTM0357' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0557.
           ADD 3899 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 7241, 2147483647)
           MOVE WS-HASH TO SCB-A-000058
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0358' TO WS-CALL-CODE
           CALL 'EXTM0358' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0558.
           ADD 3906 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 7254, 2147483647)
           MOVE WS-HASH TO SCB-A-000059
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0359' TO WS-CALL-CODE
           CALL 'EXTM0359' USING WS-CALL-AREA
           PERFORM AUX-0558.
       AUX-0558.
           MOVE 'AUX-0558' TO WS-CALL-DATA(1:12)
           ADD 73 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0559.
           ADD 3913 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 7267, 2147483647)
           MOVE WS-HASH TO SCB-A-000060
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0360' TO WS-CALL-CODE
           CALL 'EXTM0360' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0560.
           ADD 3920 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 7280, 2147483647)
           MOVE WS-HASH TO SCB-A-000061
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0361' TO WS-CALL-CODE
           CALL 'EXTM0361' USING WS-CALL-AREA
           PERFORM VALIDATE-0560.
       VALIDATE-0560.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0561.
           ADD 3927 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 7293, 2147483647)
           MOVE WS-HASH TO SCB-A-000062
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0362' TO WS-CALL-CODE
           CALL 'EXTM0362' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0562.
           ADD 3934 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 7306, 2147483647)
           MOVE WS-HASH TO SCB-A-000063
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0363' TO WS-CALL-CODE
           CALL 'EXTM0363' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0563.
           ADD 3941 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 7319, 2147483647)
           MOVE WS-HASH TO SCB-A-000064
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0364' TO WS-CALL-CODE
           CALL 'EXTM0364' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0564.
           ADD 3948 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 7332, 2147483647)
           MOVE WS-HASH TO SCB-A-000065
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0365' TO WS-CALL-CODE
           CALL 'EXTM0365' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0565.
           ADD 3955 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 7345, 2147483647)
           MOVE WS-HASH TO SCB-A-000066
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0366' TO WS-CALL-CODE
           CALL 'EXTM0366' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0566.
           ADD 3962 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 7358, 2147483647)
           MOVE WS-HASH TO SCB-A-000067
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0367' TO WS-CALL-CODE
           CALL 'EXTM0367' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0567.
           ADD 3969 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 7371, 2147483647)
           MOVE WS-HASH TO SCB-A-000068
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0368' TO WS-CALL-CODE
           CALL 'EXTM0368' USING WS-CALL-AREA
           PERFORM AUX-0567.
       AUX-0567.
           MOVE 'AUX-0567' TO WS-CALL-DATA(1:12)
           ADD 82 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0568.
           ADD 3976 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 7384, 2147483647)
           MOVE WS-HASH TO SCB-A-000069
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0369' TO WS-CALL-CODE
           CALL 'EXTM0369' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0569.
           ADD 3983 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 7397, 2147483647)
           MOVE WS-HASH TO SCB-A-000070
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0370' TO WS-CALL-CODE
           CALL 'EXTM0370' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0570.
           ADD 3990 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 7410, 2147483647)
           MOVE WS-HASH TO SCB-A-000071
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0371' TO WS-CALL-CODE
           CALL 'EXTM0371' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0571.
           ADD 3997 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 7423, 2147483647)
           MOVE WS-HASH TO SCB-A-000072
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0372' TO WS-CALL-CODE
           CALL 'EXTM0372' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0572.
           ADD 4004 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 7436, 2147483647)
           MOVE WS-HASH TO SCB-A-000073
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0373' TO WS-CALL-CODE
           CALL 'EXTM0373' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0573.
           ADD 4011 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 7449, 2147483647)
           MOVE WS-HASH TO SCB-A-000074
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0374' TO WS-CALL-CODE
           CALL 'EXTM0374' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0574.
           ADD 4018 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 7462, 2147483647)
           MOVE WS-HASH TO SCB-A-000075
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0375' TO WS-CALL-CODE
           CALL 'EXTM0375' USING WS-CALL-AREA
           PERFORM VALIDATE-0574.
       VALIDATE-0574.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0575.
           ADD 4025 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 7475, 2147483647)
           MOVE WS-HASH TO SCB-A-000076
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0376' TO WS-CALL-CODE
           CALL 'EXTM0376' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0576.
           ADD 4032 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 7488, 2147483647)
           MOVE WS-HASH TO SCB-A-000077
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0377' TO WS-CALL-CODE
           CALL 'EXTM0377' USING WS-CALL-AREA
           PERFORM AUX-0576.
       AUX-0576.
           MOVE 'AUX-0576' TO WS-CALL-DATA(1:12)
           ADD 91 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0577.
           ADD 4039 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 7501, 2147483647)
           MOVE WS-HASH TO SCB-A-000078
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0378' TO WS-CALL-CODE
           CALL 'EXTM0378' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0578.
           ADD 4046 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 7514, 2147483647)
           MOVE WS-HASH TO SCB-A-000079
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0379' TO WS-CALL-CODE
           CALL 'EXTM0379' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0579.
           ADD 4053 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 7527, 2147483647)
           MOVE WS-HASH TO SCB-A-000080
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0380' TO WS-CALL-CODE
           CALL 'EXTM0380' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0580.
           ADD 4060 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 7540, 2147483647)
           MOVE WS-HASH TO SCB-A-000081
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0381' TO WS-CALL-CODE
           CALL 'EXTM0381' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0581.
           ADD 4067 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 7553, 2147483647)
           MOVE WS-HASH TO SCB-A-000082
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0382' TO WS-CALL-CODE
           CALL 'EXTM0382' USING WS-CALL-AREA
           PERFORM VALIDATE-0581.
       VALIDATE-0581.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0582.
           ADD 4074 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 7566, 2147483647)
           MOVE WS-HASH TO SCB-A-000083
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0383' TO WS-CALL-CODE
           CALL 'EXTM0383' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0583.
           ADD 4081 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 7579, 2147483647)
           MOVE WS-HASH TO SCB-A-000084
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0384' TO WS-CALL-CODE
           CALL 'EXTM0384' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0584.
           ADD 4088 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 7592, 2147483647)
           MOVE WS-HASH TO SCB-A-000085
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0385' TO WS-CALL-CODE
           CALL 'EXTM0385' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0585.
           ADD 4095 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 7605, 2147483647)
           MOVE WS-HASH TO SCB-A-000086
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0386' TO WS-CALL-CODE
           CALL 'EXTM0386' USING WS-CALL-AREA
           PERFORM AUX-0585.
       AUX-0585.
           MOVE 'AUX-0585' TO WS-CALL-DATA(1:12)
           ADD 3 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0586.
           ADD 4102 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 7618, 2147483647)
           MOVE WS-HASH TO SCB-A-000087
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0387' TO WS-CALL-CODE
           CALL 'EXTM0387' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0587.
           ADD 4109 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 7631, 2147483647)
           MOVE WS-HASH TO SCB-A-000088
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0388' TO WS-CALL-CODE
           CALL 'EXTM0388' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0588.
           ADD 4116 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 7644, 2147483647)
           MOVE WS-HASH TO SCB-A-000089
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0389' TO WS-CALL-CODE
           CALL 'EXTM0389' USING WS-CALL-AREA
           PERFORM VALIDATE-0588.
       VALIDATE-0588.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0589.
           ADD 4123 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 7657, 2147483647)
           MOVE WS-HASH TO SCB-A-000090
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0390' TO WS-CALL-CODE
           CALL 'EXTM0390' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0590.
           ADD 4130 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 7670, 2147483647)
           MOVE WS-HASH TO SCB-A-000091
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0391' TO WS-CALL-CODE
           CALL 'EXTM0391' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0591.
           ADD 4137 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 7683, 2147483647)
           MOVE WS-HASH TO SCB-A-000092
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0392' TO WS-CALL-CODE
           CALL 'EXTM0392' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0592.
           ADD 4144 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 7696, 2147483647)
           MOVE WS-HASH TO SCB-A-000093
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0393' TO WS-CALL-CODE
           CALL 'EXTM0393' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0593.
           ADD 4151 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 7709, 2147483647)
           MOVE WS-HASH TO SCB-A-000094
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0394' TO WS-CALL-CODE
           CALL 'EXTM0394' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0594.
           ADD 4158 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 7722, 2147483647)
           MOVE WS-HASH TO SCB-A-000095
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0395' TO WS-CALL-CODE
           CALL 'EXTM0395' USING WS-CALL-AREA
           PERFORM AUX-0594.
       AUX-0594.
           MOVE 'AUX-0594' TO WS-CALL-DATA(1:12)
           ADD 12 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0595.
           ADD 4165 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 7735, 2147483647)
           MOVE WS-HASH TO SCB-A-000096
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0396' TO WS-CALL-CODE
           CALL 'EXTM0396' USING WS-CALL-AREA
           PERFORM VALIDATE-0595.
       VALIDATE-0595.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0596.
           ADD 4172 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 7748, 2147483647)
           MOVE WS-HASH TO SCB-A-000097
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0397' TO WS-CALL-CODE
           CALL 'EXTM0397' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0597.
           ADD 4179 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 7761, 2147483647)
           MOVE WS-HASH TO SCB-A-000098
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0398' TO WS-CALL-CODE
           CALL 'EXTM0398' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0598.
           ADD 4186 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 7774, 2147483647)
           MOVE WS-HASH TO SCB-A-000099
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0399' TO WS-CALL-CODE
           CALL 'EXTM0399' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0599.
           ADD 4193 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 7787, 2147483647)
           MOVE WS-HASH TO SCB-A-000100
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0400' TO WS-CALL-CODE
           CALL 'EXTM0400' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0600.
           ADD 4200 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 7800, 2147483647)
           MOVE WS-HASH TO SCB-A-000101
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0401' TO WS-CALL-CODE
           CALL 'EXTM0401' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0601.
           ADD 4207 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 7813, 2147483647)
           MOVE WS-HASH TO SCB-A-000102
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0402' TO WS-CALL-CODE
           CALL 'EXTM0402' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0602.
           ADD 4214 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 7826, 2147483647)
           MOVE WS-HASH TO SCB-A-000103
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0403' TO WS-CALL-CODE
           CALL 'EXTM0403' USING WS-CALL-AREA
           PERFORM VALIDATE-0602.
       VALIDATE-0602.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0603.
           ADD 4221 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 7839, 2147483647)
           MOVE WS-HASH TO SCB-A-000104
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0404' TO WS-CALL-CODE
           CALL 'EXTM0404' USING WS-CALL-AREA
           PERFORM AUX-0603.
       AUX-0603.
           MOVE 'AUX-0603' TO WS-CALL-DATA(1:12)
           ADD 21 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0604.
           ADD 4228 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 7852, 2147483647)
           MOVE WS-HASH TO SCB-A-000105
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0405' TO WS-CALL-CODE
           CALL 'EXTM0405' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0605.
           ADD 4235 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 7865, 2147483647)
           MOVE WS-HASH TO SCB-A-000106
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0406' TO WS-CALL-CODE
           CALL 'EXTM0406' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0606.
           ADD 4242 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 7878, 2147483647)
           MOVE WS-HASH TO SCB-A-000107
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0407' TO WS-CALL-CODE
           CALL 'EXTM0407' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0607.
           ADD 4249 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 7891, 2147483647)
           MOVE WS-HASH TO SCB-A-000108
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0408' TO WS-CALL-CODE
           CALL 'EXTM0408' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0608.
           ADD 4256 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 7904, 2147483647)
           MOVE WS-HASH TO SCB-A-000109
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0409' TO WS-CALL-CODE
           CALL 'EXTM0409' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0609.
           ADD 4263 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 7917, 2147483647)
           MOVE WS-HASH TO SCB-A-000110
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0410' TO WS-CALL-CODE
           CALL 'EXTM0410' USING WS-CALL-AREA
           PERFORM VALIDATE-0609.
       VALIDATE-0609.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0610.
           ADD 4270 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 7930, 2147483647)
           MOVE WS-HASH TO SCB-A-000111
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0411' TO WS-CALL-CODE
           CALL 'EXTM0411' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0611.
           ADD 4277 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 7943, 2147483647)
           MOVE WS-HASH TO SCB-A-000112
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0412' TO WS-CALL-CODE
           CALL 'EXTM0412' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0612.
           ADD 4284 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 7956, 2147483647)
           MOVE WS-HASH TO SCB-A-000113
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0413' TO WS-CALL-CODE
           CALL 'EXTM0413' USING WS-CALL-AREA
           PERFORM AUX-0612.
       AUX-0612.
           MOVE 'AUX-0612' TO WS-CALL-DATA(1:12)
           ADD 30 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0613.
           ADD 4291 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 7969, 2147483647)
           MOVE WS-HASH TO SCB-A-000114
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0414' TO WS-CALL-CODE
           CALL 'EXTM0414' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0614.
           ADD 4298 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 7982, 2147483647)
           MOVE WS-HASH TO SCB-A-000115
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0415' TO WS-CALL-CODE
           CALL 'EXTM0415' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0615.
           ADD 4305 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 7995, 2147483647)
           MOVE WS-HASH TO SCB-A-000116
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0416' TO WS-CALL-CODE
           CALL 'EXTM0416' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0616.
           ADD 4312 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 8008, 2147483647)
           MOVE WS-HASH TO SCB-A-000117
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0417' TO WS-CALL-CODE
           CALL 'EXTM0417' USING WS-CALL-AREA
           PERFORM VALIDATE-0616.
       VALIDATE-0616.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0617.
           ADD 4319 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 8021, 2147483647)
           MOVE WS-HASH TO SCB-A-000118
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0418' TO WS-CALL-CODE
           CALL 'EXTM0418' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0618.
           ADD 4326 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 8034, 2147483647)
           MOVE WS-HASH TO SCB-A-000119
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0419' TO WS-CALL-CODE
           CALL 'EXTM0419' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0619.
           ADD 4333 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 8047, 2147483647)
           MOVE WS-HASH TO SCB-A-000120
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0420' TO WS-CALL-CODE
           CALL 'EXTM0420' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0620.
           ADD 4340 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 8060, 2147483647)
           MOVE WS-HASH TO SCB-A-000121
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0421' TO WS-CALL-CODE
           CALL 'EXTM0421' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0621.
           ADD 4347 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 8073, 2147483647)
           MOVE WS-HASH TO SCB-A-000122
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0422' TO WS-CALL-CODE
           CALL 'EXTM0422' USING WS-CALL-AREA
           PERFORM AUX-0621.
       AUX-0621.
           MOVE 'AUX-0621' TO WS-CALL-DATA(1:12)
           ADD 39 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0622.
           ADD 4354 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 8086, 2147483647)
           MOVE WS-HASH TO SCB-A-000123
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0423' TO WS-CALL-CODE
           CALL 'EXTM0423' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0623.
           ADD 4361 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 8099, 2147483647)
           MOVE WS-HASH TO SCB-A-000124
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0424' TO WS-CALL-CODE
           CALL 'EXTM0424' USING WS-CALL-AREA
           PERFORM VALIDATE-0623.
       VALIDATE-0623.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0624.
           ADD 4368 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 8112, 2147483647)
           MOVE WS-HASH TO SCB-A-000125
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0425' TO WS-CALL-CODE
           CALL 'EXTM0425' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0625.
           ADD 4375 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 8125, 2147483647)
           MOVE WS-HASH TO SCB-A-000126
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0426' TO WS-CALL-CODE
           CALL 'EXTM0426' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0626.
           ADD 4382 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 8138, 2147483647)
           MOVE WS-HASH TO SCB-A-000127
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0427' TO WS-CALL-CODE
           CALL 'EXTM0427' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0627.
           ADD 4389 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 8151, 2147483647)
           MOVE WS-HASH TO SCB-A-000128
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0428' TO WS-CALL-CODE
           CALL 'EXTM0428' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0628.
           ADD 4396 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 8164, 2147483647)
           MOVE WS-HASH TO SCB-A-000129
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0429' TO WS-CALL-CODE
           CALL 'EXTM0429' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0629.
           ADD 4403 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 8177, 2147483647)
           MOVE WS-HASH TO SCB-A-000130
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0430' TO WS-CALL-CODE
           CALL 'EXTM0430' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0630.
           ADD 4410 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 8190, 2147483647)
           MOVE WS-HASH TO SCB-A-000131
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0431' TO WS-CALL-CODE
           CALL 'EXTM0431' USING WS-CALL-AREA
           PERFORM AUX-0630.
       AUX-0630.
           MOVE 'AUX-0630' TO WS-CALL-DATA(1:12)
           ADD 48 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0631.
           ADD 4417 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 8203, 2147483647)
           MOVE WS-HASH TO SCB-A-000132
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0432' TO WS-CALL-CODE
           CALL 'EXTM0432' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0632.
           ADD 4424 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 8216, 2147483647)
           MOVE WS-HASH TO SCB-A-000133
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0433' TO WS-CALL-CODE
           CALL 'EXTM0433' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0633.
           ADD 4431 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 8229, 2147483647)
           MOVE WS-HASH TO SCB-A-000134
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0434' TO WS-CALL-CODE
           CALL 'EXTM0434' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0634.
           ADD 4438 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 8242, 2147483647)
           MOVE WS-HASH TO SCB-A-000135
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0435' TO WS-CALL-CODE
           CALL 'EXTM0435' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0635.
           ADD 4445 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 8255, 2147483647)
           MOVE WS-HASH TO SCB-A-000136
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0436' TO WS-CALL-CODE
           CALL 'EXTM0436' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0636.
           ADD 4452 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 8268, 2147483647)
           MOVE WS-HASH TO SCB-A-000137
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0437' TO WS-CALL-CODE
           CALL 'EXTM0437' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0637.
           ADD 4459 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 8281, 2147483647)
           MOVE WS-HASH TO SCB-A-000138
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0438' TO WS-CALL-CODE
           CALL 'EXTM0438' USING WS-CALL-AREA
           PERFORM VALIDATE-0637.
       VALIDATE-0637.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0638.
           ADD 4466 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 8294, 2147483647)
           MOVE WS-HASH TO SCB-A-000139
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0439' TO WS-CALL-CODE
           CALL 'EXTM0439' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0639.
           ADD 4473 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 8307, 2147483647)
           MOVE WS-HASH TO SCB-A-000140
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0440' TO WS-CALL-CODE
           CALL 'EXTM0440' USING WS-CALL-AREA
           PERFORM AUX-0639.
       AUX-0639.
           MOVE 'AUX-0639' TO WS-CALL-DATA(1:12)
           ADD 57 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0640.
           ADD 4480 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 8320, 2147483647)
           MOVE WS-HASH TO SCB-A-000141
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0441' TO WS-CALL-CODE
           CALL 'EXTM0441' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0641.
           ADD 4487 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 8333, 2147483647)
           MOVE WS-HASH TO SCB-A-000142
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0442' TO WS-CALL-CODE
           CALL 'EXTM0442' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0642.
           ADD 4494 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 8346, 2147483647)
           MOVE WS-HASH TO SCB-A-000143
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0443' TO WS-CALL-CODE
           CALL 'EXTM0443' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0643.
           ADD 4501 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 8359, 2147483647)
           MOVE WS-HASH TO SCB-A-000144
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0444' TO WS-CALL-CODE
           CALL 'EXTM0444' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0644.
           ADD 4508 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 8372, 2147483647)
           MOVE WS-HASH TO SCB-A-000145
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0445' TO WS-CALL-CODE
           CALL 'EXTM0445' USING WS-CALL-AREA
           PERFORM VALIDATE-0644.
       VALIDATE-0644.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0645.
           ADD 4515 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 8385, 2147483647)
           MOVE WS-HASH TO SCB-A-000146
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0446' TO WS-CALL-CODE
           CALL 'EXTM0446' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0646.
           ADD 4522 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 8398, 2147483647)
           MOVE WS-HASH TO SCB-A-000147
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0447' TO WS-CALL-CODE
           CALL 'EXTM0447' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0647.
           ADD 4529 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 8411, 2147483647)
           MOVE WS-HASH TO SCB-A-000148
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0448' TO WS-CALL-CODE
           CALL 'EXTM0448' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0648.
           ADD 4536 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 8424, 2147483647)
           MOVE WS-HASH TO SCB-A-000149
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0449' TO WS-CALL-CODE
           CALL 'EXTM0449' USING WS-CALL-AREA
           PERFORM AUX-0648.
       AUX-0648.
           MOVE 'AUX-0648' TO WS-CALL-DATA(1:12)
           ADD 66 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0649.
           ADD 4543 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 8437, 2147483647)
           MOVE WS-HASH TO SCB-A-000150
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0450' TO WS-CALL-CODE
           CALL 'EXTM0450' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0650.
           ADD 4550 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 8450, 2147483647)
           MOVE WS-HASH TO SCB-A-000151
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0451' TO WS-CALL-CODE
           CALL 'EXTM0451' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0651.
           ADD 4557 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 8463, 2147483647)
           MOVE WS-HASH TO SCB-A-000152
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0452' TO WS-CALL-CODE
           CALL 'EXTM0452' USING WS-CALL-AREA
           PERFORM VALIDATE-0651.
       VALIDATE-0651.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0652.
           ADD 4564 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 8476, 2147483647)
           MOVE WS-HASH TO SCB-A-000153
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0453' TO WS-CALL-CODE
           CALL 'EXTM0453' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0653.
           ADD 4571 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 8489, 2147483647)
           MOVE WS-HASH TO SCB-A-000154
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0454' TO WS-CALL-CODE
           CALL 'EXTM0454' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0654.
           ADD 4578 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 8502, 2147483647)
           MOVE WS-HASH TO SCB-A-000155
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0455' TO WS-CALL-CODE
           CALL 'EXTM0455' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0655.
           ADD 4585 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 8515, 2147483647)
           MOVE WS-HASH TO SCB-A-000156
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0456' TO WS-CALL-CODE
           CALL 'EXTM0456' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0656.
           ADD 4592 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 8528, 2147483647)
           MOVE WS-HASH TO SCB-A-000157
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0457' TO WS-CALL-CODE
           CALL 'EXTM0457' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0657.
           ADD 4599 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 8541, 2147483647)
           MOVE WS-HASH TO SCB-A-000158
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0458' TO WS-CALL-CODE
           CALL 'EXTM0458' USING WS-CALL-AREA
           PERFORM AUX-0657.
       AUX-0657.
           MOVE 'AUX-0657' TO WS-CALL-DATA(1:12)
           ADD 75 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0658.
           ADD 4606 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 8554, 2147483647)
           MOVE WS-HASH TO SCB-A-000159
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0459' TO WS-CALL-CODE
           CALL 'EXTM0459' USING WS-CALL-AREA
           PERFORM VALIDATE-0658.
       VALIDATE-0658.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0659.
           ADD 4613 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 8567, 2147483647)
           MOVE WS-HASH TO SCB-A-000160
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0460' TO WS-CALL-CODE
           CALL 'EXTM0460' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0660.
           ADD 4620 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 8580, 2147483647)
           MOVE WS-HASH TO SCB-A-000161
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0461' TO WS-CALL-CODE
           CALL 'EXTM0461' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0661.
           ADD 4627 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 8593, 2147483647)
           MOVE WS-HASH TO SCB-A-000162
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0462' TO WS-CALL-CODE
           CALL 'EXTM0462' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0662.
           ADD 4634 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 8606, 2147483647)
           MOVE WS-HASH TO SCB-A-000163
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0463' TO WS-CALL-CODE
           CALL 'EXTM0463' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0663.
           ADD 4641 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 8619, 2147483647)
           MOVE WS-HASH TO SCB-A-000164
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0464' TO WS-CALL-CODE
           CALL 'EXTM0464' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0664.
           ADD 4648 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 8632, 2147483647)
           MOVE WS-HASH TO SCB-A-000165
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0465' TO WS-CALL-CODE
           CALL 'EXTM0465' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0665.
           ADD 4655 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 5 + 8645, 2147483647)
           MOVE WS-HASH TO SCB-A-000166
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0466' TO WS-CALL-CODE
           CALL 'EXTM0466' USING WS-CALL-AREA
           PERFORM VALIDATE-0665.
       VALIDATE-0665.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0666.
           ADD 4662 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 6 + 8658, 2147483647)
           MOVE WS-HASH TO SCB-A-000167
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0467' TO WS-CALL-CODE
           CALL 'EXTM0467' USING WS-CALL-AREA
           PERFORM AUX-0666.
       AUX-0666.
           MOVE 'AUX-0666' TO WS-CALL-DATA(1:12)
           ADD 84 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0667.
           ADD 4669 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 7 + 8671, 2147483647)
           MOVE WS-HASH TO SCB-A-000168
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0468' TO WS-CALL-CODE
           CALL 'EXTM0468' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0668.
           ADD 4676 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 8 + 8684, 2147483647)
           MOVE WS-HASH TO SCB-A-000169
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0469' TO WS-CALL-CODE
           CALL 'EXTM0469' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0669.
           ADD 4683 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 9 + 8697, 2147483647)
           MOVE WS-HASH TO SCB-A-000170
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0470' TO WS-CALL-CODE
           CALL 'EXTM0470' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0670.
           ADD 4690 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 10 + 8710, 2147483647)
           MOVE WS-HASH TO SCB-A-000171
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0471' TO WS-CALL-CODE
           CALL 'EXTM0471' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0671.
           ADD 4697 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 11 + 8723, 2147483647)
           MOVE WS-HASH TO SCB-A-000172
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0472' TO WS-CALL-CODE
           CALL 'EXTM0472' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0672.
           ADD 4704 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 12 + 8736, 2147483647)
           MOVE WS-HASH TO SCB-A-000173
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0473' TO WS-CALL-CODE
           CALL 'EXTM0473' USING WS-CALL-AREA
           PERFORM VALIDATE-0672.
       VALIDATE-0672.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0673.
           ADD 4711 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 13 + 8749, 2147483647)
           MOVE WS-HASH TO SCB-A-000174
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0474' TO WS-CALL-CODE
           CALL 'EXTM0474' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0674.
           ADD 4718 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 14 + 8762, 2147483647)
           MOVE WS-HASH TO SCB-A-000175
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0475' TO WS-CALL-CODE
           CALL 'EXTM0475' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0675.
           ADD 4725 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 15 + 8775, 2147483647)
           MOVE WS-HASH TO SCB-A-000176
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0476' TO WS-CALL-CODE
           CALL 'EXTM0476' USING WS-CALL-AREA
           PERFORM AUX-0675.
       AUX-0675.
           MOVE 'AUX-0675' TO WS-CALL-DATA(1:12)
           ADD 93 TO WS-ERR-COUNT
           MOVE 'OK' TO WS-STATUS.
       DISPATCH-0676.
           ADD 4732 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 16 + 8788, 2147483647)
           MOVE WS-HASH TO SCB-A-000177
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0477' TO WS-CALL-CODE
           CALL 'EXTM0477' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0677.
           ADD 4739 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 17 + 8801, 2147483647)
           MOVE WS-HASH TO SCB-A-000178
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0478' TO WS-CALL-CODE
           CALL 'EXTM0478' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0678.
           ADD 4746 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 18 + 8814, 2147483647)
           MOVE WS-HASH TO SCB-A-000179
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0479' TO WS-CALL-CODE
           CALL 'EXTM0479' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0679.
           ADD 4753 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 19 + 8827, 2147483647)
           MOVE WS-HASH TO SCB-A-000180
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0480' TO WS-CALL-CODE
           CALL 'EXTM0480' USING WS-CALL-AREA
           PERFORM VALIDATE-0679.
       VALIDATE-0679.
           IF WS-STATUS = 'Z'
               ADD 1 TO WS-ERR-COUNT
               MOVE 'ER' TO WS-STATUS
           END-IF.
       DISPATCH-0680.
           ADD 4760 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 3 + 8840, 2147483647)
           MOVE WS-HASH TO SCB-A-000181
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0481' TO WS-CALL-CODE
           CALL 'EXTM0481' USING WS-CALL-AREA
           CONTINUE.
       DISPATCH-0681.
           ADD 4767 TO WS-HASH
           COMPUTE WS-HASH = FUNCTION MOD(WS-HASH * 4 + 8853, 2147483647)
           MOVE WS-HASH TO SCB-A-000182
           IF FUNCTION MOD(WS-HASH, 2) = 0
               ADD 1 TO WS-SUM-A
           ELSE
               ADD 2 TO WS-SUM-B
           END-IF
           EVALUATE FUNCTION MOD(WS-HASH, 5)
               WHEN 0 MOVE 'A' TO WS-STATUS
               WHEN 1 MOVE 'B' TO WS-STATUS
               WHEN 2 MOVE 'C' TO WS-STATUS
               WHEN 3 MOVE 'D' TO WS-STATUS
               WHEN OTHER MOVE 'Z' TO WS-STATUS
           END-EVALUATE
           MOVE 'EXTM0482' TO WS-CALL-CODE
           CALL 'EXTM0482' USING WS-CALL-AREA
           CONTINUE.
       FINALIZE-9990.
           STRING 'DONE RUN=' WS-RUN-ID DELIMITED BY SIZE
                  ' ERR='     WS-ERR-COUNT DELIMITED BY SIZE
                  ' SUMA='    WS-SUM-A DELIMITED BY SIZE
                  ' SUMB='    WS-SUM-B DELIMITED BY SIZE
             INTO DUMMY-OUT-REC
           END-STRING
           MOVE 'Y' TO WS-EOF.
           EXIT PROGRAM.
       * PAD-PROGRAM-LINE-09770 **********************************************
       * PAD-PROGRAM-LINE-09771 **********************************************
       * PAD-PROGRAM-LINE-09772 **********************************************
       * PAD-PROGRAM-LINE-09773 **********************************************
       * PAD-PROGRAM-LINE-09774 **********************************************
       * PAD-PROGRAM-LINE-09775 **********************************************
       * PAD-PROGRAM-LINE-09776 **********************************************
       * PAD-PROGRAM-LINE-09777 **********************************************
       * PAD-PROGRAM-LINE-09778 **********************************************
       * PAD-PROGRAM-LINE-09779 **********************************************
       * PAD-PROGRAM-LINE-09780 **********************************************
       * PAD-PROGRAM-LINE-09781 **********************************************
       * PAD-PROGRAM-LINE-09782 **********************************************
       * PAD-PROGRAM-LINE-09783 **********************************************
       * PAD-PROGRAM-LINE-09784 **********************************************
       * PAD-PROGRAM-LINE-09785 **********************************************
       * PAD-PROGRAM-LINE-09786 **********************************************
       * PAD-PROGRAM-LINE-09787 **********************************************
       * PAD-PROGRAM-LINE-09788 **********************************************
       * PAD-PROGRAM-LINE-09789 **********************************************
       * PAD-PROGRAM-LINE-09790 **********************************************
       * PAD-PROGRAM-LINE-09791 **********************************************
       * PAD-PROGRAM-LINE-09792 **********************************************
       * PAD-PROGRAM-LINE-09793 **********************************************
       * PAD-PROGRAM-LINE-09794 **********************************************
       * PAD-PROGRAM-LINE-09795 **********************************************
       * PAD-PROGRAM-LINE-09796 **********************************************
       * PAD-PROGRAM-LINE-09797 **********************************************
       * PAD-PROGRAM-LINE-09798 **********************************************
       * PAD-PROGRAM-LINE-09799 **********************************************
       * PAD-PROGRAM-LINE-09800 **********************************************
       * PAD-PROGRAM-LINE-09801 **********************************************
       * PAD-PROGRAM-LINE-09802 **********************************************
       * PAD-PROGRAM-LINE-09803 **********************************************
       * PAD-PROGRAM-LINE-09804 **********************************************
       * PAD-PROGRAM-LINE-09805 **********************************************
       * PAD-PROGRAM-LINE-09806 **********************************************
       * PAD-PROGRAM-LINE-09807 **********************************************
       * PAD-PROGRAM-LINE-09808 **********************************************
       * PAD-PROGRAM-LINE-09809 **********************************************
       * PAD-PROGRAM-LINE-09810 **********************************************
       * PAD-PROGRAM-LINE-09811 **********************************************
       * PAD-PROGRAM-LINE-09812 **********************************************
       * PAD-PROGRAM-LINE-09813 **********************************************
       * PAD-PROGRAM-LINE-09814 **********************************************
       * PAD-PROGRAM-LINE-09815 **********************************************
       * PAD-PROGRAM-LINE-09816 **********************************************
       * PAD-PROGRAM-LINE-09817 **********************************************
       * PAD-PROGRAM-LINE-09818 **********************************************
       * PAD-PROGRAM-LINE-09819 **********************************************
       * PAD-PROGRAM-LINE-09820 **********************************************
       * PAD-PROGRAM-LINE-09821 **********************************************
       * PAD-PROGRAM-LINE-09822 **********************************************
       * PAD-PROGRAM-LINE-09823 **********************************************
       * PAD-PROGRAM-LINE-09824 **********************************************
       * PAD-PROGRAM-LINE-09825 **********************************************
       * PAD-PROGRAM-LINE-09826 **********************************************
       * PAD-PROGRAM-LINE-09827 **********************************************
       * PAD-PROGRAM-LINE-09828 **********************************************
       * PAD-PROGRAM-LINE-09829 **********************************************
       * PAD-PROGRAM-LINE-09830 **********************************************
       * PAD-PROGRAM-LINE-09831 **********************************************
       * PAD-PROGRAM-LINE-09832 **********************************************
       * PAD-PROGRAM-LINE-09833 **********************************************
       * PAD-PROGRAM-LINE-09834 **********************************************
       * PAD-PROGRAM-LINE-09835 **********************************************
       * PAD-PROGRAM-LINE-09836 **********************************************
       * PAD-PROGRAM-LINE-09837 **********************************************
       * PAD-PROGRAM-LINE-09838 **********************************************
       * PAD-PROGRAM-LINE-09839 **********************************************
       * PAD-PROGRAM-LINE-09840 **********************************************
       * PAD-PROGRAM-LINE-09841 **********************************************
       * PAD-PROGRAM-LINE-09842 **********************************************
       * PAD-PROGRAM-LINE-09843 **********************************************
       * PAD-PROGRAM-LINE-09844 **********************************************
       * PAD-PROGRAM-LINE-09845 **********************************************
       * PAD-PROGRAM-LINE-09846 **********************************************
       * PAD-PROGRAM-LINE-09847 **********************************************
       * PAD-PROGRAM-LINE-09848 **********************************************
       * PAD-PROGRAM-LINE-09849 **********************************************
       * PAD-PROGRAM-LINE-09850 **********************************************
       * PAD-PROGRAM-LINE-09851 **********************************************
       * PAD-PROGRAM-LINE-09852 **********************************************
       * PAD-PROGRAM-LINE-09853 **********************************************
       * PAD-PROGRAM-LINE-09854 **********************************************
       * PAD-PROGRAM-LINE-09855 **********************************************
       * PAD-PROGRAM-LINE-09856 **********************************************
       * PAD-PROGRAM-LINE-09857 **********************************************
       * PAD-PROGRAM-LINE-09858 **********************************************
       * PAD-PROGRAM-LINE-09859 **********************************************
       * PAD-PROGRAM-LINE-09860 **********************************************
       * PAD-PROGRAM-LINE-09861 **********************************************
       * PAD-PROGRAM-LINE-09862 **********************************************
       * PAD-PROGRAM-LINE-09863 **********************************************
       * PAD-PROGRAM-LINE-09864 **********************************************
       * PAD-PROGRAM-LINE-09865 **********************************************
       * PAD-PROGRAM-LINE-09866 **********************************************
       * PAD-PROGRAM-LINE-09867 **********************************************
       * PAD-PROGRAM-LINE-09868 **********************************************
       * PAD-PROGRAM-LINE-09869 **********************************************
       * PAD-PROGRAM-LINE-09870 **********************************************
       * PAD-PROGRAM-LINE-09871 **********************************************
       * PAD-PROGRAM-LINE-09872 **********************************************
       * PAD-PROGRAM-LINE-09873 **********************************************
       * PAD-PROGRAM-LINE-09874 **********************************************
       * PAD-PROGRAM-LINE-09875 **********************************************
       * PAD-PROGRAM-LINE-09876 **********************************************
       * PAD-PROGRAM-LINE-09877 **********************************************
       * PAD-PROGRAM-LINE-09878 **********************************************
       * PAD-PROGRAM-LINE-09879 **********************************************
       * PAD-PROGRAM-LINE-09880 **********************************************
       * PAD-PROGRAM-LINE-09881 **********************************************
       * PAD-PROGRAM-LINE-09882 **********************************************
       * PAD-PROGRAM-LINE-09883 **********************************************
       * PAD-PROGRAM-LINE-09884 **********************************************
       * PAD-PROGRAM-LINE-09885 **********************************************
       * PAD-PROGRAM-LINE-09886 **********************************************
       * PAD-PROGRAM-LINE-09887 **********************************************
       * PAD-PROGRAM-LINE-09888 **********************************************
       * PAD-PROGRAM-LINE-09889 **********************************************
       * PAD-PROGRAM-LINE-09890 **********************************************
       * PAD-PROGRAM-LINE-09891 **********************************************
       * PAD-PROGRAM-LINE-09892 **********************************************
       * PAD-PROGRAM-LINE-09893 **********************************************
       * PAD-PROGRAM-LINE-09894 **********************************************
       * PAD-PROGRAM-LINE-09895 **********************************************
       * PAD-PROGRAM-LINE-09896 **********************************************
       * PAD-PROGRAM-LINE-09897 **********************************************
       * PAD-PROGRAM-LINE-09898 **********************************************
       * PAD-PROGRAM-LINE-09899 **********************************************
       * PAD-PROGRAM-LINE-09900 **********************************************
       * PAD-PROGRAM-LINE-09901 **********************************************
       * PAD-PROGRAM-LINE-09902 **********************************************
       * PAD-PROGRAM-LINE-09903 **********************************************
       * PAD-PROGRAM-LINE-09904 **********************************************
       * PAD-PROGRAM-LINE-09905 **********************************************
       * PAD-PROGRAM-LINE-09906 **********************************************
       * PAD-PROGRAM-LINE-09907 **********************************************
       * PAD-PROGRAM-LINE-09908 **********************************************
       * PAD-PROGRAM-LINE-09909 **********************************************
       * PAD-PROGRAM-LINE-09910 **********************************************
       * PAD-PROGRAM-LINE-09911 **********************************************
       * PAD-PROGRAM-LINE-09912 **********************************************
       * PAD-PROGRAM-LINE-09913 **********************************************
       * PAD-PROGRAM-LINE-09914 **********************************************
       * PAD-PROGRAM-LINE-09915 **********************************************
       * PAD-PROGRAM-LINE-09916 **********************************************
       * PAD-PROGRAM-LINE-09917 **********************************************
       * PAD-PROGRAM-LINE-09918 **********************************************
       * PAD-PROGRAM-LINE-09919 **********************************************
       * PAD-PROGRAM-LINE-09920 **********************************************
       * PAD-PROGRAM-LINE-09921 **********************************************
       * PAD-PROGRAM-LINE-09922 **********************************************
       * PAD-PROGRAM-LINE-09923 **********************************************
       * PAD-PROGRAM-LINE-09924 **********************************************
       * PAD-PROGRAM-LINE-09925 **********************************************
       * PAD-PROGRAM-LINE-09926 **********************************************
       * PAD-PROGRAM-LINE-09927 **********************************************
       * PAD-PROGRAM-LINE-09928 **********************************************
       * PAD-PROGRAM-LINE-09929 **********************************************
       * PAD-PROGRAM-LINE-09930 **********************************************
       * PAD-PROGRAM-LINE-09931 **********************************************
       * PAD-PROGRAM-LINE-09932 **********************************************
       * PAD-PROGRAM-LINE-09933 **********************************************
       * PAD-PROGRAM-LINE-09934 **********************************************
       * PAD-PROGRAM-LINE-09935 **********************************************
       * PAD-PROGRAM-LINE-09936 **********************************************
       * PAD-PROGRAM-LINE-09937 **********************************************
       * PAD-PROGRAM-LINE-09938 **********************************************
       * PAD-PROGRAM-LINE-09939 **********************************************
       * PAD-PROGRAM-LINE-09940 **********************************************
       * PAD-PROGRAM-LINE-09941 **********************************************
       * PAD-PROGRAM-LINE-09942 **********************************************
       * PAD-PROGRAM-LINE-09943 **********************************************
       * PAD-PROGRAM-LINE-09944 **********************************************
       * PAD-PROGRAM-LINE-09945 **********************************************
       * PAD-PROGRAM-LINE-09946 **********************************************
       * PAD-PROGRAM-LINE-09947 **********************************************
       * PAD-PROGRAM-LINE-09948 **********************************************
       * PAD-PROGRAM-LINE-09949 **********************************************
       * PAD-PROGRAM-LINE-09950 **********************************************
       * PAD-PROGRAM-LINE-09951 **********************************************
       * PAD-PROGRAM-LINE-09952 **********************************************
       * PAD-PROGRAM-LINE-09953 **********************************************
       * PAD-PROGRAM-LINE-09954 **********************************************
       * PAD-PROGRAM-LINE-09955 **********************************************
       * PAD-PROGRAM-LINE-09956 **********************************************
       * PAD-PROGRAM-LINE-09957 **********************************************
       * PAD-PROGRAM-LINE-09958 **********************************************
       * PAD-PROGRAM-LINE-09959 **********************************************
       * PAD-PROGRAM-LINE-09960 **********************************************
       * PAD-PROGRAM-LINE-09961 **********************************************
       * PAD-PROGRAM-LINE-09962 **********************************************
       * PAD-PROGRAM-LINE-09963 **********************************************
       * PAD-PROGRAM-LINE-09964 **********************************************
       * PAD-PROGRAM-LINE-09965 **********************************************
       * PAD-PROGRAM-LINE-09966 **********************************************
       * PAD-PROGRAM-LINE-09967 **********************************************
       * PAD-PROGRAM-LINE-09968 **********************************************
       * PAD-PROGRAM-LINE-09969 **********************************************
       * PAD-PROGRAM-LINE-09970 **********************************************
       * PAD-PROGRAM-LINE-09971 **********************************************
       * PAD-PROGRAM-LINE-09972 **********************************************
       * PAD-PROGRAM-LINE-09973 **********************************************
       * PAD-PROGRAM-LINE-09974 **********************************************
       * PAD-PROGRAM-LINE-09975 **********************************************
       * PAD-PROGRAM-LINE-09976 **********************************************
       * PAD-PROGRAM-LINE-09977 **********************************************
       * PAD-PROGRAM-LINE-09978 **********************************************
       * PAD-PROGRAM-LINE-09979 **********************************************
       * PAD-PROGRAM-LINE-09980 **********************************************
       * PAD-PROGRAM-LINE-09981 **********************************************
       * PAD-PROGRAM-LINE-09982 **********************************************
       * PAD-PROGRAM-LINE-09983 **********************************************
       * PAD-PROGRAM-LINE-09984 **********************************************
       * PAD-PROGRAM-LINE-09985 **********************************************
       * PAD-PROGRAM-LINE-09986 **********************************************
       * PAD-PROGRAM-LINE-09987 **********************************************
       * PAD-PROGRAM-LINE-09988 **********************************************
       * PAD-PROGRAM-LINE-09989 **********************************************
       * PAD-PROGRAM-LINE-09990 **********************************************
       * PAD-PROGRAM-LINE-09991 **********************************************
       * PAD-PROGRAM-LINE-09992 **********************************************
       * PAD-PROGRAM-LINE-09993 **********************************************
       * PAD-PROGRAM-LINE-09994 **********************************************
       * PAD-PROGRAM-LINE-09995 **********************************************
       * PAD-PROGRAM-LINE-09996 **********************************************
       * PAD-PROGRAM-LINE-09997 **********************************************
       * PAD-PROGRAM-LINE-09998 **********************************************
       * PAD-PROGRAM-LINE-09999 **********************************************
       * PAD-PROGRAM-LINE-10000 **********************************************
