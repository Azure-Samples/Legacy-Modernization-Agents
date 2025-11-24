       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROGRAM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-COUNTER      PIC 9(4) VALUE ZERO.
       01  WS-MESSAGE      PIC X(50) VALUE 'Hello from COBOL'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY WS-MESSAGE.
           PERFORM UNTIL WS-COUNTER > 10
               ADD 1 TO WS-COUNTER
               DISPLAY 'Counter: ' WS-COUNTER
           END-PERFORM.
           STOP RUN.
