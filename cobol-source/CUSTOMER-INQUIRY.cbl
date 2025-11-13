       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-INQUIRY.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE ASSIGN TO 'CUSTFILE.DAT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CUST-ID.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE.
       COPY CUSTOMER-DATA.
       
       WORKING-STORAGE SECTION.
       COPY ERROR-CODES.
       
       01  WS-SEARCH-ID        PIC 9(8).
       01  WS-EOF-FLAG         PIC X VALUE 'N'.
           88  EOF             VALUE 'Y'.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT CUSTOMER-FILE.
           
           DISPLAY 'Enter Customer ID: '.
           ACCEPT WS-SEARCH-ID.
           
           PERFORM SEARCH-CUSTOMER.
           
           CLOSE CUSTOMER-FILE.
           STOP RUN.
       
       SEARCH-CUSTOMER.
           MOVE WS-SEARCH-ID TO CUST-ID.
           READ CUSTOMER-FILE
               INVALID KEY
                   DISPLAY ERR-INVALID-CUST
               NOT INVALID KEY
                   CALL 'CUSTOMER-DISPLAY' USING CUSTOMER-RECORD
           END-READ.
