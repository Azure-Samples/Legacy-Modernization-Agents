       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTOMER-DISPLAY.
       
       DATA DIVISION.
       LINKAGE SECTION.
       COPY CUSTOMER-DATA.
       
       PROCEDURE DIVISION USING CUSTOMER-RECORD.
       MAIN-LOGIC.
           DISPLAY 'Customer ID: ' CUST-ID.
           DISPLAY 'Name: ' CUST-NAME.
           DISPLAY 'Address: ' CUST-ADDRESS.
           DISPLAY 'Balance: ' CUST-BALANCE.
           
           IF ACTIVE
               DISPLAY 'Status: Active'
           ELSE
               DISPLAY 'Status: Inactive'
           END-IF.
           
           CALL 'FORMAT-BALANCE' USING CUST-BALANCE.
           
           GOBACK.
