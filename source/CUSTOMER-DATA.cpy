      * CUSTOMER DATA COPYBOOK
       01  CUSTOMER-RECORD.
           05  CUST-ID             PIC 9(8).
           05  CUST-NAME           PIC X(50).
           05  CUST-ADDRESS        PIC X(100).
           05  CUST-BALANCE        PIC S9(9)V99.
           05  CUST-STATUS         PIC X.
               88  ACTIVE          VALUE 'A'.
               88  INACTIVE        VALUE 'I'.
