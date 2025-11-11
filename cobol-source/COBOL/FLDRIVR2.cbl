       IDENTIFICATION DIVISION.  
       PROGRAM-ID. FLDRIVR2.  
       ENVIRONMENT DIVISION.  
       INPUT-OUTPUT SECTION.  
       FILE-CONTROL.  
           SELECT AGENT-NOTIFY-FILE ASSIGN TO 'AGENTFLE'  
               ORGANIZATION IS SEQUENTIAL.  
           SELECT CUSTOMER-NOTIFY-FILE ASSIGN TO 'CUSTFLE'  
               ORGANIZATION IS SEQUENTIAL.  
           SELECT NOTIFY-REPORT-FILE ASSIGN TO 'RPTFLE'  
               ORGANIZATION IS SEQUENTIAL.                 
  
       DATA DIVISION.  
       FILE SECTION.  
       FD  AGENT-NOTIFY-FILE.  
       01  AGENT-NOTIFY-RECORD.  
           05 AGENT-CODE               PIC X(10).  
           05 AGENT-NAME               PIC X(45).  
           05 AGENT-ADDRESS-1          PIC X(50).  
           05 AGENT-ADDRESS-2          PIC X(50).  
           05 AGENT-CITY               PIC X(20).  
           05 AGENT-STATE              PIC X(2).  
           05 AGENT-POLICY-NUMBER      PIC X(10).  
           05 AGENT-POLICY-FNAME       PIC X(35).  
           05 AGENT-POLICY-MNAME       PIC X(1).  
           05 AGENT-POLICY-LNAME       PIC X(35).  
           05 AGENT-POLICY-START-DATE  PIC X(10).  
           05 AGENT-POLICY-EXPIRY-DATE PIC X(10).  
           05 AGENT-NOTIFY-DATE        PIC X(10).  
           05 AGENT-NOTIFY-MESSAGES    PIC X(100).  
  
       FD  CUSTOMER-NOTIFY-FILE.  
       01  CUSTOMER-NOTIFY-RECORD.  
           05 CUST-POLICY-NUMBER       PIC X(10).  
           05 CUST-FNAME               PIC X(35).  
           05 CUST-MNAME               PIC X(1).  
           05 CUST-LNAME               PIC X(35).  
           05 CUST-POLICY-START-DATE   PIC X(10).  
           05 CUST-POLICY-EXPIRY-DATE  PIC X(10).  
           05 CUST-NOTIFY-DATE         PIC X(10).  
           05 CUST-NOTIFY-MESSAGES     PIC X(100).  
           05 CUST-AGENT-CODE          PIC X(10).  
           05 CUST-AGENT-NAME          PIC X(45).  
           05 CUST-STATUTORY-MESSAGE   PIC X(100).  

       FD  NOTIFY-REPORT-FILE.  
       01  NOTIFY-REPORT-RECORD.  
           05 REPORT-LINE            PIC X(133). 

       WORKING-STORAGE SECTION.  
       01  FILE-STATUS               PIC XX.  
       01  OPERATION-STATUS          PIC XX VALUE '00'.  
       01  OPERATION-TYPE            PIC X(10).  
       01  FILE-NAME                 PIC X(20).  
       01  IS-AGENT-RECORD           PIC X VALUE 'N'.  
       01  IS-CUST-RECORD            PIC X VALUE 'N'.    
       01  IS-REPORT-RECORD          PIC X VALUE 'N'.             
       01  DUMMY-REDEFINES.  
           05 DUMMY-FIELD            PIC X(250).  
  
       LINKAGE SECTION.  
       01  LNK-FILE-NAME             PIC X(20).  
       01  LNK-OPERATION-TYPE        PIC X(10).  
             
       01  LNK-AGENT-NOTIFY-RECORD.  
           05 LNK-AGENT-CODE          PIC X(10).  
           05 LNK-AGENT-NAME          PIC X(45).  
           05 LNK-AGENT-ADDRESS-1     PIC X(50).  
           05 LNK-AGENT-ADDRESS-2     PIC X(50).  
           05 LNK-AGENT-CITY          PIC X(20).  
           05 LNK-AGENT-STATE         PIC X(2).  
           05 LNK-POLICY-NUMBER       PIC X(10).  
           05 LNK-POLICY-HOLDER-FNAME PIC X(35).  
           05 LNK-POLICY-HOLDER-MNAME PIC X(1).  
           05 LNK-POLICY-HOLDER-LNAME PIC X(35).  
           05 LNK-POLICY-START-DATE   PIC X(10).  
           05 LNK-POLICY-EXPIRY-DATE  PIC X(10).  
           05 LNK-NOTIFY-DATE         PIC X(10).  
           05 LNK-NOTIFY-MESSAGES     PIC X(100).  
  
       01  LNK-CUSTOMER-NOTIFY-RECORD.  
           05 LNK-CUST-POLICY-NUMBER  PIC X(10).  
           05 LNK-CUST-FIRST-NAME     PIC X(35).  
           05 LNK-CUST-MIDDLE-NAME    PIC X(1).  
           05 LNK-CUST-LAST-NAME      PIC X(35).  
           05 LNK-CUST-START-DATE     PIC X(10).  
           05 LNK-CUST-EXPIRY-DATE    PIC X(10).  
           05 LNK-CUST-NOTIFY-DATE    PIC X(10).  
           05 LNK-CUST-NOTIFY-MESSAGES PIC X(100).  
           05 LNK-CUST-AGENT-CODE     PIC X(10).  
           05 LNK-CUST-AGENT-NAME     PIC X(45).  
           05 LNK-STATUTORY-MESSAGE   PIC X(100).  

       01  LNK-NOTIFY-REPORT-RECORD.  
           05 LNK-REPORT-LINE         PIC X(133).             
         01  LNK-OPERATION-STATUS      PIC XX.  

  
       PROCEDURE DIVISION USING LNK-FILE-NAME 
                                LNK-OPERATION-TYPE  
                                LNK-AGENT-NOTIFY-RECORD  
                                LNK-CUSTOMER-NOTIFY-RECORD
                                LNK-NOTIFY-REPORT-RECORD  
                                LNK-OPERATION-STATUS.  
  
       MAIN-PROCEDURE.  
           MOVE LNK-FILE-NAME      TO FILE-NAME  
           MOVE LNK-OPERATION-TYPE TO OPERATION-TYPE  
  
           IF FILE-NAME = 'AGENT-NOTIFY-FILE'  
               MOVE 'Y' TO IS-AGENT-RECORD  
           ELSE  
               MOVE 'N' TO IS-AGENT-RECORD  
           END-IF  

           IF FILE-NAME = 'CUSTOMER-NOTIFY-FILE'  
               MOVE 'Y' TO IS-CUST-RECORD  
           ELSE  
               MOVE 'N' TO IS-CUST-RECORD  
           END-IF 

           IF FILE-NAME = 'NOTIFY-REPORT-FILE'  
               MOVE 'Y' TO IS-REPORT-RECORD  
           ELSE  
               MOVE 'N' TO IS-REPORT-RECORD  
           END-IF 

           EVALUATE OPERATION-TYPE  
               WHEN 'OPEN'  
                   PERFORM FILE-OPEN  
               WHEN 'CLOSE'  
                   PERFORM FILE-CLOSE  
               WHEN 'WRITE'  
                   PERFORM FILE-WRITE  
               WHEN OTHER  
                   MOVE '99' TO OPERATION-STATUS  
           END-EVALUATE  
  
           MOVE OPERATION-STATUS TO LNK-OPERATION-STATUS  
           GOBACK.  
  
       FILE-OPEN.  
           IF IS-AGENT-RECORD = 'Y'  
               OPEN OUTPUT AGENT-NOTIFY-FILE  
               MOVE FILE-STATUS TO OPERATION-STATUS  
           ELSE  
           IF IS-CUST-RECORD = 'Y' 
               OPEN OUTPUT CUSTOMER-NOTIFY-FILE  
               MOVE FILE-STATUS TO OPERATION-STATUS  
           ELSE  
           IF IS-REPORT-RECORD = 'Y' 
               OPEN OUTPUT NOTIFY-REPORT-FILE  
               MOVE FILE-STATUS TO OPERATION-STATUS
           END-IF  
           END-IF
           END-IF
           IF FILE-STATUS NOT = '00'  
               MOVE FILE-STATUS TO OPERATION-STATUS 
               PERFORM FILE-ERROR-HANDLER 
           END-IF.  
  
       FILE-CLOSE.  
           IF IS-AGENT-RECORD = 'Y'  
               CLOSE AGENT-NOTIFY-FILE  
           ELSE  
           IF IS-CUST-RECORD = 'Y'  
               CLOSE CUSTOMER-NOTIFY-FILE  
           ELSE  
           IF IS-REPORT-RECORD = 'Y' 
               CLOSE NOTIFY-REPORT-FILE
           END-IF
           END-IF
           END-IF

           IF FILE-STATUS NOT = '00'  
               MOVE FILE-STATUS TO OPERATION-STATUS  
           END-IF.  
  
       FILE-WRITE.  
           IF IS-AGENT-RECORD = 'Y'  
               MOVE LNK-AGENT-NOTIFY-RECORD TO AGENT-NOTIFY-RECORD  
               WRITE AGENT-NOTIFY-RECORD  
               MOVE FILE-STATUS TO OPERATION-STATUS  
           ELSE 
           IF IS-CUST-RECORD = 'Y' 
               MOVE LNK-CUSTOMER-NOTIFY-RECORD TO CUSTOMER-NOTIFY-RECORD  
               WRITE CUSTOMER-NOTIFY-RECORD  
               MOVE FILE-STATUS TO OPERATION-STATUS 
           ELSE
           IF IS-REPORT-RECORD = 'Y' 
               MOVE LNK-NOTIFY-REPORT-RECORD TO NOTIFY-REPORT-RECORD  
               WRITE NOTIFY-REPORT-RECORD  
               MOVE FILE-STATUS TO OPERATION-STATUS 
           END-IF  
           END-IF
           END-IF
           IF FILE-STATUS NOT = '00'  
               MOVE FILE-STATUS TO OPERATION-STATUS  
           END-IF.  
       
       FILE-ERROR-HANDLER.
           DISPLAY 'ERROR: ' OPERATION-TYPE ' ON FILE ' FILE-NAME
           DISPLAY 'FILE STATUS: ' OPERATION-STATUS.

       END PROGRAM FLDRIVR2.  