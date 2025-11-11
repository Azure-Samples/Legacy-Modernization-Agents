       IDENTIFICATION DIVISION.  
       PROGRAM-ID. MAINPGM.  
       ENVIRONMENT DIVISION.  
       CONFIGURATION SECTION.  
       DATA DIVISION.  
       WORKING-STORAGE SECTION.  
  

      *01  WS-POLICY-RECORD.
           COPY CPOLICY.
      *01  WS-AGENT-RECORD.
           COPY CAGENT.   

       01  WS-CUST-NOTIFY-RECORD.
           COPY CUSTNTFY.
       01  WS-AGENT-NOTIFY-RECORD.
           COPY AGNTNTFY.            

       01  DBDRIVR1-AREA.  
           10  WS-DBDRIVR1-OPERATION-TYPE  PIC X(10).   
           10  WS-DBDRIVR1-PROCESS-DATE    PIC X(10).  
           10  WS-DBDRIVR1-SQLCODE         PIC S9(9) COMP.  
      *     10  WS-DBDRIVR1-POLICY-RECORD   PIC X(476).
           10  WS-DBDRIVR1-POLICY-RECORD   PIC X(787).
                                           
       01  DBDRIVR2-AREA.  
           10  WS-DBDRIVR2-OPERATION-TYPE  PIC X(10).    
           10  WS-DBDRIVR2-PROCESS-DATE    PIC X(10).  
           10  WS-DBDRIVR2-POLICY-NUMBER   PIC X(10).           
           10  WS-DBDRIVR2-SQLCODE         PIC S9(9) COMP.  

       01  FLEDIVR1-AREA.  
           10  WS-FLEDIVR1-OPERATION-TYPE      
                                            PIC X(8).  
           10  WS-FLEDIVR1-AGENT-CODE   PIC X(10).  
           10  WS-FLEDIVR1-STATUS-CODE  PIC X(2).        
           10  WS-FLEDIVR1-AGENT-RECORD PIC X(208).  
  
       01  FLEDIVR2-AREA.  
           10  WS-FILE-NAME               PIC X(20).  
           10  WS-FLEDIVR2-OPERATION-TYPE PIC X(10).  
           10  WS-FLEDIVR2-AGENT-NOTIFY-REC.  
               15  WS-AGENT-CODE          PIC X(10).  
               15  WS-AGENT-NAME          PIC X(45).  
               15  WS-AGENT-ADDRESS-1     PIC X(50).  
               15  WS-AGENT-ADDRESS-2     PIC X(50).  
               15  WS-AGENT-CITY          PIC X(20).  
               15  WS-AGENT-STATE         PIC X(2). 
               15  WS-AGENT-ZIP-CD        PIC X(10). 
               15  WS-AGENT-EMAIL         PIC X(30).
               15  WS-POLICY-NUMBER       PIC X(10).  
               15  WS-POLICY-HOLDER-FNAME PIC X(35).  
               15  WS-POLICY-HOLDER-MNAME PIC X(1).  
               15  WS-POLICY-HOLDER-LNAME PIC X(35).  
               15  WS-POLICY-START-DATE   PIC X(10).  
               15  WS-POLICY-EXPIRY-DATE  PIC X(10).  
               15  WS-NOTIFY-DATE         PIC X(10).  
               15  WS-AGENT-NOTIFY-MSG    PIC X(100).  
           10  WS-CUSTOMER-NOTIFY-RECORD.  
               15  WS-CUST-POLICY-NUMBER  PIC X(10).  
               15  WS-CUST-FIRST-NAME     PIC X(35).  
               15  WS-CUST-MIDDLE-NAME    PIC X(1).  
               15  WS-CUST-LAST-NAME      PIC X(35).  
               15  WS-CUST-ADDR-1         PIC X(100). 
               15  WS-CUST-ADDR-2         PIC X(100). 
               15  WS-CUST-CITY           PIC X(30).    
               15  WS-CUST-STATE          PIC X(2).   
               15  WS-CUST-ZIP-CD         PIC X(10).  
               15  WS-CUST-START-DATE     PIC X(10).  
               15  WS-CUST-EXPIRY-DATE    PIC X(10).  
               15  WS-CUST-NOTIFY-DATE    PIC X(10).  
               15  WS-CUST-NOTIFY-MSG     PIC X(100).  
               15  WS-CUST-AGENT-CODE     PIC X(10).  
               15  WS-CUST-AGENT-NAME     PIC X(45).  
               15  WS-CUST-EMAIL          PIC X(30).
               15  WS-CUST-BENEF-NAME     PIC X(60).
               15  WS-CUST-STATUTORY-MSG  PIC X(100).
           10  WS-NOTIFY-REPORT-RECORD.  
               15  WS-REPORT-LINE         PIC X(133).
           10  WS-FLEDIVR2-STATUS-CODE    PIC X(2).                  

       01  WS-CURRENT-DATE.  
           05  WS-MONTH                  PIC 9(2).  
           05  WS-CUR-FILLER1            PIC X(1).
           05  WS-DAY                    PIC 9(2).  
           05  WS-CUR-FILLER2            PIC X(1). 
           05  WS-YEAR                   PIC 9(4).  

       01  NO-MORE-POLICY-SW             PIC X(3) VALUE 'NO'.
           88 NO-MORE-POLICY             VALUE 'YES'.
           88 POLICY-FOUND               VALUE 'NO'.

       01  RPT-MAIN-HEADER.
           05 FILLER             PIC X(30) VALUE SPACES.
           05 FILLER             PIC X(36) VALUE '30 DAYS POLICY EXPIRY 
      -                                          'REPORT AS OF '.
           05 RPT-DATE           PIC X(10) VALUE SPACES.
           05 FILLER             PIC X(57) VALUE SPACES.  

       01  RPT-STATE-HEADER.
           05 FILLER             PIC X(03) VALUE SPACES.
           05 FILLER             PIC X(17) VALUE 'FOR THE STATE OF '.
           05 RPT-STATE-CODE     PIC X(02) VALUE SPACES.
           05 FILLER             PIC X(92) VALUE SPACES.  

       01  RPT-AGENT-HEADER-LN-1.
           05 FILLER             PIC X(03) VALUE SPACES.
           05 FILLER             PIC X(07) VALUE 'AGENT: '.
           05 RPT-AGENT-CODE     PIC X(10) VALUE SPACES.
           05 FILLER             PIC X(03) VALUE ' - '.
           05 RPT-AGENT-NAME     PIC X(45) VALUE SPACES.
           05 FILLER             PIC X(65) VALUE SPACES.  

       01  RPT-AGENT-HEADER-LN-2.
           05 FILLER             PIC X(10) VALUE SPACES.
           05 RPT-AGENT-ADDR-1   PIC X(50) VALUE SPACES.
           05 FILLER             PIC X(73) VALUE SPACES.     

       01  RPT-AGENT-HEADER-LN-3.
           05 FILLER             PIC X(10) VALUE SPACES.
           05 RPT-AGENT-ADDR-2   PIC X(50) VALUE SPACES.
           05 FILLER             PIC X(73) VALUE SPACES.    

       01  RPT-AGENT-HEADER-LN-4.
           05 FILLER             PIC X(10) VALUE SPACES.
           05 RPT-AGENT-CITY     PIC X(20) VALUE SPACES.
           05 FILLER             PIC X(02) VALUE SPACES.   
           05 RPT-AGENT-STATE    PIC X(02) VALUE SPACES.            
           05 FILLER             PIC X(02) VALUE SPACES.   
           05 RPT-AGENT-ZIP-CD   PIC X(10) VALUE SPACES.                   
           05 FILLER             PIC X(73) VALUE SPACES.  

       01  RPT-AGENT-HEADER-LN-5.
           05 FILLER             PIC X(10) VALUE SPACES.
           05 RPT-AGENT-CONTACT  PIC X(10) VALUE SPACES.
           05 FILLER             PIC X(02) VALUE SPACES.   
           05 RPT-AGENT-EMAIL    PIC X(30) VALUE SPACES.            
           05 FILLER             PIC X(81) VALUE SPACES.       

       01  RPT-POLICY-LN-1.
           05 FILLER             PIC X(10) VALUE SPACES.
           05 FILLER             PIC X(10) VALUE 'POLICY NO '.
           05 FILLER             PIC X(02) VALUE SPACES.   
           05 FILLER             PIC X(73) VALUE 'HOLDER NAME'. 
           05 FILLER             PIC X(02) VALUE SPACES.
           05 FILLER             PIC X(10) VALUE 'START DATE'.
           05 FILLER             PIC X(02) VALUE SPACES. 
           05 FILLER             PIC X(11) VALUE 'EXPIRY DATE'. 
           05 FILLER             PIC X(02) VALUE SPACES.
           05 FILLER             PIC X(10) VALUE 'PREMIUM'. 
           05 FILLER             PIC X(01) VALUE SPACES. 

       01  RPT-POLICY-LN-2.
           05 FILLER             PIC X(10) VALUE SPACES.
           05 FILLER             PIC X(10) VALUE 'POLICY NO '.
           05 FILLER             PIC X(02) VALUE SPACES.   
           05 FILLER             PIC X(73) VALUE '-----------'. 
           05 FILLER             PIC X(02) VALUE SPACES.
           05 FILLER             PIC X(10) VALUE '----------'.
           05 FILLER             PIC X(02) VALUE SPACES. 
           05 FILLER             PIC X(11) VALUE '-----------'. 
           05 FILLER             PIC X(02) VALUE SPACES.
           05 FILLER             PIC X(10) VALUE '-------'. 
           05 FILLER             PIC X(01) VALUE SPACES. 

       01  RPT-POLICY-LN-3.
           05 FILLER             PIC X(10) VALUE SPACES.
           05 RPT-POL-NO         PIC X(10) VALUE SPACES.
           05 FILLER             PIC X(02) VALUE SPACES.   
           05 RPT-POL-HOLDER     PIC X(73) VALUE SPACES.
           05 FILLER             PIC X(02) VALUE SPACES.
           05 RPT-POL-ST-DATE    PIC X(10) VALUE SPACES.
           05 FILLER             PIC X(02) VALUE SPACES. 
           05 RPT-POL-EXP-DATE   PIC X(10) VALUE SPACES. 
           05 FILLER             PIC X(03) VALUE SPACES.
           05 RPT-POL-PREMIUM    PIC X(10) VALUE SPACES. 
           05 FILLER             PIC X(01) VALUE SPACES.            

       01  RPT-AGENT-SUMMARY-LINE.
           05 FILLER             PIC X(03) VALUE SPACES.
           05 FILLER             PIC X(07) VALUE 'AGENT: '.
           05 RPT-AGENT-CD       PIC X(10) VALUE SPACES.
           05 FILLER             PIC X(02) VALUE SPACES.           
           05 FILLER             PIC X(14) VALUE 'POLICY COUNT: '.
           05 RPT-AGENT-POL-CNT  PIC 99,999.           
           05 FILLER             PIC X(02) VALUE SPACES.           
           05 FILLER             PIC X(16) VALUE 'POLICY PREMIUM: '.
           05 RPT-AGENT-POL-PREM PIC 999,999,999. 
           05 FILLER             PIC X(01) VALUE SPACES.           

       01  RPT-STATE-SUMMARY-LINE.
           05 FILLER             PIC X(03) VALUE SPACES.
           05 FILLER             PIC X(07) VALUE 'STATE: '.
           05 RPT-STATE-CD       PIC X(02) VALUE SPACES.
           05 FILLER             PIC X(02) VALUE SPACES.           
           05 FILLER             PIC X(14) VALUE 'POLICY COUNT: '.
           05 RPT-STATE-POL-CNT  PIC 999,999.           
           05 FILLER             PIC X(02) VALUE SPACES.           
           05 FILLER             PIC X(16) VALUE 'POLICY PREMIUM: '.
           05 RPT-STATE-POL-PREM PIC 999,999,999.
           05 FILLER             PIC X(69) VALUE SPACES.       

       01  RPT-GRAND-SUMMARY-LINE.
           05 FILLER             PIC X(03) VALUE SPACES.
           05 FILLER             PIC X(07) VALUE 'GRAND SUMMARY: '.
           05 FILLER             PIC X(02) VALUE SPACES.           
           05 FILLER             PIC X(14) VALUE 'POLICY COUNT: '.
           05 RPT-GRAND-POL-CNT  PIC 999,999.           
           05 FILLER             PIC X(02) VALUE SPACES.           
           05 FILLER             PIC X(16) VALUE 'POLICY PREMIUM: '.
           05 RPT-GRAND-POL-PREM PIC 999,999,999.
           05 FILLER             PIC X(69) VALUE SPACES. 

       01  RPT-FILLER-LINE. 
           05 FILLER             PIC X(133) VALUE SPACES.       

       01 WS-CURRENT-STATE       PIC X(02).  
       01 WS-CURRENT-AGENT       PIC X(10).  
      *01 WS-POLICY-COUNT        PIC 9(5) VALUE 0.  
      *01 WS-AGENT-POLICY-CNT    PIC 9(5) VALUE 0.  
      *01 WS-STATE-POLICY-CNT    PIC 9(5) VALUE 0.  
      *01 WS-TOTAL-POLICY-COUNT  PIC 9(5) VALUE 0.  
      *01 WS-PREMIUM-TOTAL       PIC 9(9)V99 VALUE 0.  
       01 WS-AGENT-TOTAL-POL-CNT PIC 9(9)    VALUE 0.  
       01 WS-AGENT-TOTAL-PREM    PIC 9(9)V99 VALUE 0.  
       01 WS-STATE-TOTAL-POL-CNT PIC 9(9)    VALUE 0.
       01 WS-STATE-TOTAL-PREM    PIC 9(9)V99 VALUE 0.
       01 WS-GRAND-TOTAL-POL-CNT PIC 9(9)    VALUE 0.  
       01 WS-GRAND-TOTAL-PREM    PIC 9(9)V99 VALUE 0. 

      *01  IS-FIRST-RECORD-SW     PIC X(1) VALUE 'N'.
      *    88 IS-FIRST-RECORD     VALUE 'Y'.


       PROCEDURE DIVISION.  
       1000-MAIN-PARA.  
           PERFORM 0000-INITIALIZE-PARA  
           PERFORM 2000-PROCESS-PARA  
           PERFORM 9000-FINALIZE-PARA  
           STOP RUN.  
  
       0000-INITIALIZE-PARA.  
           MOVE 'OPEN' TO WS-DBDRIVR1-OPERATION-TYPE  
           MOVE FUNCTION CURRENT-DATE (1:4) TO WS-YEAR  
           MOVE FUNCTION CURRENT-DATE (5:2) TO WS-MONTH  
           MOVE FUNCTION CURRENT-DATE (7:2) TO WS-DAY  
           MOVE '/'                         TO WS-CUR-FILLER1
                                               WS-CUR-FILLER2

           MOVE WS-CURRENT-DATE             TO WS-DBDRIVR1-PROCESS-DATE  
           CALL 'DBDRIVR1' USING DBDRIVR1-AREA  
           IF WS-DBDRIVR1-SQLCODE NOT = 0  
               DISPLAY 'ERROR OPENING CURSOR: ' WS-DBDRIVR1-SQLCODE  
               CALL 'ABEND'
           END-IF.  
           
           MOVE 'OPEN'                 TO WS-FLEDIVR1-OPERATION-TYPE  
           CALL 'FLDRIVR1' USING FLEDIVR1-AREA  
           IF WS-FLEDIVR1-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR OPENING AGENT FILE: ' 
                                                WS-FLEDIVR1-STATUS-CODE  
               CALL 'ABEND'  
           END-IF.

           MOVE 'CUSTOMER-NOTIFY-FILE' TO WS-FILE-NAME  
           MOVE 'OPEN'                 TO WS-FLEDIVR2-OPERATION-TYPE  
           CALL 'FLDRIVR2' USING FLEDIVR2-AREA  
           IF WS-FLEDIVR2-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR OPENING AGENT NOTIFY FILE: ' 
                                        WS-FLEDIVR2-STATUS-CODE  
               CALL 'ABEND'   
           END-IF.   

           MOVE 'NOTIFY-REPORT-FILE'   TO WS-FILE-NAME  
           MOVE 'OPEN'                 TO WS-FLEDIVR2-OPERATION-TYPE  
           CALL 'FLDRIVR2' USING FLEDIVR2-AREA  
           IF WS-FLEDIVR2-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR OPENING NOTIFY REPORT FILE: ' 
                                        WS-FLEDIVR2-STATUS-CODE  
               CALL 'ABEND'   
           END-IF.           

           MOVE 'AGENT-NOTIFY-FILE' TO WS-FILE-NAME  
           MOVE 'OPEN'              TO WS-FLEDIVR2-OPERATION-TYPE  
           CALL 'FLDRIVR2' USING FLEDIVR2-AREA  
           IF WS-FLEDIVR2-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR OPENING AGENT NOTIFY FILE: ' 
                                        WS-FLEDIVR2-STATUS-CODE  
               CALL 'ABEND'   
           END-IF.     

           PERFORM 3270-WRITE-REPORT-HEADER.       
  
       2000-PROCESS-PARA.  
           MOVE 'FETCH' TO WS-DBDRIVR1-OPERATION-TYPE  
           PERFORM UNTIL NO-MORE-POLICY   
               CALL 'DBDRIVR1' USING DBDRIVR1-AREA  
               PERFORM 2100-CHECK-POLICY-CALL-STATUS
               IF POLICY-FOUND  
                   MOVE WS-DBDRIVR1-POLICY-RECORD TO POLICY-RECORD
                   PERFORM 2200-GET-AGENT-DETAIL
                   PERFORM 3000-WRITE-CUSTOMER-NOTIFICATION 
                   PERFORM 2300-UPDATE-TRACKING 
                   PERFORM 2500-PROCESS-SUMMARY
               END-IF  

           END-PERFORM.  
  
       2100-CHECK-POLICY-CALL-STATUS.
           IF WS-DBDRIVR1-SQLCODE = 100
              SET NO-MORE-POLICY TO TRUE
           ELSE
              IF WS-DBDRIVR1-SQLCODE = 0
                 SET POLICY-FOUND TO TRUE
              ELSE   
                   DISPLAY 'ERROR FETCHING RECORD: ' WS-DBDRIVR1-SQLCODE  
                   CALL 'ABEND'  
              END-IF 
           END-IF.                 
           
       2200-GET-AGENT-DETAIL.
           MOVE POLICY-AGENT-CODE         TO WS-FLEDIVR1-AGENT-CODE  
           MOVE 'SEARCH'                  TO WS-FLEDIVR1-OPERATION-TYPE  
           CALL 'FLDRIVR1' USING FLEDIVR1-AREA  
           IF WS-FLEDIVR1-STATUS-CODE = '00'  
               MOVE WS-FLEDIVR1-AGENT-RECORD TO AGENT-RECORD
               PERFORM 3100-WRITE-AGENT-NOTIFICATION
           ELSE  
               DISPLAY 'ERROR FETCHING AGENT RECORD: ' 
                                             WS-FLEDIVR1-STATUS-CODE  
               CALL 'ABEND' 
           END-IF.             

       2300-UPDATE-TRACKING.
           MOVE 'INSERT'                   TO WS-DBDRIVR2-OPERATION-TYPE  
           MOVE WS-CURRENT-DATE            TO WS-DBDRIVR2-PROCESS-DATE
           MOVE POLICY-NUMBER              TO WS-DBDRIVR2-POLICY-NUMBER
           CALL 'DBDRIVR2' USING DBDRIVR2-AREA.
           IF WS-DBDRIVR2-SQLCODE NOT EQUAL 0
               DISPLAY 'ERROR INSERTING INTO TTRACKING  SQLCODE: ' 
                                                    WS-DBDRIVR2-SQLCODE  
               CALL 'ABEND' 
           END-IF.                  

       2500-PROCESS-SUMMARY.
           IF  POLICY-HOLDER-STATE NOT EQUAL WS-CURRENT-STATE
               IF  WS-GRAND-TOTAL-POL-CNT NOT EQUAL 0
                   PERFORM 3210-WRITE-AGENT-SUMMARY
                   PERFORM 3260-WRITE-BREAK-LINE
                   PERFORM 3220-WRITE-STATE-SUMMARY 
               END-IF 
               PERFORM 2510-RESET-AGENT-TOTALS
               PERFORM 2520-RESET-STATE-TOTALS
               MOVE POLICY-HOLDER-STATE TO WS-CURRENT-STATE  
               PERFORM 3260-WRITE-BREAK-LINE
               PERFORM 3240-WRITE-STATE-HEADER
               PERFORM 3260-WRITE-BREAK-LINE
               PERFORM 3250-WRITE-AGENT-HEADER
               PERFORM 3260-WRITE-BREAK-LINE
               PERFORM 3260-WRITE-POLICY-HEADER
           ELSE
               IF  AGENT-CODE NOT EQUAL WS-CURRENT-AGENT 
                   PERFORM 3210-WRITE-AGENT-SUMMARY
                   PERFORM 2510-RESET-AGENT-TOTALS
                   MOVE AGENT-CODE   TO WS-CURRENT-AGENT
                   PERFORM 3260-WRITE-BREAK-LINE
                   PERFORM 3250-WRITE-AGENT-HEADER
                   PERFORM 3260-WRITE-BREAK-LINE
                   PERFORM 3260-WRITE-POLICY-HEADER
                END-IF
           END-IF.  
           PERFORM 3270-WRITE-POLICY-DET-LINE.
           ADD 1                     TO  WS-AGENT-TOTAL-POL-CNT
                                         WS-STATE-TOTAL-POL-CNT
                                         WS-GRAND-TOTAL-POL-CNT.
           ADD POLICY-PREMIUM-AMOUNT TO  WS-AGENT-TOTAL-PREM
                                         WS-STATE-TOTAL-PREM
                                         WS-GRAND-TOTAL-PREM.

       2520-RESET-STATE-TOTALS.
           MOVE 0                        TO WS-STATE-TOTAL-POL-CNT.
           MOVE 0                        TO WS-STATE-TOTAL-PREM.

       2510-RESET-AGENT-TOTALS.
           MOVE 0                        TO WS-AGENT-TOTAL-POL-CNT.
           MOVE 0                        TO WS-AGENT-TOTAL-PREM.           

       3000-WRITE-CUSTOMER-NOTIFICATION.
           PERFORM 3050-POPULATE-CUSTOMER-DETAIL
           MOVE 'CUSTOMER-NOTIFY-FILE' TO WS-FILE-NAME  
           MOVE 'WRITE'                TO WS-FLEDIVR2-OPERATION-TYPE  
           MOVE WS-CUST-NOTIFY-RECORD  TO WS-CUSTOMER-NOTIFY-RECORD
           CALL 'FLDRIVR2' USING FLEDIVR2-AREA  
           IF WS-FLEDIVR2-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR WRITING TO CUSTOMER NOTIFY FILE: ' 
                           WS-FLEDIVR2-STATUS-CODE  
           END-IF.    

       3100-WRITE-AGENT-NOTIFICATION.
           PERFORM 3150-POPULATE-AGENT-DETAIL
           IF AGENT-TYPE IS EQUAL 'CORPORATE'
               MOVE 'AGENT-NOTIFY-FILE'    TO WS-FILE-NAME  
               MOVE 'WRITE'                TO WS-FLEDIVR2-OPERATION-TYPE  
               MOVE WS-AGENT-NOTIFY-RECORD
                                       TO WS-FLEDIVR2-AGENT-NOTIFY-REC
               CALL 'FLDRIVR2' USING FLEDIVR2-AREA  
               IF WS-FLEDIVR2-STATUS-CODE NOT = '00'  
                   DISPLAY 'ERROR WRITING TO AGENT NOTIFY FILE: ' 
                           WS-FLEDIVR2-STATUS-CODE  
               END-IF
           END-IF.         
       
       3200-WRITE-NOTIFICATION-REPORT.
           MOVE 'NOTIFY-REPORT-FILE' TO WS-FILE-NAME  
           MOVE 'WRITE'                TO WS-FLEDIVR2-OPERATION-TYPE  
      *    MOVE WS-CUST-NOTIFY-RECORD  TO WS-NOTIFY-REPORT-RECORD
           CALL 'FLDRIVR2' USING FLEDIVR2-AREA  
           IF WS-FLEDIVR2-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR WRITING TO NOTIFY REPORT FILE: ' 
                           WS-FLEDIVR2-STATUS-CODE  
           END-IF.   
       
       3220-WRITE-STATE-SUMMARY.
           MOVE AGENT-CODE              TO  RPT-STATE-CD.
           MOVE WS-STATE-TOTAL-POL-CNT  TO  RPT-STATE-POL-CNT.
           MOVE WS-STATE-TOTAL-PREM     TO  RPT-STATE-POL-PREM.       
           MOVE RPT-STATE-SUMMARY-LINE  TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT.

       3210-WRITE-AGENT-SUMMARY.
           MOVE AGENT-CODE              TO  RPT-AGENT-CD.
           MOVE WS-AGENT-TOTAL-POL-CNT  TO  RPT-AGENT-POL-CNT.
           MOVE WS-AGENT-TOTAL-PREM     TO  RPT-AGENT-POL-PREM.
           MOVE RPT-AGENT-SUMMARY-LINE  TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT.

       3230-WRITE-GRAND-SUMMARY.
           MOVE WS-GRAND-TOTAL-POL-CNT  TO RPT-GRAND-POL-CNT.
           MOVE WS-GRAND-TOTAL-PREM     TO RPT-GRAND-POL-PREM.
           MOVE RPT-GRAND-SUMMARY-LINE  TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT.   

       3240-WRITE-STATE-HEADER.
           MOVE WS-CURRENT-STATE        TO RPT-STATE-CODE
           MOVE RPT-STATE-HEADER        TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT.  

       3250-WRITE-AGENT-HEADER.
           MOVE AGENT-CODE              TO  RPT-AGENT-CODE
           MOVE AGENT-NAME              TO  RPT-AGENT-NAME
           MOVE AGENT-ADDRESS-1         TO  RPT-AGENT-ADDR-1
           MOVE AGENT-ADDRESS-2         TO  RPT-AGENT-ADDR-2
           MOVE AGENT-CITY              TO  RPT-AGENT-CITY
           MOVE AGENT-STATE             TO  RPT-AGENT-STATE
           MOVE AGENT-ZIP-CD            TO  RPT-AGENT-ZIP-CD
           MOVE AGENT-CONTACT-NO        TO  RPT-AGENT-CONTACT
           MOVE AGENT-EMAIL             TO  RPT-AGENT-EMAIL
           MOVE RPT-AGENT-HEADER-LN-1   TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT.
           MOVE RPT-AGENT-HEADER-LN-2   TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT.   
           MOVE RPT-AGENT-HEADER-LN-3   TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT. 
           MOVE RPT-AGENT-HEADER-LN-4   TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT. 
           MOVE RPT-AGENT-HEADER-LN-5   TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT. 

       3260-WRITE-POLICY-HEADER.
           MOVE RPT-POLICY-LN-1         TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT. 
           MOVE RPT-POLICY-LN-2         TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT. 
       
       3270-WRITE-POLICY-DET-LINE.
           MOVE POLICY-NUMBER  TO RPT-POL-NO
           STRING POLICY-HOLDER-FNAME, ' ', POLICY-HOLDER-MNAME, ' ', 
                  POLICY-HOLDER-LNAME 
             INTO RPT-POL-HOLDER
           MOVE POLICY-START-DATE     TO RPT-POL-ST-DATE
           MOVE POLICY-EXPIRY-DATE    TO RPT-POL-EXP-DATE
           MOVE POLICY-PREMIUM-AMOUNT TO RPT-POL-PREMIUM
           
           MOVE RPT-POLICY-LN-3         TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT.                       
       
       3260-WRITE-BREAK-LINE.
           MOVE RPT-FILLER-LINE         TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT.

       3270-WRITE-REPORT-HEADER.
           PERFORM 3260-WRITE-BREAK-LINE
           MOVE WS-CURRENT-DATE         TO  RPT-DATE.
           MOVE RPT-MAIN-HEADER         TO  WS-NOTIFY-REPORT-RECORD.
           PERFORM 3200-WRITE-NOTIFICATION-REPORT.
           PERFORM 3260-WRITE-BREAK-LINE.


       3050-POPULATE-CUSTOMER-DETAIL.
           MOVE POLICY-NUMBER          TO  WS-CUST-POLICY-NUMBER.  
           MOVE POLICY-HOLDER-FNAME    TO  WS-CUST-FIRST-NAME. 
           MOVE POLICY-HOLDER-MNAME    TO  WS-CUST-MIDDLE-NAME.  
           MOVE POLICY-HOLDER-LNAME    TO  WS-CUST-LAST-NAME.  
           MOVE POLICY-HOLDER-ADDR-1   TO  WS-CUST-ADDR-1.
           MOVE POLICY-HOLDER-ADDR-2   TO  WS-CUST-ADDR-2.
           MOVE POLICY-HOLDER-CITY     TO  WS-CUST-CITY.
           MOVE POLICY-HOLDER-STATE    TO  WS-CUST-STATE.
           MOVE POLICY-HOLDER-ZIP-CD   TO  WS-CUST-ZIP-CD.
           MOVE POLICY-START-DATE      TO  WS-CUST-START-DATE.  
           MOVE POLICY-EXPIRY-DATE     TO  WS-CUST-EXPIRY-DATE.  
           MOVE WS-CURRENT-DATE        TO  WS-CUST-NOTIFY-DATE.
           MOVE POLICY-BENEF-NAME      TO  WS-CUST-BENEF-NAME.
           MOVE 'PLEASE NOTE YOUR POLICY IS EXPIRING SOON. GET IT RENEWE
      -         'D TO CONTINUE COVERAGE' 
                                       TO  WS-CUST-NOTIFY-MSG.  
           MOVE POLICY-AGENT-CODE      TO  WS-CUST-AGENT-CODE. 
           MOVE AGENT-NAME             TO  WS-CUST-AGENT-NAME.  
           MOVE POLICY-BENEF-NAME      TO  WS-CUST-BENEF-NAME.
           MOVE 'IF YOU FAIL TO RENEW BY EXPIRY DATE YOUR INSURANCE COVE
      -         'RAGE WILL END'
                                       TO  WS-CUST-STATUTORY-MSG.           

       3150-POPULATE-AGENT-DETAIL.
           MOVE AGENT-CODE               TO  WS-AGENT-CODE.  
           MOVE AGENT-NAME               TO  WS-AGENT-NAME.  
           MOVE AGENT-ADDRESS-1          TO  WS-AGENT-ADDRESS-1.  
           MOVE AGENT-ADDRESS-2          TO  WS-AGENT-ADDRESS-2.
           MOVE AGENT-CITY               TO  WS-AGENT-CITY.
           MOVE AGENT-STATE              TO  WS-AGENT-STATE.
           MOVE AGENT-ZIP-CD             TO  WS-AGENT-ZIP-CD.
           MOVE AGENT-EMAIL              TO  WS-AGENT-EMAIL.
           MOVE POLICY-NUMBER            TO  WS-POLICY-NUMBER.
           MOVE POLICY-HOLDER-FNAME      TO  WS-POLICY-HOLDER-FNAME.
           MOVE POLICY-HOLDER-MNAME      TO  WS-POLICY-HOLDER-MNAME.
           MOVE POLICY-HOLDER-LNAME      TO  WS-POLICY-HOLDER-LNAME.
           MOVE POLICY-START-DATE        TO  WS-POLICY-START-DATE.
           MOVE POLICY-EXPIRY-DATE       TO  WS-POLICY-EXPIRY-DATE.
           MOVE WS-CURRENT-DATE          TO  WS-NOTIFY-DATE.
           MOVE 'PLEASE NOTE CUSTOMER POLICY IS EXPIRING SOON' 
                                        TO  WS-AGENT-NOTIFY-MSG.
           

       9000-FINALIZE-PARA.  
           MOVE 'CLOSE' TO WS-DBDRIVR1-OPERATION-TYPE  
           CALL 'DBDRIVR1' USING DBDRIVR1-AREA  
           IF WS-DBDRIVR1-SQLCODE NOT = 0  
               DISPLAY 'ERROR CLOSING CURSOR: ' WS-DBDRIVR1-SQLCODE  
               CALL 'ABEND' 
           END-IF.  

           MOVE 'CLOSE' TO WS-FLEDIVR1-OPERATION-TYPE  
           CALL 'FLDRIVR1' USING FLEDIVR1-AREA  
           IF WS-FLEDIVR1-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR CLOSING AGENT FILE: ' 
                                        WS-FLEDIVR1-STATUS-CODE  
               CALL 'ABEND'   
           END-IF.
  
           MOVE 'CUSTOMER-NOTIFY-FILE' TO WS-FILE-NAME  
           MOVE 'CLOSE' TO WS-FLEDIVR2-OPERATION-TYPE  
           CALL 'FLDRIVR2' USING FLEDIVR2-AREA  
           IF WS-FLEDIVR2-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR CLOSING AGENT NOTIFY FILE: ' 
                                     WS-FLEDIVR2-STATUS-CODE  
               CALL 'ABEND'  
           END-IF.
  
           MOVE 'NOTIFY-REPORT-FILE' TO WS-FILE-NAME  
           MOVE 'CLOSE' TO WS-FLEDIVR2-OPERATION-TYPE  
           CALL 'FLDRIVR2' USING FLEDIVR2-AREA  
           IF WS-FLEDIVR2-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR CLOSING NOTIFY REPORT FILE: ' 
                                     WS-FLEDIVR2-STATUS-CODE  
               CALL 'ABEND'  
           END-IF.

           MOVE 'AGENT-NOTIFY-FILE' TO WS-FILE-NAME  
           MOVE 'CLOSE' TO WS-FLEDIVR2-OPERATION-TYPE  
           CALL 'FLDRIVR2' USING FLEDIVR2-AREA  
           IF WS-FLEDIVR2-STATUS-CODE NOT = '00'  
               DISPLAY 'ERROR CLOSING AGENT NOTIFY FILE: ' 
                                     WS-FLEDIVR2-STATUS-CODE  
               CALL 'ABEND'  
           END-IF.

       END PROGRAM MAINPGM.  