       IDENTIFICATION DIVISION.  
       PROGRAM-ID. FLDRIVR1.  
       ENVIRONMENT DIVISION.  
       INPUT-OUTPUT SECTION.  
       FILE-CONTROL.  
           SELECT AGENT-FILE ASSIGN TO 'AGENTVSAM'  
               ORGANIZATION IS INDEXED  
               ACCESS MODE IS DYNAMIC  
               RECORD KEY IS AGENT-CODE  
               FILE STATUS IS FILE-STATUS-CODE.  
  
       DATA DIVISION.  
       FILE SECTION.  
       FD  AGENT-FILE.  
           COPY CAGENT.
  
       WORKING-STORAGE SECTION.  
  
       01  FILE-STATUS-CODE          PIC X(2).  
           88  FILE-STATUS-OK        VALUE '00'.  
           88  FILE-STATUS-NOT-FOUND VALUE '23'. 

       01  WS-OPERATION-TYPE    PIC X(8). 
       01  WS-AGENT-CODE        PIC X(10).
       01  WS-STATUS-CODE       PIC X(2).

       LINKAGE SECTION.  
       01  LNK-AREA.  
           05  LNK-INPUT-AREA.              
               10 LNK-OPERATION-TYPE    PIC X(8).  
               10 LNK-AGENT-CODE        PIC X(10).
           05 LNK-OUTPUT-AREA.                 
               10 LNK-STATUS-CODE       PIC X(2).  
               10 WS-RETURN-AGENT-RECORD.  
                  15 LNK-AGENT-NAME        PIC X(30).  
                  15 LNK-AGENT-ADDRESS-1   PIC X(50).  
                  15 LNK-AGENT-ADDRESS-2   PIC X(50).  
                  15 LNK-AGENT-CITY        PIC X(20).  
                  15 LNK-AGENT-STATE       PIC X(2).
                  15 LNK-AGENT-ZIP-CODE    PIC X(10).
                  15 LNK-AGENT-STATUS      PIC X(1).
                  15 LNK-AGENT-TYPE        PIC X(10).
                  15 LNK-AGENT-EMAIL       PIC X(30).
                  15 LNK-AGENT-CONTACT-NO  PIC X(10).
                  15 LNK-AGENT-START-DATE  PIC X(10).
                  15 LNK-AGENT-END-DATE    PIC X(10).                   

       PROCEDURE DIVISION USING LNK-AREA.  
         
       MAIN-PROCEDURE.  
           PERFORM INITIALIZE-PARA.
           EVALUATE WS-OPERATION-TYPE  
               WHEN 'OPEN'  
                   PERFORM OPEN-AGENT-FILE  
               WHEN 'CLOSE'  
                   PERFORM CLOSE-AGENT-FILE  
               WHEN 'SEARCH'  
                   PERFORM SEARCH-AGENT-FILE  
               WHEN OTHER  
                   MOVE '99' TO WS-STATUS-CODE  
           END-EVALUATE  
           GOBACK.  
  
       INITIALIZE-PARA.
           MOVE LNK-OPERATION-TYPE TO WS-OPERATION-TYPE.
           MOVE LNK-AGENT-CODE     TO WS-AGENT-CODE.

       OPEN-AGENT-FILE.  
           OPEN INPUT AGENT-FILE  
           IF FILE-STATUS-OK  
               MOVE '00' TO WS-STATUS-CODE  
           ELSE  
               MOVE FILE-STATUS-CODE TO WS-STATUS-CODE  
               PERFORM ERROR-HANDLING.  
  
       CLOSE-AGENT-FILE.  
           CLOSE AGENT-FILE  
           IF FILE-STATUS-OK  
               MOVE '00' TO WS-STATUS-CODE  
           ELSE  
               MOVE FILE-STATUS-CODE TO WS-STATUS-CODE  
               PERFORM ERROR-HANDLING.  
  
       SEARCH-AGENT-FILE.  
           MOVE WS-AGENT-CODE TO AGENT-CODE  
           READ AGENT-FILE  
               INVALID KEY  
                   MOVE FILE-STATUS-CODE TO WS-STATUS-CODE  
                   PERFORM ERROR-HANDLING  
               NOT INVALID KEY  
                   MOVE '00'            TO WS-STATUS-CODE  
                   MOVE AGENT-NAME      TO LNK-AGENT-NAME  
                   MOVE AGENT-ADDRESS-1 TO LNK-AGENT-ADDRESS-1  
                   MOVE AGENT-ADDRESS-2 TO LNK-AGENT-ADDRESS-2  
                   MOVE AGENT-CITY      TO LNK-AGENT-CITY  
                   MOVE AGENT-STATE     TO LNK-AGENT-STATE
                   MOVE AGENT-ZIP-CD    TO LNK-AGENT-ZIP-CODE
                   MOVE AGENT-STATUS    TO LNK-AGENT-STATUS
                   MOVE AGENT-TYPE      TO LNK-AGENT-TYPE
                   MOVE AGENT-CONTACT-NO
                                        TO LNK-AGENT-CONTACT-NO
                   MOVE AGENT-EMAIL     TO LNK-AGENT-EMAIL 
                   MOVE AGENT-START-DATE
                                        TO LNK-AGENT-START-DATE
                   MOVE AGENT-END-DATE  TO LNK-AGENT-END-DATE.
  
       ERROR-HANDLING.  
           DISPLAY 'IN FLEDIVR1'
           DISPLAY 'ERROR: ' WS-OPERATION-TYPE ' ON AGENTVSAM FILE STATU
      -            'S CODE: ' FILE-STATUS-CODE.
           CALL 'ABEND'.
  
       FINALIZE-PARA.
           MOVE WS-STATUS-CODE TO LNK-STATUS-CODE.

       END PROGRAM FLDRIVR1.  