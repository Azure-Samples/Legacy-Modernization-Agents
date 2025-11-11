      *************************************************************************
      * DECLGEN TABLE(INSURNCE.TTRAKING)                                      *
      *************************************************************************
           EXEC SQL DECLARE INSURNCE.TTRAKING TABLE (  
               TR_POLICY_NUMBER     CHAR(10)       NOT NULL,  
               TR_NOTIFY_DATE       DATE           NOT NULL,
               TR_STATUS            CHAR(1)        NOT NULL,  
               TR_ADD_TIMESTAMP     TIMESTAMP      NOT NULL 
                                                       WITH DEFAULT, 
               TR_UPDATE_TIMESTAMP TIMESTAMP   NOT NULL 
                                                       WITH DEFAULT   
           )
           END-EXEC.             
      *************************************************************************
      * COBOL DECLARATION FOR TABLE INSURNCE.TTRAKING                         *
      *************************************************************************
       01 DCLTRAKI.  
           05 TR-POLICY-NUMBER          PIC X(10).  
           05 TR-NOTIFY-DATE            PIC X(10).
           05 TR-STATUS                 PIC X(1).
           05 TR-ADD-TIMESTAMP          PIC X(26).  
           05 TR-UPDATE-TIMESTAMP       PIC X(26).  
      *************************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5              *
      ************************************************************************* 