      *************************************************************************
      * DECLGEN TABLE(INSURNCE.TPOLICY)                                       *
      *************************************************************************
           EXEC SQL DECLARE INSURNCE.TPOLICY TABLE (  
               POLICY_NUMBER        CHAR(10)       NOT NULL,  
               POLICY_HOLDER_FNAME  CHAR(35)       NOT NULL, 
               POLICY_HOLDER_MNAME  CHAR(1)        NOT NULL, 
               POLICY_HOLDER_LNAME  CHAR(35)       NOT NULL,  
               POLICY_BENEF_NAME    CHAR(60)       NOT NULL,
               POLICY_BENEF_RELATION CHAR(15)      NOT NULL,
               POLICY_HOLDER_ADDR_1 CHAR(100)      NOT NULL, 
               POLICY_HOLDER_ADDR_2 CHAR(100)      NOT NULL,
               POLICY_HOLDER_CITY   CHAR(30)       NOT NULL,
               POLICY_HOLDER_STATE  CHAR(2)        NOT NULL, 
               POLICY_HOLDER_ZIP_CD CHAR(10)       NOT NULL, 
               POLICY_HOLDER_DOB    CHAR(10)       NOT NULL,
               POLICY_HOLDER_GENDER CHAR(8)        NOT NULL,  
               POLICY_HOLDER_PHONE  CHAR(10)       NOT NULL,
               POLICY_HOLDER_EMAIL  CHAR(30)       NOT NULL,
               POLICY_PAYMENT_FREQ  CHAR(10)       NOT NULL,
               POLICY_PAYMENT_METHOD CHAR(8)       NOT NULL,
               POLICY_UNDERWRITER   CHAR(50)       NOT NULL,
               POLICY_TERMS_COND    CHAR(200)      NOT NULL,
               POLICY_CLAIMED       CHAR(1)        NOT NULL,
               POLICY_DISCOUNT_CODE CHAR(10)       NOT NULL,
               POLICY_PREMIUM_AMOUNT 
                                    DECIMAL(7,2)   NOT NULL,
               POLICY_COVERAGE_AMOUNT 
                                    DECIMAL(10,2)  NOT NULL,
               POLICY_TYPE          CHAR(50)       NOT NULL,  
               POLICY_START_DATE    DATE           NOT NULL,  
               POLICY_EXPIRY_DATE   DATE           NOT NULL,  
               POLICY_STATUS        CHAR(1)        NOT NULL,  
               POLICY_AGENT_CODE    CHAR(10)       NOT NULL,  
               POLICY_NOTIFY_FLAG   CHAR(1)        NOT NULL,
               POLICY_ADD_TIMESTAMP TIMESTAMP      NOT NULL 
                                                           WITH DEFAULT, 
               POLICY_UPDATE_TIMESTAMP TIMESTAMP   NOT NULL 
                                                           WITH DEFAULT 
           )
           END-EXEC.             
      *************************************************************************
      * COBOL DECLARATION FOR TABLE INSURNCE.TPOLICY                          *
      *************************************************************************
       01 DCLPOLICY.  
           05 POLICY-NUMBER            PIC X(10).  
           05 POLICY-HOLDER-FNAME      PIC X(35).  
           05 POLICY-HOLDER-MNAME      PIC X(01).
           05 POLICY-HOLDER-LNAME      PIC X(35).
           05 POLICY-BENEF-NAME        PIC X(60).
           05 POLICY-BENEF-RELATION    PIC X(15).
           05 POLICY-HOLDER-ADDR-1     PIC X(100). 
           05 POLICY-HOLDER-ADDR-2     PIC X(100). 
           05 POLICY-HOLDER-CITY       PIC X(30).           
           05 POLICY-HOLDER-STATE      PIC X(2). 
           05 POLICY-HOLDER-ZIP-CD     PIC X(10).
           05 POLICY-HOLDER-DOB        PIC X(10). 
           05 POLICY-HOLDER-GENDER     PIC X(8).   
           05 POLICY-HOLDER-PHONE      PIC X(10). 
           05 POLICY-HOLDER-EMAIL      PIC X(30). 
           05 POLICY-PAYMENT-FREQ      PIC X(10). 
           05 POLICY-PAYMENT-METHOD    PIC X(8). 
           05 POLICY-UNDERWRITER       PIC X(50). 
           05 POLICY-TERMS-COND        PIC X(200). 
           05 POLICY-CLAIMED           PIC X(1). 
           05 POLICY-DISCOUNT-CODE     PIC X(10). 
           05 POLICY-PREMIUM-AMOUNT    PIC S9(5)V9(2) COMP-3. 
           05 POLICY-COVERAGE-AMOUNT   PIC S9(8)V9(2) COMP-3.                  
           05 POLICY-TYPE              PIC X(50).  
           05 POLICY-START-DATE        PIC X(10).  
           05 POLICY-EXPIRY-DATE       PIC X(10). 
           05 POLICY-STATUS            PIC X.  
           05 POLICY-AGENT-CODE        PIC X(10).
           05 POLICY-NOTIFY-FLAG       PIC X(1).
           05 POLICY-ADD-TIMESTAMP     PIC X(26).  
           05 POLICY-UPDATE-TIMESTAMP  PIC X(26).  
      *************************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15             *
      ************************************************************************* 