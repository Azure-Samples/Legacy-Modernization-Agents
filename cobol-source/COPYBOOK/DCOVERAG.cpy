      *************************************************************************
      * DECLGEN TABLE(INSURNCE.TCOVERAG)                                      *
      *************************************************************************
           EXEC SQL DECLARE INSURNCE.TCOVERAG TABLE (  
               COVERAGE_POL_NUM     CHAR(10)       NOT NULL,  
               COVERAGE_STATUS      CHAR(10)       NOT NULL, 
               COVERAGE_START_DT    CHAR(10)       NOT NULL,
               COVERAGE_END_DT      CHAR(10)       NOT NULL,
               COVERAGE_ADD_TS      TIMESTAMP      NOT NULL 
                                                       WITH DEFAULT, 
               COVERAGE_UPDATE_TS   TIMESTAMP      NOT NULL 
                                                       WITH DEFAULT   
           )
           END-EXEC.             
      *************************************************************************
      * COBOL DECLARATION FOR TABLE INSURNCE.TCOVERAG                         *
      *************************************************************************
       01 DCLCOVGE.  
           05 COVERAGE-POL-NUM          PIC X(10).
           05 COVERAGE-STATUS           PIC X(10).           
           05 COVERAGE-STATUS           PIC X(10).  
           05 COVERAGE-START-DT         PIC X(10).
           05 COVERAGE-END-DT           PIC X(10).
           05 COVERAGE-ADD-TS           PIC X(26).  
           05 COVERAGE-UPDATE-TS        PIC X(26).  
      *************************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8              *
      ************************************************************************* 