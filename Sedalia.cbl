       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FTB86994.
      *PROGRAM-NAME.  DE-CONVERSION CHASE CARDS AND RELATED ACCOUNTS.
       AUTHOR.        TED BARSTOW (PREFERRED RESOURCES INC.).
       INSTALLATION.   
       DATE-WRITTEN.  MARCH 1996.
       DATE-COMPILED.
      ******************************************************************
      *C   PROGRAM ID: SEDALIA
      *C   FUNCTION:
      *C
      *C   -    IDENTIFY CARD RELATIONSHIPS BASED ON ACCOUNT NUMBERS
      *C        IDENTIFIED FOR DECONVERSION (SOLD TO ANOTHER BANK).
      *C
      *C   -    FILES AND REPORTS WILL BE PRODUCED TO SUPPORT DECONV
      *C        ERSION.  'ONLY' FILE AND REPORT CONTAINS CARDS THAT
      *C        ARE RELATED TO AN ACCOUNT THAT HAS BEEN SOLD.  THAT
      *C        CARD WILL BE STATUSED SO IT CAN NO LONGER BE USED.
      *C
      *C  -    'PLUS' FILES AND REPORTS CONTAIN CARDS THAT HAVE A
      *C        RELATIONSHIP WITH AT LEAST ONE SOLD ACCOUNT AND ALSO
      *C        ONE ACCOUNT THAT HAS NOT BEEN SOLD.  THE SOLD ACCOUNT
      *C        RELATIONSHIP WILL BE REMOVED FROM THE CARD.
      *C
      *C
      *C   INPUT:
      *C
      *C        TRANLATI LEGACY ACCOUNT MASTER  C1.DDA01.ACCMST01(0)
      *C        TRANLATI MERITS ACCOUNT MASTER  ????????????????????
      *C        TRANLOGI CONNEX CROSS REFERENCE V3.FTB01.AP01XRF
      *C
      *C   OUTPUT:
      *C
      *C        TRANLOGO DECOMMISSION ONLY       'C1.ATC01.TRNLOG04'
      *C        TRANLOGO DECOMMISSION PLUS       'C1.ATC01.TRNLOG04'
      *C
      *C   JOBSTREAM:
      *C
      *C        ATC01J50
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT DECONV-ACCOUNT-FILE       ASSIGN  TO ACCOUNTI
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS ACCOUNT-KEY
                  FILE STATUS IS ACCOUNT-STATUS.

           SELECT CARD-XREF-FILE            ASSIGN  TO CARDXRFI
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY IS CARD-KEY
                  FILE STATUS IS CARD-STATUS.

           SELECT DECONV-ONLY-OUT           ASSIGN  TO DONLYO.

           SELECT DECONV-PLUS-OUT           ASSIGN  TO DPLUSO.

           SELECT DECONV-ONLY-RPT           ASSIGN  TO DONLYRPT.

           SELECT DECONV-PLUS-RPT           ASSIGN  TO DPLUSRPT.

           SELECT SORT-FILE                 ASSIGN  TO SORTFILE.


       DATA DIVISION.
       FILE SECTION.


       FD  DECONV-ACCOUNT-FILE
      *    RECORD CONTAINS 1 TO 1210.
           RECORDING MODE V.

       01  DECONV-ACCOUNT-RECORD                 PIC X(1206).

       FD  CARD-XREF-FILE
      *    RECORDING MODE IS V
           RECORD IS VARYING IN SIZE FROM 1 TO 3996
                  DEPENDING ON RECORD-LENGTH.

       01  CARD-XREF-RECORD.
           05  FILLER                            PIC X(02).
           05  CARD-KEY                          PIC X(40).
           05  FILLER                            PIC X(3954).

       FD  DECONV-ONLY-OUT.

       01  DECONV-ONLY-REC                       PIC X(80).

       FD  DECONV-PLUS-OUT.

       01  DECONV-PLUS-REC                       PIC X(80).

       FD  DECONV-ONLY-RPT.

       01  DECONV-ONLY-RPT-REC                   PIC X(133).

       FD  DECONV-PLUS-RPT.

       01  DECONV-PLUS-RPT-REC.                  PIC X(133).


       SD  SORT-FILE.

       01  SORT-RECORD.
           05 SORT-CARD-NUMBER                   PIC X(16).

       WORKING-STORAGE SECTION.

       01  HEADER-01.
           05 F                    PIC X(25)   VALUE SPACE.
           05 F                    PIC X(10)   VALUE 'MERCANTILE'.
           05 F                    PIC X(03)   VALUE SPACE.
           05 F                    PIC X(04)   VALUE 'BANK'.
           05 F                    PIC X(03)   VALUE SPACE.
           05 F                    PIC X(02)   VALUE 'OF'.
           05 F                    PIC X(03)   VALUE SPACE.
           05 F                    PIC X(03)   VALUE 'ST.'.
           05 F                    PIC X(01)   VALUE SPACE.
           05 F                    PIC X(05)   VALUE 'LOUIS'.
           05 F                    PIC X(03)   VALUE SPACE.
           05 F                    PIC X(04)   VALUE 'N.A.'.
           05 F                    PIC X(50)   VALUE SPACE.
           05 F                    PIC X(05)   VALUE 'DATE:'.
           05 F                    PIC X(02)   VALUE SPACE.
           05 H01-DATE-MM          PIC X(02).
           05 F                    PIC X(01)   VALUE '/'.
           05 H01-DATE-DD          PIC X(02).
           05 F                    PIC X(01)   VALUE '/'.
           05 H01-DATE-CCYY        PIC X(04).

       01  HEADER-02.
           05 F                    PIC X(30)   VALUE SPACE.
           05 F                    PIC X(12)   VALUE 'DECOMMISSION'.
           05 F                    PIC X(03)   VALUE SPACE.
           05 F                    PIC X(06)   VALUE 'REPORT'.
           05 F                    PIC X(67)   VALUE SPACE.
           05 F                    PIC X(05)   VALUE 'TIME:'.
           05 F                    PIC X(02)   VALUE SPACE.
           05 H02-TIME-HH          PIC Z9.
           05 F                    PIC X(01)   VALUE ':'.
           05 H02-TIME-MM          PIC X(02).
           05 F                    PIC X(01)   VALUE ':'.
           05 H02-TIME-SS          PIC X(02).

       01  HEADER-03.
           05 F                    PIC X(30)   VALUE SPACE.
           05 F                    PIC X(14)   VALUE 'DECOMMISSIONED'.
           05 F                    PIC X(03)   VALUE SPACE.
           05 F                    PIC X(08)   VALUE 'ACCOUNTS'.
           05 F                    PIC X(03)   VALUE SPACE.
           05 F                    PIC X(04)   VALUE 'ONLY'.
           05 F                    PIC X(59)   VALUE SPACE.
           05 F                    PIC X(05)   VALUE 'PAGE:'.
           05 F                    PIC X(02)   VALUE SPACE.
           05 H03-PAGE             PIC Z(04).

       01  HEADER-04.
           05 F                    PIC X(01)   VALUE SPACE.
           05 F                    PIC X(10)   VALUE 'CARDHOLDER'.
           05 F                    PIC X(08)   VALUE SPACE.
           05 F                    PIC X(04)   VALUE 'NAME'.
           05 F                    PIC X(26)   VALUE SPACE.
           05 F                    PIC X(07)   VALUE 'ADDRESS'.
           05 F                    PIC X(30)   VALUE SPACE.
           05 F                    PIC X(04)   VALUE 'TYPE'.
           05 F                    PIC X(01)   VALUE SPACE.
           05 F                    PIC X(08)   VALUE 'ACCOUNTS'.
           05 F                    PIC X(02)   VALUE SPACE.
           05 F                    PIC X(08)   VALUE 'ACCOUNTS'.
           05 F                    PIC X(04)   VALUE SPACE.
           05 F                    PIC X(10)   VALUE 'PIN OFFSET'.
           05 F                    PIC X(10)   VALUE SPACE.

       01  DETAIL-01.
           05 F                    PIC X(01)   VALUE SPACE.
           05 D01-CARDHOLDER       PIC X(16).
           05 F                    PIC X(02)   VALUE SPACE.
           05 D01-NAME             PIC X(35).
           05 F                    PIC X(02)   VALUE SPACE.
           05 D01-ADDRESS          PIC X(35).
           05 F                    PIC X(01)   VALUE SPACE.
           05 D01-TYPE-1           PIC X(03).
           05 F                    PIC X(01)   VALUE SPACE.
           05 D01-ACCOUNT-1        PIC X(10).
           05 F                    PIC X(02)   VALUE SPACE.
           05 D01-TYPE-2           PIC X(03).
           05 F                    PIC X(01)   VALUE SPACE.
           05 D01-ACCOUNT-2        PIC X(10).
           05 F                    PIC X(07)   VALUE SPACE.
           05 D01-PIN-OFFSET       PIC X(04).

       01  DETAIL-02.
           05 F                    PIC X(50)   VALUE SPACE.
           05 D02-ADDRESS          PIC X(35).
           05 F                    PIC X(01)   VALUE SPACE.
           05 D02-TYPE-1           PIC X(03).
           05 F                    PIC X(01)   VALUE SPACE.
           05 D02-ACCOUNT-1        PIC X(10).
           05 F                    PIC X(02)   VALUE SPACE.
           05 D02-TYPE-2           PIC X(03).
           05 F                    PIC X(01)   VALUE SPACE.
           05 D02-ACCOUNT-2        PIC X(10).
           05 F                    PIC X(17)   VALUE SPACE.

       01  WS-CARD-REC.
           05 FILLER                             PIC X(02).
           05 WS-CARD-KEY.
              10 FILLER                          PIC X(04) VALUE '0001'.
              10 FILLER                          PIC X(02) VALUE SPACES.
              10 FILLER                          PIC X(02) VALUE '10'.
              10 WS-CARD-ACCOUNT-NBR             PIC X(10).
              10 FILLER                          PIC X(18) VALUE SPACES.
              10 WS-ACCOUNT-TYPE                 PIC X(03).
              10 FILLER                          PIC X(01) VALUE SPACES.
           05 FILLER                             PIC X(36).
           05 WS-PRIME-CARD                      PIC X(16).
           05 FILLER                             PIC X(18).
           05 SECONDARY-CARD-SEGMENTS OCCURS 25 TIMES
                                      INDEXED BY SCS-IDX.
              10 WS-SECONDARY-CODE               PIC X(02).
              10 FILLER                          PIC X(06).
              10 WS-SECONDARY-CARD               PIC X(16).
              10 FILLER                          PIC X(24).
           05 FILLER                             PIC X(2688).

       01  WS-START-KEY                     PIC X(40) VALUE LOW-VALUES.

       01  WS-DECONV-OUT.
           05 WS-CARD-NUMBER                     PIC X(16).
           05 FILLER                             PIC X(04).
           05 WS-ACCOUNT-NUMBER OCCURS 5 TIMES
                                INDEXED BY WS-ACCT-IDX.
              10 WS-ACCT-NUM                     PIC X(10).
              10 FILLER                          PIC X(02).

       01  EOF-SW                                PIC X(01).
       01  WS-CARD-ACCT-LOAD-SW                  PIC X(01).
       01  WS-DECONV-ONLY-SW                     PIC X(01).
       01  CARD-STATUS                           PIC X(02).
       01  ACCOUNT-STATUS                        PIC X(02).
       01  WS-SEARCH-CARD                        PIC X(16).
       01  HOLD-SORT-CARD-NUMBER                 PIC X(16)  VALUE SPACE.
       01  RECORD-LENGTH                         PIC 9(05)  COMP-3
                                                            VALUE ZERO.

       01  COUNTERS.
           05 WS-CARD-CNT                        PIC S9(07)  COMP-3
                                                     VALUE 0.
           05 WS-RECORDS-OUT                     PIC S9(07)  COMP-3.
           05 WS-DISPLAY-COUNTERS                PIC Z,ZZZ,ZZ9.

       01  DECONV-ACCOUNT-TABLE.
           05 ACCOUNT-TABLE        OCCURS 1 TO 15000 TIMES
                                   DEPENDING ON DECONV-ACCOUNT-CNT
                                   ASCENDING KEY IS ACCOUNT-NUMBER
                                   INDEXED BY ACCT-IDX.
              10 ACCOUNT-NUMBER                  PIC X(10).

       01  DECONV-CARD-TABLE.
           05 CARD-TABLE           OCCURS 1 TO 35000 TIMES
                                   DEPENDING ON DECONV-CARD-CNT
                                   ASCENDING KEY IS CARD-NUMBER
                                   INDEXED BY CARD-IDX.
              10 CARD-NUMBER                     PIC X(16).
              10 CARD-ONLY-PLUS                  PIC X(01).
              10 CARD-ACCOUNTS-RD                PIC X(110).
              10 CARD-ACCOUNTS     REDEFINES CARD-ACCOUNTS-RD
                                   OCCURS 10 TIMES
                                   INDEXED BY CA-IDX.
                 15 CARD-ACCOUNT                 PIC X(10).
                 15 CARD-ACCOUNT-ONLY-PLUS       PIC X(01).

       01  DECONV-ACCOUNT-CNT                  PIC 9(05)  COMP-3.
       01  DECONV-CARD-CNT                     PIC 9(05)  COMP-3.

       PROCEDURE DIVISION.

       A100-MAINLINE SECTION.
      ******************************************************************
      *C
      *C  THIS PARAGRAPH CONTROLS THE PROGRAM PROCESSING FLOW.
      *C
      ******************************************************************

      *    DISPLAY 'A100'.

           PERFORM U050-OPEN-FILES.

           PERFORM A200-LOAD-ACCOUNT-TABLE.

           SORT SORT-FILE ASCENDING KEY SORT-CARD-NUMBER
                INPUT   PROCEDURE IS  S100-SORT-CARDS
                OUTPUT  PROCEDURE IS  S200-LOAD-CARD-TABLE.


           PERFORM A300-PROCESS-XREF-SEQUENT.

           PERFORM A400-PROCESS-TABLES.

           PERFORM A800-DISPLAY-IO-COUNTS.

           PERFORM U060-CLOSE-FILES.

           STOP RUN.


       A200-LOAD-ACCOUNT-TABLE SECTION.

      *    DISPLAY 'A200'.

           MOVE 'N'                      TO EOF-SW.
           MOVE ZERO                     TO DECONV-ACCOUNT-CNT.

           PERFORM B900-READ-ACCOUNT-FILE.

           PERFORM B100-PROCESS-ACCOUNT-FILE
              VARYING ACCT-IDX FROM 1 BY 1
                 UNTIL EOF-SW = 'Y'      OR
                       ACCT-IDX  > 15000.



       A300-PROCESS-XREF-SEQUENT SECTION.

      *    DISPLAY 'A300'.

           MOVE 'N'                   TO EOF-SW.

           PERFORM D900-START-XREF.

           PERFORM D950-READ-NEXT-XREF.

           PERFORM D000-PROCESS-XREF-CONT
              UNTIL EOF-SW = 'Y'.


       A400-PROCESS-TABLES SECTION.

      *    DISPLAY 'A400'.

           PERFORM T100-WALK-CARD-TABLE
              VARYING CARD-IDX FROM 1 BY 1
                 UNTIL CARD-IDX > 35000     OR
                       CARD-IDX > DECONV-CARD-CNT.

       A800-DISPLAY-IO-COUNTS SECTION.

           DISPLAY '*  *  *  *  *  *  PROCESSING COUNTS  '
                   '*  *  *  *  *  *'.
           DISPLAY SPACES.

           MOVE DECONV-CARD-CNT        TO WS-DISPLAY-COUNTERS.
           DISPLAY 'CARDS RELEASED TO TABLE   =  ' WS-DISPLAY-COUNTERS.

           MOVE WS-RECORDS-OUT           TO WS-DISPLAY-COUNTERS.
           DISPLAY 'LOG RECORDS OUTPUT        =  ' WS-DISPLAY-COUNTERS.


       B100-PROCESS-ACCOUNT-FILE SECTION.

      *    DISPLAY 'B100'.

           IF DECONV-ACCOUNT-NUMBER > ACCOUNT-NUMBER (ACCT-IDX - 1)
              MOVE DECONV-ACCOUNT-NUMBER
                                         TO ACCOUNT-NUMBER (ACCT-IDX)
              ADD 1                      TO DECONV-ACCOUNT-CNT
           ELSE
              DISPLAY 'TABLE ORDER CORRUPTED, ABORTING......'
              STOP RUN.

      *    DISPLAY 'ACCOUNT TABLE  = '  ACCOUNT-NUMBER (ACCT-IDX).

           PERFORM B900-READ-ACCOUNT-FILE.


       B900-READ-ACCOUNT-FILE SECTION.

      *    DISPLAY 'B900'.

           READ DECONV-ACCOUNT-FILE
                AT END
                   MOVE 'Y'              TO EOF-SW.


       D000-PROCESS-XREF-CONT SECTION.

      *    DISPLAY 'D000'.

           PERFORM D100-COMPARE-CARDS.

           PERFORM D950-READ-NEXT-XREF.

       D100-COMPARE-CARDS SECTION.

      *    DISPLAY 'D100'.

           MOVE WS-PRIME-CARD            TO WS-SEARCH-CARD.

           PERFORM D110-SEARCH-CARD-TABLE.

           PERFORM D115-COMPARE-SECONDARY-CARDS
              VARYING SCS-IDX FROM 1 BY 1
                 UNTIL SCS-IDX > 25     OR
                       WS-SECONDARY-CODE (SCS-IDX) = 'XX'.

       D110-SEARCH-CARD-TABLE SECTION.

      *    DISPLAY 'D110'.

      *    DISPLAY 'WS-SEARCH-CARD  =  ' WS-SEARCH-CARD.


           SEARCH ALL CARD-TABLE
             WHEN
               CARD-NUMBER (CARD-IDX)  =  WS-SEARCH-CARD
               PERFORM D120-LOAD-CARD-ACCOUNTS.

      *    DISPLAY 'CARD-NUMBER     =  ' CARD-NUMBER (CARD-IDX).


       D115-COMPARE-SECONDARY-CARDS SECTION.

      *    DISPLAY 'D115'.

      *    DISPLAY 'SECONDARY-CODE  =  ' WS-SECONDARY-CODE (SCS-IDX).
      *    DISPLAY 'SECONDARY-CARD  =  ' WS-SECONDARY-CARD (SCS-IDX).

           IF WS-SECONDARY-CODE (SCS-IDX) = 'XS'
              MOVE WS-SECONDARY-CARD (SCS-IDX)   TO WS-SEARCH-CARD
              PERFORM D110-SEARCH-CARD-TABLE.


       D120-LOAD-CARD-ACCOUNTS SECTION.

      *    DISPLAY 'D120'.
      *    DISPLAY 'CARD-NUMBER     =  ' CARD-NUMBER (CARD-IDX).

           MOVE 'N'                  TO WS-CARD-ACCT-LOAD-SW.

           PERFORM D121-LOAD-ACCOUNTS
              VARYING CA-IDX FROM 1 BY 1
                 UNTIL CA-IDX > 10    OR
                       WS-CARD-ACCT-LOAD-SW = 'Y'.


       D121-LOAD-ACCOUNTS SECTION.

      *    DISPLAY 'D121'.

           IF CARD-ACCOUNT (CARD-IDX CA-IDX) = WS-CARD-ACCOUNT-NBR
              DISPLAY 'ACCT ALREADY EXISTED  = ' WS-CARD-ACCOUNT-NBR
              MOVE 'Y'                  TO WS-CARD-ACCT-LOAD-SW
           ELSE
           IF CARD-ACCOUNT (CARD-IDX CA-IDX) = SPACES
              DISPLAY 'ACCT LOADED           = ' WS-CARD-ACCOUNT-NBR
              MOVE 'Y'                  TO WS-CARD-ACCT-LOAD-SW
              MOVE WS-CARD-ACCOUNT-NBR TO CARD-ACCOUNT(CARD-IDX CA-IDX).

           SEARCH ALL ACCOUNT-TABLE
              AT END
                 MOVE 'P'      TO CARD-ACCOUNT-ONLY-PLUS
                                  CARD-ONLY-PLUS
              WHEN ACCOUNT-NUMBER (ACCT-IDX) =
                   CARD-ACCOUNT (CARD-IDX CA-IDX)
                 MOVE 'O'      TO CARD-ACCOUNT-ONLY-PLUS.


       D900-START-XREF SECTION.

      *    DISPLAY 'D900'.

           MOVE WS-START-KEY                TO CARD-KEY.

           START CARD-XREF-FILE KEY NOT LESS THAN CARD-KEY.

           IF CARD-STATUS = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'BAD START ON XREF FILE, STATUS IS '
              CARD-STATUS.

       D950-READ-NEXT-XREF SECTION.

      *    DISPLAY 'D950'.

           READ CARD-XREF-FILE NEXT INTO WS-CARD-REC.

           IF CARD-STATUS = '00'
              NEXT SENTENCE
           ELSE
           IF CARD-STATUS = '10'
              MOVE 'Y'                 TO EOF-SW
           ELSE
              DISPLAY 'BAD READ NEXT ON XREF FILE, STATUS IS '
              CARD-STATUS.


       S100-SORT-CARDS SECTION.

      *    DISPLAY 'S100'.

           PERFORM S110-GET-CARDS-FOR-ACCTS
              VARYING ACCT-IDX FROM 1 BY 1
                 UNTIL ACCT-IDX > DECONV-ACCOUNT-CNT   OR
                       CARD-IDX > 35000.

       S110-GET-CARDS-FOR-ACCTS SECTION.

      *    DISPLAY 'S110'.

           PERFORM S120-READ-CARD-XREF-FILE.

           IF CARD-STATUS = '00'
              PERFORM S140-RELEASE-PRIME-KEY
              PERFORM S160-RELEASE-SECONDARY-KEYS
                 VARYING SCS-IDX FROM 1 BY 1
                    UNTIL SCS-IDX > 25   OR
                          WS-SECONDARY-CODE (SCS-IDX) = 'XX'.

       S120-READ-CARD-XREF-FILE SECTION.

      *    DISPLAY 'S120'.

           MOVE ACCOUNT-NUMBER (ACCT-IDX)     TO WS-CARD-ACCOUNT-NBR.
           MOVE 'DDA'                         TO WS-ACCOUNT-TYPE.

           MOVE WS-CARD-KEY                   TO CARD-KEY.

           READ CARD-XREF-FILE INTO WS-CARD-REC
                               KEY IS CARD-KEY.

           IF CARD-STATUS = '00'
              NEXT SENTENCE
           ELSE
           IF CARD-STATUS = '23'
              MOVE 'SAV'                   TO WS-ACCOUNT-TYPE
              MOVE WS-CARD-KEY             TO CARD-KEY
              READ CARD-XREF-FILE INTO WS-CARD-REC
                                  KEY IS CARD-KEY
              IF CARD-STATUS = '00'
                 NEXT SENTENCE
              ELSE
                 DISPLAY 'BAD READ CARD FILE, STATUS IS ' CARD-STATUS
                 DISPLAY 'KEY IS ' CARD-KEY.

       S140-RELEASE-PRIME-KEY SECTION.

      *    DISPLAY 'S140'.

           MOVE WS-PRIME-CARD      TO SORT-CARD-NUMBER.

           RELEASE SORT-RECORD.

       S160-RELEASE-SECONDARY-KEYS SECTION.

      *    DISPLAY 'S160'.

           IF WS-SECONDARY-CODE (SCS-IDX) = 'XS'
              MOVE WS-SECONDARY-CARD (SCS-IDX)  TO SORT-CARD-NUMBER
              RELEASE SORT-RECORD.

       S200-LOAD-CARD-TABLE SECTION.

      *    DISPLAY 'S200'.

           MOVE 'N'                     TO EOF-SW.
           MOVE ZERO                    TO DECONV-CARD-CNT.

           PERFORM S290-RETURN-SORTED-CARDS.

           PERFORM S210-LOAD-TABLE
              VARYING CARD-IDX FROM 1 BY 1
                 UNTIL EOF-SW = 'Y'   OR
                       CARD-IDX > 35000.

       S210-LOAD-TABLE SECTION.

      *    DISPLAY 'S210'.

           IF SORT-CARD-NUMBER = HOLD-SORT-CARD-NUMBER
              SET CARD-IDX DOWN BY 1
           ELSE
              MOVE SORT-CARD-NUMBER     TO CARD-NUMBER (CARD-IDX)
                                           HOLD-SORT-CARD-NUMBER
              MOVE 'O'                  TO CARD-ONLY-PLUS (CARD-IDX)
              MOVE SPACES               TO CARD-ACCOUNTS-RD (CARD-IDX)
              ADD 1                     TO DECONV-CARD-CNT.

      *    DISPLAY CARD-NUMBER (CARD-IDX).


           PERFORM S290-RETURN-SORTED-CARDS.

       S290-RETURN-SORTED-CARDS SECTION.

      *    DISPLAY 'S290'.

           RETURN SORT-FILE
                  AT END
                     MOVE 'Y'     TO EOF-SW.


       T100-WALK-CARD-TABLE SECTION.

      *    DISPLAY 'T100'.

           MOVE 'Y'                        TO WS-DECONV-ONLY-SW.

           SET WS-ACCT-IDX                 TO 1.

           MOVE SPACES                     TO WS-DECONV-OUT.
           MOVE CARD-NUMBER (CARD-IDX)     TO WS-CARD-NUMBER.

      *    DISPLAY CARD-TABLE (CARD-IDX).


           PERFORM T200-WALK-CARD-ACCT-TABLE
              VARYING CA-IDX FROM 1 BY 1
                 UNTIL CA-IDX > 10     OR
                       CARD-ACCOUNT (CARD-IDX CA-IDX) = SPACES.
      *                WS-DECONV-ONLY-SW = 'N'.

           IF WS-DECONV-ONLY-SW = 'Y'
              WRITE DECONV-ONLY-REC FROM WS-DECONV-OUT
      *
      * ADD LOGIC TO BUILD APMAINT RECORD TO STATUS CARD
      *
           ELSE
              WRITE DECONV-PLUS-REC FROM WS-DECONV-OUT.

       T200-WALK-CARD-ACCT-TABLE SECTION.

      *    DISPLAY 'T200'.

           IF WS-ACCT-IDX < 6
              MOVE CARD-ACCOUNT (CARD-IDX CA-IDX)  TO
                   WS-ACCT-NUM (WS-ACCT-IDX).

           SET WS-ACCT-IDX UP BY 1.

           PERFORM T300-SEARCH-ACCOUNT-TABLE.


       T300-SEARCH-ACCOUNT-TABLE SECTION.

      *    DISPLAY 'T300'.
      *    DISPLAY 'CARD-ACCOUNT  = ' CARD-ACCOUNT (CARD-IDX CA-IDX).


           SEARCH ALL ACCOUNT-TABLE
              AT END
                 MOVE 'N'      TO WS-DECONV-ONLY-SW
      *
      * ADD LOGIC TO BUILD APMAINT RECORD TO REMOVE BAD ACCOUNT
      *
              WHEN ACCOUNT-NUMBER (ACCT-IDX) =
                   CARD-ACCOUNT (CARD-IDX CA-IDX).

      *    DISPLAY 'DECONV-ONLY-SW  =  ' WS-DECONV-ONLY-SW.


       U050-OPEN-FILES SECTION.
      **************************************************************
      *C
      *C  THIS PARAGRAPH OPENS ALL FILES.
      *C
      **************************************************************

      *    DISPLAY 'U050'.

           OPEN INPUT   DECONV-ACCOUNT-FILE
                        CARD-XREF-FILE
                OUTPUT  DECONV-ONLY-OUT
                        DECONV-PLUS-OUT
                        DECONV-ONLY-RPT
                        DECONV-PLUS-RPT.

           IF ACCOUNT-STATUS = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING ACCOUNT FILE, STATUS IS  '
              ACCOUNT-STATUS.

           IF CARD-STATUS = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING CARD FILE, STATUS IS  '
              CARD-STATUS.


       U060-CLOSE-FILES SECTION.
      **************************************************************
      *C
      *C  THIS PARAGRAPH CLOSES ALL FILES.
      *C
      **************************************************************

      *    DISPLAY 'U060'.

           CLOSE DECONV-ACCOUNT-FILE
                 CARD-XREF-FILE
                 DECONV-ONLY-OUT
                 DECONV-PLUS-OUT.

      **************************************************************
      *C
      *C  END OF MODULE
      *C
      **************************************************************
