       IDENTIFICATION DIVISION.
       PROGRAM-ID. DLITCBL.
       REMARKS.

           READS A FILE OF BANKS TO BE CONVERTED.  CONVERTS ALL SEGMENTS
           FOR THAT BANK, THEN PROCEEDS TO NEXT BANK.

           THIS PROGRAM EXECUTES AS A BMP.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CONVERSION-CONTROL-FILE   ASSIGN  TO CONTROLI.

       DATA DIVISION.
       FILE SECTION.

       FD  CONVERSION-CONTROL-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 CHARACTERS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS CONVERSION-CONTROL-RECORD.

       01  CONVERSION-CONTROL-RECORD.
           05 CONV-FROM-BANK                     PIC X(03).
           05 FILLER                             PIC X(01).
           05 CONV-TO-BANK                       PIC X(03).
           05 FILLER                             PIC X(73).

       WORKING-STORAGE SECTION.
       01  FILLER                           PIC  X(40) VALUE
           'WORKING STORAGE FOR S2864101 STARTS HERE'.

      ************************************************************
      *              S W I T C H E S
      ************************************************************
       01  WS-INPUT-BANK-EOF-SW        PIC X(01) VALUE 'N'.
           88  INPUT-BANK-AT-EOF                 VALUE 'Y'.
           88  INPUT-BANK-NOT-AT-EOF             VALUE 'N'.
      ************************************************************
      *           I M S   C A L L   F U N C T I O N S
      ************************************************************
       01  WS-IMS-CALL-FUNCTIONS.
           05  GU-CALL-FUNC            PIC X(04) VALUE 'GU  '.
           05  GHU-CALL-FUNC           PIC X(04) VALUE 'GHU '.
           05  GHN-CALL-FUNC           PIC X(04) VALUE 'GHN '.
           05  GNP-CALL-FUNC           PIC X(04) VALUE 'GNP '.
           05  GHNP-CALL-FUNC          PIC X(04) VALUE 'GHNP'.
           05  ISRT-CALL-FUNC          PIC X(04) VALUE 'ISRT'.
           05  DLET-CALL-FUNC          PIC X(04) VALUE 'DLET'.
           05  REPL-CALL-FUNC          PIC X(04) VALUE 'REPL'.
           05  CHKP-CALL-FUNC          PIC X(04) VALUE 'CHKP'.
           05  WS-CALL-FUNC            PIC X(04) VALUE SPACES.

      ************************************************************
      *              C O U N T E R S
      ************************************************************
       01  WS-COUNTERS.
           05  WS-BANK-RECS-READ-CTR   PIC 9(10) VALUE ZEROS.
           05  WS-ROOTS-READ-CTR       PIC 9(10) VALUE ZEROS.
           05  WS-PCS-ROOTS-READ-CTR   PIC 9(10) VALUE ZEROS.
           05  WS-PCS-ROOT-NOTFND      PIC 9(10) VALUE ZEROS.
           05  WS-ROOTS-ISRT-CTR       PIC 9(10) VALUE ZEROS.
           05  WS-ATSSRV-DLET-CTR      PIC 9(10) VALUE ZEROS.
           05  WS-SEGS-ISRT-CTR        PIC 9(10) VALUE ZEROS.
           05  WS-SEGS-READ-CTR        PIC 9(10) VALUE ZEROS.
           05  WS-RECORDS-WRITTEN-CTR  PIC 9(10) VALUE ZEROS.
           05  WS-NBR-DUPL-ROOTS-CTR   PIC 9(10) VALUE ZEROS.
           05  WS-DUP-CHILDREN-CTR     PIC 9(10) VALUE ZEROS.
           05  WS-CHKPTS-TAKEN-CTR     PIC 9(10) VALUE ZEROS.

      ************************************************************
      *              M I S C   A N D   H O L D   A R E A S
      ************************************************************
       01  ABENDIT                     PIC X(01) VALUE SPACES.
       01  BLOW-UP REDEFINES ABENDIT   PIC 9(01) COMP-3.
       01  ABEND-STATUS                PIC X(02).
       01  ABEND-CALL-FUNC             PIC X(04).
       01  ABEND-SSA                   PIC X(50).
       01  ABEND-PARAGRAPH             PIC X(40).
       01  HOLD-ACCOUNT-NBR-11.
           05  HOLD-ACCOUNT-NBR-10     PIC X(10).
           05  FILLER                  PIC X(01).
       01  CHECK-POINT-WORK-AREA.
           05  CHKPT-CTR               PIC 9(08) VALUE ZEROS.
           05  CHKPT-KEEPER            PIC 9(08) VALUE ZEROS.

       01  DB-RECORD                   PIC X(50).


      ************************************************************
      *          BALANCE       S S A ' S
      ************************************************************

       01  BALANCE-BOOLEAN-SSA.
           05  FILLER                   PIC X(08)  VALUE 'BALROOT '.
           05  FILLER                   PIC X(01)  VALUE '('.
           05  FILLER                   PIC X(08)  VALUE 'ACCTKEY '.
           05  FILLER                   PIC X(02)  VALUE 'GT'.
           05  BAL-KEY-1.
               10  FILLER               PIC X(03)  VALUE 'DDA'.
               10  FILLER               PIC 9(03)  VALUE ZERO COMP-3.
               10  BAL-BANK-NO-KEY-1    PIC 9(03)             COMP-3.
               10  FILLER               PIC 9(03)  VALUE ZERO COMP-3.
               10  BAL-ACCT-NO-KEY-1    PIC X(23).
           05  FILLER                   PIC X(01)  VALUE '*'.
           05  FILLER                   PIC X(08)  VALUE 'ACCTKEY '.
           05  FILLER                   PIC X(02)  VALUE 'LT'.
           05  BAL-KEY-2.
               10  FILLER               PIC X(03)  VALUE 'DDA'.
               10  FILLER               PIC 9(03)  VALUE ZERO COMP-3.
               10  BAL-BANK-NO-KEY-2    PIC 9(03)             COMP-3.
               10  FILLER               PIC 9(03)  VALUE ZERO COMP-3.
               10  FILLER               PIC X(23)  VALUE ZERO.
           05  FILLER                   PIC X(01)  VALUE ')'.

       01  BALANCE-SSA-ALT.
           05  FILLER                   PIC X(08)  VALUE 'BALROOT '.
           05  FILLER                   PIC X(01)  VALUE '('.
           05  FILLER                   PIC X(08)  VALUE 'ACCTKEY '.
           05  FILLER                   PIC X(02)  VALUE 'EQ'.
           05  BAL-KEY-ALT.
               10  BAL-APPL-ACR-ALT     PIC X(03)  VALUE SPACE.
               10  FILLER               PIC 9(03)  VALUE ZERO COMP-3.
               10  BAL-BANK-NO-KEY-ALT  PIC 9(03)             COMP-3.
               10  FILLER               PIC 9(03)  VALUE ZERO COMP-3.
               10  BAL-ACCT-NO-ALT.
                   15  BAL-ACCT01-ALT   PIC X(01)  VALUE SPACE.
                   15  BAL-ACCT22-ALT   PIC X(22)  VALUE SPACE.
           05  FILLER                   PIC X(01)  VALUE ')'.

       01  ARUBCKT-UNQUAL-SSA           PIC X(09)  VALUE 'ARUBCKT  '.

       01  ATSBCKT-UNQUAL-SSA           PIC X(09)  VALUE 'ATSBCKT  '.

       01  BALMISC-UNQUAL-SSA           PIC X(09)  VALUE 'BALMISC  '.

       01  BALANCE-UNQUAL-SSA           PIC X(09)  VALUE 'BALROOT  '.

       01  VBALSRV-UNQUAL-SSA           PIC X(09)  VALUE 'VBALSRV  '.

       01  ATSROOT-QUAL-SSA.
           05  SYS-SEGNAME          PIC X(08) VALUE 'ATSROOT'.
           05  SYS-L-PAREN          PIC X(01) VALUE '('.
           05  SYS-KEYNAME          PIC X(08) VALUE 'ACCTKEY'.
           05  SYS-OPERATOR         PIC X(02) VALUE ' ='.
           05  KEY-OF-ATSROOT.
               10  ATSROOT-BANK     PIC 9(03) COMP-3.
               10  ATSROOT-CARDNO   PIC X(18).
           05  SYS-R-PAREN          PIC X(01) VALUE ')'.

       01  ATSSRV-QUAL-SSA.
           05  SRV-SEGNAME          PIC X(08) VALUE 'ATSSRV '.
           05  SRV-L-PAREN          PIC X(01) VALUE '('.
           05  SRV-KEYNAME          PIC X(08) VALUE 'ATSKEY  '.
           05  SRV-OPERATOR         PIC X(02) VALUE ' ='.
           05  KEY-OF-ATSSRV.
               10  ATSSRV-APPL-ACR  PIC X(03).
               10  ATSSRV-BK-HOLDING-CO PIC 9(03) COMP-3.
               10  ATSSRV-BANK-NO   PIC 9(03) COMP-3.
               10  ATSSRV-BK-BR     PIC 9(03) COMP-3.
               10  ATSSRV-ACCT-NO   PIC X(23).
           05  SRV-R-PAREN          PIC X(01) VALUE ')'.

       01  ATSSRV-UNQUAL-SSA        PIC X(09) VALUE 'ATSSRV   '.

      ************************************************************
      *          D A T A B A S E   I O   A R E A S
      ************************************************************

       COPY BALROOT.

       01  REQUESTED-KEY-BALROOT REDEFINES BALROOT-SEGMENT.
           05  REQ-KEY-BALROOT-APPL                 PIC X(03).
           05  REQ-KEY-BALROOT-HOLDCO               PIC 9(03)  COMP-3.
           05  REQ-KEY-BALROOT-BKNO                 PIC 9(03)  COMP-3.
           05  REQ-KEY-BALROOT-BKBRAN               PIC 9(03)  COMP-3.
           05  REQ-KEY-BALROOT-ACCTNO               PIC X(23).
           05  FILLER                               PIC X(24).

       COPY BALMISC.

       COPY ARUBCKT.

       COPY ATSBCKT.

       01  BALSRVRT-SEGMENT.
           05  FILLER               PIC X(100).

       01  VBALSRV-SEGMENT.
           05  VBALSRV-BANK         PIC 9(03)  COMP-3.
           05  VBALSRV-PAN          PIC X(16).
           05  VBALSRV-FLDS         PIC X(282).

           COPY ATSROOT.

           COPY ATSSRV.

       01  FILLER                           PIC X(35)
           VALUE 'WORKING STORAGE FOR S2864101 ENDS '.

       LINKAGE SECTION.

      *
       01  IO-PCB.
           03  IO-LTERM-NAME.
               05  IO-CRT               PIC XXX.
               05  IO-CRT-BANK-NO       PIC XXX.
               05  IO-CRT-AA            PIC XX.
           03  IO-RESERVE               PIC X(2).
           03  IO-STATUS                PIC X(2).
           03  IO-DATE                  PIC S9(7)  COMP-3.
           03  IO-TIME                  PIC S9(7)  COMP-3.
           03  IO-SEQ                   PIC S9(7)  COMP.
           03  IO-MOD-NAME              PIC X(8).
           03  IO-USER-ID               PIC X(8).
           03  IO-GROUP-NAME            PIC X(8).
      *
       01  BAL-PCB.
           05  BAL-DBD-NAME             PIC X(08).
           05  BAL-SEG-LEVEL            PIC X(02).
           05  BAL-STATUS-CD            PIC X(02).
           05  FILLER                   PIC X(08).
           05  BAL-SEGMENT-NAME         PIC X(08).
           05  BAL-KEY-LENGTH           PIC S9(05) COMP.
           05  BAL-SENS-SEG             PIC S9(05) COMP.
           05  BAL-KEY-FEEDBACK         PIC X(26).
      *
       01  BAL-PCB-ALT.
           05  BAL-DBD-NAME-ALT         PIC X(08).
           05  BAL-SEG-LEVEL-ALT        PIC X(02).
           05  BAL-STATUS-CD-ALT        PIC X(02).
           05  FILLER                   PIC X(08).
           05  BAL-SEGMENT-NAME-ALT     PIC X(08).
           05  BAL-KEY-LENGTH-ALT       PIC S9(05) COMP.
           05  BAL-SENS-SEG-ALT         PIC S9(05) COMP.
           05  BAL-KEY-FEEDBACK-ALT     PIC X(26).
      *
       01  BALLSRV-PCB.
           05  DBD-NAME                 PIC X(08).
           05  SEG-LEVEL                PIC X(02).
           05  BALLSRV-STATUS           PIC X(02).
           05  FILLER                   PIC X(08).
           05  SEG-NAME-FB              PIC X(08).
           05  LENGTH-FB-KEY            PIC S9(05) COMP.
           05  SENS-SEGS                PIC S9(05) COMP.
           05  KEY-FEEDBACK             PIC X(34).
      *
       01  ATS-PCB.
           05  ATS-DBD-NAME             PIC X(08).
           05  FILLER                   PIC X(02).
           05  ATS-STATUS               PIC X(02).
           05  FILLER                   PIC X(08).
           05  ATS-NAME-FB              PIC X(08).
           05  ATS-LENGTH-FB-KEY        PIC S9(05) COMP.
           05  ATS-SENS-SEGS            PIC S9(05) COMP.
           05  ATS-KEY-FEEDBACK         PIC X(34).
      *
       PROCEDURE DIVISION USING IO-PCB
                                BAL-PCB
                                BAL-PCB-ALT
                                BALLSRV-PCB
                                ATS-PCB.

       0000-MAIN.

           OPEN INPUT CONVERSION-CONTROL-FILE.

           PERFORM 9000-READ-CONV-BANK-INFO.

           IF INPUT-BANK-AT-EOF
              MOVE 1                         TO RETURN-CODE
              PERFORM 9998-TERMINATION.

           PERFORM 1000-CONVERT-BALANCE-DB
                        UNTIL INPUT-BANK-AT-EOF.

           PERFORM 9998-TERMINATION.


       1000-CONVERT-BALANCE-DB.
      ************************************************************
      * THE BANK NUMBERS TO BE CONVERTED FROM WILL BE READ INTO THE
      * PARM FIELDS AND INSERTED INTO THE KEY FIELDS OF THE BOOLEAN
      * TO GET EACH ACCOUNT NUMBER FOR THAT BANK.
      ************************************************************

           MOVE ZERO                         TO  BAL-ACCT-NO-KEY-1.

           MOVE CONV-FROM-BANK               TO  BAL-BANK-NO-KEY-1
                                                 BAL-BANK-NO-KEY-2.

           ADD 1                             TO  BAL-BANK-NO-KEY-2.

           PERFORM 5000-GHU-BALANCE-BOOLEAN.

           PERFORM 1100-PROCESS-BALANCE-DB
                        UNTIL BAL-STATUS-CD NOT EQUAL SPACES.

           PERFORM 9000-READ-CONV-BANK-INFO.


      ********************************************
      **  ISRT NEW ROOT, GET CHILDREN IN THE OLD
      **  PARENT, ISRT THE NEW CHILDREN, DLET THE OLD ROOT
      ********************************************
       1100-PROCESS-BALANCE-DB.

      *    DISPLAY '1100-'.


           PERFORM 1110-ISRT-BALANCE.

           IF BAL-STATUS-CD-ALT = SPACES
              PERFORM 6000-GHNP-BALANCE-SEGMENT
              PERFORM 1120-CONVERT-CHILD-SEGMENTS
                      UNTIL BAL-STATUS-CD EQUAL 'GE'
              IF BALDDA-CR-ADV = '2' OR '3'
                 PERFORM 1105-CONVERT-PCS-ROOT.

           IF  CHKPT-CTR > +50
               PERFORM 7000-CHECKPOINT-CALL
               MOVE ZEROS               TO CHKPT-CTR.

           PERFORM 5000-GHU-BALANCE-BOOLEAN.


       1105-CONVERT-PCS-ROOT.

      *    DISPLAY '1105-'.

           PERFORM 1106-BUILD-PCS-KEY.

           PERFORM 1107-GU-PCS-BALROOT.

           IF BAL-STATUS-CD = SPACES
              PERFORM 1110-ISRT-BALANCE
              IF BAL-STATUS-CD-ALT = SPACES
                 PERFORM 6000-GHNP-BALANCE-SEGMENT
                 PERFORM 1120-CONVERT-CHILD-SEGMENTS
                         UNTIL BAL-STATUS-CD EQUAL 'GE'.


       1106-BUILD-PCS-KEY.

      *    DISPLAY '1106-'.

           MOVE 'PCS'                    TO BAL-APPL-ACR-ALT.
           MOVE CONV-FROM-BANK           TO BAL-BANK-NO-KEY-ALT.

           IF BALDDA-CR-ADV = '2'
              MOVE 'A'                   TO BAL-ACCT01-ALT
           ELSE
              IF BALDDA-CR-ADV = '3'
                 MOVE 'B'                TO BAL-ACCT01-ALT.

           MOVE BAL-ACCT-NO              TO BAL-ACCT22-ALT.

      *    DISPLAY 'BALANCE-SSA-ALT  = ' BALANCE-SSA-ALT.

       1107-GU-PCS-BALROOT.

      *    DISPLAY '1107-'.

           CALL 'CBLTDLI'  USING  GU-CALL-FUNC
                                  BAL-PCB
                                  BALROOT-SEGMENT
                                  BALANCE-SSA-ALT.

           IF  BAL-STATUS-CD EQUAL SPACES
               ADD 1                       TO CHKPT-CTR
                                              WS-PCS-ROOTS-READ-CTR
               MOVE CONV-TO-BANK           TO BAL-BANK-NO
                                              BAL-BANK-NO-KEY-ALT
           ELSE
              IF  BAL-STATUS-CD EQUAL 'GE'
                  DISPLAY '*  *  *  *  *  *  *  *  *  *  *  *  *'
                  DISPLAY '*  PCS BALROOT DOES NOT EXIST       *'
                  DISPLAY '*  SSA  =  ' BALANCE-SSA-ALT
                  DISPLAY '*  *  *  *  *  *  *  *  *  *  *  *  *'
                  ADD  1                    TO WS-PCS-ROOT-NOTFND
              ELSE
                 MOVE GU-CALL-FUNC          TO ABEND-CALL-FUNC
                 MOVE BAL-STATUS-CD         TO ABEND-STATUS
                 MOVE BALANCE-SSA-ALT       TO ABEND-SSA
                 MOVE '1107-GU-PCS-BALROOT' TO ABEND-PARAGRAPH
                 PERFORM 9997-BUILD-IMS-ERROR
                 PERFORM 9998-TERMINATION.


      ************************************************************
      *    IMS DATABASE CALLS - INSERT USING BALANCE ALT-PCB
      ************************************************************
       1110-ISRT-BALANCE.

      *    DISPLAY '1110-'.

           CALL 'CBLTDLI'  USING  ISRT-CALL-FUNC
                                  BAL-PCB-ALT
                                  BALROOT-SEGMENT
                                  BALANCE-UNQUAL-SSA.

           IF  BAL-STATUS-CD-ALT EQUAL SPACES
               ADD 1                       TO WS-ROOTS-ISRT-CTR
                                              CHKPT-CTR
           ELSE
              IF  BAL-STATUS-CD-ALT EQUAL 'II'
                  DISPLAY '**************************************'
                  DISPLAY '** DUPL ACCT ALREADY UNDER NEW BANK'
                  DISPLAY '** NEW BANK NUMBER = ' CONV-TO-BANK
                  DISPLAY '** ACCOUNT  NUMBER = ' BAL-ACCT-NO
                  ADD  1                   TO WS-NBR-DUPL-ROOTS-CTR
                                              CHKPT-CTR
              ELSE
                 MOVE ISRT-CALL-FUNC       TO ABEND-CALL-FUNC
                 MOVE BAL-STATUS-CD-ALT    TO ABEND-STATUS
                 MOVE BAL-ACCT-NO          TO ABEND-SSA
                 MOVE '1110-ISRT-BALROOT'  TO ABEND-PARAGRAPH
                 PERFORM 9997-BUILD-IMS-ERROR
                 PERFORM 9998-TERMINATION.


       1120-CONVERT-CHILD-SEGMENTS.

      *    DISPLAY '1120-'.

           IF BAL-SEGMENT-NAME = 'BALMISC'
              MOVE DB-RECORD       TO BALMISC-SEGMENT
              PERFORM 8100-ISRT-ALT-BALMISC
           ELSE
           IF BAL-SEGMENT-NAME = 'ATSBCKT'
              MOVE DB-RECORD       TO ATSBCKT-SEGMENT
              PERFORM 8200-ISRT-ALT-ATSBCKT
           ELSE
           IF BAL-SEGMENT-NAME = 'VBALSRV'
              PERFORM 8300-GET-ATSROOT
           ELSE
           IF BAL-SEGMENT-NAME = 'ARUBCKT'
              MOVE DB-RECORD       TO ARUBCKT-SEGMENT
              PERFORM 8400-ISRT-ALT-ARUBCKT.

           PERFORM 6000-GHNP-BALANCE-SEGMENT.

      ************************************************************
      **   IMS DATABASE CALLS - USING DDDABAL-PCB
      **   USE GHU WITH SPECIFIC INFORMATION TO GET THE PRECISE RO
      **   TO DELETE THE ROOT SEGMENT.
      ************************************************************
       1130-DELETE-OLD-ROOT-SEGMENT.

      *    DISPLAY '1130-'.

           MOVE CONV-FROM-BANK           TO BAL-BANK-NO.
           MOVE REQUESTED-KEY-BALROOT    TO BAL-KEY-ALT.

           CALL 'CBLTDLI'  USING  GHU-CALL-FUNC
                                  BAL-PCB
                                  BALROOT-SEGMENT
                                  BALANCE-SSA-ALT.

           IF  BAL-STATUS-CD EQUAL SPACES
               ADD 1                     TO CHKPT-CTR
           ELSE
               MOVE GHU-CALL-FUNC        TO ABEND-CALL-FUNC
               MOVE BAL-STATUS-CD        TO ABEND-STATUS
               MOVE SPACES               TO ABEND-SSA
               MOVE '1130-DELETE-OLD-ROOT-SEGMENT'
                                         TO ABEND-PARAGRAPH
               PERFORM 9997-BUILD-IMS-ERROR
               PERFORM 9998-TERMINATION.

           PERFORM 1131-DLET-BALANCE.

      ************************************************************
      *    IMS DATABASE CALLS - USING DDDABAL-PCB
      *    (DELETE THE ROOT SEGMENT WHICH WILL ALSO GET RID OF THE
      ************************************************************
       1131-DLET-BALANCE.

      *    DISPLAY '1131-'.

           CALL 'CBLTDLI'  USING  DLET-CALL-FUNC
                                  BAL-PCB
                                  BALROOT-SEGMENT.

           IF  BAL-STATUS-CD EQUAL SPACES
               ADD 1                      TO WS-ROOTS-DLET-CTR
                                             CHKPT-CTR
           ELSE
               MOVE DLET-CALL-FUNC        TO ABEND-CALL-FUNC
               MOVE BAL-STATUS-CD         TO ABEND-STATUS
               MOVE SPACES                TO ABEND-SSA
               MOVE '1131-DLET-BALANCE'   TO ABEND-PARAGRAPH
               PERFORM 9997-BUILD-IMS-ERROR
               PERFORM 9998-TERMINATION.

      ************************************************************
      *    IMS DATABASE CALLS -   DDDABAL-PCB
      ************************************************************
       5000-GHU-BALANCE-BOOLEAN.

      *    DISPLAY '5000-'.

           MOVE 'DDA'                       TO BAL-APPL-ACR-ALT.

           CALL 'CBLTDLI'  USING  GHU-CALL-FUNC
                                  BAL-PCB
                                  BALROOT-SEGMENT
                                  BALANCE-BOOLEAN-SSA.

           IF  BAL-STATUS-CD EQUAL SPACES
               DISPLAY 'BALROOT  = ' BALROOT-SEGMENT
               MOVE BAL-ACCT-NO             TO BAL-ACCT-NO-KEY-1
               MOVE REQUESTED-KEY-BALROOT   TO BAL-KEY-ALT
               MOVE CONV-TO-BANK            TO BAL-BANK-NO-KEY-ALT
                                               BAL-BANK-NO
               ADD 1                        TO WS-ROOTS-READ-CTR
                                               CHKPT-CTR
           ELSE
           IF  BAL-STATUS-CD EQUAL 'GE' OR 'GB'
               ADD 1                        TO CHKPT-CTR
           ELSE
               MOVE GHU-CALL-FUNC           TO ABEND-CALL-FUNC
               MOVE BAL-STATUS-CD           TO ABEND-STATUS
               MOVE BALANCE-BOOLEAN-SSA     TO ABEND-SSA
               MOVE '5000-GHU-BALANCE-BOOLEAN'
                                            TO ABEND-PARAGRAPH
               PERFORM 9997-BUILD-IMS-ERROR
               PERFORM 9998-TERMINATION.


      ************************************************************
      *    IMS DATABASE CALLS -   DDDABAL-PCB (GET NEXT CHILD SEGME
      ************************************************************
       6000-GHNP-BALANCE-SEGMENT.

      *    DISPLAY '6000-'.

           CALL 'CBLTDLI'  USING  GHNP-CALL-FUNC
                                  BAL-PCB
                                  DB-RECORD.

           IF  BAL-STATUS-CD EQUAL SPACES OR 'GK' OR 'GA'
               ADD 1                      TO CHKPT-CTR
                                             WS-SEGS-READ-CTR
           ELSE
           IF  BAL-STATUS-CD EQUAL 'GE'
               ADD 1                      TO CHKPT-CTR
           ELSE
               MOVE GHNP-CALL-FUNC        TO ABEND-CALL-FUNC
               MOVE BAL-STATUS-CD         TO ABEND-STATUS
               MOVE SPACES                TO ABEND-SSA
               MOVE '6000-GHNP-BALANCE-SEGMENT'
                                          TO ABEND-PARAGRAPH
               PERFORM 9997-BUILD-IMS-ERROR
               PERFORM 9998-TERMINATION.


      ************************************************************
      *    PERFORM CHECKPOINT CALL TO LOCK IN CHANGES MADE THIS FA
      ************************************************************
       7000-CHECKPOINT-CALL.

      *    DISPLAY '7000-'.

           ADD CHKPT-CTR                  TO  CHKPT-KEEPER.
           ADD 1                          TO  WS-CHKPTS-TAKEN-CTR.

           CALL 'CBLTDLI'  USING  CHKP-CALL-FUNC
                                  IO-PCB
                                  CHKPT-KEEPER.

           IF  IO-STATUS NOT EQUAL SPACES
               DISPLAY 'INVALID CHKP CALL'
               DISPLAY IO-STATUS
               DISPLAY CHKPT-KEEPER
               GOBACK.


       8100-ISRT-ALT-BALMISC.

      *    DISPLAY '8100-'.

           CALL 'CBLTDLI'  USING  ISRT-CALL-FUNC
                                  BAL-PCB-ALT
                                  BALMISC-SEGMENT
                                  BALANCE-SSA-ALT
                                  BALMISC-UNQUAL-SSA.

           IF BAL-STATUS-CD-ALT EQUAL SPACES
              ADD 1                        TO WS-SEGS-ISRT-CTR
                                              CHKPT-CTR
           ELSE
               MOVE ISRT-CALL-FUNC         TO ABEND-CALL-FUNC
               MOVE BAL-STATUS-CD-ALT      TO ABEND-STATUS
               MOVE BALANCE-SSA-ALT        TO ABEND-SSA
               MOVE '8100-ISRT-ALT-BALMISC'
                                           TO ABEND-PARAGRAPH
               PERFORM 9997-BUILD-IMS-ERROR
               PERFORM 9998-TERMINATION.


       8200-ISRT-ALT-ATSBCKT.

      *    DISPLAY '8200-'.

           CALL 'CBLTDLI'  USING  ISRT-CALL-FUNC
                                  BAL-PCB-ALT
                                  ATSBCKT-SEGMENT
                                  BALANCE-SSA-ALT
                                  ATSBCKT-UNQUAL-SSA.

           IF BAL-STATUS-CD-ALT EQUAL SPACES
              ADD 1                        TO WS-SEGS-ISRT-CTR
                                              CHKPT-CTR
           ELSE
               MOVE ISRT-CALL-FUNC         TO ABEND-CALL-FUNC
               MOVE BAL-STATUS-CD-ALT      TO ABEND-STATUS
               MOVE BALANCE-SSA-ALT        TO ABEND-SSA
               MOVE '8200-ISRT-ALT-ATSBCKT'
                                           TO ABEND-PARAGRAPH
               PERFORM 9997-BUILD-IMS-ERROR
               PERFORM 9998-TERMINATION.


       8300-GET-ATSROOT.

      *    DISPLAY '8300-'.

           MOVE DB-RECORD                  TO VBALSRV-SEGMENT.

           MOVE VBALSRV-BANK               TO ATSROOT-BANK.
           MOVE VBALSRV-PAN                TO ATSROOT-CARDNO.

           MOVE 'DDA'                      TO ATSSRV-APPL-ACR.
           MOVE VBALSRV-BANK               TO ATSSRV-BANK-NO.
           MOVE ZERO                       TO ATSSRV-BK-BR
                                              ATSSRV-BK-HOLDING-CO.
           MOVE BAL-ACCT-NO                TO ATSSRV-ACCT-NO.

           CALL 'CBLTDLI'  USING  GHU-CALL-FUNC
                                  ATS-PCB
                                  ATSSRV-SEGMENT
                                  ATSROOT-QUAL-SSA
                                  ATSSRV-QUAL-SSA.

           IF ATS-STATUS EQUAL SPACES
              ADD 1                        TO CHKPT-CTR
              PERFORM 8310-DLET-ATSSRV
           ELSE
               MOVE GHU-CALL-FUNC          TO ABEND-CALL-FUNC
               MOVE ATS-STATUS             TO ABEND-STATUS
               MOVE ATSROOT-QUAL-SSA       TO ABEND-SSA
               MOVE '8300-GET-ATSROOT'     TO ABEND-PARAGRAPH
               PERFORM 9997-BUILD-IMS-ERROR
               PERFORM 9998-TERMINATION.


       8310-DLET-ATSSRV.

      *    DISPLAY '8310-'.

           CALL 'CBLTDLI'  USING  DLET-CALL-FUNC
                                  ATS-PCB
                                  ATSSRV-SEGMENT.

           IF ATS-STATUS EQUAL SPACES
              ADD 1                        TO CHKPT-CTR
                                              WS-ATSSRV-DLET-CTR
              PERFORM 8320-ISRT-ATSSRV
           ELSE
               MOVE DLET-CALL-FUNC         TO ABEND-CALL-FUNC
               MOVE ATS-STATUS             TO ABEND-STATUS
               MOVE SPACES                 TO ABEND-SSA
               MOVE '8310-DLET-ATSSRV'     TO ABEND-PARAGRAPH
               PERFORM 9997-BUILD-IMS-ERROR
               PERFORM 9998-TERMINATION.


       8320-ISRT-ATSSRV.

      *    DISPLAY '8320-'.

           MOVE CONV-TO-BANK               TO AT-SRV-BANK-NO.

           CALL 'CBLTDLI'  USING  ISRT-CALL-FUNC
                                  ATS-PCB
                                  ATSSRV-SEGMENT
                                  ATSROOT-QUAL-SSA
                                  ATSSRV-UNQUAL-SSA.

           IF ATS-STATUS EQUAL SPACES
              ADD 1                        TO CHKPT-CTR
                                              WS-SEGS-ISRT-CTR
           ELSE
              MOVE ISRT-CALL-FUNC          TO ABEND-CALL-FUNC
              MOVE ATS-STATUS              TO ABEND-STATUS
              MOVE ATSROOT-QUAL-SSA        TO ABEND-SSA
              MOVE '8320-ISRT-ATSSRV'      TO ABEND-PARAGRAPH
              PERFORM 9997-BUILD-IMS-ERROR
              PERFORM 9998-TERMINATION.


       8400-ISRT-ALT-ARUBCKT.

      *    DISPLAY '8400-'.

           CALL 'CBLTDLI'  USING  ISRT-CALL-FUNC
                                  BAL-PCB-ALT
                                  ARUBCKT-SEGMENT
                                  BALANCE-SSA-ALT
                                  ARUBCKT-UNQUAL-SSA.

           IF BAL-STATUS-CD-ALT EQUAL SPACES
              ADD 1                        TO WS-SEGS-ISRT-CTR
                                              CHKPT-CTR
           ELSE
               MOVE ISRT-CALL-FUNC         TO ABEND-CALL-FUNC
               MOVE BAL-STATUS-CD-ALT      TO ABEND-STATUS
               MOVE BALANCE-SSA-ALT        TO ABEND-SSA
               MOVE '8400-ISRT-ALT-ARUBCKT'
                                           TO ABEND-PARAGRAPH
               PERFORM 9997-BUILD-IMS-ERROR
               PERFORM 9998-TERMINATION.


      ************************************************************
      *    ACCEPT PARM DATA TO GET OLD BANK NUMBER AND NEW BANK NU
      ************************************************************

       9000-READ-CONV-BANK-INFO.

      *    DISPLAY '9000-'.

           READ CONVERSION-CONTROL-FILE
                AT END MOVE 'Y'           TO WS-INPUT-BANK-EOF-SW.

           IF  INPUT-BANK-NOT-AT-EOF
               IF  CONV-FROM-BANK NUMERIC AND CONV-TO-BANK NUMERIC
                   IF  CONV-FROM-BANK > 0 AND CONV-TO-BANK > 0
                       ADD 1 TO WS-BANK-RECS-READ-CTR
                   ELSE
                       DISPLAY 'VALUES IN CONTROL CARD INVALID'
                       DISPLAY 'CONV-FROM-BANK  =  ' CONV-FROM-BANK
                       DISPLAY 'CONV-TO-BANK    =  ' CONV-TO-BANK
               ELSE
                   DISPLAY 'VALUES IN CONTROL CARD INVALID'
                   DISPLAY 'CONV-FROM-BANK  =  ' CONV-FROM-BANK
                   DISPLAY 'CONV-TO-BANK    =  ' CONV-TO-BANK.


      ************************************************************
      *    COMMON IMS ERROR ROUTINE
      ************************************************************
       9997-BUILD-IMS-ERROR.

      *    DISPLAY '9997-'.

           DISPLAY 'FTB76995 - INVALID STATUS FOR ' ABEND-CALL-FUNC
                   ' ERROR CODE '                   ABEND-STATUS.

           DISPLAY 'ABENDED IN PARAGRAPH = '        ABEND-PARAGRAPH.

           DISPLAY 'SSA                  =   '      ABEND-SSA.

           PERFORM 9999-DISPLAY-COUNTERS.

           ADD 1                           TO BLOW-UP.

      ************************************************************
      *    TERMINATION ROUTINE
      ************************************************************
       9998-TERMINATION.

           DISPLAY '9998-'.

           CLOSE CONVERSION-CONTROL-FILE.

           PERFORM 9999-DISPLAY-COUNTERS.

           GOBACK.



      ************************************************************
      *    DISPLAY COUNTERS MAINTAINED THROUGHOUT EXECUTION
      ************************************************************
       9999-DISPLAY-COUNTERS.

           DISPLAY '*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *'.
           DISPLAY '*  *  *  *  *  *    COUNTERS  *  *  *  *  *  *'.
           DISPLAY '*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *'.
           DISPLAY '** NBR BANK RECORDS READ   = '
                                                 WS-BANK-RECS-READ-CTR.
           DISPLAY '** NBR ROOTS READ          = ' WS-ROOTS-READ-CTR.
           DISPLAY '** NBR PCS ROOTS READ      = '
                                                 WS-PCS-ROOTS-READ-CTR.
           DISPLAY '** NBR ROOTS INSERTED      = ' WS-ROOTS-ISRT-CTR.
           DISPLAY '** NBR ATSSRV DELETED      = ' WS-ATSSRV-DLET-CTR.
           DISPLAY '** NBR CHILD SEGS INSERTED = ' WS-SEGS-ISRT-CTR.
           DISPLAY '** NBR CHILD SEGS READ     = ' WS-SEGS-READ-CTR.
           DISPLAY '** NBR DUPL ROOTS FOUND    = '
                                                 WS-NBR-DUPL-ROOTS-CTR.
           DISPLAY '** NBR PCS ROOTS NOTFND    = ' WS-PCS-ROOT-NOTFND.
           DISPLAY '** NBR CHECK POINTS TAKEN  = ' WS-CHKPTS-TAKEN-CTR.


      ****************************************
      **    = SEGMENT FOUND
      ** GA = NEW SEGMENT BUT MOVED UP 1 LEVEL
      ** GB = END OF DATABASE
      ** GE = SEGMENT WASN'T FOUND
      ** GK = NEW SEGMENT AT THE SAME LEVEL
      ** GP = PARENTAGE NOT ESTABLISHED
      ****************************************

