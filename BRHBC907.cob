       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BRHBC907.
      * AUTHOR.        T BARSTOW.
      * INSTALLATION.  
      * DATE-WRITTEN.  06/15/11.
      *DATE-COMPILED.
      * ----------------------------------------------------------------*
      *REMARKS.
      *
      *    FUNCTION: This program will read a sequential file of zip
      *              code data and load it to a table for subsequent
      *              processing to hopefully demonstrate different
      *              cobol verbs and how to best use them.
      *
      *
      * p2100-table-demos
      *    p2110-load-table
      *    p2120-display-table-data
      *    p2130-serial-search
      *    p2140-binary-search
      *    p2150-perform-vary
      *
      *
      * p2200-convert-to-numeric
      *    p2210-convert-tax-returns
      *    p2220-convert-pop
      *    p2230-convert-tot-wages
      *
      *
      * p2300-evaluate-zipcode
      *    p2310-display-specific-zips
      *
      *
      * U1000-read-f0aa
      *
      *
      * u2000-unstring-csv
      *
      *
      * u3000-display-time
      *
      *
      * u4000-write-output
      *
      *
      * u5000-calc-future-date
      *
      *    MODIFICATIONS:
      *
      *       MM-DD-YY  APL????
      *----------------------------------------------------------------*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           C01 IS TOP-OF-PAGE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      ******************************************************************
      * THE FILE SELECT STATEMENTS ARE ENTERED HERE.
      *  EX. F0AA
      ******************************************************************
      *    * SELECT  f0aa ORGANIZATION is LINE SEQUENTIAL.
           SELECT f0aa ORGANIZATION is SEQUENTIAL assign to ws-input.
           SELECT  H0AA ORGANIZATION is LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * COPY F0AA.                                                       00070000



       fd  f0aa
            RECORD CONTAINS 80.
      *     value of FILE-ID is "/home/ted/Downloads/OpenCobolSource/ziptax.dat".
      *      value of FILE-ID is "ziptax.dat".

       01  f0aa-record                      pic x(80).

      * COPY F0AA REPLACING ==F0AA== BY ==H0AA==                         00070000
      *                     ==F0AA-RECORD== by ==H0AA-RECORD==.


       fd  H0AA
           record contains 80
           value of FILE-ID is "outtax.dat".

       01  h0aa-record                     pic x(80).




       WORKING-STORAGE SECTION.
       01  WS-BEGIN.
           05  WS-PROGRAM-NA                PIC X(8)  VALUE 'BRHBC907'.
           05  FILLER                       PIC X(16) VALUE
               ' W/S BEGINS HERE'.

           05  ws-input                    pic x(50)  VALUE
               '/home/ted/Downloads/OpenCobolSource/ziptax.txt'.

      * data sample
      *00061,PINE MEADOW,CT,41.87,-72.96,,,
      *00501,HOLTSVILLE,NY,40.81,-73.04,259,384,2147073
      *00544,HOLTSVILLE,NY,40.81,-73.04,,,
      *00601,ADJUNTAS,PR,18.16,-66.72,,,
      *00601,CHICAGO,IL,41.83,-87.68,5676,7785,594882095

       01  WS-ZIPCODE-REC.
           05 WS-ZIPCODE                    PIC  X(05).
           05 WS-CITY                       PIC  X(25).
           05 WS-STATE                      PIC  X(02).
           05 WS-LATITUDE                   PIC  X(06).
           05 WS-LONGITUDE                  PIC  X(06).
           05 WS-TAX-RETURNS-FILED          PIC  X(10).
           05 WS-TAX-FILED-N REDEFINES
              WS-TAX-RETURNS-FILED          PIC  9(10).
           05 WS-ESTIMATED-POPULATION       PIC  X(10).
           05 WS-EST-POP-N REDEFINES
              WS-ESTIMATED-POPULATION       PIC  9(10).
           05 WS-TOTAL-WAGES                PIC  X(15).
           05 WS-TOT-WAGES-N REDEFINES
              WS-TOTAL-WAGES                PIC  9(15).

       01  WS-ZIP-TABLE.
           05 WS-ZIP-TABLE-MAX              PIC S9(05)    COMP-3
                                                          VALUE 50000.
           05 WS-ZIP-TABLE-CNT              PIC S9(05)    COMP-3
                                                          VALUE 1.
      *                                                   VALUE 0.

           05 WS-ZIP-FND-SW                 PIC  X(01)    VALUE 'N'.
              88 WS-ZIP-FND                               VALUE 'Y'.
              88 WS-ZIP-NFND                              VALUE 'N'.
           05 WS-ZIP-TBL     OCCURS 1 TO 50000 TIMES
                             DEPENDING ON WS-ZIP-TABLE-CNT
                             ASCENDING KEY IS WS-ZIP-CODE
                             INDEXED BY ZX.

              10 WS-ZIP-CODE                PIC X(05).
              10 WS-ZIP-CITY                PIC X(25).
              10 WS-ZIP-ST                  PIC X(02).
              10 WS-ZIP-LAT                 PIC X(06).
              10 WS-ZIP-LONG                PIC X(06).
              10 WS-ZIP-TAX-RETURNS         PIC X(10).
              10 WS-ZIP-POP                 PIC X(10).
              10 WS-ZIP-TOT-WAGES           PIC X(15).



       01  WS-APPLICATION-WORK-AREAS.
           05  WS-EOF-SW                    PIC X(01)   VALUE 'N'.
               88 WS-EOF                                VALUE 'Y'.
           05  WS-SEARCH-ZIP                PIC X(05).
           05  WS-CONVERT-1                 PIC X(15).
           05  WS-CONVERT-2                 PIC X(15).
           05  WS-CONVERT-2-N REDEFINES WS-CONVERT-2
                                            pic 9(15).
           05  WS-CNT-1                     PIC 9(02)   VALUE 0.
           05  WS-CNT-2                     PIC 9(02)   VALUE 0.

           05 WS-WORK-TAX-FILED             PIC 9(09)   COMP-3.
           05 WS-WORK-POP                   PIC 9(13)   COMP-3.
           05 WS-WORK-TOT-WAGES             PIC 9(15)   COMP-3.
           05 WS-AVG-SALARY                 PIC 9(15)   COMP-3.

           05  WS-DISPLAY-NBR               PIC ZZZ,ZZZ,ZZ9.

           05  WS-CURRENT-DATE-TIME.
               10  WS-CURRENT-DA.
                   15  WS-CURRENT-CC-DA     PIC 99.
                   15  WS-CURRENT-YY-DA     PIC 99.
                   15  WS-CURRENT-MM-DA     PIC 99.
                   15  WS-CURRENT-DD-DA     PIC 99.
               10  WS-CURRENT-DATE-NUM  REDEFINES  WS-CURRENT-DA
                                            PIC 9(08).
               10  WS-CURRENT-TIME.
                   15  WS-CURRENT-TIME-HHMMSS.
                       20  WS-CURRENT-HH-TIME    PIC 99.
                       20  WS-CURRENT-MM-TIME    PIC 99.
                       20  WS-CURRENT-SS-TIME    PIC 99.
                   15  WS-CURRENT-TIME-HHMMSS-N REDEFINES
                       WS-CURRENT-TIME-HHMMSS    PIC 9(06).
                   15  WS-CURRENT-MS-TIME        PIC 99.


           05  WS-TODAY                          PIC 9(08).
           05  WS-FUTURE-DATE                    PIC 9(08).


           05  WS-ACCUMULATORS.
               10  WS-READ-CNT              PIC 9(09) VALUE 0.
               10  WS-WRITE-CNT             PIC 9(09) VALUE 0.


       LINKAGE SECTION.

       PROCEDURE DIVISION.
      ******************************************************************
      *
      *
      *
      *
      ******************************************************************
       P0000-MAINLINE.

           PERFORM P1000-INITIALIZE.

           PERFORM P2000-PROCESS
                   UNTIL WS-EOF
                   OR    WS-ZIP-TABLE-CNT = WS-ZIP-TABLE-MAX.


           PERFORM P3000-TERMINATE.

           GOBACK.

      ******************************************************************
      *
      *
      *
      ******************************************************************
       P1000-INITIALIZE.

           DISPLAY '**** BEGINNING BRHBC907 ****'.

           DISPLAY 'INITITIALIZING'.
           PERFORM U3000-DISPLAY-TIME.

           PERFORM U5000-CALC-FUTURE-DATE.


      *----  open files                                         ----*   00402000
           OPEN INPUT F0AA
                OUTPUT H0AA.

      *----  Read until you reach the first detail record       ----*   00402000
           PERFORM U1000-READ-F0AA
              UNTIL WS-EOF
              OR    F0AA-RECORD(1:1) IS NUMERIC.


           display f0aa-record.

           IF WS-EOF                                                    00423000
               DISPLAY 'NO VALID DATA RECORDS AVAILABLE TO PROCESS'     00424000
           ELSE                                                         00428000
               PERFORM U2000-UNSTRING-CSV                               00429000
           END-IF.                                                      00430000

      ******************************************************************
      *p1000-exit.  exit.
      *
      * using exits I believe is a current EDJ standard.  they are not
      * required and promote lazy, unstructured coding.
      *
      * many people feel that using an exit as the subject of a goto is
      * fine, so you will find this kind of coding here -
      * 'go to p1000-exit'
      *
      ******************************************************************
      * MAINLINE LOGIC.
      ******************************************************************
       P2000-PROCESS.

           PERFORM P2100-TABLE-DEMOS.

           PERFORM P2200-CONVERT-TO-NUMERIC.

           PERFORM P2300-EVALUATE-ZIPCODE.


       P2100-TABLE-DEMOS.

           PERFORM P2110-LOAD-TABLE.

           PERFORM P2120-DISPLAY-TABLE-DATA.

           PERFORM P2130-SERIAL-SEARCH.

           PERFORM P2140-BINARY-SEARCH.

           PERFORM P2150-PERFORM-VARY.


       P2110-LOAD-TABLE.

           DISPLAY 'LOADING TABLE'.
           PERFORM U3000-DISPLAY-TIME.
           display WS-ZIPCODE-REC.

           PERFORM P2110-10-LOAD-TABLE-DATA
              VARYING ZX FROM 1 BY 1
                UNTIL WS-EOF
                OR    ZX = WS-ZIP-TABLE-MAX.
      *          OR    WS-ZIP-TABLE-CNT = WS-ZIP-TABLE-MAX.

           DISPLAY 'TABLE LOADED'.

           display 'zip table count = ' WS-ZIP-TABLE-CNT.

           PERFORM U3000-DISPLAY-TIME.


       P2110-10-LOAD-TABLE-DATA.

           MOVE WS-ZIPCODE                  TO WS-ZIP-CODE        (ZX).
           MOVE WS-CITY                     TO WS-ZIP-CITY        (ZX).
           MOVE WS-STATE                    TO WS-ZIP-ST          (ZX).
           MOVE WS-LATITUDE                 TO WS-ZIP-LAT         (ZX).
           MOVE WS-LONGITUDE                TO WS-ZIP-LONG        (ZX).
           MOVE WS-TAX-RETURNS-FILED        TO WS-ZIP-TAX-RETURNS (ZX).
           MOVE WS-ESTIMATED-POPULATION     TO WS-ZIP-POP         (ZX).
           MOVE WS-TOTAL-WAGES              TO WS-ZIP-TOT-WAGES   (ZX).

           ADD 1                            TO WS-ZIP-TABLE-CNT.

           PERFORM U1000-READ-F0AA.

           display 'foaa raw data - ' f0aa-record.

           PERFORM U2000-UNSTRING-CSV.


       P2120-DISPLAY-TABLE-DATA.

           DISPLAY 'DISPLAY 1000 OCCURANCES'.
           PERFORM U3000-DISPLAY-TIME.

           PERFORM VARYING ZX FROM 1 BY 1
                    UNTIL ZX = 1000
                      DISPLAY WS-ZIP-CODE        (ZX)
                              ' '
                              WS-ZIP-CITY        (ZX)
                              ' '
                              WS-ZIP-ST          (ZX)
                              ' '
                              WS-ZIP-LAT         (ZX)
                              ' '
                              WS-ZIP-LONG        (ZX)

           END-PERFORM.

           DISPLAY 'DISPLAY 1000 COMPLETED'.
           PERFORM U3000-DISPLAY-TIME.


       P2130-SERIAL-SEARCH.



      *    MOVE 99999                       TO WS-SEARCH-ZIP.
           MOVE 68883                       TO WS-SEARCH-ZIP.

           DISPLAY 'SERIAL SEARCH BEGINNING'.
           PERFORM U3000-DISPLAY-TIME.

      * must set the index prior to beginning serial search
           SET ZX                           TO 1.
           SEARCH WS-ZIP-TBL
             AT END
                DISPLAY 'ZIPCODE NOT FOUND - ' WS-SEARCH-ZIP

             WHEN WS-ZIP-CODE (ZX) = WS-SEARCH-ZIP
                DISPLAY 'ZIPCODE FOUND - ' WS-SEARCH-ZIP

           END-SEARCH.

           DISPLAY 'SERIAL SEARCH COMPLETED'.
           PERFORM U3000-DISPLAY-TIME.


       P2140-BINARY-SEARCH.

      * binary search is limited by the value in the depending on
      * phrase - WS-ZIP-TABLE-CNT

           MOVE 68883                       TO WS-SEARCH-ZIP.

           DISPLAY 'BINARY SEARCH BEGINNING'.
           PERFORM U3000-DISPLAY-TIME.

           SEARCH ALL WS-ZIP-TBL
             AT END
                DISPLAY 'ZIPCODE NOT FOUND - ' WS-SEARCH-ZIP
             WHEN WS-ZIP-CODE (ZX) = WS-SEARCH-ZIP
                DISPLAY 'ZIPCODE FOUND - ' WS-SEARCH-ZIP

           END-SEARCH.

           DISPLAY 'BINARY SEARCH COMPLETED'.
           PERFORM U3000-DISPLAY-TIME.


       P2150-PERFORM-VARY.

           MOVE 68883                       TO WS-SEARCH-ZIP.
           SET WS-ZIP-NFND                  TO TRUE.

           DISPLAY 'PERFORM VARY BEGINNING'.
           PERFORM U3000-DISPLAY-TIME.

           PERFORM VARYING ZX FROM 1 BY 1
             UNTIL ZX > WS-ZIP-TABLE-CNT
             OR    WS-ZIP-FND

               IF WS-ZIP-CODE (ZX) = WS-SEARCH-ZIP
                  DISPLAY 'ZIPCODE FOUND - ' WS-SEARCH-ZIP
                  SET WS-ZIP-FND            TO TRUE
               *> ELSE
                   *> DISPLAY 'zipcode was not found - ' WS-SEARCH-ZIP
               END-IF

           END-PERFORM

           DISPLAY 'PERFORM VARY COMPLETED'.
           PERFORM U3000-DISPLAY-TIME.

           IF ZX >= WS-ZIP-TABLE-CNT
               or ws-zip-fnd
              NEXT SENTENCE
           ELSE
              DISPLAY 'ZIPCODE WAS NOT FOUND - ' WS-SEARCH-ZIP
           END-IF.


      ******************************************************************
      * convert the alphanumeric left justified fields to numeric using
      * the inspect tallying verbs to figure out how many spaces are in
      * the field
      *
      ******************************************************************
       P2200-CONVERT-TO-NUMERIC.

           PERFORM VARYING ZX FROM 1 BY 1
              UNTIL ZX > WS-ZIP-TABLE-CNT
              OR    ZX > 300

                PERFORM P2210-CONVERT-TAX-RETURNS
                PERFORM P2220-CONVERT-POP
                PERFORM P2230-CONVERT-TOT-WAGES

                IF WS-WORK-TOT-WAGES > 0
                AND
                   WS-WORK-TAX-FILED > 0

                   COMPUTE WS-AVG-SALARY =
                        WS-WORK-TOT-WAGES / WS-WORK-TAX-FILED

                   MOVE WS-AVG-SALARY       TO WS-DISPLAY-NBR

                   DISPLAY 'AVERAGE SALARY PER FILER - '
                            WS-DISPLAY-NBR

                END-IF

                PERFORM U4000-WRITE-OUTPUT

           END-PERFORM.


       P2210-CONVERT-TAX-RETURNS.

           MOVE SPACES                      TO WS-CONVERT-1.
           MOVE ZEROES                      TO WS-CONVERT-2
                                               WS-CNT-1.

           MOVE WS-ZIP-TAX-RETURNS (ZX)     TO WS-CONVERT-1.

           INSPECT WS-CONVERT-1 TALLYING WS-CNT-1 FOR ALL SPACES.

           IF WS-CNT-1 < 15

              SUBTRACT WS-CNT-1 FROM 15 GIVING WS-CNT-2
              MOVE WS-CONVERT-1 (1: WS-CNT-2)
                                            TO WS-CONVERT-2 (WS-CNT-1:)
              MOVE WS-CONVERT-2 (6:)        TO WS-ZIP-TAX-RETURNS (ZX)

           ELSE

              MOVE WS-CONVERT-2 (6:)        TO WS-ZIP-TAX-RETURNS (ZX)

           END-IF

           DISPLAY 'ZIP-TAX-RETURNS - ' WS-ZIP-TAX-RETURNS (ZX).

           MOVE WS-CONVERT-2-N              TO WS-WORK-TAX-FILED.


       P2220-CONVERT-POP.

           MOVE SPACES                      TO WS-CONVERT-1.
           MOVE ZEROES                      TO WS-CONVERT-2
                                               WS-CNT-1.

           MOVE WS-ZIP-POP (ZX)             TO WS-CONVERT-1.

           INSPECT WS-CONVERT-1 TALLYING WS-CNT-1 FOR ALL SPACES.

           IF WS-CNT-1 < 15

              SUBTRACT WS-CNT-1 FROM 15 GIVING WS-CNT-2
              MOVE WS-CONVERT-1 (1: WS-CNT-2)
                                            TO WS-CONVERT-2 (WS-CNT-1:)
              MOVE WS-CONVERT-2             TO WS-ZIP-POP         (ZX)

           ELSE

              MOVE WS-CONVERT-2             TO WS-ZIP-POP         (ZX)

           END-IF

           DISPLAY 'ZIP-POP - ' WS-ZIP-POP (ZX).

           MOVE WS-CONVERT-2-N              TO WS-WORK-POP.


       P2230-CONVERT-TOT-WAGES.

           MOVE SPACES                      TO WS-CONVERT-1.
           MOVE ZEROES                      TO WS-CONVERT-2
                                               ws-cnt-1.

           MOVE WS-ZIP-TOT-WAGES (ZX)       TO WS-CONVERT-1.

           INSPECT WS-CONVERT-1 TALLYING WS-CNT-1 FOR ALL SPACES.



           IF WS-CNT-1 < 15

              SUBTRACT WS-CNT-1 FROM 15 GIVING WS-CNT-2
              MOVE WS-CONVERT-1 (1: WS-CNT-2)
                                            TO WS-CONVERT-2 (WS-CNT-1:)
              MOVE WS-CONVERT-2             TO WS-ZIP-TOT-WAGES   (ZX)

           ELSE

              MOVE WS-CONVERT-2             TO WS-ZIP-TOT-WAGES   (ZX)

           END-IF

           DISPLAY 'ZIP-TOT-WAGES - ' WS-ZIP-TOT-WAGES (ZX).

           MOVE WS-CONVERT-2-N              TO WS-WORK-TOT-WAGES.


       P2300-EVALUATE-ZIPCODE.

           MOVE 0                           TO WS-WORK-TAX-FILED
                                               WS-WORK-POP
                                               WS-WORK-TOT-WAGES.


           PERFORM VARYING ZX FROM 1 BY 1
              UNTIL ZX > WS-ZIP-TABLE-CNT

                PERFORM P2310-DISPLAY-SPECIFIC-ZIPS


           END-PERFORM.


       P2310-DISPLAY-SPECIFIC-ZIPS.

           EVALUATE WS-ZIP-CODE (ZX)(1:3)

              WHEN '633'


                 DISPLAY WS-ZIP-CODE        (ZX)
                         ' '
                         WS-ZIP-CITY        (ZX)
                         ' '
                         WS-ZIP-ST          (ZX)
                         ' '
                         WS-ZIP-LAT         (zx)
                         ' '
                         WS-ZIP-LONG        (zx)


                 PERFORM U4000-WRITE-OUTPUT

           END-EVALUATE.


      ******************************************************************
      *  -CODE PROGRAM TERMINATION LOGIC HERE
      *  -CLOSE FLAT FILES ONLY AFTER THE DAL FINISH CALLS
      *   HAVE BEEN EXECUTED.
      ******************************************************************
       P3000-TERMINATE.


           PERFORM P3200-DISPLAY-TOTALS.

           DISPLAY '**** FINISHING BRHBC907 ****'.

            PERFORM U3000-DISPLAY-TIME.

           CLOSE F0AA
                 H0AA.

      ******************************************************************
      *  DISPLAY ALL COUNTERS
      ******************************************************************
       P3200-DISPLAY-TOTALS.

            DISPLAY ' '.
            MOVE WS-READ-CNT                TO WS-DISPLAY-NBR.
            DISPLAY 'RECORDS READ ................'   WS-DISPLAY-NBR.

            DISPLAY ' '.
            MOVE WS-WRITE-CNT               TO WS-DISPLAY-NBR.
            DISPLAY 'RECORDS WRITTEN..............'   WS-DISPLAY-NBR.


            DISPLAY ' '.
            DISPLAY ' '.

      ***************************************************************** 00853000
      *                                                               * 00854000
      *    READ THE NEXT  RECORD.                                     * 00855000
      *                                                               * 00856000
      ***************************************************************** 00857000
       U1000-READ-F0AA.                                                 00858000
                                                                        00859000
           READ  F0AA                                                   00860000
             AT  END                                                    00862000
                 MOVE  'Y'                  TO  WS-EOF-SW.              TB092310
                                                                        00864000
                                                                        00865000
           ADD  1                           TO WS-READ-CNT.             TB092310
                                                                        00867000

       U2000-UNSTRING-CSV.

           UNSTRING F0AA-RECORD
                    DELIMITED BY ','
                    INTO WS-ZIPCODE
                         WS-CITY
                         WS-STATE
                         WS-LATITUDE
                         WS-LONGITUDE
                         WS-TAX-RETURNS-FILED
                         WS-ESTIMATED-POPULATION
                         WS-TOTAL-WAGES
           END-UNSTRING.


       U3000-DISPLAY-TIME.

           MOVE FUNCTION CURRENT-DATE       TO WS-CURRENT-DATE-TIME.

           DISPLAY 'CURRENT TIME: '   WS-CURRENT-HH-TIME ':'
                                      WS-CURRENT-MM-TIME ':'
                                      WS-CURRENT-SS-TIME ':'
                                      WS-CURRENT-MS-TIME.
           DISPLAY ' '.


       U4000-WRITE-OUTPUT.

           IF WS-WORK-TAX-FILED = 0
              AND
              WS-WORK-POP = 0
              AND
              WS-WORK-TOT-WAGES = 0
              MOVE WS-ZIP-CODE         (ZX) TO WS-ZIPCODE
              MOVE WS-ZIP-CITY         (ZX) TO WS-CITY
              MOVE WS-ZIP-ST           (ZX) TO WS-STATE
              MOVE WS-ZIP-LAT          (ZX) TO WS-LATITUDE
              MOVE WS-ZIP-LONG         (ZX) TO WS-LONGITUDE
              MOVE SPACES                   TO WS-TAX-RETURNS-FILED
                                               WS-ESTIMATED-POPULATION
                                               WS-TOTAL-WAGES
           ELSE
              MOVE WS-ZIP-CODE         (ZX) TO WS-ZIPCODE
              MOVE WS-ZIP-CITY         (ZX) TO WS-CITY
              MOVE WS-ZIP-ST           (ZX) TO WS-STATE
              MOVE WS-ZIP-LAT          (ZX) TO WS-LATITUDE
              MOVE WS-ZIP-LONG         (ZX) TO WS-LONGITUDE
              MOVE WS-WORK-TAX-FILED        TO WS-TAX-FILED-N
              MOVE WS-WORK-POP              TO WS-EST-POP-N
              MOVE WS-WORK-TOT-WAGES        TO WS-TOT-WAGES-N
           END-IF.


           WRITE H0AA-RECORD FROM WS-ZIPCODE-REC.

           ADD 1                            TO WS-WRITE-CNT.


       U5000-CALC-FUTURE-DATE.

           MOVE FUNCTION CURRENT-DATE (1:8) TO WS-TODAY.

           COMPUTE WS-FUTURE-DATE = FUNCTION DATE-OF-INTEGER
             (FUNCTION INTEGER-OF-DATE (WS-TODAY) + 150).



           DISPLAY 'FUTURE DATE - '   WS-FUTURE-DATE (1:4) '/'
                                      WS-FUTURE-DATE (5:2) '/'
                                      WS-FUTURE-DATE (7:2).
           DISPLAY ' '.


       U9999-ABEND.

           DISPLAY '*************************************************'
           DISPLAY '  ABEND IN PROGRAM : ' WS-PROGRAM-NA.
           DISPLAY '*************************************************'
           .

      *    CALL 'UTLBA019' USING WS-UTLBA019-PARAMETERS.

      ******************************************************************
      * THIS PARAGRAPH IS USED TO DISPLAY INFORMATION NEEDED TO
      * DEBUG ABENDS IN CASE OF A DAL IDMS ABORT.
      ******************************************************************
       DBMS-ABORT.
           DISPLAY 'DBMS ABORT'.


      ******************************************************************
      * COMMON ERROR STATUS ROUTINE
      ******************************************************************
     **********include DBACP006

       NEVER-EXIT-HERE.
           GOBACK.

