      ****** ABEXTPD  
      ******************************************************************
      * Library:         ABEXTPD (AB + EXT + PD)
      *
      * Description:     Procedure Description (private implementation)
      *                  for the FIND Methods of these objects -  
      *                  ABHISTHDR, ABHISTLINE, ABCURRHDR, ABCURRLINR 
      *
      * System:          MO
      *
      * Database file:   ABHISTHDR  (XHH)
      *                  ABHISTLINE (XHL)
      *                  ABCURRHDR  (XCH)
      *                  ABCURRLINE (XCL)
      *
      *
      * Private Methods: 2001-ABEXT-START-FIND
      *                  2002-ABEXT-FIND-NEXT 
      *      ****  THE FOLLOWING ARE NOT SUPPORTED YET  ****
      *                  2000-ABEXT-FIND
      *                  2003-ABEXT-FIND-PREV
      *
      * Working Storage: ABEXTWS
      *
      * Usage:
      * Populate ABEXTWS-PARM-FILTERS with the selection criteria for
      * your request.  Company must be provided, the rest of the fields
      * are optional.  To initiate the data selection, perform paragraph
      * 1000-ABEXT-CONTROL.  This will determine which key to use and
      * build the key, build any filters, return the first record of the
      * record set.  If ABEXTWS-ABHISTHDR-NOTFOUND, then no records were
      * found that matched the selection criteria, and ABCURRHDR will be
      * processed the same as ABHISTHDR was.  If ABEXTWS-ABCURRHDR-NOTFOUND,
      * EOF will be returned to calling program.
      *
      *
      *
      ******************************************************************
       1000-ABEXT-CONTROL              SECTION.
      ******************************************************************
       1000-START.

           IF (ABEXTWS-EXT-START-FIND)
               PERFORM 1100-DETERMINE-DRIVER-FILE
      *         PERFORM 1150-CHECK-CURRENT-ONLY-SWITCH
               PERFORM 1800-EDIT-FR-TO-PARM-DATES
           END-IF.

      * IS PROCESSING GOING TO BE DRIVEN BY THE HEADER FILES??
           IF (ABEXTWS-HEADER-DRIVING)
               PERFORM 1200-HEADER-DRIVEN
           ELSE
           IF (ABEXTWS-LINE-DRIVING)
               PERFORM 1300-LINE-DRIVEN
           END-IF
           END-IF.

      *  MOVED TO BUILD LINE PARAGRAPH, COULD MOVE BACK IF DECIDE TO 
      *  CONTROL WITH A SWITCH
      *     IF (ABEXTWS-ABHISTHDR-EOF) 
      *         IF (ABEXTWS-ABCURRHDR-EOF)
      *             NEXT SENTENCE
      *         ELSE
      *             PERFORM 30000-BUILD-ABEXTRACT-LAYOUT
      *         END-IF
      *     ELSE
      *         PERFORM 10000-BUILD-ABEXTRACT-LAYOUT
      *     END-IF.


      ******************************************************************
       1000-END.
      *****************************************************************

      ******************************************************************
      * CHECK TO SEE IF ANY OF THE FILTERS ASSOCIATED WITH THE LINE 
      * TABLE ARE POPULATED.  IF THEY ARE, SET THE DRIVER SWITCH TO 
      * LINE.  THIS WILL CAUSE THE ABCURRLINE AND ABHISTLINE TO BE USED 
      * TO SELECT DATA.  THE ASSOCIATED HEADER RECORD WILL BE THEN  
      * RETRIEVED FOR EACH GROUP OF LINE RECORDS.
      ******************************************************************
       1100-DETERMINE-DRIVER-FILE       SECTION.
      ******************************************************************

           SET ABEXTWS-HEADER-DRIVING      TO TRUE.

           IF (ABEXTWS-WORK-ITEM = SPACES)
               NEXT SENTENCE
           ELSE
               SET ABEXTWS-LINE-DRIVING    TO TRUE
               GO TO 1100-END
           END-IF.

           IF (ABEXTWS-WORK-ITEM-TYPE = SPACES)
               NEXT SENTENCE
           ELSE
               SET ABEXTWS-LINE-DRIVING    TO TRUE
               GO TO 1100-END
           END-IF.

           IF (ABEXTWS-WORK-LOCATION = SPACES)
               NEXT SENTENCE
           ELSE
               SET ABEXTWS-LINE-DRIVING    TO TRUE
               GO TO 1100-END
           END-IF.

           IF (ABEXTWS-WORK-SHIP-DATE-A = SPACES)
               NEXT SENTENCE
           ELSE
               SET ABEXTWS-LINE-DRIVING    TO TRUE
               GO TO 1100-END
           END-IF.

           IF (ABEXTWS-WORK-PRINT-DATE-A = SPACES)
               NEXT SENTENCE
           ELSE
               SET ABEXTWS-LINE-DRIVING    TO TRUE
               GO TO 1100-END
           END-IF.

           IF (ABEXTWS-WORK-ALPHA-CODE = SPACES)
               NEXT SENTENCE
           ELSE
               SET ABEXTWS-LINE-DRIVING    TO TRUE
               GO TO 1100-END
           END-IF.

           IF (ABEXTWS-WORK-SALES-MAJCL = SPACES)
               NEXT SENTENCE
           ELSE
               SET ABEXTWS-LINE-DRIVING    TO TRUE
               GO TO 1100-END
           END-IF.

           IF (ABEXTWS-WORK-SALES-MAJCL = SPACES)
               NEXT SENTENCE
           ELSE
               SET ABEXTWS-LINE-DRIVING    TO TRUE
               GO TO 1100-END
           END-IF.


      ******************************************************************
       1100-END.
      ******************************************************************


      ******************************************************************
       1200-HEADER-DRIVEN       SECTION.
      ******************************************************************

      * START CONTROL       ---- HEADER ----
      * EDIT PARM DATES, SET SWITCHES, START ABHISTHDR 

           IF (ABEXTWS-EXT-START-FIND)
               MOVE "N"                    TO ABEXTWS-ABHISTHDR-EOF-SW
                                              ABEXTWS-ABCURRHDR-EOF-SW
               PERFORM 2100-ABEXT-START-FIND

               IF (ABEXTWS-PROCESS-CURR-ONLY)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND TO TRUE
               END-IF

               IF (ABEXTWS-ABHISTHDR-NOTFOUND)
      * IF ABHISTHDR EOF ON START, TRY TO FIND ON ABCURRHDR

                   SET ABEXTWS-ABHISTHDR-EOF      TO TRUE
                   PERFORM 22100-ABEXT-START-FIND
                   IF (ABEXTWS-ABCURRHDR-NOTFOUND)
                       SET ABEXTWS-ABCURRHDR-EOF  TO TRUE
                       SET ABEXTWS-PROCESS-EOF    TO TRUE
                       PERFORM 1900-RESET-DYNAMIC-STORAGE
                   END-IF
               END-IF
           ELSE
      * FIND NEXT CONTROL STARTS HERE  

           IF (ABEXTWS-EXT-FIND-NEXT)
               IF (ABEXTWS-ABHISTHDR-EOF)
                   IF (ABEXTWS-ABCURRHDR-NOTFOUND)
      * NO MORE ABHISTHDR RECORDS, TRY TO FIND ON ABCURRHDR

                       PERFORM 22100-ABEXT-START-FIND
                       IF (ABEXTWS-ABCURRHDR-NOTFOUND)
                           SET ABEXTWS-ABCURRHDR-EOF TO TRUE
                           SET ABEXTWS-PROCESS-EOF   TO TRUE
                           PERFORM 1900-RESET-DYNAMIC-STORAGE
                       END-IF
                   ELSE
      * FIND NEXT ABCURRHDR

                       PERFORM 22200-ABEXT-FIND-NEXT
                       IF (ABEXTWS-ABCURRHDR-NOTFOUND)
                           SET ABEXTWS-ABCURRHDR-EOF  TO TRUE
                           SET ABEXTWS-PROCESS-EOF    TO TRUE
                           PERFORM 1900-RESET-DYNAMIC-STORAGE
                       END-IF
                   END-IF
               ELSE
      * FIND NEXT ABHISTHDR

                   PERFORM 2200-ABEXT-FIND-NEXT
                   IF (ABEXTWS-ABHISTHDR-NOTFOUND)
      * IF NO MORE ABHISTHDR RECORDS, TRY TO FIND ON ABCURRHDR

                       SET ABEXTWS-ABHISTHDR-EOF  TO TRUE
                       PERFORM 22100-ABEXT-START-FIND
                       IF (ABEXTWS-ABCURRHDR-NOTFOUND)
                           SET ABEXTWS-ABCURRHDR-EOF TO TRUE
                           SET ABEXTWS-PROCESS-EOF   TO TRUE
                           PERFORM 1900-RESET-DYNAMIC-STORAGE
                       END-IF
                   END-IF
               END-IF
           ELSE
      * INVALID SWITCH

               SET ABEXTWS-ABHISTHDR-EOF   TO TRUE
               SET ABEXTWS-ABCURRHDR-EOF   TO TRUE
               SET ABEXTWS-PROCESS-EOF     TO TRUE
               PERFORM 1900-RESET-DYNAMIC-STORAGE
           END-IF
           END-IF.


      ******************************************************************
       1200-END.
      ******************************************************************

      ******************************************************************
       1300-LINE-DRIVEN       SECTION.
      ******************************************************************


      *                      ---- LINE ----
      * EDIT PARM DATES, SET SWITCHES, START ABHISTLINE 

           IF (ABEXTWS-EXT-START-FIND)
               MOVE "N"                    TO ABEXTWS-ABHISTLINE-EOF-SW
                                              ABEXTWS-ABCURRLINE-EOF-SW
               PERFORM 32100-ABEXT-START-FIND

               IF (ABEXTWS-PROCESS-CURR-ONLY)
                   SET ABEXTWS-ABHISTLINE-NOTFOUND    TO TRUE
               END-IF

               IF (ABEXTWS-ABHISTLINE-NOTFOUND)
      * IF ABHISTLINE EOF ON START, TRY TO FIND ON ABCURRLINE

                   SET ABEXTWS-ABHISTLINE-EOF         TO TRUE
                   PERFORM 322100-ABEXT-START-FIND
                   IF (ABEXTWS-ABCURRLINE-NOTFOUND)
                       SET ABEXTWS-ABCURRLINE-EOF     TO TRUE
                       SET ABEXTWS-PROCESS-EOF        TO TRUE
                       PERFORM 1900-RESET-DYNAMIC-STORAGE
                   END-IF
               END-IF
           ELSE
      * FIND NEXT CONTROL STARTS HERE  

           IF (ABEXTWS-EXT-FIND-NEXT)
               IF (ABEXTWS-ABHISTLINE-EOF)
                   IF (ABEXTWS-ABCURRLINE-NOTFOUND)
      * NO MORE ABHISTLINE RECORDS, TRY TO FIND ON ABCURRLINE

                       PERFORM 322100-ABEXT-START-FIND
                       IF (ABEXTWS-ABCURRLINE-NOTFOUND)
                           SET ABEXTWS-ABCURRLINE-EOF TO TRUE
                           SET ABEXTWS-PROCESS-EOF    TO TRUE
                           PERFORM 1900-RESET-DYNAMIC-STORAGE
                       END-IF
                   ELSE
      * FIND NEXT ABCURRLINE

                       PERFORM 322200-ABEXT-FIND-NEXT
                       IF (ABEXTWS-ABCURRLINE-NOTFOUND)
                           SET ABEXTWS-ABCURRLINE-EOF TO TRUE
                           SET ABEXTWS-PROCESS-EOF    TO TRUE
                           PERFORM 1900-RESET-DYNAMIC-STORAGE
                       END-IF
                   END-IF
               ELSE
      * FIND NEXT ABHISTLINE

                   PERFORM 32200-ABEXT-FIND-NEXT
                   IF (ABEXTWS-ABHISTLINE-NOTFOUND)
      * IF NO MORE ABHISTLINE RECORDS, TRY TO FIND ON ABCURRLINE

                       SET ABEXTWS-ABHISTLINE-EOF     TO TRUE
                       PERFORM 322100-ABEXT-START-FIND
                       IF (ABEXTWS-ABCURRLINE-NOTFOUND)
                           SET ABEXTWS-ABCURRLINE-EOF TO TRUE
                           SET ABEXTWS-PROCESS-EOF    TO TRUE
                           PERFORM 1900-RESET-DYNAMIC-STORAGE
                       END-IF
                   END-IF
               END-IF
           ELSE
      * INVALID SWITCH

               SET ABEXTWS-ABHISTLINE-EOF             TO TRUE
               SET ABEXTWS-ABCURRLINE-EOF             TO TRUE
               SET ABEXTWS-PROCESS-EOF                TO TRUE
               PERFORM 1900-RESET-DYNAMIC-STORAGE
           END-IF
           END-IF.



      ******************************************************************
       1300-END.
      ******************************************************************

      ******************************************************************
       1800-EDIT-FR-TO-PARM-DATES       SECTION.
      ******************************************************************

           SET ABEXTWS-DONT-USE-FR-TO-PARMS           TO TRUE.

           IF (ABEXTWS-WORK-REQUEST-DATE-A NUMERIC)
               NEXT SENTENCE
           ELSE
               MOVE ZERO                   TO ABEXTWS-WORK-REQUEST-DATE
           END-IF.

      *     IF (ABEXTWS-WORK-REQUEST-DATE = 0)
      *                 AND
           IF (ABEXTWS-PARM-FR-DATE IS NUMERIC)
                       AND
              (ABEXTWS-PARM-TO-DATE IS NUMERIC)
               PERFORM 1810-FR-TO-VALIDITY-EDITS
           END-IF.


      ******************************************************************
       1800-END.
      ******************************************************************

      ******************************************************************
       1810-FR-TO-VALIDITY-EDITS       SECTION.
      ******************************************************************

           IF (ABEXTWS-PARM-FR-DATE  < ABEXTWS-PARM-TO-DATE)
               PERFORM 1820-FR-TO-MONTH-EDITS
               PERFORM 1830-FR-TO-DAY-EDITS
               IF (ABEXTWS-PARM-FR-DATE < ABEXTWS-PARM-TO-DATE)
                   SET ABEXTWS-USE-FR-TO-PARMS        TO TRUE
               END-IF
           END-IF.


      ******************************************************************
       1810-END.
      ******************************************************************

      ******************************************************************
       1820-FR-TO-MONTH-EDITS          SECTION.
      ******************************************************************

           IF (ABEXTWS-PARM-FR-MM-N > 0)
                   AND
              (ABEXTWS-PARM-FR-MM-N < 13)
               NEXT SENTENCE
           ELSE
               MOVE 0                      TO ABEXTWS-PARM-FR-DATE-N
                                              ABEXTWS-PARM-TO-DATE-N
           END-IF.

           IF (ABEXTWS-PARM-TO-MM-N > 0)
                   AND
              (ABEXTWS-PARM-TO-MM-N < 13)
               NEXT SENTENCE
           ELSE
               MOVE 0                      TO ABEXTWS-PARM-FR-DATE-N
                                              ABEXTWS-PARM-TO-DATE-N
           END-IF.


      ******************************************************************
       1820-END.
      ******************************************************************

      ******************************************************************
       1830-FR-TO-DAY-EDITS                SECTION.
      ******************************************************************

           IF (ABEXTWS-PARM-FR-DD-N > 0)
                   AND
              (ABEXTWS-PARM-FR-DD-N < 32)
      *        THESE COULD BE ERRORS ????
               NEXT SENTENCE
           ELSE
               MOVE 0                      TO ABEXTWS-PARM-FR-DATE-N
                                              ABEXTWS-PARM-TO-DATE-N
           END-IF.

           IF (ABEXTWS-PARM-TO-DD-N > 0)
                   AND
              (ABEXTWS-PARM-TO-DD-N < 32)
               NEXT SENTENCE
           ELSE
               MOVE 0                      TO ABEXTWS-PARM-FR-DATE-N
                                              ABEXTWS-PARM-TO-DATE-N
           END-IF.


      ******************************************************************
       1830-END.
      ******************************************************************

      ******************************************************************
       1900-RESET-DYNAMIC-STORAGE       SECTION.
      ******************************************************************

           MOVE ZERO                   TO ABEXTWS-ABHISTHDR-SW
                                          ABEXTWS-ABHISTLINE-SW
                                          ABEXTWS-ABCURRHDR-SW
                                          ABEXTWS-ABCURRLINE-SW.
                                                   
           INITIALIZE                     ABEXTWS-WORK-FILTERS
                                          ABEXTWS-PROCESS-CURR-ONLY-SW.
                                                 
           INITIALIZE                     ABEXTWS-ABHISTHDR-KEYS
                                          ABEXTWS-ABCURRHDR-KEYS
                                          ABEXTWS-ABHISTHDR
                                          ABEXTWS-ABHISTLINE
                                          ABEXTWS-ABCURRHDR
                                          ABEXTWS-ABCURRLINE.


      ******************************************************************
       1900-END.
      ******************************************************************

      ******************************************************************
       2000-ABEXT-FIND                 SECTION.
      ******************************************************************
       2000-START.

           IF (ERROR-FOUND)
               GO TO 2000-END.


           PERFORM 4000-MOVE-ABEXT-XHH-KEYS.

           IF  (ABEXTWS-XHHSET = 1)
               PERFORM 840-FIND-XHHSET1
           ELSE         
           IF  (ABEXTWS-XHHSET = 2)
               PERFORM 840-FIND-XHHSET2
           ELSE         
           IF  (ABEXTWS-XHHSET = 3)
               PERFORM 840-FIND-XHHSET3
           ELSE         
               PERFORM 840-FIND-XHHSET2
           END-IF
           END-IF
           END-IF.


           IF (ABHISTHDR-FOUND)
               SET ABEXTWS-ABHISTHDR-FOUND            TO TRUE
               PERFORM 6000-START-FIND-ABHISTLINE
               PERFORM 400000-BUILD-ABEXTWS-LAYOUTS
           ELSE
               SET ABEXTWS-ABHISTHDR-NOTFOUND         TO TRUE
           END-IF.



      ******************************************************************
       2000-END.
      *****************************************************************

      *****************************************************************
       2100-ABEXT-START-FIND           SECTION.
      ******************************************************************
       2100-START.
      
           IF (ERROR-FOUND)
               GO TO 2100-END.

           MOVE ABEXTWS-WORK-FILTERS       TO ABEXTWS-XHHP-FILTERS.

           PERFORM 4000-MOVE-ABEXT-XHH-KEYS.

           PERFORM 5000-STRING-FILTERS.

           IF  (ABEXTWS-XHHSET = 1)
                IF (ABEXTWS-XHH-HAS-FILTERS)
                    PERFORM 850-FILTER-NLT-XHHSET1
                ELSE
                    PERFORM 850-FIND-NLT-XHHSET1
                END-IF
           ELSE         
           IF  (ABEXTWS-XHHSET = 2)
                IF (ABEXTWS-XHH-HAS-FILTERS)
                    PERFORM 850-FILTER-NLT-XHHSET2
                ELSE
                    PERFORM 850-FIND-NLT-XHHSET2
                END-IF
           ELSE         
           IF  (ABEXTWS-XHHSET = 3)
                IF (ABEXTWS-XHH-HAS-FILTERS)
                    PERFORM 850-FILTER-NLT-XHHSET3
                ELSE
                    PERFORM 850-FIND-NLT-XHHSET3
                END-IF
           END-IF
           END-IF
           END-IF.

      *     IF (ABHISTHDR-FOUND)
      *             AND
      *        (ABEXTWS-USE-FR-TO-PARMS)
      *         IF  (ABEXTWS-XHHSET = 1)
      *              PERFORM 860-FIND-NEXT-XHHSET1
      *                 UNTIL ((XHH-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHH-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTHDR-NOTFOUND))
      *         ELSE
      *         IF  (ABEXTWS-XHHSET = 2)
      *              PERFORM 860-FIND-NEXT-XHHSET2
      *                 UNTIL ((XHH-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHH-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTHDR-NOTFOUND))
      *         ELSE
      *         IF  (ABEXTWS-XHHSET = 3)
      *              PERFORM 860-FIND-NEXT-XHHSET3
      *                 UNTIL ((XHH-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHH-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTHDR-NOTFOUND))
      *         END-IF
      *         END-IF
      *         END-IF
      *     END-IF.

           PERFORM 3000-CHECK-FOR-EOF.

           GO TO 2100-END.


      ******************************************************************
       2100-END.
      *****************************************************************

      *****************************************************************
       2200-ABEXT-FIND-NEXT            SECTION.
      ******************************************************************
       2200-START.
      
           IF (ERROR-FOUND)
               GO TO 2200-END.

           IF (ABEXTWS-ABHISTHDR-FOUND)
               IF (ABEXTWS-ABHISTLINE-NOTFOUND)
                   NEXT SENTENCE 
               ELSE
                   PERFORM 7000-FIND-NEXT-ABHISTLINE
                   IF  (ABEXTWS-ABHISTLINE-FOUND)
                        GO TO 2200-END
                   ELSE
                        INITIALIZE            ABEXTWS-ABHISTLINE
                   END-IF
               END-IF
           END-IF.

      *     IF (ABEXTWS-USE-FR-TO-PARMS)
      *         SET ABHISTHDR-FOUND                    TO TRUE
      *         IF  (ABEXTWS-XHHSET = 1)
      *              PERFORM 860-FIND-NEXT-XHHSET1
      *                 UNTIL ((XHH-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHH-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTHDR-NOTFOUND))
      *         ELSE
      *         IF  (ABEXTWS-XHHSET = 2)
      *              PERFORM 860-FIND-NEXT-XHHSET2
      *                 UNTIL ((XHH-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHH-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTHDR-NOTFOUND))
      *         ELSE
      *         IF  (ABEXTWS-XHHSET = 3)
      *              PERFORM 860-FIND-NEXT-XHHSET3
      *                 UNTIL ((XHH-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHH-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTHDR-NOTFOUND))
      *         END-IF
      *         END-IF
      *         END-IF
      *     ELSE
               IF  (ABEXTWS-XHHSET = 1)
                    PERFORM 860-FIND-NEXT-XHHSET1
               ELSE         
               IF  (ABEXTWS-XHHSET = 2)
                    PERFORM 860-FIND-NEXT-XHHSET2
               ELSE         
               IF  (ABEXTWS-XHHSET = 3)
                    PERFORM 860-FIND-NEXT-XHHSET3
               END-IF
               END-IF
               END-IF.
      *     END-IF.

           PERFORM 3000-CHECK-FOR-EOF.

           GO TO 2200-END.


      ******************************************************************
       2200-END.
      *****************************************************************

      *****************************************************************
       2300-ABEXT-FIND-PREV            SECTION.
      ******************************************************************
       2300-START.
      
           IF (ERROR-FOUND)
               GO TO 2300-END.

           PERFORM 4000-MOVE-ABEXT-XHH-KEYS.

           IF (ABEXTWS-XHHSET = ZERO OR 1)
               PERFORM 850-FIND-NLT-XHHSET1
               PERFORM 870-FIND-PREV-XHHSET1
           ELSE         
           IF (ABEXTWS-XHHSET = 2)
               PERFORM 850-FIND-NLT-XHHSET2
               PERFORM 870-FIND-PREV-XHHSET2
           ELSE         
           IF (ABEXTWS-XHHSET = 3)
               PERFORM 850-FIND-NLT-XHHSET3
               PERFORM 870-FIND-PREV-XHHSET3
           ELSE         
               PERFORM 850-FIND-NLT-XHHSET1
               PERFORM 870-FIND-PREV-XHHSET1.

           IF (ABHISTHDR-FOUND)
               SET ABEXTWS-ABHISTHDR-FOUND            TO TRUE
               PERFORM 6000-START-FIND-ABHISTLINE
      *         PERFORM 400000-BUILD-ABEXTWS-LAYOUTS
           ELSE
               SET ABEXTWS-ABHISTHDR-NOTFOUND         TO TRUE
           END-IF.


      ******************************************************************
       2300-END.
      *****************************************************************

      ******************************************************************
       3000-CHECK-FOR-EOF                  SECTION.
      ******************************************************************
       3000-START.

           IF (ABEXTWS-XHHSET = 1)
               PERFORM 3100-CHECK-EOF-XHHSET1
           ELSE
           IF (ABEXTWS-XHHSET = 2)
               PERFORM 3200-CHECK-EOF-XHHSET2
           ELSE
           IF (ABEXTWS-XHHSET = 3)
               PERFORM 3300-CHECK-EOF-XHHSET3
           END-IF
           END-IF
           END-IF.

           PERFORM 400000-BUILD-ABEXTWS-LAYOUTS.
            
           GO TO 3000-END.


      ******************************************************************
       3100-CHECK-EOF-XHHSET1.
      ******************************************************************

           IF (ABEXTWS-XHHSET-GEN-ELEMS = 2)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET1-REQUEST-DATE NOT =  XHH-REQUEST-DATE)
               OR (ABEXTWS-XHHSET1-COMPANY      NOT =  XHH-COMPANY) 
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           ELSE
           IF (ABEXTWS-XHHSET-GEN-ELEMS = 3)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET1-REQUEST-DATE NOT =  XHH-REQUEST-DATE)
               OR (ABEXTWS-XHHSET1-COMPANY      NOT =  XHH-COMPANY) 
               OR (ABEXTWS-XHHSET1-ORDER-NBR    NOT =  XHH-ORDER-NBR)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           END-IF
           END-IF.


      ******************************************************************
       3200-CHECK-EOF-XHHSET2.
      ******************************************************************

           IF (ABEXTWS-XHHSET-GEN-ELEMS = 1)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET2-COMPANY      NOT =  XHH-COMPANY) 
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           ELSE
           IF (ABEXTWS-XHHSET-GEN-ELEMS = 2)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET2-COMPANY      NOT =  XHH-COMPANY) 
               OR (ABEXTWS-XHHSET2-ORDSRCE-CODE NOT =  XHH-ORDSRCE-CODE)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           ELSE
           IF (ABEXTWS-XHHSET-GEN-ELEMS = 3)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET2-COMPANY      NOT =  XHH-COMPANY) 
               OR (ABEXTWS-XHHSET2-ORDSRCE-CODE NOT =  XHH-ORDSRCE-CODE)
               OR (ABEXTWS-XHHSET2-ORDSRCE-YR   NOT =  XHH-ORDSRCE-YR)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           ELSE
           IF (ABEXTWS-XHHSET-GEN-ELEMS = 4)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET2-COMPANY      NOT =  XHH-COMPANY) 
               OR (ABEXTWS-XHHSET2-ORDSRCE-CODE NOT =  XHH-ORDSRCE-CODE)
               OR (ABEXTWS-XHHSET2-ORDSRCE-YR   NOT =  XHH-ORDSRCE-YR)
               OR (ABEXTWS-XHHSET2-ORDSRCE-NBR  NOT =  XHH-ORDSRCE-NBR)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           ELSE
           IF (ABEXTWS-XHHSET-GEN-ELEMS = 5)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET2-COMPANY      NOT =  XHH-COMPANY) 
               OR (ABEXTWS-XHHSET2-ORDSRCE-CODE NOT =  XHH-ORDSRCE-CODE)
               OR (ABEXTWS-XHHSET2-ORDSRCE-YR   NOT =  XHH-ORDSRCE-YR)
               OR (ABEXTWS-XHHSET2-ORDSRCE-NBR  NOT =  XHH-ORDSRCE-NBR)
               OR (ABEXTWS-XHHSET2-CUSTOMER     NOT =  XHH-CUSTOMER)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE                                  
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           ELSE
           IF (ABEXTWS-XHHSET-GEN-ELEMS = 6)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET2-COMPANY      NOT =  XHH-COMPANY) 
               OR (ABEXTWS-XHHSET2-ORDSRCE-CODE NOT =  XHH-ORDSRCE-CODE)
               OR (ABEXTWS-XHHSET2-ORDSRCE-YR   NOT =  XHH-ORDSRCE-YR)
               OR (ABEXTWS-XHHSET2-ORDSRCE-NBR  NOT =  XHH-ORDSRCE-NBR)
               OR (ABEXTWS-XHHSET2-CUSTOMER     NOT =  XHH-CUSTOMER)
               OR (ABEXTWS-XHHSET2-AB-REQUEST   NOT =  XHH-AB-REQUEST)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           ELSE
           IF (ABEXTWS-XHHSET-GEN-ELEMS = 7)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET2-COMPANY      NOT =  XHH-COMPANY) 
               OR (ABEXTWS-XHHSET2-ORDSRCE-CODE NOT =  XHH-ORDSRCE-CODE)
               OR (ABEXTWS-XHHSET2-ORDSRCE-YR   NOT =  XHH-ORDSRCE-YR)
               OR (ABEXTWS-XHHSET2-ORDSRCE-NBR  NOT =  XHH-ORDSRCE-NBR)
               OR (ABEXTWS-XHHSET2-CUSTOMER     NOT =  XHH-CUSTOMER)
               OR (ABEXTWS-XHHSET2-AB-REQUEST   NOT =  XHH-AB-REQUEST)
               OR (ABEXTWS-XHHSET2-REQUEST-DATE NOT =  XHH-REQUEST-DATE)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           ELSE
           IF (ABEXTWS-XHHSET-GEN-ELEMS = 8)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET2-COMPANY      NOT =  XHH-COMPANY) 
               OR (ABEXTWS-XHHSET2-ORDSRCE-CODE NOT =  XHH-ORDSRCE-CODE)
               OR (ABEXTWS-XHHSET2-ORDSRCE-YR   NOT =  XHH-ORDSRCE-YR)
               OR (ABEXTWS-XHHSET2-ORDSRCE-NBR  NOT =  XHH-ORDSRCE-NBR)
               OR (ABEXTWS-XHHSET2-CUSTOMER     NOT =  XHH-CUSTOMER)
               OR (ABEXTWS-XHHSET2-AB-REQUEST   NOT =  XHH-AB-REQUEST)
               OR (ABEXTWS-XHHSET2-REQUEST-DATE NOT =  XHH-REQUEST-DATE)
               OR (ABEXTWS-XHHSET2-ORDER-NBR    NOT =  XHH-ORDER-NBR)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF
           END-IF.


       3300-CHECK-EOF-XHHSET3.

           IF (ABEXTWS-XHHSET-GEN-ELEMS = 1)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET3-COMPANY      NOT =  XHH-COMPANY) 
                   SET ABEXTWS-ABHISTHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND        TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           ELSE
           IF (ABEXTWS-XHHSET-GEN-ELEMS = 2)
               IF (ABHISTHDR-NOTFOUND)
               OR (ABEXTWS-XHHSET3-COMPANY      NOT =  XHH-COMPANY) 
               OR (ABEXTWS-XHHSET3-ORDER-NBR    NOT =  XHH-ORDER-NBR)
                   SET ABEXTWS-ABHISTHDR-NOTFOUND      TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTHDR-FOUND         TO TRUE
                   PERFORM 6000-START-FIND-ABHISTLINE
               END-IF
           END-IF
           END-IF.


      ******************************************************************
       3000-END.
      ******************************************************************

      ******************************************************************
       4000-MOVE-ABEXT-XHH-KEYS        SECTION.
      ******************************************************************
       4000-START.

      *--- force re-read of record, that is, defeat smart I/O
           SET ABHISTHDR-NOTFOUND                     TO TRUE.

           PERFORM 4200-DETERMINE-BUILD-PART-KEY.

           IF (ABEXTWS-XHHSET = 1)
               MOVE ABEXTWS-XHHSET1-REQUEST-DATE   TO DB-REQUEST-DATE
               MOVE ABEXTWS-XHHSET1-COMPANY        TO DB-COMPANY
               MOVE ABEXTWS-XHHSET1-ORDER-NBR      TO DB-ORDER-NBR
           ELSE
           IF (ABEXTWS-XHHSET = 2)
               MOVE ABEXTWS-XHHSET2-COMPANY        TO DB-COMPANY
               MOVE ABEXTWS-XHHSET2-ORDSRCE-CODE   TO DB-ORDSRCE-CODE   
               MOVE ABEXTWS-XHHSET2-ORDSRCE-YR     TO DB-ORDSRCE-YR
               MOVE ABEXTWS-XHHSET2-ORDSRCE-NBR    TO DB-ORDSRCE-NBR
               MOVE ABEXTWS-XHHSET2-CUSTOMER       TO DB-CUSTOMER
               MOVE ABEXTWS-XHHSET2-AB-REQUEST     TO DB-AB-REQUEST
               MOVE ABEXTWS-XHHSET2-REQUEST-DATE   TO DB-REQUEST-DATE
               MOVE ABEXTWS-XHHSET2-ORDER-NBR      TO DB-ORDER-NBR
           ELSE
           IF (ABEXTWS-XHHSET = 3)
               MOVE ABEXTWS-XHHSET3-COMPANY        TO DB-COMPANY
               MOVE ABEXTWS-XHHSET3-ORDER-NBR      TO DB-ORDER-NBR
           END-IF                                    
           END-IF
           END-IF.

           GO TO 4000-END.
               

      ******************************************************************
       4200-DETERMINE-BUILD-PART-KEY.
      ******************************************************************

      *----------------------------------------------------------------*
      *  DETERMINE WHICH KEY SHOULD BE USED.   
      *  COMPANY IS REQUIRED. IF NOT PRESENT AN ERROR WILL BE PRODUCED.
      *  IF PARM IS USED AS A KEY FIELD, INITIALIZE THAT PARM FIELD
      *  SO THAT IT ISN'T USED AS A FILTER OR TO BUILD A DIFFERNT KEY.
      *
      *  IF ORDSRCE-CODE IS PRESENT USE XHHSET2  
      *  ELSE  
      *  IF ORDER-NBR IS PRESENT USE XHHSET3.                           
      *  IF REQUEST-DATE IS PRESENT USE XHHSET1.                    
      *----------------------------------------------------------------*


           IF (ABEXTWS-XHHP-COMPANY                  NOT = ZERO)
              IF (ABEXTWS-XHHP-ORDSRCE-CODE          NOT = SPACE)
                  MOVE 2                 TO ABEXTWS-XHHSET
                  MOVE 2                 TO ABEXTWS-XHHSET-GEN-ELEMS
                  MOVE ABEXTWS-XHHP-COMPANY
                                         TO ABEXTWS-XHHSET2-COMPANY
                  MOVE ABEXTWS-XHHP-ORDSRCE-CODE
                                         TO ABEXTWS-XHHSET2-ORDSRCE-CODE
                  MOVE ZERO              TO ABEXTWS-XHHP-COMPANY
                  MOVE SPACE             TO ABEXTWS-XHHP-ORDSRCE-CODE

                  IF (ABEXTWS-XHHP-ORDSRCE-YR-A      NOT = SPACE)
                      MOVE 3             TO ABEXTWS-XHHSET-GEN-ELEMS
                      MOVE ABEXTWS-XHHP-ORDSRCE-YR
                                         TO ABEXTWS-XHHSET2-ORDSRCE-YR
                      MOVE SPACE         TO ABEXTWS-XHHP-ORDSRCE-YR-A
                  END-IF

                  IF (ABEXTWS-XHHP-ORDSRCE-NBR-A     NOT = SPACE)
                        AND
                     (ABEXTWS-XHHSET2-ORDSRCE-YR     NOT = ZERO)
                      MOVE 4             TO ABEXTWS-XHHSET-GEN-ELEMS
                      MOVE ABEXTWS-XHHP-ORDSRCE-NBR
                                         TO ABEXTWS-XHHSET2-ORDSRCE-NBR
                      MOVE SPACE         TO ABEXTWS-XHHP-ORDSRCE-NBR-A
                  END-IF

                  IF (ABEXTWS-XHHP-CUSTOMER          NOT = SPACE)
                       AND
                     (ABEXTWS-XHHSET2-ORDSRCE-YR     NOT = ZERO)
                       AND
                     (ABEXTWS-XHHSET2-ORDSRCE-NBR-A  NOT = SPACE)
                      MOVE 5             TO ABEXTWS-XHHSET-GEN-ELEMS
                      MOVE ABEXTWS-XHHP-CUSTOMER   
                                         TO ABEXTWS-XHHSET2-CUSTOMER  
                      MOVE SPACE         TO ABEXTWS-XHHP-CUSTOMER   
                  END-IF

                  IF (ABEXTWS-XHHP-AB-REQUEST        NOT = SPACE)
                       AND
                     (ABEXTWS-XHHSET2-ORDSRCE-YR     NOT = ZERO)
                       AND
                     (ABEXTWS-XHHSET2-ORDSRCE-NBR-A  NOT = SPACE)
                       AND
                     (ABEXTWS-XHHSET2-CUSTOMER       NOT = SPACE)
                      MOVE 6             TO ABEXTWS-XHHSET-GEN-ELEMS
                      MOVE ABEXTWS-XHHP-AB-REQUEST 
                                         TO ABEXTWS-XHHSET2-AB-REQUEST
                      MOVE SPACE         TO ABEXTWS-XHHP-AB-REQUEST 
                  END-IF

                  IF (ABEXTWS-XHHP-REQUEST-DATE      NOT = ZERO)
                       AND
                     (ABEXTWS-XHHSET2-ORDSRCE-YR     NOT = ZERO)
                       AND
                     (ABEXTWS-XHHSET2-ORDSRCE-NBR-A  NOT = SPACE)
                       AND
                     (ABEXTWS-XHHSET2-CUSTOMER       NOT = SPACE)
                       AND
                     (ABEXTWS-XHHSET2-AB-REQUEST     NOT = SPACE)
                      MOVE 7             TO ABEXTWS-XHHSET-GEN-ELEMS
                      MOVE ABEXTWS-XHHP-REQUEST-DATE 
                                         TO ABEXTWS-XHHSET2-REQUEST-DATE
                      MOVE ZERO          TO ABEXTWS-XHHP-REQUEST-DATE 
                  END-IF

                  IF (ABEXTWS-XHHP-ORDER-NBR-A       NOT = SPACE)
                       AND
                     (ABEXTWS-XHHSET2-ORDSRCE-YR     NOT = ZERO)
                       AND
                     (ABEXTWS-XHHSET2-ORDSRCE-NBR-A  NOT = SPACE)
                       AND
                     (ABEXTWS-XHHSET2-CUSTOMER       NOT = SPACE)
                       AND
                     (ABEXTWS-XHHSET2-AB-REQUEST     NOT = SPACE)
                       AND
                     (ABEXTWS-XHHSET2-REQUEST-DATE   NOT = ZERO)
                      MOVE 8             TO ABEXTWS-XHHSET-GEN-ELEMS
                      MOVE ABEXTWS-XHHP-ORDER-NBR    
                                         TO ABEXTWS-XHHSET2-ORDER-NBR   
                      MOVE SPACE         TO ABEXTWS-XHHP-ORDER-NBR-A  
                  END-IF
              ELSE
                  IF (ABEXTWS-XHHP-ORDER-NBR-A       NOT = SPACE)
                      MOVE 3             TO ABEXTWS-XHHSET
                      MOVE 2             TO ABEXTWS-XHHSET-GEN-ELEMS
                      MOVE ABEXTWS-XHHP-COMPANY
                                         TO ABEXTWS-XHHSET3-COMPANY
                      MOVE ABEXTWS-XHHP-ORDER-NBR
                                         TO ABEXTWS-XHHSET3-ORDER-NBR
                      MOVE ZERO          TO ABEXTWS-XHHP-COMPANY
                      MOVE SPACE         TO ABEXTWS-XHHP-ORDER-NBR-A
                  ELSE
                      MOVE 3             TO ABEXTWS-XHHSET
                      MOVE 1             TO ABEXTWS-XHHSET-GEN-ELEMS
                      MOVE ABEXTWS-XHHP-COMPANY
                                         TO ABEXTWS-XHHSET3-COMPANY
                      MOVE ZERO          TO ABEXTWS-XHHP-COMPANY
                  END-IF
           END-IF.

           IF (ABEXTWS-XHHP-REQUEST-DATE             NOT = ZERO)
                   AND
              (ABEXTWS-XHHP-COMPANY                  NOT = ZERO)
               MOVE 1                    TO ABEXTWS-XHHSET
               MOVE 2                    TO ABEXTWS-XHHSET-GEN-ELEMS
               MOVE ABEXTWS-XHHP-REQUEST-DATE
                                         TO ABEXTWS-XHHSET1-REQUEST-DATE
               MOVE ABEXTWS-XHHP-COMPANY TO ABEXTWS-XHHSET1-COMPANY
               MOVE ZERO                 TO ABEXTWS-XHHP-REQUEST-DATE
                                            ABEXTWS-XHHP-COMPANY
               IF (ABEXTWS-XHHP-ORDER-NBR-A          NOT = SPACE)
                   MOVE 3                TO ABEXTWS-XHHSET-GEN-ELEMS
                   MOVE ABEXTWS-XHHP-ORDER-NBR 
                                         TO ABEXTWS-XHHSET1-ORDER-NBR
                   MOVE SPACE            TO ABEXTWS-XHHP-ORDER-NBR-A
               END-IF
           END-IF.

           IF (ABEXTWS-XHHSET2-ORDSRCE-YR-A             = SPACE)
               MOVE ZERO                 TO ABEXTWS-XHHSET2-ORDSRCE-YR
           END-IF.

           IF (ABEXTWS-XHHSET2-ORDSRCE-NBR-A             = SPACE)
               MOVE ZERO                 TO ABEXTWS-XHHSET2-ORDSRCE-NBR
           END-IF.

      *****  SOME KIND OF ERROR MESSAGE SET NEEDED KEY FIELDS NOT 
      *****  POPULATED.


      ******************************************************************
       4000-END.
      ******************************************************************

      ******************************************************************
       5000-STRING-FILTERS             SECTION.
      ******************************************************************
       5000-START.

           INITIALIZE                              FILTER-STRING.

           PERFORM 5000-CHECK-FOR-FILTERS.

           IF (ABEXTWS-XHH-HAS-FILTERS)
               PERFORM 5000-POPULATE-FILTER-VALUES
           END-IF.

           GO TO 5000-END.


      ******************************************************************
       5000-CHECK-FOR-FILTERS.
      ******************************************************************

           SET ABEXTWS-XHH-NO-FILTERS                 TO TRUE.

           IF (ABEXTWS-XHHP-ORDSRCE-CODE   NOT = SPACES)
               MOVE ABEXTWS-XHHF-ORDSRCE-CODE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.      

           IF (ABEXTWS-XHHP-ORDSRCE-YR-A   NOT = SPACE)
               MOVE ABEXTWS-XHHF-ORDSRCE-YR
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHHP-ORDSRCE-NBR-A  NOT = SPACE)
               MOVE ABEXTWS-XHHF-ORDSRCE-NBR 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHHP-CUSTOMER       NOT = SPACES)
               MOVE ABEXTWS-XHHF-CUSTOMER  TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHHP-AB-REQUEST     NOT = SPACES)
               MOVE ABEXTWS-XHHF-AB-REQUEST 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-USE-FR-TO-PARMS)
               MOVE ABEXTWS-XHHF-FR-REQ-DATE
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING 
               MOVE ABEXTWS-XHHF-TO-REQ-DATE
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           ELSE
           IF (ABEXTWS-XHHP-REQUEST-DATE   NOT = ZERO)
               MOVE ABEXTWS-XHHF-REQUEST-DATE  
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF
           END-IF.

           IF (ABEXTWS-XHHP-ORDER-NBR-A    NOT = SPACE)            
               MOVE ABEXTWS-XHHF-ORDER-NBR TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHHP-CUST-TYPE      NOT = SPACES)
               MOVE ABEXTWS-XHHF-CUST-TYPE TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.
 
           IF (ABEXTWS-XHHP-CUST-ORG-CODE  NOT = SPACES)
               MOVE ABEXTWS-XHHF-CUST-ORG-CODE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHHP-SHIP-CITY     NOT = SPACES)
               MOVE ABEXTWS-XHHF-SHIP-CITY
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHHP-CUST-STATE     NOT = SPACES)
               MOVE ABEXTWS-XHHF-CUST-STATE
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.
 
           IF (ABEXTWS-XHHP-CUST-MCA       NOT = SPACES)
               MOVE ABEXTWS-XHHF-CUST-MCA  TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHHP-PAYMENT-METHOD NOT = SPACES)
               MOVE ABEXTWS-XHHF-PAYMENT-METHOD
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHHP-SHIP-METHOD    NOT = SPACES)
               MOVE ABEXTWS-XHHF-SHIP-METHOD
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 5000-BUILD-FILTER-STRING
           END-IF.
 
           IF (ABEXTWS-XHH-HAS-FILTERS)
               MOVE FILTER-STRING          TO ABEXTWS-XHH-FILTER-STRING
               STRING ABEXTWS-XHH-FILTER-STRING   DELIMITED BY "  " 
                      ")"                         DELIMITED BY SIZE
                      INTO FILTER-STRING
           END-IF.


      ******************************************************************
       5000-BUILD-FILTER-STRING.
      ******************************************************************

           IF (ABEXTWS-XHH-NO-FILTERS)
               SET ABEXTWS-XHH-HAS-FILTERS TO TRUE
               STRING "("                         DELIMITED BY SIZE
                      ABEXTWS-WORK-FILTER-STRING  DELIMITED BY "  "
                      INTO FILTER-STRING
           ELSE
               MOVE FILTER-STRING          TO ABEXTWS-XHH-FILTER-STRING
               STRING ABEXTWS-XHH-FILTER-STRING   DELIMITED BY "  "
                      " AND "
                      ABEXTWS-WORK-FILTER-STRING  DELIMITED BY "  "
                      INTO FILTER-STRING
           END-IF.


      ******************************************************************
       5000-POPULATE-FILTER-VALUES.
      ******************************************************************

           PERFORM 890-CREATE-FILTER.
             
           IF (ABEXTWS-XHHP-ORDSRCE-CODE   NOT = SPACES)
               MOVE ABEXTWS-XHHP-ORDSRCE-CODE  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHHP-ORDSRCE-YR-A   NOT = SPACES)
               MOVE ABEXTWS-XHHP-ORDSRCE-YR    TO NUMERIC-FILTER-VALUE
               PERFORM 890-SET-NUMERIC-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHHP-ORDSRCE-NBR-A   NOT = SPACE)
               MOVE ABEXTWS-XHHP-ORDSRCE-NBR   TO NUMERIC-FILTER-VALUE
               PERFORM 890-SET-NUMERIC-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHHP-CUSTOMER       NOT = SPACES)
               MOVE ABEXTWS-XHHP-CUSTOMER      TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHHP-AB-REQUEST     NOT = SPACES)
               MOVE ABEXTWS-XHHP-AB-REQUEST    TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-USE-FR-TO-PARMS)
               MOVE ABEXTWS-PARM-FR-DATE       TO DATETIME-FILTER-VALUE
               PERFORM 890-SET-DATETIME-FILTER-VALUE
               MOVE ABEXTWS-PARM-TO-DATE       TO DATETIME-FILTER-VALUE
               PERFORM 890-SET-DATETIME-FILTER-VALUE
           ELSE
           IF (ABEXTWS-XHHP-REQUEST-DATE   NOT = ZERO)
               MOVE ABEXTWS-XHHP-REQUEST-DATE  TO DATETIME-FILTER-VALUE
               PERFORM 890-SET-DATETIME-FILTER-VALUE
           END-IF
           END-IF.

           IF (ABEXTWS-XHHP-ORDER-NBR-A    NOT = SPACE)
               MOVE ABEXTWS-XHHP-ORDER-NBR     TO NUMERIC-FILTER-VALUE
               PERFORM 890-SET-NUMERIC-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHHP-CUST-TYPE      NOT = SPACES)
               MOVE ABEXTWS-XHHP-CUST-TYPE     TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.
 
           IF (ABEXTWS-XHHP-CUST-ORG-CODE  NOT = SPACES)
               MOVE ABEXTWS-XHHP-CUST-ORG-CODE TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHHP-SHIP-CITY      NOT = SPACES)
               MOVE ABEXTWS-XHHP-SHIP-CITY     TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.
 
           IF (ABEXTWS-XHHP-CUST-STATE     NOT = SPACES)
               MOVE ABEXTWS-XHHP-CUST-STATE    TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.
 
           IF (ABEXTWS-XHHP-CUST-MCA       NOT = SPACES)
               MOVE ABEXTWS-XHHP-CUST-MCA      TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHHP-PAYMENT-METHOD NOT = SPACES)
               MOVE ABEXTWS-XHHP-PAYMENT-METHOD TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHHP-SHIP-METHOD    NOT = SPACES)
               MOVE ABEXTWS-XHHP-SHIP-METHOD   TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.
 

      ******************************************************************
       5000-END.
      ******************************************************************

      ******************************************************************
       6000-START-FIND-ABHISTLINE      SECTION.
      ******************************************************************

           SET ABEXTWS-EXT-FIND-NEXT                  TO TRUE.

           MOVE XHH-REQUEST-DATE               TO DB-REQUEST-DATE.
           MOVE XHH-COMPANY                    TO DB-COMPANY.
           MOVE XHH-ORDER-NBR                  TO DB-ORDER-NBR.
           MOVE 1                              TO DB-LINE-NBR.
           
           PERFORM 850-FIND-NLT-XHLSET1.                  

           IF (ABHISTLINE-FOUND)
               SET ABEXTWS-ABHISTLINE-FOUND           TO TRUE
           ELSE
               INITIALIZE                         ABEXTWS-ABHISTLINE
               SET ABEXTWS-ABHISTLINE-NOTFOUND        TO TRUE.


      ******************************************************************
       6000-END.
      ******************************************************************

      ******************************************************************
       7000-FIND-NEXT-ABHISTLINE       SECTION.
      ******************************************************************

           PERFORM 860-FIND-NEXT-XHLSET1.

           IF (ABHISTLINE-NOTFOUND)
           OR (XHL-REQUEST-DATE         NOT = DB-REQUEST-DATE) 
           OR (XHL-COMPANY              NOT = DB-COMPANY)
           OR (XHL-ORDER-NBR            NOT = DB-ORDER-NBR)
               SET ABEXTWS-ABHISTLINE-NOTFOUND        TO TRUE
           ELSE
               SET ABEXTWS-ABHISTLINE-FOUND           TO TRUE.

           PERFORM 400000-BUILD-ABEXTWS-LAYOUTS.

      ******************************************************************
       7000-END.
      ******************************************************************




      *****************************************************************
       22100-ABEXT-START-FIND           SECTION.
      ******************************************************************
       22100-START.
      
           IF (ERROR-FOUND)
               GO TO 22100-END.

           MOVE ABEXTWS-WORK-FILTERS           TO ABEXTWS-XCHP-FILTERS.

           PERFORM 24000-MOVE-ABEXT-XCH-KEYS.

           PERFORM 25000-STRING-FILTERS.

           IF (ABEXTWS-XCH-HAS-FILTERS)
               PERFORM 850-FILTER-NLT-XCHSET1
           ELSE
               PERFORM 850-FIND-NLT-XCHSET1
           END-IF.


      *     IF (ABEXTWS-USE-FR-TO-PARMS)
      *         PERFORM 860-FIND-NEXT-XCHSET1
      *            UNTIL ((XCH-REQUEST-DATE >
      *                    ABEXTWS-PARM-FR-DATE-N) 
      *                            AND
      *                   (XCH-REQUEST-DATE <
      *                    ABEXTWS-PARM-TO-DATE-N)
      *                            OR
      *                    (ABCURRHDR-NOTFOUND))
      *     END-IF.

           PERFORM 23000-CHECK-FOR-EOF.

           GO TO 22100-END.


      ******************************************************************
       22100-END.
      *****************************************************************

      *****************************************************************
       22200-ABEXT-FIND-NEXT            SECTION.
      ******************************************************************
       22200-START.
      
           IF (ERROR-FOUND)
               GO TO 22200-END.

           MOVE ABEXTWS-WORK-FILTERS           TO ABEXTWS-XCHP-FILTERS.

           IF (ABEXTWS-ABCURRHDR-FOUND)
               IF (ABEXTWS-ABCURRLINE-NOTFOUND)
                   NEXT SENTENCE 
               ELSE
                   PERFORM 27000-FIND-NEXT-ABCURRLINE
                   IF  (ABEXTWS-ABCURRLINE-FOUND)
                        GO TO 22200-END
                   ELSE
                        INITIALIZE    ABEXTWS-ABCURRLINE
                   END-IF
               END-IF
           END-IF.

      *     IF (ABEXTWS-USE-FR-TO-PARMS)
      *         SET ABCURRHDR-FOUND         TO TRUE
      *         PERFORM 860-FIND-NEXT-XCHSET1
      *            UNTIL ((XCH-REQUEST-DATE >
      *                    ABEXTWS-PARM-FR-DATE-N) 
      *                           AND
      *                   (XCH-REQUEST-DATE <
      *                    ABEXTWS-PARM-TO-DATE-N)
      *                           OR
      *                   (ABCURRHDR-NOTFOUND))
      *     ELSE
               PERFORM 860-FIND-NEXT-XCHSET1.
      *     END-IF.

           PERFORM 23000-CHECK-FOR-EOF.

           GO TO 22200-END.


      ******************************************************************
       22200-END.
      *****************************************************************

      ******************************************************************
       23000-CHECK-FOR-EOF                  SECTION.
      ******************************************************************
       23000-START.

           PERFORM 23100-CHECK-EOF-XCHSET1.

           PERFORM 400000-BUILD-ABEXTWS-LAYOUTS.
            
           GO TO 23000-END.


      ******************************************************************
       23100-CHECK-EOF-XCHSET1.
      ******************************************************************

           IF (ABEXTWS-XCHSET-GEN-ELEMS = 1)
               IF (ABCURRHDR-NOTFOUND)
               OR (ABEXTWS-XCHSET1-COMPANY      NOT =  XCH-COMPANY) 
                   SET ABEXTWS-ABCURRHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABCURRHDR-FOUND        TO TRUE
                   PERFORM 26000-START-FIND-ABCURRLINE
               END-IF
           ELSE
           IF (ABEXTWS-XCHSET-GEN-ELEMS = 2)
               IF (ABCURRHDR-NOTFOUND)
               OR (ABEXTWS-XCHSET1-COMPANY      NOT =  XCH-COMPANY) 
               OR (ABEXTWS-XCHSET1-ORDSRCE-CODE NOT =  XCH-ORDSRCE-CODE)
                   SET ABEXTWS-ABCURRHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABCURRHDR-FOUND        TO TRUE
                   PERFORM 26000-START-FIND-ABCURRLINE
               END-IF
           ELSE
           IF (ABEXTWS-XCHSET-GEN-ELEMS = 3)
               IF (ABCURRHDR-NOTFOUND)
               OR (ABEXTWS-XCHSET1-COMPANY      NOT =  XCH-COMPANY) 
               OR (ABEXTWS-XCHSET1-ORDSRCE-CODE NOT =  XCH-ORDSRCE-CODE)
               OR (ABEXTWS-XCHSET1-ORDSRCE-YR   NOT =  XCH-ORDSRCE-YR)
                   SET ABEXTWS-ABCURRHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABCURRHDR-FOUND        TO TRUE
                   PERFORM 26000-START-FIND-ABCURRLINE
               END-IF
           ELSE
           IF (ABEXTWS-XCHSET-GEN-ELEMS = 4)
               IF (ABCURRHDR-NOTFOUND)
               OR (ABEXTWS-XCHSET1-COMPANY      NOT =  XCH-COMPANY) 
               OR (ABEXTWS-XCHSET1-ORDSRCE-CODE NOT =  XCH-ORDSRCE-CODE)
               OR (ABEXTWS-XCHSET1-ORDSRCE-YR   NOT =  XCH-ORDSRCE-YR)
               OR (ABEXTWS-XCHSET1-ORDSRCE-NBR  NOT =  XCH-ORDSRCE-NBR)
                   SET ABEXTWS-ABCURRHDR-NOTFOUND     TO TRUE
               ELSE
                   SET ABEXTWS-ABCURRHDR-FOUND        TO TRUE
                   PERFORM 26000-START-FIND-ABCURRLINE
               END-IF
           END-IF
           END-IF
           END-IF
           END-IF.


      ******************************************************************
       23000-END.
      ******************************************************************

      ******************************************************************
       24000-MOVE-ABEXT-XCH-KEYS        SECTION.
      ******************************************************************
       24000-START.

      *--- force re-read of record, that is, defeat smart I/O
           SET ABCURRHDR-NOTFOUND                     TO TRUE.

            PERFORM 24200-DETERMINE-BUILD-PART-KEY.

           IF (ABEXTWS-XCHSET = 1)
               MOVE ABEXTWS-XCHSET1-COMPANY        TO DB-COMPANY
               MOVE ABEXTWS-XCHSET1-ORDSRCE-CODE   TO DB-ORDSRCE-CODE   
               MOVE ABEXTWS-XCHSET1-ORDSRCE-YR     TO DB-ORDSRCE-YR
               MOVE ABEXTWS-XCHSET1-ORDSRCE-NBR    TO DB-ORDSRCE-NBR
           END-IF.

           GO TO 24000-END.
               

      ******************************************************************
       24200-DETERMINE-BUILD-PART-KEY.
      ******************************************************************

      *----------------------------------------------------------------*
      *  DETERMINE WHICH KEY SHOULD BE USED.   
      *  COMPANY IS REQUIRED. IF NOT PRESENT AN ERROR WILL BE PRODUCED.
      *  IF PARM IS USED AS A KEY FIELD, INITIALIZE THAT PARM FIELD
      *  SO THAT IT ISN'T USED AS A FILTER OR TO BUILD A DIFFERNT KEY.
      *
      *  IF ORDSRCE-CODE IS PRESENT USE XHHSET2  
      *  ELSE  
      *  IF ORDER-NBR IS PRESENT USE XHHSET3.                           
      *  IF REQUEST-DATE IS PRESENT USE XHHSET1.                    
      *----------------------------------------------------------------*

           IF (ABEXTWS-XCHP-COMPANY                NOT = ZERO)
               MOVE 1                    TO ABEXTWS-XCHSET
               MOVE 1                    TO ABEXTWS-XCHSET-GEN-ELEMS
               MOVE ABEXTWS-XCHP-COMPANY
                                         TO ABEXTWS-XCHSET1-COMPANY
               MOVE ZERO                 TO ABEXTWS-XCHP-COMPANY

               IF (ABEXTWS-XCHP-ORDSRCE-CODE        NOT = SPACE)
                   MOVE 2                TO ABEXTWS-XCHSET-GEN-ELEMS
                   MOVE ABEXTWS-XCHP-ORDSRCE-CODE
                                         TO ABEXTWS-XCHSET1-ORDSRCE-CODE
                   MOVE SPACE            TO ABEXTWS-XCHP-ORDSRCE-CODE
               END-IF

               IF (ABEXTWS-XCHP-ORDSRCE-YR-A      NOT = SPACE) 
                       AND
                  (ABEXTWS-XCHSET1-ORDSRCE-CODE   NOT = SPACE)
                   MOVE 3                TO ABEXTWS-XCHSET-GEN-ELEMS
                   MOVE ABEXTWS-XCHP-ORDSRCE-YR
                                         TO ABEXTWS-XCHSET1-ORDSRCE-YR
                   MOVE SPACE            TO ABEXTWS-XCHP-ORDSRCE-YR-A
               END-IF

               IF (ABEXTWS-XCHP-ORDSRCE-NBR-A     NOT = SPACE)
                       AND
                  (ABEXTWS-XCHSET1-ORDSRCE-CODE   NOT = SPACE)
                       AND
                  (ABEXTWS-XCHSET1-ORDSRCE-YR     NOT = ZERO)
                   MOVE 4                TO ABEXTWS-XCHSET-GEN-ELEMS
                   MOVE ABEXTWS-XCHP-ORDSRCE-NBR
                                         TO ABEXTWS-XCHSET1-ORDSRCE-NBR
                   MOVE SPACE            TO ABEXTWS-XCHP-ORDSRCE-NBR-A
               END-IF

           END-IF.

           IF (ABEXTWS-XCHSET1-ORDSRCE-YR-A              = SPACE)
               MOVE ZERO                 TO ABEXTWS-XCHSET1-ORDSRCE-YR
           END-IF.

           IF (ABEXTWS-XCHSET1-ORDSRCE-NBR-A             = SPACE)
               MOVE ZERO                 TO ABEXTWS-XCHSET1-ORDSRCE-NBR
           END-IF.

      *****  SOME KIND OF ERROR MESSAGE SET NEEDED KEY FIELDS NOT 
      *****  POPULATED.


      ******************************************************************
       24000-END.
      ******************************************************************

      ******************************************************************
       25000-STRING-FILTERS             SECTION.
      ******************************************************************
       25000-START.

           INITIALIZE                              FILTER-STRING.

           PERFORM 25000-CHECK-FOR-FILTERS.

           IF (ABEXTWS-XCH-HAS-FILTERS)
               PERFORM 25000-POPULATE-FILTER-VALUES
           END-IF.

           GO TO 25000-END.


      ******************************************************************
       25000-CHECK-FOR-FILTERS.
      ******************************************************************

           SET ABEXTWS-XCH-NO-FILTERS                 TO TRUE.

           IF (ABEXTWS-XCHP-ORDSRCE-CODE   NOT = SPACES)
               MOVE ABEXTWS-XCHF-ORDSRCE-CODE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.      

           IF (ABEXTWS-XCHP-ORDSRCE-YR-A   NOT = SPACE)
               MOVE ABEXTWS-XCHF-ORDSRCE-YR
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCHP-ORDSRCE-NBR-A  NOT = SPACE)
               MOVE ABEXTWS-XCHF-ORDSRCE-NBR 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCHP-CUSTOMER       NOT = SPACES)
               MOVE ABEXTWS-XCHF-CUSTOMER  TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCHP-AB-REQUEST     NOT = SPACES)
               MOVE ABEXTWS-XCHF-AB-REQUEST 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-USE-FR-TO-PARMS)
               MOVE ABEXTWS-XCHF-FR-REQ-DATE
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING 
               MOVE ABEXTWS-XCHF-TO-REQ-DATE
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           ELSE
           IF (ABEXTWS-XCHP-REQUEST-DATE   NOT = ZERO)
               MOVE ABEXTWS-XCHF-REQUEST-DATE  
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF
           END-IF.

      *    IF (ABEXTWS-XCHP-REQUEST-DATE   NOT = ZERO)
      *        MOVE ABEXTWS-XCHF-REQUEST-DATE  
      *                                    TO ABEXTWS-WORK-FILTER-STRING
      *        PERFORM 25000-BUILD-FILTER-STRING
      *    END-IF.

           IF (ABEXTWS-XCHP-ORDER-NBR-A    NOT = SPACES)
               MOVE ABEXTWS-XCHF-ORDER-NBR TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCHP-CUST-TYPE      NOT = SPACES)
               MOVE ABEXTWS-XCHF-CUST-TYPE TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.
 
           IF (ABEXTWS-XCHP-CUST-ORG-CODE  NOT = SPACES)
               MOVE ABEXTWS-XCHF-CUST-ORG-CODE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCHP-SHIP-CITY     NOT = SPACES)
               MOVE ABEXTWS-XCHF-SHIP-CITY
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCHP-CUST-STATE     NOT = SPACES)
               MOVE ABEXTWS-XCHF-CUST-STATE
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.
 
           IF (ABEXTWS-XCHP-CUST-MCA       NOT = SPACES)
               MOVE ABEXTWS-XCHF-CUST-MCA  TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCHP-PAYMENT-METHOD NOT = SPACES)
               MOVE ABEXTWS-XCHF-PAYMENT-METHOD
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCHP-SHIP-METHOD    NOT = SPACES)
               MOVE ABEXTWS-XCHF-SHIP-METHOD
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 25000-BUILD-FILTER-STRING
           END-IF.
 
           IF (ABEXTWS-XCH-HAS-FILTERS)
               MOVE FILTER-STRING          TO ABEXTWS-XCH-FILTER-STRING
               STRING ABEXTWS-XCH-FILTER-STRING   DELIMITED BY "  " 
                      ")"                         DELIMITED BY SIZE
                      INTO FILTER-STRING
           END-IF.


      ******************************************************************
       25000-BUILD-FILTER-STRING.
      ******************************************************************

           IF (ABEXTWS-XCH-NO-FILTERS)
               SET ABEXTWS-XCH-HAS-FILTERS TO TRUE
               STRING "("                         DELIMITED BY SIZE
                      ABEXTWS-WORK-FILTER-STRING  DELIMITED BY "  "
                      INTO FILTER-STRING
           ELSE
               MOVE FILTER-STRING          TO ABEXTWS-XCH-FILTER-STRING
               STRING ABEXTWS-XCH-FILTER-STRING   DELIMITED BY "  "
                      " AND "
                      ABEXTWS-WORK-FILTER-STRING  DELIMITED BY "  "
                      INTO FILTER-STRING
           END-IF.

      ***  MAYBE WE DON'T NEED TO REBUILD FILTER VALUES ??????????
      ***  I THINK WE WILL NEED THIS BECAUSE ABCURRHDR HAS BATCH-NAME IN
      ***  THE MIDDLE OF THE KEY ELEMENTS THAT ABHISTHDR HAS.  SO WHEN
      ***  BUILDING THE KEY FOR ABCURRHDR, ELEMENTS THAT COULD HAVE BEEN
      ***  BUILT INTO THE KEY WILL HAVE TO BE FILTERS.
      ******************************************************************
       25000-POPULATE-FILTER-VALUES.
      ******************************************************************

           PERFORM 890-CREATE-FILTER.
             
           IF (ABEXTWS-XCHP-ORDSRCE-CODE   NOT = SPACES)
               MOVE ABEXTWS-XCHP-ORDSRCE-CODE TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCHP-ORDSRCE-YR-A   NOT = SPACES)
               MOVE ABEXTWS-XCHP-ORDSRCE-YR    TO NUMERIC-FILTER-VALUE
               PERFORM 890-SET-NUMERIC-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCHP-ORDSRCE-NBR-A   NOT = SPACE)
               MOVE ABEXTWS-XCHP-ORDSRCE-NBR   TO NUMERIC-FILTER-VALUE
               PERFORM 890-SET-NUMERIC-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCHP-CUSTOMER       NOT = SPACES)
               MOVE ABEXTWS-XCHP-CUSTOMER     TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCHP-AB-REQUEST     NOT = SPACES)
               MOVE ABEXTWS-XCHP-AB-REQUEST   TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-USE-FR-TO-PARMS)
               MOVE ABEXTWS-PARM-FR-DATE       TO DATETIME-FILTER-VALUE
               PERFORM 890-SET-DATETIME-FILTER-VALUE
               MOVE ABEXTWS-PARM-TO-DATE       TO DATETIME-FILTER-VALUE
               PERFORM 890-SET-DATETIME-FILTER-VALUE
           ELSE
           IF (ABEXTWS-XCHP-REQUEST-DATE   NOT = ZERO)
               MOVE ABEXTWS-XCHP-REQUEST-DATE  TO DATETIME-FILTER-VALUE
               PERFORM 890-SET-DATETIME-FILTER-VALUE
           END-IF
           END-IF.

      *    IF (ABEXTWS-XCHP-REQUEST-DATE   NOT = ZERO)
      *        MOVE ABEXTWS-XCHP-REQUEST-DATE  TO NUMERIC-FILTER-VALUE
      *        PERFORM 890-SET-NUMERIC-FILTER-VALUE
      *    END-IF.

           IF (ABEXTWS-XCHP-ORDER-NBR-A    NOT = SPACES)
               MOVE ABEXTWS-XCHP-ORDER-NBR     TO NUMERIC-FILTER-VALUE
               PERFORM 890-SET-NUMERIC-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCHP-CUST-TYPE      NOT = SPACES)
               MOVE ABEXTWS-XCHP-CUST-TYPE    TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.
 
           IF (ABEXTWS-XCHP-CUST-ORG-CODE  NOT = SPACES)
               MOVE ABEXTWS-XCHP-CUST-ORG-CODE 
                                               TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCHP-SHIP-CITY      NOT = SPACES)
               MOVE ABEXTWS-XCHP-SHIP-CITY    TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.
 
           IF (ABEXTWS-XCHP-CUST-STATE     NOT = SPACES)
               MOVE ABEXTWS-XCHP-CUST-STATE   TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.
 
           IF (ABEXTWS-XCHP-CUST-MCA       NOT = SPACES)
               MOVE ABEXTWS-XCHP-CUST-MCA     TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCHP-PAYMENT-METHOD NOT = SPACES)
               MOVE ABEXTWS-XCHP-PAYMENT-METHOD 
                                               TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCHP-SHIP-METHOD    NOT = SPACES)
               MOVE ABEXTWS-XCHP-SHIP-METHOD  TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.
 

      ******************************************************************
       25000-END.
      ******************************************************************

      ******************************************************************
       26000-START-FIND-ABCURRLINE      SECTION.
      ******************************************************************

           SET ABEXTWS-EXT-FIND-NEXT                  TO TRUE.

           MOVE XCH-UNIQUE-I-D                 TO DB-HDR-UNIQUE-I-D.
           INITIALIZE                             DB-LINE-NBR
                                                  DB-ITEM.
           
           PERFORM 850-FIND-NLT-XCLSET1.                  

           IF (ABCURRLINE-NOTFOUND)
           OR (XCL-HDR-UNIQUE-I-D       NOT = DB-HDR-UNIQUE-I-D) 
               SET ABEXTWS-ABCURRLINE-NOTFOUND        TO TRUE
               INITIALIZE                  ABEXTWS-ABCURRLINE
           ELSE
               SET ABEXTWS-ABCURRLINE-FOUND           TO TRUE.


      ******************************************************************
       26000-END.
      ******************************************************************

      ******************************************************************
       27000-FIND-NEXT-ABCURRLINE       SECTION.
      ******************************************************************

           PERFORM 860-FIND-NEXT-XCLSET1.

           IF (ABCURRLINE-NOTFOUND)
           OR (XCL-HDR-UNIQUE-I-D       NOT = DB-HDR-UNIQUE-I-D) 
               SET ABEXTWS-ABCURRLINE-NOTFOUND        TO TRUE
               INITIALIZE                  ABEXTWS-ABCURRLINE
           ELSE
               SET ABEXTWS-ABCURRLINE-FOUND           TO TRUE.

           PERFORM 400000-BUILD-ABEXTWS-LAYOUTS.

      ******************************************************************
       27000-END.
      ******************************************************************



      *---------------------------------------------------------------
      *
      *   THE PROCESS TO FILTER DATA IN THE "LINE" TABLES FIRST, THEN
      *   FETCH THE MATCHING "HEADER" DATA FOLLOWS
      *
      *   SOME OF THIS CODE MAY BE ABLE TO BE CONSOLIDATED WITH PRIOR
      *   CODE.  BUT, AT THIS TIME KEEPING SEPERATE FOR SIMPLICITY
      *---------------------------------------------------------------



      *****************************************************************
       32100-ABEXT-START-FIND           SECTION.
      ******************************************************************
       32100-START.
      
           IF (ERROR-FOUND)
               GO TO 32100-END.

      *     MOVE ABEXTWS-WORK-FILTERS       TO ABEXTWS-XHLP-FILTERS.

           PERFORM 32150-LOAD-LINE-FILTERS.

           PERFORM 34000-MOVE-ABEXT-XHL-KEYS.

           PERFORM 35000-STRING-FILTERS.

           IF (ABEXTWS-XHLSET = 1)
               IF (ABEXTWS-XHL-HAS-FILTERS)
                   PERFORM 850-FILTER-NLT-XHLSET1
               ELSE
                   PERFORM 850-FIND-NLT-XHLSET1
               END-IF
           ELSE
           IF (ABEXTWS-XHLSET = 2)
               IF (ABEXTWS-XHL-HAS-FILTERS)
                   PERFORM 850-FILTER-NLT-XHLSET2
               ELSE
                   PERFORM 850-FIND-NLT-XHLSET2
               END-IF
           END-IF
           END-IF.

      *     IF (ABHISTLINE-FOUND)
      *             AND
      *        (ABEXTWS-USE-FR-TO-PARMS)
      *         IF  (ABEXTWS-XHLSET = 1)
      *              PERFORM 860-FIND-NEXT-XHLSET1
      *                 UNTIL ((XHL-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHL-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTLINE-NOTFOUND))
      *         ELSE
      *         IF  (ABEXTWS-XHLSET = 2)
      *              PERFORM 860-FIND-NEXT-XHLSET2
      *                 UNTIL ((XHL-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHL-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTLINE-NOTFOUND))
      *         END-IF
      *         END-IF
      *     END-IF.

           PERFORM 33000-CHECK-FOR-EOF.

           GO TO 32100-END.


      ******************************************************************
       32150-LOAD-LINE-FILTERS.
      *****************************************************************

           IF (ABEXTWS-WORK-REQUEST-DATE-A IS NUMERIC)
               MOVE ABEXTWS-WORK-REQUEST-DATE  
                                           TO ABEXTWS-XHLP-REQUEST-DATE
           ELSE
               MOVE ZERO                   TO ABEXTWS-XHLP-REQUEST-DATE
           END-IF.

           IF (ABEXTWS-WORK-COMPANY-A IS NUMERIC)
               MOVE ABEXTWS-WORK-COMPANY   TO ABEXTWS-XHLP-COMPANY
           ELSE
               MOVE ZERO                   TO ABEXTWS-XHLP-COMPANY
           END-IF.

           IF (ABEXTWS-WORK-ORDER-NBR-A IS NUMERIC)
               MOVE ABEXTWS-WORK-ORDER-NBR TO ABEXTWS-XHLP-ORDER-NBR
           ELSE
               MOVE SPACE                  TO ABEXTWS-XHLP-ORDER-NBR-A
           END-IF.

           MOVE ABEXTWS-WORK-ITEM          TO ABEXTWS-XHLP-ITEM.
           MOVE ABEXTWS-WORK-ITEM-TYPE     TO ABEXTWS-XHLP-ITEM-TYPE.
           MOVE ABEXTWS-WORK-LOCATION      TO ABEXTWS-XHLP-LOCATION.

           IF (ABEXTWS-WORK-SHIP-DATE-A IS NUMERIC)
               MOVE ABEXTWS-WORK-SHIP-DATE TO ABEXTWS-XHLP-SHIP-DATE
           END-IF.

           IF (ABEXTWS-WORK-PRINT-DATE-A IS NUMERIC)
               MOVE ABEXTWS-WORK-PRINT-DATE TO ABEXTWS-XHLP-PRINT-DATE
           END-IF.

           MOVE ABEXTWS-WORK-ALPHA-CODE    TO ABEXTWS-XHLP-ALPHA-CODE.
           MOVE ABEXTWS-WORK-SALES-MAJCL   TO ABEXTWS-XHLP-SALES-MAJCL.
           MOVE ABEXTWS-WORK-INVEN-MAJCL   TO ABEXTWS-XHLP-INVEN-MAJCL.


      ******************************************************************
       32100-END.
      *****************************************************************

      *****************************************************************
       32200-ABEXT-FIND-NEXT            SECTION.
      ******************************************************************
       32200-START.
      
           IF (ERROR-FOUND)
               GO TO 32200-END.

      *     IF (ABEXTWS-USE-FR-TO-PARMS)
      *         SET ABHISTLINE-FOUND                   TO TRUE
      *         IF  (ABEXTWS-XHLSET = 1)
      *              PERFORM 860-FIND-NEXT-XHLSET1
      *                 UNTIL ((XHL-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHL-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTLINE-NOTFOUND))
      *         ELSE
      *         IF  (ABEXTWS-XHLSET = 2)
      *              PERFORM 860-FIND-NEXT-XHLSET2
      *                 UNTIL ((XHL-REQUEST-DATE >
      *                         ABEXTWS-PARM-FR-DATE-N) 
      *                                AND
      *                        (XHL-REQUEST-DATE <
      *                         ABEXTWS-PARM-TO-DATE-N)
      *                                OR
      *                        (ABHISTLINE-NOTFOUND))
      *         END-IF
      *         END-IF
      *     ELSE
               IF  (ABEXTWS-XHLSET = 1)
                    PERFORM 860-FIND-NEXT-XHLSET1
               ELSE
               IF  (ABEXTWS-XHLSET = 2)
                    PERFORM 860-FIND-NEXT-XHLSET2
               END-IF
               END-IF.

           PERFORM 33000-CHECK-FOR-EOF.

           GO TO 32200-END.


      ******************************************************************
       32200-END.
      *****************************************************************

      *****************************************************************
       32300-ABEXT-FIND-PREV            SECTION.
      ******************************************************************
       32300-START.
      
           IF (ERROR-FOUND)
               GO TO 32300-END.

           PERFORM 34000-MOVE-ABEXT-XHL-KEYS.

           IF  (ABEXTWS-XHLSET = ZERO OR 1)
               PERFORM 850-FIND-NLT-XHLSET1
               PERFORM 870-FIND-PREV-XHLSET1
           ELSE
           IF  (ABEXTWS-XHLSET = 2)
               PERFORM 850-FIND-NLT-XHLSET2
               PERFORM 870-FIND-PREV-XHLSET2
           END-IF
           END-IF.

           IF (ABHISTLINE-FOUND)
               SET ABEXTWS-ABHISTLINE-FOUND           TO TRUE
               PERFORM 36000-FIND-ABHISTHDR
           ELSE
               SET ABEXTWS-ABHISTLINE-NOTFOUND        TO TRUE
           END-IF.


      ******************************************************************
       32300-END.
      *****************************************************************

      ******************************************************************
       33000-CHECK-FOR-EOF                  SECTION.
      ******************************************************************
       33000-START.


           IF (ABEXTWS-XHLSET = 1)
               PERFORM 33100-CHECK-EOF-XHLSET1
           ELSE
           IF (ABEXTWS-XHLSET = 2)
               PERFORM 33200-CHECK-EOF-XHLSET2
           END-IF
           END-IF.

           PERFORM 400000-BUILD-ABEXTWS-LAYOUTS.
            
           GO TO 33000-END.


      ******************************************************************
       33100-CHECK-EOF-XHLSET1.
      ******************************************************************

           IF (ABEXTWS-XHLSET-GEN-ELEMS = 1)
               IF (ABHISTLINE-NOTFOUND)
               OR (ABEXTWS-XHLSET1-REQUEST-DATE NOT =  XHL-REQUEST-DATE)
                   SET ABEXTWS-ABHISTLINE-NOTFOUND    TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTLINE-FOUND       TO TRUE
                   PERFORM 36000-FIND-ABHISTHDR
               END-IF
           ELSE
           IF (ABEXTWS-XHLSET-GEN-ELEMS = 2)
               IF (ABHISTLINE-NOTFOUND)
               OR (ABEXTWS-XHLSET1-REQUEST-DATE NOT =  XHL-REQUEST-DATE)
               OR (ABEXTWS-XHLSET1-COMPANY      NOT =  XHL-COMPANY) 
                   SET ABEXTWS-ABHISTLINE-NOTFOUND    TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTLINE-FOUND       TO TRUE
                   PERFORM 36000-FIND-ABHISTHDR
               END-IF
           ELSE
           IF (ABEXTWS-XHLSET-GEN-ELEMS = 3)
               IF (ABHISTLINE-NOTFOUND)
               OR (ABEXTWS-XHLSET1-REQUEST-DATE NOT =  XHL-REQUEST-DATE)
               OR (ABEXTWS-XHLSET1-COMPANY      NOT =  XHL-COMPANY) 
               OR (ABEXTWS-XHLSET1-ORDER-NBR    NOT =  XHL-ORDER-NBR)
                   SET ABEXTWS-ABHISTLINE-NOTFOUND    TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTLINE-FOUND       TO TRUE
                   PERFORM 36000-FIND-ABHISTHDR
               END-IF
           END-IF
           END-IF
           END-IF.


      ******************************************************************
       33200-CHECK-EOF-XHLSET2.
      ******************************************************************

           IF (ABEXTWS-XHLSET-GEN-ELEMS = 1)
               IF (ABHISTLINE-NOTFOUND)
               OR (ABEXTWS-XHLSET2-COMPANY      NOT =  XHL-COMPANY)
                   SET ABEXTWS-ABHISTLINE-NOTFOUND    TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTLINE-FOUND       TO TRUE
                   PERFORM 36000-FIND-ABHISTHDR
               END-IF
           ELSE
           IF (ABEXTWS-XHLSET-GEN-ELEMS = 2)
               IF (ABHISTLINE-NOTFOUND)
               OR (ABEXTWS-XHLSET2-COMPANY      NOT =  XHL-COMPANY) 
               OR (ABEXTWS-XHLSET2-ORDER-NBR    NOT =  XHL-ORDER-NBR)
                   SET ABEXTWS-ABHISTLINE-NOTFOUND    TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTLINE-FOUND       TO TRUE
                   PERFORM 36000-FIND-ABHISTHDR
               END-IF
           ELSE
           IF (ABEXTWS-XHLSET-GEN-ELEMS = 3)
               IF (ABHISTLINE-NOTFOUND)
               OR (ABEXTWS-XHLSET2-COMPANY      NOT =  XHL-COMPANY) 
               OR (ABEXTWS-XHLSET2-ORDER-NBR    NOT =  XHL-ORDER-NBR)
               OR (ABEXTWS-XHLSET2-LINE-NBR     NOT =  XHL-LINE-NBR)
                   SET ABEXTWS-ABHISTLINE-NOTFOUND    TO TRUE
               ELSE
                   SET ABEXTWS-ABHISTLINE-FOUND       TO TRUE
                   PERFORM 36000-FIND-ABHISTHDR
               END-IF
           END-IF
           END-IF
           END-IF.


      ******************************************************************
       33000-END.
      ******************************************************************

      ******************************************************************
       34000-MOVE-ABEXT-XHL-KEYS        SECTION.
      ******************************************************************
       34000-START.

      *--- force re-read of record, that is, defeat smart I/O
           SET ABHISTLINE-NOTFOUND                    TO TRUE.

           PERFORM 34200-DETERMINE-BUILD-PART-KEY.

           IF (ABEXTWS-XHLSET = 1)
               IF (ABEXTWS-XHLSET-GEN-ELEMS = 1)
                   MOVE ABEXTWS-XHLSET1-REQUEST-DATE TO DB-REQUEST-DATE
                   INITIALIZE                           DB-COMPANY
                   INITIALIZE                           DB-ORDER-NBR
               ELSE
               IF (ABEXTWS-XHLSET-GEN-ELEMS = 2)
                   MOVE ABEXTWS-XHLSET1-REQUEST-DATE TO DB-REQUEST-DATE
                   MOVE ABEXTWS-XHLSET1-COMPANY      TO DB-COMPANY
                   INITIALIZE                           DB-ORDER-NBR
               ELSE
               IF (ABEXTWS-XHLSET-GEN-ELEMS = 3)
                   MOVE ABEXTWS-XHLSET1-REQUEST-DATE TO DB-REQUEST-DATE
                   MOVE ABEXTWS-XHLSET1-COMPANY      TO DB-COMPANY
                   MOVE ABEXTWS-XHLSET1-ORDER-NBR    TO DB-ORDER-NBR
               END-IF
               END-IF
               END-IF
           ELSE
           IF (ABEXTWS-XHLSET = 2)
               IF (ABEXTWS-XHLSET-GEN-ELEMS = 1)
                   MOVE ABEXTWS-XHLSET2-COMPANY      TO DB-COMPANY     
                   INITIALIZE                           DB-ORDER-NBR
                                                        DB-LINE-NBR 
               ELSE
               IF (ABEXTWS-XHLSET-GEN-ELEMS = 2)
                   MOVE ABEXTWS-XHLSET2-COMPANY      TO DB-COMPANY
                   MOVE ABEXTWS-XHLSET2-ORDER-NBR    TO DB-ORDER-NBR
                   INITIALIZE                           DB-LINE-NBR
      *         ELSE
      *         IF (ABEXTWS-XHLSET-GEN-ELEMS = 3)
      *             MOVE ABEXTWS-XHLSET2-COMPANY      TO DB-COMPANY
      *             MOVE ABEXTWS-XHLSET2-ORDER-NBR    TO DB-ORDER-NBR
      *             MOVE ABEXTWS-XHLSET2-LINE-NBR     TO DB-LINE-NBR
      *         END-IF
               END-IF
               END-IF
           END-IF
           END-IF.

           GO TO 34000-END.
               

      ******************************************************************
       34200-DETERMINE-BUILD-PART-KEY.
      ******************************************************************

      *----------------------------------------------------------------*
      *  DETERMINE WHICH KEY SHOULD BE USED.   
      *  IF PARM IS USED AS A KEY FIELD, INITIALIZE THAT PARM FIELD
      *  SO THAT IT ISN'T USED AS A FILTER OR TO BUILD A DIFFERNT KEY.
      *----------------------------------------------------------------*

           IF (ABEXTWS-XHLP-REQUEST-DATE             NOT = ZERO)
               MOVE 1                    TO ABEXTWS-XHLSET
               MOVE 1                    TO ABEXTWS-XHLSET-GEN-ELEMS
               MOVE ABEXTWS-XHLP-REQUEST-DATE
                                         TO ABEXTWS-XHLSET1-REQUEST-DATE
               MOVE SPACE                TO ABEXTWS-XHLP-REQUEST-DATE-A

               IF (ABEXTWS-XHLSET1-REQUEST-DATE      NOT = ZERO)
                   AND
                  (ABEXTWS-XHLP-COMPANY-A            NOT = SPACE)
                   MOVE 2                TO ABEXTWS-XHLSET-GEN-ELEMS
                   MOVE ABEXTWS-XHLP-COMPANY      
                                         TO ABEXTWS-XHLSET1-COMPANY     
                   MOVE SPACE            TO ABEXTWS-XHLP-COMPANY-A   
               END-IF

               IF (ABEXTWS-XHLSET1-REQUEST-DATE      NOT = ZERO)
                   AND
                  (ABEXTWS-XHLSET1-COMPANY           NOT = ZERO)
                   AND
                  (ABEXTWS-XHLP-ORDER-NBR-A          NOT = SPACE)
                   MOVE 3                TO ABEXTWS-XHLSET-GEN-ELEMS
                   MOVE ABEXTWS-XHLP-ORDER-NBR  
                                         TO ABEXTWS-XHLSET1-ORDER-NBR 
                   MOVE SPACE            TO ABEXTWS-XHLP-ORDER-NBR-A 
               END-IF

           ELSE
      *    IF (ABEXTWS-WORK-COMPANY-A                NOT = SPACE)
               MOVE 2                    TO ABEXTWS-XHLSET
               MOVE 1                    TO ABEXTWS-XHLSET-GEN-ELEMS
               MOVE ABEXTWS-XHLP-COMPANY
                                         TO ABEXTWS-XHLSET2-COMPANY     
               MOVE SPACE                TO ABEXTWS-XHLP-COMPANY-A

               IF (ABEXTWS-XHLSET2-COMPANY           NOT = ZERO)
                   AND
                  (ABEXTWS-XHLP-ORDER-NBR-A          NOT = SPACE)
                   MOVE 2                TO ABEXTWS-XHLSET-GEN-ELEMS
                   MOVE ABEXTWS-XHLP-ORDER-NBR  
                                         TO ABEXTWS-XHLSET2-ORDER-NBR 
                   MOVE SPACE            TO ABEXTWS-XHLP-ORDER-NBR-A 
               END-IF

      *         IF (ABEXTWS-XHLSET2-COMPANY           NOT = ZERO)
      *             AND
      *            (ABEXTWS-XHLP-ORDER-NBR-A          NOT = SPACE)
      *             AND
      *            (ABEXTWS-XHLP-LINE-NBR             NOT = ZERO)
      *             MOVE 2                TO ABEXTWS-XHLSET-GEN-ELEMS
      *             MOVE ABEXTWS-XHLP-ORDER-NBR  
      *                                   TO ABEXTWS-XHLSET2-ORDER-NBR 
      *             MOVE SPACE            TO ABEXTWS-XHLP-ORDER-NBR-A 
      *         END-IF

           END-IF.


      *****  SOME KIND OF ERROR MESSAGE SET NEEDED KEY FIELDS NOT 
      *****  POPULATED.


      ******************************************************************
       34000-END.
      ******************************************************************

      ******************************************************************
       35000-STRING-FILTERS             SECTION.
      ******************************************************************
       35000-START.

           INITIALIZE                              FILTER-STRING.

           PERFORM 35000-CHECK-FOR-FILTERS.

           IF (ABEXTWS-XHL-HAS-FILTERS)
               PERFORM 35000-POPULATE-FILTER-VALUES
           END-IF.

           GO TO 35000-END.


      ******************************************************************
       35000-CHECK-FOR-FILTERS.
      ******************************************************************

           SET ABEXTWS-XHL-NO-FILTERS                 TO TRUE.

           IF (ABEXTWS-XHLP-ITEM           NOT = SPACES)
               MOVE ABEXTWS-XHLF-ITEM         
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 35000-BUILD-FILTER-STRING
           END-IF.      

           IF (ABEXTWS-XHLP-ITEM-TYPE      NOT = SPACE)
               MOVE ABEXTWS-XHLF-ITEM-TYPE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 35000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHLP-LOCATION       NOT = SPACE)
               MOVE ABEXTWS-XHLF-LOCATION    
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 35000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHLP-SHIP-DATE-A    NOT = SPACES)
               MOVE ABEXTWS-XHLF-SHIP-DATE TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 35000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHLP-PRINT-DATE-A   NOT = SPACES)
               MOVE ABEXTWS-XHLF-PRINT-DATE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 35000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHLP-ALPHA-CODE     NOT = SPACES)
               MOVE ABEXTWS-XHLF-ALPHA-CODE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 35000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHLP-SALES-MAJCL    NOT = SPACES)
               MOVE ABEXTWS-XHLF-SALES-MAJCL   
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 35000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHLP-INVEN-MAJCL    NOT = SPACES)
               MOVE ABEXTWS-XHLF-INVEN-MAJCL
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 35000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XHL-HAS-FILTERS)
               MOVE FILTER-STRING          TO ABEXTWS-XHL-FILTER-STRING
               STRING ABEXTWS-XHL-FILTER-STRING   DELIMITED BY "  " 
                      ")"                         DELIMITED BY SIZE
                      INTO FILTER-STRING
           END-IF.


      ******************************************************************
       35000-BUILD-FILTER-STRING.
      ******************************************************************

           IF (ABEXTWS-XHL-NO-FILTERS)
               SET ABEXTWS-XHL-HAS-FILTERS TO TRUE
               STRING "("                         DELIMITED BY SIZE
                      ABEXTWS-WORK-FILTER-STRING  DELIMITED BY "  "
                      INTO FILTER-STRING
           ELSE
               MOVE FILTER-STRING          TO ABEXTWS-XHL-FILTER-STRING
               STRING ABEXTWS-XHL-FILTER-STRING   DELIMITED BY "  "
                      " AND "
                      ABEXTWS-WORK-FILTER-STRING  DELIMITED BY "  "
                      INTO FILTER-STRING
           END-IF.


      ******************************************************************
       35000-POPULATE-FILTER-VALUES.
      ******************************************************************

           PERFORM 890-CREATE-FILTER.
             
           IF (ABEXTWS-XHLP-ITEM           NOT = SPACES)
               MOVE ABEXTWS-XHLP-ITEM          TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHLP-ITEM-TYPE      NOT = SPACES)
               MOVE ABEXTWS-XHLP-ITEM-TYPE     TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHLP-LOCATION        NOT = SPACE)
               MOVE ABEXTWS-XHLP-LOCATION      TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHLP-SHIP-DATE-A IS NUMERIC)
               IF (ABEXTWS-XHLP-SHIP-DATE      NOT = ZERO)
                   MOVE ABEXTWS-XHLP-SHIP-DATE 
                                           TO DATETIME-FILTER-VALUE
                   PERFORM 890-SET-DATETIME-FILTER-VALUE
               END-IF
           END-IF.

           IF (ABEXTWS-XHLP-PRINT-DATE-A IS NUMERIC)
               IF (ABEXTWS-XHLP-PRINT-DATE      NOT = ZERO)
                   MOVE ABEXTWS-XHLP-PRINT-DATE 
                                           TO DATETIME-FILTER-VALUE
                   PERFORM 890-SET-DATETIME-FILTER-VALUE
               END-IF
           END-IF.

           IF (ABEXTWS-XHLP-ALPHA-CODE     NOT = SPACES)
               MOVE ABEXTWS-XHLP-ALPHA-CODE    TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHLP-SALES-MAJCL    NOT = SPACES)
               MOVE ABEXTWS-XHLP-SALES-MAJCL   TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XHLP-INVEN-MAJCL    NOT = SPACES)
               MOVE ABEXTWS-XHLP-INVEN-MAJCL   TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.


      ******************************************************************
       35000-END.
      ******************************************************************

      ******************************************************************
       36000-FIND-ABHISTHDR       SECTION.
      ******************************************************************

           SET ABEXTWS-EXT-FIND-NEXT                  TO TRUE.

           MOVE XHL-REQUEST-DATE               TO DB-REQUEST-DATE.
           MOVE XHL-COMPANY                    TO DB-COMPANY.
           MOVE XHL-ORDER-NBR                  TO DB-ORDER-NBR.
           
           PERFORM 840-FIND-XHHSET1.                  

           IF (ABHISTHDR-FOUND)
               SET ABEXTWS-ABHISTHDR-FOUND            TO TRUE
           ELSE
               INITIALIZE                         ABEXTWS-ABHISTHDR
               SET ABEXTWS-ABHISTHDR-NOTFOUND         TO TRUE.


      ******************************************************************
       36000-END.
      ******************************************************************







      *****************************************************************
       322100-ABEXT-START-FIND           SECTION.
      ******************************************************************
       322100-START.
      
           IF (ERROR-FOUND)
               GO TO 322100-END.

           PERFORM 322150-LOAD-LINE-FILTERS.

           PERFORM 325000-STRING-FILTERS.

           PERFORM 850-FILTER-NLT-XCLSET1.

           PERFORM 323000-CHECK-FOR-EOF.

           GO TO 322100-END.


      ******************************************************************
       322150-LOAD-LINE-FILTERS.
      *****************************************************************

           MOVE ABEXTWS-WORK-ITEM          TO ABEXTWS-XCLP-ITEM.
           MOVE ABEXTWS-WORK-ITEM-TYPE     TO ABEXTWS-XCLP-ITEM-TYPE.
           MOVE ABEXTWS-WORK-LOCATION      TO ABEXTWS-XCLP-LOCATION.

           IF (ABEXTWS-WORK-SHIP-DATE-A IS NUMERIC)
               MOVE ABEXTWS-WORK-SHIP-DATE TO ABEXTWS-XCLP-SHIP-DATE
           END-IF.

           IF (ABEXTWS-WORK-PRINT-DATE-A IS NUMERIC)
               MOVE ABEXTWS-WORK-PRINT-DATE TO ABEXTWS-XCLP-PRINT-DATE
           END-IF.

           MOVE ABEXTWS-WORK-ALPHA-CODE    TO ABEXTWS-XCLP-ALPHA-CODE.
           MOVE ABEXTWS-WORK-SALES-MAJCL   TO ABEXTWS-XCLP-SALES-MAJCL.
           MOVE ABEXTWS-WORK-INVEN-MAJCL   TO ABEXTWS-XCLP-INVEN-MAJCL.


      ******************************************************************
       322100-END.
      *****************************************************************

      *****************************************************************
       322200-ABEXT-FIND-NEXT            SECTION.
      ******************************************************************
       322200-START.
      
           IF (ERROR-FOUND)
               GO TO 322200-END.

           PERFORM 860-FIND-NEXT-XCLSET1.

           PERFORM 323000-CHECK-FOR-EOF.

           GO TO 322200-END.


      ******************************************************************
       322200-END.
      *****************************************************************

      ******************************************************************
       323000-CHECK-FOR-EOF                  SECTION.
      ******************************************************************
       323000-START.

           PERFORM 323100-CHECK-EOF-XCLSET1.

           PERFORM 400000-BUILD-ABEXTWS-LAYOUTS.
            
           GO TO 323000-END.


      ******************************************************************
       323100-CHECK-EOF-XCLSET1.
      ******************************************************************

               IF (ABCURRLINE-NOTFOUND)
                   SET ABEXTWS-ABCURRLINE-NOTFOUND    TO TRUE
               ELSE
                   SET ABEXTWS-ABCURRLINE-FOUND       TO TRUE
                   PERFORM 326000-FIND-ABCURRHDR
               END-IF.


      ******************************************************************
       323000-END.
      ******************************************************************

      ******************************************************************
       325000-STRING-FILTERS             SECTION.
      ******************************************************************
       325000-START.

           INITIALIZE                              FILTER-STRING.

           PERFORM 325000-CHECK-FOR-FILTERS.

           IF (ABEXTWS-XCL-HAS-FILTERS)
               PERFORM 325000-POPULATE-FILTER-VALUES
           END-IF.

           GO TO 325000-END.


      ******************************************************************
       325000-CHECK-FOR-FILTERS.
      ******************************************************************

           SET ABEXTWS-XCL-NO-FILTERS                 TO TRUE.

           IF (ABEXTWS-XCLP-ITEM           NOT = SPACES)
               MOVE ABEXTWS-XCLF-ITEM         
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 325000-BUILD-FILTER-STRING
           END-IF.      

           IF (ABEXTWS-XCLP-ITEM-TYPE      NOT = SPACE)
               MOVE ABEXTWS-XCLF-ITEM-TYPE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 325000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCLP-LOCATION       NOT = SPACE)
               MOVE ABEXTWS-XCLF-LOCATION    
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 325000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCLP-SHIP-DATE-A    NOT = SPACES)
               MOVE ABEXTWS-XCLF-SHIP-DATE TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 325000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCLP-PRINT-DATE-A   NOT = SPACES)
               MOVE ABEXTWS-XCLF-PRINT-DATE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 325000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCLP-ALPHA-CODE     NOT = SPACES)
               MOVE ABEXTWS-XCLF-ALPHA-CODE 
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 325000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCLP-SALES-MAJCL    NOT = SPACES)
               MOVE ABEXTWS-XCLF-SALES-MAJCL   
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 325000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCLP-INVEN-MAJCL    NOT = SPACES)
               MOVE ABEXTWS-XCLF-INVEN-MAJCL
                                           TO ABEXTWS-WORK-FILTER-STRING
               PERFORM 325000-BUILD-FILTER-STRING
           END-IF.

           IF (ABEXTWS-XCL-HAS-FILTERS)
               MOVE FILTER-STRING          TO ABEXTWS-XCL-FILTER-STRING
               STRING ABEXTWS-XCL-FILTER-STRING   DELIMITED BY "  " 
                      ")"                         DELIMITED BY SIZE
                      INTO FILTER-STRING
           END-IF.


      ******************************************************************
       325000-BUILD-FILTER-STRING.
      ******************************************************************

           IF (ABEXTWS-XCL-NO-FILTERS)
               SET ABEXTWS-XCL-HAS-FILTERS TO TRUE
               STRING "("                         DELIMITED BY SIZE
                      ABEXTWS-WORK-FILTER-STRING  DELIMITED BY "  "
                      INTO FILTER-STRING
           ELSE
               MOVE FILTER-STRING          TO ABEXTWS-XCL-FILTER-STRING
               STRING ABEXTWS-XCL-FILTER-STRING   DELIMITED BY "  "
                      " AND "
                      ABEXTWS-WORK-FILTER-STRING  DELIMITED BY "  "
                      INTO FILTER-STRING
           END-IF.

      ***  MAYBE WE DON'T NEED TO REBUILD FILTER VALUES ??????????
      ***  I THINK WE WILL NEED THIS BECAUSE ABCURRHDR HAS BATCH-NAME IN
      ***  THE MIDDLE OF THE KEY ELEMENTS THAT ABHISTHDR HAS.  SO WHEN
      ***  BUILDING THE KEY FOR ABCURRHDR, ELEMENTS THAT COULD HAVE BEEN
      ***  BUILT INTO THE KEY WILL HAVE TO BE FILTERS.
      ******************************************************************
       325000-POPULATE-FILTER-VALUES.
      ******************************************************************

           PERFORM 890-CREATE-FILTER.
             
           IF (ABEXTWS-XCLP-ITEM           NOT = SPACES)
               MOVE ABEXTWS-XCLP-ITEM          TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCLP-ITEM-TYPE      NOT = SPACES)
               MOVE ABEXTWS-XCLP-ITEM-TYPE     TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCLP-LOCATION       NOT = SPACE)
               MOVE ABEXTWS-XCLP-LOCATION      TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCLP-SHIP-DATE-A    NOT = SPACES)
               MOVE ABEXTWS-XCLP-SHIP-DATE     TO DATETIME-FILTER-VALUE
               PERFORM 890-SET-DATETIME-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCLP-PRINT-DATE-A   NOT = SPACES)
               MOVE ABEXTWS-XCLP-PRINT-DATE    TO DATETIME-FILTER-VALUE
               PERFORM 890-SET-DATETIME-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCLP-ALPHA-CODE     NOT = SPACES)
               MOVE ABEXTWS-XCLP-ALPHA-CODE    TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.

           IF (ABEXTWS-XCLP-SALES-MAJCL    NOT = SPACES)
               MOVE ABEXTWS-XCLP-SALES-MAJCL   TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.
 
           IF (ABEXTWS-XCLP-INVEN-MAJCL    NOT = SPACES)
               MOVE ABEXTWS-XCLP-INVEN-MAJCL   TO ALPHANUM-FILTER-VALUE
               PERFORM 890-SET-ALPHANUM-FILTER-VALUE
           END-IF.


      ******************************************************************
       325000-END.
      ******************************************************************

      ******************************************************************
       326000-FIND-ABCURRHDR            SECTION.
      ******************************************************************

           SET ABEXTWS-EXT-FIND-NEXT                  TO TRUE.

           MOVE XCL-HDR-UNIQUE-I-D             TO DB-UNIQUE-I-D.
           
           PERFORM 840-FIND-XCHUID.                  

           IF (ABCURRHDR-NOTFOUND)
           OR (XCH-UNIQUE-I-D           NOT = DB-UNIQUE-I-D) 
               SET ABEXTWS-ABCURRHDR-NOTFOUND         TO TRUE
               INITIALIZE                  ABEXTWS-ABCURRHDR
           ELSE
               SET ABEXTWS-ABCURRHDR-FOUND            TO TRUE.


      ******************************************************************
       326000-END.
      ******************************************************************

      ******************************************************************
       327000-FIND-NEXT-ABCURRLINE       SECTION.
      ******************************************************************

           PERFORM 860-FIND-NEXT-XCLSET1.

           IF (ABCURRLINE-NOTFOUND)
           OR (XCL-HDR-UNIQUE-I-D       NOT = DB-HDR-UNIQUE-I-D) 
               SET ABEXTWS-ABCURRLINE-NOTFOUND        TO TRUE
               INITIALIZE                  ABEXTWS-ABCURRLINE
           ELSE
               SET ABEXTWS-ABCURRLINE-FOUND           TO TRUE.


      ******************************************************************
       327000-END.
      ******************************************************************


      ******************************************************************
       400000-BUILD-ABEXTWS-LAYOUTS        SECTION.
      ******************************************************************


           IF (ABEXTWS-ABHISTHDR-FOUND)
                       AND
              (ABEXTWS-ABHISTLINE-FOUND)
                   PERFORM 411000-ABEXTWS-WRK-ABHISTHDR
                   PERFORM 412000-BUILD-ABEXTWS-ABHISTHDR
                   PERFORM 413000-ABEXTWS-WRK-ABHISTLINE
                   PERFORM 414000-BUILD-ABEXTWS-ABHISTLINE
                   PERFORM 450000-BUILD-ABEXTRACT-LAYOUT
           ELSE
           IF (ABEXTWS-ABCURRHDR-FOUND)
                       AND
              (ABEXTWS-ABCURRLINE-FOUND)
                   PERFORM 421000-ABEXTWS-WRK-ABCURRHDR
                   PERFORM 422000-BUILD-ABEXTWS-ABCURRHDR
                   PERFORM 423000-ABEXTWS-WRK-ABCURRLINE
                   PERFORM 424000-BUILD-ABEXTWS-ABCURRLINE
                   PERFORM 450000-BUILD-ABEXTRACT-LAYOUT
           END-IF
           END-IF.


      *     IF (ABHISTHDR-FOUND)
      *         PERFORM 411000-ABEXTWS-WRK-ABHISTHDR
      *         PERFORM 412000-BUILD-ABEXTWS-ABHISTHDR
      *     END-IF.

      *     IF (ABHISTLINE-FOUND)
      *         PERFORM 413000-ABEXTWS-WRK-ABHISTLINE
      *         PERFORM 414000-BUILD-ABEXTWS-ABHISTLINE
      *         PERFORM 450000-BUILD-ABEXTRACT-LAYOUT
      *     END-IF.

      *     IF (ABCURRHDR-FOUND)
      *         PERFORM 421000-ABEXTWS-WRK-ABCURRHDR
      *         PERFORM 422000-BUILD-ABEXTWS-ABCURRHDR
      *     END-IF.

      *     IF (ABCURRLINE-FOUND)
      *         PERFORM 423000-ABEXTWS-WRK-ABCURRLINE
      *         PERFORM 424000-BUILD-ABEXTWS-ABCURRLINE
      *         PERFORM 450000-BUILD-ABEXTRACT-LAYOUT
      *     END-IF.


           GO TO 400000-END.


      ******************************************************************
       411000-ABEXTWS-WRK-ABHISTHDR.
      ******************************************************************

           INITIALIZE                         ABEXTWS-WRK-ABHISTHDR.

           MOVE XHH-REQUEST-DATE           TO ABEXTWS-WHH-REQUEST-DATE.
           MOVE XHH-COMPANY                TO ABEXTWS-WHH-COMPANY.
           MOVE XHH-ORDER-NBR              TO ABEXTWS-WHH-ORDER-NBR.
           MOVE XHH-CUSTOMER               TO ABEXTWS-WHH-CUSTOMER.
           MOVE XHH-AB-REQUEST             TO ABEXTWS-WHH-AB-REQUEST. 
           MOVE XHH-CUST-TYPE              TO ABEXTWS-WHH-CUST-TYPE.
           MOVE XHH-CUST-ORG-CODE          TO ABEXTWS-WHH-CUST-ORG-CODE.
           MOVE XHH-SHIP-CITY              TO ABEXTWS-WHH-SHIP-CITY.
           MOVE XHH-CUST-STATE             TO ABEXTWS-WHH-CUST-STATE.
           MOVE XHH-CUST-MCA               TO ABEXTWS-WHH-CUST-MCA.
           MOVE XHH-BUDGET-FLAG            TO ABEXTWS-WHH-BUDGET-FLAG. 
           MOVE XHH-ORDSRCE-CODE           TO ABEXTWS-WHH-ORDSRCE-CODE.
           MOVE XHH-ORDSRCE-YR             TO ABEXTWS-WHH-ORDSRCE-YR.
           MOVE XHH-ORDSRCE-NBR            TO ABEXTWS-WHH-ORDSRCE-NBR.
           MOVE XHH-PAYMENT-METHOD        TO ABEXTWS-WHH-PAYMENT-METHOD.
           MOVE XHH-SHIP-METHOD            TO ABEXTWS-WHH-SHIP-METHOD. 
           MOVE XHH-SAP-OVR-COMP           TO ABEXTWS-WHH-SAP-OVR-COMP.
           MOVE XHH-SAP-OVR-BUS-A          TO ABEXTWS-WHH-SAP-OVR-BUS-A.
           MOVE XHH-SAP-OVR-COST-C        TO ABEXTWS-WHH-SAP-OVR-COST-C.
           MOVE XHH-SAP-OVR-ACCT           TO ABEXTWS-WHH-SAP-OVR-ACCT.
           MOVE XHH-SAP-OVR-ALLOC          TO ABEXTWS-WHH-SAP-OVR-ALLOC.
           MOVE XHH-SAP-OVR-INTORD        TO ABEXTWS-WHH-SAP-OVR-INTORD.


      ******************************************************************
       412000-BUILD-ABEXTWS-ABHISTHDR.
      ******************************************************************

           INITIALIZE                         ABEXTWS-ABHISTHDR.

           MOVE XHH-REQUEST-DATE           TO ABEXTWS-XHH-REQUEST-DATE.
           MOVE XHH-COMPANY                TO ABEXTWS-XHH-COMPANY.
           MOVE XHH-ORDER-NBR              TO ABEXTWS-XHH-ORDER-NBR.
           MOVE XHH-CUSTOMER               TO ABEXTWS-XHH-CUSTOMER.
           MOVE XHH-AB-REQUEST             TO ABEXTWS-XHH-AB-REQUEST. 
           MOVE XHH-CUST-TYPE              TO ABEXTWS-XHH-CUST-TYPE.
           MOVE XHH-CUST-ORG-CODE          TO ABEXTWS-XHH-CUST-ORG-CODE.
           MOVE XHH-SHIP-CITY              TO ABEXTWS-XHH-SHIP-CITY.
           MOVE XHH-CUST-STATE             TO ABEXTWS-XHH-CUST-STATE.
           MOVE XHH-CUST-MCA               TO ABEXTWS-XHH-CUST-MCA.
           MOVE XHH-BUDGET-FLAG            TO ABEXTWS-XHH-BUDGET-FLAG. 
           MOVE XHH-ORDSRCE-CODE           TO ABEXTWS-XHH-ORDSRCE-CODE.
           MOVE XHH-ORDSRCE-YR             TO ABEXTWS-XHH-ORDSRCE-YR.
           MOVE XHH-ORDSRCE-NBR            TO ABEXTWS-XHH-ORDSRCE-NBR.
           MOVE XHH-PAYMENT-METHOD        TO ABEXTWS-XHH-PAYMENT-METHOD.
           MOVE XHH-SHIP-METHOD            TO ABEXTWS-XHH-SHIP-METHOD. 
           MOVE XHH-SAP-OVR-COMP           TO ABEXTWS-XHH-SAP-OVR-COMP.
           MOVE XHH-SAP-OVR-BUS-A          TO ABEXTWS-XHH-SAP-OVR-BUS-A.
           MOVE XHH-SAP-OVR-COST-C        TO ABEXTWS-XHH-SAP-OVR-COST-C.
           MOVE XHH-SAP-OVR-ACCT           TO ABEXTWS-XHH-SAP-OVR-ACCT.
           MOVE XHH-SAP-OVR-ALLOC          TO ABEXTWS-XHH-SAP-OVR-ALLOC.
           MOVE XHH-SAP-OVR-INTORD        TO ABEXTWS-XHH-SAP-OVR-INTORD.

      ******************************************************************
       413000-ABEXTWS-WRK-ABHISTLINE.
      ******************************************************************

           INITIALIZE                         ABEXTWS-WRK-ABHISTLINE.

           MOVE XHL-REQUEST-DATE           TO ABEXTWS-WHL-REQUEST-DATE.
           MOVE XHL-COMPANY                TO ABEXTWS-WHL-COMPANY.
           MOVE XHL-ORDER-NBR              TO ABEXTWS-WHL-ORDER-NBR.
           MOVE XHL-LINE-NBR               TO ABEXTWS-WHL-LINE-NBR.
           MOVE XHL-SHIPMENT-NBR           TO ABEXTWS-WHL-SHIPMENT-NBR.
           MOVE XHL-ITEM                   TO ABEXTWS-WHL-ITEM.
           MOVE XHL-ITEM-TYPE              TO ABEXTWS-WHL-ITEM-TYPE.
           MOVE XHL-LOCATION               TO ABEXTWS-WHL-LOCATION.
           MOVE XHL-SHIP-DATE              TO ABEXTWS-WHL-SHIP-DATE.
           MOVE XHL-PRINT-DATE             TO ABEXTWS-WHL-PRINT-DATE.
           MOVE XHL-GENERIC                TO ABEXTWS-WHL-GENERIC.
           MOVE XHL-LONG-DESC              TO ABEXTWS-WHL-LONG-DESC.
           MOVE XHL-ALPHA-CODE             TO ABEXTWS-WHL-ALPHA-CODE.
           MOVE XHL-PROJECT-NBR            TO ABEXTWS-WHL-PROJECT-NBR.
           MOVE XHL-SALES-MAJCL            TO ABEXTWS-WHL-SALES-MAJCL.
           MOVE XHL-SALES-MINCL            TO ABEXTWS-WHL-SALES-MINCL.
           MOVE XHL-INVEN-MAJCL            TO ABEXTWS-WHL-INVEN-MAJCL.
           MOVE XHL-INVEN-MINCL            TO ABEXTWS-WHL-INVEN-MINCL.
           MOVE XHL-PURCH-MAJCL            TO ABEXTWS-WHL-PURCH-MAJCL.
           MOVE XHL-PURCH-MINCL            TO ABEXTWS-WHL-PURCH-MINCL.
           MOVE XHL-GEO-MAJCL              TO ABEXTWS-WHL-GEO-MAJCL.
           MOVE XHL-GEO-MINCL              TO ABEXTWS-WHL-GEO-MINCL.
           MOVE XHL-PROMO-CLASS            TO ABEXTWS-WHL-PROMO-CLASS.
           MOVE XHL-ORDERED-QTY            TO ABEXTWS-WHL-ORDERED-QTY.
           MOVE XHL-SHIPPED-QTY            TO ABEXTWS-WHL-SHIPPED-QTY.
           MOVE XHL-UNIT-PRICE             TO ABEXTWS-WHL-UNIT-PRICE.
           MOVE XHL-SHPD-UNIT-COST         TO ABEXTWS-WHL-UNIT-COST.
           MOVE XHL-SAP-MAT-COMP           TO ABEXTWS-WHL-SAP-MAT-COMP.
           MOVE XHL-SAP-MAT-BUS-A          TO ABEXTWS-WHL-SAP-MAT-BUS-A.
           MOVE XHL-SAP-MAT-COST-C        TO ABEXTWS-WHL-SAP-MAT-COST-C.
           MOVE XHL-SAP-MAT-ACCT           TO ABEXTWS-WHL-SAP-MAT-ACCT.
           MOVE XHL-SAP-MAT-ALLOC          TO ABEXTWS-WHL-SAP-MAT-ALLOC.
           MOVE XHL-SAP-MAT-INTORD        TO ABEXTWS-WHL-SAP-MAT-INTORD.


      ******************************************************************
       414000-BUILD-ABEXTWS-ABHISTLINE.
      ******************************************************************

           INITIALIZE                         ABEXTWS-ABHISTLINE.

           MOVE XHL-REQUEST-DATE           TO ABEXTWS-XHL-REQUEST-DATE.
           MOVE XHL-COMPANY                TO ABEXTWS-XHL-COMPANY.
           MOVE XHL-ORDER-NBR              TO ABEXTWS-XHL-ORDER-NBR.
           MOVE XHL-LINE-NBR               TO ABEXTWS-XHL-LINE-NBR.
           MOVE XHL-SHIPMENT-NBR           TO ABEXTWS-XHL-SHIPMENT-NBR.
           MOVE XHL-ITEM                   TO ABEXTWS-XHL-ITEM.
           MOVE XHL-ITEM-TYPE              TO ABEXTWS-XHL-ITEM-TYPE.
           MOVE XHL-LOCATION               TO ABEXTWS-XHL-LOCATION.
           MOVE XHL-SHIP-DATE              TO ABEXTWS-XHL-SHIP-DATE.
           MOVE XHL-PRINT-DATE             TO ABEXTWS-XHL-PRINT-DATE.
           MOVE XHL-GENERIC                TO ABEXTWS-XHL-GENERIC.
           MOVE XHL-LONG-DESC              TO ABEXTWS-XHL-LONG-DESC.
           MOVE XHL-ALPHA-CODE             TO ABEXTWS-XHL-ALPHA-CODE.
           MOVE XHL-PROJECT-NBR            TO ABEXTWS-XHL-PROJECT-NBR.
           MOVE XHL-SALES-MAJCL            TO ABEXTWS-XHL-SALES-MAJCL.
           MOVE XHL-SALES-MINCL            TO ABEXTWS-XHL-SALES-MINCL.
           MOVE XHL-INVEN-MAJCL            TO ABEXTWS-XHL-INVEN-MAJCL.
           MOVE XHL-INVEN-MINCL            TO ABEXTWS-XHL-INVEN-MINCL.
           MOVE XHL-PURCH-MAJCL            TO ABEXTWS-XHL-PURCH-MAJCL.
           MOVE XHL-PURCH-MINCL            TO ABEXTWS-XHL-PURCH-MINCL.
           MOVE XHL-GEO-MAJCL              TO ABEXTWS-XHL-GEO-MAJCL.
           MOVE XHL-GEO-MINCL              TO ABEXTWS-XHL-GEO-MINCL.
           MOVE XHL-PROMO-CLASS            TO ABEXTWS-XHL-PROMO-CLASS.
           MOVE XHL-ORDERED-QTY            TO ABEXTWS-XHL-ORDERED-QTY.
           MOVE XHL-SHIPPED-QTY            TO ABEXTWS-XHL-SHIPPED-QTY.
           MOVE XHL-UNIT-PRICE             TO ABEXTWS-XHL-UNIT-PRICE.
           MOVE XHL-SHPD-UNIT-COST        TO ABEXTWS-XHL-SHPD-UNIT-COST.
           MOVE XHL-SAP-MAT-COMP           TO ABEXTWS-XHL-SAP-MAT-COMP.
           MOVE XHL-SAP-MAT-BUS-A          TO ABEXTWS-XHL-SAP-MAT-BUS-A.
           MOVE XHL-SAP-MAT-COST-C        TO ABEXTWS-XHL-SAP-MAT-COST-C.
           MOVE XHL-SAP-MAT-ACCT           TO ABEXTWS-XHL-SAP-MAT-ACCT.
           MOVE XHL-SAP-MAT-ALLOC          TO ABEXTWS-XHL-SAP-MAT-ALLOC.
           MOVE XHL-SAP-MAT-INTORD        TO ABEXTWS-XHL-SAP-MAT-INTORD.


      ******************************************************************
       421000-ABEXTWS-WRK-ABCURRHDR.
      ******************************************************************

           INITIALIZE                         ABEXTWS-WRK-ABHISTHDR.

           MOVE XCH-UNIQUE-I-D             TO ABEXTWS-WHH-UNIQUE-I-D.
           MOVE XCH-REQUEST-DATE           TO ABEXTWS-WHH-REQUEST-DATE.
           MOVE XCH-COMPANY                TO ABEXTWS-WHH-COMPANY.
           MOVE XCH-ORDER-NBR              TO ABEXTWS-WHH-ORDER-NBR.
           MOVE XCH-CUSTOMER               TO ABEXTWS-WHH-CUSTOMER.
           MOVE XCH-BATCH-NAME             TO ABEXTWS-WHH-BATCH-NAME.
           MOVE XCH-AB-REQUEST             TO ABEXTWS-WHH-AB-REQUEST.
           MOVE XCH-CUST-TYPE              TO ABEXTWS-WHH-CUST-TYPE.
           MOVE XCH-CUST-ORG-CODE          TO ABEXTWS-WHH-CUST-ORG-CODE.
           MOVE XCH-SHIP-CITY              TO ABEXTWS-WHH-SHIP-CITY.
           MOVE XCH-CUST-STATE             TO ABEXTWS-WHH-CUST-STATE.
           MOVE XCH-CUST-MCA               TO ABEXTWS-WHH-CUST-MCA.
           MOVE XCH-BUDGET-FLAG            TO ABEXTWS-WHH-BUDGET-FLAG. 
           MOVE XCH-ORDSRCE-CODE           TO ABEXTWS-WHH-ORDSRCE-CODE.
           MOVE XCH-ORDSRCE-YR             TO ABEXTWS-WHH-ORDSRCE-YR.
           MOVE XCH-ORDSRCE-NBR            TO ABEXTWS-WHH-ORDSRCE-NBR.
           MOVE XCH-PAYMENT-METHOD        TO ABEXTWS-WHH-PAYMENT-METHOD.
           MOVE XCH-SHIP-METHOD            TO ABEXTWS-WHH-SHIP-METHOD. 
           MOVE XCH-SAP-OVR-COMP           TO ABEXTWS-WHH-SAP-OVR-COMP.
           MOVE XCH-SAP-OVR-BUS-A          TO ABEXTWS-WHH-SAP-OVR-BUS-A.
           MOVE XCH-SAP-OVR-COST-C        TO ABEXTWS-WHH-SAP-OVR-COST-C.
           MOVE XCH-SAP-OVR-ACCT           TO ABEXTWS-WHH-SAP-OVR-ACCT.
           MOVE XCH-SAP-OVR-ALLOC          TO ABEXTWS-WHH-SAP-OVR-ALLOC.
           MOVE XCH-SAP-OVR-INTORD        TO ABEXTWS-WHH-SAP-OVR-INTORD.


      ******************************************************************
       422000-BUILD-ABEXTWS-ABCURRHDR.
      ******************************************************************

           INITIALIZE                         ABEXTWS-ABCURRHDR.

           MOVE XCH-UNIQUE-I-D             TO ABEXTWS-XCH-UNIQUE-I-D.
           MOVE XCH-REQUEST-DATE           TO ABEXTWS-XCH-REQUEST-DATE.
           MOVE XCH-COMPANY                TO ABEXTWS-XCH-COMPANY.
           MOVE XCH-ORDER-NBR              TO ABEXTWS-XCH-ORDER-NBR.
           MOVE XCH-CUSTOMER               TO ABEXTWS-XCH-CUSTOMER.
           MOVE XCH-BATCH-NAME             TO ABEXTWS-XCH-BATCH-NAME.
           MOVE XCH-AB-REQUEST             TO ABEXTWS-XCH-AB-REQUEST.
           MOVE XCH-CUST-TYPE              TO ABEXTWS-XCH-CUST-TYPE.
           MOVE XCH-CUST-ORG-CODE          TO ABEXTWS-XCH-CUST-ORG-CODE.
           MOVE XCH-SHIP-CITY              TO ABEXTWS-XCH-SHIP-CITY.
           MOVE XCH-CUST-STATE             TO ABEXTWS-XCH-CUST-STATE.
           MOVE XCH-CUST-MCA               TO ABEXTWS-XCH-CUST-MCA.
           MOVE XCH-BUDGET-FLAG            TO ABEXTWS-XCH-BUDGET-FLAG. 
           MOVE XCH-ORDSRCE-CODE           TO ABEXTWS-XCH-ORDSRCE-CODE.
           MOVE XCH-ORDSRCE-YR             TO ABEXTWS-XCH-ORDSRCE-YR.
           MOVE XCH-ORDSRCE-NBR            TO ABEXTWS-XCH-ORDSRCE-NBR.
           MOVE XCH-PAYMENT-METHOD        TO ABEXTWS-XCH-PAYMENT-METHOD.
           MOVE XCH-SHIP-METHOD            TO ABEXTWS-XCH-SHIP-METHOD. 
           MOVE XCH-SAP-OVR-COMP           TO ABEXTWS-XCH-SAP-OVR-COMP.
           MOVE XCH-SAP-OVR-BUS-A          TO ABEXTWS-XCH-SAP-OVR-BUS-A.
           MOVE XCH-SAP-OVR-COST-C        TO ABEXTWS-XCH-SAP-OVR-COST-C.
           MOVE XCH-SAP-OVR-ACCT           TO ABEXTWS-XCH-SAP-OVR-ACCT.
           MOVE XCH-SAP-OVR-ALLOC          TO ABEXTWS-XCH-SAP-OVR-ALLOC.
           MOVE XCH-SAP-OVR-INTORD        TO ABEXTWS-XCH-SAP-OVR-INTORD.


      ******************************************************************
       423000-ABEXTWS-WRK-ABCURRLINE.
      ******************************************************************

           INITIALIZE                         ABEXTWS-WRK-ABHISTLINE.

           MOVE XCL-HDR-UNIQUE-I-D        TO ABEXTWS-WHL-HDR-UNIQUE-I-D.
           MOVE XCL-ITEM                   TO ABEXTWS-WHL-ITEM.
           MOVE XCL-LINE-NBR               TO ABEXTWS-WHL-LINE-NBR.
           MOVE XCL-SHIPMENT-NBR           TO ABEXTWS-WHL-SHIPMENT-NBR.
           MOVE XCL-ITEM-TYPE              TO ABEXTWS-WHL-ITEM-TYPE.
           MOVE XCL-LOCATION               TO ABEXTWS-WHL-LOCATION.
           MOVE XCL-SHIP-DATE              TO ABEXTWS-WHL-SHIP-DATE.
           MOVE XCL-PRINT-DATE             TO ABEXTWS-WHL-PRINT-DATE.
           MOVE XCL-GENERIC                TO ABEXTWS-WHL-GENERIC.
           MOVE XCL-LONG-DESC              TO ABEXTWS-WHL-LONG-DESC.
           MOVE XCL-ALPHA-CODE             TO ABEXTWS-WHL-ALPHA-CODE.
           MOVE XCL-PROJECT-NBR            TO ABEXTWS-WHL-PROJECT-NBR.
           MOVE XCL-SALES-MAJCL            TO ABEXTWS-WHL-SALES-MAJCL.
           MOVE XCL-SALES-MINCL            TO ABEXTWS-WHL-SALES-MINCL.
           MOVE XCL-INVEN-MAJCL            TO ABEXTWS-WHL-INVEN-MAJCL.
           MOVE XCL-INVEN-MINCL            TO ABEXTWS-WHL-INVEN-MINCL.
           MOVE XCL-PURCH-MAJCL            TO ABEXTWS-WHL-PURCH-MAJCL.
           MOVE XCL-PURCH-MINCL            TO ABEXTWS-WHL-PURCH-MINCL.
           MOVE XCL-GEO-MAJCL              TO ABEXTWS-WHL-GEO-MAJCL.
           MOVE XCL-GEO-MINCL              TO ABEXTWS-WHL-GEO-MINCL.
           MOVE XCL-PROMO-CLASS            TO ABEXTWS-WHL-PROMO-CLASS.
           MOVE XCL-ORDERED-QTY            TO ABEXTWS-WHL-ORDERED-QTY.
           MOVE XCL-SHIPPED-QTY            TO ABEXTWS-WHL-SHIPPED-QTY.
           MOVE XCL-UNIT-PRICE             TO ABEXTWS-WHL-UNIT-PRICE.
           MOVE XCL-UNIT-COST              TO ABEXTWS-WHL-UNIT-COST.
           MOVE XCL-SAP-MAT-COMP           TO ABEXTWS-WHL-SAP-MAT-COMP.
           MOVE XCL-SAP-MAT-BUS-A          TO ABEXTWS-WHL-SAP-MAT-BUS-A.
           MOVE XCL-SAP-MAT-COST-C        TO ABEXTWS-WHL-SAP-MAT-COST-C.
           MOVE XCL-SAP-MAT-ACCT           TO ABEXTWS-WHL-SAP-MAT-ACCT.
           MOVE XCL-SAP-MAT-ALLOC          TO ABEXTWS-WHL-SAP-MAT-ALLOC.
           MOVE XCL-SAP-MAT-INTORD        TO ABEXTWS-WHL-SAP-MAT-INTORD.


      ******************************************************************
       424000-BUILD-ABEXTWS-ABCURRLINE.
      ******************************************************************

           INITIALIZE                         ABEXTWS-ABCURRLINE.

           MOVE XCL-HDR-UNIQUE-I-D        TO ABEXTWS-XCL-HDR-UNIQUE-I-D.
           MOVE XCL-ITEM                   TO ABEXTWS-XCL-ITEM.
           MOVE XCL-LINE-NBR               TO ABEXTWS-XCL-LINE-NBR.
           MOVE XCL-SHIPMENT-NBR           TO ABEXTWS-XCL-SHIPMENT-NBR.
           MOVE XCL-ITEM-TYPE              TO ABEXTWS-XCL-ITEM-TYPE.
           MOVE XCL-LOCATION               TO ABEXTWS-XCL-LOCATION.
           MOVE XCL-SHIP-DATE              TO ABEXTWS-XCL-SHIP-DATE.
           MOVE XCL-PRINT-DATE             TO ABEXTWS-XCL-PRINT-DATE.
           MOVE XCL-GENERIC                TO ABEXTWS-XCL-GENERIC.
           MOVE XCL-LONG-DESC              TO ABEXTWS-XCL-LONG-DESC.
           MOVE XCL-ALPHA-CODE             TO ABEXTWS-XCL-ALPHA-CODE.
           MOVE XCL-PROJECT-NBR            TO ABEXTWS-XCL-PROJECT-NBR.
           MOVE XCL-SALES-MAJCL            TO ABEXTWS-XCL-SALES-MAJCL.
           MOVE XCL-SALES-MINCL            TO ABEXTWS-XCL-SALES-MINCL.
           MOVE XCL-INVEN-MAJCL            TO ABEXTWS-XCL-INVEN-MAJCL.
           MOVE XCL-INVEN-MINCL            TO ABEXTWS-XCL-INVEN-MINCL.
           MOVE XCL-PURCH-MAJCL            TO ABEXTWS-XCL-PURCH-MAJCL.
           MOVE XCL-PURCH-MINCL            TO ABEXTWS-XCL-PURCH-MINCL.
           MOVE XCL-GEO-MAJCL              TO ABEXTWS-XCL-GEO-MAJCL.
           MOVE XCL-GEO-MINCL              TO ABEXTWS-XCL-GEO-MINCL.
           MOVE XCL-PROMO-CLASS            TO ABEXTWS-XCL-PROMO-CLASS.
           MOVE XCL-ORDERED-QTY            TO ABEXTWS-XCL-ORDERED-QTY.
           MOVE XCL-SHIPPED-QTY            TO ABEXTWS-XCL-SHIPPED-QTY.
           MOVE XCL-UNIT-PRICE             TO ABEXTWS-XCL-UNIT-PRICE.
           MOVE XCL-UNIT-COST              TO ABEXTWS-XCL-UNIT-COST.
           MOVE XCL-SAP-MAT-COMP           TO ABEXTWS-XCL-SAP-MAT-COMP.
           MOVE XCL-SAP-MAT-BUS-A          TO ABEXTWS-XCL-SAP-MAT-BUS-A.
           MOVE XCL-SAP-MAT-COST-C        TO ABEXTWS-XCL-SAP-MAT-COST-C.
           MOVE XCL-SAP-MAT-ACCT           TO ABEXTWS-XCL-SAP-MAT-ACCT.
           MOVE XCL-SAP-MAT-ALLOC          TO ABEXTWS-XCL-SAP-MAT-ALLOC.
           MOVE XCL-SAP-MAT-INTORD        TO ABEXTWS-XCL-SAP-MAT-INTORD.


      ******************************************************************
       450000-BUILD-ABEXTRACT-LAYOUT.
      ******************************************************************

           INITIALIZE                         ABEXTRACTWS.

      *  POPULATE OLD ABEXTRACT FILE LAYOUT FROM NEW ABCURRHDR

           MOVE ABEXTWS-WHH-COMPANY        TO ABEXTRACT-COMPANY.
           MOVE ABEXTWS-WHH-REQUEST-DATE   TO ABEXTRACT-REQUEST-DATE.
           MOVE ABEXTWS-WHH-ORDER-NBR      TO ABEXTRACT-ORDER-NBR.
           MOVE ABEXTWS-WHH-CUSTOMER       TO ABEXTRACT-CUSTOMER.
      *     MOVE ABEXTWS-XHH-AB-REQUEST
           MOVE ABEXTWS-WHH-CUST-TYPE      TO ABEXTRACT-CUST-TYPE.
           MOVE ABEXTWS-WHH-CUST-ORG-CODE  TO ABEXTRACT-CUST-ORG-CODE.
           MOVE ABEXTWS-WHH-SHIP-CITY      TO ABEXTRACT-SHIP-CITY.
           MOVE ABEXTWS-WHH-CUST-STATE     TO ABEXTRACT-CUST-STATE.
           MOVE ABEXTWS-WHH-CUST-MCA       TO ABEXTRACT-CUST-MCA.
      *     MOVE ABEXTWS-XHH-BUDGET-FLAG    TO 
           MOVE ABEXTWS-WHH-ORDSRCE-CODE   TO ABEXTRACT-ORDSRCE-CODE.
           MOVE ABEXTWS-WHH-ORDSRCE-YR     TO ABEXTRACT-ORDSRCE-YR.
           MOVE ABEXTWS-WHH-ORDSRCE-NBR    TO ABEXTRACT-ORDSRCE-NBR.
           MOVE ABEXTWS-WHH-PAYMENT-METHOD TO ABEXTRACT-PAYMENT-METHOD.
      *     MOVE ABEXTWS-XHH-SHIP-METHOD    TO 
           MOVE ABEXTWS-WHH-SAP-OVR-COMP   TO ABEXTRACT-SAP-COMPANY.
           MOVE ABEXTWS-WHH-SAP-OVR-BUS-A  TO ABEXTRACT-SAP-BUS-AREA.
           MOVE ABEXTWS-WHH-SAP-OVR-COST-C TO ABEXTRACT-SAP-COST-CENTER.
           MOVE ABEXTWS-WHH-SAP-OVR-ACCT   TO ABEXTRACT-SAP-ACCOUNT.
           MOVE ABEXTWS-WHH-SAP-OVR-ALLOC  TO ABEXTRACT-SAP-ALLOCATION.
           MOVE ABEXTWS-WHH-SAP-OVR-INTORD 
                                          TO ABEXTRACT-SAP-INTERN-ORDER.

      *  POPULATE OLD ABEXTRACT FILE LAYOUT FROM NEW ABCURRLINE

           MOVE ABEXTWS-WHL-LINE-NBR       TO ABEXTRACT-LINE-NBR.
           MOVE ABEXTWS-WHL-SHIPMENT-NBR   TO ABEXTRACT-SHIPMENT-NBR.
           MOVE ABEXTWS-WHL-ITEM           TO ABEXTRACT-ITEM.
           MOVE ABEXTWS-WHL-ITEM-TYPE      TO ABEXTRACT-ITEM-TYPE.
           MOVE ABEXTWS-WHL-LOCATION       TO ABEXTRACT-LOCATION.
           MOVE ABEXTWS-WHL-SHIP-DATE      TO ABEXTRACT-SHIP-DATE.
           MOVE ABEXTWS-WHL-PRINT-DATE     TO ABEXTRACT-PRINT-DATE.
           MOVE ABEXTWS-WHL-GENERIC        TO ABEXTRACT-GENERIC.
           MOVE ABEXTWS-WHL-LONG-DESC      TO ABEXTRACT-LONG-DESC.
           MOVE ABEXTWS-WHL-ALPHA-CODE     TO ABEXTRACT-ALPHA-CODE.
      *     MOVE ABEXTWS-XHL-PROJECT-NBR    TO ABEXTRACT-
           MOVE ABEXTWS-WHL-SALES-MAJCL    TO ABEXTRACT-SALES-MAJCL.
           MOVE ABEXTWS-WHL-SALES-MINCL    TO ABEXTRACT-SALES-MINCL.
           MOVE ABEXTWS-WHL-INVEN-MAJCL    TO ABEXTRACT-INVEN-MAJCL.
           MOVE ABEXTWS-WHL-INVEN-MINCL    TO ABEXTRACT-INVEN-MINCL.
           MOVE ABEXTWS-WHL-PURCH-MAJCL    TO ABEXTRACT-PURCH-MAJCL.
           MOVE ABEXTWS-WHL-PURCH-MINCL    TO ABEXTRACT-PURCH-MINCL.
           MOVE ABEXTWS-WHL-GEO-MAJCL      TO ABEXTRACT-GEO-MAJCL.
           MOVE ABEXTWS-WHL-GEO-MINCL      TO ABEXTRACT-GEO-MINCL.
           MOVE ABEXTWS-WHL-PROMO-CLASS    TO ABEXTRACT-PROMO-CLASS.
           MOVE ABEXTWS-WHL-ORDERED-QTY    TO ABEXTRACT-ORDERED-QTY.
           MOVE ABEXTWS-WHL-SHIPPED-QTY    TO ABEXTRACT-SHIPPED-QTY.
           MOVE ABEXTWS-WHL-UNIT-PRICE     TO ABEXTRACT-UNIT-PRICE.
           MOVE ABEXTWS-WHL-UNIT-COST    TO ABEXTRACT-SHIPPED-UNIT-COST.


      ******************************************************************
       400000-END.
      ******************************************************************



