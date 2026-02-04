       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC2000.
      * Programmer: Aidan Dunbar
      * Date : 02/04/2026
      * GitHub URL   
      *
      * Description
      *

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       * ---------------------------------
       * SALES TAX VARIABLES
       * ---------------------------------
       77  END-OF-SESSION-SWITCH    PIC X       VALUE "N".
       77  SALES-AMOUNT             PIC 9(5)V99.
       77  SALES-TAX                PIC Z,ZZZ.99.

       * ---------------------------------
       * FUTURE VALUE USER ENTRIES
       * ---------------------------------
       01  USER-ENTRIES.
           05 NUMBER-ENTERED        PIC 9.
           05 INVESTMENT-AMOUNT     PIC 9(5).
           05 NUMBER-OF-YEARS       PIC 99.
           05 YEARLY-INTEREST-RATE  PIC 99V9.

       * ---------------------------------
       * WORKING FIELDS
       * ---------------------------------
       01  WORK-FIELDS.
           05 FUTURE-VALUE          PIC 9(7)V99.
           05 YEAR-COUNTER          PIC 99       VALUE 1.
           05 EDITED-FUTURE-VALUE   PIC Z,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.

       * =================================
       * MAIN CONTROL PARAGRAPH
       * =================================
       000-MAIN.
           PERFORM 010-CALCULATE-SALES-TAX
           PERFORM 020-CALCULATE-FUTURE-VALUES
           DISPLAY "END OF SESSION.".
           STOP RUN.

       * =================================
       * SALES TAX CALCULATION
       * =================================
       010-CALCULATE-SALES-TAX.
           PERFORM 100-CALCULATE-ONE-SALES-TAX
               UNTIL END-OF-SESSION-SWITCH = "Y".

       100-CALCULATE-ONE-SALES-TAX.
           DISPLAY "-----------------------------------------------".
           DISPLAY "TO END PROGRAM, ENTER 0.".
           DISPLAY "TO CALCULATE SALES TAX, ENTER THE SALES AMOUNT.".
           ACCEPT SALES-AMOUNT.

           IF SALES-AMOUNT = ZERO
               MOVE "Y" TO END-OF-SESSION-SWITCH
           ELSE
               COMPUTE SALES-TAX ROUNDED =
                   SALES-AMOUNT * .0785
               DISPLAY "SALES TAX = " SALES-TAX
           END-IF.

       * =================================
       * FUTURE VALUE CALCULATION
       * =================================
       020-CALCULATE-FUTURE-VALUES.
           PERFORM 200-CALCULATE-FUTURE-VALUE
               UNTIL NUMBER-ENTERED = ZERO.
           DISPLAY "End of session.".

       200-CALCULATE-FUTURE-VALUE.
           DISPLAY "----------------------------------------".
           DISPLAY "To end the program, enter 0.".
           DISPLAY "To perform another calculation, enter 1.".
           ACCEPT NUMBER-ENTERED.
           DISPLAY "----------------------------------------".

           IF NUMBER-ENTERED = 1
               PERFORM 210-GET-USER-VALUES
               MOVE INVESTMENT-AMOUNT TO FUTURE-VALUE
               MOVE 1 TO YEAR-COUNTER
               PERFORM 220-CALCULATE-NEXT-FV
                   UNTIL YEAR-COUNTER > NUMBER-OF-YEARS
               MOVE FUTURE-VALUE TO EDITED-FUTURE-VALUE
               DISPLAY "Future value = " EDITED-FUTURE-VALUE
           END-IF.

       210-GET-USER-VALUES.
           DISPLAY "Enter investment amount (xxxxx).".
           ACCEPT INVESTMENT-AMOUNT.
           DISPLAY "Enter number of years (xx).".
           ACCEPT NUMBER-OF-YEARS.
           DISPLAY "Enter yearly interest rate (xx.x).".
           ACCEPT YEARLY-INTEREST-RATE.

       220-CALCULATE-NEXT-FV.
           COMPUTE FUTURE-VALUE ROUNDED =
               FUTURE-VALUE +
               (FUTURE-VALUE * YEARLY-INTEREST-RATE / 100).
           ADD 1 TO YEAR-COUNTER.
