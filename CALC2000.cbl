       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALC2000.
      * Programmer : Aidan Dunbar test
      * Date       : 02/04/2026
      * Github Url :https://github.com/ADunbar5612/Chapter1COBOLCalc
      * Description:This Is A simple Calculator program written in cobol
      * that calculates future values for an investment and doubles the
      * investment amount
       environment division.

       input-output section.

       data division.

       file section.

       working-storage section.

       01  user-entries.

           05  number-entered              pic 9        value 1.
           05  investment-amount           pic 99999    value 1000.  
           05  number-of-years             pic 99       value 10. 
           05  yearly-interest-rate        pic 99v9     value 5.5.

       01  work-fields.

           05  future-value                pic 9(7)v99.
           05  year-counter                pic 999.
           
           05 edited-whole-value           pic zz,zzz,zz9.
           05 edited-decimal-value         pic zzz,zzz,99.

       procedure division.

       000-calculate-future-values.

           perform 100-calculate-future-value
               until number-entered = zero.
           display "End of session.".
           stop run.

       100-calculate-future-value.

           display "----------------------------------------".
           display "To end the program, enter 0.".
           display "To perform another calculation, enter 1.".
           display "----------------------------------------".
           if number-entered = 1
               move investment-amount to future-value
               move 1 to year-counter
               perform 120-calculate-next-fv
                   until year-counter > number-of-years
               move future-value to edited-future-value
               display "Future value = " edited-future-value.
       
       120-calculate-next-fv.

           compute future-value rounded =
               future-value +
                   (future-value * yearly-interest-rate / 100).
           add 1 to year-counter.