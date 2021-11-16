 IDENTIFICATION DIVISION.
       PROGRAM-ID.  LAB5.
       AUTHOR. Matthew Simpson.
      *  LAB EXERCISE 1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT INPUT-FILE   ASSIGN TO 'COB1-EMPLOYEE'.
            SELECT PRNT-FILE    ASSIGN TO 'UR-S-PRNT'.

      *deleted EJECT from here
       DATA DIVISION.

      *deleted SKIP3 from here
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  INPUT-REC                 PIC X(80).

      *deleted SKIP2 from here
       FD  PRNT-FILE
           LABEL RECORDS ARE OMITTED.
       01  PRNT-REC                   PIC X(145).
       WORKING-STORAGE SECTION.
       01 MISC.
                03 LINE-COUNT           PIC 99          VALUE 0.
                03 TOTAL              PIC 9(5)V99     VALUE 0.
                03 BALANCE              PIC 9(5)V99     VALUE 0.
       01 BAL-TABLE.
                05 PAID OCCURS 4 TIMES PIC 9(4)V99.


      ***do not type this line – the asterisks are in column 7
      **************************************************************
      *           LAYOUT FOR THE INPUT FILE                       *
      **************************************************************
       01  INPUT-DATA.
                03  I-NAME                   PIC X(20).
                03  I-DEGREE                 PIC X(4).
                03  I-YEAR                   PIC X(4).
                03  I-LOAN                   PIC 9(5)V99.
                03  I-PAID1                  PIC 9(4)V99.
                03  I-PAID2                  PIC 9(4)V99.
                03  I-PAID3                  PIC 9(4)V99.
                03  I-PAID4                  PIC 9(4)V99.
      ********************************************
      *      LAYOUT FOR THE 1ST  DATA LINE OF REPORT PRNTING       *
      **************************************************************
       01  PRNT-DATA1.
            03  P-NAME                 PIC X(20).
            03  P-DEGREE               PIC X(4).
            03  P-YEAR                 PIC X(4).
            03  FILLER                 PIC X(10)      VALUE SPACES.
            03  P-LOAN                 PIC 99999.99.
            03  FILLER                 PIC X(5)      VALUE SPACES.
            03  P-PAID1                PIC 9999.99.
            03  FILLER                 PIC X(5)      VALUE SPACES.
            03  P-PAID2                PIC 9999.99.
            03  FILLER                 PIC X(5)      VALUE SPACES.
 03  P-PAID3                PIC 9999.99.
            03  FILLER                 PIC X(5)      VALUE SPACES.
            03  P-PAID4                PIC 9999.99.
            03  FILLER                 PIC X(5)      VALUE SPACES.
      **************************************************************
      *    LAYOUT FOR THE 1ST HEADING LINE OF REPORT PRNTING       *
      **************************************************************
       01  PRNT-HEADING1.
                03  FILLER                 PIC X(9)       VALUE 'NAME'.
                03  FILLER                 PIC X(21)      VALUE SPACES.
                03  FILLER                 PIC X(9)       VALUE 'DEGR.'.
                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  FILLER                 PIC X(9)       VALUE 'YEAR'.
                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  FILLER                 PIC X(9)       VALUE 'LOAN'.

                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  FILLER                 PIC X(9)       VALUE 'PAID1'.
                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  FILLER                 PIC X(9)       VALUE 'PAID2'.

                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  FILLER                 PIC X(9)       VALUE 'PAID3'.

                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  FILLER                 PIC X(9)       VALUE 'PAID4'.

                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  FILLER                 PIC X(9)       VALUE 'TOT'.

                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  FILLER                 PIC X(9)       VALUE 'BAL'.

       01 PRINT-FOOTER.
                03  FILLER                 PIC X(18)       VALUE SPACES.
                03  FILLER                 PIC X(9)   VALUE 'TOTAL'.
                03  TOTPAID                PIC 99999.99 VALUE TOTAL.
                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  FILLER                 PIC X(9)   VALUE 'BALANCE'.

                03  FILLER                 PIC X(5)       VALUE SPACES.
                03  BAL                    PIC 99999.99 VALUE BALANCE.
      **************************************************************
      *                 END OF FILE (EOF) SWITCHES                 *
      *            0 = NOT AT EOF          1 = AT EOF              *
      **************************************************************
                03  EOF-I                  PIC 9         VALUE 0.
      **************************************************************
      *               START OF PROCEDURE DIVISION                  *
      **************************************************************
       PROCEDURE DIVISION.
       000-MAINLINE.
                OPEN INPUT INPUT-FILE
                OUTPUT PRNT-FILE.
                PERFORM 2000-READ-INPUT.

                PERFORM 1400-PRINT-HEAD.
                PERFORM 1500-LOOP
				  UNTIL EOF-I = 1.
                CLOSE INPUT-FILE
                 PRNT-FILE.
                STOP RUN.
       1400-PRINT-HEAD.
                WRITE PRNT-REC FROM PRNT-HEADING1
                 AFTER ADVANCING PAGE.
                MOVE SPACES TO PRNT-REC.
                WRITE PRNT-REC
                AFTER ADVANCING 1 LINE.
       1500-LOOP.
                IF LINE-COUNT > 8
                THEN PERFORM 1800-HEADING.

                PERFORM 1600-PRINT-NAMES.
                PERFORM PRINT-FOOTER.
                PERFORM 2000-READ-INPUT.

      **************************************************************
      *   PRINTS THE SCHEDULE INFORMATION                          *
      **************************************************************
       1600-PRINT-NAMES.
                MOVE I-NAME                     TO  P-NAME.
                MOVE I-DEGREE                   TO  P-DEGREE.
                MOVE I-YEAR                     TO  P-YEAR.
                MOVE I-LOAN                     TO  P-LOAN.
           PERFORM  1900-LOAD-ARRAY.

           PERFORM 1700-MATH
           MOVE PAID(1)                    TO P-PAID1.
           MOVE PAID(2)                    TO P-PAID2.
           MOVE PAID(3)                    TO P-PAID3.
           MOVE PAID(4)                    TO P-PAID4.


                WRITE PRNT-REC FROM PRNT-DATA1
                AFTER ADVANCING 1 LINE.


       1700-MATH.
           MOVE 0 TO TOTAL.
           MOVE 0 TO BALANCE.
           PERFORM VARYING SUB FROM 1 BY 1
                   UNTIL SUB > 4
                   ADD PAID(SUB) TO TOTAL
           END-PERFORM.
           SUBTRACT TOTAL FROM I-LOAN GIVING BALANCE.


        1800-HEADING.
           WRITE PRINT-REC FROM  PRNT-HEADING1
                AFTER ADVANCING 1 PAGE.
           MOVE 0 TO LINE-CT.
        1900-LOAD-ARRAY.
		  MOVE I-PAID1 TO PAID(1).
      *COBOL ARRAY ELEMENTS START AT 1, NOT 0.
           MOVE I-PAID2 TO PAID(2).
           MOVE I-PAID3 TO PAID(3).
           MOVE I-PAID4 TO PAID(4).
      **************************************************************
      *                READS THE INPUT FILE                       *
      **************************************************************
       2000-READ-INPUT.
                READ INPUT-FILE INTO INPUT-DATA
               AT END MOVE 1 TO EOF-I.


 
