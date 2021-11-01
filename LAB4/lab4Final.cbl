       IDENTIFICATION DIVISION.
        PROGRAM-ID.  TOTAL5.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
                SELECT COURSE-FILE ASSIGN TO DA-S-COURSE.
                SELECT PRINT-FILE ASSIGN TO UR-S-PRINT.
       DATA DIVISION.
       FILE SECTION.
       FD  COURSE-FILE
               RECORDING MODE IS F
               LABEL RECORDS ARE STANDARD.
        01  EMP-REC                      PIC X(80).
        FD  PRINT-FILE
                RECORDING MODE IS F
                LABEL RECORDS ARE STANDARD.
        01  PRINT-REC                    PIC X(132).
       WORKING-STORAGE SECTION.
       01  MISC.
                  03  EOF                      PIC X         VALUE 'N'.
                      88  END-OF-DATA                        VALUE 'Y'.
                  03  LINE-CT                  PIC 99        VALUE 0.
                  03 TOT-CLASS-LIM             PIC 999       VALUE 0.
                  03 TOT-OPEN-SEAT             PIC 999       VALUE 0.
                  03 TOT-TAKEN-SEAT            PIC 999       VALUE 0.

       01  COURSE-DATA.
                  03  C-COURSE.
                     05  C-ABB                PIC XXX.
                     05  C-NUMB               PIC XXXX.
                     05  C-SEC                PIC XXX.
            03  C-TITLE                  PIC X(20).
            03  C-SEATS-REMAINING        PIC S999.
            03  C-CLASSLIMIT             PIC 999.
            03  C-TAKEN                      PIC 999.
            03  FILLER                   PIC XXX.
            03  C-STARTING-TIME.
                 05  C-STARTING-HOUR      PIC 99.
                 05  C-STARTING-MIN       PIC 99.
            03  FILLER                   PIC XX.
            03  C-DAYS                   PIC X(6).
            03  C-LOCATION.
                 05  C-BUILDING           PIC XX.
                 05  C-ROOM               PIC XXX.
            03  FILLER                   PIC X(24).
      *****************************************************************
      *       *** DESCRIPTION OF HEADING PRINT LINES **             ***
      *****************************************************************
       01 MAIN-HEADING.
            03  FILLER                       PIC X(20)     VALUE SPACES.
            03  FILLER    PIC X(27) VALUE 'EASTERN ILLINOIS UNIVERSITY'.
            03  FILLER                          PIC X(78)    VALUE SPACES.
            03  FILLER          PIC X(7)  VALUE 'PAGE  1'.

       01 SECONDARY-HEADING.
            03 FILLER                        PIC X(27)   VALUE SPACES.
            03 FILLER    PIC X(14)              VALUE 'COURSE LISTING'.
            03 FILLER                           PIC X(91)        VALUE SPACES.

       01  HEADING1.
           03  FILLER                   PIC X(10)     VALUE SPACES.
           03  FILLER                   PIC X(5)      VALUE 'CLASS'.
           03  FILLER                   PIC X(11)     VALUE SPACES.
           03  FILLER                   PIC X(8)      VALUE 'LOCATION'.
           03  FILLER                   PIC X(8)      VALUE SPACES.
           03  FILLER                   PIC X(4)      VALUE 'DAYS'.
           03  FILLER                   PIC X(11)     VALUE SPACES.
           03  FILLER                   PIC X(4)      VALUE 'TIME'.
           03  FILLER                   PIC X(10)     VALUE SPACES.
           03  FILLER                   PIC X(5)      VALUE 'CLASS'.
           03  FILLER                   PIC X(7)      VALUE SPACES.
           03  FILLER                   PIC XXXX      VALUE 'OPEN'.
           03  FILLER                   PIC X(7)      VALUE SPACES.
           03  FILLER                   PIC X(5)      VALUE 'TAKEN'.


       01  HEADING2.
               03  FILLER                   PIC X(71)     VALUE SPACES.
               03  FILLER                   PIC X(5)      VALUE 'LIMIT'.
               03  FILLER                   PIC X(7)      VALUE SPACES.
               03  FILLER                   PIC X(5)      VALUE 'SEATS'.

       01  FOOTER.
                03  FILLER                   PIC X(10)     VALUE SPACES.
                03  FILLER     PIC X(11)           VALUE 'GRAND TOTAL'.
                03  FILLER                   PIC X(61)     VALUE SPACES.
                03  FILLER                       PIC 999       VALUE TOT-CLASS-LIM.
                03  FILLER                   PIC X(10)     VALUE SPACES.
                03  FILLER                       PIC 999           VALUE TOT-OPEN-SEAT.
                03  FILLER                   PIC X(10)     VALUE SPACES.
                03 FILLER                        PIC 999           VALUE TOT-TAKEN-SEAT.

      *****************************************************************
      *          DESCRIPTION OF PRINT DATA LAYOUT                   ***
      *****************************************************************
       01  PRINT-DATA.
               03  FILLER                   PIC X(10)     VALUE SPACES.
               03  PABB                     PIC XXX.
               03  FILLER                   PIC X         VALUE SPACES.
               03  PNUMB                    PIC XXXX.
               03  FILLER                   PIC X         VALUE SPACES.
               03  PSEC                     PIC XXX.
               03  FILLER                   PIC X(5)      VALUE SPACES.
               03  PBUILDING                PIC XX.
               03  FILLER                   PIC X         VALUE SPACES.
               03  PROOM                    PIC XXX.
               03  FILLER                   PIC X(9)      VALUE SPACES.
               03  PDAYS                    PIC X(6).
               03  FILLER                   PIC X(10)     VALUE SPACES.
               03  PSTARTING-HOUR           PIC Z9.
               03  FILLER                   PIC X         VALUE ':'.
               03  PSTARTING-MIN            PIC 99.
               03  FILLER                   PIC X(9)      VALUE SPACES.
               03  PCLASSLIMIT              PIC ZZ9.
               03  FILLER                   PIC X(8)      VALUE SPACES.
               03  PSEATS-REMAINING         PIC ZZ9-.
               03  FILLER                   PIC X(8)      VALUE SPACES.
               03  PSEATS-TAKEN             PIC ZZ9.

       PROCEDURE DIVISION.
			000-MAINLINE.
               OPEN INPUT COURSE-FILE
                    OUTPUT PRINT-FILE.
               PERFORM 800-READ-COURSE-FILE.
               PERFORM 225-COURSE-HEADINGS.
               PERFORM 100-PROCESS-LOOP
                   UNTIL END-OF-DATA.
                    WRITE PRINT-REC FROM FOOTER
                          AFTER ADVANCING 2.

               CLOSE COURSE-FILE
                     PRINT-FILE.
               STOP RUN.
      ***************************************************************
      *           PRINT EACH CLASS                                ***
      ***************************************************************
       100-PROCESS-LOOP.
               IF LINE-CT > 45
                  THEN
                  PERFORM 225-COURSE-HEADINGS.
               MOVE C-ABB                   TO        PABB.
               MOVE C-NUMB                  TO        PNUMB.
               MOVE C-SEC                   TO        PSEC.
               MOVE C-BUILDING              TO        PBUILDING.
               MOVE C-ROOM                  TO        PROOM.
               MOVE C-DAYS                  TO        PDAYS.
               MOVE C-STARTING-HOUR         TO        PSTARTING-HOUR.
               MOVE C-STARTING-MIN          TO        PSTARTING-MIN.
               MOVE C-SEATS-REMAINING       TO        PSEATS-REMAINING.
               MOVE C-CLASSLIMIT            TO        PCLASSLIMIT.

                SUBTRACT C-SEATS-REMAINING FROM C-CLASSLIMIT
                        GIVING C-TAKEN.
                ADD C-CLASSLIMIT TO TOT-CLASS-LIM
                        GIVING TOT-CLASS-LIM.
                ADD C-SEATS-REMAINING TO TOT-OPEN-SEAT
                        GIVING TOT-OPEN-SEAT.
                ADD C-TAKEN TO TOT-TAKEN-SEAT
                                GIVING TOT-TAKEN-SEAT.

                MOVE C-TAKEN                 TO   PSEATS-TAKEN.
                SUBTRACT C-TAKEN FROM C-TAKEN
                        GIVING C-TAKEN.

               INSPECT PDAYS REPLACING ALL ' ' BY '-'.

               WRITE PRINT-REC FROM PRINT-DATA
                     AFTER ADVANCING 1 LINE.
               ADD 1 TO LINE CT.
               PERFORM 800-READ-COURSE-FILE.
      *****************************************************************
      *           PRINTS HEADING LINE                               ***
      *****************************************************************
       225-COURSE-HEADINGS.
          WRITE PRINT-REC FROM MAIN-HEADING
           AFTER ADVANCING PAGE.
          WRITE PRINT-REC FROM SECONDARY-HEADING
           AFTER ADVANCING 1.
          WRITE PRINT-REC FROM HEADING1
           AFTER ADVANCING 1.
		  WRITE PRINT-REC FROM HEADING2
           AFTER ADVANCING 1.
          MOVE SPACES TO PRINT-REC.
          WRITE PRINT-REC
           AFTER ADVANCING 1.
          MOVE 0 TO LINE-CT.
      *****************************************************************
      **          READS THE DATA FILE                               ***
      ****************************************************************
       800-READ-COURSE-FILE.
               READ COURSE-FILE INTO COURSE-DATA
                    AT END MOVE 'Y' TO EOF.
