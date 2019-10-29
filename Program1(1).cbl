       program-id. Program1 as "PROJECT1".
       AUTHOR. Jiazhao Zhang
       
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STU-FILE
               ASSIGN TO "C:\COBOL\STUDENTFILE.TXT"
                   ORGANIZATION IS SEQUENTIAL.

       data division.
       FILE SECTION.
       FD STU-FILE.
       01  STU-RECORD-OUT  PIC X(16).
       
       working-storage section.
       01  STU-RECORD.
           05  STU-NUMBER      PIC 9(9).
           05  PROGRAM-CODE    PIC A(3).
           05  REG-DATE        PIC 9(4).
           
       01  FLAGS.
           05  REC-INPUT   PIC X(1)    VALUE   "Y".
           
       01  PROMPTS.
           05  RECORD-PROMPT   PIC X(22)
                   VALUE   "RECORD TO ENTER (Y/N):".
           05  STU-NUMBER-PROMPT   PIC X(13)
                   VALUE   "ENTER NUMBER:".
           05  PROGRAM-PROMPT  PIC X(14)
                   VALUE   "ENTER PROGRAM:".
           05  DATE-PROMPT     PIC X(11)
                   VALUE   "ENTER DATE:".

       procedure division.
       

       *>Main module for the project.
       100-CREATE-STU-FILE.
           PERFORM 201-INIT-CREATE-STU-FILE.
           PERFORM 202-CREATE-STU-REC
               UNTIL REC-INPUT = "N" OR "n".
           PERFORM 203-TERM-CREATE-STU-FILE.
           STOP RUN.
       
       *>Initialization:
       *>Open output file and prompt for user input.
       201-INIT-CREATE-STU-FILE.
               PERFORM 301-OPEN-STU-FILE.
               PERFORM 302-PRMOPT-FOR-RECORD.
           
       *>Creation module:
       *>Prompt for user input and write student record into output file
       202-CREATE-STU-REC.
           PERFORM 303-ENTER-STUDENT-DATA.
           PERFORM 304-WRITE-STU-REC-OUT.
           PERFORM 302-PRMOPT-FOR-RECORD.
           
       *>Termination module:
       *>Close output file and display a termination message
       203-TERM-CREATE-STU-FILE.
           PERFORM 305-CLOSE-STU-FILE.
           PERFORM 306-DISPLAY-TERM-MSG.
           
       *>Open the studeng file
       301-OPEN-STU-FILE.
           OPEN OUTPUT STU-FILE.
           
       *>Prompt for user input
       302-PRMOPT-FOR-RECORD.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY RECORD-PROMPT   COLUMN  2   LINE    12.
           ACCEPT  REC-INPUT   COLUMN  2   LINE    13.
           
       *>Enter detailed information for each student record.
       303-ENTER-STUDENT-DATA.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY STU-NUMBER-PROMPT   COLUMN  5   LINE    2.
           ACCEPT  STU-NUMBER          COLUMN  5   LINE    3.
           DISPLAY PROGRAM-PROMPT      COLUMN  5   LINE    4.
           ACCEPT  PROGRAM-CODE        COLUMN  5   LINE    5.
           DISPLAY DATE-PROMPT         COLUMN  5   LINE    6.
           ACCEPT  REG-DATE            COLUMN  5   LINE    7.
           
       *>Write a student record to the output file.
       304-WRITE-STU-REC-OUT.
           MOVE    STU-RECORD  TO STU-RECORD-OUT.
           WRITE   STU-RECORD-OUT.
           
       *>Close the output file.
       305-CLOSE-STU-FILE.
           CLOSE   STU-FILE.
       
       *>Display the termination message.
       306-DISPLAY-TERM-MSG.
           DISPLAY " " WITH BLANK SCREEN.
           DISPLAY "Processing student record finished."
           goback.

       end program Program1.
