       program-id. PROJECT3.
       AUTHOR. JIAZHAO ZHANG.
      
      * ==========================================================
      * This program reads in a record from the STUDENT FILE£¬and load 
      * table from CRS-NAMES file.
      * For each record read, the program calculates the 
      * student average, and then search the course name table to 
      * find every course name by course code.The student record is then
      * written to a report file.
      * Audit counters are kept for records read and detail
      *  records written. These are displayed at the end of the job
      *
      * ========================================================== 
       environment division.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE-IN
               ASSIGN TO "C:\COBOL\STUFILE3.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CRS-NAME-IN
               ASSIGN TO "C:\COBOL\CRSNAMES.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STURPT-FILE-OUT
               ASSIGN TO "C:\COBOL\STURPT.TXT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       data division.
       FILE SECTION.
      *==========================================================
      * STUDENT-FILE-IN is the input file.
      * The output record, STUDENT-REPORT-CARD, will be populated with
      * the  data collected/calculated in the Working Storage record
      * STUDENT-REPORT-WS, COLUMN-HEADER during initializatio, 
      *  and COUNTERS during termination.                               
      * ========================================================= 
       FD STUDENT-FILE-IN.
       01  STUDENT-RECORD-IN.
           05  STUDENT-FNAME-IN     PIC X(20).
           05  STUDENT-LNAME-IN     PIC X(20).
           05  STUDENT-NUMBER-IN    PIC 9(9).
           05  COURSE1-CODE-IN      PIC X(7).
           05  COURSE1-AVERAGE-IN   PIC 9(3).
           05  COURSE2-CODE-IN      PIC X(7).
           05  COURSE2-AVERAGE-IN   PIC 9(3).
           05  COURSE3-CODE-IN      PIC X(7).
           05  COURSE3-AVERAGE-IN   PIC 9(3).
           05  COURSE4-CODE-IN      PIC X(7).
           05  COURSE4-AVERAGE-IN   PIC 9(3).
           05  TUITION-OWED-IN      PIC 9(4)V99.
          
            
       FD CRS-NAME-IN.
           01  COURSE-CODE-IN      PIC X(7).
           01  COURSE-NAME-IN      PIC X(15).                           
          
       FD  STURPT-FILE-OUT.
       01  STUDENT-AVER-RECORD-OUT PIC X(73).

       working-storage section.
       01  FIRST-LINE-RECORD.
           05  FILLER          PIC X(2)    VALUE   SPACES.
           05  STUDENT-NUMBER    PIC 9(9).
           05  FILLER          PIC X(2)    VALUE   SPACES.
           05  STUDENT-LNAME     PIC X(20).
           05  FILLER          PIC X(2)    VALUE   SPACES.           
           05  STUDENT-FNAME     PIC X(20).
           05  FILLER          PIC X(2)    VALUE   SPACES.
           05  OVERALL-AVERAGE PIC 9(3).
           05  FILLER          PIC X(2)    VALUE   SPACES.  
           05  TUITION-OWED      PIC 9(4)V99.
       01  COURSE-HEADER.
           05  FILLER          PIC X(2)    VALUE   SPACES.
           05  FILLER          PIC X(11)   VALUE   "COURSE CODE".
           05  FILLER          PIC X(2)    VALUE   SPACES.
           05  FILLER          PIC X(11)    VALUE   "COURSE NAME".
           05  FILLER          PIC X(4)    VALUE   SPACES.
           05  FILLER          PIC X(7)    VALUE   "AVERAGE".

           
       01  COURSE-RECORD.
           05  FILLER          PIC X(2)    VALUE   SPACES.
           05  COURSE-CODE-OUT     PIC X(7).
           05  FILLER          PIC X(6)    VALUE   SPACES.
           05  COURSE-NAME-OUT     PIC X(15).                    
           05  FILLER          PIC X(2)    VALUE   SPACES.
           05  COURSE-AVER-OUT     PIC 9(3).
           
       01  FLAGS.
           05  STU-EOF-FLAG    PIC X(1)    VALUE   "N".
           05  FOUND       PIC X(1)     VALUE   "N".
           
       01  COUNTERS.
           05  FILLER          PIC X(14)   VALUE "RECORDS READ  ".
           05  RECORDS-IN-CTR  PIC 9(3)    VALUE ZERO.
           05  FILLER          PIC X(19)   VALUE "RECORDS WRITTEN  ".
           05  RECORDS-OUT-CTR PIC 9(3)    VALUE  ZERO.
       
       01 OTHER-VAR.
           05  SUB PIC 9(1)    VALUE 1.
           05  SUB1 PIC 9(1)    VALUE 1.
           05  COURSE-TABLE    OCCURS 5 TIMES.
               10  COURSE-CODE      PIC X(7).    
               10  COURSE-NAME      PIC X(15).
               
           05  STU-COURSE-TABLE    OCCURS 4 TIMES.
               10  STU-COURSE-CODE-T      PIC X(7). 
               10  STU-COURSE-AVER-T      PIC 9(3).
              
       procedure division.
       

       *>Main module for the project.
       100-CREATE-STUAVER-FILE.
           PERFORM 201-INIT-CREATE-STUAVER-FILE.
           PERFORM 202-CREATE-STUAVER-CARD
               UNTIL STU-EOF-FLAG = "Y" OR "y".
           PERFORM 203-TERM-CREATE-STUAVER-FILE.
           STOP RUN.
       
       *>Initialization:
       *>Open input file and read input record, write record out header 
       201-INIT-CREATE-STUAVER-FILE.
               PERFORM 701-OPEN-FILES.
               PERFORM 702-LOAD-CRS-NAMES
               VARYING SUB FROM 1 BY 1 UNTIL SUB > 5.
               PERFORM 703-READ-STU-RECORD.
           
       *>Creation module:
       *>Compute student average mark and write to output file, repeat 
       *>until finish the last record.
       202-CREATE-STUAVER-CARD.
           PERFORM 704-PROCESS-STUAVER.
           PERFORM 705-WRITE-FIRSTLINE-OUT.
           PERFORM 706-PROCESS-STUDENT-CRS
           VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 4.
            ADD 1 TO RECORDS-OUT-CTR.
           PERFORM 703-READ-STU-RECORD.
           
       *>Termination module:
       *>Write audit trail and close inuput and output file.
       203-TERM-CREATE-STUAVER-FILE.
           PERFORM 707-DISPLAY-AUDIT-COUNTERS.
           PERFORM 708-CLOSE-FILES.
           
       *>Open the input and output file
       701-OPEN-FILES.
           OPEN INPUT STUDENT-FILE-IN
                INPUT CRS-NAME-IN
                OUTPUT STURPT-FILE-OUT.

       *>Write output file header
       702-LOAD-CRS-NAMES.
           READ CRS-NAME-IN
           AT END MOVE "Y" TO STU-EOF-FLAG.
           MOVE  COURSE-CODE-IN TO COURSE-CODE(SUB).
           MOVE  COURSE-NAME-IN TO COURSE-NAME(SUB).
           
       *>Read student record from the input file, add counter everytime
       703-READ-STU-RECORD.
           READ STUDENT-FILE-IN
           AT END MOVE "Y" TO STU-EOF-FLAG
           NOT AT END ADD 1 TO RECORDS-IN-CTR.
           MOVE COURSE1-CODE-IN TO STU-COURSE-CODE-T(1).
           MOVE COURSE2-CODE-IN TO STU-COURSE-CODE-T(2).
           MOVE COURSE3-CODE-IN TO STU-COURSE-CODE-T(3).
           MOVE COURSE4-CODE-IN TO STU-COURSE-CODE-T(4). 
           MOVE COURSE1-AVERAGE-IN TO STU-COURSE-AVER-T(1).
           MOVE COURSE2-AVERAGE-IN TO STU-COURSE-AVER-T(2).
           MOVE COURSE3-AVERAGE-IN TO STU-COURSE-AVER-T(3).
           MOVE COURSE4-AVERAGE-IN TO STU-COURSE-AVER-T(4).          
        
       *>Compute every student's average mark
       704-PROCESS-STUAVER.
           COMPUTE OVERALL-AVERAGE = (COURSE1-AVERAGE-IN+
           COURSE2-AVERAGE-IN+COURSE3-AVERAGE-IN+COURSE4-AVERAGE-IN)/4.
           MOVE STUDENT-NUMBER-IN TO  STUDENT-NUMBER.
           MOVE STUDENT-LNAME-IN TO  STUDENT-LNAME.
           MOVE STUDENT-FNAME-IN TO  STUDENT-FNAME.
           MOVE  TUITION-OWED-IN TO TUITION-OWED.
       
      *Write output record to output file
       705-WRITE-FIRSTLINE-OUT.
           WRITE STUDENT-AVER-RECORD-OUT FROM FIRST-LINE-RECORD
           AFTER ADVANCING PAGE.
           WRITE STUDENT-AVER-RECORD-OUT FROM COURSE-HEADER
           AFTER 2 LINES.
           
       706-PROCESS-STUDENT-CRS.
            
           MOVE STU-COURSE-CODE-T(SUB1) TO COURSE-CODE-OUT.
           MOVE STU-COURSE-AVER-T(SUB1) TO COURSE-AVER-OUT.             
           PERFORM 801-SEARCH-COURSE-TABLE
           VARYING SUB FROM 1 BY 1 UNTIL FOUND ="Y" OR SUB > 5.
           IF FOUND = "Y"
               MOVE COURSE-NAME(SUB) TO COURSE-NAME-OUT 
           ELSE MOVE SPACE TO COURSE-NAME-OUT 
           END-IF.
           WRITE STUDENT-AVER-RECORD-OUT FROM COURSE-RECORD.
           
           
       801-SEARCH-COURSE-TABLE.
           IF COURSE-CODE-OUT = COURSE-CODE(SUB)
               MOVE "Y" TO FOUND
           END-IF.
       
      *Write audit trail to check every record processed
       707-DISPLAY-AUDIT-COUNTERS.
            DISPLAY COUNTERS.      
       
      *Close all the files to finish this function.
       708-CLOSE-FILES.
           CLOSE STUDENT-FILE-IN CRS-NAME-IN STURPT-FILE-OUT.
 

       end program PROJECT3.

