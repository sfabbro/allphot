      SUBROUTINE  SORTER (SORT, INDEX, LOW1, HIGH1, 
     .     LOW2, HIGH2, TEXT, MAXSTR, WATCH)
C
C=======================================================================
C
C Subroutine to read in any of the data files created by DAOPHOT and
C sort the stars according to magnitude, position, ID number,
C or OTHER.
C
C              OFFICIAL DAO VERSION:  2001 February 7
C
C Argument
C
C  WATCH (INPUT) governs whether information relating to the progress 
C        of the reductions is to be typed on the terminal screen
C        during execution.
C
C WATCH is a user-definable optional parameter.
C
C=======================================================================
C
      IMPLICIT NONE
C
C Parameters
C
C    MAX is the number of different sorts which are possible
C        (currently 5: by ID number, by X, by Y, by magnitude, and
C         by OTHER [e.g. SHARP, ROUND, CHI, number of iterations]).
C
C MAXSTR is the maximum number of stars permitted in a data file.
C        It is limited by the size of the WORK array in DAOPHOT:
C        WORK must contain 296 bytes per star:  4 bytes each for SORT,
C        INDEX, LOW1, HIGH1, LOW2, HIGH2, and 272 bytes for two lines
C        of 136 characters in in TEXT.
C
C MAXITM is the maximum number of output data per line of an output.
C        (currently 15, realized in PHOTOMETRY)
C
      INTEGER MAXSTR, MAXITM
      PARAMETER (MAXITM=30)
C
      CHARACTER SWITCH*40, COMMAS*40, RNDOFF*9
      REAL DATUM(MAXITM), SORT(*)
      INTEGER INDEX(*), LOW1(*), HIGH1(*), LOW2(*), HIGH2(*)
      BYTE TEXT(*)
C
      INTEGER NINT
C
      CHARACTER INPUT*136, OUTPUT*136, FILE*40, STRNG*9, CASE*4, 
     .     ANSWER*1
      REAL LOBAD, HIBAD, THRESH, AP1, PHPADU, READNS, FRAD
      REAL WATCH, WHICH, FLIP
      INTEGER I, J, K, N, NL, NCOL, NROW, ISTAT, ITEMS, NSTAR
      INTEGER NHI, MODE, MIN0
      LOGICAL NON
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get ready.
C
C Find out how the user wants to sort.
C
      WRITE (6,610) MAXITM
  610 FORMAT (//
     .     11X, '   The following sorts are currently possible:'//
     .     11X, '+/- 1  By increasing/decreasing star ID number'//
     .     11X, '+/- 2  By increasing/decreasing  X  coordinate'//
     .     11X, '+/- 3  By increasing/decreasing  Y  coordinate'//
     .     11X, '+/- 4  By increasing/decreasing magnitude'//
     .     11X, '+/- n  By increasing/decreasing OTHER (n <= ',
     .     I2, ')'///)
      CALL GETDAT ('Which do you want?', WHICH, 1)
      IF (WHICH .LT. -1.E15) RETURN
C
      MODE=NINT(WHICH)
      IF (((IABS(MODE) .LT. 1) .OR. (IABS(MODE) .GT. MAXITM))
     .     .AND. (MODE .NE. 99)) RETURN
C
      FLIP=FLOAT(MODE/IABS(MODE))
      MODE=IABS(MODE)
C
C Get input file name, open the file, and read its header.
C
      FILE=' '
  950 CALL GETNAM ('Input file name:', FILE)
      IF ((FILE .EQ. 'END-OF-FILE') .OR. (FILE .EQ. 'GIVE UP')) RETURN
      CALL INFILE (2, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//FILE)
         FILE = 'GIVE UP'
         GO TO 950
      END IF
C
C Get output file name and open the file.
C
      FILE = SWITCH(FILE, CASE('.srt'))
  960 CALL GETNAM ('Output file name:', FILE)
      IF ((FILE .EQ. 'END-OF-FILE') .OR. (FILE .EQ. 'GIVE UP')) THEN
         CALL CLFILE (2)
         RETURN
      END IF
C
      CALL OUTFIL (3, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//FILE)
         FILE = 'GIVE UP'
         GO TO 960
      END IF
C
      CALL GETYN ('Do you want the stars renumbered?', ANSWER)
      IF (ANSWER .EQ. 'E') THEN
         CALL CLFILE (2)
      END IF
C
      NL=-1
      CALL RDHEAD (2, NL, NCOL, NROW, LOBAD, HIBAD, THRESH, AP1, 
     .     PHPADU, READNS, FRAD)
      IF (NL .LE. 0) GO TO 1010
      IF (NL .GT. 3) NL=1
C
C Copy input file header to output file.  
C
      ITEMS=6
      IF (FRAD .GT. 0.001) ITEMS=7
      IF (FILE .NE. 'APPEND') CALL WRHEAD (3, NL, NCOL, NROW, 
     .     ITEMS, LOBAD, HIBAD, THRESH, AP1, PHPADU, READNS, FRAD)
C
 1010 CONTINUE
      IF (WATCH .LT. 0.5) GO TO 1020
      IF (MODE .LE. 4) WRITE (6,611)
  611 FORMAT (/24X, 'STAR', 5X, 'X', 8X, 'Y', 6X, 'MAG(1)')
      IF (MODE .GE. 5) WRITE (6,612) MODE
  612 FORMAT (/19X, 'STAR', 5X, 'X', 8X, 'Y', 6X, 'MAG(1)',
     .     2X, 'ITEM', I3)
 1020 CONTINUE
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Read the input file in line by line, verbatim.  Pack the contents
C of the line into the REAL array WORK, keeping track of the
C lower and upper limits in the arrays LOW1 and HIGH1.  At the
C same time, extract the the particular datum 
C according to which we wish to sort.  Sort these data.  Then write 
C the file out again, line by line, verbatim, but in the new order.
C
C The sequential star number will be stored in I.
C The total number of characters read in will be stored in NHI.  This
C will eventually be used to define to pointers to the beginning and
C end of each line.
C
      I = 0
      NHI = 0
C
 2000 CALL RDCHAR (2, INPUT, N, ISTAT)
      IF (ISTAT .EQ. 1) GO TO 2100
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Unable to read input file.')
         CALL CLFILE (2)
         RETURN
      END IF
C
      IF (N .LE. 1) GO TO 2000
      READ (INPUT(1:N),*,IOSTAT=ISTAT) (DATUM(J), J=1,MIN0(15,MODE))
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Unable to read data from input file.')
         WRITE (6,*) INPUT(1:N)
         CALL CLFILE (2)
         RETURN
      END IF
C
      I = I+1
      IF (I .GT. MAXSTR) THEN
         FILE = COMMAS(I-1, K)
         WRITE (OUTPUT,77) FILE(1:K), 
     .        ' stars is all I have room for.  Sorry!'
   77    FORMAT (2A)
         CALL STUPID (OUTPUT(1:50))
         CALL CLFILE (2)
         RETURN
      END IF
C
      LOW1(I) = NHI+1
      DO J=1,N
         NHI = NHI+1
         TEXT(NHI) = ICHAR(INPUT(J:J))
      END DO
      HIGH1(I) = NHI
C
      IF (NL .EQ. 2) THEN
 2012    CALL RDCHAR (2, INPUT, N, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Unable to read data from input file.')
            WRITE (6,*) INPUT(1:N)
            CALL CLFILE (2)
            RETURN
         END IF
C
         IF (N .LE. 1) GO TO 2012
         IF (MODE .GT. 15) THEN
            READ (INPUT(1:N), 902, IOSTAT=ISTAT)
     .        (DATUM(J), J=16,MODE)
  902       FORMAT (F14.3, 2F6.2, f8.4, 11f9.4)
            IF (ISTAT .NE. 0) THEN
               CALL STUPID ('Unable to read data from input file.')
               CALL CLFILE (2)
               RETURN
            END IF
         END IF
C
         LOW2(I) = NHI+1
         DO J=1,N
            NHI = NHI+1
            TEXT(NHI) = ICHAR(INPUT(J:J))
         END DO
         HIGH2(I) = NHI
      END IF
C
      SORT(I)=FLIP*DATUM(MODE)
      GO TO 2000
C
C Perform the sort.
C
 2100 NSTAR = I
      CALL CLFILE (2)
      IF (NSTAR .LE. 0) THEN
         CALL STUPID ('No stars in input file.')
         RETURN
      END IF
      CALL QUICK (SORT, NSTAR, INDEX)
C
C The vector SORT is now arranged in order of increasing or decreasing
C whatever, and the vector INDEX now contains the ordinal position in
C the input file of the stars, in order of increasing or decreasing
C whatever.
C
C Now write the data out again.
C
      DO 2110 I=1,NSTAR
         J = INDEX(I)
         N = 0
         DO K=LOW1(J),HIGH1(J)
            N = N+1
            OUTPUT(N:N) = CHAR(TEXT(K))
         END DO
C
C Find the end of the star ID in the line (just in case line
C is not in standard format).
C
            NON = .FALSE.
            DO K=1,N
               IF (OUTPUT(K:K) .EQ. ' ') THEN
                  IF (NON) GO TO 2105
               ELSE
                  NON = .TRUE.
               END IF
            END DO
            CALL STUPID ('No blanks on data line.')
            WRITE (6,*) OUTPUT(1:N)
            CALL CLFILE (2)
            RETURN
C
 2105       CONTINUE
C
C K now points at the first blank character after the star ID.
C
C If ID numbers are to be changed, insert the new ID into the text.
C If not, and if the first blank is in column 7, add a blank at the
C beginning of the line to bring into compliance with the new format
C
         IF (ANSWER .EQ. 'Y') THEN
            WRITE (INPUT,68) I, OUTPUT(K:N)
   68       FORMAT (I7, A)
            N = 8+N-K
            OUTPUT = INPUT
         ELSE IF (K .EQ. 7) THEN
            OUTPUT = ' '//OUTPUT(1:N)
            N = N+1
         END IF
         IF (NL .EQ. 2) WRITE (3,69) ' '
         WRITE (3,69) OUTPUT(1:N)
   69    FORMAT (A)
C
         IF (WATCH .GE. 0.5) THEN
            IF (MODE .LE. 4) THEN
               WRITE (6,620) OUTPUT(1:34)
  620          FORMAT (21X, A34)
            ELSE
               STRNG = RNDOFF (FLIP*SORT(I), 9, 3)
               WRITE (6,621) OUTPUT(1:34), STRNG
  621          FORMAT (16X, A34, A9)
            END IF
         END IF
C
         IF (NL .EQ. 2) THEN
            N = 0
            DO K=LOW2(J),HIGH2(J)
               N = N+1
               OUTPUT(N:N) = CHAR(TEXT(K))
            END DO
            WRITE (3,69) OUTPUT(1:N)
         END IF
 2110 END DO
C
C-----------------------------------------------------------------------
C
C Normal return.
C
      CALL CLFILE (3)
      RETURN
C
      END
