C
C**********************************************************************
C
C                              MASTER.FOR
C
C A program for intercomparing star lists obtained from different CCD
C frames to cross-identify stars.  General linear functions of (x,y)
C are used to transform from one set of coordinates to another.  
C Eventual inclusion of color and magnitude terms is possible.  Input 
C is expected in the form of DAOPHOT ASCII data files and output in a 
C variety of forms is available, at the user's discretion.
C
C                   Official DAO version:  2005 March 7
C
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER MAXDAT, MAXFRM, MAXMTR, MAXCON, maxwhi
      PARAMETER (MAXCON=20)
      PARAMETER (MAXDAT=80 000 000, MAXMTR=1 000 000, MAXFRM=1000)
      PARAMETER (MAXWHI=150 000 000)
C
      CHARACTER FILE(MAXFRM)*40, EXTEND*40, SWITCH*40
      CHARACTER COMMAS*20
      CHARACTER CASE*3
      DOUBLE PRECISION CON(MAXCON,MAXFRM)
      REAL RWORK(MAXDAT), WHICH(MAXWHI), WHCH2(MAXWHI)
      REAL RCOL(MAXFRM), RROW(MAXFRM)
      INTEGER IDMAST(MAXMTR), LASTAR(0:MAXFRM)
      INTEGER IWORK(MAXDAT), IWHICH(MAXWHI), IWHCH2(MAXWHI)
      BYTE BWORK(4*MAXDAT)
C
      CHARACTER TEXT*310
      CHARACTER TEXT2*20, text3*20
      CHARACTER LFILE*40
      REAL DUM, CHIMAX, SIGMAX, MAGMAX, SHPMIN, SHPMAX, RX, RY, 
     .     PWT1, STEP
      INTEGER I, J, N, IFRM, MAXSTR, MAX, NFRM, NTOT, ISTAT
      LOGICAL TFR, DUP, GRIPE
C
C The big scratch array ?WORK, must be capable of holding data of type
C REAL*4, INTEGER*4, or BYTE*1.
C
      EQUIVALENCE (RWORK, IWORK), (RWORK, BWORK), (WHICH, IWHICH)
      EQUIVALENCE (WHCH2, IWHCH2)
C
      DATA  DUP/.TRUE./, PWT1/200./, STEP/3./
      DATA IWHICH /MAXWHI*0/, IWHCH2 /MAXWHI*0/
      DATA GRIPE /.TRUE./
C
C=======================================================================
C
      CALL FABORT
      CALL STUPID ('     MONGO version')
      DO I=1,MAXFRM
         DO J=1,MAXCON
            CON(J,I) = 0.D0
         END DO
         CON(3,I) = 1.D0
         CON(6,I) = 1.D0
      END DO
C
C Open the file containing the names of the star lists, get the names
C and count the number of input files, and get the estimates of the
C transformation constants.
C
      CALL TBLANK
      CHIMAX = 1.E20
      MAGMAX = 1.E20
      SHPMIN = -1.E20
      SHPMAX = 1.E20
      SIGMAX = 1.E20
      LFILE = ' '
  890 CALL GETNAM ('File with list of input files:', LFILE)
      IF ((LFILE .EQ. 'EXIT') .OR. (LFILE .EQ. 'END-OF-FILE')) 
     .     CALL BYEBYE
      RX = 1.
      RY = 1.
      TFR = .TRUE.
      DO I=30,2,-1
         IF ((LFILE(I:I).EQ.'%') .OR. (LFILE(I:I).EQ.'|').OR.
     .        (LFILE(I:I).EQ.'/') .OR. (LFILE(I:I).EQ.'!').OR.
     .        (LFILE(I:I).EQ.'>') .OR. (LFILE(I:I).EQ.'<').OR.
     .        (LFILE(I:I).EQ.'[').OR.(LFILE(I:I).EQ.'\\')) THEN
            IF (LFILE(I:I) .EQ. '%') THEN
               READ (LFILE(I+1:30),*) CHIMAX
               TFR = .FALSE.
            ELSE IF (LFILE(I:I) .EQ. '|') THEN
               READ (LFILE(I+1:30),*) SIGMAX
               TFR = .FALSE.
            ELSE IF (LFILE(I:I) .EQ. '!') THEN
               READ (LFILE(I+1:30),*) MAGMAX
               TFR = .FALSE.
            ELSE IF (LFILE(I:I) .EQ. '>') THEN
               READ (LFILE(I+1:30),*) SHPMIN
               TFR = .FALSE.
            ELSE IF (LFILE(I:I) .EQ. '<') THEN
               READ (LFILE(I+1:30),*) SHPMAX
               TFR = .FALSE.
            ELSE IF (LFILE(I:I) .EQ. '[') THEN
               READ (LFILE(I+1:30),*) SHPMAX
               SHPMIN = -SHPMAX
               TFR = .FALSE.
            ELSE IF (LFILE(I:I) .EQ. '/') THEN
               READ (LFILE(I+1:30),*) PWT1
            ELSE IF (LFILE(I:I) .EQ. '\\') THEN
               READ (LFILE(I+1:30),*) STEP
            END IF
            LFILE = LFILE(1:I-1)
         END IF
      END DO
      IF (TFR) THEN
         CALL TBLANK
      ELSE
         CALL STUPID ('Making .tfr and .mtr files forbidden!')
      END IF
  895 CONTINUE
      LFILE = EXTEND(LFILE, CASE('mch'))
      CALL INFILE (1, LFILE, ISTAT)
      IF (ISTAT .NE. 0) THEN 
         CALL STUPID ('Error opening input file '//LFILE)
         LFILE = 'EXIT'
         GO TO 890
      END IF
      IFRM = 0
  900 IFRM = IFRM+1
      CALL RDCHAR (1, TEXT, N, ISTAT)
      IF (ISTAT .GT. 0) GO TO 950
      IF (IFRM .GT. MAXFRM) THEN
         WRITE (TEXT,6) MAXFRM
    6    FORMAT ('Too many input files.  Maximum allowed is', I5, '.')
         CALL STUPID (TEXT)
         CALL CLFILE (1)
         CALL OOPS
      END IF
      READ (TEXT,*,IOSTAT=ISTAT) FILE(IFRM), (CON(I,IFRM), I=1,6),
     .     DUM, DUM, (CON(I,IFRM), I=7,20)
      IF (ISTAT .NE. 0) THEN
         ISTAT = 0
         READ (TEXT,*,IOSTAT=ISTAT) FILE(IFRM), (CON(I,IFRM), I=1,6),
     .        DUM, DUM, (CON(I,IFRM), I=7,12)
      END IF
      IF (ISTAT .NE. 0) THEN
         ISTAT = 0
         READ (TEXT,*) FILE(IFRM), (CON(I,IFRM), I=1,6)
      END IF
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error reading input from file.')
         WRITE (6,*) TEXT(1:N), N
         LFILE = 'EXIT'
         CALL CLFILE (1)
         GO TO 890
      END IF
C
      CALL INFILE (2, FILE(IFRM), ISTAT)
      IF (ISTAT .NE. 0) THEN
         IF (GRIPE) THEN
            CALL STUPID ('Error opening data file(s):')
            GRIPE = .FALSE.
         END IF
         WRITE (6,*) FILE(IFRM)
         GO TO 900
      END IF
      CALL CLFILE (2)
C
      CON(1,IFRM) = RX*CON(1,IFRM)
      CON(2,IFRM) = RY*CON(2,IFRM)
      GO TO 900
C
  950 IF (.NOT. GRIPE) THEN
         CALL CLFILE (1)
         CALL OOPS
      END IF
      NFRM = IFRM-1
C
      DO J=2,NFRM
         DO I=1,J-1
            IF (SWITCH(FILE(J), ' ') .EQ. 
     .          SWITCH(FILE(I), ' ')) THEN
               IF (DUP) CALL STUPID ('  Duplicate input files?')
               DUP = .FALSE.
               WRITE (6,*) FILE(I), FILE(J)
            END IF
         END DO
      END DO
      IF (.NOT. DUP) CALL TBLANK
      TEXT2 = COMMAS(NFRM, I)
      WRITE (6,5) TEXT2(1:I)
    5 FORMAT (2X, A, ' input files'/)
C
C Now read all the input data into the array WORK.  WORK must be able 
C to contain six 4-byte numbers per stellar observation: 
C
C     X, Y  ---  coordinates of the star
C     M, S  ---  the magnitude and its standard error (sigma)
C      CHI  ---  the CHI value from the profile fit
C    SHARP  ---  the SHARP value from the profile fit
C
C Thus, the absolute maximum number of stars there is room for is the 
C size of WORK divided by 6, which I'll call MAXSTR.
C
      MAXSTR = MAXDAT/6
      CALL RDFRMS (FILE, TFR, CHIMAX, SHPMIN, SHPMAX, MAGMAX, SIGMAX, 
     .     NFRM, MAXMTR, RWORK, MAXSTR, IDMAST, LASTAR, RCOL, RROW)
C
C RDFRMS has changed the value of the variable MAXSTR from MAXDAT/6 to 
C the number of stars in the largest single file.  
C
C At this point, we have the following input arrays stored at
C the following locations:
C
C       X(ISTAR)  ---  WORK(1)
C       Y(ISTAR)  ---  WORK(MAXSTR+1)
C       M(ISTAR)  ---  WORK(2*MAXSTR+1)
C       S(ISTAR)  ---  WORK(3*MAXSTR+1)
C     CHI(ISTAR)  ---  WORK(4*MAXSTR+1)
C   SHARP(ISTAR)  ---  WORK(5*MAXSTR+1)
C
C Now we must pack these data as tightly as possible in the upper part 
C of the array WORK, so that we can use the remainder of the array for 
C accumulating and storing the master list.
C
      NTOT = LASTAR(NFRM)
      CALL REPACK (RWORK, MAXDAT/6, NTOT )
C
C If NTOT = LASTAR(NFRM) is the total number of stellar observations in 
C all frames, then the input arrays are now stored at the following 
C locations:
C
C       X(ISTAR)  ---  WORK(1)
C       Y(ISTAR)  ---  WORK(NTOT+1)
C       M(ISTAR)  ---  WORK(2*NTOT+1)
C       S(ISTAR)  ---  WORK(3*NTOT+1)
C     CHI(ISTAR)  ---  WORK(4*NTOT+1)
C   SHARP(ISTAR)  ---  WORK(5*NTOT+1)
C
C This leaves us with a total of MAXDAT-6*NTOT storage locations 
C starting at WORK(6*NTOT+1) to divide up among the following arrays:
C
C   the three arrays XM, YM, and MM, each of dimension MAX, and
C
C EITHER
C
C   NLINE of dimension MAXSTR, and
C   
C   LINE of dimension MAXSTR*32 (to hold 128 characters per star, in the
C        single frame containing the largest number of stars = MAXSTR),
C        plus
C
C OR
C
C   the four arrays SM, RMIN, NOBS, and INDEX, each of dimension MAX;
C
C the former is always larger (because the array LINE will not be needed 
C until after we are completely finished with MM, ..., they may share
C the same storage locations).  (RMIN and NOBS need to exist in both
C INTEGER and REAL formats.)  We are also restricted to having MAX no
C greater than MAXMTR by the existence of array IDMAST.  Hence
C
      MAX = MAXDAT - 6*NTOT
C
C   IWHICH of dimension MAXDAT will become IWHICH of dimension 
C          NFRM*MAX, where MAX is the maximum number of 
C          stars for which there will be room for in the provisional 
C          master list.  We must pretend that IWHICH exists in both
C          INTEGER and REAL formats, but they can both share the same
C          storage.
C
      I = (MAX-33*MAXSTR)/3
      J = MAX/7
      ISTAT = 2*(MAXWHI/(NFRM+1))
      MAX = MIN0 ( MAXMTR, I, J, ISTAT )
c    .            (MAX-33*MAXSTR)/3,
c    .            MAX/7,
c    .            2*MAXWHI/NFRM )
      text = commas(i, n)
      text2 = commas(j, i)
      text3 = commas(istat, j)
      write (6,*) 'LINE limit ', text(1:n),
     .     '  INDEX limit ', text2(1:i),
     .     '  WHICH limit ', text3(1:j)
      call tblank
C MAXMTR is the limit set by the PARAMETER statement and the array
C IDMAST;
C (MAX-33*MAXSTR)/3 is the limit set by XM + YM + MM + NLINE + LINE
C MAX/7 is the limit set by XM + YM + MM + SM + RMIN + NOBS + INDEX
C MAXDAT/NFRM is the limit set by IWHICH.
C
C
C MAX now contains the largest permissible number of stars ever to
C appear in the provisional master list (the same star recurring in
C several frames counts once).  
C
      TEXT = COMMAS(MAX, N)
      WRITE (6,7) TEXT(1:N)
    7    FORMAT (' Maximum permitted number of ',
     .        'stars in the master list: ', A)
C
C Now we can actually run MASTER, now that we can specify where in 
C WORK the storage for each of the arrays is to start.
C
      N = 6*NTOT+1                             ! End of input data + 1
C     NN = N + MAX*NFRM                        ! End of IWHICH storage
C
C SUBROUTINE  MASTER (LFILE, FILE, NFRM, CON, MAX, IDMAST, LASTAR,
C
      CALL MASTER (LFILE, FILE, NFRM, CON, MAX, IDMAST, LASTAR,
     .     RCOL, RROW, PWT1, STEP, TFR,
C
C X, Y, M,
C
     .     RWORK(1), RWORK(NTOT+1), RWORK(2*NTOT+1), 
C
C S, CHI, SHARP,
C
     .     RWORK(3*NTOT+1), RWORK(4*NTOT+1), RWORK(5*NTOT+1),
C
C XM, YM, MM,
C
     .     RWORK(N), RWORK(N+MAX), RWORK(N+2*MAX),
C
C SM, RMIN and its integer alter-ego,
C
     .     RWORK(N+3*MAX), RWORK(N+4*MAX), IWORK(N+4*MAX), 
C
C NOBS and its real alter-ego, and INDEX
C
     .     IWORK(N+5*MAX), RWORK(N+5*MAX), IWORK(N+6*MAX),
C
C NLINE, LINE,
C
     .     IWORK(N+3*MAX), BWORK(4*(N+4*MAX)-3),
C
C IWHICH, and its real alter-ego
C
     .     IWHICH, WHICH, IWHCH2, WHCH2)
C
      CALL BYEBYE
      END!
C
C###############################################################################
C
      SUBROUTINE  MASTER  (LFILE, FILE, NFRM, CON, MAX, IDMAST, LASTAR,
     .     RCOL, RROW, PWT1, STEP, TFR,
     .     X, Y, M, 
     .     S, CHI, SHARP,
     .     XM, YM, MM,
     .     SM, RMIN, IRMIN, 
     .     NOBS, ROBS, INDEX,
     .     NLINE, LINE,
     .     IWHICH, WHICH, IWHCH2, WHCH2)
C
      IMPLICIT NONE
C
C Input data:
C 
C    MAX is the maximum number of stars that may ever appear in the
C        provisional master list (the same star reappearing in several 
C        frames counts as one.)
C
C   NFRM is the number of different data frames (input files).
C
C Parameters:
C
C MAXFRM is the maximum permitted number of frames.
C
C MINY, MAXY are the minimum and maximum y-coordinates (referred to the
C        standard data frame) for which space will be reserved in some
C        auxiliary arrays.  Larger and smaller y-values are permitted
C        by the program, however.
C
C NOTE that NLINE and LINE share storage with MM, SM, ...
C
      INTEGER MAX, MAXFRM, MINY, MAXY, MAXCON
      PARAMETER (MAXFRM=1000, MAXCON=20, MINY=-11 384, MAXY=15 000)
C
C Arrays and functions.
C
      CHARACTER COMMAS*20, RNDOFF*10, DRNDFF*12, CASE*4
      CHARACTER HEAD(3)*81, OUT(20)*12, TEXT*132
      CHARACTER FILE(*)*40, OFILE*40, EXTEND*40, SWITCH*40
      DOUBLE PRECISION C(MAXCON,MAXCON), V(MAXCON), DCON(MAXCON)
      DOUBLE PRECISION  OLD(MAXCON,MAXFRM), CON(MAXCON,*), TERM(MAXCON), 
     .     NOC(MAXCON,MAXFRM)
      DOUBLE PRECISION CLAMP(MAXCON,MAXFRM)
      REAL X(*), Y(*), M(*), S(*), CHI(*), SHARP(*), RCOL(*), RROW(*)
      REAL XM(*), YM(*), MM(*), SM(*), RMIN(*)
      REAL DATUM(2,MAXFRM), DMAG(MAXFRM), FLIP(MAXFRM), PWT(MAXFRM)
      REAL MAG90(MAXFRM), MAG95(MAXFRM), SMAG(MAXFRM)
      REAL WHICH(MAX,*), WHCH2(MAX,*), ROBS(*)
      INTEGER IDMAST(MAX), LASTAR(0:MAXFRM), NOK(MAXFRM)
      INTEGER IWHICH(MAX,*), IWHCH2(MAX,*), NOBS(*), INDEX(*), NLINE(*), 
     .     IRMIN(*)
      INTEGER LAST1(MINY:MAXY), LAST2(MINY:MAXY), NTERM(MAXFRM)
      INTEGER IBNRY, INT, NINT, ICLOSE, LENGTH
      LOGICAL STC(MAXFRM), wfc(maxfrm), pc(maxfrm)
      BYTE LINE(128,*)
C
C Variables and function names.
C
      CHARACTER LFILE*40, POSIT1*9, POSIT2*9, STRNG*9, ANSWER*1
      CHARACTER CH*9, SHP*9
      REAL AMIN1, AMAX1, PCTILE
      REAL XX, YY, RADIUS, RADSQ, SIGMAX, ZZ, A, B, SUM, SUMWT, DX, DY
      REAL WT, SUMCHI, SUMSHP, SADX, SADY, SAD, SIGN, RR, FRAC, SNGL
      REAL DEL, DELP1                                                !sm_
      REAL ROLD, XS, YS, RLIM, RLIMSQ, SCALE, DIAG, 
     .     PWT1, STEP, PWT1I, CCLAMP
      INTEGER I, J, K, L, LL, IFIRST, ISTAR, IMASTR, IFRM, NFRM, NL
      INTEGER IBUMP, III, IFLAG, MODE, NEW, NMASTR, LEGIT, MIDFRM
      INTEGER LIMIT, ISTAT, MINFRM, ENOUGH, NITER, MIN, MINUSE, MITER
      INTEGER NERROR, NFEW, NENUF
      INTEGER NFOLD
      LOGICAL TFR, SHRINK
      LOGICAL PLOT, PAUSE                                            !sm_
      DATA DMAG/MAXFRM*99.999/, NOK/MAXFRM*0/, MIN/8/
      DATA DEL /0.6/                                                 !sm_
C
C=======================================================================
C
      delp1 = del + 1.
      MIDFRM = (NFRM+1)/2
      SHRINK = .FALSE.
      CALL TBLANK
      CALL GETDAT ('Minimum number, minimum fraction, enough frames:', 
     .     RMIN, 3)
      IF (RMIN(1) .LE. 0.5) CALL BYEBYE
      MINFRM = NINT(RMIN(1))
      MINUSE = MAX0(2, MINFRM)
      FRAC = AMIN1(1., AMAX1(0., RMIN(2)))
      ENOUGH = NINT(RMIN(3))
C
      CALL GETDAT ('Maximum sigma:', SIGMAX, 1)
      IF (SIGMAX .LE. 0) CALL BYEBYE
      SIGMAX=SIGMAX**2
C
   80 CALL TBLANK
      WRITE (6,81)
   81 FORMAT (15X, 'Desired degrees of freedom ---'//
     .        17X, ' 2:  Translations only'/
     .        17X, ' 4:  Translations, rotation, and scale'/
     .        17X, ' 6:  Six-constant linear transformation'/
     .        17X, '12:  Quadratic transformation'/
     .        17X, '20:  Cubic transformation')
      CALL TBLANK
      CALL GETDAT ('Your choice:', WT, 1)
      IF ((WT .LE. 0.5) .OR. (WT .GT. 20.5)) CALL BYEBYE
      MODE = NINT(WT)
      IF (MODE .EQ. 2) THEN
         MITER = 2
      ELSE IF (MODE .EQ. 4) THEN
         MITER = 3
      ELSE IF (MODE .EQ. 6) THEN
         MITER = 4
      ELSE IF (MODE .EQ. 12) THEN
         MITER = 5
      ELSE IF (MODE .EQ. 20) THEN
         MITER = 6
      ELSE
         CALL STUPID ('Invalid response.')
         GO TO 80
      END IF
C
      DO 82 I=1,NFRM
         XX = (RCOL(I)+1.)/2.
         YY = (RROW(I)+1.)/2.
         CALL TRFM (CON(1,I), RCOL(I), RROW(I), FLIP(I), 
     .        MODE, XX, YY, MAG90(I), MAG95(I))
         ZZ = ABS(MAG90(I)-MAG90(1))+ABS(MAG95(I)-MAG95(1))
         L = LENGTH(FILE(I))
         IF (FILE(I)(L-2:L) .EQ. 'stc') THEN
            STC(I) = .TRUE.
         ELSE
            STC(I) = .FALSE.
         END IF
         pc(i) = .false.
         wfc(i) = .false.
         do j=2,29
            if (file(i)(j:j) .eq. ':') then
               if ((file(i)(j-1:j-1) .eq. 'p') .and.
     .             (file(i)(j+1:j+1) .eq. 'u')) pc(i) = .true.
               if ((file(i)(j-1:j-1) .eq. 'w') .and.
     .             (file(i)(j+1:j+1) .eq. 'u')) wfc(i) = .true.
               go to 82
            end if
         end do
   82 CONTINUE
C
      CALL GETDAT ('Critical match-up radius:', RADIUS, 1)
      IF (RADIUS .LE. -1.E10) GO TO 80
      if (radius .lt. 0.) then                               !sm_
         plot = .false.
      else
         plot = .true.
      end if
C
      RADIUS = ABS(RADIUS)
      DO I=1,NFRM
         L = LASTAR(I-1)
         XX = 0.
         YY = 0.
         DO ISTAR = L+1, LASTAR(I)
            XX = AMAX1(XX, ABS(X(ISTAR)))
            YY = AMAX1(YY, ABS(Y(ISTAR)))
         END DO
         SCALE = SQRT(ABS(CON(3,I)*CON(6,I)) + ABS(CON(4,I)*CON(5,I)))
         IF (MODE .LT. 20) THEN
            DO J=MAX0(7,MODE+1),20
               CON(J,I) = 0.0D0
            END DO
         END IF
         DMAG(I) = 99.999
         DO J=1,MODE
            OLD(J,I) = 0.0D0
         END DO
         DIAG = RADIUS/4.
         CLAMP(1,I) = DIAG
         CLAMP(2,I) = DIAG
         CLAMP(3,I) = DIAG/XX
         CLAMP(4,I) = DIAG/XX
         CLAMP(5,I) = DIAG/YY
         CLAMP(6,I) = DIAG/YY
C        DIAG = DIAG/4.
         DO J=7,20
            CLAMP(J,I) = DIAG
         END DO
         IF (MODE .EQ. 2) THEN
            CON(3,I)=1.D0
            CON(4,I)=0.D0
            CON(5,I)=0.D0
            CON(6,I)=1.D0
            DO J=7,20
               CON(J,I)=0.D0
            END DO
         ELSE IF (MODE .EQ. 4) THEN
            FLIP(I)=SIGN(1.,
     .           SNGL(CON(3,I)*CON(6,I)-CON(4,I)*CON(5,I)))
            CON(3,I) = 0.5D0*(CON(3,I) + FLIP(I)*CON(6,I))
            CON(4,I) = 0.5D0*(CON(4,I) - FLIP(I)*CON(5,I))
            CON(5,I) = -FLIP(I)*CON(4,I)
            CON(6,I) = FLIP(I)*CON(3,I)
         END IF
      END DO
      DMAG(1)=0.
C
C-----------------------------------------------------------------------
C
C Initialize the master list, by setting it equal to the starlist from
C the first frame:
C
      DO I=1,LASTAR(1)
         XM(I) = X(I)
         YM(I) = Y(I)
         MM(I) = M(I)
         NOBS(I) = 1
         IWHICH(I,1) = I
      END DO
      NOC(1,1) = 0.
      NOC(2,1) = 0.
      NOC(3,1) = 1.
      NOC(4,1) = 0.
      NOC(5,1) = 0.
      NOC(6,1) = 1.
      NMASTR = LASTAR(1)
      IFIRST=2
C
C Estimate the magnitude limit for each frame.  This is
C provisionally a sloping line from 1.0 at the 90-th percentile
C to 0.5 at the 95-th percentile and extrapolated to zero.
C
      PWT1I = 1./PWT1
      DO IFRM=1,NFRM
         PWT(IFRM) = PWT1I
         I = LASTAR(IFRM-1)
         DO ISTAR = I+1, LASTAR(IFRM)
            J = ISTAR-I
            RMIN(J) = M(ISTAR)
         END DO
         I = LASTAR(IFRM) - I
         J = NINT(0.90*I)
         MAG90(IFRM) = PCTILE(RMIN, I, J)
         MAG95(IFRM) = PCTILE(RMIN(J), I-J+1, (I+J)/2)
         IF (MAG90(IFRM) .GE. MAG95(IFRM)) THEN
            MAG95(IFRM) = 1.E6
         ELSE
            MAG95(IFRM) = 0.5/(MAG95(IFRM) - MAG90(IFRM))
         END IF
      END DO
C
      NITER = 0
C     RLIM = 20.
      PWT(1) = PWT1
      CCLAMP = 1.
 2000 NITER = NITER+1
      IF (NITER .GT. MITER) CCLAMP = 15./REAL(15+NITER-MITER)
      if (plot) then
         call sm_device ('xterm')
c        call sm_device ('x11 -geometry 800x800-1-412 -bg green')
c        call sm_ctype ('black')
         call sm_graphics
         call sm_expand (1.2)
         call sm_erase
         call sm_alpha
         pause = .true.
      else
         pause = .false.
      end if
      MIN = MAX0(1,MIN-1)
      RADSQ = RADIUS**2
      RLIM = RADIUS
      RLIMSQ = RLIM**2
      CALL TBLANK
      CALL OVRWRT (' ', 4)
C
C Sort master list by y coordinate.
C
C Perform the quicksort twice, once by X coordinate and once by
C Y.  This is much, much faster than trying to sort by Y when the
C list is already nearly sorted by Y.
C
C On the first pass, assume the stars are assorted by X or 
C magnitude, or are random---anything but sorted by Y.  Otherwise,
C considerable time may be wasted just this once.
C
      IF (NITER .NE. 1) THEN
         CALL QUICK (XM, NMASTR, INDEX)
         CALL RECTFY (YM, NMASTR, INDEX, IRMIN)
         CALL RECTFY (MM, NMASTR, INDEX, IRMIN)
      END IF
C
      CALL QUICK (YM, NMASTR, INDEX)
      CALL RECTFY (XM, NMASTR, INDEX, IRMIN)
      CALL RECTFY (MM, NMASTR, INDEX, IRMIN)
      IF (IFIRST .EQ. 2) CALL RECTFY (WHICH, NMASTR, INDEX, IRMIN)
C
      DO I=MINY,MAXY
         LAST1(I)=0
      END DO
      DO I=1,NMASTR
         J=MAX0(MINY, MIN0(MAXY, INT(YM(I))))
         LAST1(J)=I
      END DO
      DO I=MINY+1,MAXY
         IF (LAST1(I) .LE. 0) LAST1(I)=LAST1(I-1)
      END DO
C
C LAST1 contains the sequential number of the last star in the master 
C list generated during the previous iteration which has a Y coordinate
C less than I = INT(Y).
C
      NEW=NMASTR
      LIMIT=NEW
C
C Go through each individual star list, trying to match each star in the
C list with a star on the current master list.  If it cannot be matched
C with any star on the master list, try matching it with any stars in
C previous frames which have been appended to the master list during
C this iteration.  If it still cannot be matched, add it to the end
C of the master list.
C
C NMASTR is the number of stars in the master list at the end of the
C        previous iteration.
C  LIMIT is the number of stars in the master list after checking the
C        previous frame, including stars added provisionally from
C        previous frames during the current iteration, but not
C        including stars added from this frame during this iteration
C    NEW will be the number of stars in the current master list,
C        including stars from this frame added during this iteration
C
C Beginning of WHICH loop over images.
C
      DO 2190 IFRM=IFIRST,NFRM
C
C If there have been any stars added to the end of the master list during 
C this iteration, sort the provisional new stars by y coordinate.
C
      TEXT = COMMAS(LIMIT, I)
      TEXT = '  '//TEXT(1:I)
      WRITE (TEXT(I+3:I+52),2) '  Starting  ', IFRM, FILE(IFRM)
    2 FORMAT (A12, I4, 3X, A30)
      CALL OVRWRT (TEXT(1:I+52), 2)
      IF (NEW .GT. NMASTR) THEN
C
C Perform the quicksort twice, once by magnitude and once by
C Y.  This is much, much faster than trying to sort by Y when the
C list is already nearly sorted by Y.
C
         CALL  QUICK (MM(NMASTR+1), NEW-NMASTR, INDEX(NMASTR+1))
         CALL RECTFY (XM(NMASTR+1), NEW-NMASTR, INDEX(NMASTR+1), IRMIN)
         CALL RECTFY (YM(NMASTR+1), NEW-NMASTR, INDEX(NMASTR+1), IRMIN)
         DO I=1,MIN0(IFRM-1,MIDFRM)
            CALL RECTFY (WHICH(NMASTR+1,I), NEW-NMASTR, 
     .           INDEX(NMASTR+1), IRMIN)
         END DO
         IF (IFRM .GT. MIDFRM+1) THEN
            DO I=MIDFRM+1,IFRM-1
               CALL RECTFY (WHCH2(NMASTR+1,I-MIDFRM), NEW-NMASTR, 
     .              INDEX(NMASTR+1), IRMIN)
            END DO
         END IF
C
         CALL  QUICK (YM(NMASTR+1), NEW-NMASTR, INDEX(NMASTR+1))
         CALL RECTFY (XM(NMASTR+1), NEW-NMASTR, INDEX(NMASTR+1), IRMIN)
         CALL RECTFY (MM(NMASTR+1), NEW-NMASTR, INDEX(NMASTR+1), IRMIN)
         DO I=1,MIN0(IFRM-1,MIDFRM)
            CALL RECTFY (WHICH(NMASTR+1,I), NEW-NMASTR, 
     .           INDEX(NMASTR+1), IRMIN)
         END DO
         IF (IFRM-1 .GT. MIDFRM) THEN
            DO I=MIDFRM+1,IFRM-1
               CALL RECTFY (WHCH2(NMASTR+1,I-MIDFRM), NEW-NMASTR, 
     .              INDEX(NMASTR+1), IRMIN)
            END DO
         END IF
C
         DO I=MINY,MAXY
            LAST2(I)=NMASTR
         END DO
         DO I=NMASTR+1,NEW
            J=MAX0(MINY, MIN0(MAXY, INT(YM(I))))
            LAST2(J)=I
         END DO
         DO I=MINY+1,MAXY
            IF (LAST2(I) .LE. NMASTR) LAST2(I)=LAST2(I-1)
         END DO
C
C LAST2 contains the sequential number of the last star among the stars
C added to the master list during the current iteration which has a Y 
C coordinate less than I = INT(Y).
C
      END IF
C
      LIMIT=NEW
C     WRITE (TEXT,2) LIMIT, 'Checking ', IFRM, FILE(IFRM)
C     CALL OVRWRT (TEXT(1:54), 2)
C
      IF (IFRM .LE. MIDFRM) THEN
         DO I=1,LIMIT
            RMIN(I)=RADSQ
            IWHICH(I,IFRM) = -9999
         END DO
      ELSE
         DO I=1,LIMIT
            RMIN(I)=RADSQ
            J = IFRM-MIDFRM
            IWHCH2(I,J) = -9999
         END DO
      END IF
C
C For each star in this star list, identify the star in the master star
C list it lies closest to.
C
      DO 2180 ISTAR=LASTAR(IFRM-1)+1, LASTAR(IFRM)
C
C Transform the star's coordinates from the system of frame IFRM to the 
C system of the master list.
C
         CALL TRFM (CON(1,IFRM), RCOL(IFRM), RROW(IFRM), FLIP(IFRM), 
     .        MODE, X(ISTAR), Y(ISTAR), XX, YY)
         III=ISTAR
C
C See which star in the master list from the previous iteration (stars
C 1 through NMASTR) is closest to this star.  If no unmatched star in 
C the previous master list is within the critical radius, zero will be 
C returned.
C
 2155    IMASTR=ICLOSE(XX, YY, RADIUS, RADSQ, XM, YM, 
     .        NOBS, LAST1, 1, RMIN)
C
         IF (IMASTR .EQ. 0) THEN
C
C This star is not in the master list produced during the last 
C iteration, so see whether it matches any of the stars in other 
C frames which were provisionally added to the master list during this 
C iteration.
C
            IF (LIMIT .GT. NMASTR) THEN
               IMASTR = ICLOSE (XX, YY, RADIUS, RADSQ, XM, YM, 
     .              NOBS, LAST2, NMASTR+1, RMIN)
               IF (IMASTR .NE. 0) GO TO 2160
            END IF
C 
C This star doesn't match anything here, either, so add it to the end 
C of the list (BUT ONLY IF THERE IS ROOM).
C
            IF (NEW .LT. MAX) THEN
               NEW=NEW+1
               XM(NEW)=XX
               YM(NEW)=YY
               NOBS(NEW)=1
               RMIN(NEW)=RADSQ
               IF (DMAG(IFRM) .LT. 90.) THEN
                  MM(NEW)=M(III)+DMAG(IFRM)
               ELSE
                  MM(NEW)=99.999
               END IF
               IF (IFRM .GT. 1) THEN
                  DO I=1,MIN0(IFRM-1,MIDFRM)
                     IWHICH(NEW,I) = -999
                  END DO
                  IF (IFRM-1 .GT. MIDFRM) THEN
                     DO I=MIDFRM+1,IFRM-1
                        J = I-MIDFRM
                        IWHCH2(NEW,J) = -999
                     END DO
                  END IF
               END IF
               IF (IFRM .LE. MIDFRM) THEN
                  IWHICH(NEW,IFRM) = III
               ELSE
                  J = IFRM-MIDFRM
                  IWHCH2(NEW,J) = III
               END IF
               GO TO 2180
            ELSE
               CALL TBLANK
               CALL STUPID ('The maximum permitted number '//
     .            'of stars has been reached.')
               WRITE (6,*) MAX
               CALL OOPS
            END IF
         END IF
C
C This star matches a star either in the previous master list or
C in the stars added to the master list during this iteration...
C 
 2160    IF (IFRM .LE. MIDFRM) THEN
            IF (IWHICH(IMASTR,IFRM) .GT. 0) THEN
C 
C ... but another star had already been matched with this star in the
C master list.  We know that this new star is a better match, so attempt
C to re-match the previous star.
C
               IBUMP=IWHICH(IMASTR,IFRM)
               IWHICH(IMASTR,IFRM)=III
               CALL TRFM (CON(1,IFRM), RCOL(IFRM), RROW(IFRM), 
     .              FLIP(IFRM), MODE, X(IBUMP), Y(IBUMP), XX, YY)
               III=IBUMP
               GO TO 2155
            ELSE
C
C ... and no other star had previously been matched with this star in 
C the master list.
C
               IWHICH(IMASTR,IFRM)=III
            END IF
         ELSE
            J = IFRM-MIDFRM
            IF (IWHCH2(IMASTR,J) .GT. 0) THEN
C 
C ... but another star had already been matched with this star in the
C master list.  We know that this new star is a better match, so attempt
C to re-match the previous star.
C
               IBUMP=IWHCH2(IMASTR,J)
               IWHCH2(IMASTR,J)=III
               CALL TRFM (CON(1,IFRM), RCOL(IFRM), RROW(IFRM), 
     .              FLIP(IFRM), MODE, X(IBUMP), Y(IBUMP), XX, YY)
               III=IBUMP
               GO TO 2155
            ELSE
C
C ... and no other star had previously been matched with this star in 
C the master list.
C
               IWHCH2(IMASTR,J)=III
            END IF
         END IF
 2180 CONTINUE
C
C Update the mean coordinates of any stars added to the master list
C during this iteration.
C
      IF (IFRM .LE. MIDFRM) THEN
         IF (LIMIT .GT. NMASTR) THEN
            DO I=NMASTR+1,LIMIT
               J = IWHICH(I,IFRM)
               IF (J .GT. 0) THEN
                  A = REAL(NOBS(I))
                  B = A+1.
                  CALL TRFM (CON(1,IFRM), RCOL(IFRM), RROW(IFRM), 
     .                 FLIP(IFRM), MODE, X(J), Y(J), XX, YY)
                  XM(I) = (A*XM(I) + XX)/B
                  YM(I) = (A*YM(I) + YY)/B
                  NOBS(I) = NOBS(I) + 1
               END IF
            END DO
         END IF
      ELSE
         IF (LIMIT .GT. NMASTR) THEN
            DO I=NMASTR+1,LIMIT
               J = IWHCH2(I,IFRM-MIDFRM)
               IF (J .GT. 0) THEN
                  A = REAL(NOBS(I))
                  B = A+1.
                  CALL TRFM (CON(1,IFRM), RCOL(IFRM), RROW(IFRM), 
     .                 FLIP(IFRM), MODE, X(J), Y(J), XX, YY)
                  XM(I) = (A*XM(I) + XX)/B
                  YM(I) = (A*YM(I) + YY)/B
                  NOBS(I) = NOBS(I) + 1
               END IF
            END DO
         END IF
      END IF
 2190 CONTINUE
C
C End of WHICH loop over images
C
C AT THIS POINT...
C
C IWHICH(IMASTR,IFRM) tells which star in the starlist for the IFRM-th
C frame corresponds to the IMASTR-th star in the master list.
C
c     call ovrwrt ('Loop 1               ', 2)
C
C Count the number of stars lying within a square with a half-width
C equal to the the critical radius centered on each
C star that was in the master list at the start of this iteration.
C Count only those neighbors that were also in the master list at
C the start of the iteration.  This will serve as a measure of the
C star density in the neighborhood of each star, which will be
C temporarily stored in the array SM.
C
      DO IMASTR=1,NMASTR
         B = 1.
         IF (IMASTR .GT. 1) THEN
C
C Check lower y values.
C
            DO J=IMASTR-1,1,-1
               DY = YY-YM(J)
c              if (dy .lt. 0.) then
c                 print *,'bummer'
c                 call oops
c              end if
               IF (DY .GE. RLIM) GO TO 2195
               DX = ABS(XM(J)-XX)
               IF (DX .LT. RLIM) THEN
                  B = B + 1.
               END IF
            END DO
         END IF
 2195    IF (IMASTR .LT. NMASTR) THEN
C
C Check higher y values.
C
            DO J=IMASTR+1,NMASTR
               DY = YM(J)-YY
c              if (dy .lt. 0.) then
c                 print *,'bummer2'
c                 call oops
c              end if
               IF (DY .GE. RLIM) GO TO 2196
               DX = ABS(XM(J)-XX)
               IF (DX .LT. RLIM) THEN
                  B = B + 1.
               END IF
            END DO
         END IF
 2196    SM(IMASTR) = B
         NOBS(IMASTR) = 0
         XX = XM(IMASTR)
         YY = YM(IMASTR)
      END DO
C
C End of COUNT NEARBY STARS loop.  Now count total observations
C for each star.
C
c     call ovrwrt ('Loop 2               ', 2)
      DO IFRM=1,MIDFRM
         DO IMASTR=1,NMASTR
            IF (IWHICH(IMASTR,IFRM) .GT. 0) 
     .           NOBS(IMASTR)=NOBS(IMASTR)+1
         END DO
      END DO
      DO IFRM=MIDFRM+1,NFRM
         L = IFRM-MIDFRM
         DO IMASTR=1,NMASTR
            IF (IWHCH2(IMASTR,L) .GT. 0) 
     .           NOBS(IMASTR)=NOBS(IMASTR)+1
         END DO
      END DO
C
c     call ovrwrt ('Loop 3               ', 2)
      DO IMASTR=NMASTR+1,LIMIT
         IF (NOBS(IMASTR) .LE. 1) THEN
            L = IWHICH(IMASTR,1)
            IF (L .GT. 0) THEN
               MM(IMASTR) = M(L)
            ELSE
               MM(IMASTR) = 99.999
            END IF
         END IF
      END DO
C
C NOW...
C
C NOBS(IMASTR) holds the number of frames in which the star appears.
C
c SM(IMASTR) holds the number of stars within a critical radius of
c the position of each star in the master list.  Normally, this
c should be 1.
C
C Looping over the various input frames, use the current values of 
C the standard (x,y) coordinates and the table of cross-identifications
C to improve the transformation coefficients.  Employ only those stars
C which were in the master list at the beginning of this iteration,
C not those just added.  (i.e., through NMASTR, not NEW).
C
C Begin loop over frames...
C
C     RDSQ01 = RADSQ*0.1
      LEGIT = 0
C
C Beginning of CORRECT TRANSFORMATIONS loop over images
C
      DO 2850 IFRM=IFIRST,NFRM
      WRITE (TEXT,9) IFRM, FILE(IFRM)
    9 FORMAT ('  Correcting transformations for', I5, 3X, A30)
      CALL OVRWRT (TEXT(1:73), 2)
      DO I=1,MODE
         V(I)=0.0D0
         DO J=1,MODE
            C(I,J)=0.0D0
         END DO
      END DO
C
      SADX=0.0
      SADY=0.0
      SAD=-1.0
      if (plot) then
         if (ifrm.eq.ifirst) call stupid (' ')
         a = 1.e20
         b = -1.e20
         do imastr=1,nmastr
            if (ifrm .le. midfrm) then
               l = iwhich(imastr,ifrm)
            else
               l = iwhch2(imastr,ifrm-midfrm)
            end if
            if (l .gt. 0) then
               if (nobs(imastr) .ge. minuse) then
                  xx = x(l)
                  yy = y(l)
                  if (xx .lt. a) a = xx
                  if (xx .gt. b) b = xx
                  if (yy .lt. a) a = yy
                  if (yy .gt. b) b = yy
               end if
            end if
         end do
         call sm_graphics
         do i=1,1000000
             j = int(sqrt(real(i)))
         end do
         if (a .ge. b) then
            nterm(ifrm) = 0
            call sm_alpha
            go to 2850
         end if
         call sm_limits (-4., 4., a-.03*(b-a), b+.03*(b-a))
         call sm_lweight (2)
         call sm_box (3, 2, 3, 0)
         call sm_xlabel (text(33:length(text)))
         call sm_lweight (1)
         do j=-3,3,2
            call sm_relocate (real(j), a)
            call sm_draw (real(j), b)
         end do
         do j=-2,2,2
            call sm_relocate (real(j), a-.03*(b-a))
            call sm_draw (real(j), b+.03*(b-a))
         end do
C
C small tick marks
C
         do j=-3,3,2
            do k=-3,3
               xx = real(k)/4.
               xx = real(j) + delp1*xx/(del+abs(xx))
               call sm_relocate (xx, a-.03*(b-a))
               call sm_draw (xx, a-.015*(b-a))
               call sm_relocate (xx, b+.03*(b-a))
               call sm_draw (xx, b+.015*(b-a))
            end do
         end do
         iii = 0
         do 750 imastr=1,nmastr
            if (nobs(imastr) .lt. minuse) go to 750
            if (mm(imastr) .gt. 90.) go to 750
            if (ifrm .le. midfrm) then
               if (iwhich(imastr,ifrm) .gt. 0) iii = iii+1
            else
               if (iwhch2(imastr,ifrm-midfrm) .gt. 0) iii = iii+1
            end if
  750    continue
         nfold = max0(1,iii/700)
      end if
C
C Begin loop over stars...
C
      ZZ = 0.
      III = 0
      DO 2800 IMASTR=1,NMASTR
         IF (NOBS(IMASTR) .LT. MINUSE) GO TO 2800
         IF (MM(IMASTR) .GT. 90.) GO TO 2800
         IF (IFRM .LE. MIDFRM) THEN
            L = IWHICH(IMASTR,IFRM)
         ELSE
            L = IWHCH2(IMASTR,IFRM-MIDFRM)
         END IF
         IF (L .LE. 0) GO TO 2800
c        WT = NOBS(IMASTR)-MINUSE+1.
         WT = 1. - REAL(MINUSE)/REAL(NOBS(IMASTR)+1)
         WT = WT / (SM(IMASTR)*(S(L)+2.E-3)**1.5)
C
C For purposes of computing the geometric transformations,
C increase the weight of a star which happens to be in the 
C master frame.
C
         IF (IWHICH(IMASTR,1) .GT. 0) THEN
            ZZ = ZZ + 1.
            WT = WT*PWT(1)
         ELSE
            ZZ = ZZ + PWT1I
         END IF
C
         CALL TRFM (CON(1,IFRM), RCOL(IFRM), RROW(IFRM), 
     .        FLIP(IFRM), MODE, X(L), Y(L), XX, YY)
C
C Reduce the weight of a star near the edge of the critical circle.
C
         DX = XM(IMASTR) - XX
         DY = YM(IMASTR) - YY
         RR = (1. - (DX**2+DY**2)/RADSQ)**2                        !xyz
         IF (RR .LE. 1.E-10) GO TO 2800
         WT = WT*RR
C
C Finally, reduce the weight of a star with an unusual color.
C
         IF (DMAG(IFRM) .LT. 50.) THEN
            WT = WT/(1.+(M(L)+DMAG(IFRM)-MM(IMASTR))**2)
         END IF
         III = III+1
C
C X residual.
C
         SADX = SADX+DX**2
         SAD = SAD+1.
         TERM(1) = 1.
         TERM(2) = 0.
         IF (MODE .EQ. 4) THEN
           TERM(3) = X(L)
           TERM(4) = -FLIP(IFRM)*Y(L)
         ELSE IF (MODE .GE. 6) THEN
           TERM(3) = X(L)
           TERM(4) = 0.
           TERM(5) = Y(L)
           TERM(6) = 0.
           IF (MODE .GE. 7) THEN
             XS = 2.*(X(L)-1.)/(RCOL(IFRM)-1.)-1.
             YS = 2.*(Y(L)-1.)/(RROW(IFRM)-1.)-1.
             TERM(7) = 1.5*XS**2-0.5
             TERM(8) = 0.
             TERM(9) = XS*YS
             TERM(10) = 0.
             TERM(11) = 1.5*YS**2-0.5
             TERM(12) = 0.
             IF (MODE .GE. 13) THEN
               TERM(13) = XS*TERM(7)
               TERM(14) = 0.
               TERM(15) = YS*TERM(7)
               TERM(16) = 0.
               TERM(17) = XS*TERM(11)
               TERM(18) = 0.
               TERM(19) = YS*TERM(11)
               TERM(20) = 0.
             END IF
           END IF
         END IF
C
         DO I=1,MODE
            IF (TERM(I) .NE. 0.) THEN
               V(I) = V(I)+WT*TERM(I)*DX
               DO J=1,MODE
                  IF (TERM(J) .NE. 0.) C(I,J) = 
     .                 C(I,J)+WT*TERM(I)*TERM(J)
               END DO
            END IF
         END DO
C
C Y residual.
C
 2790    SADY = SADY+DY**2
         TERM(1) = 0.
         TERM(2) = 1.
         IF (MODE .EQ. 4) THEN
           TERM(3) = FLIP(IFRM)*Y(L)
           TERM(4) = X(L)
         ELSE IF (MODE .GE. 6) THEN
           TERM(3) = 0.
           TERM(4) = X(L)
           TERM(5) = 0.
           TERM(6) = Y(L)
           IF (MODE .GE. 7) THEN
             TERM(8) = TERM(7)
             TERM(7) = 0.
             TERM(10) = TERM(9)
             TERM(9) = 0.
             TERM(12) = TERM(11)
             TERM(11) = 0.
             IF (MODE .GE. 13) THEN
               TERM(14) = TERM(13)
               TERM(13) = 0.
               TERM(16) = TERM(15)
               TERM(15) = 0.
               TERM(18) = TERM(17)
               TERM(17) = 0.
               TERM(20) = TERM(19)
               TERM(19) = 0.
             END IF
           END IF
         END IF
C
         DO I=1,MODE
            IF (TERM(I) .NE. 0.) THEN
               V(I) = V(I)+WT*TERM(I)*DY
               DO J=1,MODE
                  IF (TERM(J) .NE. 0.) C(I,J) = 
     .                 C(I,J)+WT*TERM(I)*TERM(J)
               END DO
            END IF
         END DO
         if (plot .and. (mod(iii,nfold) .eq. 0)) then
            if (iwhich(imastr,1) .le. 0) then
               call sm_ptype (41.3, 1)
            else
               call sm_ptype (41., 1)
            end if
            dx = dx/radius
            dy = dy/radius
            dx = delp1*dx/(del+abs(dx))
            dy = delp1*dy/(del+abs(dy))
            call sm_relocate (dx-3., x(l))
            call sm_dot 
            call sm_relocate (dx-1., y(l))
            call sm_dot
            call sm_relocate (dy+1., x(l))
            call sm_dot
            call sm_relocate (dy+3., y(l))
            call sm_dot
         end if
 2800 CONTINUE
      if (plot) call sm_alpha
C
      III = INT(ZZ)
      IF (III .LT. MIN) THEN
         NTERM(IFRM) = 0
         GO TO 2850
      ELSE IF (III-MIN .LE. 1) THEN
         III = 2
      ELSE IF ((III-MIN .EQ. 2) .AND. (MODE .NE. 4)) THEN
         III = 2
      ELSE IF ((III-MIN .EQ. 2) .AND. (MODE .EQ. 4)) THEN
         III = 4
      ELSE IF ((III-MIN .LT. 8) .AND. (MODE .GE. 6)) THEN
         III = 6
      ELSE IF ((III-MIN .LT. 14) .AND. (MODE .GE. 12)) THEN
         III = 12
      ELSE
         III = MIN0(MODE,20)
      END IF
      IF (STC(IFRM)) III = MIN0(III,12)
      IF (IFRM .EQ. 1) III = MIN0(III,2)
      IF (NOK(IFRM) .LT. 1) THEN
         III = MIN0(III, 2)
      ELSE IF (NOK(IFRM) .LT. 2) THEN
         III = MIN0(III, 6)
      ELSE IF (NOK(IFRM) .LT. 3) THEN
         III = MIN0(III, 12)
      ELSE 
         III = MIN0(III, 20)
      END IF
      LEGIT = MAX0(LEGIT, III)
      IF (IFRM .EQ. 1) III = 2
      CALL DINVRS (C, MAXCON, III, IFLAG)
      IF (IFLAG .NE. 0) THEN
         CALL STUPID ('Matrix error.')
         GO TO 2850
      END IF
      NOK(IFRM) = NOK(IFRM)+1
      CALL DVMUL (C, MAXCON, III, V, DCON)
      DO I=1,III
         CON(I,IFRM) = CON(I,IFRM) + DCON(I) * CCLAMP
      END DO
      IF (III .EQ. 4) THEN
         CON(5,I) = -FLIP(I)*CON(4,I)
         CON(6,I) = FLIP(I)*CON(3,I)
      END IF
      if (plot) then
         text = ' '
         call ovrwrt (text(1:80), 2)
         out(1) = drndff(dcon(1), 9, 3)
         out(2) = drndff(dcon(2), 9, 3)
         if (iii .ge. 3) then
            do i=3,6
               out(i) = drndff(dcon(i), 9, 5)
            end do
            if (iii .ge. 7) then
               do i=7,iii
                  out(i) = drndff(dcon(i), 9, 4)
               end do
            end if
         end if
         strng = rndoff(pwt(ifrm), 7, 3)
         print 666,(out(i)(1:9),i=1,iii),strng(1:7),
     .        '  ', file(ifrm)(1:length(file(ifrm)))
  666    format (12A / 24X, 11A)
         if (pause) then
            call rdchar (5, text, i, istat)
         else
            call sleep (1)
         end if
         call sm_graphics
         call sm_erase
         call sm_alpha
         if (pause) then
            if ((text(1:1).eq.'e').or.(text(1:1).eq.'E')) then
               plot = .false.
            else if ((text(1:1).eq.'g').or.(text(1:1).eq.'G')) then
               pause = .false.
            end if
         end if
      end if
C
      NTERM(IFRM) = III
      IF (SAD .LE. 0.5) GO TO 2850
      IF (SAD .GT. 0.5) THEN
         DATUM(1,IFRM) = SQRT(SADX/SAD)
         DATUM(2,IFRM) = SQRT(SADY/SAD)
      ELSE
         DATUM(1,IFRM) = 0.
         DATUM(2,IFRM) = 0.
      END IF
 2850 CONTINUE
C
C End of loop CORRECT TRANSFORMATIONS over input frames.
C
C If the coordinate system of the master list has drifted away from that
C of frame 1, shift it back.
C
      DO IFRM=NFRM,1,-1
         CON(1,IFRM) = CON(1,IFRM) - CON(1,1)
         CON(2,IFRM) = CON(2,IFRM) - CON(2,1)
      END DO
      CON(3,1) = 1.D0
      CON(4,1) = 0.D0
      CON(5,1) = 0.D0
      CON(6,1) = 1.D0
C
C Now, again looping over the various input frames, compute the additive
C magnitude corrections to the system of the master list.  First, erase
C all memory of the master-list magnitudes of stars in an insufficient
C number of frames, so they won't be used in the computation.
C
      DO IMASTR=1,NMASTR
         IF (NOBS(IMASTR) .LT. MINFRM) THEN
            MM(IMASTR) = 99.999
         END IF
      END DO
C
C Begin loop over frames...
C
      DO 2950 IFRM=IFIRST,NFRM
      L = 0
C
C Begin loop over stars...
C
      DO 2900 IMASTR=1,NMASTR
         IF (MM(IMASTR) .GT. 90.) GO TO 2900
         IF (IFRM .LE. MIDFRM) THEN
            J = IWHICH(IMASTR,IFRM)
         ELSE
            J = IWHCH2(IMASTR,IFRM-MIDFRM)
         END IF
         IF (J .GT. 0) THEN
            L = L+1
            RMIN(L) = MM(IMASTR)-M(J)
C
C Soften the weight with a sigma of 0.01 mag
C
            SM(L) = 1. / (S(J) + 1.E-4)
C
C Weight of observations with really huge errors ( > 0.2 mag)
C goes as 1/sigma**4.
C
            SM(L) = SM(L) / (4.E-2+S(J))
C
C Further reduce the weights of stars near the magnitude limit.
C
            SM(L)=SM(L)*AMAX1(0., AMIN1(1., 1.-MAG95(IFRM)*
     .           (M(J)-MAG90(IFRM))))
         END IF
 2900 CONTINUE
C
      IF (L .GE. 1) THEN
         CALL QUICK (RMIN, L, INDEX)
         CALL RECTFY (SM, L, INDEX, NOBS)
         DO I=2,L
            SM(I) = SM(I)+SM(I-1)
         END DO
C
C Locate the magnitude difference of median cumulative weight, using a 
C binary search.
C
         J = IBNRY(SM, L, 0.5)
         DMAG(IFRM) = RMIN(J)
         J = IBNRY(SM, L, 0.3085)
         A = RMIN(J)
         J = IBNRY(SM, L, 0.6915)
         B = RMIN(J)
         SUM=DMAG(IFRM)-DMAG(1)
         SMAG(IFRM)=B-A
      ELSE
         IF (IFRM .NE. 1) THEN
            DMAG(IFRM) = 99.999
            SUM = 99.999
         END IF
         SMAG(IFRM) = 0.
      END IF
C
      IF (MODE .EQ. 2) THEN
         WRITE (TEXT,3) (DATUM(I,IFRM), I=1,2), 
     .        (NINT(CON(I,IFRM)), I=1,2), SUM, 
     .        SMAG(IFRM), L, NTERM(IFRM), 
     .        FILE(IFRM)(1:LENGTH(FILE(IFRM)))
    3    FORMAT (1X, 2F5.2, 2I7, F7.2, F6.3, I8, I3, 1X, A)
      ELSE IF (MODE .EQ. 4) THEN
         WRITE (TEXT,1) (DATUM(I,IFRM), I=1,2), 
     .       (NINT(CON(I,IFRM)), I=1,2), (CON(I,IFRM), I=3,4), SUM, 
     .       SMAG(IFRM), L, NTERM(IFRM), 
     .        FILE(IFRM)(1:LENGTH(FILE(IFRM)))
    1    FORMAT (1X, 2F5.2, 2I7, 2F7.3, F7.2, F6.3, I8, I3, 1X, A)
      ELSE
         WRITE (TEXT,4) (DATUM(I,IFRM), I=1,2), 
     .        (NINT(CON(I,IFRM)), I=1,2), (CON(I,IFRM), I=3,6), SUM, 
     .        SMAG(IFRM), L, NTERM(IFRM), 
     .        FILE(IFRM)(1:LENGTH(FILE(IFRM)))
    4    FORMAT (1X, 2F5.2, 2I7, 4F7.3, F7.2, F6.3, I8, I3, 1X, A)
      END IF
      CALL OVRWRT(TEXT(1:LENGTH(TEXT)), 3)
c     write (12,*)TEXT(1:LENGTH(TEXT))
C
      IF (SHRINK .AND. (IFRM .EQ. 1)) THEN
         PWT(1) = AMAX1(1., PWT(1)/STEP)
         PWT1I = 1./PWT(1)
      ELSE
C
C Maximum weight goes as the inverse square of the relative scale.
C
         IF (NTERM(IFRM) .GE. MODE) THEN
            XX = 1./(ABS(CON(3,IFRM)*CON(6,IFRM)) + 
     .               ABS(CON(4,IFRM)*CON(5,IFRM)))
            PWT(IFRM) = AMIN1( STEP*PWT(IFRM), XX )
         END IF
C
         I = 2*L
         IF (I .GT. NTERM(IFRM)) THEN
            SHRINK = .TRUE.
            PWT(IFRM) = AMAX1(1./PWT1, PWT(IFRM)) 
     .           * REAL(I-NTERM(IFRM))/REAL(I)
         ELSE
            PWT(IFRM) = 1.E-8
         END IF
      END IF
C
      SMAG(IFRM) = SMAG(IFRM)**2
 2950 CONTINUE
C
C Correct magnitude offsets
C
      DO IFRM=NFRM,2,-1
         IF (DMAG(IFRM) .LE. 90.) DMAG(IFRM)=DMAG(IFRM)-DMAG(1)
      END DO
      DMAG(1) = 0.
C
      IFIRST=1
      CALL TBLANK
C
C Now redetermine mean positions and magnitudes of stars on master list.
C
      DO IMASTR=1,NEW
         MM(IMASTR) = 0.
         SM(IMASTR) = 0.
         RMIN(IMASTR) = 0.
         NOBS(IMASTR) = 0
      END DO
C
C Update the mean coordinates in the system of the master list.  The arrays
C MM and SM will be used to accumulate the X and Y centroids, respectively.
C
      DO 2970 IFRM=1,NFRM
         DO 2960 IMASTR=1,NEW
            IF (IFRM .LE. MIDFRM) THEN
               J=IWHICH(IMASTR,IFRM)
            ELSE
               J=IWHCH2(IMASTR,IFRM-MIDFRM)
            END IF
            IF (J .GT. 0) THEN
               NOBS(IMASTR) = NOBS(IMASTR)+1
               CALL TRFM (CON(1,IFRM), RCOL(IFRM), RROW(IFRM),
     .              FLIP(IFRM), MODE, X(J), Y(J), XX, YY)
               XX = XX-XM(IMASTR)
               YY = YY-YM(IMASTR)
C
C Reduce the weight of a position near the edge of the
C critical circle.  
C
               RR = XX**2 + YY**2
               IF (RR .LT. RADSQ) THEN
                  RR = PWT(IFRM)*(1.-RR/RADSQ) / (S(J) + 1.E-3)
                  MM(IMASTR) = MM(IMASTR) + RR*XX
                  SM(IMASTR) = SM(IMASTR) + RR*YY
                  RMIN(IMASTR) = RMIN(IMASTR) + RR
               END IF
            END IF
 2960    CONTINUE
 2970 CONTINUE
C
      ZZ = 11./(REAL(NITER) + 10.)
      DO IMASTR=1,NEW
         IF (RMIN(IMASTR) .GT. 0.) THEN
            RR = MM(IMASTR)/RMIN(IMASTR)
            XM(IMASTR) = XM(IMASTR) + ZZ*RR
            RR = SM(IMASTR)/RMIN(IMASTR)
            YM(IMASTR) = YM(IMASTR) + ZZ*RR
            MM(IMASTR) = 0.
            SM(IMASTR) = 0.
         END IF
      END DO
C
C Now update the magnitudes on the system of the master list.
C
      DO IMASTR=1,NEW
         DO IFRM=1,NFRM
            IF (IFRM .LE. MIDFRM) THEN
               J=IWHICH(IMASTR,IFRM)
            ELSE
               J=IWHCH2(IMASTR,IFRM-MIDFRM)
            END IF
            IF ((J .GT. 0) .AND. (DMAG(IFRM) .LT. 90.)) THEN
C
C Soften the weights with a sigma of 0.003 mag.
C
               RR = 1. / (S(J) + 1.E-5)
               MM(IMASTR)=MM(IMASTR)+(M(J)+DMAG(IFRM))*RR
               SM(IMASTR)=SM(IMASTR)+RR
            END IF
         END DO
         IF (SM(IMASTR) .GT. 0.) THEN
            MM(IMASTR)=MM(IMASTR)/SM(IMASTR)
            SM(IMASTR)=1./SM(IMASTR)
         ELSE IF (IWHICH(IMASTR,1) .GT. 0) THEN
            MM(IMASTR) = M(IWHICH(IMASTR,1))
            SM(IMASTR) = S(IWHICH(IMASTR,1))
         ELSE
            MM(IMASTR)=99.999
         END IF
      END DO
C
C Invert the transformation equations.
C
      DO IFRM=2,NFRM
         CALL INVERT (CON(1,IFRM), MODE, RCOL(IFRM), RROW(IFRM), 
     .         RCOL(1), RROW(1), NOC(1,IFRM))
      END DO
C
      NERROR = 0
      NFEW = 0
      NENUF = 0
      IMASTR=0
 2500 CONTINUE
C
C If the LAST star on the master list is insignificant, then eliminate 
C it.
C
      IF ((SM(NEW) .GT. SIGMAX) .OR. 
     .     (NOBS(NEW) .LT. MINFRM)) THEN
         IF (SM(NEW) .GT. SIGMAX) THEN
             NERROR = NERROR+1
         ELSE
             NFEW = NFEW+1
         END IF
         NEW=NEW-1
         IF (NEW .LE. 0) THEN
            CALL STUPID ('ERROR --- No stars left.')
            CALL BYEBYE
         END IF
         GO TO 2500
      ELSE
         IF (NOBS(NEW) .GE. ENOUGH) GO TO 2600
         A = 0.
         B = 0.
C
C Count the frames in which this star COULD reasonably have
C appeared.  That means its predicted coordinates fall
C within the area of the frame [ 1.0 < x < real(NCOL),
C 1.0 < y < real(NROW) ]. Furthermore, the likelihood
C that a star SHOULD be found in a frame is a function of
C its predicted instrumental magnitude in that frame:
C the "weight" of that frame is 1.0 if the predicted 
C magnitude is brighter than the frame's 90-th percentile, and
C drops linearly to 0.0, passing through 0.5 at the
C frame's 95-th percentile.
C
         DO 2550 IFRM=1,NFRM
            IF (STC(IFRM)) GO TO 2550
            if (pc(ifrm)) then
               zz = 60.
            else if (wfc(ifrm)) then
               zz = 52.
            else
               zz = 1.
            end if
            CALL TRFM (NOC(1,IFRM), RCOL(1), RROW(1), 
     .           FLIP(IFRM), MODE, XM(NEW), YM(NEW), XX, YY)
            IF ((XX .GE. zz) .AND. (XX .LE. RCOL(IFRM)) .AND.
     .          (YY .GE. zz) .AND. (YY .LE. RROW(IFRM)) .AND.
     .          (NTERM(IFRM) .GE. 2)) THEN
              WT = AMAX1(0., AMIN1(1., 1.-MAG95(IFRM)*
     .             (MM(NEW)-DMAG(IFRM)-MAG90(IFRM))))
              A = A+WT
              IF (IFRM .LE. MIDFRM) THEN
                 IF (IWHICH(NEW,IFRM) .GT. 0) B = B+WT
              ELSE
                 IF (IWHCH2(NEW,IFRM-MIDFRM) .GT. 0) B = B+WT
              END IF
            END IF
 2550    CONTINUE
         IF (B .GE. FRAC*A) GO TO 2600
C
         NENUF = NENUF+1
         NEW=NEW-1
         IF (NEW .LE. 0) THEN
            CALL STUPID ('ERROR --- No stars left.')
            CALL BYEBYE
         END IF
         GO TO 2500
      END IF
C
C Now check the next star down from the top of the list.  If this
C star is insignificant, then overwrite it with the last star in
C the list (which we already know, from above, to be significant).
C
 2600 IMASTR=IMASTR+1
      IF (IMASTR .GE. NEW) GO TO 2700
      IF ((SM(IMASTR) .GT. SIGMAX) .OR.
     .     (NOBS(IMASTR) .LT. MINFRM)) THEN
C
C  This star is insignificant.  Overwrite it with the last star
C  in the list, decrement NEW by one, and go back up to
C  statement 2500 to test the significance of the new last star.
C
         XM(IMASTR)=XM(NEW)
         YM(IMASTR)=YM(NEW)
         MM(IMASTR)=MM(NEW)
         SM(IMASTR)=SM(NEW)
         NOBS(IMASTR)=NOBS(NEW)
         DO IFRM=1,MIDFRM
            IWHICH(IMASTR,IFRM)=IWHICH(NEW,IFRM)
         END DO
         DO IFRM=MIDFRM+1,NFRM
            J = IFRM-MIDFRM
            IWHCH2(IMASTR,J)=IWHCH2(NEW,J)
         END DO
         IF (SM(IMASTR) .GT. SIGMAX) THEN
            NERROR = NERROR+1
         ELSE
            NFEW = NFEW+1
         END IF
         NEW=NEW-1
         GO TO 2500
      ELSE
         IF (NOBS(IMASTR) .GE. ENOUGH) GO TO 2600
         A = 0.
         B = 0.
C
C Count the frames in which this star COULD reasonably have
C appeared.
C
         DO 2650 IFRM=1,NFRM
            IF (STC(IFRM)) GO TO 2650
            if (pc(ifrm)) then
               zz = 60.
            else if (wfc(ifrm)) then
               zz = 52.
            else
               zz = 1.
            end if
            CALL TRFM (NOC(1,IFRM), RCOL(1), RROW(1), 
     .           FLIP(IFRM), MODE, XM(IMASTR), YM(IMASTR), XX, YY)
            IF ((XX .GE. zz) .AND. (XX .LE. RCOL(IFRM)) .AND.
     .          (YY .GE. zz) .AND. (YY .LE. RROW(IFRM)) .AND.
     .          (NTERM(IFRM) .GE. 2)) THEN
               WT = AMAX1(0., AMIN1(1., 1.-MAG95(IFRM)*
     .             (MM(IMASTR)-DMAG(IFRM)-MAG90(IFRM))))
               A = A+WT
               IF (IFRM .LE. MIDFRM) THEN
                  IF (IWHICH(IMASTR,IFRM) .GT. 0) B = B+WT
               ELSE
                  IF (IWHCH2(IMASTR,IFRM-MIDFRM) .GT. 0) B = B+WT
               END IF
            END IF
 2650    CONTINUE
         IF (B .GE. FRAC*A) GO TO 2600
C
C  This star is insignificant.  Overwrite it with the last star
C  in the list, decrement NEW by one, and go back up to
C  statement 2500 to test the significance of the new last star.
C
         XM(IMASTR)=XM(NEW)
         YM(IMASTR)=YM(NEW)
         MM(IMASTR)=MM(NEW)
         SM(IMASTR)=SM(NEW)
         NOBS(IMASTR)=NOBS(NEW)
         DO IFRM=1,MIDFRM
            IWHICH(IMASTR,IFRM)=IWHICH(NEW,IFRM)
         END DO
         DO IFRM=MIDFRM+1,NFRM
            J = IFRM-MIDFRM
            IWHCH2(IMASTR,J)=IWHCH2(NEW,J)
         END DO
         NENUF = NENUF+1
         NEW=NEW-1
         GO TO 2500
      END IF
 2700 NMASTR = NEW
c     write (6,*)nfew,nenuf,nerror,nerror+nfew+nenuf
c     write (6,*) 
C
C Now we have a pure list of verified significant cross-identifications.
C
      POSIT1 = COMMAS(NMASTR, J)
      STRNG = RNDOFF(RADIUS, 6, 3)
      WRITE (6,603) POSIT1(1:J), STRNG
  603 FORMAT (2X, A, ' stars within radius', A6)
      CALL TBLANK
      ROLD = RADIUS
      CALL TBLANK
  602 CALL GETDAT ('New match-up radius (0 to exit):', RADIUS, 1)
      IF (RADIUS .LT. -1.E10) THEN
         CALL GETYN ('Are you sure?', ANSWER)
         IF (ANSWER .EQ. 'Y') THEN
            CALL BYEBYE
         ELSE
            GO TO 602
         END IF
      END IF
      if (radius .lt. -0.001) then                      ! sm_
         plot = .true.
      else
         plot = .false.
      end if
      RADIUS = ABS(RADIUS)
      IF (RADIUS .GT. 1.E-10) GO TO 2000
      call sm_graphics
      call sm_erase
      call sm_alpha
      call sm_device ('nodevice')
         do i=1,1000000
             j = int(sqrt(real(i)))
         end do
C
      WRITE (6,61)
   61 FORMAT (/5X,
     .   ' Transformations are in the sense  STANDARD = fn(OBSERVED).'
     .   /)
C
C Get ready to put out results.
C
      CALL GETYN ('Assign new star IDs?', ANSWER)
      IF (ANSWER .EQ. 'Y') THEN
C
C Sort stars by X coordinate.
C
         CALL QUICK (XM, NMASTR, INDEX)
         CALL RECTFY (YM, NMASTR, INDEX, IRMIN)
         CALL RECTFY (MM, NMASTR, INDEX, IRMIN)
         CALL RECTFY (SM, NMASTR, INDEX, IRMIN)
         CALL RECTFY (ROBS, NMASTR, INDEX, IRMIN)
         DO IFRM=1,MIDFRM
            CALL RECTFY (WHICH(1,IFRM), NMASTR, INDEX, IRMIN)
         END DO
         DO IFRM=MIDFRM+1,NFRM
            J = IFRM-MIDFRM
            CALL RECTFY (WHCH2(1,J), NMASTR, INDEX, IRMIN)
         END DO
         DO IMASTR=1,NMASTR
            INDEX(IMASTR) = IMASTR
         END DO
      ELSE
         III = 5 000 000
         DO IMASTR=1,NMASTR
            J = IWHICH(IMASTR,1)
            IF (J .LE. 0) THEN
               III = III+1
               INDEX(IMASTR) = III
            ELSE
               INDEX(IMASTR) = IDMAST(J)
            END IF
         END DO
      END IF
C
      DO IMASTR=1,NMASTR
         IDMAST(IMASTR) = INDEX(IMASTR)
      END DO
C
      CALL TBLANK
      CALL INQUIR ('Now, do you want...', 45)
      CALL TBLANK
      CALL GETYN ('A file with mean magnitudes and scatter?', ANSWER)
      IF (ANSWER .EQ. 'E') THEN
         CALL BYEBYE
      ELSE IF (ANSWER .EQ. 'Y') THEN
         CALL INFILE (2, FILE(1), ISTAT)
         CALL RDHEAD (2, I, J, K, XX, YY, ZZ, A, B, DX, DY)
         IF (J .LE. 0) THEN
            CALL TBLANK 
            CALL STUPID ('Error reading file header.')
            CALL CLFILE (1)
            CALL OOPS
         END IF
         CALL CLFILE (2)
         OFILE=SWITCH(LFILE, CASE('.mag'))
         CALL GETNAM ('Output file name:', OFILE)
         OFILE = EXTEND(OFILE, 'mag')
         CALL OUTFIL (1, OFILE, ISTAT)
         XX = -500.
         CALL WRHEAD (1, 1, J, K, 7, XX, YY, ZZ, A, B, DX, DY)
C
         DO IMASTR=1,NMASTR
            IF (NOBS(IMASTR) .GE. 2) THEN
C
C Determine robust weighted mean magnitudes of stars on master list.
C
               MM(IMASTR) = EXP(0.921034*(15.-MM(IMASTR)))
               SUMCHI=0.
               SUMSHP=0.
               SUMWT=0.
               DO IFRM=1,NFRM
                  IF (.NOT. STC(IFRM)) THEN
                     IF (IFRM .LE. MIDFRM) THEN
                        J=IWHICH(IMASTR,IFRM)
                     ELSE
                        J=IWHCH2(IMASTR,IFRM-MIDFRM)
                     END IF
                     IF (J .GT. 0) THEN
                        M(J) = EXP(0.921034*(15.-M(J)-DMAG(IFRM)))
                        S(J) = S(J)*(0.921034*M(J))**2
                        WT = 1. / S(J)
                        SUMWT=SUMWT+WT
                        SUMCHI=SUMCHI+CHI(J)*WT
                        SUMSHP=SUMSHP+AMAX1(-10.,AMIN1(20.,SHARP(J)))*WT
                     END IF
                  END IF
               END DO
               IF (SUMWT .LE. 0.) THEN
                  MM(IMASTR) = 15.-1.0857362*ALOG(MM(IMASTR))
                  SUMCHI = 0.
                  SUMSHP = 0.
                  GO TO 3015
               END IF
               SUMCHI=SUMCHI/SUMWT
               SUMSHP=SUMSHP/SUMWT
 3010          CONTINUE
               SUM=0.
               SUMWT=0.
               DO IFRM=1,NFRM
                  IF (.NOT. STC(IFRM)) THEN
                     IF (IFRM .LE. MIDFRM) THEN
                        J=IWHICH(IMASTR,IFRM)
                     ELSE
                        J=IWHCH2(IMASTR,IFRM-MIDFRM)
                     END IF
                     IF (J .GT. 0) THEN
                        B=M(J)-MM(IMASTR)
C
C Give half weight to 2-sigma residuals.
C
                        WT=(1. / S(J)) * (4. / (4.+(B**2 / S(J))))
                        SUM=SUM+B*WT
                        SUMWT=SUMWT+WT
                     END IF
                  END IF
               END DO
               B=SUM/SUMWT
               MM(IMASTR)=MM(IMASTR)+B
               IF (ABS(B) .GE. 0.00005*MM(IMASTR)) GO TO 3010
C
               A=0.
               RR=0.
               SUM=0.
               SUMWT=0.
               DO IFRM=1,NFRM
                  IF (.NOT. STC(IFRM)) THEN
                     IF (IFRM .LE. MIDFRM) THEN
                        J=IWHICH(IMASTR,IFRM)
                     ELSE
                        J=IWHCH2(IMASTR,IFRM-MIDFRM)
                     END IF
                     IF (J .GT. 0) THEN
                        B=M(J)-MM(IMASTR)
                        WT = 1. / S(J)
                        A=A+WT*ABS(B)
                        IF (B .GT. 0.) RR = RR+1.
                        SUM=SUM+WT*(ABS(B)/SQRT(S(J)))
                        SUMWT=SUMWT+WT
                        S(J)=S(J)/(0.921034*M(J))**2
                        M(J)=15.-1.0857362*ALOG(M(J))-DMAG(IFRM)
                     END IF
                  END IF
               END DO
               B=SQRT(NOBS(IMASTR)/(NOBS(IMASTR)-1.))
               A=1.2533*A/SUMWT                                          ! sigma(1 obs.)
               RR=RR/REAL(NOBS(IMASTR))
               A=1.0857362*A*B/MM(IMASTR)
               SUM=1.2533*B*SUM/SUMWT                                      ! CHI
               MM(IMASTR)=15.-1.0857362*ALOG(MM(IMASTR))
            ELSE
               SUM=0.
               SUMWT=0.
               SUMCHI=0.
               SUMSHP=0.
               RR=1.
               DO IFRM=1,NFRM
                  IF (IFRM .LE. MIDFRM) THEN
                     J=IWHICH(IMASTR,IFRM)
                  ELSE
                     J=IWHCH2(IMASTR,IFRM-MIDFRM)
                  END IF
                  IF (J .GT. 0) THEN
                     A=M(J)+DMAG(IFRM)
                     WT=1./S(J)
                     SUM=SUM+A*WT
                     SUMCHI=SUMCHI+CHI(J)*WT
                     SUMSHP=SUMSHP+SHARP(J)*WT
                     SUMWT=SUMWT+WT
                  END IF
               END DO
               MM(IMASTR)=SUM/SUMWT
               SUMCHI=SUMCHI/SUMWT
               SUMSHP=SUMSHP/SUMWT
               A=0.
               SUM=0.
            END IF
C
C  1. Star ID
C  2. X coordinate, in system of master frame
C  3. Y coordinate, in system of master frame
C  4. Robust intensity-weighted mean instrumental magnitude
C  5. Standard error of the mean magnitude, based on individual frame sigmas
C  6. External standard error of one measurement, based on frame-to-frame repeatability
C  7. Number of frames in which the star appeared
C  8. Weighted average CHI value
C  9. Weighted average SHARP value
C 10. Variability index: ratio of external error to internal error
C 11. Blunder index: fraction of residuals that are positive
C
 3015       POSIT1 = RNDOFF(XM(IMASTR), 9, 3)
            POSIT2 = RNDOFF(YM(IMASTR), 9, 3)
            CH = RNDOFF(SUMCHI, 9, 3)
            SHP = RNDOFF(SUMSHP, 9, 3)
            STRNG = RNDOFF(SUM, 9, 2)
            WRITE (1,110) IDMAST(IMASTR), POSIT1, POSIT2,
     .           MM(IMASTR), SQRT(SM(IMASTR)), A, NOBS(IMASTR),
     .           CH, SHP, STRNG, RR
  110       FORMAT (I7, 2A9, F9.3, 2F9.4, I8, '.', 3A9, F9.3)
         END DO
         CALL CLFILE (1)
      END IF
C
      CALL GETYN 
     .     ('A file with corrected magnitudes and errors?', ANSWER)
      IF (ANSWER .EQ. 'E') THEN 
         CALL BYEBYE
      ELSE IF (ANSWER .EQ. 'Y') THEN
         OFILE=SWITCH(LFILE, CASE('.cor'))
         CALL GETNAM ('Output file name:', OFILE)
         OFILE = EXTEND(OFILE, 'cor')
         CALL OUTFIL (1, OFILE, ISTAT)
         CALL INFILE (2, FILE(1), ISTAT)
         CALL RDCHAR (2, HEAD(1), K, ISTAT)
         CALL RDCHAR (2, HEAD(2), L, ISTAT)
         CALL RDCHAR (2, HEAD(3), LL, ISTAT)
         CALL CLFILE (2)
         IF ((K .GE. 3) .AND. (HEAD(1)(2:3) .EQ. 'NL')) THEN
            WRITE (1,115) HEAD(1)(1:K)
            IF (L .GE. 1) THEN
               WRITE (HEAD(2)(1:3),115) '  1'
            END IF
            WRITE (1,115) HEAD(2)(1:L)
            WRITE (1,115) HEAD(3)(1:LL)
         END IF
C
         DO IMASTR=1,NMASTR
            SUMCHI=0.
            SUMSHP=0.
            SUMWT=0.
            DO IFRM=1,NFRM
               IF (IFRM .LE. MIDFRM) THEN
                  J=IWHICH(IMASTR,IFRM)
               ELSE
                  J=IWHCH2(IMASTR,IFRM-MIDFRM)
               END IF
               IF (J .GT. 0) THEN
                  DATUM(1,IFRM)=M(J)+DMAG(IFRM)
                  DATUM(2,IFRM)=AMIN1
     .                 (9.9999,SQRT(AMAX1(0., S(J))))
                  A=EXP(0.921034*(15.-DATUM(1,IFRM)))
                  B=S(J)*(0.921034*A)**2
                  WT = 1./B
                  SUMWT=SUMWT+WT
                  SUMCHI=SUMCHI+CHI(J)*WT
                  SUMSHP=SUMSHP+SHARP(J)*WT
               ELSE
                  DATUM(1,IFRM)=99.9999
                  DATUM(2,IFRM)=9.9999
               END IF
            END DO
            SUMCHI = SUMCHI/SUMWT
            SUMSHP = SUMSHP/SUMWT
            POSIT1 = RNDOFF(XM(IMASTR), 9, 3)
            POSIT2 = RNDOFF(YM(IMASTR), 9, 3)
            WRITE (1,111) IDMAST(IMASTR), POSIT1, POSIT2,
     .               ((DATUM(J,I), J=1,2), I=1,NFRM), SUMCHI, SUMSHP
  111       FORMAT (I7, 2A9, 12F9.4:/ (25X, 12F9.4))
         END DO
         CALL CLFILE (1)
      END IF
C
      CALL GETYN ('A file with raw magnitudes and errors?', ANSWER)
      IF (ANSWER .EQ. 'E') THEN
         CALL BYEBYE
      ELSE IF (ANSWER .EQ. 'Y') THEN
         OFILE=SWITCH(LFILE, CASE('.raw'))
         CALL GETNAM ('Output file name:', OFILE)
         OFILE = EXTEND(OFILE, 'raw')
         CALL OUTFIL (1, OFILE, ISTAT)
         CALL INFILE (2, FILE(1), ISTAT)
         CALL RDCHAR (2, HEAD(1), K, ISTAT)
         CALL RDCHAR (2, HEAD(2), L, ISTAT)
         CALL RDCHAR (2, HEAD(3), LL, ISTAT)
         CALL CLFILE (2)
         IF ((K .GE. 3) .AND. (HEAD(1)(2:3) .EQ. 'NL')) THEN
            WRITE (1,115) HEAD(1)(1:K)
            IF (L .GE. 1) THEN
               WRITE (HEAD(2)(1:3),115) '  1'
            END IF
            WRITE (1,115) HEAD(2)(1:L)
            WRITE (1,115) HEAD(3)(1:LL)
         END IF
C
         DO IMASTR=1,NMASTR
            SUMCHI=0.
            SUMSHP=0.
            DO IFRM=1,NFRM
               IF (IFRM .LE. MIDFRM) THEN
                  J=IWHICH(IMASTR,IFRM)
               ELSE
                  J=IWHCH2(IMASTR,IFRM-MIDFRM)
               END IF
               IF (J .GT. 0) THEN
                  DATUM(1,IFRM)=M(J)
                  DATUM(2,IFRM)=AMIN1
     .                 (9.9999,SQRT(AMAX1(0., S(J))))
                  SUMCHI=SUMCHI+CHI(J)
                  SUMSHP=SUMSHP+SHARP(J)
               ELSE
                  DATUM(1,IFRM)=99.9999
                  DATUM(2,IFRM)=9.9999
               END IF
            END DO
            SUMCHI=SUMCHI/NOBS(IMASTR)
            SUMSHP=SUMSHP/NOBS(IMASTR)
            POSIT1 = RNDOFF(XM(IMASTR), 9, 3)
            POSIT2 = RNDOFF(YM(IMASTR), 9, 3)
            WRITE (1,111) IDMAST(IMASTR), POSIT1, POSIT2,
     .            ((DATUM(J,I), J=1,2), I=1,NFRM), SUMCHI, SUMSHP
         END DO
         CALL CLFILE (1)
      END IF
C
      CALL GETYN ('A file with the new transformations?', ANSWER)
      IF (ANSWER .EQ. 'E') THEN
         CALL BYEBYE
      ELSE IF (ANSWER .EQ. 'Y') THEN
         OFILE = SWITCH(LFILE, CASE('.mch'))
         CALL GETNAM ('Output file name:', OFILE)
         OFILE = EXTEND(OFILE, 'mch')
         CALL OUTFIL (1, OFILE, ISTAT)
         DO I=1,NFRM
            IF (MODE .EQ. 4) THEN
               CON(6,I)=FLIP(I)*CON(3,I)
               CON(5,I)=-FLIP(I)*CON(4,I)
            END IF
C
            OUT(1) = DRNDFF(CON(1,I), 10, 4)
            OUT(2) = DRNDFF(CON(2,I), 10, 4)
            DO J=3,6
               OUT(J) = DRNDFF(CON(J,I), 12, 9)
            END DO
C
            IF (MODE .LE. 6) THEN
               WRITE (1,109) FILE(I), (OUT(J), J=1,6), 
     .              DMAG(I), SQRT(SMAG(I))
  109          FORMAT (1X, '''', A, '''', 2A10, 4A12, F9.3, 
     .              F8.4, 14A12)
            ELSE
               DO J=7,MODE
                  OUT(J) = DRNDFF(CON(J,I), 12, 9)
               END DO
               WRITE (1,109) FILE(I), (OUT(J), J=1,6), 
     .              DMAG(I), SQRT(SMAG(I)), (OUT(J), J=7,MODE)
            END IF
         END DO
         CALL CLFILE (1)
      END IF
C
      IF (.NOT. TFR) GO TO 9500
      CALL GETYN ('A file with the transfer table?', ANSWER)
      IF (ANSWER .EQ. 'E') THEN
         CALL BYEBYE
      ELSE IF (ANSWER .EQ. 'Y') THEN
         OFILE = SWITCH(LFILE, CASE('.tfr'))
         CALL GETNAM ('Output file name:', OFILE)
         OFILE = EXTEND(OFILE, 'tfr')
         CALL OUTFIL (1, OFILE, ISTAT)
         WRITE (1,112) (FILE(I), 99.9999, 9.9999, I=1,NFRM)
  112    FORMAT (1X, A30, 2F9.4)
         WRITE (1,113)
  113    FORMAT (1X, 30('='))
         L = (NFRM+17)/18
         DO IMASTR=1,NMASTR
            DO IFRM=1,MIDFRM
               NOK(IFRM) = IWHICH(IMASTR,IFRM)
            END DO
            DO IFRM=MIDFRM+1,NFRM
               NOK(IFRM) = IWHCH2(IMASTR,IFRM-MIDFRM)
            END DO
            DO K=1,L
               I = (K-1)*18+1
               J = MIN0(NFRM, 18*K)
               POSIT1 = RNDOFF(XM(IMASTR), 9, 3)
               POSIT2 = RNDOFF(YM(IMASTR), 9, 3)
               WRITE (1,114) IDMAST(IMASTR), POSIT1, POSIT2,
     .              (MAX0(0,NOK(IFRM)-LASTAR(IFRM-1)), 
     .              IFRM=I,J)
  114          FORMAT (I7, 2A9, 18I7)
            END DO
         END DO
         CALL CLFILE (1)
      END IF
C
 9500 CALL GETYN ('Individual .COO files?', ANSWER)
      IF (ANSWER .EQ. 'E') THEN
         CALL BYEBYE
      ELSE IF (ANSWER .EQ. 'Y') THEN
         CALL TBLANK
         DO IFRM=1,NFRM
            CALL INFILE (1, FILE(IFRM), ISTAT)
            CALL RDCHAR (1, HEAD(1), K, ISTAT)
            CALL RDCHAR (1, HEAD(2), L, ISTAT)
            CALL RDCHAR (1, HEAD(3), LL, ISTAT)
            CALL CLFILE (1)
            OFILE = SWITCH(FILE(IFRM), CASE('.coo'))
            CALL OUTFIL (1, OFILE, ISTAT)
            WRITE (TEXT,6) 0, OFILE
    6       FORMAT (I7, 3X, A)
            CALL OVRWRT (TEXT(1:40), 4)
            IF ((K .GE. 3) .AND. (HEAD(1)(2:3) .EQ. 'NL')
     .           .AND. (OFILE .NE. 'APPEND')) THEN
               WRITE (1,115) HEAD(1)(1:K)
  115          FORMAT (A)
               IF (L .GE. 1) THEN
                  WRITE (HEAD(2)(1:3),115) '  1'
               END IF
               WRITE (1,115) HEAD(2)(1:L)
               WRITE (1,115) HEAD(3)(1:LL)
            END IF
C
            RCOL(IFRM) = RCOL(IFRM)+0.5
            RROW(IFRM) = RROW(IFRM)+0.5
            K = 0
            DO IMASTR=1,NMASTR
C
               CALL TRFM (NOC(1,IFRM), RCOL(1), RROW(1), FLIP(IFRM), 
     .              MODE, XM(IMASTR), YM(IMASTR), XX, YY)
C
               IF (HEAD(1)(2:3) .EQ. 'NL') THEN
                  IF ((XX .GE. 0.5) .AND. (XX .LE. RCOL(IFRM)) .AND. 
     .                 (YY .GE. 0.5) .AND. (YY .LE. RROW(IFRM))) THEN
                     K = K+1
                     IF (MOD(K,200) .EQ. 0) THEN
                        WRITE (TEXT,6) K, OFILE
                        CALL OVRWRT (TEXT(1:40), 2)
                     END IF
                     POSIT1 = RNDOFF(XX, 9, 3)
                     POSIT2 = RNDOFF(YY, 9, 3)
                     STRNG = RNDOFF(MM(IMASTR)-DMAG(IFRM), 9, 3)
                     WRITE (1,116) IDMAST(IMASTR), POSIT1, POSIT2,
     .                    STRNG, SQRT(SM(IMASTR))
                  END IF
               ELSE
                  K = K+1
                  IF (MOD(K,200) .EQ. 0) THEN
                     WRITE (TEXT,6) K, OFILE
                     CALL OVRWRT (TEXT(1:40), 2)
                  END IF
                  POSIT1 = RNDOFF(XX, 9, 3)
                  POSIT2 = RNDOFF(YY, 9, 3)
                  STRNG = RNDOFF(MM(IMASTR)-DMAG(IFRM), 9, 3)
                  WRITE (1,116) IDMAST(IMASTR), POSIT1, POSIT2,
     .                 STRNG, SQRT(SM(IMASTR))
  116             FORMAT (I7, 3A9, F9.4)
               END IF
            END DO
            WRITE (TEXT,6) K, OFILE
            CALL OVRWRT (TEXT(1:40), 2)
            CALL CLFILE (1)
         END DO
         CALL TBLANK
      END IF
C
      IF (.NOT. TFR) GO TO 9900
      CALL GETYN ('Simply transfer star IDs?', ANSWER)
      IF (ANSWER .EQ. 'E') THEN
         CALL BYEBYE
      ELSE IF (ANSWER .EQ. 'Y') THEN
         DO IFRM=1,NFRM
            CALL INFILE (1, FILE(IFRM), ISTAT)
            CALL RDCHAR (1, HEAD(1), K, ISTAT)
            CALL RDCHAR (1, HEAD(2), L, ISTAT)
            IF (HEAD(1)(2:3) .NE. 'NL') THEN 
               CALL CLFILE (1)
               CALL INFILE (1, FILE(IFRM), ISTAT)
               NL = 1
            ELSE
               READ (1,*)
               READ (HEAD(2), *) NL
            END IF
            IF (NL .EQ. 2) READ (1,*)
C
            I=0
 8101       I=I+1
 8102       CALL RDCHAR (1, TEXT, III, ISTAT)
            IF (ISTAT .GT. 0) GO TO 8103
            IF (NL .EQ. 2) READ (1,*,END=8103)
            IF (NL .EQ. 2) READ (1,*,END=8103)
            IF (ISTAT .LT. 0) GO TO 8102
            READ (TEXT,*,ERR=8102) J, XX, YY, ZZ
            IF ((J .LE. 0) .OR. (ABS(ZZ) .GT. 50.)) GO TO 8102
            NLINE(I) = III
            DO J=1,III
               LINE(J,I) = ICHAR(TEXT(J:J))
            END DO
c           READ (TEXT,117,END=8103) (LINE(J,I), J=1,III)
  117       FORMAT (132A1)
            GO TO 8101
C
 8103       CALL CLFILE (1)
            OFILE=SWITCH(FILE(IFRM), CASE('.mtr'))
            CALL OUTFIL (1, OFILE, ISTAT)
            WRITE (TEXT,6) 0, OFILE
            CALL OVRWRT (TEXT(1:40), 4)
            IF (HEAD(1)(2:3) .EQ. 'NL') THEN
               WRITE (1,115) HEAD(1)(1:K)
               WRITE (1,115) HEAD(2)(1:L)
               WRITE (1,*)
            END IF
C
            K = 0
            DO IMASTR=1,NMASTR
               IF (IFRM .LE. MIDFRM) THEN
                  J=IWHICH(IMASTR,IFRM)
               ELSE
                  J=IWHCH2(IMASTR,IFRM-MIDFRM)
               END IF
               IF (J .GT. 0) THEN 
                  I = J - LASTAR(IFRM-1)
                  CALL TRFM (CON(1,IFRM), RCOL(IFRM), RROW(IFRM), 
     .                 FLIP(IFRM), MODE, X(J), Y(J), XX, YY)
C
C Find the first non-blank.  This is the beginning of the ID number.
C
                  LL = 0
 8104             LL = LL+1
                  IF (LINE(LL,I) .EQ. 32) THEN
                     GO TO 8104
                  ELSE IF (LINE(LL,I) .LT. 32) THEN
                     LINE(LL,I) = 32
                     GO TO 8104
                  END IF
C
C Now find the first non-digit.  This is the beginning of the rest of
C the line.
C
 8105             LL = LL+1
                  III = LINE(LL,I)
                  IF ((III .LT. 48) .OR. (III .GT. 57)) GO TO 8106
                  GO TO 8105
C
 8106             CONTINUE
                  WRITE (TEXT,117) (CHAR(LINE(L,I)), L=1,LL-1)
                  READ (TEXT(1:LL-1),*) III
                  TEXT = ' '
                  WRITE (TEXT,11) IDMAST(IMASTR), 
     .                 (CHAR(LINE(L,I)), L=LL,NLINE(I))
   11             FORMAT (I7, 127A1)
                  XX = XX-XM(IMASTR)
                  YY = YY-YM(IMASTR)
                  WRITE (1,118) TEXT(1:LENGTH(TEXT)), 
     .                 III, XX, YY,
     .                 M(J)+DMAG(IFRM)-MM(IMASTR),
     .                 SQRT(XX**2+YY**2)
  118             FORMAT (A, I9, 4F9.3)
                  K = K+1
               END IF
               IF (MOD(IMASTR,1000) .EQ. 0) THEN
                  WRITE (TEXT,6) IMASTR, OFILE
                  CALL OVRWRT (TEXT(1:40), 2)
               END IF
            END DO
            WRITE (TEXT,6) K, OFILE
            CALL OVRWRT (TEXT(1:40), 2)
            CALL CLFILE (1)
         END DO
         CALL TBLANK
      END IF
C
 9900 CONTINUE
      CALL BYEBYE
      END!
C
C##########################################################################
C
      INTEGER*4 FUNCTION  IBNRY  (X, N, F)
C
C Locate the first item in a sorted list whose value is greater
C than fraction F of the largest value in the list.
C
      REAL X(*)
      T = F*X(N)                                          ! Target value
C
      IF (X(1) .GT. T) THEN
         IBNRY = 1
         RETURN
      ELSE IF (X(N) .LT. T) THEN
         IBNRY = 0
         RETURN
      END IF
C
      K = MAX0(1, NINT(0.5*N))                             ! Initial step
      IBNRY = K
C
 1000 K = MAX0(1, (K+1)/2)
      IF (X(IBNRY) .LT. T) THEN
         IBNRY = MIN0(N, IBNRY+K)
         GO TO 1000
      END IF
      IF (X(IBNRY-1) .GT. T) THEN
         IBNRY = MAX0(2, IBNRY-K)
         GO TO 1000
      END IF
      RETURN
      END!
C
C##########################################################################
C
      SUBROUTINE  RDFRMS  (FILE, TFR, CHIMAX, SHPMIN, SHPMAX, MAGMAX, 
     .   SIGMAX, NFRM, MAXMTR, WORK, MAXSTR, IDMAST, LASTAR, RCOL, RROW)
      IMPLICIT NONE
C
      REAL CHIMAX, MAGMAX, SHPMIN, SHPMAX, SIGMAX, DM, RCOL(*), RROW(*)
      INTEGER J, K, L, ID, IFRM, ISTAR, IMAX, MAXMTR, MAXSTR, 
     .     NL, NFRM, NMAX, ISTAT, NCOL, NROW, LENGTH
C
      CHARACTER*40 FILE(NFRM)
      CHARACTER*140 TEXT
      REAL WORK(MAXSTR,*)
      INTEGER IDMAST(MAXMTR), LASTAR(0:NFRM)
      LOGICAL ALS, TESTS, TFR, STC
      TESTS = .NOT. TFR
C
C Read data from all frames into the working array.
C
      LASTAR(0) = 0
      ISTAR = 0
      NMAX = 0
C     CALL OVRWRT (' ', 4)
      DO IFRM=1,NFRM
         IMAX = 0
         CALL INFILE (1, FILE(IFRM), ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Error opening input file '//FILE(IFRM))
            CALL BYEBYE
         END IF
         L = LENGTH(FILE(IFRM))
         IF (FILE(IFRM)(L-2:L) .EQ. 'stc') THEN
            STC = .TRUE.
         ELSE
            STC = .FALSE.
         END IF
         IF (FILE(IFRM)(1:4) .EQ. 'dss:') THEN
            STC = .TRUE.
         ELSE
            STC = .FALSE.
         END IF
C
         IF (TESTS) THEN
            ALS = .TRUE.
            IF (FILE(IFRM)(L-2:L) .EQ. 'stc') ALS = .FALSE.
            IF (FILE(IFRM)(L-2:L) .EQ. 'pds') ALS = .FALSE.
            IF (FILE(IFRM)(L-2:L) .EQ. 'fnl') ALS = .FALSE.
            IF (FILE(IFRM)(L-2:L) .EQ. 'dss') ALS = .FALSE.
         ELSE
            ALS = .FALSE.
         END IF
C
         NL = -1
         WRITE (TEXT,6) IFRM, FILE(IFRM)
    6    FORMAT ('  Reading  ', I4, 3X, A)
         CALL OVRWRT (TEXT(1:47), 2)
         CALL RDHEAD (1, NL, NCOL, NROW, DM, DM, DM, DM, DM, DM, DM)
         IF (NCOL .LE. 0) THEN
            CALL TBLANK 
            CALL STUPID ('Error reading file header.')
            CALL CLFILE (1)
            CALL OOPS
         END IF
         RCOL(IFRM) = REAL(NCOL)
         RROW(IFRM) = REAL(NROW)
 1010    ISTAR = ISTAR+1
 1020    CONTINUE
         IF (NL .EQ. 2) THEN
            READ (1,*,END=1090,ERR=1020) ID, (WORK(ISTAR,J), J=1,3)
            IF (ID .LE. 0) GO TO 1020
            READ (1,101,ERR=1020) WORK(ISTAR,4)
  101       FORMAT (26X, F8.4)
            WORK(ISTAR,5) = 0.
            WORK(ISTAR,6) = 0.
         ELSE
            CALL RDCHAR (1, TEXT, K, ISTAT)
            IF (ISTAT .GE. 1) GO TO 1090
            IF (K .LT. 9) GO TO 1020
            IF (TEXT(1:1) .EQ. '0') GO TO 1020
            IF (TEXT(1:1) .EQ. 'C') GO TO 1020
            IF (TEXT(K:K) .EQ. '#') GO TO 1020
            IF (TEXT(K:K) .EQ. '?') K = K-1
            TEXT = TEXT(1:K)//' 0 0 0 0 0'
            READ (TEXT,*,IOSTAT=L,END=1020) ID, (WORK(ISTAR,J), J=1,4),
     .           DM, DM, (WORK(ISTAR,J), J=5,6)
            IF (L .NE. 0) THEN
               CALL TBLANK
               CALL STUPID ('Error reading data from '//FILE(IFRM))
               WRITE (6,*) TEXT(1:K)
               CALL CLFILE (1)
               CALL OOPS
            END IF
            IF (ID .LE. 0) GO TO 1020
            IF (ALS) THEN
               IF (WORK(ISTAR,3) .GT. MAGMAX) GO TO 1020
               IF (WORK(ISTAR,4) .GT. SIGMAX) GO TO 1020
               IF (WORK(ISTAR,5) .GT. CHIMAX) GO TO 1020
               IF (WORK(ISTAR,6) .LT. SHPMIN) GO TO 1020
               IF (WORK(ISTAR,6) .GT. SHPMAX) GO TO 1020
            END IF
         END IF
         IF (ABS(WORK(ISTAR,3)) .GT. 50.) GO TO 1020
         IMAX = IMAX+1
         IF (STC) THEN
            WORK(ISTAR,4) = 0.25
         ELSE
            WORK(ISTAR,4) = WORK(ISTAR,4)**2 + 3.E-7
         END IF
         IF (IFRM .EQ. 1) IDMAST(ISTAR)=ID
         IF (ISTAR .LT. MAXSTR) THEN
            GO TO 1010
         ELSE
            WRITE (6,690) FILE(IFRM)
  690       FORMAT (/' Star limit reached in ', A/)
            CALL BYEBYE
         END IF
 1090    ISTAR = ISTAR-1
         IF ((IFRM .EQ. 1) .AND. (ISTAR .GT. MAXMTR)) THEN
            CALL STUPID ('Too many stars in the first input file!')
            CALL CLFILE (1)
            CALL OOPS
         END IF
C
         LASTAR(IFRM) = ISTAR
         IF (LASTAR(IFRM) .LE. LASTAR(IFRM-1)) THEN
            CALL STUPID ('No stars in '//FILE(IFRM))
            CALL CLFILE (1)
            CALL OOPS
         END IF
         IF (IMAX .GT. NMAX) NMAX=IMAX
         CALL CLFILE (1)
      END DO
      MAXSTR = NMAX
      RETURN
      END!
C
C#########################################################################
C
      SUBROUTINE  REPACK  (WORK, MAXSTR, NTOT)
      IMPLICIT NONE
C
      INTEGER I, J, K, M, N, MAXSTR, NTOT
C
      REAL WORK(*)
C
      CALL OVRWRT ('Repacking memory                              ', 2)
      DO J=2,6
         M = NTOT*(J-1)
         N = MAXSTR*(J-1)
         DO I=1,NTOT
            K = M+I
            WORK(K) = WORK(N+I)
         END DO
      END DO
C
      RETURN
      END!
C
C########################################################################
C
      INTEGER FUNCTION  ICLOSE  (XX, YY, RADIUS, RADSQ, XM, YM, 
     .     NOBS, LAST, IFIRST, RMIN)
      PARAMETER (MINY=-11 384, MAXY=15 000)
      REAL XM(*), YM(*), RMIN(*)
      INTEGER LAST(MINY:MAXY), NOBS(*)
C
      ICLOSE=0
      L=MAX0(MINY, MIN0(MAXY, INT(YY-RADIUS)-2))
      IF (L .EQ. MINY) THEN
         L=IFIRST
      ELSE
         L=LAST(L)+1
      END IF
      M=LAST(MAX0(MINY, MIN0(MAXY, INT(YY+RADIUS)+1)))
      RR=RADSQ
      DO IMASTR=L,M
        DX=XX-XM(IMASTR)
        IF (ABS(DX) .LE. RADIUS) THEN
          RSQ=DX**2+(YY-YM(IMASTR))**2
          IF (RSQ .LT. RADSQ) THEN
            RSQ = RSQ/REAL(NOBS(IMASTR)) 
            IF ((RSQ .LT. RR) .AND. 
     .           (RSQ .LT. 0.99999*RMIN(IMASTR))) THEN
              RR=RSQ
              ICLOSE=IMASTR
            END IF
          END IF
        END IF
      END DO
      IF (ICLOSE .NE. 0) THEN
        RMIN(ICLOSE)=RR
      END IF
      RETURN
      END
C
C########################################################################
C
      SUBROUTINE  TRFM  (CON, RCOL, RROW, FLIP, MODE, X, Y, XX, YY)
      IMPLICIT NONE
      INTEGER MODE
      DOUBLE PRECISION CON(MODE)
      REAL X, Y, XX, YY, X2, Y2, FLIP, XS, XY, YS, RCOL, RROW
      IF (MODE .EQ. 2) then
         XX=X+CON(1)
         YY=Y+CON(2)
      ELSE IF (MODE .EQ. 4) THEN
         XX=CON(1)+CON(3)*X-FLIP*CON(4)*Y
         YY=CON(2)+CON(4)*X+FLIP*CON(3)*Y
      ELSE IF (MODE .GE. 6) THEN
         XX=CON(1)+CON(3)*X+CON(5)*Y
         YY=CON(2)+CON(4)*X+CON(6)*Y
         IF (MODE .GE. 7) THEN
            XS=2.*(X-1.)/(RCOL-1.)-1.
            YS=2.*(Y-1.)/(RROW-1.)-1.
            XY=XS*YS
            X2=1.5*XS**2-0.5
            Y2=1.5*YS**2-0.5
            XX=XX+
     .           CON(7)*X2+
     .           CON(9)*XY+
     .           CON(11)*Y2
            YY=YY+
     .           CON(8)*X2+
     .           CON(10)*XY+
     .           CON(12)*Y2
            IF (MODE .GE. 13) THEN
               XX = XX+
     .              (CON(13)*XS+CON(15)*YS)*X2+
     .              (CON(17)*XS+CON(19)*YS)*Y2
               YY = YY+
     .              (CON(14)*XS+CON(16)*YS)*X2+
     .              (CON(18)*XS+CON(20)*YS)*Y2
            END IF
         END IF
      ELSE
         CALL STUPID ('TRFM MODE ERROR')
         CALL BYEBYE
      END IF
      RETURN
      END
