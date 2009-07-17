C
C     Official DAO version:  2004 January 6
C
      PARAMETER (MAXAP=12, MAXSTR=250, MAXFIL=20000, MTERM=5, NOPT=14)
      CHARACTER LINE*132, S1*9, S2*9, S3*9, RNDOFF*9
      CHARACTER TABLE*60, FILE(MAXFIL)*60, SWITCH*60, EXTEND*60, 
     .     CASE*60, NAME*60, AFILE(MAXFIL)*60
      CHARACTER LBL(NOPT)*26
CD    CHARACTER*1 PLOT
      REAL AIRIN(MAXFIL), AIR(MAXFIL), SKYBAR(MAXFIL)
      DOUBLE PRECISION SUMR, SUMY, SUMD, SUMW, OLD, DBLE
      DOUBLE PRECISION U(MTERM,MTERM), V(MTERM), DP(MTERM)
      DOUBLE PRECISION DMAG, DM(MAXAP), DDDR(MAXAP), T(MTERM,MAXAP)
      DOUBLE PRECISION P(MTERM), POLD(MTERM), PCLAMP(MTERM)
      REAL MAG(MAXAP,MAXSTR*MAXFIL), SIG(MAXAP,MAXSTR*MAXFIL)
      REAL W(MAXAP), R(MAXAP), RO(MAXFIL), RBAR(MAXAP), THEO(MAXAP)
      REAL OBS(MAXAP), WOBS(MAXAP), ADOPT(MAXAP), WADO(MAXAP), 
     .     CUM(MAXAP+1), WCUM(MAXAP+1), SUMCUM(MAXAP+1), SEE(0:4)
      REAL AVE(MAXAP), RESID(MAXAP), RESSQ(MAXAP), WR(MAXAP)
      REAL PAR(NOPT), PMIN(NOPT), PMAX(NOPT)
      REAL X(MAXSTR*MAXFIL), Y(MAXSTR*MAXFIL), SKY(MAXSTR*MAXFIL)
      INTEGER ID(MAXSTR*MAXFIL), NAP(MAXSTR*MAXFIL)
      INTEGER IFILE(MAXSTR*MAXFIL), NCH(MAXFIL), LAST(0:MAXFIL)
      LOGICAL HST
CD    REAL XX1, XX2, YY1, YY2, GX1, GX2, GY1, GY2                ! mongo
CD    INTEGER LX1, LX2, LY1, LY2, IDEV                           ! mongo
CD    COMMON /MONGOPAR/ XX1, XX2, YY1, YY2, GX1, GX2, GY1, GY2,  ! mongo
CD   .     LX1, LX2, LY1, LY2, XP, YP, EXPAND, ANGLE, LTYPE,     ! mongo
CD   .     LWEIGHT, CHEIGHT, CWIDTH, CDEF, PDEF, COFF, TERMOUT,  ! mongo
CD   .     XYSWAPPED, NUMDEV                                     ! mongo
      DATA PAR /MAXAP*0., 2*1./, PMIN /2*1.E-30, MAXAP*0./, 
     .     PMAX/MAXAP*100., 2*100./
      DATA LBL/' A1  RADIUS OF APERTURE  1',
     .         ' A2  RADIUS OF APERTURE  2',
     .         ' A3  RADIUS OF APERTURE  3',
     .         ' A4  RADIUS OF APERTURE  4',
     .         ' A5  RADIUS OF APERTURE  5',
     .         ' A6  RADIUS OF APERTURE  6',
     .         ' A7  RADIUS OF APERTURE  7',
     .         ' A8  RADIUS OF APERTURE  8',
     .         ' A9  RADIUS OF APERTURE  9',
     .         ' AA  RADIUS OF APERTURE 10',
     .         ' AB  RADIUS OF APERTURE 11',
     .         ' AC  RADIUS OF APERTURE 12',
     .         ' IS       INNER SKY RADIUS',
     .         ' OS       OUTER SKY RADIUS'/
      DATA P /1.05, 0.1, 0.5, 0.9, 0./ !xyz
      DATA OLD /0.D0/, POLD/MTERM*0./, 
     .     PCLAMP / 0.1, 0.1, 0.1, 0.1, 0.1/ 
      DATA SUMCUM / MAXAP*0., 0. /
      DATA IDEV/3/, SCALE /0.1/
      DATA HST /.FALSE./
      CALL FABORT
      IPOW = 1
      D0 = 1.5
      LAST(0) = 0
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Ascertain the name of the aperture photometry parameter table, and
C read it in.  Then set up all necessary variables for the forthcoming
C reductions. Finally, identify and open the input and output files.
C
      L = MAXAP+1
      PMIN(L) = 1.
      CALL TBLANK
      TABLE = CASE('photo.opt')
      CALL GETNAM ('File with aperture radii:', TABLE)
      CALL OPTION (TABLE, NOPT, LBL, PAR, PMIN, PMAX, 'PHO>', ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Unable to read file with aperture radii.')
         CALL OOPS
      END IF
C
C Count up the number of apertures that will be used.  The first zero or
C negative number encountered terminates the list.
C
      DO I=1,MAXAP
         IF (PAR(I) .LE. 0.0) GO TO 850
         NDIFF = I
         R(I) = PAR(I)
      END DO
  850 TABLE = ' '
      CALL GETNAM ('File with airmasses:', TABLE)
      IF (TABLE .EQ. 'GIVE UP') CALL BYEBYE
      NAIR = 0
      IF (TABLE .NE. 'END-OF-FILE') THEN
         TABLE = EXTEND(TABLE, CASE('inf'))
         CALL INFILE (1, TABLE, ISTAT)
         IF (ISTAT .NE. 0) THEN
            CALL STUPID ('Unable to open '//TABLE)
            TABLE = 'GIVE UP'
            GO TO 850
         END IF
C
  860    NAIR = NAIR+1
         READ (1,86,END=890) AFILE(NAIR), AIRIN(NAIR)
   86    FORMAT (1X, A30, 10X, F7.3)
         AFILE(NAIR) = SWITCH (AFILE(NAIR), ' ')
         GO TO 860
  890    NAIR = NAIR-1
      ELSE
         CALL TBLANK
      END IF
      TABLE = ' '
  894 CALL GETNAM ('List of input files:', TABLE)
      IF ((TABLE .EQ. 'END-OF-FILE') .OR.
     .      (TABLE .EQ. 'GIVE UP')) CALL OOPS
      TABLE = EXTEND(TABLE, CASE('lis'))
      CALL INFILE (1, TABLE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//TABLE)
         TABLE = 'GIVE UP'
         GO TO 894
      END IF
      NAME = SWITCH(TABLE, CASE('.gro'))
      CALL DELFIL (3, NAME, ISTAT)
      CALL OUTFIL (3, NAME, ISTAT)
      NAME = SWITCH(NAME, CASE('.see'))
  895 CALL DELFIL (9, NAME, ISTAT)
      CALL OUTFIL (9, NAME, ISTAT)
      NAME = SWITCH(NAME, CASE('.crl'))
      CALL DELFIL (10, NAME, ISTAT)
      CALL OUTFIL (10, NAME, ISTAT)
      CALL GETDAT ('Number of unknowns:', SIGLIM, 1)
      IF (SIGLIM .LE. 0) THEN
         TABLE = 'GIVE UP'
         GO TO 894
      END IF
      IF (NAIR .GT. 0) THEN
         J = MTERM
      ELSE
         J = MTERM-1
         P(MTERM) = 0.0D0
      END IF
      LTERM = MAX(1, MIN(J, NINT(SIGLIM)))
      IF (LTERM .LT. J) THEN
         IF (J-LTERM .EQ. 1) THEN
            CALL GETDAT ('Value for the remaining parameter:',
     .           CUM(J), 1)
         ELSE
            CALL GETDAT ('Values for remaining '//CHAR(48+J-LTERM)//
     .           ' parameters:', CUM(LTERM+1), J-LTERM)
         END IF
         IF (CUM(1) .LE. -1.E38) GO TO 895
         DO I=LTERM+1,J
            P(I) = DBLE(CUM(I))
         END DO
      END IF
C
C-----------------------------------------------------------------------
C
C Open input file.
C
  900 CALL GETDAT ('Maximum magnitude error:', SIGLIM, 1)
      IF (SIGLIM .LE. -1.E38) GO TO 850
CD    CALL GETYN ('Do you want to see the plots?', PLOT)
CD    IF (PLOT .EQ. 'E') GO TO 900
      CALL TBLANK
      NSTAR = 1
      NFILE = 1
 1000 CALL RDCHAR (1, FILE(NFILE), NCH(NFILE), ISTAT)
      IF (ISTAT .GT. 0) GO TO 3000
 1010 IF (FILE(NFILE)(1:1) .EQ. ' ') THEN
         FILE(NFILE) = FILE(NFILE)(2:30)//' '
         GO TO 1010
      END IF
C
      DO I=2,NCH(NFILE)
         IF (FILE(NFILE)(I:I) .EQ. ' ') THEN
            FILE(NFILE)(I:NCH(NFILE)) = ' '
            NCH(NFILE) = I-1
            GO TO 1050
         END IF
      END DO
 1050 TABLE = SWITCH(FILE(NFILE), ' ')
      FILE(NFILE) = EXTEND(FILE(NFILE), CASE('kap '))
      CALL INFILE (2, FILE(NFILE), ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening file '//FILE(NFILE))
         GO TO 1000
      END IF
      CALL CHECK (2,NL)
      IF (NL .NE. 2) THEN
         CALL STUPID ('This is not an aperture photometry file: '//
     .        FILE(NFILE))
         CALL OOPS
      END IF
C
      SKYBAR(NFILE) = 0.
      N = 0
 1100 READ (2,*,END=2000) ID(NSTAR), X(NSTAR), Y(NSTAR), 
     .     (MAG(I,NSTAR), I=1,NDIFF)
      IF (ID(NSTAR) .EQ. 0) GO TO 1100
      READ (2,*) SKY(NSTAR), S, SKEW, (SIG(I,NSTAR), I=1,NDIFF)
      IF (SIG(1,NSTAR) .GT. SIGLIM) GO TO 1100
C
      SKYBAR(NFILE) = SKYBAR(NFILE) + SKY(NSTAR)
      N = N+1
      NAP(NSTAR) = 1
      DO I=2,NDIFF
         IF ((MAG(I,NSTAR) .GT. 99.9) .OR. 
     .        (SIG(I,NSTAR) .GT. SIGLIM) .OR.
     .        ((MAG(I,NSTAR) .EQ. 0.0) .AND. (SIG(I,NSTAR) .EQ. 0.0)))
     .        GO TO 1120
         SIG(I,NSTAR) = SQRT(1.E-6 + SIG(I,NSTAR)**2)
         NAP(NSTAR) = I
      END DO
 1120 CONTINUE
C
      IFILE(NSTAR) = NFILE
      NSTAR = NSTAR+1
      IF (N .LT. MAXSTR) GO TO 1100
 2000 IF (N .LT. 1) THEN
         CALL STUPID ('*** WARNING ***  No useable stars in '//
     .        FILE(NFILE))
         GO TO 1000
      END IF
      CALL CLFILE (2)
      SKYBAR(NFILE) = SKYBAR(NFILE)/REAL(N)
      TABLE = SWITCH(FILE(NFILE), ' ')
      IF (NAIR .GT. 0) THEN
         DO I = 1,NAIR
            IF (TABLE .EQ. AFILE(I)) THEN
               AIR(NFILE) = AIRIN(I)
               GO TO 1090
            END IF
         END DO
         CALL STUPID ('No tabulated airmass for '//TABLE)
         CALL GETDAT ('Airmass:', AIR(NFILE), 1)
         IF (AIR(NFILE) .LT. 0.) THEN
            CALL OOPS
         END IF
 1090    AIR(NFILE) = AIR(NFILE) - 1.25
      ELSE
         AIR(NFILE) = 0.
      END IF
      RO(NFILE) = R(1) ! *0.5
      LAST(NFILE) = NSTAR-1
      NFILE = NFILE+1
      IF (NFILE .LE. MAXFIL) GO TO 1000
C
 3000 CALL CLFILE (1)
      NFILE = NFILE-1
      NSTAR = NSTAR-1
      CALL TBLANK
C
C At this point, the arrays MAG and SIG contain the raw magnitudes and
C standard errors of the aperture photometry for all the stars; the 
C array IFILE tells which of the input files each star belongs to.
C
C Now we will compute the magnitude differences:  MAG(1) will still be
C the magnitude in the first aperture, but now for each star MAG(2)
C will contain aperture 2 - aperture 1, MAG(3) will contain aperture 3 -
C aperture 2, and so on.  For simplicity, the raw standard error of the
C larger aperture in each case will be taken to be the standard error
C of the difference.
C
      DO I=1,NSTAR
         IF (NAP(I) .GT. 1) THEN
            DO J=NAP(I),2,-1
               MAG(J,I) = MAG(J,I)-MAG(J-1,I)
            END DO
         END IF
      END DO
      WRITE (LINE,601) 'A', 'B', 'C', 'D', 'E', 'sigma'
  601 FORMAT (1X, 5(9X, A1, 3X), 7X, A5)
      CALL OVRWRT (LINE(1:78), 1)
      NITER = 0
 3100 NITER = NITER+1
C
C Now improve the estimate of RO for each frame.
C
      SUMD = 0.D0
      SUMW = 0.D0
      SUMN = 0.
C
      N = 1
      IF ((NTERM .GE. 1) .AND. (GAIN .LE. 0.04)) THEN
         N = 2
         IF ((NTERM .GE. 2) .AND. (GAIN .LE. 0.016)) THEN
            N = 3
            IF ((NTERM .GE. 3) .AND. (GAIN .LE. 0.006)) THEN
               N = 4
               IF ((NTERM .GE. 4) .AND. (GAIN .LE. 0.0025)) N=5
            END IF
         END IF
      END IF
      NTERM = MIN(N, LTERM)
C
      DO I=1,NTERM
         V(I) = 0.D0
         DO J=1,NTERM
            U(I,J) = 0.D0
         END DO
      END DO
C
      DO 4900 K=1,NFILE
C     IF ((NITER .EQ. 1) .AND. (K .NE. 1)) RO(K) = RO(K-1)
C
C Update the radius estimate for this frame.
C
      ROLD = 0.0
      RCLAMP = R(1)/2.
 4870 SUMR = 0.D0
      SUMY = 0.D0
      DO J=2,NDIFF
         DM(J) = SNGL(DMAG(R(J-1), R(J), AIR(K), RO(K), P))
         DDDR(J) = SNGL( 5.D2 * (
     .       DMAG(R(J-1), R(J), AIR(K), RO(K)-0.001, P) -
     .       DMAG(R(J-1), R(J), AIR(K), RO(K)+0.001, P)))
      END DO
C
      DO 4880 I=LAST(K-1)+1, LAST(K)
         IF (NAP(I) .LE. 1) GO TO 4880
         W(1) = 1.
         DO J=2,NAP(I)
            DIFF = MAG(J,I) - DM(J)
            W(J) = 1./( 1. + ABS( DIFF/(D0*SIG(J,I)) )**IPOW )
            W(J) = MIN( W(J), W(J-1) )
            WT = W(J)/SIG(J,I)**2
            WT = (2./J)*WT
            SUMR = SUMR + WT*DIFF*DDDR(J)
            SUMY = SUMY + WT*DDDR(J)**2
         END DO
 4880 CONTINUE
C
      IF (SUMY .GT. 0.) THEN
         DR = SUMR/SUMY
         IF (DR*ROLD .LT. 0.) THEN
            RCLAMP = 0.7*RCLAMP
         END IF
         SUMY = AMIN1(RO(K)-0.2, R(NDIFF)-RO(K), RCLAMP)
         IF (SUMY .GT. 0.) THEN
            DR = DR/(1.+ABS(DR)/SUMY)
            RO(K) = RO(K) - DR
            ROLD = DR
            IF (ABS(DR/RO(K)) .GT. 1.E-3) GO TO 4870
         END IF
      END IF
C
C With the new estimate for the RO of the frame, new scatter and ---
C if appropriate, the contributions to the corrections for A, B, C, 
C and D.
C
      DO J=2,NDIFF
         DM(J) = SNGL(DMAG(R(J-1), R(J), AIR(K), RO(K), P))
         DO I=1,NTERM
c           P(I) = P(I) - 0.01
            P(I) = P(I) - 0.001
            T(I,J) = DMAG(R(J-1), R(J), AIR(K), RO(K), P)
c           P(I) = P(I) + 0.02
            P(I) = P(I) + 0.002
c           T(I,J) = 50.D0 *
            T(I,J) = 500.D0 *
     .               (T(I,J) - DMAG(R(J-1), R(J), AIR(K), RO(K), P) )
c           P(I) = P(I) - 0.01
            P(I) = P(I) - 0.001
         END DO
      END DO
C
      DO 4890 I=LAST(K-1)+1, LAST(K)
         IF (NAP(I) .LE. 1) GO TO 4890
         W(1) = 1.
         DO J=2,NAP(I)
            DIFF = MAG(J,I) - DM(J)
            DR = ABS(DIFF/(D0*SIG(J,I)))
            IF (DR .LT. 1.E10) THEN
               W(J) = 1./( 1. + ABS( DIFF/(D0*SIG(J,I)) )**IPOW )
               W(J) = MIN( W(J), W(J-1) )
               WT = W(J)/SIG(J,I)**2
               SUMD = SUMD+WT*DIFF**2
               SUMW = SUMW+WT
               SUMN = SUMN+1.
               DO L=1,NTERM
                  V(L) = V(L) + WT*DIFF*T(L,J)
                  DO M=1,NTERM
                     U(L,M) = U(L,M) + WT*T(L,J)*T(M,J)
                  END DO
               END DO
            END IF
         END DO
 4890 CONTINUE
 4900 CONTINUE
      CALL DINVRS (U, MTERM, NTERM, IFLAG)
      CALL DVMUL (U, MTERM, NTERM, V, DP)
C
      DO I=1,NTERM
         IF (SNGL(DP(I))*POLD(I) .LT. 0.) PCLAMP(I) = 0.7*PCLAMP(I)
         POLD(I) = DP(I)
      END DO
C
      P(1) = P(1) - DP(1)/(1.+ABS(DP(1)/(PCLAMP(1)*(P(1)-1.))))
      IF (NTERM .GE. 2) THEN
        P(2) = P(2) - DP(2) /
     .       (1.+ABS(DP(2)/(PCLAMP(2)*P(2)*(1.-P(2)))))
        IF (NTERM .GE. 3) THEN
          P(3) = P(3) - DP(3) /
     .         (1.+ABS(DP(3)/(PCLAMP(3)*P(3)*(1.-P(3)))))
          IF (IPOW .EQ. 1) THEN
             IPOW = 2
             D0 = 2.
             PCLAMP(1) = 0.1
             PCLAMP(2) = 0.1
          END IF
          IF (NTERM .GE. 4) THEN
            P(4) = P(4) - DP(4) /
     .           (1.+ABS(DP(4)/(PCLAMP(4)*P(4))))
            IF (NTERM .GE. 5) THEN
              P(5) = P(5) - DP(5) / 
     .             (1.+ABS(DP(5)/(PCLAMP(5)*P(2))))
            END IF
          END IF
        END IF
      END IF
C
      SUMN = DSQRT(SUMD/(SUMN-6.))
      IF (SUMW .GT. 0.) THEN
         SUMD = DSQRT(SUMD/SUMW)
      ELSE
         SUMD = 0.
      END IF
      WRITE (LINE,6662) P, SNGL(SUMD)
 6662 FORMAT (1X, 6F13.6)
      CALL OVRWRT (LINE(1:79), 3)
      DR = (OLD+SUMD)/2.
      IF (DR .GT. 0.) THEN
         GAIN = ABS(OLD-SUMD)/DR
      ELSE
         GAIN = 0.
      END IF
      OLD = SUMD
      IF ((NTERM .LT. LTERM) .OR. (GAIN .GT. 0.001)) THEN
         GO TO 3100
      ELSE IF (IPOW .LE. 2) THEN
         IPOW = 3
         D0 = 2.
         DO I=1,4
            PCLAMP(I) = 0.1
         END DO
         GO TO 3100
      END IF
C
      CALL OVRWRT (' ', 4)
      NAME = SWITCH(NAME, ' ')
      CALL STUPID (NAME(1:LENGTH(NAME))//' converged.')
C
      WRITE (3,304) P
  304 FORMAT (2X, '--> A, B, C, D, E =', 5F13.6)
      WRITE (3,305) (SUMN*DSQRT(U(J,J)), J=1,5)
  305 FORMAT (2X, '-->              +-', 5F13.6)
      WRITE (3,306)
      WRITE (3,306) '     Radii', (R(J), J=1,NDIFF)
  306 FORMAT (2X, '-->', 9X, A10, 12F9.4)
      WRITE (3,306)
      WRITE (6,6663) P
 6663 FORMAT (1X, 5F13.6)
      WRITE (6,6663) (SUMN*DSQRT(U(J,J)), J=1,NTERM)
      CALL TBLANK
      SUMD = 0.
      SUMR = 0.
      SUMRSQ = 0.
      RMIN = 1.E38
      RMAX = -1.E38
      DO K=1,NFILE
         SUMR = SUMR+RO(K)
         SUMX = SUMX+AIR(K)
         IF (RO(K) .LT. RMIN) RMIN = RO(K)
         IF (RO(K) .GT. RMAX) RMAX = RO(K)
         DO J=2,NDIFF
            THEO(J) = SNGL(DMAG(R(J-1), R(J), AIR(K), RO(K), P))
            IF (THEO(J).LT.SUMD) SUMD=THEO(J)      ! Greatest difference
         END DO
      END DO
      SUMR = SUMR/NFILE
      SUMX = SUMX/NFILE
      DO K=1,NFILE
         SUMRSQ = SUMRSQ + (RO(K)-SUMR)**2
      END DO
      IF (NFILE .GT. 1) THEN
         WRITE (3,306) '   <Ro> = ', SUMR
         SUMRSQ = SQRT(SUMRSQ/(NFILE-1))
         WRITE (3,306) 'std dev = ', SUMRSQ
         WRITE (3,306) '    <X> = ', SUMX+1.25
         WRITE (3,306)
      END IF
C
      DO I=2,NDIFF
         RBAR(I) = 0.5*(R(I)+R(I-1))
         AVE(I) = 0.
         RESID(I) = 0.
         RESSQ(I) = 0.
         WR(I) = 0.
      END DO
C
      RMAX = 0.25*(RMAX-RMIN)
      OLD = 0.
      DO J=2,NDIFF
         DIFF = -0.05*OLD
         DO I=0,4
            DR = RMIN + I*RMAX
            SEE(I) = SNGL(DMAG(R(J-1), R(J), 0., DR, P))
            IF (SEE(I) .LT. OLD) THEN
               DIFF = SEE(I)
               OLD = DIFF
            END IF
         END DO
         WRITE (9,151) 0.5*(R(J)+R(J-1)), (SEE(I), I=0,4), 
     .        DIFF
  151    FORMAT (1X, 7F13.6)
      END DO
      CALL CLFILE (9)
C
CD    IF (PLOT .EQ. 'Y') THEN
CD       READ (5,*)
CD       CALL DEVICE (IDEV)
CD       CALL TSETUP
CD       CALL ERASE
CD       CALL SETLIM (R(1)-1., 1.3*SUMD, R(NDIFF), -0.3*SUMD)
CD       CALL BOX (1, 2)
CD       CALL SETLTYPE (2)
CD       DO I=0,4
CD          DR = RMIN + I*RMAX
CD          DO J=2,NDIFF
CD             THEO(J) = SNGL(DMAG(R(J-1), R(J), 0., DR, P))
CD          END DO
CD          CALL CONNECT (RBAR(2), THEO(2), NDIFF-1)
CD       END DO
CD       CALL TIDLE
CD       CALL TCLOSE
CD       CALL SETLTYPE (1)
CD       READ (5,666) PLOT
CD    END IF
C   
      DO K=1,NFILE
         CALL DELFIL (9, SWITCH(FILE(K), CASE('.poi')), ISTAT)
         CALL OUTFIL (9, SWITCH(FILE(K), CASE('.poi')), ISTAT)
         DO J=2,NDIFF
            THEO(J) = SNGL(DMAG(R(J-1), R(J), AIR(K), RO(K), P))
            ADOPT(J) = 0.
            WOBS(J) = 0.
         END DO
C   
CD       IF (PLOT .NE. 'N') THEN
CD          CALL ERASE
CD          CALL SETLIM (R(1)-1., 1.3*SUMD, R(NDIFF), -0.3*SUMD)
CD          CALL BOX (1, 2)
CD          CALL XLABEL (NCH(K), FILE(K)(1:NCH(K)))
CD       END IF
         FLIP = 0.32*(LAST(K)-LAST(K-1)-1.)/(LAST(K)-LAST(K-1))
CD       ANGLE = 0.
CD       CALL SETEXPAND (2.5)
         DO ISTAR = LAST(K-1)+1, LAST(K)
            IF (FLIP .GT. 0.) THEN
CD             ANGLE = ANGLE/2.
               FLIP = -0.75*FLIP
            ELSE
CD             ANGLE = ANGLE+60.
               FLIP = -FLIP
            END IF
C   
CD          CALL SETANGLE (ANGLE)
            DO J=2,NAP(ISTAR)
               WRITE (9,151) RBAR(J)+FLIP, MAG(J,ISTAR)
            END DO
C
CD          IF (PLOT .NE. 'N') CALL POINTS 
CD   .             (31., 1, RBAR(2), MAG(2,ISTAR), NAP(ISTAR)-1)
            DO J=2,NAP(ISTAR)
               DIFF = MAG(J,ISTAR) - THEO(J)
               W(J) = 1./( 1. + ABS( DIFF/(2.*SIG(J,ISTAR)) ) )
               W(J) = MIN( W(J), W(J-1) )
               WT = W(J)/SIG(J,ISTAR)**2
               ADOPT(J) = ADOPT(J) + WT*MAG(J,ISTAR)
               WOBS(J) = WOBS(J) + WT
               AVE(1) = AVE(1) + WT*THEO(J)
               AVE(J) = AVE(J) + WT*THEO(J)
               RESID(1) = RESID(1) + WT*DIFF
               RESID(J) = RESID(J) + WT*DIFF
               RESSQ(1) = RESSQ(1) + WT*DIFF**2
               RESSQ(J) = RESSQ(J) + WT*DIFF**2
               WR(1) = WR(1) + WT
               WR(J) = WR(J) + WT
            END DO
         END DO
         CALL CLFILE (9)
CD       CALL SETANGLE (0.)
CD       CALL SETEXPAND (1.)
C   
         DO J=2,NDIFF
            IF (WOBS(J) .GT. 0.) THEN
               OBS(J) = ADOPT(J)/WOBS(J)
               ADOPT(J) = 0.
               WOBS(J) = 0.
            END IF
         END DO
C
         DO ISTAR = LAST(K-1)+1, LAST(K)
            DO J=2,NAP(ISTAR)
               DIFF = MAG(J,ISTAR) - OBS(J)
               W(J) = 1./( 1. + ABS( DIFF/(2.*SIG(J,ISTAR)) )**2 )
               W(J) = MIN( W(J), W(J-1) )
               WT = W(J)/SIG(J,ISTAR)**2
               ADOPT(J) = ADOPT(J) + WT*MAG(J,ISTAR)
               WOBS(J) = WOBS(J) + WT
            END DO
         END DO
C
         DO J=2,NDIFF
            IF (WOBS(J) .GT. 0.) THEN
               OBS(J) = ADOPT(J)/WOBS(J)
               ADOPT(J) = 0.
               WOBS(J) = 0.
            END IF
         END DO
C
         DO ISTAR = LAST(K-1)+1, LAST(K)
            DO J=2,NAP(ISTAR)
               DIFF = MAG(J,ISTAR) - OBS(J)
               W(J) = 1./( 1. + ABS( DIFF/(2.*SIG(J,ISTAR)) )**3 )
               W(J) = MIN( W(J), W(J-1) )
               WT = W(J)/SIG(J,ISTAR)**2
               ADOPT(J) = ADOPT(J) + WT*MAG(J,ISTAR)
               WOBS(J) = WOBS(J) + WT
            END DO
         END DO
C
         MAP = 0
         SIGSQ = 0.
         DO J=2,NDIFF
            IF (WOBS(J) .GT. 0.) THEN
               WT = 1./(SCALE*THEO(J))**2
               OBS(J) = ADOPT(J)/WOBS(J)
               ADOPT(J) = ADOPT(J)+WT*THEO(J)
               WT = 1./(WOBS(J) + WT)                         ! SIGMA**2
               ADOPT(J) = WT*ADOPT(J)
               WADO(J) = SQRT(WT + (SCALE*ADOPT(J))**2)
               WOBS(J) = SQRT(1./WOBS(J))
               MAP = J
            ELSE
               ADOPT(J) = THEO(J)
               WADO(J) = 2.*SCALE*ABS(THEO(J))
            END IF
         END DO
         CALL DELFIL (9, SWITCH(FILE(K), CASE('.cur')), ISTAT)
         CALL OUTFIL (9, SWITCH(FILE(K), CASE('.cur')), ISTAT)
         AMIN = 0.
         DO J=2,NDIFF
            WT = AMIN1(THEO(J), OBS(J), ADOPT(J))
            IF (WT .LT. AMIN) THEN
               AMIN = WT
            ELSE
               WT = -0.1*AMIN
            END IF
            WRITE (9,151) RBAR(J), THEO(J), OBS(J), ADOPT(J), WT
         END DO
         CALL CLFILE (9)
         WRITE (3,300) K, FILE(K), SKYBAR(K), RO(K)
  300    FORMAT (//2X, '-->', I5.4, 1X, A, '   <sky> =', F9.1,
     .        5X, 'Ro =', F14.7/)
         WRITE (3,301) K, '   Model', (THEO(J), J=2,NDIFF)
  301    FORMAT (2X, '-->', I5.4, 10X, A8, 12F9.4)
         WRITE (3,301) K
         WRITE (3,301) K, 'Observed', (OBS(J), J=2,MAP)
         WRITE (3,301) K, 'Sigma(O)', (WOBS(J), J=2,MAP)
         WRITE (3,301) K
         WRITE (3,301) K, ' Adopted', (ADOPT(J), J=2,NDIFF)
         WRITE (3,301) K, 'Sigma(A)', (WADO(J), J=2,NDIFF)
         WRITE (3,301) K
         L = NDIFF+1
         IF (HST) THEN
            CUM(L) = 0.
         ELSE
            CUM(L) = SNGL(
     .            DMAG(R(NDIFF), 2.*R(NDIFF), AIR(K), RO(K), P))
         END IF
         SUMCUM(L) = SUMCUM(L) + CUM(L)
         NSUM = NSUM+1
         WCUM(L) = 0.
         DO I = NDIFF,2,-1
            CUM(I) = ADOPT(I) + CUM(I+1)
            SUMCUM(I) = SUMCUM(I) + CUM(I)
            WCUM(I) = WADO(I)**2 + WCUM(I+1)
         END DO
         WRITE (3,303) K, 'Cumulative', (CUM(J), J=2,NDIFF+1)
  303    FORMAT (2X, '-->', I5.4, 4X, A10, 12F9.4)
         WRITE (3,303) K, '  Sigma(C)', (SQRT(WCUM(J)), J=2,NDIFF)
         WRITE (3,*)
         WRITE (3,*)
         WRITE (10,10) ADOPT(2), WADO(2), CUM(2), SQRT(WCUM(2)),
     .        RO(K), FILE(K)(1:LENGTH(FILE(K)))
   10    FORMAT (5F9.4, 2X, A)
C
CD       IF (PLOT .NE. 'N') THEN
CD          CALL SETLTYPE (2)
CD          CALL CONNECT (RBAR(2), THEO(2), NDIFF-1)
CD          CALL SETLTYPE (3)
CD          CALL CONNECT (RBAR(2), OBS(2), NDIFF-1)
CD          CALL SETLTYPE (0)
CD          CALL CONNECT (RBAR(2), ADOPT(2), NDIFF-1)
CD          CALL TIDLE
CD          CALL TCLOSE
CD       END IF
C
         CALL INFILE (1, FILE(K), ISTAT)
         FILE(K) = SWITCH(FILE(K), CASE('.tot'))
         CALL DELFIL (2, FILE(K), ISTAT)
         CALL OUTFIL (2, FILE(K), ISTAT)
         CALL RDCHAR (1, LINE, N, ISTAT)
         IF (LINE(1:4) .EQ. ' NL ') THEN
            WRITE (2,277) LINE(1:N)
  277       FORMAT (A)
            CALL RDCHAR (1, LINE, N, ISTAT)
            IF (N .GE. 4) THEN
               WRITE (2,278) 1, LINE(4:N)
  278          FORMAT (1X, I2, A)
            ELSE
               WRITE (2,278) 1
            END IF
            READ (1,*)
            WRITE (2,*) ' '
         END IF
         CALL CLFILE (1)
         DO ISTAR = LAST(K-1)+1, LAST(K)
            SIGMIN = 1.E38
            DO J=1,NAP(ISTAR)
               IF (J .NE. 1) THEN
                  DIFF = MAG(J,ISTAR) - ADOPT(J)
                  MAG(J,ISTAR) = MAG(J-1,ISTAR) + MAG(J,ISTAR)
C
C MAG contains the magnitudes again.
C
                  W(J) = 1./( 1. + ( DIFF/(2.*SIG(J,ISTAR)) )**2 )
                  W(J) = MIN( W(J), W(J-1) )
               ELSE
                  W(J) = 1.
               END IF
               SIGSQ = WCUM(J+1) + SIG(J,ISTAR)**2/W(J)
               OBS(J) = MAG(J,ISTAR)+CUM(J+1)
               WOBS(J) = MIN(9.999, SQRT(SIGSQ))
               IF (SIGSQ .LT. SIGMIN) THEN
                  FINAL = MAG(J,ISTAR)
                  SFINAL = WOBS(J)
                  JFINAL = J
                  SIGMIN = SIGSQ
               END IF
c     if(id(istar) .eq. 7761)type*,'C',j,sig(j,istar),
c    .sqrt(wcum(j+1)),w(j),sigsq,sigmin
            END DO
            S1 = RNDOFF (X(ISTAR), 9, 3)
            S2 = RNDOFF (Y(ISTAR), 9, 3)
            S3 = RNDOFF (SKY(ISTAR), 9, 3)
            WRITE (2,222) ID(ISTAR), S1, S2, FINAL+CUM(JFINAL+1), 
     .           SFINAL, S3, FINAL, CUM(JFINAL+1), JFINAL
  222       FORMAT (I7, 2A9, 2F9.4, A9, F9.3, F9.4, I9)
            WRITE (3,302) ID(ISTAR), X(ISTAR), Y(ISTAR), 
     .           (OBS(J), J=1,NAP(ISTAR))
  302       FORMAT (I7, 2F8.2, ':', F8.3,  11F9.3)
            WRITE (3,333) (WOBS(J), J=1,NAP(ISTAR))
  333       FORMAT (24X, 12F9.4)
         END DO
         WRITE (3,301)
         CALL CLFILE (2)
CD       IF (PLOT .NE. 'N') READ (5,666) PLOT
      END DO
C
CD    IF (PLOT .NE. 'N') THEN
CD       CALL ERASE
CD       WRITE (6,2) 27, 12, 27, 50
CD  2    FORMAT (4A1)
CD       READ (5,666) PLOT
CD666    FORMAT (A1)
CD    END IF
C
      WRITE(3,300)
      DO J=1,NDIFF
         AVE(J) = AVE(J)/WR(J)
         RESID(J) = RESID(J)/WR(J)
         RESSQ(J) = SQRT(RESSQ(J)/WR(J) - RESID(J)**2)
         WRITE (6,67) J, AVE(J), RESID(J), RESSQ(J)
   67    FORMAT (I7, 3F9.5)
         WRITE (3,67) J, AVE(J), RESID(J), RESSQ(J)
      END DO
      WRITE (3,306)
      WRITE (3,306) ' ', (SUMCUM(J)/NSUM, J=2,NDIFF+1)
      CALL CLFILE (3)
      CALL CLFILE (10)
      CALL BYEBYE
C
      END
C
C#######################################################################
C
      FUNCTION  DMAG (R1, R2, X, RO, P)
C
C The stellar profile is approximated by a circular Moffat function:
C
C                I(r) = Io / [ A + (r/Ro)**2]**Q
C
C Magnitude(0 < r < R1) = constant - 2.5 x log{Integral[ 2 pi r I(r) ] }
C
C Magnitude(2) - Magnitude(1) = 
C           
C            -2.5 x log[(integral from 0 to R2)/(integral from 0 to R1)]
C
C It so happens that 
C
C                       r / [ A + (r/Ro)**2 ]**Q
C
C has an analytic integral.
C
      DOUBLE PRECISION DMAG, DLOG10
      DOUBLE PRECISION I1, I2
      DOUBLE PRECISION P(*)
      BPEX = P(2)+P(5)*X
      PM1 = P(1)-1.
      X1 = (R1/RO)
      X2 = (R2/RO)
      X1SQ = X1**2
      X2SQ = X2**2
      X1 = X1/P(4)
      X2 = X2/P(4)
      D1 = 1. + R1**2
      D2 = 1. + R2**2
      I1 = BPEX*(1.D0 - 1.D0/DBLE(D1)**PM1) + (1.-BPEX)*(
     .     P(3)*(1.-EXP(-0.5*X1SQ)) + 
     .     (1.-P(3))*(1.-(1.+X1)*EXP(-X1))  )
      I2 = BPEX*(1.D0 - 1.D0/DBLE(D2)**PM1) + (1.-BPEX)*(
     .     P(3)*(1.-EXP(-0.5*X2SQ)) + 
     .     (1.-P(3))*(1.-(1.+X2)*EXP(-X2))  )
      DMAG = -2.5D0 * DLOG10(I2/I1)
      RETURN
      END
