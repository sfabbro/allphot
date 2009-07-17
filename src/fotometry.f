      SUBROUTINE  PHOTSB  (F, PAR, MAXPAR, PSF, MAXPSF, MAXEXP,
     .     SKY, INDEX, MAXSKY, NCOL, NROW, INBAD, WATCH, ESTMTR)
C
C=======================================================================
C
C This subroutine derives the concentric aperture photometry.  At 
C present, this is the only place in all of DAOPHOT where sky values 
C are derived for the individual stars.
C
C               OFFICIAL DAO VERSION:  1999 August 24
C
C Argument
C
C    WATCH (INPUT) governs whether information relating to the progress 
C          of the reductions is to be typed on the terminal screen
C          during execution.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MINSKY, MAXSKY, MAXAP, NCOL, NROW
      INTEGER MAXPAR, MAXPSF, MAXEXP, MAXSTR
      PARAMETER  (MINSKY=20, MAXAP=12, MAXSTR=1 000 000)
C
C Parameters:
C
C MINSKY is the smallest number of pixels from which the sky may be
C        determined.  If for some star the number of sky pixels
C        is less than MINSKY, an error code will result and 
C        control will return to the main program.
C
C MAXSKY the maximum number of pixels allowed in the sky annulus.
C        This and the user's requested inner sky radius will later
C        determine the maximum permitted outer sky radius.
C
C MAXAP  the maximum number of star apertures allowed.
C
      CHARACTER*80 LINE
      CHARACTER*40 COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL, TABLE
      CHARACTER SWITCH*40, EXTEND*40, LBL(MAXAP+2)*26, RNDOFF*10,
     .     CASE*4
      CHARACTER TEXT1*10, TEXT2*6, TEXT3*6, XT*9, YT*9
      DOUBLE PRECISION MAGLIM, MAGSQ, WT, SUMWT, DLOG10, DBLE, DMAX1
      DOUBLE PRECISION APMAG(MAXAP), AREA(MAXAP)
      REAL X(MAXSTR), Y(MAXSTR), AMAG(MAXSTR)
      REAL PSF(MAXPSF,MAXPSF, MAXEXP), PAR(MAXPAR)
      REAL SKY(MAXSKY), MAGERR(MAXAP), BTOT(MAXAP)
      REAL F(NCOL,NROW), ERROR(3)
      REAL APR(MAXAP+2), PMIN(MAXAP+2), PMAX(MAXAP+2), RADSQ(MAXAP)
      REAL AVERAG(MAXAP), SIGMAS(MAXAP)
      REAL AMIN1, AMAX1, SQRT, PCTILE, USEPSF
      INTEGER INDEX(MAXSKY), IDIN(MAXSTR)
      INTEGER MIN0, MAX0, LENGTH
C
      REAL A, B, FACTOR, D
      REAL LOBAD, SKYMOD, SKYSIG, SKYSKW, SIGSQ, SKYVAR
      REAL DATUM, R, RSQ, FRACTN, EDGE, HIBAD, INBAD, THRESH, DUM
      REAL PHPADU, READNS, XC, YC, DMAG, WATCH, APMXSQ, RONOIS
      REAL RINSQ, ROUT, ROUTSQ, DYSQ, XMAX, YMAX
      REAL ESTMTR, SKYMN, SKYMED, SKYIN
      INTEGER I, J, K, L, NAPER, ID, NMAG, LX, LY, NX, NY
      INTEGER ISTAR, MX, MY, NSKY, IER, NL, IANNUL
C
      REAL PSFMAG, BRIGHT, XPSF, YPSF, DELTAX, DELTAY,
     .     PSFRAD, PSFRSQ, SCALE, DX, DY
      INTEGER IPSTYP, NPAR, NPSF, NEXP, NFRAC, NSTAR, RDPSF
      LOGICAL SBTRCT
C
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, PROFIL, GRPFIL
C
      DATA FACTOR /3./
      DATA APR /MAXAP*0., 2*0./, PMIN /1.E-30, MAXAP*0., 1./, 
     .     PMAX/MAXAP*1.E30, 2*1.E30/
      DATA TABLE / 'photo.opt                     ' /
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
      CALL GETNAM ('File with aperture radii:', TABLE)
      IF (TABLE .EQ. 'END-OF-FILE') THEN
         CALL TBLANK
         RETURN
      END IF
C
 1000 CALL OPTION (TABLE, MAXAP+2, LBL, APR, PMIN, PMAX, 'PHO>', IER)
      IF (IER .NE. 0) RETURN
      CALL TBLANK
C
C Count up the number of apertures that will be used.  The first zero or
C negative number encountered terminates the list.
C
      NAPER=MAXAP
      APMXSQ=-1.
      DO 1010 I=1,MAXAP
      IF (APR(I) .LE. 0.0) GO TO 1020
      RADSQ(I) = APR(I)**2
 1010 APMXSQ=AMAX1(APMXSQ, (APR(I)+0.5)**2)
      GO TO 1030
C
 1020 NAPER=I-1
 1030 CONTINUE
C
C NAPER   is the number of apertures, whose radii are stored in 
C         elements 1 through NAPER of the array APR.  
C
C APMXSQ  is the outermost edge of the largest aperture-- if the 
C         distance squared of the center of a pixel from the centroid of
C         the star is greater than APMXSQ, then we know that no part 
C         of the pixel is to be included in any aperture.
C
C Now define the other variables whose values are in the table.
C
      RINSQ=AMAX1(APR(MAXAP+1), 0.)**2      ! Inner sky radius squared 
      ROUTSQ = REAL(MAXSKY)/3.142 + RINSQ
      DUM = APR(MAXAP+2)**2
      IF (DUM .GT. ROUTSQ) THEN
         CALL TBLANK
         CALL TBLANK
         CALL STUPID 
     .        ('   *** You have specified too big a sky annulus. ***')
         WRITE (6,6) SQRT(ROUTSQ)
    6    FORMAT (F10.2, ' pixels is the largest outer sky radius ',
     .        'currently permitted.')
         RETURN
      ELSE IF (DUM .LE. RINSQ) THEN
         CALL TBLANK
         CALL STUPID 
     .   ('Your outer sky radius is no bigger than the inner radius.')
         WRITE (6,8)
    8    FORMAT ('Please try again.')
         GO TO 1000
      ELSE
         ROUT = APR(MAXAP+2)
         ROUTSQ = DUM
      END IF
C
C Get file names and set up the needed numerical constants.
C
      SBTRCT = .FALSE.
      IER = RDPSF (PSFFIL, IPSTYP, PAR, MAXPAR, NPAR,
     .     PSF, MAXPSF, MAXEXP, NPSF, NEXP, NFRAC,
     .     PSFMAG, BRIGHT, XPSF, YPSF)
      IF (IER .EQ. 0) THEN
         WRITE (6,9) PSFFIL(1:LENGTH(PSFFIL))
    9    FORMAT (/'  Found PSF file ', A/)
  800    CALL GETNAM ('Profile-fitting photometry:', PROFIL)
         IF (PROFIL .EQ. 'END-OF-FILE') GO TO 940
         IF (PROFIL .EQ. 'GIVE UP') THEN
            PROFIL = ' '
            RETURN
         END IF
         PSFRAD = (REAL(NPSF-1)/2. - 1.)/2.
         PSFRSQ = PSFRAD**2
         CALL INFILE (2, PROFIL, IER)
         IF (IER .NE. 0) THEN
            CALL STUPID ('Error opening input file '//PROFIL)
            PROFIL = 'GIVE UP'
            GO TO 800
         END IF
         CALL CHECK (2, NL)
         NSTAR = 0
  850    NSTAR = NSTAR+1
  855    READ (2,*,ERR=855,END=859) IDIN(NSTAR), X(NSTAR), 
     .        Y(NSTAR), AMAG(NSTAR)
         GO TO 850
  859    NSTAR = NSTAR-1
         CALL CLFILE (2)
C
C Inquire the name of the input data file with the stellar positions,
C and open it.
C
         PROFIL = SWITCH(PROFIL, '.lst')
  900    CALL GETNAM ('Star ID file:', PROFIL)
         IF ((PROFIL .EQ. 'END-OF-FILE') .OR.
     .       (PROFIL .EQ. 'GIVE UP')) THEN
            CALL TBLANK
            PROFIL = ' '
            RETURN
         END IF
C
         CALL INFILE (2, PROFIL, IER)
         IF (IER .NE. 0) THEN
            CALL STUPID ('Error opening input file '//PROFIL)
            PROFIL = 'GIVE UP'
            GO TO 900
         END IF
         NL = -1
         LOBAD = -10.
         HIBAD = INBAD
         CALL RDHEAD(2, NL, ID, ID, LOBAD, HIBAD, THRESH, DUM, 
     .        PHPADU, READNS, R)
         IF (NL .LT. 1) NL=1
         MAGFIL=SWITCH(PROFIL, CASE('.ap'))
         SBTRCT = .TRUE.
         GO TO 949
      END IF
C
  940 CALL GETNAM ('Input position file:', COOFIL)
      IF ((COOFIL .EQ. 'END-OF-FILE') .OR. 
     .    (COOFIL .EQ. 'GIVE UP')) THEN
         COOFIL = ' '
         RETURN
      END IF
C
      CALL INFILE (2, COOFIL, IER)
      IF (IER .NE. 0) THEN
         CALL STUPID ('Error opening '//COOFIL)
         COOFIL = 'GIVE UP'
         GO TO 940
      END IF
C
      CALL RDHEAD(2, NL, ID, ID, LOBAD, HIBAD, THRESH, DUM, 
     .     PHPADU, READNS, R)
  949 RONOIS = READNS**2
C
C Inquire file name for output aperture photometry results, and open
C the new file.
C
  950 CALL GETNAM ('Output file:', MAGFIL)
      IF ((MAGFIL .EQ. 'END-OF-FILE') .OR. 
     .     (MAGFIL .EQ. 'GIVE UP')) THEN
         CALL CLFILE (2)
         MAGFIL = ' '
         RETURN
      END IF
      MAGFIL = EXTEND(MAGFIL, CASE('ap'))
      CALL OUTFIL (3, MAGFIL, IER)
      IF (IER .NE. 0) THEN
         CALL STUPID ('Error opening output file '//MAGFIL)
         MAGFIL = 'GIVE UP'
         GO TO 950
      END IF
      CALL WRHEAD (3, 2, NCOL, NROW, 7, LOBAD, HIBAD, THRESH, 
     .     APR(1), PHPADU, READNS, R)
C
C If progress is being monitored, type out column headers.
C
      IF (WATCH .GT. 0.5) WRITE (6,610)
  610 FORMAT (/13X, 'STAR', 5X, 'X', 7X, 'Y', 9X, 'MAG.(1)', 8X, 'SKY')
C
C Initialize variables for the computation of the magnitude limit.
C
      MAGLIM=0.0D0
      MAGSQ=0.0D0
      SUMWT=0.0D0
      NMAG=0
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Derive aperture photometry object by object.
C
C Read entire image.
C
      LY = 1
      NY = NROW
      XMAX = NCOL+0.5
      YMAX = NROW+0.5
      CALL RDSECT (1, LY, NY, F, NCOL, IER)
C
C Subtract every star.
C
      IF (SBTRCT) THEN
         CALL TBLANK
         DO 990 ISTAR=1,NSTAR
            IF (AMAG(ISTAR) .GT. 50.) GO TO 990
            DELTAX = (X(ISTAR)-1.)/XPSF - 1.
            DELTAY = (Y(ISTAR)-1.)/YPSF - 1.
            LX=MAX0(1, INT(X(ISTAR)-PSFRAD)+1)
            LY=MAX0(1, INT(Y(ISTAR)-PSFRAD)+1)
            NX=MIN0(NCOL, INT(X(ISTAR)+PSFRAD))
            NY=MIN0(NROW, INT(Y(ISTAR)+PSFRAD))
            SCALE=10.**(0.4*(PSFMAG-AMAG(ISTAR)))
C
C Subtract the shifted scaled PSF
C
            DO 3030 J=LY,NY
               DY=FLOAT(J)-Y(ISTAR)
               DYSQ=DY**2
               DO 3020 I=LX,NX
                  IF (F(I,J) .GT. HIBAD) GO TO 3020
                  DX=FLOAT(I)-X(ISTAR)
                  IF (DX**2+DYSQ .GE. PSFRSQ) THEN
                     IF (DX .GT. 0.) GO TO 3030
                  ELSE
                     D = SCALE*USEPSF(IPSTYP, DX, DY, BRIGHT, 
     .                    PAR, PSF, NPSF, NPAR, NEXP, NFRAC, 
     .                    DELTAX, DELTAY, A, B)
                     F(I,J) = F(I,J) - D
                  END IF
 3020          CONTINUE
 3030       CONTINUE
  990    CONTINUE
      END IF
C
 2000 CALL RDSTAR (2, NL, ID, XC, YC, DMAG, SKYIN)
      IF (ID .LT. 0) GO TO 9000
      IF (ID .EQ. 0) GO TO 2000
      IF (SBTRCT) THEN
         DO ISTAR=1,NSTAR
            IF (ID .EQ. IDIN(ISTAR)) GO TO 2100
         END DO
         WRITE (LINE,666) ID
  666    FORMAT ('Star not found in input:', I7)
         CALL STUPID (LINE)
         GO TO 2000
C
 2100    XC = X(ISTAR)
         YC = Y(ISTAR)
         DMAG = AMAG(ISTAR)
         IF ((XC .LT. 0.5) .OR. (YC .LT. 0.5) .OR. 
     .        (XC .GT. XMAX) .OR. (YC .GT. YMAX)) GO TO 2000
C
C Add this star back into the image.
C
         DELTAX = (X(ISTAR)-1.)/XPSF - 1.
         DELTAY = (Y(ISTAR)-1.)/YPSF - 1.
         LX = MAX0(1, INT(X(ISTAR)-PSFRAD)+1)
         LY = MAX0(1, INT(Y(ISTAR)-PSFRAD)+1)
         NX = MIN0(NCOL, INT(X(ISTAR)+PSFRAD))
         NY = MIN0(NROW, INT(Y(ISTAR)+PSFRAD))
         SCALE = 10.**(0.4*(PSFMAG-AMAG(ISTAR)))
C
C Add the shifted scaled PSF
C
         DO 4030 J=LY,NY
            DY=FLOAT(J)-Y(ISTAR)
            DYSQ=DY**2
            DO 4020 I=LX,NX
               IF (F(I,J) .GT. HIBAD) GO TO 4020
               DX=FLOAT(I)-X(ISTAR)
               IF (DX**2+DYSQ .GE. PSFRSQ) THEN
                  IF (DX .GT. 0.) GO TO 4030
               ELSE
                  D = SCALE*USEPSF(IPSTYP, DX, DY, BRIGHT, 
     .                 PAR, PSF, NPSF, NPAR, NEXP, NFRAC, 
     .                 DELTAX, DELTAY, A, B)
                  F(I,J) = F(I,J) + D
               END IF
 4020       CONTINUE
 4030    CONTINUE
      END IF
C
C Compute the limits of the submatrix.
C
      LX = MAX0(1, INT(XC-ROUT)+1)
      MX = MIN0(NCOL, INT(XC+ROUT))
      LY = MAX0(1, INT(YC-ROUT)+1)
      MY = MIN0(NROW, INT(YC+ROUT))
      EDGE=AMIN1(XC-0.5, (NCOL+0.5)-XC, YC-0.5, (NROW+0.5)-YC)
C
C EDGE is the distance of the star's centroid from the outermost
C extremum of the array.
C
C Initialize star counts and aperture area.
C
      DO 2010 K=1,NAPER
      APMAG(K) = 0.D0
C 
C If this star aperture extends outside the array, the magnitude
C in this aperture will be no good.
C
      IF (EDGE .LT. APR(K)) APMAG(K)=-1.1D30            ! Null magnitude
C
C If aperture 2 or greater, determine the average flux value in the
C annulus.
C
      BTOT(K) = 0.
      IF ((K .GT. 1) .AND. (FACTOR .GT. 1.E-20)) THEN
         NSKY = 0
         DO J=LY,MY
            DYSQ = (J-YC)**2
            DO 2005 I=LX,MX
               A = F(I,J)
               IF ((A .LT. LOBAD) .OR. (A .GT. HIBAD)) GO TO 2005
               R = SQRT(DYSQ+(I-XC)**2)
               IF ((R .GT. APR(K-1)-0.35) .AND.
     .             (R .LT. APR(K)+0.35)) THEN
                  NSKY = NSKY+1
                  SKY(NSKY) = A
               END IF
 2005       CONTINUE
         END DO
         IF (NSKY .LE. 0) THEN
            AVERAG(K) = 0.
            SIGMAS(K) = 100.
         ELSE
            A = PCTILE(SKY, NSKY, NINT(0.1587*NSKY+0.5))
            B = PCTILE(SKY, NSKY, NINT(0.8413*NSKY+0.5))
            AVERAG(K) = 0.5*(A+B)
            SIGMAS(K) = AMAX1(0.25*(B-A)**2, RONOIS+AVERAG(K)/PHPADU)*
     .           (2.*FACTOR)**2
         END IF
      END IF
 2010 AREA(K)=0.0D0
C
C Now read through the submatrix, picking out the data we want.
C
      NSKY=0
C
      DO 2130 J=LY,MY
      DYSQ=(J-YC)**2
C
         DO 2125 I=LX,MX
            RSQ=DYSQ+(I-XC)**2
            DATUM=F(I,J)
C
C Is this pixel within the sky annulus?
C
            IF ((RSQ .LT. RINSQ) .OR. (RSQ .GT. ROUTSQ) .OR. 
     .           (NSKY .GT. MAXSKY)) GO TO 2110
            IF ((DATUM .LT. LOBAD) .OR. 
     .           (DATUM .GT. HIBAD)) GO TO 2110
            NSKY=NSKY+1
            SKY(NSKY)=DATUM
C
C The inclusion of partial pixels inside the aperture is done as 
C follows:  if the distance of the center of the current pixel from the
C centroid of the star [radius vector r(i,j)] is exactly equal to the 
C radius of the aperture [R(k)], then one-half of the counts in the
C pixel are included.  If r(i,j) < R(k)-0.5, then the entire pixel is
C included, while if r(i,j) > R(k)+0.5, the pixel is wholly excluded.
C In between, viz. for  R(k)-0.5 < r(i,j) < R(k)+0.5, the fraction of
C the counts included varies linearly.  Therefore a circular aperture
C is approximated by an irregular (not even convex) polygon.
C
C If this pixel falls completely outside the LARGEST aperture, go on
C to the next pixel.  Notice that APMXSQ has actually been defined
C as (R(k)+0.5)**2 for the largest value of R(k), in accordance with
C the formula used for the partial pixels.
C
C If DATUM is less than LOBAD, set very negative and bug out.
C IF DATUM is greater than HIBAD, set even more negative and but out.
C
 2110       CONTINUE
            IF (RSQ .GT. APMXSQ) GO TO 2125
            R=SQRT(RSQ)-0.5
            IANNUL = 1
            IF (NAPER .GT. 1) THEN
               DO K=2,NAPER
                  IF ((RSQ .GT. RADSQ(K-1)) .AND.
     .                (RSQ .LT. RADSQ(K))) THEN
                     IANNUL = K
                     GO TO 2112
                  END IF
               END DO
            END IF
 2112       CONTINUE
            IF ((DATUM .LT. LOBAD) .OR. (DATUM .GT. HIBAD) .OR.
     .           (IANNUL .LE. 1) .OR. (FACTOR .LE. 1.E-20)) THEN
               A = DATUM
               B = 1.
            ELSE
               B = ((DATUM-AVERAG(IANNUL))**2/
     .              (SIGMAS(IANNUL)*FACTOR**2))**FACTOR
               A = (B*AVERAG(IANNUL)+DATUM)
               B = 1./(B+1.)
               A = A*B
            END IF
C
            DO 2120 K=1,NAPER
C
C If this pixel falls completely outside THIS aperture, go on to the
C next aperture.
C
               IF (R .GT. APR(K)) GO TO 2120
               IF (APMAG(K) .LT. -1.D38) GO TO 2120
               FRACTN=AMAX1(0.0, AMIN1(1.0,APR(K)-R))
C
C FRACTN is the fraction of the pixel that falls inside the 
C (irregular) aperture.
C
C If the pixel is bad, set the total counts in this aperture to a number
C so negative that it will never be positive again.
C
               IF ((A .GE. LOBAD) .AND. (A .LE. HIBAD) .AND.
     .              (APMAG(K) .GT. -1.0D20)) THEN
                  APMAG(K) = APMAG(K)+DBLE(FRACTN*A)
                  AREA(K) = AREA(K)+DBLE(FRACTN)
                  IF (IANNUL .EQ. 1) THEN
                     BTOT(K) = BTOT(K)+FRACTN
                  ELSE
                     BTOT(K) = BTOT(K)+FRACTN*B
                  END IF
               ELSE IF (A .GT. HIBAD) THEN
                  APMAG(K) = -1.1D38
               ELSE
                  APMAG(K) = -1.0D29
               END IF
 2120       CONTINUE
 2125    CONTINUE
C
 2130 CONTINUE
C
C We have accumulated the brightnesses of individual sky pixels in the
C one-dimensional array SKY.  Pixels falling above or below the BAD 
C limits have already been eliminated.  Now sort SKY to place the 
C pixels in order of increasing brightness.
C
      IF (NSKY .LT. MINSKY)  THEN
         CALL STUPID ('There aren''t enough pixels in the sky annulus.')
         WRITE (6,*)
     .        ' Are you sure your bad pixel thresholds are all right?'
         WRITE (6,*)
     .        ' If so, then you need a larger outer sky radius.'
         CALL TBLANK
         DO K=1,NAPER
            APMAG(K) = 98.999
            MAGERR(K) = 9.9999
         END DO
         GO TO 8000
      END IF
      CALL QUICK (SKY, NSKY, INDEX)
C
C Obtain the mode, standard deviation, and skewness of the peak in the
C sky histogram.
C
      CALL MMM (SKY, NSKY, HIBAD, READNS, 
     .     SKYMN, SKYMED, SKYMOD, SKYSIG, SKYSKW)
      SKYVAR=SKYSIG**2
      SIGSQ=SKYVAR/FLOAT(NSKY)
C
C If ESTMTR is 0 use the mode; if ESTMTR is 1 use the median;
C if it is 2 use the mean.
C
C In star-subtraction mode, always use the median.
C
      IF (SBTRCT) THEN
         SKYMOD = SKYMED
      ELSE
         I = NINT(ESTMTR)
         IF (I .EQ. 1) THEN
            SKYMOD = SKYMED
         ELSE IF (I .EQ. 2) THEN
            SKYMOD = SKYMN
         ELSE IF (I .EQ. 3) THEN
            SKYMOD = SKYIN
         END IF
      END IF
C
C SKYMOD has units of (ADU/pixel), and SKYSIG is the pixel-to-pixel
C scatter of SKYMOD, in units of (ADU/pixel).  SKYVAR is the
C variance (square of the standard deviation) of the sky brightness,
C (ADU/pixel)**2, and SIGSQ is the square of the standard error of the 
C mean sky brightness.
C
C Subtract the sky from the integrated brightnesses in the apertures,
C convert the results to magnitudes, and compute standard errors.
C
C If the modal sky value could not be determined, set all  magnitudes
C to 98.999.  
C
      IF (SKYSIG .LT. -0.5) THEN
         DO I=1,NAPER
            APMAG(I) = 98.999D0
            MAGERR(I) = 9.9999
         END DO
         GO TO 8000
      END IF
C
      DO 2220 I=1,NAPER
C
C If there is a HIGH bad pixel in this aperture, i.e., likely saturation,
C the aperture magitude is of order -1e38.  Set the magnitude to -99.999.
C
      IF (APMAG(I) .LT. -1.D31) THEN
         APMAG(I) = -99.999D0
         MAGERR(I) = 9.9999
         GO TO 2220
      END IF
C
C If the star + sky is fainter than the sky, or if the star aperture 
C extends beyond the limits of the picture, or if there is a 
C LOW bad pixel in the star aperture, set the magnitude to 99.999.
C
      APMAG(I)=APMAG(I)-DBLE(SKYMOD)*AREA(I)
      IF (APMAG(I) .LE. 0.0D0) THEN
         APMAG(I) = 99.999D0
         MAGERR(I) = 9.9999
         GO TO 2220
      END IF
      ERROR(1)=SNGL(AREA(I))*SKYVAR
      ERROR(2)=SNGL(APMAG(I))/PHPADU
      ERROR(3)=SIGSQ*SNGL(AREA(I))**2
C
C These variables ERRORn are the respective variances (squares of the
C mean errors) for: (1) random noise inside the star aperture, including
C readout noise and the degree of contamination by other stars in the
C neighborhood, as estimated by the scatter in the sky values (this
C standard error increases as the square root of the area of the 
C aperture); (2) the Poisson statistics of the observed star brightness;
C (3) the uncertainty of the mean sky brightness (this standard error 
C increases directly with the area of the aperture).
C
      MAGERR(I)=1.0857*SQRT(ERROR(1)+ERROR(2)+ERROR(3))/SNGL(APMAG(I))
      IF (I .GT. 1) MAGERR(I) = MAGERR(I) * SQRT(SNGL(AREA(I))/BTOT(I))
      MAGERR(I) = AMIN1(9.9999, MAGERR(I))
      APMAG(I)=25.D0-2.5D0*DLOG10(APMAG(I))
      IF (APMAG(I) .GT. 94.999D0) MAGERR(I) = 9.9999
 2220 CONTINUE
C
C Subtract this star from the image again.
C
 8000 CONTINUE
      IF (SBTRCT) THEN
         DELTAX = (X(ISTAR)-1.)/XPSF - 1.
         DELTAY = (Y(ISTAR)-1.)/YPSF - 1.
         LX=MAX0(1, INT(X(ISTAR)-PSFRAD)+1)
         LY=MAX0(1, INT(Y(ISTAR)-PSFRAD)+1)
         NX=MIN0(NCOL, INT(X(ISTAR)+PSFRAD))
         NY=MIN0(NROW, INT(Y(ISTAR)+PSFRAD))
         SCALE=10.**(0.4*(PSFMAG-AMAG(ISTAR)))
C
C Subtract the shifted scaled PSF
C
         DO 2030 J=LY,NY
            DY=FLOAT(J)-Y(ISTAR)
            DYSQ=DY**2
            DO 2020 I=LX,NX
               IF (F(I,J) .GT. HIBAD) GO TO 2020
               DX=FLOAT(I)-X(ISTAR)
               IF (DX**2+DYSQ .GE. PSFRSQ) THEN
                  IF (DX .GT. 0.) GO TO 2030
               ELSE
                  D = SCALE*USEPSF(IPSTYP, DX, DY, BRIGHT, 
     .                 PAR, PSF, NPSF, NPAR, NEXP, NFRAC, 
     .                 DELTAX, DELTAY, A, B)
                  F(I,J) = F(I,J) - D
               END IF
 2020       CONTINUE
 2030    CONTINUE
      END IF
C
C Write out the answers.
C      
      IF (WATCH .GT. 0.5) WRITE (6,620)ID, XC, YC, 
     .     APMAG(1), MIN(9.999, MAGERR(1)), SKYMOD
  620 FORMAT (10X, I7, 2F8.2, F9.3, ' +-', F6.3, F8.1)
      XT = RNDOFF(XC, 9, 3)
      YT = RNDOFF(YC, 9, 3)
      WRITE (3,320) ID, XT, YT, (APMAG(I), I=1,NAPER)
  320 FORMAT (/I7, 2A9, 12F9.3)
      TEXT1 = RNDOFF(SKYMOD, 10, 3)
      TEXT2 = RNDOFF(SKYSIG, 6, 2)
      TEXT3 = RNDOFF(SKYSKW, 6, 2)
      WRITE (3,321) TEXT1, TEXT2, TEXT3,
     .     (MAGERR(I), I=1,NAPER)
  321 FORMAT (4X, A10, 2A6, F8.4, 11F9.4)
C
      IF (APMAG(1) .GT. 99.D0) GO TO 2000
      WT=(2./(2.-DMAG))*(100./MAGERR(1))**2
      MAGLIM=MAGLIM+WT*(SNGL(APMAG(1))-DMAG)
      MAGSQ=MAGSQ+WT*(SNGL(APMAG(1))-DMAG)**2
      SUMWT=SUMWT+WT
      NMAG=NMAG+1
      GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.  
C
C Estimate magnitude limit, close files, and return.
C
 9000 CONTINUE
      CALL CLFILE (3)
      CALL CLFILE (2)
      IF (SUMWT .LE. 0.0) RETURN
      MAGLIM=MAGLIM/SUMWT
      MAGSQ=MAGSQ/SUMWT-MAGLIM**2
      MAGSQ=DSQRT(DMAX1(0.D0, MAGSQ))
      WRITE (LINE,630) MAGLIM, MAGSQ
  630 FORMAT (' Estimated magnitude limit (Aperture 1): ', F4.1,
     .      ' +-', F4.1, ' per star.')
      CALL STUPID (LINE)
cxyz  call clpic ('copy')                                        ! test
      RETURN
C
C-----------------------------------------------------------------------
C
      END!
