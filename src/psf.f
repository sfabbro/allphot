      SUBROUTINE  GETPSF (PIC, NCOL, NROW, PAR, PSF, OPT, NOPT)
      IMPLICIT NONE
C
C=======================================================================
C
C This subroutine generates a point-spread function from one or the
C average of several stars.
C
C                OFFICIAL DAO VERSION:  2001 June 13
C
C The subroutine reads in a subarray around the first desired 
C point-spread function star, fits a gaussian profile to the core of 
C the image, and generates a look-up table of the residuals of the 
C actual image data from the gaussian fit.  If desired, it will then
C fit this PSF to another star to determine its precise centroid, 
C scale the same Gaussian to the new star's core, and add the 
C differences between the actual data and the scaled Gaussian to the
C look-up table.  This can go on star after star after star.
C The parameters of the Gaussian approximation and the table of 
C corrections from the approximation to the true PSF are stored in 
C a disk file.
C
C Arguments
C
C PSFRAD (INPUT) is the radius, in pixels, of the circular area within
C        which we ultimately wish to define the PSF.  Because of the
C        need to perform numerical interpolation, we will actually
C        generate and store a look-up table occupying a larger area.
C        The look-up table will be square, even though the corners will
C        be unused.
C
C FITRAD (INPUT) is the fitting radius, which will be used in defining 
C        what is meant by a "neighbor" of the PSF star.
C
C Both of these arguments are user-definable parameters, whose values
C may be altered by a DEFAULT.OPT file, or by the OPTIONS command.
C
C=======================================================================
C
C Modifications as of 1991 May 10.
C ===================================
C
C
C I'm going to try to rig this thing so it can reduce the weights of
C (that is, "ignore") discordant pixels in the calculation of the
C look-up tables of corrections.  To accomplish this, I'm going to
C change the way PSF operates.  Instead of reading in a subsection
C for each PSF star one at a time, it will read in the whole image
C and work in memory.
C
      INTEGER MAXPSF, MAXSTR, MAXN, MAXPAR, MAXEXP, NCOL, NROW
      INTEGER NOPT
      PARAMETER  (MAXPAR=6, MAXPSF=207, MAXEXP=10) 
      PARAMETER  (MAXSTR=400 000, MAXN=400) 
C
C Parameters
C
C MAXPSF is the dimension of the largest lookup table that will ever 
C        need to be generated.  Recall that the corrections from the
C        Gaussian approximation of the PSF to the true PSF will be
C        stored in a table with a half-pixel grid size.
C        MAXPSF must then equal
C
C                   2*[ 2*INT(R) + 1 ] + 1 = 4*INT(R) + 3
C
C MAXSTR is the largest number of stars permitted in a data file.
C
C MAXN is the maximum permitted number of PSF stars.
C
C MAXPAR is the maximum number of parameters which may be used to 
C        describe the analytic part of the PSF, *** IN ADDITION TO
C        the central intensity, and the x and y centroids.
C
C MAXEXP is the maximum number of terms in the expansion of the
C        look-up table (1 for constant, 3 for linear, 6 for quadratic).
C
      CHARACTER LINE*85, LABEL*8
      CHARACTER COOFIL*40, MAGFIL*40, PSFFIL*40, FITFIL*40, 
     .     GRPFIL*40, SWITCH*40
      CHARACTER EXTEND*40, CASE*40, FLAG(MAXN)*1, ANSWER*1
      CHARACTER RNDOFF*9, TEXT*9, T1*9, T2*9
      REAL C(MAXEXP,MAXEXP), V(MAXEXP), A(MAXEXP)
      REAL CON(MAXPSF,MAXPSF), TERM(MAXEXP)
c     real corner(maxpsf*maxpsf)
      REAL PIC(NCOL,*)
      REAL PAR(MAXPAR)
      REAL PSF(MAXPSF,MAXPSF,MAXEXP)
      REAL XCEN(MAXSTR), YCEN(MAXSTR), APMAG(MAXSTR), SKY(MAXSTR)
      REAL HPSF(MAXN), WEIGHT(MAXN), OPT(NOPT)
      REAL HJNK(MAXN), XJNK(MAXN), YJNK(MAXN)
      REAL PROFIL, BICUBC, SQRT
      INTEGER ID(MAXSTR), NTAB(-1:3), NPARAM
      LOGICAL IN(MAXPSF,MAXPSF), EDGE(MAXPSF,MAXPSF), SATR8D(MAXN)
C
      DOUBLE PRECISION PSFMAG, DLOG10, DBLE
      REAL LOBAD, HIBAD, THRESH, AP1, PHPADU, READNS, RONOIS,
     .     FWHM, WATCH, FITRAD, RADSQ, PSFRAD, PSFRSQ, FMAX, RATIO,
     .     DFDX, DFDY, SIG, DX, DY, RDX, RDY, XMID, YMID, DP, OLD, W, 
     .     WT, RADIUS, SCALE, SUMSQ, SUMN, DATUM, RSQ, DX2, DY2
      REAL ERRMAG, CHI, SHARP
      real rmin, sum1, sum2
      integer n1, n2
      INTEGER I, J, K, L, N, LX, LY, MX, MY, NTOT, NSTAR, ISTAR, NL
      INTEGER IEXPAND, IFRAC, IPSTYP, NPSF, NPAR, ISTAT, IRADSQ,
     .     NEXP, MIDDLE, MIDSQ, IDX, JDY, JDYSQ, IX, JY, ITER, NPASS
      INTEGER II, JJ, JRADSQ, NPSFSQ
      LOGICAL STAROK, SATUR8, REDO
C
      COMMON /FILNAM/ COOFIL, MAGFIL, PSFFIL, FITFIL, GRPFIL
C     COMMON /ERROR/ PHPADU, RONOIS, PERR, PKERR
C
      DATA NTAB / 0, 1, 3, 6, 10 /
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Set up the necessary variables, open the necessary files, read in the
C relevant data for all stars.
C
      FWHM = OPT(5)
      WATCH = OPT(11)
      FITRAD = OPT(12)
      RADSQ = FITRAD**2
      PSFRAD = OPT(13)
      PSFRSQ = (PSFRAD+2.)**2
      IEXPAND = NINT(OPT(14))
      IFRAC = 0
      NPASS = NINT(OPT(17))
C
C If in the final analysis we want to be able to determine a value for
C the point-spread function anywhere within a radius of R pixels, and
C we want the lookup table to have a one-half pixel grid spacing in 
C each coordinate.  To go from 0 to R with half-pixel steps, we
C need 2*R points, not including the central ("0") one.  Outside this
C we will need an additional pixel, just to make sure we will be able to
C do the interpolations at the outermost radii.  Then, to get a
C square box centered on (0,0) we will need 2*(2*R+1)+1 pixels.
C
      NPSF = 2*(NINT(2.*PSFRAD)+1)+1
      PSFRAD = (REAL(NPSF-1)/2. - 1.)/2.
      IRADSQ = NINT(2.*PSFRAD)**2
      JRADSQ = NINT(1.4*PSFRAD)**2
C
C Ascertain the name of the input (aperture, usually) photometry file, 
C and read in the relevant data for all stars.
C
      CALL TBLANK
  920 CALL GETNAM ('File with aperture results:', MAGFIL)
      IF ((MAGFIL .EQ. 'END-OF-FILE') .OR.
     .     (MAGFIL .EQ. 'GIVE UP')) THEN
         MAGFIL = ' '
         RETURN
      END IF
      CALL INFILE (2, MAGFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//MAGFIL)
         MAGFIL = 'GIVE UP'
         GO TO 920
      END IF
      CALL RDHEAD (2, NL, I, I, LOBAD, HIBAD, THRESH, AP1,
     .     PHPADU, READNS, RADIUS)
      RONOIS = READNS**2
C
      I = 0
 1010 I = I+1
      IF (I .GT. MAXSTR) GO TO 1100
 1020 CALL RDSTAR (2, NL, ID(I), XCEN(I), YCEN(I), APMAG(I), SKY(I))
      IF (ID(I) .LT. 0) GO TO 1100
      IF (ID(I) .EQ. 0) GO TO 1020
      IF (APMAG(I) .GT. 50.) GO TO 1020
      GO TO 1010
C
 1100 NTOT = I-1
      CALL CLFILE (2)
      PSFFIL = SWITCH(MAGFIL, CASE('.lst'))
      CALL GETNAM ('File with PSF stars:', PSFFIL)
      IF ((PSFFIL .EQ. 'END-OF-FILE') .OR.
     .     (PSFFIL .EQ. 'GIVE UP')) THEN
         PSFFIL = ' '
         RETURN
      END IF
      CALL INFILE (2, PSFFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//PSFFIL)
         PSFFIL = 'GIVE UP'
         GO TO 1120
      END IF
      CALL CHECK (2, NL)
      PSFFIL = SWITCH(PSFFIL, CASE('.psf'))
 1120 CALL GETNAM ('File for the PSF:', PSFFIL)
      IF ((PSFFIL .EQ. 'END-OF-FILE') .OR.
     .     (PSFFIL .EQ. 'GIVE UP')) THEN
         PSFFIL = ' '
         RETURN
      END IF
      PSFFIL = EXTEND(PSFFIL, CASE('psf'))
      CALL OUTFIL (3, PSFFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//PSFFIL)
         PSFFIL = 'GIVE UP'
         GO TO 1120
      END IF
      CALL TBLANK
C
C Read in the entire image.
C
      LY = 1
      MY = NROW
      CALL RDSECT (1, LY, MY, PIC, NCOL, ISTAT)
C
C-----------------------------------------------------------------------
C
C SECTIONS 2
C
C Learn the name of each PSF star and find it in the star list.  Display
C a subarray around it if desired, and check it for invalid pixels.
C
      NSTAR = 0
      N = NPARAM(1, FWHM, LABEL, PAR, MAXPAR)
C
C NSTAR will point at the last PSF star in the stack after the PSF
C stars have been brought to the top.
C
 2000 CONTINUE
      CALL RDSTAR (2, NL, I, DX, DY, RDX, RDY)
      IF (I .EQ. 0) GO TO 2000
      IF (I .LT. 0) THEN
         CALL CLFILE (2)
         GO TO 3000
      END IF
C
      DO 2010 ISTAR=1,NTOT
      IF (ID(ISTAR) .EQ. I) GO TO 2020
 2010 CONTINUE
      WRITE (6,701) I, ' was not found.'
  701 FORMAT (I7, A)
      GO TO 2000
C
C If a given star appears in the .LST file more than once, employ it
C only once.
C
 2020 CONTINUE
      IF (ISTAR .LE. NSTAR) GO TO 2000
      IF ( ( INT(XCEN(ISTAR)-FITRAD) .LT. 0 ) .OR.
     .     ( INT(XCEN(ISTAR)+FITRAD) .GT. NCOL ) .OR.
     .     ( INT(YCEN(ISTAR)-FITRAD) .LT. 0 ) .OR.
     .     ( INT(YCEN(ISTAR)+FITRAD) .GT. NROW ) ) THEN
         WRITE (6,701) ID(ISTAR), ' is too near edge of frame.'
         GO TO 2000
      END IF
C
C Define the subarray containing the star and search it for "bad" 
C pixels.  This will include a three-pixel border around the PSF box 
C itself, to guarantee valid interpolations to within the PSF box, and
C to allow for a possible one-pixel shift in the star's centroid.
C
      LX = MAX0( 1, INT(XCEN(ISTAR)-PSFRAD)-2 )
      LY = MAX0( 1, INT(YCEN(ISTAR)-PSFRAD)-2 )
      MX = MIN0(NCOL, INT(XCEN(ISTAR)+PSFRAD)+3 )
      MY = MIN0(NROW, INT(YCEN(ISTAR)+PSFRAD)+3 )
C
      FMAX=-32768.
      STAROK=.TRUE.
      SATUR8=.FALSE.
C
 2040 REDO = .FALSE.
      DO 2070 J=LY,MY
         DP = (REAL(J)-YCEN(ISTAR))**2
         DO 2050 I=LX,MX
            DX = REAL(I)-XCEN(ISTAR)
            RSQ = DX**2 + DP
            IF (RSQ .GT. PSFRSQ) THEN
               IF (DX .GE. 0.) GO TO 2070
               GO TO 2050
            END IF
            DATUM = PIC(I,J)
            IF ((DATUM .LT. LOBAD) .OR. (DATUM .GT. HIBAD)) THEN
C
C There is a defective pixel.
C
               IF (RSQ .LE. RADSQ) THEN
C
C It is inside the fitting radius.
C
                 IF (DATUM .LT. LOBAD) THEN
C
C The pixel is bad low, so reject the star.
C
                   WRITE (6,701) ID(ISTAR), 
     .                  ' is not a good star.'
                   GO TO 2000
                 ELSE IF (.NOT. SATUR8) THEN
C
C The pixel is bad high, so presume the star is saturated.
C
                   WRITE (6,701) ID(ISTAR),
     .                  ' is saturated.'
                   IF (OPT(18) .GT. 0.5) THEN
                       SATUR8 = .TRUE.
                    ELSE
                       GO TO 2000
                    END IF
                 END IF
                 GO TO 2050
               END IF
C
C The bad pixel isn't inside the fitting radius.
C
               IF (STAROK) THEN
                 STAROK = .FALSE.
                 IF (WATCH .GT. -0.5) THEN
                   CALL TBLANK
                   WRITE (6,628) ID(ISTAR), DATUM, 7, I, J
 628               FORMAT (I7, ' has a bad pixel:',  F9.1, 
     .                  ' at position', A1, 2I5)
                 END IF
               ELSE
                 IF (WATCH .GT. -0.5) 
     .                WRITE (6,628) ID(ISTAR), DATUM, 0, I, J
               END IF
C
C Replace the defective pixel with an average of surrounding
C pixels.
C
               DATUM = 0.
               SUMN = 0.
               DO L=MAX0(1,J-1),MIN0(NROW,J+1)
                  DO K=MAX0(1,I-1),MIN0(NCOL,I+1)
                     IF ((PIC(K,L) .GE. LOBAD) .AND.
     .                   (PIC(K,L) .LE. HIBAD)) THEN
                        DATUM = DATUM + PIC(K,L)
                        SUMN = SUMN+1.
                     END IF
                  END DO
               END DO
C
C If there are fewer than three valid neighboring pixels, do not
C fudge the pixel yet.  Instead, we will go through again (and again
C if necessary) and allow the fudge to eat into the defect from the
C outside.
C
               IF (SUMN .LT. 2.5) THEN
                  REDO = .TRUE.
               ELSE
                  PIC(I,J) = DATUM/SUMN
               END IF
            ELSE
C
C The pixel wasn't bad.
C
               IF (DATUM .GT. FMAX) FMAX=DATUM
            END IF
 2050    CONTINUE
 2070 CONTINUE
      IF (REDO) GO TO 2040
C
      IF (STAROK) THEN
         IF ((.NOT. SATUR8) .AND. (WATCH .LT. -0.5) .AND.
     .        (WATCH .GT. -1.5)) THEN
         END IF
      ELSE IF (.NOT. SATUR8) THEN
         IF ((WATCH .LT. -0.5) .AND. (WATCH .GT. -1.5)) THEN
            WRITE (6,701) ID(ISTAR), ' has bad pixels.'
         ELSE
            IF (WATCH .GT. -0.5) THEN
               CALL TBLANK
               CALL GETYN ('Try this one anyway?', ANSWER)
               IF (ANSWER .EQ. 'E') THEN
                  CALL CLFILE (2)
                  CALL CLFILE (3)
                  RETURN
               END IF
               IF (ANSWER .NE. 'Y') GO TO 2000
            END IF
         END IF
      END IF
C
      IF (WATCH .GT. 0.5) THEN
         CALL SHOW (PIC(LX,LY), FMAX, SKY(ISTAR), 
     .        MX-LX+1, MY-LY+1, NCOL)
         WRITE (6,622) ID(ISTAR), XCEN(ISTAR), YCEN(ISTAR),
     .        APMAG(ISTAR), SKY(ISTAR), NINT(FMAX)
  622    FORMAT (I7, 2F9.2, F9.3, F9.1, 4X, 'Brightest pixel:', I7)
         CALL GETYN ('Use this one?', ANSWER)
         IF (ANSWER .EQ. 'E') THEN
            CALL TBLANK
            CALL CLFILE (2)
            GO TO 3000
         END IF
         IF (ANSWER .EQ. 'N') GO TO 2000
      END IF
C
C Switch this star (at position ISTAR) with the one at position NSTAR+1
C (which is the highest star in the stack not already a PSF star).
C Estimate the height of the best-fitting profile of type 1 (probably
C a Gaussian) on the basis of the central pixel.
C
      NSTAR = NSTAR+1
      CALL SWAPP (ID, XCEN, YCEN, APMAG, SKY, ISTAR, NSTAR)
      HPSF(NSTAR) = (FMAX-SKY(NSTAR))/
     .     PROFIL(1, 0., 0., PAR, DFDX, DFDY, TERM, 0)
      SATR8D(NSTAR) = SATUR8
      IF (NSTAR .LT. MAXN) GO TO 2000
C
 3000 CONTINUE
      NEXP = NTAB(IEXPAND) + 2*IFRAC
      IF (NSTAR .LT. NEXP) THEN
         CALL STUPID (
     .   'There aren''t enough PSF stars for a PSF this variable.')
         WRITE (6,*) ' Please change something.'
         CALL TBLANK
         RETURN
      END IF
C
C If the first star is saturated, exchange it for the first 
C unsaturated one.
C
      IF (SATR8D(1)) THEN
         DO ISTAR=2,NSTAR
            IF (.NOT. SATR8D(ISTAR)) GO TO 3005
         END DO
         CALL STUPID ('Every single PSF star is saturated.')
         RETURN
C
 3005    CONTINUE
         CALL SWAPP (ID, XCEN, YCEN, APMAG, SKY, 1, ISTAR)
         SATR8D(ISTAR) = .TRUE.
         SATR8D(1) = .FALSE.
      END IF
C
      CALL TBLANK
      CALL OVRWRT ('       Chi    Parameters...', 1)
C
C If OPT(16) (Analytic model PSF) is negative, then try all PSF types
C from 1 to | OPT(16) |, inclusive.
C
      K = NINT(OPT(16))
      J = MAX0(1,K)
      K = IABS(K)
      OLD = 1.E30
      IPSTYP = 0
      DO 3050 I=J,K
C
         DO L=1,NSTAR
            HJNK(L) = HPSF(L)
            XJNK(L) = XCEN(L)
            YJNK(L) = YCEN(L)
         END DO
C
         N = NPARAM(I, FWHM, LABEL, PAR, MAXPAR)
         CALL  FITANA  (PIC, NCOL, NROW, ID, HJNK, XJNK, YJNK, 
     .        SKY, SATR8D, NSTAR, FITRAD, WATCH, I, PAR, N, SIG)
C
C SIG is the root-mean-square scatter about the best-fitting analytic
C function averaged over the central disk of radius FITRAD, expressed
C as a fraction of the peak amplitude of the analytic model.
C
         IF (PAR(1) .LT. 0.) GO TO 3050
C
C If this latest fit is better than any previous one, store the
C parameters of the stars and the model for later reference.
C
         IF (SIG .LT. OLD) THEN
            IPSTYP = I
            WRITE (LINE,201) (PAR(L), L=1,N)
  201       FORMAT (1X, 1P, 6E13.6)
            DO L=1,NSTAR
               HPSF(L) = HJNK(L)
               XCEN(L) = XJNK(L)
               YCEN(L) = YJNK(L)
            END DO
            OLD = SIG
         END IF
 3050 CONTINUE
      IF (IPSTYP .LE. 0) RETURN
      SIG = OLD
      XMID = REAL(NCOL-1)/2.
      YMID = REAL(NROW-1)/2.
      CALL TBLANK
      NPAR = NPARAM(IPSTYP, FWHM, LABEL, PAR, MAXPAR)
      READ (LINE,201) (PAR(L), L=1,NPAR)
C
C This last bit ensures that the subsequent computations will be 
C performed with the parameters rounded off exactly the same as they
C appear in the output .PSF file.
C
C=======================================================================
C
C At this point, we have values for the parameters of the best-fitting
C analytic function.  Now we may want to generate the look-up table of
C corrections from the best-fitting analytic function to the actual
C data.  This will be generated within a square box of size NPSF x NPSF
C with half-pixel spacing, centered on the centroid of the star.
C
C First, subtract the analytic function from all the PSF stars in the
C original image.
C
 3300 DP = 0.
      SATUR8 = .FALSE.
      DO 3400 ISTAR=1,NSTAR
         FLAG(ISTAR) = ' '
         IF (HPSF(ISTAR) .LE. 0.) GO TO 3400
         RDX = AMAX1(PROFIL(IPSTYP,0.,0.5,PAR,DFDX,DFDY,TERM,0),
     .               PROFIL(IPSTYP,0.,-0.5,PAR,DFDX,DFDY,TERM,0),
     .               PROFIL(IPSTYP,0.5,0.,PAR,DFDX,DFDY,TERM,0),
     .               PROFIL(IPSTYP,-0.5,0.,PAR,DFDX,DFDY,TERM,0))
         IF (HPSF(ISTAR)*RDX+SKY(ISTAR) .GT. HIBAD) THEN
            SATR8D(ISTAR)=.TRUE.
         END IF
C
C The centroids and peak heights are not yet known for saturated
C stars.
C
      IF (SATR8D(ISTAR)) THEN
         SATUR8 = .TRUE.
         GO TO 3400
      END IF
C
      LX = MAX0( 1, INT(XCEN(ISTAR)-PSFRAD)-1 )
      LY = MAX0( 1, INT(YCEN(ISTAR)-PSFRAD)-1 )
      MX = MIN0(NCOL, INT(XCEN(ISTAR)+PSFRAD)+2 )
      MY = MIN0(NROW, INT(YCEN(ISTAR)+PSFRAD)+2 )
      RDX = 0.0
      RDY = 0.0
      DO 3350 J=LY,MY
         DY = REAL(J) - YCEN(ISTAR)
         W = DY**2
         DO I=LX,MX
            DX = REAL(I) - XCEN(ISTAR)
            PIC(I,J) = PIC(I,J) - HPSF(ISTAR) *
     .           PROFIL(IPSTYP, DX, DY, PAR, DFDX, DFDY, TERM, 0)
            IF (DX**2 + W .LT. RADSQ) THEN
               RDX = RDX + (PIC(I,J)-SKY(ISTAR))**2
               RDY = RDY + 1.
            END IF
         END DO
 3350 CONTINUE
      DP = DP + RDY
      RDX = SQRT(RDX/RDY)
      HJNK(ISTAR) = RDX/(HPSF(ISTAR)*
     .     PROFIL(IPSTYP, 0., 0., PAR, DFDX, DFDY, TERM, 0))
 3400 CONTINUE
C
      DP = SQRT(DP/(DP-REAL(NEXP+3*NSTAR)))
      DO 3450 ISTAR=1,NSTAR
         WEIGHT(ISTAR) = 0.
         IF (HPSF(ISTAR) .LE. 0.) GO TO 3450
         IF (SATR8D(ISTAR)) THEN
            IF (OPT(18) .LT. 0.5) FLAG(ISTAR) = '#'
            GO TO 3450
         END IF
         HJNK(ISTAR) = DP * HJNK(ISTAR)
         IF (SIG .GT. 0.) THEN
            RDX = HJNK(ISTAR)/SIG
            IF (HPSF(ISTAR) .GT. 0.) 
     .           WEIGHT(ISTAR) = 1. /(1. + (RDX/2.)**2)
            IF (RDX .GE. 3.) THEN
               FLAG(ISTAR) = '*'
            ELSE IF (RDX .GE. 2.) THEN
               FLAG(ISTAR) = '?'
            END IF
         END IF
 3450 CONTINUE
      K = (NSTAR-1)/5 + 1
      WRITE (6,66)
   66 FORMAT (/' Profile errors:'/)
      DO I=1,K
         LINE = ' '
         DO ISTAR=I,NSTAR,K
            J = 17*(ISTAR-I)/K + 1
            IF (SATR8D(ISTAR)) THEN
               WRITE (LINE(J:J+16),67) ID(ISTAR)
   67          FORMAT (I7, ' saturated')
            ELSE IF (HPSF(ISTAR) .LE. 0.) THEN
               WRITE (LINE(J:J+16),68) ID(ISTAR)
   68          FORMAT (I7, ' defective')
            ELSE
               WRITE (LINE(J:J+16),69) ID(ISTAR), 
     .              HJNK(ISTAR), FLAG(ISTAR)
   69          FORMAT (I7, F7.3, 1X, A1, 1X)
            END IF
         END DO
         WRITE (6,6) LINE(1:85)
    6    FORMAT (A)
      END DO
      CALL TBLANK
C
 3470 CONTINUE
      IF (HPSF(1) .LE. 0) THEN
         NSTAR = NSTAR-1
         IF (NSTAR .LE. 0) THEN
            CALL STUPID 
     .         ('All PSF stars defective.  Please start over.')
            CALL CLFILE (3)
            RETURN
         END IF
         DO I=1,NSTAR
            J = I+1
            ID(I) = ID(J)
            HPSF(I) = HPSF(J)
            XCEN(I) = XCEN(J)
            YCEN(I) = YCEN(J)
            SKY(I) = SKY(J)
            WEIGHT(I) = WEIGHT(J)
            SATR8D(I) = SATR8D(J)
         END DO
         GO TO 3470
      END IF
C
      SCALE = HPSF(1)
      DO ISTAR=NSTAR,1,-1
         HPSF(ISTAR) = HPSF(ISTAR)/HPSF(1)
      END DO
C
C Tabulate the constant part of the PSF.
C
      MIDDLE = (NPSF+1)/2
      MIDSQ = MIDDLE**2
      NPSFSQ = ((NPSF-1)/2)**2
      DO J=1,NPSF
         JDY = J-MIDDLE
         JDYSQ = JDY**2
         DY = REAL(JDY)/2.
         DO I=1,NPSF
            IDX = I-MIDDLE
            K = IDX**2 + JDYSQ
            IF (K .LE. MIDSQ) THEN
               IN(I,J) = .TRUE.
               DX = REAL(IDX)/2.
               CON(I,J) = SCALE * 
     .              PROFIL(IPSTYP, DX, DY, PAR, DFDX, DFDY, V, 0)
               IF (K .GE. NPSFSQ) THEN
                  EDGE(I,J) = .TRUE.
               ELSE
                  EDGE(I,J) = .FALSE.
               END IF
            ELSE
               IN(I,J) = .FALSE.
               CON(I,J) = 0.
               DO K=1,NEXP
                  PSF(I,J,K) = 0.
               END DO
            END IF
         END DO
      END DO
      IF (IEXPAND+IFRAC .LT. 0) THEN
         PSFMAG = 0.0D0
         DO J=1,NPSF
            JDY = J-MIDDLE
            JDYSQ = JDY**2
            DO I=1,NPSF
               IF (IN(I,J)) THEN
                  IDX = I-MIDDLE
                  IF (IDX**2+JDYSQ .LE. IRADSQ) PSFMAG = PSFMAG + 
     .                 CON(I,J)
               END IF
            END DO
         END DO
         PSFMAG = 25.D0-2.5D0*DLOG10(PSFMAG/4.D0)
         GO TO 2900
      END IF
C
C=======================================================================
C
C Now compute look-up tables.
C
 3500 CONTINUE
C
C MIDDLE is the center of the look-up table, which will correspond to
C the centroid of the analytic PSF.  MIDSQ is the square of the
C radius of the PSF table, plus a pixel's worth of slack just to
C make sure you'll always have a 4x4 array suitable for interpolation.
C
      DO K=1,NEXP
         DO J=1,NPSF
            DO I=1,NPSF
               PSF(I,J,K) = 0.0
            END DO
         END DO
      END DO
C
c     NPASS = AMIN0(NSTAR-1,NPASS)
      DO 4950 JY=1,NPSF
         IF (WATCH .GT. -1.5) THEN
            WRITE (LINE,*) '  Computed', JY, '  rows of', NPSF, 
     .           '  in the PSF.'
            CALL OVRWRT (LINE(1:60), 2)
         END IF
         RDY = REAL(JY-MIDDLE)/2.
C
         DO 4940 IX=1,NPSF
C
C Don't waste time with pixels outside a radius = (MIDDLE-1).
C
            IF (.NOT. IN(IX,JY)) GO TO 4940
            RDX = REAL(IX-MIDDLE)/2.
            DO 4935 ITER=0,NPASS
C
C Initialize accumulators.
C
            TERM(1) = 1.
            SUMSQ = 0.
            SUMN = 0.
            DO L=1,NEXP
               V(L) = 0.
               DO K=1,NEXP
                  C(K,L) = 0.
               END DO
            END DO
C
C Now, for this point in the set of lookup table(s) [(IX,JY), where
C (MIDDLE,MIDDLE) corresponds to the centroid of the PSF], consult
C each of the PSF stars to determine the value(s) to put into the
C table(s).
C
            DO 4900 ISTAR = 1,NSTAR
               IF (SATR8D(ISTAR) .OR. (HPSF(ISTAR) .LT. 0.)) GO TO 4900
C
C What pixels in the original image constitute a 4x4 box surrounding
C this (IX,JY) in the PSF, as referred to this star's centroid?
C
               DX = XCEN(ISTAR) + RDX
               DY = YCEN(ISTAR) + RDY
               I = INT(DX)
               J = INT(DY)
C
C The 4x4 box is given by  I-1 <= x <= I+2, J-1 <= y <= J+2.
C
               IF ((I .LT. 2) .OR. (J .LT. 2) .OR. 
     .              (I+2 .GT. NCOL) .OR.
     .              (J+2 .GT. NROW)) GO TO 4900
C
               DO L=J-1,J+2
                  DO K=I-1,I+2
                     IF ((PIC(K,L) .GT. HIBAD) .OR.
     .                    (PIC(K,L) .LT. -HIBAD)) GO TO 4900
                  END DO
               END DO
C
               DX = DX - I
               DY = DY - J
C
C The point which corresponds PRECISELY to the offset (RDX,RDY)
C from the star's centroid, lies a distance (DX,DY) from pixel
C (I,J).
C
C Use bicubic interpolation to evaluate the residual PSF amplitude
C at this point.  Scale the residual up to match the first PSF star.
C
               DP = BICUBC(PIC(I-1,J-1), NCOL, DX, DY, DFDX, DFDY)
     .              - SKY(ISTAR)
               DP = DP/HPSF(ISTAR)
               SUMSQ = SUMSQ + ABS(DP)
               SUMN = SUMN + 1.
               IF (IEXPAND .GE. 1) THEN
                  TERM(2) = (XCEN(ISTAR)-1.)/XMID-1.
                  TERM(3) = (YCEN(ISTAR)-1.)/YMID-1.
                  IF (IEXPAND .GE. 2) THEN
                     TERM(4) = 1.5*TERM(2)**2-0.5
                     TERM(5) = TERM(2)*TERM(3)
                     TERM(6) = 1.5*TERM(3)**2-0.5
                     IF (IEXPAND .GE. 3) THEN
                        TERM(7) = TERM(2)*(5.*TERM(4)-2.)/3.
                        TERM(8) = TERM(4)*TERM(3)
                        TERM(9) = TERM(2)*TERM(6)
                        TERM(10) = TERM(3)*(5.*TERM(6)-2.)/3.
                     END IF
                  END IF
               END IF
C
C               IF (IFRAC .GE. 1) THEN
C
C INSERT CODE HERE
C
               IF (ITER .GT. 0) THEN
                  OLD = 0.
                  DO K=1,NEXP
                     OLD = OLD + PSF(IX,JY,K) * TERM(K)
                  END DO
C
                  IF (ITER .LE. MAX0(3,NPASS/2)) THEN
                     W = HPSF(ISTAR)/(1. + (ABS(DP - OLD)/SIG))
                  ELSE 
                     W = HPSF(ISTAR)/(1. + ((DP - OLD)/SIG)**2)
                  END IF
               ELSE
                  W = HPSF(ISTAR)
               END IF
C
               W = W*WEIGHT(ISTAR)
               DO K=1,NEXP
                  WT = W*TERM(K)
                  V(K) = V(K) + WT*DP
                  DO L=1,NEXP
                     C(K,L) = C(K,L) + WT*TERM(L)
                  END DO
               END DO
 4900       CONTINUE
C
            IF (SUMN .LT. NEXP) THEN
               CALL STUPID 
     .            ('Not enough PSF stars.  Please start over.')
               CALL CLFILE (3)
               RETURN
            END IF
            CALL INVERS (C, MAXEXP, NEXP, ISTAT)
            IF (ISTAT .NE. 0) THEN
               CALL STUPID ('Singular matrix.')
               CALL CLFILE (3)
               RETURN
            END IF
            CALL VMUL (C, MAXEXP, NEXP, V, A)
            DO K=1,NEXP
               PSF(IX,JY,K) = A(K)
            END DO
            IF (SUMN .LE. NEXP) GO TO 4940
            SIG = 1.2533*SUMSQ/SQRT(SUMN*(SUMN - NEXP))
 4935       CONTINUE
 4940    CONTINUE
 4950 CONTINUE
C
C Make the average value outside the PSF radius identically zero.
C
      DX = 0.
      K = 0
      DO J=1,NPSF
         DO I=1,NPSF
            IF (EDGE(I,J)) THEN
               DX = DX+CON(I,J)+PSF(I,J,1)
               K = K+1
            END IF
         END DO
      END DO
      DX = DX/REAL(K)
      DO J=1,NPSF
         DO I=1,NPSF
            IF (IN(I,J)) PSF(I,J,1) = PSF(I,J,1)-DX
         END DO
      END DO
      CALL TBLANK
C
C At this point, we must be sure that any higher order terms
C in the PSF [i.e. those that go as powers of (XCEN-XMID) and 
C (YCEN-YMID)] contain zero volume, so that the total volume
C of the PSF is independent of position.  This will be done
C by looking at the total flux contained in the look-up
C tables of index 2 and higher.  The analytic function
C will be scaled to the same total flux and subtracted from
C each of the higher lookup tables and added into the
C lookup table of index 1.  I scale the analytic function
C instead of simply transferring the net flux from the 
C higher tables to table 1, because it is poor fits of
C the analytic profile to the image data which has caused
C the net flux in the lookup tables to depend upon position.
C
C Compute the brightness weighted average value of each of 
C the terms in the polynomial expansion.
C
      DO K=1,NEXP
         TERM(K) = 0.
      END DO
C
      DO ISTAR=NSTAR,1,-1
         DX = (XCEN(ISTAR)-1.)/XMID-1.
         DY = (YCEN(ISTAR)-1.)/YMID-1.
         W = WEIGHT(ISTAR)*HPSF(ISTAR)
         TERM(1) = TERM(1) + W
         TERM(2) = TERM(2) + W*DX
         TERM(3) = TERM(3) + W*DY
         IF (IEXPAND .GE. 2) THEN
            DX2 = DX**2
            DY2 = DY**2
            TERM(4) = TERM(4) + W*(1.5*DX2-0.5)
            TERM(5) = TERM(5) + W*(DX*DY)
            TERM(6) = TERM(6) + W*(1.5*DY2-0.5)
            IF (IEXPAND .GE. 3) THEN
               TERM(7) = TERM(7) + DX*(5.*DX2-3.)/2.
               TERM(8) = TERM(8) + DY*(1.5*DX2-0.5)
               TERM(9) = TERM(9) + DX*(1.5*DY2-0.5)
               TERM(10) = TERM(10) + DY*(5.*DY2-3.)/2.
            END IF
         END IF
C
C            IF (IFRAC .GE. 1) THEN
C
C INSERT CODE HERE
C
      END DO
C
      DO K=NEXP,1,-1
         TERM(K) = TERM(K)/TERM(1)
      END DO
C
C Okey doke.  Now if there is any net volume contained in any of the
C higher order (variable) terms of the PSF, we will remove it in two
C components.  First of all, there is the possibility that there were 
C some errors in the sky estimates which were correlated with position.
C Then there is the possibility that there was some systematic error
C in estimating HPSF across the frame.  We correct these by subtracting
C any net volume in the outermost pixels of the higer-order look-up 
C tables and by removing a scaled copy of the constant PSF to ensure
C zero net volume overall.
C
C Ensure that the array CON has zero net value around the periphery, 
C but that it retains its total net volume.  (I.e., ensure that the 
C constant look-up table --- PSF(I,J,1) --- cancels any non-zero 
C value of the analytic model at the PSF radius.)
C
      RMIN = (MIDDLE-2.414)**2
      SUM1 = 0.
      SUM2 = 0.
      N1 = 0
      PSFMAG = 0.D0
      DO JJ=0,1
         DO J=1+JJ,NPSF-JJ,2
            MY = (J-MIDDLE)**2
            DO II=0,1
               DO I=1+II,NPSF-II,2
                  IF (IN(I,J)) THEN
                     CON(I,J) = CON(I,J) + PSF(I,J,1)
                     PSFMAG = PSFMAG+DBLE(CON(I,J))
                     IF (MY+(I-MIDDLE)**2 .GT. RMIN) THEN
                        N1 = N1+1
                        SUM1 = SUM1 + CON(I,J)
                     ELSE
                        SUM2 = SUM2 + CON(I,J)
                     END IF
                  END IF
               END DO
            END DO
         END DO
      END DO
      PSFMAG = 25.D0-2.5D0*DLOG10(PSFMAG/4.D0)
C
      RATIO = SUM1+SUM2
      SUM2 = RATIO/SUM2
      SUM1 = SUM1/N1
      DO JJ=0,1
         DO J=1+JJ,NPSF-JJ,2
            MY = (J-MIDDLE)**2
            DO II=0,1
               DO I=1+II,NPSF-II,2
                  IF (IN(I,J)) CON(I,J) = SUM2*(CON(I,J)-SUM1)
               END DO
            END DO
         END DO
      END DO
      IF (NEXP .LT. 2) GO TO 2800
C
C Now make sure that all the higher-order tables have
C zero around the periphery AND zero total volume.
C
      DO K=2,NEXP
         SUM1 = 0.
         SUM2 = 0.
         N1 = 0
         N2 = 0
         DO JJ=0,1
            DO J=1+JJ,NPSF-JJ,2
               MY = (J-MIDDLE)**2
               DO II=0,1
                  DO I=1+II,NPSF-II,2
                     IF (IN(I,J)) THEN
                        IF (MY+(I-MIDDLE)**2 .GT. RMIN) THEN
                           SUM1 = SUM1+PSF(I,J,K)
                           N1 = N1+1
                        ELSE
                           SUM2 = SUM2+PSF(I,J,K)
                           N2 = N2+1
                        END IF
                     END IF
                  END DO
               END DO
            END DO
         END DO
         SUM1 = SUM1/N1
         SUM2 = SUM2-N2*SUM1
         SUM2 = SUM2/RATIO
         DO JJ=0,1
            DO J=1+JJ,NPSF-JJ,2
               DO II=0,1
                  DO I=1+II,NPSF-II,2
                     IF (IN(I,J)) THEN
                        PSF(I,J,K) = PSF(I,J,K)-SUM1-SUM2*CON(I,J)
                        PSF(I,J,1) = PSF(I,J,1) + TERM(K)*
     .                       (SUM1+SUM2*CON(I,J))
                     END IF
                  END DO
               END DO
            END DO
         END DO
      END DO
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
 2800 CONTINUE
C
C If there are saturated stars, we must now guesstimate their positions
C and peak brightnesses.  Determine these from single-profile fits a la
C the PEAK routine, with a fitting radius equal to half the PSF radius.
C Subtract the analytic part of their profile from the image, and
C go back and redetermine the lookup tables.
C
      IF (SATUR8 .AND. (OPT(18) .GT. 0.5)) THEN
C        PERR = 0.01*OPT(19)
C        PKERR = 0.01*OPT(20)/(PAR(1)*PAR(2))
         CALL TBLANK
         DO ISTAR=1,NSTAR
            IF (SATR8D(ISTAR) .AND. (HPSF(ISTAR) .GT. 0.)) THEN
               LX = MAX0( 1, INT(XCEN(ISTAR)-PSFRAD)-1 )
               LY = MAX0( 1, INT(YCEN(ISTAR)-PSFRAD)-1 )
               MX = MIN0(NCOL, INT(XCEN(ISTAR)+PSFRAD)+2 )
               MY = MIN0(NROW, INT(YCEN(ISTAR)+PSFRAD)+2 )
               K = MX-LX+1
               L = MY-LY+1
               DX = (XCEN(ISTAR)-1.)/XMID-1.
               DY = (YCEN(ISTAR)-1.)/YMID-1.
               HPSF(ISTAR) = 3.*HPSF(1)
               XCEN(ISTAR) = XCEN(ISTAR)-LX+1.
               YCEN(ISTAR) = YCEN(ISTAR)-LY+1.
               CALL PKFIT (PIC(LX,LY), K, L, NCOL, XCEN(ISTAR), 
     .              YCEN(ISTAR), HPSF(ISTAR), SKY(ISTAR), 0.5*PSFRAD,
     .              LOBAD, HIBAD, PHPADU, RONOIS, 0., 0., SCALE, 
     .              IPSTYP, PAR, MAXPAR, NPAR,
     .              PSF, MAXPSF, MAXEXP, NPSF, NEXP, IFRAC, DX,
     .              DY, ERRMAG, CHI, SHARP, N)
               XCEN(ISTAR) = XCEN(ISTAR)+LX-1.
               YCEN(ISTAR) = YCEN(ISTAR)+LY-1.
               WRITE (6,666) ID(ISTAR), XCEN(ISTAR), YCEN(ISTAR), 
     .              PSFMAG-2.5*ALOG10(HPSF(ISTAR)), 
     .              AMIN1(2.0, 1.086*ERRMAG/HPSF(ISTAR)), 
     .              REAL(N), CHI, SHARP
  666               FORMAT (I7, 4F9.3, F9.0, F9.2, F9.3)
               LX = MAX0( 1, INT(XCEN(ISTAR)-PSFRAD)-1 )
               LY = MAX0( 1, INT(YCEN(ISTAR)-PSFRAD)-1 )
               MX = MIN0(NCOL, INT(XCEN(ISTAR)+PSFRAD)+2 )
               MY = MIN0(NROW, INT(YCEN(ISTAR)+PSFRAD)+2 )
               RDX = SCALE*HPSF(ISTAR)
               DO J=LY,MY
                  DY = REAL(J) - YCEN(ISTAR)
                  W = DY**2
                  DO I=LX,MX
                     IF (PIC(I,J) .LE. HIBAD) THEN
                        DX = REAL(I) - XCEN(ISTAR)
                        RDY = RDX * PROFIL(IPSTYP, 
     .                       DX, DY, PAR, DFDX, DFDY, TERM, 0)
                        IF (RDY+SKY(ISTAR) .LE. HIBAD) THEN
                           PIC(I,J) = PIC(I,J) - RDY
                        ELSE
                           PIC(I,J) = RDY+SKY(ISTAR)
C
C Ensuring it is greater than HIBAD, and hence will be ignored.
C
                        END IF
                     END IF
                  END DO
               END DO
               SATR8D(ISTAR) = .FALSE.
               WEIGHT(ISTAR) = 0.5
            END IF
         END DO
         CALL TBLANK
         SATUR8 = .FALSE.
         GO TO 3500
      END IF
C
C---------------------------------------------------------------------
C
 2900 CONTINUE
C
C Write PSF file header.
C
      WRITE (3,202) LABEL, NPSF, NPAR, NTAB(IEXPAND), 5*IFRAC,
     .     PSFMAG, SCALE, XMID, YMID
  202 FORMAT (1X, A8, 4I5, F9.3, F15.3, 2F9.1)
      WRITE (3,201) (PAR(L), L=1,NPAR)
C
      IF (NEXP .GT. 0) THEN
         DO K=1,NEXP
            WRITE (3,204) ((PSF(I,J,K), I=1,NPSF), J=1,NPSF)
  204       FORMAT (1X, 1P, 6E13.6)
         END DO
      END IF
      CALL CLFILE (3)
C
C Now generate the file of neighbors.
C
      MAGFIL = SWITCH(PSFFIL, CASE('.nei'))
      CALL TBLANK
      WRITE (6,*) 'File with PSF stars and neighbors = ', MAGFIL
 2920 CALL OUTFIL (3, MAGFIL, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//MAGFIL)
         MAGFIL = 'GIVE UP'
         CALL GETNAM ('Name for neighbor-star file:', MAGFIL)
         IF ((MAGFIL .EQ. 'END-OF-FILE') .OR.
     .        (MAGFIL .EQ. 'GIVE UP')) THEN
            MAGFIL = ' '
            RETURN
         ELSE
            MAGFIL = EXTEND(MAGFIL, CASE('nei'))
            GO TO 2920
         END IF
      END IF
C
C First, write out the PSF stars.
C
      IF (RADIUS .GT. 0.001) THEN
         I = 7
      ELSE
         I = 6
      END IF
C
      CALL WRHEAD (3, 3, NCOL, NROW, I, LOBAD, HIBAD, THRESH, AP1, 
     .     PHPADU, READNS, RADIUS)
      DO I=1,NSTAR
         T1 = RNDOFF(XCEN(I), 9, 3)
         T2 = RNDOFF(YCEN(I), 9, 3)
         TEXT = RNDOFF(SKY(I), 9, 3)
         WRITE (3,280) ID(I), T1, T2, APMAG(I), TEXT, FLAG(I)
 280        FORMAT (I7, 2A9, F9.3, A9, 3X, A1)
      END DO
      IF (NSTAR .GE. NTOT) GO TO 9000
C
C Now look for all stars within a radius equal to 
C 1.5*PSFRAD+2.*FITRAD+1 of any of the PSF stars.
C
      RSQ = (1.5*PSFRAD+2.*FITRAD+1.)**2
      L = NSTAR+1
C
C L will point at the first irrelevant star.
C
      DO I=1,NSTAR
         J = L
 5020    IF ((XCEN(J)-XCEN(I))**2 + (YCEN(J)-YCEN(I))**2 .LE. RSQ) THEN
            CALL SWAPP (ID, XCEN, YCEN, APMAG, SKY, J, L)
            T1 = RNDOFF(XCEN(L), 9, 3)
            T2 = RNDOFF(YCEN(L), 9, 3)
            TEXT = RNDOFF(SKY(L), 9, 3)
            WRITE (3,280) ID(L), T1, T2, APMAG(L), TEXT
            L = L+1
         END IF
         IF (J .LT. NTOT) THEN
            J = J+1
            GO TO 5020
         END IF
      END DO
C
C Now all stars in the stack between positions NSTAR+1 and L-1,
C inclusive, lie within the specified radius of the PSF stars and have
C been written out.  Finally, look for any stars within a radius 
C equal to 2*FITRAD+1 of any of THESE stars, and of each other.
C
      IF (L .GT. NTOT) GO TO 9000
      RSQ = (2.*FITRAD+1)**2
      I = NSTAR+1
 5110 J = L
 5120 IF ((XCEN(J)-XCEN(I))**2 + (YCEN(J)-YCEN(I))**2 .LE. RSQ) THEN
         CALL SWAPP (ID, XCEN, YCEN, APMAG, SKY, J, L)
         T1 = RNDOFF(XCEN(L), 9, 3)
         T2 = RNDOFF(YCEN(L), 9, 3)
         TEXT = RNDOFF(SKY(L), 9, 3)
         WRITE (3,280) ID(L), T1, T2, APMAG(L), TEXT
         L = L+1
      END IF
      IF (J .LT. NTOT) THEN
         J = J+1
         GO TO 5120
      END IF
C
      I = I+1
      IF (I .LT. L) GO TO 5110
 9000 CALL CLFILE (3)
      RETURN
      END
C
C#######################################################################
C
      SUBROUTINE  FITANA  (PIC, NCOL, NROW, ID, H, XCEN, YCEN, 
     .     SKY, SATR8D, NSTAR, FITRAD, WATCH, IPSTYP, PAR, NPAR, CHI)
C
C This subroutine fits the ANALYTIC profile to the selected stars.
C
C  OFFICIAL DAO VERSION:    2001 June 13
C
      IMPLICIT NONE
      INTEGER MAXPAR, MAXN, NCOL, NROW
      PARAMETER  (MAXPAR=6, MAXN=400)
      CHARACTER LINE*80
      REAL PIC(NCOL,*)
      REAL H(*), XCEN(*), YCEN(*), SKY(*), CLAMP(MAXPAR), OLD(MAXPAR)
      REAL C(MAXPAR,MAXPAR), V(MAXPAR), PAR(MAXPAR), T(MAXPAR), 
     .     Z(MAXPAR)
      REAL XCLAMP(MAXN), YCLAMP(MAXN), XOLD(MAXN), YOLD(MAXN)
      INTEGER ID(*)
      REAL ABS, PROFIL
      LOGICAL SATR8D(*)
C
      REAL DHN, DHD, DXN, DXD, DYN, DYD, DX, DY, DYSQ
      REAL PEAK, DP, PROD, DHDXC, DHDYC, P, WT
      REAL RSQ, SUMWT, OLDCHI, CHI, WATCH, FITRAD, RADLIM
      INTEGER I, J, K, L, LX, LY, MX, MY, ISTAT
      INTEGER ISTAR, MPAR, NITER, NPAR, IPSTYP, NSTAR
      LOGICAL FULL
C
C-----------------------------------------------------------------------
C
      FULL = .FALSE.
      RADLIM = 3.*FITRAD
      DO I=1,NPAR
         Z(I) = 0.
         OLD(I) = 0.
         CLAMP(I) = 0.5
      END DO
      CLAMP(1) = 2.
      CLAMP(2) = 2.
      NITER = 0
      OLDCHI = 1.
      DO I=1,NSTAR
         XOLD(I) = 0.
         YOLD(I) = 0.
         XCLAMP(I) = 1.
         YCLAMP(I) = 1.
      END DO
C
      MPAR = 2
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Now we will fit an integrated analytic function to the central part 
C of the stellar profile.  For each star we will solve for three 
C parameters: (1) H, the central height of the model profile (above 
C sky); (2) XCEN, C the centroid of the star in x; and (3) YCEN, 
C likewise for y.  In addition, from ALL STARS CONSIDERED TOGETHER
C we will determine any other parameters, PAR(i), required to describe 
C the profile.  We will use a circle of radius FITRAD centered on the 
C position of each PSF star. NOTE THAT we do not fit the data to an
C actual analytic profile, but rather the function is numerically
C integrated over the area of each pixel, and the observed data are fit
C to these integrals.
C
      RSQ = FITRAD**2
 1000 NITER = NITER+1
      IF (NITER .GT. 1000) THEN
         CALL STUPID ('Failed to converge.')
         PAR(1) = -1.
         RETURN
      END IF
C
C Initialize the big accumulators.
C
      DO I=1,NPAR
         V(I)=0.0
         DO J=1,NPAR
            C(J,I)=0.0
         END DO
      END DO
      CHI = 0.0
      SUMWT = 0.0
C
C Using the analytic model PSF defined by the current set of parameters,
C compute corrections to the brightnesses and centroids of all the PSF
C stars.  MEANWHILE, accumulate the corrections to the model parameters.
C
      DO 1400 ISTAR=1,NSTAR
      IF (SATR8D(ISTAR) .OR. (H(ISTAR) .LT. 0.)) GO TO 1400
      IF ( ( INT(XCEN(ISTAR)-FITRAD) .LT. 0 ) .OR.
     .     ( INT(XCEN(ISTAR)+FITRAD) .GT. NCOL ) .OR.
     .     ( INT(YCEN(ISTAR)-FITRAD) .LT. 0 ) .OR.
     .     ( INT(YCEN(ISTAR)+FITRAD) .GT. NROW ) ) THEN
         WRITE (6,701) ID(ISTAR), ' is too near edge of frame.'
  701    FORMAT (I7, A)
         H(ISTAR) = -1.E20
         GO TO 1400
      END IF
      LX = INT(XCEN(ISTAR)-FITRAD) + 1
      LY = INT(YCEN(ISTAR)-FITRAD) + 1
      MX = INT(XCEN(ISTAR)+FITRAD)
      MY = INT(YCEN(ISTAR)+FITRAD)
C
      DHN = 0.0D0
      DHD = 0.0D0
      DXN = 0.0D0
      DXD = 0.0D0
      DYN = 0.0D0
      DYD = 0.0D0
      DO J=LY,MY
         DY = REAL(J) - YCEN(ISTAR)
         DYSQ = DY**2
         DO I=LX,MX
            DX = REAL(I) - XCEN(ISTAR)
            WT = (DX**2+DYSQ)/RSQ
            IF (WT .LT. 1.) THEN
               P = PROFIL(IPSTYP, DX, DY, PAR, DHDXC, DHDYC, T, 0)
               DP = PIC(I,J) - H(ISTAR)*P - SKY(ISTAR)
               DHDXC = H(ISTAR)*DHDXC
               DHDYC = H(ISTAR)*DHDYC
C              WT = 5./(5.+WT/(1.-WT))
               WT = 1.-WT
               PROD = WT*P
               DHN = DHN + PROD*DP
               DHD = DHD + PROD*P
               PROD = WT*DHDXC
               DXN = DXN + PROD*DP
               DXD = DXD + PROD*DHDXC
               PROD = WT*DHDYC
               DYN = DYN + PROD*DP
               DYD = DYD + PROD*DHDYC
            END IF
         END DO
      END DO
C
      H(ISTAR) = H(ISTAR) + DHN/DHD
C
      DXN = DXN/DXD
      IF (XOLD(ISTAR)*DXN .LT. 0.) XCLAMP(ISTAR) = 0.5*XCLAMP(ISTAR)
      XOLD(ISTAR) = DXN
      IF ((XCLAMP(ISTAR) .LE. 0.) .OR. (YCLAMP(ISTAR) .LE. 0)) THEN
         H(ISTAR) = -10000.
         GO TO 1400
      END IF
      XCEN(ISTAR) = XCEN(ISTAR)+DXN/(1.+ABS(DXN)/XCLAMP(ISTAR))
C
      DYN = DYN/DYD
      IF (YOLD(ISTAR)*DYN .LT. 0.) YCLAMP(ISTAR) = 0.5*YCLAMP(ISTAR)
      YOLD(ISTAR) = DYN
      YCEN(ISTAR) = YCEN(ISTAR)+DYN/(1.+ABS(DYN)/YCLAMP(ISTAR))
C
      PEAK = H(ISTAR) * PROFIL(IPSTYP,0.,0., PAR, DHDXC, DHDYC, T, 0)
      DO J=LY,MY
         DY = REAL(J)-YCEN(ISTAR)
         DYSQ = DY**2
         DO I=LX,MX
            DX = REAL(I)-XCEN(ISTAR)
            WT = (DX**2+DYSQ)/RSQ
            IF (WT .LT. 1.) THEN
               P = PROFIL(IPSTYP, DX, DY, PAR, DHDXC, DHDYC, T, 1)
               DP = PIC(I,J) - H(ISTAR)*P - SKY(ISTAR)
               DO K=1,MPAR
                  T(K) = H(ISTAR)*T(K)
               END DO
               CHI = CHI + ABS(DP/PEAK)
               SUMWT = SUMWT + 1.
C              WT = 5./(5.+WT/(1.-WT))
               WT = 1.-WT
C              WT = WT/(1.+(0.25*(DP/(OLDCHI*PEAK))**4))
               WT = WT/(1.+(0.125*(DP/(OLDCHI*PEAK))**8))
               DO K=1,MPAR
                  V(K) = V(K) + WT*DP*T(K)
                  DO L=1,MPAR
                     C(L,K) = C(L,K) + WT*T(L)*T(K)
                  END DO
               END DO
            END IF
         END DO
      END DO
 1400 CONTINUE
C
C Correct the fitting parameters.
C
      CALL INVERS (C, MAXPAR, MPAR, ISTAT)
      CALL VMUL (C, MAXPAR, MPAR, V, Z)
      DO I=1,MPAR
         IF (Z(I)*OLD(I) .LT. 0.) THEN
            CLAMP(I) = 0.5*CLAMP(I)
c        ELSE
c           CLAMP(I) = AMIN1(1., 1.1*CLAMP(I))
         END IF
         OLD(I) = Z(I)
         Z(I) = CLAMP(I)*Z(I)
      END DO
      Z(1) = AMAX1(-0.1*PAR(1), AMIN1(0.1*PAR(1), Z(1)))
      Z(2) = AMAX1(-0.1*PAR(2), AMIN1(0.1*PAR(2), Z(2)))
      Z(3) = Z(3)/(1.+ABS(Z(3))/(AMIN1(0.1,1.-ABS(PAR(3)))))
      DO I=1,MPAR
         PAR(I) = PAR(I)+Z(I)
      END DO
C
      IF ((PAR(1) .GT. RADLIM) .OR. (PAR(2) .GT. RADLIM)) THEN
         PAR(1) = -1.
         RETURN
      END IF
      IF ((PAR(1) .LE. 0.05) .OR. (PAR(2) .LE. 0.05)) THEN
         PAR(1) = -1.
         RETURN
      END IF
C
      SUMWT = SUMWT - REAL(MPAR + 3*NSTAR)
      IF (SUMWT .GT. 0) THEN
         CHI = 1.2533*CHI/SUMWT
      ELSE
         CHI = 9.9999
      END IF
C
      WRITE (LINE,661) '    ', CHI, (PAR(I), I=1,MPAR)
  661 FORMAT (A4, F7.4, 6F10.5)
      I=10*MPAR+14
      IF (WATCH .GT. -1.5) CALL OVRWRT (LINE(1:I), 2)
      IF (MPAR .EQ. NPAR) THEN
         IF (ABS(OLDCHI/CHI-1.) .LT. 1.E-5) THEN
            WRITE (LINE(1:4),661) '>>  '
            CALL OVRWRT (LINE(1:I), 3)
            RETURN
         END IF
      ELSE
         IF (ABS(OLDCHI/CHI-1.) .LT. 1.E-3) THEN
            MPAR = MPAR+1
C           OLDCHI = 0.
            GO TO 1000
         END IF
      END IF
C
      OLDCHI = CHI
      GO TO 1000
      END
C
C#######################################################################
C
C Exchange two stars in the star list.
C
C   OFFICIAL DAO VERSION:       1991 May 10
C
      SUBROUTINE SWAPP (ID, X, Y, A, S, I, J)
      IMPLICIT NONE
      REAL X(*), Y(*), A(*), S(*)
      INTEGER ID(*)
C
      REAL HOLD
      INTEGER I, J, IHOLD
C
      IHOLD = ID(I)
      ID(I) = ID(J)
      ID(J) = IHOLD
      HOLD = X(I)
      X(I) = X(J)
      X(J) = HOLD
      HOLD = Y(I)
      Y(I) = Y(J)
      Y(J) = HOLD
      HOLD = A(I)
      A(I) = A(J)
      A(J) = HOLD
      HOLD = S(I)
      S(I) = S(J)
      S(J) = HOLD
      RETURN
      END
