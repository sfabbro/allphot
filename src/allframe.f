C
C#######################################################################
C
C                              allframe.f
C
C                 Official DAO version: 2005 June 15
C
C#######################################################################
C
C Abort if too many stars 2005 June 15
C SHARP pruning 2005 May 17
C
      IMPLICIT NONE
      INCLUDE 'arrays.inc'
C
C maxpic is the largest number of images that can be treated by
C any program.  Note that for allframe this must be three times
C larger than the number of input images.
C
      INTEGER MAXALL, MAXSTR, MAXLST, MAXCOL, MAXROW, RAT
      PARAMETER (MAXSTR=100 000, MAXLST=20 000 000)
      PARAMETER (MAXCOL=4099, MAXROW=4099, RAT=1)
C
C MAXSTR is the maximum number of stars in the star list.
C
C MAXPIC is the maximum number of frames.  NOTE THAT THE ARRAY DIMENSIONS
C      IN maxpic.inc MUST BE THREE TIMES THE NUMBER OF INPUT IMAGES.
C
C MAXLST is the maximum number of star images and is LESS THAN OR EQUAL TO
C      MAXSTR*MAXPIC/3.  If all stars are in all images, then 
C      MAXLST = MAXSTR*MAXPIC/3.  However, if the images do not fully overlap,
C      most especially with modern mosaic cameras, it is possible that
C      MAXLST could be << MAXSTR*MAXPIC/3.  This allows substantial savings in
C      the computer resources required by the program.
C
C RAT > 0 means allframe will compute a new fitting radius for each frame.
C
      INTEGER MAXPSF, MAXPAR, MAXEXP, NOPT, ITCLIP, MXTERM
      PARAMETER (NOPT=10, MAXPSF=207, MAXPAR=6, MAXEXP=10)
      PARAMETER (ITCLIP=2, MXTERM=10)
C 
C MAXCOL, MAXROW represent the maximum image size the program is designed to 
C        tolerate. MAXLST represents the total number of star images in all
C        frames, less than or equal to MAXSTR*MAXPIC/3.
C
C   NOPT is just the number of user-definable options (see below).
C
C Functions
C
      CHARACTER EXPAND*132, EXTEND*40, SWITCH*40, COMMAS*40, CASE*40,
     .     SQUEEZ*40, RNDOFF*9, DRNDFF*11
      REAL AMIN1, SQRT, USEPSF, PCTILE
      INTEGER RDPSF, LENGTH, IPSTYP
C
C Arrays
C
      CHARACTER OUTPUT(MAXPIC/3+1)*40, LBL(NOPT)*26, STRNG(10)*10
      DOUBLE PRECISION CX(MXTERM,MXTERM), CY(MXTERM,MXTERM), VX(MXTERM), 
     .     VY(MXTERM), ZX(MXTERM), ZY(MXTERM), DBLE
      DOUBLE PRECISION COEFF(2*MXTERM,MAXPIC/3), 
     .     CFFSQ(2*MXTERM,MAXPIC/3), FFEOC(2*MXTERM,MAXPIC/3), 
     .     CLAMP(2*MXTERM,MAXPIC/3)
      REAL OPT(NOPT), OMIN(NOPT), OMAX(NOPT), T(MXTERM)
      REAL ORIG(MAXCOL,MAXROW), DATA(MAXCOL,MAXROW), SIGM(MAXCOL,MAXROW)
      REAL FITRAD(MAXPIC/3), SEPCRT(MAXPIC/3), DMAG(MAXPIC/3), 
     .    XCEN(MAXPIC/3), XWID(MAXPIC/3), YCEN(MAXPIC/3), YWID(MAXPIC/3)
      REAL XC(MAXSTR), YC(MAXSTR), X(MAXSTR), Y(MAXSTR),
     .     XSUM(MAXSTR), XWT(MAXSTR), YSUM(MAXSTR), YWT(MAXSTR),
     .     DXOLD(MAXSTR), DYOLD(MAXSTR), DXC(MAXSTR), DXWT(MAXSTR),
     .     DYC(MAXSTR), DYWT(MAXSTR), XCLAMP(MAXSTR), YCLAMP(MAXSTR)
      REAL MAGERR(MAXLST), AMAG(MAXLST), SKY(MAXLST), MCLAMP(MAXLST), 
     .      MOLD(MAXLST), CHI(MAXLST), SHP(MAXLST)
      REAL PSF(MAXPSF,MAXPSF,MAXEXP), NMRTR(MAXSTR), CROWD(MAXSTR)
      REAL RCOL(MAXPIC/3), RROW(MAXPIC/3), LOBAD(MAXPIC/3), 
     .     HIBAD(MAXPIC/3), PHPADU(MAXPIC/3), RONOIS(MAXPIC/3)
      REAL PAR(MAXPAR)
      INTEGER IWHICH(MAXSTR,MAXPIC/3)
      INTEGER NOUT(MAXPIC/3), WROTE(MAXPIC/3), MODE(MAXPIC/3)
      INTEGER ID(MAXSTR)
      LOGICAL DONE(MAXSTR)
C
C Variables
C
      CHARACTER LINE*310
      CHARACTER OPTFIL*40, INPFIL*40, BACKUP*40, NEWFIL*40, TFRFIL*40
      CHARACTER STRNG1*9, STRNG2*9, STRNG3*9, STRNG4*9, STRNG5*9
      REAL PERERR, PROERR, DUM, SKYOUT, SKYISQ, SKYOSQ, PSFMAG, BRIGHT, 
     .     XPSF, YPSF, DVDXC, DVDYC, PSFRAD, THRESH, AP1, FITRSQ, FR,
     .     PKERR, PSFRSQ, RADSQ, CLMPMX, RADIUS, DYSQ, DX, WATCH, RSQ, 
     .     DY, DELTAX, DELTAY, SUMRES, SUMWT, DPOS, RELERR, D, SIGSQ, 
     .     WT, DWT, RANGE, SNMIN, SEPMAX, SEP1, XMIN, XMAX, YMIN, YMAX
      REAL NUMER, DENOM, PEAK, RHOSQ, DFDSIG, SRADSQ, CRDMAX, RMAX, CMAX
      REAL RATIO, SOFT, SHPMIN, SHPMAX, SHPCRT
      INTEGER I, NPAR, NPSF, NEXP, NFRAC, LX, LY, IX, JY
      INTEGER NPIC, IER, NSTAR, NL, MINSKY, L, MAXIT, KTERM, NC,
     .     NITER, IMNO, IEXP, IXMAX, JYMIN, NCNVRG, NDISAP, JYMAX, 
     .     INTRVL, N, MX, IXMIN, ISTAR, JSTAR, KSTAR, LSTAR, MY, J, 
     .     ITCONV, K, NTERM, NTFM, NPIX, NXTSKY, NADJ, NDONE, 
     .     KITER, LITER, NLIST
C     INTEGER MADRID                              ! MIDAS
      LOGICAL CENTER, CLIP, CONVRG, DOSKY, START, SHOW
C
C     INCLUDE 'MID_INCLUDE:ST_DEF.INC'            ! MIDAS
C     COMMON /VMR/ MADRID                         ! MIDAS
C     INCLUDE 'MID_INCLUDE:ST_DAT.INC'            ! MIDAS
C
      DATA LBL/'    CE (CLIPPING EXPONENT)',     ! 1
     .         '       CR (CLIPPING RANGE)',     ! 2
     .         '    GEOMETRIC COEFFICIENTS',     ! 3
     .         '            WATCH PROGRESS',     ! 4
     .         '        MINIMUM ITERATIONS',     ! 5
     .         '        MAXIMUM ITERATIONS',     ! 6
     .         '      PERCENT ERROR (in %)',     ! 7
     .         '      PROFILE ERROR (in %)',     ! 8
     .         '     IS (INNER SKY RADIUS)',     ! 9
     .         '     OS (OUTER SKY RADIUS)'/     ! 10
      DATA OPT / 6., 2.5, 6., 2., 5., 200., 0.75, 5., 2.5, 25./
      DATA OMIN  / 0., 0., -0.5, -3.5, 0., 1., 0., 0., 0., 2.5/
      DATA OMAX  / 16., 100., 20.5, 3.5, 100., 1000., 100., 100.,
     .     35., 50./
      DATA CENTER /.TRUE./, CRDMAX /400./
      data imid /maxpic*0/
      data soft /2./, ratio /1.4/
      data shpmin /-10./, shpmax/20./
C
C Set up the values of the optional parameters.
C
C Call OPTION, first with OPTFIL = 'allframe.opt' to set initial
C values for the optional parameters.  If the file isn't there, the
C routine will check that the default values (specified in the data
C statement above) are valid, and return here with those values intact.
C
C     CALL STSPRO ('-1')                             ! MIDAS
C     CALL STECNT('PUT', 1, 0, 0)                    ! MIDAS
      SHOW = .FALSE.
      START = .TRUE.
      MAXALL = MAXPIC/3
      CALL FABORT
c     if (rat .gt. 0) PRINT *,'ratio =',ratio
      NEWFIL = COMMAS(MAXSTR, I)
      TFRFIL = SQUEEZ(MAXCOL, J)
      INPFIL = SQUEEZ(MAXROW, K)
      OPTFIL = SQUEEZ(MAXALL, N)
      WRITE (6,6) NEWFIL(1:I), OPTFIL(1:N), TFRFIL(1:J), INPFIL(1:K)
    6 FORMAT (/'      Currently dimensioned for ', A,
     .     ' stars in ', A, ' frames of size ', A, 'x', A, '.'/)
      INPFIL = ' '
      MINSKY = 20
      OPTFIL=CASE('allframe.opt')
      CALL OPTION (OPTFIL, NOPT, LBL, OPT, OMIN, OMAX, 'OPT>', IER)
      CALL TBLANK
      IEXP = NINT(OPT(1))
      CLMPMX = 5.
      RANGE = OPT(2)
      WATCH = OPT(4)
      ITCONV = NINT(OPT(5))
      MAXIT = NINT(OPT(6))
      PERERR = 0.01*OPT(7)
      PROERR = 0.01*OPT(8)
      SKYISQ = OPT(9)**2
      SKYOUT = OPT(10)
      IF (SKYOUT .LT. OPT(9)) THEN
         CALL STUPID ('Outer sky radius is too small.'//
     .     '  Please increase it.')
         CALL OOPS
      END IF
      SKYOSQ = SKYOUT**2
      IF (OPT(3) .LE. 1.) THEN
         NADJ = 0
      ELSE IF (OPT(3) .LE. 3.) THEN
         NADJ = 1
      ELSE IF (OPT(3) .LE. 9.) THEN
         NADJ = 3
      ELSE IF (OPT(3) .LE. 16.) THEN
         NADJ = 6
      ELSE
         NADJ = 10
      END IF
      CLIP = .FALSE.
C
 1900 CALL GETNAM ('File with list of images:', INPFIL)
      IF ((INPFIL .EQ. 'END-OF-FILE') .OR. (INPFIL .EQ. 'EXIT')) 
     .     CALL BYEBYE
      INPFIL = EXTEND(INPFIL, CASE('mch'))
      BACKUP = SWITCH(INPFIL, CASE('.bck'))
      OPEN (1, FILE=EXPAND(BACKUP), ACCESS='SEQUENTIAL',
     .     STATUS='OLD', FORM='UNFORMATTED', IOSTAT=IER)
      IF (IER .EQ. 0) THEN
C
C-----------------------------------------------------------------------
C
         READ (1,END=900,ERR=900) NITER, NXTSKY, NPIC, NSTAR, NLIST, 
     .        NDISAP, NCNVRG, IEXP, CLMPMX, CENTER, RANGE, PERERR, 
     .        PROERR, SKYISQ, SKYOUT, SKYOSQ, NADJ, CLIP, 
     .        SEPMAX, CRDMAX, START
C
         J = 3*NPIC
         READ (1,END=900,ERR=900) (PICTUR(I), MODE(I), 
     .        IMGTYP(I), DATTYP(I), I=1,J)
C
         READ (1,END=900,ERR=900) (NCOL(I), NROW(I), 
     .        XCEN(I), XWID(I), YCEN(I), YWID(I),
     .        FITRAD(I), LOBAD(I), HIBAD(I), RONOIS(I), PHPADU(I), 
     .        DMAG(I), NOUT(I), SEPCRT(I), (COEFF(J,I), CFFSQ(J,I), 
     .        FFEOC(J,I), CLAMP(J,I), J=1,MODE(I)), I=1,NPIC)
C
         N = INT((NSTAR-1)/100)
         DO L=0,N
            J = 100*L + 1
            K = MIN0(100*(L+1), NSTAR)
            READ (1,END=900,ERR=900) ((IWHICH(I,IMNO), IMNO=1,NPIC),
     .            ID(I), XC(I), YC(I), XCLAMP(I), YCLAMP(I), 
     .            DXOLD(I), DYOLD(I), I=J,K)
         END DO
C
         N = INT((NLIST-1)/1000)
         DO L=0,N
            J = 1000*L + 1
            K = MIN0(1000*(L+1), NLIST)
            READ (1,END=900,ERR=900) (AMAG(I), MAGERR(I), SKY(I), 
     .           CHI(I), MOLD(I), MCLAMP(I), I=J,K)
         END DO
C
         READ (1,END=900,ERR=900) (OUTPUT(I), I=1,NPIC+1)
C
C-----------------------------------------------------------------------
C
         IF (OUTPUT(NPIC+1)(1:8) .EQ. 'COMPLETE') THEN
            NEWFIL = SWITCH(INPFIL, '.nmg')
            TFRFIL = SWITCH(INPFIL, '.tfr')
            DO J=1,NPIC
               RCOL(J) = REAL(NCOL(J))
               RROW(J) = REAL(NROW(J))
            END DO
            CALL TBLANK
            CALL STUPID ('Resuming crashed job.')
            GO TO 950
         END IF
  900    CALL STUPID ('Incomplete backup file.  Starting over.')
         CALL CLFILE (1)
      END IF
      CALL INFILE (1, INPFIL, IER)
      IF (IER .NE. 0) THEN
         CALL STUPID ('A.  Unable to open input file '//INPFIL)
         CALL OOPS
      END IF
C
      NEWFIL = SWITCH(INPFIL, '.nmg')
      TFRFIL = SWITCH(INPFIL, '.tfr')
      CALL DELFIL (2, TFRFIL, IER)
      CALL OUTFIL (2, TFRFIL, IER)
C
C Open previous photometry files to extract header information.
C
      NPIC = 0
 1910 NPIC = NPIC+1
 1915 LINE = ' '
      CALL RDCHAR (1, LINE, NC, I)
      IF (NC .LE. 0) GO TO 1920
      IF (I .NE. 0) THEN
         CALL STUPID ('Defective line in '//INPFIL)
         CALL STUPID (LINE)
         CALL OOPS
      END IF
C
      IF (NPIC .GT. MAXALL) THEN
         CALL STUPID ('Too many images!')
         CALL CLFILE (1)
         CALL CLFILE (2)
         CALL OOPS
      END IF
C
      MODE(NPIC) = 0
      READ (LINE,*,IOSTAT=IER) PICTUR(NPIC), (COEFF(I,NPIC), I=1,6),
     .        DMAG(NPIC), DUM, (COEFF(I,NPIC), I=7,20)
      IF (IER .EQ. 0) THEN
        MODE(NPIC) = 20
      ELSE 
         READ (LINE,*,IOSTAT=IER) PICTUR(NPIC), (COEFF(I,NPIC), 
     .        I=1,6), DMAG(NPIC), DUM, (COEFF(I,NPIC), I=7,12)
         IF (IER .EQ. 0) THEN
            MODE(NPIC) = 12
         ELSE
            READ (LINE,*,IOSTAT=IER) PICTUR(NPIC), 
     .           (COEFF(I,NPIC), I=1,6), DMAG(NPIC)
            IF (IER .EQ. 0) THEN
               MODE(NPIC) = 6
            ELSE
               CALL STUPID ('Unable to read input line:')
               WRITE (6,*) LINE(1:NC)
               CALL CLFILE (1)
               CALL CLFILE (2)
               CALL OOPS
            END IF
         END IF
      END IF
C
      IF (DMAG(NPIC) .GT. 50.) THEN
         CALL STUPID ('Invalid magnitude offset:')
         WRITE (6,*) LINE(1:NC)
         CALL CLFILE (1)
         CALL CLFILE (2)
         CALL OOPS
      END IF
C
      NL = LENGTH(PICTUR(NPIC))
C
C Ignore any .stc, .nmg, .mag, .fet, .dss, or .pds file.
C
      IF (PICTUR(NPIC)(NL-3:NL) .EQ. CASE('.stc')) GO TO 1915
      IF (PICTUR(NPIC)(NL-3:NL) .EQ. CASE('.nmg')) GO TO 1915
      IF (PICTUR(NPIC)(NL-3:NL) .EQ. CASE('.mag')) GO TO 1915
      IF (PICTUR(NPIC)(NL-3:NL) .EQ. CASE('.fet')) GO TO 1915
      IF (PICTUR(NPIC)(NL-3:NL) .EQ. CASE('.dss')) GO TO 1915
      IF (PICTUR(NPIC)(NL-3:NL) .EQ. CASE('.pds')) GO TO 1915
      CALL OVRWRT (' Reading '//PICTUR(NPIC), 2)
      CALL INFILE (3, PICTUR(NPIC), IER)
      IF (IER .NE. 0) THEN
         CALL STUPID ('B.  Unable to open input file '//PICTUR(NPIC))
         CALL OOPS
      END IF
C
      CALL RDHEAD (3, NL, IX, JY, LOBAD(NPIC), HIBAD(NPIC), 
     .     THRESH, AP1, PHPADU(NPIC), RONOIS(NPIC), FITRAD(NPIC))
      FITRAD(NPIC) = AMAX1(1.5, FITRAD(NPIC))
      CALL CLFILE (3)
      OUTPUT(NPIC) = SWITCH(PICTUR(NPIC), CASE('.alf'))
      WRITE (2,119) OUTPUT(NPIC), 99.9999, 9.9999
  119 FORMAT (1X, A30, 2F9.4)
C
C Open the image to make sure it is there.
C
      PICTUR(NPIC) = SWITCH(PICTUR(NPIC), ' ')
      IMGTYP(NPIC) = ' '
      DATTYP(NPIC) = ' '
      CALL OPNPIC (NPIC, 'R', SHOW, IER)
      IF (NCOL(NPIC) .LE. 0) THEN
         CALL STUPID ('C.  Unable to open input image '//PICTUR(NPIC))
         CALL OOPS
      END IF
      RCOL(NPIC) = REAL(NCOL(NPIC))
      RROW(NPIC) = REAL(NROW(NPIC))
      CALL CLPIC (NPIC, IER)
      IF ((NCOL(NPIC) .GT. MAXCOL) .OR. 
     .     (NROW(NPIC) .GT. MAXROW)) THEN
         CALL STUPID ('This picture is too large: '//PICTUR(NPIC))
         WRITE (6,69) MAXCOL, MAXROW
  69     FORMAT (/' Maximum image size =', I6, ' by', I6)
         CALL OOPS
      END IF
C
      DO I=1,6
         CFFSQ(I,NPIC) = COEFF(I,NPIC)**2
      END DO
C
      SEPCRT(NPIC) = 0.5*
     .     (CFFSQ(3,NPIC)+CFFSQ(4,NPIC)+CFFSQ(5,NPIC)+CFFSQ(6,NPIC))
      CALL INV (COEFF(1,NPIC), MODE(NPIC), RCOL(NPIC), RROW(NPIC), 
     .     XCEN(NPIC), XWID(NPIC), YCEN(NPIC), YWID(NPIC), 
     .     FFEOC(1,NPIC))
      GO TO 1910
C
 1920 NPIC = NPIC-1
      CALL OVRWRT ('                                        ', 2)
      WRITE (2,120)
  120 FORMAT (1X, 30('='))
      CALL CLFILE (2)
      INPFIL = SWITCH(INPFIL, CASE('.mag'))
 1921 CALL GETNAM ('File with list of stars:', INPFIL)
      IF (INPFIL .EQ. 'END-OF-FILE') GO TO 1900
      IF (INPFIL .EQ. 'EXIT') CALL OOPS
      CALL INFILE (1, INPFIL, IER)
      IF (IER .NE. 0) THEN
         CALL STUPID ('D.  Unable to open input file '//INPFIL)
         CALL OOPS
      END IF
      CALL CHECK (1, NL)
C
      NSTAR = 0
 1930 NSTAR = NSTAR+1
 1931 READ (1,*,ERR=1931,END=1932) ID(NSTAR), XC(NSTAR), YC(NSTAR), 
     .     XSUM(NSTAR)
      IF (NSTAR .GT. MAXSTR) THEN
         CALL STUPID ('Too many stars in input star list!')
         CALL CLFILE (1)
         CALL OOPS
      END IF
      IF (XSUM(NSTAR) .LE. 0.) GO TO 1931
      IF (ID(NSTAR) .GT. 0) THEN
         XCLAMP(NSTAR) = CLMPMX
         YCLAMP(NSTAR) = CLMPMX
         DXOLD(NSTAR) = 0.
         DYOLD(NSTAR) = 0.
         DONE(NSTAR) = .FALSE.
         XMIN = AMIN1(XMIN, XC(NSTAR))
         XMAX = AMAX1(XMAX, XC(NSTAR))
         YMIN = AMIN1(YMIN, YC(NSTAR))
         YMAX = AMAX1(YMAX, YC(NSTAR))
         IF (NSTAR .LT. MAXSTR) GO TO 1930
         GO TO 1933
      ELSE IF (ID(NSTAR) .EQ. 0) THEN
         GO TO 1931
      END IF
 1932 NSTAR = NSTAR-1
 1933 CALL CLFILE (1)
C
C Make scratch images to contain noise map and residual map.
C
      CALL TBLANK
      SEPMAX = 0.
      DO IMNO=1,NPIC
         IF (WATCH .GT. 0.5) THEN
            WRITE (LINE,*) '  Reading ', IMNO, '  ', PICTUR(IMNO)
            CALL OVRWRT (LINE(1:70), 2)
         END IF
C
         NOUT(IMNO) = 0
         INPFIL = SWITCH(PICTUR(IMNO), CASE('.psf'))
         IER = RDPSF(INPFIL, IPSTYP, PAR, MAXPAR, NPAR, PSF, MAXPSF, 
     .        MAXEXP, NPSF, NEXP, NFRAC, PSFMAG, BRIGHT, XPSF, YPSF)
         IF (IER .NE. 0) THEN
            CALL TBLANK
            CALL STUPID ('Error reading PSF file '//INPFIL)
            CALL OOPS
         END IF
         IF (RAT .GT. 0) 
     .        FITRAD(IMNO) = MIN(25.99,
     .        SQRT(SOFT**2+(RATIO*(PAR(1)+PAR(2)))**2))
         IF ((FITRAD(IMNO) .LT. 1.) .OR. (FITRAD(IMNO) .GT. 26.)) THEN
            CALL STUPID ('Invalid fitting radius in '//PICTUR(IMNO))
            CALL CLFILE(3)
            CALL OOPS
         END IF
C
C Delete output photometry files, if they exist.  Then create them.
C
         CALL DELFIL (11, OUTPUT(IMNO), IER)
         CALL OUTFIL (11, OUTPUT(IMNO), IER)
         NL = 1
         CALL WRHEAD (11, NL, NCOL(IMNO), NROW(IMNO), 7, LOBAD(IMNO), 
     .        HIBAD(IMNO), THRESH, AP1, PHPADU(IMNO), RONOIS(IMNO), 
     .        FITRAD(IMNO))
         CALL CLFILE (11)
         RONOIS(IMNO) = RONOIS(IMNO)**2
C
         CLAMP(1,IMNO) = (2./FITRAD(IMNO))**2
         CLAMP(2,IMNO) = CLAMP(1,IMNO) * 1.5
         CLAMP(3,IMNO) = CLAMP(2,IMNO)
         CLAMP(4,IMNO) = CLAMP(2,IMNO) * 1.5
         CLAMP(5,IMNO) = CLAMP(4,IMNO)
         CLAMP(6,IMNO) = CLAMP(4,IMNO)
         CLAMP(7,IMNO) = CLAMP(4,IMNO) * 1.5
         CLAMP(8,IMNO) = CLAMP(7,IMNO)
         CLAMP(9,IMNO) = CLAMP(7,IMNO)
         CLAMP(10,IMNO) = CLAMP(7,IMNO)
C
C The critical separation for considering two stars to be blended will
C be 0.375*FWHM, where (a) FWHM = 2*<HWHM>, and (b) <HWHM>**2 =
C 0.5*(HWHMx**2 + HWHMy**2). This must be corrected for the image
C scale (currently stored in SEPCRT).  Undersampled images (FWHM < 2.5)
C will be treated as though they were critically sampled.
C
         SEPCRT(IMNO) = SEPCRT(IMNO) * 
     .        AMAX1(0.875, 0.28*(PAR(1)**2+PAR(2)**2))
         IF (SEPCRT(IMNO) .GT. SEPMAX) SEPMAX = SEPCRT(IMNO)
C
         CALL OPNPIC (IMNO, 'R', SHOW, IER)
         LY = 1
         MY = NROW(IMNO)
         CALL RDSECT (IMNO, LY, MY, DATA, MAXCOL, IER)
         DO JY=1,NROW(IMNO)
            DO IX=1,NCOL(IMNO)
               IF ((DATA(IX,JY) .GT. HIBAD(IMNO)) .OR. 
     .              (DATA(IX,JY) .LT. LOBAD(IMNO))) THEN
                  SIGM(IX,JY) = -1.1E15
               ELSE
                  SIGM(IX,JY) = -RONOIS(IMNO)
               END IF
            END DO
         END DO
C
         J = NPIC+IMNO
         PICTUR(J) = SWITCH(PICTUR(IMNO), CASE('j'))
         IMGTYP(J) = IMGTYP(IMNO)
         DATTYP(J) = 'REAL'
         NCOL(J) = NCOL(IMNO)
         NROW(J) = NROW(IMNO)
         CALL CREPIC (IMNO, J, IER)
         IF (IER .NE. 0) THEN
            CALL TBLANK
            CALL STUPID ('Unable to open scratch image '//PICTUR(J))
            CALL OOPS
         END IF
         IF (WATCH .GT. 0.5) THEN
            WRITE (LINE,*) '  Writing ', IMNO, '  ', PICTUR(J)
            CALL OVRWRT (LINE(1:70), 2)
         END IF
         CALL WRSECT (J, LY, MY, DATA, MAXCOL, IER)
         CALL CLPIC (J, IER)
C
         K = 2*NPIC+IMNO
         PICTUR(K) = SWITCH(PICTUR(IMNO), CASE('k'))
         IMGTYP(K) = IMGTYP(IMNO)
         DATTYP(K) = 'REAL'
         NCOL(K) = NCOL(IMNO)
         NROW(K) = NROW(IMNO)
         CALL CREPIC (IMNO, K, IER)
         IF (IER .NE. 0) THEN
            CALL TBLANK
            CALL STUPID ('Unable to open scratch image '//PICTUR(K))
            CALL OOPS
         END IF
         IF (WATCH .GT. 0.5) THEN
            WRITE (LINE,*) '  Writing ', IMNO, '  ', PICTUR(K)
            CALL OVRWRT (LINE(1:70), 2)
         END IF
         CALL WRSECT (K, LY, MY, SIGM, MAXCOL, IER)
         CALL CLPIC (K, IER)
         CALL CLPIC (IMNO, IER)
      END DO
C
C Now determine which stars fall within the areas of which images,
C and set up the appropriate pointers.
C
      NLIST = 0
      DO IMNO=1,NPIC
         INPFIL = SWITCH(PICTUR(IMNO), CASE('.psf'))
         IER=RDPSF(INPFIL, IPSTYP, PAR, MAXPAR, NPAR, PSF, MAXPSF,
     .        MAXEXP, NPSF, NEXP, NFRAC, PSFMAG, BRIGHT, XPSF, YPSF)
         IF (IER .NE. 0) THEN
            CALL TBLANK
            CALL STUPID ('Unable to open '//INPFIL(1:LENGTH(INPFIL)))
            CALL OOPS
         END IF
         XMIN = -FITRAD(IMNO)
         XMAX = RCOL(IMNO) + FITRAD(IMNO) + 1.
         YMAX = RROW(IMNO) + FITRAD(IMNO) + 1.
         DO ISTAR=1,NSTAR
            CALL BACK (XC(ISTAR), YC(ISTAR),
     .           XCEN(IMNO), XWID(IMNO), YCEN(IMNO), YWID(IMNO),
     .           FFEOC(1,IMNO), MODE(IMNO), DX, DY)
            IF ((DX .LT. XMIN) .OR. (DX .GT. XMAX) .OR.
     .           (DY .LT. XMIN) .OR. (DY .GT. YMAX)) THEN
               IWHICH(ISTAR,IMNO) = 0
            ELSE
               NLIST = NLIST+1
               IF (NLIST .LE. MAXLST) THEN
                  IWHICH(ISTAR,IMNO) = NLIST
                  D = XSUM(ISTAR) - DMAG(IMNO)
                  AMAG(NLIST) = AMAX1(1.E-5, 10.**(0.4*(PSFMAG-D)))
                  MAGERR(NLIST) = 1.
                  CHI(NLIST) = 1.
                  MCLAMP(NLIST) = 18.
                  MOLD(NLIST) = 0.
               END IF
            END IF
         END DO
         DMAG(IMNO) = 10**(-0.4*(PSFMAG+DMAG(IMNO)))
      END DO
      IF (NLIST .GT. MAXLST) THEN
         CALL STUPID ('Too many individual measurements!')
         OPTFIL = COMMAS(NLIST, I)
         WRITE (6,*) OPTFIL(1:I)
         CALL OOPS
      END IF
      NCNVRG = 0
      NDISAP = 0
      NITER = 0
      NXTSKY = 1
C
C===================================================================
C
C ITERATION LOOP
C
  950 SEP1 = SQRT(SEPMAX)
      CMAX = SQRT(CRDMAX)
 1000 LITER = MIN0(NITER/ITCONV+1, 4)
      LITER = 1
      NITER = NITER+1
      IF (NITER .GE. ITCONV) SHPCRT = REAL(MAXIT)/REAL(NITER)
      IF (NITER .EQ. 1) THEN
         NTFM = MIN(1, NADJ)
      ELSE IF (NITER .EQ. 2) THEN
         NTFM = MIN(3, NADJ)
      ELSE IF (NITER .EQ. 3) THEN
         NTFM = MIN(6, NADJ)
      ELSE
         NTFM = NADJ
      END IF
      IF (NITER .LE. 2) THEN
         SNMIN = 0.
C     ELSE IF (NITER .LT. 2*ITCONV) THEN
C        SNMIN = 1.
C     ELSE IF (NITER .LT. 3*ITCONV) THEN
C        SNMIN = 4.
      ELSE 
C        SNMIN = 9.
         SNMIN = AMIN1(9., (REAL(NITER)/ITCONV)**2)
      END IF
C
      IF (NITER .EQ. NXTSKY) THEN
         DOSKY = .TRUE.
         IF (NITER .LT. ITCONV) THEN
            NXTSKY = NITER+1
         ELSE IF (NITER .LE. 2*ITCONV) THEN
            NXTSKY = NITER+3
         ELSE IF (NITER .LE. 4*ITCONV) THEN
            NXTSKY = NITER+5
         ELSE IF (NITER .LE. 8*ITCONV) THEN
            NXTSKY = NITER+7
         ELSE
            NXTSKY = NITER+9
         END IF
      ELSE
         DOSKY = .FALSE.
      END IF
      IF ((IEXP .GT. 0) .AND. (NITER .GE. ITCLIP)) CLIP = .TRUE.
C
C----------------------------------------------------------------
C
C OK.  The first thing we do is eliminate any stars that are
C too close together.  For the nonce, this will be defined
C to be centroids within 0.375 times the FWHM of the best-sampled
C frame in which they both appear.
C
      IF (NSTAR .LE. 1) GO TO 1490
C
      IF (NCNVRG .LE. 0) THEN
         RMAX = MAX(SEP1, CMAX)
         DO ISTAR = 1,NSTAR
            CROWD(ISTAR) = 1.
         END DO
      ELSE
         RMAX = SEP1
      END IF
C
      ISTAR = 0
 1401 ISTAR = ISTAR+1
      IF (ISTAR .GE. NSTAR) GO TO 1490
C
      IF ((WATCH .GT. 0.5) .AND. 
     .     (MOD(ISTAR,1000) .EQ. 0)) THEN
         LINE = ' '
         WRITE (LINE,84) ISTAR
   84    FORMAT (I11, ' stars checked for crowding.')
         CALL OVRWRT (LINE(1:70), 2)
      ELSE IF ((WATCH .GT. -1.5) .AND. 
     .     (MOD(ISTAR,10000) .EQ. 0)) THEN
         LINE = ' '
         WRITE (LINE,84) ISTAR
         CALL OVRWRT (LINE(1:70), 2)
      END IF
C
 1402 JSTAR = ISTAR
 1405 JSTAR = JSTAR+1
C
 1407 DPOS = YC(ISTAR)-YC(JSTAR)
      IF (ABS(DPOS) .LT. RMAX) THEN
      DPOS = DPOS**2 + (XC(ISTAR)-XC(JSTAR))**2
      IF (DPOS .LT. SEPMAX) THEN
         D = SEPMAX
         J = 0
         DO IMNO=1,NPIC
            KSTAR = IWHICH(ISTAR,IMNO)
            IF (KSTAR .GT. 0) THEN
               LSTAR = IWHICH(JSTAR,IMNO)
               IF (LSTAR .GT. 0) THEN
                  IF ((MAGERR(KSTAR) .GT. 0.) .AND.
     .                (MAGERR(LSTAR) .GT. 0.)) THEN
                     IF (SEPCRT(IMNO) .LE. D) THEN
                        D = SEPCRT(IMNO)
                        J = IMNO
                     END IF
                  END IF
               END IF
            END IF
         END DO
C
         IF (DPOS .LT. D) THEN
            XC(ISTAR) = (XC(ISTAR)+XC(JSTAR))/2.
            YC(ISTAR) = (YC(ISTAR)+YC(JSTAR))/2.
            DO IMNO=1,NPIC
               KSTAR = IWHICH(ISTAR,IMNO)
               IF (KSTAR .GT. 0) THEN
                  LSTAR = IWHICH(JSTAR,IMNO)
                  IF (LSTAR .GT. 0) THEN
                     CHI(KSTAR) = AMAG(KSTAR)*CHI(KSTAR)
     .                    + AMAG(LSTAR)*CHI(LSTAR)
                     AMAG(KSTAR) = AMAG(KSTAR) + AMAG(LSTAR)
                     MAGERR(KSTAR) = AMAX1(MAGERR(KSTAR),
     .                  MAGERR(LSTAR))
                     CHI(KSTAR) = CHI(KSTAR)/AMAG(KSTAR)
                     SKY(KSTAR) = (SKY(KSTAR)+SKY(LSTAR))/2.
                  END IF
               END IF
            END DO
            DXOLD(ISTAR) = 0.
            DYOLD(ISTAR) = 0.
            XCLAMP(ISTAR) = AMAX1(XCLAMP(ISTAR),XCLAMP(JSTAR))
            YCLAMP(ISTAR) = AMAX1(YCLAMP(ISTAR),YCLAMP(JSTAR))
            NDISAP = NDISAP+1
            IF (ABS(WATCH) .GT. 2.5) PRINT 664, ID(JSTAR), XC(JSTAR), 
     .           YC(JSTAR), ID(ISTAR), XC(ISTAR), YC(ISTAR), DPOS, D
  664       FORMAT (I11, 2F9.3, '  blended with ', I11, 4F9.3)
C
            IF (JSTAR .LT. NSTAR) THEN
               ID(JSTAR) = ID(NSTAR)
               XC(JSTAR) = XC(NSTAR)
               YC(JSTAR) = YC(NSTAR)
               DO IMNO=1,NPIC
                  IWHICH(JSTAR,IMNO) = IWHICH(NSTAR,IMNO)
               END DO
               DXOLD(JSTAR) = DXOLD(NSTAR)
               DYOLD(JSTAR) = DYOLD(NSTAR)
               XCLAMP(JSTAR) = XCLAMP(NSTAR)
               YCLAMP(JSTAR) = YCLAMP(NSTAR)
            END IF
            NSTAR = NSTAR-1
C
C Must now check the new ISTAR against everything which follows.
C
            IF (ISTAR .LT. NSTAR) GO TO 1402
         END IF
      END IF
      IF (NCNVRG .LE. 0) THEN
         IF  (DPOS .LE. CRDMAX) THEN
            RADSQ = 1. - DPOS/CRDMAX
            CROWD(ISTAR) = CROWD(ISTAR) + RADSQ
            CROWD(JSTAR) = CROWD(JSTAR) + RADSQ
         END IF
      END IF
      END IF
C
      IF (JSTAR .LT. NSTAR) GO TO 1405
      GO TO 1401
C
C-------------------------------------------------------------------
C
 1490 CONTINUE
      LINE = ' '
      WRITE (LINE(1:8),90) NITER-1
   90 FORMAT ('It ', I3, ':')
C
      IF (NSTAR .GT. 1) THEN
         WRITE (LINE(9:30),91) NSTAR
   91    FORMAT (I8, ' stars remain,')
      ELSE IF (NSTAR .EQ. 1) THEN
         WRITE (LINE(9:30),92) NSTAR
   92    FORMAT (I8, ' star remains,')
      ELSE
         WRITE (LINE(9:30),89)
   89    FORMAT ('      No stars remain,')
      END IF
C
      IF (NCNVRG .GT. 1) THEN
         WRITE (LINE(31:54),93) NCNVRG
   93    FORMAT (I8, ' have converged,')
      ELSE IF (NCNVRG .EQ. 1) THEN
         WRITE (LINE(31:54),94) NCNVRG
   94    FORMAT (I8, '  has converged,')
      ELSE
         WRITE (LINE(31:54),95)
   95    FORMAT ('    none have converged,')
      END IF
C
      IF (NDISAP .GT. 1) THEN
         WRITE (LINE(55:80),96) NDISAP
   96    FORMAT (I8, ' have disappeared.')
      ELSE IF (NDISAP .EQ. 1) THEN
         WRITE (LINE(55:80),97) NDISAP
   97    FORMAT (I8, '  has disappeared.')
      ELSE
         WRITE (LINE(55:80),98)
   98    FORMAT ('    none have disappeared.')
      END IF
C
      CALL OVRWRT (LINE(1:80), 3)
      IF (NSTAR .LE. 0) GO TO 9000
      IF ((NCNVRG .LE. 0) .AND. (WATCH .GE. 1.5)) CALL TBLANK
C
C Set up reporting intervals for star counter.
C
      IF (WATCH .GT. 0.5) THEN
         DUM = AMAX1(0.1, ALOG10(FLOAT(NSTAR)/10.))
         INTRVL = INT(DUM)
         DUM = DUM - FLOAT(INTRVL)
         IF (DUM .GT. 0.8) THEN
            INTRVL = 10**(INTRVL+1)
         ELSE IF (DUM .GT. 0.5) THEN
            INTRVL = 5*10**INTRVL
         ELSE IF (DUM .GT. 0.2) THEN
            INTRVL = 2*10**INTRVL
         ELSE
            INTRVL = 10**INTRVL
         END IF
      END IF
      IF (INTRVL .LT. 1000) INTRVL = 1000
C
      DO ISTAR=1,NSTAR
         XSUM(ISTAR) = 0.
         XWT(ISTAR) = 0.
         YSUM(ISTAR) = 0.
         YWT(ISTAR) = 0.
         IF (NITER .GE. ITCONV) DONE(ISTAR) = .TRUE.
      END DO
C
C====================================================================
C
C LOOP OVER IMAGES
C
      DO 5000 IMNO=1,NPIC
C
      IF ((NSTAR .GT. 200) .AND. (WATCH .GT. -1.5)) THEN
         LINE = ' '
         WRITE (LINE,*) ' Starting image', IMNO, ':  ', PICTUR(IMNO)
         CALL OVRWRT (LINE(1:70), 2)
      END IF
C
C Read the PSF for this image.
C
      XMIN = 1.E6
      XMAX = -1.E6
      YMIN = 1.E6
      YMAX = -1.E6
C
C-----------------------------------------------------------------------
C
C Transform the star list to the system of this image, and determine
C which pixels will be needed.
C
      SRADSQ = FITRAD(IMNO)+1.
      IF (DOSKY) THEN
         RADIUS = AMAX1(SRADSQ, SKYOUT)
      ELSE
         RADIUS = SRADSQ
      END IF
      SRADSQ = SRADSQ**2
      RADSQ = RADIUS**2
C
      DO ISTAR=1,NSTAR
         KSTAR = IWHICH(ISTAR,IMNO)
         IF (KSTAR .GT. 0) THEN
            CALL BACK (XC(ISTAR), YC(ISTAR),
     .           XCEN(IMNO), XWID(IMNO), YCEN(IMNO), YWID(IMNO),
     .           FFEOC(1,IMNO), MODE(IMNO), X(ISTAR), Y(ISTAR))
            IF ((X(ISTAR) .LT. 0.) .OR. (X(ISTAR) .GT. RCOL(IMNO)+1.)
     .           .OR. (Y(ISTAR) .LT. 0.) .OR.
     .           (Y(ISTAR) .GT. RROW(IMNO)+1.)) THEN
               MAGERR(KSTAR) = -0.5
               DXWT(ISTAR) = -0.5
            ELSE
               XMIN = AMIN1(XMIN, X(ISTAR))
               XMAX = AMAX1(XMAX, X(ISTAR))
               YMIN = AMIN1(YMIN, Y(ISTAR))
               YMAX = AMAX1(YMAX, Y(ISTAR))
               MAGERR(KSTAR) = 1.
            END IF
         ELSE
            DXWT(ISTAR) = -0.25
            DYWT(ISTAR) = -0.25
         END IF
      END DO
      IXMIN = MAX0(1, INT(XMIN-RADIUS)+1)
      IXMAX = MIN0(NCOL(IMNO), INT(XMAX+RADIUS))
      JYMIN = MAX0(1, INT(YMIN-RADIUS)+1)
      JYMAX = MIN0(NROW(IMNO), INT(YMAX+RADIUS))
      IF ((IXMAX .LT. IXMIN) .OR. (JYMAX .LT. JYMIN)) GO TO 5000
C
      INPFIL = SWITCH(PICTUR(IMNO), CASE('.psf'))
      IER=RDPSF(INPFIL, IPSTYP, PAR, MAXPAR, NPAR, PSF, MAXPSF, 
     .     MAXEXP, NPSF, NEXP, NFRAC, PSFMAG, BRIGHT, XPSF, YPSF)
      IF (IER .NE. 0) THEN
         CALL TBLANK
         CALL STUPID ('Unable to open '//INPFIL(1:LENGTH(INPFIL)))
         CALL OOPS
      END IF
      PEAK = USEPSF(IPSTYP, 0., 0., BRIGHT, PAR, PSF,
     .     NPSF, NPAR, NEXP, NFRAC, 0., 0., DVDXC, DVDYC)
      PSFRAD = (REAL(NPSF-1)/2.-1.)/2.
      PSFRSQ = PSFRAD**2
      PKERR = PROERR/(PAR(1)*PAR(2)) ! **2
C
C-----------------------------------------------------------------------
C
C Read in the working copy of the image itself and the noise
C map.
C
      J = NPIC+IMNO
      K = 2*NPIC+IMNO
      CALL OPNPIC (IMNO, 'R', SHOW, IER)
      CALL OPNPIC (J, 'W', SHOW, IER)
      CALL OPNPIC (K, 'W', SHOW, IER)
      IF (REAL(JYMAX-JYMIN+1) .LE. 
     .     40.*REAL(NSTAR)*RADIUS) THEN
         CALL RDSECT (IMNO, JYMIN, JYMAX, ORIG, MAXCOL, IER)
         IF (IER .NE. 0) THEN
            CALL STUPID ('Unable to open '//PICTUR(IMNO))
            CALL OOPS
         END IF
         CALL RDSECT (J, JYMIN, JYMAX, DATA, MAXCOL, IER)
         IF (IER .NE. 0) THEN
            CALL STUPID ('Unable to open '//PICTUR(J))
            CALL OOPS
         END IF
         CALL RDSECT (K, JYMIN, JYMAX, SIGM, MAXCOL, IER)
         IF (IER .NE. 0) THEN
            CALL STUPID ('Unable to open '//PICTUR(K))
            CALL OOPS
         END IF
      ELSE
         DO 2083 ISTAR=1,NSTAR
            KSTAR = IWHICH(ISTAR,IMNO)
            IF (KSTAR .LE. 0) GO TO 2083
            IF (MAGERR(KSTAR) .LT. 0.) GO TO 2083
            LX = MAX0(1, INT(X(ISTAR)-RADIUS)+1)
            MX = MIN0(NCOL(IMNO), INT(X(ISTAR)+RADIUS))
            LY = MAX0(1, INT(Y(ISTAR)-RADIUS)+1)
            MY = MIN0(NROW(IMNO), INT(Y(ISTAR)+RADIUS))
            IF ((LX .GT. MX) .OR. (LY .GT. MY)) GO TO 2083
            CALL RDSECT (IMNO, LY, MY, ORIG, MAXCOL, IER)
            IF (IER .NE. 0) THEN
               CALL STUPID ('Unable to open '//PICTUR(IMNO))
               CALL OOPS
            END IF
            CALL RDSECT (J, LY, MY, DATA, MAXCOL, IER)
            IF (IER .NE. 0) THEN
               CALL STUPID ('Unable to open '//PICTUR(J))
               CALL OOPS
            END IF
            CALL RDSECT (K, LY, MY, SIGM, MAXCOL, IER)
            IF (IER .NE. 0) THEN
               CALL STUPID ('Unable to open '//PICTUR(K))
               CALL OOPS
            END IF
 2083    CONTINUE
      END IF
      CALL CLPIC (IMNO, IER)
      CALL CLPIC (J, IER)
      CALL CLPIC (K, IER)
C
C-----------------------------------------------------------------------
C
C Figure out which pixels will actually be needed this iteration.
C
      DO 2089 ISTAR=1,NSTAR
         KSTAR = IWHICH(ISTAR,IMNO)
         IF (KSTAR .LE. 0) GO TO 2089
         IF (MAGERR(KSTAR) .LT. 0.) GO TO 2089
         LX = MAX0(1, INT(X(ISTAR)-RADIUS) + 1)
         MX = MIN0(NCOL(IMNO), INT(X(ISTAR)+RADIUS))
         LY = MAX0(1, INT(Y(ISTAR)-RADIUS) + 1)
         MY = MIN0(NROW(IMNO), INT(Y(ISTAR)+RADIUS))
         IF ((LX .GT. MX) .OR. (LY .GT. MY)) GO TO 2089
         DO 2088 JY=LY,MY
            DYSQ = (REAL(JY)-Y(ISTAR))**2
            DO 2086 IX=LX,MX
               DX = REAL(IX)-X(ISTAR)
               IF (DX**2+DYSQ .LE. RADSQ) THEN
                  IF(SIGM(IX,JY) .GT. -1.E15) SIGM(IX,JY) =
     .                 ABS(SIGM(IX,JY))
               ELSE
                  IF (DX .GT. 0.) GO TO 2088
               END IF
 2086       CONTINUE
 2088    CONTINUE
 2089 CONTINUE
C
C-----------------------------------------------------------------------
C
C Subtract the stellar profiles from the pixels that will be needed
C and add the inferred noise into the noise map.
C
      DO 2099 ISTAR=1,NSTAR
         KSTAR = IWHICH(ISTAR,IMNO)
         IF (KSTAR .LE. 0) GO TO 2098
         IF (MAGERR(KSTAR) .LT. 0.) GO TO 2098
         LX = MAX0(1, INT(X(ISTAR)-PSFRAD) + 1)
         MX = MIN0(NCOL(IMNO), INT(X(ISTAR)+PSFRAD))
         LY = MAX0(1, INT(Y(ISTAR)-PSFRAD) + 1)
         MY = MIN0(NROW(IMNO), INT(Y(ISTAR)+PSFRAD))
         IF ((MX .LT. LX) .OR. (MY .LT. LY)) GO TO 2098
         DELTAX = (X(ISTAR)-1.)/XPSF-1.
         DELTAY = (Y(ISTAR)-1.)/YPSF-1.
         DO 2094 JY=LY,MY
            DY = REAL(JY)-Y(ISTAR)
            DYSQ = DY**2
            DO 2092 IX=LX,MX
               IF (SIGM(IX,JY) .LT. 0.) GO TO 2092
               DX = REAL(IX)-X(ISTAR)
               IF (DX**2+DYSQ .GT. PSFRSQ) THEN
                  IF (DX .GT. 0) GO TO 2094
                  GO TO 2092
               END IF
               DPOS = AMAG(KSTAR)*
     .              USEPSF(IPSTYP, DX, DY, BRIGHT, PAR, PSF,
     .              NPSF, NPAR, NEXP, NFRAC, DELTAX, DELTAY,
     .              DVDXC, DVDYC)
               DATA(IX,JY) = DATA(IX,JY) - DPOS
               IF (DPOS .LE. 0.) GO TO 2092
               SIGM(IX,JY) = SIGM(IX,JY)
     .              + DPOS/PHPADU(IMNO)      ! Photon noise in star
     .              + (PKERR*DPOS)**2        ! Profile error
 2092       CONTINUE
 2094    CONTINUE
 2098    IF ((WATCH .GT. 0.5) .AND. (MOD(ISTAR,INTRVL) .EQ. 0)) THEN
            LINE = ' '
            WRITE (LINE,81) ISTAR, IMNO, PICTUR(IMNO)
   81       FORMAT (I11, ' stars subtracted from image ',
     .           I3, ':  ', A30)
            CALL OVRWRT (LINE(1:70), 2)
         END IF
 2099 CONTINUE
      IF ((WATCH .GT. 0.5) .AND. (NSTAR .GT. INTRVL)) THEN
         LINE = ' '
         WRITE (LINE,81) NSTAR, IMNO, PICTUR(IMNO)
         CALL OVRWRT (LINE(1:70), 2)
      END IF
C
C-----------------------------------------------------------------------
C
C If sky values are to be redetermined this iteration, do it now.
C
      IF (DOSKY) THEN
         DO 2199 ISTAR=1,NSTAR
            KSTAR = IWHICH(ISTAR,IMNO)
            IF (KSTAR .LE. 0) GO TO 2198
            IF (MAGERR(KSTAR) .LT. 0.) GO TO 2198
            LX = MAX0(IXMIN, INT(X(ISTAR)-FITRAD(IMNO))+1)
            MX = MIN0(IXMAX, INT(X(ISTAR)+FITRAD(IMNO)))
            LY = MAX0(JYMIN, INT(Y(ISTAR)-FITRAD(IMNO))+1)
            MY = MIN0(JYMAX, INT(Y(ISTAR)+FITRAD(IMNO)))
            IF ((LX .GT. MX) .OR. (LY .GT. MY)) THEN
C
C This star is not in this frame.
C
               SKY(KSTAR) = -1.1E6
               GO TO 2198
            END IF
            LX = MAX0(IXMIN, INT(X(ISTAR)-SKYOUT)+1)
            MX = MIN0(IXMAX, INT(X(ISTAR)+SKYOUT))
            LY = MAX0(JYMIN, INT(Y(ISTAR)-SKYOUT)+1)
            MY = MIN0(JYMAX, INT(Y(ISTAR)+SKYOUT))
C
            N = 0
            DO 2195 JY=LY,MY
               DYSQ = (REAL(JY) - Y(ISTAR))**2
               DO 2194 IX=LX,MX
                  IF (DATA(IX,JY) .GT. LOBAD(IMNO)) THEN
                  DX = REAL(IX) - X(ISTAR)
                  RSQ = DX**2 + DYSQ
                  IF (RSQ .LT. SKYOSQ) THEN
                     IF (RSQ .GE. SKYISQ) THEN
                        IF (SIGM(IX,JY) .GT. -1.E15) THEN
                           N = N+1
                           NMRTR(N) = DATA(IX,JY)
                           IF (N .GE. MAXSTR) GO TO 2196
                        END IF
                     END IF
                  ELSE
                     IF (DX .GT. 0.) GO TO 2195
                  END IF
                  end if
 2194          CONTINUE
 2195       CONTINUE
C
 2196       CONTINUE
            IF (N .GT. MINSKY) THEN
               JY = 0
               D = PCTILE (NMRTR, N, (N+1)/2)
 2197          DUM = 4.*(RONOIS(IMNO) + AMAX1(0.5,D)/PHPADU(IMNO))
C DUM = 4 * SIGMA**2
C
               DX = 0.
               DY = 0.
               DO I=1,N
                  DPOS = NMRTR(I) - D
                  WT = 1./(1.+DPOS**2/DUM)
                  DX = DX + WT*DPOS
                  DY = DY + WT
               END DO
               DX = DX/DY
               D = D+DX
               JY = JY+1
C
C Convergence criterion corresponds to a 0.1 sigma maximum error in an
C aperture containing 100 pixels, or a 1 sigma error in an aperture
C containing 10,000 pixels (radius = 56 pixels).
C
               IF ((DX**2/DUM .GT. 2.5E-5) .AND. 
     .              (JY .LE. 5)) GO TO 2197
               SKY(KSTAR) = D
            ELSE
               SKY(KSTAR) = -1.1E6
            END IF
C
 2198       IF ((WATCH .GT. 0.5) .AND. (MOD(ISTAR,INTRVL) .EQ. 0)) THEN
               LINE = ' '
               WRITE (LINE,82) ISTAR, IMNO, PICTUR(IMNO)
   82          FORMAT (I11, ' sky values redetermined for ',
     .              'image ', I3, ':  ', A30)
               CALL OVRWRT (LINE(1:70), 2)
            END IF
 2199    CONTINUE
         IF ((WATCH .GT. 0.5) .AND. (NSTAR .GT. INTRVL)) THEN
            LINE = ' '
            CALL OVRWRT (LINE(1:70), 2)
         END IF
      END IF
C
C-----------------------------------------------------------------------
C
C Now compute parameter corrections, star by star.
C
      NTERM = 3
C
      DO 2300 ISTAR=1,NSTAR
         KSTAR = IWHICH(ISTAR,IMNO)
         IF (KSTAR .LE. 0) GO TO 2299
         IF (SKY(KSTAR) .LT. -1.E6) THEN
            MAGERR(KSTAR) = -1.
            DXWT(ISTAR) = -1.
         ELSE
            DXWT(ISTAR) = 0.
         END IF
         IF (MAGERR(KSTAR) .LT. 0.) GO TO 2299
         FR = FITRAD(IMNO)
 2205    FITRSQ = FR**2
         LX = MAX0(1, INT(X(ISTAR)-FR)+1)
         MX = MIN0(NCOL(IMNO), INT(X(ISTAR)+FR))
         LY = MAX0(1, INT(Y(ISTAR)-FR)+1)
         MY = MIN0(NROW(IMNO), INT(Y(ISTAR)+FR))
C
C LX, MX, LY, and MY are now the limits of a rectangular 
C array containing all pixels within one fitting radius of the star.
C (Two, if the star is saturated.)
C
         DELTAX = (X(ISTAR)-1.)/XPSF-1.
         DELTAY = (Y(ISTAR)-1.)/YPSF-1.
C
C Zero the normal matrix and the vector of residuals.
C
         KITER = 0
 2210    KITER = KITER+1
         DO 2270 J=1,NTERM
         VX(J) = 0.0D0
         DO 2270 I=J,NTERM
 2270    CX(I,J) = 0.0D0
         SUMRES = 0.
         SUMWT = 0.
         NUMER = 0.
         DENOM = 0.
         NPIX = 0
C
C Now deal with the pixels one by one.
C
         DO 2390 JY=LY,MY
            DY = REAL(JY)-Y(ISTAR)
            DYSQ = DY**2
            DO 2380 IX=LX,MX
               IF (SIGM(IX,JY) .LE. 0.) GO TO 2380
               DX = REAL(IX)-X(ISTAR)
               RSQ = DX**2 + DYSQ
               IF (RSQ .GT. SRADSQ) THEN
                  IF (DX .GT. 0.) GO TO 2390
                  GO TO 2380
               END IF
C
               D = DATA(IX,JY)-SKY(KSTAR)         ! Residual of this pixel
               DPOS = AMAX1(0.5, ORIG(IX,JY)-D)
C
C If this pixel IS bad or is PREDICTED to be saturated, skip it.
C
               IF ((ORIG(IX,JY) .GT. HIBAD(IMNO)) .OR. 
     .             (ORIG(IX,JY) .LT. LOBAD(IMNO)) .OR.
     .                    (DPOS .GT. HIBAD(IMNO))) GO TO 2380
C
C DPOS = raw data minus residual
C      = model-predicted brightness in this pixel, consisting of sky
C        plus all stellar profiles, which presumably is non-negative.
C
C The four error sources in our noise model are:
C     (1) Readout noise
C     (2) Poisson noise in stars
C     (3) Poisson noise in sky
C     (4) Flat-field errors
C     (5) Errors in the PSF
C
C All but (3) and (4) were taken care of above.
C
               SIGSQ = SIGM(IX,JY) + (PERERR*DPOS)**2         ! Flat-field error
     .              + AMAX1(0.5,SKY(KSTAR)/PHPADU(IMNO))  ! Photon noise in sky
               IF (SIGSQ .LE. 0.) THEN
                  WRITE (6,*) 'SIGSQ .LE. 0.'
                  WRITE (6,*)
               END IF
C
C Now that we have the residual and error, accumulate for the SHARP index.
C
               RHOSQ = (DX/PAR(1))**2+(DY/PAR(2))**2
               RHOSQ = 0.6931472*RHOSQ
               DFDSIG = EXP(-RHOSQ)*(RHOSQ-1.)
               NUMER = NUMER+DFDSIG*D/SIGSQ
               DENOM = DENOM+DFDSIG**2/SIGSQ
C
               RSQ = 1. - RSQ/FITRSQ
               IF (RSQ .LT. 1.E-10) GO TO 2380
C
               DUM = SQRT(SIGSQ)
               RELERR = ABS(D)/DUM
               IF (CLIP .AND. (RELERR .GT. 100.)) GO TO 2380
               WT = 5./(5.+(1.-RSQ)/RSQ)
C
C This is the radial part of the weight.  Now reduce the weight of
C any pixel whose predicted brightness value is near HIBAD.  Use
C an approximate cumulative normal probability distribution to
C represent the probability that the "true" brightness value 
C really is above HIBAD. 
C
               DUM = (DPOS-HIBAD(IMNO))/DUM
               WT = WT*0.5*(1.-SIGN(1.,DUM)*(
     .                 1.-1./(1.+0.906538*DUM**2)**2))
C
C The condition equation for pixel (IX,JY) is of the form
C
C data(IX,JY)-summation{scale*psf(IX-Xcenter,JY-Ycenter)}-sky=residual
C
C Then we will jigger the scale's, Xcenter's, and Ycenter's such that
C
C                Summation{weight * residual**2}
C
C is minimized.  'weight' will be a function (1) of the distance of this
C pixel from the center of the nearest star, (2) of the model-predicted
C brightness of the pixel (taking into consideration the readout noise, 
C the photons/ADU, and the interpolation error of the PSF), and (3) of 
C the size of the residual itself.  (1) is necessary to prevent the
C non-linear least-squares solution from oscillating:  oft-times it will
C come to pass that if you include a pixel in the solution, then the
C predicted shift of the centroid will cause that pixel to be excluded 
C in the next iteration, and the new predicted shift of the centroid
C will cause that pixel to be included again.  This could go on ad
C infinitum.  The cure is to have the weight of a pixel go 
C continuously to zero as its distance from the stellar centroid
C approaches the fitting radius.  In a case like that just described,
C the solution can then find a real minimum of the sum of the
C weighted squared residuals with that pixel at some low-weight position
C just inside the fitting radius.  (2) is just sensible weighting.
C (3) is just a crude attempt at making the solution more robust against
C bad pixels.
C
               ZX(1) = -USEPSF(IPSTYP, DX, DY, BRIGHT, PAR, PSF, NPSF, 
     .              NPAR, NEXP, NFRAC, DELTAX, DELTAY, DVDXC, DVDYC)
               IF (NTERM .GT. 1) THEN
                  ZX(2) = -AMAG(KSTAR)*DVDXC
                  ZX(3) = -AMAG(KSTAR)*DVDYC
               END IF
C
C At this point, the vector Z contains the first derivative of
C the condition equation for pixel (IX,JY) with respect to each of
C the fitting parameters for star ISTAR.  Now these derivatives 
C will be added into the normal matrix and the vector of residuals.
C
C Add this residual into the weighted sum of the absolute relative 
C residuals.  At this point WT is the product of the radial
C weighting scheme and the downweighting factor for pixels
C predicted to be near saturation.
C
               DWT = WT*RELERR
               SUMRES = SUMRES+DWT
               SUMWT = SUMWT+WT
C
C SUMRES is the weighted sum of [ABS(residual)/sigma] for all the 
C pixels in the star.  Now include the 1/sigma**2 weight.
C
               IF (SIGSQ .LE. 0.) THEN
                  WRITE (6,*) 'SIGSQ .LE. 0.'
                  WRITE (6,*)
               END IF
               WT = WT/SIGSQ
               IF (CHI(KSTAR) .LE. 0.) THEN
                  WRITE (6,*) 'CHI .LE. 0.'
                  WRITE (6,*)
               END IF
               IF (CLIP) THEN
                  WT = WT/(1.+(RELERR/(CHI(KSTAR)*RANGE))**IEXP)
               END IF
               IF (WT .LE. 0.) GO TO 2380
C
C Now work this pixel into the normal matrix.
C
               DWT=D*WT
               DO J=1,NTERM
                  VX(J) = VX(J) + DBLE(DWT*ZX(J))
                  DO I=J,NTERM
                     CX(I,J) = CX(I,J) + DBLE(WT*ZX(I)*ZX(J))
                  END DO
               END DO
               NPIX = NPIX + 1
 2380       CONTINUE
 2390    CONTINUE
C
C At least half the pixels inside the fitting radius must be good:
C
C NPIX > pi * fr**2 / 2
C
         IF (NPIX .LT. INT(1.57*FR**2)) THEN
            IF (FR .LT. 0.99*PSFRAD) THEN
               FR = AMIN1(1.414*FR, PSFRAD)
               GO TO 2205
            ELSE
C
C Fitting radius equals the PSF radius.  If there are at least three
C good pixels, solve for brightness; otherwise discard the star.
C
               IF (NPIX .LE. 2) THEN
                  MAGERR(KSTAR) = -2.
                  DXWT(ISTAR) = -2.
                  GO TO 2299
               ELSE
                  KTERM = 1
               END IF
            END IF
         ELSE 
            IF ((MX .GT. LX) .AND. (MY .GT. LY)) THEN
               KTERM = NTERM
            ELSE
               KTERM = 1
            END IF
         END IF
         IF (KITER .LT. LITER) KTERM = 1
         SHP(KSTAR) = 1.4427*PAR(1)*PAR(2)*NUMER/
     .        (AMAG(KSTAR)*PEAK*DENOM)
         IF (NITER .GT. ITCONV) THEN
            IF (SHP(KSTAR) .LT. SHPMIN*SHPCRT) THEN
               iwhich(istar,imno) = -7
               dxwt(istar) = -7.
               GO TO 2299
            ELSE IF (SHP(KSTAR) .GT. SHPMAX*SHPCRT) THEN
               iwhich(istar,imno) = -7
               dxwt(istar) = -7.
               GO TO 2299
            END IF
         END IF
C
C Reflect the normal matrix across the diagonal.
C
         IF (KTERM .GT. 1) THEN
            DO 2410 L=2,KTERM
            DO 2410 K=1,L-1
 2410       CX(K,L)=CX(L,K)
         END IF
         CY(1,1) = CX(1,1)
C
         CALL DINVRS (CX, MXTERM, KTERM, IER)
         IF (IER .NE. 0) THEN
            IF (KTERM .GT. 1) THEN
               CX(1,1) = 1.0D0/CY(1,1)
               KTERM = 1
            ELSE
               MAGERR(KSTAR) = -3.
               DXWT(ISTAR) = -3.
               GO TO 2299
            END IF
         ELSE
            DO J=1,KTERM
               IF (CX(J,J) .LE. 0.D0) THEN
                  IF (KTERM .GT. 1) THEN
                     CX(1,1) = 1.0D0/CY(1,1)
                     KTERM = 1
                  ELSE
                     MAGERR(KSTAR) = -4.
                     DXWT(ISTAR) = -4.
                     GO TO 2299
                  END IF
               END IF
            END DO
         END IF
C
C Compute the estimate of the standard deviation of the residuals for 
C the star.  This estimate starts out as 
C      SQRT(PI/2)*{SUM[weight*ABS(residual/sigma)]/SUM(weight)} 
C and then gets corrected for bias by 
C      SQRT(no. of pixels/(no. of pixels - degrees of freedom)).
C
         IF (SUMWT .GT. KTERM) THEN
            IF (SUMWT*(SUMWT-KTERM) .LE. 0.) THEN
               WRITE (6,*) 'SUMWT'
               WRITE (6,*)
            END IF
            DWT = 1.2533141*SUMRES/SQRT(SUMWT*(SUMWT-KTERM))
C
C But then I drive the value toward unity, depending on exactly how
C many pixels were involved:  if CHI is based on exactly a total 
C weight of 2, then it is extremely poorly determined, and we just
C want to keep CHI = 1.  The larger SUMWT is, the better determined
C CHI is, and the less we want to force it toward unity.  So,
C just take the weighted average of CHI and unity, with weights
C SUMWT-2 and 2, respectively.
C
            IF (SUMWT .GT. 2.) THEN
               CHI(KSTAR) = ((SUMWT-2.)*DWT+2.)/SUMWT
            ELSE
               CHI(KSTAR) = 1.
            END IF
         END IF
         CALL DVMUL (CX, MXTERM, KTERM, VX, ZX)
C
         DWT = MCLAMP(KSTAR)
         IF (ZX(1)*MOLD(KSTAR) .LE. 0.) THEN
            DWT = AMAX1(3.E-5,0.5*DWT)
         ELSE
            IF (DWT .LT. 0.5) THEN
               DWT = AMIN1(0.5, 1.2*DWT)
            END IF
         END IF
         MCLAMP(KSTAR) = DWT
         MOLD(KSTAR) = ZX(1)
C
         IF (ZX(1) .GT. 0.) THEN
             DUM = -AMIN1(REAL(ZX(1)), DWT*AMAG(KSTAR)/(1.+DWT)) 
         ELSE
             DUM = AMIN1(-REAL(ZX(1)), DWT*AMAG(KSTAR))
         END IF
C
C Subtract the increment to the star's profile from the data frame,
C at least within the fitting radius, so subsequent stars won't try to 
C correct the same residuals.
C
         DO 2294 JY=LY,MY
            DY = REAL(JY)-Y(ISTAR)
            DYSQ = DY**2
            DO 2292 IX=LX,MX
               IF (SIGM(IX,JY) .LT. 0.) GO TO 2292
               DX = REAL(IX)-X(ISTAR)
               IF (DX**2+DYSQ .GT. FITRSQ) THEN
                  IF (DX .GT. 0) GO TO 2294
                  GO TO 2292
               END IF
               DPOS = USEPSF(IPSTYP, DX, DY, BRIGHT, PAR, 
     .              PSF, NPSF, NPAR, NEXP, NFRAC, DELTAX, DELTAY,
     .              DVDXC, DVDYC)
               DATA(IX,JY) = DATA(IX,JY) - DUM * DPOS
C
C Increment the noise map.
C
C Extra photon noise in star:
C
C     DUM*DPOS/PHPADU
C
C Extra profile error:
C
C     ((AMAG+DUM)*DPOS*PKERR)**2 - (AMAG*DPOS*PKERR)**2 =
C                                          (2*AMAG*DUM + DUM**2)*(DPOS*PKERR)**2
C
               SIGM(IX,JY) = AMAX1( RONOIS(IMNO), SIGM(IX,JY)
     .              + DUM*(DPOS/PHPADU(IMNO)+(2.*AMAG(KSTAR)+DUM)*
     .              (DPOS*PKERR)**2) )
 2292       CONTINUE
 2294    CONTINUE
C
C Adjust the star's magnitude.
C
         DUM = AMAG(KSTAR) + DUM
         IF (DUM .LT. 1.E-8) THEN
C
C More than 20 magnitudes fainter than the PSF ==> it's not really
C there.
C
            MAGERR(KSTAR) = -6.
            DXWT(ISTAR) = -6.
            GO TO 2299
         END IF
         AMAG(KSTAR) = DUM
         WT = CHI(KSTAR)**2
         MAGERR(KSTAR) = WT*SNGL(CX(1,1))
         IF (MAGERR(KSTAR) .LT. 0.) THEN
            WRITE (6,*) IMNO,
     .           ID(ISTAR), '  NEGATIVE MAGERR', WT, CX(1,1)
            WRITE (6,*)
         END IF
         IF (KITER .LT. LITER) GO TO 2210
C
C End of little internal loop.
C
         IF (AMAG(KSTAR)**2 .GT. MAGERR(KSTAR)) THEN
C
C A star which is less than a 1-sigma detection in this frame won't
C be bothered with and won't be allowed to impede convergence.
C
            XMAX = ABS(SNGL(ZX(1)))
            IF (XMAX .GT. 0.05*AMAG(KSTAR)) THEN
               DONE(ISTAR) = .FALSE.
            ELSE IF (XMAX**2 .GT. 0.01*MAGERR(KSTAR)) THEN
               DONE(ISTAR) = .FALSE.
            END IF
         END IF
C
         IF ((NITER .GE. MAXIT-5) .AND. (WATCH .GT. 2.5)) THEN
            WRITE (6,665) IMNO, ID(ISTAR), AMAG(KSTAR), ZX(1),
     .           CHI(KSTAR), SQRT(MAGERR(KSTAR)), 
     .           MCLAMP(KSTAR), DONE(ISTAR)
  665       FORMAT (I3, I7, 1P2E12.4, 0PF9.5, 1P2E12.4, A4)
         END IF
C
C Now accumulate the positional corrections.
C
         IF (KTERM .GE. 3) THEN
            DXC(ISTAR) = -ZX(2)
            DXWT(ISTAR) = 1./(WT*SNGL(CX(2,2)) + 1.E-3)
            DYC(ISTAR) = -ZX(3)
            DYWT(ISTAR) = 1./(WT*SNGL(CX(3,3)) + 1.E-3)
         ELSE
            DXWT(ISTAR) = -7.
         END IF
C
 2299    IF ((WATCH .GT. 0.5) .AND. (MOD(ISTAR,INTRVL) .EQ. 0)) THEN
            LINE = ' '
            WRITE (LINE,83) ISTAR, IMNO, PICTUR(IMNO)
   83       FORMAT (I11, ' stars corrected for image ',
     .           I3, ':  ', A30)
            CALL OVRWRT (LINE(1:70), 2)
         END IF
 2300 CONTINUE
C
      IF ((WATCH .GT. 0.5) .AND. (NSTAR .GT. INTRVL)) THEN
         LINE = ' '
         CALL OVRWRT (LINE(1:70), 2)
      END IF
C
C If no stars have converged yet, remove any systematic trends in the 
C positional residuals, and add these to the transformation coefficients.
C
      IF ((NCNVRG .GT. 0) .OR. (NADJ .LT. 1)) GO TO 4900
C
      DO J=1,NTFM
         VX(J) = 0.0D0
         VY(J) = 0.0D0
         DO I=1,NTFM
            CX(I,J) = 0.0D0
            CY(I,J) = 0.0D0
         END DO
C
      END DO
      T(1) = 1.
      L = 0
      DO 4500 ISTAR=1,NSTAR
         IF (DXWT(ISTAR) .LE. 0.) GO TO 4500
         WT = DXWT(ISTAR)*DXC(ISTAR)**2 + DYWT(ISTAR)*DYC(ISTAR)**2
         WT = 1./(1.+0.25*WT**2)
         WT = WT/CROWD(ISTAR)**1.5
         IF (NTFM .GT. 2) THEN
            T(2) = (XC(ISTAR)-XCEN(IMNO))/XWID(IMNO)
            T(3) = (YC(ISTAR)-YCEN(IMNO))/YWID(IMNO)
            IF (NTFM .GT. 5) THEN
               T(4) = (3.*T(2)**2-1.)/2.
               T(5) = T(2)*T(3)
               T(6) = (3.*T(3)**2-1.)/2.
               IF (NTFM .GT. 8) THEN
                  T(7) = (5.*T(4)-2.)*T(2)/3.
                  T(8) = T(4)*T(3)
                  T(9) = T(2)*T(6)
                  T(10) = (5.*T(6)-2.)*T(3)/3.
               END IF
            END IF
         END IF
         DO J=1,NTFM
            DWT = T(J)*DXWT(ISTAR)*WT
            VX(J) = VX(J) + DBLE(DXC(ISTAR)*DWT)
            DO I=1,NTFM
               CX(I,J) = CX(I,J) + DBLE(T(I)*DWT)
            END DO
C
            DWT = T(J)*DYWT(ISTAR)*WT
            VY(J) = VY(J) + DBLE(DYC(ISTAR)*DWT)
            DO I=1,NTFM
               CY(I,J) = CY(I,J) + DBLE(T(I)*DWT)
            END DO
         END DO
C
C Count the four-sigma detections.
C
         if (amag(istar)**2 .gt. 16.*magerr(istar)) L = L+1
 4500 CONTINUE
      MX = NTFM
      IF (L .LE. 3) MX = MIN(1, MX)
      IF (L .LE. 9) MX = MIN(3, MX)
      IF (L .LE. 18) MX = MIN(6, MX)
CC
CC Apply biases to design matrix.
CC
C      DO K=1,MX
C         CX(K,K) = CX(K,K) + CLAMP(K,IMNO)
C         CY(K,K) = CY(K,K) + CLAMP(K,IMNO)
C      END DO
      CALL DINVRS (CX, MXTERM, MX, IER)
      CALL DINVRS (CY, MXTERM, MX, IER)
      CALL DVMUL (CX, MXTERM, MX, VX, ZX)
      CALL DVMUL (CY, MXTERM, MX, VY, ZY)
      IF (MX .GE. 5) THEN
         DO I=4,MX
            K = 2*I
            J = K-1
            ZX(I) = ZX(I) / (1.D0+2.D0*DABS(ZX(I)))
            ZY(I) = ZY(I) / (1.D0+2.D0*DABS(ZY(I)))
         END DO
      END IF
      DO I=1,MX
         K = 2*I
         J = K-1
         FFEOC(J,IMNO) = FFEOC(J,IMNO) + ZX(I)
         FFEOC(K,IMNO) = FFEOC(K,IMNO) + ZY(I)
      END DO
      IF (WATCH .GE. 1.5) THEN
         CALL TBLANK
         DO J=1,MX
            STRNG(J) = DRNDFF (ZX(J), 10, 3)
         END DO
         WRITE (6,6666) IMNO, ' x: ', (STRNG(J), J=1,MX)
         DO J=1,MX
            STRNG(J) = DRNDFF (ZY(J), 10, 3)
         END DO
         WRITE (6,6666) IMNO, ' y: ', (STRNG(J), J=1,MX)
         CALL TBLANK
         DO J=1,MX
            STRNG(J) = DRNDFF (FFEOC(2*J-1,IMNO), 10, 3)
         END DO
         WRITE (6,6666) IMNO, ' x: ', (STRNG(J), J=1,MX)
         DO J=1,MX
            STRNG(J) = DRNDFF (FFEOC(2*J,IMNO), 10, 3)
         END DO
         WRITE (6,6666) IMNO, ' y: ', (STRNG(J), J=1,MX)
 6666    FORMAT (I5, A, 10A11)
         CALL TBLANK
      END IF
C
C Correct every star's mean position for the adjustments that
C have just been made to the coefficients.
C
      DO 4800 ISTAR=1,NSTAR
        IF (DXWT(ISTAR) .LT. 0.) GO TO 4800
        DXC(ISTAR) = DXC(ISTAR) - T(1)*SNGL(ZX(1))
        DYC(ISTAR) = DYC(ISTAR) - T(1)*SNGL(ZY(1))
        IF (MX .GT. 2) THEN
          T(2) = (XC(ISTAR)-XCEN(IMNO))/XWID(IMNO)
          T(3) = (YC(ISTAR)-YCEN(IMNO))/YWID(IMNO)
          DXC(ISTAR) = DXC(ISTAR) - 
     .         (T(2)*SNGL(ZX(2)) + T(3)*SNGL(ZX(3)))
          DYC(ISTAR) = DYC(ISTAR) - 
     .         (T(2)*SNGL(ZY(2)) + T(3)*SNGL(ZY(3)))
          IF (MX .GT. 5) THEN
            T(4) = (3.*T(2)**2-1.)/2.
            T(5) = T(2)*T(3)
            T(6) = (3.*T(3)**2-1.)/2.
            DXC(ISTAR) = DXC(ISTAR) - (T(4)*SNGL(ZX(4))
     .           + T(5)*SNGL(ZX(5)) + T(6)*SNGL(ZX(6)))
            DYC(ISTAR) = DYC(ISTAR) - (T(4)*SNGL(ZY(4))
     .           + T(5)*SNGL(ZY(5)) + T(6)*SNGL(ZY(6)))
            IF (MX .GT. 8) THEN
              T(7) = (5.*T(4)-2.)*T(2)/3.
              T(8) = T(4)*T(3)
              T(9) = T(2)*T(6)
              T(10) = (5.*T(6)-2.)*T(3)/3.
              DXC(ISTAR) = DXC(ISTAR) - (T(7)*SNGL(ZX(7)) +
     .             T(8)*SNGL(ZX(8)) + T(9)*SNGL(ZX(9)) + 
     .             T(10)*SNGL(ZX(10)))
              DYC(ISTAR) = DYC(ISTAR) - (T(7)*SNGL(ZY(7)) +
     .             T(8)*SNGL(ZY(8)) + T(9)*SNGL(ZY(9)) + 
     .             T(10)*SNGL(ZY(10)))
            END IF
          END IF
        END IF
 4800 CONTINUE
C
 4900 CONTINUE
      DO ISTAR=1,NSTAR
         IF (DXWT(ISTAR) .GT. 0.) THEN
            DWT = CFFSQ(3,IMNO)/DXWT(ISTAR) + CFFSQ(5,IMNO)/DYWT(ISTAR)
            DWT = 1./DWT
            XSUM(ISTAR) = XSUM(ISTAR) + DWT*
     .           (COEFF(3,IMNO)*DXC(ISTAR)+COEFF(5,IMNO)*DYC(ISTAR))
            XWT(ISTAR) = XWT(ISTAR) + DWT
            DWT = CFFSQ(4,IMNO)/DXWT(ISTAR) + CFFSQ(6,IMNO)/DYWT(ISTAR)
            DWT = 1./DWT
            YSUM(ISTAR) = YSUM(ISTAR) + DWT*
     .           (COEFF(4,IMNO)*DXC(ISTAR)+COEFF(6,IMNO)*DYC(ISTAR))
            YWT(ISTAR) = YWT(ISTAR) + DWT
         END IF
      END DO
 5000 CONTINUE
C
C Detect and delete any star whose signal-to-noise ratio is
C less than the minimum allowed for this iteration.  A
C star which is above the minimum for this iteration but
C less than S/N = 3 will not be allowed to converge.  It
C must be retained until its signal-to-noise ratio improves
C or until it is deleted.
C
      ISTAR = 0
 5003 IF (NSTAR .LE. 0) GO TO 1000
C
C Check that the LAST star is valid.  If not, shorten the star list by
C one.
C
      DWT = 0.
      DO IMNO=1,NPIC
         KSTAR = IWHICH(NSTAR,IMNO)
         IF (KSTAR .GT. 0) THEN
            IF (MAGERR(KSTAR) .GT. 0.) THEN
               DWT = DWT + AMAG(KSTAR)**2/MAGERR(KSTAR)
            END IF
         END IF
      END DO
C
      IF ((NITER .GE. MAXIT-5) .AND. (WATCH .GT. 2.5)) THEN
         WRITE (6,667) NSTAR, '  DWT =', DWT, SNMIN
  667    FORMAT (I11, A, 1P2E13.5)
      END IF
C
      IF (DWT .LT. SNMIN) THEN
         IF (ABS(WATCH) .GT. 2.5) PRINT *, 
     .        ID(NSTAR), ' disappear signal-to-noise'
         NDISAP = NDISAP+1
         NSTAR = NSTAR-1
         GO TO 5003
      ELSE IF (DWT .LT. 9.) THEN
         IF (ABS(WATCH) .GT. 2.5) PRINT *,
     .        ID(NSTAR), ' continue signal-to-noise'
         DONE(NSTAR) = .FALSE.
      END IF
C
 5005 ISTAR = ISTAR+1
C
      IF ((WATCH .GT. 0.5) .AND. 
     .     (MOD(ISTAR,1000) .EQ. 0)) THEN
         LINE = ' '
         WRITE (LINE,85) ISTAR
   85    FORMAT (I11, ' stars checked for validity.')
         CALL OVRWRT (LINE(1:70), 2)
      ELSE IF ((WATCH .GT. -1.5) .AND. 
     .     (MOD(ISTAR,10000) .EQ. 0)) THEN
         LINE = ' '
         WRITE (LINE,85) ISTAR
         CALL OVRWRT (LINE(1:70), 2)
      END IF
C
      IF (ISTAR .GE. NSTAR) GO TO 5007
C
C Check that the NEXT star (counting forward from the beginning
C of the star list) is valid.  If it is, proceed to the NEXT star.
C If not, overwrite it with the LAST star (which we already know is 
C valid), and shorten the star list by one.  Then go and check the 
C new LAST star.
C
      DWT = 0.
      DO IMNO=1,NPIC
         KSTAR = IWHICH(ISTAR,IMNO)
         IF (KSTAR .GT. 0) THEN
            IF (MAGERR(KSTAR) .GT. 0.) THEN
               DWT = DWT + AMAG(KSTAR)**2/MAGERR(KSTAR)
            END IF
         END IF
      END DO
      IF ((NITER .GE. MAXIT-5) .AND. (WATCH .GT. 2.5)) THEN
         WRITE (6,667) ISTAR, '  DWT =', DWT
      END IF
      IF (DWT .LT. SNMIN) THEN
         ID(ISTAR) = ID(NSTAR)
         XC(ISTAR) = XC(NSTAR)
         YC(ISTAR) = YC(NSTAR)
         XSUM(ISTAR) = XSUM(NSTAR)
         XWT(ISTAR) = XWT(NSTAR)
         YSUM(ISTAR) = YSUM(NSTAR)
         YWT(ISTAR) = YWT(NSTAR)
         DXOLD(ISTAR) = DXOLD(NSTAR)
         XCLAMP(ISTAR) = XCLAMP(NSTAR)
         DYOLD(ISTAR) = DYOLD(NSTAR)
         YCLAMP(ISTAR) = YCLAMP(NSTAR)
         DONE(ISTAR) = DONE(NSTAR)
         DO IMNO=1,NPIC
            IWHICH(ISTAR,IMNO) = IWHICH(NSTAR,IMNO)
         END DO
         NDISAP = NDISAP+1
         IF (ABS(WATCH) .GT. 2.5) PRINT *, 
     .        ID(NSTAR), '  disappear signal-to-noise 2'
         NSTAR = NSTAR-1
         GO TO 5003
      ELSE IF (DWT .LT. 9.) THEN
         DONE(ISTAR) = .FALSE.
         IF (ABS(WATCH) .GT. 2.5) PRINT *,
     .        ID(NSTAR), '  continue signal-to-noise 2'
      END IF
      GO TO 5005
C
 5007 CONVRG = .FALSE.
      NDONE = 0
      DO ISTAR=1,NSTAR
         IF (XWT(ISTAR) .LE. 0.) GO TO 5008
         DX = XSUM(ISTAR)/XWT(ISTAR)
         DWT = DXOLD(ISTAR)*DX
         IF (DWT .LT. 0.) THEN
            XCLAMP(ISTAR) = AMAX1(0.0001,0.5*XCLAMP(ISTAR))
         ELSE
            XCLAMP(ISTAR) = AMIN1(CLMPMX,1.04*XCLAMP(ISTAR))
         END IF
         IF (XCLAMP(ISTAR) .LE. 0.) THEN
            WRITE (6,*) 'XCLAMP', ISTAR
            WRITE (6,*)
         END IF
         DXOLD(ISTAR) = DX
         DX = DX/(1.+ABS(DX/XCLAMP(ISTAR)))
         XC(ISTAR) = XC(ISTAR)+DX
         IF (DONE(ISTAR)) THEN
            IF (DX**2 .GT. AMAX1(0.003/XWT(ISTAR), 1.E-6) ) THEN
               DONE(ISTAR) = .FALSE.
               IF (ABS(WATCH) .GT. 2.5) PRINT *, id(istar),
     .              '  continue DX =', DX
            END IF
            IF ((NITER .GE. MAXIT-5) .AND. (WATCH .GT. 2.5)) THEN
               WRITE (6,667) ISTAR, '  DX =', DX
            END IF
         END IF
C
 5008    IF (YWT(ISTAR) .LE. 0.) GO TO 5009
         DY = YSUM(ISTAR)/YWT(ISTAR)
         DWT = DYOLD(ISTAR)*DY
         IF (DWT .LT. 0.) THEN
            YCLAMP(ISTAR) = AMAX1(0.0001,0.5*YCLAMP(ISTAR))
         ELSE
            YCLAMP(ISTAR) = AMIN1(CLMPMX,1.04*YCLAMP(ISTAR))
         END IF
         IF (YCLAMP(ISTAR) .LE. 0.) THEN
            WRITE (6,*) 'YCLAMP', ISTAR
            WRITE (6,*)
         END IF
         DYOLD(ISTAR) = DY
         DY = DY/(1.+ABS(DY/YCLAMP(ISTAR)))
         YC(ISTAR) = YC(ISTAR)+DY
         IF (DONE(ISTAR)) THEN
            IF (DY**2 .GT. AMAX1(0.003/YWT(ISTAR), 1.E-6) ) THEN
               DONE(ISTAR) = .FALSE.
               IF (ABS(WATCH) .GT. 2.5) PRINT *, id(istar),
     .              '  continue DY =', DY
            END IF
            IF ((NITER .GE. MAXIT-5) .AND. (WATCH .GT. 2.5)) THEN
               WRITE (6,667) ISTAR, '  DY =', DY
            END IF
         END IF
C
 5009    CONTINUE
         IF (NITER .GE. MAXIT) DONE(ISTAR) = .TRUE.
         IF (DONE(ISTAR)) THEN
            CONVRG = .TRUE.
            NDONE = NDONE+1
         END IF
      END DO
C
C Image by image, if any star converged, write out the results 
C for it, subtract it from the reference copy of the image,
C and add it into the noise map.
C
      CALL DELFIL (1, BACKUP, IER)
      IF ((.NOT. CONVRG) .AND. (NITER .LT. MAXIT)) GO TO 8000
      IF ((WATCH .GT. -1.5) .AND. (NSTAR .GT. 200)) THEN
         LINE = ' '
         WRITE (LINE,87)
   87    FORMAT ('  Subtracting stars')
         CALL OVRWRT (LINE(1:70), 2)
      END IF
C
C Create/update .nmg file
C
      IF (START) THEN
         XMIN = 1.E20
         YMIN = 1.E20
         XMAX = -1.E20
         YMAX = -1.E20
         DO ISTAR=1,NSTAR
            XMIN = AMIN1 (XMIN, XC(ISTAR))
            YMIN = AMIN1 (YMIN, YC(ISTAR))
            XMAX = AMAX1 (XMAX, XC(ISTAR))
            YMAX = AMAX1 (YMAX, YC(ISTAR))
         END DO
         CALL DELFIL (1, EXPAND(NEWFIL), IER)
         OPEN (1, FILE=EXPAND(NEWFIL), STATUS='NEW')
         CALL WRHEAD (1, 1, NINT(XMAX-XMIN), NINT(YMAX-YMIN), 
     .        7, -500.0, 32766., THRESH, AP1, PHPADU(1), 
     .        SQRT(RONOIS(1)), FITRAD(1))
         START = .FALSE.
      ELSE
         OPEN (1, FILE=EXPAND(NEWFIL), STATUS='OLD', ACCESS='APPEND')
      END IF
C
      DO 6010 ISTAR=1,NSTAR
      IF (.NOT. DONE(ISTAR)) GO TO 6010
      DX = 0.
      DWT = 0.
      DY = 0.
      SUMRES = 0.
      SUMWT = 0.
      L = 0
      DO 6005 IMNO=1,NPIC
      KSTAR = IWHICH(ISTAR,IMNO)
      IF (KSTAR .LE. 0) GO TO 6004
      IF (MAGERR(KSTAR) .LT. 0.) GO TO 6004
C
C Check sigma and sharp
C
      DPOS = 1.085736*SQRT(MAGERR(KSTAR))/AMAG(KSTAR)
      IF (DPOS .GE. 9.9995) THEN
         MAGERR(KSTAR) = -10.
         GO TO 6004
      END IF
C
      IF ((SHP(KSTAR) .LT. SHPMIN) .OR. (SHP(KSTAR) .GT. SHPMAX)) THEN
         IWHICH(ISTAR,IMNO) = -8
         DONE(ISTAR) = .FALSE.
         GO TO 6004
      END IF
C
C Flux-weighted average magnitude
C
      DPOS = DMAG(IMNO) * AMAG(KSTAR)
      WT = 1./(MAGERR(KSTAR)*DMAG(IMNO)**2)
      DX = DX + DPOS*WT
      DWT = DWT + WT
C
C Magnitude-weighted average CHI and SHARP
C
      WT = AMAG(KSTAR)**2 / MAGERR(KSTAR)
      SUMRES = SUMRES + WT*CHI(KSTAR)
      DY = DY + WT*SHP(KSTAR)
      SUMWT = SUMWT + WT
      L = L+1
 6004 CONTINUE
 6005 CONTINUE
      IF (.NOT. DONE(ISTAR)) GO TO 6010
C
      IF (DWT .LE. 0.) THEN
         IF (ABS(WATCH) .GT. 2.5) 
     .      WRITE (6,*) ISTAR, ID(ISTAR), '  DWT < 0'
         ID(ISTAR) = -1
         NDISAP = NDISAP+1
         GO TO 6010
      END IF
      DWT = -2.5*ALOG10(DX/DWT)
      DY = AMAX1(-99.999, AMIN1(99.999, DY/SUMWT))
      DPOS = 1.085736*SQRT(1./SUMWT)
C
      IF (XC(ISTAR) .LT. 0.) THEN
         J = MIN(3, 5-INT(ALOG10(-XC(ISTAR)+0.001)))
      ELSE
         J = MIN(3, 6-INT(ALOG10(XC(ISTAR))))
      END IF
C
      IF (YC(ISTAR) .LT. 0.) THEN
         K = MIN(3, 5-INT(ALOG10(-YC(ISTAR)+0.001)))
      ELSE
         K = MIN(3, 6-INT(ALOG10(YC(ISTAR))))
      END IF
C
      STRNG1 = RNDOFF(XC(ISTAR), 9, 3)
      STRNG2 = RNDOFF(YC(ISTAR), 9, 3)
      WRITE (1,902) ID(ISTAR), STRNG1, STRNG2, DWT,
     .     DPOS, NITER, L, SUMRES/SUMWT, DY
  902 FORMAT (I7, 2A9, F9.3, F9.4, 2(I8, '.'), 2F9.3)
 6010 CONTINUE
      CALL CLFILE (1)
C
C Update scratch images
C
      DO 5500 IMNO=1,NPIC
      XMIN = 1.E6
      XMAX = -1.E6
      YMIN = 1.E6
      YMAX = -1.E6
C
C Any stars converged in this image?
C
      DO 4450 ISTAR=1,NSTAR
         IF (.NOT. DONE(ISTAR)) GO TO 4450
         IF (ID(ISTAR) .LE. 0) GO TO 4450
         KSTAR = IWHICH(ISTAR,IMNO)
         IF (KSTAR .LE. 0) GO TO 4450
         IF (MAGERR(KSTAR) .LT. 0.) GO TO 4450
         CALL BACK (XC(ISTAR), YC(ISTAR),
     .        XCEN(IMNO), XWID(IMNO), YCEN(IMNO), YWID(IMNO),
     .        FFEOC(1,IMNO), MODE(IMNO), X(ISTAR), Y(ISTAR))
         IF (AMAG(KSTAR) .LE. 0.) THEN
            CALL STUPID ('Zero magnitude.')
            WRITE (6,*) IMNO, X(ISTAR), Y(ISTAR), AMAG(KSTAR),
     .         MAGERR(KSTAR)
            CALL OOPS
         END IF
         XMIN = AMIN1(XMIN, X(ISTAR))
         XMAX = AMAX1(XMAX, X(ISTAR))
         YMIN = AMIN1(YMIN, Y(ISTAR))
         YMAX = AMAX1(YMAX, Y(ISTAR))
 4450 CONTINUE
      IF (XMIN-XMAX .GT. 1.) GO TO 5500
C
C Something converged.
C
      INPFIL = SWITCH(PICTUR(IMNO), CASE('.psf'))
      IER = RDPSF(INPFIL, IPSTYP, PAR, MAXPAR, NPAR, PSF, MAXPSF, 
     .     MAXEXP, NPSF, NEXP, NFRAC, PSFMAG, BRIGHT, XPSF, YPSF)
      IF (IER .NE. 0) THEN
         CALL TBLANK
         CALL STUPID ('Unable to open '//INPFIL(1:LENGTH(INPFIL)))
         CALL OOPS
      END IF
      PSFRAD = (REAL(NPSF-1)/2.-1.)/2.
      PSFRSQ = PSFRAD**2
      PKERR = PROERR/(PAR(1)*PAR(2)) ! **2
C
      IXMIN = MAX0(1, INT(XMIN-PSFRAD)+1)
      IXMAX = MIN0(NCOL(IMNO), INT(XMAX+PSFRAD))
      JYMIN = MAX0(1, INT(YMIN-PSFRAD)+1)
      JYMAX = MIN0(NROW(IMNO), INT(YMAX+PSFRAD))
      IF ((IXMIN .GT. IXMAX) .OR. (JYMIN .GT. JYMAX)) THEN
         GO TO 5500
      END IF
C
      IF ((NSTAR .GT. INTRVL) .AND. (WATCH .GT. 0.5)) THEN
         LINE = ' '
         WRITE (LINE,88) IMNO, PICTUR(IMNO)
   88    FORMAT ('  Subtracting stars from image ',
     .           I3, ':  ', A30)
         CALL OVRWRT (LINE(1:70), 2)
      END IF
C
      J = NPIC+IMNO
      K = 2*NPIC+IMNO
      CALL OPNPIC (J, 'W', SHOW, IER)
      CALL OPNPIC (K, 'W', SHOW, IER)
C
C Will we read/write a section for each star, or all at once?
C
      IF ((JYMAX-JYMIN+1) .LE. 2.*NDONE*NPSF) THEN
C
C All at once:  read the image section to be altered.
C
         CONVRG = .FALSE.
         CALL RDSECT (J, JYMIN, JYMAX, DATA, MAXCOL, IER)
         IF (IER .NE. 0) THEN
            WRITE (6,*) 'During subtraction, error reading',
     .           PICTUR(NPIC+IMNO)
            CALL OOPS
         END IF
         CALL RDSECT (K, JYMIN, JYMAX, SIGM, MAXCOL, IER)
         IF (IER .NE. 0) THEN
            WRITE (6,*) 'During subtraction, error reading',
     .           PICTUR(2*NPIC+IMNO)
            CALL CLPIC (NPIC+IMNO, IER)
            CALL OOPS
         END IF
      ELSE
C
C Star by star.
C
         CONVRG = .TRUE.
      END IF
C
      DO 4470 ISTAR=1,NSTAR
         IF (.NOT. DONE(ISTAR)) GO TO 4470
         IF (ID(ISTAR) .LE. 0) GO TO 4470
         KSTAR = IWHICH(ISTAR,IMNO)
         IF (KSTAR .LE. 0) GO TO 4470
         IF (MAGERR(KSTAR) .LE. 0.) GO TO 4470
         LX = MAX0(IXMIN, INT(X(ISTAR)-PSFRAD) + 1)
         MX = MIN0(IXMAX, INT(X(ISTAR)+PSFRAD))
         LY = MAX0(JYMIN, INT(Y(ISTAR)-PSFRAD) + 1)
         MY = MIN0(JYMAX, INT(Y(ISTAR)+PSFRAD))
         IF ((MX .LT. LX) .OR. (MY .LT. LY)) THEN
            MAGERR(KSTAR) = -9.
            GO TO 4470
         END IF
         IF (CONVRG) THEN
            CALL RDSECT (J, LY, MY, DATA, MAXCOL, IER)
            IF (IER .NE. 0) THEN
               WRITE (6,*) 'During subtraction, error reading',
     .              PICTUR(NPIC+IMNO)
               CALL OOPS
            END IF
            CALL RDSECT (K, LY, MY, SIGM, MAXCOL, IER)
            IF (IER .NE. 0) THEN
               WRITE (6,*) 'During subtraction, error reading',
     .              PICTUR(2*NPIC+IMNO)
               CALL CLPIC (NPIC+IMNO, IER)
               CALL OOPS
            END IF
         END IF
         DELTAX = (X(ISTAR)-1.)/XPSF-1.
         DELTAY = (Y(ISTAR)-1.)/YPSF-1.
         DO 4195 JY=LY,MY
            DY = FLOAT(JY)-Y(ISTAR)
            DYSQ=DY**2
            DO 4194 IX=LX,MX
               IF (SIGM(IX,JY) .LE. -1.E15) GO TO 4194
               DX=FLOAT(IX)-X(ISTAR)
               IF (DX**2+DYSQ .GE. PSFRSQ) THEN
                  IF (DX .GT. 0.) GO TO 4195
                  GO TO 4194
               END IF
               DUM = USEPSF(IPSTYP, DX, DY, 
     .                 BRIGHT, PAR, PSF, NPSF, NPAR, NEXP, 
     .                 NFRAC, DELTAX, DELTAY, DVDXC, DVDYC)
               DPOS = AMAG(KSTAR)*DUM
               DATA(IX,JY) = DATA(IX,JY)-DPOS
               IF (DPOS .GT. 0.) SIGM(IX,JY) = SIGM(IX,JY) - 
     .              DPOS/PHPADU(IMNO) - (PKERR*DPOS)**2
 4194       CONTINUE
 4195    CONTINUE                      ! End of loops over pixels
         IF (CONVRG) THEN
            CALL WRSECT (J, LY, MY, DATA, MAXCOL, IER)
            IF (IER .NE. 0) THEN
               WRITE (6,*) 'During subtraction, error writing',
     .              PICTUR(J)
               CALL OOPS
            END IF
            CALL WRSECT (K, LY, MY, SIGM, MAXCOL, IER)
            IF (IER .NE. 0) THEN
               WRITE (6,*) 'During subtraction, error writing',
     .              PICTUR(K)
               CALL CLPIC (NPIC+IMNO, IER)
               CALL OOPS
            END IF
         END IF
 4470 CONTINUE
C
      IF (.NOT. CONVRG) THEN
C
C All at once:  write out the altered image section.
C
         CALL WRSECT (J, JYMIN, JYMAX, DATA, MAXCOL, IER)
         IF (IER .NE. 0) THEN
            WRITE (6,*) 'During subtraction, error writing',
     .           PICTUR(J)
            CALL OOPS
         END IF
         CALL WRSECT (K, JYMIN, JYMAX, SIGM, MAXCOL, IER)
         IF (IER .NE. 0) THEN
            WRITE (6,*) 'During subtraction, error writing',
     .           PICTUR(K)
            CALL OOPS
         END IF
      END IF
      CALL CLPIC (NPIC+IMNO, IER)
      CALL CLPIC (2*NPIC+IMNO, IER)
C
C Fill up .alf file.
C
      OPEN (11, FILE=EXPAND(OUTPUT(IMNO)), STATUS='OLD',
     .     ACCESS='APPEND')
      DO ISTAR=1,NSTAR
         IF ((DONE(ISTAR)) .AND. (ID(ISTAR) .GT. 0)) THEN
            KSTAR = IWHICH(ISTAR,IMNO)
            IF (KSTAR .GT. 0) THEN
               IF (MAGERR(KSTAR).GT.0.) THEN
                  DPOS = 1.085736*SQRT(MAGERR(KSTAR))/AMAG(KSTAR)
                  CALL BACK (XC(ISTAR), YC(ISTAR),
     .                 XCEN(IMNO), XWID(IMNO), YCEN(IMNO), YWID(IMNO),
     .                 FFEOC(1,IMNO), MODE(IMNO), X(ISTAR), Y(ISTAR))
                  DWT = PSFMAG - 2.5 * ALOG10(AMAG(KSTAR))
C
                  IF (X(ISTAR) .LE. 0.) THEN
                     J = MIN(3, 5-INT(ALOG10(-X(ISTAR)+0.001)))
                  ELSE
                     J = MIN(3, 6-INT(ALOG10(X(ISTAR))))
                  END IF
C
                  IF (Y(ISTAR) .LE. 0.) THEN
                     K = MIN(3, 5-INT(ALOG10(-Y(ISTAR)+0.001)))
                  ELSE
                     K = MIN(3, 6-INT(ALOG10(Y(ISTAR))))
                  END IF
C
                  IF (SKY(KSTAR) .LE. 0.) THEN
                     L = MIN(3, MAX(0, 5-INT(
     .                    ALOG10(-SKY(KSTAR)+0.001))))
                  ELSE
                     L = MIN(3, MAX(0, 6-INT(
     .                    ALOG10(SKY(KSTAR)))))
                  END IF
C
                  STRNG1 = RNDOFF(X(ISTAR), 9, 3)
                  STRNG2 = RNDOFF(Y(ISTAR), 9, 3)
                  STRNG3 = RNDOFF(DPOS, 9, 4)
                  STRNG4 = RNDOFF(SKY(KSTAR), 9, 2)
                  STRNG5 = RNDOFF(SHP(KSTAR), 9, 3)
                  WRITE (11,321) ID(ISTAR), STRNG1, STRNG2, 
     .                 DWT, STRNG3, STRNG4, 
     .                 FLOAT(NITER), CHI(KSTAR), 
     .                 STRNG5
  321             FORMAT (I7, 2A9, F9.3, 2A9, F9.0, F9.2, A9)
               END IF
            END IF
         END IF
      END DO
      CALL CLFILE (11)
 5500 CONTINUE
C
C Fill up .tfr file.
C
      OPEN (11, FILE=EXPAND(TFRFIL), STATUS='OLD',
     .     ACCESS='APPEND')
      DO 5800 ISTAR=1,NSTAR
         IF (ID(ISTAR) .LE. 0) GO TO 5800
         IF (.NOT. DONE(ISTAR)) GO TO 5800
         L = (NPIC+17)/18
         DO 5700 IMNO=1,NPIC
            WROTE(IMNO) = 0
            K = IWHICH(ISTAR,IMNO)
            IF (K .LE. 0) GO TO 5700
            IF (MAGERR(K) .LE. 0.) GO TO 5700
            NOUT(IMNO) = NOUT(IMNO)+1
            WROTE(IMNO) = NOUT(IMNO)
 5700    CONTINUE
C
         DO K=1,L
            I = (K-1)*18+1
            J = MIN0(NPIC, 18*K)
            STRNG1 = RNDOFF(XC(ISTAR), 9, 3)
            STRNG2 = RNDOFF(YC(ISTAR), 9, 3)
            WRITE (11,157) ID(ISTAR), STRNG1, STRNG2,
     .           (WROTE(IMNO), IMNO=I,J)
  157       FORMAT (I7, 2A9, 18I7)
         END DO
 5800 CONTINUE
      CALL CLFILE (11)
C
C Now drop the stars that have disappeared off the bottom of the
C star list.
C
      ISTAR = 0
 4405 IF (NSTAR .LE. ISTAR) GO TO 4420
      IF (ID(NSTAR) .LE. 0) THEN
         NSTAR = NSTAR-1
         GO TO 4405
      END IF
 4410 ISTAR = ISTAR+1
      IF (ISTAR .GE. NSTAR) GO TO 4420
      IF (ID(ISTAR) .LE. 0) THEN
         ID(ISTAR) = ID(NSTAR)
         XC(ISTAR) = XC(NSTAR)
         YC(ISTAR) = YC(NSTAR)
         DO IMNO=1,NPIC
            IWHICH(ISTAR,IMNO) = IWHICH(NSTAR,IMNO)
         END DO
         DXOLD(ISTAR) = DXOLD(NSTAR)
         DYOLD(ISTAR) = DYOLD(NSTAR)
         XCLAMP(ISTAR) = XCLAMP(NSTAR)
         YCLAMP(ISTAR) = YCLAMP(NSTAR)
         NSTAR = NSTAR-1
         GO TO 4405
      ELSE
         GO TO 4410
      END IF
 4420 CONTINUE
C
C Now drop the stars that have converged off the bottom of the
C star list.
C
      ISTAR = 0
 4505 IF (NSTAR .LE. ISTAR) GO TO 4520
      IF (DONE(NSTAR)) THEN
         NCNVRG = NCNVRG+1
         IF (ABS(WATCH) .GT. 2.5) WRITE (6,*) NSTAR,
     .        ID(NSTAR), ' converged A', NCNVRG
         NSTAR = NSTAR-1
         GO TO 4505
      END IF
 4510 ISTAR = ISTAR+1
      IF (ISTAR .GE. NSTAR) GO TO 4520
      IF (DONE(ISTAR)) THEN
         NCNVRG = NCNVRG+1
         IF (ABS(WATCH) .GT. 2.5) WRITE (6,*) ISTAR,
     .        ID(ISTAR), ' converged B', NCNVRG
         ID(ISTAR) = ID(NSTAR)
         XC(ISTAR) = XC(NSTAR)
         YC(ISTAR) = YC(NSTAR)
         DO IMNO=1,NPIC
            IWHICH(ISTAR,IMNO) = IWHICH(NSTAR,IMNO)
         END DO
         DXOLD(ISTAR) = DXOLD(NSTAR)
         DYOLD(ISTAR) = DYOLD(NSTAR)
         XCLAMP(ISTAR) = XCLAMP(NSTAR)
         YCLAMP(ISTAR) = YCLAMP(NSTAR)
         NSTAR = NSTAR-1
         GO TO 4505
      ELSE
         GO TO 4510
      END IF
 4520 CONTINUE
 8000 OPEN (1, FILE=EXPAND(BACKUP), ACCESS='SEQUENTIAL',
     .     STATUS='NEW', FORM='UNFORMATTED')
C
C-----------------------------------------------------------------------
C
      WRITE (1) NITER, NXTSKY, NPIC, NSTAR, NLIST, 
     .     NDISAP, NCNVRG, IEXP, CLMPMX, CENTER, RANGE, PERERR, 
     .     PROERR, SKYISQ, SKYOUT, SKYOSQ, NADJ, CLIP, 
     .     SEPMAX, CRDMAX, START
C
      J = 3*NPIC
      WRITE (1) (PICTUR(I), MODE(I), IMGTYP(I), DATTYP(I), I=1,J)
C
      WRITE (1) (NCOL(I), NROW(I), XCEN(I), XWID(I), YCEN(I), YWID(I),
     .     FITRAD(I), LOBAD(I), HIBAD(I), 
     .     RONOIS(I), PHPADU(I), DMAG(I), NOUT(I), SEPCRT(I), 
     .     (COEFF(J,I), CFFSQ(J,I), FFEOC(J,I), CLAMP(J,I), 
     .     J=1,MODE(I)), I=1,NPIC)
C
C Write out a hundred stars at once to save transfer time.
C
      N = INT((NSTAR-1)/100)
      DO L=0,N
         J = 100*L + 1
         K = MIN0(100*(L+1), NSTAR)
         WRITE (1) ((IWHICH(I,IMNO), IMNO=1,NPIC),
     .        ID(I), XC(I), YC(I), XCLAMP(I), YCLAMP(I), 
     .        DXOLD(I), DYOLD(I), I=J,K)
      END DO
C
C Write out a thousand stars at once to save transfer time.
C
      N = INT((NLIST-1)/1000)
      DO L=0,N
         J = 1000*L + 1
         K = MIN0(1000*(L+1), NLIST)
         WRITE (1) (AMAG(I), MAGERR(I), SKY(I), 
     .        CHI(I), MOLD(I), MCLAMP(I), I=J,K)
      END DO
C
      J = NPIC+1
      OUTPUT(J) = 'COMPLETE                      '
      WRITE (1) (OUTPUT(I), I=1,J)
C
C-----------------------------------------------------------------------
C
      CALL CLFILE (1)
      GO TO 1000
C
 9000 CALL OVRWRT (' ', 2)
C
C Normal exit.
C
      CALL DELFIL (1, BACKUP, IER)
      DO IMNO=1,NPIC
         J = NPIC+IMNO
         K = NPIC+J
c        CALL OPNPIC (K, 'R', SHOW, IER)
         CALL DELPIC (K, IER)
         IF (DATTYP(J) .NE. DATTYP(IMNO)) THEN
            CALL OPNPIC (J, 'R', SHOW, IER)
            JYMIN = 1
            JYMAX = NROW(J)
            CALL RDSECT (J, JYMIN, JYMAX, DATA, MAXCOL, IER)
            CALL CLPIC (J, IER)
            CALL DELPIC (J, IER)
            CALL OPNPIC (IMNO, 'R', SHOW, IER)
            NCOL(J) = NCOL(IMNO)
            NROW(J) = NROW(IMNO)
            IMGTYP(J) = IMGTYP(IMNO)
            DATTYP(J) = DATTYP(IMNO)
            CALL CREPIC (IMNO, J, IER)
            CALL WRSECT (J, JYMIN, JYMAX, DATA, MAXCOL, IER)
            CALL CLPIC (IMNO, IER)
            CALL CLPIC (J, IER)
         END IF
      END DO
      CALL BYEBYE
      END!
C
C#######################################################################
C
      SUBROUTINE  INV  (COEFF, MODE, COL, ROW, XCEN, XWID, YCEN, YWID,
     .      FFEOC)
      PARAMETER (MAXCFF=20, NPLACE=8)
      DOUBLE PRECISION C(MAXCFF/2,MAXCFF/2), VX(MAXCFF/2),
     .     VY(MAXCFF/2), D(MAXCFF/2), COEFF(MAXCFF), FFEOC(MAXCFF)
      REAL PLACES(NPLACE), XC(4), YC(4)
      INTEGER ID(4)
      DATA PLACES /0.,0.08333,0.25,0.41667,0.58333,0.75,0.91667,1./
C
C Determine area in master list spanned by this image.
C
      CALL GTFM (1.0, 1.0, COL, ROW, COEFF, MODE, XC(1), YC(1))
      CALL GTFM (1.0, ROW, COL, ROW, COEFF, MODE, XC(2), YC(2))
      CALL GTFM (COL, ROW, COL, ROW, COEFF, MODE, XC(3), YC(3))
      CALL GTFM (COL, 1.0, COL, ROW, COEFF, MODE, XC(4), YC(4))
      CALL QUICK (XC, 4, ID)
      CALL QUICK (YC, 4, ID)
      XCEN = (XC(1) + XC(2) + XC(3) + XC(4))/4.
      XWID = (XC(3) + XC(4) - XC(2) - XC(1))/4.
      YCEN = (YC(1) + YC(2) + YC(3) + YC(4))/4.
      YWID = (YC(3) + YC(4) - YC(2) - YC(1))/4.
C
      NGEO = MODE/2
      DO I=1,NGEO
         VX(I) = 0.0D0
         VY(I) = 0.0D0
         DO J=1,NGEO
            C(J,I) = 0.0D0
         END DO
      END DO
C
      DO J=1,NPLACE
         Y = PLACES(J)*(ROW-1.)+1.
         DO I=1,NPLACE
            X = PLACES(I)*(COL-1.)+1.
            CALL GTFM (X, Y, COL, ROW, COEFF, MODE, XX, YY)
            D(1) = 1.D0
            D(2) = (XX-XCEN)/XWID
            D(3) = (YY-YCEN)/YWID
            IF (MODE .GE. 9) THEN
               D(4) = (3.D0*D(2)**2-1.D0)/2.D0
               D(5) = D(2)*D(3)
               D(6) = (3.D0*D(3)**2-1.D0)/2.D0
               IF (MODE .GE. 15) THEN
                  D(7) = (5.D0*D(4)-2.D0)*D(2)/3.D0
                  D(8) = D(4)*D(3)
                  D(9) = D(6)*D(2)
                  D(10) = (5.D0*D(6)-2.D0)*D(3)/3.D0
               END IF
            END IF
            DO L=1,NGEO
               VX(L) = VX(L)+D(L)*X
               VY(L) = VY(L)+D(L)*Y
               DO K=1,NGEO
                  C(K,L) = C(K,L)+D(K)*D(L)
               END DO
            END DO
         END DO
      END DO
      CALL DINVRS (C, MAXCFF/2, NGEO, IFLAG)
      CALL DVMUL (C, MAXCFF/2, NGEO, VX, D)
      DO K=1,MODE-1,2
         FFEOC(K) = D((K+1)/2)
      END DO
      CALL DVMUL (C, MAXCFF/2, NGEO, VY, D)
      DO K=2,MODE,2
         FFEOC(K) = D(K/2)
      END DO
C
      RETURN
      END
C
C#######################################################################
C
      SUBROUTINE  BACK  (X, Y, XCEN, XWID, YCEN, YWID,  
     .     FFEOC, MODE, XX, YY) 
      DOUBLE PRECISION FFEOC(20), D(10)
      D(2) = (X-XCEN)/XWID 
      D(3) = (Y-YCEN)/YWID
      XX = FFEOC(1) + FFEOC(3)*D(2) + FFEOC(5)*D(3)
      YY = FFEOC(2) + FFEOC(4)*D(2) + FFEOC(6)*D(3)
      IF (MODE .GE. 9) THEN 
         D(4) = (3.D0*D(2)**2-1.D0)/2.D0
         D(5) = D(2)*D(3)
         D(6) = (3.D0*D(3)**2-1.D0)/2.D0
         XX = XX + FFEOC(7)*D(4) + FFEOC(9)*D(5) + FFEOC(11)*D(6)
         YY = YY + FFEOC(8)*D(4) + FFEOC(10)*D(5) + FFEOC(12)*D(6)
         IF (MODE .GE. 15) THEN
            D(7) = (5.D0*D(4)-2.D0)*D(2)/3.D0
            D(8) = D(4)*D(3)
            D(9) = D(6)*D(2)
            D(10) = (5.D0*D(6)-2.D0)*D(3)/3.D0
            XX = XX + FFEOC(13)*D(7) + FFEOC(15)*D(8) +
     .           FFEOC(17)*D(9) + FFEOC(19)*D(10)
            YY = YY + FFEOC(14)*D(7) + FFEOC(16)*D(8) +
     .           FFEOC(18)*D(9) + FFEOC(20)*D(10)
         END IF
      END IF  
      RETURN  
      END     
