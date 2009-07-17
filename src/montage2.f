C
C Official DAO version:  2004 January 15
C
C=======================================================================
C
      include 'arrays.inc'
C
      PARAMETER (MAXOPN=57, MAXUSE=57, MINSKY=1000)
      PARAMETER (MXINC=12288, MXOUTC=12288, MXOUTR=12288)
C
      CHARACTER FILE(MAXPIC)*40, NAME*40, SUFF*40, SWITCH*40, 
     .     EXTEND*40, SQUEEZ*40, TEMP*40
      CHARACTER LINE*310, COMMAS*19, OFFSCA*31, CASE*5, ANSWER*1
      CHARACTER STRNG1*19, STRNG2*19, STRNG3*19
      DOUBLE PRECISION COEFF(20,MAXPIC), FFEOC(6,MAXPIC)
      REAL GETSKY
      REAL SKY(MAXPIC), DATUM(MAXPIC), WT(MAXPIC), HOLD(MAXPIC)
      REAL ROW(MXINC,MAXPIC), WORK(MXOUTC,MXOUTR),
     .     WEIGHT(MAXPIC), PHPADU(MAXPIC), READNS(MAXPIC)
      REAL COLS(MAXPIC), ROWS(MAXPIC), DMAG(MAXPIC), HIBAD(MAXPIC),
     .     LOBAD(MAXPIC)
     
      REAL XC(4), YC(4)
      INTEGER IWORK(MXOUTC,MXOUTR)
      INTEGER INDEX(MAXPIC), LAST(MAXPIC), LENGTH, IWHICH(MAXPIC)
      INTEGER XLO(MAXPIC), XHI(MAXPIC), YLO(MAXPIC), YHI(MAXPIC)
      INTEGER LCOL(MAXPIC), LROW(MAXPIC), MODE(MAXPIC)
      INTEGER IXMIN, IXMAX, IYMIN, IYMAX, NSUFF
      INTEGER LXMIN, LXMAX, LYMIN, LYMAX
      LOGICAL ALIGN(MAXPIC), ROTATE, USEPDS, USEMAG, SHOW
C
      EQUIVALENCE (WORK, IWORK)
      DATA LCOL/MAXPIC*0/, LROW/MAXPIC*0/
C
C-----------------------------------------------------------------------
C
      SHOW = .false.
      CALL FABORT
      NUSE = 13
      OFFSCA = COMMAS(MAXPIC-1,I)
      WRITE (6,1) OFFSCA(1:I)
    1 FORMAT (/'   Currently dimensioned for ', A, ' input images')
      USEMAG = .FALSE.
      USEPDS = .FALSE.
      CALL TBLANK
      NMAX = MAXPIC-1
C
C Get file with transformations.
C
  700 NAME = ' '
      CALL GETNAM ('File with transformations:', NAME)
      IF (NAME .EQ. 'END-OF-FILE') CALL BYEBYE
      DELTAM = -99.999
      I = 31
  705 I = I-1
      IF (I .LE. 0) GO TO 710
      IF ((NAME(I:I) .EQ. '/') .OR. (NAME(I:I) .EQ. '*') .OR.
     .     (NAME(I:I) .EQ. '!')) THEN
         IF (NAME(I:I) .EQ. '/') THEN
            READ (NAME(I+1:30),*) NUSE
            NUSE = MIN0(MAXUSE, NUSE)
         ELSE IF (NAME(I:I) .EQ. '*') THEN
            USEPDS = .TRUE.
         ELSE IF (NAME(I:I) .EQ. '!') THEN
            USEMAG = .TRUE.
         END IF
         NAME(I:30) = ' '
      END IF
      GO TO 705
C
  710 NAME = EXTEND(NAME, CASE('mch'))
      CALL INFILE (1, NAME, IER)
      IF (IER .NE. 0) THEN
         CALL STUPID ('Unable to open '//name)
         CALL OOPS
      END IF
      NFRAME = 0
  750 NFRAME = NFRAME+1
  755 CALL RDCHAR (1, LINE, N, IER)
      IF (IER .GT. 0) GO TO 790
      READ (LINE,*,IOSTAT=IER) FILE(NFRAME), 
     .   (COEFF(J,NFRAME), J=1,6), DMAG(NFRAME), SIG, 
     .   (COEFF(J,NFRAME), J=7,20)
      IF (IER .EQ. 0) THEN
         MODE(NFRAME) = 20
      ELSE 
         READ (LINE,*,IOSTAT=IER) FILE(NFRAME), 
     .      (COEFF(J,NFRAME), J=1,6), DMAG(NFRAME), SIG, 
     .      (COEFF(J,NFRAME), J=7,12)
         IF (IER .EQ. 0) THEN
            MODE(NFRAME) = 12
         ELSE
            READ (LINE,*,IOSTAT=IER) FILE(NFRAME), 
     .           (COEFF(J,NFRAME), J=1,6), DMAG(NFRAME)
            IF (IER .EQ. 0) THEN
               MODE(NFRAME) = 6
            ELSE
               CALL STUPID ('Error reading line:')
               WRITE (6,*) LINE(1:N)
               CALL TBLANK
               GO TO 755
            END IF
         END IF
      END IF
      IF (IER .NE. 0) THEN
         CALL STUPID ('Invalid transformation file.')
         CALL CLFILE (1)
         CALL OOPS
      END IF
      IF (NFRAME .GE. MAXPIC) THEN
         CALL STUPID ('Too many input images!')
         CALL CLFILE (1)
         CALL OOPS
      END IF
C
C Ignore all .stc, .2ma, .mag, .nmg, .fet, .jnk and .dss files.
C
      do i=1,27
         if (file(nframe)(i:i+3) .eq. '.stc') go to 755
         if (file(nframe)(i:i+3) .eq. '.2ma') go to 755
         if (file(nframe)(i:i+3) .eq. '.nmg') go to 755
         if (file(nframe)(i:i+3) .eq. '.fet') go to 755
         if (file(nframe)(i:i+3) .eq. '.jnk') go to 755
         if (file(nframe)(i:i+3) .eq. '.dss') go to 755
         if (.not. usemag) then
            if (file(nframe)(i:i+3) .eq. '.mag') go to 755
         end if
         if (.not. usepds) then
            if (file(nframe)(i:i+3) .eq. '.pds') go to 755
         end if
      end do
C
  758 IF (DMAG(NFRAME) .GT. DELTAM) DELTAM = DMAG(NFRAME)
      GO TO 750
C
  790 NFRAME = NFRAME-1
      CALL CLFILE (1)
      IF (NFRAME .EQ. 0) THEN
         CALL STUPID ('No valid lines in input file.')
         CALL OOPS
      END IF
      CALL GETCHR ('Image-name suffix:', SUFF, NSUFF)
      CALL TBLANK
C
      XFIRST = 0.
      YFIRST = 0.
      DO IMNO=1,NFRAME
         PICTUR(IMNO) = SWITCH(FILE(IMNO), SUFF(1:NSUFF))
C
C Open the picture, just to make sure it is there.
C
         IMGTYP(IMNO) = '    '
         DATTYP(IMNO) = '    '
         CALL OPNPIC (IMNO, 'R', .FALSE., IER)
         IF (NCOL(IMNO) .LE. 0) THEN
            CALL STUPID (' This image can''t be opened: '//
     .           PICTUR(IMNO))
            CALL OOPS
         ELSE IF (NCOL(IMNO) .gt. MXINC) then
            CALL STUPID (' Exceeded buffer size: '//PICTUR(IMNO))
            WRITE (6,*) NCOL(IMNO), ' > ', MXINC
            CALL OOPS
         END IF
         CALL CLPIC (IMNO, IER)
         COLS(IMNO) = REAL(NCOL(IMNO))
         ROWS(IMNO) = REAL(NROW(IMNO))
C
C Provisionally invert the transformations assuming the output
C image will have the same dimensions as the input image.
C This is good enough for predicting where the corners will be
C
         CALL INVRT (COEFF(1,IMNO), MODE(IMNO), FFEOC(1,IMNO))
C
         DX = ABS(COEFF(3,IMNO)*COEFF(6,IMNO))
         DY = ABS(COEFF(4,IMNO)*COEFF(5,IMNO))
         IF (DX .GT. DY) THEN
            ALIGN(IMNO) = .TRUE.
            XFIRST = XFIRST + (DX+DY)*COLS(IMNO)*ROWS(IMNO)
         ELSE
            ALIGN(IMNO) = .FALSE.
            YFIRST = YFIRST + (DX+DY)*COLS(IMNO)*ROWS(IMNO)
         END IF
C
C Get correct transformation constants for this frame.
C
         CALL INFILE (1, FILE(IMNO), IER)
         IF (IER .NE. 0) THEN
            CALL STUPID (' Unable to open '//FILE(IMNO))
            CALL OOPS
         END IF
         CALL RDHEAD (1, IDUM, I, J, LOBAD(IMNO), HIBAD(IMNO), 
     .        DUM, DUM, PHPADU(IMNO), READNS(IMNO), DUM)
C
         CALL CLFILE (1)
         TEMP = SWITCH(FILE(IMNO), '.psf')
         CALL INFILE (1, TEMP, IER)
         IF (IER .NE. 0) THEN
C
C If no PSF assume critical sampling.
C
            CALL STUPID ('Error opening PSF file '//TEMP)
            XX = 1.12
            YY = 1.12
         ELSE
            READ (1,*)
            READ (1,201) XX, YY
  201       FORMAT (1X, 2E13.6)
            CALL CLFILE (1)
         END IF
C
C Determine where the corners of this frame lie in the coordinate
C system of the master frame.
C
         CALL GTFM (1., 1., COLS(IMNO), ROWS(IMNO), 
     .        COEFF(1,IMNO), MODE(IMNO), XC(1), YC(1))
         CALL GTFM (COLS(IMNO), 1., COLS(IMNO), ROWS(IMNO), 
     .        COEFF(1,IMNO), MODE(IMNO), XC(2), YC(2))
         CALL GTFM (COLS(IMNO), ROWS(IMNO), COLS(IMNO), 
     .        ROWS(IMNO), COEFF(1,IMNO), MODE(IMNO), XC(3), YC(3))
         CALL GTFM (1., ROWS(IMNO), COLS(IMNO), ROWS(IMNO), 
     .        COEFF(1,IMNO), MODE(IMNO), XC(4), YC(4))
C
C Determine the minimum and maximum  x  and  y  coordinates of the 
C corners of this frame, in the coordinate system of the master frame.
C
         XLO(IMNO) = NINT(SMLLST(XC, 4, 1)-0.50001) + 1
         XHI(IMNO) = NINT(BIGGST(XC, 4, 1)-0.49999)
         YLO(IMNO) = NINT(SMLLST(YC, 4, 1)-0.50001) + 1
         YHI(IMNO) = NINT(BIGGST(YC, 4, 1)-0.49999)
         WRITE (6,697) XLO(IMNO), XHI(IMNO), YLO(IMNO),
     .        YHI(IMNO), PICTUR(IMNO)(1:LENGTH(PICTUR(IMNO)))
  697    FORMAT ( I7, ' < x < ', I6, ', ', I7, ' < y < ', I6, 
     .        '  spanned by ', A)
C
C Correct counts to consitent number of DN per pixel.
C
C
C Factor by which images must be multiplied to achieve comparable numbers of
C DN/PSF --- aperture and exposure time.
C
         DMAG(IMNO)=EXP(-0.921*DMAG(IMNO))
C
C Fluxes must be scaled by sampling interval squared to go from DN per
C arcsecond to DN per pixel (assuming constant aperture radius in
C arcseconds).
C
         DY = (COEFF(3,IMNO)**2 + COEFF(4,IMNO)**2 +
     .        COEFF(5,IMNO)**2 + COEFF(6,IMNO)**2) / 2.
C
         DMAG(IMNO) = DMAG(IMNO)/DY
c     print*,'V ',PICTUR(IMNO)(1:LENGTH(PICTUR(IMNO))),dy,dmag(imno)
C
C Image scale in arcsec/pixel
C
         DY = SQRT(DY)
C
C XX and YY are the HWHM in x and y.  Their sum is the average FWHM in pixels
C
         XX = XX + YY
C
C FWHM in arcseconds
C
         HOLD(IMNO) = XX*DY
C
C Scale WEIGHT as (1/flux scale) ! **2
C
         WEIGHT(IMNO) = 1./DMAG(IMNO) ! **2
c     print*,'W ',PICTUR(IMNO)(1:LENGTH(PICTUR(IMNO))),dmag(imno),
c    .   weight(imno)
C
C Effective seeing is FWHM in arcseconds plus one pixel in arcseconds
C added in quadrature (prevents a severely undersampled image from
C dominating.)
C
         HOLD(IMNO) = HOLD(IMNO)**2 + DY**2
c     print*,'X ',PICTUR(IMNO)(1:LENGTH(PICTUR(IMNO))),dy,
c    .     hold(imno)
C
C Give greater weight to better-seeing frames.
C
         WEIGHT(IMNO) = WEIGHT(IMNO)/HOLD(IMNO)**2 / dy    !xyz
c        WEIGHT(IMNO) = WEIGHT(IMNO)/HOLD(IMNO)**2 / dy**4 !xyz
c     print*,'Y ',PICTUR(IMNO)(1:LENGTH(PICTUR(IMNO))),dy,
c    .     hold(imno),weight(imno)
      END DO
C
C XFIRST and YFIRST are the total areas, in units proportional to 
C square arcseconds, of all input images basically aligned with and
C basically rotated with respect to the master list.
C
      IF (YFIRST .GT. 1.2*XFIRST) THEN
         ROTATE = .TRUE.
      ELSE
         ROTATE = .FALSE.
      END IF
C
      CALL ISORT (XLO, NFRAME)
      CALL ISORT (XHI, NFRAME)
      CALL ISORT (YLO, NFRAME)
      CALL ISORT (YHI, NFRAME)
      WRITE (6,5)
    5 FORMAT (/'              X range           Y range',
     .     '        Dimensions'/)
      J = 0
      DO I=1,NFRAME
         IDX = XHI(NFRAME-I+1)-XLO(I)+1
         IDY = YHI(NFRAME-I+1)-YLO(I)+1
         WRITE (6,6) I, XLO(I), XHI(NFRAME-I+1),
     .              YLO(I), YHI(NFRAME-I+1), IDX, IDY
    6    FORMAT (1X, I5, 2(I9, '   ', I6), I8, ' x', I6, '    <<')
CCC      IF ((IDX .GT. 0) .AND. (IDX*IDY .GT. MINSKY)) J = I
      END DO
C
CCC   IF (J .LE. 0) THEN
CCC      CALL STUPID (' Insufficient overlap to compare sky levels.')
CCC      CALL OOPS
CCC   END IF
C
      CALL TBLANK
      CALL GETDAT ('Minimum number of frames, percentile:', DATUM, 2)
      IF (DATUM(1) .LE. 0.) GO TO 3100
      NMIN=NINT(DATUM(1))
      IF (NMIN .GT. NUSE) THEN
         CALL STUPID ('Minumum number of frames is too high!')
         CALL OOPS
      END IF
      PCT=AMAX1(0.,AMIN1(1.,DATUM(2)))
      LXMIN=XLO(NMIN)
      LXMAX=XHI(NFRAME-NMIN+1)
      LYMIN=YLO(NMIN)
      LYMAX=YHI(NFRAME-NMIN+1)
 1901 CALL GETDAT ('X limits of output image:', DATUM, 2)
      IF ((DATUM(1).GT.-1.E19) .AND. (DATUM(1).LE.DATUM(2))) THEN
         IXMIN = NINT(AMIN1(DATUM(1), DATUM(2)))
         IXMAX = NINT(AMAX1(DATUM(1), DATUM(2)))
      ELSE
         IXMIN = LXMIN
         IXMAX = LXMAX
      END IF
      CALL GETDAT ('Y limits of output image:', DATUM, 2)
      IF ((DATUM(1).GT.-1.E19) .AND. (DATUM(1).LE.DATUM(2))) THEN
         IYMIN = NINT(AMIN1(DATUM(1), DATUM(2)))
         IYMAX = NINT(AMAX1(DATUM(1), DATUM(2)))
      ELSE
         IYMIN = LYMIN
         IYMAX = LYMAX
      END IF
      CALL GETDAT ('Expansion factor:', DATUM, 1)
      IF (DATUM(1) .LE. 0.) GO TO 1901
      IEXPND = NINT(DATUM(1))
      IEXPND = MIN0( IEXPND, 12288/(IXMAX-IXMIN+1),
     .     12288/(IYMAX-IYMIN+1) )
      IF (IEXPND .LE. 0) CALL OOPS
C
C IXMIN, IXMAX, IYMIN, IYMAX now delimit the rectangular extent of the
C output image, in the coordinate system of the master frame.
C
      MCOL = IEXPND*(IXMAX-IXMIN+1)
      MROW = IEXPND*(IYMAX-IYMIN+1)
      STRNG1 = SQUEEZ(IXMIN-1, IX1)
      STRNG2 = SQUEEZ(IYMIN-1, IX2)
      STRNG3 = SQUEEZ(IEXPND, IY1)
      WRITE (6,7) IXMIN, IXMAX, IYMIN, IYMAX, MCOL, MROW,
     .     STRNG1(1:IX1), STRNG2(1:IX2), STRNG3(1:IY1)
    7 FORMAT (/20X, '                      X range = ', 2I7 /
     .         20X, '                      Y range = ', 2I7 /
     .         20X, '                Expanded size = ', 2I7 //
     .         ' Offsets (Star list) - (montaged image) and scale = ',
     .     A, 1X, A, 1X, A/)
      WRITE (OFFSCA,4) STRNG1(1:IX1), STRNG2(1:IX2), STRNG3(1:IY1)
    4 FORMAT ('xo=', A, ' yo=', A, ' sca=', A)
C
      IX1 = MAX0(IXMIN, XLO(NFRAME))
      IX2 = MIN0(IXMAX, XHI(1))
      IY1 = MAX0(IYMIN, YLO(NFRAME))
      IY2 = MIN0(IYMAX, YHI(1))
      WRITE (6,9) IX1, IX2, IY1, IY2, (IX2-IX1+1), (IY2-IY1+1)
    9 FORMAT (/' Sky will be determined from...'//
     .     6X, 2(I9, ' to', I7), I9, ' x', I7/)
      CALL GETYN ('Determine sky from overlap region?', ANSWER)
      IF (ANSWER .EQ. 'E') CALL BYEBYE
      NAME = SWITCH(NAME, SUFF)
      CALL GETNAM ('Name for output image:', NAME)
C
C Now determine the sky value for each of the input frames, from
C whatever rectangular area most closely corresponds to region contained
C in the largest possible number of frames.
C
      CALL TBLANK
C
C These coordinates delimit the minimum rectangle that contains at
C least some piece of each frame.
C
      WTMAX = 0.
      DO IMNO=1,NFRAME
C
         IF (ANSWER .EQ. 'Y') THEN
C Reversing the transformations, determine where the corners of the
C maximum overlap area lie in the coordinate system of this frame and
C determine the sky value.
C
            CALL BCKWRD (REAL(IX1), REAL(IY1), COLS(IMNO), ROWS(IMNO),
     .           COEFF(1,IMNO), FFEOC(1,IMNO), MODE(IMNO), XC(1), YC(1))
            XC(1) = AMIN1(COLS(IMNO), AMAX1(1., XC(1)))
            YC(1) = AMIN1(ROWS(IMNO), AMAX1(1., YC(1)))
c
            CALL BCKWRD (REAL(IX2), REAL(IY1), COLS(IMNO), ROWS(IMNO),
     .           COEFF(1,IMNO), FFEOC(1,IMNO), MODE(IMNO), XC(2), YC(2))
            XC(2) = AMIN1(COLS(IMNO), AMAX1(1., XC(2)))
            YC(2) = AMIN1(ROWS(IMNO), AMAX1(1., YC(2)))
c
            CALL BCKWRD (REAL(IX2), REAL(IY2), COLS(IMNO), ROWS(IMNO),
     .           COEFF(1,IMNO), FFEOC(1,IMNO), MODE(IMNO), XC(3), YC(3))
            XC(3) = AMIN1(COLS(IMNO), AMAX1(1., XC(3)))
            YC(3) = AMIN1(ROWS(IMNO), AMAX1(1., YC(3)))
c
            CALL BCKWRD (REAL(IX1), REAL(IY2), COLS(IMNO), ROWS(IMNO),
     .           COEFF(1,IMNO), FFEOC(1,IMNO), MODE(IMNO), XC(4), YC(4))
            XC(4) = AMIN1(COLS(IMNO), AMAX1(1., XC(4)))
            YC(4) = AMIN1(ROWS(IMNO), AMAX1(1., YC(4)))
         ELSE
            XC(1) = 1.
            XC(2) = 1.
            XC(3) = COLS(IMNO)
            XC(4) = COLS(IMNO)
            YC(1) = 1.
            YC(2) = 1.
            YC(3) = ROWS(IMNO)
            YC(4) = ROWS(IMNO)
         END IF
c
         CALL OPNPIC (IMNO, 'R', .FALSE., IER)
         SKY(IMNO) = GETSKY(IMNO, HIBAD(IMNO), LOBAD(IMNO), MXOUTC, 
     .        XC, YC, WORK, IWORK)
         CALL CLPIC (IMNO, IER)
         IF (PHPADU(IMNO) .GT. 0.) THEN
            DX = READNS(IMNO)**2 + MAX(0., SKY(IMNO))/PHPADU(IMNO)
         ELSE
            DX = READNS(IMNO)**2
         END IF
         IF (DX .GT. 1.) WEIGHT(IMNO) = WEIGHT(IMNO)/DX
c     print*,'Z ',PICTUR(IMNO)(1:LENGTH(PICTUR(IMNO))),dx,
c    .     weight(imno)
         IF (WEIGHT(IMNO) .GT. WTMAX) WTMAX = WEIGHT(IMNO)
      END DO
      WTMAX = WTMAX/1000.
C
      DO IMNO=1,NFRAME
         WEIGHT(IMNO) = WEIGHT(IMNO)/WTMAX
         IF (NSUFF .GT. 0) PICTUR(IMNO) = 
     .        SWITCH(FILE(IMNO), SUFF(1:NSUFF))
         WRITE (6,10) NINT(SKY(IMNO)), SQRT(HOLD(IMNO)),
     .        WEIGHT(IMNO),
     .        PICTUR(IMNO)(1:LENGTH(PICTUR(IMNO)))
   10    FORMAT (' Sky =', I6, ', seeing =', f7.2, 
     .        ', weight =', F9.3, ' for ', A)
         DATUM(IMNO) = -WEIGHT(IMNO)
      END DO
C
C Sort the frames in order of decreasing weight.
C
      CALL QUICK (DATUM, NFRAME, IWHICH)
      IF (NAME .EQ. 'END-OF-FILE') CALL BYEBYE
C
      IDISP = NFRAME*(IXMAX-IXMIN+1)*IEXPND**2
      DX = 200 000./IDISP + 1.
      DX = ALOG10(DX)
      IDISP = INT(DX)
      DX = DX - IDISP
      IDISP = 10**IDISP
      IF (DX .GT. 0.7) THEN
         IDISP = 5*IDISP
      ELSE IF (DX .GT. 0.3) THEN
         IDISP = 2*IDISP
      END IF
C
C Copy first input frame into output frame.
C
      PICTUR(MAXPIC) = NAME
      IMGTYP(MAXPIC) = IMGTYP(1)
      DATTYP(MAXPIC) = 'REAL'
      NCOL(MAXPIC) = MCOL
      NROW(MAXPIC) = MROW
      COLS(MAXPIC) = REAL(MCOL)
      ROWS(MAXPIC) = REAL(MROW)
      CALL OPNPIC (1, 'R', .FALSE., IER)
      CALL CREPIC (1, MAXPIC, IER)
      CALL CLPIC (1, IER)
C
      DO I=1,30
         IF (NAME(I:I) .EQ. ':') THEN
            NAME = NAME(I+1:30)//' '
            GO TO 1990
         END IF
      END DO
 1990 I = LENGTH(NAME)
      STRNG1 = SQUEEZ(IEXPND, L)
      IF (NFRAME .GT. 1) THEN
         TEMP = SQUEEZ(NFRAME, J)
         IF (NUSE .LT. NFRAME) THEN
            SUFF = SQUEEZ(NUSE, K)
            WRITE (LINE,199) SUFF(1:K), TEMP(1:J), STRNG1(1:L)
  199       FORMAT ('Best ', A, ' of ', A, ' images, scale = ', A, 
     .           ' px/arcsec')
         ELSE
            WRITE (LINE,198) TEMP(1:J), STRNG1(1:L)
  198       FORMAT ('Montage of ', A, ' images, scale = ', A, 
     .           ' px/arcsec')
         END IF
      ELSE
         LINE = 'Scale = '//STRNG1(1:L)//' px/arcsec'
      END IF
      J = LENGTH(LINE)
      IF (IMGTYP(MAXPIC) .EQ. 'imh') THEN
         CALL SPUT (MAXPIC, 'title', LINE(1:J), ' ', IER)
      ELSE
         CALL SPUT (MAXPIC, 'OBJECT', LINE(1:J), ' ', IER)
      END IF
      CALL SPUT (MAXPIC, 'OFFSCALE', OFFSCA, ' ', IER)
C
      IPIX = -1073676289
      NOPEN = 0
C
C#######################################################################
C
C The remainder of this code exists in two copies: the first one for
C the case where most of the input images are aligned with the master
C list and the X-coordinate will vary most rapidly, the second for
C the case where most input images are rotated by +/- 90 degrees and
C the Y-coordinate will vary most rapidly.
C
      IF (ROTATE) GO TO 5000
      DO IMNO=1,NFRAME
         DX = ABS(COEFF(3,IMNO)*COEFF(6,IMNO))
         DY = ABS(COEFF(4,IMNO)*COEFF(5,IMNO))
         IF (DX .GT. 4.*DY) THEN
            ALIGN(IMNO) = .TRUE.
         ELSE
            ALIGN(IMNO) = .FALSE.
         END IF
      END DO
C
      ISTART = IYMIN
C
 1999 CALL TBLANK
      ZPT = (1.-IEXPND)/(2.*IEXPND)
      DEL = 1./REAL(IEXPND)
      DO 3000 IROW=ISTART,IYMAX
        DO 2975 JE=1,IEXPND
C
        JROW=IEXPND*(IROW-IYMIN)+JE
        IF ((IROW.LT.LYMIN) .OR. (IROW.GT.LYMAX)) THEN
           DO ICOL=IXMIN,IXMAX
              DO KE=1,IEXPND
                 JCOL=IEXPND*(ICOL-IXMIN)+KE
                 WORK(JCOL,JROW) = 1.1E19
              END DO
           END DO
           GO TO 2950
        END IF
        YYY = REAL(IROW) + ZPT + (JE-1.)*DEL
C
        DO 2900 ICOL=IXMIN,IXMAX
        DO 2900 KE=1,IEXPND
          IPIX = IPIX+1
          JCOL=IEXPND*(ICOL-IXMIN)+KE
C
          IF ((ICOL.LT.LXMIN) .OR. (ICOL.GT.LXMAX)) THEN
             WORK(JCOL,JROW) = 1.1E19
             GO TO 2900
          END IF
C
          XXX = REAL(ICOL) + ZPT + (KE-1.)*DEL
C
C IROW and ICOL are the coordinates of the pixel in the reference
C frame of the first image in the .MCH file.  JROW and JCOL are
C the coordinates of the pixel in the output frame.  XXX and YYY
C are the centroid of the subpixel.
C
          M = 0
          INSIDE = 0
          DO 2500 III=1,NFRAME
            IMNO = IWHICH(III)
            CALL BCKWRD (XXX, YYY, COLS(IMNO), ROWS(IMNO), 
     .           COEFF(1,IMNO), FFEOC(1,IMNO), MODE(IMNO), X, Y)
C
            I = NINT(X)
            J = NINT(Y)
            IF ((I .LT. 1) .OR. (I .GT. NCOL(IMNO))) GO TO 2500
            IF ((J .LT. 1) .OR. (J .GT. NROW(IMNO))) GO TO 2500
            CALL GTFM (X, Y, COLS(IMNO), ROWS(IMNO), COEFF(1,IMNO),
     .           MODE(IMNO), DX, DY)
            IF ((DX-XXX)**2+(DY-YYY)**2 .GT. 1.) GO TO 2500
            INSIDE = INSIDE+1
            LAST(IMNO) = IPIX
C
C If the required pixel is in the buffer, don't bother to check that
C the image is open.
C
            IF (J .EQ. LROW(IMNO)) THEN
               IF (ALIGN(IMNO)) THEN
                  GO TO 2498
               ELSE IF (I .EQ. LCOL(IMNO)) THEN
                  GO TO 2498
               END IF
            END IF
C
C The pixel is not in the buffer, so we may need to open the image.
C
            IF (IMID(IMNO) .EQ. 0) THEN
               IF (NOPEN .GE. MAXOPN) THEN
                  MIN = 1073676289
                  DO K=NFRAME,1,-1
                     IF (IMID(K) .NE. 0) THEN
                        IF (LAST(K) .LT. MIN) THEN
                           KOLD = K
                           MIN = LAST(K)
                        END IF
                     END IF
                  END DO
                  CALL CLPIC (KOLD, IER)
                  NOPEN = NOPEN-1
                  IER = 0
               END IF
 2495          CALL OPNPIC (IMNO, 'R', .FALSE., IER)
               IF (NCOL(IMNO) .LE. 0) THEN
                  WRITE (LINE,66) IMNO, PICTUR(IMNO)
   66             FORMAT (' Unable to open ', I3, 2X, A)
                  CALL STUPID (LINE(1:LENGTH(LINE)))
                  WRITE (6,*) ' Please fix the problem and hit <CR>.'
                  READ (5,*,END=3099)
                  GO TO 2495
               END IF
               NOPEN = NOPEN+1
            END IF
C
C If the image wasn't open before, it is now.
C
            IF (ALIGN(IMNO)) THEN
               IER = 0
               CALL RDROW (IMNO, J, ROW(1,IMNO), SHOW, IER)
               LROW(IMNO) = J
            ELSE
               ROW(I,IMNO) = RDPIXL (IMNO, I, J)
               LCOL(IMNO) = I
               LROW(IMNO) = J
            END IF
C
 2498       DD = ROW(I,IMNO)
            IF ((DD .GE. LOBAD(IMNO)) .AND.
     .           (DD .LE. HIBAD(IMNO))) THEN
               M = M+1
               DATUM(M)=(DD-SKY(IMNO))*DMAG(IMNO)
               WT(M) = WEIGHT(IMNO)
               IF (M .GE. NUSE) GO TO 2501
            END IF
 2500     CONTINUE
 2501     CONTINUE
          IF ((M .LT. 1) .OR. (M .GT. NMAX) .OR.
     .         (INSIDE .LT. NMIN)) THEN
             WORK(JCOL,JROW) = 1.1E19
C         ELSE IF (M .EQ. 0) THEN
C            WORK(JCOL,JROW) = 1.1E19
          ELSE IF (M .EQ. 1) THEN
             WORK(JCOL,JROW) = DATUM(1)
          ELSE
             CALL QUICK (DATUM, M, INDEX)
             CALL RECTFY (WT, M, INDEX, HOLD)
             HOLD(1) = 0.5*WT(1)
             m = m-1                         ! OMIT SINGLE HIGHEST VALUE
             DO I=2,M
                HOLD(I) = HOLD(I-1) + 0.5*(WT(I)+WT(I-1))
             END DO
             X = PCT*(HOLD(M)+0.5*WT(M))
             IF (X .LE. HOLD(1)) THEN
                WORK(JCOL,JROW) = DATUM(1)
             ELSE IF (X .GE. HOLD(M)) THEN
                WORK(JCOL,JROW) = DATUM(M)
             ELSE
                DO I=2,M
                   J = I-1
                   IF (HOLD(I) .GT. X) THEN
                      WORK(JCOL,JROW) = 
     .                     DATUM(J) + (DATUM(I)-DATUM(J))*
     .                     (X-HOLD(J))/(HOLD(I)-HOLD(J))
                      GO TO 2900
                   END IF
                END DO
             END IF
          END IF
 2900   CONTINUE
 2950   CONTINUE
 2975   CONTINUE
        IF (MOD(IROW,IDISP) .EQ. 0) THEN
           WRITE (LINE,62) IYMIN, IROW, IYMAX
   62      FORMAT (3I6)
           CALL OVRWRT (LINE(1:23), 2)
        END IF
 3000 CONTINUE
      GO TO 3099
C
C#######################################################################
C
 5000 CONTINUE
      DO IMNO=1,NFRAME
         DX = ABS(COEFF(3,IMNO)*COEFF(6,IMNO))
         DY = ABS(COEFF(4,IMNO)*COEFF(5,IMNO))
         IF (DX .GT. DY/4.) THEN
            ALIGN(IMNO) = .TRUE.
         ELSE
            ALIGN(IMNO) = .FALSE.
         END IF
      END DO
C
      ISTART = IXMIN
C
 5999 CALL TBLANK
      ZPT = (1.-IEXPND)/(2.*IEXPND)
      DEL = 1./REAL(IEXPND)
      DO 7000 ICOL=ISTART,IXMAX
        DO 6975 JE=1,IEXPND
C
        JCOL=IEXPND*(ICOL-IXMIN)+JE
        IF ((ICOL.LT.LXMIN) .OR. (ICOL.GT.LXMAX)) THEN
           DO IROW=IYMIN,IYMAX
              DO KE=1,IEXPND
                 JROW=IEXPND*(IROW-IYMIN)+KE
                 WORK(JCOL,JROW) = 1.1E19
              END DO
           END DO
           GO TO 6950
        END IF
        XXX = REAL(ICOL) + ZPT + (JE-1.)*DEL
C
        DO 6900 IROW=IYMIN,IYMAX
        DO 6900 KE=1,IEXPND
          IPIX = IPIX+1
          JROW=IEXPND*(IROW-IYMIN)+KE
C
          IF ((IROW.LT.LYMIN) .OR. (IROW.GT.LYMAX)) THEN
             WORK(JCOL,JROW) = 1.1E19
             GO TO 6900
          END IF
C
          YYY = REAL(IROW) + ZPT + (KE-1.)*DEL
C
C IROW and ICOL are the coordinates of the pixel in the reference
C frame of the first image in the .MCH file.  JROW and JCOL are
C the coordinates of the pixel in the output frame.  XXX and YYY
C are the centroid of the subpixel.
C
          M = 0
          INSIDE = 0
          DO 6500 III=1,NFRAME
            IMNO = IWHICH(III)
            CALL BCKWRD (XXX, YYY, COLS(IMNO), ROWS(IMNO), 
     .           COEFF(1,IMNO), FFEOC(1,IMNO), MODE(IMNO), X, Y)
C
            I=NINT(X)
            J=NINT(Y)
            IF ((I .LT. 1) .OR. (I .GT. NCOL(IMNO))) GO TO 6500
            IF ((J .LT. 1) .OR. (J .GT. NROW(IMNO))) GO TO 6500
            CALL GTFM (X, Y, COLS(IMNO), ROWS(IMNO), COEFF(1,IMNO),
     .           MODE(IMNO), DX, DY)
            IF ((DX-XXX)**2+(DY-YYY)**2 .GT. 1.) GO TO 6500
            INSIDE = INSIDE+1
            LAST(IMNO) = IPIX
C
C If the required pixel is in the buffer, don't bother to check that
C the image is open.
C
            IF (J .EQ. LROW(IMNO)) THEN
               IF (ALIGN(IMNO)) THEN
                  IF (I .EQ. LCOL(IMNO)) GO TO 6498
               ELSE 
                  GO TO 6498
               END IF
            END IF
C
C The pixel is not in the buffer, so we may need to open the image.
C
            IER = 0
            IF (IMID(IMNO) .EQ. 0) THEN
               IF (NOPEN .GE. MAXOPN) THEN
                  MIN = 1073676289
                  DO K=NFRAME,1,-1
                  IF (IMID(K) .NE. 0) THEN
                     IF (LAST(K) .LT. MIN) THEN
                        KOLD = K
                        MIN = LAST(K)
                     END IF
                  END IF
               END DO
               CALL CLPIC (KOLD, IER)
               IER = 0
               NOPEN = NOPEN-1
               END IF
 6495          CALL OPNPIC (IMNO, 'R', .FALSE., IER)
               IF (NCOL(IMNO) .LE. 0) THEN
                  WRITE (LINE,66) IMNO, PICTUR(IMNO)
                  CALL STUPID (LINE(1:LENGTH(LINE)))
                  WRITE (6,*) ' Please fix the problem and hit <CR>.'
                  READ (5,*,END=3099)
                  GO TO 6495
               END IF
               NOPEN = NOPEN+1
            END IF
C
C If the image wasn't open before, it is now.
C
            IF (ALIGN(IMNO)) THEN
               ROW(I,IMNO) = RDPIXL (IMNO, I, J)
               LCOL(IMNO) = I
               LROW(IMNO) = J
            ELSE
               IER = 0
               CALL RDROW (IMNO, J, ROW(1,IMNO), SHOW, IER)
               LROW(IMNO) = J
            END IF
C
 6498       DD = ROW(I,IMNO)
            IF ((DD .GE. LOBAD(IMNO)) .AND.
     .           (DD .LE. HIBAD(IMNO))) THEN
               M = M+1
               DATUM(M)=(DD-SKY(IMNO))*DMAG(IMNO)
               WT(M) = WEIGHT(IMNO)
               IF (M .GE. NUSE) GO TO 6501
            END IF
 6500     CONTINUE
C6501     IF ((M .LT. NMIN) .OR. (M .GT. NMAX)) THEN
 6501     CONTINUE
          IF ((M .LT. 1) .OR. (M .GT. NMAX) .OR.
     .         (INSIDE .LT. NMIN)) THEN
             WORK(JCOL,JROW) = 1.1E19
          ELSE IF (M .EQ. 1) THEN
             WORK(JCOL,JROW) = DATUM(1)
          ELSE
             CALL QUICK (DATUM, M, INDEX)
             CALL RECTFY (WT, M, INDEX, HOLD)
             HOLD(1) = 0.5*WT(1)
             m = m-1                         ! OMIT SINGLE HIGHEST VALUE
             DO I=2,M
                HOLD(I) = HOLD(I-1) + 0.5*(WT(I)+WT(I-1))
             END DO
             X = PCT*(HOLD(M)+0.5*WT(M))
             IF (X .LE. HOLD(1)) THEN
                WORK(JCOL,JROW) = DATUM(1)
             ELSE IF (X .GE. HOLD(M)) THEN
                WORK(JCOL,JROW) = DATUM(M)
             ELSE
                DO I=2,M
                   J = I-1
                   IF (HOLD(I) .GT. X) THEN
                      WORK(JCOL,JROW) = DATUM(J) + 
     .                     (DATUM(I)-DATUM(J))*
     .                     (X-HOLD(J))/(HOLD(I)-HOLD(J))
                      GO TO 6900
                   END IF
                END DO
             END IF
          END IF
 6900   CONTINUE
 6950   CONTINUE
 6975   CONTINUE
        IF (MOD(ICOL,IDISP) .EQ. 0) THEN
           WRITE (LINE,62) IXMIN, ICOL, IXMAX
           CALL OVRWRT (LINE(1:23), 2)
        END IF
 7000 CONTINUE
C
C#######################################################################
C
 3099 LY = 1
      MY = MROW
      CALL WRSECT (MAXPIC, LY, MY, WORK, MXOUTC, IER)
      CALL CLPIC (MAXPIC, IER)
 3100 DO IMNO=1,NFRAME
         IF (IMID(IMNO) .NE. 0) CALL CLPIC (IMNO, IER)
      END DO
      CALL OVRWRT ('                           ', 3)
      CALL BYEBYE
      END
C
C#############################################################################
C
      REAL FUNCTION  GETSKY  (IMNO, HIBAD, LOBAD, MAXSKY, XC, YC, 
     .     WORK, IWORK)
      INCLUDE 'arrays.inc'
      REAL XC(4), YC(4), WORK(*)
      INTEGER INDEX(4), IWORK(*)
C
      REAL HIBAD, LOBAD
      LOGICAL SHOW
      SHOW = .FALSE.
C
      MINSKY = 1000
C
C XC and YC are the corners of a (generally irregular) quadrilateral
C wherein the sky is to be determined.  Sort these into the vectors
C X and Y as follows:  
C
C     corner 1:  the corner with the smallest Y value
C     corner 2:  the corner of intermediate Y with the smaller X value
C     corner 3:  the corner of intermediate Y with the larger X value
C     corner 4:  the corner with the largest Y value
C
      CALL QUICK (YC, 4, INDEX)
      CALL QUICK (XC, 4, INDEX)
      ILO = MAX0(1, MIN0(NCOL(IMNO), NINT((XC(1)+XC(2))/2.)))
      IHI = MAX0(1, MIN0(NCOL(IMNO), NINT((XC(3)+XC(4))/2.)))
      K =   MAX0(1, MIN0(NROW(IMNO), NINT((YC(1)+YC(2))/2.)))
      KK =  MAX0(1, MIN0(NROW(IMNO), NINT((YC(3)+YC(4))/2.)))
      NX = IHI-ILO+1
      ISTEP = INT(SQRT( REAL((KK-K+1)*NX) / REAL(MAXSKY)))+1
 4000 CONTINUE
      NX = IHI-ILO+1
      I = 0
      N = 0
      DO IROW=K,KK,ISTEP
         Y = REAL(IROW)
         CALL RDROW (IMNO, IROW, WORK(I+1), SHOW, IER)
         IF (IER .NE. 0) THEN
            CALL STUPID (' Error reading input data.')
            WRITE (6,*) IMNO, IROW, I+1
            CALL OOPS
         END IF
         DO J=ILO,IHI,ISTEP
            L = N+J
            IF ((WORK(L) .GE. LOBAD) .AND. 
     .          (WORK(L) .LE. HIBAD)) THEN
               I = I+1
               WORK(I) = WORK(L)
            END IF
         END DO
         N = I
      END DO
C
      IF (N .LT. MINSKY) THEN
         ILO = MAX(1, ILO-1)
         IHI = MIN(NCOL(IMNO), IHI+1)
         K = MAX(1, K-1)
         KK = MIN(NROW(IMNO), KK+1)
         IF ((ILO .EQ. 1) .AND. (IHI .EQ. NCOL(IMNO)) .AND.
     .       (K .EQ. 1) .AND. (KK .EQ. NROW(IMNO))) THEN
            IF (ISTEP .GT. 1) THEN
               ISTEP = MAX(1,ISTEP-1)
            ELSE
               GO TO 4001
            END IF
         END IF
         GO TO 4000
      END IF
      R = REAL(N-1)/REAL(MAXSKY-1)
      IF (R .GT. 1.) THEN
         N = MAXSKY
         DO I=2,N
            J = NINT(R*REAL(I-1)) + 1
            WORK(I) = WORK(J)
         END DO
      END IF
 4001 CONTINUE
      CALL QUICK (WORK, N, IWORK(N+1))
      CALL MMM (WORK, N, HIBAD, 4., SKYMN, SKYMED, SKYMOD,
     .     SIGMA, SKEW)
      GETSKY = SKYMOD
      RETURN
      END
C
C###########################################################################
C
      SUBROUTINE ISORT (DATUM, N)
      PARAMETER (MAXSTAK=14)
C
C A quick-sorting algorithm suggested by the discussion on pages 114-119
C of THE ART OF COMPUTER PROGRAMMING, Vol. 3, SORTING AND SEARCHING, by
C D.E. Knuth, which was referenced in Don Wells' subroutine QUIK.  This
C is my own attempt at encoding a quicksort-- PBS.
C
C The array DATUM contains randomly ordered data. 
C
C The limiting stack length of 14 will limit this quicksort subroutine
C to vectors of maximum length of order 32,768 (= 2**15).
C
      INTEGER DATUM(N), STKLO(MAXSTAK), STKHI(MAXSTAK), HI
C
C Initialize the pointers.
C
      NSTAK=0
      LIMLO=1
      LIMHI=N
C
  100 DKEY=DATUM(LIMLO)
D     TYPE *,LIMLO,LIMHI
C
C Compare all elements in the sub-vector between LIMLO and LIMHI with
C the current key datum.
C
      LO=LIMLO
      HI=LIMHI
  101 CONTINUE
      IF (LO .EQ. HI) GO TO 200
      IF (DATUM(HI) .LE. DKEY) GO TO 109
      HI=HI-1
C
C The pointer HI is to be left pointing at a datum SMALLER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  109 DATUM(LO)=DATUM(HI)
      LO=LO+1
  110 CONTINUE
      IF (LO .EQ. HI) GO TO 200
      IF (DATUM(LO) .GE. DKEY) GO TO 119
      LO=LO+1
      GO TO 110
C
  119 DATUM(HI)=DATUM(LO)
      HI=HI-1
C
C The pointer LO is to be left pointing at a datum LARGER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  200 CONTINUE
C
C LO and HI are equal, and point at a value which is intended to
C be overwritten.  Since all values below this point are less than
C the key and all values above this point are greater than the key,
C this is where we stick the key back into the vector.
C
      DATUM(LO)=DKEY
D     DO 1666 I=LIMLO,LO-1
D1666 TYPE *,DATUM(I)
D     TYPE *,DATUM(L0),' KEY'
D     DO 2666 I=LO+1,LIMHI
D2666 TYPE *,DATUM(I)
C
C At this point in the subroutine, all data between LIMLO and LO-1, 
C inclusive, are less than DATUM(LO), and all data between LO+1 and 
C LIMHI are larger than DATUM(LO).
C
C If both subarrays contain no more than one element, then take the most
C recent interval from the stack (if the stack is empty, we're done).
C If the larger of the two subarrays contains more than one element, and
C if the shorter subarray contains one or no elements, then forget the 
C shorter one and reduce the other subarray.  If the shorter subarray
C contains two or more elements, then place the larger subarray on the
C stack and process the subarray.
C
      IF (LIMHI-LO .GT. LO-LIMLO) GO TO 300
C
C Case 1:  the lower subarray is longer.  If it contains one or no 
C elements then take the most recent interval from the stack and go 
C back and operate on it.
C
      IF (LO-LIMLO .LE. 1) GO TO 400
C
C If the upper (shorter) subinterval contains one or no elements, then
C process the lower (longer) one, but if the upper subinterval contains
C more than one element, then place the lower (longer) subinterval on
C the stack and process the upper one.
C
      IF (LIMHI-LO .GE. 2) GO TO 250
C
C Case 1a:  the upper (shorter) subinterval contains no or one elements,
C so we go back and operate on the lower (longer) subinterval.
C
      LIMHI=LO-1
      GO TO 100
C
  250 CONTINUE
C
C Case 1b:  the upper (shorter) subinterval contains at least two 
C elements, so we place the lower (longer) subinterval on the stack and
C then go back and operate on the upper subinterval.
C 
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LIMLO
      STKHI(NSTAK)=LO-1
      LIMLO=LO+1
D     DO 3666 I=1,NSTAK
D3666 TYPE *,' STACK: ',STKLO(I),STKHI(I)
      GO TO 100
C
  300 CONTINUE
C
C Case 2:  the upper subarray is longer.  If it contains one or no 
C elements then take the most recent interval from the stack and 
C operate on it.
C
      IF (LIMHI-LO .LE. 1) GO TO 400
C
C If the lower (shorter) subinterval contains one or no elements, then
C process the upper (longer) one, but if the lower subinterval contains
C more than one element, then place the upper (longer) subinterval on
C the stack and process the lower one.
C
      IF (LO-LIMLO .GE. 2) GO TO 350
C
C Case 2a:  the lower (shorter) subinterval contains no or one elements,
C so we go back and operate on the upper (longer) subinterval.
C
      LIMLO=LO+1
      GO TO 100
C
  350 CONTINUE
C
C Case 2b:  the lower (shorter) subinterval contains at least two 
C elements, so we place the upper (longer) subinterval on the stack and
C then go back and operate on the lower subinterval.
C 
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LO+1
      STKHI(NSTAK)=LIMHI
      LIMHI=LO-1
D     DO 4666 I=1,NSTAK
D4666 TYPE *,' STACK: ',STKLO(I),STKHI(I)
      GO TO 100
C
  400 CONTINUE
C
C Take the most recent interval from the stack.  If the stack happens 
C to be empty, we are done.
C
      IF (NSTAK .LE. 0) RETURN
      LIMLO=STKLO(NSTAK)
      LIMHI=STKHI(NSTAK)
      NSTAK=NSTAK-1
      GO TO 100
C
      END
C
C#######################################################################
C
C Invert the first six transformation constants.
C
      SUBROUTINE INVRT (COEFF, MODE, FFEOC)
      DOUBLE PRECISION COEFF(20), FFEOC(6), DENOM
      DENOM = COEFF(3)*COEFF(6)-COEFF(4)*COEFF(5)
      FFEOC(3) = COEFF(6)/DENOM
      FFEOC(4) = -COEFF(4)/DENOM
      FFEOC(5) = -COEFF(5)/DENOM
      FFEOC(6) = COEFF(3)/DENOM
      FFEOC(1) = (COEFF(2)*COEFF(5) - COEFF(1)*COEFF(6))/DENOM
      FFEOC(2) = (COEFF(1)*COEFF(4) - COEFF(2)*COEFF(3))/DENOM
      RETURN
      END
C
C#######################################################################
C
      SUBROUTINE  BCKWRD  (XX, YY, RCOL, RROW, COEFF, FFEOC, MODE, X, Y)
      DOUBLE PRECISION COEFF(20), FFEOC(6)
C
C Use the crude inverse constants to project the master-system position
C to an approximate position in this frame.
C
      N = 0
      OLDX = 0.
      OLDY = 0.
      OLDERX = 0.
      OLDERY = 0.
      X = FFEOC(1) + FFEOC(3)*XX + FFEOC(5)*YY
      Y = FFEOC(2) + FFEOC(4)*XX + FFEOC(6)*YY
 1000 N = N+1
C
C Now apply the forward transformation to this position to see how
C close we come to the starting point.
C
      CALL GTFM (X, Y, RCOL, RROW, COEFF, MODE, DX, DY)
      DX = XX - DX
      DY = YY - DY
      IF ((DX .EQ. OLDERX) .AND. (DY .EQ. OLDERY)) RETURN
      X = X + FFEOC(3)*DX + FFEOC(5)*DY
      Y = Y + FFEOC(4)*DX + FFEOC(6)*DY
      IF (DX**2+DY**2 .LT. 1.E-6) RETURN
      IF (X**2+Y**2 .GT. 1.E10) RETURN
      IF (N .GE. 200) THEN
         X = 1.1E10
         Y = 1.1E10
         RETURN
      END IF
      OLDERX = OLDX
      OLDERY = OLDY
      OLDX = DX
      OLDY = DY
      GO TO 1000
      END!
