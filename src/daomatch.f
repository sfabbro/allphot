C
C=======================================================================
C A program to cross-identify stars in two star-lists, by matching the
C shapes of triangles.
C
C                 OFFICIAL DAO VERSION:  2003 December 8
C
C=======================================================================
C
      PARAMETER (MAXSTR=36, MAXTRI=11480, NRRES=30, NZRES=30)
      PARAMETER (MAXMAT=50000)
C
      CHARACTER FILE*40, OFILE*40, SWITCH*40, EXTEND*40
      CHARACTER SYMB(MAXSTR)*3
      CHARACTER CASE*4, DOT*3, ANSWER*1
      DOUBLE PRECISION G(4,4), U(4), V(4), T(4), E(4), F(4), H(4)
      REAL OLD(4), CLAMP(4)
      REAL X(MAXSTR+10,2), Y(MAXSTR+10,2), MAG(MAXSTR+10,2)
      REAL R1(MAXTRI,2), R2(MAXTRI,2), R3(MAXTRI,2)
      REAL A(MAXMAT), B(MAXMAT), C(MAXMAT), D(MAXMAT)
      REAL WT(MAXMAT), TEMP(MAXMAT)
      REAL SUMWT(NZRES,NZRES,-NRRES:NRRES,NRRES,2)
      REAL WTMAX(2), LONG, MEDIUM, MULT, MAGMAX
      INTEGER IA(MAXMAT), IB(MAXMAT), IS(MAXMAT), IR(MAXMAT)
      INTEGER STAR(3,MAXMAT,2), WHICH(MAXMAT,2), IFLIP(MAXMAT)
      INTEGER MATCH(MAXSTR,MAXSTR)
      INTEGER MROW(MAXSTR), MCOL(MAXSTR)
      LOGICAL USE1(MAXMAT), USE2(MAXMAT)
      LOGICAL LIMIT, SKIP, SETSCA, SETROT
C
C STAR(i,j,k) gives the identity of star opposite the i-th longest arm 
C             in the j-th triangle for the k-th star list
C
C WHICH(i,j)   gives the number of the triangle for the j-th
C              starlist for the i-th match
C
C SIGMASQ governs how well the two triangles must agree before they
C are considered the same.  SIGMASQ represents the square of the 
C standard error of an individual stellar position.  FACT governs
C how many triangles a star must appear in to be considered a real
C match
C
      DATA FACTOR/18./, DELLOG/1/, DOT/'  -'/
      DATA TWOPI/6.283184/
      DATA XYLIM /8000./, SCTOL/0.01/, PSTOL/1./
      CALL FABORT
      ZRESM1 = REAL(NZRES)-1.00001
      RRESM1 = REAL(NRRES)-1.00001
      TXYLIM = 2.*XYLIM/NZRES
      RRES = TWOPI/NRRES
      SCTOL3 = 3.*SCTOL
      SCTOLS = SCTOL**2
      PSTOLS = PSTOL**2
      LIMIT = .FALSE.
C
C-----------------------------------------------------------------------
C
C Read in the first (standard) star list, retaining only the MAXSTR
C brightest stars.
C
      CALL TBLANK
      FILE=' '
 1019 CALL GETNAM ('Master input file:', FILE)
      IF ((FILE .EQ. 'END-OF-FILE') .OR. (FILE .EQ. 'EXIT')) CALL BYEBYE
      CHIMAX = 1.1E20
      MAGMAX = 1.1E20
      SHPMAX = 1.1E20
      SIGMAX = 1.1E20
      NMIN = -32768
      L = 0
      DO K=40,2,-1
         IF (FILE(K:K) .EQ. '%') THEN
            READ (FILE(K+1:40),*) CHIMAX
c           L = K
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. '<') THEN
            READ (FILE(K+1:40),*) MAGMAX
            L = K
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. '[') THEN
            READ (FILE(K+1:40),*) SHPMAX
            L = K
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. '|') THEN
            READ (FILE(K+1:40),*) SIGMAX
c           L = K
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. '/') THEN
            READ (FILE(K+1:40),*) SIGMASQ
            SIGMASQ = SIGMASQ**2
c           L = K
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. '*') THEN
            LIMIT = .TRUE.
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. '^') THEN
            READ (FILE(K+1:40),*) NMIN
            FILE(K:40) = ' '
         END IF
      END DO
c     IF (L .GT. 0) FILE(L:40) = ' '
C
      FILE = EXTEND(FILE, 'als')
      CALL INFILE (2, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening file '//FILE)
         FILE = 'EXIT'
         GO TO 1019
      END IF
      NL = -1
      CALL RDHEAD (2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AP1, 
     .     PHPADU, READNS, FRAD)
      IF (NL .LE. 0) NL=1
      J = NCOL*NROW
      IF (J .GT. 0) THEN
         RLIM = 3.E-3*J
      ELSE
         RLIM = 25.
      END IF
      CALL BRIGHT (NL, LIMIT, RLIM, CHIMAX, MAGMAX, SHPMAX, SIGMAX, 
     .     NMIN, MAXSTR+10, X(1,1), Y(1,1), MAG(1,1), N1, WT, TEMP, IA)
      N1 = MIN(N1, MAXSTR)
 1020 CALL CLFILE (2)
 1021 OFILE=SWITCH(FILE, CASE('.mch'))
 1022 CALL GETNAM ('Output file name:', OFILE)
      IF ((OFILE .EQ. 'END-OF-FILE') .OR.
     .     (OFILE .EQ. 'GIVE UP')) CALL BYEBYE
      CALL OUTFIL (1, OFILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening output file '//OFILE)
         OFILE = 'GIVE UP'
         GO TO 1022
      END IF
      IF (OFILE .NE. 'APPEND') THEN
         WRITE (1,101) FILE, 0., 0., 1., 0., 0., 1., 0., 0.
  101    FORMAT (1X, '''', A, '''', 2F10.2, 4F10.5, 2F10.3)
      END IF
C
C=======================================================================
C
C Form all possible triangles from the input file list.
C At this point A, B, C represent the first, second, and
C third arms.
C
      N = 0
      DO K=3,N1
         DO J=2,K-1
            DO I=1,J-1
               N = N+1
               AA = (X(I,1) + X(J,1) + X(K,1))/3.
               BB = (Y(I,1) + Y(J,1) + Y(K,1))/3.
               ARM1 = SQRT((X(I,1) - AA)**2 + (Y(I,1) - BB)**2)
               ARM2 = SQRT((X(J,1) - AA)**2 + (Y(J,1) - BB)**2)
               ARM3 = SQRT((X(K,1) - AA)**2 + (Y(K,1) - BB)**2)
               IF (ARM1 .GT. ARM2) THEN
                  IF (ARM2 .GT. ARM3) THEN
                     STAR(1,N,1) = I
                     STAR(2,N,1) = J
                     STAR(3,N,1) = K
                     LONG = ARM1
                     MEDIUM = ARM2
                     SHORT = ARM3
                  ELSE IF (ARM1 .GT. ARM3) THEN
                     STAR(1,N,1) = I
                     STAR(2,N,1) = K
                     STAR(3,N,1) = J
                     LONG = ARM1
                     MEDIUM = ARM3
                     SHORT = ARM2
                  ELSE
                     STAR(1,N,1) = K
                     STAR(2,N,1) = I
                     STAR(3,N,1) = J
                     LONG = ARM3
                     MEDIUM = ARM1
                     SHORT = ARM2
                  END IF
               ELSE
                  IF (ARM1 .GT. ARM3) THEN
                     STAR(1,N,1) = J
                     STAR(2,N,1) = I
                     STAR(3,N,1) = K
                     LONG = ARM2
                     MEDIUM = ARM1
                     SHORT = ARM3
                  ELSE IF (ARM2 .GT. ARM3) THEN
                     STAR(1,N,1) = J
                     STAR(2,N,1) = K
                     STAR(3,N,1) = I
                     LONG = ARM2
                     MEDIUM = ARM3
                     SHORT = ARM1
                  ELSE
                     STAR(1,N,1) = K
                     STAR(2,N,1) = J
                     STAR(3,N,1) = I
                     LONG = ARM3
                     MEDIUM = ARM2
                     SHORT = ARM1
                  END IF
               END IF
               R1(N,1) = MEDIUM/LONG
               R2(N,1) = SHORT/LONG
               R3(N,1) = LONG
            END DO
         END DO
      END DO
C
C=======================================================================
C
      SETSCA = .FALSE.
      SETROT = .FALSE.
      SCALE = 1.
      FILE=' '
 1000 CALL GETNAM ('Next input file:', FILE)
      IF ((FILE .EQ. 'EXIT') .OR. (FILE .EQ. 'END-OF-FILE')) THEN
         CALL CLFILE (1)
         CALL BYEBYE
      END IF
C
      NMIN = -32768
      DO K=29,2,-1
         IF (FILE(K:K) .EQ. '/') THEN
            IF (FILE(K+1:40) .EQ. ' ') THEN
               SCALE = 1.
            ELSE
               READ (FILE(K+1:40),*) SCALE
            END IF
            SETSCA = .TRUE.
            SETROT = .FALSE.
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. ';') THEN
            SCALE = 1.
            SETSCA = .TRUE.
            SETROT = .TRUE.
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. '?') THEN
            SETSCA = .FALSE.
            SETROT = .FALSE.
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. '*') THEN
            LIMIT = .TRUE.
            FILE(K:K) = ' '
         ELSE IF (FILE(K:K) .EQ. '^') THEN
            READ (FILE(K+1:40),*) NMIN
            FILE(K:40) = ' '
         ELSE IF (FILE(K:K) .EQ. '!') THEN
            LIMIT = .FALSE.
            FILE(K:K) = ' '
         END IF
      END DO
C
 1100 IF (SETSCA) THEN
         ELIM = 15.
         IF (SETROT) THEN
            WTMIN = 0.5
            MULT = 4.
         ELSE
            WTMIN = 1.
            MULT = 2.
         END IF
      ELSE
         ELIM = 10.
         WTMIN = 2.
         MULT = 1.5
      END IF
C
      FILE = EXTEND(FILE, 'als')
      CALL INFILE (2, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//FILE)
         FILE = 'EXIT'
         GO TO 1000
      END IF
      NL = -1
      CALL RDHEAD (2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AP1, 
     .     PHPADU, READNS, FRAD)
      IF (NL .LE. 0) NL=1
      CALL BRIGHT (NL, LIMIT, RLIM/SCALE, CHIMAX, MAGMAX, SHPMAX, 
     .     SIGMAX, NMIN,
     .     MAXSTR+10, X(1,2), Y(1,2), MAG(1,2), N2, WT, TEMP, IA)
      N2 = MIN(N2, MAXSTR)
      DO I=1,N2
         X(I,2) = SCALE*X(I,2)
         Y(I,2) = SCALE*Y(I,2)
      END DO
      CALL CLFILE (2)
C
C Special case if SETROT is .TRUE. and N1 or N2 is less than 3.
C
      IF (SETROT .AND. ((N1 .LT. 3) .OR. (N2 .LT. 3))) GO TO 8000
C
C=======================================================================
C
      DO L=1,2
         WTMAX(L) = 0.
         DO II=1,NRRES
            DO K=-NRRES,NRRES
               DO J=1,NZRES
                  DO I=1,NZRES
                     SUMWT(I,J,K,II,L) = 0.
                  END DO
               END DO
            END DO
         END DO
      END DO
C
      NNEW = 0
      M = 0
      NN = 0
      KK = 2
 2000 KK = KK+1
      IF ((KK .GT. N1) .OR. (KK .GT. N2)) THEN
         if (setrot) go to 8000
         CALL TBLANK
         CALL GETYN (CHAR(7)//'Write this transformation?', ANSWER)
         IF (ANSWER .EQ. 'Y') GO TO 9000
         FILE='EXIT'
         GO TO 1000
      END IF
C
C Form all possible triangles from the input file list.
C Compute the distance of each star from the centroid of the triangle.
C Sort these distances.  Star 1 is the most distant, star 3 the least
C distant.
C
      DO J=2,KK-1
         DO I=1,J-1
            NN = NN+1
            AA = (X(I,2) + X(J,2) + X(KK,2))/3.
            BB = (Y(I,2) + Y(J,2) + Y(KK,2))/3.
            ARM1 = SQRT((X(I,2) - AA)**2 + (Y(I,2) - BB)**2)
            ARM2 = SQRT((X(J,2) - AA)**2 + (Y(J,2) - BB)**2)
            ARM3 = SQRT((X(KK,2) - AA)**2 + (Y(KK,2) - BB)**2)
            IF (ARM1 .GT. ARM2) THEN
               IF (ARM2 .GT. ARM3) THEN
                  STAR(1,NN,2) = I
                  STAR(2,NN,2) = J
                  STAR(3,NN,2) = KK
                  LONG = ARM1
                  MEDIUM = ARM2
                  SHORT = ARM3
               ELSE IF (ARM1 .GT. ARM3) THEN
                  STAR(1,NN,2) = I
                  STAR(2,NN,2) = KK
                  STAR(3,NN,2) = J
                  LONG = ARM1
                  MEDIUM = ARM3
                  SHORT = ARM2
               ELSE
                  STAR(1,NN,2) = KK
                  STAR(2,NN,2) = I
                  STAR(3,NN,2) = J
                  LONG = ARM3
                  MEDIUM = ARM1
                  SHORT = ARM2
               END IF
            ELSE
               IF (ARM1 .GT. ARM3) THEN
                  STAR(1,NN,2) = J
                  STAR(2,NN,2) = I
                  STAR(3,NN,2) = KK
                  LONG = ARM2
                  MEDIUM = ARM1
                  SHORT = ARM3
               ELSE IF (ARM2 .GT. ARM3) THEN
                  STAR(1,NN,2) = J
                  STAR(2,NN,2) = KK
                  STAR(3,NN,2) = I
                  LONG = ARM2
                  MEDIUM = ARM3
                  SHORT = ARM1
               ELSE
                  STAR(1,NN,2) = KK
                  STAR(2,NN,2) = J
                  STAR(3,NN,2) = I
                  LONG = ARM3
                  MEDIUM = ARM2
                  SHORT = ARM1
               END IF
            END IF
            R1(NN,2) = MEDIUM/LONG
            R2(NN,2) = SHORT/LONG
            R3(NN,2) = LONG
         END DO
      END DO
C
C=======================================================================
C
C Compare the new triangles.
C
      NOLD = NNEW
      NNEW = KK*(KK-1)*(KK-2)/6
      T(1) = 1.
      SKIP = .TRUE.
      DO J=1,NNEW
         R3S = R3(J,1)**2
         DO 2900 I=1,NNEW
            IF ((I .LE. NOLD) .AND. (J .LE. NOLD)) GO TO 2900
C
C R1 = MEDIUM/LONG
C R2 = SHORT/LONG
C R3 = LONG
C
            DA = R1(I,1) - R1(J,2)
            DB = R2(I,1) - R2(J,2)
C
C Assume a centering uncertainty of PSTOL pixels for each image, on the 
C scale of image 1, and a relative scale uncertainty of SCTOL.
C
            R = (DA**2 + DB**2) / (PSTOLS*(1./R3S + 1./R3(I,2)**2))
            IF (SETSCA) THEN
               DC = R3(I,1) / R3(J,2) - 1.
               R = R + DC**2/SCTOLS
            END IF
            IF (R .LT. ELIM) THEN
C
C Perform four-constant least-squares for these three stars.
C
               DO L=1,3
                  U(L) = 0.
                  V(L) = 0.
                  DO II=1,L
                     G(II,L) = 0.
                  END DO
               END DO
C
               DO K=1,3
                  L = STAR(K,J,2)
                  T(2) = X(L,2) 
                  T(3) = Y(L,2)
                  L = STAR(K,I,1)
                  X1 = X(L,1)
                  Y1 = Y(L,1)
                  U(1) = U(1) + X1
                  V(1) = V(1) + Y1
                  G(1,2) = G(1,2) + T(2)
                  G(1,3) = G(1,3) + T(3)
                  DO L=2,3
                     U(L) = U(L) + X1*T(L)
                     V(L) = V(L) + Y1*T(L)
                     IF (L .GT. 1) THEN
                        DO II=2,L
                           G(II,L) = G(II,L) + T(II)*T(L)
                        END DO
                     END IF
                  END DO
               END DO
               G(1,1) = 3.
               G(2,1) = G(1,2)
               G(3,1) = G(1,3)
               G(3,2) = G(2,3)
C
               CALL DINVRS (G, 4, 3, ISTAT)
               CALL DVMUL (G, 4, 3, U, E)
               CALL DVMUL (G, 4, 3, V, F)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C S is a measure of flip.
C Q is the scale in X, squared.
C P is the scale in Y, squared.
C
               S = E(2)*F(3) - E(3)*F(2)
               Q = E(2)**2 + E(3)**2
               IF (SETSCA .AND. (ABS(1.-Q) .GT. SCTOL3)) GO TO 2900
               P = F(2)**2 + F(3)**2
               IF (SETSCA .AND. (ABS(1.-P) .GT. SCTOL3)) GO TO 2900
               IF (ABS(1.-P/Q) .GT. SCTOL3) GO TO 2900
               IF (SETROT .AND. (ABS(E(3)) .GT. SCTOL3)) GO TO 2900
               IF (SETROT .AND. (ABS(F(2)) .GT. SCTOL3)) GO TO 2900
               IF (S .EQ. 0.) THEN
                  GO TO 2900
               ELSE IF (S .GT. 0.) THEN
                  K = 1
               ELSE 
                  K = 2
               END IF
               S = 0.5*(Q+P)
C
C K is the flip state.  S is the average scale, squared.
C
               M = M+1
               WHICH(M,1) = I
               WHICH(M,2) = J
               WT(M) = 1. / (1. + R**2)
               IFLIP(M) = K
               A(M) = E(1)
               B(M) = F(1)
               IF (K .LE. 1) THEN
                  C(M) = 0.5*(E(2)+F(3))
                  D(M) = 0.5*(F(2)-E(3))
               ELSE
                  C(M) = 0.5*(E(2)-F(3))
                  D(M) = 0.5*(F(2)+E(3))
               END IF
               P = MAX(0., MIN(ZRESM1, (A(M)+XYLIM)/TXYLIM)) + 0.99995
               Q = MAX(0., MIN(ZRESM1, (B(M)+XYLIM)/TXYLIM)) + 0.99995
               S = MAX(-RRESM1, MIN(RRESM1, ALOG10(S)/RRES))
               R = MAX(0., MIN(RRESM1, AT360(C(M), D(M))/RRES)) 
     .              + 0.99995
               IA(M) = MAX (1, NINT(P-0.49996))
               IB(M) = MAX (1, NINT(Q-0.49996))
               IS(M) = NINT(S)
               IR(M) = MAX (1, NINT(R-0.49996))
C
C Now add this match into the four nearest cells of the match table.
C
               DO MM=IR(M),IR(M)+1
                 N = MM
                 IF (N .GT. NRRES) N = 1
                 DO LL=IS(M),IS(M)+1
                   DO JJ=IB(M),IB(M)+1
                     DO II=IA(M),IA(M)+1
                       SUMWT(II,JJ,LL,N,K) = SUMWT(II,JJ,LL,N,K)+
     .                      (1.-ABS(II-P))*(1.-ABS(JJ-Q))*
C    .                      WT(M)*(1.-ABS(II-P))*(1.-ABS(JJ-Q))*
     .                      (1.-ABS(LL-S))*(1.-ABS(MM-R))
                       IF (SUMWT(II,JJ,LL,N,K) .GT. WTMAX(K)) THEN
                         WTMAX(K) = SUMWT(II,JJ,LL,N,K)
                         IF (WTMAX(K) .GT. WTMAX(3-K)) THEN
                           JA = II
                           JB = JJ
                           JS = LL
                           JR = N
                           JF = K
                         END IF
                       END IF
                     END DO
                   END DO
                 END DO
               END DO
               SKIP = .FALSE.
            END IF
            IF (M .EQ. MAXMAT) THEN
               CALL GETYN (CHAR(7)//'Write this transformation?', 
     .              ANSWER)
               IF (ANSWER .EQ. 'Y') GO TO 9000
               FILE='EXIT'
               GO TO 1000
            END IF
 2900    CONTINUE
      END DO
      IF (SKIP) GO TO 2000
      IF (WTMAX(1) .EQ. WTMAX(2)) GO TO 2000
C
C  M  is the number of matching triangles.
C
C=======================================================================
C
C Choose the better flip.
C
C Cross-map the star ID's
C
      DO J=1,KK
         DO I=1,KK
            MATCH(I,J) = 0
         END DO
      END DO
C
      JJ = 0
      DO 3000 J=1,M
         USE1(J) = .FALSE.
         IF (IFLIP(J) .NE. JF) GO TO 3000
         IF (ABS(IA(J)-JA) .GT. 1) GO TO 3000
         IF (ABS(IB(J)-JB) .GT. 1) GO TO 3000
         IF (ABS(IS(J)-JS) .GT. 1) GO TO 3000
         L = ABS(IR(J) - JR)
         IF ((L .GT. 1) .AND. (L .LT. 19)) GO TO 3000
            USE1(J) = .TRUE.
            JJ = JJ+1
            DO I=1,3
               L = STAR(I,WHICH(J,1),1)
               K = STAR(I,WHICH(J,2),2)
               MATCH(K,L) = MATCH(K,L) + 1
            END DO
 3000 CONTINUE
C
C Eliminate any candidate match where a star has better matches
C elsewhere.
C
      DO 3100 J=1,M
         USE2(J) = .FALSE.
         IF (USE1(J)) THEN
            USE2(J) = .TRUE.
            DO I=1,3
               L = STAR(I,WHICH(J,1),1)
               K = STAR(I,WHICH(J,2),2)
					DO II=1,KK
                  IF ((II .NE. L) .AND. 
     .                 (MATCH(K,II) .GE. MATCH(K,L))) THEN
                     USE2(J) = .FALSE.
                     GO TO 3100
                  END IF
                  IF ((II .NE. K) .AND. 
     .                 (MATCH(II,L) .GE. MATCH(K,L))) THEN
                     USE2(J) = .FALSE.
                     GO TO 3100
                  END IF
               END DO
            END DO
         END IF
 3100 CONTINUE
C
      II = 0
      DO J=1,M
         IF (USE1(J)) THEN
            IF (USE2(J)) THEN
               II = II+1
            ELSE
               DO I=1,3
                  L = STAR(I,WHICH(J,1),1)
                  K = STAR(I,WHICH(J,2),2)
                  MATCH(K,L) = MATCH(K,L) - 1
               END DO
            END IF
         END IF
      END DO
      IF (II .LE. 0) GO TO 2000
C
      CALL TABLE (MATCH, SYMB, MAXSTR, KK)
C
C Sum the maxima in each row/column.
C
      NTOT = 0
      DO I=1,KK
         MCOL(I) = 0
      END DO
      DO J=1,KK
         MROW(J) = 0
         DO I=1,KK
            IF (MATCH(I,J) .GT. MCOL(I)) MCOL(I) = MATCH(I,J)
            IF (MATCH(I,J) .GT. MROW(J)) MROW(J) = MATCH(I,J)
         END DO
      END DO
      DO J=1,KK
         DO I=1,KK
            IF ((MATCH(I,J) .EQ. MCOL(I)) .AND. 
     .           (MATCH(I,J) .EQ. MROW(J))) THEN
               NTOT = NTOT+MATCH(I,J)
            END IF
         END DO
      END DO
c     accept*
C
C Calculate median values of A, B, C, D.
C
      K = 0
      DO J=1,M
         IF (USE2(J)) THEN
            K = K+1
            TEMP(K) = A(J)
         END IF
      END DO
      E(1) = 0.5*(PCTILE(TEMP, K, (K+1)/2) +
     .            PCTILE(TEMP, K, (K/2)+1))
C
      K = 0
      DO J=1,M
         IF (USE2(J)) THEN
            K = K+1
            TEMP(K) = B(J)
         END IF
      END DO
      E(2) = 0.5*(PCTILE(TEMP, K, (K+1)/2) +
     .            PCTILE(TEMP, K, (K/2)+1))
C
      K = 0
      DO J=1,M
         IF (USE2(J)) THEN
            K = K+1
            TEMP(K) = C(J)
         END IF
      END DO
      E(3) = 0.5*(PCTILE(TEMP, K, (K+1)/2) +
     .            PCTILE(TEMP, K, (K/2)+1))
C
      K = 0
      DO J=1,M
         IF (USE2(J)) THEN
            K = K+1
            TEMP(K) = D(J)
         END IF
      END DO
      E(4) = 0.5*(PCTILE(TEMP, K, (K+1)/2) +
     .            PCTILE(TEMP, K, (K/2)+1))
C
C-----------------------------------------------------------------------
C
C Using the matching triangles consistent with the best
C flip, determine the transformations.
C
      SZ = 5.
      DO J=1,4
         OLD(J) = 0.
         CLAMP(J) = 1.
      END DO
C
 7000 DO J=1,4
         V(J) = 0.
         DO I=1,4
            G(I,J) = 0.
         END DO
      END DO
      XITER = XITER + 0.1
      SUMZ = 0.
      SUMW = 0.
C
      N = 0
      DO J=1,M
         IF (USE2(J)) THEN
            N = N+1
            IF (IFLIP(J) .LE. 1) THEN
               DO I=1,3
                  L = STAR(I,WHICH(J,1),1)
                  K = STAR(I,WHICH(J,2),2)
                  T(1) = 1.
                  T(2) = 0.
                  T(3) = X(K,2)
                  T(4) = -Y(K,2)
                  Q = X(L,1) - E(1) - E(3)*T(3) - E(4)*T(4)
                  R = Y(L,1) - E(2) - E(4)*T(3) + E(3)*T(4)
                  W = 4./(4. + (Q**2 + R**2)/SZ**2)
                  SUMZ = SUMZ + W*(ABS(Q) + ABS(R))
                  SUMW = SUMW + W
                  DO JJ=1,4
                     V(JJ) = V(JJ) + W*T(JJ)*Q
                     DO II=1,4
                        G(II,JJ) = G(II,JJ) + W*T(II)*T(JJ)
                     END DO
                  END DO
                  T(1) = 0.
                  T(2) = 1.
                  T(3) = -T(4)
                  T(4) = X(K,2)
                  DO JJ=1,4
                     V(JJ) = V(JJ) + W*T(JJ)*R
                     DO II=1,4
                        G(II,JJ) = G(II,JJ) + W*T(II)*T(JJ)
                     END DO
                  END DO
               END DO
            ELSE
               DO I=1,3
                  L = STAR(I,WHICH(J,1),1)
                  K = STAR(I,WHICH(J,2),2)
                  T(1) = 1.
                  T(2) = 0.
                  T(3) = X(K,2)
                  T(4) = Y(K,2)
                  Q = X(L,1) - E(1) - E(3)*T(3) - E(4)*T(4)
                  R = Y(L,1) - E(2) - E(4)*T(3) + E(3)*T(4)
                  W = 4./(4. + (Q**2 + R**2)/SZ**2)
                  SUMZ = SUMZ + W*(ABS(Q) + ABS(R))
                  SUMW = SUMW + W
                  DO JJ=1,4
                     V(JJ) = V(JJ) + W*T(JJ)*Q
                     DO II=1,4
                        G(II,JJ) = G(II,JJ) + W*T(II)*T(JJ)
                     END DO
                  END DO
                  T(1) = 0.
                  T(2) = 1.
                  T(3) = -T(4)
                  T(4) = X(K,2)
                  DO JJ=1,4
                     V(JJ) = V(JJ) + W*T(JJ)*R
                     DO II=1,4
                        G(II,JJ) = G(II,JJ) + W*T(II)*T(JJ)
                     END DO
                  END DO
               END DO
            END IF
         END IF
      END DO
      CALL DINVRS (G, 4, 4, ISTAT)
      CALL DVMUL (G, 4, 4, V, F)
      DO II=1,4
         IF (F(II)*OLD(II) .LT. 0.) CLAMP(II) = 0.7*CLAMP(II)
         E(II) = E(II) + F(II)*CLAMP(II)
         OLD(II) = F(II)
      END DO
      SZ = 0.626657*SUMZ/SUMW
      IF (ABS(SZ-OLDA) .GT. 1.E-4) THEN
         OLDA = SZ
         SZ = SQRT(0.01 + SZ**2)
         GO TO 7000
      END IF
C
      H(1) = E(1)
      H(2) = E(2)
      H(3) = SCALE*E(3)
      H(4) = SCALE*E(4)
C
      N = 0
      DO J=1,M
         IF (USE2(J)) THEN
            DO I=1,3
               N = N+1
               L = STAR(I,WHICH(J,1),1)
               K = STAR(I,WHICH(J,2),2)
               TEMP(N) = MAG(L,1) - MAG(K,2)
            END DO
         END IF
      END DO
C
      Q = 0.5*(PCTILE(TEMP, N, (N+1)/2) +
     .          PCTILE(TEMP, N, (N/2)+1))
C
      CALL TBLANK
      IF (JF .LE. 1) THEN
         WRITE (6,601) FILE, NINT(H(1)), NINT(H(2)), 
     .        H(3), H(4), -H(4), H(3), Q
  601    FORMAT (1X, A, 2I6, 4F8.3, F7.2)
      ELSE
         WRITE (6,601) FILE, NINT(H(1)), NINT(H(2)), 
     .        H(3), H(4), H(4), -H(3), Q
      END IF
      CALL TBLANK
C
      JJ = 3-JF
      SZ = SZ / SQRT(3.*(N-1))
      write(6,6)N,WTMAX(JF), WTMAX(JJ),
     .     WTMAX(JJ)/(WTMAX(JF)+WTMAX(JJ)), SZ, sz/sqrt(sumw)
    6 FORMAT (I10, 5F9.3)
      write(6,6),j,wtmin,wtmin,0.4,0.5
C
      IF (N .LE. 1) THEN
         write(6,*)'N', N, '  <  1'
         GO TO 2000
      END IF
C
c     IF (WTMAX(JF)-WTMAX(JJ) .LT. WTMIN) THEN
c        write(6,*)'WT',WTMAX(JF)-WTMAX(JJ),'  <  ', WTMIN
      IF (WTMAX(JF) .LT. WTMIN) THEN
         write(6,*)'WT',WTMAX(JF),'  <  ', WTMIN
         GO TO 2000
      END IF
C
      IF (WTMAX(JJ)/(WTMAX(JF)+WTMAX(JJ)) .GT. 0.46) THEN
         write(6,*)'%W',WTMAX(JJ)/(WTMAX(JF)+WTMAX(JJ)),
     .        '  >  0.46'
         GO TO 2000
      END IF
C
      IF (SZ .GT. 0.5) THEN
         write(6,*)'SZ', SZ, '  >  0.5'
         GO TO 2000
      END IF
C
      NTOT = NINT(NTOT*MULT)
      IF (NTOT .LT. KK) THEN
         write(6,*) NTOT, ' < ', kk
         GO TO 2000
      END IF
      write(6,*) NTOT, ' > ', kk
      CALL TBLANK
      GO TO 9000
CC----------------------------------------------------------------------
C
C Special case:  SETROT is .TRUE. and unable to make a match.
C
 8000 DO J=1,NZRES
         DO I=1,NZRES
            SUMWT(I,J,1,1,1) = 0
         END DO
      END DO
C
      P = 1.E20
      Q = -1.E20
      R = 1.E20
      S = -1.E20
      DO J=1,N2
         DO I=1,N1
            DA = X(I,1) - X(J,2)
            DB = Y(I,1) - Y(J,2)
            IF (DA .LT. P) P = DA
            IF (DA .GT. Q) Q = DA
            IF (DB .LT. R) R = DB
            IF (DB .GT. S) S = DB
         END DO
      END DO
C
      Q = REAL(NZRES-1)/(Q-P)
      S = REAL(NZRES-1)/(S-R)
      W = 0.
      DO J=1,N2
         DO I=1,N1
            DA = X(I,1) - X(J,2)
            DB = Y(I,1) - Y(J,2)
            K = NINT(Q*(DA-P)) + 1
            L = NINT(S*(DB-R)) + 1
            SUMWT(K,L,1,1,1) = SUMWT(K,L,1,1,1)
     .           + (N1+1-I) + (N2+1-J)
            IF (SUMWT(K,L,1,1,1) .GT. W) THEN
               W = SUMWT(K,L,1,1,1)
               KK = K
               LL = L
            END IF
         END DO
      END DO
C
      SUMZ = 0.
      SUMW = 0.
      Q = 0.
      W = 0.
      DO J=1,N2
         DO I=1,N1
            DA = X(I,1) - X(J,2)
            DB = Y(I,1) - Y(J,2)
            K = NINT(Q*(DA-P)) + 1
            L = NINT(S*(DB-R)) + 1
            IF ((K .EQ. KK) .AND. (L .EQ. LL)) THEN
               WRITE (6,602) X(I,1), Y(I,1), MAG(I,1),
     .              X(J,2), Y(J,2), MAG(J,2)
  602          FORMAT (2(2X, 3F9.2))
               SUMZ = SUMZ + DA
               SUMW = SUMW + DB
               Q = Q + MAG(I,1) - MAG(J,2)
               W = W + 1.
            END IF
         END DO
      END DO
      IF (W .GT. 0.5) THEN
         CALL GETYN (CHAR(7)//'Write this transformation?', ANSWER)
         IF (ANSWER .EQ. 'Y') THEN
            H(1) = SUMZ/W
            H(2) = SUMW/W
            H(3) = 1.
            H(4) = 0.
            JF = -1.
            Q = Q/W
            GO TO 9000
         END IF
      END IF
      FILE = 'EXIT'
      GO TO 1000
C
 9000 CONTINUE
      IF (JF .LE. 1) THEN
         WRITE (1,101) FILE, H(1), H(2), H(3), H(4), -H(4), H(3), Q,
     .     0.
      ELSE
         WRITE (1,101) FILE, H(1), H(2), H(3), H(4), H(4), -H(3), Q,
     .     0.
      END IF
      CALL TBLANK
      FILE='EXIT'
      GO TO 1000
      END!
C
C#######################################################################
C
      SUBROUTINE  TABLE  (MATCH, SYMB, MAXSTR, K)
      IMPLICIT NONE
      INTEGER MAXSTR
      CHARACTER*3 SYMB(*), DOT
      CHARACTER TEXT*8
      INTEGER MATCH(MAXSTR,*)
C
      INTEGER I, J, K
C
      DATA DOT/'  -'/
C
      WRITE (6,6660) (J, J=1,K)
 6660 FORMAT (/15X, 'Frame 2:' / 8X, 'star ', 40I3)
      WRITE (6,6669) ('---', I=1,K)
 6669 FORMAT (12X, '+', 40A3)
      TEXT = 'Frame 1:'
      DO J=1,K
         DO I=1,K
            IF (MATCH(I,J) .EQ. 0) THEN
               SYMB(I) = DOT
            ELSE
               WRITE (SYMB(I),61) MATCH(I,J)
   61          FORMAT (I3)
            END IF
         END DO
         WRITE (6,6661) TEXT, J, (SYMB(I), I=1,K)
 6661    FORMAT (1X, A8, I2, ' |', 40A3)
         TEXT = '        '
      END DO
      CALL TBLANK
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE BRIGHT (NL, LIMIT, RLIM, CHIMAX, MAGMAX, SHPMAX, 
     .     SIGMAX, NMIN, MAX, X, Y, M, N, G, HOLD, INDEX)
      CHARACTER LINE*132
      REAL X(MAX), Y(MAX), M(MAX), G(MAX), HOLD(MAX), MM
      REAL DATA(2), MAGMAX
      INTEGER INDEX(MAX)
      LOGICAL LIMIT, SORT
C
C-----------------------------------------------------------------------
C
      IF (LIMIT) THEN
         CALL GETDAT ('X limits:', DATA, 2)
         IF (DATA(1) .LT. -1.E15) THEN
            DATA(2) = 1.E15
            CALL TBLANK
         END IF
         XMIN = AMIN1(DATA(1), DATA(2))
         XMAX = AMAX1(DATA(1), DATA(2))
         CALL GETDAT ('Y limits:', DATA, 2)
         IF (DATA(1) .LT. -1.E15) THEN
            DATA(2) = 1.E15
            CALL TBLANK
         END IF
         YMIN = AMIN1(DATA(1), DATA(2))
         YMAX = AMAX1(DATA(1), DATA(2))
         R = 3.E-3*((XMAX-XMIN)**2 + (YMAX-YMIN)**2)
         IF (R .LT. RLIM) RLIM = R
      END IF
C
C-----------------------------------------------------------------------
C
      N = 0
 1010 CONTINUE
      IF (NL .EQ. 2) THEN
         READ (2,*,END=9000)
         READ (2,*,END=9000) II, XX, YY, MM
         READ (2,*,END=9000) SIG
         IF (II .EQ. 0) GO TO 1010                   ! Blank encountered
      ELSE
 1015    CALL RDCHAR (2, LINE, NC, ISTAT)
         IF (ISTAT .GT. 0) GO TO 9000
         IF (LINE(1:1) .EQ. 'C') GO TO 1015
         LINE = LINE(1:NC)//' 0 0 0 0 0'
         READ (LINE,*,ERR=1015,END=1015) II, XX, YY, MM, 
     .        SIG, DUM, R, CHI, SHP
         NN = NINT(R)
         IF ((II .LE. 0) .OR. (CHI .GT. CHIMAX) .OR. 
     .        (SIG .GT. SIGMAX) .OR. (MM .GT. MAGMAX) .OR.
     .        (ABS(SHP) .GT. SHPMAX) .OR. (NN .LT. NMIN)) GO TO 1010
C
C-----------------------------------------------------------------------
C
         IF (LIMIT) THEN
            IF ((XX .LT. XMIN) .OR. (XX .GT. XMAX) .OR. (YY .LT. YMIN)
     .           .OR. (YY .GT. YMAX)) GO TO 1010
         END IF
C
C-----------------------------------------------------------------------
C
      END IF
      IF (N .LE. 0) THEN
         N = 1
         X(1) = XX
         Y(1) = YY
         M(1) = MM
         G(1) = MM
C
C The goodness `G' of a star starts out equal to its magnitude.
C
         GO TO 1010
      ELSE
         DO I=1,N
            IF (MM .LT. G(I)) THEN
C
C This star is better than star I.  Every star from I to N shifts
C up one position.
C
               IF (N .LT. MAX) N = N+1
               DO J=N-1,I,-1
                  K = J+1
                  X(K) = X(J)
                  Y(K) = Y(J)
                  M(K) = M(J)
                  G(K) = G(J)
               END DO
               X(I) = XX
               Y(I) = YY
               M(I) = MM
               G(I) = MM
               GO TO 1020
            ELSE
C
C This star is worse than star I.  If it is within RLIM of star I, 
C increase the goodness by 1 to make it less attractive and keep going.
C
               R = (X(I)-XX)**2 + (Y(I)-YY)**2
               IF (R .LE. 1.) THEN
                  MM = MM+100.
               ELSE IF (R .LE. RLIM) THEN
                  MM = MM+1.
               END IF
            END IF
         END DO
C
C This star is worse than all stars in the list.  Add it to the 
C end of the list if there are fewer than MAX stars in the list.
C
         IF (N .LT. MAX) THEN
            N = N+1
            X(N) = XX
            Y(N) = YY
            M(N) = MM
            G(N) = MM
            GO TO 1010
         END IF
      END IF
C
 1020 IF (I .LT. N) THEN
C
C This star has been insterted into the middle of the list.
C Look for fainter ones within RLIM of it.
C
         SORT = .FALSE.
         DO J=I+1,N
            R = (X(J)-X(I))**2 + (Y(J)-Y(I))**2
            IF (R .LE. 1.) THEN
               G(J) = G(J)+100.
               SORT = .TRUE.
            ELSE IF (R .LT. RLIM) THEN
               G(J) = G(J)+1.
               SORT = .TRUE.
            END IF
         END DO
C
         IF (SORT) THEN
            J = I+1
            K = N-I
            CALL QUICK (G(J), K, INDEX)
            CALL RECTFY (X(J), K, INDEX, HOLD)
            CALL RECTFY (Y(J), K, INDEX, HOLD)
            CALL RECTFY (M(J), K, INDEX, HOLD)
         END IF
      END IF
      GO TO 1010
 9000 CONTINUE
      RETURN
      END!
