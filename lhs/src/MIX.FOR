C****************************************************************
C SUBROUTINE MIX IMPLEMENTS THE SCHEME DESCRIBED IN IMAN AND
C CONOVER (1982) -SEE USERS GUIDE- FOR INDUCING A DESIRED
C CORRELATION STRUCTURE
C
      SUBROUTINE MIX
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CCMATR.INC'                                              GDW-96  
      USE CCMATR                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C     INCLUDE 'CRANK.INC'                                               GDW-96  
      USE CRANK                         
C     INCLUDE 'CWORKC.INC'                                              GDW-96  
      USE CWORKC                        
C
      LOC1(I,J)=J+(I*I-I)/2
      LOC(I,J)=(J-1)*N+I
C
C     IF ICM EQUALS 0 THEN A MIX OF THE GENERATED DATA IS
C     OBTAINED.  OTHERWISE, A MIX OF RANDOM INTEGERS IS
C     GENERATED AND MULTIPLIED BY THE CORRELATION MATRIX.
C     THE RESULT IS RANKED AND THE RANKS ARE USED TO GENERATE
C     THE RANDOM SAMPLE.
C
C     FOR THE FULL RANK CASE(N.GT.NV)THE FOLLOWING PROCEDURE IS USED,
C     GENERATE A SAMPLE SUCH THAT NONE OF THE OFF-DIAGONAL ELEMENTS OF
C     THE CORRESPONDING CORRELATION MARTIX ARE EQUAL TO ONE IN ABSOLUTE
C     VALUE.CHOLESKY FACTORIZATION IS DONE NEXT.
C     FOR THE LESS THAN FULL RANK CASE(N.LE.NV) 25 SAMPLES ARE GENERATED
C     AND THAT SAMPLE WHOSE CORRESPONDING CORRELATION MATRIX HAS THE
C     SMALLEST MAXIMUM(IN ABSOLUTE VALUE)OFF-DIAGONAL ELEMENT IS THE
C     SAMPLE THAT IS CHOSEN. CHOLESKY FACTORIZATION IS SKIPPED IN
C     THIS CASE.
C
      NVN=NV*N
      CMXOLD=9999.0
      DN=N+1
      DO 1 I = 1,N
         XV(I) = FINVNOR(FLOAT(I)/DN)
    1 CONTINUE
C
      DO 100 IJK=1,25
   10    CONTINUE
         DO 20 I=1,N
            IWK(I)=I
   20    CONTINUE
         DO 50 I=1,NV
            DO 30 J=2,N
               IR=N+2-J
               JJ=IR*RNUM1()+1
               KK=IWK(IR)
               IWK(IR)=IWK(JJ)
               IWK(JJ)=KK
   30       CONTINUE
            DO 40 J=1,N
               NDX=IWK(J)
               X(LOC(J,I))=XV(NDX)
   40       CONTINUE
   50    CONTINUE
         IF (NV .EQ. 1  .OR.  IRP .EQ. 1) GO TO 1000
C
C        COMPUTE THE CORRELATION MATRIX ON THE VDW SCORES
C
         CALL CORCAL
         If(KLLERR) Return
         IF (N .GT. NV) THEN
            DO 60 I=2,NV
               IMIN=I-1
               DO 60 J=1,IMIN
                  IF (ABS(CORR(LOC1(I,J))) .GE. 0.9999) GO TO 10
   60       CONTINUE
            GO TO 400
         END IF
         CMX=0.0
         DO 70 I=2,NV
            IMIN=I-1
            DO 70 J=1,IMIN
               ELEM=ABS(CORR(LOC1(I,J)))
               CMX=MAX(CMX,ELEM)
   70    CONTINUE
         IF (CMX .LT. CMXOLD) THEN
            CMXOLD=CMX
            REWIND 2
            WRITE (2) (X(I),I=1,NVN)
         END IF
  100 CONTINUE
      REWIND 2
      READ (2) (X(I),I=1,NVN)
      GO TO 1000
C
C     X IS AN N X NV MATRIX OF VDW SCORES. IF THIS IS A FULL RANK CASE
C     (N.GT.NV) THE RANK CORRELATION MATRIX OF X MUST BE FOUND AND
C     THE CHOLESKY FACTORIZATION PERFORMED. IF THIS IS NOT A FULL RANK
C     CASE (N.LE.NV) THIS MATRIX OF VDW SCORES WILL BE USED TO MIX THE
C     INTERVALS OF THE RAW DATA MATRIX XSAVE
C
  400 NVX=(NV*(NV+1))/2
      CALL CHLSKY
      If(KLLERR) Return
C
C     INVERT THE LOWER TRIANGULAR MATRIX Q RESULTING FROM THE
C     CHOLESKY FACTORIZATION
C
      CALL MATINV
      If(KLLERR) Return
C
      IF (ICM .NE. 0  .OR. (N .EQ. 3 .AND. NV .EQ. 2)) THEN
C
C        IF THE USER HAS SPECIFIED HIS OWN RANK CORRELATION STRUCTURE
C        THIS MATRIX MULTIPLICATION LOOP IS NEEDED
C
         REWIND 3
         READ(3)CORR
         DO 520 K=1,NV
            KX=K
            DO 500 IT=1,KX
               XV(IT)=0.0
               ITX=IT
               DO 500 IW=ITX,KX
                  XV(IT)=CORR(LOC1(K,IW))*Q(LOC1(IW,IT))+XV(IT)
  500       CONTINUE
            DO 520 IW=1,KX
               S(LOC1(K,IW))=XV(IW)
  520    CONTINUE
C
      ELSE
C
C        THIS ASSUMES THE DESIRED CORRELATION STRUCTURE IS THE IDENTITY
C        MATRIX, I.E. CLOSE TO ORTHOGONAL
C
         DO 540 I=1,NVX
            S(I)=Q(I)
  540    CONTINUE
C
      END IF
C
      DO 620 K=1,N
         DO 600 IT=1,NV
            XV(IT)=0.0
            ITX=IT
            DO 600 IW=1,ITX
               XV(IT)=X(LOC(K,IW))*S(LOC1(IT,IW))+XV(IT)
  600    CONTINUE
         DO 610 IW=1,NV
            X(LOC(K,IW))=XV(IW)
  610    CONTINUE
  620 CONTINUE
C
C     SINCE X NO LONGER CONTAINS INTEGERS WE MUST RANK EACH COLUMN
C     IN ORDER TO GET A NEW MATRIX TO USE FOR MIXING THE INTERVALS
C     OF THE RAW DATA MATRIX XSAVE
C
 1000 CONTINUE
      DO 1300 J=1,NV
         DO 1100 I=1,N
            XV(I)=X(LOC(I,J))
 1100    CONTINUE
         CALL RANKER
         If(KLLERR) Return
         DO 1200 I=1,N
            X(LOC(I,J))=RXV(I)
 1200    CONTINUE
 1300 CONTINUE
      DO 1500 J=1,NV
         DO 1400 I=1,N
            K=INT(X(LOC(I,J))+0.01)
            X(LOC(I,J))=XSAVE(LOC(K,J))
 1400    CONTINUE
 1500 CONTINUE
C
      RETURN
      END
