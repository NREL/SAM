C     Last change:  S    26 Jan 98   10:46 am
C****************************************************************
C SUBROUTINE GAMMA GENERATES GAMMA DISTRIBUTIONS WITH PARAMETERS
C ALPHA AND BETA.  THE DISTRIBUTION IS EXACT IF ALPHA = 1.
C OTHERWISE, AN ACCEPTANCE-REJECTION SCHEME IS USED TO GENERATE
C 10,000 RANDOM OBSERVATIONS FROM A GAMMA DISTRIBUTION WITH
C PARAMETERS ALPHA AND BETA AND THE RESULTING EDF IS SAMPLED TO
C GENERATE THE DESIRED DISTRIBUTION. ALPHA < 1 THE ALGORITHM OF
C BEST (1983) IS USED.  FOR ALPHA > 1 THE ALGORITHM OF MINH (1988)
C IS USED.
C
      SUBROUTINE GAMMA(J)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CSAMP.INC'                                               GDW-96  
      USE CSAMP                         
C     INCLUDE 'CWORKX.INC'                                              GDW-96  
      USE CWORKX                        
C
      DOUBLE PRECISION VALUE,Z,B
C
      LOC(I,J)=(J-1)*N+I
C
      PROBINC = 1./FLOAT(N)
      IF(IRS .EQ. 1) PROBINC = 1.0
      STRTPT = 0.
      READ(8) ALPHA, BETA
C
C     CHECK FOR ALPHA = 1, WHICH CAN BE DONE BY DIRECT INVERSION
C
      IF (ALPHA .EQ. 1.) THEN
         DO 1 I = 1,N
            R = PROBINC*RNUM1() + STRTPT
            RES = -LOG(1. - R)/BETA
            X(LOC(I,J)) = MAX(RES,1.E-10)
            IF(IRS .EQ. 0) STRTPT = STRTPT + PROBINC
    1    CONTINUE
         RETURN
      END IF
C
C     GENERATE "MAXTB*2" RANDOM DEVIATES FROM THE GAMMA DISTRIBTION WITH
C     PARAMETERS ALPHA AND BETA
C
      IF (ALPHA .LT. 1.) THEN
         Z = .07 + .75*(1. - ALPHA)**.5
         B = 1. + EXP(-Z)*ALPHA/Z
      ENDIF
      NOBS=2*MAXTB
      DO 3 I = 1,NOBS
         IF (ALPHA .GT. 1.)THEN
C           -- CALL ROUTINE BY MINH
            CALL GAMMAM(ALPHA,VALUE)
            If(KLLERR) Return
         ELSE
C           -- CALL ROUTINE BY BEST
            CALL GAMMAB(ALPHA,VALUE,Z,B)
            If(KLLERR) Return
         ENDIF
         XX(I) = VALUE/BETA
    3 CONTINUE
C
C     SORT THE RANDOM DEVIATES TO FORM THE EDF
C
      CALL SIFT(XX,NOBS)
      If(KLLERR) Return
C
C     GENERATE THE DESIRED SAMPLE BY SAMPLING FROM THE EDF
C
      DO I = 1, N
         R = PROBINC*RNUM1() + STRTPT
         NR = MAX(1 , NINT(R*NOBS))
         RES = XX(NR)
         X(LOC(I,J)) = MAX(RES,1.E-10)
         IF (IRS == 0) STRTPT = REAL(I) / REAL(N)
      END DO
C
      RETURN
      END
