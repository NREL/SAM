C****************************************************************
C SUBROUTINE DSINV IS USED IN INVERTING A CORRELATION MATRIX
C
      SUBROUTINE DSINV (N)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CCMATR.INC'                                              GDW-96  
      USE CCMATR                        
C
      IPARM=0
      CALL DMFSD (N,IPARM)
      If(KLLERR) Return
      IF (IPARM.LT.0) RETURN
      IPIV=N*(N+1)/2
      IND=IPIV
      DO 40 I=1,N
      DIN=1.0/CORR(IPIV)
      CORR(IPIV)=DIN
      MIN=N
      KEND=I-1
      LANF=N-KEND
      IF (KEND.LE.0) GO TO 30
      J=IND
      DO 20 K=1,KEND
      WORK=0.0
      MIN=MIN-1
      LHOR=IPIV
      LVER=J
      DO 10 L=LANF,MIN
      LVER=LVER+1
      LHOR=LHOR+L
   10 WORK=WORK+CORR(LVER)*CORR(LHOR)
      CORR(J)=-WORK*DIN
   20 J=J-MIN
   30 IPIV=IPIV-MIN
   40 IND=IND-1
      DO 60 I=1,N
      IPIV=IPIV+I
      J=IPIV
      DO 60 K=I,N
      WORK=0.0
      LHOR=J
      DO 50 L=K,N
      LVER=LHOR+K-I
      WORK=WORK+CORR(LHOR)*CORR(LVER)
   50 LHOR=LHOR+L
      CORR(J)=WORK
   60 J=J+K
      RETURN
      END
