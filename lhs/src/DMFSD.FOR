C****************************************************************
C SUBROUTINE DMFSD IS USED IN INVERTING A CORRELATION MATRIX
C
      SUBROUTINE DMFSD (N,IPARM)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CCMATR.INC'                                              GDW-96  
      USE CCMATR                        
C
      KPIV=0
      DO 60 K=1,N
      KPIV=KPIV+K
      IND=KPIV
      LEND=K-1
      TOL=ABS(.01*(CORR(KPIV)))
      DO 60 I=K,N
      DSUM=0.0
      IF (LEND.EQ.0) GO TO 20
      DO 10 L=1,LEND
      LANF=KPIV-L
      LIND=IND-L
   10 DSUM=DSUM+CORR(LANF)*CORR(LIND)
   20 DSUM=CORR(IND)-DSUM
      IF (I.NE.K) GO TO 50
      IF (DSUM-TOL) 30,30,40
   30 IF (DSUM.LE.0.0) GO TO 70
      KT=K-1
      WRITE(6,80)KT
   40 DPIV=SQRT(DSUM)
      CORR(KPIV)=DPIV
      DPIV=1.0/DPIV
      GO TO 60
   50 CORR(IND)=DSUM*DPIV
   60 IND=IND+I
      RETURN
   70 WRITE(6,90)K
      IPARM=-K
      RETURN
C
C
   80 FORMAT(20X,'ROUNDING ERROR IN ROW ',I2)
   90 FORMAT(20X,'MATRIX IS SINGULAR AT ROW ',I2)
      END
