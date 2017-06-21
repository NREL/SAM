C****************************************************************
C SUBROUTINE HPSRT IS USED IN THE RANKING OF THE SAMPLED DATA
C
      SUBROUTINE HPSRT
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CRANK.INC'                                               GDW-96  
      USE CRANK                         
C
      INTEGER R
C
      L=N/2+1
      R=N
   10 IF (L.LE.1) GO TO 70
      L=L-1
      XHOLD=XV(L)
      YHOLD=RXV(L)
   20 J=L
   30 I=J
      J=2*J
      IF (J-R) 40,50,60
   40 IF (XV(J).LT.XV(J+1)) J=J+1
   50 IF (XHOLD.GE.XV(J)) GO TO 60
      XV(I)=XV(J)
      RXV(I)=RXV(J)
      GO TO 30
   60 XV(I)=XHOLD
      RXV(I)=YHOLD
      GO TO 10
   70 XHOLD=XV(R)
      YHOLD=RXV(R)
      XV(R)=XV(1)
      RXV(R)=RXV(1)
      R=R-1
      IF (R.GT.1) GO TO 20
      XV(1)=XHOLD
      RXV(1)=YHOLD
      RETURN
      END
