C****************************************************************
C SUBROUTINE RANKER IS USED TO FIND THE RANKS OF A VECTOR OF DATA
C
      SUBROUTINE RANKER
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
      DO 10 I=1,N
         RXV(I)=FLOAT(I)
   10 CONTINUE
C
      CALL HPSRT
      If(KLLERR) Return
      DO 20 I=1,N
         IWK(I)=IFIX(RXV(I))
         RXV(I)=FLOAT(I)
   20 CONTINUE
C
C     FIND TIES
C
      I=0
   30 I=I+1
      IF (I.GE.N) GO TO 80
      IF (XV(I).NE.XV(I+1)) GO TO 30
C
C     COUNT TIES
C
      NTIES=2
   40 II=I+NTIES
      IF (II .LE. N) THEN
         IF (XV(I) .EQ. XV(II)) THEN
            NTIES=NTIES+1
            GO TO 40
         END IF
      END IF
C
C     AVERAGE TIED RANKS
C
      AVG=0.0
      DO 60 J=1,NTIES
         AVG=AVG+RXV(I+J-1)
   60 CONTINUE
      AVG=AVG/FLOAT(NTIES)
      I=I-1
      DO 70 J=1,NTIES
         I=I+1
         RXV(I)=AVG
   70 CONTINUE
      GO TO 30
C
   80 CONTINUE
C
C     REORDER
C
      DO 200 I=1, N-1
   90    K=IWK(I)
         IF (K.NE.I) THEN
            XHOLD=XV(I)
            RHOLD=RXV(I)
            XV(I)=XV(K)
            RXV(I)=RXV(K)
            XV(K)=XHOLD
            RXV(K)=RHOLD
            IWK(I)=IWK(K)
            IWK(K)=K
            GO TO 90
         END IF
  200 CONTINUE
C
      RETURN
      END
