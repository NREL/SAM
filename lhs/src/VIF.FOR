C****************************************************************
C SUBROUTINE VIF COMPUTES THE VARIANCE INFLATION FACTOR OF A
C CORRELATION MATRIX
C
      SUBROUTINE VIF
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
C
C     INCLUDE 'PARMS.INC'                                               GDW-96  
      USE PARMS                         
C     INCLUDE 'CPARAM.INC'                                              GDW-96  
      USE CPARAM                        
C     INCLUDE 'CCMATR.INC'                                              GDW-96  
      USE CCMATR                        
C
      LOC1(I,J)=J+(I*I-I)/2
C
      CALL DSINV(NV)
      If(KLLERR) Return
      CRKMX=0.0
      DO 630 I=1,NV
        CRK=CORR(LOC1(I,I))
        IF(CRK.GT.CRKMX)CRKMX=CRK
  630 CONTINUE
      WRITE(6,9001)CRKMX
      RETURN
 9001 FORMAT('0','THE VARIANCE INFLATION FACTOR FOR THIS MATRIX IS',
     1       F6.2)
      END
