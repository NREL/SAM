C****************************************************************
C SUBROUTINE BETAFN IS USED IN GENERATING A BETA DISTRIBUTION
C
      SUBROUTINE BETAFN(X,FOFX)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
      DIMENSION Y(1)
      COMMON /PQ/P,Q,NZ
      IF(X.LT.0.)X=0.
      IF(X.GT.1.)X=1.
      OMX = 1.-X
      N=1
      CALL BETAIC(X,OMX,P,Q,N,Y,NZ)
      If(KLLERR) Return
      FOFX = Y(1)
      RETURN
      END
