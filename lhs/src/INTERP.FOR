C****************************************************************
      SUBROUTINE INTERP (Y, X, XTABLE, ISMAX, IMIN, IMX1, INTFL)
C     INCLUDE 'KILLFILE.INC'                                            GDW-96  
      USE KILLFILE                      
      DIMENSION XTABLE(ISMAX,2)
C
C     INTERPOLATE ON Y TO RETURN AN X VALUE
C
C     IF INTFL = 0  USE LINEAR INTERPOLATION.  OTHERWISE USE LOGARITHMIC.
C
C     XTABLE(I,1) IS X
C     XTABLE(I,2) IS Y
C
      IMAX=IMX1
C     -- This line changed to correct out of bounds condition 3/5/96
C     IF ( IMIN+1 .EQ. IMAX ) IMIN=IMAX-2
      IF ( IMIN+1 .GE. IMAX ) IMIN = MAX( IMAX-2, 1 )
C
  100 I = IMIN + (IMAX-IMIN)/2
      IF ( Y .LT. XTABLE(I,2) ) THEN
         IMAX=I
      ELSE
         IMIN=I
      END IF
      IF (IMIN+1 .LT. IMAX) GO TO 100
C
C     --- NOW INTERPOLATE
C
      IF (INTFL .EQ. 0) THEN
C
C        --- LINEAR INTERPOLATION:
         X = XTABLE(IMIN,1) + (XTABLE(IMAX,1)-XTABLE(IMIN,1))*
     1      (Y-XTABLE(IMIN,2))/(XTABLE(IMAX,2)-XTABLE(IMIN,2))
C
      ELSE
C
C        --- FOR LOGARITHMOC INTERPOLATION:
         XMX=LOG(XTABLE(IMAX,1))
         XMN=LOG(XTABLE(IMIN,1))
         X = XMN + (XMX-XMN) * (Y-XTABLE(IMIN,2)) /
     1               (XTABLE(IMAX,2)-XTABLE(IMIN,2))
         X=EXP(X)
C
      END IF
C
      RETURN
      END
