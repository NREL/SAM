C=======================================================================
      SUBROUTINE FEMINMAX (NPTS, PTS, VMIN, VMAX)
C=======================================================================

C   --*** FEMINMAX *** (CAMCON_LIB) Calculate min/max value
C   --   Written by Amy Gilkey - revised 02/27/93
C   --
C   --FEMINMAX calculates the minimum and maximum of the data.
C   --
C   --Parameters:
C   --   NPTS - IN integer - the number of points
C   --   PTS - IN real array - the points
C   --   VMIN, VMAX - OUT real - the minimum and maximum value of the points

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER NPTS
      REAL PTS(NPTS)
      REAL VMIN, VMAX

      INTEGER I

      IF (NPTS .LE. 0) THEN
         VMIN =  1.0E+30
         VMAX = -1.0E+30
      ELSE
         VMIN = PTS(1)
         VMAX = PTS(1)
      END IF
      DO 100 I = 1, NPTS
         VMIN = MIN (VMIN, PTS(I))
         VMAX = MAX (VMAX, PTS(I))
  100 CONTINUE

      RETURN
      END
