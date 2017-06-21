C=======================================================================
      SUBROUTINE CALCWTVAR (NUMOBS, XYRAW, IWTVAR, WTS)
C=======================================================================

C   --*** CALCWTVAR *** (STEPWISE) Calculate weight variable
C   --   Modified by Amy Gilkey - revised 01/08/91
C   --
C   --CALCWTVAR gets the weighted variable, which is the input values for the
C   --variable divided by the weighted sum.
C   --
C   --Parameters:
C   --   NUMOBS - IN - the number of observations
C   --   XYRAW - IN - the raw variable data
C   --   IWTVAR - IN - the variable number of the variable containing weights
C   --   WTS - OUT - the weights for the regression (if WTVAR)

      IMPLICIT NONE

      INTEGER NUMOBS
      DOUBLE PRECISION XYRAW(NUMOBS,*)
      INTEGER IWTVAR
      DOUBLE PRECISION WTS(NUMOBS)

      INTEGER IOBS
      DOUBLE PRECISION WTSUM
      DOUBLE PRECISION WTCORR

C   --Calculate the weight variable
      WTSUM = 0.0
      DO 100 IOBS = 1, NUMOBS
         WTS(IOBS) = XYRAW(IOBS,IWTVAR)
         WTSUM = WTSUM + WTS(IOBS)
  100 CONTINUE
      WTCORR = DFLOAT(NUMOBS) / WTSUM
      DO 110 IOBS = 1, NUMOBS
         WTS(IOBS) = WTS(IOBS) * WTCORR
  110 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_CALCWTVAR.FOR
C *2     3-MAR-1996 11:35:51 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:00 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_CALCWTVAR.FOR
