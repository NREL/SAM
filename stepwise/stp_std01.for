C=======================================================================
      SUBROUTINE STD01 (NUMOBS, NUMVAR, XYRAW, XYSTD)
C=======================================================================

C   --*** STD01 *** (STEPWISE) Calculate standard 0-1 for all variable values
C   --   Modified by Amy Gilkey - revised 11/28/95
C   --
C   --STD01 calculates the standard 0-1 value over all observations for each
C   --variable.  The resulting values have a mean of zero and a variance of 1.
C   --
C   --Parameters:
C   --   NUMOBS - IN - the number of observations
C   --   NUMVAR - IN - the number of variables
C   --   XYRAW - IN - the variable data
C   --   XYSTD - OUT - the variable data (standard 0-1)

      IMPLICIT NONE

      INTEGER NUMOBS
      INTEGER NUMVAR
      REAL XYRAW(NUMOBS,NUMVAR)
      REAL XYSTD(NUMOBS,NUMVAR)

      INTEGER IV
      INTEGER IOBS
      DOUBLE PRECISION XSUM
      DOUBLE PRECISION VMEAN
      DOUBLE PRECISION SS
      DOUBLE PRECISION B1
      DOUBLE PRECISION VAR
      DOUBLE PRECISION STDEV


      DO 140 IV = 1, NUMVAR

C      --Calculate mean

         XSUM = 0.0
         DO 100 IOBS = 1, NUMOBS
            XSUM = XSUM + XYRAW(IOBS,IV)
  100    CONTINUE
         VMEAN = XSUM / NUMOBS

C      --Calculate standard deviation

         SS = 0.0
         DO 110 IOBS = 1, NUMOBS
            B1 = XYRAW(IOBS,IV) - VMEAN
            SS = SS + B1*B1
  110    CONTINUE
         VAR = SS / (NUMOBS-1)
         STDEV = SQRT (VAR)

C      --Standardize

         IF (STDEV .GT. 1E-30) THEN
            DO 120 IOBS = 1, NUMOBS
               XYSTD(IOBS,IV) = (XYRAW(IOBS,IV) - VMEAN) / STDEV
  120       CONTINUE
         ELSE
            DO 130 IOBS = 1, NUMOBS
               XYSTD(IOBS,IV) = (XYRAW(IOBS,IV) - VMEAN)
  130       CONTINUE
         END IF
  140 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_STD01.FOR
C *3     3-MAR-1996 11:36:31 APGILKE "Convert to double precision"
C *2     4-DEC-1995 19:14:37 APGILKE "Calc mean/stdev instead of passing"
C *1     1-NOV-1995 11:24:13 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_STD01.FOR
