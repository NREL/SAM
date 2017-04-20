C=======================================================================
      SUBROUTINE RANKALLDATA (NUMOBS, NUMVAR, XYRAW, XYRNK, IWORK)
C=======================================================================

C   --*** RANKALLDATA *** (STEPWISE) Rank variable values over all observations
C   --   Modified by Amy Gilkey - revised 01/08/91
C   --
C   --RANKALLDATA ranks the values over all observations for each variable.
C   --
C   --Parameters:
C   --   NUMOBS - IN - the number of observations
C   --   NUMVAR - IN - the number of variables
C   --   XYRAW - IN - the variable data
C   --   XYRNK - OUT - the variable data (rank)
C   --   IWORK - SCRATCH - size = NUMOBS

      IMPLICIT NONE

      INTEGER NUMOBS
      INTEGER NUMVAR
      REAL XYRAW(NUMOBS,NUMVAR)
      REAL XYRNK(NUMOBS,NUMVAR)
      INTEGER IWORK(NUMOBS)

      INTEGER IV

      DO 100 IV = 1, NUMVAR
         CALL RANKER (NUMOBS, XYRAW(1,IV), XYRNK(1,IV), IWORK)
  100 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_RANKALLDATA.FOR
C *1     1-NOV-1995 11:24:08 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_RANKALLDATA.FOR
