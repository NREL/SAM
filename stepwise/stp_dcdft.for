C=======================================================================
      DOUBLE PRECISION FUNCTION DCDFT (X, V)
C=======================================================================

C   --*** DCDFT *** (STEPWISE) Calculate CDF of t distribution
C   --   Modified by Amy Gilkey - revised 01/08/91
C   --
C   --DCDFT calculates CDF of t distribution with V degrees of freedom,
C   --at X.
C   --From Hubert J Chen J Statis. Comput. Simul. 1978 vol 7 pp 167-180.

      IMPLICIT NONE

      DOUBLE PRECISION X
      DOUBLE PRECISION V

      INTEGER N
      INTEGER M
      DOUBLE PRECISION PI
      DOUBLE PRECISION THETA
      DOUBLE PRECISION SS, CS
      DOUBLE PRECISION CSS
      DOUBLE PRECISION TERM
      DOUBLE PRECISION SUM
      DOUBLE PRECISION SUMS

      PI = 4.0 * ATAN (1.0)
      N = INT(V)
      THETA = ATAN (X / SQRT (V))
      IF (N .NE. 1) THEN
         SS = SIN (THETA)
         CS = COS (THETA)
         CSS = CS * CS
      END IF
      IF (MOD (N, 2) .LE. 0) THEN
         TERM = 1.0
         SUM = TERM
         DO 100 M = 2, N-2, 2
            TERM = TERM * CSS * (DFLOAT(M-1) / DFLOAT(M))
            SUM = SUM + TERM
  100    CONTINUE
         SUM = SS * SUM
      ELSE
         IF (N .EQ. 1) THEN
            SUMS = 0.0
         ELSE
            TERM = CS
            SUMS = TERM
            DO 110 M = 3, N-2, 2
               TERM = TERM * CSS * (DFLOAT(M-1) / DFLOAT(M))
               SUMS = SUMS + TERM
  110       CONTINUE
            SUMS = SS * SUMS
         END IF
         SUM = (2.0/PI) * (THETA+SUMS)
      END IF
      DCDFT = (SUM+1.0) / 2.0

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_DCDFT.FOR
C *2     3-MAR-1996 11:36:01 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:02 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_DCDFT.FOR
