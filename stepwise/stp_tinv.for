C=======================================================================
      DOUBLE PRECISION FUNCTION TINV (R, K, N)
C=======================================================================

C   --*** TINV *** (STEPWISE) Calculates inverse CDF of t distribution
C   --   Modified by Amy Gilkey - revised 01/08/91
C   --
C   --TINV calculates the inverse CDF of the t distribution the N degrees of
C   --freedom at the Kth root of R (R is between 0 and 1).
C   --TINV can generate random variates from t distribution with K=1 and
C   --evaluate percentage points with K>=1.
C   --From Hubert J Chen J Statis. Comput. Simul. 1978 vol 7 pp 167-180.

      IMPLICIT NONE

      DOUBLE PRECISION R
      INTEGER K
      INTEGER N

      DOUBLE PRECISION DCDFT

      DOUBLE PRECISION P
      DOUBLE PRECISION DF
      DOUBLE PRECISION X1, X2
      DOUBLE PRECISION XNEW
      DOUBLE PRECISION R1

      DOUBLE PRECISION W(19)
      DATA W / 3183500., 2250., 230., 75., 41., 28., 21., 17., 15., 13.,
     &   12., 11., 10.5, 10., 9.5, 9., 8.5, 8.3, 8.1 /

      P = R**(1.0/DFLOAT(K))
      IF ((P .LT. 1.0E-6) .OR. (P .GT. (1.00-1.0E-6))) THEN
         CALL QAMESSAG (-1, '+ERROR', 'No inverse due to extreme value')
         TINV = 0.0
         RETURN
      END IF
      DF = DFLOAT(N)
      IF (N .GT. 19) THEN
         X2 = 8.0
      ELSE
         X2 = W(N)
      END IF
      X1 = -X2
  100 CONTINUE
      XNEW = (X1+X2) / 2.0
      R1 = DCDFT (XNEW, DF)
      IF (ABS (R1-P) .GE. 5.0E-6) THEN
         IF (R1 .LE. P) THEN
            X1 = XNEW
         ELSE
            X2 = XNEW
         END IF
         GOTO 100
      END IF

      TINV = XNEW
      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_TINV.FOR
C *2     3-MAR-1996 11:36:55 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:19 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_TINV.FOR
