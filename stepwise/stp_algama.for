C=======================================================================
      DOUBLE PRECISION FUNCTION ALGAMA (A)
C=======================================================================

C   --*** ALGAMA *** (STEPWISE) Return the log gamma
C   --   Modified by Amy Gilkey - revised 02/18/96

      IMPLICIT NONE

      DOUBLE PRECISION A

      INTEGER N
      INTEGER I
      DOUBLE PRECISION W
      DOUBLE PRECISION W2
      DOUBLE PRECISION TEMP

      W = A
      TEMP = 0.0
      IF (W .LE. 13.0) THEN
         N = 14.0 - W
         TEMP = 1.0
         DO 100 I = 1, N
            TEMP = TEMP * W
            W = W + 1.0
  100    CONTINUE
         TEMP = LOG(TEMP)
      END IF
      W2 = W * W
      ALGAMA = (8.33333333E-02 -
     &   (2.77777777E-03 - 7.93650793E-04/W2)/W2)/W +
     &   0.918938533 - W + (W-0.5)*LOG(W) - TEMP

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_ALGAMA.FOR
C *2     3-MAR-1996 11:35:38 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:23:57 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_ALGAMA.FOR
