C=======================================================================
      SUBROUTINE CALCCONFINTV (NUMOBS, NUMVAR, NIV, IXIV,
     &   XRNK, VMEAN, DIAG, CORR, PSTP, TMPINV)
C=======================================================================

C   --*** CALCCONFINTV *** (STEPWISE) Calculate value for confidence interval
C   --   Modified by Amy Gilkey - revised 01/08/91
C   --
C   --CALCCONFINTV calculates a value for the confidence interval for each
C   --observation.
C   --
C   --Parameters:
C   --   NUMOBS - IN - the number of observations
C   --   NIV - IN - the number of selected independent variables
C   --   IXIV - IN - the indices of the selected independent variables
C   --   XRNK - IN - the variable data (rank if 'RANK', standard 0-1 if 'STD01')
C   --   VMEAN - IN - the mean for each selected independent variable
C   --   DIAG - IN - the cross product ? for all selected independent variables
C   --   CORR - IN - the correlation matrix for all selected independent
C   --      variables
C   --   PSTP - IN - the values for the confidence interval
C   --   TMPINV - SCRATCH - size = IXSYM(NIV+1,NIV+1)

      IMPLICIT NONE

      INTEGER NUMOBS
      INTEGER NUMVAR
      INTEGER NIV
      INTEGER IXIV(NIV)
      DOUBLE PRECISION XRNK(NUMOBS,NUMVAR)
      DOUBLE PRECISION VMEAN(NIV)
      DOUBLE PRECISION DIAG(NIV)
      DOUBLE PRECISION CORR(*)
      DOUBLE PRECISION PSTP(NUMOBS)
      DOUBLE PRECISION TMPINV(*)

      INTEGER NIV1
      INTEGER IV1, IV2
      INTEGER INVERR
      INTEGER IOBS
      DOUBLE PRECISION RNOBS
      DOUBLE PRECISION P
      DOUBLE PRECISION CORR12
      DOUBLE PRECISION X1, X2
      DOUBLE PRECISION PSUM

      INTEGER IXSYM
      INTEGER I, J

      IXSYM(I,J) = (I*I-I)/2 + J
C      --IXSYM statement function calculates (i,j) index for half-stored
C      --   lower-row-wise matrix stored in 1D


      NIV1 = NIV + 1

      RNOBS = NUMOBS
      TMPINV(IXSYM(1,1)) = RNOBS
      DO 110 IV1 = 1, NIV
         TMPINV(IXSYM(IV1+1,1)) = VMEAN(IV1) * RNOBS
         DO 100 IV2 = 1, IV1-1
            TMPINV(IXSYM(IV1+1,IV2+1)) = CORR(IXSYM(IV1,IV2))
     &         * SQRT (DIAG(IV1)*DIAG(IV2))
     &         + VMEAN(IV1)*VMEAN(IV2)*RNOBS
  100    CONTINUE
         TMPINV(IXSYM(IV1+1,IV1+1)) = CORR(IXSYM(IV1,IV1))
     &      * DIAG(IV1)
     &      + VMEAN(IV1)*VMEAN(IV1)*RNOBS
  110 CONTINUE

      CALL INVERTSYM (NIV1, TMPINV, INVERR)

      DO 140 IOBS = 1, NUMOBS
         P = 0.0
         DO 130 IV1 = 1, NIV1
            PSUM = 0.0
            DO 120 IV2 = 1, NIV1
               IF (IV2 .LE. IV1) THEN
                  CORR12 = TMPINV(IXSYM(IV1,IV2))
               ELSE
                  CORR12 = TMPINV(IXSYM(IV2,IV1))
               END IF
               IF (IV2 .GT. 1) THEN
                  X2 = XRNK(IOBS,IXIV(IV2-1))
               ELSE
                  X2 = 1.0
               END IF
               PSUM = PSUM + CORR12*X2
  120       CONTINUE
            IF (IV1 .GT. 1) THEN
               X1 = XRNK(IOBS,IXIV(IV1-1))
            ELSE
               X1 = 1.0
            END IF
            P = P + PSUM*X1
  130    CONTINUE
         PSTP(IOBS) = P
  140 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_CALCCONFINTV.FOR
C *2     3-MAR-1996 11:35:41 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:23:58 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_CALCCONFINTV.FOR
