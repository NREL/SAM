C=======================================================================
      SUBROUTINE STEPPICK (IADIM, A, IVAROK, NOVAR)
C=======================================================================

C   --*** STEPPICK *** (STEPWISE) Picks variable to add in stepwise regression
C   --   Modified by Amy Gilkey - revised 01/10/91
C   --
C   --STEPPICK determines which variable to add next to the stepwise regression.
C   --
C   --Parameters:
C   --   IADIM - IN - the dimension of the A array (NIV+1+NIV)
C   --   A - IN/OUT - the full form correlation matrix (internal to stepwise
C   --      regression routines)
C   --   IVAROK - IN/OUT - the status of the selected independent variables
C   --      (internal to stepwise regression routines):
C   --      <0 - possibility for this step
C   --      =0 - selected for this step
C   --      >0 - forced variable
C   --   NOVAR - OUT - true iff no variable is picked to be added

      IMPLICIT NONE

      INCLUDE 'stp_force_common.inc'
      INCLUDE 'stp_step_1_common.inc'

      INTEGER IADIM
      DOUBLE PRECISION A(IADIM,IADIM)
      INTEGER IVAROK(*)
      LOGICAL NOVAR

      DOUBLE PRECISION FSUB

      INTEGER IMAX
      INTEGER I
      DOUBLE PRECISION V
      DOUBLE PRECISION VMAX
      DOUBLE PRECISION F
      DOUBLE PRECISION FP

C   --Calculate V for each vector; Set VMAX to the maximum V, IMAX to the
C   --variable index

      IMAX = -999
      DO 100 I = 1, NIV1-1
         IF (IVAROK(I) .LE. -1) THEN
            IF (A(I,I) .NE. 0.0) THEN  !!! divide by zero
               V = A(I,NIV1) * A(NIV1,I) / A(I,I)
               IF ((IMAX .LT. 1) .OR. (VMAX .LT. V)) THEN
                  VMAX = V
                  IMAX = I
               END IF
            END IF
         END IF
  100 CONTINUE

      NPHI = NPHI - 1

C   --Compute sequential F for variable selected for entry

      IF (VMAX .NE. A(NIV1,NIV1)) THEN
         F = DFLOAT(NPHI) * VMAX / (A(NIV1,NIV1)-VMAX)
      ELSE
         F = 1.0E35
      END IF
      FP = FSUB (F, 1, NPHI)
      IF (FP .GT. SIGF(1)) THEN
         NOVAR = .TRUE.
         GOTO 110
      END IF

C   --Add selected variable

      NOVAR = .FALSE.
      NVOK = NVOK + 1
      IVAROK(IMAX) = 0
C   --Adjust A after entry of a variable
      CALL STEPADD (IMAX, IADIM, A)

  110 CONTINUE
      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_STEPPICK.FOR
C *2     3-MAR-1996 11:36:48 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:17 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_STEPPICK.FOR
