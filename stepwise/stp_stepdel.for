C=======================================================================
      SUBROUTINE STEPDEL (IADIM, A, IVAROK, F1)
C=======================================================================

C   --*** STEPDEL *** (STEPWISE) Delete variable in stepwise regression
C   --   Modified by Amy Gilkey - revised 01/10/91
C   --
C   --STEPDEL determines which variable to delete in this step of the stepwise
C   --regression.  The variable is deleted and the A array is adjusted.
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
C   --   F1 - SCRATCH - size = NIV (original)

      IMPLICIT NONE

      INCLUDE 'stp_force_common.inc'
      INCLUDE 'stp_step_1_common.inc'

      INTEGER IADIM
      DOUBLE PRECISION A(IADIM,IADIM)
      INTEGER IVAROK(*)
      DOUBLE PRECISION F1(*)

      DOUBLE PRECISION FSUB

      LOGICAL ANYDEL
      INTEGER I, J
      INTEGER IDEL
      INTEGER IP1
      DOUBLE PRECISION D
      DOUBLE PRECISION FP

  100 CONTINUE
C   --Calculate partial Fs for variable in regression
      DO 110 I = 1, NIV1-1
         IF (IVAROK(I) .EQ. 0) THEN
            D = A(NIV1,NIV1) * A(NIV1+I,NIV1+I)
            IF (D .NE. 0.0) THEN
               F1(I) = A(I,NIV1) * A(I,NIV1) * DFLOAT(NPHI) / D
            ELSE
               F1(I) = 1.0E35
            END IF
         ELSE IF (IVAROK(I) .GE. 1) THEN
C         --Handle forced variable
            F1(I) = 1001.0
         END IF
  110 CONTINUE

C   --Check if any variable is to be deleted
      ANYDEL = .FALSE.
      DO 160 IDEL = 1, NIV1-1
         IF (IVAROK(IDEL) .GE. 0) THEN
            FP = FSUB (F1(IDEL), 1, NPHI)
            IF (FP .GT. SIGF(2)) THEN
               ANYDEL = .TRUE.
C            --Delete variable as ok (mark as possible)
               IVAROK(IDEL) = -1
               NVOK = NVOK - 1
               NPHI = NPHI + 1
C            --Adjust A after deletion
               IP1 = NIV1 + NIV1-1
               D = A(NIV1+IDEL,NIV1+IDEL)
               DO 130 I = 1, IP1
                  DO 120 J = 1, IP1
                     IF ((I .NE. IDEL) .AND. (J .NE. NIV1+IDEL))
     &                  A(I,J) = A(I,J) - A(I,NIV1+IDEL)*A(IDEL,J) / D
  120             CONTINUE
  130          CONTINUE
               DO 140 J = 1, IP1
                  A(IDEL,J) = A(IDEL,J) / D
  140          CONTINUE
               DO 150 J = 1, IP1
                  IF (J .NE. IDEL) THEN
                     A(J,NIV1+IDEL) = 0.0
                     A(NIV1+IDEL,J) = 0.0
                  END IF
  150          CONTINUE
            END IF
         END IF
  160 CONTINUE

      IF (ANYDEL) GOTO 100

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_STEPDEL.FOR
C *2     3-MAR-1996 11:36:38 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:15 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_STEPDEL.FOR
