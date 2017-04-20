C=======================================================================
      SUBROUTINE STEPSTEP (NVARIN, NVAR, IVAR,
     &   ISEND, IADIM, A, IVAROK, F1)
C=======================================================================

C   --*** STEPSTEP *** (STEPWISE) Performs step of stepwise regression
C   --   Modified by Amy Gilkey - revised 10/11/92
C   --
C   --STEPSTEP performs a single step of a stepwise regression.  It returns
C   --with the variables selected in this step.  The first step selects the
C   --forced variables only (if any).  Routine STEPINIT must be called before
C   --this routine.
C   --
C   --STEPSTEP uses the algorithm described in Draper : Smith.
C   --Array A is used as a working matrix which is initialized (in STEPINIT)
C   --by copying the correlation matrix in full form for the model.
C   --A is modified in each step and used to select variables to be
C   --included or deleted.
C   --
C   --Parameters:
C   --   NVARIN - IN/OUT - the number of selected variables for LAST step
C   --   NVAR - OUT - the number of selected variables for this step
C   --   IVAR - IN/OUT - the indices of the selected variables (indexes IXIV)
C   --   ISEND - OUT - true iff no new variables added this step
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

      INTEGER NVARIN
      INTEGER NVAR
      INTEGER IVAR(*)
      LOGICAL ISEND
      INTEGER IADIM
      DOUBLE PRECISION A(IADIM,IADIM)
      INTEGER IVAROK(*)
      DOUBLE PRECISION F1(*)

      INTEGER LOCINT

      INTEGER IX
      INTEGER I
      LOGICAL NOVAR

      ISEND = .FALSE.

C   --If all variables included, stop
      IF (NVOK .GE. MAXOK) GOTO 100

C   --Force the inclusion of all force variables
      IF (DOFRC) THEN
         DOFRC = .FALSE.
         CALL STEPFORCE (IADIM, A, IVAROK)
C      --If some force variables added, return
         IF (NVOK .GT. 0) GOTO 110
      END IF

C   --Check which variable to enter; NOVAR set if the variable with highest
C   --doesn't qualify for entry (return with ISEND true)
      CALL STEPPICK (IADIM, A, IVAROK, NOVAR)
      IF (NOVAR) GOTO 100

C   --Compute partial alphas for deletion, then delete
      CALL STEPDEL (IADIM, A, IVAROK, F1)

      GOTO 110
  100 CONTINUE
      ISEND = .TRUE.
      GOTO 110

C   --Return IVAR(1:NVAR) with array of variables in model
  110 CONTINUE
      NVAR = 0
      DO 120 IX = 1, NVARIN
         I = IVAR(IX)
         IF (IVAROK(I) .GE. 0) THEN
            NVAR = NVAR + 1
            IVAR(NVAR) = I
         END IF
  120 CONTINUE
      NVARIN = NVAR
      DO 130 I = 1, NIV1-1
         IF (IVAROK(I) .GE. 0) THEN
            IF (LOCINT (I, NVAR, IVAR) .LE. 0) THEN
               NVAR = NVAR + 1
               IVAR(NVAR) = I
            END IF
         END IF
  130 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_STEPSTEP.FOR
C *2     3-MAR-1996 11:36:52 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:18 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_STEPSTEP.FOR
