C=======================================================================
      SUBROUTINE STEPINIT (NUMOBS, NIV, CORR, CORDV, IADIM, A, IVAROK)
C=======================================================================

C   --*** STEPINIT *** (STEPWISE) Initialize stepwise regression
C   --   Modified by Amy Gilkey - revised 01/10/91
C   --
C   --STEPINIT initializes the stepwise regression procedure for each
C   --dependent variable.  This routine must be called before routine STEPSTEP.
C   --
C   --Parameters:
C   --   NUMOBS - IN - the number of observations
C   --   NIV - IN - the number of selected independent variables
C   --   CORR - IN - the correlation matrix for all selected independent
C   --      variables
C   --   CORDV - IN - the correlation for all selected independent variables
C   --      with the dependent variable
C   --   IADIM - IN - the dimension of the A array (NIV+1+NIV)
C   --   A - OUT - the full form correlation matrix (internal to stepwise
C   --      regression routines)
C   --   IVAROK - OUT - the status of the selected independent variables
C   --      (internal to stepwise regression routines):
C   --      <0 - possibility for this step
C   --      =0 - selected for this step
C   --      >0 - forced variable

      IMPLICIT NONE

      INCLUDE 'stp_force_common.inc'
      INCLUDE 'stp_step_1_common.inc'

      INTEGER NUMOBS
      INTEGER NIV
      DOUBLE PRECISION CORR(*)
      DOUBLE PRECISION CORDV(*)
      INTEGER IADIM
      DOUBLE PRECISION A(IADIM,IADIM)
      INTEGER IVAROK(*)

      INTEGER IV1, IV2

      INTEGER IXSYM
      INTEGER I, J

      IXSYM(I,J) = (I*I-I)/2 + J
C      --IXSYM statement function calculates (i,j) index for half-stored
C      --   lower-row-wise matrix stored in 1D

C   --Save the number of variables (+1)
      NIV1 = NIV + 1

C   --Copy CORR to A
      DO 110 IV1 = 1, IADIM
         DO 100 IV2 = 1, IADIM
            A(IV1,IV2) = 0.0
  100    CONTINUE
  110 CONTINUE
      DO 130 IV1 = 1, NIV
         DO 120 IV2 = 1, IV1-1
            A(IV1,IV2) = CORR(IXSYM(IV1,IV2))
            A(IV2,IV1) = A(IV1,IV2)
  120    CONTINUE
         A(IV1,IV1) = CORR(IXSYM(IV1,IV1))
  130 CONTINUE
      DO 140 IV2 = 1, NIV
         A(NIV1,IV2) = CORDV(IV2)
         A(IV2,NIV1) = A(NIV1,IV2)
  140 CONTINUE
      A(NIV1,NIV1) = 1.0
C   --Augment to form full A-matrix
      DO 150 IV2 = 1, NIV
         A(IV2,NIV1+IV2) = 1.0
         A(NIV1+IV2,IV2) = -1.0
  150 CONTINUE

C   --Set all variables to possible
      NVOK = 0
      CALL INIINT (NIV, -1, IVAROK)

C   --Set NPHI
      NPHI = NUMOBS - 1

C   --Set maximum number of variables that may be selected
      MAXOK = MIN (NIV, NUMOBS-1)

C   --Set force variables flag
      DOFRC = (NUMFRC .GT. 0)

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_STEPINIT.FOR
C *2     3-MAR-1996 11:36:45 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:17 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_STEPINIT.FOR
