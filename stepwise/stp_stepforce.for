C=======================================================================
      SUBROUTINE STEPFORCE (IADIM, A, IVAROK)
C=======================================================================

C   --*** STEPFORCE *** (STEPWISE) Add forced variables in stepwise regresssion
C   --   Modified by Amy Gilkey - revised 01/08/91
C   --
C   --STEPFORCE adds all the forced variables to the stepwise regression and
C   --adjusts the A array.
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

      IMPLICIT NONE

      INCLUDE 'stp_force_common.inc'
      INCLUDE 'stp_step_1_common.inc'

      INTEGER IADIM
      DOUBLE PRECISION A(IADIM,IADIM)
      INTEGER IVAROK(*)

      INTEGER IFRC
      INTEGER IMAX

      DO 100 IFRC = 1, NUMFRC
         IF (NVOK .GE. MAXOK) GOTO 100
         IMAX = IXFRC(IFRC)
         IF (IVAROK(IMAX) .LE. -1) THEN
            NPHI = NPHI - 1
C         --Add selected variable as forced variable
            NVOK = NVOK + 1
            IVAROK(IMAX) = 1
C         --Adjust A after entry of a variable
            CALL STEPADD (IMAX, IADIM, A)
         END IF
  100 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_STEPFORCE.FOR
C *2     3-MAR-1996 11:36:41 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:15 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_STEPFORCE.FOR
