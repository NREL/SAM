C=======================================================================
      SUBROUTINE STEPADD (IMAX, IADIM, A)
C=======================================================================

C   --*** STEPADD *** (STEPWISE) Add selected variable in stepwise regression
C   --   Modified by Amy Gilkey - revised 01/08/91
C   --
C   --STEPADD adjusts the A array for the addition of a selected variable
C   --in the stepwise regression.
C   --
C   --Parameters:
C   --   IMAX - IN - the index of the variable to add
C   --   IADIM - IN - the dimension of the A array (NIV+1+NIV)
C   --   A - IN/OUT - the full form correlation matrix (internal to stepwise
C   --      regression routines)

      IMPLICIT NONE

      INCLUDE 'stp_step_1_common.inc'

      INTEGER IMAX
      INTEGER IADIM
      DOUBLE PRECISION A(IADIM,IADIM)

      INTEGER IP1
      INTEGER I, J
      DOUBLE PRECISION D

C   --Adjust A after entry of a variable
      IP1 = NIV1 + NIV1-1
      D = A(IMAX,IMAX)
      DO 110 I = 1, IP1
         DO 100 J = 1, IP1
            IF ((I .NE. IMAX) .AND. (J .NE. IMAX))
     &         A(I,J) = A(I,J) - A(I,IMAX)*A(IMAX,J) / D
  100    CONTINUE
  110 CONTINUE
      DO 120 J = 1, IP1
         IF (J .NE. IMAX) THEN
            A(J,IMAX) = 0.0
            A(IMAX,J) = A(IMAX,J) / D
         END IF
  120 CONTINUE

      RETURN
      END
C CMS REPLACEMENT HISTORY, Element STP_STEPADD.FOR
C *2     3-MAR-1996 11:36:35 APGILKE "Convert to double precision"
C *1     1-NOV-1995 11:24:14 APGILKE "Initial load - Source code"
C CMS REPLACEMENT HISTORY, Element STP_STEPADD.FOR
