C=======================================================================
      SUBROUTINE FENENUM (I, J, K, NX, NY, NZ, N)
C=======================================================================

C   --*** FENENUM *** (CAMCON_LIB) Get node/element number from I,J,K index
C   --   Modified by Amy Gilkey - revised 02/27/93
C   --   Written by David Rudeen - 11/03/88
C   --
C   --FENENUM determines the node or element number from the given I,J,K index
C   --assuming a 2- or 3-dimensional rectilinear grid where nodes and
C   --elements are numbered first in order of increasing X (I-index),
C   --then Y (J-index), then Z (K-index).
C   --
C   --Parameters:
C   --   I, J, K - IN integer - for a node, the I,J,K index of the node;
C   --      for an element, the first node and smallest node number in the
C   --      connectivity list for the element
C   --   NX, NY, NZ - IN integer - for a node, the number of nodes in the X,Y,Z
C   --      directions; for an element, the number of elements in the X,Y,Z
C   --      directions (the number of nodes - 1); Use 0 for not defined
C   --   N - OUT integer - the node or element number

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER I, J, K, NX, NY, NZ, N

      IF (NZ .NE. 0) THEN
         N = I + (J-1)*NX + (K-1)*NX*NY
         IF ((I .LE. 0) .OR. (I .GT. NX)) N = 0
         IF ((J .LE. 0) .OR. (J .GT. NY)) N = 0
         IF ((K .LE. 0) .OR. (K .GT. NZ)) N = 0
      ELSE IF (NY .NE. 0) THEN
         N = I + (J-1)*NX
         IF ((I .LE. 0) .OR. (I .GT. NX)) N = 0
         IF ((J .LE. 0) .OR. (J .GT. NY)) N = 0
         IF (K .GT. 1) N = 0
      ELSE
         N = I
         IF ((I .LE. 0) .OR. (I .GT. NX)) N = 0
         IF ((J .GT. 1) .OR. (K .GT. 1)) N = 0
      END IF

      RETURN
      END
