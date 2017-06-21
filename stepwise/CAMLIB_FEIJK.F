C=======================================================================
      SUBROUTINE FEIJK (N, NX, NY, NZ, I, J, K)
C=======================================================================

C   --*** FEIJK *** (CAMCON_LIB) Determine I,J,K index from node/element number
C   --   Modified by Amy Gilkey - revised 01/10/96
C   --   Written by David Rudeen - 11/03/88
C   --
C   --FEIJK determines the I,J,K index for the given node or element number
C   --assuming a 2- or 3-dimensional rectilinear grid where nodes and
C   --elements are numbered first in order of increasing X (I-index),
C   --then Y (J-index), then Z (K-index).
C   --
C   --Parameters:
C   --   N - IN integer - the node or element number
C   --   NX, NY, NZ - IN integer - for a node, the number of nodes in the X,Y,Z
C   --      directions; for an element, the number of elements in the X,Y,Z
C   --      directions (the number of nodes - 1); Use 0 for not defined
C   --   I, J, K - OUT integer - for a node, the I,J,K index of the node;
C   --      for an element, the first node and smallest node number in the
C   --      connectivity list for the element

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER N, NX, NY, NZ, I, J, K

      INTEGER ITEMP

      ITEMP = N

      IF (NZ .NE. 0) THEN
         K = (ITEMP-1) / (NX*NY) + 1
         ITEMP = ITEMP - (K-1) * (NX*NY)
         IF (K .GT. NZ) K = -999
      ELSE
         K = 1
      END IF

      IF (NY .NE. 0) THEN
         J = (ITEMP-1) / NX + 1
         ITEMP = ITEMP - (J-1) * NX
         IF (J .GT. NY) J = -999
      ELSE
         J = 1
      END IF

      I = ITEMP
      IF (I .GT. NX) I = -999

      IF ((I .LE. 0) .OR. (J .LE. 0) .OR. (K .LE. 0)) THEN
         I = 0
         J = 0
         K = 0
      END IF

      RETURN
      END
