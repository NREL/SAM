C=======================================================================
      SUBROUTINE FEELEM (NODE, NX, NY, NZ, IELEM)
C=======================================================================

C   --*** FEELEM *** (CAMCON_LIB) Calculate element number associated with node
C   --   Modified by Amy Gilkey - revised 01/12/96
C   --   Written by David Rudeen - 11/03/88
C   --
C   --FEELEM uses routines FEIJK and FENENUM to calculate the element number
C   --associated with the given node number.  Nodes and elements are assumed
C   --to be numbered first in order of increasing X (I-index), then
C   --Y (J-index), and then Z (K-index).
C   --
C   --Parameters:
C   --   NODE - IN integer - the node number of the first node and smallest node
C   --      number in the connectivity list for the element
C   --   NX, NY, NZ - IN integer - the number of nodes in the X,Y,Z (or I,J,K)
C   --      directions
C   --   IELEM - OUT integer - the corresponding element number

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER NODE, NX, NY, NZ, IELEM

      INTEGER I, J, K

C   --Determine (i,j,k) for node
      CALL FEIJK (NODE, NX, NY, NZ, I, J, K)

C   --Determine element from (i,j,k)
      CALL FENENUM (I, J, K, 
     &   MAX (NX-1, 0), MAX (NY-1, 0), MAX (NZ-1, 0), IELEM)

      RETURN
      END
