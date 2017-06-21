C=======================================================================
      SUBROUTINE FFGETLIN (RETLIN)
C=======================================================================

C   --*** FFGETLIN *** (CAMCON_LIB) Return line read by free-field reader
C   --   Revised by Amy Gilkey - revised 02/27/93
C   --   Free-field reader taken from the SUPES library
C   --
C   --FFGETLIN returns the line read by the free-field reader.
C   --
C   --Parameters:
C   --   RETLIN - OUT char*(*) - the line read by the free-field reader

C   --Common Variables:
C   --   Uses SAVLIN of /FREEFIELD_COMMON/

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_FREEFIELD_COMMON.H'

C   --Parameter Definitions:
      CHARACTER*(*) RETLIN

************************************************************************

      RETLIN = SAVLIN

      RETURN
      END
