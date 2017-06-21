C=======================================================================
      SUBROUTINE FFGETCMT (CMTNW, CONTNW)
C=======================================================================

C   --*** FFGETCMT *** (CAMCON_LIB) Get special characters for free-field reader
C   --   Revised by Amy Gilkey - revised 02/27/93
C   --   Free-field reader taken from the SUPES library
C   --
C   --FFGETCMT gets the comment and continuation characters recognized by
C   --the free-field reader.
C   --
C   --Parameters:
C   --   CMTNW - OUT char*1 - the comment character
C   --   CONTNW - OUT char*1 - the continuation character

C   --Common Variables:
C   --   Uses CMTCH, CONTCH of /FREEFIELD_COMMON/

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_FREEFIELD_COMMON.H'

C   --Parameter Definitions:
      CHARACTER CMTNW, CONTNW

************************************************************************

      CMTNW = CMTCH
      CONTNW = CONTCH

      RETURN
      END
