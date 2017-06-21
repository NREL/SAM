C=======================================================================
      SUBROUTINE EXLOGICAL (LOGNAM, LOGVAL, LENLOG)
C=======================================================================
C     FORTRAN Extension Library - UNIX 

C   --*** EXLOGICAL *** (CAMSUPES_LIB) Get system logical
C   --   Written by Steve Tisinger - 06/30/95
C   --   Revised by Amy Gilkey - revised 07/06/95
C   --
C   --EXLOGICAL gets the value of a system logical.
C   --
C   --Parameters:
C   --   LOGNAM - IN char*(*) - the logical name
C   --   LOGVAL - OUT char*(*) - the returned logical value
C   --   LENLOG - OUT integer - the length of the returned logical value;
C   --      <0 if not defined.

C   --Machine Dependencies:
C   --   GETENV gets value of environment variable.
C   --   LNBLNK returns string length.

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) LOGNAM
      CHARACTER*(*) LOGVAL
      INTEGER LENLOG

      INTEGER LNBLNK

      CALL GETENV (LOGNAM, LOGVAL)
      LENLOG = LNBLNK (LOGVAL)
      IF (LENLOG .LE. 0) LENLOG = -1

      RETURN
      END
