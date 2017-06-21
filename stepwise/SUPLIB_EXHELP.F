C=======================================================================
      LOGICAL FUNCTION EXHELP (HELPLIB, TOPIC)
C=======================================================================
C     FORTRAN Extension Library - UNIX

C   --*** EXHELP *** (CAMSUPES_LIB) Display HELP for a specific topic
C   --   Revised by Amy Gilkey - revised 05/18/95
C   --   Written by D. P. Flanagan, 12/18/83
C   --
C   --EXHELP displays help for a specific topic in a system dependent fashion.
C   --
C   --Parameters:
C   --   EXHELP - OUT logical - returns true iff help topic was found in
C   --      help library
C   --   HELPLIB - IN char*(*) - the HELP library name
C   --   TOPIC - IN char*(*) - the HELP topic

C   --Machine Dependencies:
C   --   Always returns false.

      IMPLICIT NONE

      CHARACTER*(*) HELPLIB
      CHARACTER*(*) TOPIC

      EXHELP = .FALSE.

      RETURN
      END
