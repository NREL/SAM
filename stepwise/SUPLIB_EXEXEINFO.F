C=======================================================================
      SUBROUTINE EXEXEINFO (EXEFULL,
     &   EXENAM, EXEDAT, EXETYP, EXECLASS, EXEVERS, IERROR)
C=======================================================================
C     FORTRAN EXTENSION LIBRARY - UNIX ### NOT DONE ###

C   --*** EXEXEINFO *** (CAMSUPES_LIB) Get information about executable
C   --   Written by Amy Gilkey - revised 09/04/98
C   --
C   --EXEXEINFO spawns a process to get information about the current
C   --or supplied executable.  Most of the information is supplied by a
C   --CMS build, so executables not linked in this way will not have
C   --this information
C   --
C   --Parameters:
C   --   EXEFULL - IN/OUT char*(*) - the full path file name of the executable;
C   --      if blank, the current executable will be used and EXEFULL set
C   --   EXENAM - OUT char*(*) - the build name of the executable
C   --   EXEDAT - OUT char*(*) - the build link date in the format
C   --      mm/dd/yyyy (if EXEDAT 10 or 11 characters) or mm/dd/yy
C   --   EXETYP - OUT char*(*) - the build type (PROD, etc.)
C   --   EXECLASS - OUT char*(*) - the build class
C   --   EXEVERS - OUT char*(*) - the build version
C   --   IERROR - OUT integer - the returned error code
C   --      0 - no error
C   --      1 - executable was not linked with CMS build
C   --      2 - executable does not exist or other serious problem

C   --Machine Dependencies:
C   --   ###

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) EXEFULL, EXENAM, EXEDAT, EXETYP, EXECLASS, EXEVERS
      INTEGER IERROR

C   --Initialize parameters
      EXENAM = ' '
      EXEDAT = ' '
      EXETYP = ' '
      EXECLASS = ' '
      EXEVERS = ' '
      IERROR = 2

      RETURN
      END
