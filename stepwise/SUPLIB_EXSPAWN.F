C=======================================================================
      SUBROUTINE EXSPAWN (DCLCMD, IERROR)
C=======================================================================
C     FORTRAN EXTENSION LIBRARY - UNIX

C   --*** EXSPAWN *** (CAMSUPES_LIB) Spawn a process
C   --   Written by Mike Williamson - 10/04/95
C   --   Revised by Amy Gilkey - revised 10/07/95
C   --
C   --EXSPAWN spawns a process which executes independently, then returns
C   --to this routine
C   --
C   --Parameters:
C   --   DCLCMD - IN char*(*) - the command line to spawn
C   --   IERROR - OUT integer - the error indicator
C   --       0 if no error
C   --       1 if error from process
C   --       2 if error from spawn

C   --Machine Dependencies:
C   --   Always returns IERROR = 2 (error from spawn).

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) DCLCMD
      INTEGER IERROR

      INTEGER SYSTEM

      INTEGER ISTAT

      ISTAT = SYSTEM (DCLCMD)
      IERROR = ISTAT

      RETURN
      END
