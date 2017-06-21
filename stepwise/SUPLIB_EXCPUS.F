C=======================================================================
      SUBROUTINE EXCPUS (CPUSEC)
C=======================================================================
C     FORTRAN Extension Library - UNIX

C   --*** EXCPUS *** (CAMSUPES_LIB) Get CPU time
C   --   Written by Dennis Morrison and Bill Mill-Curran - revised xx/xx/xx
C   --
C   --EXCPUS returns an accumulated CPU time.  The base time is undefined;
C   --only relative times are valid.
C   --
C   --Parameters:
C   --   CPUSEC - OUT real - the returned CPU time

C   --Machine Dependencies:
C   --   Calls ETIME for elapsed CPU time.

      IMPLICIT NONE

C   --Parameter Definitions:
      REAL CPUSEC

      REAL ETIME

      REAL RTIME, T, TIMARY(2)

      LOGICAL INIT
      REAL RTIME0
      SAVE INIT, RTIME0
      DATA INIT /.TRUE./

      IF (INIT) THEN
C      --Initialize timer first time in
         T = ETIME (TIMARY)
         RTIME0 = TIMARY(1)
         RTIME = 0.0
         INIT = .FALSE.
      ELSE
C      --Get CPU time and subtract initial time
         T = ETIME (TIMARY)
         RTIME = TIMARY(1) - RTIME0
      END IF

      CPUSEC = RTIME

      RETURN
      END
