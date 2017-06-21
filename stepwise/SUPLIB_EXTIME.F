C=======================================================================
      SUBROUTINE EXTIME (STRING)
C=======================================================================
C     FORTRAN Extension Library - UNIX

C   --*** EXTIME *** (CAMSUPES_LIB) Get current time
C   --   Written by Dennis Morrison and Bill Mill-Curran - revised xx/xx/xx
C   --
C   --EXTIME returns the current time in a character string.  The format is:
C   --HH:MM:SS, where HH is the hour, MM is the minute, SS is the second.
C   --
C   --Parameters:
C   --   STRING - OUT char*(*) - the returned time string

C   --Machine Dependencies:
C   --   TIME returns the system time in seconds.
C   --   LTIME converts the system time to hours, minutes, seconds, etc.

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) STRING

      INTEGER TIME

      INTEGER ITIMSEC, ITIMDAT(9), IDUM

      ITIMSEC = TIME ()
      CALL LTIME (ITIMSEC, ITIMDAT)
      WRITE (STRING, 10000, IOSTAT=IDUM)
     &   ITIMDAT(3), ITIMDAT(2), ITIMDAT(1)
10000  FORMAT (I2.2, ':', I2.2, ':', I2.2)

      RETURN
      END
