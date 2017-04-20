C=======================================================================
      SUBROUTINE EXFILDAT (FILNAM, FDATE, FTIME)
C=======================================================================
C     FORTRAN Extension Library - UNIX

C   --*** EXFILDAT *** (CAMSUPES_LIB) Return the file date and time
C   --   Written by Amy Gilkey - revised 10/27/98
C   --
C   --EXFILDAT returns the last update date and time for a file.
C   --
C   --Parameters:
C   --   FILNAM - IN char*(*) - the name of the file
C   --   FDATE - OUT char*(*) - the date the file was last updated
C   --      (mm/dd/yyyy if FDATE 10 or 11 characters or mm/dd/yy)
C   --   FTIME - OUT char*(*) - the date the file was last updated (hh:mm:ss)

C   --Machine Dependencies:
C   --   LSTAT returns status array for file.
C   --   LTIME converts the system time.

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) FILNAM
      CHARACTER*(*) FDATE
      CHARACTER*(*) FTIME

      INTEGER LSTAT

      INTEGER IERR
      INTEGER IDUM
      INTEGER ISTATARY(13)
      INTEGER ITIMSEC, ITIMDAT(9)

C   --Initialize date and time
      FDATE = ' '
      FTIME = ' '

C   --Get status array for the file
      IERR = LSTAT (FILNAM, ISTATARY)

C   --If not found, return with blank date
      IF (IERR .NE. 0) RETURN

C   --Get the system time
      ITIMSEC = ISTATARY(10)
      CALL LTIME (ITIMSEC, ITIMDAT)

C   --Convert system time into mm/dd/yyyy or mm/dd/yy
      IF ((LEN (FDATE) .EQ. 10) .OR. (LEN (FDATE) .EQ. 11)) THEN
         WRITE (FDATE, 10000, IOSTAT=IDUM)
     &      ITIMDAT(5), ITIMDAT(4)+1, ITIMDAT(6)+1900
10000     FORMAT (I2.2, '/', I2.2, '/', I4.4)
      ELSE
         WRITE (FDATE, 10010, IOSTAT=IDUM)
     &      ITIMDAT(5), ITIMDAT(4)+1, (MOD (ITIMDAT(6), 100))
10010     FORMAT (I2.2, '/', I2.2, '/', I2.2)
      END IF

C   --Convert system time into hh:mm:ss
      WRITE (FTIME, 10020, IOSTAT=IDUM)
     &   ITIMDAT(3), ITIMDAT(2), ITIMDAT(1)
10020  FORMAT (I2.2, ':', I2.2, ':', I2.2)

      RETURN
      END
