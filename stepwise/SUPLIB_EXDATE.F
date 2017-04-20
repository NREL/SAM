C=======================================================================
      SUBROUTINE EXDATE (STRING)
C=======================================================================
C     FORTRAN Extension Library - UNIX

C   --*** EXDATE *** (CAMSUPES_LIB) Get current date
C   --   Written by Dennis Morrison and Bill Mill-Curran - revised 09/04/98
C   --
C   --EXDATE returns the current date in a character string.
C   --
C   --Parameters:
C   --   STRING - OUT char*(*) - the returned date string in the format
C   --      mm/dd/yyyy (if STRING 10 or 11 characters) or mm/dd/yy

C   --Machine Dependencies:
C   --   DATE_AND_TIME returns the date and time as strings

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) STRING

      CHARACTER*8 SDATE
      CHARACTER*10 STIME
      CHARACTER*2 SDAY, SMON, SYEAR2
      CHARACTER*4 SYEAR4

C   --Retrieve date
      CALL DATE_AND_TIME (SDATE, STIME)

C   --Format date into strings
      SYEAR4 = SDATE(1:4)
      SYEAR2 = SDATE(3:4)
      SMON = SDATE(5:6)
      SDAY = SDATE(7:8)

C   --Create date string with 2-digit or 4-digit year
      IF ((LEN (STRING) .EQ. 10) .OR. (LEN (STRING) .EQ. 11)) THEN
         STRING = SMON // '/' // SDAY // '/' // SYEAR4
      ELSE
         STRING = SMON // '/' // SDAY // '/' // SYEAR2
      END IF

      RETURN
      END
