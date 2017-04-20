C=======================================================================
      SUBROUTINE FILDFNAM (FILDSC, INOUT, OPTREQ, DEFSPC)
C=======================================================================

C   --*** FILDFNAM *** (CAMCON_LIB) Define a file for use by FILRDNAMS
C   --   Written by Amy Gilkey - revised 08/17/98
C   --
C   --FILDFNAM defines a file whose name will be read by FILRDNAMS.  Each time
C   --FILDFNAM is called, a file description is added to a table used by
C   --FILRDNAMS.
C   --
C   --Parameters:
C   --   FILDES - IN char*(*) - the file description (for prompts and
C   --      messages
C   --   INOUT - IN char*(*) - the file status:
C   --      (case is unimportant)
C   --      'INPUT' for an input file
C   --      'OUTPUT' for an output file
C   --      'INTERACTIVE' for an input file that may be interactive
C   --      'TEXT' for a non-file entry (a text string entry)
C   --   OPTREQ - IN char*(*) - the optional/required file flag:
C   --      (only the first character is checked; case is unimportant)
C   --      'OPTIONAL' for an optional file
C   --      'REQUIRED' for a required file
C   --   DEFSPC - IN char*(*) - the default file name

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_FILES_COMMON.H'

C   --Parameters Definitions:
      CHARACTER*(*) FILDSC
      CHARACTER*(*) INOUT, OPTREQ
      CHARACTER*(*) DEFSPC

      INTEGER L
      CHARACTER*8 TMPSTR

      IF (NUMFIL .GE. MAXFIL) THEN
         CALL QAMESSAG (0, '+ERROR',
     &      'The maximum number of files has been exceeded')
         RETURN
      END IF

      NUMFIL = NUMFIL + 1

      DSCTBL(NUMFIL) = FILDSC

      TMPSTR = INOUT
      CALL STRUPCASE (TMPSTR)
      IF (TMPSTR(1:1) .EQ. 'T') THEN
         INOTBL(NUMFIL) = 'T'
      ELSE IF (TMPSTR(1:5) .EQ. 'INTER') THEN
         INOTBL(NUMFIL) = 'U'
      ELSE IF (TMPSTR(1:1) .EQ. 'I') THEN
         INOTBL(NUMFIL) = 'I'
      ELSE IF (TMPSTR(1:1) .EQ. 'O') THEN
         INOTBL(NUMFIL) = 'O'
      ELSE
         INOTBL(NUMFIL) = 'T'
      END IF

      TMPSTR = OPTREQ(1:1)
      CALL STRUPCASE (TMPSTR)
      OPTTBL(NUMFIL) = (TMPSTR .EQ. 'O')

      IF ((DEFSPC .NE. ' ') .AND. (INOTBL(NUMFIL) .NE. 'T')) THEN
         CALL EXLOGICAL (DEFSPC, FILTBL(NUMFIL), L)
         IF (L .LT. 0) FILTBL(NUMFIL) = DEFSPC
      ELSE
         FILTBL(NUMFIL) = DEFSPC
      END IF
      IF (INOTBL(NUMFIL) .NE. 'T') THEN
c!!!         CALL STRUPCASE (FILTBL(NUMFIL))  !#VAX only, lowercase on UNIX
      END IF

      RETURN
      END
