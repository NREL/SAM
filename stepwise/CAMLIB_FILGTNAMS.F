C=======================================================================
      SUBROUTINE FILGTNAMS (FILDSC, INOUT, OPTREQ, DEFSPC)
C=======================================================================

C   --*** FILGTNAMS *** (CAMCON_LIB) Return a file read by FILRDNAMS
C   --   Written by Amy Gilkey - revised 01/06/96
C   --
C   --FILGTNAMS returns a file whose name was be read by FILRDNAMS.  Each time
C   --FILGTNAMS is called, a new file description is returned.
C   --
C   --Parameters:
C   --   FILDES - OUT char*(*) - the file description (for prompts and
C   --      messages
C   --   INOUT - OUT char*(*) - the file status:
C   --      (case is unimportant)
C   --      'INPUT' for an input file
C   --      'OUTPUT' for an output file
C   --      'INTERACTIVE' for an input file that may be interactive
C   --      'TEXT' for a non-file entry (a text string entry)
C   --   OPTREQ - OUT char*(*) - the optional/required file flag:
C   --      (only the first character is checked; case is unimportant)
C   --      'OPTIONAL' for an optional file
C   --      'REQUIRED' for a required file
C   --   DEFSPC - OUT char*(*) - the default file name or the returned file
C   --      name

      IMPLICIT NONE

C   --Common Variables:
      INCLUDE 'CAMLIB_FILES_COMMON.H'

C   --Parameters Definitions:
      CHARACTER*(*) FILDSC
      CHARACTER*(*) INOUT, OPTREQ
      CHARACTER*(*) DEFSPC

      ICTFIL = ICTFIL + 1
      IF (ICTFIL .LE. NUMFIL) THEN
         FILDSC = DSCTBL(ICTFIL)
         IF (INOTBL(ICTFIL) .EQ. 'T') THEN
            INOUT = 'TEXT'
         ELSE IF (INOTBL(ICTFIL) .EQ. 'U') THEN
            INOUT = 'INTERACTIVE'
         ELSE IF (INOTBL(ICTFIL) .EQ. 'I') THEN
            INOUT = 'INPUT'
         ELSE IF (INOTBL(ICTFIL) .EQ. 'O') THEN
            INOUT = 'OUTPUT'
         ELSE
            INOUT = ' '
         END IF
         IF (OPTTBL(ICTFIL)) THEN
            OPTREQ = 'OPTIONAL'
         ELSE
            OPTREQ = 'REQUIRED'
         END IF
         DEFSPC = FILTBL(ICTFIL)
      ELSE
         ICTFIL = 0
         FILDSC = ' '
         INOUT = ' '
         OPTREQ = ' '
         DEFSPC = ' '
      END IF

      RETURN
      END
