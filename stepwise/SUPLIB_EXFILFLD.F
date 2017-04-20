C=======================================================================
      SUBROUTINE EXFILFLD (FNAME, NODE, DISK, DIR, BASE, EXT, VERS)
C=======================================================================
C     FORTRAN Extension Library - UNIX

C   --*** EXFILFLD *** (CAMSUPES_LIB) Parse file name into fields
C   --   Written by Amy Gilkey - revised 05/28/95
C   --
C   --EXFILFLD parses an input file name into its component fields.
C   --
C   --Parameters:
C   --   FNAME - IN char*(*) - the input file name
C   --   NODE - OUT char*(*) - the node name
C   --   DISK - OUT char*(*) - the disk name
C   --   DIR - OUT char*(*) - the directory name
C   --   BASE - OUT char*(*) - the base file name
C   --   EXT - OUT char*(*) - the extension
C   --   VERS - OUT char*(*) - the version

C   --Machine Dependencies:
C   --   The parsing is dependent on the file name structure of the computer.

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) FNAME
      CHARACTER*(*) NODE, DISK, DIR, BASE, EXT, VERS

      INTEGER LENNAM
      INTEGER ISTART, IEND
      INTEGER ICH
      INTEGER IEXT
      INTEGER IVERS

      NODE = ' '
      DISK = ' '
      DIR = ' '
      BASE = ' '
      EXT = ' '
      VERS = ' '

      IF (FNAME .EQ. ' ') RETURN

      LENNAM = INDEX (FNAME, ' ') - 1
      IF (LENNAM .LE. 0) LENNAM = LEN (FNAME)
      ISTART = 1
      IEND = ISTART-1
      ICH = INDEX (FNAME(ISTART:), '/') + ISTART - 1
  100 CONTINUE
      IF (ICH .GT. IEND) THEN
         IEND = ICH
         ICH = INDEX (FNAME(ICH+1:), '/') + ICH+1 - 1
         GOTO 100
      END IF
      IF (ISTART .LT. IEND) THEN
         DIR = FNAME(ISTART:IEND)
         ISTART = IEND + 1
      END IF
      IEXT = INDEX (FNAME(ISTART:), '.') + ISTART - 1
      IVERS = 0
      IF (IVERS .LT. ISTART) IVERS = LENNAM + 1
      IF (IEXT .LT. ISTART) IEXT = IVERS
      IF (ISTART .LT. IEXT) THEN
         IEND = IEXT - 1
         BASE = FNAME(ISTART:IEND)
         ISTART = IEND + 1
      END IF
      IF (ISTART .LT. IVERS) THEN
         IEND = IVERS - 1
         EXT = FNAME(ISTART:IEND)
         ISTART = IEND + 1
      END IF
      IF (ISTART .LE. LENNAM) THEN
         IEND = LENNAM
         VERS = FNAME(ISTART:IEND)
         ISTART = IEND + 1
      END IF

      RETURN
      END
