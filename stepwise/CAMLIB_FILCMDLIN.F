C=======================================================================
      SUBROUTINE FILCMDLIN (MAXFLD, NUMFLD, FIELDS)
C=======================================================================

C   --*** FILCMDLIN *** (CAMCON_LIB) Read and parse command line
C   --   Modified by Amy Gilkey - revised 09/02/94
C   --   Written by Harold Iuzzolino - 04/19/88
C   --
C   --FILCMDLIN reads and parses the command line fields.   Fields may be
C   --enclosed by quotes (which are stripped).
C   --
C   --Parameters:
C   --   MAXFLD - IN integer - the maximum number of command line arguments
C   --      to be read
C   --   NUMFLD - OUT integer - the number of fields read (including null
C   --      arguments)
C   --   FIELDS - OUT char*(*) array - the command line fields
C   --
C   --See EXCMDLIN for comments on setting up command line arguments.

      IMPLICIT NONE

C   --Parameter Definitions:
      INTEGER MAXFLD, NUMFLD
      CHARACTER*(*) FIELDS(MAXFLD)

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER I
      INTEGER LENLIN
      INTEGER ISTART, IEND
      CHARACTER*1024 CMDLIN

      DO 100 I = 1, MAXFLD
         FIELDS(I) = ' '
  100 CONTINUE

C   --EXCMDLIN reads the command line
      CALL EXCMDLIN (CMDLIN)
      LENLIN = ISTRLEN (CMDLIN)

      NUMFLD = 0
C   --Set the start-character pointer to the first command line character
      ISTART = 1

C   --Repeat the following block for each field
  110 CONTINUE
      IF ((ISTART .LE. LENLIN) .AND. (NUMFLD .LT. MAXFLD)) THEN

C      --Increment argument counter
         NUMFLD = NUMFLD+1

         IF (CMDLIN(ISTART:ISTART) .EQ. '"') THEN
C         --Seek the next quote
            ISTART = ISTART + 1
            IEND = INDEX (CMDLIN(ISTART:LENLIN), '"') + ISTART-2
         ELSE
C         --Seek the next blank
            IEND = INDEX (CMDLIN(ISTART:LENLIN), ' ') + ISTART-2
         END IF
C      --If no blank, use through the end of the command line
         IF (IEND .LE. ISTART-2) IEND = LENLIN

C      --Save the argument
         IF (ISTART .LE. IEND) FIELDS(NUMFLD) = CMDLIN(ISTART:IEND)

C      --Advance the start-character pointer beyond the blank
         ISTART = IEND+2
         IF (CMDLIN(ISTART:ISTART) .EQ. ' ') ISTART = ISTART + 1

         GOTO 110
      END IF

      RETURN
      END
