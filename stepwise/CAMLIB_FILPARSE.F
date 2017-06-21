C=======================================================================
      SUBROUTINE FILPARSE (FIELD, INNAM, DEFNAM, OUTNAM)
C=======================================================================

C   --*** FILPARSE *** (CAMCON_LIB) Create new file name from input and default
C   --   Written by Amy Gilkey - revised 06/11/94
C   --
C   --FILPARSE takes an input file name and a default file name and creates
C   --an output file name by substituting the input fields into the default
C   --file name.  Either the file name or a requested field are returned.
C   --
C   --Parameters:
C   --   FIELD - IN char*(*) - the requested file field:
C   --      (case is unimportant)
C   --      ' '         to return the entire file name
C   --      'NODE'      to return the node name
C   --      'DISK'      to return the disk name
C   --      'DIRECTORY' to return the directory name
C   --      'NAME'      to return the base file name
C   --      'EXTENSION' to return the extension
C   --      'VERSION'   to return the version
C   --   INNAM - IN char*(*) - the input file name
C   --   DEFNAM - IN char*(*) - the default file name
C   --   OUTNAM - OUT char*(*) - the output file name or field (see FIELD)

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) FIELD
      CHARACTER*(*) INNAM, DEFNAM, OUTNAM

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER LNAM
      CHARACTER*10 XFIELD
      CHARACTER*80 INNODE, DEFNODE
      CHARACTER*80 INDISK, DEFDISK
      CHARACTER*128 INDIR, DEFDIR
      CHARACTER*40 INBASE, DEFBASE
      CHARACTER*40 INEXT, DEFEXT
      CHARACTER*10 INVERS, DEFVERS
      CHARACTER*128 INNAMT

      CALL EXFILFLD (INNAM,
     &   INNODE, INDISK, INDIR, INBASE, INEXT, INVERS)
      CALL EXFILFLD (DEFNAM,
     &   DEFNODE, DEFDISK, DEFDIR, DEFBASE, DEFEXT, DEFVERS)

C   --Store input name before output file name set in case same variable;
C   --Input file name is used to reset version number
      INNAMT = INNAM
      OUTNAM = ' '

      XFIELD = FIELD
      CALL STRUPCASE (XFIELD)

      IF (XFIELD(1:3) .EQ. 'NOD') THEN
         XFIELD = 'NODE'
      ELSE IF (XFIELD(1:3) .EQ. 'DIS') THEN
         XFIELD = 'DISK'
      ELSE IF (XFIELD(1:3) .EQ. 'DIR') THEN
         XFIELD = 'DIRECTORY'
      ELSE IF (XFIELD(1:3) .EQ. 'NAM') THEN
         XFIELD = 'NAME'
      ELSE IF (XFIELD(1:3) .EQ. 'EXT') THEN
         XFIELD = 'EXTENSION'
      ELSE IF (XFIELD(1:3) .EQ. 'VER') THEN
         XFIELD = 'VERSION'
      ELSE
         XFIELD = ' '
      END IF

      IF ((XFIELD .EQ. 'NODE') .OR. (XFIELD .EQ. ' ')) THEN
         IF (INNODE .EQ. ' ') INNODE = DEFNODE
      END IF
      IF ((XFIELD .EQ. 'DISK') .OR. (XFIELD .EQ. ' ')) THEN
C      --Default to current disk if a directory is given
         IF (INDISK .EQ. ' ') THEN
            IF (INDIR .EQ. ' ') THEN
               INDISK = DEFDISK
            ELSE
               INDISK = ' '
            END IF
         END IF
      END IF
      IF ((XFIELD .EQ. 'DIRECTORY') .OR. (XFIELD .EQ. ' ')) THEN
         IF (INDIR .EQ. ' ') INDIR = DEFDIR
      END IF
      IF ((XFIELD .EQ. 'NAME') .OR. (XFIELD .EQ. ' ')) THEN
         IF (INBASE .EQ. ' ') INBASE = DEFBASE
      END IF
      IF ((XFIELD .EQ. 'EXTENSION') .OR. (XFIELD .EQ. ' ')) THEN
         IF (INEXT .EQ. ' ') INEXT = DEFEXT
      END IF
      IF ((XFIELD .EQ. 'VERSION') .OR. (XFIELD .EQ. ' ')) THEN
C      --Note that the version is not substituted unless the entire input
C      --name is blank
         IF (INNAMT .EQ. ' ') INVERS = DEFVERS
      END IF

      IF (XFIELD .EQ. 'NODE') THEN
         OUTNAM = INNODE
      ELSE IF (XFIELD .EQ. 'DISK') THEN
         OUTNAM = INDISK
      ELSE IF (XFIELD .EQ. 'DIRECTORY') THEN
         OUTNAM = INDIR
      ELSE IF (XFIELD .EQ. 'NAME') THEN
         OUTNAM = INBASE
      ELSE IF (XFIELD .EQ. 'EXTENSION') THEN
         OUTNAM = INEXT
      ELSE IF (XFIELD .EQ. 'VERSION') THEN
         OUTNAM = INVERS
      ELSE
         OUTNAM = ' '
         LNAM = 0
         IF ((INNODE .NE. ' ') .AND. (INNODE .NE. '-')) THEN
            OUTNAM(LNAM+1:) = INNODE
            LNAM = ISTRLEN (OUTNAM)
         END IF
         IF ((INDISK .NE. ' ') .AND. (INDISK .NE. '-')) THEN
            OUTNAM(LNAM+1:) = INDISK
            LNAM = ISTRLEN (OUTNAM)
         END IF
         IF ((INDIR .NE. ' ') .AND. (INDIR .NE. '-')) THEN
            OUTNAM(LNAM+1:) = INDIR
            LNAM = ISTRLEN (OUTNAM)
         END IF
         IF ((INBASE .NE. ' ') .AND. (INBASE .NE. '-')) THEN
            OUTNAM(LNAM+1:) = INBASE
            LNAM = ISTRLEN (OUTNAM)
         END IF
         IF ((INEXT .NE. ' ') .AND. (INEXT .NE. '-')) THEN
            OUTNAM(LNAM+1:) = INEXT
            LNAM = ISTRLEN (OUTNAM)
         END IF
         IF ((INVERS .NE. ' ') .AND. (INVERS .NE. '-')) THEN
            OUTNAM(LNAM+1:) = INVERS
            LNAM = ISTRLEN (OUTNAM)
         END IF
      END IF
      IF (OUTNAM .EQ. '-') OUTNAM = ' '

      RETURN
      END
