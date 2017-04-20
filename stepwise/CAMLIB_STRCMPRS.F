C=======================================================================
      SUBROUTINE STRCMPRS (STRING, LSTR)
C=======================================================================

C   --*** STRCMPRS *** (CAMCON_LIB) Remove extra blanks from string
C   --   Written by Amy Gilkey - revised 05/28/95
C   --
C   --STRCMPRS left justifies the string and deletes extra blanks from the
C   --string.  An extra blank is any blank that follows another blank.
C   --Blanks are also deleted after characters ([{ and before characters )]}.
C   --
C   --Parameters:
C   --   STRING - IN/OUT char*(*) - the string to be compressed, returned,
C   --      may be up to 132 characters long
C   --   LSTR - OUT integer - the length of the compressed string
C
C   --Routines Called:
C   --   ISTRLEN - (CAMCON_LIB) Find string length

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) STRING
      INTEGER LSTR

C   --Definitions for functions called:
      INTEGER ISTRLEN

      INTEGER IBLK, LBLK
      INTEGER INOBLK
      CHARACTER*128 TMPSTR
      CHARACTER C0, C1

      LSTR = ISTRLEN (STRING)
      IF (LSTR .LE. 1) RETURN

C   --Compress multiple blanks into single blank
      IBLK = INDEX (STRING, '  ')
  100 CONTINUE
      IF ((IBLK .GT. 0) .AND. (IBLK .LE. LSTR)) THEN
         INOBLK = IBLK + 2
  110    CONTINUE
         IF (STRING(INOBLK:INOBLK) .EQ. ' ') THEN
            INOBLK = INOBLK + 1
            GOTO 110
         END IF
         TMPSTR = STRING(INOBLK:LSTR)
         STRING(IBLK+1:LSTR) = TMPSTR
         LSTR = LSTR - (INOBLK-IBLK-1)
         IBLK = INDEX (STRING, '  ')
         GOTO 100
      END IF

C   --Remove leading blank; note that multiple leading blanks have been
C   --compressed into single blank.
      IF (STRING(1:1) .EQ. ' ') THEN
         TMPSTR = STRING(2:LSTR)
         STRING(1:LSTR) = TMPSTR
         LSTR = LSTR - 1
      END IF

C   --Delete blanks after ([{ and before )]]
      LBLK = 0
      IBLK = INDEX (STRING, ' ')
  120 CONTINUE
      IF ((IBLK .GT. LBLK) .AND. (IBLK .LE. LSTR)) THEN
         C0 = STRING(IBLK-1:IBLK-1)
         C1 = STRING(IBLK+1:IBLK+1)
         IF ((C0 .EQ. '(') .OR. (C0 .EQ. '[') .OR. (C0 .EQ. '{') .OR.
     &      (C1 .EQ. ')') .OR. (C1 .EQ. ']') .OR. (C1 .EQ. '}')) THEN
            TMPSTR = STRING(IBLK+1:LSTR)
            STRING(IBLK:LSTR) = TMPSTR
            LSTR = LSTR - 1
            IBLK = IBLK - 1
         END IF
         LBLK = IBLK
         IBLK = IBLK + INDEX (STRING(IBLK+1:), ' ')
         GOTO 120
      END IF

      RETURN
      END
