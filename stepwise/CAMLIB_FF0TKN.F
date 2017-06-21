C=======================================================================
      SUBROUTINE FF0TKN (LINE, SCRLIN, ILEFT, ISTOP, EQUAL, STORIT,
     &   INTYP, CFIELD, IFIELD, RFIELD)
C=======================================================================

C   --*** FF0TKN *** (CAMCON_LIB internal) Return free-field reader field
C   --   Revised by Amy Gilkey - revised 12/03/94
C   --   Free-field reader taken from the SUPES library
C   --
C   --FF0TKN returns the next field of a free-field reader line.
C   --
C   --Parameters:
C   --   LINE - IN - the input string
C   --   SCRLIN - IN - the processed input line
C   --   ILEFT - IN/OUT - the index of the first character to process
C   --      in the input line
C   --   ISTOP - IN - the index of the last character in the processed line
C   --   EQUAL - IN - true iff equal sign should be flagged as field
C   --   STORIT - IN - true iff data field should be stored;
C   --      if false INTYP, CFIELD, IFIELD, RFIELD are not set!
C   --   INTYP - OUT - the translation states of the data field:
C   --      -1 = a null field
C   --       0 = a non-numeric field
C   --       1 = a REAL numeric field
C   --       2 = an INTEGER numeric field
C   --       3 = an equal sign field (if EQUAL)
C   --   CFIELD - OUT - the character values of the data field
C   --   IFIELD - OUT - the integer values of the data field
C   --   RFIELD - OUT - the floating-point values of the data field

      IMPLICIT NONE

C   --Parameter Definitions:
      CHARACTER*(*) LINE
      CHARACTER*(*) SCRLIN
      INTEGER ILEFT, ISTOP
      LOGICAL EQUAL
      LOGICAL STORIT
      INTEGER INTYP
      CHARACTER*(*) CFIELD
      INTEGER IFIELD
      REAL RFIELD

      INTEGER ICOMMA
      INTEGER IRIGHT
      INTEGER IOFF
      INTEGER LFLD
      INTEGER IJUST
      INTEGER ITRANS
      INTEGER INUM
      INTEGER IX
      INTEGER IXQUO
      REAL RNUM
      CHARACTER CH
      CHARACTER*32 CNUM
      CHARACTER*88 CFLD

      CHARACTER QUOCH, DQUOCH
      PARAMETER (QUOCH = '''', DQUOCH = '"')

************************************************************************

C--Extract, translate, and catagorize data field.
C--LINE(ILEFT:IRIGHT) represents the current data field.
C--ICOMMA points to the next field separator and ISTOP always
C--points to the last character in the effective input line.

C   --Find the next separator -
      ICOMMA = INDEX (SCRLIN(ILEFT:ISTOP), ',') + ILEFT - 1
      IF (ICOMMA .EQ. ILEFT) THEN
C      --Check if the null field has an equal sign as a separator;
C      --if so save the equal sign -
         IF (EQUAL .AND. (LINE(ICOMMA:ICOMMA) .EQ. '=')) THEN
            IRIGHT = ICOMMA
         ELSE
            IRIGHT = ICOMMA - 1
         END IF
      ELSE IF (ICOMMA .GE. ILEFT) THEN
C      --Set the right edge of this field to the character immediately before
C      --the next separator -
         IRIGHT = ICOMMA - 1
      ELSE
C      --There are no more separators in this line; set the right edge and the
C      --separator pointer to the last data character -
         IRIGHT = ISTOP
         ICOMMA = ISTOP
      END IF

************************************************************************

C   --Don't process this field unless the store flag is set -
      IF (STORIT) THEN

C      --Initialize output field to default values -
         INTYP = -1
         CFIELD = ' '
         IFIELD = 0
         RFIELD = 0.0

C      --Strip blanks from this field -
         IF (ILEFT .LE. IRIGHT) THEN
            IOFF = ILEFT - 1
            CALL STRLIMIT (LINE(ILEFT:IRIGHT), ILEFT, IRIGHT)
            ILEFT  = ILEFT  + IOFF
            IRIGHT = IRIGHT + IOFF
         END IF

C      --Calculate the effective length of this field -
         LFLD = IRIGHT - ILEFT + 1
         IF (LFLD .LE. 0) THEN
C         --This is a null field -
            CFLD = ' '
            INTYP = -1

         ELSE IF (LFLD .GT. 32) THEN
C         --This field exceeds the maximum allowable numeric field size;
C         --define only the character value -
            CFLD = LINE(ILEFT:IRIGHT)
            INTYP = 0

         ELSE

C         --Define the character value for this field, then right-justify and
C         --attempt numeric translations -
            CFLD = LINE(ILEFT:IRIGHT)
            INTYP = 0
            CNUM = ' '
            IJUST = 32 - LFLD + 1
            CNUM(IJUST:32) = LINE(ILEFT:IRIGHT)
            READ (CNUM, '(E32.0)', IOSTAT=ITRANS) RNUM
            IF (ITRANS .EQ. 0) THEN
C            --This field has a valid floating-point value -
               RFIELD = RNUM
               INTYP = 1
            END IF
            READ (CNUM, '(I32)', IOSTAT=ITRANS) INUM
            IF (ITRANS .EQ. 0) THEN
C            --This field has a valid integer value -
               IFIELD = INUM
               INTYP = 2
            END IF
         END IF

         CH = CFLD(1:1)
         IF ((CH .EQ. QUOCH) .OR. (CH .EQ. DQUOCH)) THEN
C         --This field is a quoted string; remove starting quote -
            CFLD = CFLD(2:)
            LFLD = LFLD - 1
            IX = 1
  100       CONTINUE
            IF (IX .LE. LFLD) THEN
C            --Find quote and delete from string; do not check next
C            --character (to allow two quote characters to become one quote) -
               IXQUO = INDEX (CFLD(IX:LFLD), CH) + IX - 1
               IF (IXQUO .GE. IX) THEN
                  CFLD(IXQUO:LFLD) = CFLD(IXQUO+1:LFLD)
                  LFLD = LFLD - 1
                  IX = IXQUO + 1
                  GOTO 100
               END IF
            END IF
         ELSE IF (EQUAL .AND. (CH .EQ. '=')) THEN
C         --Identify equal sign -
            INTYP = 3
         ELSE
C         --Convert non-quoted field to upper case -
            CALL STRUPCASE (CFLD)
         END IF
         CFIELD = CFLD
      END IF

************************************************************************

C   --Reposition left edge pointer beyond the next separator -
      IF (EQUAL .AND. (INTYP .EQ. 3)) THEN
         ILEFT = ICOMMA + 1
      ELSE IF (EQUAL .AND. (LINE(ICOMMA:ICOMMA) .EQ. '=')) THEN
         ILEFT = ICOMMA
      ELSE
         ILEFT = ICOMMA + 1
      END IF

      RETURN
      END
